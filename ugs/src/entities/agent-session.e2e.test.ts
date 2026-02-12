#!/usr/bin/env bun
/**
 * End-to-End Integration Tests for Agent + Session
 *
 * Tests the full flow: Task → Agent → Session → Model → Provider → LLM
 *
 * Required environment variables:
 * - CLOUDFLARE_ACCOUNT_ID: Your Cloudflare account ID
 * - CLOUDFLARE_GATEWAY_ID: Your AI Gateway ID
 * - CLOUDFLARE_API_TOKEN: Gateway token (unified billing - no provider keys needed)
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import { rm } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import GraphStore from '../graph.ts';
import { ProviderManager } from './provider.ts';
import { ModelManager } from './model.ts';
import { SessionManager } from './session.ts';
import { TaskManager } from './task.ts';
import { AgentManager } from './agent.ts';
import {
  ExecutionContext,
  runWithContextAsync,
  createContextFromEnv,
} from '../context.ts';

// Check credentials (unified billing - only need CF Gateway token)
// Support both /ai config names (CF_*) and standard names (CLOUDFLARE_*)
const hasCloudflareConfig =
  (process.env.CLOUDFLARE_ACCOUNT_ID || process.env.CF_ACCOUNT_ID) &&
  (process.env.CLOUDFLARE_GATEWAY_ID || process.env.CF_GATEWAY_NAME) &&
  (process.env.CLOUDFLARE_API_TOKEN || process.env.CF_AIG_TOKEN);

// Helper to skip tests if credentials missing
// Supports both /ai config names and standard names
const credentialAliases: Record<string, string[]> = {
  'CLOUDFLARE_ACCOUNT_ID': ['CLOUDFLARE_ACCOUNT_ID', 'CF_ACCOUNT_ID'],
  'CLOUDFLARE_GATEWAY_ID': ['CLOUDFLARE_GATEWAY_ID', 'CF_GATEWAY_NAME'],
  'CLOUDFLARE_API_TOKEN': ['CLOUDFLARE_API_TOKEN', 'CF_AIG_TOKEN'],
};

const describeWithCredentials = (
  name: string,
  requiredCreds: string[],
  fn: () => void
) => {
  const missing = requiredCreds.filter((k) => {
    const aliases = credentialAliases[k] || [k];
    return !aliases.some((alias) => process.env[alias]);
  });
  if (missing.length > 0) {
    describe.skip(`${name} (missing: ${missing.join(', ')})`, fn);
  } else {
    describe(name, fn);
  }
};

const DATA_DIR = './data-agent-session-e2e';

describeWithCredentials(
  'E2E: Agent + Session Integration',
  ['CLOUDFLARE_ACCOUNT_ID', 'CLOUDFLARE_GATEWAY_ID', 'CLOUDFLARE_API_TOKEN'],
  () => {
    let store: GraphStore;
    let providerManager: ProviderManager;
    let modelManager: ModelManager;
    let sessionManager: SessionManager;
    let taskManager: TaskManager;
    let agentManager: AgentManager;

    beforeEach(async () => {
      // Clean up data directory
      if (existsSync(DATA_DIR)) {
        await rm(DATA_DIR, { recursive: true });
      }

      // Pass data directory to GraphStore for WAL persistence
      store = new GraphStore(DATA_DIR);
      await store.initialize();  // Create data directory and load any existing state

      providerManager = new ProviderManager(store);
      modelManager = new ModelManager(store, providerManager);
      sessionManager = new SessionManager(store, modelManager, DATA_DIR);
      taskManager = new TaskManager(store);
      agentManager = new AgentManager(store, taskManager, modelManager, sessionManager);

      // Set up provider
      await providerManager.createProvider('cf-gateway', 'cloudflare-ai-gateway', {
        accountId: process.env.CLOUDFLARE_ACCOUNT_ID,
        gatewayId: process.env.CLOUDFLARE_GATEWAY_ID,
      });
      await providerManager.publishProvider('cf-gateway');

      // Set up model
      await modelManager.createModel('claude-test', 'claude-sonnet-4-5', 'cf-gateway', {
        temperature: 0.1,
        maxTokens: 100,
      });
      await modelManager.publishModel('claude-test');
    });

    afterEach(async () => {
      // Clean up
      if (existsSync(DATA_DIR)) {
        await rm(DATA_DIR, { recursive: true });
      }
    });

    test('agent creates session when assigned task', async () => {
      const ctx = createContextFromEnv({ id: 'e2e-test', type: 'system' });

      await runWithContextAsync(ctx, async () => {
        // Create task
        await taskManager.createTask('test-task', 'What is 2 + 2?');

        // Create agent
        await agentManager.createAgent('test-agent', 'Calculator', 'You are a math assistant. Answer briefly.', {
          defaultModel: '@(claude-test)',
        });

        // Assign task - this should create a session
        const agent = await agentManager.assignTask('test-agent', 'test-task');

        expect(agent.state).toBe('thinking');
        expect(agent.config.currentTask).toBe('test-task');
        expect(agent.config.currentSession).toBeDefined();
        expect(agent.config.currentSession).toContain('test-agent_test-task');

        // Verify session was created
        const session = sessionManager.getSession(agent.config.currentSession!);
        expect(session).not.toBeNull();
        expect(session!.config.owner).toBe('@(test-agent)');
      });
    });

    test('agent.step() uses session for inference', async () => {
      const ctx = createContextFromEnv({ id: 'e2e-test', type: 'system' });

      const result = await runWithContextAsync(ctx, async () => {
        // Create task
        await taskManager.createTask('math-task', 'What is 2 + 2?');

        // Create agent
        await agentManager.createAgent('math-agent', 'Math Assistant', 'You are a math assistant. Always answer with just the number.', {
          defaultModel: '@(claude-test)',
          harness: { maxTurns: 3 }
        });

        // Assign task
        const agent = await agentManager.assignTask('math-agent', 'math-task');

        // Execute one step
        return agentManager.step('math-agent');
      });

      console.log('Step result:', result);
      expect(result.turn).toBe(1);
      expect(result.response).toBeDefined();
      // The response should contain "4" somewhere
      expect(result.response).toMatch(/4/);
    }, 30000);

    test('session preserves conversation history across turns', async () => {
      const ctx = createContextFromEnv({ id: 'e2e-test', type: 'system' });

      const history = await runWithContextAsync(ctx, async () => {
        // Create task
        await taskManager.createTask('multi-turn-task', 'Tell me a number between 1 and 10');

        // Create agent
        await agentManager.createAgent('multi-agent', 'Number Agent', 'You are a helpful assistant. Be brief.', {
          defaultModel: '@(claude-test)',
          harness: { maxTurns: 5 }
        });

        // Assign task
        const agent = await agentManager.assignTask('multi-agent', 'multi-turn-task');

        // Execute two steps
        await agentManager.step('multi-agent');
        await agentManager.step('multi-agent');

        // Get session history
        return sessionManager.getHistory(agent.config.currentSession!);
      });

      console.log('History entries:', history.length);
      // Should have 4 entries: 2 user messages + 2 assistant responses
      expect(history.length).toBe(4);

      // Verify message structure
      const userMessages = history.filter(e => e.message.role === 'user');
      const assistantMessages = history.filter(e => e.message.role === 'assistant');
      expect(userMessages.length).toBe(2);
      expect(assistantMessages.length).toBe(2);

      // Assistant messages should reference user messages via parentUuid
      expect(assistantMessages[0].parentUuid).toBe(userMessages[0].uuid);
      expect(assistantMessages[1].parentUuid).toBe(userMessages[1].uuid);
    }, 60000);

    test('full workflow: Task → Agent → Session → Model → Provider → LLM', async () => {
      const ctx = createContextFromEnv({ id: 'e2e-test', type: 'system' });

      const events = await runWithContextAsync(ctx, async () => {
        // 1. Create a task
        const task = await taskManager.createTask('research-task', 'What is the capital of France?');
        expect(task.lifecycle).toBe('pending');

        // 2. Create an agent
        const agent = await agentManager.createAgent('researcher', 'Researcher', 'You are a knowledgeable assistant. Answer questions concisely.', {
          defaultModel: '@(claude-test)',
        });
        expect(agent.state).toBe('idle');

        // 3. Assign task to agent (creates session)
        const assignedAgent = await agentManager.assignTask('researcher', 'research-task');
        expect(assignedAgent.state).toBe('thinking');
        expect(assignedAgent.config.currentSession).toBeDefined();

        // 4. Execute a step (Session → Model → Provider → LLM)
        const stepResult = await agentManager.step('researcher');
        console.log('Full workflow response:', stepResult.response);

        // Verify response mentions Paris
        expect(stepResult.response?.toLowerCase()).toContain('paris');

        // 5. Get session history to verify logging
        const history = await sessionManager.getHistory(assignedAgent.config.currentSession!);
        expect(history.length).toBe(2); // 1 user + 1 assistant

        // 6. Collect events from all managers
        return {
          providerEvents: providerManager.getProviderEvents(),
          modelEvents: modelManager.getModelEvents(),
          sessionEvents: sessionManager.getSessionEvents(),
          taskEvents: taskManager.getTaskEvents(),
          agentEvents: agentManager.getAgentEvents(),
          history
        };
      });

      // Verify event logging
      console.log('Events collected:');
      console.log('  Provider:', events.providerEvents.length);
      console.log('  Model:', events.modelEvents.length);
      console.log('  Session:', events.sessionEvents.length);
      console.log('  Task:', events.taskEvents.length);
      console.log('  Agent:', events.agentEvents.length);

      expect(events.providerEvents.length).toBeGreaterThan(0);
      expect(events.modelEvents.length).toBeGreaterThan(0);
      expect(events.sessionEvents.length).toBeGreaterThan(0);
      expect(events.taskEvents.length).toBeGreaterThan(0);
      expect(events.agentEvents.length).toBeGreaterThan(0);
    }, 60000);
  }
);

// Always run credential check
describe('Agent+Session E2E: Credential check', () => {
  test('reports available credentials', () => {
    console.log('\n--- Agent+Session E2E Test Credential Status (Unified Billing) ---');
    console.log('CLOUDFLARE_ACCOUNT_ID:', (process.env.CLOUDFLARE_ACCOUNT_ID || process.env.CF_ACCOUNT_ID) ? '✓ set' : '✗ missing');
    console.log('CLOUDFLARE_GATEWAY_ID:', (process.env.CLOUDFLARE_GATEWAY_ID || process.env.CF_GATEWAY_NAME) ? '✓ set' : '✗ missing');
    console.log('CLOUDFLARE_API_TOKEN:', (process.env.CLOUDFLARE_API_TOKEN || process.env.CF_AIG_TOKEN) ? '✓ set' : '✗ missing');
    console.log('(No provider API keys needed - using unified billing)');
    console.log('-----------------------------------------------------------------\n');
    expect(true).toBe(true);
  });
});
