#!/usr/bin/env bun
/**
 * End-to-End Integration Tests for Model Inference
 *
 * These tests require actual API credentials and make real API calls.
 * They are skipped automatically if credentials are not available.
 *
 * Required environment variables:
 * - CLOUDFLARE_ACCOUNT_ID: Your Cloudflare account ID
 * - CLOUDFLARE_GATEWAY_ID: Your AI Gateway ID
 * - CLOUDFLARE_API_TOKEN: Gateway token (unified billing - no provider keys needed)
 *
 * Run with: bun test model.e2e.test.ts
 */

import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import GraphStore from '../graph.ts';
import { ProviderManager } from './provider.ts';
import { ModelManager } from './model.ts';
import {
  ExecutionContext,
  runWithContextAsync,
  createContextFromEnv,
} from '../context.ts';

// Skip E2E tests by default - only run when RUN_E2E=true
// This keeps default test runs fast (no external API calls)
const skipE2E = !process.env.RUN_E2E;

// Check if we have the required credentials (unified billing - only need CF Gateway token)
// Support both /ai config names (CF_*) and standard names (CLOUDFLARE_*)
const hasCloudflareConfig =
  (process.env.CLOUDFLARE_ACCOUNT_ID || process.env.CF_ACCOUNT_ID) &&
  (process.env.CLOUDFLARE_GATEWAY_ID || process.env.CF_GATEWAY_NAME) &&
  (process.env.CLOUDFLARE_API_TOKEN || process.env.CF_AIG_TOKEN);

// Helper to skip tests if E2E disabled or credentials missing
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
  // Skip if E2E tests disabled
  if (skipE2E) {
    describe.skip(`${name} (E2E tests disabled - set RUN_E2E=true to run)`, fn);
    return;
  }

  // Skip if credentials missing (check aliases)
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

describeWithCredentials(
  'E2E: Cloudflare AI Gateway with Anthropic',
  ['CLOUDFLARE_ACCOUNT_ID', 'CLOUDFLARE_GATEWAY_ID', 'CLOUDFLARE_API_TOKEN'],
  () => {
    let store: GraphStore;
    let providerManager: ProviderManager;
    let modelManager: ModelManager;

    beforeEach(async () => {
      store = new GraphStore();
      providerManager = new ProviderManager(store);
      modelManager = new ModelManager(store, providerManager);

      // Create and publish provider
      await providerManager.createProvider('cf-gateway', 'cloudflare-ai-gateway', {
        accountId: process.env.CLOUDFLARE_ACCOUNT_ID,
        gatewayId: process.env.CLOUDFLARE_GATEWAY_ID,
      });
      await providerManager.publishProvider('cf-gateway');
    });

    afterEach(() => {
      // GraphStore is in-memory, no close needed
    });

    test('invokes Claude model successfully', async () => {
      // Create and publish model
      await modelManager.createModel('claude-test', 'claude-sonnet-4-5', 'cf-gateway', {
        temperature: 0.1,
        maxTokens: 100,
      });
      await modelManager.publishModel('claude-test');

      // Run with context containing credentials
      const ctx = createContextFromEnv({ id: 'e2e-test', type: 'system' });

      const result = await runWithContextAsync(ctx, async () => {
        return modelManager.invokeModel('claude-test', {
          message: 'What is 2 + 2? Answer with just the number.',
          system: 'You are a helpful assistant. Be brief.',
        });
      });

      if (!result.success) {
        console.log('Inference failed:', result.error);
      }
      expect(result.success).toBe(true);
      expect(result.text).toBeDefined();
      expect(result.text).toContain('4');
      expect(result.usage).toBeDefined();
      expect(result.model).toBe('claude-sonnet-4-5');
      console.log('Claude response:', result.text);
      console.log('Usage:', result.usage);
    }, 30000); // 30s timeout for API call

    test('respects situational params', async () => {
      // Create model with situations
      await modelManager.createModel('claude-situations', 'claude-sonnet-4-5', 'cf-gateway', {
        temperature: 0.5,
        maxTokens: 200,
        situations: {
          creative: { temperature: 0.9, maxTokens: 500 },
          precise: { temperature: 0.1, maxTokens: 50 },
        },
      });
      await modelManager.publishModel('claude-situations');

      const ctx = createContextFromEnv({ id: 'e2e-test', type: 'system' });

      // Test with 'precise' situation
      const result = await runWithContextAsync(ctx, async () => {
        return modelManager.invokeModel('claude-situations', {
          message: 'What is 2 + 2?',
          situation: 'precise',
        });
      });

      expect(result.success).toBe(true);
      expect(result.situation).toBe('precise');
      console.log('Precise response:', result.text);
    }, 30000);
  }
);

describeWithCredentials(
  'E2E: Cloudflare AI Gateway with OpenAI',
  ['CLOUDFLARE_ACCOUNT_ID', 'CLOUDFLARE_GATEWAY_ID', 'CLOUDFLARE_API_TOKEN'],
  () => {
    let store: GraphStore;
    let providerManager: ProviderManager;
    let modelManager: ModelManager;

    beforeEach(async () => {
      store = new GraphStore();
      providerManager = new ProviderManager(store);
      modelManager = new ModelManager(store, providerManager);

      // Create and publish provider
      await providerManager.createProvider('cf-gateway', 'cloudflare-ai-gateway', {
        accountId: process.env.CLOUDFLARE_ACCOUNT_ID,
        gatewayId: process.env.CLOUDFLARE_GATEWAY_ID,
      });
      await providerManager.publishProvider('cf-gateway');
    });

    afterEach(() => {
      // GraphStore is in-memory, no close needed
    });

    test('invokes GPT model successfully', async () => {
      // Create and publish model
      await modelManager.createModel('gpt-test', 'gpt-4o-mini', 'cf-gateway', {
        temperature: 0.1,
        maxTokens: 100,
      });
      await modelManager.publishModel('gpt-test');

      // Run with context containing credentials
      const ctx = createContextFromEnv({ id: 'e2e-test', type: 'system' });

      const result = await runWithContextAsync(ctx, async () => {
        return modelManager.invokeModel('gpt-test', {
          message: 'What is 2 + 2? Answer with just the number.',
        });
      });

      expect(result.success).toBe(true);
      expect(result.text).toBeDefined();
      expect(result.text).toContain('4');
      expect(result.model).toBe('gpt-4o-mini');
      console.log('GPT response:', result.text);
      console.log('Usage:', result.usage);
    }, 30000);
  }
);

// Always run this test to document what's needed
describe('E2E: Credential check', () => {
  test('reports available credentials', () => {
    console.log('\n--- E2E Test Credential Status (Unified Billing) ---');
    console.log('CLOUDFLARE_ACCOUNT_ID:', (process.env.CLOUDFLARE_ACCOUNT_ID || process.env.CF_ACCOUNT_ID) ? '✓ set' : '✗ missing');
    console.log('CLOUDFLARE_GATEWAY_ID:', (process.env.CLOUDFLARE_GATEWAY_ID || process.env.CF_GATEWAY_NAME) ? '✓ set' : '✗ missing');
    console.log('CLOUDFLARE_API_TOKEN:', (process.env.CLOUDFLARE_API_TOKEN || process.env.CF_AIG_TOKEN) ? '✓ set' : '✗ missing');
    console.log('(No provider API keys needed - using unified billing)');
    console.log('----------------------------------------------------\n');

    // This test always passes - it's just informational
    expect(true).toBe(true);
  });
});
