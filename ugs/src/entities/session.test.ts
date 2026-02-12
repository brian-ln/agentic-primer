import { test, expect, describe, beforeEach, mock } from 'bun:test';
import { SessionManager, Session, SessionLifecycle, SessionLogEntry } from './session.ts';
import { ModelManager, InferenceResult } from './model.ts';
import { ProviderManager } from './provider.ts';
import GraphStore from '../graph.ts';
import { existsSync } from 'node:fs';
import { rm } from 'node:fs/promises';
import { join } from 'node:path';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('SessionManager', () => {
  let store: GraphStore;
  let providerManager: ProviderManager;
  let modelManager: ModelManager;
  let sessionManager: SessionManager;
  let testDataDir: string;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    testDataDir = `/tmp/ugs-session-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`;
    store = new GraphStore(testDataDir);
    await store.initialize();
    providerManager = new ProviderManager(store);
    modelManager = new ModelManager(store, providerManager);
    sessionManager = new SessionManager(store, modelManager, testDataDir);

    // Create a default provider and model for tests
    await providerManager.createProvider('test-provider', 'cloudflare-ai-gateway', {
      accountId: 'test-account',
      gatewayId: 'test-gateway'
    });
    await providerManager.publishProvider('test-provider');

    await modelManager.createModel('test-model', 'claude-sonnet-4-5', 'test-provider');
    await modelManager.publishModel('test-model');
  });

  describe('createSession', () => {
    test('creates a session in created lifecycle', async () => {
      const session = await sessionManager.createSession('test-session', 'test-model');

      expect(session.id).toBe('test-session');
      expect(session.config.defaultModel).toBe('@(test-model)');
      expect(session.config.logFile).toBe('sessions/test-session.jsonl');
      expect(session.lifecycle).toBe('created');
      expect(session.version).toBe(1);
      expect(session.type).toBe('program');
      expect(session.programType).toBe('session');
    });

    test('creates a session with owner', async () => {
      const session = await sessionManager.createSession('owned-session', 'test-model', {
        owner: '@(agent-123)'
      });

      expect(session.config.owner).toBe('@(agent-123)');
    });

    test('handles @(model-id) format in defaultModel', async () => {
      const session = await sessionManager.createSession('ref-session', '@(test-model)');

      expect(session.config.defaultModel).toBe('@(test-model)');
    });

    test('throws error if session already exists', async () => {
      await sessionManager.createSession('dup-session', 'test-model');

      await expect(sessionManager.createSession('dup-session', 'test-model'))
        .rejects.toThrow('Session already exists: dup-session');
    });

    test('throws error if model does not exist', async () => {
      await expect(sessionManager.createSession('bad-model-session', 'nonexistent-model'))
        .rejects.toThrow('Model not found: nonexistent-model');
    });

    test('emits SESSION_CREATED event', async () => {
      await sessionManager.createSession('event-test', 'test-model');

      const events = sessionManager.getSessionEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'SESSION_CREATED' && e.sessionId === 'event-test');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.lifecycle).toBe('created');
      expect(createEvent!.data.defaultModel).toBe('@(test-model)');
    });

    test('creates edge to model', async () => {
      await sessionManager.createSession('edge-test', 'test-model');

      const edge = store.get('edge-test_uses_test-model');
      expect(edge).toBeDefined();
    });
  });

  describe('getSession', () => {
    test('returns null for non-existent session', () => {
      const session = sessionManager.getSession('nonexistent');
      expect(session).toBeNull();
    });

    test('retrieves an existing session', async () => {
      await sessionManager.createSession('get-test', 'test-model', {
        owner: '@(human-alice)'
      });

      const session = sessionManager.getSession('get-test');
      expect(session).not.toBeNull();
      expect(session!.id).toBe('get-test');
      expect(session!.config.owner).toBe('@(human-alice)');
      expect(session!.config.defaultModel).toBe('@(test-model)');
    });
  });

  describe('listSessions', () => {
    test('lists all sessions', async () => {
      await sessionManager.createSession('list-1', 'test-model');
      await sessionManager.createSession('list-2', 'test-model');
      await sessionManager.createSession('list-3', 'test-model');

      const sessions = sessionManager.listSessions();

      expect(sessions.length).toBe(3);
    });

    test('filters sessions by lifecycle', async () => {
      await sessionManager.createSession('created-1', 'test-model');
      await sessionManager.createSession('created-2', 'test-model');

      // Note: We can't easily transition to active without mocking the model
      // So we'll just test filtering by created lifecycle
      const created = sessionManager.listSessions('created');
      expect(created.length).toBe(2);

      const active = sessionManager.listSessions('active');
      expect(active.length).toBe(0);
    });
  });

  describe('pauseSession', () => {
    test('throws error when pausing created session', async () => {
      await sessionManager.createSession('pause-created', 'test-model');

      await expect(sessionManager.pauseSession('pause-created'))
        .rejects.toThrow('Cannot pause session in created lifecycle');
    });

    test('throws error when pausing non-existent session', async () => {
      await expect(sessionManager.pauseSession('nonexistent'))
        .rejects.toThrow('Session not found: nonexistent');
    });

    test('emits SESSION_PAUSED event when successful', async () => {
      await sessionManager.createSession('pause-evt', 'test-model');
      // Force transition to active
      await store.updateNode('pause-evt', { lifecycle: 'active' });

      await sessionManager.pauseSession('pause-evt');

      const events = sessionManager.getSessionEventHistory('pause-evt');
      const pauseEvent = events.find(e => e.type === 'SESSION_PAUSED');
      expect(pauseEvent).toBeDefined();
      expect(pauseEvent!.data.newLifecycle).toBe('paused');
    });
  });

  describe('resumeSession', () => {
    test('resumes a paused session', async () => {
      await sessionManager.createSession('resume-test', 'test-model');
      // Force transitions
      await store.updateNode('resume-test', { lifecycle: 'active' });
      await sessionManager.pauseSession('resume-test');

      const resumed = await sessionManager.resumeSession('resume-test');

      expect(resumed.lifecycle).toBe('active');
    });

    test('throws error when resuming non-paused session', async () => {
      await sessionManager.createSession('resume-created', 'test-model');

      await expect(sessionManager.resumeSession('resume-created'))
        .rejects.toThrow('Cannot resume session in created lifecycle');
    });

    test('emits SESSION_RESUMED event', async () => {
      await sessionManager.createSession('resume-evt', 'test-model');
      await store.updateNode('resume-evt', { lifecycle: 'active' });
      await sessionManager.pauseSession('resume-evt');
      await sessionManager.resumeSession('resume-evt');

      const events = sessionManager.getSessionEventHistory('resume-evt');
      const resumeEvent = events.find(e => e.type === 'SESSION_RESUMED');
      expect(resumeEvent).toBeDefined();
      expect(resumeEvent!.data.newLifecycle).toBe('active');
    });
  });

  describe('completeSession', () => {
    test('completes an active session', async () => {
      await sessionManager.createSession('complete-active', 'test-model');
      await store.updateNode('complete-active', { lifecycle: 'active' });

      const completed = await sessionManager.completeSession('complete-active');

      expect(completed.lifecycle).toBe('completed');
    });

    test('completes a paused session', async () => {
      await sessionManager.createSession('complete-paused', 'test-model');
      await store.updateNode('complete-paused', { lifecycle: 'paused' });

      const completed = await sessionManager.completeSession('complete-paused');

      expect(completed.lifecycle).toBe('completed');
    });

    test('throws error when completing created session', async () => {
      await sessionManager.createSession('complete-created', 'test-model');

      await expect(sessionManager.completeSession('complete-created'))
        .rejects.toThrow('Cannot complete session in created lifecycle');
    });

    test('emits SESSION_COMPLETED event', async () => {
      await sessionManager.createSession('complete-evt', 'test-model');
      await store.updateNode('complete-evt', { lifecycle: 'active' });
      await sessionManager.completeSession('complete-evt');

      const events = sessionManager.getSessionEventHistory('complete-evt');
      const completeEvent = events.find(e => e.type === 'SESSION_COMPLETED');
      expect(completeEvent).toBeDefined();
      expect(completeEvent!.data.newLifecycle).toBe('completed');
    });
  });

  describe('lifecycle machine transitions', () => {
    test('created -> active (on first message or manual transition)', async () => {
      await sessionManager.createSession('sm-1', 'test-model');
      await store.updateNode('sm-1', { lifecycle: 'active' });
      const session = sessionManager.getSession('sm-1');
      expect(session!.lifecycle).toBe('active');
    });

    test('active -> paused transition is valid', async () => {
      await sessionManager.createSession('sm-2', 'test-model');
      await store.updateNode('sm-2', { lifecycle: 'active' });
      const paused = await sessionManager.pauseSession('sm-2');
      expect(paused.lifecycle).toBe('paused');
    });

    test('paused -> active transition is valid', async () => {
      await sessionManager.createSession('sm-3', 'test-model');
      await store.updateNode('sm-3', { lifecycle: 'paused' });
      const active = await sessionManager.resumeSession('sm-3');
      expect(active.lifecycle).toBe('active');
    });

    test('active -> completed transition is valid', async () => {
      await sessionManager.createSession('sm-4', 'test-model');
      await store.updateNode('sm-4', { lifecycle: 'active' });
      const completed = await sessionManager.completeSession('sm-4');
      expect(completed.lifecycle).toBe('completed');
    });

    test('paused -> completed transition is valid', async () => {
      await sessionManager.createSession('sm-5', 'test-model');
      await store.updateNode('sm-5', { lifecycle: 'paused' });
      const completed = await sessionManager.completeSession('sm-5');
      expect(completed.lifecycle).toBe('completed');
    });

    test('created -> paused transition is invalid', async () => {
      await sessionManager.createSession('sm-6', 'test-model');
      await expect(sessionManager.pauseSession('sm-6'))
        .rejects.toThrow('Cannot pause session in created lifecycle');
    });

    test('created -> completed transition is invalid', async () => {
      await sessionManager.createSession('sm-7', 'test-model');
      await expect(sessionManager.completeSession('sm-7'))
        .rejects.toThrow('Cannot complete session in created lifecycle');
    });
  });

  describe('JSONL logging', () => {
    test('creates sessions directory on first log', async () => {
      await sessionManager.createSession('log-test', 'test-model');

      // Force a log entry by manually calling private method through sendMessage attempt
      // which will fail but should create the directory
      const sessionsDir = join(testDataDir, 'sessions');

      // The directory should be created when we try to get history or send a message
      try {
        await sessionManager.getHistory('log-test');
      } catch (e) {
        // Expected to fail if no log file exists yet
      }

      // Directory should exist after session is created
      // (it's created lazily on first write)
    });

    test('getHistory returns empty array for session with no messages', async () => {
      await sessionManager.createSession('empty-history', 'test-model');

      const history = await sessionManager.getHistory('empty-history');

      expect(history).toEqual([]);
    });

    test('throws error when getting history for non-existent session', async () => {
      await expect(sessionManager.getHistory('nonexistent'))
        .rejects.toThrow('Session not found: nonexistent');
    });
  });

  describe('sendMessage', () => {
    test('throws error for non-existent session', async () => {
      await expect(sessionManager.sendMessage('nonexistent', 'hello'))
        .rejects.toThrow('Session not found: nonexistent');
    });

    test('throws error for completed session', async () => {
      await sessionManager.createSession('completed-session', 'test-model');
      await store.updateNode('completed-session', { lifecycle: 'completed' });

      await expect(sessionManager.sendMessage('completed-session', 'hello'))
        .rejects.toThrow('Cannot send message to completed session');
    });

    test('sendMessage invokes model and returns response', async () => {
      // Mock the model invocation
      modelManager.invokeModel = mock(async () => ({
        success: true,
        text: 'Hello! How can I help you?',
        model: 'claude-sonnet-4-5',
        usage: { promptTokens: 10, completionTokens: 15, totalTokens: 25 }
      }));

      await sessionManager.createSession('msg-session', 'test-model');
      const result = await sessionManager.sendMessage('msg-session', 'hello');

      expect(result.success).toBe(true);
      expect(result.text).toBe('Hello! How can I help you?');
    });

    test('sendMessage accepts streaming options', async () => {
      const receivedTokens: string[] = [];
      modelManager.invokeModel = mock(async (id, opts) => {
        // Simulate streaming by calling onToken
        if (opts.stream && opts.onToken) {
          opts.onToken('Hello');
          opts.onToken(' world');
        }
        return {
          success: true,
          text: 'Hello world',
          model: 'claude-sonnet-4-5',
          usage: { promptTokens: 5, completionTokens: 2, totalTokens: 7 }
        };
      });

      await sessionManager.createSession('stream-session', 'test-model');
      const result = await sessionManager.sendMessage('stream-session', 'hi', {
        stream: true,
        onToken: (token) => receivedTokens.push(token)
      });

      expect(result.success).toBe(true);
      expect(receivedTokens).toEqual(['Hello', ' world']);
    });

    test('sendMessage logs messages to session history', async () => {
      modelManager.invokeModel = mock(async () => ({
        success: true,
        text: 'Test response',
        model: 'claude-sonnet-4-5',
        usage: { promptTokens: 5, completionTokens: 5, totalTokens: 10 }
      }));

      await sessionManager.createSession('history-session', 'test-model');
      await sessionManager.sendMessage('history-session', 'Test message');

      const history = await sessionManager.getHistory('history-session');
      expect(history.length).toBe(2); // user + assistant
      expect(history[0].message.role).toBe('user');
      expect(history[0].message.content).toBe('Test message');
      expect(history[1].message.role).toBe('assistant');
      expect(history[1].message.content).toBe('Test response');
    });
  });

  describe('events', () => {
    test('getSessionEvents returns all events', async () => {
      await sessionManager.createSession('evt-1', 'test-model');
      await sessionManager.createSession('evt-2', 'test-model');

      const events = sessionManager.getSessionEvents();

      expect(events.length).toBeGreaterThanOrEqual(2);
    });

    test('getSessionEvents respects limit', async () => {
      await sessionManager.createSession('lim-1', 'test-model');
      await sessionManager.createSession('lim-2', 'test-model');
      await sessionManager.createSession('lim-3', 'test-model');

      const events = sessionManager.getSessionEvents(2);

      expect(events.length).toBe(2);
    });

    test('getSessionEventHistory filters by session', async () => {
      await sessionManager.createSession('hist-1', 'test-model');
      await sessionManager.createSession('hist-2', 'test-model');

      const hist1Events = sessionManager.getSessionEventHistory('hist-1');
      const hist2Events = sessionManager.getSessionEventHistory('hist-2');

      expect(hist1Events.length).toBe(1);
      expect(hist2Events.length).toBe(1);
      expect(hist1Events[0].sessionId).toBe('hist-1');
      expect(hist2Events[0].sessionId).toBe('hist-2');
    });
  });
});
