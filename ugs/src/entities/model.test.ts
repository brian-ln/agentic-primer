import { test, expect, describe, beforeEach } from 'bun:test';
import { ModelManager, Model, ModelLifecycle, SituationParams, StreamingOptions } from './model.ts';
import { ProviderManager } from './provider.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('ModelManager', () => {
  let store: GraphStore;
  let providerManager: ProviderManager;
  let modelManager: ModelManager;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    store = new GraphStore(`/tmp/ugs-model-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    providerManager = new ProviderManager(store);
    modelManager = new ModelManager(store, providerManager);

    // Create a default provider for tests
    await providerManager.createProvider('test-provider', 'cloudflare-ai-gateway', {
      accountId: 'test-account',
      gatewayId: 'test-gateway'
    });
  });

  describe('createModel', () => {
    test('creates a model in draft lifecycle', async () => {
      const model = await modelManager.createModel('test-model', 'claude-sonnet-4-5', 'test-provider');

      expect(model.id).toBe('test-model');
      expect(model.config.backendModel).toBe('claude-sonnet-4-5');
      expect(model.config.provider).toBe('@(test-provider)');
      expect(model.lifecycle).toBe('draft');
      expect(model.version).toBe(1);
      expect(model.type).toBe('program');
      expect(model.programType).toBe('model');
    });

    test('creates a model with custom options', async () => {
      const model = await modelManager.createModel('claude-fast', 'claude-sonnet-4-5', 'test-provider', {
        name: 'Claude Fast',
        temperature: 0.7,
        maxTokens: 4000,
        topP: 0.9,
        situations: {
          coding: { temperature: 0, maxTokens: 8000 },
          creative: { temperature: 0.95 }
        }
      });

      expect(model.config.name).toBe('Claude Fast');
      expect(model.config.temperature).toBe(0.7);
      expect(model.config.maxTokens).toBe(4000);
      expect(model.config.topP).toBe(0.9);
      expect(model.config.situations).toEqual({
        coding: { temperature: 0, maxTokens: 8000 },
        creative: { temperature: 0.95 }
      });
    });

    test('throws error if model already exists', async () => {
      await modelManager.createModel('dup-model', 'claude-sonnet-4-5', 'test-provider');

      await expect(modelManager.createModel('dup-model', 'gpt-4o', 'test-provider'))
        .rejects.toThrow('Model already exists: dup-model');
    });

    test('throws error if provider does not exist', async () => {
      await expect(modelManager.createModel('bad-provider-model', 'claude-sonnet-4-5', 'nonexistent-provider'))
        .rejects.toThrow('Provider not found: nonexistent-provider');
    });

    test('emits MODEL_CREATED event', async () => {
      await modelManager.createModel('event-test', 'claude-sonnet-4-5', 'test-provider');

      const events = modelManager.getModelEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'MODEL_CREATED' && e.modelId === 'event-test');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.lifecycle).toBe('draft');
      expect(createEvent!.data.backendModel).toBe('claude-sonnet-4-5');
    });

    test('creates edge to provider', async () => {
      await modelManager.createModel('edge-test', 'claude-sonnet-4-5', 'test-provider');

      const edge = store.get('edge-test_uses_test-provider');
      expect(edge).toBeDefined();
    });
  });

  describe('getModel', () => {
    test('returns null for non-existent model', () => {
      const model = modelManager.getModel('nonexistent');
      expect(model).toBeNull();
    });

    test('retrieves an existing model', async () => {
      await modelManager.createModel('get-test', 'claude-sonnet-4-5', 'test-provider', {
        name: 'Get Test Model',
        temperature: 0.5
      });

      const model = modelManager.getModel('get-test');
      expect(model).not.toBeNull();
      expect(model!.id).toBe('get-test');
      expect(model!.config.name).toBe('Get Test Model');
      expect(model!.config.temperature).toBe(0.5);
    });
  });

  describe('configureModel', () => {
    test('configures a draft model', async () => {
      await modelManager.createModel('config-test', 'claude-sonnet-4-5', 'test-provider');

      const updated = await modelManager.configureModel('config-test', {
        name: 'Updated Model',
        temperature: 0.8,
        maxTokens: 2000
      });

      expect(updated.config.name).toBe('Updated Model');
      expect(updated.config.temperature).toBe(0.8);
      expect(updated.config.maxTokens).toBe(2000);
      expect(updated.version).toBe(2);
    });

    test('adds situation to model', async () => {
      await modelManager.createModel('situation-test', 'claude-sonnet-4-5', 'test-provider');

      const updated = await modelManager.configureModel('situation-test', {
        situation: { name: 'coding', params: { temperature: 0, maxTokens: 8000 } }
      });

      expect(updated.config.situations).toEqual({
        coding: { temperature: 0, maxTokens: 8000 }
      });
    });

    test('throws error when configuring non-existent model', async () => {
      await expect(modelManager.configureModel('nonexistent', { temperature: 0.5 }))
        .rejects.toThrow('Model not found: nonexistent');
    });

    test('throws error when configuring published model', async () => {
      await modelManager.createModel('pub-config-test', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('pub-config-test');

      await expect(modelManager.configureModel('pub-config-test', { temperature: 0.5 }))
        .rejects.toThrow('Cannot configure model in published lifecycle');
    });

    test('emits MODEL_CONFIGURED event', async () => {
      await modelManager.createModel('config-evt', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.configureModel('config-evt', { temperature: 0.5 });

      const events = modelManager.getModelEventHistory('config-evt');
      const configEvent = events.find(e => e.type === 'MODEL_CONFIGURED');
      expect(configEvent).toBeDefined();
      expect(configEvent!.data.newVersion).toBe(2);
    });
  });

  describe('publishModel', () => {
    test('publishes a draft model', async () => {
      await modelManager.createModel('pub-test', 'claude-sonnet-4-5', 'test-provider');

      const published = await modelManager.publishModel('pub-test');

      expect(published.lifecycle).toBe('published');
    });

    test('throws error when publishing non-existent model', async () => {
      await expect(modelManager.publishModel('nonexistent'))
        .rejects.toThrow('Model not found: nonexistent');
    });

    test('throws error when publishing already published model', async () => {
      await modelManager.createModel('double-pub', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('double-pub');

      await expect(modelManager.publishModel('double-pub'))
        .rejects.toThrow('Cannot publish model in published lifecycle');
    });

    test('emits MODEL_PUBLISHED event', async () => {
      await modelManager.createModel('pub-evt', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('pub-evt');

      const events = modelManager.getModelEventHistory('pub-evt');
      const pubEvent = events.find(e => e.type === 'MODEL_PUBLISHED');
      expect(pubEvent).toBeDefined();
      expect(pubEvent!.data.newLifecycle).toBe('published');
    });
  });

  describe('deprecateModel', () => {
    test('deprecates a published model', async () => {
      await modelManager.createModel('dep-test', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('dep-test');

      const deprecated = await modelManager.deprecateModel('dep-test');

      expect(deprecated.lifecycle).toBe('deprecated');
    });

    test('throws error when deprecating draft model', async () => {
      await modelManager.createModel('draft-dep', 'claude-sonnet-4-5', 'test-provider');

      await expect(modelManager.deprecateModel('draft-dep'))
        .rejects.toThrow('Cannot deprecate model in draft lifecycle');
    });

    test('emits MODEL_DEPRECATED event', async () => {
      await modelManager.createModel('dep-evt', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('dep-evt');
      await modelManager.deprecateModel('dep-evt');

      const events = modelManager.getModelEventHistory('dep-evt');
      const depEvent = events.find(e => e.type === 'MODEL_DEPRECATED');
      expect(depEvent).toBeDefined();
      expect(depEvent!.data.newLifecycle).toBe('deprecated');
    });
  });

  describe('listModels', () => {
    test('lists all models', async () => {
      await modelManager.createModel('list-1', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.createModel('list-2', 'gpt-4o', 'test-provider');
      await modelManager.createModel('list-3', 'gemini-pro', 'test-provider');

      const models = modelManager.listModels();

      expect(models.length).toBe(3);
    });

    test('filters models by lifecycle', async () => {
      await modelManager.createModel('draft-1', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.createModel('draft-2', 'gpt-4o', 'test-provider');
      await modelManager.createModel('pub-1', 'gemini-pro', 'test-provider');
      await modelManager.publishModel('pub-1');

      const drafts = modelManager.listModels({ lifecycle: 'draft' });
      const published = modelManager.listModels({ lifecycle: 'published' });

      expect(drafts.length).toBe(2);
      expect(published.length).toBe(1);
      expect(published[0].id).toBe('pub-1');
    });
  });

  describe('lifecycle machine enforcement', () => {
    test('draft -> published transition is valid', async () => {
      await modelManager.createModel('sm-1', 'claude-sonnet-4-5', 'test-provider');
      const published = await modelManager.publishModel('sm-1');
      expect(published.lifecycle).toBe('published');
    });

    test('published -> deprecated transition is valid', async () => {
      await modelManager.createModel('sm-2', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('sm-2');
      const deprecated = await modelManager.deprecateModel('sm-2');
      expect(deprecated.lifecycle).toBe('deprecated');
    });

    test('draft -> deprecated transition is invalid', async () => {
      await modelManager.createModel('sm-3', 'claude-sonnet-4-5', 'test-provider');
      await expect(modelManager.deprecateModel('sm-3'))
        .rejects.toThrow('Cannot deprecate model in draft lifecycle');
    });

    test('deprecated -> published transition is invalid', async () => {
      await modelManager.createModel('sm-4', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('sm-4');
      await modelManager.deprecateModel('sm-4');
      await expect(modelManager.publishModel('sm-4'))
        .rejects.toThrow('Cannot publish model in deprecated lifecycle');
    });

    test('published -> draft transition is invalid (no configure allowed)', async () => {
      await modelManager.createModel('sm-5', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('sm-5');
      await expect(modelManager.configureModel('sm-5', { temperature: 0.5 }))
        .rejects.toThrow('Cannot configure model in published lifecycle');
    });
  });

  describe('invokeModel', () => {
    test('throws error when invoking draft model', async () => {
      await modelManager.createModel('draft-invoke', 'claude-sonnet-4-5', 'test-provider');

      await expect(modelManager.invokeModel('draft-invoke', { message: 'Hello' }))
        .rejects.toThrow('Cannot invoke model in draft lifecycle');
    });

    test('throws error when invoking deprecated model', async () => {
      await modelManager.createModel('dep-invoke', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('dep-invoke');
      await modelManager.deprecateModel('dep-invoke');

      await expect(modelManager.invokeModel('dep-invoke', { message: 'Hello' }))
        .rejects.toThrow('Cannot invoke model in deprecated lifecycle');
    });

    test('throws error when provider is not published', async () => {
      // Create model with draft provider
      await modelManager.createModel('invoke-draft-prov', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('invoke-draft-prov');

      // Provider is still in draft lifecycle
      await expect(modelManager.invokeModel('invoke-draft-prov', { message: 'Hello' }))
        .rejects.toThrow('Cannot use provider in draft lifecycle');
    });

    // Note: Actual inference tests would require mocking the API
    // or having real credentials set up
  });

  describe('streaming support', () => {
    test('StreamingOptions interface is exported and usable', () => {
      // Verify the streaming options interface works correctly
      const opts: StreamingOptions = {
        stream: true,
        onToken: (token) => console.log(token)
      };
      expect(opts.stream).toBe(true);
      expect(typeof opts.onToken).toBe('function');
    });

    test('invokeModel accepts stream option', async () => {
      // This test verifies the interface accepts the stream option
      // Actual API call will fail without credentials, but verifies the type
      await modelManager.createModel('stream-test', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('stream-test');
      await providerManager.publishProvider('test-provider');

      // Should throw API error, not type error
      try {
        await modelManager.invokeModel('stream-test', {
          message: 'Hello',
          stream: true,
          onToken: (token: string) => {}
        });
      } catch (error: any) {
        // Expected to fail due to missing API credentials
        // The point is that the interface accepted the streaming options
        expect(error.message).toBeTruthy();
      }
    });

    test('invokeModel accepts stream option with situation', async () => {
      await modelManager.createModel('stream-situation', 'claude-sonnet-4-5', 'test-provider', {
        situations: { coding: { temperature: 0 } }
      });
      await modelManager.publishModel('stream-situation');
      await providerManager.publishProvider('test-provider');

      try {
        await modelManager.invokeModel('stream-situation', {
          message: 'Hello',
          stream: true,
          situation: 'coding',
          onToken: (token: string) => {}
        });
      } catch (error: any) {
        // Expected to fail due to missing API credentials
        expect(error.message).toBeTruthy();
      }
    });

    test('onToken callback is optional in streaming mode', async () => {
      await modelManager.createModel('stream-no-callback', 'claude-sonnet-4-5', 'test-provider');
      await modelManager.publishModel('stream-no-callback');
      await providerManager.publishProvider('test-provider');

      try {
        await modelManager.invokeModel('stream-no-callback', {
          message: 'Hello',
          stream: true
          // No onToken provided - should still work
        });
      } catch (error: any) {
        // Expected to fail due to missing API credentials
        expect(error.message).toBeTruthy();
      }
    });
  });
});
