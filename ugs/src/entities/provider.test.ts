import { test, expect, describe, beforeEach } from 'bun:test';
import { ProviderManager, Provider, ProviderLifecycle } from './provider.ts';
import GraphStore from '../graph.ts';

// Counter to ensure unique data directories per test
let testCounter = 0;

describe('ProviderManager', () => {
  let store: GraphStore;
  let manager: ProviderManager;

  beforeEach(async () => {
    // Create a fresh in-memory store for each test with unique path
    testCounter++;
    store = new GraphStore(`/tmp/ugs-provider-test-${Date.now()}-${testCounter}-${Math.random().toString(36).slice(2)}`);
    await store.initialize();
    manager = new ProviderManager(store);
  });

  describe('createProvider', () => {
    test('creates a provider in draft lifecycle', async () => {
      const provider = await manager.createProvider('test-provider', 'cloudflare-ai-gateway');

      expect(provider.id).toBe('test-provider');
      expect(provider.config.providerType).toBe('cloudflare-ai-gateway');
      expect(provider.lifecycle).toBe('draft');
      expect(provider.version).toBe(1);
      expect(provider.type).toBe('program');
      expect(provider.programType).toBe('provider');
    });

    test('creates a provider with custom options', async () => {
      const provider = await manager.createProvider('cf-gateway', 'cloudflare-ai-gateway', {
        accountId: 'my-account-123',
        gatewayId: 'my-gateway-456'
      });

      expect(provider.config.accountId).toBe('my-account-123');
      expect(provider.config.gatewayId).toBe('my-gateway-456');
    });

    test('throws error if provider already exists', async () => {
      await manager.createProvider('dup-provider', 'cloudflare-ai-gateway');

      await expect(manager.createProvider('dup-provider', 'cloudflare-ai-gateway'))
        .rejects.toThrow('Provider already exists: dup-provider');
    });

    test('emits PROVIDER_CREATED event', async () => {
      await manager.createProvider('event-test', 'cloudflare-ai-gateway');

      const events = manager.getProviderEvents();
      expect(events.length).toBeGreaterThanOrEqual(1);

      const createEvent = events.find(e => e.type === 'PROVIDER_CREATED' && e.providerId === 'event-test');
      expect(createEvent).toBeDefined();
      expect(createEvent!.data.lifecycle).toBe('draft');
    });
  });

  describe('getProvider', () => {
    test('returns null for non-existent provider', () => {
      const provider = manager.getProvider('nonexistent');
      expect(provider).toBeNull();
    });

    test('retrieves an existing provider', async () => {
      await manager.createProvider('get-test', 'cloudflare-ai-gateway', {
        accountId: 'test-account'
      });

      const provider = manager.getProvider('get-test');
      expect(provider).not.toBeNull();
      expect(provider!.id).toBe('get-test');
      expect(provider!.config.accountId).toBe('test-account');
    });
  });

  describe('configureProvider', () => {
    test('configures a draft provider', async () => {
      await manager.createProvider('config-test', 'cloudflare-ai-gateway');

      const updated = await manager.configureProvider('config-test', {
        accountId: 'new-account',
        gatewayId: 'new-gateway'
      });

      expect(updated.config.accountId).toBe('new-account');
      expect(updated.config.gatewayId).toBe('new-gateway');
      expect(updated.version).toBe(2);
    });

    test('throws error when configuring non-existent provider', async () => {
      await expect(manager.configureProvider('nonexistent', { accountId: 'test' }))
        .rejects.toThrow('Provider not found: nonexistent');
    });

    test('throws error when configuring published provider', async () => {
      await manager.createProvider('pub-config-test', 'cloudflare-ai-gateway');
      await manager.publishProvider('pub-config-test');

      await expect(manager.configureProvider('pub-config-test', { accountId: 'test' }))
        .rejects.toThrow('Cannot configure provider in published lifecycle');
    });

    test('emits PROVIDER_CONFIGURED event', async () => {
      await manager.createProvider('config-evt', 'cloudflare-ai-gateway');
      await manager.configureProvider('config-evt', { accountId: 'test' });

      const events = manager.getProviderEventHistory('config-evt');
      const configEvent = events.find(e => e.type === 'PROVIDER_CONFIGURED');
      expect(configEvent).toBeDefined();
      expect(configEvent!.data.newVersion).toBe(2);
    });
  });

  describe('publishProvider', () => {
    test('publishes a draft provider', async () => {
      await manager.createProvider('pub-test', 'cloudflare-ai-gateway');

      const published = await manager.publishProvider('pub-test');

      expect(published.lifecycle).toBe('published');
    });

    test('throws error when publishing non-existent provider', async () => {
      await expect(manager.publishProvider('nonexistent'))
        .rejects.toThrow('Provider not found: nonexistent');
    });

    test('throws error when publishing already published provider', async () => {
      await manager.createProvider('double-pub', 'cloudflare-ai-gateway');
      await manager.publishProvider('double-pub');

      await expect(manager.publishProvider('double-pub'))
        .rejects.toThrow('Cannot publish provider in published lifecycle');
    });

    test('emits PROVIDER_PUBLISHED event', async () => {
      await manager.createProvider('pub-evt', 'cloudflare-ai-gateway');
      await manager.publishProvider('pub-evt');

      const events = manager.getProviderEventHistory('pub-evt');
      const pubEvent = events.find(e => e.type === 'PROVIDER_PUBLISHED');
      expect(pubEvent).toBeDefined();
      expect(pubEvent!.data.newLifecycle).toBe('published');
    });
  });

  describe('deprecateProvider', () => {
    test('deprecates a published provider', async () => {
      await manager.createProvider('dep-test', 'cloudflare-ai-gateway');
      await manager.publishProvider('dep-test');

      const deprecated = await manager.deprecateProvider('dep-test');

      expect(deprecated.lifecycle).toBe('deprecated');
    });

    test('throws error when deprecating draft provider', async () => {
      await manager.createProvider('draft-dep', 'cloudflare-ai-gateway');

      await expect(manager.deprecateProvider('draft-dep'))
        .rejects.toThrow('Cannot deprecate provider in draft lifecycle');
    });

    test('emits PROVIDER_DEPRECATED event', async () => {
      await manager.createProvider('dep-evt', 'cloudflare-ai-gateway');
      await manager.publishProvider('dep-evt');
      await manager.deprecateProvider('dep-evt');

      const events = manager.getProviderEventHistory('dep-evt');
      const depEvent = events.find(e => e.type === 'PROVIDER_DEPRECATED');
      expect(depEvent).toBeDefined();
      expect(depEvent!.data.newLifecycle).toBe('deprecated');
    });
  });

  describe('listProviders', () => {
    test('lists all providers', async () => {
      await manager.createProvider('list-1', 'cloudflare-ai-gateway');
      await manager.createProvider('list-2', 'cloudflare-ai-gateway');
      await manager.createProvider('list-3', 'cloudflare-ai-gateway');

      const providers = manager.listProviders();

      expect(providers.length).toBe(3);
    });

    test('filters providers by lifecycle', async () => {
      await manager.createProvider('draft-1', 'cloudflare-ai-gateway');
      await manager.createProvider('draft-2', 'cloudflare-ai-gateway');
      await manager.createProvider('pub-1', 'cloudflare-ai-gateway');
      await manager.publishProvider('pub-1');

      const drafts = manager.listProviders('draft');
      const published = manager.listProviders('published');

      expect(drafts.length).toBe(2);
      expect(published.length).toBe(1);
      expect(published[0].id).toBe('pub-1');
    });
  });

  describe('lifecycle machine enforcement', () => {
    test('draft -> published transition is valid', async () => {
      await manager.createProvider('sm-1', 'cloudflare-ai-gateway');
      const published = await manager.publishProvider('sm-1');
      expect(published.lifecycle).toBe('published');
    });

    test('published -> deprecated transition is valid', async () => {
      await manager.createProvider('sm-2', 'cloudflare-ai-gateway');
      await manager.publishProvider('sm-2');
      const deprecated = await manager.deprecateProvider('sm-2');
      expect(deprecated.lifecycle).toBe('deprecated');
    });

    test('draft -> deprecated transition is invalid', async () => {
      await manager.createProvider('sm-3', 'cloudflare-ai-gateway');
      await expect(manager.deprecateProvider('sm-3'))
        .rejects.toThrow('Cannot deprecate provider in draft lifecycle');
    });

    test('deprecated -> published transition is invalid', async () => {
      await manager.createProvider('sm-4', 'cloudflare-ai-gateway');
      await manager.publishProvider('sm-4');
      await manager.deprecateProvider('sm-4');
      await expect(manager.publishProvider('sm-4'))
        .rejects.toThrow('Cannot publish provider in deprecated lifecycle');
    });
  });

  describe('getProviderBaseUrl', () => {
    test('returns correct Cloudflare AI Gateway URL', async () => {
      const provider = await manager.createProvider('url-test', 'cloudflare-ai-gateway', {
        accountId: 'my-account',
        gatewayId: 'my-gateway'
      });

      const baseUrl = manager.getProviderBaseUrl(provider);

      expect(baseUrl).toBe('https://gateway.ai.cloudflare.com/v1/my-account/my-gateway');
    });
  });
});
