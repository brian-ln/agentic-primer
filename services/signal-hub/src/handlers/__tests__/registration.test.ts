/**
 * Registration Handler Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  handleRegister,
  handleUnregister,
  handleDiscover,
  handleListActors,
} from '../registration';
import type { SharedMessage, ActorRegistration, Env } from '../../types';
import { toCanonicalAddress } from '../../utils';

describe('Registration Handlers', () => {
  let mockEnv: Env;
  let registry: Map<string, ActorRegistration>;

  beforeEach(() => {
    mockEnv = {
      SIGNAL_HUB: {} as DurableObjectNamespace,
      PROTOCOL_VERSION: '0.1.0',
      MAX_MESSAGE_SIZE: '1048576',
      HEARTBEAT_INTERVAL: '30000',
      ACTOR_REGISTRY_LIMIT: '50000',
      DEFAULT_ACTOR_TTL: '300000',
      MAX_ACTOR_TTL: '3600000',
      BROADCAST_SYNC_THRESHOLD: '100',
      JWT_SECRET: 'test-secret',
      AUTH_ENABLED: 'false',
    };

    registry = new Map();
  });

  describe('handleRegister', () => {
    it('should register a new actor', () => {
      // @spec: registration/REGISTRATION.spec.md#L28-L41
      // @requirement: Registration flow stores actor in registry
      // @requirement: Generate renewal token on registration
      // @requirement: Response includes actorAddress, renewalToken, and version
      const registerMsg: SharedMessage = {
        id: 'msg-1',
        from: toCanonicalAddress('browser/widget-123'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          actorAddress: toCanonicalAddress('browser/widget-123'),
          capabilities: ['render', 'handle-click'],
          metadata: { widgetType: 'chart' },
          ttlSeconds: 300,
        },
        metadata: { renewOnHeartbeat: true },
        ttl: 5000,
        signature: null,
      };

      const response = handleRegister(registerMsg, registry, 'conn-1', mockEnv);

      expect(response.type).toBe('hub:registered');
      expect(response.payload).toHaveProperty('actorAddress', toCanonicalAddress('browser/widget-123'));
      expect(response.payload).toHaveProperty('renewalToken');
      expect(response.payload).toHaveProperty('version', 1);
      expect(registry.size).toBe(1);
    });

    it('should increment version on re-registration', () => {
      // @spec: registration/REGISTRATION.spec.md#L222-L240
      // @requirement: Duplicate registration updates existing entry
      // @requirement: Version increments on each re-registration
      const address = toCanonicalAddress('browser/widget-123');

      // First registration
      const registerMsg1: SharedMessage = {
        id: 'msg-1',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          actorAddress: address,
          capabilities: ['render'],
          metadata: {},
          ttlSeconds: 300,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response1 = handleRegister(registerMsg1, registry, 'conn-1', mockEnv);
      expect(response1.payload).toHaveProperty('version', 1);

      // Second registration (same actor)
      const registerMsg2: SharedMessage = {
        ...registerMsg1,
        id: 'msg-2',
        payload: {
          ...(registerMsg1.payload as Record<string, unknown>),
          capabilities: ['render', 'compute'],
        },
      };

      const response2 = handleRegister(registerMsg2, registry, 'conn-2', mockEnv);
      expect(response2.payload).toHaveProperty('version', 2);
      expect(registry.size).toBe(1); // Still one actor
    });
  });

  describe('handleUnregister', () => {
    it('should unregister an actor', () => {
      const address = toCanonicalAddress('browser/widget-123');

      // Register first
      registry.set(address, {
        actorAddress: address,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'token-1',
      });

      const unregisterMsg: SharedMessage = {
        id: 'msg-1',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unregister',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { actorAddress: address },
        metadata: {},
        ttl: null,
        signature: null,
      };

      handleUnregister(unregisterMsg, registry, mockEnv);

      expect(registry.size).toBe(0);
    });
  });

  describe('handleDiscover', () => {
    beforeEach(() => {
      // Populate registry
      registry.set(toCanonicalAddress('browser/widget-123'), {
        actorAddress: toCanonicalAddress('browser/widget-123'),
        capabilities: ['render'],
        metadata: { type: 'chart' },
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'token-1',
      });

      registry.set(toCanonicalAddress('browser/widget-456'), {
        actorAddress: toCanonicalAddress('browser/widget-456'),
        capabilities: ['render'],
        metadata: { type: 'table' },
        connectionId: 'conn-2',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'token-2',
      });

      registry.set(toCanonicalAddress('local/coordinator'), {
        actorAddress: toCanonicalAddress('local/coordinator'),
        capabilities: ['orchestrate'],
        metadata: {},
        connectionId: 'conn-3',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'token-3',
      });
    });

    it('should discover actors matching pattern', () => {
      const discoverMsg: SharedMessage = {
        id: 'msg-1',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          pattern: '@(browser/widget-*)',
          limit: 50,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleDiscover(discoverMsg, registry, mockEnv);

      expect(response.type).toBe('hub:discovered');
      expect(response.payload).toHaveProperty('actors');
      expect((response.payload as any).actors).toHaveLength(2);
      expect((response.payload as any).hasMore).toBe(false);
    });

    it('should respect limit', () => {
      const discoverMsg: SharedMessage = {
        id: 'msg-2',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          pattern: '@(browser/widget-*)',
          limit: 1,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleDiscover(discoverMsg, registry, mockEnv);

      expect((response.payload as any).actors).toHaveLength(1);
      expect((response.payload as any).hasMore).toBe(true);
    });
  });

  describe('handleListActors', () => {
    beforeEach(() => {
      // Populate registry with multiple actors
      for (let i = 0; i < 10; i++) {
        registry.set(toCanonicalAddress(`browser/actor-${i}`), {
          actorAddress: toCanonicalAddress(`browser/actor-${i}`),
          capabilities: ['test'],
          metadata: {},
          connectionId: `conn-${i}`,
          registeredAt: Date.now(),
          expiresAt: Date.now() + 300000,
          version: 1,
          renewalToken: `token-${i}`,
        });
      }
    });

    it('should list all actors', () => {
      const listMsg: SharedMessage = {
        id: 'msg-1',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:list_actors',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleListActors(listMsg, registry);

      expect(response.type).toBe('hub:actor_list');
      expect((response.payload as any).total).toBe(10);
      expect((response.payload as any).actors).toHaveLength(10);
      expect((response.payload as any).hasMore).toBe(false);
    });

    it('should paginate results', () => {
      const listMsg: SharedMessage = {
        id: 'msg-2',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:list_actors',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          offset: 5,
          limit: 3,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleListActors(listMsg, registry);

      expect((response.payload as any).total).toBe(10);
      expect((response.payload as any).actors).toHaveLength(3);
      expect((response.payload as any).offset).toBe(5);
      expect((response.payload as any).limit).toBe(3);
      expect((response.payload as any).hasMore).toBe(true);
    });
  });
});
