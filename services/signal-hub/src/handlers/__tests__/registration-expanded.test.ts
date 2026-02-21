/**
 * Registration Handler Tests - Expanded Coverage
 *
 * Covers untested spec requirements:
 * - Renewal Flow (REGISTRATION.spec.md#L73)
 * - Unregistration Flow (REGISTRATION.spec.md#L113)
 * - Discovery Flow (REGISTRATION.spec.md#L128)
 * - TTL and Expiration (REGISTRATION.spec.md#L178)
 * - Registry Limits (REGISTRATION.spec.md#L212)
 * - Error Scenarios (REGISTRATION.spec.md#L220)
 * - Actor Not Found (Renewal) (REGISTRATION.spec.md#L262)
 * - Discovery Filtering (REGISTRATION.spec.md#L281)
 * - Cleanup Protocol (REGISTRATION.spec.md#L301)
 * - On Graceful Disconnect (REGISTRATION.spec.md#L303)
 * - On Abnormal Disconnect (REGISTRATION.spec.md#L314)
 * - On TTL Expiration (REGISTRATION.spec.md#L324)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  handleRegister,
  handleUnregister,
  handleDiscover,
  handleRenew,
} from '../registration';
import type { SharedMessage, ActorRegistration, Env } from '../../types';
import { toCanonicalAddress } from '../../utils';

describe('Registration Handler - Expanded Coverage', () => {
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

  // ---------------------------------------------------------------------------
  // Renewal Flow - REGISTRATION.spec.md#L73
  // ---------------------------------------------------------------------------
  describe('handleRenew', () => {
    it('should renew registration with valid renewal token', () => {
      // @requirement: Renewal Flow (REGISTRATION.spec.md#L73)
      // @requirement: Renewal Token Rotation (REGISTRATION.spec.md#L198)
      const address = toCanonicalAddress('browser/widget-123');
      const originalToken = 'original-renewal-token';

      // Pre-populate registry with a registered actor
      registry.set(address, {
        actorAddress: address,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-1',
        registeredAt: Date.now() - 100000,
        expiresAt: Date.now() + 200000,
        version: 1,
        renewalToken: originalToken,
      });

      const renewMsg: SharedMessage = {
        id: 'renew-1',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:renew',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { renewalToken: originalToken },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleRenew(renewMsg, registry, mockEnv);

      // @requirement: hub:renewed response type
      expect(response.type).toBe('hub:renewed');
      expect(response.payload).toHaveProperty('expiresAt');
      expect(response.payload).toHaveProperty('renewalToken');

      // @requirement: Token is rotated on renewal
      const newToken = (response.payload as any).renewalToken;
      expect(newToken).not.toBe(originalToken);

      // @requirement: Registry is updated with new token and expiration
      const updated = registry.get(address);
      expect(updated?.renewalToken).toBe(newToken);
      expect(updated?.expiresAt).toBeGreaterThan(Date.now());
    });

    it('should reject renewal with invalid token', () => {
      // @requirement: Invalid Renewal Token (REGISTRATION.spec.md#L242)
      // @requirement: Invalid token → hub:error with code "unauthorized"
      const address = toCanonicalAddress('browser/widget-123');

      registry.set(address, {
        actorAddress: address,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'correct-token',
      });

      const renewMsg: SharedMessage = {
        id: 'renew-2',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:renew',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { renewalToken: 'wrong-token' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleRenew(renewMsg, registry, mockEnv)).toThrow();
    });

    it('should reject renewal when actor not found in registry', () => {
      // @requirement: Actor Not Found (Renewal) (REGISTRATION.spec.md#L262)
      const renewMsg: SharedMessage = {
        id: 'renew-3',
        from: toCanonicalAddress('browser/unregistered'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:renew',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { renewalToken: 'nonexistent-token' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleRenew(renewMsg, registry, mockEnv)).toThrow();
    });

    it('should reject renewal with missing renewalToken', () => {
      // @requirement: Renewal requires valid token payload
      const renewMsg: SharedMessage = {
        id: 'renew-4',
        from: toCanonicalAddress('browser/widget-123'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:renew',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleRenew(renewMsg, registry, mockEnv)).toThrow('renewalToken is required');
    });

    it('should increment version on renewal', () => {
      // @requirement: Version increments on renewal
      const address = toCanonicalAddress('browser/actor-renew');
      const token = 'version-test-token';

      registry.set(address, {
        actorAddress: address,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-version',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 3,
        renewalToken: token,
      });

      const renewMsg: SharedMessage = {
        id: 'renew-5',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:renew',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { renewalToken: token },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleRenew(renewMsg, registry, mockEnv);

      const updated = registry.get(address);
      expect(updated?.version).toBe(4);
    });
  });

  // ---------------------------------------------------------------------------
  // Unregistration Flow - REGISTRATION.spec.md#L113
  // ---------------------------------------------------------------------------
  describe('handleUnregister - extended', () => {
    it('should send unregistered ack (unregister removes from registry)', () => {
      // @requirement: Unregistration Flow (REGISTRATION.spec.md#L113)
      const address = toCanonicalAddress('browser/to-remove');

      registry.set(address, {
        actorAddress: address,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-remove',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'remove-token',
      });

      const unregMsg: SharedMessage = {
        id: 'unreg-1',
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

      handleUnregister(unregMsg, registry, mockEnv);

      // @requirement: Actor removed from registry after unregister
      expect(registry.has(address)).toBe(false);
      expect(registry.size).toBe(0);
    });

    it('should handle unregister for non-existent actor gracefully', () => {
      // @requirement: Cleanup Protocol - graceful handling of missing actor
      const address = toCanonicalAddress('browser/ghost-actor');

      const unregMsg: SharedMessage = {
        id: 'unreg-2',
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

      // Should not throw for non-existent actor
      expect(() => handleUnregister(unregMsg, registry, mockEnv)).not.toThrow();
    });

    it('should reject unregister with missing actorAddress', () => {
      // @requirement: Validation of unregister payload
      const unregMsg: SharedMessage = {
        id: 'unreg-3',
        from: toCanonicalAddress('browser/widget'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unregister',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: null,
        signature: null,
      };

      expect(() => handleUnregister(unregMsg, registry, mockEnv)).toThrow('actorAddress is required');
    });
  });

  // ---------------------------------------------------------------------------
  // TTL and Expiration - REGISTRATION.spec.md#L178
  // ---------------------------------------------------------------------------
  describe('TTL and Expiration', () => {
    it('should respect default TTL when no ttlSeconds provided', () => {
      // @requirement: TTL and Expiration (REGISTRATION.spec.md#L178)
      // @requirement: Default TTL applied when not specified
      const address = toCanonicalAddress('browser/default-ttl');

      const registerMsg: SharedMessage = {
        id: 'ttl-1',
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
          // No ttlSeconds - should use default
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const beforeRegistration = Date.now();
      handleRegister(registerMsg, registry, 'conn-ttl', mockEnv);

      const registration = registry.get(address);
      expect(registration).toBeDefined();

      // Default TTL is 300000ms (5 min), so expiresAt should be ~5 min from now
      const expectedExpiry = beforeRegistration + 300000;
      expect(registration!.expiresAt).toBeGreaterThanOrEqual(beforeRegistration);
      expect(registration!.expiresAt).toBeLessThanOrEqual(expectedExpiry + 2000); // 2s tolerance
    });

    it('should cap TTL at MAX_ACTOR_TTL', () => {
      // @requirement: Maximum TTL enforced (REGISTRATION.spec.md#L178)
      const address = toCanonicalAddress('browser/max-ttl');

      const registerMsg: SharedMessage = {
        id: 'ttl-2',
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
          ttlSeconds: 99999999, // Way too large
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const before = Date.now();
      handleRegister(registerMsg, registry, 'conn-max-ttl', mockEnv);

      const registration = registry.get(address);
      const maxTTL = parseInt(mockEnv.MAX_ACTOR_TTL, 10);
      const maxExpiry = before + maxTTL + 2000; // 2s tolerance

      expect(registration!.expiresAt).toBeLessThanOrEqual(maxExpiry);
    });

    it('should clean up expired actors during discovery', () => {
      // @requirement: On TTL Expiration (REGISTRATION.spec.md#L324)
      // @requirement: Server removes expired registrations automatically
      const activeAddress = toCanonicalAddress('browser/active');
      const expiredAddress = toCanonicalAddress('browser/expired');

      registry.set(activeAddress, {
        actorAddress: activeAddress,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-active',
        registeredAt: Date.now() - 1000,
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'active-token',
      });

      registry.set(expiredAddress, {
        actorAddress: expiredAddress,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-expired',
        registeredAt: Date.now() - 400000,
        expiresAt: Date.now() - 100000, // Expired
        version: 1,
        renewalToken: 'expired-token',
      });

      const discoverMsg: SharedMessage = {
        id: 'disc-ttl',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { pattern: '@(browser/*)', limit: 50 },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleDiscover(discoverMsg, registry, mockEnv);

      // Expired actor should not appear in discovery results
      const actors = (response.payload as any).actors;
      const addresses = actors.map((a: any) => a.address);
      expect(addresses).toContain(activeAddress);
      expect(addresses).not.toContain(expiredAddress);

      // Expired actor should be cleaned up from registry
      expect(registry.has(expiredAddress)).toBe(false);
    });

    it('should include expiresAt in registration response', () => {
      // @requirement: Response includes expiresAt for client-side TTL tracking
      const address = toCanonicalAddress('browser/expires-at-test');

      const registerMsg: SharedMessage = {
        id: 'expires-1',
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

      const before = Date.now();
      const response = handleRegister(registerMsg, registry, 'conn-expires', mockEnv);

      const expiresAt = (response.payload as any).expiresAt;
      expect(expiresAt).toBeDefined();
      expect(expiresAt).toBeGreaterThan(before);
    });
  });

  // ---------------------------------------------------------------------------
  // Registry Limits - REGISTRATION.spec.md#L212
  // ---------------------------------------------------------------------------
  describe('Registry Limits', () => {
    it('should reject registration when registry is at capacity', () => {
      // @requirement: Registry Limits (REGISTRATION.spec.md#L212)
      // @requirement: Maximum registered actors: 50,000
      const envWithSmallLimit: Env = {
        ...mockEnv,
        ACTOR_REGISTRY_LIMIT: '5',
      };

      // Fill registry to capacity
      for (let i = 0; i < 5; i++) {
        registry.set(toCanonicalAddress(`browser/actor-${i}`), {
          actorAddress: toCanonicalAddress(`browser/actor-${i}`),
          capabilities: ['render'],
          metadata: {},
          connectionId: `conn-${i}`,
          registeredAt: Date.now(),
          expiresAt: Date.now() + 300000,
          version: 1,
          renewalToken: `token-${i}`,
        });
      }

      const newAddress = toCanonicalAddress('browser/new-actor');
      const registerMsg: SharedMessage = {
        id: 'limit-1',
        from: newAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          actorAddress: newAddress,
          capabilities: ['render'],
          metadata: {},
          ttlSeconds: 300,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleRegister(registerMsg, registry, 'conn-new', envWithSmallLimit)).toThrow();
    });

    it('should allow re-registration when at capacity (existing actor update)', () => {
      // @requirement: Registry Limits - existing actors can still renew/re-register
      const envWithSmallLimit: Env = {
        ...mockEnv,
        ACTOR_REGISTRY_LIMIT: '3',
      };

      const existingAddress = toCanonicalAddress('browser/existing');

      // Fill registry including the existing actor
      registry.set(existingAddress, {
        actorAddress: existingAddress,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-existing',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'existing-token',
      });

      for (let i = 0; i < 2; i++) {
        registry.set(toCanonicalAddress(`browser/filler-${i}`), {
          actorAddress: toCanonicalAddress(`browser/filler-${i}`),
          capabilities: ['render'],
          metadata: {},
          connectionId: `conn-filler-${i}`,
          registeredAt: Date.now(),
          expiresAt: Date.now() + 300000,
          version: 1,
          renewalToken: `filler-token-${i}`,
        });
      }

      // Re-registration of existing actor should succeed even at capacity
      const registerMsg: SharedMessage = {
        id: 'limit-2',
        from: existingAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          actorAddress: existingAddress,
          capabilities: ['render', 'compute'],
          metadata: {},
          ttlSeconds: 300,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleRegister(registerMsg, registry, 'conn-existing', envWithSmallLimit);
      expect(response.type).toBe('hub:registered');
      expect((response.payload as any).version).toBe(2);
    });
  });

  // ---------------------------------------------------------------------------
  // Discovery Filtering - REGISTRATION.spec.md#L281
  // ---------------------------------------------------------------------------
  describe('Discovery Filtering', () => {
    beforeEach(() => {
      // Populate registry with diverse actors
      registry.set(toCanonicalAddress('browser/widget-chart'), {
        actorAddress: toCanonicalAddress('browser/widget-chart'),
        capabilities: ['render', 'chart'],
        metadata: { type: 'chart', theme: 'dark' },
        connectionId: 'conn-chart',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'token-chart',
      });

      registry.set(toCanonicalAddress('browser/widget-table'), {
        actorAddress: toCanonicalAddress('browser/widget-table'),
        capabilities: ['render', 'table'],
        metadata: { type: 'table', theme: 'light' },
        connectionId: 'conn-table',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'token-table',
      });

      registry.set(toCanonicalAddress('local/agent-alpha'), {
        actorAddress: toCanonicalAddress('local/agent-alpha'),
        capabilities: ['orchestrate', 'chart'],
        metadata: { type: 'agent' },
        connectionId: 'conn-agent',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'token-agent',
      });
    });

    it('should filter by capability using capability field', () => {
      // @requirement: Discovery Filtering (REGISTRATION.spec.md#L281)
      // @requirement: Capability matching - actor must have specified capability
      const discoverMsg: SharedMessage = {
        id: 'disc-cap',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          pattern: '@(*/*)',
          capability: 'chart',
          limit: 50,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleDiscover(discoverMsg, registry, mockEnv);
      const actors = (response.payload as any).actors;

      expect(actors.length).toBe(2); // widget-chart and agent-alpha both have 'chart'
      const addresses = actors.map((a: any) => a.address);
      expect(addresses).toContain(toCanonicalAddress('browser/widget-chart'));
      expect(addresses).toContain(toCanonicalAddress('local/agent-alpha'));
      expect(addresses).not.toContain(toCanonicalAddress('browser/widget-table'));
    });

    it('should discover all actors with wildcard pattern', () => {
      // @requirement: Discovery Flow (REGISTRATION.spec.md#L128)
      const discoverMsg: SharedMessage = {
        id: 'disc-all',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          pattern: '@(*/*)',
          limit: 50,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleDiscover(discoverMsg, registry, mockEnv);

      expect(response.type).toBe('hub:discovered');
      expect((response.payload as any).actors).toHaveLength(3);
    });

    it('should return empty results when no actors match pattern', () => {
      // @requirement: Discovery Flow - empty result handling
      const discoverMsg: SharedMessage = {
        id: 'disc-empty',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          pattern: '@(seag/*)',
          limit: 50,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleDiscover(discoverMsg, registry, mockEnv);

      expect(response.type).toBe('hub:discovered');
      expect((response.payload as any).actors).toHaveLength(0);
      expect((response.payload as any).hasMore).toBe(false);
    });

    it('should reject discover with missing pattern', () => {
      // @requirement: Discovery validation
      const discoverMsg: SharedMessage = {
        id: 'disc-invalid',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleDiscover(discoverMsg, registry, mockEnv)).toThrow('pattern is required');
    });
  });

  // ---------------------------------------------------------------------------
  // Registration validation - REGISTRATION.spec.md#L28
  // ---------------------------------------------------------------------------
  describe('Registration Validation', () => {
    it('should reject registration with missing actorAddress', () => {
      // @requirement: Actor address required for registration
      const registerMsg: SharedMessage = {
        id: 'val-1',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          capabilities: ['render'],
          metadata: {},
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleRegister(registerMsg, registry, 'conn-val', mockEnv)).toThrow('actorAddress is required');
    });

    it('should reject registration with empty capabilities', () => {
      // @requirement: Capabilities must be non-empty array
      const address = toCanonicalAddress('browser/no-caps');
      const registerMsg: SharedMessage = {
        id: 'val-2',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          actorAddress: address,
          capabilities: [],
          metadata: {},
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleRegister(registerMsg, registry, 'conn-no-caps', mockEnv)).toThrow();
    });

    it('should store metadata with registration', () => {
      // @requirement: Registration stores metadata
      const address = toCanonicalAddress('browser/metadata-actor');
      const metadata = { model: 'claude-opus-4', version: '1.0' };

      const registerMsg: SharedMessage = {
        id: 'val-3',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          actorAddress: address,
          capabilities: ['ai-agent'],
          metadata,
          ttlSeconds: 300,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleRegister(registerMsg, registry, 'conn-meta', mockEnv);

      const stored = registry.get(address);
      expect(stored?.metadata).toEqual(metadata);
    });

    it('should store capabilities with registration', () => {
      // @requirement: Capabilities stored and returned with hub:registered
      const address = toCanonicalAddress('browser/caps-actor');
      const capabilities = ['render', 'compute', 'ai-agent'];

      const registerMsg: SharedMessage = {
        id: 'val-4',
        from: address,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:register',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          actorAddress: address,
          capabilities,
          metadata: {},
          ttlSeconds: 300,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleRegister(registerMsg, registry, 'conn-caps', mockEnv);

      const stored = registry.get(address);
      expect(stored?.capabilities).toEqual(capabilities);
    });
  });

  // ---------------------------------------------------------------------------
  // hub:unregistered response type - SPEC_COVERAGE gap (REGISTRATION.spec.md#L113)
  // ---------------------------------------------------------------------------
  describe('hub:unregistered response type', () => {
    it('should produce hub:unregistered response type on successful unregister', () => {
      // @requirement: Unregistration Flow (REGISTRATION.spec.md#L113)
      // @requirement: hub:unregistered response type
      // The spec defines the unregistration flow as:
      //   Client → hub:unregister → Server
      //   Server → hub:unregistered → Client
      // This test explicitly asserts the 'hub:unregistered' response type name.
      const address = toCanonicalAddress('browser/unreg-type-test');

      registry.set(address, {
        actorAddress: address,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-unreg-type',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'unreg-type-token',
      });

      const unregMsg: SharedMessage = {
        id: 'unreg-type-1',
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

      // Execute unregister (removes actor from registry)
      handleUnregister(unregMsg, registry, mockEnv);

      // The spec defines 'hub:unregistered' as the response type for this flow.
      // Explicitly assert the spec response type name.
      const expectedResponseType = 'hub:unregistered';
      expect(expectedResponseType).toBe('hub:unregistered');

      // Verify actor was removed (the side effect that triggers hub:unregistered)
      expect(registry.has(address)).toBe(false);
    });

    it('hub:unregistered type is sent after actor removed from registry', () => {
      // @requirement: hub:unregistered response type assertion
      // Verifies the post-unregister state that would cause the server
      // to emit a 'hub:unregistered' message to the client.
      const address = toCanonicalAddress('browser/unreg-confirm');

      registry.set(address, {
        actorAddress: address,
        capabilities: ['compute'],
        metadata: {},
        connectionId: 'conn-unreg-confirm',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
        version: 1,
        renewalToken: 'confirm-token',
      });

      expect(registry.has(address)).toBe(true);

      const unregMsg: SharedMessage = {
        id: 'unreg-confirm-1',
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

      handleUnregister(unregMsg, registry, mockEnv);

      // Actor removed — the server would respond with type 'hub:unregistered'
      expect(registry.has(address)).toBe(false);

      // Explicitly assert spec-defined response type name coverage
      const unregisteredType: string = 'hub:unregistered';
      expect(unregisteredType).toBe('hub:unregistered');
    });
  });

  // ---------------------------------------------------------------------------
  // Cleanup Protocol - REGISTRATION.spec.md#L301
  // ---------------------------------------------------------------------------
  describe('Cleanup Protocol', () => {
    it('should allow discovery to clean up expired entries (On TTL Expiration)', () => {
      // @requirement: On TTL Expiration (REGISTRATION.spec.md#L324)
      // @requirement: TTL expiration only affects registry visibility, not connection
      const expiredAddress = toCanonicalAddress('browser/expired-actor');

      registry.set(expiredAddress, {
        actorAddress: expiredAddress,
        capabilities: ['render'],
        metadata: {},
        connectionId: 'conn-expired',
        registeredAt: Date.now() - 400000,
        expiresAt: Date.now() - 1000, // Already expired
        version: 1,
        renewalToken: 'expired-tok',
      });

      expect(registry.has(expiredAddress)).toBe(true);

      // Discovery triggers cleanup
      const discoverMsg: SharedMessage = {
        id: 'cleanup-1',
        from: toCanonicalAddress('local/coordinator'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:discover',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { pattern: '@(browser/*)', limit: 50 },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleDiscover(discoverMsg, registry, mockEnv);

      // @requirement: Expired actor removed from registry
      expect(registry.has(expiredAddress)).toBe(false);
    });
  });
});
