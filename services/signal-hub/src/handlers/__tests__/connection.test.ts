/**
 * Connection Handler Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { handleConnect, handleHeartbeat } from '../connection';
import type { SharedMessage, Session, Env } from '../../types';
import { toCanonicalAddress } from '../../utils';

describe('Connection Handlers', () => {
  let mockEnv: Env;
  let session: Session;

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

    session = {
      sessionId: 'session-test',
      actorIdentity: null,
      capabilities: [],
      connectedAt: Date.now(),
      lastHeartbeat: Date.now(),
      authenticated: false,
      paused: false,
      connectionState: 'connected' as const,
      rateLimitBucket: { tokens: 100, capacity: 100, refillRate: 100 / 60, lastRefill: Date.now() },
    };
  });

  describe('handleConnect', () => {
    it('should successfully connect with valid metadata', async () => {
      const connectMsg: SharedMessage = {
        id: 'msg-1',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {
          protocolVersion: '0.1.0',
          capabilities: ['send', 'broadcast'],
        },
        ttl: 5000,
        signature: null,
      };

      const response = await handleConnect(connectMsg, session, mockEnv);

      expect(response.type).toBe('hub:connected');
      expect(response.correlationId).toBe('msg-1');
      expect(response.payload).toHaveProperty('sessionId');
      expect(response.payload).toHaveProperty('serverVersion', '0.1.0');
      expect(response.payload).toHaveProperty('maxMessageSize', 1048576);
    });

    it('should reject missing protocolVersion', async () => {
      const connectMsg: SharedMessage = {
        id: 'msg-2',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {
          capabilities: ['send'],
        },
        ttl: 5000,
        signature: null,
      };

      await expect(handleConnect(connectMsg, session, mockEnv)).rejects.toThrow(
        'Missing protocolVersion in metadata'
      );
    });

    it('should reject version mismatch', async () => {
      const connectMsg: SharedMessage = {
        id: 'msg-3',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {
          protocolVersion: '1.0.0', // Major version mismatch
          capabilities: ['send'],
        },
        ttl: 5000,
        signature: null,
      };

      await expect(handleConnect(connectMsg, session, mockEnv)).rejects.toThrow(
        'Protocol version mismatch'
      );
    });

    it('should accept minor version differences', async () => {
      const connectMsg: SharedMessage = {
        id: 'msg-4',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {
          protocolVersion: '0.2.1', // Minor/patch version difference OK
          capabilities: ['send'],
        },
        ttl: 5000,
        signature: null,
      };

      const response = await handleConnect(connectMsg, session, mockEnv);
      expect(response.type).toBe('hub:connected');
    });
  });

  describe('handleHeartbeat', () => {
    it('should respond with heartbeat_ack', () => {
      const heartbeatMsg: SharedMessage = {
        id: 'msg-5',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:heartbeat',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { timestamp: 1234567890 },
        metadata: {},
        ttl: 10000,
        signature: null,
      };

      const initialHeartbeat = session.lastHeartbeat;
      const response = handleHeartbeat(heartbeatMsg, session);

      expect(response.type).toBe('hub:heartbeat_ack');
      expect(response.payload).toHaveProperty('timestamp', 1234567890);
      expect(response.payload).toHaveProperty('serverTime');
      expect(session.lastHeartbeat).toBeGreaterThanOrEqual(initialHeartbeat);
    });
  });
});
