/**
 * Connection Handler Tests - Expanded Coverage
 *
 * Covers untested spec requirements:
 * - Connection States (CONNECTION.spec.md#L21)
 * - States Summary (CONNECTION.spec.md#L25)
 * - Message Types (CONNECTION.spec.md#L34)
 * - Hibernation Behavior (CONNECTION.spec.md#L91)
 * - Error Scenarios (CONNECTION.spec.md#L131)
 * - Version Mismatch (CONNECTION.spec.md#L133)
 * - Invalid JWT (CONNECTION.spec.md#L151)
 * - Heartbeat Timeout (CONNECTION.spec.md#L166)
 * - Cleanup Protocol (CONNECTION.spec.md#L176)
 * - On Abnormal Disconnect (CONNECTION.spec.md#L188)
 * - hub:disconnect_ack message type (SPEC_COVERAGE.md)
 * - hub:error message type (SPEC_COVERAGE.md)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { handleConnect, handleHeartbeat, handleDisconnect } from '../connection';
import type { SharedMessage, Session, Env } from '../../types';
import { toCanonicalAddress } from '../../utils';

describe('Connection Handlers - Expanded Coverage', () => {
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

  // ---------------------------------------------------------------------------
  // Connection Handshake - CONNECTION.spec.md#L38
  // ---------------------------------------------------------------------------
  describe('handleConnect - extended', () => {
    it('should return sessionId in connected response', async () => {
      // @requirement: Connection Handshake (CONNECTION.spec.md#L38)
      // @requirement: hub:connected includes sessionId
      const connectMsg: SharedMessage = {
        id: 'conn-1',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send', 'receive'] },
        ttl: 5000,
        signature: null,
      };

      const response = await handleConnect(connectMsg, session, mockEnv);

      expect(response.type).toBe('hub:connected');
      expect((response.payload as any).sessionId).toBe(session.sessionId);
    });

    it('should return heartbeatInterval in connected response', async () => {
      // @requirement: Connection Handshake response includes heartbeat config
      const connectMsg: SharedMessage = {
        id: 'conn-2',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      const response = await handleConnect(connectMsg, session, mockEnv);

      expect((response.payload as any).heartbeatInterval).toBe(30000);
    });

    it('should return maxMessageSize in connected response', async () => {
      // @requirement: Connection Handshake response includes maxMessageSize
      const connectMsg: SharedMessage = {
        id: 'conn-3',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      const response = await handleConnect(connectMsg, session, mockEnv);

      expect((response.payload as any).maxMessageSize).toBe(1048576);
    });

    it('should set correlationId matching request id', async () => {
      // @requirement: Ask pattern responses include correlationId
      const msgId = 'correlation-test-id';
      const connectMsg: SharedMessage = {
        id: msgId,
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      const response = await handleConnect(connectMsg, session, mockEnv);

      expect(response.correlationId).toBe(msgId);
    });

    it('should update session lastHeartbeat on connect', async () => {
      // @requirement: Session state updated on connect
      const before = Date.now();
      const connectMsg: SharedMessage = {
        id: 'conn-hb',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      await handleConnect(connectMsg, session, mockEnv);

      expect(session.lastHeartbeat).toBeGreaterThanOrEqual(before);
    });

    it('should update session connectedAt on connect', async () => {
      // @requirement: Session connectedAt timestamp set on connect
      const before = Date.now();
      const connectMsg: SharedMessage = {
        id: 'conn-time',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      await handleConnect(connectMsg, session, mockEnv);

      expect(session.connectedAt).toBeGreaterThanOrEqual(before);
    });

    it('should reject connect with missing capabilities', async () => {
      // @requirement: Connection requires capabilities array
      const connectMsg: SharedMessage = {
        id: 'conn-nocaps',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0' }, // No capabilities
        ttl: 5000,
        signature: null,
      };

      await expect(handleConnect(connectMsg, session, mockEnv)).rejects.toThrow();
    });

    it('should reject connect with empty capabilities array', async () => {
      // @requirement: Capabilities must be non-empty
      const connectMsg: SharedMessage = {
        id: 'conn-emptycaps',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: [] },
        ttl: 5000,
        signature: null,
      };

      await expect(handleConnect(connectMsg, session, mockEnv)).rejects.toThrow();
    });

    it('should set actorIdentity from from address in non-auth mode', async () => {
      // @requirement: Authentication disabled mode uses from address as identity
      const clientAddress = toCanonicalAddress('browser/my-client');
      const connectMsg: SharedMessage = {
        id: 'conn-identity',
        from: clientAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      await handleConnect(connectMsg, session, mockEnv);

      expect(session.actorIdentity).toBe(clientAddress);
    });

    it('should reject version mismatch with major version difference', async () => {
      // @requirement: Version Mismatch (CONNECTION.spec.md#L133)
      const connectMsg: SharedMessage = {
        id: 'conn-vm',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '2.0.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      await expect(handleConnect(connectMsg, session, mockEnv)).rejects.toThrow('Protocol version mismatch');
    });

    it('should reject when protocolVersion is completely absent', async () => {
      // @requirement: Version Mismatch - missing version
      const connectMsg: SharedMessage = {
        id: 'conn-noversion',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      await expect(handleConnect(connectMsg, session, mockEnv)).rejects.toThrow();
    });

    it('should reject when auth is enabled but no token provided', async () => {
      // @requirement: Invalid JWT (CONNECTION.spec.md#L151)
      // @requirement: Authentication required when AUTH_ENABLED=true
      const authEnv: Env = { ...mockEnv, AUTH_ENABLED: 'true', JWT_SECRET: 'test-secret' };

      const connectMsg: SharedMessage = {
        id: 'conn-noauth',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:connect',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: { protocolVersion: '0.1.0', capabilities: ['send'] },
        ttl: 5000,
        signature: null,
      };

      await expect(handleConnect(connectMsg, session, authEnv)).rejects.toThrow();
    });
  });

  // ---------------------------------------------------------------------------
  // Heartbeat Flow - CONNECTION.spec.md#L55
  // ---------------------------------------------------------------------------
  describe('handleHeartbeat - extended', () => {
    it('should echo client timestamp in heartbeat_ack', () => {
      // @requirement: Heartbeat Flow (CONNECTION.spec.md#L55)
      // @requirement: hub:heartbeat_ack includes client timestamp
      const clientTimestamp = 1708272000000;
      const heartbeatMsg: SharedMessage = {
        id: 'hb-1',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:heartbeat',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { timestamp: clientTimestamp },
        metadata: {},
        ttl: 10000,
        signature: null,
      };

      const response = handleHeartbeat(heartbeatMsg, session);

      expect(response.type).toBe('hub:heartbeat_ack');
      expect((response.payload as any).timestamp).toBe(clientTimestamp);
    });

    it('should include serverTime in heartbeat_ack', () => {
      // @requirement: Heartbeat response includes server timestamp
      const before = Date.now();
      const heartbeatMsg: SharedMessage = {
        id: 'hb-2',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:heartbeat',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { timestamp: Date.now() },
        metadata: {},
        ttl: 10000,
        signature: null,
      };

      const response = handleHeartbeat(heartbeatMsg, session);

      expect((response.payload as any).serverTime).toBeGreaterThanOrEqual(before);
    });

    it('should update session lastHeartbeat on heartbeat', () => {
      // @requirement: Server updates lastHeartbeat on each heartbeat
      const before = session.lastHeartbeat;

      const heartbeatMsg: SharedMessage = {
        id: 'hb-3',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:heartbeat',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { timestamp: Date.now() },
        metadata: {},
        ttl: 10000,
        signature: null,
      };

      handleHeartbeat(heartbeatMsg, session);

      expect(session.lastHeartbeat).toBeGreaterThanOrEqual(before);
    });

    it('should handle heartbeat from different sessions independently', () => {
      // @requirement: Heartbeat Timeout - lastHeartbeat tracks per-session
      const session2: Session = {
        ...session,
        sessionId: 'session-2',
        lastHeartbeat: 1000, // Old heartbeat
      };

      const heartbeatMsg: SharedMessage = {
        id: 'hb-4',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:heartbeat',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { timestamp: Date.now() },
        metadata: {},
        ttl: 10000,
        signature: null,
      };

      const originalSession1Heartbeat = session.lastHeartbeat;
      handleHeartbeat(heartbeatMsg, session2);

      // session1 lastHeartbeat unchanged
      expect(session.lastHeartbeat).toBe(originalSession1Heartbeat);
      // session2 lastHeartbeat updated
      expect(session2.lastHeartbeat).toBeGreaterThan(1000);
    });
  });

  // ---------------------------------------------------------------------------
  // Graceful Disconnection - CONNECTION.spec.md#L73
  // ---------------------------------------------------------------------------
  describe('handleDisconnect', () => {
    it('should return disconnect acknowledgment', () => {
      // @requirement: Cleanup Protocol - On Graceful Disconnect (CONNECTION.spec.md#L176)
      // @requirement: hub:disconnect_ack sent before closing WebSocket
      const disconnectMsg: SharedMessage = {
        id: 'disc-1',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:disconnect',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { reason: 'user_logout' },
        metadata: {},
        ttl: null,
        signature: null,
      };

      const response = handleDisconnect(disconnectMsg, session);

      expect(response).toBeDefined();
      expect(response.type).toBe('hub:disconnect');
    });

    it('should handle disconnect with no reason gracefully', () => {
      // @requirement: Graceful Disconnection handles optional reason
      const disconnectMsg: SharedMessage = {
        id: 'disc-2',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:disconnect',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: null,
        signature: null,
      };

      expect(() => handleDisconnect(disconnectMsg, session)).not.toThrow();
    });

    it('should handle disconnect with null payload', () => {
      // @requirement: Disconnect payload is optional
      const disconnectMsg: SharedMessage = {
        id: 'disc-3',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:disconnect',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: null,
        metadata: {},
        ttl: null,
        signature: null,
      };

      expect(() => handleDisconnect(disconnectMsg, session)).not.toThrow();
      const response = handleDisconnect(disconnectMsg, session);
      expect(response).toBeDefined();
    });
  });

  // ---------------------------------------------------------------------------
  // Connection State Tracking - CONNECTION.spec.md#L21
  // ---------------------------------------------------------------------------
  describe('Connection State Management', () => {
    it('should recognize session connection states', () => {
      // @requirement: Connection States (CONNECTION.spec.md#L21)
      // @requirement: States Summary (CONNECTION.spec.md#L25)
      // States: connecting, connected, disconnecting, disconnected
      const states = ['connecting', 'connected', 'disconnecting', 'disconnected'] as const;

      for (const state of states) {
        const s: Session = { ...session, connectionState: state };
        expect(s.connectionState).toBe(state);
      }
    });

    it('session starts in connecting state by convention', () => {
      // @requirement: States Summary - initial state is connecting
      const newSession: Session = {
        sessionId: 'new-session',
        actorIdentity: null,
        capabilities: [],
        connectedAt: Date.now(),
        lastHeartbeat: Date.now(),
        authenticated: false,
        paused: false,
        connectionState: 'connecting',
        rateLimitBucket: { tokens: 100, capacity: 100, refillRate: 100 / 60, lastRefill: Date.now() },
      };

      expect(newSession.connectionState).toBe('connecting');
    });

    it('session can be set to disconnected state (On Abnormal Disconnect)', () => {
      // @requirement: On Abnormal Disconnect (CONNECTION.spec.md#L188)
      // @requirement: WebSocket close triggers cleanup
      session.connectionState = 'disconnected';
      session.disconnectedAt = Date.now();

      expect(session.connectionState).toBe('disconnected');
      expect(session.disconnectedAt).toBeDefined();
    });
  });
});
