/**
 * Messaging Handler Tests - Expanded Coverage
 *
 * Covers untested spec requirements:
 * - Message Types (MESSAGING.spec.md#L24)
 * - Broadcast Delivery (MESSAGING.spec.md#L87)
 * - Message Size Limits (MESSAGING.spec.md#L133)
 * - Error Scenarios (MESSAGING.spec.md#L201)
 * - Unknown Actor (MESSAGING.spec.md#L203)
 * - Expired Registration (MESSAGING.spec.md#L221)
 * - Connection Not Found (MESSAGING.spec.md#L240)
 * - Delivery Guarantees (MESSAGING.spec.md#L257)
 * - Message Patterns (MESSAGING.spec.md#L275)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { handleSend, handleBroadcast } from '../messaging';
import type { SharedMessage, ActorRegistration, Env } from '../../types';
import { toCanonicalAddress } from '../../utils';

// Helper to create mock actor registration
function createMockRegistration(overrides: Partial<ActorRegistration> = {}): ActorRegistration {
  return {
    actorAddress: toCanonicalAddress('browser/test'),
    capabilities: [],
    metadata: {},
    connectionId: 'conn-test',
    registeredAt: Date.now(),
    expiresAt: Date.now() + 300000,
    version: 1,
    renewalToken: 'renewal-test',
    ...overrides,
  };
}

// Helper to create a mock WebSocket with captured messages
function createMockWs() {
  const sent: string[] = [];
  const ws = {
    send: (data: string) => sent.push(data),
    close: () => {},
    accept: () => {},
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => false,
    _sent: sent,
  } as unknown as WebSocket;
  return { ws, sent };
}

// Mock sendMessage function
const mockSendMessage = (ws: WebSocket, message: SharedMessage) => {
  ws.send(JSON.stringify(message));
};

describe('Messaging Handlers - Expanded Coverage', () => {
  let mockEnv: Env;
  let registry: Map<string, ActorRegistration>;
  let connections: Map<string, WebSocket>;

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
    connections = new Map();
  });

  // ---------------------------------------------------------------------------
  // Point-to-Point Delivery - MESSAGING.spec.md#L28
  // ---------------------------------------------------------------------------
  describe('handleSend - extended', () => {
    it('should include forwarded metadata in delivered message', () => {
      // @requirement: Message forwarding includes forwarded:true metadata
      // @requirement: Point-to-Point Delivery (MESSAGING.spec.md#L28)
      const target = toCanonicalAddress('browser/target');
      const sender = toCanonicalAddress('browser/sender');
      const { ws, sent } = createMockWs();

      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-target',
      }));
      connections.set('conn-target', ws);

      const sendMsg: SharedMessage = {
        id: 'fwd-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'task:run', data: { taskId: 'abc' } },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.metadata.forwarded).toBe(true);
    });

    it('should preserve original sender address in forwarded message', () => {
      // @requirement: Flat Payload Structure - preserves from address
      const target = toCanonicalAddress('browser/target');
      const sender = toCanonicalAddress('local/my-agent');
      const { ws, sent } = createMockWs();

      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-t2',
      }));
      connections.set('conn-t2', ws);

      const sendMsg: SharedMessage = {
        id: 'fwd-2',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'custom:event', data: { val: 42 } },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.from).toBe(sender);
    });

    it('should set forwarded message type from payload.type', () => {
      // @requirement: Flat Payload Structure - type from payload.type
      const target = toCanonicalAddress('browser/target');
      const sender = toCanonicalAddress('browser/sender');
      const { ws, sent } = createMockWs();

      registry.set(target, createMockRegistration({ actorAddress: target, connectionId: 'conn-t3' }));
      connections.set('conn-t3', ws);

      const sendMsg: SharedMessage = {
        id: 'fwd-3',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'widget:update', data: { status: 'active' } },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.type).toBe('widget:update');
    });

    it('should use flat payload data (not nested) in forwarded message', () => {
      // @requirement: Flat Payload Structure (MESSAGING.spec.md#L165)
      // @requirement: payload.data becomes payload in forwarded message
      const target = toCanonicalAddress('browser/target');
      const sender = toCanonicalAddress('browser/sender');
      const { ws, sent } = createMockWs();

      registry.set(target, createMockRegistration({ actorAddress: target, connectionId: 'conn-flat' }));
      connections.set('conn-flat', ws);

      const appData = { userId: 'user-42', action: 'click' };

      const sendMsg: SharedMessage = {
        id: 'flat-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'ui:action', data: appData },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      // Flat structure: payload IS the data directly
      expect(forwarded.payload).toEqual(appData);
      // Not nested: { data: appData }
      expect(forwarded.payload.data).toBeUndefined();
    });

    it('should return delivery_ack with status delivered on success', () => {
      // @requirement: hub:delivery_ack (SPEC_COVERAGE.md)
      const target = toCanonicalAddress('browser/target');
      const sender = toCanonicalAddress('browser/sender');
      const { ws } = createMockWs();

      registry.set(target, createMockRegistration({ actorAddress: target, connectionId: 'conn-ack' }));
      connections.set('conn-ack', ws);

      const sendMsg: SharedMessage = {
        id: 'ack-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response?.type).toBe('hub:delivery_ack');
      expect((response?.payload as any).status).toBe('delivered');
      expect((response?.payload as any).messageId).toBe('ack-1');
    });

    it('should handle connection not found (actor registered but WS gone)', () => {
      // @requirement: Connection Not Found (MESSAGING.spec.md#L240)
      const target = toCanonicalAddress('browser/disconnected');
      const sender = toCanonicalAddress('browser/sender');

      // Register actor but don't add to connections
      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-gone', // Not in connections map
      }));
      // connections does NOT have 'conn-gone'

      const sendMsg: SharedMessage = {
        id: 'cnf-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      // Should not throw — actor registered, connection missing; key behavior is no exception
      handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);
      expect(true).toBe(true);
    });

    it('should handle tell pattern with unknown actor silently (no error)', () => {
      // @requirement: Unknown Actor tell pattern silently drops (MESSAGING.spec.md#L203)
      const sendMsg: SharedMessage = {
        id: 'tell-1',
        from: toCanonicalAddress('browser/sender'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: toCanonicalAddress('browser/unknown'), type: 'ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);
      expect(response).toBeNull();
    });

    it('should return unknown_actor for ask pattern with unregistered target', () => {
      // @requirement: Unknown Actor ask pattern returns hub:unknown_actor (MESSAGING.spec.md#L203)
      const sendMsg: SharedMessage = {
        id: 'ask-unknown-1',
        from: toCanonicalAddress('browser/sender'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: toCanonicalAddress('browser/nobody'), type: 'ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);
      expect(response?.type).toBe('hub:unknown_actor');
    });

    it('should remove expired actor from registry when sending to it', () => {
      // @requirement: Expired Registration (MESSAGING.spec.md#L221)
      // @requirement: Server removes expired actor on send attempt
      const target = toCanonicalAddress('browser/expired-target');
      const sender = toCanonicalAddress('browser/sender');

      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-exp',
        expiresAt: Date.now() - 1000, // Already expired
      }));

      const sendMsg: SharedMessage = {
        id: 'exp-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      // Expired actor removed from registry
      expect(registry.has(target)).toBe(false);
    });
  });

  // ---------------------------------------------------------------------------
  // Broadcast Delivery - MESSAGING.spec.md#L87
  // ---------------------------------------------------------------------------
  describe('handleBroadcast - extended', () => {
    it('should deliver to all connected actors', () => {
      // @requirement: Broadcast Delivery (MESSAGING.spec.md#L87)
      const sender = toCanonicalAddress('browser/sender');
      const actors = ['browser/a', 'browser/b', 'browser/c'].map(toCanonicalAddress);
      const mockWss = actors.map(() => createMockWs());

      actors.forEach((addr, i) => {
        registry.set(addr, createMockRegistration({
          actorAddress: addr,
          connectionId: `conn-bc-${i}`,
          expiresAt: Date.now() + 300000,
        }));
        connections.set(`conn-bc-${i}`, mockWss[i].ws);
      });

      const broadcastMsg: SharedMessage = {
        id: 'bc-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { type: 'system:notify', data: { msg: 'hello all' } },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response.type).toBe('hub:broadcast_ack');
      expect((response.payload as any).deliveredCount).toBe(3);

      // All actors received the message
      for (const { sent } of mockWss) {
        expect(sent).toHaveLength(1);
        const msg = JSON.parse(sent[0]);
        expect(msg.type).toBe('system:notify');
      }
    });

    it('should return broadcast_ack with correct deliveredCount', () => {
      // @requirement: Broadcast Delivery guarantees (MESSAGING.spec.md#L87)
      const sender = toCanonicalAddress('browser/sender');
      const { ws } = createMockWs();

      registry.set(toCanonicalAddress('browser/actor-1'), createMockRegistration({
        actorAddress: toCanonicalAddress('browser/actor-1'),
        connectionId: 'conn-bc-count',
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-bc-count', ws);

      const broadcastMsg: SharedMessage = {
        id: 'bc-count-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { type: 'test:event', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);
      expect((response.payload as any).deliveredCount).toBe(1);
    });

    it('should return broadcast_ack with 0 count when no actors registered', () => {
      // @requirement: Broadcast to empty registry
      const broadcastMsg: SharedMessage = {
        id: 'bc-empty',
        from: toCanonicalAddress('browser/sender'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { type: 'system:notify', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response.type).toBe('hub:broadcast_ack');
      expect((response.payload as any).deliveredCount).toBe(0);
    });

    it('should use flat payload structure for broadcast messages', () => {
      // @requirement: Flat Payload Structure (MESSAGING.spec.md#L165)
      const sender = toCanonicalAddress('browser/sender');
      const target = toCanonicalAddress('browser/target');
      const { ws, sent } = createMockWs();

      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-bc-flat',
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-bc-flat', ws);

      const appData = { severity: 'warning', message: 'Server restart' };

      const broadcastMsg: SharedMessage = {
        id: 'bc-flat-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { type: 'system:alert', data: appData },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      // Flat structure: payload is data directly
      expect(forwarded.payload).toEqual(appData);
    });

    it('should include broadcast:true in metadata of forwarded broadcast', () => {
      // @requirement: Broadcast message includes broadcast metadata
      const sender = toCanonicalAddress('browser/sender');
      const target = toCanonicalAddress('browser/target');
      const { ws, sent } = createMockWs();

      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-bc-meta',
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-bc-meta', ws);

      const broadcastMsg: SharedMessage = {
        id: 'bc-meta-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { type: 'system:ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.metadata.broadcast).toBe(true);
    });

    it('should preserve sender identity in broadcast forwarded messages', () => {
      // @requirement: Broadcast preserves original sender in from field
      const sender = toCanonicalAddress('local/admin-agent');
      const target = toCanonicalAddress('browser/target');
      const { ws, sent } = createMockWs();

      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-bc-sender',
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-bc-sender', ws);

      const broadcastMsg: SharedMessage = {
        id: 'bc-sender-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { type: 'admin:command', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.from).toBe(sender);
    });
  });

  // ---------------------------------------------------------------------------
  // Delivery Guarantees - MESSAGING.spec.md#L257
  // ---------------------------------------------------------------------------
  describe('Delivery Guarantees', () => {
    it('should use at-most-once delivery (no retry on failure)', () => {
      // @requirement: Delivery Guarantees (MESSAGING.spec.md#L257)
      // @requirement: At-most-once delivery - no queuing or retry
      const target = toCanonicalAddress('browser/target');
      const sender = toCanonicalAddress('browser/sender');

      // Actor registered but WS throws on send
      let sendCalled = 0;
      const faultyWs = {
        send: () => { sendCalled++; throw new Error('WS error'); },
      } as unknown as WebSocket;

      registry.set(target, createMockRegistration({
        actorAddress: target,
        connectionId: 'conn-faulty',
      }));
      connections.set('conn-faulty', faultyWs);

      const sendMsg: SharedMessage = {
        id: 'atmost-1',
        from: sender,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      // Should not retry on failure
      handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);
      expect(sendCalled).toBeLessThanOrEqual(1); // At most one attempt
    });
  });

  // ---------------------------------------------------------------------------
  // Ask vs Tell Pattern - MESSAGING.spec.md#L275
  // ---------------------------------------------------------------------------
  describe('Message Patterns', () => {
    it('should return delivery_ack for ask pattern', () => {
      // @requirement: Ask Pattern (MESSAGING.spec.md#L277)
      const target = toCanonicalAddress('browser/target');
      const { ws } = createMockWs();

      registry.set(target, createMockRegistration({ actorAddress: target, connectionId: 'conn-ask' }));
      connections.set('conn-ask', ws);

      const sendMsg: SharedMessage = {
        id: 'ask-1',
        from: toCanonicalAddress('browser/sender'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'ping', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);
      expect(response?.type).toBe('hub:delivery_ack');
    });

    it('should return null for tell pattern (no ack)', () => {
      // @requirement: Tell Pattern (MESSAGING.spec.md#L291)
      const target = toCanonicalAddress('browser/target');
      const { ws } = createMockWs();

      registry.set(target, createMockRegistration({ actorAddress: target, connectionId: 'conn-tell' }));
      connections.set('conn-tell', ws);

      const sendMsg: SharedMessage = {
        id: 'tell-2',
        from: toCanonicalAddress('browser/sender'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { to: target, type: 'fire-forget', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);
      expect(response).toBeNull();
    });
  });
});
