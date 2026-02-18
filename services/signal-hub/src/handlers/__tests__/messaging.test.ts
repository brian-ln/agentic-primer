/**
 * Messaging Handler Tests
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

// Mock sendMessage function
const mockSendMessage = (ws: WebSocket, message: SharedMessage) => {
  ws.send(JSON.stringify(message));
};

describe('Messaging Handlers', () => {
  let mockEnv: Env;
  let registry: Map<string, ActorRegistration>;
  let connections: Map<string, WebSocket>;
  let mockWs: WebSocket;

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

    // Create mock WebSocket
    const sentMessages: string[] = [];
    mockWs = {
      send: (data: string) => {
        sentMessages.push(data);
      },
      close: () => {},
      accept: () => {},
      addEventListener: () => {},
      removeEventListener: () => {},
      dispatchEvent: () => false,
      // Add custom property for testing
      _sentMessages: sentMessages,
    } as unknown as WebSocket;
  });

  describe('handleSend', () => {
    it('should deliver point-to-point message to registered actor', () => {
      const targetAddress = toCanonicalAddress('browser/widget-123');
      const senderAddress = toCanonicalAddress('browser/client');

      // Register target actor
      registry.set(targetAddress, createMockRegistration({
        actorAddress: targetAddress,
        capabilities: ['render'],
        connectionId: 'conn-123',
      }));
      connections.set('conn-123', mockWs);

      const sendMsg: SharedMessage = {
        id: 'msg-1',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          to: targetAddress,
          type: 'task:assign',
          data: { taskId: '456' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      // Should return delivery acknowledgment
      expect(response).not.toBeNull();
      expect(response?.type).toBe('hub:delivery_ack');
      expect(response?.payload).toHaveProperty('messageId', 'msg-1');
      expect(response?.payload).toHaveProperty('status', 'delivered');

      // Verify message was sent to WebSocket
      const sentMessages = (mockWs as any)._sentMessages;
      expect(sentMessages.length).toBe(1);

      const forwardedMsg = JSON.parse(sentMessages[0]);
      expect(forwardedMsg.type).toBe('task:assign');
      expect(forwardedMsg.from).toBe(senderAddress);
      expect(forwardedMsg.to).toBe(targetAddress);
      expect(forwardedMsg.payload).toEqual({ taskId: '456' });
      expect(forwardedMsg.metadata.forwarded).toBe(true);
    });

    it('should return error for unknown actor (ask pattern)', () => {
      const targetAddress = toCanonicalAddress('browser/unknown');
      const senderAddress = toCanonicalAddress('browser/client');

      const sendMsg: SharedMessage = {
        id: 'msg-2',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          to: targetAddress,
          type: 'task:assign',
          data: { taskId: '456' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response).not.toBeNull();
      expect(response?.type).toBe('hub:unknown_actor');
      expect(response?.payload).toHaveProperty('actorAddress', targetAddress);
    });

    it('should silently drop message for unknown actor (tell pattern)', () => {
      const targetAddress = toCanonicalAddress('browser/unknown');
      const senderAddress = toCanonicalAddress('browser/client');

      const sendMsg: SharedMessage = {
        id: 'msg-3',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          to: targetAddress,
          type: 'task:assign',
          data: { taskId: '456' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response).toBeNull();
    });

    it('should reject invalid payload', () => {
      const sendMsg: SharedMessage = {
        id: 'msg-4',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null, // Invalid
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage)).toThrow(
        'payload must be an object'
      );
    });

    it('should reject missing payload.type', () => {
      const sendMsg: SharedMessage = {
        id: 'msg-5',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          to: toCanonicalAddress('browser/widget-123'),
          data: { taskId: '456' },
          // Missing type
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage)).toThrow(
        'payload.type is required'
      );
    });

    it('should handle expired actor registration', () => {
      const targetAddress = toCanonicalAddress('browser/widget-123');
      const senderAddress = toCanonicalAddress('browser/client');

      // Register target actor with past expiration
      registry.set(targetAddress, createMockRegistration({
        actorAddress: targetAddress,
        capabilities: ['render'],
        connectionId: 'conn-123',
        registeredAt: Date.now() - 400000,
        expiresAt: Date.now() - 100000, // Expired
      }));
      connections.set('conn-123', mockWs);

      const sendMsg: SharedMessage = {
        id: 'msg-6',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:send',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          to: targetAddress,
          type: 'task:assign',
          data: { taskId: '456' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSend(sendMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response?.type).toBe('hub:unknown_actor');
      expect(response?.payload).toHaveProperty('message', 'Actor registration expired');

      // Should remove expired actor from registry
      expect(registry.has(targetAddress)).toBe(false);
    });
  });

  describe('handleBroadcast', () => {
    it('should fan-out message to all registered actors', () => {
      const senderAddress = toCanonicalAddress('browser/client');
      const actor1 = toCanonicalAddress('browser/widget-1');
      const actor2 = toCanonicalAddress('browser/widget-2');

      const mockWs1 = {
        send: () => {},
        _sentMessages: [] as string[],
      } as unknown as WebSocket;
      (mockWs1 as any).send = (data: string) => {
        (mockWs1 as any)._sentMessages.push(data);
      };

      const mockWs2 = {
        send: () => {},
        _sentMessages: [] as string[],
      } as unknown as WebSocket;
      (mockWs2 as any).send = (data: string) => {
        (mockWs2 as any)._sentMessages.push(data);
      };

      // Register two actors
      registry.set(actor1, createMockRegistration({
        actorAddress: actor1,
        capabilities: ['render'],
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-1', mockWs1);

      registry.set(actor2, createMockRegistration({
        actorAddress: actor2,
        capabilities: ['render'],
        connectionId: 'conn-2',
      }));
      connections.set('conn-2', mockWs2);

      const broadcastMsg: SharedMessage = {
        id: 'msg-7',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          type: 'system:shutdown',
          data: { reason: 'maintenance' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      // Should return broadcast acknowledgment
      expect(response.type).toBe('hub:broadcast_ack');
      expect(response.payload).toHaveProperty('deliveredCount', 2);
      expect(response.payload).toHaveProperty('failedCount', 0);

      // Verify both actors received the message
      expect((mockWs1 as any)._sentMessages.length).toBe(1);
      expect((mockWs2 as any)._sentMessages.length).toBe(1);

      const msg1 = JSON.parse((mockWs1 as any)._sentMessages[0]);
      expect(msg1.type).toBe('system:shutdown');
      expect(msg1.from).toBe(senderAddress);
      expect(msg1.payload).toEqual({ reason: 'maintenance' });
    });

    it('should exclude sender when excludeSelf is true', () => {
      const senderAddress = toCanonicalAddress('browser/client');
      const actor1 = toCanonicalAddress('browser/widget-1');

      const mockWs1 = {
        send: () => {},
        _sentMessages: [] as string[],
      } as unknown as WebSocket;
      (mockWs1 as any).send = (data: string) => {
        (mockWs1 as any)._sentMessages.push(data);
      };

      const mockWsSender = {
        send: () => {},
        _sentMessages: [] as string[],
      } as unknown as WebSocket;
      (mockWsSender as any).send = (data: string) => {
        (mockWsSender as any)._sentMessages.push(data);
      };

      // Register sender and another actor
      registry.set(senderAddress, createMockRegistration({
        actorAddress: senderAddress,
        capabilities: ['render'],
        connectionId: 'conn-sender',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-sender', mockWsSender);

      registry.set(actor1, createMockRegistration({
        actorAddress: actor1,
        capabilities: ['render'],
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-1', mockWs1);

      const broadcastMsg: SharedMessage = {
        id: 'msg-8',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          type: 'system:shutdown',
          data: { reason: 'maintenance' },
          excludeSelf: true,
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response.payload).toHaveProperty('deliveredCount', 1);

      // Sender should not receive message
      expect((mockWsSender as any)._sentMessages.length).toBe(0);

      // Other actor should receive message
      expect((mockWs1 as any)._sentMessages.length).toBe(1);
    });

    it('should filter by capability when targetCapability specified', () => {
      const senderAddress = toCanonicalAddress('browser/client');
      const actor1 = toCanonicalAddress('browser/widget-1');
      const actor2 = toCanonicalAddress('browser/widget-2');

      const mockWs1 = {
        send: () => {},
        _sentMessages: [] as string[],
      } as unknown as WebSocket;
      (mockWs1 as any).send = (data: string) => {
        (mockWs1 as any)._sentMessages.push(data);
      };

      const mockWs2 = {
        send: () => {},
        _sentMessages: [] as string[],
      } as unknown as WebSocket;
      (mockWs2 as any).send = (data: string) => {
        (mockWs2 as any)._sentMessages.push(data);
      };

      // Register two actors with different capabilities
      registry.set(actor1, createMockRegistration({
        actorAddress: actor1,
        capabilities: ['render', 'compute'],
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-1', mockWs1);

      registry.set(actor2, createMockRegistration({
        actorAddress: actor2,
        capabilities: ['render'], // No 'compute'
        connectionId: 'conn-2',
      }));
      connections.set('conn-2', mockWs2);

      const broadcastMsg: SharedMessage = {
        id: 'msg-9',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          type: 'task:compute',
          data: { job: 'analysis' },
        },
        metadata: {
          targetCapability: 'compute',
        },
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      // Only actor1 has 'compute' capability
      expect(response.payload).toHaveProperty('deliveredCount', 1);

      expect((mockWs1 as any)._sentMessages.length).toBe(1);
      expect((mockWs2 as any)._sentMessages.length).toBe(0);
    });

    it('should reject invalid payload', () => {
      const broadcastMsg: SharedMessage = {
        id: 'msg-10',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: null, // Invalid
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage)).toThrow(
        'payload must be an object'
      );
    });

    it('should reject missing payload.type', () => {
      const broadcastMsg: SharedMessage = {
        id: 'msg-11',
        from: toCanonicalAddress('browser/client'),
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          data: { reason: 'maintenance' },
          // Missing type
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage)).toThrow(
        'payload.type is required'
      );
    });

    // @spec: resource-protection/RESOURCE_PROTECTION.spec.md#broadcast-limit
    // @requirement: Broadcast to >1000 recipients returns hub:error with BROADCAST_LIMIT_EXCEEDED
    it('should return hub:error when recipient count exceeds 1000', () => {
      // @spec: WS1.2 broadcast subscriber limit
      const senderAddress = toCanonicalAddress('browser/client');

      // Register 1001 actors
      for (let i = 0; i < 1001; i++) {
        const actorAddress = toCanonicalAddress(`browser/widget-${i}`);
        const connId = `conn-${i}`;
        const actorWs = {
          send: () => {},
          _sentMessages: [] as string[],
        } as unknown as WebSocket;
        (actorWs as any).send = (data: string) => {
          (actorWs as any)._sentMessages.push(data);
        };
        registry.set(actorAddress, createMockRegistration({
          actorAddress,
          capabilities: ['render'],
          connectionId: connId,
        }));
        connections.set(connId, actorWs);
      }

      const broadcastMsg: SharedMessage = {
        id: 'msg-broadcast-limit',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          type: 'system:update',
          data: { version: '2.0' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      expect(response).not.toBeNull();
      expect(response.type).toBe('hub:error');
      expect(response.payload).toHaveProperty('code', 'BROADCAST_LIMIT_EXCEEDED');
      expect(response.payload).toHaveProperty('message');
      expect((response.payload as any).message).toContain('1001');
    });

    it('should allow broadcast to exactly 1000 recipients', () => {
      // @spec: WS1.2 broadcast subscriber limit — boundary condition
      const senderAddress = toCanonicalAddress('browser/client');

      // Register exactly 1000 actors
      for (let i = 0; i < 1000; i++) {
        const actorAddress = toCanonicalAddress(`browser/actor-${i}`);
        const connId = `conn-exact-${i}`;
        const actorWs = {
          send: () => {},
        } as unknown as WebSocket;
        registry.set(actorAddress, createMockRegistration({
          actorAddress,
          capabilities: ['render'],
          connectionId: connId,
        }));
        connections.set(connId, actorWs);
      }

      const broadcastMsg: SharedMessage = {
        id: 'msg-broadcast-ok',
        from: senderAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:broadcast',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          type: 'system:ping',
          data: {},
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleBroadcast(broadcastMsg, registry, connections, mockEnv, mockSendMessage);

      // Should succeed with broadcast_ack, not error
      expect(response.type).toBe('hub:broadcast_ack');
      expect((response.payload as any).deliveredCount).toBe(1000);
    });
  });
});
