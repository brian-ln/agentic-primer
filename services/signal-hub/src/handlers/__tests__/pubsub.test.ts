/**
 * Pub/Sub Handler Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { handleSubscribe, handlePublish, handleUnsubscribe, cleanupSubscriptions } from '../pubsub';
import type { SharedMessage, ActorRegistration, CanonicalAddress } from '../../types';
import { toCanonicalAddress } from '../../utils';

// Helper to create mock actor registration
function createMockRegistration(overrides: Partial<ActorRegistration> = {}): ActorRegistration {
  return {
    actorAddress: toCanonicalAddress("browser/test"),
    capabilities: [],
    metadata: {},
    connectionId: "conn-test",
    registeredAt: Date.now(),
    expiresAt: Date.now() + 300000,
    version: 1,
    renewalToken: "renewal-test",
    ...overrides,
  };
}

// Mock sendMessage function
const mockSendMessage = (ws: WebSocket, message: SharedMessage) => {
  ws.send(JSON.stringify(message));
};

describe('Pub/Sub Handlers', () => {
  let subscriptions: Map<string, Set<CanonicalAddress>>;
  let registry: Map<string, ActorRegistration>;
  let connections: Map<string, WebSocket>;

  beforeEach(() => {
    subscriptions = new Map();
    registry = new Map();
    connections = new Map();
  });

  describe('handleSubscribe', () => {
    it('should subscribe actor to topic', () => {
      const actorAddress = toCanonicalAddress('browser/widget-123');

      const subscribeMsg: SharedMessage = {
        id: 'msg-1',
        from: actorAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          topic: 'events',
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSubscribe(subscribeMsg, subscriptions, actorAddress);

      expect(response.type).toBe('hub:subscribed');
      expect(response.payload).toHaveProperty('topic', 'events');
      expect(response.payload).toHaveProperty('subscriptionId');

      // Verify subscription was added
      expect(subscriptions.has('events')).toBe(true);
      expect(subscriptions.get('events')?.has(actorAddress)).toBe(true);
    });

    it('should add multiple actors to same topic', () => {
      const actor1 = toCanonicalAddress('browser/widget-1');
      const actor2 = toCanonicalAddress('browser/widget-2');

      const msg1: SharedMessage = {
        id: 'msg-1',
        from: actor1,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'events' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const msg2: SharedMessage = {
        id: 'msg-2',
        from: actor2,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'events' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleSubscribe(msg1, subscriptions, actor1);
      handleSubscribe(msg2, subscriptions, actor2);

      const subscribers = subscriptions.get('events');
      expect(subscribers?.size).toBe(2);
      expect(subscribers?.has(actor1)).toBe(true);
      expect(subscribers?.has(actor2)).toBe(true);
    });

    it('should reject missing topic', () => {
      const actorAddress = toCanonicalAddress('browser/widget-123');

      const subscribeMsg: SharedMessage = {
        id: 'msg-3',
        from: actorAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleSubscribe(subscribeMsg, subscriptions, actorAddress)).toThrow(
        'topic is required'
      );
    });
  });

  describe('handlePublish', () => {
    it('should publish message to topic subscribers', () => {
      const publisher = toCanonicalAddress('browser/publisher');
      const subscriber1 = toCanonicalAddress('browser/subscriber-1');
      const subscriber2 = toCanonicalAddress('browser/subscriber-2');

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

      // Register subscribers
      registry.set(subscriber1, createMockRegistration({
        actorAddress: subscriber1,
        capabilities: ['render'],
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-1', mockWs1);

      registry.set(subscriber2, createMockRegistration({
        actorAddress: subscriber2,
        capabilities: ['render'],
        connectionId: 'conn-2',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-2', mockWs2);

      // Add subscriptions
      const subscribers = new Set<CanonicalAddress>();
      subscribers.add(subscriber1);
      subscribers.add(subscriber2);
      subscriptions.set('events', subscribers);

      const publishMsg: SharedMessage = {
        id: 'msg-4',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          topic: 'events',
          type: 'event:created',
          data: { eventId: '123' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      expect(response.type).toBe('hub:published');
      expect(response.payload).toHaveProperty('topic', 'events');
      expect(response.payload).toHaveProperty('subscriberCount', 2);

      // Verify both subscribers received message
      expect((mockWs1 as any)._sentMessages.length).toBe(1);
      expect((mockWs2 as any)._sentMessages.length).toBe(1);

      const msg1 = JSON.parse((mockWs1 as any)._sentMessages[0]);
      expect(msg1.type).toBe('event:created');
      expect(msg1.from).toBe(publisher);
      expect(msg1.to).toBe(subscriber1);
      expect(msg1.payload).toEqual({ eventId: '123' });
      expect(msg1.metadata.topic).toBe('events');
      expect(msg1.metadata.publication).toBe(true);
    });

    it('should handle topic with no subscribers', () => {
      const publisher = toCanonicalAddress('browser/publisher');

      const publishMsg: SharedMessage = {
        id: 'msg-5',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          topic: 'empty-topic',
          type: 'event:created',
          data: { eventId: '123' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      expect(response.type).toBe('hub:published');
      expect(response.payload).toHaveProperty('subscriberCount', 0);
    });

    it('should skip expired subscribers', () => {
      const publisher = toCanonicalAddress('browser/publisher');
      const subscriber1 = toCanonicalAddress('browser/subscriber-1');
      const subscriber2 = toCanonicalAddress('browser/subscriber-2');

      const mockWs1 = {
        send: () => {},
        _sentMessages: [] as string[],
      } as unknown as WebSocket;
      (mockWs1 as any).send = (data: string) => {
        (mockWs1 as any)._sentMessages.push(data);
      };

      // Register active subscriber
      registry.set(subscriber1, createMockRegistration({
        actorAddress: subscriber1,
        capabilities: ['render'],
        connectionId: 'conn-1',
        registeredAt: Date.now(),
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-1', mockWs1);

      // Register expired subscriber
      registry.set(subscriber2, createMockRegistration({
        actorAddress: subscriber2,
        capabilities: ['render'],
        connectionId: 'conn-2',
        registeredAt: Date.now() - 400000,
        expiresAt: Date.now() - 100000, // Expired
      }));

      // Add subscriptions
      const subscribers = new Set<CanonicalAddress>();
      subscribers.add(subscriber1);
      subscribers.add(subscriber2);
      subscriptions.set('events', subscribers);

      const publishMsg: SharedMessage = {
        id: 'msg-6',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          topic: 'events',
          type: 'event:created',
          data: { eventId: '123' },
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      // Only active subscriber should receive message
      expect(response.payload).toHaveProperty('subscriberCount', 1);
      expect((mockWs1 as any)._sentMessages.length).toBe(1);

      // Expired subscriber should be removed from both registry and subscriptions
      expect(registry.has(subscriber2)).toBe(false);
      expect(subscriptions.get('events')?.has(subscriber2)).toBe(false);
    });

    it('should reject missing topic', () => {
      const publisher = toCanonicalAddress('browser/publisher');

      const publishMsg: SharedMessage = {
        id: 'msg-7',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          type: 'event:created',
          data: { eventId: '123' },
          // Missing topic
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage)).toThrow(
        'topic is required'
      );
    });

    it('should reject missing payload.type', () => {
      const publisher = toCanonicalAddress('browser/publisher');

      const publishMsg: SharedMessage = {
        id: 'msg-8',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          topic: 'events',
          data: { eventId: '123' },
          // Missing type
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage)).toThrow(
        'payload.type is required'
      );
    });
  });

  describe('handleUnsubscribe', () => {
    it('should unsubscribe actor from topic', () => {
      const actorAddress = toCanonicalAddress('browser/widget-123');

      // Add subscription
      const subscribers = new Set<CanonicalAddress>();
      subscribers.add(actorAddress);
      subscriptions.set('events', subscribers);

      const unsubscribeMsg: SharedMessage = {
        id: 'msg-9',
        from: actorAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unsubscribe',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          subscriptionId: 'sub-123',
        },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleUnsubscribe(unsubscribeMsg, subscriptions, actorAddress);

      // Empty topic set should be cleaned up (no longer exists in map)
      expect(subscriptions.has('events')).toBe(false);
    });

    it('should reject missing subscriptionId', () => {
      const actorAddress = toCanonicalAddress('browser/widget-123');

      const unsubscribeMsg: SharedMessage = {
        id: 'msg-10',
        from: actorAddress,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unsubscribe',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleUnsubscribe(unsubscribeMsg, subscriptions, actorAddress)).toThrow(
        'subscriptionId is required'
      );
    });
  });

  describe('cleanupSubscriptions', () => {
    it('should remove actor from all topics', () => {
      const actorAddress = toCanonicalAddress('browser/widget-123');

      // Add actor to multiple topics
      const subscribers1 = new Set<CanonicalAddress>();
      subscribers1.add(actorAddress);
      subscribers1.add(toCanonicalAddress('browser/other-actor'));
      subscriptions.set('events', subscribers1);

      const subscribers2 = new Set<CanonicalAddress>();
      subscribers2.add(actorAddress);
      subscriptions.set('notifications', subscribers2);

      cleanupSubscriptions(subscriptions, actorAddress);

      // Actor should be removed from topic with other subscribers
      expect(subscriptions.get('events')?.has(actorAddress)).toBe(false);

      // Topic with other actors should remain
      expect(subscriptions.has('events')).toBe(true);

      // Empty topic should be cleaned up (no longer exists in map)
      expect(subscriptions.has('notifications')).toBe(false);
    });

    it('should handle actor not subscribed to any topics', () => {
      const actorAddress = toCanonicalAddress('browser/widget-123');

      // Should not throw
      expect(() => cleanupSubscriptions(subscriptions, actorAddress)).not.toThrow();
    });
  });
});
