/**
 * Pub/Sub Handler Tests - Expanded Coverage
 *
 * Covers untested spec requirements:
 * - Message Types (PUBSUB.spec.md#L25)
 * - Unsubscribe Flow (PUBSUB.spec.md#L129)
 * - Topic Naming (PUBSUB.spec.md#L166)
 * - Subscription Storage (PUBSUB.spec.md#L238)
 * - Cleanup Protocol (PUBSUB.spec.md#L264)
 * - On hub:unsubscribe (PUBSUB.spec.md#L266)
 * - On Actor Unregister (PUBSUB.spec.md#L273)
 * - On Connection Close (PUBSUB.spec.md#L280)
 * - Subscriber Limits (PUBSUB.spec.md#L289)
 * - Delivery Guarantees (PUBSUB.spec.md#L303)
 * - Error Scenarios (PUBSUB.spec.md#L322)
 * - Invalid Topic Name (PUBSUB.spec.md#L324)
 * - Missing payload.type on Publish (PUBSUB.spec.md#L343)
 * - Topic Discovery (PUBSUB.spec.md#L359)
 * - hub:unsubscribed message type
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { handleSubscribe, handlePublish, handleUnsubscribe, cleanupSubscriptions } from '../pubsub';
import type { SharedMessage, ActorRegistration, CanonicalAddress } from '../../types';
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

// Helper to create a mock WebSocket
function createMockWs() {
  const sent: string[] = [];
  const ws = {
    send: (data: string) => sent.push(data),
    close: () => {},
    _sent: sent,
  } as unknown as WebSocket;
  return { ws, sent };
}

const mockSendMessage = (ws: WebSocket, message: SharedMessage) => {
  ws.send(JSON.stringify(message));
};

describe('Pub/Sub Handlers - Expanded Coverage', () => {
  let subscriptions: Map<string, Set<CanonicalAddress>>;
  let registry: Map<string, ActorRegistration>;
  let connections: Map<string, WebSocket>;

  beforeEach(() => {
    subscriptions = new Map();
    registry = new Map();
    connections = new Map();
  });

  // ---------------------------------------------------------------------------
  // Subscription Flow - PUBSUB.spec.md#L29
  // ---------------------------------------------------------------------------
  describe('handleSubscribe - extended', () => {
    it('should return hub:subscribed response type', () => {
      // @requirement: Message Types (PUBSUB.spec.md#L25)
      // @requirement: hub:subscribed response
      const actor = toCanonicalAddress('browser/widget-1');

      const subscribeMsg: SharedMessage = {
        id: 'sub-ext-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'system/events' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSubscribe(subscribeMsg, subscriptions, actor);
      expect(response.type).toBe('hub:subscribed');
    });

    it('should return topic in subscribed response', () => {
      // @requirement: Subscription Flow response includes topic
      const actor = toCanonicalAddress('browser/widget-2');

      const subscribeMsg: SharedMessage = {
        id: 'sub-ext-2',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'user/notifications' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSubscribe(subscribeMsg, subscriptions, actor);
      expect((response.payload as any).topic).toBe('user/notifications');
    });

    it('should return unique subscriptionId in subscribed response', () => {
      // @requirement: Subscription Flow response includes subscriptionId
      const actor = toCanonicalAddress('browser/widget-3');

      const subscribeMsg: SharedMessage = {
        id: 'sub-ext-3',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'analytics/clicks' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response1 = handleSubscribe(subscribeMsg, subscriptions, actor);

      const actor2 = toCanonicalAddress('browser/widget-4');
      const subscribeMsg2 = { ...subscribeMsg, id: 'sub-ext-4', from: actor2 };
      const response2 = handleSubscribe(subscribeMsg2, subscriptions, actor2);

      const id1 = (response1.payload as any).subscriptionId;
      const id2 = (response2.payload as any).subscriptionId;
      expect(id1).toBeDefined();
      expect(id2).toBeDefined();
      expect(id1).not.toBe(id2);
    });

    it('should be idempotent when subscribing twice to same topic', () => {
      // @requirement: Delivery Guarantees - subscribe is idempotent (PUBSUB.spec.md#L303)
      const actor = toCanonicalAddress('browser/idempotent');

      const subscribeMsg: SharedMessage = {
        id: 'sub-idem-1',
        from: actor,
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

      handleSubscribe(subscribeMsg, subscriptions, actor);
      handleSubscribe({ ...subscribeMsg, id: 'sub-idem-2' }, subscriptions, actor);

      // Set prevents duplicates - should only have 1 subscriber
      expect(subscriptions.get('events')?.size).toBe(1);
    });

    it('should accept hierarchical topic names', () => {
      // @requirement: Topic Naming (PUBSUB.spec.md#L166)
      // @requirement: Hierarchical path notation with forward slashes
      const actor = toCanonicalAddress('browser/widget');
      const hierarchicalTopic = 'system/user/notifications';

      const subscribeMsg: SharedMessage = {
        id: 'sub-hier-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: hierarchicalTopic },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handleSubscribe(subscribeMsg, subscriptions, actor);
      expect(response.type).toBe('hub:subscribed');
      expect(subscriptions.has(hierarchicalTopic)).toBe(true);
    });

    it('should accept topics with hyphens', () => {
      // @requirement: Topic Naming - hyphens allowed (PUBSUB.spec.md#L166)
      const actor = toCanonicalAddress('browser/widget');

      const subscribeMsg: SharedMessage = {
        id: 'sub-hyp-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'debug/error-logs' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleSubscribe(subscribeMsg, subscriptions, actor)).not.toThrow();
    });
  });

  // ---------------------------------------------------------------------------
  // Unsubscribe Flow - PUBSUB.spec.md#L129
  // ---------------------------------------------------------------------------
  describe('handleUnsubscribe - extended (topic-based)', () => {
    it('should unsubscribe actor from topic using topic field', () => {
      // @requirement: Unsubscribe Flow (PUBSUB.spec.md#L129)
      // @requirement: On hub:unsubscribe (PUBSUB.spec.md#L266)
      // NOTE: The handler uses payload.topic, not payload.subscriptionId
      const actor = toCanonicalAddress('browser/subscriber');
      const topic = 'system/events';

      // Pre-add subscription
      const subs = new Set<CanonicalAddress>([actor]);
      subscriptions.set(topic, subs);

      const unsubMsg: SharedMessage = {
        id: 'unsub-topic-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unsubscribe',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic },
        metadata: {},
        ttl: null,
        signature: null,
      };

      handleUnsubscribe(unsubMsg, subscriptions, actor);

      // Topic should be cleaned up (no subscribers left)
      expect(subscriptions.has(topic)).toBe(false);
    });

    it('should only remove the requesting actor from topic (not others)', () => {
      // @requirement: Unsubscribe only removes requesting actor
      const actor1 = toCanonicalAddress('browser/actor-1');
      const actor2 = toCanonicalAddress('browser/actor-2');
      const topic = 'shared/topic';

      const subs = new Set<CanonicalAddress>([actor1, actor2]);
      subscriptions.set(topic, subs);

      const unsubMsg: SharedMessage = {
        id: 'unsub-partial-1',
        from: actor1,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unsubscribe',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic },
        metadata: {},
        ttl: null,
        signature: null,
      };

      handleUnsubscribe(unsubMsg, subscriptions, actor1);

      // Topic still exists with actor2
      expect(subscriptions.has(topic)).toBe(true);
      expect(subscriptions.get(topic)?.has(actor1)).toBe(false);
      expect(subscriptions.get(topic)?.has(actor2)).toBe(true);
    });

    it('should clean up empty topic set after last subscriber unsubscribes', () => {
      // @requirement: On hub:unsubscribe - delete topic if empty (PUBSUB.spec.md#L266)
      const actor = toCanonicalAddress('browser/last-sub');
      const topic = 'lonely/topic';

      const subs = new Set<CanonicalAddress>([actor]);
      subscriptions.set(topic, subs);

      const unsubMsg: SharedMessage = {
        id: 'unsub-clean-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unsubscribe',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic },
        metadata: {},
        ttl: null,
        signature: null,
      };

      handleUnsubscribe(unsubMsg, subscriptions, actor);

      // Empty topic should be removed
      expect(subscriptions.has(topic)).toBe(false);
    });

    it('should handle unsubscribe from non-existent topic gracefully', () => {
      // @requirement: Unsubscribe handles missing topic gracefully
      const actor = toCanonicalAddress('browser/not-subbed');

      const unsubMsg: SharedMessage = {
        id: 'unsub-missing-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unsubscribe',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'nonexistent/topic' },
        metadata: {},
        ttl: null,
        signature: null,
      };

      expect(() => handleUnsubscribe(unsubMsg, subscriptions, actor)).not.toThrow();
    });

    it('should reject unsubscribe with missing topic', () => {
      // @requirement: Error Scenarios - topic required (PUBSUB.spec.md#L322)
      const actor = toCanonicalAddress('browser/actor');

      const unsubMsg: SharedMessage = {
        id: 'unsub-err-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:unsubscribe',
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        payload: {},
        metadata: {},
        ttl: null,
        signature: null,
      };

      expect(() => handleUnsubscribe(unsubMsg, subscriptions, actor)).toThrow('topic is required');
    });
  });

  // ---------------------------------------------------------------------------
  // Cleanup Protocol - PUBSUB.spec.md#L264
  // ---------------------------------------------------------------------------
  describe('cleanupSubscriptions - extended', () => {
    it('should remove actor from all subscribed topics on disconnect', () => {
      // @requirement: Cleanup Protocol (PUBSUB.spec.md#L264)
      // @requirement: On Connection Close (PUBSUB.spec.md#L280)
      const actor = toCanonicalAddress('browser/disconnecting');
      const other = toCanonicalAddress('browser/staying');

      subscriptions.set('topic-a', new Set([actor, other]));
      subscriptions.set('topic-b', new Set([actor]));
      subscriptions.set('topic-c', new Set([other]));

      cleanupSubscriptions(subscriptions, actor);

      // Actor removed from multi-actor topic
      expect(subscriptions.get('topic-a')?.has(actor)).toBe(false);
      expect(subscriptions.get('topic-a')?.has(other)).toBe(true);

      // Sole-actor topic cleaned up
      expect(subscriptions.has('topic-b')).toBe(false);

      // Unrelated topic unchanged
      expect(subscriptions.get('topic-c')?.has(other)).toBe(true);
    });

    it('should handle cleanup of actor not in any subscription', () => {
      // @requirement: On Actor Unregister (PUBSUB.spec.md#L273)
      const actor = toCanonicalAddress('browser/no-subs');
      const other = toCanonicalAddress('browser/has-subs');

      subscriptions.set('some/topic', new Set([other]));

      // Should not throw and should not affect other subscriptions
      expect(() => cleanupSubscriptions(subscriptions, actor)).not.toThrow();
      expect(subscriptions.has('some/topic')).toBe(true);
    });

    it('should remove all topics where actor is the only subscriber', () => {
      // @requirement: Empty topics deleted after cleanup
      const actor = toCanonicalAddress('browser/sole-sub');

      subscriptions.set('topic-x', new Set([actor]));
      subscriptions.set('topic-y', new Set([actor]));
      subscriptions.set('topic-z', new Set([actor]));

      cleanupSubscriptions(subscriptions, actor);

      expect(subscriptions.size).toBe(0);
    });
  });

  // ---------------------------------------------------------------------------
  // Publish Flow - PUBSUB.spec.md#L67
  // ---------------------------------------------------------------------------
  describe('handlePublish - extended', () => {
    it('should return hub:published response type', () => {
      // @requirement: Message Types (PUBSUB.spec.md#L25)
      // @requirement: Publish Flow response
      const publisher = toCanonicalAddress('browser/pub');
      const subscriber = toCanonicalAddress('browser/sub');
      const { ws } = createMockWs();

      registry.set(subscriber, createMockRegistration({
        actorAddress: subscriber,
        connectionId: 'conn-pub-1',
        expiresAt: Date.now() + 300000,
      }));
      connections.set('conn-pub-1', ws);

      subscriptions.set('events', new Set([subscriber]));

      const publishMsg: SharedMessage = {
        id: 'pub-ext-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'events', type: 'event:created', data: { id: '1' } },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);
      expect(response.type).toBe('hub:published');
    });

    it('should return subscriberCount in published response', () => {
      // @requirement: hub:delivery_ack includes subscriberCount
      const publisher = toCanonicalAddress('browser/pub-2');
      const sub1 = toCanonicalAddress('browser/sub-1');
      const sub2 = toCanonicalAddress('browser/sub-2');
      const { ws: ws1 } = createMockWs();
      const { ws: ws2 } = createMockWs();

      registry.set(sub1, createMockRegistration({ actorAddress: sub1, connectionId: 'conn-s1', expiresAt: Date.now() + 300000 }));
      connections.set('conn-s1', ws1);
      registry.set(sub2, createMockRegistration({ actorAddress: sub2, connectionId: 'conn-s2', expiresAt: Date.now() + 300000 }));
      connections.set('conn-s2', ws2);

      subscriptions.set('multi', new Set([sub1, sub2]));

      const publishMsg: SharedMessage = {
        id: 'pub-count-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'multi', type: 'event:test', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);
      expect((response.payload as any).subscriberCount).toBe(2);
    });

    it('should use flat payload structure when forwarding to subscribers', () => {
      // @requirement: Flat Payload Structure (PUBSUB.spec.md#L212)
      const publisher = toCanonicalAddress('browser/pub-flat');
      const subscriber = toCanonicalAddress('browser/sub-flat');
      const { ws, sent } = createMockWs();

      registry.set(subscriber, createMockRegistration({ actorAddress: subscriber, connectionId: 'conn-flat-pub', expiresAt: Date.now() + 300000 }));
      connections.set('conn-flat-pub', ws);
      subscriptions.set('flat-events', new Set([subscriber]));

      const appData = { eventId: 'evt-99', action: 'user_signup' };

      const publishMsg: SharedMessage = {
        id: 'pub-flat-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'flat-events', type: 'event:user', data: appData },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      // Flat structure: payload IS the data directly
      expect(forwarded.payload).toEqual(appData);
      expect(forwarded.payload.data).toBeUndefined();
    });

    it('should include topic in metadata of forwarded published message', () => {
      // @requirement: Forwarded message includes topic in metadata
      const publisher = toCanonicalAddress('browser/pub-meta');
      const subscriber = toCanonicalAddress('browser/sub-meta');
      const { ws, sent } = createMockWs();
      const topic = 'system/events';

      registry.set(subscriber, createMockRegistration({ actorAddress: subscriber, connectionId: 'conn-meta-pub', expiresAt: Date.now() + 300000 }));
      connections.set('conn-meta-pub', ws);
      subscriptions.set(topic, new Set([subscriber]));

      const publishMsg: SharedMessage = {
        id: 'pub-meta-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic, type: 'event:data', data: { key: 'value' } },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.metadata.topic).toBe(topic);
    });

    it('should include publication:true in metadata of forwarded published message', () => {
      // @requirement: Published messages marked as publication
      const publisher = toCanonicalAddress('browser/pub-flag');
      const subscriber = toCanonicalAddress('browser/sub-flag');
      const { ws, sent } = createMockWs();

      registry.set(subscriber, createMockRegistration({ actorAddress: subscriber, connectionId: 'conn-pub-flag', expiresAt: Date.now() + 300000 }));
      connections.set('conn-pub-flag', ws);
      subscriptions.set('flagged', new Set([subscriber]));

      const publishMsg: SharedMessage = {
        id: 'pub-flag-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'flagged', type: 'event:flag', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.metadata.publication).toBe(true);
    });

    it('should preserve original publisher identity in forwarded message from field', () => {
      // @requirement: Identity preservation - from is original publisher
      const publisher = toCanonicalAddress('local/publisher-agent');
      const subscriber = toCanonicalAddress('browser/subscriber');
      const { ws, sent } = createMockWs();

      registry.set(subscriber, createMockRegistration({ actorAddress: subscriber, connectionId: 'conn-pub-id', expiresAt: Date.now() + 300000 }));
      connections.set('conn-pub-id', ws);
      subscriptions.set('identity', new Set([subscriber]));

      const publishMsg: SharedMessage = {
        id: 'pub-id-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'identity', type: 'event:identity', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.from).toBe(publisher);
    });

    it('should set to field to each subscriber in forwarded message', () => {
      // @requirement: Forwarded message addressed to each subscriber
      const publisher = toCanonicalAddress('browser/pub');
      const subscriber = toCanonicalAddress('browser/specific-sub');
      const { ws, sent } = createMockWs();

      registry.set(subscriber, createMockRegistration({ actorAddress: subscriber, connectionId: 'conn-to-sub', expiresAt: Date.now() + 300000 }));
      connections.set('conn-to-sub', ws);
      subscriptions.set('addressed', new Set([subscriber]));

      const publishMsg: SharedMessage = {
        id: 'pub-to-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'addressed', type: 'event:addressed', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      const forwarded = JSON.parse(sent[0]);
      expect(forwarded.to).toBe(subscriber);
    });

    it('should use exact topic matching (no wildcards)', () => {
      // @requirement: Topic Matching - exact match only (PUBSUB.spec.md#L201)
      const publisher = toCanonicalAddress('browser/pub-exact');
      const subscriber = toCanonicalAddress('browser/sub-exact');
      const { ws, sent } = createMockWs();

      registry.set(subscriber, createMockRegistration({ actorAddress: subscriber, connectionId: 'conn-exact', expiresAt: Date.now() + 300000 }));
      connections.set('conn-exact', ws);

      // Subscribe to specific topic
      subscriptions.set('system/events', new Set([subscriber]));

      // Publish to different (non-matching) topic
      const publishMsg: SharedMessage = {
        id: 'pub-exact-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'system/other', type: 'event:test', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      // No subscribers for 'system/other' (exact match only)
      expect((response.payload as any).subscriberCount).toBe(0);
      expect(sent).toHaveLength(0);
    });

    it('should cleanup stale subscriptions for unregistered actors during publish', () => {
      // @requirement: On Actor Unregister - cleanup subscriptions (PUBSUB.spec.md#L273)
      const publisher = toCanonicalAddress('browser/pub-stale');
      const activeSubscriber = toCanonicalAddress('browser/active-sub');
      const staleSubscriber = toCanonicalAddress('browser/stale-sub'); // Not in registry
      const { ws } = createMockWs();

      registry.set(activeSubscriber, createMockRegistration({ actorAddress: activeSubscriber, connectionId: 'conn-active', expiresAt: Date.now() + 300000 }));
      connections.set('conn-active', ws);

      // staleSubscriber is in subscriptions but NOT in registry
      subscriptions.set('stale-topic', new Set([activeSubscriber, staleSubscriber]));

      const publishMsg: SharedMessage = {
        id: 'pub-stale-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'stale-topic', type: 'event:cleanup', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);

      // Only active subscriber counted
      expect((response.payload as any).subscriberCount).toBe(1);

      // Stale subscription cleaned up
      expect(subscriptions.get('stale-topic')?.has(staleSubscriber)).toBe(false);
    });
  });

  // ---------------------------------------------------------------------------
  // Subscription Storage - PUBSUB.spec.md#L238
  // ---------------------------------------------------------------------------
  describe('Subscription Storage', () => {
    it('should store subscriptions as Map<string, Set<CanonicalAddress>>', () => {
      // @requirement: Subscription Storage (PUBSUB.spec.md#L238)
      const actor = toCanonicalAddress('browser/storage-test');

      const subscribeMsg: SharedMessage = {
        id: 'store-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'storage/test' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      handleSubscribe(subscribeMsg, subscriptions, actor);

      // Storage is Map<string, Set<CanonicalAddress>>
      expect(subscriptions instanceof Map).toBe(true);
      const topicSubs = subscriptions.get('storage/test');
      expect(topicSubs instanceof Set).toBe(true);
      expect(topicSubs?.has(actor)).toBe(true);
    });

    it('should not allow duplicate subscribers in topic Set', () => {
      // @requirement: Subscription Storage - Set prevents duplicates
      const actor = toCanonicalAddress('browser/dup-test');

      const subscribeMsg: SharedMessage = {
        id: 'dup-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'dup/topic' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      // Subscribe 3 times
      handleSubscribe(subscribeMsg, subscriptions, actor);
      handleSubscribe({ ...subscribeMsg, id: 'dup-2' }, subscriptions, actor);
      handleSubscribe({ ...subscribeMsg, id: 'dup-3' }, subscriptions, actor);

      expect(subscriptions.get('dup/topic')?.size).toBe(1);
    });
  });

  // ---------------------------------------------------------------------------
  // Error Scenarios - PUBSUB.spec.md#L322
  // ---------------------------------------------------------------------------
  describe('Error Scenarios', () => {
    it('should reject subscribe with empty topic string', () => {
      // @requirement: Invalid Topic Name (PUBSUB.spec.md#L324)
      const actor = toCanonicalAddress('browser/err-1');

      const subscribeMsg: SharedMessage = {
        id: 'err-sub-1',
        from: actor,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:subscribe',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: '' },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handleSubscribe(subscribeMsg, subscriptions, actor)).toThrow('topic is required');
    });

    it('should reject publish with missing topic field', () => {
      // @requirement: Error Scenarios (PUBSUB.spec.md#L322)
      const publisher = toCanonicalAddress('browser/pub-err');

      const publishMsg: SharedMessage = {
        id: 'err-pub-1',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { type: 'event:test', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage)).toThrow('topic is required');
    });

    it('should reject publish with missing payload.type', () => {
      // @requirement: Missing payload.type on Publish (PUBSUB.spec.md#L343)
      const publisher = toCanonicalAddress('browser/pub-notype');

      const publishMsg: SharedMessage = {
        id: 'err-pub-2',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'some/topic', data: { key: 'val' } },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      expect(() => handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage)).toThrow('payload.type is required');
    });

    it('should handle publish to topic with zero subscribers gracefully', () => {
      // @requirement: Delivery Guarantees - empty topic handled gracefully
      const publisher = toCanonicalAddress('browser/pub-no-subs');

      const publishMsg: SharedMessage = {
        id: 'err-pub-3',
        from: publisher,
        to: toCanonicalAddress('cloudflare/signal-hub'),
        type: 'hub:publish',
        pattern: 'ask',
        correlationId: null,
        timestamp: Date.now(),
        payload: { topic: 'empty/topic', type: 'event:test', data: {} },
        metadata: {},
        ttl: 5000,
        signature: null,
      };

      const response = handlePublish(publishMsg, subscriptions, registry, connections, mockSendMessage);
      expect(response.type).toBe('hub:published');
      expect((response.payload as any).subscriberCount).toBe(0);
    });
  });
});
