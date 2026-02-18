/**
 * Security & Correctness Tests (ayo WS2.1)
 *
 * Tests for:
 * 1. Per-session rate limiting (10 msg/sec, RATE_LIMIT_EXCEEDED)
 * 2. Topic-specific unsubscribe
 * 3. Target address resolution (deterministic last-write-wins)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createTokenBucket, consumeToken, toCanonicalAddress } from '../../utils';
import { handleUnsubscribe } from '../pubsub';
import { handleSend } from '../messaging';
import type { SharedMessage, ActorRegistration, Env, CanonicalAddress, TokenBucket } from '../../types';

// ============================================================
// 1. Rate Limiting Tests
// ============================================================

describe('Rate Limiting (10 msg/sec per session)', () => {
  it('should allow up to 10 messages before exhausting bucket', () => {
    // Token bucket: 10 capacity, 10 tokens/sec refill
    const bucket: TokenBucket = createTokenBucket(10, 10);

    let successCount = 0;
    for (let i = 0; i < 10; i++) {
      if (consumeToken(bucket)) {
        successCount++;
      }
    }

    expect(successCount).toBe(10);
  });

  it('should reject the 11th message when bucket is empty', () => {
    const bucket: TokenBucket = createTokenBucket(10, 10);

    // Drain all 10 tokens
    for (let i = 0; i < 10; i++) {
      consumeToken(bucket);
    }

    // 11th should fail — bucket is empty
    const allowed = consumeToken(bucket);
    expect(allowed).toBe(false);
  });

  it('should start fresh with 10 tokens (not zero)', () => {
    const bucket: TokenBucket = createTokenBucket(10, 10);
    expect(bucket.tokens).toBe(10);
    expect(bucket.capacity).toBe(10);
    expect(bucket.refillRate).toBe(10);
  });

  it('should refill tokens over time', () => {
    const bucket: TokenBucket = createTokenBucket(10, 10);

    // Drain all tokens
    for (let i = 0; i < 10; i++) {
      consumeToken(bucket);
    }
    expect(bucket.tokens).toBeLessThan(1);

    // Simulate 0.5 seconds passing (5 tokens should be refilled)
    bucket.lastRefill = Date.now() - 500;

    // Should be able to consume again after refill
    const allowed = consumeToken(bucket);
    expect(allowed).toBe(true);
  });

  it('should not exceed capacity when refilling', () => {
    const bucket: TokenBucket = createTokenBucket(10, 10);

    // Simulate 10 seconds passing — would add 100 tokens without cap
    bucket.lastRefill = Date.now() - 10000;

    consumeToken(bucket); // triggers refill

    // Tokens should be capped at capacity (10), not 100
    expect(bucket.tokens).toBeLessThanOrEqual(10);
  });

  it('should report correct limit for 10 msg/sec configuration', () => {
    const bucket: TokenBucket = createTokenBucket(10, 10);

    // Verify the configuration matches the spec
    expect(bucket.capacity).toBe(10);
    expect(bucket.refillRate).toBe(10); // 10 tokens per second
  });
});

// ============================================================
// 2. Topic-Specific Unsubscribe Tests
// ============================================================

describe('Topic-Specific Unsubscribe', () => {
  let subscriptions: Map<string, Set<CanonicalAddress>>;

  beforeEach(() => {
    subscriptions = new Map();
  });

  const makeUnsubscribeMsg = (topic: string, from: CanonicalAddress): SharedMessage => ({
    id: `unsub-${topic}`,
    from,
    to: toCanonicalAddress('cloudflare/signal-hub'),
    type: 'hub:unsubscribe',
    pattern: 'tell',
    correlationId: null,
    timestamp: Date.now(),
    payload: { topic },
    metadata: {},
    ttl: null,
    signature: null,
  });

  it('should only remove actor from the specified topic', () => {
    const actor = toCanonicalAddress('browser/test-actor');

    // Subscribe to two topics
    const set1 = new Set<CanonicalAddress>([actor]);
    const set2 = new Set<CanonicalAddress>([actor]);
    subscriptions.set('topic-a', set1);
    subscriptions.set('topic-b', set2);

    // Unsubscribe from topic-a only
    handleUnsubscribe(makeUnsubscribeMsg('topic-a', actor), subscriptions, actor);

    // topic-a gone, topic-b intact
    expect(subscriptions.has('topic-a')).toBe(false);
    expect(subscriptions.has('topic-b')).toBe(true);
    expect(subscriptions.get('topic-b')?.has(actor)).toBe(true);
  });

  it('should preserve other actors on the same topic', () => {
    const actor1 = toCanonicalAddress('browser/actor-1');
    const actor2 = toCanonicalAddress('browser/actor-2');

    const set = new Set<CanonicalAddress>([actor1, actor2]);
    subscriptions.set('shared-topic', set);

    handleUnsubscribe(makeUnsubscribeMsg('shared-topic', actor1), subscriptions, actor1);

    // actor1 removed but actor2 and topic remain
    expect(subscriptions.has('shared-topic')).toBe(true);
    expect(subscriptions.get('shared-topic')?.has(actor1)).toBe(false);
    expect(subscriptions.get('shared-topic')?.has(actor2)).toBe(true);
  });

  it('should not affect subscriptions on other topics for same actor', () => {
    const actor = toCanonicalAddress('browser/multi-topic-actor');
    const topics = ['alerts', 'updates', 'system'];

    for (const t of topics) {
      subscriptions.set(t, new Set<CanonicalAddress>([actor]));
    }

    // Unsubscribe from 'alerts' only
    handleUnsubscribe(makeUnsubscribeMsg('alerts', actor), subscriptions, actor);

    expect(subscriptions.has('alerts')).toBe(false);
    expect(subscriptions.has('updates')).toBe(true);
    expect(subscriptions.has('system')).toBe(true);
    expect(subscriptions.get('updates')?.has(actor)).toBe(true);
    expect(subscriptions.get('system')?.has(actor)).toBe(true);
  });

  it('should require topic field in payload', () => {
    const actor = toCanonicalAddress('browser/actor');
    const badMsg: SharedMessage = {
      id: 'bad',
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

    expect(() => handleUnsubscribe(badMsg, subscriptions, actor)).toThrow('topic is required');
  });
});

// ============================================================
// 3. Target Address Resolution Tests
// ============================================================

describe('Target Address Resolution (deterministic last-write-wins)', () => {
  let registry: Map<string, ActorRegistration>;
  let connections: Map<string, WebSocket>;
  let mockEnv: Env;

  function makeMockWs(): WebSocket & { _sent: string[] } {
    const ws = { _sent: [] as string[] } as unknown as WebSocket & { _sent: string[] };
    (ws as any).send = (data: string) => ws._sent.push(data);
    return ws;
  }

  function makeRegistration(
    address: CanonicalAddress,
    connectionId: string,
    overrides: Partial<ActorRegistration> = {}
  ): ActorRegistration {
    return {
      actorAddress: address,
      capabilities: ['render'],
      metadata: {},
      connectionId,
      registeredAt: Date.now(),
      expiresAt: Date.now() + 300000,
      version: 1,
      renewalToken: 'token',
      ...overrides,
    };
  }

  function makeSendMsg(to: CanonicalAddress, from: CanonicalAddress): SharedMessage {
    return {
      id: 'send-test',
      from,
      to: toCanonicalAddress('cloudflare/signal-hub'),
      type: 'hub:send',
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        to,
        type: 'test:ping',
        data: { hello: true },
      },
      metadata: {},
      ttl: null,
      signature: null,
    };
  }

  const mockSendMessage = (ws: WebSocket, msg: SharedMessage) => ws.send(JSON.stringify(msg));

  beforeEach(() => {
    registry = new Map();
    connections = new Map();
    mockEnv = {
      SIGNAL_HUB: {} as DurableObjectNamespace,
      PROTOCOL_VERSION: '0.1.0',
      MAX_MESSAGE_SIZE: '1048576',
      HEARTBEAT_INTERVAL: '30000',
      ACTOR_REGISTRY_LIMIT: '50000',
      DEFAULT_ACTOR_TTL: '300000',
      MAX_ACTOR_TTL: '3600000',
      BROADCAST_SYNC_THRESHOLD: '100',
      AUTH_ENABLED: 'false',
    };
  });

  it('should route to the most recent registration when same address re-registers', () => {
    const actorAddr = toCanonicalAddress('browser/reconnecting-actor');
    const oldWs = makeMockWs();
    const newWs = makeMockWs();

    // Old registration (session-1)
    registry.set(actorAddr, makeRegistration(actorAddr, 'session-1', { version: 1 }));
    connections.set('session-1', oldWs as unknown as WebSocket);

    // New registration overwrites old (session-2 — last-write-wins)
    registry.set(actorAddr, makeRegistration(actorAddr, 'session-2', { version: 2 }));
    connections.set('session-2', newWs as unknown as WebSocket);

    const response = handleSend(
      makeSendMsg(actorAddr, toCanonicalAddress('browser/sender')),
      registry,
      connections,
      mockEnv,
      mockSendMessage
    );

    // Message delivered to new session only
    expect(response?.type).toBe('hub:delivery_ack');
    expect(newWs._sent.length).toBe(1);
    expect(oldWs._sent.length).toBe(0);
  });

  it('should route to single unambiguous registration', () => {
    const actorAddr = toCanonicalAddress('browser/unique-actor');
    const ws = makeMockWs();

    registry.set(actorAddr, makeRegistration(actorAddr, 'session-x'));
    connections.set('session-x', ws as unknown as WebSocket);

    const response = handleSend(
      makeSendMsg(actorAddr, toCanonicalAddress('browser/sender')),
      registry,
      connections,
      mockEnv,
      mockSendMessage
    );

    expect(response?.type).toBe('hub:delivery_ack');
    expect(ws._sent.length).toBe(1);
  });

  it('should return hub:unknown_actor for unregistered address (ask pattern)', () => {
    const unknownAddr = toCanonicalAddress('browser/ghost-actor');

    const response = handleSend(
      makeSendMsg(unknownAddr, toCanonicalAddress('browser/sender')),
      registry,
      connections,
      mockEnv,
      mockSendMessage
    );

    expect(response?.type).toBe('hub:unknown_actor');
    expect((response?.payload as any).actorAddress).toBe(unknownAddr);
  });
});
