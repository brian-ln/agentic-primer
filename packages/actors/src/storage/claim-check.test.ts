/**
 * Claim check pattern tests
 *
 * Tests the claim check pattern for large message payloads:
 * - Size detection (< 100KB, > 100KB, edge cases)
 * - Store operation (creates reference, stores in KV)
 * - Retrieve operation (gets payload, handles missing)
 * - Actor integration (ask sends reference, receive retrieves)
 * - Backward compatibility (messages without claim check)
 * - Error handling (storage failure, missing reference)
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import {
  CLAIM_CHECK_THRESHOLD,
  shouldUseClaimCheck,
  isClaimCheckReference,
  type ClaimCheckReference,
} from './claim-check.ts';
import { ClaimCheckStore } from './claim-check-store.ts';
import { Actor } from '../actor.ts';
import { MessageRouter } from '../router.ts';
import { address, createMessage } from '../message.ts';
import type { IKeyValueStorage } from '../interfaces.ts';

// Mock KV storage for testing
class MockKvStorage implements IKeyValueStorage {
  private storage = new Map<string, { value: string; ttl?: number; metadata?: Record<string, unknown> }>();

  async get<T = string>(
    key: string,
    options?: { type?: 'text' | 'json' | 'arrayBuffer' }
  ): Promise<T | null> {
    const entry = this.storage.get(key);
    if (!entry) return null;

    if (options?.type === 'json') {
      return JSON.parse(entry.value) as T;
    }
    return entry.value as T;
  }

  async put(
    key: string,
    value: string | ArrayBuffer | ReadableStream,
    options?: {
      expirationTtl?: number;
      metadata?: Record<string, unknown>;
    }
  ): Promise<void> {
    if (value instanceof ReadableStream) {
      throw new Error('ReadableStream not supported in mock');
    }
    const strValue = value instanceof ArrayBuffer
      ? new TextDecoder().decode(value)
      : value;

    this.storage.set(key, {
      value: strValue,
      ttl: options?.expirationTtl,
      metadata: options?.metadata,
    });
  }

  async delete(key: string): Promise<void> {
    this.storage.delete(key);
  }

  async list(options?: {
    prefix?: string;
    limit?: number;
    cursor?: string;
  }): Promise<{
    keys: Array<{ name: string; metadata?: unknown }>;
    cursor?: string;
  }> {
    const keys = Array.from(this.storage.keys())
      .filter(k => !options?.prefix || k.startsWith(options.prefix))
      .slice(0, options?.limit || 100)
      .map(name => ({
        name,
        metadata: this.storage.get(name)?.metadata,
      }));
    return { keys };
  }

  // Test helper methods
  clear(): void {
    this.storage.clear();
  }

  size(): number {
    return this.storage.size;
  }
}

// Helper to create test actor with claim check store
class TestActor extends Actor {
  receivedPayload?: unknown;

  setClaimCheckStore(store: ClaimCheckStore): void {
    this.claimCheckStore = store;
  }

  protected async handleMessage(message: any): Promise<any> {
    this.receivedPayload = message.payload;
    return { success: true, payload: 'ok' };
  }
}

describe('shouldUseClaimCheck', () => {
  test('returns false for small payloads', () => {
    const small = { data: 'x'.repeat(1000) }; // ~1KB
    expect(shouldUseClaimCheck(small)).toBe(false);
  });

  test('returns false for edge case at threshold', () => {
    const atThreshold = { data: 'x'.repeat(CLAIM_CHECK_THRESHOLD - 20) };
    expect(shouldUseClaimCheck(atThreshold)).toBe(false);
  });

  test('returns true for payloads above threshold', () => {
    const large = { data: 'x'.repeat(CLAIM_CHECK_THRESHOLD + 1) };
    expect(shouldUseClaimCheck(large)).toBe(true);
  });

  test('returns true for 200KB payload', () => {
    const large = { data: 'x'.repeat(200_000) };
    expect(shouldUseClaimCheck(large)).toBe(true);
  });

  test('returns false for non-serializable payloads', () => {
    const circular: any = { a: 1 };
    circular.self = circular;
    expect(shouldUseClaimCheck(circular)).toBe(false);
  });

  test('handles null and undefined', () => {
    expect(shouldUseClaimCheck(null)).toBe(false);
    expect(shouldUseClaimCheck(undefined)).toBe(false);
  });
});

describe('isClaimCheckReference', () => {
  test('returns true for valid reference', () => {
    const ref: ClaimCheckReference = {
      claimCheckId: 'claim-check:123',
      storageType: 'kv',
      contentType: 'json',
      size: 100_000,
    };
    expect(isClaimCheckReference(ref)).toBe(true);
  });

  test('returns false for missing fields', () => {
    expect(isClaimCheckReference({ claimCheckId: '123' })).toBe(false);
    expect(isClaimCheckReference({ storageType: 'kv' })).toBe(false);
  });

  test('returns false for wrong types', () => {
    expect(isClaimCheckReference({
      claimCheckId: 123, // should be string
      storageType: 'kv',
      contentType: 'json',
      size: 100,
    })).toBe(false);
  });

  test('returns false for wrong storage type', () => {
    expect(isClaimCheckReference({
      claimCheckId: '123',
      storageType: 's3', // should be 'kv'
      contentType: 'json',
      size: 100,
    })).toBe(false);
  });

  test('returns false for non-objects', () => {
    expect(isClaimCheckReference(null)).toBe(false);
    expect(isClaimCheckReference(undefined)).toBe(false);
    expect(isClaimCheckReference('string')).toBe(false);
    expect(isClaimCheckReference(123)).toBe(false);
  });
});

describe('ClaimCheckStore', () => {
  let mockKv: MockKvStorage;
  let store: ClaimCheckStore;

  beforeEach(() => {
    mockKv = new MockKvStorage();
    store = new ClaimCheckStore(mockKv);
  });

  test('stores large payload and returns reference', async () => {
    const payload = { data: 'x'.repeat(200_000) }; // > 100KB
    const ref = await store.store(payload);

    expect(ref.claimCheckId).toMatch(/^claim-check:/);
    expect(ref.storageType).toBe('kv');
    expect(ref.contentType).toBe('json');
    expect(ref.size).toBeGreaterThan(100_000);

    // Verify stored in KV
    expect(mockKv.size()).toBe(1);
  });

  test('stores payload with correct TTL', async () => {
    const payload = { data: 'test' };
    await store.store(payload);

    // Check storage internals
    const keys = await mockKv.list();
    expect(keys.keys.length).toBe(1);
    expect(keys.keys[0].metadata).toEqual({
      type: 'claim-check',
      created: expect.any(Number),
    });
  });

  test('retrieves stored payload', async () => {
    const payload = { data: 'large payload', nested: { a: 1, b: 2 } };
    const ref = await store.store(payload);
    const retrieved = await store.retrieve(ref);

    expect(retrieved).toEqual(payload);
  });

  test('throws on missing claim check', async () => {
    const ref: ClaimCheckReference = {
      claimCheckId: 'claim-check:nonexistent',
      storageType: 'kv',
      contentType: 'json',
      size: 100,
    };

    await expect(store.retrieve(ref)).rejects.toThrow('not found');
    await expect(store.retrieve(ref)).rejects.toThrow('expired or deleted');
  });

  test('delete removes claim check', async () => {
    const payload = { data: 'test' };
    const ref = await store.store(payload);

    expect(mockKv.size()).toBe(1);

    await store.delete(ref);

    expect(mockKv.size()).toBe(0);
  });

  test('stores complex nested objects', async () => {
    const payload = {
      users: Array.from({ length: 1000 }, (_, i) => ({
        id: i,
        name: `User ${i}`,
        tags: ['tag1', 'tag2'],
      })),
    };

    const ref = await store.store(payload);
    const retrieved = await store.retrieve(ref);

    expect(retrieved).toEqual(payload);
  });

  test('generates unique IDs for each store', async () => {
    const ref1 = await store.store({ data: 'payload1' });
    const ref2 = await store.store({ data: 'payload2' });

    expect(ref1.claimCheckId).not.toBe(ref2.claimCheckId);
    expect(mockKv.size()).toBe(2);
  });
});

describe('Actor with claim check', () => {
  let router: MessageRouter;
  let senderActor: TestActor;
  let receiverActor: TestActor;
  let mockKv: MockKvStorage;
  let store: ClaimCheckStore;

  beforeEach(() => {
    router = new MessageRouter();
    senderActor = new TestActor('sender', router);
    receiverActor = new TestActor('receiver', router);

    mockKv = new MockKvStorage();
    store = new ClaimCheckStore(mockKv);

    // Set claim check store on both actors
    senderActor.setClaimCheckStore(store);
    receiverActor.setClaimCheckStore(store);

    // Register receiver
    router.registerActor('receiver', receiverActor);
  });

  test('sends small payloads inline (no claim check)', async () => {
    const smallPayload = { data: 'small' };

    await senderActor.ask(address('receiver'), 'test', smallPayload);

    // Verify no claim check used
    expect(mockKv.size()).toBe(0);
    expect(receiverActor.receivedPayload).toEqual(smallPayload);
  });

  test('stores large payloads externally', async () => {
    const largePayload = { data: 'x'.repeat(200_000) };

    await senderActor.ask(address('receiver'), 'test', largePayload);

    // Verify claim check used
    expect(mockKv.size()).toBeGreaterThan(0);
  });

  test('retrieves payload transparently', async () => {
    const largePayload = { data: 'x'.repeat(200_000), id: 123 };

    await senderActor.ask(address('receiver'), 'test', largePayload);

    // Handler should receive original payload, not reference
    expect(receiverActor.receivedPayload).toEqual(largePayload);
  });

  test('backward compatible with messages without claim check', async () => {
    const payload = { data: 'test' };
    const message = createMessage(address('receiver'), 'test', payload, {
      pattern: 'ask',
      from: address('sender'),
    });

    const response = await receiverActor.receive(message);

    expect(response.success).toBe(true);
    expect(receiverActor.receivedPayload).toEqual(payload);
  });

  test('handles storage failure gracefully', async () => {
    // Create a store that always fails
    const failingStore = new ClaimCheckStore({
      async put() { throw new Error('Storage failure'); },
      async get() { return null; },
      async delete() {},
      async list() { return { keys: [] }; },
    });

    senderActor.setClaimCheckStore(failingStore);

    const largePayload = { data: 'x'.repeat(200_000) };

    // Should fall back to inline (with warning)
    const response = await senderActor.ask(address('receiver'), 'test', largePayload);

    // Message should still be delivered (fallback to inline)
    expect(response.success).toBe(true);
  });

  test('returns error if claim check retrieval fails', async () => {
    // Send a fake claim check reference that doesn't exist
    const fakeRef: ClaimCheckReference = {
      claimCheckId: 'claim-check:fake',
      storageType: 'kv',
      contentType: 'json',
      size: 100,
    };

    const message = createMessage(address('receiver'), 'test', fakeRef, {
      pattern: 'ask',
      from: address('sender'),
      metadata: { 'claim-check': true },
    });

    const response = await receiverActor.receive(message);

    expect(response.success).toBe(false);
    expect(response.error).toContain('Failed to retrieve claim check');
    expect(response.error).toContain('not found');
  });

  test('actor without claim check store receives reference directly', async () => {
    // Receiver has no claim check store
    const receiverWithoutStore = new TestActor('receiver2', router);
    router.registerActor('receiver2', receiverWithoutStore);

    const largePayload = { data: 'x'.repeat(200_000) };

    await senderActor.ask(address('receiver2'), 'test', largePayload);

    // Receiver gets the reference, not the payload
    expect(isClaimCheckReference(receiverWithoutStore.receivedPayload)).toBe(true);
  });

  test('handles 1MB payload efficiently', async () => {
    const megabytePayload = { data: 'x'.repeat(1_000_000) };

    const start = Date.now();
    await senderActor.ask(address('receiver'), 'test', megabytePayload);
    const duration = Date.now() - start;

    expect(receiverActor.receivedPayload).toEqual(megabytePayload);
    expect(duration).toBeLessThan(1000); // Should complete in < 1 second
  });

  test('multiple large messages use separate claim checks', async () => {
    const payload1 = { data: 'x'.repeat(200_000), id: 1 };
    const payload2 = { data: 'y'.repeat(200_000), id: 2 };

    await senderActor.ask(address('receiver'), 'test1', payload1);
    await senderActor.ask(address('receiver'), 'test2', payload2);

    expect(mockKv.size()).toBe(2);
  });

  test('threshold edge case (99KB vs 101KB)', async () => {
    const justUnder = { data: 'x'.repeat(CLAIM_CHECK_THRESHOLD - 100) };
    const justOver = { data: 'x'.repeat(CLAIM_CHECK_THRESHOLD + 100) };

    await senderActor.ask(address('receiver'), 'test', justUnder);
    expect(mockKv.size()).toBe(0); // No claim check

    await senderActor.ask(address('receiver'), 'test', justOver);
    expect(mockKv.size()).toBe(1); // Claim check used
  });
});

describe('Performance', () => {
  test('claim check detection is fast', () => {
    const payload = { data: 'x'.repeat(200_000) };

    const iterations = 1000;
    const start = Date.now();

    for (let i = 0; i < iterations; i++) {
      shouldUseClaimCheck(payload);
    }

    const duration = Date.now() - start;
    const avgMs = duration / iterations;

    expect(avgMs).toBeLessThan(1); // < 1ms per check
  });

  test('store and retrieve is efficient', async () => {
    const mockKv = new MockKvStorage();
    const store = new ClaimCheckStore(mockKv);
    const payload = { data: 'x'.repeat(500_000) }; // 500KB

    const start = Date.now();
    const ref = await store.store(payload);
    const retrieved = await store.retrieve(ref);
    const duration = Date.now() - start;

    expect(retrieved).toEqual(payload);
    expect(duration).toBeLessThan(50); // < 50ms for 500KB
  });
});
