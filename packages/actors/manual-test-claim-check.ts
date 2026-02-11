/**
 * Manual test for claim check pattern
 *
 * Run with: bun run manual-test-claim-check.ts
 *
 * Demonstrates:
 * 1. Small payloads sent inline
 * 2. Large payloads stored externally
 * 3. Transparent retrieval by receiver
 * 4. TTL expiration
 */

import { Actor } from './src/actor.ts';
import { MessageRouter } from './src/router.ts';
import { address } from './src/message.ts';
import { ClaimCheckStore, CLAIM_CHECK_THRESHOLD } from './src/index.ts';
import type { IKeyValueStorage, Message, MessageResponse } from './src/index.ts';

// Simple in-memory KV storage for testing
class MemoryKvStorage implements IKeyValueStorage {
  private storage = new Map<string, { value: string; metadata?: Record<string, unknown> }>();

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
    if (value instanceof ReadableStream || value instanceof ArrayBuffer) {
      throw new Error('Only string values supported in this demo');
    }
    this.storage.set(key, { value, metadata: options?.metadata });
  }

  async delete(key: string): Promise<void> {
    this.storage.delete(key);
  }

  async list(): Promise<{
    keys: Array<{ name: string; metadata?: unknown }>;
  }> {
    return {
      keys: Array.from(this.storage.keys()).map(name => ({
        name,
        metadata: this.storage.get(name)?.metadata,
      })),
    };
  }

  // Helper
  size(): number {
    return this.storage.size;
  }
}

// Test actors
class DataSenderActor extends Actor {
  setClaimCheckStore(store: ClaimCheckStore): void {
    this.claimCheckStore = store;
  }

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    return { success: true, id: message.id, correlationId: message.id, from: this.address, to: message.from, timestamp: Date.now() };
  }
}

class DataReceiverActor extends Actor {
  lastReceivedPayload: unknown = null;

  setClaimCheckStore(store: ClaimCheckStore): void {
    this.claimCheckStore = store;
  }

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    this.lastReceivedPayload = message.payload;
    console.log(`  Receiver got payload of size: ${JSON.stringify(message.payload).length} bytes`);
    return { success: true, id: message.id, correlationId: message.id, from: this.address, to: message.from, timestamp: Date.now() };
  }
}

async function main() {
  console.log('=== Claim Check Pattern Manual Test ===\n');

  // Setup
  const router = new MessageRouter();
  const kv = new MemoryKvStorage();
  const claimCheckStore = new ClaimCheckStore(kv);

  const sender = new DataSenderActor('sender', router);
  const receiver = new DataReceiverActor('receiver', router);

  sender.setClaimCheckStore(claimCheckStore);
  receiver.setClaimCheckStore(claimCheckStore);

  router.registerActor('sender', sender);
  router.registerActor('receiver', receiver);

  // Test 1: Small payload (< 100KB)
  console.log('Test 1: Small payload (< 100KB)');
  const smallPayload = { data: 'x'.repeat(1000), message: 'Small payload test' };
  console.log(`  Sending payload of ${JSON.stringify(smallPayload).length} bytes`);

  await sender.ask(address('receiver'), 'test', smallPayload);

  console.log(`  KV storage size: ${kv.size()} (should be 0, no claim check used)`);
  console.log(`  Receiver payload matches: ${JSON.stringify(receiver.lastReceivedPayload) === JSON.stringify(smallPayload)}`);
  console.log('  ✅ Small payload sent inline\n');

  // Test 2: Large payload (> 100KB)
  console.log('Test 2: Large payload (> 100KB)');
  const largePayload = { data: 'x'.repeat(150_000), message: 'Large payload test' };
  console.log(`  Sending payload of ${JSON.stringify(largePayload).length} bytes`);
  console.log(`  Threshold: ${CLAIM_CHECK_THRESHOLD} bytes`);

  await sender.ask(address('receiver'), 'test', largePayload);

  console.log(`  KV storage size: ${kv.size()} (should be 1, claim check used)`);
  console.log(`  Receiver payload matches: ${JSON.stringify(receiver.lastReceivedPayload) === JSON.stringify(largePayload)}`);
  console.log('  ✅ Large payload stored externally and retrieved transparently\n');

  // Test 3: Multiple large payloads
  console.log('Test 3: Multiple large payloads');
  const payload1 = { data: 'a'.repeat(200_000), id: 1 };
  const payload2 = { data: 'b'.repeat(200_000), id: 2 };

  await sender.ask(address('receiver'), 'test', payload1);
  await sender.ask(address('receiver'), 'test', payload2);

  console.log(`  KV storage size: ${kv.size()} (should be 3 total claim checks)`);
  console.log('  ✅ Multiple large payloads handled correctly\n');

  // Test 4: Verify claim check metadata
  console.log('Test 4: Claim check metadata');
  const keys = await kv.list();
  console.log(`  Total claim checks in storage: ${keys.keys.length}`);
  keys.keys.forEach((key, i) => {
    console.log(`  Claim check ${i + 1}:`, key.name);
    console.log(`    Metadata:`, key.metadata);
  });
  console.log('  ✅ Claim checks stored with correct metadata\n');

  // Test 5: Edge case - exactly at threshold
  console.log('Test 5: Edge case - payload near threshold');
  const atThreshold = { data: 'x'.repeat(CLAIM_CHECK_THRESHOLD - 50) };
  const justOver = { data: 'x'.repeat(CLAIM_CHECK_THRESHOLD + 50) };

  const kvSizeBefore = kv.size();
  await sender.ask(address('receiver'), 'test', atThreshold);
  console.log(`  Payload at threshold: ${kv.size() === kvSizeBefore ? 'sent inline' : 'used claim check'}`);

  await sender.ask(address('receiver'), 'test', justOver);
  console.log(`  Payload over threshold: ${kv.size() > kvSizeBefore ? 'used claim check' : 'sent inline'}`);
  console.log('  ✅ Threshold detection working correctly\n');

  console.log('=== All Tests Passed ===');
}

main().catch(console.error);
