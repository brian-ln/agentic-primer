#!/usr/bin/env bun
/**
 * Streaming Performance Benchmark
 * Measures overhead per item for AsyncIterator streaming
 */

import { MessageRouter } from './router.ts';
import { Actor } from './actor.ts';
import {
  type AsyncStreamMessage,
  address,
  generateCorrelationId,
  generateMessageId,
} from '@agentic-primer/actors';

// Simple streaming actor
class BenchmarkActor extends Actor {
  private itemCount: number;

  constructor(id: string, router: MessageRouter, itemCount: number) {
    super(id, router);
    this.itemCount = itemCount;
  }

  async receive() {
    return {
      id: generateMessageId(),
      correlationId: generateCorrelationId(),
      from: this.address,
      to: this.address,
      success: true,
      timestamp: Date.now(),
    };
  }

  async *streamAsync(payload: any): AsyncIterableIterator<AsyncStreamMessage<number>> {
    const items = Array.from({ length: this.itemCount }, (_, i) => i);
    yield* this.createAsyncStream(items);
  }
}

// Create mock store and manager
const store = {
  get: () => undefined,
  set: () => {},
} as any;

const programManager = {} as any;

async function benchmark() {
  const router = new MessageRouter(store, programManager);

  console.log('\n=== AsyncIterator Streaming Performance Benchmark ===\n');

  // Test 1: 1,000 items
  {
    const actor = new BenchmarkActor('bench-1k', router, 1000);
    router.registerActor('bench/1k', actor);

    const startTime = performance.now();
    let count = 0;

    for await (const msg of router.streamAsync(address('bench/1k'), 'test', {})) {
      if (msg.type === 'data') count++;
      if (msg.type === 'end') break;
    }

    const duration = performance.now() - startTime;
    const perItemMs = duration / 1000;
    const perItemUs = perItemMs * 1000;

    console.log(`Test 1: 1,000 items`);
    console.log(`  Total duration: ${duration.toFixed(2)}ms`);
    console.log(`  Per item: ${perItemMs.toFixed(4)}ms (${perItemUs.toFixed(2)}µs)`);
    console.log(`  Throughput: ${(1000 / (duration / 1000)).toFixed(0)} items/sec`);
    console.log(`  Target: <1µs per item - ${perItemUs < 1 ? '✓ PASS' : '✗ FAIL'}\n`);
  }

  // Test 2: 10,000 items
  {
    const actor = new BenchmarkActor('bench-10k', router, 10000);
    router.registerActor('bench/10k', actor);

    const startTime = performance.now();
    let count = 0;

    for await (const msg of router.streamAsync(address('bench/10k'), 'test', {})) {
      if (msg.type === 'data') count++;
      if (msg.type === 'end') break;
    }

    const duration = performance.now() - startTime;
    const perItemMs = duration / 10000;
    const perItemUs = perItemMs * 1000;

    console.log(`Test 2: 10,000 items`);
    console.log(`  Total duration: ${duration.toFixed(2)}ms`);
    console.log(`  Per item: ${perItemMs.toFixed(4)}ms (${perItemUs.toFixed(2)}µs)`);
    console.log(`  Throughput: ${(10000 / (duration / 1000)).toFixed(0)} items/sec`);
    console.log(`  Target: <1µs per item - ${perItemUs < 1 ? '✓ PASS' : '✗ FAIL'}\n`);
  }

  // Test 3: 100,000 items
  {
    const actor = new BenchmarkActor('bench-100k', router, 100000);
    router.registerActor('bench/100k', actor);

    const startTime = performance.now();
    let count = 0;

    for await (const msg of router.streamAsync(address('bench/100k'), 'test', {})) {
      if (msg.type === 'data') count++;
      if (msg.type === 'end') break;
    }

    const duration = performance.now() - startTime;
    const perItemMs = duration / 100000;
    const perItemUs = perItemMs * 1000;

    console.log(`Test 3: 100,000 items`);
    console.log(`  Total duration: ${duration.toFixed(2)}ms`);
    console.log(`  Per item: ${perItemMs.toFixed(4)}ms (${perItemUs.toFixed(2)}µs)`);
    console.log(`  Throughput: ${(100000 / (duration / 1000)).toFixed(0)} items/sec`);
    console.log(`  Target: <1µs per item - ${perItemUs < 1 ? '✓ PASS' : '✗ FAIL'}\n`);
  }

  // Test 4: With backpressure (slow consumer)
  {
    const actor = new BenchmarkActor('bench-bp', router, 1000);
    router.registerActor('bench/backpressure', actor);

    const startTime = performance.now();
    let count = 0;

    for await (const msg of router.streamAsync(address('bench/backpressure'), 'test', {}, {
      bufferSize: 50,
    })) {
      if (msg.type === 'data') {
        count++;
        // Simulate slow consumer: 0.5ms per item
        await new Promise((resolve) => setTimeout(resolve, 0.5));
      }
      if (msg.type === 'end') break;
    }

    const duration = performance.now() - startTime;

    console.log(`Test 4: 1,000 items with backpressure (0.5ms consumer)`);
    console.log(`  Total duration: ${duration.toFixed(2)}ms`);
    console.log(`  Expected: ~500ms (0.5ms × 1000)`);
    console.log(`  Overhead: ${(duration - 500).toFixed(2)}ms`);
    console.log(`  Per item overhead: ${((duration - 500) / 1000).toFixed(4)}ms\n`);
  }

  console.log('=== Benchmark Complete ===\n');
}

benchmark().catch(console.error);
