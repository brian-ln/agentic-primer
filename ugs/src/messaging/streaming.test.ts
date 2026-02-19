#!/usr/bin/env bun
/**
 * AsyncIterator Streaming Tests
 * Tests for AsyncIterator-based streaming with backpressure and cancellation
 * Target: 100% coverage of streaming functionality
 */

import { test, expect, describe, beforeEach } from 'bun:test';
import { MessageRouter } from './router.ts';
import { Actor } from './actor.ts';
import {
  type AsyncStreamMessage,
  type StreamAsyncOptions,
  address,
  generateCorrelationId,
  generateMessageId,
  createMessage,
  createResponse,
} from '@agentic-primer/actors';
import type GraphStore from '@src/graph.ts';
import type { ProgramManager } from '@src/entities/program.ts';
import { SchedulerActor, VirtualClock } from '@src/system-actors/scheduler.ts';
import type { Message, MessageResponse } from '@agentic-primer/actors';

// Create mock GraphStore
function createMockGraphStore(): GraphStore {
  const nodes = new Map();
  return {
    get: (id: string) => nodes.get(id),
    set: (id: string, data: any) => nodes.set(id, data),
    has: (id: string) => nodes.has(id),
    delete: (id: string) => nodes.delete(id),
    clear: () => nodes.clear(),
  } as any as GraphStore;
}

// Create mock ProgramManager
function createMockProgramManager(): ProgramManager {
  return {
    invokeProgram: async (id: string, params: any) => ({
      success: true,
      output: `Program ${id} executed`,
    }),
  } as any as ProgramManager;
}

// Mock streaming actor with AsyncIterator support
// NOTE: Uses SchedulerActor when delay > 0 for testability with virtual time
class MockStreamingActor extends Actor {
  private items: any[];
  private delayMs: number;
  private useScheduler: boolean;
  private resolveDelay: (() => void) | null = null;

  constructor(id: string, router: MessageRouter, items: any[] = [], delayMs: number = 0, useScheduler: boolean = false) {
    super(id, router);
    this.items = items;
    this.delayMs = delayMs;
    this.useScheduler = useScheduler;
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Handle delay completion from SchedulerActor
    if (message.type === 'stream.delay-complete') {
      if (this.resolveDelay) {
        this.resolveDelay();
        this.resolveDelay = null;
      }
      return createResponse(message, { success: true });
    }

    return {
      id: generateMessageId(),
      correlationId: generateCorrelationId(),
      from: this.address,
      to: this.address,
      success: true,
      timestamp: Date.now(),
    };
  }

  async *streamAsync<T>(payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
    const corrId = generateCorrelationId();

    try {
      for (const item of this.items) {
        if (this.delayMs > 0) {
          if (this.useScheduler) {
            // Use SchedulerActor for delays (virtual time compatible)
            await new Promise<void>((resolve) => {
              this.resolveDelay = resolve;
              this.schedule(this.delayMs, 'stream.delay-complete').catch((err) => {
                console.error('Schedule failed:', err);
                resolve(); // Fallback to continue streaming
              });
            });
          } else {
            // Use real setTimeout (for compatibility with existing tests)
            await new Promise((resolve) => setTimeout(resolve, this.delayMs));
          }
        }

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: item,
          timestamp: Date.now(),
        };
      }

      // Emit end
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'end',
        timestamp: Date.now(),
      };
    } catch (error: any) {
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'error',
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }
}

// Mock actor that throws errors during streaming
class ErrorStreamingActor extends Actor {
  constructor(id: string, router: MessageRouter) {
    super(id, router);
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

  async *streamAsync<T>(payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
    const corrId = generateCorrelationId();

    yield {
      id: generateMessageId(),
      correlationId: corrId,
      from: this.address,
      type: 'data',
      payload: 'item1' as any,
      timestamp: Date.now(),
    };

    // Throw error mid-stream
    throw new Error('Stream processing failed');
  }
}

describe('MessageRouter - streamAsync() Basic Functionality', () => {
  let router: MessageRouter;
  let store: GraphStore;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('streamAsync() yields data events from actor', async () => {
    const items = ['item1', 'item2', 'item3'];
    const actor = new MockStreamingActor('stream-actor', router, items);
    router.registerActor('stream-actor', actor);

    const results: AsyncStreamMessage<string>[] = [];
    for await (const msg of router.streamAsync(address('stream-actor'), 'test', {})) {
      results.push(msg);
    }

    // Should have 3 data + 1 end
    expect(results).toHaveLength(4);
    expect(results[0].type).toBe('data');
    expect(results[0].payload).toBe('item1');
    expect(results[1].payload).toBe('item2');
    expect(results[2].payload).toBe('item3');
    expect(results[3].type).toBe('end');
  });

  test('streamAsync() throws if actor not found', async () => {
    await expect(async () => {
      for await (const msg of router.streamAsync(address('non-existent'), 'test', {})) {
        // Should not get here
      }
    }).toThrow(/Node not found/);
  });

  test('streamAsync() throws if actor has no streamAsync method', async () => {
    const actor = {
      async receive() {
        return {
          id: generateMessageId(),
          correlationId: generateCorrelationId(),
          from: address('no-stream'),
          to: address('no-stream'),
          success: true,
          timestamp: Date.now(),
        };
      },
    };
    router.registerActor('no-stream', actor);

    await expect(async () => {
      for await (const msg of router.streamAsync(address('no-stream'), 'test', {})) {
        // Should not get here
      }
    }).toThrow(/does not support async streaming/);
  });

  test('streamAsync() handles empty stream', async () => {
    const actor = new MockStreamingActor('empty-actor', router, []);
    router.registerActor('empty-actor', actor);

    const results: AsyncStreamMessage<any>[] = [];
    for await (const msg of router.streamAsync(address('empty-actor'), 'test', {})) {
      results.push(msg);
    }

    // Should only have end event
    expect(results).toHaveLength(1);
    expect(results[0].type).toBe('end');
  });
});

describe('MessageRouter - streamAsync() Backpressure Handling', () => {
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('streamAsync() buffers items when consumer is slow', async () => {
    // Create actor that produces items fast
    const items = Array.from({ length: 1000 }, (_, i) => `item${i}`);
    const actor = new MockStreamingActor('fast-producer', router, items, 0, false);
    router.registerActor('fast-producer', actor);

    const results: AsyncStreamMessage<string>[] = [];
    const startTime = Date.now();

    // Consume slowly (simulate slow processing)
    for await (const msg of router.streamAsync(address('fast-producer'), 'test', {}, {
      bufferSize: 100,
    })) {
      if (msg.type === 'data') {
        // Slow consumer: 1ms per item
        await new Promise((resolve) => setTimeout(resolve, 1));
        results.push(msg);
      }
      if (msg.type === 'end') break;
    }

    const duration = Date.now() - startTime;

    // Should have received all items
    expect(results).toHaveLength(1000);
    // Should take at least 1000ms (1ms * 1000 items)
    expect(duration).toBeGreaterThan(900);
  }, 15000); // 15s timeout for slow test

  test('streamAsync() handles custom buffer size', async () => {
    const items = Array.from({ length: 200 }, (_, i) => i);
    const actor = new MockStreamingActor('buffer-test', router, items);
    router.registerActor('buffer-test', actor);

    const results: number[] = [];
    for await (const msg of router.streamAsync(address('buffer-test'), 'test', {}, {
      bufferSize: 50, // Smaller buffer
    })) {
      if (msg.type === 'data') {
        results.push(msg.payload);
      }
      if (msg.type === 'end') break;
    }

    expect(results).toHaveLength(200);
    expect(results[0]).toBe(0);
    expect(results[199]).toBe(199);
  });
});

describe('MessageRouter - streamAsync() Cancellation Support', () => {
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('streamAsync() can be cancelled via AbortSignal', async () => {
    const items = Array.from({ length: 1000 }, (_, i) => i);
    // Use virtual time for deterministic cancellation test
    const actor = new MockStreamingActor('cancellable', router, items, 1, true);
    router.registerActor('cancellable', actor);

    const controller = new AbortController();
    const results: number[] = [];

    // Start streaming
    const streamPromise = (async () => {
      for await (const msg of router.streamAsync(address('cancellable'), 'test', {}, {
        signal: controller.signal,
      })) {
        if (msg.type === 'data') {
          results.push(msg.payload);
        }
      }
    })();

    // Advance time to process 50 items (50ms at 1ms per item)
    for (let i = 0; i < 50; i++) {
      await virtualClock.advance(1);
    }

    // Cancel the stream
    controller.abort('User cancelled');

    // Expect the stream to throw
    await expect(streamPromise).rejects.toThrow(/cancelled/i);

    // Should have received partial results (around 50 items)
    expect(results.length).toBeLessThan(1000);
    expect(results.length).toBeGreaterThan(0);
  });

  test('streamAsync() handles early break (consumer stops)', async () => {
    const items = Array.from({ length: 1000 }, (_, i) => i);
    const actor = new MockStreamingActor('breakable', router, items);
    router.registerActor('breakable', actor);

    const results: number[] = [];
    for await (const msg of router.streamAsync(address('breakable'), 'test', {})) {
      if (msg.type === 'data') {
        results.push(msg.payload);
        // Break after 10 items
        if (results.length >= 10) break;
      }
    }

    expect(results).toHaveLength(10);
  });
});

describe('MessageRouter - streamAsync() Timeout Handling', () => {
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('streamAsync() times out if stream takes too long', async () => {
    const items = Array.from({ length: 100 }, (_, i) => i);
    // Use virtual time for instant test execution
    const actor = new MockStreamingActor('slow-stream', router, items, 20, true);
    router.registerActor('slow-stream', actor);

    // Start the stream
    const streamPromise = (async () => {
      const results: number[] = [];
      for await (const msg of router.streamAsync(address('slow-stream'), 'test', {}, {
        timeout: 500, // Timeout after 500ms
      })) {
        if (msg.type === 'data') {
          results.push(msg.payload);
        }
      }
    })();

    // Advance virtual time to trigger timeout
    await virtualClock.advance(20); // First item
    await virtualClock.advance(20); // Second item
    await virtualClock.advance(500); // Jump to timeout

    await expect(streamPromise).rejects.toThrow(/timeout/i);
  });

  test('streamAsync() completes before timeout', async () => {
    const items = Array.from({ length: 10 }, (_, i) => i);
    // Use virtual time for instant test execution
    const actor = new MockStreamingActor('fast-stream', router, items, 1, true);
    router.registerActor('fast-stream', actor);

    const results: number[] = [];
    const streamPromise = (async () => {
      for await (const msg of router.streamAsync(address('fast-stream'), 'test', {}, {
        timeout: 1000, // Plenty of time
      })) {
        if (msg.type === 'data') {
          results.push(msg.payload);
        }
        if (msg.type === 'end') break;
      }
    })();

    // Advance virtual time for all items (10 items * 1ms each = 10ms total)
    for (let i = 0; i < 10; i++) {
      await virtualClock.advance(1);
    }
    await streamPromise;

    expect(results).toHaveLength(10);
  });
});

describe('MessageRouter - streamAsync() Error Handling', () => {
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('streamAsync() propagates actor errors', async () => {
    const actor = new ErrorStreamingActor('error-stream', router);
    router.registerActor('error-stream', actor);

    await expect(async () => {
      for await (const msg of router.streamAsync(address('error-stream'), 'test', {})) {
        // Should throw before completing
      }
    }).toThrow(/Stream processing failed/);
  });

  test('streamAsync() handles error messages in stream', async () => {
    class ErrorMessageActor extends Actor {
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

      async *streamAsync<T>(payload: any): AsyncIterableIterator<AsyncStreamMessage<T>> {
        const corrId = generateCorrelationId();

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: 'item1' as any,
          timestamp: Date.now(),
        };

        // Emit error message
        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'error',
          error: 'Something went wrong',
          timestamp: Date.now(),
        };
      }
    }

    const actor = new ErrorMessageActor('error-msg-stream', router);
    router.registerActor('error-msg-stream', actor);

    await expect(async () => {
      const results: any[] = [];
      for await (const msg of router.streamAsync(address('error-msg-stream'), 'test', {})) {
        results.push(msg);
      }
    }).toThrow(/Something went wrong/);
  });
});

describe('Actor - Helper Methods', () => {
  let router: MessageRouter;
  let actor: Actor;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);

    actor = new Actor('test-actor', router);
  });

  test('createAsyncStream() creates stream from array', async () => {
    const items = [1, 2, 3];
    const results: number[] = [];

    for await (const msg of (actor as any).createAsyncStream(items)) {
      if (msg.type === 'data') {
        results.push(msg.payload);
      }
    }

    expect(results).toEqual([1, 2, 3]);
  });

  test('createAsyncStream() emits end event', async () => {
    const items = [1, 2, 3];
    const events: string[] = [];

    for await (const msg of (actor as any).createAsyncStream(items)) {
      events.push(msg.type);
    }

    expect(events).toContain('end');
    expect(events[events.length - 1]).toBe('end');
  });

  test('createAsyncStream() handles async iterables', async () => {
    async function* generate() {
      yield 1;
      yield 2;
      yield 3;
    }

    const results: number[] = [];
    for await (const msg of (actor as any).createAsyncStream(generate())) {
      if (msg.type === 'data') {
        results.push(msg.payload);
      }
    }

    expect(results).toEqual([1, 2, 3]);
  });

  test('createAsyncStream() handles regular iterables', async () => {
    function* generate() {
      yield 1;
      yield 2;
      yield 3;
    }

    const results: number[] = [];
    for await (const msg of (actor as any).createAsyncStream(generate())) {
      if (msg.type === 'data') {
        results.push(msg.payload);
      }
    }

    expect(results).toEqual([1, 2, 3]);
  });

  test('createAsyncStreamWithBackpressure() detects slow consumer', async () => {
    const items = Array.from({ length: 100 }, (_, i) => i);
    let backpressureDetected = false;

    const results: number[] = [];
    for await (const msg of (actor as any).createAsyncStreamWithBackpressure(items, {
      checkInterval: 10,
      onBackpressure: (isPaused: boolean) => {
        if (isPaused) backpressureDetected = true;
      },
    })) {
      if (msg.type === 'data') {
        results.push(msg.payload);
        // Simulate slow consumer
        await new Promise((resolve) => setTimeout(resolve, 2));
      }
    }

    expect(results).toHaveLength(100);
    expect(backpressureDetected).toBe(true);
  }, 5000);
});

describe('MessageRouter - streamAsync() Performance', () => {
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('streamAsync() has <1µs overhead per item (fast consumer)', async () => {
    const items = Array.from({ length: 1000 }, (_, i) => i);
    const actor = new MockStreamingActor('perf-test', router, items);
    router.registerActor('perf-test', actor);

    const startTime = performance.now();
    const results: number[] = [];

    for await (const msg of router.streamAsync(address('perf-test'), 'test', {})) {
      if (msg.type === 'data') {
        results.push(msg.payload);
      }
      if (msg.type === 'end') break;
    }

    const duration = performance.now() - startTime;
    const perItemMs = duration / 1000;
    const perItemUs = perItemMs * 1000;

    expect(results).toHaveLength(1000);
    // Should be < 1µs per item (i.e., < 1ms total for 1000 items)
    expect(perItemUs).toBeLessThan(1);
  });

  test('streamAsync() handles large streams efficiently', async () => {
    const items = Array.from({ length: 10000 }, (_, i) => i);
    const actor = new MockStreamingActor('large-stream', router, items, 0, false);
    router.registerActor('large-stream', actor);

    const startTime = performance.now();
    let count = 0;

    for await (const msg of router.streamAsync(address('large-stream'), 'test', {})) {
      if (msg.type === 'data') count++;
      if (msg.type === 'end') break;
    }

    const duration = performance.now() - startTime;

    expect(count).toBe(10000);
    // Should complete in < 100ms
    expect(duration).toBeLessThan(100);
  });

});

describe('MessageRouter - streamAsync() Memory Safety', () => {
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // Setup SchedulerActor with virtual time
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('streamAsync() cleans up on early termination', async () => {
    const items = Array.from({ length: 1000 }, (_, i) => i);
    const actor = new MockStreamingActor('cleanup-test', router, items);
    router.registerActor('cleanup-test', actor);

    let count = 0;
    for await (const msg of router.streamAsync(address('cleanup-test'), 'test', {})) {
      if (msg.type === 'data') {
        count++;
        if (count >= 10) break; // Early termination
      }
    }

    expect(count).toBe(10);
    // No memory leak test possible without instrumentation, but ensures no hanging promises
  });

  test('streamAsync() cleans up on error', async () => {
    const actor = new ErrorStreamingActor('cleanup-error', router);
    router.registerActor('cleanup-error', actor);

    try {
      for await (const msg of router.streamAsync(address('cleanup-error'), 'test', {})) {
        // Should throw
      }
    } catch (error) {
      // Expected
    }

    // Ensure router state is clean (no pending requests)
    const stats = router.getStats();
    expect(stats.pendingRequests).toBe(0);
  });
});
