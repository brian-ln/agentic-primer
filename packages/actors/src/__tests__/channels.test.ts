/**
 * Tests for unified Channel<T> abstraction
 *
 * Ported from simplify/src/messaging/__tests__/channels.test.ts
 * Adapted from bun:test to node:test
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import {
  createStreamChannel,
  createPortChannel,
  createBridgeChannel,
  ChannelClosedError,
  ChannelCancelledError,
} from '../channels/index.ts';
import type { AsyncStreamMessage, Address } from '../message.ts';

// --- StreamChannel ---

describe('StreamChannel', () => {
  it('yields data events from AsyncStreamMessage source', async () => {
    const source = (async function* () {
      yield createDataEvent(1);
      yield createDataEvent(2);
      yield createDataEvent(3);
      yield createEndEvent();
    })();

    const channel = createStreamChannel(source);
    const results: number[] = [];

    for await (const value of channel) {
      results.push(value);
    }

    assert.deepEqual(results, [1, 2, 3]);
    assert.equal(channel.closed, true);
  });

  it('closes on end event', async () => {
    const source = (async function* () {
      yield createDataEvent('a');
      yield createEndEvent();
      yield createDataEvent('b'); // Should not be yielded
    })();

    const channel = createStreamChannel(source);
    const results: string[] = [];

    for await (const value of channel) {
      results.push(value);
    }

    assert.deepEqual(results, ['a']);
  });

  it('throws on error event', async () => {
    const source = (async function* () {
      yield createDataEvent(1);
      yield createErrorEvent('Something went wrong');
    })();

    const channel = createStreamChannel(source);

    await assert.rejects(async () => {
      for await (const _value of channel) {
        // Should throw on error event
      }
    }, { message: 'Something went wrong' });
  });

  it('supports AbortSignal cancellation', async () => {
    const controller = new AbortController();
    const source = (async function* () {
      yield createDataEvent(1);
      yield createDataEvent(2);
      yield createDataEvent(3);
    })();

    const channel = createStreamChannel(source, { signal: controller.signal });
    const results: number[] = [];

    try {
      for await (const value of channel) {
        results.push(value);
        if (results.length === 2) {
          controller.abort();
        }
      }
    } catch (_error) {
      // May or may not throw depending on timing
    }

    assert.ok(results.length <= 2);
    assert.equal(channel.closed, true);
  });

  it('throws when iterating twice', async () => {
    const source = (async function* () {
      yield createDataEvent(1);
      yield createDataEvent(2);
    })();

    const channel = createStreamChannel(source);

    // Start first iteration
    const iter1 = channel[Symbol.asyncIterator]();
    await iter1.next();

    // Attempt second iteration (async throw)
    const iter2 = channel[Symbol.asyncIterator]();
    await assert.rejects(
      () => iter2.next(),
      { message: /can only be iterated once/ }
    );
  });

  it('send() throws (read-only)', () => {
    const source = (async function* () {
      yield createDataEvent(1);
    })();

    const channel = createStreamChannel(source);

    assert.throws(() => {
      channel.send?.(123);
    }, { message: /read-only/ });
  });
});

// --- PortChannel ---

describe('PortChannel', () => {
  it('broadcasts to multiple subscribers', async () => {
    const port = createPortChannel<number>();

    const results1: number[] = [];
    const results2: number[] = [];

    // Start subscribers
    const consumer1 = (async () => {
      for await (const value of port.subscribe()) {
        results1.push(value);
        if (results1.length >= 3) break;
      }
    })();

    const consumer2 = (async () => {
      for await (const value of port.subscribe()) {
        results2.push(value);
        if (results2.length >= 3) break;
      }
    })();

    // Give subscribers time to setup
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Send values
    await port.send(1);
    await port.send(2);
    await port.send(3);

    await Promise.all([consumer1, consumer2]);

    assert.deepEqual(results1, [1, 2, 3]);
    assert.deepEqual(results2, [1, 2, 3]);
  });

  it('buffers per-subscriber when slow', async () => {
    const port = createPortChannel<number>({ bufferSize: 5 });

    const results: number[] = [];
    let backpressureCount = 0;

    const consumer = (async () => {
      for await (const value of port.subscribe({
        onBackpressure: (isPaused) => {
          if (isPaused) backpressureCount++;
        },
      })) {
        results.push(value);
        await new Promise((resolve) => setTimeout(resolve, 50)); // Slow consumer
        if (results.length >= 10) break;
      }
    })();

    // Give consumer time to setup
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Send values rapidly (should trigger backpressure)
    for (let i = 1; i <= 10; i++) {
      await port.send(i);
    }

    await consumer;

    assert.deepEqual(results, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    assert.ok(backpressureCount > 0);
  });

  it('unsubscribe removes subscriber', () => {
    const port = createPortChannel<number>();

    const sub = port.subscribe();
    assert.equal(port.subscriberCount, 1);

    sub.close();
    assert.equal(port.subscriberCount, 0);
  });

  it('close() ends all subscriptions', async () => {
    const port = createPortChannel<number>();

    const results1: number[] = [];
    const results2: number[] = [];

    const consumer1 = (async () => {
      for await (const value of port.subscribe()) {
        results1.push(value);
      }
    })();

    const consumer2 = (async () => {
      for await (const value of port.subscribe()) {
        results2.push(value);
      }
    })();

    await new Promise((resolve) => setTimeout(resolve, 10));

    await port.send(1);
    await port.send(2);

    // Give time for values to be delivered
    await new Promise((resolve) => setTimeout(resolve, 10));

    port.close();

    await Promise.all([consumer1, consumer2]);

    assert.deepEqual(results1, [1, 2]);
    assert.deepEqual(results2, [1, 2]);
    assert.equal(port.subscriberCount, 0);
  });

  it('send() throws on closed port', async () => {
    const port = createPortChannel<number>();
    port.close();

    await assert.rejects(
      () => port.send(123),
      (err: any) => err instanceof ChannelClosedError
    );
  });

  it('AbortSignal unsubscribes', async () => {
    const port = createPortChannel<number>();
    const controller = new AbortController();

    const results: number[] = [];
    const consumer = (async () => {
      for await (const value of port.subscribe({ signal: controller.signal })) {
        results.push(value);
      }
    })();

    await new Promise((resolve) => setTimeout(resolve, 10));

    await port.send(1);
    await port.send(2);

    controller.abort();

    await new Promise((resolve) => setTimeout(resolve, 10));
    await port.send(3); // Should not be received

    await consumer;

    assert.ok(results.length <= 2);
  });

  it('tracks buffered count per subscriber', async () => {
    const port = createPortChannel<number>();
    const sub = port.subscribe();

    // Send without consuming (should buffer)
    await port.send(1);
    await port.send(2);
    await port.send(3);

    assert.equal(sub.buffered, 3);

    sub.close();
  });
});

// --- BridgeChannel ---

describe('BridgeChannel', () => {
  it('adapts external source to channel', async () => {
    let pushFn: ((value: number) => void) | null = null;

    const channel = createBridgeChannel<number>((push) => {
      pushFn = push;
      return () => {
        pushFn = null;
      };
    });

    const results: number[] = [];
    const consumer = (async () => {
      for await (const value of channel) {
        results.push(value);
        if (results.length >= 3) {
          channel.close();
        }
      }
    })();

    // Give consumer time to setup
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Push values from external source
    pushFn?.(1);
    pushFn?.(2);
    pushFn?.(3);

    await consumer;

    assert.deepEqual(results, [1, 2, 3]);
    assert.equal(pushFn, null); // Cleanup should have been called
  });

  it('buffers when consumer is slow', async () => {
    let pushFn: ((value: number) => void) | null = null;
    let backpressureCount = 0;

    const channel = createBridgeChannel<number>(
      (push) => {
        pushFn = push;
        return () => {};
      },
      {
        bufferSize: 5,
        onBackpressure: (isPaused) => {
          if (isPaused) backpressureCount++;
        },
      }
    );

    const results: number[] = [];
    const consumer = (async () => {
      for await (const value of channel) {
        results.push(value);
        await new Promise((resolve) => setTimeout(resolve, 20)); // Slow
        if (results.length >= 10) {
          channel.close();
        }
      }
    })();

    await new Promise((resolve) => setTimeout(resolve, 10));

    // Push rapidly
    for (let i = 1; i <= 10; i++) {
      pushFn?.(i);
    }

    await consumer;

    assert.equal(results.length, 10);
    assert.ok(backpressureCount > 0);
  });

  it('cleanup called on close', async () => {
    let cleanupCalled = false;
    let pushFn: ((value: number) => void) | null = null;

    const channel = createBridgeChannel<number>((push) => {
      pushFn = push;
      return () => {
        cleanupCalled = true;
      };
    });

    const consumer = (async () => {
      for await (const _value of channel) {
        // Will exit when channel closes
      }
    })();

    await new Promise((resolve) => setTimeout(resolve, 10));

    // Push a value to ensure iteration started
    pushFn?.(1);

    await new Promise((resolve) => setTimeout(resolve, 10));

    channel.close();

    await consumer;

    assert.equal(cleanupCalled, true);
  });

  it('AbortSignal cancels iteration', async () => {
    const controller = new AbortController();
    let pushFn: ((value: number) => void) | null = null;

    const channel = createBridgeChannel<number>(
      (push) => {
        pushFn = push;
        return () => {};
      },
      { signal: controller.signal }
    );

    const results: number[] = [];

    const consumer = (async () => {
      try {
        for await (const value of channel) {
          results.push(value);
        }
      } catch (error) {
        if (error instanceof ChannelCancelledError) {
          // Expected
        } else {
          throw error;
        }
      }
    })();

    await new Promise((resolve) => setTimeout(resolve, 10));

    pushFn?.(1);
    pushFn?.(2);

    controller.abort();

    await consumer;

    assert.ok(results.length <= 2);
  });

  it('send() throws (read-only)', () => {
    const channel = createBridgeChannel<number>((_push) => () => {});

    assert.throws(() => {
      channel.send?.(123);
    }, { message: /read-only/ });
  });

  it('ignores pushes after close', () => {
    let pushFn: ((value: number) => void) | null = null;

    const channel = createBridgeChannel<number>((push) => {
      pushFn = push;
      return () => {};
    });

    channel.close();

    // Push after close (should be ignored)
    pushFn?.(1);
    pushFn?.(2);

    assert.equal(channel.buffered, 0);
  });
});

// --- Helper functions for creating test messages ---

function createDataEvent<T>(payload: T): AsyncStreamMessage<T> {
  return {
    id: `msg-${Date.now()}`,
    correlationId: 'test',
    from: '@(test-actor)' as Address,
    type: 'data',
    payload,
    timestamp: Date.now(),
  };
}

function createEndEvent<T>(): AsyncStreamMessage<T> {
  return {
    id: `msg-${Date.now()}`,
    correlationId: 'test',
    from: '@(test-actor)' as Address,
    type: 'end',
    timestamp: Date.now(),
  };
}

function createErrorEvent<T>(error: string): AsyncStreamMessage<T> {
  return {
    id: `msg-${Date.now()}`,
    correlationId: 'test',
    from: '@(test-actor)' as Address,
    type: 'error',
    error,
    timestamp: Date.now(),
  };
}
