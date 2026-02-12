/**
 * Tests for Signal↔Channel bridge utilities
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import {
  signalToChannel,
  signalToChannelWithSubscribe,
  channelToSignal,
  domEventChannel,
  eventTargetChannel,
  mergeChannels,
  type Signal,
} from '../browser/bridges.ts';
import { createPortChannel } from '@agentic-primer/actors';

// Minimal Signal implementation for testing
class TestSignal<T> implements Signal<T> {
  private value: T;
  private subscribers = new Set<(value: T) => void>();

  constructor(initial: T) {
    this.value = initial;
  }

  get(): T {
    return this.value;
  }

  set(value: T): void {
    if (value !== this.value) {
      this.value = value;
      this.subscribers.forEach((cb) => cb(value));
    }
  }

  subscribe(callback: (value: T) => void): () => void {
    this.subscribers.add(callback);
    return () => this.subscribers.delete(callback);
  }
}

// Test helper to create signal
function signal<T>(initial: T): TestSignal<T> {
  return new TestSignal(initial);
}

describe('Signal↔Channel Bridges', () => {
  describe('signalToChannel() - polling-based', () => {
    test('yields signal changes', async () => {
      const sig = signal('initial');
      const channel = signalToChannel(sig, { pollInterval: 10 });

      const values: string[] = [];
      const consumer = (async () => {
        for await (const value of channel) {
          values.push(value);
          if (values.length >= 3) {
            channel.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set('first');
      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set('second');
      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set('third');
      await new Promise((resolve) => setTimeout(resolve, 20));

      await consumer;

      expect(values).toContain('first');
      expect(values).toContain('second');
      expect(values).toContain('third');
    });

    test('does not yield if value unchanged', async () => {
      const sig = signal(42);
      const channel = signalToChannel(sig, { pollInterval: 10 });

      const values: number[] = [];
      const consumer = (async () => {
        for await (const value of channel) {
          values.push(value);
          if (values.length >= 2) {
            channel.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set(42); // Same value
      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set(100); // Changed
      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set(100); // Same again
      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set(200); // Changed
      await new Promise((resolve) => setTimeout(resolve, 20));

      await consumer;

      expect(values.length).toBe(2);
      expect(values).toEqual([100, 200]);
    });

    test('cleanup stops polling', async () => {
      const sig = signal('test');
      const channel = signalToChannel(sig, { pollInterval: 10 });

      const values: string[] = [];
      const consumer = (async () => {
        for await (const value of channel) {
          values.push(value);
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 20));

      sig.set('changed');
      await new Promise((resolve) => setTimeout(resolve, 30));

      channel.close();

      await consumer;

      // Should have received the change before close
      expect(values).toContain('changed');
      expect(channel.closed).toBe(true);
    });
  });

  describe('signalToChannelWithSubscribe() - subscription-based', () => {
    test('yields signal changes via subscription', async () => {
      const sig = signal('initial');
      const channel = signalToChannelWithSubscribe(sig);

      const values: string[] = [];
      const consumer = (async () => {
        for await (const value of channel) {
          values.push(value);
          if (values.length >= 3) {
            channel.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      sig.set('first');
      sig.set('second');
      sig.set('third');

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      expect(values).toEqual(['first', 'second', 'third']);
    });

    test('cleanup unsubscribes from signal', async () => {
      const sig = signal(1);
      const channel = signalToChannelWithSubscribe(sig);

      const values: number[] = [];
      const consumer = (async () => {
        for await (const value of channel) {
          values.push(value);
          if (values.length >= 1) {
            channel.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      sig.set(100);

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      // After close, new changes should not be received
      sig.set(200);
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(values).toEqual([100]);
      expect(sig.get()).toBe(200); // Signal still updated, but channel didn't receive it
    });
  });

  describe('channelToSignal()', () => {
    test('updates signal with channel values', async () => {
      const port = createPortChannel<number>();
      const sig = channelToSignal(port.subscribe(), 0, signal);

      await new Promise((resolve) => setTimeout(resolve, 10));

      await port.send(10);
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(sig.get()).toBe(10);

      await port.send(20);
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(sig.get()).toBe(20);

      port.close();
    });

    test('retains last value after channel closes', async () => {
      const port = createPortChannel<string>();
      const sig = channelToSignal(port.subscribe(), 'initial', signal);

      await new Promise((resolve) => setTimeout(resolve, 10));

      await port.send('updated');
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(sig.get()).toBe('updated');

      port.close();
      await new Promise((resolve) => setTimeout(resolve, 10));

      // Signal should retain last value
      expect(sig.get()).toBe('updated');
    });
  });

  describe('domEventChannel()', () => {
    test.skip('yields DOM events', async () => {
      // Requires browser environment (happy-dom or jsdom)
      // Skipping in Node/Bun test environment
      if (typeof document === 'undefined') return;

      const button = document.createElement('button');
      const channel = domEventChannel(button, 'click');

      const events: MouseEvent[] = [];
      const consumer = (async () => {
        for await (const event of channel) {
          events.push(event);
          if (events.length >= 2) {
            channel.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      button.click();
      button.click();

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      expect(events.length).toBe(2);
      expect(events[0].type).toBe('click');
      expect(events[1].type).toBe('click');
    });

    test.skip('cleanup removes event listener', async () => {
      // Requires browser environment
      if (typeof document === 'undefined') return;

      const button = document.createElement('button');
      const channel = domEventChannel(button, 'click');

      const events: MouseEvent[] = [];
      const consumer = (async () => {
        for await (const event of channel) {
          events.push(event);
          if (events.length >= 1) {
            channel.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      button.click();

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      // After close, events should not be captured
      button.click();
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(events.length).toBe(1);
    });

    test.skip('works with AbortSignal', async () => {
      // Requires browser environment
      if (typeof document === 'undefined') return;

      const button = document.createElement('button');
      const controller = new AbortController();
      const channel = domEventChannel(button, 'click', {
        signal: controller.signal,
      });

      const events: MouseEvent[] = [];
      const consumer = (async () => {
        try {
          for await (const event of channel) {
            events.push(event);
          }
        } catch (error) {
          // Expected when aborted
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      button.click();
      await new Promise((resolve) => setTimeout(resolve, 10));

      controller.abort();

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      expect(events.length).toBe(1);
    });
  });

  describe('eventTargetChannel()', () => {
    test('yields events from EventTarget', async () => {
      const target = new EventTarget();
      const channel = eventTargetChannel<CustomEvent>(target, 'custom');

      const events: CustomEvent[] = [];
      const consumer = (async () => {
        for await (const event of channel) {
          events.push(event);
          if (events.length >= 2) {
            channel.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      target.dispatchEvent(new CustomEvent('custom', { detail: 'first' }));
      target.dispatchEvent(new CustomEvent('custom', { detail: 'second' }));

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      expect(events.length).toBe(2);
      expect(events[0].detail).toBe('first');
      expect(events[1].detail).toBe('second');
    });
  });

  describe('mergeChannels()', () => {
    test('merges multiple channels into one', async () => {
      const port1 = createPortChannel<string>();
      const port2 = createPortChannel<string>();
      const port3 = createPortChannel<string>();

      const merged = mergeChannels([
        port1.subscribe(),
        port2.subscribe(),
        port3.subscribe(),
      ]);

      const values: string[] = [];
      const consumer = (async () => {
        for await (const value of merged) {
          values.push(value);
          if (values.length >= 6) {
            merged.close();
            break;
          }
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      await port1.send('p1-a');
      await port2.send('p2-a');
      await port3.send('p3-a');
      await port1.send('p1-b');
      await port2.send('p2-b');
      await port3.send('p3-b');

      await new Promise((resolve) => setTimeout(resolve, 10));
      await consumer;

      expect(values.length).toBe(6);
      expect(values).toContain('p1-a');
      expect(values).toContain('p2-a');
      expect(values).toContain('p3-a');
      expect(values).toContain('p1-b');
      expect(values).toContain('p2-b');
      expect(values).toContain('p3-b');

      port1.close();
      port2.close();
      port3.close();
    });

    test('closes all source channels on cleanup', async () => {
      const port1 = createPortChannel<number>();
      const port2 = createPortChannel<number>();

      const sub1 = port1.subscribe();
      const sub2 = port2.subscribe();

      const merged = mergeChannels([sub1, sub2]);

      // Start consuming
      const consumer = (async () => {
        for await (const _value of merged) {
          // Will exit when merged closes
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      merged.close();

      await consumer;

      // Source subscriptions should be closed
      expect(sub1.closed).toBe(true);
      expect(sub2.closed).toBe(true);

      port1.close();
      port2.close();
    });
  });

  describe('integration scenarios', () => {
    test('signal → channel → another signal (round trip)', async () => {
      const sourceSignal = signal('initial');

      // Signal → Channel
      const channel = signalToChannelWithSubscribe(sourceSignal);

      // Channel → Signal
      const targetSignal = channelToSignal(channel, '', signal);

      await new Promise((resolve) => setTimeout(resolve, 10));

      sourceSignal.set('updated');
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(targetSignal.get()).toBe('updated');

      sourceSignal.set('final');
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(targetSignal.get()).toBe('final');

      channel.close();
    });

    test.skip('DOM event → channel → signal', async () => {
      // Requires browser environment
      if (typeof document === 'undefined') return;

      const button = document.createElement('button');
      const clickChannel = domEventChannel(button, 'click');

      const clickCount = signal(0);

      (async () => {
        for await (const _event of clickChannel) {
          clickCount.set(clickCount.get() + 1);
        }
      })();

      await new Promise((resolve) => setTimeout(resolve, 10));

      button.click();
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(clickCount.get()).toBe(1);

      button.click();
      button.click();
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(clickCount.get()).toBe(3);

      clickChannel.close();
    });
  });
});
