/**
 * PortChannel - Pub/sub (1:N) broadcasting
 *
 * Implements multicast channel where one producer broadcasts to multiple consumers.
 * Each subscriber gets their own buffer and backpressure handling.
 *
 * Ported from simplify/src/messaging/channels/port.ts
 */

import type { Channel, ChannelOptions } from './channel.ts';
import { BaseChannel, ChannelClosedError } from './channel.ts';

/**
 * Subscriber state for PortChannel multicast.
 */
interface Subscriber<T> {
  id: string;
  buffer: T[];
  resolver: ((value: IteratorResult<T>) => void) | null;
  bufferSize: number;
  onBackpressure?: (isPaused: boolean) => void;
}

/**
 * PortChannel implements multicast (1:N) broadcasting.
 *
 * One producer sends values to multiple subscribers.
 * Each subscriber has independent buffer and backpressure state.
 *
 * @example
 * ```typescript
 * const port = new PortChannel<StatusEvent>();
 *
 * // Producer side
 * await port.send({ type: 'ready', timestamp: Date.now() });
 *
 * // Consumer 1
 * for await (const event of port.subscribe()) {
 *   console.log('Consumer 1:', event);
 * }
 *
 * // Consumer 2
 * for await (const event of port.subscribe()) {
 *   console.log('Consumer 2:', event);
 * }
 * ```
 */
export class PortChannel<T> extends BaseChannel<T> {
  private subscribers = new Map<string, Subscriber<T>>();
  private nextSubscriberId = 0;

  constructor(options: ChannelOptions = {}) {
    super(options);
  }

  /**
   * Subscribe to the port, creating a new async iterator for this subscriber.
   * Each subscriber gets independent buffering and backpressure handling.
   *
   * @param options - Per-subscriber options (overrides port defaults)
   * @returns Channel for this specific subscriber
   */
  subscribe(options?: ChannelOptions): Channel<T> {
    const subscriberId = `sub-${this.nextSubscriberId++}`;
    const mergedOptions = { ...this.options, ...options };

    const subscriber: Subscriber<T> = {
      id: subscriberId,
      buffer: [],
      resolver: null,
      bufferSize: mergedOptions.bufferSize ?? 100,
      onBackpressure: mergedOptions.onBackpressure,
    };

    this.subscribers.set(subscriberId, subscriber);

    // Auto-cleanup on abort
    if (mergedOptions.signal) {
      mergedOptions.signal.addEventListener(
        'abort',
        () => this.unsubscribe(subscriberId),
        { once: true }
      );
    }

    // Capture 'this' in closure for proper binding
    const port = this;

    return {
      [Symbol.asyncIterator]: () => port.createSubscriberIterator(subscriberId),
      close: () => port.unsubscribe(subscriberId),
      get closed() {
        return !port.subscribers.has(subscriberId);
      },
      get buffered() {
        return subscriber.buffer.length;
      },
      [Symbol.dispose]: () => port.unsubscribe(subscriberId),
    } as Channel<T>;
  }

  /**
   * Unsubscribe a subscriber by ID.
   */
  private unsubscribe(subscriberId: string): void {
    const subscriber = this.subscribers.get(subscriberId);
    if (subscriber?.resolver) {
      subscriber.resolver({ value: undefined, done: true });
    }
    this.subscribers.delete(subscriberId);
  }

  /**
   * Create async iterator for a specific subscriber.
   */
  private async *createSubscriberIterator(subscriberId: string): AsyncIterableIterator<T> {
    const subscriber = this.subscribers.get(subscriberId);
    if (!subscriber) return;

    try {
      while (this.subscribers.has(subscriberId)) {
        // Drain buffer first
        while (subscriber.buffer.length > 0) {
          const value = subscriber.buffer.shift();
          if (value !== undefined) {
            yield value;
          }

          // Check backpressure state
          const wasBackpressured = subscriber.buffer.length >= subscriber.bufferSize;
          const isBackpressured = subscriber.buffer.length >= subscriber.bufferSize;
          if (wasBackpressured !== isBackpressured) {
            subscriber.onBackpressure?.(isBackpressured);
          }
        }

        // Wait for next value
        if (!this._closed && this.subscribers.has(subscriberId)) {
          const result = await new Promise<IteratorResult<T>>((resolve) => {
            subscriber.resolver = resolve;

            // Check if value arrived while we were setting up promise
            if (subscriber.buffer.length > 0 || this._closed) {
              resolve({ value: subscriber.buffer.shift(), done: false } as IteratorResult<T>);
              subscriber.resolver = null;
            }
          });

          if (result.done) break;
          if (result.value !== undefined) {
            yield result.value;
          }
        } else {
          break;
        }
      }
    } finally {
      this.unsubscribe(subscriberId);
    }
  }

  /**
   * Standard iteration creates a new subscription.
   * Most code should use subscribe() explicitly for clarity.
   */
  async *[Symbol.asyncIterator](): AsyncIterableIterator<T> {
    yield* this.subscribe();
  }

  /**
   * Send a value to all subscribers.
   * Buffers per-subscriber if consumers are slow.
   */
  async send(value: T): Promise<void> {
    if (this._closed) {
      throw new ChannelClosedError();
    }

    // Broadcast to all subscribers
    for (const subscriber of this.subscribers.values()) {
      if (subscriber.resolver) {
        // Subscriber is waiting, deliver immediately
        subscriber.resolver({ value, done: false });
        subscriber.resolver = null;
      } else {
        // Subscriber is busy, buffer the value
        subscriber.buffer.push(value);

        // Check backpressure
        const isBackpressured = subscriber.buffer.length >= subscriber.bufferSize;
        subscriber.onBackpressure?.(isBackpressured);
      }
    }
  }

  /**
   * Close the port, ending all subscriptions.
   */
  close(): void {
    if (this._closed) return;
    this._closed = true;

    // Notify all subscribers
    for (const subscriber of this.subscribers.values()) {
      if (subscriber.resolver) {
        subscriber.resolver({ value: undefined, done: true });
      }
    }
    this.subscribers.clear();
  }

  /**
   * Get the number of active subscribers.
   */
  get subscriberCount(): number {
    return this.subscribers.size;
  }
}

/**
 * Create a PortChannel for pub/sub broadcasting.
 *
 * @param options - Channel options
 * @returns A Channel that supports multiple subscribers
 *
 * @example
 * ```typescript
 * const statusPort = createPortChannel<StatusEvent>();
 *
 * // Multiple subscribers
 * const sub1 = statusPort.subscribe();
 * const sub2 = statusPort.subscribe();
 *
 * // Producer
 * await statusPort.send({ type: 'ready' });
 * ```
 */
export function createPortChannel<T>(options?: ChannelOptions): PortChannel<T> {
  return new PortChannel<T>(options);
}
