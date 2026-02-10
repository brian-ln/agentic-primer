/**
 * Unified Channel<T> Abstraction
 *
 * Provides a common interface for async communication patterns:
 * - StreamChannel: Point-to-point (1:1) streaming
 * - PortChannel: Pub/sub (1:N) broadcasting
 * - BridgeChannel: External async sources (DOM, WebSocket, etc.)
 *
 * Key insight: AsyncIterator IS a channel primitive.
 *
 * Ported from simplify/src/messaging/channel.ts
 * Uses web platform APIs only -- no Node/Bun/browser-specific deps.
 */

/**
 * Core channel interface for async message passing.
 * Extends AsyncIterable for standard for-await-of consumption.
 * Implements Disposable for automatic cleanup with `using` keyword.
 */
export interface Channel<T> extends AsyncIterable<T>, Disposable {
  /**
   * Standard async iteration protocol.
   * Allows for-await-of consumption with automatic backpressure.
   */
  [Symbol.asyncIterator](): AsyncIterableIterator<T>;

  /**
   * Optional sending capability for duplex channels.
   * One-way channels (like read-only ports) may not implement this.
   */
  send?(value: T): Promise<void>;

  /**
   * Close the channel, signaling no more values will be sent.
   * Safe to call multiple times (idempotent).
   */
  close(): void;

  /**
   * Whether the channel has been closed.
   * Once closed, iteration will complete and send() will throw.
   */
  readonly closed: boolean;

  /**
   * Number of buffered items waiting to be consumed.
   * Useful for backpressure monitoring.
   */
  readonly buffered: number;

  /**
   * Cleanup resources when channel is disposed.
   * Automatically called when using `using` keyword (TC39 Explicit Resource Management).
   */
  [Symbol.dispose](): void;
}

/**
 * Configuration options for creating channels.
 */
export interface ChannelOptions {
  /**
   * Maximum buffer size before applying backpressure.
   * Default: 100
   */
  bufferSize?: number;

  /**
   * AbortSignal for cancellation support.
   * Channel will close when signal is aborted.
   */
  signal?: AbortSignal;

  /**
   * Callback when backpressure state changes.
   * Useful for monitoring and adjusting producer rate.
   */
  onBackpressure?: (isPaused: boolean) => void;
}

/**
 * Error thrown when attempting to send on a closed channel.
 */
export class ChannelClosedError extends Error {
  constructor(message = 'Cannot send on closed channel') {
    super(message);
    this.name = 'ChannelClosedError';
  }
}

/**
 * Error thrown when channel operation is cancelled via AbortSignal.
 */
export class ChannelCancelledError extends Error {
  constructor(message = 'Channel operation cancelled') {
    super(message);
    this.name = 'ChannelCancelledError';
  }
}

/**
 * Base class for channel implementations.
 * Provides common functionality for buffer management and lifecycle.
 */
export abstract class BaseChannel<T> implements Channel<T> {
  protected buffer: T[] = [];
  protected resolvers: Array<(value: T | PromiseLike<T>) => void> = [];
  protected _closed = false;
  protected options: ChannelOptions;

  constructor(options: ChannelOptions = {}) {
    this.options = {
      bufferSize: 100,
      ...options,
    };

    // Auto-close on abort signal
    if (options.signal) {
      options.signal.addEventListener('abort', () => this.close(), { once: true });
    }
  }

  abstract [Symbol.asyncIterator](): AsyncIterableIterator<T>;

  get closed(): boolean {
    return this._closed;
  }

  get buffered(): number {
    return this.buffer.length;
  }

  close(): void {
    if (this._closed) return;
    this._closed = true;

    // Resolve any waiting consumers with end-of-stream
    for (const resolve of this.resolvers) {
      resolve({ value: undefined, done: true } as any);
    }
    this.resolvers = [];
  }

  [Symbol.dispose](): void {
    this.close();
  }

  /**
   * Check if buffer is experiencing backpressure.
   * Returns true if buffer is at or above configured size.
   */
  protected isBackpressured(): boolean {
    return this.buffer.length >= (this.options.bufferSize ?? 100);
  }

  /**
   * Notify backpressure callback if state changed.
   */
  protected notifyBackpressure(isPaused: boolean): void {
    this.options.onBackpressure?.(isPaused);
  }
}
