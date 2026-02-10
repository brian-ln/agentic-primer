/**
 * BridgeChannel - External async source adapter
 *
 * Adapts external async sources (DOM events, WebSockets, callbacks)
 * into the Channel protocol.
 *
 * Ported from simplify/src/messaging/channels/bridge.ts
 */

import type { Channel, ChannelOptions } from './channel.ts';
import { BaseChannel, ChannelCancelledError } from './channel.ts';

/**
 * Cleanup function for external resource.
 */
export type CleanupFn = () => void;

/**
 * Setup function that registers listener and returns cleanup.
 *
 * @param push - Function to push values into the channel
 * @returns Cleanup function to unregister listener
 *
 * @example
 * ```typescript
 * const setup: SetupFn<MouseEvent> = (push) => {
 *   const handler = (e: MouseEvent) => push(e);
 *   element.addEventListener('click', handler);
 *   return () => element.removeEventListener('click', handler);
 * };
 * ```
 */
export type SetupFn<T> = (push: (value: T) => void) => CleanupFn;

/**
 * BridgeChannel adapts external async sources into Channel protocol.
 *
 * Use this to bridge:
 * - DOM events -> Channel
 * - WebSocket messages -> Channel
 * - Callback-based APIs -> Channel
 * - EventEmitter -> Channel
 *
 * @example
 * ```typescript
 * // DOM Event
 * const clicks = new BridgeChannel<MouseEvent>((push) => {
 *   const handler = (e: MouseEvent) => push(e);
 *   button.addEventListener('click', handler);
 *   return () => button.removeEventListener('click', handler);
 * });
 *
 * for await (const click of clicks) {
 *   console.log('Clicked at:', click.clientX, click.clientY);
 * }
 * ```
 */
export class BridgeChannel<T> extends BaseChannel<T> {
  private cleanup: CleanupFn | null = null;
  private setupFn: SetupFn<T>;

  constructor(setupFn: SetupFn<T>, options: ChannelOptions = {}) {
    super(options);
    this.setupFn = setupFn;
  }

  async *[Symbol.asyncIterator](): AsyncIterableIterator<T> {
    // Setup external source
    this.cleanup = this.setupFn((value) => this.push(value));

    try {
      while (!this._closed) {
        // Check abort signal
        if (this.options.signal?.aborted) {
          throw new ChannelCancelledError();
        }

        // Wait for next value
        const value = await this.take();
        if (value !== undefined) {
          yield value;
        }
      }
    } finally {
      this.cleanup?.();
      this.cleanup = null;
    }
  }

  /**
   * Push value from external source into channel buffer.
   */
  private push(value: T): void {
    if (this._closed) return;

    if (this.resolvers.length > 0) {
      // Consumer is waiting, deliver immediately
      const resolve = this.resolvers.shift();
      resolve?.(value);
    } else {
      // Consumer is busy, buffer the value
      const wasBackpressured = this.isBackpressured();
      this.buffer.push(value);
      const isBackpressured = this.isBackpressured();

      if (wasBackpressured !== isBackpressured) {
        this.notifyBackpressure(isBackpressured);
      }
    }
  }

  /**
   * Take next value from buffer or wait for one.
   */
  private async take(): Promise<T | undefined> {
    // Drain buffer first
    if (this.buffer.length > 0) {
      const wasBackpressured = this.isBackpressured();
      const value = this.buffer.shift();
      const isBackpressured = this.isBackpressured();

      if (wasBackpressured !== isBackpressured) {
        this.notifyBackpressure(isBackpressured);
      }

      return value;
    }

    // Wait for next push
    if (this._closed) return undefined;

    return new Promise<T>((resolve) => {
      this.resolvers.push(resolve);

      // Recheck buffer in case value arrived while setting up promise
      if (this.buffer.length > 0) {
        this.resolvers.pop(); // Remove the resolver we just added
        resolve(this.buffer.shift()!);
      }
    });
  }

  /**
   * BridgeChannel is read-only (external source controls values).
   */
  send(_value: T): Promise<void> {
    throw new Error('BridgeChannel is read-only. Values come from external source.');
  }

  /**
   * Close channel and cleanup external source.
   */
  close(): void {
    if (this._closed) return;

    super.close();
    this.cleanup?.();
    this.cleanup = null;
  }
}

/**
 * Create a BridgeChannel from external async source.
 *
 * @param setup - Function to setup listener and return cleanup
 * @param options - Channel options
 * @returns Channel that yields values from external source
 *
 * @example
 * ```typescript
 * // WebSocket messages
 * const messages = createBridgeChannel<string>((push) => {
 *   const handler = (e: MessageEvent) => push(e.data);
 *   ws.addEventListener('message', handler);
 *   return () => ws.removeEventListener('message', handler);
 * });
 *
 * for await (const msg of messages) {
 *   handleMessage(msg);
 * }
 * ```
 */
export function createBridgeChannel<T>(
  setup: SetupFn<T>,
  options?: ChannelOptions
): Channel<T> {
  return new BridgeChannel(setup, options);
}
