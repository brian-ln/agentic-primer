/**
 * StreamChannel - Point-to-point (1:1) streaming
 *
 * Wraps existing streamAsync() implementation as a Channel.
 * Provides async iteration over stream events with backpressure handling.
 *
 * Ported from simplify/src/messaging/channels/stream.ts
 */

import type { Channel, ChannelOptions } from './channel.ts';
import { BaseChannel } from './channel.ts';
import type { AsyncStreamMessage } from '../message.ts';

/**
 * StreamChannel wraps AsyncIterator-based streaming into the Channel protocol.
 *
 * Use this for point-to-point streaming where one producer sends to one consumer.
 * Automatically handles 'data', 'end', and 'error' events from AsyncStreamMessage protocol.
 *
 * @example
 * ```typescript
 * const stream = router.streamAsync(address('domain/actor'), 'query', {});
 * const channel = new StreamChannel(stream);
 *
 * for await (const item of channel) {
 *   console.log(item);
 * }
 * ```
 */
export class StreamChannel<T> extends BaseChannel<T> {
  private source: AsyncIterableIterator<AsyncStreamMessage<T>>;
  private started = false;

  constructor(
    source: AsyncIterableIterator<AsyncStreamMessage<T>>,
    options: ChannelOptions = {}
  ) {
    super(options);
    this.source = source;
  }

  async *[Symbol.asyncIterator](): AsyncIterableIterator<T> {
    if (this.started) {
      throw new Error('StreamChannel can only be iterated once');
    }
    this.started = true;

    yield* this.iterate();
  }

  private async *iterate(): AsyncIterableIterator<T> {
    try {
      for await (const event of this.source) {
        if (this._closed) break;

        // Check for abort signal
        if (this.options.signal?.aborted) {
          this.close();
          break;
        }

        switch (event.type) {
          case 'data':
            if (event.payload !== undefined) {
              yield event.payload;
            }
            break;

          case 'end':
            this.close();
            return;

          case 'error':
            this.close();
            throw new Error(event.error ?? 'Stream error');
        }
      }
    } finally {
      this.close();
    }
  }

  /**
   * StreamChannel is read-only (no send capability).
   * Use router.streamAsync() to create the stream.
   */
  send(_value: T): Promise<void> {
    throw new Error('StreamChannel is read-only. Use the source actor to send messages.');
  }
}

/**
 * Create a StreamChannel from an AsyncIterableIterator of AsyncStreamMessage.
 *
 * @param source - The stream source (typically from router.streamAsync())
 * @param options - Channel options
 * @returns A Channel that yields the stream's payload values
 *
 * @example
 * ```typescript
 * const stream = router.streamAsync(address('domain/actor'), 'query', {});
 * const channel = createStreamChannel(stream);
 *
 * using ch = channel; // Auto-cleanup with TC39 Disposable
 * for await (const item of ch) {
 *   processItem(item);
 * }
 * ```
 */
export function createStreamChannel<T>(
  source: AsyncIterableIterator<AsyncStreamMessage<T>>,
  options?: ChannelOptions
): Channel<T> {
  return new StreamChannel(source, options);
}
