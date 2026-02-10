/**
 * Signal <-> Channel Bridge Utilities
 *
 * Bridges between TC39 Signals (internal reactivity) and Channels (external communication).
 * Enables Widget Actors to expose internal signals as ports and consume external channels as signals.
 *
 * Browser-specific: uses DOM APIs (HTMLElement, EventTarget, etc.)
 *
 * Ported from simplify/src/messaging/browser/bridges.ts
 */

import type { Channel, ChannelOptions } from '@agentic-primer/actors';
import { createBridgeChannel } from '@agentic-primer/actors';

/**
 * TC39 Signal interface (minimal subset for bridge compatibility).
 *
 * Widget Actors can use any Signal implementation that matches this interface.
 * Compatible with:
 * - TC39 Signals proposal (@preact/signals, @angular/core, etc.)
 * - Custom signal implementations from agentic-ui
 */
export interface BridgeSignal<T> {
  get(): T;
  set(value: T): void;
  subscribe?(callback: (value: T) => void): () => void;
}

/**
 * Convert a Signal to a Channel for external broadcasting.
 *
 * Exposes signal changes as a Channel that can be subscribed to by other actors.
 * Useful for exposing internal widget state as a reactive port.
 *
 * Note: This creates a polling-based channel. For better performance,
 * use `signalToChannelWithSubscribe()` if your Signal supports `.subscribe()`.
 *
 * @param sig - The signal to convert
 * @param options - Channel options with optional poll interval
 * @returns Channel that yields signal values when they change
 *
 * @example
 * ```typescript
 * class MyWidget extends HTMLElement implements Actor {
 *   private status = signal('idle');
 *
 *   port(name: string): Channel<any> {
 *     if (name === 'status') {
 *       return signalToChannel(this.status);
 *     }
 *   }
 * }
 * ```
 */
export function signalToChannel<T>(
  sig: BridgeSignal<T>,
  options?: ChannelOptions & { pollInterval?: number }
): Channel<T> {
  const pollInterval = options?.pollInterval ?? 16; // ~60fps default

  return createBridgeChannel<T>(
    (push) => {
      let prev = sig.get();
      let stopped = false;

      // Poll for changes
      const interval = setInterval(() => {
        if (stopped) return;

        const current = sig.get();
        if (current !== prev) {
          push(current);
          prev = current;
        }
      }, pollInterval);

      return () => {
        stopped = true;
        clearInterval(interval);
      };
    },
    options
  );
}

/**
 * Convert a Signal to a Channel using `.subscribe()` for efficient change detection.
 *
 * More efficient than `signalToChannel()` as it uses the signal's native
 * subscription mechanism instead of polling.
 *
 * @param sig - Signal with .subscribe() method
 * @param options - Channel options
 * @returns Channel that yields signal values when they change
 *
 * @example
 * ```typescript
 * // With @preact/signals or similar
 * const status = signal('idle');
 * const channel = signalToChannelWithSubscribe(status);
 *
 * for await (const value of channel) {
 *   console.log('Status changed:', value);
 * }
 * ```
 */
export function signalToChannelWithSubscribe<T>(
  sig: BridgeSignal<T> & { subscribe(cb: (value: T) => void): () => void },
  options?: ChannelOptions
): Channel<T> {
  return createBridgeChannel<T>(
    (push) => {
      // Subscribe to signal changes
      const unsubscribe = sig.subscribe((value) => {
        push(value);
      });

      return unsubscribe;
    },
    options
  );
}

/**
 * Convert a Channel to a Signal for reactive UI updates.
 *
 * Consumes a channel and updates a signal with each received value.
 * Useful for Widget Actors consuming external actor ports.
 *
 * Note: The background subscription runs until the channel closes.
 * Ensure proper cleanup by closing the channel when no longer needed.
 *
 * @param channel - Channel to convert
 * @param initial - Initial signal value
 * @param createSignal - Signal factory function
 * @returns Signal that updates with channel values
 *
 * @example
 * ```typescript
 * const statusChannel = sessionActor.port('status').subscribe();
 * const statusSignal = channelToSignal(
 *   statusChannel,
 *   'unknown',
 *   (val) => signal(val)
 * );
 *
 * // Use in reactive UI
 * effect(() => {
 *   console.log('Status:', statusSignal.get());
 * });
 * ```
 */
export function channelToSignal<T>(
  channel: Channel<T>,
  initial: T,
  createSignal: (value: T) => BridgeSignal<T>
): BridgeSignal<T> {
  const sig = createSignal(initial);

  // Background subscription
  (async () => {
    try {
      for await (const value of channel) {
        sig.set(value);
      }
    } catch (_error) {
      // Channel closed or errored - signal retains last value
    }
  })();

  return sig;
}

/**
 * Convert DOM events to a Channel for actor integration.
 *
 * Bridges browser DOM events into the actor messaging system.
 * Widget Actors can expose DOM events as reactive ports.
 *
 * @param element - DOM element to listen to
 * @param eventName - Event name (e.g., 'click', 'input', 'change')
 * @param options - Channel options with optional AbortSignal
 * @returns Channel that yields DOM events
 *
 * @example
 * ```typescript
 * class ChatInput extends HTMLElement implements Actor {
 *   port(name: string): Channel<any> {
 *     if (name === 'user-input') {
 *       const button = this.querySelector('button')!;
 *       return domEventChannel(button, 'click');
 *     }
 *   }
 * }
 *
 * // Consumer
 * for await (const clickEvent of chatInput.port('user-input')) {
 *   console.log('Button clicked:', clickEvent.target);
 * }
 * ```
 */
export function domEventChannel<K extends keyof HTMLElementEventMap>(
  element: HTMLElement,
  eventName: K,
  options?: ChannelOptions
): Channel<HTMLElementEventMap[K]> {
  return createBridgeChannel<HTMLElementEventMap[K]>(
    (push) => {
      const handler = (event: HTMLElementEventMap[K]) => {
        push(event);
      };

      element.addEventListener(eventName, handler as EventListener);

      return () => {
        element.removeEventListener(eventName, handler as EventListener);
      };
    },
    options
  );
}

/**
 * Convert generic EventTarget events to a Channel.
 *
 * More generic version of domEventChannel for any EventTarget.
 * Works with WebSockets, XMLHttpRequest, etc.
 *
 * @param target - EventTarget to listen to
 * @param eventName - Event name
 * @param options - Channel options
 * @returns Channel that yields events
 *
 * @example
 * ```typescript
 * // WebSocket events
 * const messages = eventTargetChannel(
 *   webSocket,
 *   'message',
 *   { signal: abortSignal }
 * );
 *
 * for await (const event of messages) {
 *   console.log('Message:', event.data);
 * }
 * ```
 */
export function eventTargetChannel<T extends Event = Event>(
  target: EventTarget,
  eventName: string,
  options?: ChannelOptions
): Channel<T> {
  return createBridgeChannel<T>(
    (push) => {
      const handler = (event: Event) => {
        push(event as T);
      };

      target.addEventListener(eventName, handler);

      return () => {
        target.removeEventListener(eventName, handler);
      };
    },
    options
  );
}

/**
 * Merge multiple channels into a single channel.
 *
 * Useful for combining multiple event sources into one stream.
 * Values are yielded as they arrive from any source channel.
 *
 * @param channels - Array of channels to merge
 * @param options - Channel options for the merged channel
 * @returns Channel that yields values from all input channels
 *
 * @example
 * ```typescript
 * const clicks = domEventChannel(button, 'click');
 * const keys = domEventChannel(input, 'keypress');
 * const interactions = mergeChannels([clicks, keys]);
 *
 * for await (const event of interactions) {
 *   console.log('User interaction:', event.type);
 * }
 * ```
 */
export function mergeChannels<T>(
  channels: Channel<T>[],
  options?: ChannelOptions
): Channel<T> {
  return createBridgeChannel<T>(
    (push) => {
      // Start consuming all channels
      const _consumers = channels.map((channel) =>
        (async () => {
          for await (const value of channel) {
            push(value);
          }
        })()
      );

      return () => {
        // Close all source channels on cleanup
        channels.forEach((channel) => channel.close());
      };
    },
    options
  );
}
