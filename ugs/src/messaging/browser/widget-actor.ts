/**
 * Widget Actor - Web Components as Actors
 *
 * Bridges Web Components and the Actor messaging protocol.
 * Enables unified communication between browser UI components and backend actors.
 */

import type { Address, Message, MessageResponse } from '../message.ts';
import { address } from '../message.ts';
import type { Channel } from '../channel.ts';
import { actorRegistry } from './actor-registry.ts';

/**
 * Constructor type for classes (enables mixin pattern)
 */
export type Constructor<T = {}> = new (...args: any[]) => T;

/**
 * Widget Actor interface - combines HTMLElement and Actor protocols.
 *
 * Widget Actors are Web Components that can:
 * - Send and receive messages via actor protocol
 * - Expose reactive ports for pub/sub broadcasting
 * - Use TC39 Signals for internal reactivity
 * - Communicate with backend actors and other Widget Actors
 */
export interface WidgetActor extends HTMLElement {
  // Actor protocol
  readonly address: Address;
  receive(msg: Message): Promise<MessageResponse>;
  port?(name: string): Channel<any>;

  // Web Component lifecycle
  connectedCallback(): void;
  disconnectedCallback(): void;
  attributeChangedCallback?(name: string, oldValue: string | null, newValue: string | null): void;
}

/**
 * Actor mixin for Web Components.
 *
 * Adds actor protocol to any HTMLElement-based class.
 * Automatically registers/unregisters with browser actor registry on mount/unmount.
 *
 * @param Base - Base class (typically HTMLElement or a custom element class)
 * @returns Extended class with Actor protocol
 *
 * @example
 * ```typescript
 * // Basic usage
 * class ChatMessage extends ActorMixin(HTMLElement) {
 *   async receive(msg: Message): Promise<MessageResponse> {
 *     if (msg.type === 'update') {
 *       this.textContent = msg.payload.text;
 *       return { success: true };
 *     }
 *     return { success: false, error: 'Unknown message' };
 *   }
 * }
 *
 * customElements.define('chat-message', ChatMessage);
 *
 * // With ports
 * class ChatRoom extends ActorMixin(HTMLElement) {
 *   private statusPort = this.createPort<StatusEvent>('status');
 *
 *   async receive(msg: Message): Promise<MessageResponse> {
 *     // Handle messages
 *   }
 *
 *   port(name: string): Channel<any> {
 *     if (name === 'status') return this.statusPort;
 *     throw new Error(\`Unknown port: \${name}\`);
 *   }
 * }
 * ```
 */
export function ActorMixin<T extends Constructor<HTMLElement>>(Base: T) {
  return class extends Base implements WidgetActor {
    readonly address: Address;
    private _isActorRegistered = false;

    constructor(...args: any[]) {
      super(...args);

      // Generate address from tag name and ID
      // Fall back to random ID if element doesn't have one
      const tagName = this.tagName?.toLowerCase() || 'widget';
      const elementId = this.id || `${tagName}-${Math.random().toString(36).substr(2, 9)}`;

      this.address = address(`widgets/${tagName}:${elementId}`);
    }

    /**
     * Receive a message (must be implemented by subclass).
     */
    async receive(_msg: Message): Promise<MessageResponse> {
      throw new Error('receive() must be implemented by Widget Actor subclass');
    }

    /**
     * Optional: Get a reactive port by name.
     * Override in subclass to expose ports.
     */
    port?(_name: string): Channel<any>;

    /**
     * Web Component lifecycle: Called when element is added to DOM.
     * Automatically registers actor with browser registry.
     */
    connectedCallback(): void {
      // Call parent connectedCallback if it exists
      if (super.connectedCallback) {
        super.connectedCallback();
      }

      // Register with actor registry
      if (!this._isActorRegistered) {
        actorRegistry.register(this.address, this);
        this._isActorRegistered = true;
      }
    }

    /**
     * Web Component lifecycle: Called when element is removed from DOM.
     * Automatically unregisters actor and cleans up resources.
     */
    disconnectedCallback(): void {
      // Unregister from actor registry
      if (this._isActorRegistered) {
        actorRegistry.unregister(this.address);
        this._isActorRegistered = false;
      }

      // Call parent disconnectedCallback if it exists
      if (super.disconnectedCallback) {
        super.disconnectedCallback();
      }
    }

    /**
     * Send a message to another actor.
     *
     * @param to - Destination actor address
     * @param msg - Message to send
     * @returns Response from the target actor
     */
    async send(to: Address, msg: Message): Promise<MessageResponse> {
      return actorRegistry.send(to, msg);
    }

    /**
     * Send a typed message to another actor (convenience method).
     *
     * @param to - Destination actor address
     * @param type - Message type
     * @param payload - Message payload
     * @returns Response from the target actor
     */
    async sendMessage<T = any>(
      to: Address,
      type: string,
      payload: any
    ): Promise<MessageResponse<T>> {
      return actorRegistry.send(to, {
        id: `msg-${Date.now()}-${Math.random()}`,
        from: this.address,
        to,
        type,
        payload,
        timestamp: Date.now(),
      });
    }
  };
}

/**
 * Base Widget Actor class (alternative to mixin).
 *
 * Extend this class instead of using the mixin if you prefer traditional inheritance.
 *
 * @example
 * ```typescript
 * class MyWidget extends BaseWidgetActor {
 *   async receive(msg: Message): Promise<MessageResponse> {
 *     if (msg.type === 'greet') {
 *       this.textContent = `Hello, ${msg.payload.name}!`;
 *       return { success: true };
 *     }
 *     return { success: false, error: 'Unknown message' };
 *   }
 * }
 *
 * customElements.define('my-widget', MyWidget);
 * ```
 */
export class BaseWidgetActor extends ActorMixin(HTMLElement) implements WidgetActor {
  /**
   * Override this to handle messages.
   */
  async receive(_msg: Message): Promise<MessageResponse> {
    return { success: false, error: 'No message handler implemented' };
  }
}

/**
 * Create a Widget Actor from a simple message handler.
 *
 * Utility for quick prototyping without defining a full class.
 *
 * @param tagName - Custom element tag name
 * @param handler - Message handler function
 * @param options - Optional configuration
 *
 * @example
 * ```typescript
 * createWidgetActor('simple-counter', async (msg, element) => {
 *   if (msg.type === 'increment') {
 *     const count = parseInt(element.textContent || '0') + 1;
 *     element.textContent = count.toString();
 *     return { success: true, data: count };
 *   }
 *   return { success: false, error: 'Unknown message' };
 * });
 *
 * // Use in HTML
 * // <simple-counter id="counter-1"></simple-counter>
 * ```
 */
export function createWidgetActor(
  tagName: string,
  handler: (msg: Message, element: BaseWidgetActor) => Promise<MessageResponse>,
  options?: {
    render?: (element: BaseWidgetActor) => void;
  }
): void {
  class SimpleWidgetActor extends BaseWidgetActor {
    async receive(msg: Message): Promise<MessageResponse> {
      return handler(msg, this);
    }

    connectedCallback(): void {
      super.connectedCallback();
      options?.render?.(this);
    }
  }

  customElements.define(tagName, SimpleWidgetActor);
}
