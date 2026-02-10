/**
 * Widget Actor - Web Components as Actors (mixin pattern)
 *
 * Bridges Web Components and the Actor messaging protocol.
 * Two patterns available:
 * 1. ActorMixin(HTMLElement) - mixin for any HTMLElement class
 * 2. BaseWidgetActor - concrete base class (extends ActorMixin(HTMLElement))
 *
 * Widget Actors automatically register/unregister with the BrowserActorRegistry
 * on DOM connect/disconnect.
 *
 * Ported from simplify/src/messaging/browser/widget-actor.ts
 * Adapted to use @agentic-primer/actors types.
 */

import type {
  Address,
  Message,
  MessageResponse,
} from '@agentic-primer/actors';
import { address } from '@agentic-primer/actors';
import { actorRegistry } from './actor-registry.ts';

export type Constructor<T = {}> = new (...args: any[]) => T;

export interface WidgetActor extends HTMLElement {
  readonly actorAddress: Address;
  receive(msg: Message): Promise<MessageResponse>;
  connectedCallback(): void;
  disconnectedCallback(): void;
}

/**
 * Actor mixin for Web Components.
 * Adds actor protocol to any HTMLElement-based class.
 */
export function ActorMixin<T extends Constructor<HTMLElement>>(Base: T) {
  return class extends Base implements WidgetActor {
    readonly actorAddress: Address;
    /** @internal */ _isActorRegistered = false;

    constructor(...args: any[]) {
      super(...args);

      const tagName = this.tagName?.toLowerCase() || 'widget';
      const elementId = this.id || `${tagName}-${Math.random().toString(36).substr(2, 9)}`;
      this.actorAddress = address(`widgets/${tagName}:${elementId}`);
    }

    async receive(_msg: Message): Promise<MessageResponse> {
      throw new Error('receive() must be implemented by Widget Actor subclass');
    }

    connectedCallback(): void {
      if (!this._isActorRegistered) {
        actorRegistry.register(this.actorAddress, this);
        this._isActorRegistered = true;
      }
    }

    disconnectedCallback(): void {
      if (this._isActorRegistered) {
        actorRegistry.unregister(this.actorAddress);
        this._isActorRegistered = false;
      }
    }

    async send(to: Address, msg: Message): Promise<MessageResponse> {
      return actorRegistry.send(to, msg);
    }
  };
}

/**
 * Base Widget Actor class (alternative to mixin).
 * Extend this if you prefer traditional inheritance over mixin.
 */
export class BaseWidgetActor extends ActorMixin(HTMLElement) implements WidgetActor {
  async receive(_msg: Message): Promise<MessageResponse> {
    return {
      id: '',
      correlationId: '',
      from: this.actorAddress,
      to: undefined,
      success: false,
      error: 'No message handler implemented',
      timestamp: Date.now(),
    };
  }
}

/**
 * Create a Widget Actor from a simple message handler function.
 * Utility for quick prototyping without defining a full class.
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
