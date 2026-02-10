/**
 * Browser Actor Registry
 *
 * Lightweight registry for browser-based actors (Widget Actors, non-DOM actors).
 * Provides address-based lookup and message routing within the browser context.
 *
 * Ported from simplify/src/messaging/browser/actor-registry.ts
 * Adapted to use @agentic-primer/actors types.
 */

import type {
  Address,
  Message,
  MessageResponse,
  MessageHandler,
} from '@agentic-primer/actors';

export class ActorNotFoundError extends Error {
  constructor(public readonly actorAddress: Address) {
    super(`Actor not found: ${actorAddress}`);
    this.name = 'ActorNotFoundError';
  }
}

export class DuplicateActorError extends Error {
  constructor(public readonly actorAddress: Address) {
    super(`Actor already registered: ${actorAddress}`);
    this.name = 'DuplicateActorError';
  }
}

/**
 * Browser-side actor registry for local actor discovery and messaging.
 */
export class BrowserActorRegistry {
  private actors = new Map<Address, MessageHandler>();

  register(actorAddress: Address, actor: MessageHandler): void {
    if (this.actors.has(actorAddress)) {
      throw new DuplicateActorError(actorAddress);
    }
    this.actors.set(actorAddress, actor);
  }

  registerOrReplace(actorAddress: Address, actor: MessageHandler): MessageHandler | undefined {
    const previous = this.actors.get(actorAddress);
    this.actors.set(actorAddress, actor);
    return previous;
  }

  unregister(actorAddress: Address): boolean {
    return this.actors.delete(actorAddress);
  }

  lookup(actorAddress: Address): MessageHandler | undefined {
    return this.actors.get(actorAddress);
  }

  has(actorAddress: Address): boolean {
    return this.actors.has(actorAddress);
  }

  async send(to: Address, msg: Message): Promise<MessageResponse> {
    const actor = this.lookup(to);
    if (!actor) {
      throw new ActorNotFoundError(to);
    }
    return actor.receive(msg);
  }

  list(): Address[] {
    return Array.from(this.actors.keys());
  }

  get size(): number {
    return this.actors.size;
  }

  clear(): void {
    this.actors.clear();
  }

  snapshot(): Map<Address, MessageHandler> {
    return new Map(this.actors);
  }
}

/** Global browser actor registry singleton */
export const actorRegistry = new BrowserActorRegistry();
