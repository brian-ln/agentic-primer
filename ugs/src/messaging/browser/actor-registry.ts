/**
 * Browser-side Actor Registry
 *
 * Lightweight registry for Widget Actors and other browser-based actors.
 * Provides address-based lookup and message routing within the browser context.
 */

import type { Actor } from '../actor.ts';
import type { Address, Message, MessageResponse } from '../message.ts';
import { address } from '../message.ts';

/**
 * Error thrown when attempting to send to an unknown actor.
 */
export class ActorNotFoundError extends Error {
  constructor(public readonly actorAddress: Address) {
    super(`Actor not found: ${actorAddress}`);
    this.name = 'ActorNotFoundError';
  }
}

/**
 * Error thrown when attempting to register an actor with a duplicate address.
 */
export class DuplicateActorError extends Error {
  constructor(public readonly actorAddress: Address) {
    super(`Actor already registered: ${actorAddress}`);
    this.name = 'DuplicateActorError';
  }
}

/**
 * Browser-side actor registry for local actor discovery and messaging.
 *
 * Provides a global registry for actors running in the browser:
 * - Widget Actors (Web Components implementing Actor interface)
 * - Non-DOM actors (pure TypeScript actors)
 * - Future: Remote actor proxies (WebSocket bridge to backend)
 *
 * @example
 * ```typescript
 * // Register an actor
 * const actor = new MyActor();
 * actorRegistry.register(address('domain/my-actor'), actor);
 *
 * // Send a message
 * const response = await actorRegistry.send(
 *   address('domain/my-actor'),
 *   { type: 'greet', payload: { name: 'World' } }
 * );
 *
 * // Cleanup
 * actorRegistry.unregister(address('domain/my-actor'));
 * ```
 */
export class BrowserActorRegistry {
  private actors = new Map<Address, Actor>();

  /**
   * Register an actor with the given address.
   * Throws if an actor with this address is already registered.
   *
   * @param actorAddress - The unique address for this actor
   * @param actor - The actor instance
   */
  register(actorAddress: Address, actor: Actor): void {
    if (this.actors.has(actorAddress)) {
      throw new DuplicateActorError(actorAddress);
    }
    this.actors.set(actorAddress, actor);
  }

  /**
   * Register an actor, replacing any existing actor at this address.
   * Useful for hot-reloading or component updates.
   *
   * @param actorAddress - The address for this actor
   * @param actor - The actor instance
   * @returns The previous actor at this address, if any
   */
  registerOrReplace(actorAddress: Address, actor: Actor): Actor | undefined {
    const previous = this.actors.get(actorAddress);
    this.actors.set(actorAddress, actor);
    return previous;
  }

  /**
   * Unregister an actor at the given address.
   * Safe to call even if actor is not registered (idempotent).
   *
   * @param actorAddress - The address to unregister
   * @returns true if actor was removed, false if not found
   */
  unregister(actorAddress: Address): boolean {
    return this.actors.delete(actorAddress);
  }

  /**
   * Look up an actor by address.
   *
   * @param actorAddress - The address to look up
   * @returns The actor instance, or undefined if not found
   */
  lookup(actorAddress: Address): Actor | undefined {
    return this.actors.get(actorAddress);
  }

  /**
   * Check if an actor is registered at the given address.
   *
   * @param actorAddress - The address to check
   * @returns true if an actor is registered at this address
   */
  has(actorAddress: Address): boolean {
    return this.actors.has(actorAddress);
  }

  /**
   * Send a message to an actor by address.
   * Throws ActorNotFoundError if actor is not registered.
   *
   * @param to - The destination actor address
   * @param msg - The message to send
   * @returns The actor's response
   *
   * @example
   * ```typescript
   * const response = await actorRegistry.send(
   *   address('domain/chat-room'),
   *   { type: 'send-message', payload: { text: 'Hello!' } }
   * );
   * ```
   */
  async send(to: Address, msg: Message): Promise<MessageResponse> {
    const actor = this.lookup(to);
    if (!actor) {
      throw new ActorNotFoundError(to);
    }
    return actor.receive(msg);
  }

  /**
   * List all registered actor addresses.
   * Useful for debugging and dev tools.
   *
   * @returns Array of all registered addresses
   */
  list(): Address[] {
    return Array.from(this.actors.keys());
  }

  /**
   * Get the number of registered actors.
   */
  get size(): number {
    return this.actors.size;
  }

  /**
   * Clear all registered actors.
   * Useful for cleanup in tests.
   */
  clear(): void {
    this.actors.clear();
  }

  /**
   * Get a snapshot of all actors and their addresses.
   * Returns a Map for efficient lookups by tests/dev tools.
   */
  snapshot(): Map<Address, Actor> {
    return new Map(this.actors);
  }
}

/**
 * Global browser actor registry singleton.
 * Use this to register and look up actors in browser context.
 *
 * @example
 * ```typescript
 * // Widget Actor auto-registration
 * class ChatMessage extends HTMLElement implements Actor {
 *   connectedCallback() {
 *     actorRegistry.register(this.address, this);
 *   }
 *
 *   disconnectedCallback() {
 *     actorRegistry.unregister(this.address);
 *   }
 * }
 * ```
 */
export const actorRegistry = new BrowserActorRegistry();
