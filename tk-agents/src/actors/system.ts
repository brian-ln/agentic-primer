/**
 * System implementation - provides send and maintains actor registry
 * Following ACTOR_SYSTEM.spec.md
 */

import type { Actor, Address, Message, Response, SendFunction } from "./base";

/**
 * System interface
 * Provides messaging infrastructure and actor registration
 */
export interface System {
  /**
   * Send function - routes messages to actors by address
   */
  send: SendFunction;

  /**
   * Register an actor and return its Address proxy
   * The returned Address has both identity (__id) and behavior (.send())
   */
  register: (actor: Actor) => Address;
}

/**
 * Create a new actor system
 *
 * The system:
 * - Provides the send implementation
 * - Maintains the actor registry (symbol -> actor mapping)
 * - Creates Address proxies with .send() methods
 * - Routes messages to actors
 */
export function System(): System {
  const actors = new Map<symbol, Actor>();

  // The send primitive implementation
  const send: SendFunction = async (addr: Address, message: Message) => {
    const actor = actors.get(addr.__id);
    if (!actor) {
      return {
        success: false,
        error: "Actor not found",
      };
    }
    return actor.send(message);
  };

  // Register an actor and return Address proxy
  const register = (actor: Actor): Address => {
    const id = Symbol();
    actors.set(id, actor);

    // Create Address proxy with .send() method
    const address: Address = {
      __id: id,
      send: (message: Message) => send(address, message),
    };

    return address;
  };

  return {
    send,
    register,
  };
}
