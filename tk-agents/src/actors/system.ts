/**
 * System implementation - provides send and maintains actor registry
 * Following ACTOR_SPEC.md
 */

import type { Actor, Address, Message, Response, SendFunction } from "./base";

/**
 * System interface - extends Actor with registration capability
 * Systems ARE actors (uniform composition)
 */
export interface System extends Actor {
  /**
   * Send function that can be used both as:
   * 1. SendFunction - send(targetId, message) for actor-to-actor
   * 2. Actor.send - send(message) for external -> actor
   */
  send: SendFunction & ((message: Message) => Promise<Response>);

  /**
   * Register an actor with an address
   */
  register: (address: Address, actor: Actor) => void;
}

/**
 * Create a new actor system
 *
 * The system:
 * - Provides the send implementation
 * - Maintains the actor registry
 * - Routes messages to actors
 * - IS an actor itself (uniform composition)
 */
export function createSystem(): System {
  const actors = new Map<Address, Actor>();

  // The send primitive implementation
  const send: SendFunction = async (targetAddress: Address, message: Message) => {
    const actor = actors.get(targetAddress);
    if (!actor) {
      return {
        success: false,
        error: `Actor not found: ${targetAddress}`,
      };
    }
    return actor.send(message);
  };

  // Register an actor
  const register = (address: Address, actor: Actor) => {
    actors.set(address, actor);
  };

  // System as actor - when messages sent to system itself
  // This enables uniform composition (systems in systems)
  const systemAsSend = async (message: Message) => {
    // System can handle meta-messages like "list", "stats", etc.
    if (message.type === "list") {
      return {
        success: true,
        data: Array.from(actors.keys()),
      };
    }

    if (message.type === "stats") {
      return {
        success: true,
        data: { actorCount: actors.size },
      };
    }

    // Route to specific actor if targetAddress in payload
    if (
      message.type === "route" &&
      typeof message.payload === "object" &&
      message.payload !== null &&
      "targetAddress" in message.payload &&
      "message" in message.payload
    ) {
      const { targetAddress, message: innerMessage } = message.payload as {
        targetAddress: Address;
        message: Message;
      };
      return send(targetAddress, innerMessage);
    }

    return {
      success: false,
      error: `Unknown system message type: ${message.type}`,
    };
  };

  // Create the dual-purpose send function
  // It can be called with 1 arg (as Actor) or 2 args (as SendFunction)
  const dualSend = ((...args: unknown[]) => {
    if (args.length === 1) {
      // Called as actor.send(message)
      return systemAsSend(args[0] as Message);
    } else if (args.length === 2) {
      // Called as send(targetId, message)
      return send(args[0] as string, args[1] as Message);
    }
    throw new Error("Invalid send call - expected 1 or 2 arguments");
  }) as SendFunction & ((message: Message) => Promise<Response>);

  return {
    send: dualSend,
    register,
  };
}
