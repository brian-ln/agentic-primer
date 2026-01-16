// BaseActor: Abstract base class for actors that need to send to other actors
//
// Provides:
// - System injection via setSystem()
// - Protected send() method that routes through System
// - Actor implementations extend this and implement receive()
//
// Pattern:
// Actors NEVER directly reference System in their code.
// They call `this.send(targetId, message)` - System is hidden

import type { Actor, Message, Response, ActorType } from "./base";
import type { System } from "./system";

export abstract class BaseActor implements Actor {
  readonly id: string;
  readonly type: ActorType;

  protected system?: System;

  constructor(id: string, type: ActorType) {
    this.id = id;
    this.type = type;
  }

  // System injects itself during registration
  setSystem(system: System): void {
    this.system = system;
  }

  // Subclasses implement this
  abstract receive(message: Message): Promise<Response>;

  // PROTECTED: Send to other actors (routes through System)
  // NOTE: Currently blocking (request-response). Future: consider non-blocking
  // send() with async request() built on top using correlation IDs (Erlang pattern)
  protected async send(targetId: string, message: Message): Promise<Response> {
    if (!this.system) {
      // Return error Response - don't throw (actors communicate via messages)
      return {
        success: false,
        error: `Actor ${this.id} not connected to System - cannot route message`
      };
    }

    return this.system.receive({
      id: crypto.randomUUID(),
      type: 'route',
      payload: { targetId, message },
      sender: this.id
    });
  }
}
