// BaseActor: Abstract base class for actors that need to send to other actors
//
// This is NEW infrastructure added in Phase 1. It will be used in Phase 2+
// when we migrate existing actors to the Hewitt Actor Model.
//
// Provides:
// - System injection via setSystem()
// - Protected sendTo() method that routes through System
// - Actor implementations extend this and implement receive()
//
// Pattern (from .actor-model-clarification.md):
// Actors NEVER directly reference System in their code.
// They call `this.sendTo(targetId, message)` - System is hidden

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

  // Required by Actor interface
  abstract send(message: Message): Promise<Response>;

  // NEW: Semantically correct receive method (optional during Phase 1-2)
  // Will become required in Phase 3
  receive?(message: Message): Promise<Response>;

  // PROTECTED: Send to other actors (goes through System)
  // Actors call this.sendTo() to communicate - no System in their code
  protected async sendTo(targetId: string, message: Message): Promise<Response> {
    if (!this.system) {
      throw new Error(`Actor ${this.id} not registered with System`);
    }

    return this.system.receive({
      id: crypto.randomUUID(),
      type: 'route',
      payload: { targetId, message },
      sender: this.id
    });
  }
}
