// Registry - Actor management and message routing

import type { Actor, Message, Response } from "./base";

export interface ActorInfo {
  actor: Actor;
  registeredAt: Date;
  messageCount: number;
  lastMessageAt?: Date;
}

export class Registry {
  private actors: Map<string, ActorInfo> = new Map();

  // Register an actor
  register(actor: Actor): void {
    if (this.actors.has(actor.id)) {
      throw new Error(`Actor already registered: ${actor.id}`);
    }

    this.actors.set(actor.id, {
      actor,
      registeredAt: new Date(),
      messageCount: 0,
    });
  }

  // Unregister an actor
  unregister(actorId: string): boolean {
    const info = this.actors.get(actorId);
    if (info?.actor.stop) {
      info.actor.stop();
    }
    return this.actors.delete(actorId);
  }

  // Get an actor by ID
  get(actorId: string): Actor | undefined {
    return this.actors.get(actorId)?.actor;
  }

  // Check if actor exists
  has(actorId: string): boolean {
    return this.actors.has(actorId);
  }

  // List all actors
  list(): Array<{ id: string; type: string; messageCount: number }> {
    return [...this.actors.entries()].map(([id, info]) => ({
      id,
      type: info.actor.type,
      messageCount: info.messageCount,
    }));
  }

  // Send message to actor (the main routing function)
  async send(actorId: string, message: Message): Promise<Response> {
    const info = this.actors.get(actorId);
    if (!info) {
      return {
        success: false,
        error: `Actor not found: ${actorId}`,
      };
    }

    info.messageCount++;
    info.lastMessageAt = new Date();

    try {
      return await info.actor.send(message);
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  // Convenience: send with just type and payload
  async sendTo(actorId: string, type: string, payload: unknown): Promise<Response> {
    const message: Message = {
      id: `msg_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      type,
      payload,
    };
    return this.send(actorId, message);
  }

  // Clear all actors
  clear(): void {
    for (const info of this.actors.values()) {
      if (info.actor.stop) {
        info.actor.stop();
      }
    }
    this.actors.clear();
  }
}

// Singleton instance
export const registry = new Registry();
