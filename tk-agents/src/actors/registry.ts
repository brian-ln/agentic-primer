// Registry - Actor management and message routing

import { EventEmitter } from "events";
import type { Actor, Message, Response } from "./base";
import { MailboxManagerActor } from "./mailbox-manager";

export interface ActorInfo {
  actor: Actor;
  registeredAt: Date;
  messageCount: number;
  lastMessageAt?: Date;
  lastSuccessAt?: Date;
  heartbeatInterval?: NodeJS.Timeout;
  processingLoop?: NodeJS.Timeout; // Message processing interval
}

export class Registry extends EventEmitter {
  private actors: Map<string, ActorInfo> = new Map();
  private mailboxManager: MailboxManagerActor;
  private processingIntervalMs: number = 10; // Process messages every 10ms

  constructor() {
    super();
    this.mailboxManager = new MailboxManagerActor("registry-mailbox-manager");
  }

  // Register an actor
  register(actor: Actor): void {
    if (this.actors.has(actor.id)) {
      throw new Error(`Actor already registered: ${actor.id}`);
    }

    // Create mailbox for actor
    const mailboxResult = await this.mailboxManager.send({
      id: `create_mailbox_${actor.id}`,
      type: "create_mailbox",
      payload: { actorId: actor.id },
    });

    if (!mailboxResult.success) {
      throw new Error(`Failed to create mailbox for ${actor.id}: ${mailboxResult.error}`);
    }

    const info: ActorInfo = {
      actor,
      registeredAt: new Date(),
      messageCount: 0,
    };

    this.actors.set(actor.id, info);

    // Start message processing loop for this actor
    this.startMessageProcessing(actor.id);
  }

  // Unregister an actor
  unregister(actorId: string): boolean {
    const info = this.actors.get(actorId);
    if (info) {
      this.stopHeartbeat(actorId);
      if (info.actor.stop) {
        info.actor.stop();
      }
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
      const response = await info.actor.send(message);
      info.lastSuccessAt = new Date(); // Track last successful message
      return response;
    } catch (error) {
      // Actor crashed!
      this.emit('actor_died', {
        actorId,
        error: error instanceof Error ? error.message : String(error),
        reason: 'exception',
        lastMessageAt: info.lastMessageAt,
      });

      // Remove dead actor from registry
      this.actors.delete(actorId);

      return {
        success: false,
        error: `Actor ${actorId} died: ${error instanceof Error ? error.message : String(error)}`,
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
    for (const [actorId, info] of this.actors.entries()) {
      this.stopHeartbeat(actorId);
      if (info.actor.stop) {
        info.actor.stop();
      }
    }
    this.actors.clear();
  }

  /**
   * Start heartbeat monitoring for an actor
   * Pings actor periodically, emits 'actor_died' if ping fails
   */
  startHeartbeat(actorId: string, intervalMs: number = 30000): void {
    const info = this.actors.get(actorId);
    if (!info) {
      throw new Error(`Cannot start heartbeat: Actor not found ${actorId}`);
    }

    // Clear existing heartbeat if any
    if (info.heartbeatInterval) {
      clearInterval(info.heartbeatInterval);
    }

    info.heartbeatInterval = setInterval(async () => {
      // Check if actor still exists (might have been removed by exception handler)
      const currentInfo = this.actors.get(actorId);
      if (!currentInfo) {
        // Actor was already removed, clean up interval
        if (info.heartbeatInterval) {
          clearInterval(info.heartbeatInterval);
        }
        return;
      }

      const response = await this.send(actorId, { type: 'ping', id: `heartbeat_${Date.now()}`, payload: {} });

      // If send() returned an error, the actor is dead
      // Note: send() already emitted 'actor_died' if there was an exception
      if (!response.success) {
        // Actor already removed by send() if it threw exception
        // Just clean up the interval
        if (currentInfo.heartbeatInterval) {
          clearInterval(currentInfo.heartbeatInterval);
        }
      }
    }, intervalMs);
  }

  /**
   * Stop heartbeat monitoring
   */
  stopHeartbeat(actorId: string): void {
    const info = this.actors.get(actorId);
    if (info?.heartbeatInterval) {
      clearInterval(info.heartbeatInterval);
      info.heartbeatInterval = undefined;
    }
  }
}

// Singleton instance
export const registry = new Registry();
