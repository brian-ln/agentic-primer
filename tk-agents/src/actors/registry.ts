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

// Pending response tracker
interface PendingResponse {
  resolve: (response: Response) => void;
  reject: (error: Error) => void;
}

export class Registry extends EventEmitter {
  private actors: Map<string, ActorInfo> = new Map();
  private mailboxManager: MailboxManagerActor;
  private processingIntervalMs: number = 10; // Process messages every 10ms
  private pendingResponses: Map<string, PendingResponse> = new Map();
  private useMailboxes: boolean = true; // Feature flag for mailbox integration

  constructor() {
    super();
    this.mailboxManager = new MailboxManagerActor("registry-mailbox-manager");
  }

  // Register an actor
  register(actor: Actor): void {
    if (this.actors.has(actor.id)) {
      throw new Error(`Actor already registered: ${actor.id}`);
    }

    const info: ActorInfo = {
      actor,
      registeredAt: new Date(),
      messageCount: 0,
    };

    this.actors.set(actor.id, info);

    // Create mailbox and start processing loop if mailboxes enabled
    if (this.useMailboxes) {
      this.ensureMailboxAndProcessing(actor.id);
    }
  }

  // Unregister an actor
  unregister(actorId: string): boolean {
    const info = this.actors.get(actorId);
    if (info) {
      this.stopHeartbeat(actorId);
      this.stopMessageProcessing(actorId);
      if (info.actor.stop) {
        info.actor.stop();
      }

      // Delete mailbox if using mailboxes
      if (this.useMailboxes) {
        this.mailboxManager.send({
          id: `delete_mailbox_${actorId}`,
          type: "delete_mailbox",
          payload: { actorId },
        });
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
    return Array.from(this.actors.entries()).map(([id, info]) => ({
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

    // Populate sender if not already set
    if (!message.sender) {
      message.sender = "registry";
    }

    info.messageCount++;
    info.lastMessageAt = new Date();

    // If mailboxes disabled, use direct delivery (original behavior)
    if (!this.useMailboxes) {
      return this.sendDirect(actorId, message, info);
    }

    // Mailbox-based delivery with promise-based response
    return new Promise((resolve, reject) => {
      // Store response handlers
      this.pendingResponses.set(message.id, { resolve, reject });

      // Enqueue message
      this.mailboxManager.send({
        id: `enqueue_${actorId}_${Date.now()}`,
        type: "enqueue",
        payload: { actorId, message },
      }).then((enqueueResult) => {
        if (!enqueueResult.success) {
          // Failed to enqueue - reject immediately
          this.pendingResponses.delete(message.id);
          const errorMsg = typeof enqueueResult.error === 'string'
            ? enqueueResult.error
            : (enqueueResult.error as { message?: string })?.message || 'Unknown error';

          resolve({
            success: false,
            error: `Failed to enqueue message for ${actorId}: ${errorMsg}`,
          });
        }
        // Message enqueued - response will come from processing loop
      }).catch((error) => {
        this.pendingResponses.delete(message.id);
        resolve({
          success: false,
          error: `Enqueue error: ${error instanceof Error ? error.message : String(error)}`,
        });
      });
    });
  }

  // Direct send (original synchronous behavior)
  private async sendDirect(actorId: string, message: Message, info: ActorInfo): Promise<Response> {
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
      sender: "registry",
    };
    return this.send(actorId, message);
  }

  // Clear all actors
  clear(): void {
    for (const [actorId, info] of Array.from(this.actors.entries())) {
      this.stopHeartbeat(actorId);
      this.stopMessageProcessing(actorId);
      if (info.actor.stop) {
        info.actor.stop();
      }
    }
    this.actors.clear();
    this.pendingResponses.clear();
  }

  /**
   * Ensure mailbox exists and processing loop is running for an actor
   */
  private ensureMailboxAndProcessing(actorId: string): void {
    const info = this.actors.get(actorId);
    if (!info) return;

    // Create mailbox (idempotent - will fail if already exists, which is fine)
    this.mailboxManager.send({
      id: `create_mailbox_${actorId}`,
      type: "create_mailbox",
      payload: { actorId },
    }).catch(() => {
      // Ignore errors - mailbox might already exist
    });

    // Start processing loop if not already running
    if (!info.processingLoop) {
      this.startMessageProcessing(actorId);
    }
  }

  /**
   * Start message processing loop for an actor
   * Continuously dequeues messages from mailbox and delivers to actor
   */
  private startMessageProcessing(actorId: string): void {
    const info = this.actors.get(actorId);
    if (!info || info.processingLoop) return;

    info.processingLoop = setInterval(async () => {
      const currentInfo = this.actors.get(actorId);
      if (!currentInfo) {
        // Actor removed, clean up
        if (info.processingLoop) {
          clearInterval(info.processingLoop);
        }
        return;
      }

      // Dequeue next message
      const dequeueResult = await this.mailboxManager.send({
        id: `dequeue_${actorId}_${Date.now()}`,
        type: "dequeue",
        payload: { actorId },
      });

      const dequeueData = dequeueResult.data as { message?: Message; size?: number };
      if (!dequeueResult.success || !dequeueData?.message) {
        // No message available or error - continue polling
        return;
      }

      const message = dequeueData.message;

      // Deliver message to actor
      try {
        const response = await currentInfo.actor.send(message);
        currentInfo.lastSuccessAt = new Date();

        // Resolve pending promise if exists
        const pending = this.pendingResponses.get(message.id);
        if (pending) {
          pending.resolve(response);
          this.pendingResponses.delete(message.id);
        }
      } catch (error) {
        // Actor crashed!
        this.emit('actor_died', {
          actorId,
          error: error instanceof Error ? error.message : String(error),
          reason: 'exception',
          lastMessageAt: currentInfo.lastMessageAt,
        });

        // Resolve/reject pending promise
        const pending = this.pendingResponses.get(message.id);
        if (pending) {
          pending.resolve({
            success: false,
            error: `Actor ${actorId} died: ${error instanceof Error ? error.message : String(error)}`,
          });
          this.pendingResponses.delete(message.id);
        }

        // Clean up
        this.stopMessageProcessing(actorId);
        this.actors.delete(actorId);
      }
    }, this.processingIntervalMs);
  }

  /**
   * Stop message processing loop for an actor
   */
  private stopMessageProcessing(actorId: string): void {
    const info = this.actors.get(actorId);
    if (info?.processingLoop) {
      clearInterval(info.processingLoop);
      info.processingLoop = undefined;
    }
  }

  /**
   * Get mailbox status for an actor
   */
  async getMailboxStatus(actorId: string): Promise<Response> {
    if (!this.useMailboxes) {
      return {
        success: false,
        error: "Mailboxes are disabled",
      };
    }

    return this.mailboxManager.send({
      id: `status_${actorId}_${Date.now()}`,
      type: "status",
      payload: { actorId },
    });
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
