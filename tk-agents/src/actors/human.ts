// HumanActor - Represents human input in the actor system
//
// Design: Minimal viable actor that follows the same message protocol
// as other actors, allowing humans to participate in actor workflows.
//
// Current implementation: Simple stub that signals human response needed.
// Future: Could integrate with stdin, callbacks, or UI for actual input.

import type { Actor, Message, Response } from "./base";

export interface HumanActorConfig {
  id: string;
  /** Optional callback to request human input */
  onInputNeeded?: (message: Message) => Promise<string>;
}

/**
 * HumanActor - Actor representing human input/decision-making
 *
 * This actor type represents a human participant in the system.
 * It follows the same message protocol as other actors, making
 * humans first-class participants in multi-agent workflows.
 *
 * Current implementation returns "human response needed" message.
 * Future enhancements could add stdin input, callback integration,
 * or web UI for collecting human responses.
 */
export class HumanActor implements Actor {
  readonly id: string;
  readonly type = "agent" as const;  // Humans are non-deterministic decision-makers

  private onInputNeeded?: (message: Message) => Promise<string>;
  private pendingMessages: Message[] = [];

  constructor(config: HumanActorConfig) {
    this.id = config.id;
    this.onInputNeeded = config.onInputNeeded;
  }

  async send(message: Message): Promise<Response> {
    // Handle ping for heartbeat
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Store message for potential future retrieval
    this.pendingMessages.push(message);

    // If callback provided, use it to get human input
    if (this.onInputNeeded) {
      try {
        const humanResponse = await this.onInputNeeded(message);
        return {
          success: true,
          data: { humanResponse },
          metadata: { respondedAt: Date.now() },
        };
      } catch (error) {
        return {
          success: false,
          error: `Human input failed: ${error instanceof Error ? error.message : String(error)}`,
        };
      }
    }

    // Default: Signal that human response is needed
    return {
      success: true,
      data: {
        status: "awaiting_human_response",
        message: "Human input required",
        prompt: message.payload,
      },
      metadata: {
        messageType: message.type,
        messageId: message.id,
      },
    };
  }

  // NEW: Semantically correct method (Hewitt Actor Model)
  // During Phase 2, this delegates to send() for backward compatibility
  async receive(message: Message): Promise<Response> {
    return this.send(message);
  }

  /**
   * Get pending messages awaiting human response
   */
  getPendingMessages(): Message[] {
    return [...this.pendingMessages];
  }

  /**
   * Clear pending messages (e.g., after processing)
   */
  clearPendingMessages(): void {
    this.pendingMessages = [];
  }
}

/**
 * Factory function to create HumanActor
 */
export function createHumanActor(config: HumanActorConfig): HumanActor {
  return new HumanActor(config);
}
