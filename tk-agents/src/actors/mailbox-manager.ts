// MailboxManager Actor - Manages mailboxes for other actors
// Follows actor model: Everything is an actor, all interactions are messages

import type { Actor, Message, Response, ActorType } from "./base";
import type { MailboxConfig } from "./mailbox";
import { Mailbox } from "./mailbox";

/**
 * MailboxManager Actor
 *
 * Responsibilities:
 * - Create mailboxes for actors
 * - Enqueue messages to actor mailboxes
 * - Dequeue messages from actor mailboxes
 * - Report mailbox status (size, capacity, full/empty)
 * - Manage mailbox lifecycle
 *
 * Message Types:
 * - create_mailbox: Create a new mailbox for an actor
 * - enqueue: Add message to an actor's mailbox
 * - dequeue: Get next message from an actor's mailbox
 * - peek: Look at next message without removing
 * - status: Get mailbox status
 * - clear: Clear all messages from a mailbox
 * - delete_mailbox: Remove an actor's mailbox
 */

interface CreateMailboxPayload {
  actorId: string;
  config?: MailboxConfig;
}

interface MailboxOperationPayload {
  actorId: string;
  message?: Message; // For enqueue
}

interface MailboxStatusResponse {
  exists: boolean;
  size?: number;
  maxSize?: number;
  isFull?: boolean;
  isEmpty?: boolean;
  availableCapacity?: number;
}

export class MailboxManagerActor implements Actor {
  readonly id: string;
  readonly type: ActorType = "deterministic";

  private mailboxes = new Map<string, Mailbox>();
  private readonly defaultConfig: MailboxConfig = { maxSize: 1000 };

  constructor(id: string = "mailbox-manager") {
    this.id = id;
  }

  async send(message: Message): Promise<Response> {
    try {
      switch (message.type) {
        case "create_mailbox":
          return this.handleCreateMailbox(message.payload as CreateMailboxPayload);

        case "enqueue":
          return this.handleEnqueue(message.payload as MailboxOperationPayload);

        case "dequeue":
          return this.handleDequeue(message.payload as MailboxOperationPayload);

        case "peek":
          return this.handlePeek(message.payload as MailboxOperationPayload);

        case "status":
          return this.handleStatus(message.payload as MailboxOperationPayload);

        case "clear":
          return this.handleClear(message.payload as MailboxOperationPayload);

        case "delete_mailbox":
          return this.handleDeleteMailbox(message.payload as MailboxOperationPayload);

        default:
          return {
            success: false,
            error: {
              message: `Unknown message type: ${message.type}`,
              category: "validation",
              retryable: false,
            },
          };
      }
    } catch (error) {
      return {
        success: false,
        error: {
          message: error instanceof Error ? error.message : String(error),
          category: "fatal",
          retryable: false,
          cause: error instanceof Error ? error : undefined,
        },
      };
    }
  }

  private handleCreateMailbox(payload: CreateMailboxPayload): Response {
    const { actorId, config } = payload;

    if (!actorId) {
      return {
        success: false,
        error: {
          message: "actorId is required",
          category: "validation",
          retryable: false,
        },
      };
    }

    if (this.mailboxes.has(actorId)) {
      return {
        success: false,
        error: {
          message: `Mailbox already exists for actor: ${actorId}`,
          category: "validation",
          retryable: false,
        },
      };
    }

    const mailbox = new Mailbox(config || this.defaultConfig);
    this.mailboxes.set(actorId, mailbox);

    return {
      success: true,
      data: {
        actorId,
        maxSize: mailbox.isFull() ? 0 : this.defaultConfig.maxSize,
      },
    };
  }

  private handleEnqueue(payload: MailboxOperationPayload): Response {
    const { actorId, message } = payload;

    if (!actorId || !message) {
      return {
        success: false,
        error: {
          message: "actorId and message are required",
          category: "validation",
          retryable: false,
        },
      };
    }

    const mailbox = this.mailboxes.get(actorId);
    if (!mailbox) {
      return {
        success: false,
        error: {
          message: `No mailbox exists for actor: ${actorId}`,
          category: "validation",
          retryable: false,
        },
      };
    }

    const enqueued = mailbox.enqueue(message);
    if (!enqueued) {
      return {
        success: false,
        error: {
          message: `Mailbox full for actor: ${actorId}`,
          category: "transient",
          retryable: true, // Can retry when mailbox has space
        },
      };
    }

    return {
      success: true,
      data: {
        actorId,
        size: mailbox.size(),
        availableCapacity: mailbox.availableCapacity(),
      },
    };
  }

  private handleDequeue(payload: MailboxOperationPayload): Response {
    const { actorId } = payload;

    if (!actorId) {
      return {
        success: false,
        error: {
          message: "actorId is required",
          category: "validation",
          retryable: false,
        },
      };
    }

    const mailbox = this.mailboxes.get(actorId);
    if (!mailbox) {
      return {
        success: false,
        error: {
          message: `No mailbox exists for actor: ${actorId}`,
          category: "validation",
          retryable: false,
        },
      };
    }

    const message = mailbox.dequeue();
    return {
      success: true,
      data: {
        message, // undefined if mailbox was empty
        size: mailbox.size(),
      },
    };
  }

  private handlePeek(payload: MailboxOperationPayload): Response {
    const { actorId } = payload;

    if (!actorId) {
      return {
        success: false,
        error: {
          message: "actorId is required",
          category: "validation",
          retryable: false,
        },
      };
    }

    const mailbox = this.mailboxes.get(actorId);
    if (!mailbox) {
      return {
        success: false,
        error: {
          message: `No mailbox exists for actor: ${actorId}`,
          category: "validation",
          retryable: false,
        },
      };
    }

    const message = mailbox.peek();
    return {
      success: true,
      data: {
        message, // undefined if mailbox is empty
        size: mailbox.size(),
      },
    };
  }

  private handleStatus(payload: MailboxOperationPayload): Response<MailboxStatusResponse> {
    const { actorId } = payload;

    if (!actorId) {
      return {
        success: false,
        error: {
          message: "actorId is required",
          category: "validation",
          retryable: false,
        },
      };
    }

    const mailbox = this.mailboxes.get(actorId);
    if (!mailbox) {
      return {
        success: true,
        data: {
          exists: false,
        },
      };
    }

    return {
      success: true,
      data: {
        exists: true,
        size: mailbox.size(),
        maxSize: this.defaultConfig.maxSize,
        isFull: mailbox.isFull(),
        isEmpty: mailbox.isEmpty(),
        availableCapacity: mailbox.availableCapacity(),
      },
    };
  }

  private handleClear(payload: MailboxOperationPayload): Response {
    const { actorId } = payload;

    if (!actorId) {
      return {
        success: false,
        error: {
          message: "actorId is required",
          category: "validation",
          retryable: false,
        },
      };
    }

    const mailbox = this.mailboxes.get(actorId);
    if (!mailbox) {
      return {
        success: false,
        error: {
          message: `No mailbox exists for actor: ${actorId}`,
          category: "validation",
          retryable: false,
        },
      };
    }

    mailbox.clear();
    return {
      success: true,
      data: {
        actorId,
        size: 0,
      },
    };
  }

  private handleDeleteMailbox(payload: MailboxOperationPayload): Response {
    const { actorId } = payload;

    if (!actorId) {
      return {
        success: false,
        error: {
          message: "actorId is required",
          category: "validation",
          retryable: false,
        },
      };
    }

    const existed = this.mailboxes.delete(actorId);
    return {
      success: true,
      data: {
        actorId,
        existed,
      },
    };
  }

  /**
   * Utility: List all managed mailboxes with their status
   */
  listMailboxes(): Array<{ actorId: string; size: number; maxSize: number; isFull: boolean }> {
    const result: Array<{ actorId: string; size: number; maxSize: number; isFull: boolean }> = [];

    for (const [actorId, mailbox] of this.mailboxes.entries()) {
      result.push({
        actorId,
        size: mailbox.size(),
        maxSize: this.defaultConfig.maxSize,
        isFull: mailbox.isFull(),
      });
    }

    return result;
  }
}
