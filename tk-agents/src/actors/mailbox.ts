// Simple bounded mailbox for actor message queueing
// KISS principle: Just a queue with a size limit

import { Message } from "./base";

export interface MailboxConfig {
  maxSize: number; // Reasonable default: 1000 messages
}

export class Mailbox {
  private queue: Message[] = [];
  private readonly maxSize: number;

  constructor(config: MailboxConfig = { maxSize: 1000 }) {
    this.maxSize = config.maxSize;
  }

  /**
   * Enqueue a message. Returns false if mailbox is full.
   */
  enqueue(message: Message): boolean {
    if (this.queue.length >= this.maxSize) {
      return false; // Reject message - mailbox full
    }
    this.queue.push(message);
    return true;
  }

  /**
   * Dequeue a message. Returns undefined if mailbox is empty.
   */
  dequeue(): Message | undefined {
    return this.queue.shift();
  }

  /**
   * Peek at the next message without removing it.
   */
  peek(): Message | undefined {
    return this.queue[0];
  }

  /**
   * Get current queue size.
   */
  size(): number {
    return this.queue.length;
  }

  /**
   * Check if mailbox is empty.
   */
  isEmpty(): boolean {
    return this.queue.length === 0;
  }

  /**
   * Check if mailbox is full.
   */
  isFull(): boolean {
    return this.queue.length >= this.maxSize;
  }

  /**
   * Clear all messages.
   */
  clear(): void {
    this.queue = [];
  }

  /**
   * Get available capacity.
   */
  availableCapacity(): number {
    return this.maxSize - this.queue.length;
  }
}
