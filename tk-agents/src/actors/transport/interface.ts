
import type { Message, Response } from "../base.ts";

/**
 * Transport interface for actor communication
 * Handles sending messages across process boundaries
 */
export interface Transport {
  /**
   * Send a message to a target actor address
   * @param target Address string (e.g. "primer.knowledge")
   * @param message Message payload
   */
  send(target: string, message: Message): Promise<Response>;

  /**
   * Subscribe to messages from a source address (optional)
   * @param source Address string
   * @param callback Handler function
   */
  subscribe?(source: string, callback: (message: Message) => void): void;
}
