
import { HttpTransport } from "./transport/http.ts";
import type { Message, Response } from "./base.ts";

/**
 * Generic Actor Client
 * Communicates with actors in the daemon via transport
 */
export class ActorClient {
  private transport = new HttpTransport();

  /**
   * Send a message to a target actor
   * @param target Hierarchical address or node ID
   * @param type Message type
   * @param payload Data payload
   */
  async send(target: string, type: string, payload: any = {}): Promise<Response> {
    const message: Message = {
      id: crypto.randomUUID(),
      type,
      payload,
    };

    return this.transport.send(target, message);
  }
}
