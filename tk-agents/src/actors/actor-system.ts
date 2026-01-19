/**
 * Actor System - Message routing infrastructure
 *
 * Lightweight actor system for message-based communication
 */

export interface ActorMessage {
  type: string;
  payload: any;
  sender?: string;
  timestamp?: string;
}

export class ActorSystem {
  private actors: Map<string, (message: ActorMessage) => Promise<any>> = new Map();

  register(id: string, handler: (message: ActorMessage) => Promise<any>): void {
    this.actors.set(id, handler);
  }

  unregister(id: string): void {
    this.actors.delete(id);
  }

  async send(actorId: string, messageType: string, payload: any): Promise<any> {
    const handler = this.actors.get(actorId);
    if (!handler) {
      throw new Error(`Actor not found: ${actorId}`);
    }

    return handler({
      type: messageType,
      payload,
      timestamp: new Date().toISOString(),
    });
  }

  has(actorId: string): boolean {
    return this.actors.has(actorId);
  }

  get(actorId: string): ((message: ActorMessage) => Promise<any>) | undefined {
    return this.actors.get(actorId);
  }
}
