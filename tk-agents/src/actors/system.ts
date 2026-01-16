// System: The Actor That Manages Actors
//
// Implements the Hewitt Actor Model where System is itself an actor
// that manages other actors through message passing.
//
// System receives messages to:
// - route: Route messages to contained actors
// - register: Register new actors
// - unregister: Unregister actors
// - list: List all registered actors
// - ping: Health check

import type { Actor, Message, Response } from "./base";

export class System implements Actor {
  readonly id = "system";
  readonly type = "deterministic" as const;

  private actors: Map<string, Actor> = new Map();

  // PUBLIC: System receives messages from outside
  async receive(message: Message): Promise<Response> {
    switch (message.type) {
      case 'route':
        // Route message to contained actor
        const routePayload = message.payload as { targetId: string; message: Message };
        return this.routeToActor(routePayload.targetId, routePayload.message);

      case 'register':
        // Register new actor
        const regPayload = message.payload as { actor: Actor };
        return this.registerActor(regPayload.actor);

      case 'unregister':
        // Unregister actor
        const unregPayload = message.payload as { actorId: string };
        return this.unregisterActor(unregPayload.actorId);

      case 'list':
        // List all actors
        return this.listActors();

      case 'ping':
        // System responds to ping
        return { success: true, data: { alive: true, timestamp: Date.now() } };

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  }

  // PRIVATE: Route to contained actor
  private async routeToActor(actorId: string, message: Message): Promise<Response> {
    const actor = this.actors.get(actorId);
    if (!actor) {
      return { success: false, error: `Actor not found: ${actorId}` };
    }

    // Call actor's receive() method
    return actor.receive(message);
  }

  // PRIVATE: Registration management
  private registerActor(actor: Actor): Response {
    if (this.actors.has(actor.id)) {
      return { success: false, error: `Actor already registered: ${actor.id}` };
    }

    this.actors.set(actor.id, actor);

    // If actor extends BaseActor, inject System reference
    if ('setSystem' in actor && typeof (actor as any).setSystem === 'function') {
      (actor as any).setSystem(this);
    }

    // Call actor's start() if it exists
    if (actor.start) {
      actor.start();
    }

    return { success: true, data: { actorId: actor.id } };
  }

  private unregisterActor(actorId: string): Response {
    const actor = this.actors.get(actorId);
    if (!actor) {
      return { success: false, error: `Actor not found: ${actorId}` };
    }

    // Call actor's stop() if it exists
    if (actor.stop) {
      actor.stop();
    }

    this.actors.delete(actorId);
    return { success: true, data: { actorId } };
  }

  private listActors(): Response {
    const actors = Array.from(this.actors.values()).map(actor => ({
      id: actor.id,
      type: actor.type
    }));
    return { success: true, data: { actors } };
  }

  // Convenience method for external code to send to actors
  // (wraps the route message pattern)
  async sendTo(targetId: string, message: Message): Promise<Response> {
    return this.receive({
      id: crypto.randomUUID(),
      type: 'route',
      payload: { targetId, message },
      sender: 'system'
    });
  }

  // Convenience method for registering actors
  register(actor: Actor): Response {
    return this.registerActor(actor);
  }

  // Convenience method for unregistering actors
  unregister(actorId: string): Response {
    return this.unregisterActor(actorId);
  }
}
