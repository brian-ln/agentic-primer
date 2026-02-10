/**
 * ActorSystem - Lifecycle management layer for actors
 *
 * Supports two complementary patterns:
 * 1. Functional behaviors: spawn(behavior, initialState) — from brianln.ai
 * 2. Class-based actors: register(actor) — from simplify
 *
 * Both use simplify's Message types as the wire format.
 * Enriched with brianln.ai patterns: service discovery, DLQ, transport registry.
 */

import type {
  Message,
  MessageResponse,
  MessageHandler,
  Address,
} from './message.ts';
import type {
  ActorBehavior,
  ActorContext,
  ActorInstance,
  ActorSystemConfig,
  SupervisionStrategy,
  SupervisionDirective,
  DeadLetterEntry,
} from './types.ts';
import type { ITransport } from './interfaces.ts';
import { MessageRouter, type MessageRouterConfig } from './router.ts';
import {
  address,
  parseAddress,
  createMessage,
  createResponse,
  createErrorResponse,
  generateMessageId,
  generateCorrelationId,
} from './message.ts';

const DEFAULT_SUPERVISION: SupervisionStrategy = {
  onFailure: () => 'Restart' as SupervisionDirective,
};

/**
 * ActorSystem manages actor lifecycle, routing, and cross-system communication.
 *
 * Usage with functional behaviors:
 * ```typescript
 * const system = new ActorSystem({ name: 'my-system' });
 * const counterAddr = system.spawn((state, msg, ctx) => {
 *   if (msg.type === 'increment') return state + 1;
 *   return state;
 * }, 0, 'counter');
 * system.send(counterAddr, 'increment', {});
 * ```
 *
 * Usage with class-based actors:
 * ```typescript
 * const myActor = new MyActor('my-actor', system.messageRouter);
 * system.register(myActor);
 * system.send(myActor.address, 'hello', { text: 'world' });
 * ```
 */
export class ActorSystem {
  private readonly config: Required<ActorSystemConfig>;
  private readonly router: MessageRouter;
  private readonly behaviors = new Map<string, ActorInstance>();

  constructor(config: ActorSystemConfig, routerConfig?: MessageRouterConfig) {
    this.config = {
      ...config,
      supervisionStrategy: config.supervisionStrategy || DEFAULT_SUPERVISION,
      deadLetterQueueSize: config.deadLetterQueueSize || 100,
    };
    this.router = new MessageRouter({
      ...routerConfig,
      deadLetterQueueSize: this.config.deadLetterQueueSize,
    });
  }

  /** Access the underlying message router for class-based actors. */
  get messageRouter(): MessageRouter {
    return this.router;
  }

  /** System name. */
  get name(): string {
    return this.config.name;
  }

  // --- Functional Behavior Pattern (from brianln.ai) ---

  /**
   * Spawn a new actor with functional behavior.
   * Returns the actor's address for messaging.
   */
  spawn<State, Protocol>(
    behavior: ActorBehavior<State, Protocol>,
    initialState: State,
    name?: string
  ): Address {
    const actorId = name || generateMessageId();
    const addr = address(actorId);

    const instance: ActorInstance<State, Protocol> = {
      address: addr,
      state: initialState,
      behavior,
      mailbox: [],
      processing: false,
    };

    this.behaviors.set(actorId, instance as ActorInstance);

    // Register a MessageHandler adapter so the router can deliver messages
    const handler: MessageHandler = {
      receive: async (message: Message): Promise<MessageResponse> => {
        return this.deliverToBehavior(actorId, message);
      },
    };
    this.router.registerActor(actorId, handler);

    return addr;
  }

  // --- Class-Based Actor Pattern (from simplify) ---

  /** Register a class-based actor. */
  register(actor: MessageHandler & { address: Address }): void {
    const id = parseAddress(actor.address);
    this.router.registerActor(id, actor);
  }

  // --- Actor Lifecycle ---

  /** Stop an actor (functional or class-based). */
  stop(addr: Address): void {
    const id = parseAddress(addr);
    this.behaviors.delete(id);
    this.router.unregisterActor(id);
  }

  // --- Messaging ---

  /** Send a message (fire-and-forget). */
  send(to: Address, type: string, payload: unknown): void {
    const message = createMessage(to, type, payload, {
      pattern: 'tell',
      from: address(`system/${this.config.name}`),
    });
    this.router.tell(message);
  }

  /** Send a message and wait for response. */
  async ask<R = unknown>(
    to: Address,
    type: string,
    payload: unknown,
    timeout: number = 5000
  ): Promise<R> {
    const message = createMessage(to, type, payload, {
      pattern: 'ask',
      from: address(`system/${this.config.name}`),
      correlationId: generateCorrelationId(),
    });

    const response = await this.router.ask<R>(message, timeout);
    if (!response.success) {
      throw new Error(response.error || 'Ask failed');
    }
    return response.payload as R;
  }

  // --- Transport & Service Discovery (delegated to router) ---

  registerTransport(protocol: string, transport: ITransport): void {
    this.router.registerTransport(protocol, transport);
  }

  registerService(serviceName: string, addr: Address): void {
    this.router.registerService(serviceName, parseAddress(addr));
  }

  requestService(
    serviceName: string,
    requesterId: string,
    callback: (actorAddr: Address) => void
  ): void {
    this.router.requestService(serviceName, requesterId, callback);
  }

  unregisterService(serviceName: string): void {
    this.router.unregisterService(serviceName);
  }

  // --- Introspection ---

  getDeadLetterQueue(): DeadLetterEntry[] {
    return this.router.getDeadLetterQueue();
  }

  getStats() {
    return {
      name: this.config.name,
      behaviors: this.behaviors.size,
      routing: this.router.getRoutingStats(),
    };
  }

  // --- Private: Functional behavior message delivery ---

  private async deliverToBehavior(actorId: string, message: Message): Promise<MessageResponse> {
    const instance = this.behaviors.get(actorId);
    if (!instance) {
      return createErrorResponse(message, `Actor not found: ${actorId}`);
    }

    instance.mailbox.push(message);

    if (!instance.processing) {
      await this.processMailbox(instance);
    }

    return createResponse(message, { delivered: true });
  }

  private async processMailbox(instance: ActorInstance): Promise<void> {
    instance.processing = true;

    while (instance.mailbox.length > 0) {
      const message = instance.mailbox.shift()!;

      try {
        const context: ActorContext = {
          self: instance.address,
          send: (to, type, payload) => this.send(to, type, payload),
          ask: (to, type, payload, timeout) => this.ask(to, type, payload, timeout),
          spawn: (behavior, initialState, name) => this.spawn(behavior, initialState, name),
          stop: (addr) => this.stop(addr),
        };

        const newState = await instance.behavior(instance.state, message, context);
        instance.state = newState;
      } catch (error) {
        await this.handleActorFailure(instance, error as Error, message);
      }
    }

    instance.processing = false;
  }

  private async handleActorFailure(
    instance: ActorInstance,
    error: Error,
    message: Message
  ): Promise<void> {
    const directive = this.config.supervisionStrategy.onFailure(
      instance.address,
      error,
      message
    );

    switch (directive) {
      case 'Resume':
        break;
      case 'Restart':
        instance.mailbox = [];
        break;
      case 'Stop':
        this.stop(instance.address);
        break;
      case 'Escalate':
        if (instance.supervisor) {
          this.send(instance.supervisor, 'ACTOR_FAILURE', {
            actor: instance.address,
            error: error.message,
            message,
          });
        }
        break;
    }
  }
}
