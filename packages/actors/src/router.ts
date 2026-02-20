/**
 * MessageRouter - Routes messages to actors
 *
 * Simplified from simplify/src/messaging/router.ts.
 * Decoupled from GraphStore/ProgramManager - uses Map-based actor registry.
 *
 * Enriched with brianln.ai patterns:
 * - Service discovery (registerService/requestService)
 * - Dead letter queue
 * - Transport registry for protocol-based routing
 */

import type {
  Message,
  MessageResponse,
  MessageHandler,
  Address,
} from './message.ts';
import type { IMessageRouter, ITransport } from './interfaces.ts';
import {
  address,
  parseAddress,
  createMessage,
  createResponse,
  createErrorResponse,
  generateCorrelationId,
} from './message.ts';
import { parseAddressInfo, isHierarchicalPath } from './routing/address-parser.ts';
import { PathCache, type PathCacheConfig } from './routing/path-cache.ts';
import type { DeadLetterEntry } from './types.ts';

/**
 * A virtual actor factory.
 *
 * Called when an actor address cannot be resolved through the normal registry.
 * If the factory returns a handler, the router registers it (auto-cached) and
 * delivers the pending message. Return null to pass to the next factory.
 */
export type ActorFactory = (
  address: string,
) => Promise<MessageHandler | null> | MessageHandler | null;

export interface ActorFactoryEntry {
  /** Optional address prefix. If omitted, factory is tried for every unresolved address. */
  prefix?: string;
  factory: ActorFactory;
}

export interface MessageRouterConfig {
  /** Path cache configuration */
  cache?: PathCacheConfig;
  /** Dead letter queue max size. @default 100 */
  deadLetterQueueSize?: number;
  /** Default ask timeout in ms. @default 30000 */
  defaultTimeout?: number;
  /** Virtual actor factories consulted when address resolution fails. */
  factories?: ActorFactoryEntry[];
}

export class MessageRouter implements IMessageRouter {
  private actorRegistry = new Map<string, MessageHandler>();
  private pathCache: PathCache<MessageHandler>;
  private transports = new Map<string, ITransport>();
  private pendingRequests = new Map<string, {
    resolve: (response: MessageResponse) => void;
    reject: (error: Error) => void;
    timeout: ReturnType<typeof setTimeout>;
  }>();

  // Service discovery (from brianln.ai)
  private serviceRegistry = new Map<string, string>();
  private pendingServiceBindings = new Map<string, Array<{
    requesterId: string;
    callback: (actorAddr: Address) => void;
  }>>();

  // Dead letter queue (from brianln.ai)
  private deadLetterQueue: DeadLetterEntry[] = [];
  private deadLetterQueueSize: number;
  private defaultTimeout: number;

  // Virtual actor factories
  private factories: ActorFactoryEntry[] = [];

  constructor(config: MessageRouterConfig = {}) {
    this.pathCache = new PathCache<MessageHandler>(config.cache);
    this.deadLetterQueueSize = config.deadLetterQueueSize ?? 100;
    this.defaultTimeout = config.defaultTimeout ?? 30000;
    if (config.factories) {
      this.factories = [...config.factories];
    }
  }

  // --- Actor Management ---

  registerActor(id: string, actor: MessageHandler): void {
    this.actorRegistry.set(id, actor);
    this.pathCache.set(id, actor);
  }

  unregisterActor(id: string): void {
    this.actorRegistry.delete(id);
    this.pathCache.invalidate(id);
  }

  getActor(path: string): MessageHandler | undefined {
    return this.actorRegistry.get(path);
  }

  listActors(): string[] {
    return Array.from(this.actorRegistry.keys());
  }

  /**
   * Register a virtual actor factory.
   * Called when an actor address cannot be resolved through the normal registry.
   * The factory may create and return a MessageHandler for the address, or return
   * null to pass control to the next registered factory.
   */
  registerFactory(entry: ActorFactoryEntry): void {
    this.factories.push(entry);
  }

  cacheActor(path: string, actor: MessageHandler): void {
    this.pathCache.set(path, actor);
  }

  invalidatePath(path: string): void {
    this.pathCache.invalidate(path);
  }

  // --- Transport Registry (from brianln.ai) ---

  registerTransport(protocol: string, transport: ITransport): void {
    this.transports.set(protocol, transport);
    transport.onReceive((sender, message) => {
      this.handleRemoteMessage(sender, message as Message);
    });
  }

  // --- Service Discovery (from brianln.ai) ---

  registerService(serviceName: string, actorId: string): void {
    this.serviceRegistry.set(serviceName, actorId);

    const pending = this.pendingServiceBindings.get(serviceName);
    if (pending && pending.length > 0) {
      for (const { callback } of pending) {
        callback(address(actorId));
      }
      this.pendingServiceBindings.delete(serviceName);
    }
  }

  requestService(
    serviceName: string,
    requesterId: string,
    callback: (actorAddr: Address) => void
  ): void {
    const actorId = this.serviceRegistry.get(serviceName);
    if (actorId) {
      callback(address(actorId));
    } else {
      if (!this.pendingServiceBindings.has(serviceName)) {
        this.pendingServiceBindings.set(serviceName, []);
      }
      this.pendingServiceBindings.get(serviceName)!.push({ requesterId, callback });
    }
  }

  unregisterService(serviceName: string): void {
    this.serviceRegistry.delete(serviceName);
  }

  // --- Dead Letter Queue ---

  getDeadLetterQueue(): DeadLetterEntry[] {
    return [...this.deadLetterQueue];
  }

  // --- Message Routing ---

  async tell(message: Message): Promise<void> {
    try {
      await this.route(message);
    } catch (error) {
      // Fire-and-forget: log but don't throw
      console.error(`Failed to deliver message ${message.id}:`, error instanceof Error ? error.message : error);
    }
  }

  async ask<T = unknown>(message: Message, timeoutMs?: number): Promise<MessageResponse<T>> {
    if (!message.correlationId) {
      throw new Error('ask() requires message with correlationId');
    }

    const timeout = timeoutMs ?? this.defaultTimeout;

    return new Promise<MessageResponse<T>>(async (resolve, reject) => {
      const timeoutId = setTimeout(() => {
        this.pendingRequests.delete(message.correlationId!);
        reject(new Error(`Ask timeout after ${timeout}ms`));
      }, timeout);

      this.pendingRequests.set(message.correlationId!, {
        resolve: (response) => {
          clearTimeout(timeoutId);
          resolve(response as MessageResponse<T>);
        },
        reject: (error) => {
          clearTimeout(timeoutId);
          reject(error);
        },
        timeout: timeoutId,
      });

      try {
        const response = await this.route(message);
        clearTimeout(timeoutId);
        this.pendingRequests.delete(message.correlationId!);
        resolve(response as MessageResponse<T>);
      } catch (error) {
        clearTimeout(timeoutId);
        this.pendingRequests.delete(message.correlationId!);
        reject(error);
      }
    });
  }

  // --- Metrics ---

  getRoutingStats() {
    return {
      actors: this.actorRegistry.size,
      services: this.serviceRegistry.size,
      transports: this.transports.size,
      deadLetters: this.deadLetterQueue.length,
      pendingRequests: this.pendingRequests.size,
      cacheMetrics: this.pathCache.getMetrics(),
    };
  }

  // --- Private ---

  private async route(message: Message): Promise<MessageResponse> {
    const targetPath = parseAddress(message.to);

    // 1. Check path cache
    const cached = this.pathCache.get(targetPath);
    if (cached) {
      return await cached.receive(message);
    }

    // 2. Direct actor registry lookup
    const actor = this.actorRegistry.get(targetPath);
    if (actor) {
      this.pathCache.set(targetPath, actor);
      return await actor.receive(message);
    }

    // 3. Hierarchical path routing - walk segments
    if (isHierarchicalPath(message.to)) {
      const info = parseAddressInfo(message.to);
      if (info.segments.length > 0) {
        // Try root segment as actor
        const rootActor = this.actorRegistry.get(info.segments[0]);
        if (rootActor) {
          return await rootActor.receive(message);
        }
      }
    }

    // 4. Transport-based routing (protocol://address)
    const protocolMatch = targetPath.match(/^(\w+):\/\//);
    if (protocolMatch) {
      const transport = this.transports.get(protocolMatch[1]);
      if (transport) {
        try {
          await transport.send(targetPath, message);
          return createResponse(message, { delivered: true });
        } catch (error) {
          this.addToDeadLetterQueue(message.to, message, error as Error);
          throw error;
        }
      }
    }

    // 5. Virtual actor factory — provision on demand
    if (this.factories.length > 0) {
      for (const entry of this.factories) {
        if (entry.prefix && !targetPath.startsWith(entry.prefix)) continue;
        const actor = await entry.factory(targetPath);
        if (actor) {
          // Cache at the full address so future sends bypass factory lookup
          this.actorRegistry.set(targetPath, actor);
          this.pathCache.set(targetPath, actor);
          return await actor.receive(message);
        }
      }
    }

    // 6. Dead letter
    const error = new Error(`No actor found for address: ${targetPath}`);
    this.addToDeadLetterQueue(message.to, message, error);
    return createErrorResponse(message, error.message);
  }

  private handleRemoteMessage(sender: string, message: Message): void {
    if (message.correlationId) {
      const pending = this.pendingRequests.get(message.correlationId);
      if (pending) {
        this.pendingRequests.delete(message.correlationId);
        pending.resolve(createResponse(message, message.payload));
        return;
      }
    }
  }

  private addToDeadLetterQueue(addr: Address, message: Message, error: Error): void {
    this.deadLetterQueue.push({
      address: addr,
      message,
      error,
      timestamp: Date.now(),
    });
    if (this.deadLetterQueue.length > this.deadLetterQueueSize) {
      this.deadLetterQueue.shift();
    }
  }
}
