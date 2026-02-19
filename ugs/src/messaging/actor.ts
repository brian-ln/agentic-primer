#!/usr/bin/env bun
/**
 * Actor System - Wraps programs as actors with uniform interface
 *
 * Provides actor abstraction on top of UGS program system.
 * All actors respond to messages via receive() method.
 */

import type GraphStore from '../graph.ts';
import type { ProgramManager } from '../entities/program.ts';
import { MessageRouter, type RouterStats, type BridgeRoute } from './router.ts';
import {
  ActorWithIntrospection,
  type Message,
  type MessageResponse,
  type Address,
  type MessageHandler,
  type StreamCallback,
  type TokenStreamEvent,
  type AsyncStreamMessage,
  type MessageAcceptance,
  type HandlerRegistration,
  type ActorMetadata,
  type IntrospectResponse,
  address,
  createMessage,
  generateCorrelationId,
  generateMessageId,
  validateJSONSchema,
  validateJSONSchemaErrors,
  findClosestMatch,
  createPortChannel,
} from '@agentic-primer/actors';
import type { Channel, ChannelOptions } from './channel.ts';

/**
 * Actor system statistics for monitoring
 */
export interface ActorSystemStats {
  /** Number of active actors in the system */
  actors: number;
  /** Router statistics */
  router: RouterStats;
}

/**
 * Actor - Uniform interface for all graph nodes
 * Re-exports @agentic-primer/actors ActorWithIntrospection.
 * All introspection, decorators, and schema validation now in the package.
 */
export class Actor extends ActorWithIntrospection {
  private ports = new Map<string, ReturnType<typeof createPortChannel>>();

  /**
   * Decorator-registered message handlers (set by @accepts decorator)
   */
  protected _acceptedMessages?: Map<string, HandlerRegistration>;

  constructor(id: string, router: MessageRouter) {
    super(id, router);
  }

  /**
   * Receive a message
   *
   * Handles introspection protocol messages first, then dispatches to
   * decorator-registered handlers or subclass implementation.
   */
  async receive(message: Message): Promise<MessageResponse> {
    // Handle introspection protocol (all actors MUST support)
    switch (message.type) {
      case 'ping':
        return this.handlePing(message);

      case 'accepts':
        return this.handleAccepts(message);

      case 'introspect':
        return this.handleIntrospect(message);

      case 'describe-actor':
        return this.handleDescribeActor(message);
    }

    // Check acceptance criteria (not just type!)
    if (!this.canAccept(message)) {
      return this.handleUnacceptable(message);
    }

    // Dispatch to registered handler or subclass
    return this.dispatch(message);
  }

  /**
   * Stream a request with real-time updates (optional, for streaming actors)
   */
  async stream?(payload: any, onChunk: StreamCallback<TokenStreamEvent>): Promise<void>;

  /**
   * Stream with AsyncIterator support (optional, for streaming actors)
   */
  streamAsync?<T = any>(payload: any): AsyncIterableIterator<AsyncStreamMessage<T>>;

  /**
   * Get a reactive port for pub/sub broadcasting (optional, for reactive actors)
   *
   * Ports enable 1:N broadcasting where one actor sends events to multiple subscribers.
   * Each subscriber gets independent buffering and backpressure handling.
   *
   * Subclasses should override this method to expose named ports.
   *
   * @param name - The port name (e.g., 'state-changes', 'errors', 'notifications')
   * @returns A Channel for subscribing to this port's events
   *
   * @example
   * ```typescript
   * class SessionActor extends Actor {
   *   port(name: 'messages' | 'users'): Channel<Event> {
   *     if (name === 'messages') return this.createPort('messages');
   *     if (name === 'users') return this.createPort('users');
   *     throw new Error(`Unknown port: ${name}`);
   *   }
   *
   *   async handleNewMessage(msg: Message) {
   *     // Broadcast to all subscribers
   *     await this.getPort('messages').send(msg);
   *   }
   * }
   *
   * // Consumer
   * using messages = sessionActor.port('messages');
   * for await (const msg of messages.subscribe()) {
   *   console.log('New message:', msg);
   * }
   * ```
   */
  port?<T = any>(name: string): Channel<T>;

  /**
   * Send a message without waiting for response (tell pattern)
   */
  async tell(to: Address, type: string, payload: any): Promise<void> {
    const message = createMessage(to, type, payload, {
      pattern: 'tell',
      from: this.address,
    });
    await this.router.tell(message);
  }

  /**
   * Send a message and wait for response (ask pattern)
   */
  async ask<T = any>(to: Address, type: string, payload: any): Promise<MessageResponse<T>> {
    const message = createMessage(to, type, payload, {
      pattern: 'ask',
      from: this.address,
      correlationId: generateCorrelationId(),
    });
    return await this.router.ask<T>(message);
  }

  /**
   * Send a debug-level log message (async, non-blocking)
   */
  protected logDebug(msg: string, context?: Record<string, any>): void {
    this.log('debug', msg, context);
  }

  /**
   * Send an info-level log message (async, non-blocking)
   */
  protected logInfo(msg: string, context?: Record<string, any>): void {
    this.log('info', msg, context);
  }

  /**
   * Send a warning-level log message (async, non-blocking)
   */
  protected logWarn(msg: string, context?: Record<string, any>): void {
    this.log('warn', msg, context);
  }

  /**
   * Send an error-level log message (async, non-blocking)
   */
  protected logError(msg: string, context?: Record<string, any>): void {
    this.log('error', msg, context);
  }

  /**
   * Internal: Send log message to logger actor
   */
  private log(level: string, msg: string, context?: Record<string, any>): void {
    const logMessage = createMessage(
      address('logger'),
      `log.${level}`,
      {
        message: msg,
        context: {
          ...context,
          actor: this.address,
        },
      },
      { pattern: 'tell', from: this.address }
    );

    // Fire-and-forget, silently ignore if logger not registered
    this.router.tell(logMessage).catch(() => {});
  }

  /**
   * Helper: Create an async stream from an array or generator
   */
  protected async *createAsyncStream<T>(
    items: T[] | AsyncIterable<T> | Iterable<T>,
    correlationId?: string
  ): AsyncIterableIterator<AsyncStreamMessage<T>> {
    const corrId = correlationId || generateCorrelationId();

    try {
      // Handle async iterables
      if (Symbol.asyncIterator in (items as any)) {
        for await (const item of items as AsyncIterable<T>) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: item,
            timestamp: Date.now(),
          };
        }
      }
      // Handle regular iterables
      else if (Symbol.iterator in (items as any)) {
        for (const item of items as Iterable<T>) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: item,
            timestamp: Date.now(),
          };
        }
      }
      // Handle arrays
      else if (Array.isArray(items)) {
        for (const item of items) {
          yield {
            id: generateMessageId(),
            correlationId: corrId,
            from: this.address,
            type: 'data',
            payload: item,
            timestamp: Date.now(),
          };
        }
      }

      // Emit end event
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'end',
        timestamp: Date.now(),
      };
    } catch (error: any) {
      // Emit error event
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'error',
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }

  /**
   * Helper: Create an async stream with backpressure detection
   * Yields items and monitors consumer speed
   */
  protected async *createAsyncStreamWithBackpressure<T>(
    source: AsyncIterable<T> | Iterable<T>,
    options: {
      correlationId?: string;
      onBackpressure?: (isPaused: boolean) => void;
      checkInterval?: number; // Check backpressure every N items
    } = {}
  ): AsyncIterableIterator<AsyncStreamMessage<T>> {
    const corrId = options.correlationId || generateCorrelationId();
    const checkInterval = options.checkInterval ?? 10;
    let itemCount = 0;
    let lastCheckTime = Date.now();

    try {
      const iterable = Symbol.asyncIterator in (source as any)
        ? (source as AsyncIterable<T>)
        : (async function* () { yield* source as Iterable<T>; })();

      for await (const item of iterable) {
        const beforeYield = Date.now();

        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: item,
          timestamp: Date.now(),
        };

        itemCount++;

        // Check for backpressure every N items
        if (itemCount % checkInterval === 0 && options.onBackpressure) {
          const afterYield = Date.now();
          const yieldTime = afterYield - beforeYield;
          const timeDiff = afterYield - lastCheckTime;

          // If average time per item > 1ms, signal backpressure
          const avgTimePerItem = timeDiff / checkInterval;
          const isPaused = avgTimePerItem > 1;

          options.onBackpressure(isPaused);
          lastCheckTime = afterYield;
        }
      }

      // Emit end event
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'end',
        timestamp: Date.now(),
      };
    } catch (error: any) {
      // Emit error event
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'error',
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }

  /**
   * Helper: Create or get a named port for pub/sub broadcasting.
   *
   * Creates a PortChannel on first access, reuses it on subsequent calls.
   * Ports are automatically managed by the actor.
   *
   * @param name - Unique port name
   * @param options - Channel options (bufferSize, etc.)
   * @returns PortChannel for this port
   *
   * @example
   * ```typescript
   * class MyActor extends Actor {
   *   private statusSignal = signal('ready');
   *
   *   port(name: string): Channel<any> {
   *     if (name === 'status') {
   *       return this.createPort('status');
   *     }
   *     throw new Error(`Unknown port: ${name}`);
   *   }
   *
   *   async updateStatus(newStatus: string) {
   *     this.statusSignal.set(newStatus);
   *     // Broadcast to all subscribers
   *     await this.getPort('status').send({ status: newStatus });
   *   }
   * }
   * ```
   */
  protected createPort<T = any>(name: string, options?: ChannelOptions): ReturnType<typeof createPortChannel<T>> {
    if (!this.ports.has(name)) {
      this.ports.set(name, createPortChannel<T>(options));
    }
    return this.ports.get(name) as ReturnType<typeof createPortChannel<T>>;
  }

  /**
   * Helper: Get an existing port by name.
   *
   * Throws if port doesn't exist. Use createPort() to create ports.
   *
   * @param name - Port name
   * @returns The existing PortChannel
   */
  protected getPort<T = any>(name: string): ReturnType<typeof createPortChannel<T>> {
    const port = this.ports.get(name);
    if (!port) {
      throw new Error(`Port not found: ${name}. Use createPort() to create it first.`);
    }
    return port as ReturnType<typeof createPortChannel<T>>;
  }

  /**
   * Helper: Close all ports and clean up resources.
   *
   * Call this in actor shutdown logic to ensure clean cleanup.
   */
  protected closePorts(): void {
    for (const port of this.ports.values()) {
      port.close();
    }
    this.ports.clear();
  }

  /**
   * Schedule a delayed message to self
   *
   * Uses the SchedulerActor to schedule a message to be delivered
   * to this actor after the specified delay.
   *
   * @param delay - Delay in milliseconds
   * @param type - Message type
   * @param payload - Optional message payload
   * @returns Schedule ID that can be used to cancel
   *
   * @example
   * ```typescript
   * class TaskActor extends Actor {
   *   async receive(message: Message): Promise<MessageResponse> {
   *     if (message.type === 'start') {
   *       await this.schedule(5000, 'timeout');
   *       return createResponse(message, { status: 'started' });
   *     }
   *   }
   * }
   * ```
   */
  protected async schedule(delay: number, type: string, payload?: any): Promise<string> {
    const response = await this.ask<{ scheduleId: string }>(
      address('/system/scheduler'),
      'scheduler.schedule',
      {
        delay,
        message: {
          to: this.address,
          type,
          payload,
        },
      }
    );

    if (!response.success || !response.payload) {
      throw new Error(`Failed to schedule message: ${response.error || 'Unknown error'}`);
    }

    return response.payload.scheduleId;
  }

  /**
   * Schedule a recurring message to self
   *
   * Uses the SchedulerActor to schedule a message to be delivered
   * to this actor at regular intervals.
   *
   * @param interval - Interval in milliseconds
   * @param type - Message type
   * @param payload - Optional message payload
   * @returns Schedule ID that can be used to cancel
   *
   * @example
   * ```typescript
   * class HealthCheckActor extends Actor {
   *   async receive(message: Message): Promise<MessageResponse> {
   *     if (message.type === 'start') {
   *       await this.scheduleRecurring(60000, 'ping');
   *       return createResponse(message, { status: 'monitoring' });
   *     }
   *   }
   * }
   * ```
   */
  protected async scheduleRecurring(interval: number, type: string, payload?: any): Promise<string> {
    const response = await this.ask<{ scheduleId: string }>(
      address('/system/scheduler'),
      'scheduler.recurring',
      {
        interval,
        message: {
          to: this.address,
          type,
          payload,
        },
      }
    );

    if (!response.success || !response.payload) {
      throw new Error(`Failed to schedule recurring message: ${response.error || 'Unknown error'}`);
    }

    return response.payload.scheduleId;
  }

  /**
   * Cancel a scheduled or recurring message
   *
   * @param scheduleId - Schedule ID returned by schedule() or scheduleRecurring()
   *
   * @example
   * ```typescript
   * const scheduleId = await this.schedule(5000, 'timeout');
   * // ... later
   * await this.cancelSchedule(scheduleId);
   * ```
   */
  protected async cancelSchedule(scheduleId: string): Promise<void> {
    const response = await this.ask(
      address('/system/scheduler'),
      'scheduler.cancel',
      { scheduleId }
    );

    if (!response.success) {
      throw new Error(`Failed to cancel schedule: ${response.error || 'Unknown error'}`);
    }
  }

  // ===== Introspection Protocol Implementation =====

  /**
   * Handle ping message - health check
   */
  private handlePing(message: Message): MessageResponse {
    const start = Date.now();
    return {
      id: message.id,
      success: true,
      payload: {
        status: 'ok',
        latency: Date.now() - start,
      },
      timestamp: Date.now(),
    };
  }

  /**
   * Handle accepts message - what messages this actor accepts
   */
  private handleAccepts(message: Message): MessageResponse {
    const { messageType } = message.payload || {};

    // Get acceptance metadata from decorators + manual overrides
    const accepted = this.getAcceptedMessages();

    if (messageType) {
      // Describe specific message
      const acceptance = accepted.find(a => a.type === messageType);

      if (!acceptance) {
        const acceptedTypes = accepted.map(a => a.type);
        const suggestion = findClosestMatch(messageType, acceptedTypes);

        return {
          id: message.id,
          success: false,
          error: `Does not accept message type: ${messageType}`,
          details: {
            acceptedTypes,
            suggestion: suggestion
              ? `Did you mean '${suggestion}'?`
              : 'No similar message types found',
          },
          timestamp: Date.now(),
        };
      }

      // Return single acceptance
      return {
        id: message.id,
        success: true,
        payload: acceptance,
        timestamp: Date.now(),
      };
    }

    // List all accepted messages
    return {
      id: message.id,
      success: true,
      payload: accepted,
      timestamp: Date.now(),
    };
  }

  /**
   * Handle introspect message - full actor + accepts info
   */
  private handleIntrospect(message: Message): MessageResponse {
    const includeExamples = message.payload?.includeExamples ?? false;

    const accepted = this.getAcceptedMessages();

    // Remove examples if not requested
    const acceptedFiltered = includeExamples
      ? accepted
      : accepted.map(a => {
          const { examples, ...rest } = a;
          return rest;
        });

    const response: IntrospectResponse = {
      actor: this.describeActor(),
      accepts: acceptedFiltered,
    };

    return {
      id: message.id,
      success: true,
      payload: response,
      timestamp: Date.now(),
    };
  }

  /**
   * Handle describe-actor message - actor metadata
   */
  private handleDescribeActor(message: Message): MessageResponse {
    return {
      id: message.id,
      success: true,
      payload: this.describeActor(),
      timestamp: Date.now(),
    };
  }

  /**
   * Get actor metadata
   *
   * Subclasses can override to provide custom metadata.
   */
  protected describeActor(): ActorMetadata {
    return {
      address: this.address.id,
      type: this.constructor.name,
      version: '1.0.0',
      description: 'Generic actor',
      state: 'active',
    };
  }

  /**
   * Get accepted messages (from decorators + manual overrides)
   */
  private getAcceptedMessages(): MessageAcceptance[] {
    // Get decorator metadata
    const decoratorAccepted = this._acceptedMessages
      ? Array.from(this._acceptedMessages.values()).map(h => h.metadata)
      : [];

    // Get manual overrides from subclass
    const manualAccepted = this.getManualAcceptedMessages();

    // Combine (manual overrides take precedence)
    const combined = [...decoratorAccepted, ...manualAccepted];

    // Add introspection protocol messages
    const introspection: MessageAcceptance[] = [
      {
        type: 'ping',
        description: 'Health check - responds with OK if actor is responsive',
        consequences: {
          category: 'read-only',
          sideEffects: [],
          canUndo: true,
          requiresConfirm: false,
        },
      },
      {
        type: 'accepts',
        description: 'List messages this actor accepts, optionally filtered by type',
        expectedPayload: {
          type: 'object',
          properties: {
            messageType: { type: 'string' },
          },
        },
        consequences: {
          category: 'read-only',
          sideEffects: [],
          canUndo: true,
          requiresConfirm: false,
        },
      },
      {
        type: 'introspect',
        description: 'Get full actor metadata and accepted messages',
        expectedPayload: {
          type: 'object',
          properties: {
            includeExamples: { type: 'boolean' },
          },
        },
        consequences: {
          category: 'read-only',
          sideEffects: [],
          canUndo: true,
          requiresConfirm: false,
        },
      },
      {
        type: 'describe-actor',
        description: 'Get actor metadata (address, type, version, etc.)',
        consequences: {
          category: 'read-only',
          sideEffects: [],
          canUndo: true,
          requiresConfirm: false,
        },
      },
    ];

    return [...introspection, ...combined];
  }

  /**
   * Override in subclass to provide manual message acceptances
   * (when not using decorators)
   */
  protected getManualAcceptedMessages(): MessageAcceptance[] {
    return [];
  }

  /**
   * Check if actor can accept a message (not just type!)
   *
   * Checks acceptance criteria: payload shape, sender pattern, state condition
   */
  private canAccept(message: Message): boolean {
    if (!this._acceptedMessages) {
      // No registered handlers, allow subclass to handle
      return true;
    }

    const handler = this._acceptedMessages.get(message.type);
    if (!handler) {
      // Unknown message type
      return false;
    }

    // Check richer acceptance criteria
    const { criteria } = handler.metadata;
    if (!criteria) {
      return true;  // No additional criteria, just type match
    }

    // Check payload shape
    if (criteria.payloadShape) {
      if (!validateJSONSchema(message.payload, criteria.payloadShape)) {
        return false;
      }
    }

    // Check sender pattern
    if (criteria.fromPattern) {
      const pattern = new RegExp(criteria.fromPattern);
      if (!pattern.test(message.from.id)) {
        return false;
      }
    }

    // Check state condition
    if (criteria.stateCondition) {
      try {
        const result = this.evaluateStateCondition(criteria.stateCondition);
        if (!result) {
          return false;
        }
      } catch (error) {
        return false;
      }
    }

    return true;
  }

  /**
   * Evaluate a state condition expression
   *
   * IMPORTANT: In production, this should use a safe evaluator like JSONata
   * or a sandboxed expression evaluator. eval() is unsafe and used here
   * only for prototyping.
   */
  private evaluateStateCondition(condition: string): boolean {
    try {
      // FIXME: Replace with safe evaluator (JSONata, etc.)
      // eslint-disable-next-line no-eval
      return eval(condition);
    } catch {
      return false;
    }
  }

  /**
   * Handle message that doesn't meet acceptance criteria
   */
  private handleUnacceptable(message: Message): MessageResponse {
    const accepted = this.getAcceptedMessages();
    const acceptance = accepted.find(a => a.type === message.type);

    if (!acceptance) {
      // Unknown message type
      const acceptedTypes = accepted.map(a => a.type);
      const suggestion = findClosestMatch(message.type, acceptedTypes);

      return {
        id: message.id,
        success: false,
        error: `Does not accept message type: ${message.type}`,
        details: {
          acceptedTypes,
          suggestion: suggestion
            ? `Did you mean '${suggestion}'?`
            : 'Query accepted messages with: { type: "accepts", payload: {} }',
        },
        timestamp: Date.now(),
      };
    }

    // Known message type, but criteria not met
    const { criteria } = acceptance;
    const reasons: string[] = [];

    if (criteria?.payloadShape) {
      const errors = validateJSONSchemaErrors(message.payload, criteria.payloadShape);
      if (errors.length > 0) {
        reasons.push(`Payload validation failed: ${errors.map(e => `${e.path}: ${e.message}`).join(', ')}`);
      }
    }

    if (criteria?.fromPattern) {
      reasons.push(`Sender must match pattern: ${criteria.fromPattern} (got: ${message.from.id})`);
    }

    if (criteria?.stateCondition) {
      reasons.push(`State condition not met: ${criteria.stateCondition}`);
    }

    return {
      id: message.id,
      success: false,
      error: 'Message does not meet acceptance criteria',
      details: {
        messageType: message.type,
        reasons,
        criteria,
        suggestion: 'Query message requirements with: { type: "accepts", payload: { messageType: "' + message.type + '" } }',
      },
      timestamp: Date.now(),
    };
  }

  /**
   * Dispatch message to registered handler or subclass
   */
  protected dispatch(message: Message): Promise<MessageResponse> {
    if (!this._acceptedMessages) {
      // No decorators, delegate to subclass
      return this.handleUnknownMessage(message);
    }

    const handler = this._acceptedMessages.get(message.type);
    if (!handler) {
      return this.handleUnknownMessage(message);
    }

    // Call handler method
    return handler.handler.call(this, message);
  }

  /**
   * Handle unknown message type
   *
   * Subclasses can override to provide custom handling.
   */
  protected handleUnknownMessage(message: Message): Promise<MessageResponse> {
    const accepted = this.getAcceptedMessages();
    const acceptedTypes = accepted.map(a => a.type);
    const suggestion = findClosestMatch(message.type, acceptedTypes);

    return Promise.resolve({
      id: message.id,
      success: false,
      error: `Unknown message type: ${message.type}`,
      details: {
        acceptedTypes,
        suggestion: suggestion
          ? `Did you mean '${suggestion}'?`
          : 'Query accepted messages with: { type: "accepts", payload: {} }',
      },
      timestamp: Date.now(),
    });
  }
}

/**
 * ProgramActor - Wraps a UGS program as an actor
 */
export class ProgramActor extends Actor {
  private programId: string;

  constructor(programId: string, router: MessageRouter) {
    super(programId, router);
    this.programId = programId;
  }

  /**
   * Receive message - delegates to program invocation
   */
  async receive(message: Message): Promise<MessageResponse> {
    // Directly invoke the program to avoid infinite recursion
    return await this.router.invokeProgram(this.programId, message);
  }
}

/**
 * DocumentActor - Wraps a graph node as read-only document
 */
export class DocumentActor extends Actor {
  private documentId: string;

  constructor(documentId: string, router: MessageRouter) {
    super(documentId, router);
    this.documentId = documentId;
  }

  /**
   * Receive message - returns document data
   */
  async receive(message: Message): Promise<MessageResponse> {
    // Directly query the document to avoid infinite recursion
    return await this.router.queryDocument(this.documentId, message);
  }
}

/**
 * ActorSystem - Creates and manages actors
 */
export class ActorSystem {
  private store: GraphStore;
  private programManager: ProgramManager;
  private router: MessageRouter;
  private actors: Map<string, Actor> = new Map();

  constructor(store: GraphStore, programManager: ProgramManager) {
    this.store = store;
    this.programManager = programManager;
    this.router = new MessageRouter(store, programManager);
  }

  /**
   * Get or create an actor for a node
   */
  actor(id: string): Actor {
    if (this.actors.has(id)) {
      return this.actors.get(id)!;
    }

    const node = this.store.get(id);
    if (!node) {
      throw new Error(`Node not found: ${id}`);
    }

    let actor: Actor;

    if (node.type === 'program') {
      actor = new ProgramActor(id, this.router);
    } else {
      actor = new DocumentActor(id, this.router);
    }

    this.actors.set(id, actor);
    return actor;
  }

  /**
   * Send a message to an actor by address
   */
  async send(to: Address, type: string, payload: any): Promise<MessageResponse> {
    const message = createMessage(to, type, payload, {
      pattern: 'ask',
      correlationId: generateCorrelationId(),
      from: address('system/actor-system'),
    });
    return await this.router.ask(message);
  }

  /**
   * Tell (fire-and-forget) a message to an actor
   */
  async tell(to: Address, type: string, payload: any): Promise<void> {
    const message = createMessage(to, type, payload, {
      pattern: 'tell',
      from: address('system/actor-system'),
    });
    await this.router.tell(message);
  }

  /**
   * Register a bridge route for cross-runtime message delivery.
   *
   * Messages addressed to targets starting with `bridge.prefix` are serialized
   * with `bridge.serde` and dispatched via `bridge.transport`.
   *
   * Example — wire Signal Hub transport:
   * ```typescript
   * system.registerBridge({
   *   prefix: 'hub://',
   *   transport: new SignalHubTransport(client),
   *   serde: new JsonSerde(),
   * });
   * // Any message to address('hub://my-actor') now routes through Signal Hub
   * ```
   */
  registerBridge(bridge: BridgeRoute): void {
    this.router.registerBridge(bridge);
  }

  /**
   * Get system statistics
   *
   * @returns System metrics for monitoring
   */
  getStats(): ActorSystemStats {
    return {
      actors: this.actors.size,
      router: this.router.getStats(),
    };
  }
}

export default ActorSystem;
