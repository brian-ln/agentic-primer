/**
 * Actor - Base class for message-processing actors
 *
 * Provides uniform interface for all actors with tell/ask patterns.
 * Ported from simplify/src/messaging/actor.ts, decoupled from GraphStore.
 *
 * Uses IMessageRouter interface instead of concrete MessageRouter.
 *
 * Auto-validation: If a schema is registered for a message type via
 * registerMessageSchema(), the base Actor will automatically validate
 * payloads before calling receive(). Subclasses can override this
 * behavior by setting enableAutoValidation = false.
 */

import type {
  Message,
  MessageResponse,
  MessageHandler,
  Address,
  AsyncStreamMessage,
  StreamCallback,
  TokenStreamEvent,
} from './message.ts';
import type { IMessageRouter } from './interfaces.ts';
import {
  address,
  createMessage,
  createErrorResponse,
  generateCorrelationId,
  generateMessageId,
} from './message.ts';
import type { JSONSchema } from './introspection.ts';
import { validateJSONSchemaErrors } from './schema-validator.ts';

/**
 * Global message type → schema registry for auto-validation
 * Populated by generated validator registration code at startup
 */
const globalMessageSchemaRegistry = new Map<string, JSONSchema>();

/**
 * Register a message schema for auto-validation
 *
 * @param messageType - Message type (e.g., "create", "query")
 * @param schema - JSON Schema for payload validation
 */
export function registerMessageSchema(messageType: string, schema: JSONSchema): void {
  globalMessageSchemaRegistry.set(messageType, schema);
}

/**
 * Check if a message type has a registered schema
 */
export function hasMessageSchema(messageType: string): boolean {
  return globalMessageSchemaRegistry.has(messageType);
}

/**
 * Get registered schema for a message type (if any)
 */
export function getMessageSchema(messageType: string): JSONSchema | undefined {
  return globalMessageSchemaRegistry.get(messageType);
}

export class Actor implements MessageHandler {
  readonly address: Address;
  protected router: IMessageRouter;

  /**
   * Enable/disable auto-validation for this actor
   * Set to false in constructor to disable schema validation
   */
  protected enableAutoValidation = true;

  constructor(id: string, router: IMessageRouter) {
    this.address = address(id);
    this.router = router;
  }

  /**
   * Receive a message with optional auto-validation
   *
   * If enableAutoValidation is true and a schema is registered for the
   * message type, the payload will be validated before handleMessage() is called.
   *
   * Subclasses should override handleMessage(), not this method.
   */
  async receive(message: Message): Promise<MessageResponse> {
    // Auto-validate if enabled and schema exists
    if (this.enableAutoValidation) {
      const schema = getMessageSchema(message.type);
      if (schema && message.payload) {
        const errors = validateJSONSchemaErrors(message.payload, schema);
        if (errors.length > 0) {
          return createErrorResponse(
            message,
            `Invalid payload: ${errors.map(e => `${e.path}: ${e.message}`).join('; ')}`
          );
        }
      }
    }

    // Call the actual handler
    return this.handleMessage(message);
  }

  /**
   * Handle a validated message. Override in subclasses.
   *
   * This method is called after auto-validation (if enabled).
   * Subclasses should implement their message handling logic here.
   */
  protected async handleMessage(message: Message): Promise<MessageResponse> {
    throw new Error('handleMessage() must be implemented by subclass');
  }

  /** Optional streaming support. */
  async stream?(payload: unknown, onChunk: StreamCallback<TokenStreamEvent>): Promise<void>;

  /** Optional AsyncIterator streaming. */
  streamAsync?<T = unknown>(payload: unknown): AsyncIterableIterator<AsyncStreamMessage<T>>;

  /** Send a message without waiting (tell pattern). */
  async tell(to: Address, type: string, payload: unknown): Promise<void> {
    const message = createMessage(to, type, payload, {
      pattern: 'tell',
      from: this.address,
    });
    await this.router.tell(message);
  }

  /** Send a message and wait for response (ask pattern). */
  async ask<T = unknown>(to: Address, type: string, payload: unknown): Promise<MessageResponse<T>> {
    const message = createMessage(to, type, payload, {
      pattern: 'ask',
      from: this.address,
      correlationId: generateCorrelationId(),
    });
    return await this.router.ask<T>(message);
  }

  /** Send a log message to the logger actor (fire-and-forget). */
  protected log(level: string, msg: string, context?: Record<string, unknown>): void {
    const logMessage = createMessage(
      address('logger'),
      `log.${level}`,
      { message: msg, context: { ...context, actor: this.address } },
      { pattern: 'tell', from: this.address }
    );
    this.router.tell(logMessage).catch(() => {});
  }

  protected logDebug(msg: string, context?: Record<string, unknown>): void { this.log('debug', msg, context); }
  protected logInfo(msg: string, context?: Record<string, unknown>): void { this.log('info', msg, context); }
  protected logWarn(msg: string, context?: Record<string, unknown>): void { this.log('warn', msg, context); }
  protected logError(msg: string, context?: Record<string, unknown>): void { this.log('error', msg, context); }

  /** Create an async stream from items. */
  protected async *createAsyncStream<T>(
    items: T[] | AsyncIterable<T> | Iterable<T>,
    correlationId?: string
  ): AsyncIterableIterator<AsyncStreamMessage<T>> {
    const corrId = correlationId || generateCorrelationId();

    try {
      if (Symbol.asyncIterator in (items as object)) {
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
      } else if (Symbol.iterator in (items as object)) {
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

      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'end',
        timestamp: Date.now(),
      };
    } catch (error: unknown) {
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'error',
        error: error instanceof Error ? error.message : String(error),
        timestamp: Date.now(),
      };
    }
  }
}
