/**
 * Actor with Introspection Support
 *
 * Extends base Actor with:
 * - @accepts decorator support
 * - Introspection protocol (ping, accepts, introspect, describe-actor)
 * - JSON Schema validation
 * - Automatic message dispatch
 */

import type {
  Message,
  MessageResponse,
  Address,
} from './message.ts';
import {
  createResponse,
  createErrorResponse,
  parseAddress,
} from './message.ts';
import type {
  MessageAcceptance,
  HandlerRegistration,
  ActorMetadata,
  IntrospectResponse,
} from './introspection.ts';
import {
  validateJSONSchema,
  validateJSONSchemaErrors,
  findClosestMatch,
} from './validation/schema-validator.ts';
import { Actor as BaseActor } from './actor.ts';
import type { IMessageRouter } from './interfaces.ts';

/**
 * Actor with introspection - decorator-based message acceptance
 */
export class ActorWithIntrospection extends BaseActor {
  /**
   * Decorator-registered message handlers (set by @accepts decorator)
   */
  protected _acceptedMessages?: Map<string, HandlerRegistration>;

  constructor(id: string, router: IMessageRouter) {
    super(id, router);
  }

  /**
   * Receive a message with introspection protocol support
   */
  async receive(message: Message): Promise<MessageResponse> {
    // Handle introspection protocol (all actors support)
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

    // Check acceptance criteria
    if (!this.canAccept(message)) {
      return this.handleUnacceptable(message);
    }

    // Dispatch to registered handler or subclass
    return this.dispatch(message);
  }

  /**
   * Handle ping - health check
   */
  private handlePing(message: Message): MessageResponse {
    const start = Date.now();
    return createResponse(message, {
      status: 'ok',
      latency: Date.now() - start,
    });
  }

  /**
   * Handle accepts - query accepted messages
   */
  private handleAccepts(message: Message): MessageResponse {
    const { messageType } = message.payload || {};
    const accepted = this.getAcceptedMessages();

    if (messageType) {
      const acceptance = accepted.find((a) => a.type === messageType);
      if (!acceptance) {
        const types = accepted.map((a) => a.type);
        const suggestion = findClosestMatch(messageType, types);
        return createErrorResponse(
          message,
          `Does not accept message type: ${messageType}. ${
            suggestion ? `Did you mean '${suggestion}'?` : ''
          }`
        );
      }
      return createResponse(message, acceptance);
    }

    return createResponse(message, accepted);
  }

  /**
   * Handle introspect - full actor info
   */
  private handleIntrospect(message: Message): MessageResponse {
    const includeExamples = message.payload?.includeExamples ?? false;
    const accepted = this.getAcceptedMessages();

    const acceptedFiltered = includeExamples
      ? accepted
      : accepted.map((a) => {
          const { examples, ...rest } = a;
          return rest;
        });

    const response: IntrospectResponse = {
      actor: this.describeActor(),
      accepts: acceptedFiltered,
    };

    return createResponse(message, response);
  }

  /**
   * Handle describe-actor - actor metadata
   */
  private handleDescribeActor(message: Message): MessageResponse {
    return createResponse(message, this.describeActor());
  }

  /**
   * Check if message is acceptable (type + schema validation)
   */
  protected canAccept(message: Message): boolean {
    const accepted = this._acceptedMessages;
    if (!accepted) return true; // No decorators = accept all

    const handler = accepted.get(message.type);
    if (!handler) return false; // Unknown type

    // Check schema if provided
    const schema = handler.metadata.expectedPayload;
    if (schema && message.payload) {
      return validateJSONSchema(message.payload, schema);
    }

    return true;
  }

  /**
   * Handle unacceptable message
   */
  protected handleUnacceptable(message: Message): MessageResponse {
    const accepted = this._acceptedMessages;
    if (!accepted) {
      return createErrorResponse(message, 'Unknown message type');
    }

    const handler = accepted.get(message.type);
    if (!handler) {
      const types = Array.from(accepted.keys());
      const suggestion = findClosestMatch(message.type, types);
      return createErrorResponse(
        message,
        `Unknown message type: ${message.type}. ${
          suggestion ? `Did you mean '${suggestion}'?` : ''
        }`
      );
    }

    // Schema validation failed
    const schema = handler.metadata.expectedPayload;
    if (schema) {
      const errors = validateJSONSchemaErrors(message.payload, schema);
      return createErrorResponse(
        message,
        `Invalid payload: ${errors.map((e) => e.message).join(', ')}`
      );
    }

    return createErrorResponse(message, 'Message not acceptable');
  }

  /**
   * Dispatch to registered handler
   */
  protected dispatch(message: Message): Promise<MessageResponse> {
    if (!this._acceptedMessages) {
      return this.handleUnknownMessage(message);
    }

    const handler = this._acceptedMessages.get(message.type);
    if (!handler) {
      return this.handleUnknownMessage(message);
    }

    return handler.handler.call(this, message);
  }

  /**
   * Handle unknown message (default error response)
   */
  protected handleUnknownMessage(message: Message): Promise<MessageResponse> {
    const accepted = this.getAcceptedMessages();
    const types = accepted.map((a) => a.type);
    const suggestion = findClosestMatch(message.type, types);

    return Promise.resolve(
      createErrorResponse(
        message,
        `Unknown message type: ${message.type}. ${
          suggestion ? `Did you mean '${suggestion}'?` : ''
        }`
      )
    );
  }

  /**
   * Get accepted messages metadata
   */
  protected getAcceptedMessages(): MessageAcceptance[] {
    if (!this._acceptedMessages) return [];
    return Array.from(this._acceptedMessages.values()).map((h) => h.metadata);
  }

  /**
   * Get actor metadata (override in subclasses)
   */
  protected describeActor(): ActorMetadata {
    return {
      address: parseAddress(this.address),
      type: this.constructor.name,
      version: '1.0.0',
      description: 'Actor with introspection support',
      state: 'active',
    };
  }
}
