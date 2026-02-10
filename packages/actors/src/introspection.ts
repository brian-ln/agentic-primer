#!/usr/bin/env bun
/**
 * Actor Introspection Protocol v2
 *
 * Defines the standard introspection interface all actors must support.
 * Enables AI agents to discover actor capabilities at runtime.
 *
 * Key improvements over v1:
 * - Single `accepts` message (combines messages + describe-message)
 * - Decorator-based handler registration (@accepts)
 * - Richer acceptance criteria (not just type matching)
 */

/**
 * JSON Schema type (subset for validation)
 */
export interface JSONSchema {
  type?: string | string[];
  properties?: Record<string, JSONSchema>;
  required?: string[];
  items?: JSONSchema;
  enum?: any[];
  const?: any;
  minimum?: number;
  maximum?: number;
  minLength?: number;
  maxLength?: number;
  pattern?: string;
  minProperties?: number;
  maxProperties?: number;
  additionalProperties?: boolean | JSONSchema;
  [key: string]: any;
}

/**
 * Request to discover what messages an actor accepts
 */
export interface AcceptsRequest {
  type: 'accepts';
  payload: {
    messageType?: string;  // Optional: filter to specific type
  };
}

/**
 * Response from accepts query
 * If messageType provided, returns single MessageAcceptance
 * If messageType omitted, returns array of all MessageAcceptance
 */
export type AcceptsResponse = MessageAcceptance | MessageAcceptance[];

/**
 * Description of a message type the actor accepts
 */
export interface MessageAcceptance {
  type: string;                    // Message type: 'filter', 'add', etc.
  description: string;              // How actor handles this message

  // Acceptance criteria (richer than just type!)
  criteria?: AcceptanceCriteria;

  // Payload guidance
  expectedPayload?: JSONSchema;
  typicalResponse?: JSONSchema;

  // Behavior properties
  consequences: MessageConsequences;
  examples?: MessageExample[];
  annotations?: Record<string, any>;
}

/**
 * Criteria for accepting a message (beyond just type matching)
 */
export interface AcceptanceCriteria {
  type?: string;               // Message type (implicit from handler name)
  payloadShape?: JSONSchema;   // Payload must match schema
  fromPattern?: string;        // Sender must match regex pattern
  stateCondition?: string;     // Actor state condition (expression)
}

/**
 * Message behavior properties (safety categories, side effects, etc.)
 */
export interface MessageConsequences {
  category: 'read-only' | 'write' | 'destructive' | 'admin';
  sideEffects: string[];
  canUndo: boolean;
  requiresConfirm: boolean;
  estimatedTime?: string;
  async?: boolean;
  streaming?: boolean;
}

/**
 * Example usage of a message
 */
export interface MessageExample {
  description: string;
  payload: any;
  expectedResponse?: any;
}

/**
 * Metadata provided to @accepts decorator
 */
export interface MessageAcceptanceMetadata {
  type?: string;           // Override message type (default: method name)
  description: string;
  payload?: JSONSchema;
  consequences: MessageConsequences;
  examples?: MessageExample[];
  criteria?: AcceptanceCriteria;
}

/**
 * Internal: Handler registration entry
 */
export interface HandlerRegistration {
  handler: Function;
  metadata: MessageAcceptance;
}

/**
 * @accepts decorator - Register a message handler with metadata
 *
 * Captures handler metadata at definition site for runtime introspection.
 * Enables actors to describe their capabilities via the introspection protocol.
 *
 * @param metadata - Message acceptance metadata
 * @returns Method decorator
 *
 * @example
 * ```typescript
 * class CollectionActor extends Actor {
 *   @accepts({
 *     description: 'Filter collection items using a predicate',
 *     payload: {
 *       type: 'object',
 *       properties: {
 *         predicate: { type: 'object' }
 *       }
 *     },
 *     consequences: {
 *       category: 'read-only',
 *       sideEffects: [],
 *       canUndo: true,
 *       requiresConfirm: false
 *     }
 *   })
 *   async filter(message: Message): Promise<MessageResponse> {
 *     // Handle filter
 *   }
 * }
 * ```
 */
export function accepts(metadata: MessageAcceptanceMetadata): MethodDecorator {
  return function (
    target: any,
    propertyKey: string | symbol,
    descriptor: PropertyDescriptor
  ) {
    // Initialize _acceptedMessages map if not exists
    if (!target._acceptedMessages) {
      target._acceptedMessages = new Map<string, HandlerRegistration>();
    }

    // Use provided type or default to method name
    const messageType = metadata.type || String(propertyKey);

    // Register handler with metadata
    target._acceptedMessages.set(messageType, {
      handler: descriptor.value,
      metadata: {
        type: messageType,
        description: metadata.description,
        expectedPayload: metadata.payload,
        consequences: metadata.consequences,
        examples: metadata.examples,
        criteria: metadata.criteria,
      },
    });

    return descriptor;
  };
}

/**
 * Introspect request - Get full actor + accepts info
 */
export interface IntrospectRequest {
  type: 'introspect';
  payload: {
    includeExamples?: boolean;
  };
}

/**
 * Introspect response - Actor metadata + accepted messages
 */
export interface IntrospectResponse {
  actor: ActorMetadata;
  accepts: MessageAcceptance[];
}

/**
 * Actor metadata (describe-actor response)
 */
export interface ActorMetadata {
  address: string;
  type: string;
  version?: string;
  description?: string;
  capabilities?: string[];
  state?: string;
  annotations?: Record<string, any>;
}

/**
 * Ping request - Health check
 */
export interface PingRequest {
  type: 'ping';
  payload: Record<string, never>;
}

/**
 * Ping response - Actor is alive
 */
export interface PingResponse {
  status: 'ok';
  latency?: number;
}

/**
 * Describe actor request - Get actor metadata
 */
export interface DescribeActorRequest {
  type: 'describe-actor';
  payload: Record<string, never>;
}
