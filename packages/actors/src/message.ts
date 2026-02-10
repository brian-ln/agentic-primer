/**
 * Message Protocol for Actor System
 *
 * Portable message types for actor-to-actor communication.
 * Adapted from simplify/src/messaging/message.ts - uses web platform APIs only.
 *
 * Address format: @(id) canonical references.
 */

// Address type - @(id) references any actor
export type Address = `@(${string})`;

// Message types for different communication patterns
export type MessagePattern = 'tell' | 'ask' | 'stream';

// Core message structure
export interface Message<T = any> {
  id: string;
  pattern: MessagePattern;
  to: Address;
  from?: Address;
  type: string;
  payload: T;
  correlationId?: string;
  timestamp: number;
  metadata?: Record<string, any>;
}

// Response structure for ask pattern
export interface MessageResponse<T = any> {
  id: string;
  correlationId: string;
  from: Address;
  to: Address | undefined;
  success: boolean;
  payload?: T;
  error?: string;
  timestamp: number;
}

// Stream event for continuous updates
export interface StreamEvent<T = any> {
  id: string;
  correlationId: string;
  from: Address;
  type: 'data' | 'end' | 'error';
  payload?: T;
  error?: string;
  timestamp: number;
}

// Token stream event for LLM-style streaming
export interface TokenStreamEvent {
  type: 'token' | 'done' | 'error';
  content?: string;
  error?: string;
  timestamp: number;
}

// AsyncIterator-based streaming
export interface AsyncStreamMessage<T = any> {
  id: string;
  correlationId: string;
  from: Address;
  type: 'data' | 'end' | 'error';
  payload?: T;
  error?: string;
  timestamp: number;
}

export interface StreamAsyncOptions {
  signal?: AbortSignal;
  bufferSize?: number;
  timeout?: number;
}

// Stream callback type
export type StreamCallback<T> = (event: T) => void | Promise<void>;

// Message handler interface
export interface MessageHandler {
  receive(message: Message): Promise<MessageResponse>;
}

// Create an address from an ID
export function address(id: string): Address {
  return `@(${id})`;
}

// Parse an address to get the ID
export function parseAddress(addr: Address): string {
  const match = addr.match(/^@\((.+)\)$/);
  if (!match) {
    throw new Error(`Invalid address format: ${addr}`);
  }
  return match[1];
}

// Generate unique message ID
export function generateMessageId(): string {
  return crypto.randomUUID();
}

// Generate correlation ID for ask/reply pattern
export function generateCorrelationId(): string {
  return crypto.randomUUID();
}

// Create a message
export function createMessage<T = any>(
  to: Address,
  type: string,
  payload: T,
  options: {
    pattern?: MessagePattern;
    from?: Address;
    correlationId?: string;
    metadata?: Record<string, any>;
  } = {}
): Message<T> {
  return {
    id: generateMessageId(),
    pattern: options.pattern || 'tell',
    to,
    from: options.from,
    type,
    payload,
    correlationId: options.correlationId,
    timestamp: Date.now(),
    metadata: options.metadata,
  };
}

// Create a response
export function createResponse<T = any>(
  originalMessage: Message,
  payload: T,
  success: boolean = true
): MessageResponse<T> {
  return {
    id: generateMessageId(),
    correlationId: originalMessage.correlationId || originalMessage.id,
    from: originalMessage.to,
    to: originalMessage.from,
    success,
    payload,
    timestamp: Date.now(),
  };
}

// Create an error response
export function createErrorResponse(
  originalMessage: Message,
  error: string
): MessageResponse {
  return {
    id: generateMessageId(),
    correlationId: originalMessage.correlationId || originalMessage.id,
    from: originalMessage.to,
    to: originalMessage.from,
    success: false,
    error,
    timestamp: Date.now(),
  };
}
