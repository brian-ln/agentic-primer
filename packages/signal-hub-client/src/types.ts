/**
 * Type definitions for Signal Hub browser client
 */

import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';

/**
 * Client configuration options
 */
export interface SignalHubClientOptions {
  /** WebSocket URL for Signal Hub (wss:// or ws://) */
  url: string;

  /** JWT authentication token */
  jwt: string;

  /** Auto-reconnect on disconnect (default: true) */
  autoReconnect?: boolean;

  /** Maximum reconnection attempts (default: Infinity) */
  maxReconnectAttempts?: number;

  /** Initial reconnect delay in ms (default: 1000) */
  reconnectDelay?: number;

  /** Maximum reconnect delay in ms (default: 30000) */
  maxReconnectDelay?: number;

  /** Heartbeat interval in ms (default: 30000) */
  heartbeatInterval?: number;

  /** Protocol version (default: "0.1.0") */
  protocolVersion?: string;
}

/**
 * Actor registration options
 */
export interface ActorRegistration {
  /** Actor address (auto-generated if not provided) */
  address?: CanonicalAddress;

  /** Actor capabilities */
  capabilities: string[];

  /** Additional metadata */
  metadata?: Record<string, unknown>;
}

/**
 * Connection states
 */
export type ConnectionState =
  | 'disconnected'
  | 'connecting'
  | 'connected'
  | 'reconnecting'
  | 'disconnecting';

/**
 * Message event payload
 */
export interface HubMessageEvent {
  /** The received message */
  message: SharedMessage;

  /** Original actor address (from metadata.originalFrom) */
  originalFrom?: CanonicalAddress;

  /** Whether this was forwarded through hub */
  forwarded?: boolean;
}

/**
 * Error event payload
 */
export interface ErrorEvent {
  /** Error message */
  message: string;

  /** Error code */
  code?: string;

  /** Original error */
  error?: Error;
}

/**
 * Connection event payload
 */
export interface ConnectionEvent {
  /** Session ID from hub:connected */
  sessionId?: string;

  /** Server protocol version */
  serverVersion?: string;

  /** Actor identity (from JWT) */
  actorIdentity?: CanonicalAddress;
}

/**
 * Event handler types
 */
export type MessageHandler = (event: HubMessageEvent) => void;
export type ConnectionHandler = (event: ConnectionEvent) => void;
export type ErrorHandler = (event: ErrorEvent) => void;
export type StateChangeHandler = (state: ConnectionState) => void;

/**
 * Event types
 */
export type ClientEvent =
  | 'message'
  | 'connected'
  | 'disconnected'
  | 'error'
  | 'stateChange'
  | 'registered'
  | 'unregistered';

/**
 * Hub message types (subset used by client)
 */
export type HubMessageType =
  | 'hub:connect'
  | 'hub:connected'
  | 'hub:register'
  | 'hub:registered'
  | 'hub:unregister'
  | 'hub:unregistered'
  | 'hub:send'
  | 'hub:delivery_ack'
  | 'hub:heartbeat'
  | 'hub:heartbeat_ack'
  | 'hub:disconnect'
  | 'hub:error'
  | 'hub:unknown_actor';

/**
 * Hub error payload
 */
export interface HubError {
  code: string;
  message: string;
  details?: unknown;
}

/**
 * Send message options
 */
export interface SendOptions {
  /** Message type (application-level) */
  type: string;

  /** Message payload (application data) */
  data: unknown;

  /** Require delivery acknowledgment (default: false) */
  requireAck?: boolean;

  /** Message TTL in ms (default: 30000) */
  ttl?: number;

  /** Trace ID for distributed tracing */
  traceId?: string;

  /** Message priority (0=high, 2=low) */
  priority?: number;
}
