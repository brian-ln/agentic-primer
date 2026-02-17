#!/usr/bin/env bun
/**
 * Signal Hub Client Types
 *
 * Type definitions for Signal Hub client integration.
 * Uses SharedMessage from @agentic-primer/protocols for wire format.
 */

import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';

/**
 * Signal Hub connection state
 */
export type ConnectionState =
  | 'disconnected'
  | 'connecting'
  | 'connected'
  | 'reconnecting'
  | 'error';

/**
 * Signal Hub client configuration
 */
export interface SignalHubConfig {
  /** Signal Hub WebSocket URL (e.g., 'wss://signal-hub.example.com') */
  url: string;

  /** JWT authentication token */
  jwt: string;

  /** Protocol version (default: '0.1.0') */
  protocolVersion?: string;

  /** Heartbeat interval in ms (default: 25000) */
  heartbeatInterval?: number;

  /** Reconnection configuration */
  reconnect?: ReconnectConfig;

  /** Message queue configuration */
  messageQueue?: MessageQueueConfig;
}

/**
 * Reconnection configuration
 */
export interface ReconnectConfig {
  /** Enable automatic reconnection (default: true) */
  enabled: boolean;

  /** Max reconnection attempts (default: 10) */
  maxAttempts: number;

  /** Initial backoff delay in ms (default: 1000) */
  initialDelay: number;

  /** Max backoff delay in ms (default: 30000) */
  maxDelay: number;

  /** Backoff multiplier (default: 2 for exponential) */
  multiplier: number;
}

/**
 * Message queue configuration
 */
export interface MessageQueueConfig {
  /** Enable message queuing during disconnect (default: true) */
  enabled: boolean;

  /** Max queue size (default: 1000) */
  maxSize: number;

  /** Default message TTL in ms (default: 60000) */
  defaultTtl: number;
}

/**
 * Actor registration info
 */
export interface ActorRegistration {
  /** Actor's canonical address */
  actorAddress: CanonicalAddress;

  /** Actor capabilities */
  capabilities: string[];

  /** Custom metadata */
  metadata: Record<string, unknown>;

  /** Registration expiration timestamp (epoch ms) */
  expiresAt: number;

  /** Renewal token from hub:registered */
  renewalToken: string;

  /** Registration version */
  version: number;
}

/**
 * Queued message (waiting for connection)
 */
export interface QueuedMessage {
  /** The message to send */
  message: SharedMessage;

  /** When the message was queued */
  queuedAt: number;

  /** When the message expires */
  expiresAt: number;

  /** Resolve function for ask pattern */
  resolve?: (response: SharedMessage) => void;

  /** Reject function for errors */
  reject?: (error: Error) => void;
}

/**
 * Signal Hub client events
 */
export interface SignalHubEvents {
  /** Connection established and hub:connected received */
  connected: (sessionId: string) => void;

  /** Connection lost */
  disconnected: (reason: string) => void;

  /** Reconnecting to hub */
  reconnecting: (attempt: number) => void;

  /** Message received from Signal Hub */
  message: (message: SharedMessage) => void;

  /** Error occurred */
  error: (error: Error) => void;

  /** Actor registered with Signal Hub */
  actorRegistered: (actorAddress: CanonicalAddress) => void;

  /** Actor unregistered from Signal Hub */
  actorUnregistered: (actorAddress: CanonicalAddress) => void;
}

/**
 * Hub connection info (from hub:connected)
 */
export interface HubConnectionInfo {
  /** Session ID assigned by hub */
  sessionId: string;

  /** Server protocol version */
  serverVersion: string;

  /** Max message size in bytes */
  maxMessageSize: number;

  /** Recommended heartbeat interval in ms */
  heartbeatInterval: number;

  /** Server capabilities */
  capabilities: {
    maxActorsPerInstance: number;
    supportsBackpressure: boolean;
    supportedContentTypes: string[];
  };

  /** Server-verified actor identity (from JWT) */
  actorIdentity?: CanonicalAddress;
}
