#!/usr/bin/env bun
/**
 * Signal Hub Client - Public API
 *
 * Exports Signal Hub client components for use in SEAG/UGS applications.
 *
 * @example
 * ```typescript
 * import { SignalHubClient } from './messaging/signal-hub';
 *
 * const client = new SignalHubClient({
 *   url: 'wss://signal-hub.example.com',
 *   jwt: 'your-jwt-token',
 * });
 *
 * await client.connect();
 * ```
 */

// Core client
export { SignalHubClient } from './client.ts';

// Type definitions
export type {
  SignalHubConfig,
  SignalHubEvents,
  ActorRegistration,
  QueuedMessage,
  ConnectionState,
  HubConnectionInfo,
  ReconnectConfig,
  MessageQueueConfig,
} from './types.ts';
