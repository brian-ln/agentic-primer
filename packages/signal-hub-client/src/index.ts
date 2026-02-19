/**
 * Signal Hub Browser Client
 *
 * Lightweight WebSocket client for connecting browser actors to Signal Hub.
 */

export { SignalHubClient } from './SignalHubClient.js';
export { SignalHubTransport, createSignalHubTransport } from './SignalHubTransport.js';

export type {
  SignalHubClientOptions,
  ActorRegistration,
  ConnectionState,
  HubMessageEvent,
  ErrorEvent,
  ConnectionEvent,
  MessageHandler,
  ConnectionHandler,
  ErrorHandler,
  StateChangeHandler,
  SendOptions,
  HubMessageType,
  HubError,
  ClientEvent,
} from './types.js';

export {
  generateBrowserAddress,
  isValidAddress,
  calculateBackoff,
  createDeferred,
  wait,
  isPlainObject,
  type Deferred,
} from './utils.js';
