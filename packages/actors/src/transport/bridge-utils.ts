/**
 * WebSocket Bridge Utilities
 *
 * Shared logic for server-side WebSocket bridge implementations.
 * Platform-agnostic: used by BunWebSocketBridge (packages/bun) and
 * WebSocketBridge (packages/cloudflare), and any future bridge targets.
 *
 * Protocol contract
 * -----------------
 * RemoteTransport (browser client) sends heartbeats as:
 *   { type: 'HEARTBEAT_PING', to: '__system__', from: '__client__',
 *     payload: { timestamp }, id: 'heartbeat-<timestamp>' }
 *
 * Every server-side bridge MUST reply with:
 *   { type: 'HEARTBEAT_PONG', to: '__client__', from: '__system__',
 *     payload: {}, id: <same id as ping> }
 *
 * The id must be echoed verbatim — RemoteTransport uses it for RTT tracking
 * and timeout detection.
 */

import type { ISerde } from '../interfaces.js';
import type { ActorSystem } from '../actor-system.js';
import { address, type Message } from '../message.js';

// ---------------------------------------------------------------------------
// Heartbeat protocol types
// ---------------------------------------------------------------------------

/** Sent by RemoteTransport clients on a fixed interval to detect dead connections. */
export interface HeartbeatPing {
  type: 'HEARTBEAT_PING';
  to: '__system__';
  from: '__client__';
  payload: { timestamp: number };
  id: string;
}

/** Server reply to a HeartbeatPing. Must echo the same `id` for RTT tracking. */
export interface HeartbeatPong {
  type: 'HEARTBEAT_PONG';
  to: '__client__';
  from: '__system__';
  payload: Record<string, never>;
  id: string;
}

/** Build the canonical PONG response for a received PING. */
export function makeHeartbeatPong(ping: Pick<HeartbeatPing, 'id'>): HeartbeatPong {
  return {
    type: 'HEARTBEAT_PONG',
    to: '__client__',
    from: '__system__',
    payload: {} as Record<string, never>,
    id: ping.id,
  };
}

// ---------------------------------------------------------------------------
// Deserialization
// ---------------------------------------------------------------------------

/**
 * Parse an incoming WebSocket message frame into a plain object.
 *
 * Handles:
 * - string  → JSON.parse
 * - Uint8Array / ArrayBuffer → ISerde.deserialize (if provided) or JSON decode
 *
 * Note: Bun's ServerWebSocket delivers binary frames as Buffer (a Uint8Array
 * subclass) when binaryType is the default "nodebuffer". This function handles
 * that correctly since Buffer extends Uint8Array.
 */
export function deserializeWsMessage(
  message: string | Uint8Array | ArrayBuffer,
  serde?: ISerde
): Record<string, unknown> {
  if (typeof message === 'string') {
    return JSON.parse(message) as Record<string, unknown>;
  }
  const bytes = message instanceof ArrayBuffer ? new Uint8Array(message) : message;
  if (serde) {
    return serde.deserialize(bytes) as Record<string, unknown>;
  }
  return JSON.parse(new TextDecoder().decode(bytes)) as Record<string, unknown>;
}

// ---------------------------------------------------------------------------
// Actor message routing
// ---------------------------------------------------------------------------

/**
 * Route an actor protocol message from a WebSocket frame to the ActorSystem.
 *
 * Returns true if a message was routed, false if the frame did not contain
 * a valid actor message ({ to, type } fields required). Callers may use the
 * return value to log or handle unrecognized frame types.
 */
export function routeWsActorMessage(
  data: Record<string, unknown>,
  system: ActorSystem
): boolean {
  if (!data.to || !data.type) return false;

  const actorMessage: Message = {
    id: (data.id as string) || crypto.randomUUID(),
    pattern: (data.pattern as Message['pattern']) || 'tell',
    to: address(data.to as string),
    from: data.from ? address(data.from as string) : undefined,
    type: data.type as string,
    payload: data.payload,
    correlationId: data.correlationId as string | undefined,
    timestamp: (data.timestamp as number) || Date.now(),
    metadata: data.metadata as Record<string, unknown> | undefined,
  };

  system.send(actorMessage.to, actorMessage.type, actorMessage.payload);
  return true;
}
