/**
 * WebSocket Bridge Utilities
 *
 * Middleware system and shared logic for server-side WebSocket bridges.
 * Platform-agnostic: used by BunWebSocketBridge (packages/bun),
 * WebSocketBridge (packages/cloudflare), and any future bridge targets.
 *
 * Heartbeat protocol contract
 * ---------------------------
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
 *
 * Middleware usage
 * ----------------
 * import { composeWsMiddleware, heartbeatMiddleware, actorRoutingMiddleware } from '@agentic-primer/actors';
 *
 * const handle = composeWsMiddleware(
 *   heartbeatMiddleware,
 *   actorRoutingMiddleware(system),
 * );
 *
 * // In any bridge's handleMessage:
 * void handle(message, (json) => ws.send(json), serde);
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
// Middleware types
// ---------------------------------------------------------------------------

/** Per-message context threaded through the middleware stack. */
export interface WsContext {
  /** Deserialized message data. */
  readonly data: Record<string, unknown>;
  /** Send a JSON string back to this specific WebSocket connection. */
  readonly send: (json: string) => void;
}

/**
 * A WebSocket middleware function.
 *
 * Call next() to pass control to the next middleware.
 * Omit next() to consume the message (short-circuit the stack).
 * May be async — await anything before or after next().
 *
 * @example
 * // Logging middleware — runs before and after downstream
 * const logger: WsMiddleware = async (ctx, next) => {
 *   console.log('[ws:in]', ctx.data.type);
 *   await next();
 *   console.log('[ws:done]', ctx.data.type);
 * };
 */
export type WsMiddleware = (ctx: WsContext, next: () => Promise<void>) => Promise<void> | void;

// ---------------------------------------------------------------------------
// Compose
// ---------------------------------------------------------------------------

/**
 * Compose an ordered stack of WsMiddleware into a single async message handler.
 *
 * The returned function is called once per incoming WebSocket frame.
 * Errors thrown by any middleware are caught and logged; they do not propagate
 * to the platform WebSocket handler (which would close the connection).
 *
 * @example
 * const handle = composeWsMiddleware(
 *   authMiddleware(kvStore),
 *   heartbeatMiddleware,
 *   actorRoutingMiddleware(system),
 * );
 *
 * // In BunWebSocketBridge.handleMessage:
 * void handle(message, (json) => ws.send(json), this.serde);
 *
 * // In WebSocketBridge.handleMessage (Cloudflare):
 * void handle(message, (json) => ws.send(json), this.serde);
 */
export function composeWsMiddleware(
  ...middlewares: WsMiddleware[]
): (message: string | Uint8Array | ArrayBuffer, send: (json: string) => void, serde?: ISerde) => Promise<void> {
  return async (message, send, serde) => {
    const ctx: WsContext = {
      data: deserializeWsMessage(message, serde),
      send,
    };

    let cursor = -1;

    async function dispatch(i: number): Promise<void> {
      if (i <= cursor) throw new Error('next() called multiple times in the same middleware');
      cursor = i;
      const middleware = middlewares[i];
      if (!middleware) return;
      await middleware(ctx, () => dispatch(i + 1));
    }

    try {
      await dispatch(0);
    } catch (err) {
      console.error('WS middleware error:', err);
    }
  };
}

// ---------------------------------------------------------------------------
// Built-in middleware
// ---------------------------------------------------------------------------

/**
 * Intercepts HEARTBEAT_PING frames and replies with HEARTBEAT_PONG.
 * Consumes the message (does not call next) — heartbeats are not actor messages.
 */
export const heartbeatMiddleware: WsMiddleware = (ctx, next) => {
  if (ctx.data.type !== 'HEARTBEAT_PING') return next();
  ctx.send(JSON.stringify(makeHeartbeatPong(ctx.data as { id: string })));
};

/**
 * Routes actor protocol messages ({ to, type, ... }) to the ActorSystem.
 * Always calls next() — allows downstream middleware to observe routed messages.
 */
export function actorRoutingMiddleware(system: ActorSystem): WsMiddleware {
  return (ctx, next) => {
    routeWsActorMessage(ctx.data, system);
    return next();
  };
}

// ---------------------------------------------------------------------------
// Low-level utilities (used by compose and available for custom middleware)
// ---------------------------------------------------------------------------

/**
 * Parse an incoming WebSocket message frame into a plain object.
 *
 * Handles:
 * - string        → JSON.parse
 * - Uint8Array    → ISerde.deserialize (if provided) or JSON decode
 * - ArrayBuffer   → converted to Uint8Array, then same as above
 *
 * Note: Bun's ServerWebSocket delivers binary frames as Buffer (a Uint8Array
 * subclass) when binaryType is the default "nodebuffer". Handled correctly
 * since Buffer extends Uint8Array.
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

/**
 * Route an actor protocol message to the ActorSystem.
 * Returns true if routed, false if the data lacked required { to, type } fields.
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
