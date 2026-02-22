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
// Binary channel protocol types
// (Schema source of truth: packages/protocols/schema/hub-messages.schema.json)
// ---------------------------------------------------------------------------

/**
 * Client → Server: open a binary channel to an actor address.
 * Message type: 'channel:open'
 * Server replies with ChannelOpened containing the assigned channelId.
 */
export interface ChannelOpen {
  type: 'channel:open';
  /** Target actor address, e.g. @(ai/stt/bln_ai/deepgram/flux) */
  address: string;
}

/**
 * Server → Client: binary channel confirmed.
 * Message type: 'channel:opened'
 * channelId = fnv32a(address) & 0xFFFFFFFF.
 * All subsequent binary frames prefixed with these 4 bytes (LE uint32) route to the bound actor.
 */
export interface ChannelOpened {
  type: 'channel:opened';
  channelId: number;
  address: string;
}

/**
 * Client or Server → peer: close a binary channel.
 * Message type: 'channel:close'
 */
export interface ChannelClose {
  type: 'channel:close';
  channelId: number;
  reason?: string;
}

/**
 * Server → Client: error during channel open or frame delivery.
 * Message type: 'channel:error'
 */
export interface ChannelError {
  type: 'channel:error';
  channelId: number;
  code: 'address_not_found' | 'channel_id_collision' | 'channel_not_open' | 'actor_rejected' | 'internal_error';
  message: string;
}

// ---------------------------------------------------------------------------
// Binary channel utilities
// ---------------------------------------------------------------------------

/**
 * FNV-1a 32-bit hash — deterministic, non-crypto, no external deps.
 * Returns an unsigned 32-bit integer.
 *
 * Used to derive binary channel IDs from actor addresses:
 *   channelId = fnv32a(actorAddress) & 0xFFFFFFFF
 *
 * Collision probability across 16 distinct actors: ~0.5%.
 * On collision, channel:open returns 'channel_id_collision' and the
 * client should retry with a salted address.
 */
export function fnv32a(str: string): number {
  let hash = 0x811c9dc5;
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193) >>> 0; // unsigned 32-bit
  }
  return hash;
}

/**
 * Derive the channel ID for an actor address.
 * Matches the server-side derivation in binaryChannelMiddleware.
 */
export function addressToChannelId(actorAddress: string): number {
  return fnv32a(actorAddress);
}

/**
 * Encode a binary frame: 4-byte LE channelId prefix + payload bytes.
 * Used by clients (browser, Bun CLI) to send audio/binary data on a channel.
 */
export function encodeBinaryFrame(channelId: number, payload: Uint8Array): Uint8Array {
  const frame = new Uint8Array(4 + payload.byteLength);
  const view = new DataView(frame.buffer);
  view.setUint32(0, channelId, /* littleEndian= */ true);
  frame.set(payload, 4);
  return frame;
}

/**
 * Decode a binary frame: extract channelId and payload from a prefixed frame.
 * Returns null if the frame is too short to contain a valid channelId prefix.
 */
export function decodeBinaryFrame(frame: Uint8Array): { channelId: number; payload: Uint8Array } | null {
  if (frame.byteLength < 4) return null;
  const view = new DataView(frame.buffer, frame.byteOffset, frame.byteLength);
  const channelId = view.getUint32(0, /* littleEndian= */ true);
  const payload = frame.subarray(4);
  return { channelId, payload };
}

/**
 * Route an incoming binary frame to a registered actor via the actor system.
 *
 * Called by bridge handleMessage implementations BEFORE composeWsMiddleware
 * when the frame is binary. The channel map is maintained per-connection by
 * binaryChannelMiddleware (populated on channel:open, cleared on channel:close).
 *
 * Returns true if routed, false if channelId not registered (frame is dropped).
 */
export function handleBinaryFrame(
  frame: Uint8Array,
  channelMap: Map<number, string>,
  system: ActorSystem,
): boolean {
  const decoded = decodeBinaryFrame(frame);
  if (!decoded) return false;

  const actorAddress = channelMap.get(decoded.channelId);
  if (!actorAddress) return false;

  system.send(address(actorAddress), 'audio.frame', decoded.payload);
  return true;
}

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
 * Binary channel control-plane middleware.
 *
 * Intercepts channel:open and channel:close text frames and manages a per-connection
 * channel map (channelId → actor address). Binary data frames are NOT handled here —
 * they bypass composeWsMiddleware entirely via handleBinaryFrame() in each bridge's
 * handleMessage implementation.
 *
 * Usage:
 *   const channelMap = new Map<number, string>();
 *   const handle = composeWsMiddleware(
 *     binaryChannelMiddleware(channelMap),
 *     heartbeatMiddleware,
 *     actorRoutingMiddleware(system),
 *   );
 *
 * Each bridge creates one channelMap per connection and passes it to this middleware.
 * The map is ephemeral — cleared on WebSocket close. Actors are durable; the channel
 * binding is not.
 */
export function binaryChannelMiddleware(channelMap: Map<number, string>): WsMiddleware {
  return (ctx, next) => {
    const { type } = ctx.data;

    if (type === 'channel:open') {
      const { address: actorAddress } = ctx.data as { address: string };
      const channelId = fnv32a(actorAddress);

      // Collision check: reject if a *different* address already holds this channelId
      const existing = channelMap.get(channelId);
      if (existing && existing !== actorAddress) {
        const err: ChannelError = {
          type: 'channel:error',
          channelId,
          code: 'channel_id_collision',
          message: `Channel ID ${channelId} already bound to ${existing}`,
        };
        ctx.send(JSON.stringify(err));
        return;
      }

      channelMap.set(channelId, actorAddress);
      const opened: ChannelOpened = { type: 'channel:opened', channelId, address: actorAddress };
      ctx.send(JSON.stringify(opened));
      return; // consume — do not route as actor message
    }

    if (type === 'channel:close') {
      const { channelId } = ctx.data as { channelId: number };
      channelMap.delete(channelId);
      return; // consume
    }

    return next();
  };
}

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
