/// <reference types="bun-types" />

/**
 * BunWebSocketBridge — Server-side Bun WebSocket handler
 *
 * Bun equivalent of WebSocketBridge in packages/cloudflare.
 * Bridges browser RemoteTransport connections to a Bun-hosted ActorSystem.
 *
 * Handles:
 * - WebSocket upgrade via Bun.serve()
 * - Actor message deserialization and routing to ActorSystem
 * - Heartbeat HEARTBEAT_PING/PONG (matching RemoteTransport protocol exactly)
 * - Broadcasting messages to all connected WebSockets
 * - Connection tracking via a Set (no DurableObjectState, no hibernation)
 */

import type { ActorSystem, ISerde } from '@agentic-primer/actors';
import { address, type Message } from '@agentic-primer/actors';

/** Per-connection data attached at upgrade time. */
export interface ConnectionData {
  id: string;
}

export class BunWebSocketBridge {
  private readonly system: ActorSystem;
  private readonly serde: ISerde | undefined;
  private readonly connections = new Set<Bun.ServerWebSocket<ConnectionData>>();

  constructor(system: ActorSystem, serde?: ISerde) {
    this.system = system;
    this.serde = serde;
  }

  /**
   * Handle a WebSocket upgrade request.
   * Attach a random id to the connection data, call server.upgrade(), return
   * undefined when upgraded (Bun sends the 101 response itself) or a 400
   * response when the request is not a valid WebSocket upgrade.
   */
  handleUpgrade(req: Request, server: Bun.Server<ConnectionData>): Response | undefined {
    const upgraded = server.upgrade(req, {
      data: { id: crypto.randomUUID() } satisfies ConnectionData,
    });
    if (upgraded) return undefined;
    return new Response('Not a WebSocket request', { status: 400 });
  }

  /**
   * Handle a new WebSocket connection opening.
   * Adds the socket to the connection tracking Set.
   */
  handleOpen(ws: Bun.ServerWebSocket<ConnectionData>): void {
    this.connections.add(ws);
  }

  /**
   * Handle an incoming WebSocket message.
   *
   * Protocol:
   * - HEARTBEAT_PING → reply with HEARTBEAT_PONG (same id, correct addressing)
   * - Actor messages (to + type present) → route via system.send()
   */
  handleMessage(
    ws: Bun.ServerWebSocket<ConnectionData>,
    message: string | Uint8Array
  ): void {
    try {
      let data: Record<string, unknown>;

      if (typeof message === 'string') {
        data = JSON.parse(message) as Record<string, unknown>;
      } else if (this.serde) {
        data = this.serde.deserialize(message) as Record<string, unknown>;
      } else {
        data = JSON.parse(new TextDecoder().decode(message)) as Record<string, unknown>;
      }

      // Handle heartbeat PING — must match RemoteTransport.sendHeartbeat() exactly:
      // incoming: { type: 'HEARTBEAT_PING', to: '__system__', from: '__client__', payload: { timestamp }, id }
      // outgoing: { type: 'HEARTBEAT_PONG', to: '__client__', from: '__system__', payload: {}, id }
      if (data.type === 'HEARTBEAT_PING') {
        const pong = {
          type: 'HEARTBEAT_PONG',
          to: '__client__',
          from: '__system__',
          payload: {},
          id: data.id,
        };
        ws.send(JSON.stringify(pong));
        return;
      }

      // Route actor protocol messages: { to, from, type, payload, id, ... }
      if (data.to && data.type) {
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

        this.system.send(actorMessage.to, actorMessage.type, actorMessage.payload);
      }
    } catch (error) {
      console.error('BunWebSocketBridge: Failed to handle message:', error);
    }
  }

  /**
   * Handle a WebSocket connection closing.
   * Removes the socket from the connection tracking Set.
   */
  handleClose(
    ws: Bun.ServerWebSocket<ConnectionData>,
    _code: number,
    _reason: string
  ): void {
    this.connections.delete(ws);
  }

  /**
   * Handle WebSocket backpressure drain event.
   * No-op — callers may override via subclass or composition if needed.
   */
  handleDrain(_ws: Bun.ServerWebSocket<ConnectionData>): void {
    // no-op
  }

  /**
   * Broadcast a message to all currently connected WebSockets.
   */
  broadcast<T>(message: T): void {
    const serialized = JSON.stringify(message);
    for (const ws of this.connections) {
      try {
        ws.send(serialized);
      } catch {
        // Socket may have closed between Set iteration and send()
      }
    }
  }

  /** Returns the number of currently tracked open connections. */
  getConnectionCount(): number {
    return this.connections.size;
  }

  /**
   * Returns an object suitable for the `websocket` key of Bun.serve().
   * All handlers are bound to this BunWebSocketBridge instance.
   *
   * Usage:
   * ```typescript
   * const bridge = new BunWebSocketBridge(system);
   * Bun.serve({
   *   fetch: (req, server) => bridge.handleUpgrade(req, server),
   *   websocket: bridge.createWebSocketHandlers(),
   * });
   * ```
   */
  createWebSocketHandlers(): {
    open(ws: Bun.ServerWebSocket<ConnectionData>): void;
    message(ws: Bun.ServerWebSocket<ConnectionData>, message: string | Uint8Array): void;
    close(ws: Bun.ServerWebSocket<ConnectionData>, code: number, reason: string): void;
    drain(ws: Bun.ServerWebSocket<ConnectionData>): void;
  } {
    return {
      open: (ws) => this.handleOpen(ws),
      message: (ws, msg) => this.handleMessage(ws, msg),
      close: (ws, code, reason) => this.handleClose(ws, code, reason),
      drain: (ws) => this.handleDrain(ws),
    };
  }
}
