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
import {
  deserializeWsMessage,
  makeHeartbeatPong,
  routeWsActorMessage,
} from '@agentic-primer/actors';

/** Per-connection data attached at upgrade time. */
export interface ConnectionData {
  id: string;
}

/** Minimal duck type for the server passed to fetch handlers — avoids bun-types generic variance issues. */
interface UpgradableServer {
  upgrade(req: Request, options?: { data?: ConnectionData; headers?: HeadersInit }): boolean;
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
  handleUpgrade(req: Request, server: UpgradableServer): Response | undefined {
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
      const data = deserializeWsMessage(message, this.serde);

      // Handle heartbeat — must match RemoteTransport.sendHeartbeat() exactly
      if (data.type === 'HEARTBEAT_PING') {
        ws.send(JSON.stringify(makeHeartbeatPong(data as { id: string })));
        return;
      }

      routeWsActorMessage(data, this.system);
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
