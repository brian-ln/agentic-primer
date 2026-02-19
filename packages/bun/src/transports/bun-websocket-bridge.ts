/// <reference types="bun-types" />

/**
 * BunWebSocketBridge — Server-side Bun WebSocket handler
 *
 * Bun equivalent of WebSocketBridge in packages/cloudflare.
 * Bridges browser RemoteTransport connections to a Bun-hosted ActorSystem.
 *
 * Handles:
 * - WebSocket upgrade via Bun.serve()
 * - Actor message routing via composable middleware stack
 * - Heartbeat HEARTBEAT_PING/PONG (via heartbeatMiddleware)
 * - Broadcasting messages to all connected WebSockets
 * - Connection tracking via a Set (no DurableObjectState, no hibernation)
 */

import type { ActorSystem, ISerde, WsMiddleware } from '@agentic-primer/actors';
import {
  composeWsMiddleware,
  heartbeatMiddleware,
  actorRoutingMiddleware,
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
  private readonly serde: ISerde | undefined;
  private readonly connections = new Set<Bun.ServerWebSocket<ConnectionData>>();
  private readonly handle: ReturnType<typeof composeWsMiddleware>;

  constructor(system: ActorSystem, serde?: ISerde, ...extraMiddleware: WsMiddleware[]) {
    this.serde = serde;
    this.handle = composeWsMiddleware(
      heartbeatMiddleware,
      actorRoutingMiddleware(system),
      ...extraMiddleware,
    );
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

  /** Adds the socket to the connection tracking Set. */
  handleOpen(ws: Bun.ServerWebSocket<ConnectionData>): void {
    this.connections.add(ws);
  }

  /** Routes the message through the middleware stack. */
  handleMessage(ws: Bun.ServerWebSocket<ConnectionData>, message: string | Uint8Array): void {
    void this.handle(message, (json) => ws.send(json), this.serde);
  }

  /** Removes the socket from the connection tracking Set. */
  handleClose(ws: Bun.ServerWebSocket<ConnectionData>, _code: number, _reason: string): void {
    this.connections.delete(ws);
  }

  /** No-op — override via extra middleware passed to the constructor. */
  handleDrain(_ws: Bun.ServerWebSocket<ConnectionData>): void {}

  /** Broadcasts a message to all currently connected WebSockets. */
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
   *
   * @example
   * const bridge = new BunWebSocketBridge(system);
   * Bun.serve({
   *   fetch: (req, server) => bridge.handleUpgrade(req, server),
   *   websocket: bridge.createWebSocketHandlers(),
   * });
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
