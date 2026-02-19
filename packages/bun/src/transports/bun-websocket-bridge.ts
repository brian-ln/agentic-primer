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
  /**
   * Per-connection drain queue for backpressure handling.
   *
   * send() return values (from bun-types):
   *   0   = dropped (buffer overflow — message NOT sent; must retry on drain)
   *  -1   = backpressured (Bun buffered the message; stop sending until drain)
   *  > 0  = success
   *
   * Presence in this Map means the socket is currently congested. An empty
   * array marks congestion without a pending retry (status === -1 case).
   * A non-empty array contains messages dropped (status === 0) that must be
   * resent on the next drain event.
   */
  private readonly drainQueue = new Map<Bun.ServerWebSocket<ConnectionData>, string[]>();

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

  /** Removes the socket from the connection tracking Set and clears its drain queue. */
  handleClose(ws: Bun.ServerWebSocket<ConnectionData>, _code: number, _reason: string): void {
    this.connections.delete(ws);
    this.drainQueue.delete(ws);
  }

  /**
   * Flushes queued messages for a socket whose send buffer has drained.
   *
   * Stops early if the socket becomes congested again mid-flush:
   *   status === 0  → still dropped; leave remaining messages in queue
   *   status === -1 → Bun accepted the message but buffer is filling again;
   *                   shift it off our queue (Bun owns it now) then stop
   *   status  > 0  → sent immediately; continue draining
   */
  handleDrain(ws: Bun.ServerWebSocket<ConnectionData>): void {
    const queue = this.drainQueue.get(ws);
    if (!queue) return;

    while (queue.length > 0) {
      const status = ws.send(queue[0]);
      if (status === 0) break;      // still dropped — wait for next drain
      queue.shift();                // sent (immediately or buffered by Bun)
      if (status === -1) break;     // buffer filling again — stop sending
    }

    if (queue.length === 0) this.drainQueue.delete(ws);
  }

  /**
   * Broadcasts a message to all currently connected WebSockets.
   *
   * Handles backpressure per connection:
   *   - If a socket already has queued messages, appends to its queue instead
   *     of sending directly (preserves order).
   *   - If send() returns -1 (Bun buffered the message but socket is now
   *     congested), records an empty queue so future messages are queued.
   *   - If send() returns 0 (dropped), adds the message to the queue for
   *     retry when drain fires.
   *
   * Note: backpressure for individual sends via handleMessage (middleware
   * send callbacks) is not tracked — those are typically small response
   * messages where Bun buffering (status -1) is acceptable.
   */
  broadcast<T>(message: T): void {
    const serialized = JSON.stringify(message);
    for (const ws of this.connections) {
      try {
        const queue = this.drainQueue.get(ws);
        if (queue) {
          // Socket is congested — queue locally to preserve order.
          queue.push(serialized);
          continue;
        }

        const status = ws.send(serialized);
        if (status === -1) {
          // Bun buffered the message, but socket is now under backpressure.
          // Queue future messages; nothing to retry (Bun owns this one).
          this.drainQueue.set(ws, []);
        } else if (status === 0) {
          // Message was dropped. Queue it for retry on drain.
          this.drainQueue.set(ws, [serialized]);
        }
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
