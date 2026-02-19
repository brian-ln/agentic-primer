/**
 * WebSocket Bridge — Server-side DO WebSocket handler
 *
 * Bridges browser RemoteTransport connections to the DO's internal ActorSystem.
 * This is the server-side counterpart to @agentic-primer/browser's RemoteTransport.
 *
 * Handles:
 * - WebSocket upgrade with Hibernatable WebSocket API
 * - Actor message routing via composable middleware stack
 * - Heartbeat HEARTBEAT_PING/PONG (via heartbeatMiddleware)
 * - Broadcasting messages to all connected WebSockets
 * - Connection tracking via ctx.getWebSockets()
 */

import type { ActorSystem, ISerde, WsMiddleware } from '@agentic-primer/actors';
import {
  composeWsMiddleware,
  heartbeatMiddleware,
  actorRoutingMiddleware,
} from '@agentic-primer/actors';

export class WebSocketBridge {
  private readonly ctx: DurableObjectState;
  private readonly serde: ISerde | undefined;
  private readonly handle: ReturnType<typeof composeWsMiddleware>;

  constructor(ctx: DurableObjectState, system: ActorSystem, serde?: ISerde, ...extraMiddleware: WsMiddleware[]) {
    this.ctx = ctx;
    this.serde = serde;
    this.handle = composeWsMiddleware(
      heartbeatMiddleware,
      actorRoutingMiddleware(system),
      ...extraMiddleware,
    );
  }

  /**
   * Handle a WebSocket upgrade request.
   * Creates a WebSocketPair, accepts the server socket with hibernation support,
   * and returns the client socket in the Response.
   */
  handleUpgrade(request: Request): Response {
    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);

    const url = new URL(request.url);
    const tag = url.searchParams.get('tag') || undefined;
    const tags = tag ? [tag] : [];

    this.ctx.acceptWebSocket(server, tags);

    return new Response(null, { status: 101, webSocket: client });
  }

  /** Routes the message through the middleware stack. */
  handleMessage(ws: WebSocket, message: string | ArrayBuffer): void {
    void this.handle(message, (json) => ws.send(json), this.serde);
  }

  /** No-op — hibernation handles connection lifecycle. */
  handleClose(_ws: WebSocket): void {}

  /** Broadcasts a message to all connected WebSockets. */
  broadcast<T>(message: T): void {
    const serialized = JSON.stringify(message);
    for (const ws of this.ctx.getWebSockets()) {
      try {
        ws.send(serialized);
      } catch {
        // Socket may have been closed between getWebSockets() and send()
      }
    }
  }

  /** Returns the number of currently connected WebSockets. */
  getConnectionCount(): number {
    return this.ctx.getWebSockets().length;
  }
}
