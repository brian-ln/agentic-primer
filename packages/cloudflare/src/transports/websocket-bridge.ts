/**
 * WebSocket Bridge — Server-side DO WebSocket handler
 *
 * Bridges browser RemoteTransport connections to the DO's internal ActorSystem.
 * This is the server-side counterpart to @agentic-primer/browser's RemoteTransport.
 *
 * Handles:
 * - WebSocket upgrade with Hibernatable WebSocket API
 * - Actor message deserialization and routing to ActorSystem
 * - Heartbeat HEARTBEAT_PING/PONG (matching RemoteTransport protocol exactly)
 * - Broadcasting messages to all connected WebSockets
 * - Connection tracking via ctx.getWebSockets()
 */

import type { ActorSystem, ISerde } from '@agentic-primer/actors';
import {
  deserializeWsMessage,
  makeHeartbeatPong,
  routeWsActorMessage,
} from '@agentic-primer/actors';

export class WebSocketBridge {
  private readonly ctx: DurableObjectState;
  private readonly system: ActorSystem;
  private readonly serde: ISerde | undefined;

  constructor(ctx: DurableObjectState, system: ActorSystem, serde?: ISerde) {
    this.ctx = ctx;
    this.system = system;
    this.serde = serde;
  }

  /**
   * Handle a WebSocket upgrade request.
   * Creates a WebSocketPair, accepts the server socket with hibernation support,
   * and returns the client socket in the Response.
   */
  handleUpgrade(request: Request): Response {
    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);

    // Extract tags from URL params for WebSocket identification
    const url = new URL(request.url);
    const tag = url.searchParams.get('tag') || undefined;
    const tags = tag ? [tag] : [];

    this.ctx.acceptWebSocket(server, tags);

    return new Response(null, {
      status: 101,
      webSocket: client,
    });
  }

  /**
   * Handle an incoming WebSocket message.
   * Deserializes the message and routes actor protocol messages to the ActorSystem.
   * Handles HEARTBEAT_PING/PONG matching RemoteTransport protocol exactly.
   */
  handleMessage(ws: WebSocket, message: string | ArrayBuffer): void {
    try {
      const data = deserializeWsMessage(message, this.serde);

      // Handle heartbeat — must match RemoteTransport.sendHeartbeat() exactly
      if (data.type === 'HEARTBEAT_PING') {
        ws.send(JSON.stringify(makeHeartbeatPong(data as { id: string })));
        return;
      }

      routeWsActorMessage(data, this.system);
    } catch (error) {
      console.error('WebSocketBridge: Failed to handle message:', error);
    }
  }

  /**
   * Handle WebSocket close event.
   * Clean up is minimal since hibernation handles connection lifecycle.
   */
  handleClose(_ws: WebSocket): void {
    // Connection cleanup is handled automatically by the hibernation API.
    // Subclasses of DOActorSystem can override webSocketClose for custom logic.
  }

  /**
   * Broadcast a message to all connected WebSockets.
   * Uses ctx.getWebSockets() for hibernation-aware connection tracking.
   */
  broadcast<T>(message: T): void {
    const serialized = JSON.stringify(message);
    const sockets = this.ctx.getWebSockets();
    for (const ws of sockets) {
      try {
        ws.send(serialized);
      } catch {
        // Socket may have been closed between getWebSockets() and send()
      }
    }
  }

  /**
   * Get the number of currently connected WebSockets.
   */
  getConnectionCount(): number {
    return this.ctx.getWebSockets().length;
  }
}
