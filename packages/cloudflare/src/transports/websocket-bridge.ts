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
  binaryChannelMiddleware,
  handleBinaryFrame,
} from '@agentic-primer/actors';

export class WebSocketBridge {
  private readonly ctx: DurableObjectState;
  private readonly system: ActorSystem;
  private readonly serde: ISerde | undefined;
  private readonly extraMiddleware: WsMiddleware[];
  /**
   * Per-connection binary channel maps: channelId → actor address.
   * Populated by binaryChannelMiddleware on channel:open, cleared on channel:close.
   * Map (not WeakMap) so handleClose can explicitly delete entries.
   */
  private readonly channelMaps = new Map<WebSocket, Map<number, string>>();

  constructor(ctx: DurableObjectState, system: ActorSystem, serde?: ISerde, ...extraMiddleware: WsMiddleware[]) {
    this.ctx = ctx;
    this.system = system;
    this.serde = serde;
    this.extraMiddleware = extraMiddleware;
  }

  private getChannelMap(ws: WebSocket): Map<number, string> {
    let map = this.channelMaps.get(ws);
    if (!map) {
      map = new Map();
      this.channelMaps.set(ws, map);
    }
    return map;
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

  /**
   * Route an incoming WebSocket frame.
   *
   * Binary frames (ArrayBuffer): binary channel fast-path — look up the actor
   * address from the per-connection channel map via the 4-byte channelId prefix,
   * deliver as Message<Uint8Array> to the actor. Bypasses middleware entirely.
   *
   * Text/string frames: channel control (channel:open/close) and actor protocol
   * messages are handled via the composable middleware stack.
   */
  handleMessage(ws: WebSocket, message: string | ArrayBuffer): void {
    if (message instanceof ArrayBuffer) {
      handleBinaryFrame(new Uint8Array(message), this.getChannelMap(ws), this.system);
      return;
    }
    const channelMap = this.getChannelMap(ws);
    void composeWsMiddleware(
      binaryChannelMiddleware(channelMap),
      heartbeatMiddleware,
      actorRoutingMiddleware(this.system),
      ...this.extraMiddleware,
    )(message, (json) => ws.send(json), this.serde);
  }

  /** Clears the binary channel map for this connection. */
  handleClose(ws: WebSocket): void {
    this.channelMaps.delete(ws);
  }

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
