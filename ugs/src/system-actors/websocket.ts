#!/usr/bin/env bun
/**
 * WebSocketActor - WebSocket client with host whitelisting and reconnection
 *
 * Pure actor model: WebSocketActor IS the capability.
 * Access control happens through routing, not helper objects.
 *
 * Features:
 * - Host whitelisting (allowedHosts)
 * - Connection lifecycle management
 * - Message send/receive via ports
 * - Automatic reconnection (configurable)
 * - Connection limits
 */

import { Actor, createResponse, createErrorResponse, createPortChannel } from '@agentic-primer/actors';
import type { Message, MessageResponse, MessageRouter } from '@agentic-primer/actors';
import type { PortChannel } from '@agentic-primer/actors';

/**
 * Reconnection configuration
 */
export interface ReconnectConfig {
  enabled: boolean;
  maxAttempts: number;
  backoff: 'linear' | 'exponential';
}

/**
 * WebSocketActor configuration
 */
export interface WebSocketActorConfig {
  /** Allowed hosts (whitelist) */
  allowedHosts: string[];

  /** Maximum concurrent connections */
  maxConnections: number;

  /** Reconnection configuration */
  reconnect: ReconnectConfig;
}

/**
 * WebSocket event types
 */
export type WSEvent =
  | { type: 'open'; connectionId: string }
  | { type: 'message'; connectionId: string; data: any }
  | { type: 'error'; connectionId: string; error: string }
  | { type: 'close'; connectionId: string; code: number; reason: string };

/**
 * Internal connection state
 */
interface WSConnection {
  id: string;
  ws: WebSocket;
  url: string;
  reconnectAttempts: number;
}

/**
 * WebSocketActor - Provides WebSocket client functionality with internal validation
 *
 * @example
 * ```typescript
 * const ws = new WebSocketActor('ws', router, {
 *   allowedHosts: ['ws.example.com', 'realtime.api.com'],
 *   maxConnections: 10,
 *   reconnect: {
 *     enabled: true,
 *     maxAttempts: 5,
 *     backoff: 'exponential'
 *   }
 * });
 * router.registerActor('/workflows/system/websocket', ws);
 *
 * // Connect
 * const response = await actor.ask(
 *   address('/workflows/system/websocket'),
 *   'ws.connect',
 *   { url: 'wss://realtime.api.com/stream' }
 * );
 *
 * // Subscribe to events
 * const subscription = await actor.ask(
 *   address('/workflows/system/websocket'),
 *   'ws.subscribe',
 *   { connectionId: response.payload.connectionId }
 * );
 *
 * for await (const event of subscription.payload.stream) {
 *   console.log('Event:', event);
 * }
 * ```
 */
export class WebSocketActor extends Actor {
  private allowedHosts: Set<string>;
  private maxConnections: number;
  private reconnectConfig: ReconnectConfig;
  private connections: Map<string, WSConnection>;
  private eventPorts: Map<string, PortChannel<WSEvent>>;

  constructor(id: string, router: MessageRouter, config: WebSocketActorConfig) {
    super(id, router);
    this.allowedHosts = new Set(config.allowedHosts);
    this.maxConnections = config.maxConnections;
    this.reconnectConfig = config.reconnect;
    this.connections = new Map();
    this.eventPorts = new Map();
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      if (type === 'ws.connect') {
        return await this.handleConnect(message, payload);
      }

      if (type === 'ws.send') {
        return await this.handleSend(message, payload);
      }

      if (type === 'ws.close') {
        return await this.handleClose(message, payload);
      }

      if (type === 'ws.subscribe') {
        return await this.handleSubscribe(message, payload);
      }

      return createErrorResponse(message, `Unknown message type: ${type}`);
    } catch (error: any) {
      this.logError('WebSocket operation failed', {
        type,
        error: error.message
      });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleConnect(
    message: Message,
    payload: {
      url: string;
      protocols?: string[];
      headers?: Record<string, string>;
    }
  ): Promise<MessageResponse> {
    // 1. Validate host
    const host = this.extractHost(payload.url);
    if (!this.allowedHosts.has(host)) {
      return createErrorResponse(message,
        `Host '${host}' not in allowedHosts: [${Array.from(this.allowedHosts).join(', ')}]`
      );
    }

    // 2. Check connection limit
    if (this.connections.size >= this.maxConnections) {
      return createErrorResponse(message,
        `Max connections exceeded: ${this.maxConnections}`
      );
    }

    // 3. Create WebSocket connection
    const connectionId = `ws-${Math.random().toString(36).substring(7)}`;
    const ws = new WebSocket(payload.url, payload.protocols);

    // 4. Create event port for this connection
    const port = createPortChannel<WSEvent>();
    this.eventPorts.set(connectionId, port);

    // 5. Setup event handlers
    ws.onopen = () => {
      port.send({ type: 'open', connectionId });
    };

    ws.onmessage = (event) => {
      let data: any;
      try {
        data = JSON.parse(event.data);
      } catch {
        data = event.data;
      }
      port.send({ type: 'message', connectionId, data });
    };

    ws.onerror = (event) => {
      port.send({ type: 'error', connectionId, error: 'WebSocket error' });
    };

    ws.onclose = (event) => {
      port.send({ type: 'close', connectionId, code: event.code, reason: event.reason });
      this.cleanup(connectionId);

      // Attempt reconnection if enabled
      if (this.reconnectConfig.enabled) {
        this.scheduleReconnect(connectionId, payload.url, 0);
      }
    };

    // 6. Store connection
    this.connections.set(connectionId, {
      id: connectionId,
      ws,
      url: payload.url,
      reconnectAttempts: 0
    });

    return createResponse(message, {
      connectionId,
      readyState: ws.readyState
    });
  }

  private async handleSend(
    message: Message,
    payload: {
      connectionId: string;
      data: any;
    }
  ): Promise<MessageResponse> {
    const connection = this.connections.get(payload.connectionId);

    if (!connection) {
      return createErrorResponse(message,
        `Connection not found: ${payload.connectionId}`
      );
    }

    if (connection.ws.readyState !== WebSocket.OPEN) {
      return createErrorResponse(message,
        `Connection not open: readyState=${connection.ws.readyState}`
      );
    }

    // Send message
    const data = typeof payload.data === 'string'
      ? payload.data
      : JSON.stringify(payload.data);

    connection.ws.send(data);

    return createResponse(message, { sent: true });
  }

  private async handleClose(
    message: Message,
    payload: {
      connectionId: string;
      code?: number;
      reason?: string;
    }
  ): Promise<MessageResponse> {
    const connection = this.connections.get(payload.connectionId);

    if (!connection) {
      return createErrorResponse(message,
        `Connection not found: ${payload.connectionId}`
      );
    }

    // Close WebSocket
    connection.ws.close(payload.code ?? 1000, payload.reason ?? 'Normal closure');

    // Cleanup
    this.cleanup(payload.connectionId);

    return createResponse(message, { closed: true });
  }

  private async handleSubscribe(
    message: Message,
    payload: {
      connectionId: string;
    }
  ): Promise<MessageResponse> {
    const port = this.eventPorts.get(payload.connectionId);

    if (!port) {
      return createErrorResponse(message,
        `No event port for connection: ${payload.connectionId}`
      );
    }

    // Return async iterator for port events
    return createResponse(message, {
      stream: port[Symbol.asyncIterator]()
    });
  }

  private scheduleReconnect(connectionId: string, url: string, attempt: number): void {
    if (attempt >= this.reconnectConfig.maxAttempts) {
      this.logError('Max reconnect attempts reached', { connectionId, attempt });
      return;
    }

    // Calculate backoff delay
    const delay = this.reconnectConfig.backoff === 'exponential'
      ? Math.pow(2, attempt) * 1000 // 1s, 2s, 4s, 8s
      : (attempt + 1) * 1000;        // 1s, 2s, 3s, 4s

    setTimeout(async () => {
      this.logInfo('Attempting reconnect', { connectionId, attempt, delay });

      // Recreate connection
      const message = {
        id: `reconnect-${connectionId}`,
        from: { type: 'actor' as const, id: this.id },
        to: { type: 'actor' as const, id: this.id },
        type: 'ws.connect',
        payload: { url },
        timestamp: Date.now()
      };

      const response = await this.handleConnect(message, { url });

      if (!response.success) {
        // Retry
        this.scheduleReconnect(connectionId, url, attempt + 1);
      }
    }, delay);
  }

  private cleanup(connectionId: string): void {
    this.connections.delete(connectionId);

    const port = this.eventPorts.get(connectionId);
    if (port) {
      port.close();
      this.eventPorts.delete(connectionId);
    }
  }

  private extractHost(url: string): string {
    try {
      const parsed = new URL(url);
      return parsed.hostname;
    } catch {
      throw new Error(`Invalid WebSocket URL: ${url}`);
    }
  }
}
