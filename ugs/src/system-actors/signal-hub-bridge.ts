#!/usr/bin/env bun
/**
 * SignalHubBridgeActor - Bridge between simplify actors and brianln.ai Signal Hub
 *
 * Outbound: simplify Message -> HTTP POST to signal-hub webhook
 * Inbound:  Brain DO WebSocket notifications -> simplify Message
 *
 * Uses SharedMessage as the canonical wire format at the bridge boundary.
 *
 * Registers at: @(bridges/signal-hub)
 *
 * Message types:
 *   'emit-signal'   - Send a signal to the hub (outbound)
 *   'subscribe'     - Register to receive notifications (inbound)
 *   'unsubscribe'   - Stop receiving notifications
 *   'status'        - Get bridge connection status
 *   'connect'       - Explicitly connect WebSocket
 *   'disconnect'    - Explicitly disconnect WebSocket
 */

import { Actor, createResponse, createErrorResponse, createMessage, address } from '@agentic-primer/actors';
import type { Message, MessageResponse, Address, IMessageRouter } from '@agentic-primer/actors';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface SignalHubBridgeConfig {
  /** Signal Hub webhook URL for outbound signals */
  webhookUrl: string;

  /** Brain DO WebSocket URL for inbound notifications */
  wsUrl: string;

  /** Auth token for webhook and WebSocket */
  authToken?: string;

  /** Auto-connect WebSocket on actor start */
  autoConnect: boolean;

  /** Reconnect on disconnect */
  reconnect: {
    enabled: boolean;
    maxAttempts: number;
    baseDelayMs: number;
  };
}

// ---------------------------------------------------------------------------
// Signal types (matches brianln.ai/services/signal-hub/src/types.ts)
// ---------------------------------------------------------------------------

interface OutboundSignal {
  source: string;
  type: string;
  sender: string;
  subject: string;
  body: string;
  metadata?: Record<string, unknown>;
}

interface InboundNotification {
  type: 'new_signal' | 'briefing_ready';
  signal?: {
    id: string;
    source: string;
    sender: string;
    subject: string;
    urgency: string;
    category: string | null;
    timestamp: string;
  };
  date?: string;
}

// ---------------------------------------------------------------------------
// Bridge Actor
// ---------------------------------------------------------------------------

export class SignalHubBridgeActor extends Actor {
  private config: SignalHubBridgeConfig;
  private ws: WebSocket | null = null;
  private subscribers = new Set<Address>();
  private reconnectAttempts = 0;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
  private connected = false;

  constructor(router: IMessageRouter, config: SignalHubBridgeConfig) {
    super('bridges/signal-hub', router);
    this.config = config;
  }

  /**
   * Start the bridge. Call after registering with router.
   */
  async start(): Promise<void> {
    if (this.config.autoConnect) {
      await this.connectWebSocket();
    }
  }

  /**
   * Stop the bridge. Clean up connections.
   */
  async stop(): Promise<void> {
    this.clearReconnectTimer();
    this.disconnectWebSocket();
    this.subscribers.clear();
  }

  async receive(message: Message): Promise<MessageResponse> {
    switch (message.type) {
      case 'emit-signal':
        return this.handleEmitSignal(message);

      case 'subscribe':
        return this.handleSubscribe(message);

      case 'unsubscribe':
        return this.handleUnsubscribe(message);

      case 'status':
        return this.handleStatus(message);

      case 'connect':
        await this.connectWebSocket();
        return createResponse(message, { connected: this.connected });

      case 'disconnect':
        this.disconnectWebSocket();
        return createResponse(message, { connected: false });

      default:
        return createErrorResponse(message, `Unknown message type: ${message.type}`);
    }
  }

  // =========================================================================
  // Outbound: simplify -> Signal Hub (HTTP POST)
  // =========================================================================

  private async handleEmitSignal(message: Message): Promise<MessageResponse> {
    const signal = message.payload as OutboundSignal;

    if (!signal.source || !signal.subject) {
      return createErrorResponse(message, 'Signal must have source and subject');
    }

    const webhookPayload = {
      source: signal.source,
      type: signal.type || 'notification',
      sender: signal.sender || 'simplify',
      subject: signal.subject,
      body: signal.body || '',
      metadata: signal.metadata || {},
    };

    try {
      const headers: Record<string, string> = {
        'Content-Type': 'application/json',
      };
      if (this.config.authToken) {
        headers['Authorization'] = `Bearer ${this.config.authToken}`;
      }

      const response = await fetch(this.config.webhookUrl, {
        method: 'POST',
        headers,
        body: JSON.stringify(webhookPayload),
      });

      if (!response.ok) {
        const body = await response.text();
        return createErrorResponse(
          message,
          `Webhook POST failed: ${response.status} ${body}`,
        );
      }

      const result = await response.json() as { signalId?: string };
      return createResponse(message, {
        sent: true,
        signalId: result.signalId,
      });
    } catch (error) {
      return createErrorResponse(
        message,
        `Webhook POST error: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  // =========================================================================
  // Inbound: Signal Hub -> simplify (WebSocket)
  // =========================================================================

  private async connectWebSocket(): Promise<void> {
    if (this.ws) {
      return; // Already connected
    }

    try {
      const url = new URL(this.config.wsUrl);
      if (this.config.authToken) {
        url.searchParams.set('token', this.config.authToken);
      }

      this.ws = new WebSocket(url.toString());

      this.ws.addEventListener('open', () => {
        this.connected = true;
        this.reconnectAttempts = 0;
      });

      this.ws.addEventListener('message', (event) => {
        this.handleInboundMessage(event.data as string);
      });

      this.ws.addEventListener('close', (event) => {
        this.connected = false;
        this.ws = null;
        if (this.config.reconnect.enabled) {
          this.scheduleReconnect();
        }
      });

      this.ws.addEventListener('error', () => {
        // Error event is always followed by close event
      });
    } catch (error) {
      this.ws = null;
      this.connected = false;
      if (this.config.reconnect.enabled) {
        this.scheduleReconnect();
      }
    }
  }

  private disconnectWebSocket(): void {
    this.clearReconnectTimer();
    if (this.ws) {
      this.ws.close(1000, 'Bridge disconnecting');
      this.ws = null;
    }
    this.connected = false;
  }

  private handleInboundMessage(data: string): void {
    try {
      const notification = JSON.parse(data) as InboundNotification;
      this.broadcastToSubscribers(notification);
    } catch {
      // Ignore malformed messages
    }
  }

  private broadcastToSubscribers(notification: InboundNotification): void {
    for (const subscriber of this.subscribers) {
      const msg = createMessage(subscriber, 'signal-hub-notification', notification, {
        from: this.address,
      });
      this.router.tell(msg);
    }
  }

  // =========================================================================
  // Subscription management
  // =========================================================================

  private handleSubscribe(message: Message): MessageResponse {
    const subscriber = message.from;
    if (!subscriber) {
      return createErrorResponse(message, 'Subscribe requires a from address');
    }
    this.subscribers.add(subscriber);
    return createResponse(message, {
      subscribed: true,
      subscriberCount: this.subscribers.size,
    });
  }

  private handleUnsubscribe(message: Message): MessageResponse {
    const subscriber = message.from;
    if (!subscriber) {
      return createErrorResponse(message, 'Unsubscribe requires a from address');
    }
    this.subscribers.delete(subscriber);
    return createResponse(message, {
      subscribed: false,
      subscriberCount: this.subscribers.size,
    });
  }

  // =========================================================================
  // Status
  // =========================================================================

  private handleStatus(message: Message): MessageResponse {
    return createResponse(message, {
      connected: this.connected,
      subscriberCount: this.subscribers.size,
      webhookUrl: this.config.webhookUrl,
      wsUrl: this.config.wsUrl,
      reconnectAttempts: this.reconnectAttempts,
    });
  }

  // =========================================================================
  // Reconnection
  // =========================================================================

  private scheduleReconnect(): void {
    if (this.reconnectAttempts >= this.config.reconnect.maxAttempts) {
      return;
    }

    this.reconnectAttempts++;
    const delay = this.config.reconnect.baseDelayMs * Math.pow(2, this.reconnectAttempts - 1);
    const jitter = delay * 0.25 * (Math.random() - 0.5);

    this.reconnectTimer = setTimeout(() => {
      this.connectWebSocket();
    }, delay + jitter);
  }

  private clearReconnectTimer(): void {
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
  }
}
