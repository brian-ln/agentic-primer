/**
 * ChannelActor - Standard Interface for Messaging Platform Adapters
 *
 * Defines a uniform interface for connecting to external messaging platforms
 * (WhatsApp, Telegram, Discord, Slack, etc.) using the actor model with supervision.
 *
 * Based on OpenClaw channel architecture adapted for UGS actor system.
 */

import type { Actor } from '../actor';
import type { Message, MessageResponse } from '../message';
import type { SupervisedActor } from '../supervision/types';

/**
 * Channel Connection Status
 *
 * Represents the current connection state of a messaging channel.
 */
export type ChannelConnectionStatus =
  | 'disconnected'    // Not connected, initial state
  | 'connecting'      // Connection in progress
  | 'authenticating'  // Connected, awaiting authentication
  | 'connected'       // Connected and ready to send/receive
  | 'reconnecting'    // Attempting to reconnect after disconnect
  | 'error';          // Error state, requires intervention

/**
 * Channel Status
 *
 * Detailed status information about a channel's connection and health.
 */
export interface ChannelStatus {
  /**
   * Current connection status.
   */
  status: ChannelConnectionStatus;

  /**
   * Whether channel is connected and ready.
   */
  connected: boolean;

  /**
   * Whether channel is authenticated.
   */
  authenticated: boolean;

  /**
   * QR code for device pairing (WhatsApp, etc.).
   * Base64-encoded PNG or URL.
   */
  qrCode?: string;

  /**
   * Last error that occurred.
   */
  error?: Error;

  /**
   * Timestamp of last successful connection.
   */
  lastConnected?: number;

  /**
   * Timestamp of last message sent.
   */
  lastSent?: number;

  /**
   * Timestamp of last message received.
   */
  lastReceived?: number;

  /**
   * Number of messages sent in current session.
   */
  messagesSent: number;

  /**
   * Number of messages received in current session.
   */
  messagesReceived: number;

  /**
   * Number of reconnection attempts.
   */
  reconnectAttempts: number;

  /**
   * Platform-specific metadata.
   * Example: WhatsApp may include phone number, device info
   */
  metadata?: Record<string, any>;
}

/**
 * Channel Configuration
 *
 * Configuration required to initialize a channel.
 */
export interface ChannelConfig {
  /**
   * Unique channel ID (e.g., 'whatsapp', 'telegram-bot-123').
   */
  id: string;

  /**
   * Display name for the channel.
   */
  name: string;

  /**
   * Channel type/platform.
   */
  platform: 'whatsapp' | 'telegram' | 'discord' | 'slack' | 'msteams' | string;

  /**
   * Whether to auto-connect on startup.
   *
   * @default true
   */
  autoConnect?: boolean;

  /**
   * Whether to auto-reconnect on disconnect.
   *
   * @default true
   */
  autoReconnect?: boolean;

  /**
   * Reconnection backoff configuration.
   */
  reconnect?: {
    /**
     * Initial delay before first reconnect (ms).
     *
     * @default 1000
     */
    initialDelay?: number;

    /**
     * Maximum delay between reconnects (ms).
     *
     * @default 30000
     */
    maxDelay?: number;

    /**
     * Backoff multiplier.
     *
     * @default 2
     */
    multiplier?: number;

    /**
     * Maximum reconnect attempts before giving up.
     *
     * @default Infinity
     */
    maxAttempts?: number;
  };

  /**
   * Platform-specific configuration.
   * Example: WhatsApp requires session path, Telegram requires bot token
   */
  platformConfig: Record<string, any>;
}

/**
 * Channel Message Recipient
 *
 * Identifies the recipient of a message on the platform.
 */
export interface ChannelRecipient {
  /**
   * Platform-specific user/chat ID.
   */
  id: string;

  /**
   * Recipient type.
   */
  type: 'user' | 'group' | 'channel';

  /**
   * Display name (optional).
   */
  name?: string;

  /**
   * Platform-specific metadata.
   */
  metadata?: Record<string, any>;
}

/**
 * Channel Message Content
 *
 * Represents content that can be sent via a channel.
 */
export type ChannelContent =
  | { type: 'text'; text: string }
  | { type: 'image'; url: string; caption?: string }
  | { type: 'file'; url: string; filename?: string; mimeType?: string }
  | { type: 'audio'; url: string; duration?: number }
  | { type: 'video'; url: string; duration?: number; thumbnail?: string }
  | { type: 'location'; latitude: number; longitude: number; name?: string }
  | { type: 'contact'; name: string; phone?: string; email?: string };

/**
 * Inbound Message from Channel
 *
 * Normalized message received from external platform.
 */
export interface InboundChannelMessage {
  /**
   * Platform-specific message ID.
   */
  id: string;

  /**
   * Sender information.
   */
  from: ChannelRecipient;

  /**
   * Recipient information (for group messages).
   */
  to?: ChannelRecipient;

  /**
   * Message content.
   */
  content: ChannelContent[];

  /**
   * Timestamp from platform.
   */
  timestamp: number;

  /**
   * Whether this is a reply to another message.
   */
  replyTo?: string;

  /**
   * Platform-specific metadata.
   */
  metadata?: Record<string, any>;
}

/**
 * Outbound Message to Channel
 *
 * Message to be sent to external platform.
 */
export interface OutboundChannelMessage {
  /**
   * Recipient.
   */
  to: ChannelRecipient;

  /**
   * Message content.
   */
  content: ChannelContent[];

  /**
   * Reply to message ID (optional).
   */
  replyTo?: string;

  /**
   * Platform-specific options.
   */
  options?: Record<string, any>;
}

/**
 * ChannelActor Interface
 *
 * Standard interface for all messaging channel actors.
 * Channels implement this interface to integrate with the actor system.
 */
export interface ChannelActor extends SupervisedActor {
  /**
   * Channel configuration.
   */
  readonly config: ChannelConfig;

  /**
   * Receive actor messages with standard protocol.
   *
   * Supported message types:
   * - 'connect': Connect to platform
   * - 'disconnect': Disconnect from platform
   * - 'reconnect': Force reconnection
   * - 'send': Send message to platform
   * - 'status': Get channel status
   *
   * @param message - Actor message
   * @returns Message response
   */
  receive(message: Message): Promise<MessageResponse>;

  /**
   * Get current channel status.
   *
   * @returns Current status
   */
  getStatus(): ChannelStatus;

  /**
   * Send message to platform.
   *
   * This is a convenience method that wraps the actor 'send' message.
   *
   * @param message - Outbound message
   * @returns Platform message ID
   *
   * @throws Error if not connected or send fails
   */
  send(message: OutboundChannelMessage): Promise<string>;

  /**
   * Connect to messaging platform.
   *
   * This is a convenience method that wraps the actor 'connect' message.
   *
   * @throws Error if connection fails
   */
  connect(): Promise<void>;

  /**
   * Disconnect from messaging platform.
   *
   * This is a convenience method that wraps the actor 'disconnect' message.
   */
  disconnect(): Promise<void>;

  /**
   * Force reconnection to messaging platform.
   *
   * This is a convenience method that wraps the actor 'reconnect' message.
   *
   * @throws Error if reconnection fails
   */
  reconnect(): Promise<void>;
}

/**
 * Message Handler for Inbound Messages
 *
 * Callback invoked when channel receives messages from platform.
 */
export type InboundMessageHandler = (message: InboundChannelMessage) => Promise<void> | void;

/**
 * Base ChannelActor Implementation
 *
 * Provides common functionality for channel actors.
 * Subclasses implement platform-specific logic.
 */
export abstract class BaseChannelActor implements ChannelActor {
  readonly config: ChannelConfig;
  protected _status: ChannelStatus;
  protected reconnectTimer?: NodeJS.Timeout;
  protected messageHandler?: InboundMessageHandler;

  constructor(config: ChannelConfig) {
    this.config = config;
    this._status = {
      status: 'disconnected',
      connected: false,
      authenticated: false,
      messagesSent: 0,
      messagesReceived: 0,
      reconnectAttempts: 0,
    };
  }

  /**
   * Actor interface - must be implemented by subclass
   */
  abstract get address(): any;
  abstract tell(to: any, type: string, payload: any): Promise<void>;
  abstract ask<T = any>(to: any, type: string, payload: any): Promise<any>;

  /**
   * Standard actor message handler
   */
  async receive(message: Message): Promise<MessageResponse> {
    switch (message.type) {
      case 'connect':
        await this.connect();
        return {
          id: `${message.id}-response`,
          correlationId: message.correlationId,
          from: this.address,
          success: true,
          payload: { status: this._status.status },
          timestamp: Date.now(),
        };

      case 'disconnect':
        await this.disconnect();
        return {
          id: `${message.id}-response`,
          correlationId: message.correlationId,
          from: this.address,
          success: true,
          payload: { status: 'disconnected' },
          timestamp: Date.now(),
        };

      case 'reconnect':
        await this.reconnect();
        return {
          id: `${message.id}-response`,
          correlationId: message.correlationId,
          from: this.address,
          success: true,
          payload: { status: this._status.status },
          timestamp: Date.now(),
        };

      case 'send':
        const messageId = await this.send(message.payload);
        return {
          id: `${message.id}-response`,
          correlationId: message.correlationId,
          from: this.address,
          success: true,
          payload: { messageId },
          timestamp: Date.now(),
        };

      case 'status':
        return {
          id: `${message.id}-response`,
          correlationId: message.correlationId,
          from: this.address,
          success: true,
          payload: this.getStatus(),
          timestamp: Date.now(),
        };

      default:
        return {
          id: `${message.id}-response`,
          correlationId: message.correlationId,
          from: this.address,
          success: false,
          error: `Unknown message type: ${message.type}`,
          timestamp: Date.now(),
        };
    }
  }

  /**
   * Get current status
   */
  getStatus(): ChannelStatus {
    return { ...this._status };
  }

  /**
   * Register handler for inbound messages
   */
  onMessage(handler: InboundMessageHandler): void {
    this.messageHandler = handler;
  }

  /**
   * Send message - must be implemented by platform
   */
  abstract send(message: OutboundChannelMessage): Promise<string>;

  /**
   * Connect - must be implemented by platform
   */
  abstract connect(): Promise<void>;

  /**
   * Disconnect - must be implemented by platform
   */
  abstract disconnect(): Promise<void>;

  /**
   * Reconnect with exponential backoff
   */
  async reconnect(): Promise<void> {
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = undefined;
    }

    this._status.status = 'reconnecting';
    this._status.reconnectAttempts++;

    try {
      await this.disconnect();
      await this.connect();
      this._status.reconnectAttempts = 0; // Reset on success
    } catch (error: any) {
      this._status.error = error;

      // Schedule next reconnect if enabled
      if (this.config.autoReconnect) {
        const reconnectConfig = this.config.reconnect || {};
        const initialDelay = reconnectConfig.initialDelay || 1000;
        const maxDelay = reconnectConfig.maxDelay || 30000;
        const multiplier = reconnectConfig.multiplier || 2;
        const maxAttempts = reconnectConfig.maxAttempts || Infinity;

        if (this._status.reconnectAttempts < maxAttempts) {
          const delay = Math.min(
            initialDelay * Math.pow(multiplier, this._status.reconnectAttempts - 1),
            maxDelay
          );

          this.reconnectTimer = setTimeout(() => {
            this.reconnect();
          }, delay);
        }
      }

      throw error;
    }
  }

  /**
   * Handle inbound message from platform
   */
  protected async handleInboundMessage(message: InboundChannelMessage): Promise<void> {
    this._status.messagesReceived++;
    this._status.lastReceived = Date.now();

    if (this.messageHandler) {
      await this.messageHandler(message);
    }
  }

  /**
   * Update status
   */
  protected updateStatus(updates: Partial<ChannelStatus>): void {
    this._status = { ...this._status, ...updates };
  }

  /**
   * Supervision lifecycle hook - save state before restart
   */
  async preRestart?(error: Error, message?: Message): Promise<any> {
    // Default implementation: disconnect gracefully
    try {
      await this.disconnect();
    } catch (e) {
      // Ignore disconnect errors during restart
    }

    // Return checkpoint data
    return {
      config: this.config,
      status: this._status,
      reconnectAttempts: this._status.reconnectAttempts,
    };
  }

  /**
   * Supervision lifecycle hook - restore state after restart
   */
  async postRestart?(checkpoint?: any): Promise<void> {
    // Default implementation: reconnect if previously connected
    if (checkpoint?.status?.connected) {
      await this.connect();
    }
  }

  /**
   * Supervision health check
   */
  async healthCheck?(): Promise<boolean> {
    // Default implementation: check connection status
    return this._status.connected;
  }
}

/**
 * Channel Message Router
 *
 * Routes inbound messages from channels to sessions/agents.
 */
export interface ChannelMessageRouter {
  /**
   * Route inbound message to appropriate session/agent.
   *
   * @param channelId - Source channel ID
   * @param message - Inbound message
   */
  route(channelId: string, message: InboundChannelMessage): Promise<void>;
}

/**
 * Channel Registry
 *
 * Manages registered channels in the system.
 */
export interface ChannelRegistry {
  /**
   * Register a channel.
   *
   * @param channel - Channel actor to register
   */
  register(channel: ChannelActor): void;

  /**
   * Unregister a channel.
   *
   * @param channelId - Channel ID to unregister
   */
  unregister(channelId: string): void;

  /**
   * Get channel by ID.
   *
   * @param channelId - Channel ID
   * @returns Channel actor or undefined
   */
  get(channelId: string): ChannelActor | undefined;

  /**
   * Get all registered channels.
   *
   * @returns Array of channel actors
   */
  getAll(): ChannelActor[];

  /**
   * Get channels by platform.
   *
   * @param platform - Platform type
   * @returns Array of channel actors for platform
   */
  getByPlatform(platform: string): ChannelActor[];
}
