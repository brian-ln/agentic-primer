/**
 * SignalHubClient - Browser WebSocket client for Signal Hub
 *
 * Provides connection management, actor registration, and message routing
 * for browser-based actors communicating through Cloudflare Signal Hub.
 */

import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';
import type {
  SignalHubClientOptions,
  ActorRegistration,
  ConnectionState,
  HubMessageEvent,
  ErrorEvent,
  ConnectionEvent,
  MessageHandler,
  ConnectionHandler,
  ErrorHandler,
  StateChangeHandler,
  SendOptions,
  HubMessageType,
  HubError,
} from './types.js';
import {
  generateBrowserAddress,
  isValidAddress,
  calculateBackoff,
  createDeferred,
  wait,
  type Deferred,
} from './utils.js';

/**
 * SignalHubClient - Main client class
 */
export class SignalHubClient {
  private readonly url: string;
  private readonly jwt: string;
  private readonly autoReconnect: boolean;
  private readonly maxReconnectAttempts: number;
  private readonly reconnectDelay: number;
  private readonly maxReconnectDelay: number;
  private readonly heartbeatInterval: number;
  private readonly protocolVersion: string;

  private ws: WebSocket | null = null;
  private state: ConnectionState = 'disconnected';
  private sessionId: string | null = null;
  private actorIdentity: CanonicalAddress | null = null;
  private serverVersion: string | null = null;

  // Actor registry
  private registeredActors: Set<CanonicalAddress> = new Set();

  // Reconnection state
  private reconnectAttempts = 0;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
  private isManualDisconnect = false;

  // Heartbeat
  private heartbeatTimer: ReturnType<typeof setInterval> | null = null;
  private lastHeartbeatAck: number = 0;

  // Message queue (during disconnect)
  private messageQueue: SharedMessage[] = [];

  // Pending asks (awaiting ack)
  private pendingAsks = new Map<string, Deferred<string>>();

  // Event handlers
  private messageHandlers: MessageHandler[] = [];
  private connectedHandlers: ConnectionHandler[] = [];
  private disconnectedHandlers: ConnectionHandler[] = [];
  private errorHandlers: ErrorHandler[] = [];
  private stateChangeHandlers: StateChangeHandler[] = [];

  constructor(options: SignalHubClientOptions) {
    this.url = options.url;
    this.jwt = options.jwt;
    this.autoReconnect = options.autoReconnect ?? true;
    this.maxReconnectAttempts = options.maxReconnectAttempts ?? Infinity;
    this.reconnectDelay = options.reconnectDelay ?? 1000;
    this.maxReconnectDelay = options.maxReconnectDelay ?? 30000;
    this.heartbeatInterval = options.heartbeatInterval ?? 25000; // 25s (< 30s Cloudflare hibernation)
    this.protocolVersion = options.protocolVersion ?? '0.1.0';
  }

  // -------------------------------------------------------------------------
  // Public API - Connection
  // -------------------------------------------------------------------------

  /**
   * Connect to Signal Hub
   */
  async connect(): Promise<void> {
    if (this.state === 'connected' || this.state === 'connecting') {
      return;
    }

    this.isManualDisconnect = false;
    this.setState('connecting');

    return new Promise((resolve, reject) => {
      try {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
          this.onWebSocketOpen();
          // Don't resolve yet - wait for hub:connected
        };

        this.ws.onmessage = (event: MessageEvent<any>) => {
          this.onWebSocketMessage(event);
        };

        this.ws.onerror = (event) => {
          this.onWebSocketError(event);
          reject(new Error('WebSocket connection failed'));
        };

        this.ws.onclose = (event) => {
          this.onWebSocketClose(event);
        };

        // Set up one-time listener for hub:connected
        const connectedHandler = (event: ConnectionEvent) => {
          this.off('connected', connectedHandler);
          resolve();
        };
        this.on('connected', connectedHandler);

        // Timeout after 10s
        setTimeout(() => {
          if (this.state === 'connecting') {
            this.off('connected', connectedHandler);
            reject(new Error('Connection timeout'));
            this.disconnect();
          }
        }, 10000);
      } catch (error) {
        this.setState('disconnected');
        reject(error);
      }
    });
  }

  /**
   * Disconnect from Signal Hub
   */
  async disconnect(): Promise<void> {
    this.isManualDisconnect = true;

    // Clear reconnect timer
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }

    // Send hub:disconnect if connected
    if (this.state === 'connected' && this.ws?.readyState === WebSocket.OPEN) {
      try {
        await this.sendHubMessage('hub:disconnect', null);
      } catch (error) {
        // Ignore errors during disconnect
      }
    }

    this.setState('disconnecting');
    this.stopHeartbeat();

    if (this.ws) {
      this.ws.close(1000, 'Client disconnecting');
      this.ws = null;
    }

    this.setState('disconnected');
  }

  /**
   * Get current connection state
   */
  get connected(): boolean {
    return this.state === 'connected';
  }

  /**
   * Get connection state
   */
  get connectionState(): ConnectionState {
    return this.state;
  }

  /**
   * Get session ID (available after connection)
   */
  get session(): string | null {
    return this.sessionId;
  }

  // -------------------------------------------------------------------------
  // Public API - Actor Management
  // -------------------------------------------------------------------------

  /**
   * Register an actor with Signal Hub
   */
  async registerActor(
    addressOrOptions: CanonicalAddress | ActorRegistration
  ): Promise<CanonicalAddress> {
    if (!this.connected) {
      throw new Error('Not connected to Signal Hub');
    }

    let address: CanonicalAddress;
    let capabilities: string[];
    let metadata: Record<string, unknown>;

    if (typeof addressOrOptions === 'string') {
      address = addressOrOptions;
      capabilities = [];
      metadata = {};
    } else {
      address = addressOrOptions.address || generateBrowserAddress();
      capabilities = addressOrOptions.capabilities;
      metadata = addressOrOptions.metadata || {};
    }

    if (!isValidAddress(address)) {
      throw new Error(`Invalid actor address: ${address}`);
    }

    // Send hub:register
    const registerMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: address,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:register',
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        actorAddress: address,
        capabilities,
        metadata,
      },
      metadata: {},
      ttl: 5000,
      signature: null,
    };

    await this.sendMessage(registerMsg);

    // Wait for hub:registered
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        this.off('message', handler);
        reject(new Error('Actor registration timeout'));
      }, 5000);

      const handler = (event: HubMessageEvent) => {
        if (
          event.message.type === 'hub:registered' &&
          event.message.correlationId === registerMsg.id
        ) {
          clearTimeout(timeout);
          this.off('message', handler);
          this.registeredActors.add(address);
          resolve(address);
        } else if (
          event.message.type === 'hub:error' &&
          event.message.correlationId === registerMsg.id
        ) {
          clearTimeout(timeout);
          this.off('message', handler);
          const error = event.message.payload as HubError;
          reject(new Error(`Registration failed: ${error.message}`));
        }
      };

      this.on('message', handler);
    });
  }

  /**
   * Unregister an actor from Signal Hub
   */
  async unregisterActor(address: CanonicalAddress): Promise<void> {
    if (!this.connected) {
      throw new Error('Not connected to Signal Hub');
    }

    if (!this.registeredActors.has(address)) {
      return; // Already unregistered
    }

    const unregisterMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: address,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:unregister',
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        actorAddress: address,
      },
      metadata: {},
      ttl: null,
      signature: null,
    };

    await this.sendMessage(unregisterMsg);
    this.registeredActors.delete(address);
  }

  /**
   * Get list of registered actor addresses
   */
  get actors(): CanonicalAddress[] {
    return Array.from(this.registeredActors);
  }

  // -------------------------------------------------------------------------
  // Public API - Messaging
  // -------------------------------------------------------------------------

  /**
   * Send a message to another actor (fire-and-forget)
   */
  async send(
    from: CanonicalAddress,
    to: CanonicalAddress,
    type: string,
    data: unknown,
    options?: Partial<SendOptions>
  ): Promise<void> {
    if (!this.connected) {
      throw new Error('Not connected to Signal Hub');
    }

    const hubSendMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:send',
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        to,
        type,
        data,
      },
      metadata: {
        traceId: options?.traceId,
        priority: options?.priority ?? 1,
      },
      ttl: options?.ttl ?? 30000,
      signature: null,
    };

    await this.sendMessage(hubSendMsg);
  }

  /**
   * Send a message and wait for delivery acknowledgment
   */
  async sendWithAck(
    from: CanonicalAddress,
    to: CanonicalAddress,
    type: string,
    data: unknown,
    options?: Partial<SendOptions>
  ): Promise<string> {
    if (!this.connected) {
      throw new Error('Not connected to Signal Hub');
    }

    const messageId = crypto.randomUUID();
    const deferred = createDeferred<string>();

    this.pendingAsks.set(messageId, deferred);

    const hubSendMsg: SharedMessage = {
      id: messageId,
      from,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:send',
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        to,
        type,
        data,
      },
      metadata: {
        traceId: options?.traceId,
        priority: options?.priority ?? 1,
      },
      ttl: options?.ttl ?? 30000,
      signature: null,
    };

    await this.sendMessage(hubSendMsg);

    // Timeout after TTL + 5s
    const timeout = (options?.ttl ?? 30000) + 5000;
    setTimeout(() => {
      if (this.pendingAsks.has(messageId)) {
        this.pendingAsks.delete(messageId);
        deferred.reject(new Error('Message delivery timeout'));
      }
    }, timeout);

    return deferred.promise;
  }

  // -------------------------------------------------------------------------
  // Public API - Events
  // -------------------------------------------------------------------------

  /**
   * Register event handler
   */
  on(event: 'message', handler: MessageHandler): void;
  on(event: 'connected', handler: ConnectionHandler): void;
  on(event: 'disconnected', handler: ConnectionHandler): void;
  on(event: 'error', handler: ErrorHandler): void;
  on(event: 'stateChange', handler: StateChangeHandler): void;
  on(event: string, handler: any): void {
    switch (event) {
      case 'message':
        this.messageHandlers.push(handler);
        break;
      case 'connected':
        this.connectedHandlers.push(handler);
        break;
      case 'disconnected':
        this.disconnectedHandlers.push(handler);
        break;
      case 'error':
        this.errorHandlers.push(handler);
        break;
      case 'stateChange':
        this.stateChangeHandlers.push(handler);
        break;
    }
  }

  /**
   * Remove event handler
   */
  off(event: 'message', handler: MessageHandler): void;
  off(event: 'connected', handler: ConnectionHandler): void;
  off(event: 'disconnected', handler: ConnectionHandler): void;
  off(event: 'error', handler: ErrorHandler): void;
  off(event: 'stateChange', handler: StateChangeHandler): void;
  off(event: string, handler: any): void {
    switch (event) {
      case 'message':
        this.messageHandlers = this.messageHandlers.filter((h) => h !== handler);
        break;
      case 'connected':
        this.connectedHandlers = this.connectedHandlers.filter((h) => h !== handler);
        break;
      case 'disconnected':
        this.disconnectedHandlers = this.disconnectedHandlers.filter((h) => h !== handler);
        break;
      case 'error':
        this.errorHandlers = this.errorHandlers.filter((h) => h !== handler);
        break;
      case 'stateChange':
        this.stateChangeHandlers = this.stateChangeHandlers.filter((h) => h !== handler);
        break;
    }
  }

  // -------------------------------------------------------------------------
  // Private - WebSocket Event Handlers
  // -------------------------------------------------------------------------

  private onWebSocketOpen(): void {
    // Send hub:connect
    const connectMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: '@(browser/unknown)' as CanonicalAddress,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:connect',
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      payload: null,
      metadata: {
        protocolVersion: this.protocolVersion,
        authToken: this.jwt,
        capabilities: ['send', 'receive'],
        clientMetadata: {
          userAgent: navigator.userAgent,
          platform: navigator.platform,
        },
      },
      ttl: 5000,
      signature: null,
    };

    this.sendMessage(connectMsg).catch((error) => {
      this.emitError({ message: 'Failed to send hub:connect', error });
    });
  }

  private onWebSocketMessage(event: MessageEvent<any>): void {
    try {
      const message = JSON.parse(event.data as string) as SharedMessage;

      // Handle hub protocol messages
      if (message.type.startsWith('hub:')) {
        this.handleHubMessage(message);
      } else {
        // Application message - emit to handlers
        this.emitMessage({
          message,
          originalFrom: message.metadata?.originalFrom as CanonicalAddress,
          forwarded: !!message.metadata?.via,
        });
      }
    } catch (error) {
      this.emitError({
        message: 'Failed to parse WebSocket message',
        error: error instanceof Error ? error : new Error(String(error)),
      });
    }
  }

  private onWebSocketError(event: Event): void {
    this.emitError({
      message: 'WebSocket error',
      error: new Error('WebSocket error'),
    });
  }

  private onWebSocketClose(event: CloseEvent): void {
    this.stopHeartbeat();
    this.setState('disconnected');

    this.emitDisconnected({});

    // Auto-reconnect if enabled and not manual disconnect
    if (this.autoReconnect && !this.isManualDisconnect) {
      this.scheduleReconnect();
    }
  }

  // -------------------------------------------------------------------------
  // Private - Hub Message Handling
  // -------------------------------------------------------------------------

  private handleHubMessage(message: SharedMessage): void {
    const type = message.type as HubMessageType;

    switch (type) {
      case 'hub:connected':
        this.handleConnected(message);
        break;

      case 'hub:heartbeat_ack':
        this.lastHeartbeatAck = Date.now();
        break;

      case 'hub:delivery_ack':
        this.handleDeliveryAck(message);
        break;

      case 'hub:error':
        this.handleHubError(message);
        break;

      case 'hub:registered':
      case 'hub:unregistered':
        // Pass through to message handlers (for registration promises)
        this.emitMessage({ message });
        break;

      default:
        // Forward other messages to application
        this.emitMessage({ message });
    }
  }

  private handleConnected(message: SharedMessage): void {
    const payload = message.payload as any;

    this.sessionId = payload.sessionId;
    this.serverVersion = payload.serverVersion;
    this.actorIdentity = message.metadata?.actorIdentity as CanonicalAddress;

    this.setState('connected');
    this.reconnectAttempts = 0;

    // Start heartbeat
    this.startHeartbeat();

    // Flush message queue
    this.flushMessageQueue();

    this.emitConnected({
      sessionId: this.sessionId ?? undefined,
      serverVersion: this.serverVersion ?? undefined,
      actorIdentity: this.actorIdentity ?? undefined,
    });
  }

  private handleDeliveryAck(message: SharedMessage): void {
    const correlationId = message.correlationId;
    if (correlationId && this.pendingAsks.has(correlationId)) {
      const deferred = this.pendingAsks.get(correlationId)!;
      this.pendingAsks.delete(correlationId);
      deferred.resolve(correlationId);
    }
  }

  private handleHubError(message: SharedMessage): void {
    const error = message.payload as HubError;
    const correlationId = message.correlationId;

    // Reject pending ask if applicable
    if (correlationId && this.pendingAsks.has(correlationId)) {
      const deferred = this.pendingAsks.get(correlationId)!;
      this.pendingAsks.delete(correlationId);
      deferred.reject(new Error(error.message));
    }

    // Emit error event
    this.emitError({
      message: error.message,
      code: error.code,
    });
  }

  // -------------------------------------------------------------------------
  // Private - Heartbeat
  // -------------------------------------------------------------------------

  private startHeartbeat(): void {
    this.stopHeartbeat();

    this.heartbeatTimer = setInterval(() => {
      if (this.connected && this.ws?.readyState === WebSocket.OPEN) {
        const heartbeatMsg: SharedMessage = {
          id: crypto.randomUUID(),
          from: this.actorIdentity || ('@(browser/unknown)' as CanonicalAddress),
          to: '@(cloudflare/signal-hub)' as CanonicalAddress,
          type: 'hub:heartbeat',
          pattern: 'tell',
          correlationId: null,
          timestamp: Date.now(),
          payload: {
            timestamp: Date.now(),
          },
          metadata: {},
          ttl: null,
          signature: null,
        };

        this.sendMessage(heartbeatMsg).catch((error) => {
          this.emitError({ message: 'Heartbeat failed', error });
        });

        // Check if last heartbeat ack was received
        if (this.lastHeartbeatAck > 0 && Date.now() - this.lastHeartbeatAck > 10000) {
          // No ack for 10s - consider connection dead
          this.emitError({
            message: 'Heartbeat timeout - connection may be dead',
          });
          this.disconnect();
        }
      }
    }, this.heartbeatInterval);

    this.lastHeartbeatAck = Date.now();
  }

  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

  // -------------------------------------------------------------------------
  // Private - Reconnection
  // -------------------------------------------------------------------------

  private scheduleReconnect(): void {
    if (this.reconnectAttempts >= this.maxReconnectAttempts) {
      this.emitError({
        message: 'Max reconnection attempts reached',
        code: 'max_reconnect_attempts',
      });
      return;
    }

    const delay = calculateBackoff(
      this.reconnectAttempts,
      this.reconnectDelay,
      this.maxReconnectDelay
    );

    this.setState('reconnecting');
    this.reconnectAttempts++;

    this.reconnectTimer = setTimeout(() => {
      this.connect().catch((error) => {
        this.emitError({
          message: 'Reconnection failed',
          error: error instanceof Error ? error : new Error(String(error)),
        });
      });
    }, delay);
  }

  // -------------------------------------------------------------------------
  // Private - Message Queue
  // -------------------------------------------------------------------------

  private async sendMessage(message: SharedMessage): Promise<void> {
    if (this.ws?.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    } else {
      // Queue for later
      this.messageQueue.push(message);
    }
  }

  private flushMessageQueue(): void {
    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift()!;
      this.sendMessage(message);
    }
  }

  // -------------------------------------------------------------------------
  // Private - State Management
  // -------------------------------------------------------------------------

  private setState(state: ConnectionState): void {
    if (this.state !== state) {
      this.state = state;
      this.emitStateChange(state);
    }
  }

  // -------------------------------------------------------------------------
  // Private - Event Emission
  // -------------------------------------------------------------------------

  private emitMessage(event: HubMessageEvent): void {
    this.messageHandlers.forEach((handler) => {
      try {
        handler(event);
      } catch (error) {
        console.error('Error in message handler:', error);
      }
    });
  }

  private emitConnected(event: ConnectionEvent): void {
    this.connectedHandlers.forEach((handler) => {
      try {
        handler(event);
      } catch (error) {
        console.error('Error in connected handler:', error);
      }
    });
  }

  private emitDisconnected(event: ConnectionEvent): void {
    this.disconnectedHandlers.forEach((handler) => {
      try {
        handler(event);
      } catch (error) {
        console.error('Error in disconnected handler:', error);
      }
    });
  }

  private emitError(event: ErrorEvent): void {
    this.errorHandlers.forEach((handler) => {
      try {
        handler(event);
      } catch (error) {
        console.error('Error in error handler:', error);
      }
    });
  }

  private emitStateChange(state: ConnectionState): void {
    this.stateChangeHandlers.forEach((handler) => {
      try {
        handler(state);
      } catch (error) {
        console.error('Error in state change handler:', error);
      }
    });
  }

  private async sendHubMessage(type: HubMessageType, payload: any): Promise<void> {
    const message: SharedMessage = {
      id: crypto.randomUUID(),
      from: this.actorIdentity || ('@(browser/unknown)' as CanonicalAddress),
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type,
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      payload,
      metadata: {},
      ttl: null,
      signature: null,
    };

    await this.sendMessage(message);
  }
}
