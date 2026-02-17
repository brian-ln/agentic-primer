#!/usr/bin/env bun
/**
 * SignalHubClient - WebSocket client for Signal Hub protocol
 *
 * Implements the formal Signal Hub protocol (docs/signal-hub/PROTOCOL.md):
 * - Connection lifecycle (hub:connect, hub:heartbeat, hub:disconnect)
 * - Actor registration (hub:register, hub:renew)
 * - Message routing (hub:send with flat payload)
 * - Reconnection with exponential backoff
 *
 * Integration with SEAG:
 * - Auto-registers local actors on connect
 * - Routes messages between local actors and Signal Hub
 * - Handles reconnection transparently
 *
 * @example
 * ```typescript
 * const client = new SignalHubClient({
 *   url: 'wss://signal-hub.example.com',
 *   jwt: 'eyJhbGci...',
 *   protocolVersion: '0.1.0',
 * });
 *
 * // Connect to Signal Hub
 * await client.connect();
 *
 * // Register an actor
 * await client.registerActor('@(local/my-actor)', ['compute', 'inference']);
 *
 * // Send message
 * await client.send({
 *   to: '@(browser/widget-123)',
 *   type: 'app:message',
 *   payload: { data: 'hello' },
 * });
 *
 * // Receive messages
 * client.on('message', (msg) => {
 *   console.log('Received:', msg);
 * });
 * ```
 */

import type {
  SharedMessage,
  CanonicalAddress,
} from '@agentic-primer/protocols';
import type {
  SignalHubConfig,
  SignalHubEvents,
  ActorRegistration,
  QueuedMessage,
  ConnectionState,
  HubConnectionInfo,
} from './types.ts';

// ---------------------------------------------------------------------------
// Default Configuration
// ---------------------------------------------------------------------------

const DEFAULT_CONFIG: Partial<SignalHubConfig> = {
  protocolVersion: '0.1.0',
  heartbeatInterval: 25000, // 25s (under Cloudflare's 30s hibernation)
  reconnect: {
    enabled: true,
    maxAttempts: 10,
    initialDelay: 1000,
    maxDelay: 30000,
    multiplier: 2,
  },
  messageQueue: {
    enabled: true,
    maxSize: 1000,
    defaultTtl: 60000, // 60s
  },
};

// ---------------------------------------------------------------------------
// SignalHubClient
// ---------------------------------------------------------------------------

export class SignalHubClient {
  private config: SignalHubConfig;
  private ws: WebSocket | null = null;
  private state: ConnectionState = 'disconnected';
  private sessionId: string | null = null;
  private connectionInfo: HubConnectionInfo | null = null;

  // Actor registry (local actors registered with Signal Hub)
  private actors = new Map<CanonicalAddress, ActorRegistration>();

  // Message queue (pending messages during disconnect)
  private messageQueue: QueuedMessage[] = [];

  // Reconnection state
  private reconnectAttempt = 0;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
  private userInitiatedDisconnect = false;

  // Heartbeat
  private heartbeatTimer: ReturnType<typeof setInterval> | null = null;

  // Event listeners
  private listeners = new Map<keyof SignalHubEvents, Set<Function>>();

  // Pending asks (waiting for response)
  private pendingAsks = new Map<
    string,
    {
      resolve: (response: SharedMessage) => void;
      reject: (error: Error) => void;
      timeout: ReturnType<typeof setTimeout>;
    }
  >();

  constructor(config: SignalHubConfig) {
    this.config = {
      ...DEFAULT_CONFIG,
      ...config,
      reconnect: { ...DEFAULT_CONFIG.reconnect, ...config.reconnect },
      messageQueue: { ...DEFAULT_CONFIG.messageQueue, ...config.messageQueue },
    } as SignalHubConfig;

    // Initialize event listener sets
    const eventTypes: (keyof SignalHubEvents)[] = [
      'connected',
      'disconnected',
      'reconnecting',
      'message',
      'error',
      'actorRegistered',
      'actorUnregistered',
    ];
    for (const type of eventTypes) {
      this.listeners.set(type, new Set());
    }
  }

  // ---------------------------------------------------------------------------
  // Public API: Connection Management
  // ---------------------------------------------------------------------------

  /**
   * Connect to Signal Hub with authentication and protocol negotiation
   */
  async connect(): Promise<void> {
    if (this.state === 'connected' || this.state === 'connecting') {
      return; // Already connected or connecting
    }

    this.userInitiatedDisconnect = false;
    this.state = 'connecting';
    this.reconnectAttempt = 0;

    try {
      await this.establishConnection();
    } catch (error) {
      this.state = 'error';
      this.emit('error', error instanceof Error ? error : new Error(String(error)));
      if (this.config.reconnect?.enabled) {
        this.scheduleReconnect();
      }
      throw error;
    }
  }

  /**
   * Disconnect from Signal Hub gracefully
   */
  async disconnect(): Promise<void> {
    this.userInitiatedDisconnect = true;
    this.clearReconnectTimer();
    this.stopHeartbeat();

    // Send hub:disconnect if connected
    if (this.ws && this.state === 'connected') {
      const disconnectMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: '@(local/signal-hub-client)' as CanonicalAddress,
        to: '@(cloudflare/signal-hub)' as CanonicalAddress,
        type: 'hub:disconnect',
        payload: null,
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        metadata: {},
        ttl: null,
        signature: null,
      };

      try {
        this.ws.send(JSON.stringify(disconnectMsg));
      } catch {
        // Ignore send errors during disconnect
      }
    }

    // Close WebSocket
    if (this.ws) {
      this.ws.close(1000, 'Client disconnect');
      this.ws = null;
    }

    this.state = 'disconnected';
    this.sessionId = null;
    this.connectionInfo = null;
    this.emit('disconnected', 'Client initiated disconnect');
  }

  // ---------------------------------------------------------------------------
  // Public API: Actor Registration
  // ---------------------------------------------------------------------------

  /**
   * Register a local actor with Signal Hub
   */
  async registerActor(
    actorAddress: CanonicalAddress,
    capabilities: string[],
    metadata: Record<string, unknown> = {}
  ): Promise<void> {
    if (this.state !== 'connected') {
      throw new Error('Cannot register actor: not connected to Signal Hub');
    }

    const registerMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: actorAddress,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:register',
      payload: {
        actorAddress,
        capabilities,
        metadata,
        ttlSeconds: 300, // 5 minutes (will renew with heartbeat)
      },
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      metadata: {},
      ttl: 5000, // 5s timeout for registration
      signature: null,
    };

    const response = await this.sendAndWait(registerMsg);

    if (response.type === 'hub:registered') {
      const registration: ActorRegistration = {
        actorAddress,
        capabilities,
        metadata,
        expiresAt: response.metadata.expiresAt as number,
        renewalToken: response.metadata.renewalToken as string,
        version: response.metadata.version as number,
      };
      this.actors.set(actorAddress, registration);
      this.emit('actorRegistered', actorAddress);
    } else if (response.type === 'hub:error') {
      throw new Error(`Actor registration failed: ${response.payload}`);
    } else {
      throw new Error(`Unexpected response to hub:register: ${response.type}`);
    }
  }

  /**
   * Unregister a local actor from Signal Hub
   */
  async unregisterActor(actorAddress: CanonicalAddress): Promise<void> {
    if (this.state !== 'connected') {
      throw new Error('Cannot unregister actor: not connected to Signal Hub');
    }

    const unregisterMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: actorAddress,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:unregister',
      payload: null,
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      metadata: {},
      ttl: null,
      signature: null,
    };

    this.sendMessage(unregisterMsg);
    this.actors.delete(actorAddress);
    this.emit('actorUnregistered', actorAddress);
  }

  // ---------------------------------------------------------------------------
  // Public API: Message Routing
  // ---------------------------------------------------------------------------

  /**
   * Send a message to an actor via Signal Hub (fire-and-forget)
   */
  send(params: {
    to: CanonicalAddress;
    type: string;
    payload: unknown;
    from?: CanonicalAddress;
    metadata?: Record<string, unknown>;
  }): void {
    const hubSendMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: params.from ?? ('@(local/signal-hub-client)' as CanonicalAddress),
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:send',
      payload: {
        to: params.to,
        type: params.type,
        data: params.payload,
      },
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      metadata: {
        via: '@(cloudflare/signal-hub)',
        ...(params.metadata ?? {}),
      },
      ttl: null,
      signature: null,
    };

    this.sendMessage(hubSendMsg);
  }

  /**
   * Send a message and wait for acknowledgment
   */
  async sendWithAck(params: {
    to: CanonicalAddress;
    type: string;
    payload: unknown;
    from?: CanonicalAddress;
    metadata?: Record<string, unknown>;
  }): Promise<SharedMessage> {
    const hubSendMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: params.from ?? ('@(local/signal-hub-client)' as CanonicalAddress),
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:send',
      payload: {
        to: params.to,
        type: params.type,
        data: params.payload,
      },
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      metadata: {
        via: '@(cloudflare/signal-hub)',
        ...(params.metadata ?? {}),
      },
      ttl: 10000, // 10s timeout
      signature: null,
    };

    return this.sendAndWait(hubSendMsg);
  }

  /**
   * Broadcast a message to all registered actors
   */
  async broadcast(
    from: CanonicalAddress,
    type: string,
    data: unknown,
    options?: { excludeSelf?: boolean; targetCapability?: string }
  ): Promise<void> {
    if (this.state !== 'connected') {
      throw new Error('Cannot broadcast: not connected to Signal Hub');
    }

    const broadcastMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:broadcast',
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        type,
        data,
        excludeSelf: options?.excludeSelf ?? false,
      },
      metadata: {
        targetCapability: options?.targetCapability,
      },
      ttl: null,
      signature: null,
    };

    this.sendMessage(broadcastMsg);
  }

  /**
   * Publish a message to topic subscribers
   */
  async publish(
    from: CanonicalAddress,
    topic: string,
    type: string,
    data: unknown
  ): Promise<void> {
    if (this.state !== 'connected') {
      throw new Error('Cannot publish: not connected to Signal Hub');
    }

    const publishMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:publish',
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        topic,
        type,
        data,
      },
      metadata: {},
      ttl: null,
      signature: null,
    };

    this.sendMessage(publishMsg);
  }

  /**
   * Subscribe to a topic for pub/sub messaging
   */
  async subscribe(
    from: CanonicalAddress,
    topic: string,
    durable: boolean = false
  ): Promise<string> {
    if (this.state !== 'connected') {
      throw new Error('Cannot subscribe: not connected to Signal Hub');
    }

    const subscribeMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:subscribe',
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        topic,
        durable,
      },
      metadata: {},
      ttl: 5000,
      signature: null,
    };

    const response = await this.sendAndWait(subscribeMsg);

    if (response.type === 'hub:subscribed') {
      const payload = response.payload as { subscriptionId: string };
      return payload.subscriptionId;
    } else if (response.type === 'hub:error') {
      throw new Error(`Subscription failed: ${response.payload}`);
    } else {
      throw new Error(`Unexpected response to hub:subscribe: ${response.type}`);
    }
  }

  /**
   * Unsubscribe from a topic
   */
  async unsubscribe(from: CanonicalAddress, subscriptionId: string): Promise<void> {
    if (this.state !== 'connected') {
      throw new Error('Cannot unsubscribe: not connected to Signal Hub');
    }

    const unsubscribeMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:unsubscribe',
      pattern: 'tell',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        subscriptionId,
      },
      metadata: {},
      ttl: null,
      signature: null,
    };

    this.sendMessage(unsubscribeMsg);
  }

  /**
   * Discover actors by glob pattern or capability
   */
  async discover(
    from: CanonicalAddress,
    pattern?: string,
    capability?: string,
    limit: number = 50
  ): Promise<Array<{ address: CanonicalAddress; capabilities: string[] }>> {
    if (this.state !== 'connected') {
      throw new Error('Cannot discover: not connected to Signal Hub');
    }

    const discoverMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:discover',
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      payload: {
        pattern: pattern || '*',
        capability,
        limit: Math.min(limit, 100),
      },
      metadata: {},
      ttl: 5000,
      signature: null,
    };

    const response = await this.sendAndWait(discoverMsg);

    if (response.type === 'hub:discovered') {
      const payload = response.payload as { actors: Array<{ address: CanonicalAddress; capabilities: string[] }> };
      return payload.actors;
    } else if (response.type === 'hub:error') {
      throw new Error(`Discovery failed: ${response.payload}`);
    } else {
      throw new Error(`Unexpected response to hub:discover: ${response.type}`);
    }
  }

  // ---------------------------------------------------------------------------
  // Public API: Event Listeners
  // ---------------------------------------------------------------------------

  on<K extends keyof SignalHubEvents>(
    event: K,
    handler: SignalHubEvents[K]
  ): void {
    this.listeners.get(event)?.add(handler);
  }

  off<K extends keyof SignalHubEvents>(
    event: K,
    handler: SignalHubEvents[K]
  ): void {
    this.listeners.get(event)?.delete(handler);
  }

  // ---------------------------------------------------------------------------
  // Public API: State Inspection
  // ---------------------------------------------------------------------------

  getState(): ConnectionState {
    return this.state;
  }

  getSessionId(): string | null {
    return this.sessionId;
  }

  getConnectionInfo(): HubConnectionInfo | null {
    return this.connectionInfo;
  }

  getRegisteredActors(): CanonicalAddress[] {
    return Array.from(this.actors.keys());
  }

  // ---------------------------------------------------------------------------
  // Internal: Connection Establishment
  // ---------------------------------------------------------------------------

  private async establishConnection(): Promise<void> {
    return new Promise((resolve, reject) => {
      const ws = new WebSocket(this.config.url);

      ws.onopen = () => {
        this.ws = ws;
        this.sendHubConnect().then(resolve).catch(reject);
      };

      ws.onerror = (event) => {
        reject(new Error(`WebSocket error: ${event}`));
      };

      ws.onclose = (event) => {
        this.handleClose(event.code, event.reason);
      };

      ws.onmessage = (event) => {
        this.handleMessage(event.data);
      };
    });
  }

  private async sendHubConnect(): Promise<void> {
    const connectMsg: SharedMessage = {
      id: crypto.randomUUID(),
      from: '@(local/signal-hub-client)' as CanonicalAddress,
      to: '@(cloudflare/signal-hub)' as CanonicalAddress,
      type: 'hub:connect',
      payload: null,
      pattern: 'ask',
      correlationId: null,
      timestamp: Date.now(),
      metadata: {
        protocolVersion: this.config.protocolVersion,
        authToken: this.config.jwt,
        capabilities: ['send', 'broadcast', 'register'],
        clientMetadata: {
          userAgent: 'SignalHubClient/0.1.0',
          platform: 'node',
        },
      },
      ttl: 5000, // 5s timeout
      signature: null,
    };

    const response = await this.sendAndWait(connectMsg);

    if (response.type === 'hub:connected') {
      const payload = response.payload as any;
      this.sessionId = payload.sessionId as string;
      this.connectionInfo = {
        sessionId: this.sessionId,
        serverVersion: payload.serverVersion as string,
        maxMessageSize: payload.maxMessageSize as number,
        heartbeatInterval: payload.heartbeatInterval as number,
        capabilities: payload.capabilities as HubConnectionInfo['capabilities'],
        actorIdentity: response.metadata.actorIdentity as CanonicalAddress | undefined,
      };

      // Set state to connected BEFORE starting heartbeat and emitting events
      this.state = 'connected';
      this.startHeartbeat();
      this.emit('connected', this.sessionId);
      this.flushMessageQueue();
    } else if (response.type === 'hub:error') {
      throw new Error(`Connection failed: ${response.payload}`);
    } else {
      throw new Error(`Unexpected response to hub:connect: ${response.type}`);
    }
  }

  // ---------------------------------------------------------------------------
  // Internal: Message Handling
  // ---------------------------------------------------------------------------

  private handleMessage(data: string): void {
    try {
      const msg = JSON.parse(data) as SharedMessage;

      // Handle responses to pending asks
      if (msg.correlationId && this.pendingAsks.has(msg.correlationId)) {
        const pending = this.pendingAsks.get(msg.correlationId)!;
        clearTimeout(pending.timeout);
        this.pendingAsks.delete(msg.correlationId);
        pending.resolve(msg);
        return;
      }

      // Handle incoming messages
      if (msg.type.startsWith('hub:')) {
        this.handleHubMessage(msg);
      } else {
        // Application message routed through Signal Hub
        this.emit('message', msg);
      }
    } catch (error) {
      this.emit('error', new Error(`Failed to parse message: ${error}`));
    }
  }

  private handleHubMessage(msg: SharedMessage): void {
    switch (msg.type) {
      case 'hub:heartbeat_ack':
        // Heartbeat acknowledged, do nothing
        break;

      case 'hub:error':
        this.emit('error', new Error(`Signal Hub error: ${msg.payload}`));
        break;

      case 'hub:unknown_actor':
        this.emit('error', new Error(`Unknown actor: ${msg.payload}`));
        break;

      case 'hub:unauthorized':
        this.emit('error', new Error(`Unauthorized: ${msg.payload}`));
        this.disconnect(); // Unauthorized = disconnect
        break;

      default:
        // Unknown hub message type, emit as regular message
        this.emit('message', msg);
    }
  }

  private handleClose(code: number, reason: string): void {
    this.ws = null;
    this.stopHeartbeat();

    const wasConnected = this.state === 'connected';
    this.state = 'disconnected';

    this.emit('disconnected', reason || `WebSocket closed (code ${code})`);

    // Attempt reconnection if enabled and not user-initiated
    // Note: Bun normalizes server-side closes to code 1000, so we can't rely on close code
    if (wasConnected && !this.userInitiatedDisconnect && this.config.reconnect?.enabled) {
      this.scheduleReconnect();
    }
  }

  // ---------------------------------------------------------------------------
  // Internal: Message Sending
  // ---------------------------------------------------------------------------

  private sendMessage(msg: SharedMessage): void {
    if (this.state !== 'connected' || !this.ws) {
      // Queue message if queuing enabled
      if (this.config.messageQueue?.enabled) {
        this.queueMessage(msg);
      } else {
        this.emit('error', new Error('Cannot send message: not connected'));
      }
      return;
    }

    try {
      this.ws.send(JSON.stringify(msg));
    } catch (error) {
      this.emit('error', new Error(`Failed to send message: ${error}`));
    }
  }

  private async sendAndWait(msg: SharedMessage): Promise<SharedMessage> {
    return new Promise((resolve, reject) => {
      // Allow sending during 'connecting' or 'reconnecting' state for hub:connect message
      if (!this.ws || (this.state !== 'connected' && this.state !== 'connecting' && this.state !== 'reconnecting')) {
        reject(new Error('Cannot send message: not connected'));
        return;
      }

      const timeout = setTimeout(() => {
        this.pendingAsks.delete(msg.id);
        reject(new Error(`Request timeout: ${msg.type}`));
      }, msg.ttl ?? 10000);

      this.pendingAsks.set(msg.id, { resolve, reject, timeout });

      try {
        this.ws.send(JSON.stringify(msg));
      } catch (error) {
        clearTimeout(timeout);
        this.pendingAsks.delete(msg.id);
        reject(new Error(`Failed to send message: ${error}`));
      }
    });
  }

  // ---------------------------------------------------------------------------
  // Internal: Message Queue
  // ---------------------------------------------------------------------------

  private queueMessage(msg: SharedMessage): void {
    const queue = this.config.messageQueue!;
    const now = Date.now();
    const ttl = msg.ttl ?? queue.defaultTtl;

    // Remove expired messages
    this.messageQueue = this.messageQueue.filter((q) => q.expiresAt > now);

    // Check queue size
    if (this.messageQueue.length >= queue.maxSize) {
      this.emit('error', new Error('Message queue full, dropping message'));
      return;
    }

    this.messageQueue.push({
      message: msg,
      queuedAt: now,
      expiresAt: now + ttl,
    });
  }

  private flushMessageQueue(): void {
    const now = Date.now();
    const toSend = this.messageQueue.filter((q) => q.expiresAt > now);

    this.messageQueue = [];

    for (const queued of toSend) {
      this.sendMessage(queued.message);
    }
  }

  // ---------------------------------------------------------------------------
  // Internal: Heartbeat
  // ---------------------------------------------------------------------------

  private startHeartbeat(): void {
    this.stopHeartbeat();

    const interval = this.connectionInfo?.heartbeatInterval ?? this.config.heartbeatInterval!;

    this.heartbeatTimer = setInterval(() => {
      if (this.state === 'connected' && this.ws) {
        const heartbeatMsg: SharedMessage = {
          id: crypto.randomUUID(),
          from: '@(local/signal-hub-client)' as CanonicalAddress,
          to: '@(cloudflare/signal-hub)' as CanonicalAddress,
          type: 'hub:heartbeat',
          payload: null,
          pattern: 'tell',
          correlationId: null,
          timestamp: Date.now(),
          metadata: {},
          ttl: null,
          signature: null,
        };

        this.sendMessage(heartbeatMsg);

        // Also renew actor registrations
        this.renewActorRegistrations();
      }
    }, interval);
  }

  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

  private async renewActorRegistrations(): Promise<void> {
    for (const [address, registration] of this.actors.entries()) {
      const renewMsg: SharedMessage = {
        id: crypto.randomUUID(),
        from: address,
        to: '@(cloudflare/signal-hub)' as CanonicalAddress,
        type: 'hub:renew',
        payload: null,
        pattern: 'tell',
        correlationId: null,
        timestamp: Date.now(),
        metadata: {
          renewalToken: registration.renewalToken,
        },
        ttl: null,
        signature: null,
      };

      this.sendMessage(renewMsg);
    }
  }

  // ---------------------------------------------------------------------------
  // Internal: Reconnection
  // ---------------------------------------------------------------------------

  private scheduleReconnect(): void {
    if (!this.config.reconnect?.enabled) {
      return;
    }

    if (this.reconnectAttempt >= this.config.reconnect.maxAttempts) {
      this.emit('error', new Error('Max reconnection attempts reached'));
      return;
    }

    this.clearReconnectTimer();
    this.state = 'reconnecting';
    this.reconnectAttempt++;

    const delay = Math.min(
      this.config.reconnect.initialDelay * Math.pow(this.config.reconnect.multiplier, this.reconnectAttempt - 1),
      this.config.reconnect.maxDelay
    );

    this.emit('reconnecting', this.reconnectAttempt);

    this.reconnectTimer = setTimeout(async () => {
      try {
        await this.establishConnection();
        // Re-register all actors after reconnection
        await this.reregisterActors();
      } catch (error) {
        this.emit('error', error instanceof Error ? error : new Error(String(error)));
        this.scheduleReconnect();
      }
    }, delay);
  }

  private clearReconnectTimer(): void {
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
  }

  private async reregisterActors(): Promise<void> {
    const actorsToRegister = Array.from(this.actors.entries());
    this.actors.clear(); // Clear old registrations

    for (const [address, registration] of actorsToRegister) {
      try {
        await this.registerActor(address, registration.capabilities, registration.metadata);
      } catch (error) {
        this.emit('error', new Error(`Failed to re-register actor ${address}: ${error}`));
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Internal: Event Emission
  // ---------------------------------------------------------------------------

  private emit<K extends keyof SignalHubEvents>(
    event: K,
    ...args: Parameters<SignalHubEvents[K]>
  ): void {
    const handlers = this.listeners.get(event);
    if (handlers) {
      for (const handler of handlers) {
        try {
          (handler as any)(...args);
        } catch (error) {
          console.error(`Error in ${event} handler:`, error);
        }
      }
    }
  }
}
