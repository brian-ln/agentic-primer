/**
 * SignalHub Durable Object
 *
 * Main WebSocket server implementing Signal Hub protocol
 */

import type {
  Env,
  SharedMessage,
  Session,
  ActorRegistration,
  CanonicalAddress,
  QueueStats,
} from '../types';
import { HubError } from '../types';
import {
  generateSessionId,
  toCanonicalAddress,
  validateSharedMessage,
  createErrorMessage,
  createTokenBucket,
  consumeToken,
} from '../utils';
import { getValidator } from '../validation/schema-validator';

// Handlers
import { handleConnect, handleHeartbeat, handleDisconnect } from '../handlers/connection';
import {
  handleRegister,
  handleUnregister,
  handleDiscover,
  handleListActors,
  handleRenew,
} from '../handlers/registration';
import { handleSend, handleBroadcast } from '../handlers/messaging';
import {
  handleSubscribe,
  handlePublish,
  handleUnsubscribe,
  cleanupSubscriptions,
} from '../handlers/pubsub';
import { handleQueueStats } from '../handlers/flowcontrol';

const SIGNAL_HUB_ADDRESS = toCanonicalAddress('cloudflare/signal-hub');

/**
 * SignalHub Durable Object Class
 *
 * Implements hibernatable WebSocket server with:
 * - Connection lifecycle management
 * - Actor registration and discovery
 * - Point-to-point and broadcast messaging
 * - Pub/sub topic subscriptions
 * - Backpressure and flow control
 */
export class SignalHub implements DurableObject {
  private ctx: DurableObjectState;
  private env: Env;
  private sessions: Map<WebSocket, Session>;
  private connections: Map<string, WebSocket>;
  private registry: Map<string, ActorRegistration>;
  private subscriptions: Map<string, Set<CanonicalAddress>>;
  private queueStats: QueueStats;

  constructor(state: DurableObjectState, env: Env) {
    this.ctx = state;
    this.env = env;
    this.sessions = new Map();
    this.connections = new Map();
    this.registry = new Map();
    this.subscriptions = new Map();
    this.queueStats = {
      pending: 0,
      processed: 0,
      failed: 0,
      paused: false,
    };

    // Enable hibernation for WebSocket connections
    state.setWebSocketAutoResponse(
      new WebSocketRequestResponsePair(
        JSON.stringify({ type: 'ping' }),
        JSON.stringify({ type: 'pong' })
      )
    );

    // Log validation mode on startup
    const validator = getValidator();
    const mode = validator.getMode();
    console.log(JSON.stringify({
      event: 'signalhub_initialized',
      validationMode: mode.mode,
      validationFailOnError: mode.failOnError,
      timestamp: Date.now(),
    }));
  }

  /**
   * HTTP fetch handler - upgrade to WebSocket
   */
  async fetch(request: Request): Promise<Response> {
    const upgradeHeader = request.headers.get('Upgrade');

    if (upgradeHeader !== 'websocket') {
      return new Response('Expected WebSocket upgrade', { status: 426 });
    }

    // Create WebSocket pair
    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);

    // Accept WebSocket connection
    this.handleWebSocketConnection(server);

    return new Response(null, {
      status: 101,
      webSocket: client,
    });
  }

  /**
   * Send message through WebSocket with schema validation
   *
   * Validates outgoing message before sending:
   * - Test mode: throws on schema violation
   * - Production: logs warning but sends anyway
   */
  private sendMessage(ws: WebSocket, message: SharedMessage): void {
    // SCHEMA VALIDATION: Outgoing message
    const validator = getValidator();
    validator.validateMessage(message, 'outgoing');

    // Send message
    ws.send(JSON.stringify(message));
  }

  /**
   * Handle new WebSocket connection
   */
  private handleWebSocketConnection(ws: WebSocket): void {
    // Create session with initial connection state and rate limiting
    const session: Session = {
      sessionId: generateSessionId(),
      actorIdentity: null,
      capabilities: [],
      connectedAt: Date.now(),
      lastHeartbeat: Date.now(),
      authenticated: false,
      paused: false,
      connectionState: 'connecting', // Initial state before hub:connect
      rateLimitBucket: createTokenBucket(100, 100 / 60), // 100 msg/min = 1.67 msg/sec
    };

    // Store session and connection
    this.sessions.set(ws, session);

    // CRITICAL: Use ctx.acceptWebSocket() for wrangler dev to route messages
    this.ctx.acceptWebSocket(ws);

    console.log(JSON.stringify({
      event: 'websocket_connection_accepted',
      sessionId: session.sessionId,
      connectionState: session.connectionState,
      timestamp: Date.now(),
    }));
  }

  /**
   * Handle incoming WebSocket message
   */
  async webSocketMessage(ws: WebSocket, message: string | ArrayBuffer): Promise<void> {
    const session = this.sessions.get(ws);
    if (!session) {
      console.error('[SignalHub] WebSocket message from unknown session');
      ws.close(1011, 'Unknown session');
      return;
    }

    try {
      // Check raw message size BEFORE parsing (DoS protection)
      const rawSize = typeof message === 'string' ? message.length : message.byteLength;
      const maxSize = parseInt(this.env.MAX_MESSAGE_SIZE, 10);

      if (rawSize > maxSize) {
        throw new HubError(
          'message_too_large',
          `Message size (${rawSize} bytes) exceeds limit (${maxSize} bytes) before parsing`,
          { actualSize: rawSize, maxSize }
        );
      }

      // Parse message
      const rawMessage =
        typeof message === 'string' ? message : new TextDecoder().decode(message);
      const parsedMessage = JSON.parse(rawMessage);

      // Validate SharedMessage structure (basic runtime check)
      if (!validateSharedMessage(parsedMessage)) {
        throw new HubError('internal_error', 'Invalid SharedMessage structure');
      }

      const msg = parsedMessage as SharedMessage;

      // SCHEMA VALIDATION: Incoming message
      // In test mode: throws on violation
      // In production: logs warning only
      const validator = getValidator();
      validator.validateMessage(msg, 'incoming');

      // Update heartbeat timestamp
      session.lastHeartbeat = Date.now();

      // Route message by type
      const response = await this.routeMessage(msg, session, ws);

      // Send response if returned
      if (response) {
        this.sendMessage(ws, response);
      }

      // Update stats
      this.queueStats.processed++;
    } catch (err) {
      console.error('[SignalHub] Error processing WebSocket message:', err);

      // Send error response
      try {
        const parsedMessage = JSON.parse(
          typeof message === 'string' ? message : new TextDecoder().decode(message)
        );

        const errorMessage = createErrorMessage(
          err instanceof HubError ? err.code : 'internal_error',
          err instanceof Error ? err.message : 'Unknown error',
          parsedMessage,
          SIGNAL_HUB_ADDRESS,
          err instanceof HubError ? err.details : undefined
        );

        this.sendMessage(ws, errorMessage);
      } catch {
        // Failed to send error - close connection
        ws.close(1011, 'Internal error');
      }

      this.queueStats.failed++;
    }
  }

  /**
   * Route message to appropriate handler
   */
  private async routeMessage(
    msg: SharedMessage,
    session: Session,
    ws: WebSocket
  ): Promise<SharedMessage | null> {
    const messageType = msg.type;

    // Rate limiting check (skip for hub:connect to allow initial connection)
    if (messageType !== 'hub:connect') {
      if (!consumeToken(session.rateLimitBucket)) {
        // Calculate retry-after time
        const tokensNeeded = 1 - session.rateLimitBucket.tokens;
        const retryAfter = Math.ceil(tokensNeeded / session.rateLimitBucket.refillRate);

        throw new HubError(
          'rate_limited',
          `Rate limit exceeded. Max 100 messages per minute.`,
          { retryAfter, limit: '100 messages/min' }
        );
      }
    }

    // Connection lifecycle
    if (messageType === 'hub:connect') {
      const response = await handleConnect(msg, session, this.env);

      // Update connection state to connected
      session.connectionState = 'connected';

      // Store connection after successful connect
      this.connections.set(session.sessionId, ws);

      console.log(JSON.stringify({
        event: 'actor_connected',
        sessionId: session.sessionId,
        actorIdentity: session.actorIdentity,
        connectionState: session.connectionState,
        timestamp: Date.now(),
      }));

      return response;
    }

    // Check authentication for subsequent messages (if auth enabled)
    if (this.env.AUTH_ENABLED === 'true' && !session.authenticated) {
      throw new HubError('unauthorized', 'Not authenticated - send hub:connect first');
    }

    // Heartbeat
    if (messageType === 'hub:heartbeat') {
      return handleHeartbeat(msg, session);
    }

    // Disconnect
    if (messageType === 'hub:disconnect') {
      console.log(JSON.stringify({
        event: 'disconnect_requested',
        sessionId: session.sessionId,
        actorIdentity: session.actorIdentity,
        timestamp: Date.now(),
      }));

      // Update connection state
      session.connectionState = 'disconnecting';

      const response = handleDisconnect(msg, session);

      // CRITICAL: Send disconnect acknowledgment BEFORE closing WebSocket
      // Otherwise the response is sent on a closed socket and never reaches the client
      if (response) {
        console.log(JSON.stringify({
          event: 'sending_disconnect_ack',
          sessionId: session.sessionId,
          timestamp: Date.now(),
        }));
        this.sendMessage(ws, response);
      }

      this.cleanupConnection(ws, session);
      console.log(JSON.stringify({
        event: 'closing_websocket',
        sessionId: session.sessionId,
        timestamp: Date.now(),
      }));
      ws.close(1000, 'Client requested disconnect');

      // Return null to prevent double-send in webSocketMessage handler
      return null;
    }

    // Actor registration
    if (messageType === 'hub:register') {
      // Extract the actor address being registered
      const payload = msg.payload as { actorAddress: CanonicalAddress };
      const actorAddress = payload.actorAddress;

      // CRITICAL: Detect duplicate connections (same actorAddress)
      // Check BEFORE registering to close old session first
      this.handleDuplicateConnection(actorAddress, session.sessionId, ws);

      // Update session's actorIdentity to the registered address
      // This ensures subscriptions use the registered address, not the temporary connection ID
      session.actorIdentity = actorAddress;

      // Register the actor
      const response = handleRegister(msg, this.registry, session.sessionId, this.env);

      return response;
    }

    if (messageType === 'hub:unregister') {
      handleUnregister(msg, this.registry);
      return null; // Fire-and-forget
    }

    if (messageType === 'hub:discover') {
      return handleDiscover(msg, this.registry);
    }

    if (messageType === 'hub:list_actors') {
      return handleListActors(msg, this.registry);
    }

    if (messageType === 'hub:renew') {
      return handleRenew(msg, this.registry, this.env);
    }

    // Messaging
    if (messageType === 'hub:send') {
      return handleSend(msg, this.registry, this.connections, this.env, this.sendMessage.bind(this));
    }

    if (messageType === 'hub:broadcast') {
      return handleBroadcast(msg, this.registry, this.connections, this.env, this.sendMessage.bind(this));
    }

    // Pub/sub
    if (messageType === 'hub:subscribe') {
      if (!session.actorIdentity) {
        throw new HubError('unauthorized', 'Actor identity required for subscriptions');
      }
      return handleSubscribe(msg, this.subscriptions, session.actorIdentity);
    }

    if (messageType === 'hub:publish') {
      return handlePublish(msg, this.subscriptions, this.registry, this.connections, this.sendMessage.bind(this));
    }

    if (messageType === 'hub:unsubscribe') {
      if (!session.actorIdentity) {
        throw new HubError('unauthorized', 'Actor identity required for subscriptions');
      }
      handleUnsubscribe(msg, this.subscriptions, session.actorIdentity);
      return null; // Fire-and-forget
    }

    // Flow control
    if (messageType === 'hub:queue_stats') {
      return handleQueueStats(msg, this.queueStats);
    }

    // Unknown message type
    throw new HubError('internal_error', `Unknown message type: ${messageType}`);
  }

  /**
   * Handle WebSocket close
   */
  async webSocketClose(
    ws: WebSocket,
    code: number,
    reason: string,
    wasClean: boolean
  ): Promise<void> {
    const session = this.sessions.get(ws);
    if (!session) {
      return;
    }

    this.cleanupConnection(ws, session);
  }

  /**
   * Handle WebSocket error
   */
  async webSocketError(ws: WebSocket, error: Error): Promise<void> {
    const session = this.sessions.get(ws);
    console.error(
      `WebSocket error: ${session?.sessionId ?? 'unknown'}`,
      error
    );

    if (session) {
      this.cleanupConnection(ws, session);
    }
  }

  /**
   * Handle duplicate connection detection
   * Closes old session when same actor connects again (last connection wins)
   */
  private handleDuplicateConnection(
    actorIdentity: CanonicalAddress,
    newSessionId: string,
    newWs: WebSocket
  ): void {
    console.log(JSON.stringify({
      event: 'handleDuplicateConnection_start',
      actorIdentity,
      newSessionId,
      totalSessions: this.sessions.size,
      timestamp: Date.now(),
    }));

    // Find existing session with same actorIdentity
    let checkedSessions = 0;
    for (const [ws, session] of this.sessions.entries()) {
      checkedSessions++;
      console.log(JSON.stringify({
        event: 'checking_session',
        sessionId: session.sessionId,
        sessionActorIdentity: session.actorIdentity,
        sessionConnectionState: session.connectionState,
        targetActorIdentity: actorIdentity,
        matches: session.actorIdentity === actorIdentity,
        isDifferentSession: session.sessionId !== newSessionId,
        isNotDisconnected: session.connectionState !== 'disconnected',
      }));

      if (
        session.actorIdentity === actorIdentity &&
        session.sessionId !== newSessionId &&
        session.connectionState !== 'disconnected'
      ) {
        console.log(JSON.stringify({
          event: 'duplicate_connection_detected',
          actorIdentity,
          oldSessionId: session.sessionId,
          newSessionId,
          timestamp: Date.now(),
        }));

        // Send disconnect notification to old session
        try {
          const disconnectMsg: SharedMessage = {
            id: crypto.randomUUID(),
            type: 'hub:disconnect',
            from: toCanonicalAddress('cloudflare/signal-hub'),
            to: actorIdentity,
            payload: {
              reason: 'duplicate_connection',
              message: 'New connection established for this actor',
            },
            pattern: 'tell',
            correlationId: null,
            timestamp: Date.now(),
            metadata: {},
            ttl: null,
            signature: null,
          };

          console.log(JSON.stringify({
            event: 'sending_duplicate_disconnect',
            oldSessionId: session.sessionId,
            timestamp: Date.now(),
          }));

          this.sendMessage(ws, disconnectMsg);
        } catch (err) {
          console.error('Failed to send disconnect to old session:', err);
        }

        // Clean up old session
        this.cleanupConnection(ws, session);

        // Close old WebSocket
        ws.close(1000, 'Duplicate connection - new session established');

        console.log(JSON.stringify({
          event: 'old_session_closed',
          oldSessionId: session.sessionId,
          newSessionId,
          timestamp: Date.now(),
        }));
      }
    }

    console.log(JSON.stringify({
      event: 'handleDuplicateConnection_complete',
      actorIdentity,
      newSessionId,
      sessionsChecked: checkedSessions,
      timestamp: Date.now(),
    }));
  }

  /**
   * Cleanup connection resources
   */
  private cleanupConnection(ws: WebSocket, session: Session): void {
    const now = Date.now();
    const sessionDuration = now - session.connectedAt;

    // Update session state
    session.connectionState = 'disconnected';
    session.disconnectedAt = now;

    console.log(JSON.stringify({
      event: 'cleanup_connection_start',
      sessionId: session.sessionId,
      actorIdentity: session.actorIdentity,
      sessionDuration,
      registrySize: this.registry.size,
      sessionsSize: this.sessions.size,
      connectionsSize: this.connections.size,
      timestamp: now,
    }));

    // Remove from sessions
    this.sessions.delete(ws);

    // Remove from connections
    this.connections.delete(session.sessionId);

    // Cleanup registrations for this connection
    let removedCount = 0;
    for (const [address, registration] of this.registry.entries()) {
      if (registration.connectionId === session.sessionId) {
        this.registry.delete(address);
        removedCount++;
        console.log(JSON.stringify({
          event: 'actor_unregistered_on_disconnect',
          actorAddress: address,
          sessionId: session.sessionId,
          timestamp: now,
        }));
      }
    }

    // Cleanup subscriptions
    if (session.actorIdentity) {
      cleanupSubscriptions(this.subscriptions, session.actorIdentity);
    }

    console.log(JSON.stringify({
      event: 'cleanup_connection_complete',
      sessionId: session.sessionId,
      removedRegistrations: removedCount,
      registrySize: this.registry.size,
      sessionsSize: this.sessions.size,
      connectionsSize: this.connections.size,
      timestamp: now,
    }));
  }

  /**
   * Alarm handler (for future use - scheduled cleanup, etc.)
   */
  async alarm(): Promise<void> {
    console.log('SignalHub alarm triggered');

    // Cleanup expired registrations
    const now = Date.now();
    for (const [address, registration] of this.registry.entries()) {
      if (now >= registration.expiresAt) {
        this.registry.delete(address);
        console.log(`Expired actor registration: ${address}`);
      }
    }
  }
}
