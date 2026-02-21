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
  HubMetrics,
  ConnectionState,
} from '../types';
import { HubError } from '../types';
import {
  generateSessionId,
  toCanonicalAddress,
  validateSharedMessage,
  createErrorMessage,
  createTokenBucket,
  consumeToken,
  createReply,
  log,
} from '../utils';
import { getValidator } from '../validation/schema-validator';
import { getFSMValidator } from '../validation/fsm-validator';

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
import { handleRefreshToken } from '../handlers/auth';

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
 * - Per-minute metrics (cumulative since last alarm reset)
 */
export class SignalHub implements DurableObject {
  private ctx: DurableObjectState;
  private env: Env;
  private sessions: Map<WebSocket, Session>;
  private connections: Map<string, WebSocket>;
  private registry: Map<string, ActorRegistration>;
  private subscriptions: Map<string, Set<CanonicalAddress>>;
  /** Inverse index: actor address → set of subscribed topic names (O(S) disconnect cleanup) */
  private actorTopics: Map<CanonicalAddress, Set<string>>;
  private queueStats: QueueStats;

  /**
   * Metrics counters — cumulative since last alarm tick (windowStart).
   * Call resetMetrics() to begin a new window.
   */
  private metrics: {
    messages_received: number;
    messages_sent: number;
    errors: number;
    connections_opened: number;
    connections_closed: number;
    windowStart: number;
  };

  constructor(state: DurableObjectState, env: Env) {
    this.ctx = state;
    this.env = env;
    this.sessions = new Map();
    this.connections = new Map();
    this.registry = new Map();
    this.subscriptions = new Map();
    this.actorTopics = new Map();
    this.queueStats = {
      pending: 0,
      processed: 0,
      failed: 0,
      paused: false,
    };
    this.metrics = {
      messages_received: 0,
      messages_sent: 0,
      errors: 0,
      connections_opened: 0,
      connections_closed: 0,
      windowStart: Date.now(),
    };

    // Enable hibernation for WebSocket connections
    state.setWebSocketAutoResponse(
      new WebSocketRequestResponsePair(
        JSON.stringify({ type: 'ping' }),
        JSON.stringify({ type: 'pong' })
      )
    );

    // Schedule per-minute TTL cleanup alarm if not already set.
    // Non-blocking; alarm() re-schedules itself on each invocation.
    void state.storage.getAlarm().then(scheduled => {
      if (scheduled === null) {
        void state.storage.setAlarm(Date.now() + 60_000);
      }
    });

    // Log validation mode on startup (lifecycle event — always emit)
    const validator = getValidator();
    const mode = validator.getMode();
    const fsmValidator = getFSMValidator();
    const fsmMode = fsmValidator.getMode();
    console.log(JSON.stringify({
      event: 'signalhub_initialized',
      validationMode: mode.mode,
      validationFailOnError: mode.failOnError,
      fsmValidationMode: fsmMode.mode,
      fsmValidationFailOnError: fsmMode.failOnError,
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
   * Return current metrics snapshot.
   *
   * Counters are cumulative since windowStart (last alarm reset).
   * After reading, counters are NOT automatically reset — call resetMetrics() if desired.
   */
  getMetrics(): HubMetrics {
    return {
      ...this.metrics,
      timestamp: Date.now(),
    };
  }

  /**
   * Reset metrics counters and start a new window.
   * Called automatically on each alarm tick.
   */
  private resetMetrics(): void {
    this.metrics = {
      messages_received: 0,
      messages_sent: 0,
      errors: 0,
      connections_opened: 0,
      connections_closed: 0,
      windowStart: Date.now(),
    };
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
    this.metrics.messages_sent++;
  }

  /**
   * Send a pre-serialized JSON string directly to a WebSocket.
   *
   * Used by handleBroadcast for the stringify-once optimization: the message
   * body is serialized once outside this method, so we skip JSON.stringify here
   * and send the raw string directly. Schema validation is intentionally skipped
   * on this path since the payload was already validated when the broadcast
   * message was constructed.
   */
  private sendRawMessage(ws: WebSocket, raw: string): void {
    ws.send(raw);
    this.metrics.messages_sent++;
  }

  /**
   * Transition connection state with FSM validation
   *
   * Validates state transition against state machine specification:
   * - Test mode: throws on illegal transition
   * - Production: logs warning but continues
   *
   * @param session - Session to update
   * @param fromState - Current state (for validation)
   * @param toState - Target state
   * @param event - Event triggering the transition
   */
  private transitionState(
    session: Session,
    fromState: ConnectionState,
    toState: ConnectionState,
    event: string
  ): void {
    // FSM VALIDATION: State transition
    const fsmValidator = getFSMValidator();
    fsmValidator.isValidTransition(fromState, toState, event);

    // Update state
    session.connectionState = toState;

    log(this.env, 'state_transition', {
      sessionId: session.sessionId,
      fromState,
      toState,
      transitionEvent: event,
    });
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
      rateLimitBucket: createTokenBucket(10, 10), // 10 msg/sec per session
    };

    // Store session and connection
    this.sessions.set(ws, session);

    // CRITICAL: Use ctx.acceptWebSocket() for wrangler dev to route messages
    this.ctx.acceptWebSocket(ws);

    this.metrics.connections_opened++;

    log(this.env, 'websocket_connection_accepted', {
      sessionId: session.sessionId,
      connectionState: session.connectionState,
    });
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

    this.metrics.messages_received++;

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

      this.metrics.errors++;

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
    // Per-session token bucket: 10 messages/sec capacity, 10 tokens/sec refill.
    // When exceeded, returns hub:error with code RATE_LIMIT_EXCEEDED.
    if (messageType !== 'hub:connect') {
      if (!consumeToken(session.rateLimitBucket)) {
        // Calculate retry-after time (seconds until next token available)
        const tokensNeeded = 1 - session.rateLimitBucket.tokens;
        const retryAfter = Math.ceil(tokensNeeded / session.rateLimitBucket.refillRate);

        throw new HubError(
          'rate_limited',
          `Rate limit exceeded. Max 10 messages per second per session.`,
          { retryAfter, limit: '10 msg/sec', code: 'RATE_LIMIT_EXCEEDED' }
        );
      }
    }

    // Connection lifecycle
    if (messageType === 'hub:connect') {
      try {
        const response = await handleConnect(msg, session, this.env);

        // Transition: connecting → connected (hub:connect_success)
        this.transitionState(session, 'connecting', 'connected', 'hub:connect_success');

        // Store connection after successful connect
        this.connections.set(session.sessionId, ws);

        log(this.env, 'actor_connected', {
          sessionId: session.sessionId,
          actorIdentity: session.actorIdentity,
          connectionState: session.connectionState,
        });

        return response;
      } catch (err) {
        // Transition: connecting → disconnected (hub:connect_fail)
        this.transitionState(session, 'connecting', 'disconnected', 'hub:connect_fail');

        log(this.env, 'connect_failed', {
          sessionId: session.sessionId,
          error: err instanceof Error ? err.message : 'Unknown error',
        });

        // Re-throw to be caught by outer error handler
        throw err;
      }
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
      log(this.env, 'disconnect_requested', {
        sessionId: session.sessionId,
        actorIdentity: session.actorIdentity,
      });

      // Transition: connected → disconnecting (hub:disconnect)
      this.transitionState(session, 'connected', 'disconnecting', 'hub:disconnect');

      const response = handleDisconnect(msg, session);

      // CRITICAL: Send disconnect acknowledgment BEFORE closing WebSocket
      // Otherwise the response is sent on a closed socket and never reaches the client
      if (response) {
        this.sendMessage(ws, response);
      }

      this.cleanupConnection(ws, session);
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
      handleUnregister(msg, this.registry, this.env);
      return null; // Fire-and-forget
    }

    if (messageType === 'hub:discover') {
      return handleDiscover(msg, this.registry, this.env);
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
      return handleBroadcast(
        msg,
        this.registry,
        this.connections,
        this.env,
        this.sendMessage.bind(this),
        this.sendRawMessage.bind(this)
      );
    }

    // Pub/sub
    if (messageType === 'hub:subscribe') {
      if (!session.actorIdentity) {
        throw new HubError('unauthorized', 'Actor identity required for subscriptions');
      }
      return handleSubscribe(msg, this.subscriptions, session.actorIdentity, this.actorTopics);
    }

    if (messageType === 'hub:publish') {
      return handlePublish(msg, this.subscriptions, this.registry, this.connections, this.sendMessage.bind(this));
    }

    if (messageType === 'hub:unsubscribe') {
      if (!session.actorIdentity) {
        throw new HubError('unauthorized', 'Actor identity required for subscriptions');
      }
      handleUnsubscribe(msg, this.subscriptions, session.actorIdentity, this.actorTopics);
      return null; // Fire-and-forget
    }

    // Flow control
    if (messageType === 'hub:queue_stats') {
      return handleQueueStats(msg, this.queueStats);
    }

    // Metrics
    if (messageType === 'hub:metrics') {
      const metrics = this.getMetrics();
      return createReply('hub:metrics_response', metrics, msg, SIGNAL_HUB_ADDRESS);
    }

    // Authentication - token refresh for long-lived sessions
    if (messageType === 'hub:refresh_token') {
      return handleRefreshToken(msg, session, this.env);
    }

    // Unknown message type
    throw new HubError('internal_error', `Unknown message type: ${messageType}`);
  }

  /**
   * Handle WebSocket close
   */
  async webSocketClose(
    ws: WebSocket,
    _code: number,
    _reason: string,
    _wasClean: boolean
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

    this.metrics.errors++;

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
    _newWs: WebSocket
  ): void {
    log(this.env, 'handleDuplicateConnection_start', {
      actorIdentity,
      newSessionId,
      totalSessions: this.sessions.size,
    });

    // Find existing session with same actorIdentity
    for (const [ws, session] of this.sessions.entries()) {
      if (
        session.actorIdentity === actorIdentity &&
        session.sessionId !== newSessionId &&
        session.connectionState !== 'disconnected'
      ) {
        log(this.env, 'duplicate_connection_detected', {
          actorIdentity,
          oldSessionId: session.sessionId,
          newSessionId,
        });

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

          this.sendMessage(ws, disconnectMsg);
        } catch (err) {
          console.error('Failed to send disconnect to old session:', err);
        }

        // Clean up old session
        this.cleanupConnection(ws, session);

        // Close old WebSocket
        ws.close(1000, 'Duplicate connection - new session established');
      }
    }
  }

  /**
   * Cleanup connection resources
   */
  private cleanupConnection(ws: WebSocket, session: Session): void {
    const now = Date.now();
    const fromState = session.connectionState;

    // Transition to disconnected based on current state
    // Valid transitions: connecting→disconnected, connected→disconnected, disconnecting→disconnected
    if (fromState !== 'disconnected') {
      this.transitionState(session, fromState, 'disconnected', 'websocket_close');
    }

    session.disconnectedAt = now;

    this.metrics.connections_closed++;

    log(this.env, 'cleanup_connection', {
      sessionId: session.sessionId,
      actorIdentity: session.actorIdentity,
      sessionDuration: now - session.connectedAt,
      registrySize: this.registry.size,
      sessionsSize: this.sessions.size,
    });

    // Remove from sessions
    this.sessions.delete(ws);

    // Remove from connections
    this.connections.delete(session.sessionId);

    // Cleanup registrations for this connection
    for (const [address, registration] of this.registry.entries()) {
      if (registration.connectionId === session.sessionId) {
        this.registry.delete(address);
      }
    }

    // Cleanup subscriptions — O(S) with inverse index
    if (session.actorIdentity) {
      cleanupSubscriptions(this.subscriptions, session.actorIdentity, this.actorTopics);
    }
  }

  /**
   * Alarm handler — resets per-minute metrics window and cleans up expired registrations.
   * Re-schedules itself for the next minute so cleanup fires continuously.
   */
  async alarm(): Promise<void> {
    // Reset metrics for the new window
    this.resetMetrics();

    // Cleanup expired registrations
    const now = Date.now();
    for (const [address, registration] of this.registry.entries()) {
      if (now >= registration.expiresAt) {
        this.registry.delete(address);
      }
    }

    // Re-schedule for the next minute
    await this.ctx.storage.setAlarm(Date.now() + 60_000);
  }
}
