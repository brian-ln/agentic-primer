# Specification: Signal Hub Server Implementation

**Generated:** 2026-02-18T00:28:30Z
**Current Date Verified:** true

## 1. Core Principles

### 1.1 Durable Object Architecture

Signal Hub is implemented as a Cloudflare Durable Object, providing:
- **Single-threaded execution:** No concurrency within a single instance
- **In-memory state:** Registry and sessions stored in memory (volatile)
- **WebSocket lifecycle:** Hibernatable WebSocket API with automatic wake
- **Geographic locality:** Instance created near first client, persists

### 1.2 Separation of Concerns

Server implementation follows modular design:
- **SignalHub class:** Orchestration and WebSocket lifecycle
- **Handlers:** Business logic organized by domain (connection, registration, messaging, pub/sub)
- **Utils:** Shared utilities (validation, token generation, canonical addresses)

### 1.3 Stateless Handlers

All handler functions are pure, taking explicit parameters:

```typescript
// Good: Pure function
function handleRegister(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connectionId: string,
  env: Env
): SharedMessage {
  // No access to 'this' or global state
}

// Bad: Stateful method
class Handler {
  handleRegister(msg: SharedMessage) {
    // Depends on instance state
  }
}
```

## 2. SignalHub Class Structure

### 2.1 Class Definition

```typescript
export class SignalHub implements DurableObject {
  private ctx: DurableObjectState;
  private env: Env;
  private sessions: Map<WebSocket, Session>;
  private connections: Map<string, WebSocket>; // sessionId -> WebSocket
  private registry: Map<string, ActorRegistration>; // actorAddress -> registration
  private subscriptions: Map<string, Set<CanonicalAddress>>; // topic -> subscribers
  private queueStats: QueueStats;

  constructor(state: DurableObjectState, env: Env) {
    this.ctx = state;
    this.env = env;
    this.sessions = new Map();
    this.connections = new Map();
    this.registry = new Map();
    this.subscriptions = new Map();
    this.queueStats = { pending: 0, processed: 0, failed: 0, paused: false };

    // Enable hibernation for WebSocket connections
    state.setWebSocketAutoResponse(
      new WebSocketRequestResponsePair(
        JSON.stringify({ type: 'ping' }),
        JSON.stringify({ type: 'pong' })
      )
    );
  }
}
```

### 2.2 State Management

#### In-Memory State
All state is stored in instance variables (RAM):

```typescript
// Session tracking
private sessions: Map<WebSocket, Session>;
// Key: WebSocket object (unique per connection)
// Value: Session metadata (identity, auth, rate limit)

// Connection lookup
private connections: Map<string, WebSocket>;
// Key: sessionId (string UUID)
// Value: WebSocket object (for reverse lookup)

// Actor registry
private registry: Map<string, ActorRegistration>;
// Key: actorAddress (canonical address string)
// Value: Registration metadata (capabilities, expiration, version)

// Topic subscriptions
private subscriptions: Map<string, Set<CanonicalAddress>>;
// Key: topic name (string)
// Value: Set of subscriber actor addresses
```

#### No Persistent Storage
Signal Hub does NOT use `DurableObjectStorage`:
- No `ctx.storage.put()` calls
- All data volatile (lost on eviction)
- Clients responsible for re-registration after eviction

**Rationale:**
- Simplifies implementation
- Reduces storage costs
- Forces explicit client reconnection logic
- Matches ephemeral nature of WebSocket connections

### 2.3 Hibernation Configuration

```typescript
// Auto-response for ping/pong (prevents connection drops)
state.setWebSocketAutoResponse(
  new WebSocketRequestResponsePair(
    JSON.stringify({ type: 'ping' }),
    JSON.stringify({ type: 'pong' })
  )
);
```

**Critical Understanding:**
- Hibernation is AUTOMATIC and TRANSPARENT
- Cannot be prevented or controlled
- In-memory state PERSISTS across hibernation
- Only affects power consumption, not behavior

## 3. Request Lifecycle

### 3.1 HTTP Upgrade to WebSocket

```typescript
async fetch(request: Request): Promise<Response> {
  const upgradeHeader = request.headers.get('Upgrade');

  if (upgradeHeader !== 'websocket') {
    return new Response('Expected WebSocket upgrade', { status: 426 });
  }

  // Create WebSocket pair
  const pair = new WebSocketPair();
  const [client, server] = Object.values(pair);

  // Accept and initialize connection
  this.handleWebSocketConnection(server);

  // Return client socket to user
  return new Response(null, {
    status: 101,
    webSocket: client,
  });
}
```

**Key Points:**
- Only accepts WebSocket upgrades (rejects HTTP GET)
- Creates pair: client (returned to user) + server (handled internally)
- `handleWebSocketConnection()` initializes session

### 3.2 WebSocket Connection Initialization

```typescript
private handleWebSocketConnection(ws: WebSocket): void {
  // Create session with initial state
  const session: Session = {
    sessionId: generateSessionId(), // crypto.randomUUID()
    actorIdentity: null, // Set during hub:connect
    capabilities: [],
    connectedAt: Date.now(),
    lastHeartbeat: Date.now(),
    authenticated: false,
    paused: false,
    connectionState: 'connecting', // Initial state
    rateLimitBucket: createTokenBucket(100, 100 / 60) // 100 msg/min
  };

  // Store session
  this.sessions.set(ws, session);

  // CRITICAL: Accept WebSocket for Cloudflare routing
  this.ctx.acceptWebSocket(ws);

  console.log(JSON.stringify({
    event: 'websocket_connection_accepted',
    sessionId: session.sessionId,
    timestamp: Date.now()
  }));
}
```

**Why `ctx.acceptWebSocket()` is Required:**
- Routes incoming WebSocket messages to `webSocketMessage()` handler
- Without this call, messages are NOT delivered (black hole)
- Must be called in `fetch()` handler, not later

### 3.3 Message Routing

```typescript
async webSocketMessage(ws: WebSocket, message: string | ArrayBuffer): Promise<void> {
  const session = this.sessions.get(ws);
  if (!session) {
    ws.close(1011, 'Unknown session');
    return;
  }

  try {
    // 1. Size validation (BEFORE parsing)
    const rawSize = typeof message === 'string' ? message.length : message.byteLength;
    if (rawSize > parseInt(this.env.MAX_MESSAGE_SIZE, 10)) {
      throw new HubError('message_too_large', `Size: ${rawSize} bytes`);
    }

    // 2. Parse and validate
    const rawMessage = typeof message === 'string' ? message : new TextDecoder().decode(message);
    const parsedMessage = JSON.parse(rawMessage);

    if (!validateSharedMessage(parsedMessage)) {
      throw new HubError('internal_error', 'Invalid SharedMessage structure');
    }

    const msg = parsedMessage as SharedMessage;

    // 3. Update heartbeat (ANY message counts)
    session.lastHeartbeat = Date.now();

    // 4. Route to handler
    const response = await this.routeMessage(msg, session, ws);

    // 5. Send response (if returned)
    if (response) {
      ws.send(JSON.stringify(response));
    }

    // 6. Update stats
    this.queueStats.processed++;

  } catch (err) {
    console.error('[SignalHub] Error processing message:', err);

    // Send error response
    const errorMessage = createErrorMessage(
      err instanceof HubError ? err.code : 'internal_error',
      err instanceof Error ? err.message : 'Unknown error',
      parsedMessage,
      SIGNAL_HUB_ADDRESS,
      err instanceof HubError ? err.details : undefined
    );

    ws.send(JSON.stringify(errorMessage));
    this.queueStats.failed++;
  }
}
```

**Design Patterns:**
1. **Size check before parse:** Prevents DoS via large payloads
2. **Heartbeat on any message:** All messages update liveness timestamp
3. **Synchronous response:** Handler returns response, not async send
4. **Error envelope:** All errors wrapped in `hub:error` message

### 3.4 Message Router Implementation

```typescript
private async routeMessage(
  msg: SharedMessage,
  session: Session,
  ws: WebSocket
): Promise<SharedMessage | null> {
  const messageType = msg.type;

  // Rate limiting (skip for hub:connect)
  if (messageType !== 'hub:connect') {
    if (!consumeToken(session.rateLimitBucket)) {
      const tokensNeeded = 1 - session.rateLimitBucket.tokens;
      const retryAfter = Math.ceil(tokensNeeded / session.rateLimitBucket.refillRate);

      throw new HubError('rate_limited', 'Rate limit exceeded', {
        retryAfter,
        limit: '100 messages/min'
      });
    }
  }

  // Connection lifecycle
  if (messageType === 'hub:connect') {
    const response = await handleConnect(msg, session, this.env);
    session.connectionState = 'connected';
    this.connections.set(session.sessionId, ws);
    return response;
  }

  // Authentication check (if enabled)
  if (this.env.AUTH_ENABLED === 'true' && !session.authenticated) {
    throw new HubError('unauthorized', 'Not authenticated - send hub:connect first');
  }

  if (messageType === 'hub:heartbeat') {
    return handleHeartbeat(msg, session);
  }

  if (messageType === 'hub:disconnect') {
    session.connectionState = 'disconnecting';
    const response = handleDisconnect(msg, session);

    // CRITICAL: Send ack BEFORE closing
    if (response) {
      ws.send(JSON.stringify(response));
    }

    this.cleanupConnection(ws, session);
    ws.close(1000, 'Normal closure');
    return null; // Don't send again
  }

  // Actor registration
  if (messageType === 'hub:register') {
    const payload = msg.payload as { actorAddress: CanonicalAddress };
    this.handleDuplicateConnection(payload.actorAddress, session.sessionId, ws);
    session.actorIdentity = payload.actorAddress;
    return handleRegister(msg, this.registry, session.sessionId, this.env);
  }

  if (messageType === 'hub:unregister') {
    handleUnregister(msg, this.registry);
    return null;
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
    return handleSend(msg, this.registry, this.connections, this.env);
  }

  if (messageType === 'hub:broadcast') {
    return handleBroadcast(msg, this.registry, this.connections, this.env);
  }

  // Pub/sub
  if (messageType === 'hub:subscribe') {
    if (!session.actorIdentity) {
      throw new HubError('unauthorized', 'Actor identity required for subscriptions');
    }
    return handleSubscribe(msg, this.subscriptions, session.actorIdentity);
  }

  if (messageType === 'hub:publish') {
    return handlePublish(msg, this.subscriptions, this.registry, this.connections);
  }

  if (messageType === 'hub:unsubscribe') {
    if (!session.actorIdentity) {
      throw new HubError('unauthorized', 'Actor identity required for subscriptions');
    }
    handleUnsubscribe(msg, this.subscriptions, session.actorIdentity);
    return null;
  }

  // Flow control
  if (messageType === 'hub:queue_stats') {
    return handleQueueStats(msg, this.queueStats);
  }

  throw new HubError('internal_error', `Unknown message type: ${messageType}`);
}
```

## 4. Handler Implementations

### 4.1 Connection Handlers

Located in `src/handlers/connection.ts`:

```typescript
export function handleConnect(
  msg: SharedMessage,
  session: Session,
  env: Env
): SharedMessage {
  // Version check
  const version = msg.payload.version;
  if (version !== env.PROTOCOL_VERSION) {
    throw new HubError('version_mismatch', 'Protocol version mismatch', {
      expected: env.PROTOCOL_VERSION,
      received: version
    });
  }

  // Authentication (if enabled)
  if (env.AUTH_ENABLED === 'true') {
    if (!msg.payload.jwt) {
      throw new HubError('unauthorized', 'JWT required');
    }

    const payload = validateJWT(msg.payload.jwt, env.JWT_SECRET);
    session.actorIdentity = toCanonicalAddress(payload.actorId);
    session.capabilities = payload.capabilities;
    session.authenticated = true;
  }

  return {
    type: 'hub:connected',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: {
      sessionId: session.sessionId,
      actorIdentity: session.actorIdentity,
      capabilities: session.capabilities,
      serverTime: Date.now()
    }
  };
}

export function handleHeartbeat(msg: SharedMessage, session: Session): SharedMessage {
  return {
    type: 'hub:heartbeat_ack',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: { serverTime: Date.now() }
  };
}

export function handleDisconnect(msg: SharedMessage, session: Session): SharedMessage {
  return {
    type: 'hub:disconnect_ack',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: {
      sessionId: session.sessionId,
      cleanedUp: true
    }
  };
}
```

### 4.2 Registration Handlers

Located in `src/handlers/registration.ts`:

```typescript
export function handleRegister(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connectionId: string,
  env: Env
): SharedMessage {
  const payload = msg.payload as {
    actorAddress: CanonicalAddress;
    capabilities: string[];
    metadata: Record<string, unknown>;
    ttl: number;
  };

  const ttl = Math.min(
    payload.ttl ?? parseInt(env.DEFAULT_ACTOR_TTL, 10),
    parseInt(env.MAX_ACTOR_TTL, 10)
  );

  const existing = registry.get(payload.actorAddress);

  const registration: ActorRegistration = {
    actorAddress: payload.actorAddress,
    capabilities: payload.capabilities,
    metadata: payload.metadata,
    connectionId,
    registeredAt: Date.now(),
    expiresAt: Date.now() + ttl,
    version: (existing?.version ?? 0) + 1,
    renewalToken: crypto.randomUUID()
  };

  registry.set(payload.actorAddress, registration);

  return {
    type: 'hub:registered',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: {
      actorAddress: registration.actorAddress,
      renewalToken: registration.renewalToken,
      expiresAt: registration.expiresAt,
      version: registration.version
    }
  };
}

export function handleDiscover(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>
): SharedMessage {
  const query = msg.payload as {
    pattern?: string;
    capabilities?: string[];
    metadata?: Record<string, unknown>;
    limit?: number;
    offset?: number;
  };

  const limit = Math.min(query.limit ?? 100, 1000);
  const offset = query.offset ?? 0;

  const matches: ActorRegistration[] = [];

  for (const registration of registry.values()) {
    // Check expiration
    if (Date.now() > registration.expiresAt) {
      registry.delete(registration.actorAddress); // Lazy cleanup
      continue;
    }

    // Pattern match
    if (query.pattern && !new RegExp(query.pattern).test(registration.actorAddress)) {
      continue;
    }

    // Capabilities match (must have ALL)
    if (query.capabilities?.length) {
      const hasAll = query.capabilities.every(cap => registration.capabilities.includes(cap));
      if (!hasAll) continue;
    }

    // Metadata match
    if (query.metadata) {
      const metadataMatches = Object.entries(query.metadata).every(
        ([key, value]) => registration.metadata[key] === value
      );
      if (!metadataMatches) continue;
    }

    matches.push(registration);
  }

  const paged = matches.slice(offset, offset + limit);

  return {
    type: 'hub:discovery_result',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: {
      actors: paged.map(r => ({
        actorAddress: r.actorAddress,
        capabilities: r.capabilities,
        metadata: r.metadata,
        registeredAt: r.registeredAt
      })),
      count: paged.length,
      hasMore: matches.length > offset + limit,
      totalMatches: matches.length
    }
  };
}
```

### 4.3 Messaging Handlers

Located in `src/handlers/messaging.ts`:

```typescript
export function handleSend(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connections: Map<string, WebSocket>,
  env: Env
): SharedMessage | null {
  const recipient = registry.get(msg.to);

  if (!recipient) {
    throw new HubError('unknown_actor', `Actor ${msg.to} not registered`);
  }

  if (Date.now() > recipient.expiresAt) {
    registry.delete(msg.to); // Cleanup stale entry
    throw new HubError('unknown_actor', `Actor ${msg.to} registration expired`);
  }

  const ws = connections.get(recipient.connectionId);

  if (!ws) {
    throw new HubError('unknown_actor', `Actor ${msg.to} disconnected`);
  }

  // Forward message to recipient
  const deliveryMessage = {
    ...msg,
    from: msg.from, // Preserve original sender
    to: recipient.actorAddress
  };

  try {
    ws.send(JSON.stringify(deliveryMessage));
  } catch (err) {
    throw new HubError('internal_error', `Failed to deliver message: ${err.message}`);
  }

  return null; // No ack for fire-and-forget send
}

export function handleBroadcast(
  msg: SharedMessage,
  registry: Map<string, ActorRegistration>,
  connections: Map<string, WebSocket>,
  env: Env
): SharedMessage {
  const recipientCount = registry.size;
  let successCount = 0;
  let failureCount = 0;

  const serialized = JSON.stringify({
    ...msg,
    to: '@(broadcast)' // Special broadcast address
  });

  for (const registration of registry.values()) {
    const ws = connections.get(registration.connectionId);
    if (!ws) {
      failureCount++;
      continue;
    }

    try {
      ws.send(serialized);
      successCount++;
    } catch (err) {
      failureCount++;
      console.error(`Broadcast failed for ${registration.actorAddress}:`, err);
    }
  }

  return {
    type: 'hub:broadcast_ack',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: {
      recipientCount,
      successCount,
      failureCount
    }
  };
}
```

### 4.4 Pub/Sub Handlers

Located in `src/handlers/pubsub.ts`:

```typescript
export function handleSubscribe(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorIdentity: CanonicalAddress
): SharedMessage {
  const payload = msg.payload as { topic: string };

  if (!subscriptions.has(payload.topic)) {
    subscriptions.set(payload.topic, new Set());
  }

  subscriptions.get(payload.topic)!.add(actorIdentity);

  return {
    type: 'hub:subscribed',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: {
      topic: payload.topic,
      subscribedAt: Date.now()
    }
  };
}

export function handlePublish(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  registry: Map<string, ActorRegistration>,
  connections: Map<string, WebSocket>
): SharedMessage {
  const payload = msg.payload as {
    topic: string;
    message: unknown;
  };

  const subscribers = subscriptions.get(payload.topic) ?? new Set();
  let deliveredCount = 0;

  const deliveryMessage = {
    type: 'hub:topic_message',
    from: msg.from,
    to: '@(topic)' as CanonicalAddress,
    payload: {
      topic: payload.topic,
      message: payload.message
    },
    timestamp: Date.now(),
    metadata: {}
  };

  const serialized = JSON.stringify(deliveryMessage);

  for (const subscriberAddr of subscribers) {
    const registration = registry.get(subscriberAddr);
    if (!registration) continue;

    const ws = connections.get(registration.connectionId);
    if (!ws) continue;

    try {
      ws.send(serialized);
      deliveredCount++;
    } catch (err) {
      console.error(`Failed to deliver to ${subscriberAddr}:`, err);
    }
  }

  return {
    type: 'hub:delivery_ack',
    from: SIGNAL_HUB_ADDRESS,
    to: msg.from,
    payload: {
      topic: payload.topic,
      subscriberCount: subscribers.size,
      deliveredCount
    }
  };
}

export function cleanupSubscriptions(
  actorAddress: CanonicalAddress,
  subscriptions: Map<string, Set<CanonicalAddress>>
): void {
  for (const [topic, subscribers] of subscriptions.entries()) {
    subscribers.delete(actorAddress);

    // Cleanup empty topics
    if (subscribers.size === 0) {
      subscriptions.delete(topic);
    }
  }
}
```

## 5. Cleanup and Lifecycle

### 5.1 Connection Cleanup

```typescript
private cleanupConnection(ws: WebSocket, session: Session): void {
  // Unregister actor
  if (session.actorIdentity) {
    this.registry.delete(session.actorIdentity);
  }

  // Cleanup subscriptions
  if (session.actorIdentity) {
    cleanupSubscriptions(session.actorIdentity, this.subscriptions);
  }

  // Update state
  session.connectionState = 'disconnected';
  session.disconnectedAt = Date.now();

  // Remove session
  this.sessions.delete(ws);
  this.connections.delete(session.sessionId);

  console.log(JSON.stringify({
    event: 'connection_cleaned_up',
    sessionId: session.sessionId,
    actorIdentity: session.actorIdentity,
    timestamp: Date.now()
  }));
}
```

### 5.2 Duplicate Connection Handling

```typescript
private handleDuplicateConnection(
  actorIdentity: CanonicalAddress,
  newSessionId: string,
  newWs: WebSocket
): void {
  // Find existing session with same actorIdentity
  for (const [ws, session] of this.sessions.entries()) {
    if (
      session.actorIdentity === actorIdentity &&
      session.sessionId !== newSessionId &&
      session.connectionState !== 'disconnected'
    ) {
      // Send disconnect notification
      const disconnectMsg = {
        type: 'hub:disconnect',
        from: SIGNAL_HUB_ADDRESS,
        to: actorIdentity,
        payload: {
          reason: 'duplicate_connection',
          message: 'New connection established for this actor'
        }
      };

      try {
        ws.send(JSON.stringify(disconnectMsg));
      } catch (err) {
        console.error('Failed to send disconnect to old session:', err);
      }

      // Cleanup old session
      this.cleanupConnection(ws, session);
      ws.close(1000, 'Duplicate connection');
    }
  }
}
```

## 6. Environment Configuration

### 6.1 Environment Variables

```typescript
interface Env {
  SIGNAL_HUB: DurableObjectNamespace;
  PROTOCOL_VERSION: string;          // "1.0"
  MAX_MESSAGE_SIZE: string;          // "524288" (512KB)
  HEARTBEAT_INTERVAL: string;        // "30000" (30s)
  ACTOR_REGISTRY_LIMIT: string;      // "50000"
  DEFAULT_ACTOR_TTL: string;         // "300000" (5 minutes)
  MAX_ACTOR_TTL: string;             // "3600000" (1 hour)
  BROADCAST_SYNC_THRESHOLD: string;  // "100"
  JWT_SECRET?: string;
  AUTH_ENABLED: string;              // "true" or "false"
}
```

### 6.2 Wrangler Configuration

```toml
# wrangler.toml
name = "signal-hub"
main = "src/index.ts"
compatibility_date = "2024-01-01"

[durable_objects]
bindings = [
  { name = "SIGNAL_HUB", class_name = "SignalHub" }
]

[[migrations]]
tag = "v1"
new_classes = ["SignalHub"]

[vars]
PROTOCOL_VERSION = "1.0"
MAX_MESSAGE_SIZE = "524288"
HEARTBEAT_INTERVAL = "30000"
ACTOR_REGISTRY_LIMIT = "50000"
DEFAULT_ACTOR_TTL = "300000"
MAX_ACTOR_TTL = "3600000"
BROADCAST_SYNC_THRESHOLD = "100"
AUTH_ENABLED = "false"
```

## 7. Testing and Observability

### 7.1 Logging Pattern

All significant events logged as structured JSON:

```typescript
console.log(JSON.stringify({
  event: 'websocket_connection_accepted',
  sessionId: session.sessionId,
  timestamp: Date.now()
}));
```

**Key Events:**
- `websocket_connection_accepted`
- `actor_connected`
- `disconnect_requested`
- `duplicate_connection_detected`
- `connection_cleaned_up`
- `state_transition`

### 7.2 Metrics Collection

```typescript
interface QueueStats {
  pending: number;   // Currently processing (always 0 in single-threaded model)
  processed: number; // Successfully handled messages
  failed: number;    // Error count
  paused: boolean;   // Backpressure state (future use)
}
```

## 8. Cross-Reference

See also:
- **PROTOCOL.spec.md** - Wire protocol and message types
- **STATE_MACHINE.spec.md** - Connection lifecycle
- **SCHEMAS.spec.md** - Type definitions
- **EDGE_CASES.spec.md** - Failure scenarios
- **CLIENT.spec.md** - Client implementation patterns
