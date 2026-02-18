# Specification: Edge Cases and Failure Modes

**Generated:** 2026-02-18T00:28:30Z
**Current Date Verified:** true

## 1. Core Principle: Explicit Failure Handling

Signal Hub explicitly handles edge cases rather than relying on implicit behavior. Every failure mode has a documented recovery path.

## 2. Connection Lifecycle Failures

### 2.1 WebSocket Disconnect During Message Send

**Scenario:** Actor sends message to Signal Hub, WebSocket drops mid-frame, actor retries before reconnect completes.

**Manifestation:**
- Client calls `ws.send()` successfully
- Network drops connection before server receives frame
- Client's `onclose` handler fires
- Retry logic attempts to resend on new connection

**Server Behavior:**
```typescript
try {
  const response = await routeMessage(msg, session, ws);
  if (response && session.connectionState === 'connected') {
    ws.send(JSON.stringify(response));
  }
} catch (err) {
  if (session.connectionState === 'disconnected') {
    console.log('Suppressing send to disconnected session');
    return; // Don't throw - connection already cleaned up
  }
  throw err;
}
```

**Client Recovery:**
```typescript
// Client-side retry with idempotency
private async sendWithRetry(msg: SharedMessage, maxRetries = 3) {
  for (let attempt = 0; attempt < maxRetries; attempt++) {
    try {
      if (this.connectionState !== 'connected') {
        await this.waitForConnection(5000);
      }

      this.ws.send(JSON.stringify(msg));
      return; // Success
    } catch (err) {
      if (attempt === maxRetries - 1) throw err;
      await this.exponentialBackoff(attempt);
    }
  }
}
```

**Outcome:** At-most-once delivery (message may be lost, never duplicated)

### 2.2 Duplicate Connections (Same Actor, Multiple Sessions)

**Scenario:** Actor restarts, reconnects before old connection times out. Signal Hub receives two `hub:register` messages for same actor address.

**Detection:**
```typescript
// In handleRegister() - BEFORE registering
for (const [ws, session] of this.sessions.entries()) {
  if (
    session.actorIdentity === actorAddress &&
    session.sessionId !== newSessionId &&
    session.connectionState !== 'disconnected'
  ) {
    // Duplicate detected!
  }
}
```

**Server Resolution (Last Connection Wins):**
```typescript
// 1. Send disconnect notification to old session
const disconnectMsg = {
  type: 'hub:disconnect',
  from: 'cloudflare/signal-hub',
  to: actorIdentity,
  payload: {
    reason: 'duplicate_connection',
    message: 'New connection established for this actor'
  }
};
oldWs.send(JSON.stringify(disconnectMsg));

// 2. Cleanup old session
this.cleanupConnection(oldWs, oldSession);

// 3. Register new connection
this.registry.set(actorAddress, newRegistration);
```

**Old Client Behavior:**
```typescript
ws.onmessage = (event) => {
  const msg = JSON.parse(event.data);

  if (msg.type === 'hub:disconnect' &&
      msg.payload.reason === 'duplicate_connection') {
    // Graceful shutdown - don't auto-reconnect
    this.connectionState = 'disconnected';
    this.ws.close();
    this.emit('superseded');
  }
};
```

**Outcome:** Old connection closed gracefully, new connection active

### 2.3 Heartbeat Timeout Race Condition

**Scenario:** Actor sends heartbeat, immediately crashes. Hub records heartbeat, cancels timeout, but never hears from actor again until TTL expires.

**Problem:** Race between heartbeat arrival and cleanup timer.

**Server Implementation:**
```typescript
// Update lastHeartbeat on ANY message (not just hub:heartbeat)
session.lastHeartbeat = Date.now();

// Periodic cleanup (every 30s)
setInterval(() => {
  const now = Date.now();
  for (const [ws, session] of this.sessions.entries()) {
    if (
      session.connectionState === 'connected' &&
      now - session.lastHeartbeat > 60_000 // 60 second timeout
    ) {
      console.log(`Heartbeat timeout for session ${session.sessionId}`);
      ws.close(1000, 'Heartbeat timeout');
      this.cleanupConnection(ws, session);
    }
  }
}, 30_000);
```

**Key Insight:**
- ANY message updates `lastHeartbeat` (not just explicit heartbeats)
- This prevents false timeouts during active communication
- Dedicated `hub:heartbeat` messages keep quiet connections alive

**Outcome:** Sessions timeout after 60s of no messages (including heartbeats)

### 2.4 Graceful Disconnect Acknowledgment Lost

**Scenario:** Client sends `hub:disconnect`, server sends `hub:disconnect_ack`, but client's connection closes before ack received.

**Server Implementation:**
```typescript
// CRITICAL: Send ack BEFORE closing WebSocket
const ack = {
  type: 'hub:disconnect_ack',
  from: SIGNAL_HUB_ADDRESS,
  to: msg.from,
  payload: { sessionId: session.sessionId, cleanedUp: true }
};

ws.send(JSON.stringify(ack)); // Send first

// THEN close (with small delay for network flush)
setTimeout(() => {
  ws.close(1000, 'Normal closure');
}, 10);
```

**Client Implementation:**
```typescript
async disconnect() {
  this.connectionState = 'disconnecting';

  this.send({ type: 'hub:disconnect', ... });

  // Wait for ack with timeout
  try {
    await this.waitForAck('hub:disconnect_ack', 5000);
    console.log('Graceful disconnect confirmed');
  } catch (err) {
    console.log('Disconnect ack timeout - proceeding anyway');
  }

  this.ws.close();
}
```

**Outcome:** Best-effort ack delivery, cleanup always completes on server

## 3. Registration Failures

### 3.1 Registration Expiration Race

**Scenario:** Actor's registration TTL expires between discovery and send.

**Timeline:**
1. Actor A: `hub:discover` → finds Actor B
2. Actor B's TTL expires (5 minutes elapsed)
3. Actor A: `hub:send` to Actor B → `unknown_actor` error

**Server Behavior:**
```typescript
// In handleSend()
const recipient = this.registry.get(msg.to);

if (!recipient) {
  throw new HubError(
    'unknown_actor',
    `Actor ${msg.to} not registered or disconnected`,
    { targetActor: msg.to }
  );
}

// Check expiration
if (Date.now() > recipient.expiresAt) {
  this.registry.delete(msg.to); // Cleanup stale entry
  throw new HubError(
    'unknown_actor',
    `Actor ${msg.to} registration expired`,
    { targetActor: msg.to, expiresAt: recipient.expiresAt }
  );
}
```

**Client Recovery:**
```typescript
// Retry with fresh discovery
try {
  await this.send({ type: 'hub:send', to: actorB, ... });
} catch (err) {
  if (err.code === 'unknown_actor') {
    // Re-discover
    const result = await this.discover({ pattern: actorB });
    if (result.actors.length > 0) {
      await this.send({ type: 'hub:send', to: actorB, ... });
    }
  }
}
```

**Outcome:** Client must re-discover, or target actor must re-register

### 3.2 Registration Version Conflict

**Scenario:** Two clients try to register the same actor address simultaneously (rare in practice, but possible in distributed systems).

**Server Behavior:**
```typescript
// Atomic check-and-set pattern
const existing = this.registry.get(actorAddress);

if (existing && existing.connectionId !== newConnectionId) {
  // Check if existing is still active
  const existingSession = this.findSessionByConnectionId(existing.connectionId);

  if (existingSession && existingSession.connectionState === 'connected') {
    throw new HubError(
      'unknown_actor', // Reused for conflict
      `Actor ${actorAddress} already registered from another connection`,
      {
        existingVersion: existing.version,
        existingExpiresAt: existing.expiresAt,
        hint: 'Wait for expiration or use different address'
      }
    );
  }
}

// Register with incremented version
this.registry.set(actorAddress, {
  ...newRegistration,
  version: (existing?.version ?? 0) + 1
});
```

**Outcome:** First successful registration wins, second gets error

### 3.3 Renewal Token Mismatch

**Scenario:** Client tries to renew registration with incorrect renewal token (e.g., after server restart).

**Server Behavior:**
```typescript
// In handleRenew()
const registration = this.registry.get(msg.payload.actorAddress);

if (!registration) {
  throw new HubError('unknown_actor', 'Actor not registered');
}

if (registration.renewalToken !== msg.payload.renewalToken) {
  throw new HubError(
    'unauthorized',
    'Invalid renewal token',
    { hint: 'Re-register to obtain new token' }
  );
}
```

**Client Recovery:**
```typescript
try {
  await this.renew({ actorAddress, renewalToken, ttl });
} catch (err) {
  if (err.code === 'unauthorized' || err.code === 'unknown_actor') {
    // Re-register from scratch
    await this.register({ actorAddress, capabilities, metadata, ttl });
  }
}
```

**Outcome:** Client re-registers, obtains new renewal token

## 4. Broadcast and Pub/Sub Failures

### 4.1 Broadcast Fan-Out Exceeds CPU Limit

**Scenario:** 10,000 actors registered. Hub broadcasts to all. Each WebSocket `send()` takes ~1ms. Total: 10s of synchronous work.

**Problem:** Cloudflare Workers CPU limit is 30s per request (can be exceeded with large fan-outs).

**Server Implementation (Batched):**
```typescript
// In handleBroadcast()
const BROADCAST_BATCH_SIZE = 100;

async function broadcastToAll(message: SharedMessage, connections: Map<string, WebSocket>) {
  const sockets = Array.from(connections.values());
  const serialized = JSON.stringify(message);

  let successCount = 0;
  let failureCount = 0;

  for (let i = 0; i < sockets.length; i += BROADCAST_BATCH_SIZE) {
    const batch = sockets.slice(i, i + BROADCAST_BATCH_SIZE);

    for (const ws of batch) {
      try {
        ws.send(serialized);
        successCount++;
      } catch (err) {
        failureCount++;
        console.error('Broadcast send failed:', err);
      }
    }

    // Yield to event loop every batch
    if (i + BROADCAST_BATCH_SIZE < sockets.length) {
      await new Promise(resolve => setTimeout(resolve, 0));
    }
  }

  return { successCount, failureCount };
}
```

**Configuration:**
```typescript
const BROADCAST_SYNC_THRESHOLD = 100; // From env

if (recipientCount <= BROADCAST_SYNC_THRESHOLD) {
  // Synchronous broadcast (fast path)
  await broadcastSync(message, connections);
} else {
  // Async batched broadcast
  await broadcastToAll(message, connections);
}
```

**Outcome:** Large broadcasts complete without CPU timeout, with best-effort delivery

### 4.2 Subscriber Disconnected Mid-Publish

**Scenario:** Actor publishes to topic with 100 subscribers. Subscriber 50 disconnects between publish start and delivery.

**Server Behavior:**
```typescript
// In handlePublish()
const subscribers = this.subscriptions.get(topic) ?? new Set();

let deliveredCount = 0;

for (const subscriberAddr of subscribers) {
  const registration = this.registry.get(subscriberAddr);

  if (!registration) {
    // Subscriber no longer registered - skip
    console.log(`Subscriber ${subscriberAddr} not found - skipping`);
    continue;
  }

  const ws = this.connections.get(registration.connectionId);

  if (!ws) {
    // Connection lost - skip
    console.log(`Connection ${registration.connectionId} not found - skipping`);
    continue;
  }

  try {
    ws.send(JSON.stringify(deliveryMessage));
    deliveredCount++;
  } catch (err) {
    console.error(`Failed to deliver to ${subscriberAddr}:`, err);
  }
}

return {
  type: 'hub:delivery_ack',
  payload: {
    topic,
    subscriberCount: subscribers.size,
    deliveredCount
  }
};
```

**Outcome:** Best-effort delivery to active subscribers, failures logged but not blocking

### 4.3 Topic Subscription Cleanup on Disconnect

**Scenario:** Actor subscribes to 10 topics, then disconnects abnormally. Subscriptions must be cleaned up to prevent memory leaks.

**Server Implementation:**
```typescript
// In cleanupConnection()
if (session.actorIdentity) {
  // Cleanup all topic subscriptions for this actor
  cleanupSubscriptions(session.actorIdentity, this.subscriptions);
}

// Utility function
function cleanupSubscriptions(
  actorAddress: CanonicalAddress,
  subscriptions: Map<string, Set<CanonicalAddress>>
) {
  let removedCount = 0;

  for (const [topic, subscribers] of subscriptions.entries()) {
    if (subscribers.has(actorAddress)) {
      subscribers.delete(actorAddress);
      removedCount++;

      // Cleanup empty topics
      if (subscribers.size === 0) {
        subscriptions.delete(topic);
      }
    }
  }

  console.log(`Removed ${removedCount} subscriptions for ${actorAddress}`);
}
```

**Outcome:** No orphaned subscriptions, memory efficient

## 5. Message Delivery Failures

### 5.1 Message Too Large

**Scenario:** Client sends 2MB message. WebSocket frame limit is 1MB.

**Server Validation (BEFORE Parsing):**
```typescript
// Check raw message size BEFORE JSON.parse()
const rawSize = typeof message === 'string' ? message.length : message.byteLength;
const maxSize = parseInt(this.env.MAX_MESSAGE_SIZE, 10); // 512KB

if (rawSize > maxSize) {
  throw new HubError(
    'message_too_large',
    `Message size (${rawSize} bytes) exceeds limit (${maxSize} bytes)`,
    { actualSize: rawSize, maxSize }
  );
}
```

**Client Prevention:**
```typescript
// Client-side validation
function send(message: SharedMessage) {
  const serialized = JSON.stringify(message);

  if (serialized.length > MAX_PAYLOAD_SIZE) {
    throw new Error(
      `Message too large: ${serialized.length} bytes (max: ${MAX_PAYLOAD_SIZE})`
    );
  }

  this.ws.send(serialized);
}
```

**Outcome:** Oversized messages rejected before parsing, preventing DoS

### 5.2 Rate Limit Exceeded

**Scenario:** Actor sends 150 messages in 60 seconds. Rate limit is 100 messages/minute.

**Server Behavior:**
```typescript
// Token bucket rate limiting
if (messageType !== 'hub:connect') { // Skip for initial connection
  if (!consumeToken(session.rateLimitBucket)) {
    const tokensNeeded = 1 - session.rateLimitBucket.tokens;
    const retryAfter = Math.ceil(tokensNeeded / session.rateLimitBucket.refillRate);

    throw new HubError(
      'rate_limited',
      'Rate limit exceeded. Max 100 messages per minute.',
      { retryAfter, limit: '100 messages/min' }
    );
  }
}
```

**Client Recovery:**
```typescript
try {
  await this.send(message);
} catch (err) {
  if (err.code === 'rate_limited') {
    const retryAfter = err.details?.retryAfter ?? 10;
    console.log(`Rate limited - waiting ${retryAfter}s`);
    await sleep(retryAfter * 1000);
    await this.send(message); // Retry
  }
}
```

**Outcome:** Client backs off, messages queued locally or dropped

### 5.3 Message TTL Expired

**Scenario:** Message sent with 5-second TTL, but processing delayed due to backpressure. Message arrives after TTL.

**Server Behavior:**
```typescript
// Check TTL if present
if (msg.ttl && msg.timestamp) {
  const age = Date.now() - msg.timestamp;

  if (age > msg.ttl) {
    throw new HubError(
      'message_expired',
      `Message expired (age: ${age}ms, ttl: ${msg.ttl}ms)`,
      { age, ttl: msg.ttl }
    );
  }
}
```

**Client Strategy:**
```typescript
// Don't retry expired messages
try {
  await this.send(message);
} catch (err) {
  if (err.code === 'message_expired') {
    console.log('Message expired - not retrying');
    return; // Don't retry
  }
  throw err;
}
```

**Outcome:** Expired messages dropped, preventing stale data delivery

## 6. Hibernation and Eviction

### 6.1 WebSocket Hibernation Behavior

**Fact:** Cloudflare hibernates WebSockets automatically after inactivity. This is NOT preventable.

**Hibernation Characteristics:**
- WebSocket enters low-power state
- Automatic wake on incoming message
- In-memory state (registry, sessions) **persists**
- No data loss during hibernation

**Server Impact:** None - hibernation is transparent to application code

**Client Impact:** None - messages still delivered, with potential latency increase

### 6.2 Durable Object Eviction (Not Hibernation!)

**Eviction Triggers:**
- No requests for 30+ seconds (configurable)
- Manual eviction by Cloudflare
- Deployment or code update

**Effects:**
- All WebSocket connections close (code 1001: "Going Away")
- All in-memory state lost (registry, sessions, subscriptions)
- Clients must reconnect and re-register

**Server Behavior on Eviction:**
```typescript
// No explicit handler - eviction is abrupt
// All WebSockets receive close(1001)
```

**Client Auto-Reconnect:**
```typescript
ws.onclose = (event) => {
  console.log(`WebSocket closed: code=${event.code}, reason=${event.reason}`);

  if (event.code === 1001) { // Going Away
    console.log('Server evicted - reconnecting...');
    setTimeout(() => this.connect(), this.getReconnectDelay());
  }
};

async connect() {
  this.ws = new WebSocket(this.url);

  this.ws.onopen = async () => {
    await this.send({ type: 'hub:connect', ... });
    await this.register({ ... }); // Re-register

    // Re-subscribe to all topics
    for (const topic of this.subscribedTopics) {
      await this.subscribe({ topic });
    }
  };
}
```

**Outcome:** Clients automatically recover from evictions

## 7. Authentication Failures

### 7.1 JWT Expiration Mid-Session

**Scenario:** Client connects with valid JWT, session lasts 2 hours, JWT expires after 1 hour.

**Current Behavior:** JWT validated only during `hub:connect`, not on subsequent messages.

**Risk:** Expired credentials used for active session.

**Mitigation (Future):**
```typescript
// Periodic token refresh
setInterval(async () => {
  if (this.sessionExpiresSoon()) {
    const newToken = await this.refreshToken();
    await this.send({
      type: 'hub:refresh_token',
      payload: { jwt: newToken }
    });
  }
}, 300_000); // Every 5 minutes
```

**Current Workaround:** Keep JWT validity > expected session duration (e.g., 24 hours)

### 7.2 Invalid JWT on Connect

**Scenario:** Client provides malformed or unsigned JWT.

**Server Behavior:**
```typescript
// In handleConnect()
if (this.env.AUTH_ENABLED === 'true') {
  if (!msg.payload.jwt) {
    throw new HubError('unauthorized', 'JWT required');
  }

  try {
    const payload = await validateJWT(msg.payload.jwt, this.env.JWT_SECRET);
    session.actorIdentity = toCanonicalAddress(payload.actorId);
    session.capabilities = payload.capabilities;
    session.authenticated = true;
  } catch (err) {
    throw new HubError('unauthorized', `Invalid JWT: ${err.message}`);
  }
}
```

**Client Recovery:**
```typescript
try {
  await this.connect();
} catch (err) {
  if (err.code === 'unauthorized') {
    // Re-authenticate with identity provider
    const newToken = await this.authenticate();
    await this.connect(newToken);
  }
}
```

**Outcome:** Invalid credentials rejected at connection time

## 8. Thundering Herd Scenarios

### 8.1 Mass Reconnection After Server Restart

**Scenario:** Signal Hub restarts. All 10,000 actors attempt to reconnect simultaneously.

**Problem:** Connection flood exceeds server capacity, causes cascading failures.

**Client-Side Mitigation (Jittered Backoff):**
```typescript
private getReconnectDelay(attempt: number): number {
  const baseDelay = Math.min(
    100 * Math.pow(2, attempt), // Exponential: 100ms, 200ms, 400ms, ...
    30_000 // Max 30 seconds
  );

  // Add 25% random jitter
  const jitter = baseDelay * 0.25 * (Math.random() - 0.5);

  return baseDelay + jitter;
}
```

**Server-Side Mitigation (Connection Rate Limiting - Future):**
```typescript
// Leaky bucket for connection rate
interface ConnectionRateLimiter {
  maxConnectionsPerSecond: 1000;
  bucket: number;
  lastRefill: number;
}

async handleWebSocketUpgrade(request: Request) {
  if (!this.tryConsumeConnectionToken()) {
    return new Response('Service Unavailable: Rate Limited', { status: 429 });
  }

  return this.acceptWebSocket(request);
}
```

**Outcome:** Reconnections spread over time, avoiding server overload

## 9. Cross-Reference

See also:
- **PROTOCOL.spec.md** - Normal message flows
- **STATE_MACHINE.spec.md** - State transitions
- **SCHEMAS.spec.md** - Type definitions
- **SERVER.spec.md** - Server implementation details
- **CLIENT.spec.md** - Client retry logic
