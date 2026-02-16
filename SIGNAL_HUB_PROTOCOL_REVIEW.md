# Signal Hub Protocol Design: Practical Implementation Review

**Date:** 2026-02-16
**Reviewer:** Claude Code
**Status:** Critical Issues Identified
**Risk Assessment:** MEDIUM-HIGH (rescoping recommended)

---

## Executive Summary

The Signal Hub Protocol Design plan is architecturally sound but **lacks critical implementation detail for Cloudflare Workers constraints and production resilience**. Key risks stem from:

1. **Underspecified error handling** in message delivery
2. **Naive fan-out assumptions** without backpressure/throttling
3. **WebSocket hibernation incompleteness** (plan assumes standard WebSocket API)
4. **No explicit handling of DurableObject CPU/storage limits**
5. **Registration schema is incomplete** (missing capabilities, versioning)

**Recommendation:** Before implementation, expand Phase 3-5 to include:
- Explicit backpressure/queue management strategy
- DurableObject cost model and scaling limits
- Message size validation and chunking strategy
- Connection state machine with full edge case coverage

---

## 1. IMPLEMENTATION RISK ASSESSMENT

### Overall Risk: MEDIUM-HIGH

| Component | Risk | Impact | Mitigation |
|-----------|------|--------|-----------|
| WebSocket lifecycle | **HIGH** | Connection thrashing, memory leaks | Explicit hibernation strategy required |
| Broadcast fan-out | **HIGH** | CPU limit exceeded, message loss | Backpressure + async batch needed |
| Registration consistency | **MEDIUM** | Duplicate/stale registrations | Versioning + heartbeat validation |
| Message delivery | **MEDIUM** | Silent failures, duplicate processing | Explicit delivery guarantee model |
| DurableObject scaling | **MEDIUM** | Unexpected costs, capacity exhaustion | Per-instance actor limits required |

---

## 2. TOP 5 EDGE CASES TO HANDLE

### Edge Case #1: WebSocket Disconnect During Message Send
**Scenario:** Actor sends message to Signal Hub, WebSocket drops mid-frame, actor retries before reconnect completes.

**Problem in Current Design:**
- No mention of how `send()` distinguishes "message accepted by server" from "server acknowledged message"
- Phase 5 says "At-most-once for MVP" but doesn't specify detection mechanism
- If Socket closes between `ws.send()` and server `onMessage()`, message is lost with no indication

**Implementation Required:**
```typescript
// Need explicit server-side ACK for critical messages
interface SignalMessage {
  id: string;           // UUID for deduplication
  type: string;
  payload: unknown;
  ttl: number;          // Milliseconds, not just a field
  requiresAck?: boolean; // For critical messages
}

// Client-side tracking
private pendingMessages = new Map<string, PendingMessage>();
private ackTimeout = 5000; // ms

private async sendWithRetry(msg: SignalMessage, maxRetries = 3) {
  for (let attempt = 0; attempt < maxRetries; attempt++) {
    const promise = new Promise((resolve, reject) => {
      this.pendingMessages.set(msg.id, {
        resolve,
        reject,
        timer: setTimeout(
          () => reject(new Error('ACK timeout')),
          this.ackTimeout
        )
      });
    });

    try {
      this.ws.send(JSON.stringify(msg));
      await promise;
      return; // Success
    } catch (e) {
      if (attempt === maxRetries - 1) throw e;
      await this.exponentialBackoff(attempt);
    }
  }
}
```

**Risk Without This:** Silent message loss. Actors won't know if their registration persisted.

---

### Edge Case #2: Duplicate Registrations (Same Actor, Multiple Connections)
**Scenario:** Actor restarts, reconnects before old connection times out. Signal Hub receives two `register` messages for same actor address.

**Problem in Current Design:**
- Phase 3 defines registration schema but doesn't mention version/generation tracking
- No specification for handling: "Is this the same actor or a fork?"
- DurableObject state will have two entries if TTL-based cleanup isn't explicit

**Implementation Required:**
```typescript
// Registration schema expansion (Phase 3)
interface ActorRegistration {
  actorAddress: string;
  capabilities: string[];
  metadata: Record<string, unknown>;
  connectionState: 'connecting' | 'connected' | 'reconnecting' | 'disconnected';

  // NEW: Lifecycle tracking
  version: number;                    // Incremented on each registration
  connectionId: string;               // Unique per WebSocket connection
  registeredAt: number;               // Timestamp
  lastHeartbeat: number;              // Latest heartbeat received
  ttl: number;                        // Max 300_000 (5 minutes for MVP)

  // NEW: Conflict resolution
  formerConnectionId?: string;        // Previous connection if re-registering
  metadata?: {
    restartCount?: number;
    clientVersion?: string;
    // ...
  }
}

// Registration deduplication
private handleRegister(msg: SignalMessage): void {
  const addr = msg.payload.actorAddress;
  const existing = this.actors.get(addr);

  if (existing && existing.connectionId !== msg.metadata.connectionId) {
    // Fork detected
    if (existing.lastHeartbeat > Date.now() - 10_000) {
      // Old connection still active — reject or warn
      return this.send(msg.from, 'error', {
        code: 'DUPLICATE_REGISTRATION',
        message: `Actor ${addr} already registered from another connection`,
        retryAfter: 5000
      });
    } else {
      // Old connection stale — replace
      this.actors.set(addr, {
        ...msg.payload,
        version: (existing.version ?? 0) + 1,
        connectionId: msg.metadata.connectionId,
        formerConnectionId: existing.connectionId,
        registeredAt: Date.now(),
        lastHeartbeat: Date.now(),
      });
    }
  } else {
    // First registration or same connection re-registering
    this.actors.set(addr, {
      ...msg.payload,
      version: (existing?.version ?? 0) + 1,
      connectionId: msg.metadata.connectionId,
      registeredAt: Date.now(),
      lastHeartbeat: Date.now(),
    });
  }
}
```

**Risk Without This:** Broadcast sends duplicate messages; stale registrations cause hard-to-debug failures.

---

### Edge Case #3: Broadcast Fan-Out Exceeds DurableObject CPU Limit
**Scenario:** 10,000 actors registered. Hub broadcasts to all via subscription list. Each WebSocket `send()` takes ~1ms. Total: 10s of CPU.

**Problem in Current Design:**
- Phase 7 says "Per-connection FIFO, no global ordering" but doesn't address fan-out serialization
- Phase 2 lists `broadcast` type but no backpressure mechanism
- No mention of Cloudflare's 30s CPU limit per request

**Implementation Required:**
```typescript
// Broadcast with async batching
private async broadcast(message: unknown): Promise<void> {
  const sockets = this.ctx.getWebSockets();
  const batchSize = 100;

  for (let i = 0; i < sockets.length; i += batchSize) {
    const batch = sockets.slice(i, i + batchSize);
    const serialized = JSON.stringify(message);

    for (const ws of batch) {
      try {
        ws.send(serialized);
      } catch {
        // Log but don't throw — one socket failure shouldn't block others
      }
    }

    // Yield to other requests
    if (i + batchSize < sockets.length) {
      await new Promise(r => setTimeout(r, 0));
    }
  }
}

// Alternative: Use durable queue for large broadcasts
interface PendingBroadcast {
  message: unknown;
  recipients: string[]; // Actor addresses, not WebSocket refs
  createdAt: number;
  attempts: number;
}

private broadcasts = new Queue<PendingBroadcast>();

async handleBroadcast(msg: SignalMessage) {
  // Don't send immediately—queue for async processing
  await this.broadcasts.send({
    message: msg.payload,
    recipients: Array.from(this.actors.keys()),
    createdAt: Date.now(),
    attempts: 0
  });

  return this.send(msg.from, 'broadcast-queued', { id: msg.id });
}
```

**Risk Without This:** Broadcast exceeds CPU limit → DurableObject evicts → all connections drop → "cascade failure".

---

### Edge Case #4: Actor Registry Storage Explosion
**Scenario:** 100,000 actors connect over a week. Each registration is ~500 bytes. Total: 50MB. DurableObject storage limit is typically 1-10GB but costs $0.25/1M PUT operations.

**Problem in Current Design:**
- No mention of storage strategy (in-memory vs persistent)
- Phase 3 shows schema but no TTL management strategy
- No discussion of PUT operation costs

**Implementation Required:**
```typescript
// Memory-efficient actor registry with TTL
interface ActorRegistry {
  // Volatile: in-memory, cleared on restart
  volatile: Map<string, ActorRegistration>; // 100K actors

  // Durable: persisted, but only critical metadata
  durable: DurableObjectStorage; // Only active last 24h
}

// Two-tier strategy
private volatile = new Map<string, ActorRegistration>();

async handleRegister(msg: SignalMessage) {
  const addr = msg.payload.actorAddress;
  const key = `actor:${addr}`;

  // Always update memory
  this.volatile.set(addr, { ...msg.payload, lastHeartbeat: Date.now() });

  // Only persist if it's a "new" registration or if we're under quota
  const existing = await this.ctx.storage?.get<ActorRegistration>(key);
  if (!existing || Date.now() - existing.registeredAt > 3600_000) {
    // Persist this registration (once per hour per actor max)
    await this.ctx.storage?.put(key, {
      ...msg.payload,
      registeredAt: Date.now(),
      version: (existing?.version ?? 0) + 1
    }, { expirationTtl: 86400 }); // Expire after 24h
  }
}

// Garbage collection
private async cleanup() {
  const now = Date.now();
  const stale: string[] = [];

  for (const [addr, reg] of this.volatile) {
    if (now - reg.lastHeartbeat > 300_000) { // 5 minutes
      stale.push(addr);
    }
  }

  for (const addr of stale) {
    this.volatile.delete(addr);
    await this.ctx.storage?.delete(`actor:${addr}`);
  }
}
```

**Risk Without This:** Runaway storage costs ($25+/month) or storage exhaustion.

---

### Edge Case #5: Heartbeat Timeout Race Condition
**Scenario:** Actor sends heartbeat, immediately crashes. Hub records heartbeat, cancels timeout, but never hears from actor again until TTL expires.

**Problem in Current Design:**
- Phase 4 mentions "State: connecting, connected, reconnecting, disconnected" but doesn't specify heartbeat rules
- No explicit mention of heartbeat interval vs TTL
- Race: what if heartbeat arrives after cleanup started?

**Implementation Required:**
```typescript
interface HeartbeatConfig {
  interval: 30_000;      // Client sends every 30s
  timeout: 60_000;       // Server expects one within 60s
  ttl: 300_000;          // Total connection TTL
}

private heartbeatTimers = new Map<string, NodeJS.Timeout>();

handleHeartbeat(msg: SignalMessage) {
  const addr = msg.payload.actorAddress;

  // Clear existing timer
  const existingTimer = this.heartbeatTimers.get(addr);
  if (existingTimer) {
    clearTimeout(existingTimer);
  }

  // Update registration
  const reg = this.volatile.get(addr);
  if (reg) {
    reg.lastHeartbeat = Date.now();
  }

  // Set new timeout
  const timer = setTimeout(() => {
    // Heartbeat expired
    this.volatile.delete(addr);
    this.heartbeatTimers.delete(addr);
    console.log(`Heartbeat timeout: ${addr}`);
  }, HeartbeatConfig.timeout);

  this.heartbeatTimers.set(addr, timer);
}

// CRITICAL: On actor disconnect, also clear timer
handleDisconnect(addr: string) {
  const timer = this.heartbeatTimers.get(addr);
  if (timer) {
    clearTimeout(timer);
    this.heartbeatTimers.delete(addr);
  }
  this.volatile.delete(addr);
}
```

**Risk Without This:** Memory leak of timeouts, stale actors persisting indefinitely, false "reconnecting" states.

---

## 3. CLOUDFLARE-SPECIFIC GOTCHAS

### Gotcha #1: WebSocket Hibernation API Behavior
**Issue:** Plan says "Uses WebSocket for connections" but doesn't specify hibernation mode.

Cloudflare's **Hibernatable WebSocket API** has different semantics than standard Node.js WebSocket:
- Messages arrive asynchronously; no guaranteed in-order delivery across hibernation cycles
- `ctx.acceptWebSocket()` **must** be called in request handler
- Sockets automatically pause if no message for 30s; resume on new message (can lose in-flight data)

**Current Code Reality (from websocket-bridge.ts):**
```typescript
this.ctx.acceptWebSocket(server, tags);
// This returns a HIBERNATABLE socket, not a standard socket
```

**What the plan is missing:**
```typescript
// Specify hibernation strategy explicitly
interface SignalHubConfig {
  hibernation?: {
    enabled: boolean;          // Default: true
    pauseTimeoutMs: 30_000;    // When to pause
    maxMessageBuffer: 1000;    // Buffered messages before rejection
    flushInterval: 5_000;      // Async flush of buffered messages
  };
}

// If hibernation pauses, what happens to broadcasts?
// Plan doesn't say. Implementation options:
// A) Buffer in-memory (loses on eviction)
// B) Queue to durable queue (latency hit)
// C) Send on wake-up (could be 30s old)
// D) Reject new messages while paused (reliability)
```

**Risk:** Implement without hibernation strategy → actors lose broadcast messages during pause cycles.

---

### Gotcha #2: DurableObject CPU Exhaustion During Actor Discovery
**Issue:** Phase 2 mentions `discover` and `list_actors` message types.

If 10,000 actors are registered and one sends `list_actors`:
```typescript
// Current plan doesn't specify what this returns:
// Option A: Entire registry as JSON (~5MB)
// Option B: Paginated (needs cursor implementation)
// Option C: Filtered (but spec doesn't define filters)

// Each option has CPU cost:
// A: 1-2s to serialize, 5MB+ network cost
// B: Complex, requires persistent cursor state
// C: Incomplete spec
```

**Implementation Required:**
```typescript
// Phase 2 expansion needed: Discovery Query Language
interface DiscoveryQuery {
  pattern?: string;              // Regex on actor address
  capabilities?: string[];       // Actors with ALL these
  limit?: number;                // Max results (default: 100)
  offset?: number;               // Pagination cursor
  metadata?: Record<string, any>; // Filter on metadata fields
}

async handleDiscover(msg: SignalMessage) {
  const query = msg.payload as DiscoveryQuery;
  const limit = Math.min(query.limit ?? 100, 1000); // Cap at 1000

  let results: ActorRegistration[] = [];
  for (const reg of this.volatile.values()) {
    if (this.matchesQuery(reg, query)) {
      results.push(reg);
      if (results.length >= limit) break;
    }
  }

  return this.send(msg.from, 'discovery-result', {
    results,
    hasMore: results.length === limit,
    count: results.length,
  });
}

private matchesQuery(reg: ActorRegistration, query: DiscoveryQuery): boolean {
  if (query.pattern) {
    const regex = new RegExp(query.pattern);
    if (!regex.test(reg.actorAddress)) return false;
  }

  if (query.capabilities?.length) {
    for (const cap of query.capabilities) {
      if (!reg.capabilities.includes(cap)) return false;
    }
  }

  if (query.metadata) {
    for (const [key, value] of Object.entries(query.metadata)) {
      if (reg.metadata?.[key] !== value) return false;
    }
  }

  return true;
}
```

**Risk:** Simple `list_actors` response → CPU limit exceeded → DurableObject crash.

---

### Gotcha #3: Message Size Limits & Serialization
**Issue:** Plan specifies "JSON serialization for MVP" but doesn't validate message sizes.

Cloudflare Workers constraints:
- **WebSocket frame limit:** 1MB per message
- **Request body limit:** 100MB total (not a per-message issue, but relevant for bulk ops)
- **DurableObject storage value:** 128KB limit per key

**Implementation Required:**
```typescript
// Message validation (missing from Phase 2)
interface SignalHubMessageSpec {
  maxPayloadSize: 512_000;        // 512KB (leave 512KB for envelope)
  maxBroadcastRecipients: 10_000;  // Rough estimate
  serializationFormat: 'json' | 'binary'; // Binary in Phase 3+?
}

// Validate on send
async send(recipient: string, message: unknown): Promise<void> {
  const serialized = JSON.stringify(message);

  if (serialized.length > SignalHubMessageSpec.maxPayloadSize) {
    throw new Error(
      `Message too large: ${serialized.length} bytes ` +
      `(max: ${SignalHubMessageSpec.maxPayloadSize})`
    );
  }

  this.ws.send(serialized);
}

// Validate on receive
handleMessage(data: string) {
  if (data.length > SignalHubMessageSpec.maxPayloadSize) {
    return this.send(msg.from, 'error', {
      code: 'PAYLOAD_TOO_LARGE',
      maxSize: SignalHubMessageSpec.maxPayloadSize
    });
  }
}
```

**Risk:** Send large payload → WebSocket frame split → incomplete message → silent failure.

---

### Gotcha #4: Storage Durability & Request Isolation
**Issue:** Plan mentions registration schema but doesn't address request isolation.

In Durable Objects, mutations are **isolated per request**:
```typescript
// Request 1
this.volatile.set('actor-1', registration);  // Memory only
await this.ctx.storage?.put('actor-1', ...); // Durable

// Request 2 (concurrent)
const val = this.volatile.get('actor-1');    // Might be undefined!
```

The two requests see different in-memory state. **This is correct and necessary**, but the plan doesn't acknowledge it.

**Implementation Required:**
```typescript
// Must handle concurrent requests properly
interface SignalHubState {
  // Volatile per-request only
  localHeartbeatTimers: Map<string, NodeJS.Timeout>;

  // Durable (persisted)
  storage: DurableObjectStorage;
}

async handleHeartbeat(msg: SignalMessage) {
  const addr = msg.payload.actorAddress;

  // Read-check-write pattern is NOT atomic
  const existing = await this.ctx.storage?.get<ActorRegistration>(`actor:${addr}`);
  if (existing && existing.version > msg.payload.version) {
    // Stale heartbeat from old version
    return this.send(msg.from, 'error', { code: 'STALE_VERSION' });
  }

  // Update storage (atomic put)
  await this.ctx.storage?.put(`actor:${addr}`, {
    ...existing,
    lastHeartbeat: Date.now(),
  });
}
```

**Risk:** Race condition on concurrent registrations/heartbeats if plan assumes in-memory state is globally consistent.

---

### Gotcha #5: Actor Shutdown & Graceful Cleanup
**Issue:** No mention of what happens when Signal Hub DurableObject is evicted or restarted.

Cloudflare evicts DurableObjects after:
- Request handler timeout (30s)
- No requests for 30 seconds (can be configured)
- Manual eviction

**What gets lost:**
- All volatile state (pending messages, heartbeat timers, broadcast queues)
- WebSocket connections (all actors reconnect)

**Implementation Required:**
```typescript
// Graceful shutdown protocol
private async beforeEviction() {
  // Close all WebSockets with code 1001 (going away)
  const sockets = this.ctx.getWebSockets();
  for (const ws of sockets) {
    ws.close(1001, 'Signal Hub restarting');
  }

  // Persist any pending state
  await this.ctx.storage?.put('last-checkpoint', {
    registryVersion: this.registryVersion,
    broadcastQueue: Array.from(this.broadcasts),
    checkpointAt: Date.now(),
  }, { expirationTtl: 3600 });
}

// Reconnect recovery protocol
private async restoreFromCheckpoint() {
  const checkpoint = await this.ctx.storage?.get('last-checkpoint');
  if (checkpoint) {
    // Actors know to reconnect; we don't need to restore volatile state
    // But we do need to restore any persisted registrations
    const keys = await this.ctx.storage?.list({ prefix: 'actor:' });
    for (const key of keys ?? []) {
      const reg = await this.ctx.storage?.get<ActorRegistration>(key);
      if (reg && Date.now() - reg.lastHeartbeat < 300_000) {
        this.volatile.set(reg.actorAddress, reg);
      }
    }
  }
}
```

**Risk:** No explicit shutdown handling → actors left in "connected" state → retry logic kicks in → cascade reconnection.

---

## 4. SCALABILITY CONCERNS & RECOMMENDATIONS

### Question: How many concurrent actors per Signal Hub instance?

**Current Plan:** Doesn't specify.

**Analysis:**
```
Memory constraints:
- Per-actor metadata: ~500 bytes
- Heartbeat timer: ~200 bytes
- Total per actor: ~700 bytes
- DO memory: ~128MB (soft limit)
- Theoretical capacity: 180,000 actors

However, practical limits are much lower:

Request concurrency:
- Cloudflare DO: ~10-50 concurrent requests (typical)
- Each heartbeat is ~10ms processing
- Throughput: 1,000-5,000 heartbeats/second per instance

Storage throughput:
- Each registration = 1 PUT operation (~$0.25 per million)
- 10,000 registrations/second = $250/day

RECOMMENDATION: Single instance → max 50,000 actors
```

**Proposed Scaling Strategy:**
```typescript
interface SignalHubShardingConfig {
  // Shard by actor address hash
  shardCount: number;

  // Address -> shard mapping
  getShard(addr: string): number {
    const hash = addr.split('').reduce((h, c) => {
      return ((h << 5) - h) + c.charCodeAt(0);
    }, 0);
    return Math.abs(hash) % this.shardCount;
  }

  // Actor must connect to correct shard
  getHubId(addr: string): DurableObjectId {
    const shard = this.getShard(addr);
    return this.hubNamespace.idFromName(`signal-hub-${shard}`);
  }
}

// Router/load-balancer worker
async handleActorMessage(req: Request) {
  const body = await req.json() as { actorAddress: string };
  const shardId = config.getShard(body.actorAddress);

  const hubStub = env.SIGNAL_HUB_SHARDS.get(
    config.hubNamespace.idFromName(`signal-hub-${shardId}`)
  );

  return hubStub.fetch(req);
}
```

### Question: What's the fan-out limit for broadcast?

**Current Plan:** Doesn't address.

**Analysis:**
```
Broadcasting to all 50,000 actors:
- Send time: 50,000 WebSocket sends × 1ms = 50 seconds
- Problem: Exceeds 30s CPU limit

Solution: Async batch + durable queue

With 100-socket batches + 5ms yields:
- 500 batches × 5ms = 2.5 seconds
- Safe, but still significant
- Queue overhead: ~100ms per broadcast

RECOMMENDATION: Single broadcast ~1000 actors per second
```

**Implementation (required in Phase 5):**
```typescript
interface BroadcastQueue {
  pending: Array<{ message: unknown; recipients: string[]; createdAt: number }>;
  processing: boolean;
}

async handleBroadcast(msg: SignalMessage) {
  const recipients = Array.from(this.volatile.keys()); // All registered actors

  // Queue for async processing
  this.broadcastQueue.pending.push({
    message: msg.payload,
    recipients,
    createdAt: Date.now(),
  });

  if (!this.broadcastQueue.processing) {
    this.processBroadcastQueue();
  }
}

private async processBroadcastQueue() {
  this.broadcastQueue.processing = true;

  while (this.broadcastQueue.pending.length > 0) {
    const item = this.broadcastQueue.pending[0];

    try {
      await this.broadcastToList(item.recipients, item.message);
      this.broadcastQueue.pending.shift();
    } catch (e) {
      console.error('Broadcast error:', e);
      if (item.createdAt < Date.now() - 60_000) {
        // Older than 1 minute, give up
        this.broadcastQueue.pending.shift();
      } else {
        break;
      }
    }
  }

  this.broadcastQueue.processing = false;
}

private async broadcastToList(addrs: string[], message: unknown) {
  const batchSize = 100;
  const serialized = JSON.stringify(message);

  for (let i = 0; i < addrs.length; i += batchSize) {
    const batch = addrs.slice(i, i + batchSize);
    const sockets = this.ctx.getWebSockets();

    for (const ws of sockets) {
      try {
        ws.send(serialized);
      } catch {
        // Skip failed sockets
      }
    }

    // Yield
    await new Promise(r => setTimeout(r, 5));
  }
}
```

### Question: How to handle thundering herd (many actors reconnecting)?

**Current Plan:** Connection state machine mentioned but no throttling.

**Risk Scenario:**
```
Signal Hub restart (1s eviction)
↓
All 50,000 actors get connection:close (code 1001)
↓
Each actor has exponential backoff starting at 100ms
↓
But if backoff jitter isn't good, 30,000 actors try to reconnect at t=500ms
↓
DurableObject receives 30,000 WebSocket upgrade requests
↓
CPU limit exceeded, evicts again
↓
Cascade failure
```

**Implementation Required:**
```typescript
// Client-side backoff (each actor)
private reconnectConfig = {
  initialDelayMs: 100,
  maxDelayMs: 30_000,
  multiplier: 2,
  jitterFraction: 0.25, // 25% jitter
};

async reconnect(attempt: number) {
  // Exponential backoff with jitter
  const delay = Math.min(
    this.reconnectConfig.initialDelayMs * Math.pow(this.reconnectConfig.multiplier, attempt),
    this.reconnectConfig.maxDelayMs
  );

  const jitter = delay * this.reconnectConfig.jitterFraction * (Math.random() - 0.5);
  const totalDelay = delay + jitter;

  await new Promise(r => setTimeout(r, totalDelay));

  try {
    await this.connect();
  } catch (e) {
    if (attempt < 10) { // Max 10 attempts
      return this.reconnect(attempt + 1);
    } else {
      throw new Error(`Failed to reconnect after 10 attempts: ${e}`);
    }
  }
}

// Server-side connection rate limiting (required)
private connectionRateLimiter = {
  maxConnectionsPerSecond: 1000,
  bucket: 0,
  lastRefill: Date.now(),
};

async acceptWebSocket(request: Request) {
  const now = Date.now();
  const elapsed = now - this.connectionRateLimiter.lastRefill;

  // Refill bucket (leaky bucket algorithm)
  this.connectionRateLimiter.bucket +=
    (elapsed / 1000) * this.connectionRateLimiter.maxConnectionsPerSecond;
  this.connectionRateLimiter.bucket = Math.min(
    this.connectionRateLimiter.bucket,
    this.connectionRateLimiter.maxConnectionsPerSecond
  );
  this.connectionRateLimiter.lastRefill = now;

  if (this.connectionRateLimiter.bucket < 1) {
    // Rate limit exceeded
    return new Response('Service Unavailable: Rate Limited', { status: 429 });
  }

  this.connectionRateLimiter.bucket--;
  return this.handleWebSocketUpgrade(request);
}
```

**Risk Without This:** Single DO restart cascades to complete system failure for 1-2 minutes.

---

## 5. TESTING STRATEGY (Prioritized)

### Priority 1: Reconnection Scenarios
**Why:** Most fragile; not easily caught in unit tests.

**Test Plan:**
```typescript
describe('Signal Hub - Reconnection', () => {
  it('should handle WebSocket drop mid-message', async () => {
    const hub = new SignalHubDurableObject();
    const client = new SignalHubClient();

    // Register actor
    await client.register({ actorAddress: 'test-actor' });

    // Start a send, then drop connection
    let sendPromise: Promise<void>;
    client.ws.addEventListener('message', () => {
      // Connection established
      sendPromise = client.send({ type: 'test', payload: {} });
      client.ws.close(1006, 'Connection lost'); // Abnormal
    });

    // Client should retry
    await client.waitForReconnect();
    expect(client.isConnected).toBe(true);

    // Message should still be pending or lost (but not silently)
    try {
      await sendPromise!;
      // Either succeeded or threw — both acceptable
    } catch (e) {
      expect(e.message).toContain('connection lost');
    }
  });

  it('should deduplicate reconnecting actors', async () => {
    const hub = new SignalHubDurableObject();
    const client1 = new SignalHubClient('actor-1');
    const client2 = new SignalHubClient('actor-1'); // Same address

    // Register from both
    await client1.register();
    await client2.register();

    // Wait for race to settle
    await new Promise(r => setTimeout(r, 100));

    // Only one should be registered
    const list = await hub.listActors();
    expect(list.filter(a => a.actorAddress === 'actor-1')).toHaveLength(1);
  });

  it('should handle reconnect backoff properly', async () => {
    const client = new SignalHubClient();
    const connectionTimes: number[] = [];

    // Break connection 3 times
    for (let i = 0; i < 3; i++) {
      await client.connect();
      connectionTimes.push(Date.now());
      client.ws.close(1006);
      await new Promise(r => setTimeout(r, 100));
    }

    // Verify backoff timing (with jitter tolerance)
    const delays = [
      connectionTimes[1] - connectionTimes[0],
      connectionTimes[2] - connectionTimes[1],
    ];

    expect(delays[0]).toBeLessThan(200); // Initial: 100-200ms with jitter
    expect(delays[1]).toBeGreaterThan(100);
    expect(delays[1]).toBeLessThan(500); // Exponential: 100-500ms with jitter
  });
});
```

### Priority 2: Message Delivery Guarantees
**Why:** "At-most-once" needs verification; silent losses are catastrophic.

**Test Plan:**
```typescript
describe('Signal Hub - Message Delivery', () => {
  it('should not duplicate messages on resend', async () => {
    const hub = new SignalHubDurableObject();
    const client = new SignalHubClient();

    let receiveCount = 0;
    client.on('message', () => { receiveCount++; });

    // Simulate: client sends, gets ACK, but connection breaks before client sees ACK
    await client.connect();

    const msgId = crypto.randomUUID();
    const sendPromise = client.send(
      { id: msgId, type: 'test', payload: {}, requiresAck: true },
      { timeout: 1000 }
    );

    // Hub receives and ACKs
    await new Promise(r => setTimeout(r, 50));
    client.ws.close(); // Drop after ACK sent but before client received

    // Client retries
    await client.waitForReconnect();
    const retryPromise = client.send(
      { id: msgId, type: 'test', payload: {} },
      { timeout: 1000 }
    );

    await Promise.all([sendPromise, retryPromise]).catch(() => {});

    // Should only receive once (or error)
    expect(receiveCount).toBeLessThanOrEqual(1);
  });

  it('should lose messages with at-most-once guarantee', async () => {
    const hub = new SignalHubDurableObject();
    const client = new SignalHubClient();

    const sent: string[] = [];
    const received: string[] = [];

    client.on('message', (msg: any) => {
      received.push(msg.id);
    });

    // Send 1000 messages while randomly dropping connection
    for (let i = 0; i < 1000; i++) {
      const msgId = `msg-${i}`;
      sent.push(msgId);

      try {
        await client.send({ id: msgId, type: 'test', payload: {} });
      } catch (e) {
        // Connection lost during send — expected sometimes
      }

      if (Math.random() < 0.05) { // 5% chance drop
        client.ws.close();
        await client.waitForReconnect();
      }
    }

    // Verify: received <= sent (at-most-once)
    const receivedSet = new Set(received);
    for (const msg of received) {
      expect(sent).toContain(msg);
    }

    console.log(`Sent: ${sent.length}, Received: ${received.length}, Loss rate: ${((sent.length - received.length) / sent.length * 100).toFixed(1)}%`);
  });
});
```

### Priority 3: Fan-Out & Backpressure
**Why:** Broadcast is the highest-risk operation for CPU exhaustion.

**Test Plan:**
```typescript
describe('Signal Hub - Broadcast', () => {
  it('should handle broadcast to 10k actors without CPU timeout', async () => {
    const hub = new SignalHubDurableObject();
    const cpuTimes: number[] = [];

    // Register 10,000 actors
    for (let i = 0; i < 10_000; i++) {
      const client = new SignalHubClient(`actor-${i}`);
      await client.register();
    }

    // Broadcast
    const startCpu = Date.now();
    const result = await hub.broadcast({
      type: 'test-broadcast',
      payload: { size: 1024 }, // 1KB payload
    });
    const endCpu = Date.now();

    expect(endCpu - startCpu).toBeLessThan(30_000); // Under CPU timeout
    expect(result.failed).toBe(0); // Or < 10 (acceptable)
  });

  it('should queue broadcasts if queue fills up', async () => {
    const hub = new SignalHubDurableObject({
      broadcastQueueLimit: 100,
    });

    // Register 10k actors
    for (let i = 0; i < 10_000; i++) {
      const client = new SignalHubClient(`actor-${i}`);
      await client.register();
    }

    // Send 200 broadcasts rapidly
    const promises = [];
    for (let i = 0; i < 200; i++) {
      promises.push(
        hub.broadcast({ type: 'broadcast', payload: { index: i } })
      );
    }

    const results = await Promise.allSettled(promises);

    // Some should queue (or reject with 429)
    const queued = results.filter(r =>
      r.status === 'fulfilled' && (r.value as any).queued
    );
    const rejected = results.filter(r => r.status === 'rejected');

    expect(queued.length + rejected.length).toBeGreaterThan(0);
  });
});
```

### Priority 4: Registration Consistency
**Why:** Duplicates cause silent failures downstream.

**Test Plan:**
```typescript
describe('Signal Hub - Registration', () => {
  it('should detect and resolve duplicate registrations', async () => {
    const hub = new SignalHubDurableObject();

    // Register same actor twice from different connections
    const client1 = new SignalHubClient('actor-1', { connectionId: 'conn-1' });
    const client2 = new SignalHubClient('actor-1', { connectionId: 'conn-2' });

    await client1.register({ capabilities: ['send', 'receive'] });
    await new Promise(r => setTimeout(r, 10));
    await client2.register({ capabilities: ['send'] });

    // Hub should either:
    // A) Reject one (error)
    // B) Keep newest (client2)
    const reg = await hub.getActorRegistration('actor-1');

    expect(reg.connectionId).toBe('conn-2');
    expect(reg.version).toBe(2);
    expect(reg.formerConnectionId).toBe('conn-1');
  });

  it('should expire stale registrations', async () => {
    const hub = new SignalHubDurableObject({
      heartbeatConfig: {
        timeout: 100, // 100ms for test
      }
    });

    const client = new SignalHubClient('actor-1');
    await client.register();

    // No heartbeat for 200ms
    await new Promise(r => setTimeout(r, 200));

    // Should be gone
    const reg = await hub.getActorRegistration('actor-1');
    expect(reg).toBeUndefined();
  });
});
```

### Priority 5: Load Testing
**Why:** Integration test covering realistic scale.

**Test Plan:**
```bash
# Load test script
npm run test:load -- \
  --actors 10000 \
  --duration 300 \
  --message-rate 1000 \
  --broadcast-rate 10 \
  --reconnect-rate 0.01

# Expected metrics (from test output):
# - Latency p99: <100ms
# - Message loss rate: <0.1%
# - Broadcast completion time: <5s per 10k actors
# - CPU time: <20s per 30s window
# - Storage: <100MB
```

---

## 6. DEVELOPER EXPERIENCE IMPROVEMENTS

### DX Improvement #1: Wire Format Debugging
**Current State:** No mention of debugging tools.

**Recommended Addition (Phase 7):**
```typescript
// Protocol debugging utilities
export class SignalHubDebugger {
  private capturedMessages: SignalMessage[] = [];

  /**
   * Capture all messages for local analysis
   */
  startCapture(filter?: { types?: string[]; fromAddress?: string }) {
    this.capturedMessages = [];
    // ...
  }

  getCapture() {
    return this.capturedMessages;
  }

  /**
   * Pretty-print a message for debugging
   */
  static formatMessage(msg: SignalMessage): string {
    return `
      [${msg.timestamp}] ${msg.from} -> ${msg.to}
      Type: ${msg.type} (ID: ${msg.id})
      Pattern: ${msg.pattern}
      ${msg.correlationId ? `CorrelationId: ${msg.correlationId}` : ''}
      Payload: ${JSON.stringify(msg.payload, null, 2)}
      ${msg.ttl ? `TTL: ${msg.ttl}ms` : ''}
    `.trim();
  }
}

// Usage in browser dev tools or CLI
const hub = new SignalHubClient();
hub.debugger.startCapture({ types: ['register', 'error'] });
// ... do stuff ...
console.table(hub.debugger.getCapture());
```

### DX Improvement #2: Error Messages
**Current State:** "error, unknown_actor, unauthorized, rate_limited" listed but no examples.

**Recommended Addition:**
```typescript
// Structured error codes with actionable messages
const ErrorMessages = {
  UNKNOWN_ACTOR: {
    code: 'UNKNOWN_ACTOR',
    message: 'Target actor is not registered',
    hint: 'Ensure the actor has called register() and the registration has not expired',
    httpStatus: 404,
  },
  DUPLICATE_REGISTRATION: {
    code: 'DUPLICATE_REGISTRATION',
    message: 'An actor with this address is already registered',
    hint: 'If restarting: wait 5 minutes for old registration to expire, or use a new actor address',
    httpStatus: 409,
  },
  RATE_LIMITED: {
    code: 'RATE_LIMITED',
    message: 'Too many requests from this actor',
    hint: 'Back off exponentially: wait 1-30 seconds before retrying',
    retryAfter: number; // seconds
    httpStatus: 429,
  },
  PAYLOAD_TOO_LARGE: {
    code: 'PAYLOAD_TOO_LARGE',
    message: `Message payload exceeds ${MAX_PAYLOAD_SIZE} bytes`,
    hint: 'Break your message into smaller chunks or use a different transport',
    httpStatus: 413,
  },
  STALE_VERSION: {
    code: 'STALE_VERSION',
    message: 'Registration version is stale',
    hint: 'Reconnect and re-register with the latest configuration',
    httpStatus: 400,
  },
};

// Usage
if (error.code === 'UNKNOWN_ACTOR') {
  console.error(ErrorMessages.UNKNOWN_ACTOR.hint);
}
```

### DX Improvement #3: Registration Flow Documentation
**Current State:** Schema defined but flow unclear.

**Recommended Addition (Phase 7 - PROTOCOL.md):**
```markdown
## Registration Flow

### Step 1: Connect
```
Client                        Signal Hub
  |                               |
  | WebSocket upgrade             |
  |-------------------------------|->
  |                     101 Switching Protocols
  |<------------------------------|
  |
```

### Step 2: Register
```
  | Message: { type: 'register', ... }
  |-------------------------------|->
  |                 ACK: { code: 'REGISTERED', ... }
  |<------------------------------|
```

### Step 3: Heartbeat
```
  | Heartbeat every 30 seconds
  |------|------|------|------|
  | beat | beat | beat | beat |
  |------|------|------|------|

  If no heartbeat for 60 seconds: registration expires
```

### Error Scenarios
- **Registration rejected**: Actor at same address already registered
  - **Action**: Wait 5 minutes or use different address
- **Connection timeout**: No heartbeat for 60s
  - **Action**: Automatic reconnect with exponential backoff
- **Payload too large**: Message > 512KB
  - **Action**: Break into smaller messages

### Example Client Implementation
\`\`\`typescript
const client = new SignalHubClient('ws://signal-hub.example.com');

client.on('registered', () => {
  console.log('Ready to send/receive');
});

client.on('error', (err) => {
  console.error(`${err.code}: ${err.message}`);
  console.error(`Hint: ${err.hint}`);
});

// Automatic heartbeat and reconnection handled internally
\`\`\`
```

### DX Improvement #4: Monitoring/Observability
**Current State:** No mention of metrics or observability.

**Recommended Addition (Phase 6-7):**
```typescript
// Metrics collection
interface SignalHubMetrics {
  // Actors
  registeredActors: number;
  registrationRate: number; // per second
  registrationErrors: number;

  // Messages
  messagesReceived: number;
  messagesSent: number;
  messagesLost: number; // Dropped due to queue full

  // Broadcasts
  broadcastsRequested: number;
  broadcastsQueued: number;
  broadcastsCompleted: number;
  broadcastsPartiallyFailed: number;
  broadcastAverageLatencyMs: number;

  // Connections
  activeConnections: number;
  connectionErrors: number;
  reconnectionAttempts: number;

  // System
  cpuTimePerSecond: number;
  memoryUsageBytes: number;
  storageUsageBytes: number;

  // Errors (last 5 minutes)
  errorCounts: Record<string, number>; // Error code -> count
}

// Export as JSON for monitoring tools
async handleMetricsRequest(): Promise<Response> {
  const metrics = this.getMetrics();
  return new Response(JSON.stringify(metrics), {
    headers: { 'Content-Type': 'application/json' },
  });
}
```

---

## 7. SUMMARY & RECOMMENDATIONS

### Before Implementation

1. **Expand Phase 3 (Registration Schema):**
   - Add version, connectionId, TTL tracking
   - Specify deduplication rules
   - Add capabilities schema (not just string array)

2. **Complete Phase 4 (Connection State Machine):**
   - Explicit heartbeat rules (interval, timeout, TTL)
   - WebSocket hibernation handling
   - Graceful shutdown protocol

3. **Rewrite Phase 5 (Message Delivery):**
   - Define "At-most-once" more explicitly (with examples)
   - Add backpressure/queue management strategy
   - Specify message size limits and validation
   - Add deduplication rules (based on message ID + TTL)

4. **Add Phase 5.5 (Scaling & Sharding):**
   - Per-instance actor limits (recommend: 50K)
   - Broadcast throughput limits (recommend: 1K actors/sec)
   - Sharding strategy for >50K actors

5. **Expand Phase 7 (Documentation):**
   - Add runnable test fixtures
   - Include error scenarios & recovery
   - Add monitoring/observability examples
   - Include load test methodology

### Implementation Checklist

- [ ] Implement message ID + version for deduplication
- [ ] Add heartbeat timeout tracking with explicit timers
- [ ] Implement broadcast queuing (don't serialize all sends)
- [ ] Add message size validation (512KB max)
- [ ] Implement WebSocket hibernation strategy
- [ ] Add connection rate limiting (1000/sec per instance)
- [ ] Implement storage TTL for registrations (24h)
- [ ] Add metrics collection & export
- [ ] Write integration tests for all 5 edge cases
- [ ] Load test with 10K actors + broadcasts
- [ ] Document all error codes with hints

### Risk Reduction Priority

1. **HIGH PRIORITY:** Heartbeat timeout management (Edge Case #5)
2. **HIGH PRIORITY:** Broadcast backpressure (Edge Case #3)
3. **MEDIUM PRIORITY:** Duplicate registration handling (Edge Case #2)
4. **MEDIUM PRIORITY:** WebSocket disconnect recovery (Edge Case #1)
5. **MEDIUM PRIORITY:** Storage explosion prevention (Edge Case #4)

---

## Files Referenced

- `/Users/bln/play/agentic-primer/packages/protocols/src/shared-message.ts` - Cross-runtime wire format
- `/Users/bln/play/agentic-primer/packages/cloudflare/src/transports/websocket-bridge.ts` - Hibernatable WebSocket implementation
- `/Users/bln/play/agentic-primer/packages/cloudflare/src/transports/do-transport.ts` - DurableObject transport
- `/Users/bln/play/agentic-primer/ugs/src/system-actors/signal-hub-bridge.ts` - Signal Hub Bridge actor (reference implementation)

---

**END OF REVIEW**
