# Signal Hub Delivery Guarantees

**Status:** Design Phase
**Last Updated:** 2026-02-16
**Protocol Version:** 0.1.0

---

## Overview

Signal Hub provides **configurable delivery guarantees** for messages routed between actors. The guarantee level determines the reliability, performance, and complexity trade-offs for message delivery.

This document defines:
1. **At-most-once delivery** (MVP) - Fire-and-forget with no acknowledgment
2. **At-least-once delivery** (Phase 2 Future) - Acknowledgment-based with retry
3. **Ordering guarantees** - Per-connection FIFO semantics
4. **TTL handling** - Message expiration and timeout behavior
5. **Error propagation** - Delivery failure scenarios and handling
6. **Deduplication** - Preventing duplicate message processing
7. **Queue behavior** - When messages are queued vs delivered immediately

---

## 1. At-Most-Once Delivery (MVP)

**Default behavior for Signal Hub.**

### 1.1 Semantics

- **Fire-and-forget:** Client sends message, server attempts delivery, no acknowledgment required
- **Pattern:** `pattern: 'tell'` on `hub:send` message
- **Guarantee:** Message may be delivered 0 or 1 times
- **Performance:** Lowest latency, highest throughput

### 1.2 When Messages May Be Lost

Messages may be lost during:

| Scenario | Reason | Preventable? |
|----------|--------|--------------|
| WebSocket disconnect | Network failure between send and delivery | No |
| Durable Object eviction | Cloudflare evicts inactive DO during delivery | No |
| Hibernation transition | In-flight message lost during hibernation wake | Partially (heartbeat) |
| Target actor offline | Actor disconnected, no queue persistence | Yes (use pattern='ask') |
| Message too large | Exceeds 1MB WebSocket frame limit | Yes (split message) |
| TTL expired | Message expired before delivery attempt | Yes (increase TTL) |

### 1.3 Use Cases

At-most-once is suitable for:

- **Heartbeats** - Periodic keep-alive signals (loss is non-critical)
- **Status updates** - Transient state that will be refreshed (e.g., "typing indicator")
- **Metrics/telemetry** - High-volume data where individual loss is acceptable
- **Best-effort notifications** - Non-critical alerts that can be missed

### 1.4 Example

```typescript
// Fire-and-forget message delivery
const msg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(local/coordinator-main)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:send',
  pattern: 'tell',  // ← At-most-once (no ack)
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    targetAddress: '@(browser/widget-123)',
    message: {
      type: 'status:update',
      payload: { status: 'idle' }
    }
  },
  metadata: {},
  ttl: 5000,  // 5s expiration
  signature: null
};

// Client sends and continues - no waiting for ack
await signalHub.send(msg);
// ✓ Message sent, no guarantee of delivery
```

---

## 2. At-Least-Once Delivery (Phase 2 - Future)

**With acknowledgment and retry.**

### 2.1 Semantics

- **Acknowledged delivery:** Client sends message, server confirms delivery, client retries if no ack
- **Pattern:** `pattern: 'ask'` on `hub:send` message
- **Guarantee:** Message delivered 1 or more times (duplicates possible)
- **Performance:** Higher latency, lower throughput (due to ack round-trip)

### 2.2 Acknowledgment Flow

```
Client                    Signal Hub                   Target Actor
  |                           |                               |
  | hub:send (pattern='ask')  |                               |
  |-------------------------->|                               |
  |                           | Lookup target actor           |
  |                           | Deliver message               |
  |                           |------------------------------>|
  |                           |                               |
  | hub:delivery_ack          |                               |
  |<--------------------------|                               |
  |                           |                               |
```

**Success path:**
1. Client sends `hub:send` with `pattern: 'ask'`
2. Server delivers message to target actor
3. Server responds with `hub:delivery_ack` (status: `delivered`)
4. Client receives ack, considers delivery successful

**Failure path (timeout):**
1. Client sends `hub:send` with `pattern: 'ask'`, sets timeout (default 30s)
2. No `hub:delivery_ack` received within timeout
3. Client retries (with same message ID)
4. Server deduplicates on retry (see Section 6)

### 2.3 Retry Logic

**Client-side retry strategy:**

```typescript
async function sendWithRetry(
  targetAddress: CanonicalAddress,
  message: unknown,
  maxRetries = 3,
  timeoutMs = 30000
): Promise<void> {
  const messageId = crypto.randomUUID();

  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      const msg: SharedMessage = {
        id: messageId,  // ← Same ID for retries (deduplication)
        from: selfAddress,
        to: '@(cloudflare/signal-hub)',
        type: 'hub:send',
        pattern: 'ask',  // ← Require ack
        correlationId: null,
        timestamp: Date.now(),
        payload: {
          targetAddress,
          message
        },
        metadata: {
          requireAck: true,
          attemptNumber: attempt
        },
        ttl: timeoutMs,
        signature: null
      };

      // Send and wait for ack
      const ack = await signalHub.sendAndWait(msg, timeoutMs);

      if (ack.type === 'hub:delivery_ack') {
        console.log(`Delivered on attempt ${attempt}`);
        return;
      }
    } catch (err) {
      if (err.code === 'timeout' && attempt < maxRetries) {
        console.warn(`Attempt ${attempt} timed out, retrying...`);
        await sleep(1000 * attempt);  // Exponential backoff
        continue;
      }
      throw err;
    }
  }

  throw new Error(`Failed to deliver after ${maxRetries} attempts`);
}
```

**Key points:**
- Same message ID for all retries (enables deduplication)
- Exponential backoff between retries
- Timeout per attempt (default 30s)
- Max retries configurable (default 3)

### 2.4 Delivery Acknowledgment Types

```typescript
interface HubDeliveryAckMessage extends HubMessage {
  type: 'hub:delivery_ack';
  pattern: 'tell';
  correlationId: string;
  payload: {
    messageId: string;
    deliveredAt: number;
    status: 'delivered' | 'queued';
  };
}
```

**Status values:**

| Status | Meaning | Client Action |
|--------|---------|---------------|
| `delivered` | Message delivered to connected target actor | Consider successful, no retry |
| `queued` | Target offline, message queued for delivery | Wait for actual delivery (future phase) |

**Phase 2 constraint:** Server only returns `delivered` when target actor is **currently connected**. If target is offline, server returns `hub:unknown_actor` error (not `queued`).

### 2.5 Use Cases

At-least-once is suitable for:

- **Task assignments** - Critical work must reach worker
- **State synchronization** - Ensure state changes propagate
- **Commands** - Actions that must be executed (e.g., "deploy", "shutdown")
- **Durable workflows** - Multi-step processes requiring reliable messaging

### 2.6 Example

```typescript
// Acknowledged delivery with retry
const msg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(local/coordinator-main)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:send',
  pattern: 'ask',  // ← At-least-once (require ack)
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    targetAddress: '@(browser/worker-456)',
    message: {
      type: 'task:assign',
      payload: { taskId: 'task-789', priority: 1 }
    }
  },
  metadata: {
    requireAck: true,
    traceId: 'trace-xyz'
  },
  ttl: 30000,  // 30s timeout
  signature: null
};

// Client sends and waits for ack
const ack = await signalHub.sendAndWait(msg, 30000);

if (ack.type === 'hub:delivery_ack') {
  console.log('Message delivered:', ack.payload.status);
} else if (ack.type === 'hub:error') {
  console.error('Delivery failed:', ack.payload);
  // Retry or handle error
}
```

---

## 3. Ordering Guarantees

### 3.1 Per-Connection FIFO

**Guarantee:** Messages from Actor A → Actor B maintain send order.

**Example:**

```typescript
// Actor A sends three messages to Actor B
await signalHub.send({ to: actorB, message: { seq: 1 } });
await signalHub.send({ to: actorB, message: { seq: 2 } });
await signalHub.send({ to: actorB, message: { seq: 3 } });

// Actor B receives messages in order: seq=1, seq=2, seq=3
```

**Why it works:**
- Single WebSocket connection between A and Signal Hub
- Single WebSocket connection between Signal Hub and B
- TCP guarantees in-order delivery within a connection
- Server processes messages from A sequentially

**Constraint:** Only applies to messages sent **from the same actor** to **the same target actor** over **the same WebSocket connection**.

### 3.2 No Global Ordering

**No guarantee:** Messages from different actors have no ordering.

**Example:**

```typescript
// Actor A and Actor C both send to Actor B
actorA.send({ to: actorB, message: { from: 'A', seq: 1 } });
actorC.send({ to: actorB, message: { from: 'C', seq: 1 } });

// Actor B may receive in ANY order:
// - A:1, C:1  OR  C:1, A:1
```

**Why no global ordering:**
- Different WebSocket connections (A→Hub and C→Hub)
- Concurrent delivery paths
- No coordination between actor connections

**Implication:** Applications requiring total ordering must implement their own sequencing (e.g., vector clocks, logical timestamps).

### 3.3 Broadcast Ordering

**No guarantee:** Broadcast messages may arrive in different order at different actors.

**Example:**

```typescript
// Coordinator broadcasts two messages
await signalHub.broadcast({ type: 'event:1' });
await signalHub.broadcast({ type: 'event:2' });

// Actor X receives: event:1, event:2
// Actor Y receives: event:2, event:1  ← Different order!
```

**Why no broadcast ordering:**
- Async fan-out (for >100 actors, messages queued)
- Variable network latency per target actor
- No coordination between broadcast deliveries

**Mitigation:**
- Include sequence numbers in broadcast payload
- Receivers buffer out-of-order messages
- Use causal ordering (vector clocks) if needed

### 3.4 Topic (Pub/Sub) Ordering

**No guarantee:** Published messages to a topic have no ordering guarantee.

**Example:**

```typescript
// Publisher sends two messages to topic "events"
await signalHub.publish({ topic: 'events', message: { seq: 1 } });
await signalHub.publish({ topic: 'events', message: { seq: 2 } });

// Subscriber A receives: seq:1, seq:2
// Subscriber B receives: seq:2, seq:1  ← Different order!
```

**Why no topic ordering:**
- Same as broadcast (fan-out to all subscribers)
- Topic subscriptions are independent connections
- No coordination between subscribers

**Future enhancement (Phase 3):**
- Ordered topics with sequence numbers
- Subscribers can detect gaps and request replay

---

## 4. TTL Handling

### 4.1 TTL Field Semantics

The `ttl` field on `SharedMessage` specifies **message lifetime in milliseconds**.

```typescript
interface SharedMessage {
  id: string;
  timestamp: number;  // Epoch ms when message created
  ttl: number | null; // Milliseconds before expiration (null = no expiration)
  // ...
}
```

**Expiration check:**

```typescript
function isExpired(msg: SharedMessage, now: number): boolean {
  if (msg.ttl === null) return false;  // No expiration
  return (msg.timestamp + msg.ttl) < now;
}
```

### 4.2 TTL Expiration Behavior

**Server checks TTL:**
1. When receiving message from client
2. Before delivering to target actor
3. Before queuing for async delivery (broadcast, pub/sub)

**If message expired:**
1. Server drops message (no delivery attempt)
2. Server sends `hub:error` with `code: 'message_expired'` to sender
3. If `pattern: 'ask'`, error returned as response to correlation ID

**Example:**

```typescript
// Client sends message with 5s TTL
const msg: SharedMessage = {
  id: 'msg-123',
  timestamp: Date.now(),  // T=0
  ttl: 5000,              // 5s expiration
  type: 'hub:send',
  pattern: 'ask',
  payload: { targetAddress: '@(slow/actor)', message: { data: 'test' } },
  // ...
};

await signalHub.send(msg);

// Server attempts delivery at T=6000 (6 seconds later)
// isExpired(msg, Date.now()) === true

// Server responds with error:
{
  type: 'hub:error',
  correlationId: 'msg-123',
  payload: {
    code: 'message_expired',
    message: 'Message expired before delivery (TTL: 5000ms)',
    retryable: false  // ← Don't retry expired messages
  }
}
```

### 4.3 TTL Best Practices

**Recommended TTL values:**

| Message Type | TTL | Rationale |
|--------------|-----|-----------|
| Heartbeat | 10s | Short-lived, frequent |
| Status update | 5s | Transient state, low value after expiration |
| Task assignment | 30s | Critical, but stale tasks not useful |
| Broadcast | 60s | Large fan-out may take time |
| Query (ask) | 5s | User waiting, fast timeout preferred |

**Guidelines:**
- **Too short:** Messages expire before delivery (especially during hibernation)
- **Too long:** Stale messages delivered, waste resources
- **Null TTL:** Only for messages that never expire (rare)

**Hibernation consideration:**
- Cloudflare hibernates after 30s idle
- Set TTL > 30s for messages that may arrive during hibernation
- Heartbeat keeps connection alive (prevents hibernation)

### 4.4 TTL vs Timeout

**TTL (message expiration):**
- Defines how long message is valid
- Server-side check before delivery
- Prevents delivering stale messages

**Timeout (client wait duration):**
- Defines how long client waits for response (ack)
- Client-side check after sending
- Triggers retry on timeout

**Relationship:**
```typescript
// TTL should be >= timeout to avoid premature expiration
const timeoutMs = 30000;  // Client waits 30s for ack
const msg: SharedMessage = {
  ttl: 35000,  // ← 35s TTL (5s buffer for delivery)
  // ...
};
```

---

## 5. Error Propagation

### 5.1 Delivery Failure Scenarios

| Failure Type | Error Message Type | Error Code | Retryable? | Client Action |
|--------------|-------------------|------------|------------|---------------|
| Actor not registered | `hub:unknown_actor` | N/A | No | Check actor exists, re-register |
| Message too large | `hub:message_too_large` | N/A | No | Split message or reduce payload |
| Rate limited | `hub:rate_limited` | N/A | Yes | Wait `retryAfter` ms, retry |
| Timeout | `hub:error` | `timeout` | Yes | Retry with exponential backoff |
| TTL expired | `hub:error` | `message_expired` | No | Increase TTL or don't retry |
| Unauthorized | `hub:unauthorized` | N/A | No | Fix auth token, reconnect |
| Internal error | `hub:error` | `internal_error` | Yes | Retry, report to ops |

### 5.2 Error Message Format

**Generic error:**

```typescript
{
  type: 'hub:error',
  pattern: 'tell',
  correlationId: 'msg-123',  // Links to failed request
  payload: {
    code: 'timeout',
    message: 'Message delivery timed out after 30s',
    details: {
      targetAddress: '@(offline/actor-456)',
      attemptedAt: 1739731234567
    },
    retryable: true
  }
}
```

**Specific errors:**

```typescript
// Actor not found
{
  type: 'hub:unknown_actor',
  correlationId: 'msg-123',
  payload: {
    actorAddress: '@(browser/missing-actor)',
    message: 'Actor not registered with Signal Hub'
  }
}

// Message too large
{
  type: 'hub:message_too_large',
  correlationId: 'msg-123',
  payload: {
    messageSize: 2097152,  // 2MB
    maxSize: 1048576       // 1MB
  }
}

// Rate limited
{
  type: 'hub:rate_limited',
  correlationId: 'msg-123',
  payload: {
    retryAfter: 1000  // Wait 1s before retry
  }
}
```

### 5.3 Error Handling Strategy

**Client-side error handler:**

```typescript
async function handleDeliveryError(error: HubMessage): Promise<void> {
  switch (error.type) {
    case 'hub:unknown_actor':
      // Target actor not registered
      console.error('Actor not found:', error.payload.actorAddress);
      // Option 1: Discover actor (may have moved)
      // Option 2: Notify user actor unavailable
      break;

    case 'hub:message_too_large':
      // Message exceeds 1MB limit
      console.error('Message too large:', error.payload.messageSize);
      // Option 1: Split message into chunks
      // Option 2: Use external storage + reference
      break;

    case 'hub:rate_limited':
      // Too many requests
      const { retryAfter } = error.payload;
      console.warn(`Rate limited, retrying in ${retryAfter}ms`);
      await sleep(retryAfter);
      // Retry original request
      break;

    case 'hub:error':
      const { code, retryable } = error.payload;

      if (code === 'timeout' && retryable) {
        // Timeout - retry with exponential backoff
        console.warn('Delivery timeout, retrying...');
        await retryWithBackoff();
      } else if (code === 'message_expired' && !retryable) {
        // Message expired - don't retry
        console.error('Message expired, discarding');
      } else if (code === 'internal_error' && retryable) {
        // Server error - retry with limit
        console.error('Server error, retrying...');
        await retryWithBackoff(maxRetries: 1);
      }
      break;

    default:
      console.error('Unknown error type:', error.type);
  }
}
```

### 5.4 Correlation and Tracing

**Every error includes correlation ID:**

```typescript
// Original request
const sendMsg: SharedMessage = {
  id: 'msg-abc-123',  // ← Request ID
  type: 'hub:send',
  pattern: 'ask',
  // ...
};

// Error response
{
  type: 'hub:error',
  correlationId: 'msg-abc-123',  // ← Links to request
  payload: { code: 'timeout', message: '...' }
}
```

**Distributed tracing:**

```typescript
// Add trace ID in metadata
const sendMsg: SharedMessage = {
  id: 'msg-123',
  type: 'hub:send',
  metadata: {
    traceId: 'trace-xyz-789',  // ← Distributed trace ID
    spanId: 'span-001'
  },
  // ...
};

// Server includes trace ID in error
{
  type: 'hub:error',
  correlationId: 'msg-123',
  metadata: {
    traceId: 'trace-xyz-789',  // ← Preserved from request
    spanId: 'span-002'
  },
  payload: { code: 'timeout', message: '...' }
}
```

---

## 6. Deduplication

### 6.1 Problem Statement

With at-least-once delivery, messages may be delivered multiple times due to:
- Client retry on timeout (message was actually delivered, ack lost)
- Network duplicate (rare, TCP prevents most cases)
- Server retry on internal failure

**Without deduplication:**
```
Client sends task:assign (id: 'msg-123')
Server delivers → Actor processes task
Ack lost (network glitch)
Client retries (same id: 'msg-123')
Server delivers again → Actor processes task TWICE  ← Problem!
```

### 6.2 Message ID Seen-Set

**Server maintains short-lived cache of processed message IDs:**

```typescript
// Durable Object state
class SignalHubState {
  private seenMessages: Map<string, number>;  // messageId → timestamp

  async handleSend(msg: SharedMessage): Promise<void> {
    const { id, timestamp, ttl } = msg;

    // Check if already processed
    if (this.seenMessages.has(id)) {
      const firstSeenAt = this.seenMessages.get(id)!;
      console.log(`Duplicate message ${id}, ignoring (first seen at ${firstSeenAt})`);

      // Return same ack as before (idempotent)
      return this.sendDeliveryAck(msg.from, id);
    }

    // Mark as seen
    this.seenMessages.set(id, Date.now());

    // Deliver to target actor
    await this.deliverToActor(msg);

    // Send ack
    await this.sendDeliveryAck(msg.from, id);
  }
}
```

### 6.3 TTL-Bounded Cache

**Problem:** Seen-set grows unbounded without eviction.

**Solution:** TTL-bounded cache with automatic eviction.

```typescript
class SeenMessageCache {
  private cache: Map<string, { timestamp: number; expiresAt: number }>;

  constructor(private defaultTtlMs = 60000) {}  // 60s default

  has(messageId: string): boolean {
    const entry = this.cache.get(messageId);
    if (!entry) return false;

    // Check expiration
    if (Date.now() > entry.expiresAt) {
      this.cache.delete(messageId);
      return false;
    }

    return true;
  }

  add(messageId: string, ttlMs?: number): void {
    const now = Date.now();
    const ttl = ttlMs ?? this.defaultTtlMs;
    this.cache.set(messageId, {
      timestamp: now,
      expiresAt: now + ttl
    });
  }

  // Periodic cleanup (run every 60s)
  evictExpired(): number {
    const now = Date.now();
    let evicted = 0;

    for (const [id, entry] of this.cache.entries()) {
      if (now > entry.expiresAt) {
        this.cache.delete(id);
        evicted++;
      }
    }

    return evicted;
  }
}
```

**Cache TTL strategy:**
- Default TTL: 60s (1 minute)
- Max TTL: 300s (5 minutes)
- Eviction: Every 60s, remove expired entries
- Cache size limit: 10,000 entries (FIFO eviction if exceeded)

**Why 60s TTL:**
- Client retries typically within 30s
- Covers retry + exponential backoff window
- Balances memory usage vs deduplication window

### 6.4 Handling Retransmissions

**Idempotent ack response:**

```typescript
async handleSend(msg: SharedMessage): Promise<void> {
  const { id } = msg;

  // Check seen-set
  if (this.seenCache.has(id)) {
    // Retransmission detected
    console.log(`Duplicate message ${id}, returning cached ack`);

    // Return SAME ack as first delivery (idempotent)
    const cachedAck = this.ackCache.get(id);
    if (cachedAck) {
      await this.sendMessage(msg.from, cachedAck);
      return;
    }

    // Ack not cached (expired), generate new ack
    await this.sendDeliveryAck(msg.from, id);
    return;
  }

  // First time seeing this message
  this.seenCache.add(id, msg.ttl ?? 60000);

  // Deliver to target
  await this.deliverToActor(msg);

  // Cache ack for idempotent retry
  const ack = this.createDeliveryAck(id);
  this.ackCache.set(id, ack, 60000);  // Cache for 60s

  // Send ack
  await this.sendMessage(msg.from, ack);
}
```

### 6.5 Application-Level Deduplication

**Server-side deduplication only prevents double-delivery by Signal Hub.**

**Actors must implement their own deduplication for:**
- Messages that bypass Signal Hub (direct connections)
- Messages from external systems (webhooks, queues)
- Idempotent operation semantics

**Example actor-level deduplication:**

```typescript
class Actor {
  private processedMessages = new Set<string>();

  async handleMessage(msg: SharedMessage): Promise<void> {
    // Check if already processed
    if (this.processedMessages.has(msg.id)) {
      console.log(`Duplicate ${msg.id}, ignoring`);
      return;
    }

    // Mark as processed
    this.processedMessages.add(msg.id);

    // Process message
    await this.processTask(msg.payload);
  }
}
```

---

## 7. Queue Behavior

### 7.1 Immediate Delivery vs Queued Delivery

**Signal Hub delivery modes:**

| Scenario | Delivery Mode | Latency | Guarantee |
|----------|--------------|---------|-----------|
| Target actor online | Immediate | <10ms | Delivered now |
| Target actor offline (MVP) | Drop + error | N/A | `hub:unknown_actor` |
| Target actor offline (Phase 2 Future) | Queued | Variable | Delivered on reconnect |
| Broadcast <100 actors | Immediate | <100ms | Synchronous fan-out |
| Broadcast >100 actors | Queued | Variable | Async via Cloudflare Queues |

### 7.2 Immediate Delivery Path

**When target actor is connected:**

```
Client                    Signal Hub                   Target Actor
  |                           |                               |
  | hub:send                  |                               |
  |-------------------------->|                               |
  |                           | Lookup target in registry     |
  |                           | Target online? YES            |
  |                           |                               |
  |                           | Send via WebSocket            |
  |                           |------------------------------>|
  |                           |                               |
  | hub:delivery_ack          |                               |
  |<--------------------------|                               |
```

**Performance:**
- Latency: 5-15ms (WebSocket round-trip)
- No persistence overhead
- Direct memory-to-memory transfer

### 7.3 Queued Delivery Path (Phase 2 - Future)

**When target actor is offline:**

```
Client                    Signal Hub                   Durable Storage
  |                           |                               |
  | hub:send                  |                               |
  |-------------------------->|                               |
  |                           | Lookup target in registry     |
  |                           | Target online? NO             |
  |                           |                               |
  |                           | Queue for delivery            |
  |                           |------------------------------>|
  |                           |                               |
  | hub:delivery_ack          |                               |
  | (status: 'queued')        |                               |
  |<--------------------------|                               |
  |                           |                               |
  |     [Target reconnects]   |                               |
  |                           |<------------------------------|
  |                           | Deliver queued messages       |
```

**Queued delivery semantics (Phase 2):**
- Messages queued in Durable Object storage (persistent)
- Queue bound by TTL (expired messages dropped)
- FIFO delivery on actor reconnect
- Max queue size: 100 messages per actor (prevent unbounded growth)

**Ack status for queued messages:**

```typescript
{
  type: 'hub:delivery_ack',
  payload: {
    messageId: 'msg-123',
    deliveredAt: Date.now(),
    status: 'queued'  // ← Not yet delivered, queued for later
  }
}
```

### 7.4 Broadcast Queue Behavior

**Small broadcast (<100 actors):**

```typescript
// Synchronous fan-out
async function broadcastSmall(message: SharedMessage, actors: Actor[]): Promise<void> {
  for (const actor of actors) {
    await sendToActor(actor, message);  // Sequential delivery
  }
}
```

**Large broadcast (>100 actors):**

```typescript
// Async queue-based fan-out
async function broadcastLarge(message: SharedMessage, actors: Actor[]): Promise<void> {
  // Batch into chunks of 100
  const batches = chunk(actors, 100);

  for (const batch of batches) {
    // Enqueue batch for async processing
    await cloudflareQueue.send({
      messageId: message.id,
      targetActors: batch.map(a => a.address),
      message
    });
  }
}
```

**Why queue for large broadcasts:**
- Cloudflare Workers CPU limit: 30s
- Synchronous fan-out to 1000 actors = ~30s (at limit)
- Async queue prevents timeout, spreads load

### 7.5 Queue Backpressure

**Server detects queue overflow:**

```typescript
class SignalHub {
  private outboundQueue: Message[] = [];
  private readonly pauseThreshold = 1000;
  private readonly resumeThreshold = 500;

  async send(msg: SharedMessage): Promise<void> {
    this.outboundQueue.push(msg);

    // Check if queue exceeds threshold
    if (this.outboundQueue.length > this.pauseThreshold) {
      // Send pause to client
      await this.sendPause(msg.from, 'outbound_queue_full');
    }

    // Process queue
    await this.processQueue();

    // Check if queue drained below resume threshold
    if (this.outboundQueue.length < this.resumeThreshold) {
      // Send resume to client
      await this.sendResume(msg.from);
    }
  }
}
```

**Client respects pause/resume:**

```typescript
class SignalHubClient {
  private paused = false;

  async send(msg: SharedMessage): Promise<void> {
    // Wait for resume if paused
    while (this.paused) {
      await sleep(100);
    }

    // Send message
    await this.ws.send(JSON.stringify(msg));
  }

  handlePause(pauseMsg: HubMessage): void {
    console.warn('Server paused:', pauseMsg.payload.reason);
    this.paused = true;
  }

  handleResume(resumeMsg: HubMessage): void {
    console.log('Server resumed');
    this.paused = false;
  }
}
```

---

## Summary

| Guarantee Level | Pattern | Ack? | Retry? | Dedup? | Use Case |
|----------------|---------|------|--------|--------|----------|
| **At-most-once (MVP)** | `tell` | No | No | No | Heartbeats, status, metrics |
| **At-least-once (Phase 2)** | `ask` | Yes | Yes | Yes | Tasks, commands, state sync |

**Ordering:**
- Per-connection FIFO: YES (A→B maintains order)
- Global ordering: NO (use sequence numbers)
- Broadcast ordering: NO (use causal ordering)

**TTL:**
- Default: 30s for critical messages, 5s for transient
- Expired messages dropped before delivery
- Server sends `hub:error` (code: `message_expired`)

**Errors:**
- All errors include `correlationId` linking to failed request
- `retryable: true` → client may retry
- `retryable: false` → don't retry (fix root cause)

**Deduplication:**
- Server seen-set with 60s TTL
- Prevents double-delivery on retry
- Actors should implement own deduplication for external sources

**Queue:**
- Online actors → immediate delivery (<10ms)
- Offline actors (MVP) → `hub:unknown_actor` error
- Broadcast >100 actors → async queue (Cloudflare Queues)
- Backpressure via `hub:pause`/`hub:resume`

---

## Next Steps

1. ✅ Delivery guarantees documented
2. ⏳ Implement at-most-once delivery (MVP)
3. ⏳ Implement deduplication cache
4. ⏳ Implement TTL expiration checks
5. ⏳ Phase 2: At-least-once with ack + retry
6. ⏳ Phase 2: Queued delivery for offline actors
7. ⏳ Testing: Reconnection scenarios, retry logic, deduplication

**Status:** Phase 5 design complete, ready for implementation
