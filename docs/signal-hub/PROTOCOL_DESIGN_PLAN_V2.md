# Signal Hub Protocol Design Plan v2

**Status:** Design Phase (Revised after Opus/Haiku review)
**Last Updated:** 2026-02-16
**Supersedes:** Original 7-phase plan (inline in session)

---

## Changes from v1

**Critical updates based on architectural review:**

1. ✅ **Use SharedMessage** - Don't create WireMessage (Opus P0)
2. ✅ **Add protocol versioning** - Version negotiation in connect (Opus P1)
3. ✅ **Define security model** - Auth token + server-enforced identity (Opus P1)
4. ✅ **Align connection states** - Use existing ConnectionState enum (Opus P3)
5. ✅ **Add backpressure** - Flow control for fast producers (Opus P4)
6. ✅ **Address Cloudflare constraints** - Hibernation, CPU limits, message size (Haiku)
7. ✅ **Scalability strategy** - Fan-out queue, max actors, sharding (Haiku)

---

## Phase 1: Wire Protocol - Use SharedMessage ✓

**Deliverable:** `docs/signal-hub/SHARED_MESSAGE_INTEGRATION.md` ✅

**Decision:** Signal Hub uses `SharedMessage` from `@agentic-primer/protocols` as the wire format.

**Key points:**
- Message types = `type` discriminators (`hub:connect`, `hub:register`, etc.)
- Metadata bag for extensibility (version, auth, capabilities, tracing)
- CanonicalAddress (@(path)) for runtime-agnostic addressing
- Runtime converters (browser ↔ cloudflare ↔ local)
- Ask/tell pattern for request-response

**Status:** Complete (documented in SHARED_MESSAGE_INTEGRATION.md)

---

## Phase 2: Message Type Catalog (Formal Spec)

**Deliverable:** `docs/signal-hub/MESSAGE_TYPES.md`

**Define all Signal Hub message types with:**

### 2.1 Connection Lifecycle

| Type | Pattern | Request Payload | Response Type | Response Payload |
|------|---------|-----------------|---------------|------------------|
| `hub:connect` | ask | `{ protocolVersion, authToken?, capabilities }` | `hub:connected` | `{ sessionId, serverVersion, maxMessageSize }` |
| `hub:heartbeat` | tell | `{ timestamp }` | `hub:heartbeat_ack` | `{ timestamp, serverTime }` |
| `hub:disconnect` | tell | `{ reason? }` | — | — |

**Constraints:**
- `hub:connect` MUST include `protocolVersion` in metadata
- `authToken` in metadata is REQUIRED for production (optional for local dev)
- Server responds with `hub:error` if version incompatible
- Heartbeat interval: 30s (configurable, must be < Cloudflare hibernation timeout)

### 2.2 Actor Discovery

| Type | Pattern | Request Payload | Response Type | Response Payload |
|------|---------|-----------------|---------------|------------------|
| `hub:register` | ask | `{ actorAddress, capabilities, metadata, ttlSeconds }` | `hub:registered` | `{ actorAddress, expiresAt, renewalToken }` |
| `hub:unregister` | tell | `{ actorAddress }` | — | — |
| `hub:discover` | ask | `{ pattern, limit? }` | `hub:discovered` | `{ actors: ActorRegistration[], hasMore }` |
| `hub:list_actors` | ask | `{ offset?, limit? }` | `hub:actor_list` | `{ actors: ActorRegistration[], total, hasMore }` |
| `hub:renew` | ask | `{ renewalToken }` | `hub:renewed` | `{ expiresAt, newToken }` |

**Constraints:**
- `hub:register` - Default TTL: 300s (5 min), max: 3600s (1 hour)
- Duplicate registration: Last-write-wins with version tracking
- `hub:list_actors` - Max limit: 100 (prevents CPU timeout on large registries)
- Registry size: Max 50K actors per Signal Hub instance (shard beyond this)

### 2.3 Message Delivery

| Type | Pattern | Request Payload | Response Type | Response Payload |
|------|---------|-----------------|---------------|------------------|
| `hub:send` | tell/ask | `{ targetAddress, message, requireAck? }` | `hub:delivery_ack` | `{ messageId, deliveredAt, status }` |
| `hub:broadcast` | tell | `{ message, excludeSelf? }` | `hub:broadcast_ack` | `{ messageId, deliveredCount, failedCount }` |
| `hub:subscribe` | ask | `{ topic, durable? }` | `hub:subscribed` | `{ topic, subscriptionId }` |
| `hub:publish` | tell | `{ topic, message }` | `hub:published` | `{ topic, subscriberCount }` |
| `hub:unsubscribe` | tell | `{ subscriptionId }` | — | — |

**Constraints:**
- `hub:send` - Max message size: 1MB (Cloudflare WebSocket frame limit)
- `hub:broadcast` - Async batching for >100 actors (queue-based, not synchronous fan-out)
- `hub:broadcast` - Max throughput: ~1K actors/sec per instance
- `pattern: 'ask'` on `hub:send` requires `hub:delivery_ack` response

### 2.4 Flow Control (Backpressure)

| Type | Pattern | Request Payload | Response Type | Response Payload |
|------|---------|-----------------|---------------|------------------|
| `hub:pause` | tell | `{ reason }` | — | — |
| `hub:resume` | tell | `{}` | — | — |
| `hub:queue_stats` | ask | `{}` | `hub:queue_stats_response` | `{ queueDepth, processingRate }` |

**Constraints:**
- Server sends `hub:pause` when client's outbound queue exceeds threshold
- Client MUST respect pause (stop sending until `hub:resume`)
- Default pause threshold: 1000 messages in queue
- Auto-resume when queue < 500 messages

### 2.5 Error Handling

| Type | Pattern | Payload | Notes |
|------|---------|---------|-------|
| `hub:error` | tell | `{ code, message, details?, retryable }` | Generic error |
| `hub:unknown_actor` | tell | `{ actorAddress }` | Actor not registered |
| `hub:unauthorized` | tell | `{ action, reason }` | Permission denied |
| `hub:rate_limited` | tell | `{ retryAfter }` | Too many requests |
| `hub:version_mismatch` | tell | `{ clientVersion, serverVersion, supportedVersions }` | Incompatible protocol |
| `hub:message_too_large` | tell | `{ messageSize, maxSize }` | Exceeds 1MB limit |

**Constraints:**
- All errors include `correlationId` linking to failed request
- `retryable: true` - client may retry (e.g., rate limit)
- `retryable: false` - don't retry (e.g., unauthorized, version mismatch)

---

## Phase 3: Security & Authentication

**Deliverable:** `docs/signal-hub/SECURITY.md`

### 3.1 Authentication Flow

**Connect with auth token:**

```typescript
// Client sends
{
  type: 'hub:connect',
  metadata: {
    protocolVersion: '0.1.0',
    authToken: 'bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...'
  }
}

// Server validates JWT, extracts identity, responds
{
  type: 'hub:connected',
  metadata: {
    actorIdentity: '@(verified/actor-123)',  // Server-verified
    sessionId: 'sess-abc'
  }
}
```

**Server-enforced identity:**
- Server extracts identity from JWT on connect
- Server stamps `from` field on all subsequent messages
- Clients cannot spoof `from` address

### 3.2 Authorization Model (Phase 2 - Future)

- Actor-to-actor permissions (A can send to B but not C)
- Topic-level ACLs (who can publish/subscribe to topics)
- Rate limiting per actor

### 3.3 HMAC Signatures (Phase 3 - Future)

Use `SharedMessage.signature` field:
- Sign entire message with shared secret
- Server validates before processing
- Prevents message tampering

---

## Phase 4: Connection State Machine

**Deliverable:** `docs/signal-hub/CONNECTION_LIFECYCLE.md`

### 4.1 States (Use Existing ConnectionState Enum)

From `/packages/protocols/schema/domain.schema.json`:

```typescript
type ConnectionState = 'disconnected' | 'connecting' | 'connected' | 'disconnecting';
```

**DO NOT add `reconnecting` to schema.** Model reconnection as client-side concern:
- Client sees: `disconnected` → `connecting` → `connected`
- Server sees: `connecting` → `connected` OR `disconnected` (no intermediate state)

### 4.2 Transitions

```
disconnected
  ↓ (client sends hub:connect)
connecting
  ↓ (server sends hub:connected)
connected
  ↓ (client sends hub:disconnect OR WebSocket closes)
disconnecting
  ↓ (cleanup complete)
disconnected
```

### 4.3 Cloudflare Hibernation Handling

**Critical constraint:** Durable Object WebSocket hibernates after 30s idle.

**Strategy:**
1. Client sends `hub:heartbeat` every 25s (< 30s threshold)
2. Server responds with `hub:heartbeat_ack`
3. On hibernation: WebSocket preserved by Cloudflare, no reconnect needed
4. On wake: Message queue processed, connection resumes

**Edge case - hibernation during message:**
- In-flight messages may be lost during hibernation transition
- Client must implement retry logic for critical messages
- Use `pattern: 'ask'` + timeout for delivery confirmation

### 4.4 Duplicate Connection Handling

**Scenario:** Same actor connects twice (e.g., page refresh)

**Resolution:**
1. Server receives second `hub:connect` from same `actorIdentity`
2. Server closes first WebSocket with `hub:disconnect` (reason: 'duplicate_connection')
3. Server accepts second connection
4. Actor registry updated with new connectionId

### 4.5 Timeout Handling

| Event | Timeout | Action |
|-------|---------|--------|
| Connect response | 5s | Client: Close connection, retry |
| Heartbeat ack | 10s | Client: Consider connection dead, reconnect |
| Message delivery (ask) | 30s | Client: Treat as failed, retry or report error |
| Actor registration | 5s | Client: Retry registration |

---

## Phase 5: Message Delivery Guarantees

**Deliverable:** `docs/signal-hub/DELIVERY_GUARANTEES.md`

### 5.1 At-Most-Once (MVP)

**Default behavior:**
- Fire-and-forget (`pattern: 'tell'`)
- No server acknowledgment required
- Message may be lost during:
  - WebSocket disconnect
  - Durable Object eviction
  - Hibernation transition

**Use case:** Non-critical messages (heartbeats, status updates)

### 5.2 At-Least-Once (Phase 2 - Future)

**With acknowledgment:**
- Use `pattern: 'ask'` on `hub:send`
- Server responds with `hub:delivery_ack`
- Client retries if no ack within timeout

**Deduplication:**
- Server maintains short-lived (TTL-bounded) seen-set of message IDs
- Prevents duplicate processing on retransmission

### 5.3 Ordering Guarantees

**Per-connection FIFO:**
- Messages from Actor A → Actor B maintain send order
- Messages from different actors have no ordering guarantee

**No global ordering:**
- Broadcast messages may arrive in different order at different actors
- Topics (pub/sub) have no ordering guarantee

### 5.4 TTL Handling

- Message `ttl` field = milliseconds before expiration
- Server checks `timestamp + ttl < now()` before delivery
- Expired messages dropped with `hub:error` (code: 'message_expired')

### 5.5 Error Propagation

**Delivery failure scenarios:**

| Failure | Error Type | Retryable |
|---------|------------|-----------|
| Actor not registered | `hub:unknown_actor` | No |
| Message too large | `hub:message_too_large` | No |
| Rate limited | `hub:rate_limited` | Yes (after retryAfter) |
| Timeout | `hub:error` (code: 'timeout') | Yes |
| TTL expired | `hub:error` (code: 'message_expired') | No |

---

## Phase 6: Broadcast & Scalability

**Deliverable:** `docs/signal-hub/SCALABILITY.md`

### 6.1 Naive Broadcast Problem (Haiku Finding)

**Don't do this:**

```typescript
// ❌ Synchronous fan-out - CPU exhaustion on 1000+ actors
for (const actor of actors) {
  await sendToActor(actor, message);
}
```

**Why it fails:**
- Cloudflare Workers CPU limit: 30s
- 1000 actors × 30ms per send = 30s (exactly at limit)
- Any delay → timeout, partial broadcast

### 6.2 Async Batching Strategy

**Do this instead:**

```typescript
// ✅ Queue-based async batching
async function broadcast(message: SharedMessage) {
  const actors = await getRegisteredActors();

  if (actors.length <= 100) {
    // Small broadcast - synchronous OK
    for (const actor of actors) {
      await sendToActor(actor, message);
    }
    return { deliveredCount: actors.length };
  }

  // Large broadcast - queue for async processing
  await enqueueBroadcast({
    messageId: message.id,
    targetActors: actors.map(a => a.address),
    message
  });

  return { deliveredCount: 0, queued: actors.length };
}
```

**Durable queue implementation:**
- Use Cloudflare Queues for reliable async delivery
- Batch processing: 100 actors per queue message
- Max throughput: ~1K actors/sec per instance
- Queue consumer sends delivery status back to originating DO

### 6.3 Actor Registry Limits

**Per Signal Hub instance:**
- Max actors: 50K (Durable Object storage limit ~128MB)
- Registry structure: In-memory Map + durable storage backup
- Eviction: LRU with TTL (expired actors auto-removed)

**Two-tier storage (Haiku recommendation):**
- Tier 1 (volatile): In-memory Map for active actors (fast lookup)
- Tier 2 (durable): Durable Object storage for persistence
- On wake from hibernation: Restore from Tier 2 → Tier 1

### 6.4 Sharding Strategy (>50K Actors)

**Consistent hashing:**

```typescript
function getShardForActor(actorAddress: string, numShards: number): number {
  const hash = hashCode(actorAddress);
  return hash % numShards;
}

// Route actor registration to correct shard
const shardId = getShardForActor(actorAddress, 10);  // 10 shards = 500K actors
const hubStub = env.SIGNAL_HUB.get(env.SIGNAL_HUB.idFromName(`hub-shard-${shardId}`));
```

**Cross-shard discovery:**
- `hub:discover` queries all shards in parallel
- Aggregate results, deduplicate
- Return first N results (pagination)

### 6.5 Thundering Herd Protection

**Scenario:** 10K actors reconnect simultaneously (e.g., after deploy)

**Mitigation:**

**Client-side jitter:**
```typescript
// Random 0-30s delay before reconnect
const jitter = Math.random() * 30000;
await sleep(jitter);
await connect();
```

**Server-side rate limiting:**
```typescript
// Max 100 connections/sec per Signal Hub instance
const connectRate = new RateLimiter({ maxRate: 100, window: 1000 });

if (!await connectRate.allow(clientId)) {
  return { type: 'hub:rate_limited', payload: { retryAfter: 1000 } };
}
```

---

## Phase 7: Testing Strategy

**Deliverable:** `docs/signal-hub/TESTING.md`

### 7.1 Reconnection Scenarios (Priority 1)

**Test cases:**
- WebSocket disconnect during `hub:send` (message lost?)
- Reconnect after hibernation (session preserved?)
- Duplicate connection (first closed, second accepted?)
- Heartbeat timeout (client detects and reconnects?)

**Implementation:**
```typescript
// Use Miniflare to simulate WebSocket close
const ws = await connectToHub();
ws.close();  // Simulate network failure
await sleep(100);
const ws2 = await connectToHub();  // Reconnect
// Assert: Session restored, messages delivered
```

### 7.2 Message Delivery Guarantees (Priority 2)

**Test cases:**
- At-most-once: Message sent, no ack expected
- At-least-once: Message sent with `pattern: 'ask'`, ack received
- Deduplication: Same message ID sent twice, processed once
- TTL expiration: Message with ttl=100ms expires before delivery

### 7.3 Fan-Out & Backpressure (Priority 3)

**Test cases:**
- Broadcast to 100 actors (synchronous, should succeed)
- Broadcast to 1000 actors (async queue, check queue depth)
- Fast producer (1000 msgs/sec) → slow consumer (pause sent?)
- Queue stats: Check queueDepth, processingRate

**Load test:**
```typescript
// Simulate 10K actors, each sends 1 message
const actors = await createActors(10000);
await Promise.all(actors.map(a => a.send({ type: 'test' })));
// Assert: No CPU timeout, all messages delivered
```

### 7.4 Registration Consistency (Priority 4)

**Test cases:**
- Duplicate registration (version tracking, last-write-wins)
- Concurrent registration (atomic updates)
- TTL expiration (actor auto-removed after 5 min)
- Registry size limit (reject registration beyond 50K?)

### 7.5 Integration Tests (Priority 5)

**End-to-end flow:**

```typescript
// SEAG (local) → Signal Hub (Workers) → Browser
const seagActor = await createLocalActor();
const browserActor = await createBrowserActor();

// Register both with Signal Hub
await seagActor.register();
await browserActor.register();

// Send message SEAG → Browser via Signal Hub
const msg = await seagActor.send('@(browser/widget-123)', { type: 'render', payload: { data } });

// Assert: Browser receives message
const received = await browserActor.waitForMessage();
expect(received.type).toBe('render');
```

---

## Phase 8: Documentation (PROTOCOL.md)

**Deliverable:** `docs/signal-hub/PROTOCOL.md` (master document)

**Sections:**

1. **Overview** - What Signal Hub does, key use cases
2. **Architecture** - How it fits with SEAG/Browser, Durable Objects design
3. **Wire Protocol** - SharedMessage integration, type discriminators
4. **Message Types** - Complete catalog with request/response pairs
5. **Connection Lifecycle** - State machine, hibernation handling, timeouts
6. **Security** - Auth token validation, server-enforced identity
7. **Delivery Guarantees** - At-most-once (MVP), at-least-once (future)
8. **Scalability** - Broadcast queue, actor limits, sharding strategy
9. **Error Handling** - Error types, correlation, retryability
10. **Examples** - Common flows with runnable TypeScript code
11. **Cloudflare Constraints** - Hibernation, CPU limits, message size, storage
12. **Protocol Versioning** - How to evolve protocol (semver, negotiation)

**Include code examples for:**
- Connect with auth
- Register actor with TTL
- Send point-to-point with ack
- Broadcast to all actors
- Subscribe to topic
- Handle errors with retry

---

## Success Criteria (Updated)

✅ **Phase 1 - Wire Protocol**
- Uses SharedMessage from @agentic-primer/protocols
- No parallel WireMessage created
- Message types defined as type discriminators

✅ **Phase 2 - Message Types**
- All message types formally defined with request/response
- Cloudflare constraints documented (1MB, 30s CPU, hibernation)
- Flow control messages specified

✅ **Phase 3 - Security**
- Auth token in metadata, server validates JWT
- Server-enforced `from` field (prevent spoofing)
- Protocol version negotiation in connect

✅ **Phase 4 - Connection Lifecycle**
- Uses existing ConnectionState enum (no new states)
- Hibernation strategy defined (25s heartbeat)
- Duplicate connection handling specified

✅ **Phase 5 - Delivery Guarantees**
- At-most-once clearly stated for MVP
- At-least-once with ack defined for Phase 2
- Per-connection FIFO ordering

✅ **Phase 6 - Scalability**
- Async broadcast queue for >100 actors
- Max 50K actors per instance, sharding beyond
- Thundering herd protection (jitter + rate limit)

✅ **Phase 7 - Testing**
- 5 priority areas defined with specific test cases
- Reconnection scenarios (most fragile) = Priority 1
- End-to-end SEAG → Hub → Browser integration test

✅ **Phase 8 - Documentation**
- PROTOCOL.md master document with 12 sections
- Runnable TypeScript examples for each message type
- Cloudflare constraints explicitly documented

**Estimated effort:** 4-6 hours (2 focused sessions)

---

## Next Steps

1. ✅ **Phase 1 complete** - SHARED_MESSAGE_INTEGRATION.md written
2. 🔄 **Phase 2 in progress** - Writing MESSAGE_TYPES.md (formal spec)
3. ⏳ **Phase 3-8** - Sequential implementation after Phase 2

**Ready to proceed:** Yes - foundation is solid (SharedMessage integration)
