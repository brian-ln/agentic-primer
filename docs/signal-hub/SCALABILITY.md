# Signal Hub Scalability Design

**Status:** Phase 6 Complete
**Last Updated:** 2026-02-16
**Protocol Version:** 0.1.0

---

## Table of Contents

1. [Overview](#overview)
2. [Critical Problem: Naive Broadcast](#critical-problem-naive-broadcast)
3. [Async Batching Strategy](#async-batching-strategy)
4. [Actor Registry Limits](#actor-registry-limits)
5. [Sharding Strategy](#sharding-strategy)
6. [Thundering Herd Protection](#thundering-herd-protection)
7. [Performance Targets](#performance-targets)
8. [Implementation Examples](#implementation-examples)
9. [Monitoring and Observability](#monitoring-and-observability)

---

## Overview

Signal Hub scalability is constrained by three primary Cloudflare Workers/Durable Objects limits:

| Constraint | Limit | Impact |
|------------|-------|--------|
| CPU time per request | 30 seconds | Broadcast to >1000 actors risks timeout |
| Durable Object storage | 128MB (soft) | Max ~50K actor registrations per instance |
| WebSocket connections | ~50K per DO | Connection limit per Durable Object |

**This document addresses:**
- How to broadcast to 1000+ actors without CPU exhaustion
- How to scale actor registries beyond 50K actors
- How to handle thundering herd reconnections
- Production-ready implementation patterns

---

## Critical Problem: Naive Broadcast

### Why Synchronous Fan-Out Fails

**Problematic pattern (DO NOT USE):**

```typescript
// ❌ DANGEROUS: Synchronous broadcast causes CPU timeout
async function naiveBroadcast(message: SharedMessage) {
  const actors = await getRegisteredActors(); // 1000+ actors

  for (const actor of actors) {
    await sendToActor(actor, message); // ~30ms per send
  }

  return { deliveredCount: actors.length };
}
```

**Why this fails:**

```
1000 actors × 30ms per send = 30,000ms = 30 seconds
```

- At exactly the Cloudflare CPU limit
- Any delay (network jitter, serialization overhead) → timeout
- Timeout causes partial broadcast → message loss
- Worse: timeout may trigger Durable Object eviction → all connections drop

**Real-world measurement:**
- WebSocket `send()` in Durable Objects: 20-40ms per message
- Serialization overhead: 1-5ms per message
- Total per-actor cost: 25-45ms
- **Safe threshold: 100 actors maximum for synchronous fan-out**

### The CPU Exhaustion Cascade

```
Broadcast to 2000 actors (60s needed)
    ↓
CPU timeout after 30s (1000 actors reached)
    ↓
Durable Object evicts
    ↓
All 2000 WebSocket connections drop
    ↓
2000 actors attempt reconnect simultaneously
    ↓
Thundering herd → another CPU timeout
    ↓
Cascade failure
```

**Critical insight:** Broadcast is the highest-risk operation in Signal Hub. Without proper batching, it becomes a denial-of-service vector.

---

## Async Batching Strategy

### Small Broadcast: Synchronous Fan-Out

For **≤100 actors**, synchronous fan-out is safe and fast:

```typescript
async function smallBroadcast(message: SharedMessage, actors: ActorRegistration[]) {
  if (actors.length > 100) {
    throw new Error('Use largeBroadcast() for >100 actors');
  }

  const serialized = JSON.stringify(message);
  const sockets = this.ctx.getWebSockets();

  let deliveredCount = 0;
  let failedCount = 0;

  for (const actor of actors) {
    const ws = sockets.find(s => s.actorAddress === actor.actorAddress);
    if (!ws) {
      failedCount++;
      continue;
    }

    try {
      ws.send(serialized);
      deliveredCount++;
    } catch (error) {
      failedCount++;
      console.error(`Broadcast failed for ${actor.actorAddress}:`, error);
    }
  }

  return { deliveredCount, failedCount };
}
```

**Performance:**
- 100 actors × 30ms = 3 seconds (safe)
- All messages sent in single request
- No queue overhead
- Immediate delivery confirmation

### Large Broadcast: Queue-Based Async Batching

For **>100 actors**, use Cloudflare Queues for async processing:

```typescript
interface BroadcastJob {
  messageId: string;
  message: SharedMessage;
  targetActors: string[];  // Actor addresses
  batchIndex: number;
  totalBatches: number;
  originatingHubId: string;
}

async function largeBroadcast(
  message: SharedMessage,
  actors: ActorRegistration[]
): Promise<{ queued: number; immediate: number }> {

  const BATCH_SIZE = 100; // Safe synchronous limit
  const batches = chunkArray(actors, BATCH_SIZE);

  // Send first batch immediately (no queue latency)
  const firstBatch = batches[0];
  const immediate = await smallBroadcast(message, firstBatch);

  // Queue remaining batches
  for (let i = 1; i < batches.length; i++) {
    const batch = batches[i];
    const job: BroadcastJob = {
      messageId: message.id,
      message,
      targetActors: batch.map(a => a.actorAddress),
      batchIndex: i,
      totalBatches: batches.length,
      originatingHubId: this.durableObjectId,
    };

    await this.env.BROADCAST_QUEUE.send(job);
  }

  const queued = actors.length - firstBatch.length;

  return {
    immediate: immediate.deliveredCount,
    queued,
  };
}

// Helper: Chunk array into batches
function chunkArray<T>(arr: T[], size: number): T[][] {
  const chunks: T[][] = [];
  for (let i = 0; i < arr.length; i += size) {
    chunks.push(arr.slice(i, i + size));
  }
  return chunks;
}
```

### Queue Consumer Implementation

```typescript
export default {
  async queue(batch: MessageBatch<BroadcastJob>, env: Env): Promise<void> {
    for (const message of batch.messages) {
      const job = message.body;

      try {
        // Get Durable Object stub for originating hub
        const hubId = env.SIGNAL_HUB.idFromString(job.originatingHubId);
        const hubStub = env.SIGNAL_HUB.get(hubId);

        // Execute batch delivery
        const response = await hubStub.fetch(new Request('https://hub/internal/broadcast-batch', {
          method: 'POST',
          body: JSON.stringify(job),
        }));

        if (!response.ok) {
          console.error(`Batch ${job.batchIndex}/${job.totalBatches} failed:`, await response.text());
          message.retry(); // Retry failed batch
        } else {
          message.ack(); // Success
        }
      } catch (error) {
        console.error(`Queue consumer error:`, error);
        message.retry({ delaySeconds: 5 }); // Exponential backoff
      }
    }
  }
};
```

### Durable Object Batch Handler

```typescript
// In SignalHubDurableObject
async handleBroadcastBatch(job: BroadcastJob): Promise<Response> {
  const sockets = this.ctx.getWebSockets();
  const serialized = JSON.stringify(job.message);

  let deliveredCount = 0;
  let failedCount = 0;

  for (const actorAddress of job.targetActors) {
    const ws = sockets.find(s => s.actorAddress === actorAddress);
    if (!ws) {
      failedCount++;
      continue;
    }

    try {
      ws.send(serialized);
      deliveredCount++;
    } catch (error) {
      failedCount++;
    }
  }

  // Report batch completion to monitoring
  console.log(`Batch ${job.batchIndex}/${job.totalBatches} complete:`, {
    deliveredCount,
    failedCount,
    messageId: job.messageId,
  });

  return new Response(JSON.stringify({ deliveredCount, failedCount }), {
    status: 200,
    headers: { 'Content-Type': 'application/json' },
  });
}
```

### Broadcast Flow Diagram

```
Client sends hub:broadcast
    ↓
Server: actors.length > 100?
    ↓                    ↓
  YES (large)          NO (small)
    ↓                    ↓
Split into batches    Synchronous fan-out
    ↓                    ↓
Batch 0 → immediate   Return immediately
Batches 1+ → queue
    ↓
Return hub:broadcast_ack
    ↓
Queue consumer processes batches (async)
    ↓
Each batch → hub:internal/broadcast-batch
    ↓
Send to actors in batch (100 at a time)
    ↓
Batch complete (2-5s latency)
```

**Performance characteristics:**

| Actor Count | Strategy | Latency | CPU Time | Risk |
|-------------|----------|---------|----------|------|
| 1-100 | Synchronous | <5s | 3-5s | Low |
| 100-1000 | Mixed (first batch sync, rest queued) | 5-15s | <5s per batch | Low |
| 1000-10K | Queued batching | 15-60s | <5s per batch | Low |
| 10K+ | Queued + sharding | 30-120s | <5s per batch | Medium |

---

## Actor Registry Limits

### Single Instance Capacity

**Maximum actors per Signal Hub instance: 50,000**

**Calculation:**

```
Per-actor memory footprint:
- Registration record: ~400 bytes
- WebSocket metadata: ~200 bytes
- Heartbeat timer: ~100 bytes
Total: ~700 bytes/actor

Durable Object memory limit: ~128MB (soft limit)
Maximum actors: 128MB / 700 bytes ≈ 180,000 actors (theoretical)

Practical limit (with overhead): 50,000 actors
```

**Why 50K instead of 180K?**
- Need memory headroom for:
  - Broadcast queues
  - Pending message buffers
  - Connection state machines
  - Error tracking
- CPU throughput: 50K actors × 30s heartbeat = 1,666 heartbeats/sec
- Storage operations: 50K PUTs/hour = $0.01/hour cost

### Two-Tier Storage Architecture

**Problem:** Durable Object restarts lose in-memory state.

**Solution:** Hybrid volatile + durable storage:

```typescript
interface ActorRegistry {
  // Tier 1: Volatile (fast, in-memory)
  volatile: Map<string, ActorRegistration>;

  // Tier 2: Durable (persistent, slower)
  storage: DurableObjectStorage;
}

class SignalHubDurableObject {
  private volatile = new Map<string, ActorRegistration>();

  constructor(state: DurableObjectState, env: Env) {
    this.ctx = state;
    this.env = env;

    // Restore from durable storage on wake
    this.ctx.blockConcurrencyWhile(async () => {
      await this.restoreVolatileFromDurable();
    });
  }

  // Restore volatile registry from durable storage
  private async restoreVolatileFromDurable(): Promise<void> {
    const keys = await this.ctx.storage.list({ prefix: 'actor:' });

    let restoredCount = 0;
    let expiredCount = 0;

    for (const [key, reg] of keys) {
      const actorReg = reg as ActorRegistration;

      // Check if registration expired during downtime
      if (Date.now() > actorReg.expiresAt) {
        expiredCount++;
        await this.ctx.storage.delete(key); // Clean up expired
        continue;
      }

      // Restore to volatile map
      this.volatile.set(actorReg.actorAddress, actorReg);
      restoredCount++;
    }

    console.log(`Registry restored: ${restoredCount} active, ${expiredCount} expired`);
  }

  // Register actor (write to both tiers)
  async handleRegister(msg: HubRegisterMessage): Promise<void> {
    const { actorAddress, capabilities, metadata, ttlSeconds = 300 } = msg.payload;
    const now = Date.now();
    const expiresAt = now + (ttlSeconds * 1000);

    const registration: ActorRegistration = {
      actorAddress,
      capabilities,
      metadata,
      connectionId: msg.metadata.connectionId,
      registeredAt: now,
      expiresAt,
      version: 1,
    };

    // Update volatile (immediate)
    this.volatile.set(actorAddress, registration);

    // Update durable (persistent)
    await this.ctx.storage.put(`actor:${actorAddress}`, registration, {
      expirationTtl: ttlSeconds, // Auto-delete after TTL
    });

    // Respond
    await this.send(msg.from, {
      type: 'hub:registered',
      pattern: 'tell',
      correlationId: msg.id,
      payload: {
        actorAddress,
        expiresAt,
        renewalToken: this.generateRenewalToken(actorAddress),
        version: registration.version,
      },
    });
  }

  // Heartbeat updates volatile only (reduce storage writes)
  handleHeartbeat(msg: HubHeartbeatMessage): void {
    const actor = this.volatile.get(msg.from);
    if (actor) {
      actor.lastHeartbeat = Date.now();
      // Don't write to durable storage on every heartbeat
      // Only persist on registration/renewal
    }
  }

  // Periodic cleanup of expired actors
  async cleanupExpiredActors(): Promise<void> {
    const now = Date.now();
    const expired: string[] = [];

    for (const [address, reg] of this.volatile) {
      if (now > reg.expiresAt) {
        expired.push(address);
      }
    }

    for (const address of expired) {
      this.volatile.delete(address);
      await this.ctx.storage.delete(`actor:${address}`);
    }

    console.log(`Cleaned up ${expired.length} expired actors`);
  }
}
```

### Registry Size Monitoring

```typescript
interface RegistryMetrics {
  totalActors: number;
  memoryUsageBytes: number;
  storageUsageBytes: number;
  capacityUtilization: number; // 0.0-1.0
}

function getRegistryMetrics(): RegistryMetrics {
  const totalActors = this.volatile.size;
  const memoryUsageBytes = totalActors * 700; // ~700 bytes/actor
  const storageUsageBytes = totalActors * 500; // ~500 bytes/actor in durable storage
  const capacityUtilization = totalActors / 50000; // 50K actor limit

  return {
    totalActors,
    memoryUsageBytes,
    storageUsageBytes,
    capacityUtilization,
  };
}

// Reject new registrations if over capacity
async handleRegister(msg: HubRegisterMessage): Promise<void> {
  const metrics = this.getRegistryMetrics();

  if (metrics.capacityUtilization > 0.95) { // 95% full
    await this.send(msg.from, {
      type: 'hub:error',
      pattern: 'tell',
      correlationId: msg.id,
      payload: {
        code: 'registry_full',
        message: `Signal Hub instance at capacity (${metrics.totalActors}/50000 actors)`,
        details: { metrics },
        retryable: true, // Client should try different shard
      },
    });
    return;
  }

  // Proceed with registration...
}
```

---

## Sharding Strategy

### When to Shard

**Shard when:**
- Single instance exceeds 50K actors
- Broadcast latency exceeds acceptable threshold
- Geographic distribution needed (edge locations)

### Consistent Hashing for Sharding

```typescript
interface ShardingConfig {
  numShards: number;
  shardIds: string[];
}

// FNV-1a hash (fast, good distribution)
function hashString(str: string): number {
  let hash = 2166136261; // FNV offset basis
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash *= 16777619; // FNV prime
  }
  return hash >>> 0; // Convert to unsigned 32-bit
}

// Get shard ID for actor address
function getShardForActor(actorAddress: string, config: ShardingConfig): string {
  const hash = hashString(actorAddress);
  const shardIndex = hash % config.numShards;
  return config.shardIds[shardIndex];
}

// Example: 10 shards = 500K actor capacity
const config: ShardingConfig = {
  numShards: 10,
  shardIds: Array.from({ length: 10 }, (_, i) => `signal-hub-shard-${i}`),
};

// Route actor to correct shard
async function connectToSignalHub(
  actorAddress: string,
  env: Env
): Promise<DurableObjectStub> {
  const shardId = getShardForActor(actorAddress, config);
  const durableObjectId = env.SIGNAL_HUB.idFromName(shardId);
  return env.SIGNAL_HUB.get(durableObjectId);
}
```

### Shard Router Worker

```typescript
export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);

    // WebSocket upgrade request
    if (request.headers.get('Upgrade') === 'websocket') {
      // Extract actor address from query param or auth token
      const actorAddress = url.searchParams.get('actor');
      if (!actorAddress) {
        return new Response('Missing actor address', { status: 400 });
      }

      // Route to correct shard
      const shardId = getShardForActor(actorAddress, SHARD_CONFIG);
      const hubStub = env.SIGNAL_HUB.get(env.SIGNAL_HUB.idFromName(shardId));

      // Forward request to shard
      return hubStub.fetch(request);
    }

    // HTTP API requests
    if (url.pathname === '/discover') {
      return handleCrossShardDiscover(request, env);
    }

    return new Response('Not Found', { status: 404 });
  }
};
```

### Cross-Shard Discovery

```typescript
interface DiscoveryQuery {
  pattern: string;
  limit: number;
}

async function handleCrossShardDiscover(
  request: Request,
  env: Env
): Promise<Response> {
  const body = await request.json() as DiscoveryQuery;
  const { pattern, limit } = body;

  // Query all shards in parallel
  const shardPromises = SHARD_CONFIG.shardIds.map(async (shardId) => {
    const hubStub = env.SIGNAL_HUB.get(env.SIGNAL_HUB.idFromName(shardId));

    try {
      const response = await hubStub.fetch(new Request('https://hub/internal/discover', {
        method: 'POST',
        body: JSON.stringify({ pattern, limit }),
      }));

      if (!response.ok) {
        return [];
      }

      const data = await response.json() as { actors: ActorRegistration[] };
      return data.actors;
    } catch (error) {
      console.error(`Shard ${shardId} discovery failed:`, error);
      return [];
    }
  });

  // Aggregate results
  const shardResults = await Promise.all(shardPromises);
  const allActors = shardResults.flat();

  // Deduplicate (shouldn't happen, but safety check)
  const uniqueActors = new Map<string, ActorRegistration>();
  for (const actor of allActors) {
    if (!uniqueActors.has(actor.actorAddress)) {
      uniqueActors.set(actor.actorAddress, actor);
    }
  }

  // Sort by registration time, return first N
  const sorted = Array.from(uniqueActors.values())
    .sort((a, b) => b.registeredAt - a.registeredAt)
    .slice(0, limit);

  return new Response(JSON.stringify({
    actors: sorted,
    hasMore: allActors.length > limit,
    queriedShards: SHARD_CONFIG.numShards,
  }), {
    headers: { 'Content-Type': 'application/json' },
  });
}
```

### Shard Rebalancing

**When to rebalance:**
- Add new shards (numShards increases)
- Remove failed shards
- Migrate actors between shards

**Strategy:**

```typescript
// Gradual migration (minimize disruption)
async function rebalanceShards(
  oldConfig: ShardingConfig,
  newConfig: ShardingConfig
): Promise<void> {

  // For each actor in old shards
  for (const oldShardId of oldConfig.shardIds) {
    const oldHub = env.SIGNAL_HUB.get(env.SIGNAL_HUB.idFromName(oldShardId));

    // Get all actors in this shard
    const response = await oldHub.fetch(new Request('https://hub/internal/list-all'));
    const actors = await response.json() as ActorRegistration[];

    // Determine new shard for each actor
    for (const actor of actors) {
      const newShardId = getShardForActor(actor.actorAddress, newConfig);

      if (newShardId !== oldShardId) {
        // Actor needs to move
        console.log(`Migrate ${actor.actorAddress}: ${oldShardId} → ${newShardId}`);

        // Send hub:disconnect to actor (with reason: 'rebalancing')
        // Actor will reconnect, router will send to new shard
        await oldHub.fetch(new Request('https://hub/internal/disconnect-actor', {
          method: 'POST',
          body: JSON.stringify({
            actorAddress: actor.actorAddress,
            reason: 'shard_rebalancing',
          }),
        }));
      }
    }
  }
}
```

---

## Thundering Herd Protection

### Problem Statement

**Scenario:** Signal Hub Durable Object restarts (deployment, eviction, crash)

```
10,000 actors connected
    ↓
Durable Object restarts
    ↓
All 10,000 WebSocket connections close simultaneously
    ↓
All 10,000 actors attempt reconnect immediately
    ↓
10,000 WebSocket upgrade requests hit Signal Hub in <1 second
    ↓
CPU exhaustion → timeout → eviction
    ↓
Cascade failure
```

### Client-Side Jitter

```typescript
interface ReconnectConfig {
  initialDelayMs: number;
  maxDelayMs: number;
  multiplier: number;
  jitterMs: number; // Random delay on first attempt
}

const RECONNECT_CONFIG: ReconnectConfig = {
  initialDelayMs: 100,
  maxDelayMs: 30000,
  multiplier: 2,
  jitterMs: 30000, // 0-30s random jitter
};

class SignalHubClient {
  private reconnectAttempt = 0;

  async connect(): Promise<void> {
    try {
      // Establish WebSocket connection
      this.ws = new WebSocket(this.url);
      await this.waitForOpen();

      // Send hub:connect
      await this.sendConnect();

      // Reset reconnect counter on success
      this.reconnectAttempt = 0;

    } catch (error) {
      console.error('Connection failed:', error);
      await this.reconnect();
    }
  }

  private async reconnect(): Promise<void> {
    const attempt = this.reconnectAttempt++;

    // Calculate delay with exponential backoff
    const baseDelay = Math.min(
      RECONNECT_CONFIG.initialDelayMs * Math.pow(RECONNECT_CONFIG.multiplier, attempt),
      RECONNECT_CONFIG.maxDelayMs
    );

    // Add jitter ONLY on first attempt (distribute thundering herd)
    const jitter = attempt === 0
      ? Math.random() * RECONNECT_CONFIG.jitterMs
      : 0;

    const totalDelay = baseDelay + jitter;

    console.log(`Reconnect attempt ${attempt} in ${totalDelay}ms`);
    await new Promise(resolve => setTimeout(resolve, totalDelay));

    // Retry connection
    await this.connect();
  }
}
```

**Why this works:**
- First reconnect: 0-30s random delay spreads reconnections over 30-second window
- Subsequent reconnects: Exponential backoff (100ms, 200ms, 400ms, ...)
- 10,000 actors / 30 seconds = ~333 connections/second (manageable)

### Server-Side Rate Limiting

```typescript
class TokenBucketRateLimiter {
  private tokens: number;
  private lastRefill: number;
  private readonly capacity: number;
  private readonly refillRate: number; // tokens/second

  constructor(capacity: number, refillRate: number) {
    this.capacity = capacity;
    this.refillRate = refillRate;
    this.tokens = capacity;
    this.lastRefill = Date.now();
  }

  async allow(): Promise<boolean> {
    this.refill();

    if (this.tokens >= 1) {
      this.tokens--;
      return true;
    }

    return false;
  }

  private refill(): void {
    const now = Date.now();
    const elapsed = (now - this.lastRefill) / 1000; // seconds
    const tokensToAdd = elapsed * this.refillRate;

    this.tokens = Math.min(this.capacity, this.tokens + tokensToAdd);
    this.lastRefill = now;
  }

  getRetryAfterMs(): number {
    const tokensNeeded = 1 - this.tokens;
    const secondsNeeded = tokensNeeded / this.refillRate;
    return Math.ceil(secondsNeeded * 1000);
  }
}

class SignalHubDurableObject {
  private connectionRateLimiter = new TokenBucketRateLimiter(
    100, // capacity: 100 tokens
    100  // refillRate: 100 tokens/sec = 100 connections/sec
  );

  async fetch(request: Request): Promise<Response> {
    // WebSocket upgrade request
    if (request.headers.get('Upgrade') === 'websocket') {
      // Check rate limit
      if (!await this.connectionRateLimiter.allow()) {
        const retryAfter = this.connectionRateLimiter.getRetryAfterMs();

        return new Response('Rate Limited', {
          status: 429,
          headers: {
            'Retry-After': String(Math.ceil(retryAfter / 1000)), // seconds
            'X-RateLimit-Limit': '100',
            'X-RateLimit-Remaining': String(Math.floor(this.connectionRateLimiter.tokens)),
          },
        });
      }

      // Accept WebSocket connection
      return this.handleWebSocketUpgrade(request);
    }

    // Other requests...
    return new Response('Not Found', { status: 404 });
  }
}
```

**Rate limit configuration:**

| Scenario | Capacity | Refill Rate | Burst Support | Steady State |
|----------|----------|-------------|---------------|--------------|
| Production | 100 | 100/sec | 100 immediate | 100/sec sustained |
| High traffic | 200 | 200/sec | 200 immediate | 200/sec sustained |
| Development | 10 | 10/sec | 10 immediate | 10/sec sustained |

### Graceful Degradation

```typescript
async handleWebSocketUpgrade(request: Request): Promise<Response> {
  const metrics = this.getRegistryMetrics();

  // If at 95% capacity, reject new connections
  if (metrics.capacityUtilization > 0.95) {
    return new Response('Service at capacity', {
      status: 503,
      headers: {
        'Retry-After': '60', // Try again in 60 seconds
        'X-Capacity-Utilization': String(metrics.capacityUtilization),
      },
    });
  }

  // If at 90% capacity, warn client
  if (metrics.capacityUtilization > 0.90) {
    console.warn(`Signal Hub at ${metrics.capacityUtilization * 100}% capacity`);
  }

  // Accept connection
  const pair = new WebSocketPair();
  const [client, server] = Object.values(pair);

  this.ctx.acceptWebSocket(server);

  return new Response(null, {
    status: 101,
    webSocket: client,
  });
}
```

---

## Performance Targets

### Broadcast Throughput

| Actor Count | Target Latency | CPU Time | Strategy |
|-------------|----------------|----------|----------|
| 1-100 | <5s | <5s | Synchronous |
| 100-1K | <15s | <5s per batch | Queue (first batch immediate) |
| 1K-10K | <60s | <5s per batch | Queue (all batches) |
| 10K-50K | <120s | <5s per batch | Queue + parallel workers |

**Target: 1000 actors/second per Signal Hub instance**

```typescript
// Benchmark broadcast throughput
async function benchmarkBroadcast(actorCount: number): Promise<void> {
  const startTime = Date.now();

  const result = await hub.broadcast({
    type: 'hub:broadcast',
    pattern: 'tell',
    payload: {
      message: { type: 'benchmark', payload: { size: 1024 } },
    },
  });

  const endTime = Date.now();
  const durationSec = (endTime - startTime) / 1000;
  const throughput = actorCount / durationSec;

  console.log(`Broadcast to ${actorCount} actors:`);
  console.log(`  Duration: ${durationSec.toFixed(2)}s`);
  console.log(`  Throughput: ${throughput.toFixed(0)} actors/sec`);
  console.log(`  Delivered: ${result.deliveredCount}`);
  console.log(`  Queued: ${result.queuedCount || 0}`);
  console.log(`  Failed: ${result.failedCount || 0}`);
}
```

### Registry Lookup Performance

**Target: <10ms per lookup**

```typescript
// In-memory Map lookup (Tier 1)
function getActorRegistration(actorAddress: string): ActorRegistration | undefined {
  return this.volatile.get(actorAddress); // O(1) lookup, <1ms
}

// Durable storage lookup (Tier 2, slower)
async function getActorRegistrationDurable(actorAddress: string): Promise<ActorRegistration | undefined> {
  return await this.ctx.storage.get<ActorRegistration>(`actor:${actorAddress}`); // <10ms
}
```

### Connection Rate

**Target: 100 connections/second sustained**

```typescript
// Monitor connection rate
class ConnectionMetrics {
  private connectionsLastSecond = 0;
  private lastReset = Date.now();

  recordConnection(): void {
    this.connectionsLastSecond++;

    const now = Date.now();
    if (now - this.lastReset >= 1000) {
      console.log(`Connection rate: ${this.connectionsLastSecond}/sec`);
      this.connectionsLastSecond = 0;
      this.lastReset = now;
    }
  }
}
```

### Message Delivery Latency

| Message Type | Target P50 | Target P99 | Notes |
|--------------|------------|------------|-------|
| hub:send | <50ms | <200ms | Point-to-point |
| hub:broadcast (≤100) | <5s | <10s | Synchronous |
| hub:broadcast (>100) | <30s | <60s | Queued |
| hub:heartbeat | <10ms | <50ms | Critical for connection health |

---

## Implementation Examples

### Complete Broadcast Implementation

```typescript
class SignalHubDurableObject {
  private volatile = new Map<string, ActorRegistration>();
  private env: Env;
  private ctx: DurableObjectState;

  async handleBroadcast(msg: HubBroadcastMessage): Promise<void> {
    const actors = Array.from(this.volatile.values());

    // Filter by capability if specified
    const targetActors = msg.metadata.targetCapability
      ? actors.filter(a => a.capabilities.includes(msg.metadata.targetCapability))
      : actors;

    // Exclude sender if requested
    const recipients = msg.payload.excludeSelf
      ? targetActors.filter(a => a.actorAddress !== msg.from)
      : targetActors;

    let result: BroadcastResult;

    if (recipients.length <= 100) {
      // Small broadcast: synchronous
      result = await this.broadcastSync(msg.payload.message, recipients);
    } else {
      // Large broadcast: queued
      result = await this.broadcastQueued(msg.payload.message, recipients);
    }

    // Send acknowledgment
    await this.send(msg.from, {
      type: 'hub:broadcast_ack',
      pattern: 'tell',
      payload: {
        messageId: msg.id,
        deliveredCount: result.deliveredCount,
        queuedCount: result.queuedCount || 0,
        failedCount: result.failedCount || 0,
      },
    });
  }

  private async broadcastSync(
    message: unknown,
    recipients: ActorRegistration[]
  ): Promise<BroadcastResult> {
    const serialized = JSON.stringify(message);
    const sockets = this.ctx.getWebSockets();

    let deliveredCount = 0;
    let failedCount = 0;

    for (const recipient of recipients) {
      const ws = sockets.find(s => (s as any).actorAddress === recipient.actorAddress);
      if (!ws) {
        failedCount++;
        continue;
      }

      try {
        ws.send(serialized);
        deliveredCount++;
      } catch (error) {
        failedCount++;
        console.error(`Broadcast failed for ${recipient.actorAddress}:`, error);
      }
    }

    return { deliveredCount, failedCount };
  }

  private async broadcastQueued(
    message: unknown,
    recipients: ActorRegistration[]
  ): Promise<BroadcastResult> {
    const BATCH_SIZE = 100;
    const batches = chunkArray(recipients, BATCH_SIZE);

    // Send first batch immediately (no queue latency)
    const firstBatch = batches[0];
    const immediate = await this.broadcastSync(message, firstBatch);

    // Queue remaining batches
    for (let i = 1; i < batches.length; i++) {
      const batch = batches[i];
      const job: BroadcastJob = {
        messageId: crypto.randomUUID(),
        message,
        targetActors: batch.map(a => a.actorAddress),
        batchIndex: i,
        totalBatches: batches.length,
        originatingHubId: this.ctx.id.toString(),
      };

      await this.env.BROADCAST_QUEUE.send(job);
    }

    return {
      deliveredCount: immediate.deliveredCount,
      queuedCount: recipients.length - firstBatch.length,
      failedCount: immediate.failedCount,
    };
  }
}

interface BroadcastResult {
  deliveredCount: number;
  queuedCount?: number;
  failedCount?: number;
}
```

### Token Bucket Rate Limiter (Production-Ready)

```typescript
export class TokenBucketRateLimiter {
  private tokens: number;
  private lastRefill: number;
  private readonly capacity: number;
  private readonly refillRate: number;

  /**
   * Create a token bucket rate limiter.
   * @param capacity - Maximum tokens (burst capacity)
   * @param refillRate - Tokens added per second (sustained rate)
   */
  constructor(capacity: number, refillRate: number) {
    if (capacity <= 0 || refillRate <= 0) {
      throw new Error('Capacity and refillRate must be positive');
    }

    this.capacity = capacity;
    this.refillRate = refillRate;
    this.tokens = capacity; // Start full
    this.lastRefill = Date.now();
  }

  /**
   * Check if a request can proceed.
   * @param tokens - Number of tokens to consume (default: 1)
   * @returns true if allowed, false if rate limited
   */
  async allow(tokens: number = 1): Promise<boolean> {
    if (tokens <= 0) {
      throw new Error('Token count must be positive');
    }

    this.refill();

    if (this.tokens >= tokens) {
      this.tokens -= tokens;
      return true;
    }

    return false;
  }

  /**
   * Refill tokens based on elapsed time.
   */
  private refill(): void {
    const now = Date.now();
    const elapsedMs = now - this.lastRefill;
    const elapsedSec = elapsedMs / 1000;

    const tokensToAdd = elapsedSec * this.refillRate;
    this.tokens = Math.min(this.capacity, this.tokens + tokensToAdd);
    this.lastRefill = now;
  }

  /**
   * Calculate retry delay in milliseconds.
   * @param tokens - Number of tokens needed
   * @returns Milliseconds until tokens available
   */
  getRetryAfterMs(tokens: number = 1): number {
    this.refill();

    if (this.tokens >= tokens) {
      return 0; // Already have enough tokens
    }

    const tokensNeeded = tokens - this.tokens;
    const secondsNeeded = tokensNeeded / this.refillRate;
    return Math.ceil(secondsNeeded * 1000);
  }

  /**
   * Get current state for monitoring.
   */
  getState(): { tokens: number; capacity: number; refillRate: number } {
    this.refill();
    return {
      tokens: this.tokens,
      capacity: this.capacity,
      refillRate: this.refillRate,
    };
  }
}
```

### Consistent Hash Ring (Advanced Sharding)

```typescript
export class ConsistentHashRing {
  private readonly virtualNodes: number;
  private readonly ring: Map<number, string>;
  private readonly sortedHashes: number[];

  /**
   * Create a consistent hash ring.
   * @param shardIds - List of shard identifiers
   * @param virtualNodes - Virtual nodes per shard (higher = better distribution)
   */
  constructor(shardIds: string[], virtualNodes: number = 150) {
    this.virtualNodes = virtualNodes;
    this.ring = new Map();
    this.sortedHashes = [];

    // Add virtual nodes for each shard
    for (const shardId of shardIds) {
      this.addShard(shardId);
    }
  }

  /**
   * Add a shard to the ring.
   */
  private addShard(shardId: string): void {
    for (let i = 0; i < this.virtualNodes; i++) {
      const virtualKey = `${shardId}:vnode:${i}`;
      const hash = hashString(virtualKey);
      this.ring.set(hash, shardId);
      this.sortedHashes.push(hash);
    }

    // Keep hashes sorted for binary search
    this.sortedHashes.sort((a, b) => a - b);
  }

  /**
   * Get shard for a given key.
   */
  getShard(key: string): string {
    if (this.ring.size === 0) {
      throw new Error('Hash ring is empty');
    }

    const hash = hashString(key);

    // Find first virtual node >= hash
    const index = this.binarySearch(hash);
    const nodeHash = this.sortedHashes[index];

    return this.ring.get(nodeHash)!;
  }

  /**
   * Binary search for first hash >= target.
   */
  private binarySearch(target: number): number {
    let left = 0;
    let right = this.sortedHashes.length - 1;

    while (left < right) {
      const mid = Math.floor((left + right) / 2);
      if (this.sortedHashes[mid] < target) {
        left = mid + 1;
      } else {
        right = mid;
      }
    }

    // Wrap around if target > all hashes
    return left % this.sortedHashes.length;
  }

  /**
   * Get distribution statistics.
   */
  getDistribution(keys: string[]): Record<string, number> {
    const distribution: Record<string, number> = {};

    for (const key of keys) {
      const shard = this.getShard(key);
      distribution[shard] = (distribution[shard] || 0) + 1;
    }

    return distribution;
  }
}

// FNV-1a hash function
function hashString(str: string): number {
  let hash = 2166136261;
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0;
}
```

---

## Monitoring and Observability

### Key Metrics to Track

```typescript
interface SignalHubMetrics {
  // Registry
  totalActors: number;
  actorsPerMinute: number; // Registration rate
  capacityUtilization: number; // 0.0-1.0

  // Broadcast
  broadcastsPerMinute: number;
  broadcastLatencyP50: number;
  broadcastLatencyP99: number;
  broadcastFailureRate: number;

  // Messages
  messagesReceivedPerSec: number;
  messagesSentPerSec: number;
  messageQueueDepth: number;

  // Connections
  activeConnections: number;
  connectionRatePerSec: number;
  reconnectionRatePerSec: number;

  // Performance
  cpuTimeMs: number;
  memoryUsageBytes: number;
  storageUsageBytes: number;

  // Errors
  errorCountByCode: Record<string, number>;
}

class MetricsCollector {
  private metrics: SignalHubMetrics = {
    totalActors: 0,
    actorsPerMinute: 0,
    capacityUtilization: 0,
    broadcastsPerMinute: 0,
    broadcastLatencyP50: 0,
    broadcastLatencyP99: 0,
    broadcastFailureRate: 0,
    messagesReceivedPerSec: 0,
    messagesSentPerSec: 0,
    messageQueueDepth: 0,
    activeConnections: 0,
    connectionRatePerSec: 0,
    reconnectionRatePerSec: 0,
    cpuTimeMs: 0,
    memoryUsageBytes: 0,
    storageUsageBytes: 0,
    errorCountByCode: {},
  };

  private broadcastLatencies: number[] = [];

  recordBroadcast(latencyMs: number, failed: boolean): void {
    this.broadcastLatencies.push(latencyMs);
    if (this.broadcastLatencies.length > 100) {
      this.broadcastLatencies.shift(); // Keep last 100
    }

    if (failed) {
      this.metrics.broadcastFailureRate++;
    }
  }

  getMetrics(): SignalHubMetrics {
    // Calculate percentiles
    if (this.broadcastLatencies.length > 0) {
      const sorted = this.broadcastLatencies.slice().sort((a, b) => a - b);
      this.metrics.broadcastLatencyP50 = sorted[Math.floor(sorted.length * 0.5)];
      this.metrics.broadcastLatencyP99 = sorted[Math.floor(sorted.length * 0.99)];
    }

    return { ...this.metrics };
  }

  exportPrometheus(): string {
    const m = this.metrics;
    return `
# HELP signal_hub_actors_total Total registered actors
# TYPE signal_hub_actors_total gauge
signal_hub_actors_total ${m.totalActors}

# HELP signal_hub_capacity_utilization Registry capacity utilization (0-1)
# TYPE signal_hub_capacity_utilization gauge
signal_hub_capacity_utilization ${m.capacityUtilization}

# HELP signal_hub_broadcast_latency_seconds Broadcast latency
# TYPE signal_hub_broadcast_latency_seconds summary
signal_hub_broadcast_latency_seconds{quantile="0.5"} ${m.broadcastLatencyP50 / 1000}
signal_hub_broadcast_latency_seconds{quantile="0.99"} ${m.broadcastLatencyP99 / 1000}

# HELP signal_hub_connections_active Active WebSocket connections
# TYPE signal_hub_connections_active gauge
signal_hub_connections_active ${m.activeConnections}

# HELP signal_hub_messages_received_total Messages received
# TYPE signal_hub_messages_received_total counter
signal_hub_messages_received_total ${m.messagesReceivedPerSec}
    `.trim();
  }
}
```

### Alerting Thresholds

```typescript
interface AlertThresholds {
  capacityUtilization: number; // Alert at 90%
  broadcastLatencyP99: number; // Alert at 120s
  errorRate: number; // Alert at 5%
  connectionRate: number; // Alert at 150/sec (over capacity)
}

const ALERT_THRESHOLDS: AlertThresholds = {
  capacityUtilization: 0.90,
  broadcastLatencyP99: 120000, // 120s
  errorRate: 0.05, // 5%
  connectionRate: 150,
};

function checkAlerts(metrics: SignalHubMetrics): string[] {
  const alerts: string[] = [];

  if (metrics.capacityUtilization > ALERT_THRESHOLDS.capacityUtilization) {
    alerts.push(`Capacity at ${(metrics.capacityUtilization * 100).toFixed(1)}% (threshold: 90%)`);
  }

  if (metrics.broadcastLatencyP99 > ALERT_THRESHOLDS.broadcastLatencyP99) {
    alerts.push(`Broadcast P99 latency ${metrics.broadcastLatencyP99}ms (threshold: 120s)`);
  }

  if (metrics.connectionRatePerSec > ALERT_THRESHOLDS.connectionRate) {
    alerts.push(`Connection rate ${metrics.connectionRatePerSec}/sec (threshold: 150/sec)`);
  }

  return alerts;
}
```

---

## Summary

### Critical Takeaways

1. **Never use synchronous broadcast for >100 actors** - CPU timeout risk
2. **Use queue-based batching for large broadcasts** - Cloudflare Queues with 100-actor batches
3. **Two-tier registry storage** - In-memory volatile + durable persistent
4. **50K actor limit per instance** - Shard beyond this using consistent hashing
5. **Client-side jitter is mandatory** - 0-30s random delay prevents thundering herd
6. **Server-side rate limiting** - 100 connections/sec with token bucket

### Production Checklist

- [ ] Implement queue-based broadcast for >100 actors
- [ ] Set up Cloudflare Queue for BROADCAST_QUEUE
- [ ] Implement two-tier registry (volatile + durable)
- [ ] Add token bucket rate limiter for connections
- [ ] Add client-side jitter (0-30s) on reconnect
- [ ] Implement registry capacity monitoring
- [ ] Set up alerts for 90% capacity utilization
- [ ] Test broadcast with 1000 actors (should complete in <30s)
- [ ] Test thundering herd (10K simultaneous reconnects)
- [ ] Implement consistent hash sharding (if >50K actors expected)

### Performance Targets (Recap)

| Metric | Target | Strategy |
|--------|--------|----------|
| Broadcast throughput | 1000 actors/sec | Queue batching |
| Registry lookup | <10ms | In-memory Map |
| Connection rate | 100/sec sustained | Token bucket limiter |
| Max actors per instance | 50K | Two-tier storage |
| Broadcast latency (1000 actors) | <30s | First batch immediate, rest queued |

---

**Phase 6 complete.** Scalability strategy defined with production-ready implementations for broadcast queuing, registry management, sharding, and thundering herd protection.
