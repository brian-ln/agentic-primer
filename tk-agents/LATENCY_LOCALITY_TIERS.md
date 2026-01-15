# Latency & Locality Tiers for Addressable Actors

**Status**: Research / Exploration Direction
**Created**: 2026-01-15
**Purpose**: Explore tiering strategies for actor placement based on access patterns and latency profiles

---

## Executive Summary

This document explores how actors in tk-agents can be organized across different storage and network tiers, each with distinct latency, availability, and cost profiles. Drawing from memory hierarchies, distributed systems patterns, and content-addressable storage, we propose a tiered actor architecture where placement decisions are driven by access patterns, data size, and mutability.

**Key Insight**: Like CPU cache hierarchies and CDN edge networks, actor placement should be **access-pattern-driven** rather than manually configured. Actors that are frequently accessed should be "hot" (in-memory), while rarely accessed actors can be "cold" (on disk or network). Access logging provides the signals for automatic tier promotion/demotion.

**Connection to User's Insight**: "Memory reconsolidation" in neuroscience - frequently accessed memories are strengthened and kept accessible, while rarely accessed memories fade or are archived. The same principle applies to actor placement.

---

## 1. Tier Hierarchy: From Registers to Network

### The Complete Latency Spectrum

```
CPU Registers    <-- 0.5 ns      -- Variables in hot loop
    ↓
L1 Cache         <-- 1-2 ns      -- Recently used data
    ↓
L2 Cache         <-- 4-10 ns     -- Function-local caches
    ↓
L3 Cache         <-- 20-50 ns    -- Shared cache
    ↓
RAM              <-- 100 ns       -- In-memory actors (current tk-agents)
    ↓
SSD              <-- 100 μs       -- Persisted actors (SQLite)
    ↓
HDD              <-- 10 ms        -- Archive storage
    ↓
Network (LAN)    <-- 1-10 ms      -- Remote actors (same datacenter)
    ↓
Network (WAN)    <-- 50-200 ms    -- Distributed actors (multi-region)
    ↓
Network (Edge)   <-- 100-500 ms   -- Edge/CDN caching
```

### Latency Profiles (Typical Access Times)

| Tier | Latency | Throughput | Capacity | Volatility | Cost/GB |
|------|---------|------------|----------|------------|---------|
| **L1 Cache** | 1-2 ns | 1 TB/s | 32-64 KB | Volatile | N/A |
| **L2 Cache** | 4-10 ns | 500 GB/s | 256 KB - 1 MB | Volatile | N/A |
| **L3 Cache** | 20-50 ns | 100 GB/s | 8-32 MB | Volatile | N/A |
| **RAM** | 100 ns | 20-50 GB/s | 8-128 GB | Volatile | $5-10/GB |
| **SSD (NVMe)** | 100 μs | 3-7 GB/s | 500 GB - 4 TB | Persistent | $0.10-0.30/GB |
| **SSD (SATA)** | 500 μs | 500 MB/s | 1-4 TB | Persistent | $0.05-0.15/GB |
| **HDD** | 10 ms | 100-200 MB/s | 4-20 TB | Persistent | $0.02-0.05/GB |
| **Network (LAN)** | 1-10 ms | 1-10 Gb/s | Unlimited | N/A | $0.01-0.05/GB |
| **Network (WAN)** | 50-200 ms | 100 Mb/s | Unlimited | N/A | $0.05-0.15/GB |

**Sources**: CPU cache latencies from Intel/AMD documentation, storage benchmarks from industry reports (2025), network latencies from AWS/GCP published metrics.

**Note**: These are typical/representative values as of 2025-2026. Actual latencies vary by workload, hardware, and configuration.

---

## 2. Fallacies of Distributed Computing

### The Eight Fallacies (Revised for Actor Placement)

When designing a tiered actor system, we must avoid these classic distributed systems mistakes:

#### 1. The Network is Reliable
**Fallacy**: Actors on remote tiers (network, disk) will always be reachable.
**Reality**: Network partitions, disk failures, process crashes.
**Impact**: Need timeouts, retries, circuit breakers for remote actor access.
**Mitigation**: Cache actor state locally, implement read-through/write-through patterns.

#### 2. Latency is Zero
**Fallacy**: Actors on all tiers have equivalent access time.
**Reality**: 1000x latency difference between RAM and SSD, 1,000,000x to network.
**Impact**: Synchronous `await actor.send()` becomes unacceptable for cold actors.
**Mitigation**: Async mailboxes, prefetching, speculative execution.

#### 3. Bandwidth is Infinite
**Fallacy**: Can transfer large actor state freely between tiers.
**Reality**: Network/disk bandwidth is limited, especially under load.
**Impact**: Large actors (e.g., knowledge nodes with embeddings) become bottlenecks.
**Mitigation**: Compression, delta encoding, reference passing instead of value passing.

#### 4. The Network is Secure
**Fallacy**: Actor messages across network tiers don't need encryption.
**Reality**: Network traffic can be intercepted, actors on remote machines need authentication.
**Impact**: Need secure channels for inter-tier communication.
**Mitigation**: TLS for network actors, signed messages, capability-based security.

#### 5. Topology Doesn't Change
**Fallacy**: Actor placement is static once decided.
**Reality**: Access patterns change, servers go down, load shifts.
**Impact**: Static placement leads to poor performance as usage evolves.
**Mitigation**: Dynamic tier promotion/demotion based on access patterns.

#### 6. There is One Administrator
**Fallacy**: All actors in all tiers are controlled by one system.
**Reality**: Distributed actors may span multiple organizations, clouds, edge locations.
**Impact**: No single source of truth, conflicting configurations.
**Mitigation**: Federation protocols, consistent hashing, eventual consistency.

#### 7. Transport Cost is Zero
**Fallacy**: Moving actors between tiers is free.
**Reality**: Network egress fees, disk I/O costs, CPU for serialization/deserialization.
**Impact**: Frequent tier changes can be more expensive than poor placement.
**Mitigation**: Hysteresis in promotion/demotion, cost-aware placement algorithms.

#### 8. The Network is Homogeneous
**Fallacy**: All network paths (LAN, WAN, edge) have similar characteristics.
**Reality**: Edge connections have high latency, WAN has variable bandwidth.
**Impact**: One-size-fits-all routing leads to poor experience for edge users.
**Mitigation**: Tier-aware routing, edge caching, regional affinity.

### Application to tk-agents

Current tk-agents assumes:
- All actors are in RAM (Tier 1 only)
- Network is reliable (no retries in ClaudeActor or BashActor)
- Latency is uniform (synchronous `await actor.send()`)
- Topology doesn't change (actors registered once, never moved)

**To address these fallacies**, we need:
1. Multi-tier actor storage (in-memory, disk, network)
2. Async mailboxes with timeouts and retries
3. Access pattern tracking for tier placement
4. Dynamic actor migration based on usage

---

## 3. Tiering Strategies

### Hot/Warm/Cold Data Classification

Actors can be classified by access pattern into three tiers:

#### Hot Actors (In-Memory: RAM)
**Characteristics**:
- Accessed frequently (multiple times per second)
- Small to medium size (< 10 MB)
- Low latency required (< 1 ms)
- Examples: Active TaskNodes, ClaudeActors in conversation, Registry

**Storage**: JavaScript `Map`, in-memory Graph
**Access Time**: 100 ns - 1 μs
**Eviction**: LRU when memory pressure, promote to warm tier

#### Warm Actors (Fast Storage: SSD)
**Characteristics**:
- Accessed occasionally (once per minute to hour)
- Medium size (10 MB - 1 GB)
- Moderate latency acceptable (1-100 ms)
- Examples: Completed tasks, knowledge nodes, archived conversations

**Storage**: SQLite, LevelDB, RocksDB
**Access Time**: 100 μs - 10 ms
**Eviction**: Age-based or size-based, move to cold tier

#### Cold Actors (Archive Storage: HDD / S3)
**Characteristics**:
- Accessed rarely (days to never)
- Large size acceptable (> 1 GB)
- High latency acceptable (100 ms - seconds)
- Examples: Historical task archives, old knowledge bases, audit logs

**Storage**: S3, GCS, local HDD
**Access Time**: 10 ms - 1 second
**Eviction**: Never (permanent archive)

### Promotion and Demotion Policies

#### Access-Pattern-Driven Promotion

```typescript
interface AccessPattern {
  actorId: string;
  accessCount: number;
  lastAccessed: Date;
  avgAccessInterval: number;  // ms
  sizeBytes: number;
}

function shouldPromote(pattern: AccessPattern, currentTier: Tier): boolean {
  const recentAccessRate = pattern.accessCount / (Date.now() - pattern.lastAccessed.getTime());

  // Promote to hot if accessed >1/sec and small enough
  if (currentTier === 'warm' && recentAccessRate > 1 && pattern.sizeBytes < 10_000_000) {
    return true;
  }

  // Promote to warm if accessed >1/hour
  if (currentTier === 'cold' && pattern.avgAccessInterval < 3600_000) {
    return true;
  }

  return false;
}
```

#### Time-Based Demotion

```typescript
function shouldDemote(pattern: AccessPattern, currentTier: Tier): boolean {
  const timeSinceLastAccess = Date.now() - pattern.lastAccessed.getTime();

  // Demote to warm if not accessed in 1 hour
  if (currentTier === 'hot' && timeSinceLastAccess > 3600_000) {
    return true;
  }

  // Demote to cold if not accessed in 1 week
  if (currentTier === 'warm' && timeSinceLastAccess > 7 * 86400_000) {
    return true;
  }

  return false;
}
```

#### Size-Aware Placement

Large actors should never occupy hot tier:

```typescript
function getInitialTier(actorSize: number): Tier {
  if (actorSize < 1_000_000) {        // < 1 MB
    return 'hot';
  } else if (actorSize < 100_000_000) { // < 100 MB
    return 'warm';
  } else {
    return 'cold';
  }
}
```

### Prefetching and Predictive Loading

#### Relationship-Based Prefetching

When accessing a TaskNode, prefetch its children and required knowledge:

```typescript
async function prefetchRelated(taskId: string, graph: TieredGraph): Promise<void> {
  const task = await graph.get(taskId);  // May load from disk

  // Prefetch child tasks in parallel
  const childIds = graph.getChildTasks(taskId);
  await Promise.all(childIds.map(id => graph.prefetch(id)));

  // Prefetch required knowledge
  const knowledgeIds = task.requiredKnowledge ?? [];
  await Promise.all(knowledgeIds.map(id => graph.prefetch(id)));
}
```

#### Temporal Prefetching

Predict which actors will be accessed next based on history:

```typescript
interface TemporalPattern {
  actorId: string;
  accessedAfter: string[];  // IDs of actors accessed before this one
  probability: number;
}

// Example: After accessing task-1, there's 80% chance of accessing task-2
const patterns: TemporalPattern[] = [
  { actorId: 'task-2', accessedAfter: ['task-1'], probability: 0.8 },
  { actorId: 'knowledge-1', accessedAfter: ['task-2'], probability: 0.9 },
];

async function prefetchByTemporal(
  accessedId: string,
  patterns: TemporalPattern[],
  graph: TieredGraph
): Promise<void> {
  const likely = patterns.filter(p =>
    p.accessedAfter.includes(accessedId) && p.probability > 0.7
  );

  await Promise.all(likely.map(p => graph.prefetch(p.actorId)));
}
```

### Caching Policies

#### Read-Through Cache

Actor lookup checks hot tier first, then warm, then cold:

```typescript
class TieredGraph {
  private hot: Map<string, Actor> = new Map();
  private warm: SQLiteStore;
  private cold: S3Store;

  async get(actorId: string): Promise<Actor> {
    // Check hot tier
    if (this.hot.has(actorId)) {
      this.recordAccess(actorId, 'hot');
      return this.hot.get(actorId)!;
    }

    // Check warm tier
    const warmActor = await this.warm.get(actorId);
    if (warmActor) {
      this.recordAccess(actorId, 'warm');
      // Promote to hot if accessed frequently
      if (this.shouldPromote(actorId)) {
        this.hot.set(actorId, warmActor);
      }
      return warmActor;
    }

    // Check cold tier
    const coldActor = await this.cold.get(actorId);
    if (coldActor) {
      this.recordAccess(actorId, 'cold');
      // Consider promoting to warm
      return coldActor;
    }

    throw new Error(`Actor not found: ${actorId}`);
  }
}
```

#### Write-Through vs Write-Back

**Write-Through**: Updates propagate to all tiers immediately
- Pro: Consistency, no data loss
- Con: Slower writes, network overhead

**Write-Back**: Updates only to hot tier, flush to warm/cold later
- Pro: Fast writes, batch I/O
- Con: Risk of data loss if crash before flush

**Recommendation**: Write-back for frequently mutating actors (tasks), write-through for immutable actors (completed tasks, knowledge).

---

## 4. Actor Placement Heuristics

### Size-Based Heuristics

```typescript
interface PlacementHeuristic {
  name: string;
  decide: (actor: Actor, context: PlacementContext) => Tier;
}

const sizeBasedHeuristic: PlacementHeuristic = {
  name: 'Size-Based',
  decide: (actor, context) => {
    const size = estimateSize(actor);
    if (size < 1_000_000) return 'hot';       // < 1 MB
    if (size < 100_000_000) return 'warm';    // < 100 MB
    return 'cold';
  }
};
```

### Access-Frequency Heuristics

```typescript
const accessFrequencyHeuristic: PlacementHeuristic = {
  name: 'Access Frequency',
  decide: (actor, context) => {
    const pattern = context.getAccessPattern(actor.id);
    const accessesPerMinute = pattern.accessCount / (
      (Date.now() - pattern.firstAccessed.getTime()) / 60_000
    );

    if (accessesPerMinute > 10) return 'hot';    // >10/min
    if (accessesPerMinute > 0.1) return 'warm';  // >0.1/min (1 per 10 min)
    return 'cold';
  }
};
```

### Mutability Heuristics

```typescript
const mutabilityHeuristic: PlacementHeuristic = {
  name: 'Mutability',
  decide: (actor, context) => {
    const writes = context.getWriteCount(actor.id);
    const reads = context.getReadCount(actor.id);
    const writeRatio = writes / (reads + writes);

    // High write ratio -> keep in hot tier for fast updates
    if (writeRatio > 0.5) return 'hot';

    // Low write ratio -> can be in warm/cold
    if (reads > 100) return 'warm';
    return 'cold';
  }
};
```

### State-Based Heuristics

```typescript
const stateBasedHeuristic: PlacementHeuristic = {
  name: 'State-Based',
  decide: (actor, context) => {
    if (actor.type !== 'task') return 'warm';

    const task = actor as TaskNode;

    // Active tasks need fast access
    if (task.properties.state === 'active') return 'hot';

    // Blocked tasks might be accessed soon
    if (task.properties.state === 'blocked') return 'warm';

    // Completed tasks are archival
    if (task.properties.state === 'completed') return 'cold';

    return 'warm';
  }
};
```

### Composite Heuristic

Combine multiple heuristics with weights:

```typescript
function compositeDecision(
  actor: Actor,
  context: PlacementContext,
  heuristics: Array<{ heuristic: PlacementHeuristic; weight: number }>
): Tier {
  const scores = { hot: 0, warm: 0, cold: 0 };

  for (const { heuristic, weight } of heuristics) {
    const tier = heuristic.decide(actor, context);
    scores[tier] += weight;
  }

  // Return tier with highest score
  return Object.entries(scores).reduce((a, b) => a[1] > b[1] ? a : b)[0] as Tier;
}

// Example usage:
const placement = compositeDecision(actor, context, [
  { heuristic: sizeBasedHeuristic, weight: 2 },
  { heuristic: accessFrequencyHeuristic, weight: 3 },
  { heuristic: stateBasedHeuristic, weight: 1 },
]);
```

---

## 5. Access Logging and Pattern Detection

### Access Event Recording

```typescript
interface AccessEvent {
  actorId: string;
  timestamp: Date;
  operation: 'read' | 'write' | 'delete';
  tier: Tier;
  latencyMs: number;
  sizeBytes: number;
}

class AccessLogger {
  private events: AccessEvent[] = [];
  private maxEvents: number = 10_000;

  log(event: AccessEvent): void {
    this.events.push(event);

    // Ring buffer: drop oldest events
    if (this.events.length > this.maxEvents) {
      this.events.shift();
    }
  }

  // Aggregate access patterns per actor
  getPattern(actorId: string): AccessPattern {
    const actorEvents = this.events.filter(e => e.actorId === actorId);

    if (actorEvents.length === 0) {
      return {
        actorId,
        accessCount: 0,
        lastAccessed: new Date(0),
        avgAccessInterval: Infinity,
        sizeBytes: 0,
      };
    }

    const sortedEvents = actorEvents.sort((a, b) =>
      a.timestamp.getTime() - b.timestamp.getTime()
    );

    const intervals = [];
    for (let i = 1; i < sortedEvents.length; i++) {
      intervals.push(
        sortedEvents[i].timestamp.getTime() - sortedEvents[i - 1].timestamp.getTime()
      );
    }

    return {
      actorId,
      accessCount: actorEvents.length,
      lastAccessed: sortedEvents[sortedEvents.length - 1].timestamp,
      avgAccessInterval: intervals.length > 0
        ? intervals.reduce((a, b) => a + b) / intervals.length
        : Infinity,
      sizeBytes: actorEvents[actorEvents.length - 1].sizeBytes,
    };
  }
}
```

### Periodic Analysis and Rebalancing

```typescript
class TierManager {
  private logger: AccessLogger;
  private graph: TieredGraph;
  private rebalanceIntervalMs: number = 300_000;  // 5 minutes

  startRebalancing(): void {
    setInterval(() => this.rebalance(), this.rebalanceIntervalMs);
  }

  private async rebalance(): Promise<void> {
    const allActorIds = this.graph.getAllActorIds();

    for (const actorId of allActorIds) {
      const currentTier = this.graph.getTier(actorId);
      const pattern = this.logger.getPattern(actorId);

      // Check if promotion needed
      if (shouldPromote(pattern, currentTier)) {
        const targetTier = this.getPromotionTarget(currentTier);
        console.log(`Promoting ${actorId}: ${currentTier} -> ${targetTier}`);
        await this.graph.migrate(actorId, targetTier);
      }

      // Check if demotion needed
      if (shouldDemote(pattern, currentTier)) {
        const targetTier = this.getDemotionTarget(currentTier);
        console.log(`Demoting ${actorId}: ${currentTier} -> ${targetTier}`);
        await this.graph.migrate(actorId, targetTier);
      }
    }
  }

  private getPromotionTarget(current: Tier): Tier {
    if (current === 'cold') return 'warm';
    if (current === 'warm') return 'hot';
    return current;
  }

  private getDemotionTarget(current: Tier): Tier {
    if (current === 'hot') return 'warm';
    if (current === 'warm') return 'cold';
    return current;
  }
}
```

### Temporal Pattern Detection

Detect recurring access patterns (e.g., "task-2 is always accessed after task-1"):

```typescript
class TemporalPatternDetector {
  private logger: AccessLogger;

  detectSequentialPatterns(windowMs: number = 60_000): TemporalPattern[] {
    const events = this.logger.getEvents();
    const patterns: Map<string, Map<string, number>> = new Map();

    // Find pairs of accesses within window
    for (let i = 0; i < events.length - 1; i++) {
      const current = events[i];

      for (let j = i + 1; j < events.length; j++) {
        const next = events[j];

        // If next access is within window
        const timeDiff = next.timestamp.getTime() - current.timestamp.getTime();
        if (timeDiff > windowMs) break;

        // Record (current.actorId -> next.actorId) pattern
        if (!patterns.has(next.actorId)) {
          patterns.set(next.actorId, new Map());
        }
        const predecessors = patterns.get(next.actorId)!;
        predecessors.set(current.actorId, (predecessors.get(current.actorId) ?? 0) + 1);
      }
    }

    // Convert to TemporalPattern with probability
    const result: TemporalPattern[] = [];
    for (const [actorId, predecessors] of patterns) {
      const totalPredecessors = Array.from(predecessors.values()).reduce((a, b) => a + b, 0);

      for (const [predId, count] of predecessors) {
        const probability = count / totalPredecessors;

        if (probability > 0.5) {  // Only strong patterns
          result.push({
            actorId,
            accessedAfter: [predId],
            probability,
          });
        }
      }
    }

    return result;
  }
}
```

---

## 6. Research Directions

### Direction 1: Content-Addressable Actor Storage

**Motivation**: Actors with identical state should be deduplicated across tiers.

**Approach**:
- Content-hash actor state (SHA-256 of serialized state)
- Store actors by hash, not by ID
- Actor ID becomes pointer to content-hash
- Similar to Git's object storage

**Benefits**:
- Deduplication (multiple tasks with same state share storage)
- Immutable snapshots (changing state creates new hash)
- Time-travel (keep old hashes for history)

**Experiment**:
1. Implement content-addressable store (CAS) for actors
2. Measure storage savings for task graph with many similar tasks
3. Compare lookup performance (ID -> hash -> state) vs direct lookup

### Direction 2: Speculative Execution with Prefetching

**Motivation**: Hide disk/network latency by predicting which actors will be accessed.

**Approach**:
- Learn temporal access patterns (Markov chains)
- Speculatively load predicted actors into hot tier
- Evict if prediction was wrong

**Benefits**:
- Perceived latency reduction (next actor already in memory)
- Better throughput (parallel prefetch while processing)

**Risks**:
- Wasted I/O if predictions are wrong
- Memory pressure from speculative loads

**Experiment**:
1. Implement temporal pattern detector (sequential, co-occurrence)
2. Add prefetch mechanism with configurable confidence threshold
3. Benchmark: hit rate, latency reduction, memory overhead

### Direction 3: Actor Compression and Delta Encoding

**Motivation**: Large actors (knowledge nodes with embeddings) are expensive to transfer between tiers.

**Approach**:
- Compress actor state before writing to warm/cold tier (gzip, zstd)
- Delta encoding: store only changes from previous version
- Lazy decompression: keep compressed in memory if not accessed

**Benefits**:
- Reduced storage cost (3-10x compression ratio)
- Reduced network transfer time
- More actors fit in hot tier

**Tradeoffs**:
- CPU cost for compression/decompression
- Complexity of delta encoding

**Experiment**:
1. Benchmark compression ratios for different actor types
2. Measure CPU overhead of compress/decompress
3. Implement delta encoding for KnowledgeNode appends

### Direction 4: Distributed Actor Placement (Multi-Machine)

**Motivation**: Scale beyond single machine by distributing actors across nodes.

**Approach**:
- Consistent hashing for actor placement (hash(actorId) -> node)
- Remote actor proxy: sends messages over network
- Replication: hot actors replicated to multiple nodes

**Benefits**:
- Horizontal scalability
- Fault tolerance (replicas survive node failure)

**Challenges**:
- Network latency (50-200ms for cross-region)
- Consistency (CAP theorem - partition tolerance vs consistency)
- Coordination overhead (distributed locks, leader election)

**Experiment**:
1. Implement RemoteActorProxy with HTTP/gRPC
2. Benchmark single-node vs multi-node performance
3. Test fault tolerance (kill node, verify failover)

### Direction 5: Edge Caching for Distributed Agents

**Motivation**: ClaudeActors spawning at edge locations need low-latency access to knowledge.

**Approach**:
- Edge nodes cache frequently accessed knowledge actors
- Cache invalidation on knowledge updates
- Regional affinity: route users to nearest edge node

**Benefits**:
- Lower latency for edge users (100-500ms -> 10-50ms)
- Reduced load on central database

**Challenges**:
- Cache staleness (edge cache may be outdated)
- Cache invalidation complexity
- Cost (egress fees for cache population)

**Experiment**:
1. Simulate edge topology (3 regions: US, EU, APAC)
2. Implement cache-aside pattern with TTL
3. Measure hit rate, latency reduction, staleness impact

### Direction 6: WASM Actors for CPU Register Tier

**Motivation**: Ultra-hot actors (tight loops, real-time) need CPU-level performance.

**Approach**:
- Compile deterministic actors (BashActor-like scripts) to WASM
- Run in-process with zero serialization overhead
- JIT compilation for hot paths

**Benefits**:
- 10-100x speedup for tight loops
- Zero-copy message passing (shared memory)

**Tradeoffs**:
- Limited to deterministic actors (no I/O, network)
- Compilation overhead
- Security (sandboxing)

**Experiment**:
1. Compile simple actor (counter, state machine) to WASM
2. Benchmark vs JavaScript implementation
3. Test interop with TypeScript actors

---

## 7. Initial Exploration Plan

### Phase 1: Foundation (Weeks 1-2)

**Goal**: Implement basic tiering with hot/warm/cold classification.

**Tasks**:
1. **Access Logging**
   - Add `AccessLogger` class
   - Instrument `Graph.send()` to record access events
   - Implement `getPattern(actorId)` for access frequency analysis

2. **Tier Classification**
   - Add `tier` property to NodeProperties
   - Implement size-based initial placement
   - Create `TieredGraph` wrapper around existing Graph

3. **Simple Warm Tier (SQLite)**
   - Add SQLite persistence for actors
   - Implement read-through cache (hot -> warm)
   - Test actor load/save performance

**Success Criteria**:
- Access logging captures all actor reads/writes
- Actors auto-classify to hot/warm based on size
- SQLite persistence works with <10ms overhead

**Deliverables**:
- `src/tiering/access-logger.ts`
- `src/tiering/tiered-graph.ts`
- `src/tiering/sqlite-store.ts`
- Tests for tier classification and persistence

### Phase 2: Dynamic Placement (Weeks 3-4)

**Goal**: Implement access-pattern-driven tier migration.

**Tasks**:
1. **Promotion/Demotion**
   - Implement `shouldPromote()` and `shouldDemote()` heuristics
   - Add `TierManager` for periodic rebalancing
   - Test promotion on hot access, demotion on cold access

2. **Migration**
   - Implement `Graph.migrate(actorId, targetTier)` method
   - Handle in-flight messages during migration
   - Test migration correctness (no lost updates)

3. **Metrics and Monitoring**
   - Add tier distribution metrics (count per tier)
   - Track promotion/demotion rates
   - Log migration decisions for debugging

**Success Criteria**:
- Frequently accessed actors auto-promote to hot tier
- Idle actors auto-demote to warm tier
- Migration happens without data loss

**Deliverables**:
- `src/tiering/tier-manager.ts`
- `src/tiering/heuristics.ts`
- Tests for dynamic placement

### Phase 3: Prefetching (Weeks 5-6)

**Goal**: Hide latency with predictive loading.

**Tasks**:
1. **Relationship-Based Prefetch**
   - Prefetch child tasks when parent accessed
   - Prefetch required knowledge for active tasks
   - Measure hit rate and latency reduction

2. **Temporal Pattern Detection**
   - Implement `TemporalPatternDetector`
   - Learn sequential access patterns (actor A -> actor B)
   - Speculative prefetch based on detected patterns

3. **Benchmarking**
   - Compare latency with/without prefetching
   - Measure memory overhead of speculative loads
   - Tune prefetch confidence threshold

**Success Criteria**:
- 50%+ reduction in perceived latency for sequential access
- <20% memory overhead from prefetching
- Prefetch hit rate >70%

**Deliverables**:
- `src/tiering/prefetch.ts`
- `src/tiering/temporal-patterns.ts`
- Benchmarks for prefetch effectiveness

### Phase 4: Advanced Tiers (Weeks 7-8)

**Goal**: Add cold tier (S3/archive) and explore compression.

**Tasks**:
1. **Cold Tier (S3)**
   - Implement S3Store for archival actors
   - Test cold tier for completed tasks (>7 days old)
   - Measure cost savings vs warm tier

2. **Compression**
   - Add zstd compression for warm/cold actors
   - Benchmark compression ratios per actor type
   - Measure CPU overhead vs I/O savings

3. **Distributed Actors (Optional)**
   - Prototype RemoteActorProxy with HTTP
   - Test cross-machine actor access
   - Benchmark network latency overhead

**Success Criteria**:
- Cold tier reduces storage cost by 5-10x
- Compression achieves 3-5x size reduction
- Remote actors work with <100ms added latency

**Deliverables**:
- `src/tiering/s3-store.ts`
- `src/tiering/compression.ts`
- `src/tiering/remote-proxy.ts` (optional)

---

## 8. Measurement and Evaluation

### Key Metrics

1. **Latency Percentiles**
   - P50, P90, P99 for actor access (per tier)
   - Target: P99 < 10ms for hot tier, < 100ms for warm tier

2. **Hit Rates**
   - Hot tier hit rate (target: >80%)
   - Prefetch hit rate (target: >70%)

3. **Memory Efficiency**
   - Hot tier size / total actors (target: <10%)
   - Memory overhead from prefetching (target: <20%)

4. **Tier Distribution**
   - Count of actors per tier (hot/warm/cold)
   - Migration rate (promotions + demotions per minute)

5. **Cost Metrics**
   - Storage cost per tier ($/GB)
   - Network egress cost ($/GB)
   - Compute cost for compression/decompression

### Benchmarking Methodology

```typescript
class TierBenchmark {
  async benchmarkAccess(actorId: string, iterations: number): Promise<{
    p50: number;
    p90: number;
    p99: number;
  }> {
    const latencies: number[] = [];

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      await this.graph.get(actorId);
      const latency = performance.now() - start;
      latencies.push(latency);
    }

    latencies.sort((a, b) => a - b);

    return {
      p50: latencies[Math.floor(iterations * 0.5)],
      p90: latencies[Math.floor(iterations * 0.9)],
      p99: latencies[Math.floor(iterations * 0.99)],
    };
  }

  async benchmarkTierDistribution(): Promise<{
    hot: number;
    warm: number;
    cold: number;
  }> {
    const allActorIds = this.graph.getAllActorIds();
    const distribution = { hot: 0, warm: 0, cold: 0 };

    for (const actorId of allActorIds) {
      const tier = this.graph.getTier(actorId);
      distribution[tier]++;
    }

    return distribution;
  }
}
```

### Success Criteria for Each Phase

**Phase 1 (Foundation)**:
- [ ] Access logging captures 100% of actor accesses
- [ ] SQLite warm tier adds <10ms overhead vs in-memory
- [ ] Tests pass for basic tier classification

**Phase 2 (Dynamic Placement)**:
- [ ] Frequently accessed actors (>1/sec) promoted to hot tier within 5 minutes
- [ ] Idle actors (no access in 1 hour) demoted to warm tier within 10 minutes
- [ ] Zero data loss during tier migration

**Phase 3 (Prefetching)**:
- [ ] Relationship-based prefetch reduces P90 latency by 30%+
- [ ] Temporal pattern detection achieves 70%+ hit rate
- [ ] Memory overhead from prefetching <20%

**Phase 4 (Advanced Tiers)**:
- [ ] Cold tier reduces storage cost by 5-10x vs warm tier
- [ ] Compression achieves 3-5x size reduction for knowledge nodes
- [ ] Remote actors (if implemented) work with <200ms latency

---

## 9. Connections to Other Concepts

### Memory Reconsolidation (Neuroscience)

**Parallel**: Frequently accessed memories are "reconsolidated" (strengthened) during sleep, while rarely accessed memories fade.

**Application**: Actor access logging is like memory reconsolidation - track which actors are "important" (frequently accessed) and keep them hot, while rarely accessed actors are demoted.

### CPU Cache Hierarchies (Computer Architecture)

**Parallel**: L1/L2/L3 cache tiers with LRU eviction.

**Application**: Hot/warm/cold actor tiers mirror cache hierarchy. Access patterns drive automatic promotion/demotion, just like cache replacement policies.

### CDN Edge Caching (Distributed Systems)

**Parallel**: Edge servers cache popular content closer to users.

**Application**: Distributed actor placement with edge caching for ClaudeActors spawning in different regions.

### Content-Addressable Storage (Git, IPFS)

**Parallel**: Store objects by content hash, deduplicate identical content.

**Application**: Actors with identical state share storage, enabling efficient snapshots and time-travel.

### Least Recently Used (LRU) Caches

**Parallel**: Evict least recently accessed items when cache is full.

**Application**: Hot tier evicts LRU actors to warm tier when memory pressure increases.

---

## 10. Open Questions

1. **How fine-grained should access logging be?**
   - Per-message? Per-property-access? Per-graph-traversal?
   - Tradeoff: Granularity vs logging overhead

2. **Should tier placement be per-actor or per-property?**
   - E.g., TaskNode.goal (hot) vs TaskNode.artifacts (cold)
   - Tradeoff: Complexity vs memory efficiency

3. **How to handle tier placement for actors with references?**
   - If task-1 (hot) references knowledge-1 (cold), promote knowledge-1?
   - Or keep reference and pay latency cost on access?

4. **What consistency model for distributed tiers?**
   - Strong consistency (slow, safe) vs eventual consistency (fast, risky)
   - Actor model allows eventual consistency by design

5. **How to prevent thrashing (rapid promotion/demotion)?**
   - Hysteresis: require sustained access pattern before migration
   - Cooldown period: minimum time in tier before migration

6. **Should compression be transparent or explicit?**
   - Transparent: Graph automatically compresses/decompresses
   - Explicit: Caller chooses compressed vs uncompressed

7. **How to handle actors that don't fit in any tier?**
   - E.g., 10GB knowledge node (too large for hot, accessed frequently)
   - Solution: Stream/chunk large actors, keep index in hot tier

---

## 11. Next Steps

1. **Implement Phase 1 (Foundation)** - Access logging, basic tiering, SQLite persistence
2. **Benchmark current system** - Establish baseline latency/memory without tiering
3. **Prototype promotion/demotion** - Test access-pattern-driven migration
4. **Explore prefetching** - Measure latency reduction from predictive loading
5. **Document findings** - Update this document with experimental results

---

## References

### Academic Papers
- "The Five-Minute Rule for Trading Memory for Disk Accesses" (Gray & Graefe, 1987)
- "Fallacies of Distributed Computing Explained" (Rotem-Gal-Oz, 2006)
- "CAP Theorem: Revisited" (Brewer, 2012)

### Industry Resources
- AWS S3 Storage Classes: https://aws.amazon.com/s3/storage-classes/
- Redis Persistence: https://redis.io/docs/management/persistence/
- SQLite Performance Tuning: https://www.sqlite.org/speed.html

### Memory Hierarchies
- "Computer Architecture: A Quantitative Approach" (Hennessy & Patterson)
- CPU Cache Latency Numbers: https://gist.github.com/jboner/2841832

### Content-Addressable Storage
- Git Internals: https://git-scm.com/book/en/v2/Git-Internals-Git-Objects
- IPFS (InterPlanetary File System): https://ipfs.tech/

### Related tk-agents Documents
- [ERLANG_ARCHITECTURE_ANALYSIS.md](ERLANG_ARCHITECTURE_ANALYSIS.md) - Supervision, monitoring, mailboxes
- [DESIGN.md](DESIGN.md) - Core actor model and message passing
- [PROJECT_CONTEXT.md](PROJECT_CONTEXT.md) - Project goals and philosophy

---

**Status**: Ready for experimentation. Start with Phase 1 (Foundation) to validate core tiering concepts before advancing to dynamic placement and prefetching.
