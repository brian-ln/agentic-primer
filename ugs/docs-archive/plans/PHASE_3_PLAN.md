# Phase 3: Message-Passing & Reactivity

## Overview

Phase 3 transforms the query layer from a graph manipulation system into a true **actor orchestration platform** by adding:
- Bidirectional message-passing (ask/reply, streaming)
- Reactive queries (live subscriptions)
- Event-driven workflows (triggers)
- Relationship upsert semantics

## Context

**What we have (Phase 1 + 2):**
- ✅ Pattern matching and graph queries
- ✅ CRUD operations for nodes
- ✅ CREATE/DELETE operations for relationships
- ✅ Query optimization (join reordering, predicate pushdown, index hints)
- ✅ EXPLAIN plans with visualization
- ✅ Fire-and-forget messaging (`send().tell()`)

**What we need (Phase 3):**
- ❌ Request-response messaging (`send().ask()`)
- ❌ Streaming messaging (`send().stream()`)
- ❌ Reactive queries (`subscribe()`)
- ❌ Event triggers (`on()`)
- ❌ UPDATE relationships (upsert semantics)

## Design Decisions

### 1. Relationship Update Semantics

**Decision: UPSERT**

```typescript
// API: upsertRelationship()
upsertRelationship('task', 'user', {
  type: 'assignedTo',
  properties: { priority: 'high', assignedAt: Date.now() }
})

// Behavior:
// - If relationship exists: Update properties
// - If relationship missing: Create with properties
// - Always idempotent (safe for retries)
```

**Rationale:**
- Graph databases favor MERGE semantics (Neo4j, etc.)
- Actor systems benefit from idempotent operations
- "Ensure this connection has these properties" is clearer than "maybe create, maybe update"
- Keep `createRelationship()` for strict "must not exist" enforcement

### 2. Message-Passing API

**Three messaging patterns:**

```typescript
// 1. Fire-and-forget (existing)
send('task').tell('start')

// 2. Request-response (NEW)
send('task').ask('getStatus')  // Returns Promise<Response>

// 3. Streaming (NEW)
send('task').stream('logs')    // Returns AsyncIterable<LogEntry>
```

**Integration with QueryBuilder:**

```typescript
query()
  .match(pattern('task').where({ id: 'task-1' }))
  .forEach(send('task').ask('getStatus'))
  .return(['task', 'response'])  // Responses bound as variables
```

### 3. Reactive Queries

**Live subscriptions to pattern matches:**

```typescript
const subscription = query()
  .match(pattern('task').where({ status: 'failed' }))
  .subscribe({
    onMatch: (tasks) => console.log('Failed:', tasks),
    onUnmatch: (tasks) => console.log('Recovered:', tasks),
    onError: (error) => console.error(error)
  });

// Cleanup
subscription.unsubscribe();
```

**Implementation approach:**
- Use actor ports (existing infrastructure)
- Subscribe to relevant actors' state changes
- Re-evaluate patterns when notified
- Emit events on match/unmatch

### 4. Event Triggers

**Declarative event-driven workflows:**

```typescript
query()
  .on('task.lifecycle.completed')  // Listen for events
  .where(
    pattern('test')
      .label('Task')
      .where({ type: 'test', result: { passed: true } })
  )
  .forEach(send('deploy').tell('start'));
```

**Event sources:**
- Actor lifecycle events (created, updated, deleted)
- Relationship changes (connected, disconnected)
- Custom domain events (task.completed, build.failed)

## Work Streams

### R1: Relationship UPSERT Operation

**Goal:** Add idempotent relationship update

**Files to create/modify:**
- `src/query/actions.ts` - Add `upsertRelationship()` builder
- `src/query/mutations/upsert-relationship.test.ts` - 40+ tests
- `src/query/compiler.ts` - Handle `upsert_relationship` action type
- `src/messaging/actors/query-executor.ts` - Execute upsert (check exists, create or update)
- `src/messaging/actors/relationship.ts` - Add `upsert` message handler

**API:**
```typescript
upsertRelationship(from: string, to: string, spec: {
  type: string;
  properties?: Record<string, any>;
})
```

**Tests:**
- Upsert on non-existent relationship (creates)
- Upsert on existing relationship (updates)
- Property merging behavior
- Type immutability (cannot change relationship type)
- Parallel upserts (last-write-wins or merge?)
- Error handling

**Estimated:** 200 lines + 40 tests

---

### M1: Request-Response Messaging (ask)

**Goal:** Bidirectional message-passing with replies

**Files to create/modify:**
- `src/query/builder.ts` - Add `ask()` to ActionBuilder
- `src/query/actions/ask.test.ts` - 35+ tests
- `src/query/compiler.ts` - Compile `ask` actions
- `src/messaging/actors/query-executor.ts` - Handle ask/reply pattern
- `src/query/types.ts` - Add `AskSpec` and response binding types

**API:**
```typescript
query()
  .match(pattern('task').where({ id: 'task-1' }))
  .forEach(send('task').ask('getStatus'))
  .return(['task', 'response'])

// With timeout
query()
  .match(pattern('task'))
  .forEach(send('task').ask('compute', { timeout: 5000 }))
```

**Implementation:**
- Use existing actor `ask()` infrastructure
- Bind responses to query variables
- Support timeout configuration
- Handle errors (timeout, actor unavailable, reject)

**Tests:**
- Ask single actor
- Ask multiple actors (parallel)
- Response binding to variables
- Timeout handling
- Error propagation
- Return with responses

**Estimated:** 250 lines + 35 tests

---

### M2: Streaming Messaging (stream)

**Goal:** Continuous message streams from actors

**Files to create/modify:**
- `src/query/builder.ts` - Add `stream()` to ActionBuilder
- `src/query/actions/stream.test.ts` - 30+ tests
- `src/query/compiler.ts` - Compile `stream` actions
- `src/messaging/actors/query-executor.ts` - Handle async iterables
- `src/query/types.ts` - Add `StreamSpec` types

**API:**
```typescript
const results = query()
  .match(pattern('task').where({ type: 'build' }))
  .forEach(send('task').stream('logs'))
  .stream();  // Returns AsyncIterable

for await (const log of results) {
  console.log(log);
}
```

**Implementation:**
- Use actor `stream()` infrastructure
- Multiplex multiple streams
- Backpressure handling
- Cleanup on cancel

**Tests:**
- Stream from single actor
- Multiplex multiple actor streams
- Stream cancellation
- Backpressure handling
- Error in stream
- Completion handling

**Estimated:** 280 lines + 30 tests

---

### S1: Reactive Queries (subscribe)

**Goal:** Live queries that update on graph changes

**Files to create/modify:**
- `src/query/builder.ts` - Add `subscribe()` method
- `src/query/reactive/subscriber.ts` - Subscription manager
- `src/query/reactive/reactive.test.ts` - 45+ tests
- `src/messaging/actors/query-executor.ts` - Setup subscriptions
- `src/query/types.ts` - Add `Subscription` and callback types

**API:**
```typescript
const sub = query()
  .match(pattern('task').where({ status: 'failed' }))
  .subscribe({
    onMatch: (results) => console.log('Matched:', results),
    onUnmatch: (results) => console.log('Unmatched:', results),
    onError: (error) => console.error(error)
  });

// Cleanup
sub.unsubscribe();
```

**Implementation:**
- Subscribe to relevant actor ports
- Re-evaluate pattern on state changes
- Track previous results for diff (onMatch vs onUnmatch)
- Efficient incremental updates
- Memory leak prevention

**Tests:**
- Initial match notification
- New matches appear
- Existing matches disappear
- Multiple subscribers
- Unsubscribe cleanup
- Error handling
- Memory leak verification

**Estimated:** 400 lines + 45 tests

---

### S2: Event Triggers (on)

**Goal:** Declarative event-driven workflows

**Files to create/modify:**
- `src/query/builder.ts` - Add `on()` method
- `src/query/reactive/trigger.ts` - Event trigger system
- `src/query/reactive/trigger.test.ts` - 40+ tests
- `src/messaging/actors/query-executor.ts` - Register event handlers
- `src/query/types.ts` - Add `TriggerSpec` types

**API:**
```typescript
query()
  .on('task.lifecycle.completed')
  .where(
    pattern('task')
      .where({ type: 'test', result: { passed: true } })
  )
  .forEach(send('deploy').tell('start'));

// Multiple event types
query()
  .on(['task.created', 'task.updated'])
  .where(pattern('task').where({ priority: 'critical' }))
  .forEach(send('alerts').tell('notify'));
```

**Implementation:**
- Event bus integration (existing actor events)
- Filter events by pattern
- Execute actions on match
- Debouncing/throttling options
- Error handling and retry

**Tests:**
- Single event type
- Multiple event types
- Pattern filtering
- Action execution
- Debouncing
- Error handling
- Cleanup on destroy

**Estimated:** 350 lines + 40 tests

---

### I1: Integration & Examples

**Goal:** End-to-end examples and documentation

**Files to create/modify:**
- `src/query/examples-phase3.ts` - 10+ examples
- `src/query/live-demo-reactive-messaging.ts` - Interactive demo
- `docs/PHASE_3_GUIDE.md` - User guide
- `README.md` - Update with Phase 3 features

**Examples:**
1. Request-response pattern (ask task for status)
2. Streaming logs from build tasks
3. Live query: Failed tasks dashboard
4. Event trigger: Test pass → auto-deploy
5. Complex workflow: Multi-stage reactive pipeline
6. Relationship upsert in action
7. Combined patterns: ask + subscribe + on
8. Error handling across all patterns
9. Performance monitoring (subscribe to metrics)
10. Real-time collaboration (subscribe to shared state)

**Live Demo:**
- Demo all 5 new features
- Show reactive updates in real-time
- Performance comparison (reactive vs polling)
- Error scenarios and recovery

**Documentation:**
- API reference for all new methods
- Migration guide from Phase 2
- Best practices for reactive queries
- Performance considerations
- Memory management guide

**Estimated:** 600 lines + docs

---

### T1: Comprehensive Testing

**Goal:** Integration tests and benchmarks

**Files to create:**
- `src/query/integration/phase3.test.ts` - 25+ integration tests
- `src/query/benchmarks/reactive.bench.ts` - Performance benchmarks

**Integration tests:**
- Ask → process response → update graph
- Stream → filter → aggregate → action
- Subscribe → trigger → cascade updates
- Event trigger → conditional workflow
- Complex: ask + stream + subscribe together
- Error propagation across patterns
- Cleanup and resource management
- Concurrent subscriptions (10+ simultaneous)

**Benchmarks:**
- Subscribe latency (time to first notification)
- Event trigger throughput (events/sec)
- Ask vs tell performance overhead
- Stream backpressure handling
- Memory usage (1000+ subscriptions)
- Reactive query re-evaluation cost

**Estimated:** 400 lines

---

## Dependencies

```
R1: Relationship UPSERT
  ↓
M1: Ask + M2: Stream (parallel)
  ↓
S1: Subscribe + S2: Triggers (parallel)
  ↓
I1: Integration & Examples
  ↓
T1: Comprehensive Testing
```

**Critical path:**
1. R1 (foundation for remaining work)
2. M1 + M2 (can run parallel)
3. S1 + S2 (can run parallel, depend on M1/M2)
4. I1 (depends on all features)
5. T1 (final validation)

## Success Metrics

**Functionality:**
- ✅ All 5 new features implemented
- ✅ 185+ tests passing (40+35+30+45+40 feature tests + 25 integration)
- ✅ Live demo runs successfully
- ✅ Zero memory leaks in subscriptions

**Performance:**
- ✅ Ask overhead <10% vs direct actor call
- ✅ Subscribe latency <50ms (time to first notification)
- ✅ Event triggers handle >1000 events/sec
- ✅ Stream backpressure prevents memory bloat

**API Quality:**
- ✅ Type-safe (full TypeScript inference)
- ✅ Consistent with Phase 1/2 patterns
- ✅ Clear error messages
- ✅ Comprehensive documentation

## Testing Strategy

**Unit tests (per work stream):**
- R1: 40 tests
- M1: 35 tests
- M2: 30 tests
- S1: 45 tests
- S2: 40 tests

**Integration tests (T1):**
- 25 end-to-end scenarios
- Error handling
- Resource cleanup
- Concurrent usage

**Benchmarks:**
- Latency measurements
- Throughput tests
- Memory profiling
- Stress tests

**Total: 215+ new tests**

## Implementation Timeline

**Parallel execution (9 background agents):**

**Wave 1 (Foundation):**
- Agent R1: Relationship upsert (2-3 hours)

**Wave 2 (Messaging - parallel):**
- Agent M1: Ask messaging (2-3 hours)
- Agent M2: Stream messaging (2-3 hours)

**Wave 3 (Reactivity - parallel):**
- Agent S1: Subscribe (3-4 hours)
- Agent S2: Event triggers (3-4 hours)

**Wave 4 (Integration):**
- Agent I1: Examples & docs (2-3 hours)

**Wave 5 (Validation):**
- Agent T1: Integration tests (2 hours)

**Total estimated: 6-8 hours (wall time ~4-5 hours with parallelism)**

## Files Summary

**New files (15):**
- `src/query/mutations/upsert-relationship.test.ts`
- `src/query/actions/ask.test.ts`
- `src/query/actions/stream.test.ts`
- `src/query/reactive/subscriber.ts`
- `src/query/reactive/subscriber.test.ts`
- `src/query/reactive/trigger.ts`
- `src/query/reactive/trigger.test.ts`
- `src/query/examples-phase3.ts`
- `src/query/live-demo-reactive-messaging.ts`
- `src/query/integration/phase3.test.ts`
- `src/query/benchmarks/reactive.bench.ts`
- `docs/PHASE_3_GUIDE.md`

**Modified files (7):**
- `src/query/builder.ts` (add ask, stream, subscribe, on)
- `src/query/actions.ts` (add upsertRelationship)
- `src/query/compiler.ts` (handle new action types)
- `src/query/types.ts` (add new specs and types)
- `src/messaging/actors/query-executor.ts` (execute new patterns)
- `src/messaging/actors/relationship.ts` (upsert handler)
- `README.md` (document Phase 3)

## Verification

**Before commit:**
1. ✅ All 711+ tests pass (496 existing + 215 new)
2. ✅ Live demo runs successfully
3. ✅ Benchmarks show acceptable performance
4. ✅ No memory leaks (subscription cleanup verified)
5. ✅ Documentation complete
6. ✅ Examples run without errors

**Post-commit:**
- Create Phase 3 demo video/GIF
- Update main README with Phase 3 capabilities
- Tag release: `v0.3.0-phase3`

## Next Steps After Phase 3

**Potential Phase 4:**
- Distributed query execution (multi-node)
- Query result caching (semantic deduplication)
- Advanced optimizations (materialized views, incremental computation)
- Visual query builder UI
- Query debugging tools (step-through execution)

---

**Total estimate:** 2,430+ new lines, 215+ tests, 6-8 hours of agent work
