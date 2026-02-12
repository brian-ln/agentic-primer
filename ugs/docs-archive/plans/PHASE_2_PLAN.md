# Phase 2: Actions & Mutations - Task Breakdown

**Status:** Planning
**Version:** 1.0
**Date:** 2026-02-05

## Overview

Phase 2 extends the query layer with write operations (CREATE, UPDATE, DELETE for both nodes and relationships), transactions, and advanced optimization. This document identifies parallelizable work items.

## Dependencies

Phase 2 requires Phase 1 complete:
- ✅ Pattern matching (PatternBuilder)
- ✅ Query building (QueryBuilder)
- ✅ Query compilation (QueryCompiler)
- ✅ Query caching (QueryCache)
- ✅ Query execution (QueryExecutor)

## Task Categories

### Category A: Write Operations (Independent)
### Category B: Transactions (Depends on A)
### Category C: Optimization (Independent of A/B)
### Category D: Streaming (Independent of A/B)

---

## Category A: Write Operations

**Estimated effort:** 3-4 days
**Parallelization:** 5 independent streams

### Stream A1: CREATE Node Operations

**Files:**
- `src/query/mutations/create.ts` - CREATE node implementation
- `src/query/mutations/create.test.ts` - Unit tests

**Tasks:**
1. Implement `CreateMutation` class for nodes
2. Add node creation (CREATE (n:Label {...}))
3. Add MERGE operation (CREATE or MATCH nodes)
4. Integrate with QueryCompiler (create node steps)
5. Add validation (required properties, type checking)
6. Route to collection actors (@tasks, @knowledge, etc.)

**API:**
```typescript
query()
  .create('task').as({ title: 'New Task', status: 'open' })

query()
  .merge('user').as({ id: 'alice', name: 'Alice' })  // CREATE or MATCH
```

**Dependencies:** None (independent stream)

---

### Stream A2: UPDATE Operations

**Files:**
- `src/query/mutations/update.ts` - UPDATE implementation
- `src/query/mutations/update.test.ts` - Unit tests

**Tasks:**
1. Implement `UpdateMutation` class
2. Add property updates (SET n.prop = value)
3. Add property deletion (REMOVE n.prop)
4. Add label addition/removal (SET n:Label, REMOVE n:Label)
5. Integrate with QueryCompiler (update steps)
6. Add optimistic locking (version checks)

**API:**
```typescript
query()
  .match(pattern('task').where({ id: 'task-123' }))
  .update('task').set({ status: 'in_progress' })

query()
  .match(pattern('task').where({ status: 'open' }))
  .update('task').set({ priority: 'high' })  // Bulk update
```

**Dependencies:** None (independent stream)

---

### Stream A3: DELETE Operations

**Files:**
- `src/query/mutations/delete.ts` - DELETE implementation
- `src/query/mutations/delete.test.ts` - Unit tests

**Tasks:**
1. Implement `DeleteMutation` class
2. Add node deletion (DELETE n)
3. Add relationship deletion (DELETE r)
4. Add DETACH DELETE (delete node + relationships)
5. Integrate with QueryCompiler (delete steps)
6. Add cascade policies (prevent orphans)

**API:**
```typescript
query()
  .match(pattern('task').where({ id: 'task-123' }))
  .delete('task')

query()
  .match(pattern('task').where({ status: 'cancelled' }))
  .detachDelete('task')  // Delete tasks + relationships
```

**Dependencies:** None (independent stream)

---

### Stream A4: CREATE Relationship Operations

**Files:**
- `src/query/mutations/create-relationship.ts` - CREATE relationship implementation
- `src/query/mutations/create-relationship.test.ts` - Unit tests

**Tasks:**
1. Implement `CreateRelationshipMutation` class
2. Add relationship creation between matched nodes
3. Support relationship properties
4. Integrate with QueryCompiler (create relationship steps)
5. Route to RelationshipActor (@relationships)
6. Add validation (both nodes exist, type checking)

**API:**
```typescript
query()
  .match(
    pattern('task').label('Task').where({ id: 'task-1' }),
    pattern('blocker').label('Task').where({ id: 'task-2' })
  )
  .createRelationship('task', 'blocker', {
    type: 'requires',
    properties: { priority: 'high' }
  })

// Shorthand in query builder
query()
  .match(pattern('task'), pattern('user'))
  .link('task', 'user', 'assignedTo')
```

**Dependencies:** None (independent stream)

---

### Stream A5: DELETE Relationship Operations

**Files:**
- `src/query/mutations/delete-relationship.ts` - DELETE relationship implementation
- `src/query/mutations/delete-relationship.test.ts` - Unit tests

**Tasks:**
1. Implement `DeleteRelationshipMutation` class
2. Add relationship deletion by pattern
3. Support selective deletion (by type, properties)
4. Integrate with QueryCompiler (delete relationship steps)
5. Route to RelationshipActor (@relationships)
6. Add cascade options (what happens to orphaned nodes)

**API:**
```typescript
query()
  .match(
    pattern('task').relatedTo('old-dep', {
      type: 'requires',
      direction: 'outbound'
    })
  )
  .deleteRelationship('task', 'old-dep', { type: 'requires' })

// Delete all relationships of a type
query()
  .match(pattern('task').where({ id: 'task-1' }))
  .deleteAllRelationships('task', { type: 'assignedTo' })
```

**Dependencies:** None (independent stream)

---

**Parallel Strategy A:**
- Assign Stream A1 to Developer 1 (CREATE nodes)
- Assign Stream A2 to Developer 2 (UPDATE nodes)
- Assign Stream A3 to Developer 3 (DELETE nodes)
- Assign Stream A4 to Developer 4 (CREATE relationships)
- Assign Stream A5 to Developer 5 (DELETE relationships)
- Merge after 3-4 days
- Integration tests after merge

---

## Category B: Transactions

**Estimated effort:** 2-3 days
**Parallelization:** Limited (sequential work on transaction manager)
**Dependencies:** Category A complete

### Stream B1: Transaction Manager

**Files:**
- `src/query/transactions/manager.ts` - Transaction coordination
- `src/query/transactions/isolation.ts` - Isolation levels
- `src/query/transactions/lock.ts` - Locking mechanisms
- `src/query/transactions/transaction.test.ts` - Unit tests

**Tasks:**
1. Implement `TransactionManager` class
2. Add transaction begin/commit/rollback
3. Add isolation levels (READ_COMMITTED, SERIALIZABLE)
4. Add distributed transaction support (2PC)
5. Add deadlock detection
6. Add rollback compensation (undo operations)

**API:**
```typescript
const tx = await txManager.begin();

await query()
  .match(pattern('task').where({ id: 'task-123' }))
  .update('task').set({ status: 'in_progress' })
  .execute(tx);

await query()
  .create('log').as({ message: 'Task started' })
  .execute(tx);

await tx.commit();  // Or tx.rollback()
```

**Dependencies:** Streams A1, A2, A3

---

### Stream B2: Multi-Query Transactions

**Files:**
- `src/query/transactions/batch.ts` - Batch execution
- `src/query/transactions/savepoint.ts` - Savepoints
- `src/query/transactions/batch.test.ts` - Unit tests

**Tasks:**
1. Implement batch query execution
2. Add savepoint support (partial rollback)
3. Add query dependencies within transaction
4. Add conflict resolution strategies

**Dependencies:** Stream B1

---

**Parallel Strategy B:**
- Sequential execution (B1 → B2)
- Cannot parallelize transaction manager work
- Focus on thorough testing

---

## Category C: Optimization

**Estimated effort:** 3-4 days
**Parallelization:** 4 independent streams
**Dependencies:** None (independent of write operations)

### Stream C1: Join Optimization

**Files:**
- `src/query/optimizer/join.ts` - Join reordering
- `src/query/optimizer/join.test.ts` - Unit tests

**Tasks:**
1. Implement join order optimization
2. Add cardinality estimation
3. Add cost-based join selection (nested loop, hash, merge)
4. Add multi-pattern join optimization

**Dependencies:** None

---

### Stream C2: Predicate Pushdown

**Files:**
- `src/query/optimizer/pushdown.ts` - Predicate pushdown
- `src/query/optimizer/pushdown.test.ts` - Unit tests

**Tasks:**
1. Implement predicate pushdown optimizer
2. Push filters into pattern matching
3. Push filters into traversals
4. Add selectivity estimation

**Dependencies:** None

---

### Stream C3: Index Hints

**Files:**
- `src/query/optimizer/index-hints.ts` - Index selection
- `src/query/optimizer/index-hints.test.ts` - Unit tests

**Tasks:**
1. Implement index hint support
2. Add index selection heuristics
3. Add USING INDEX directive
4. Add index statistics tracking

**API:**
```typescript
query()
  .match(pattern('task').where({ id: 'task-123' }))
  .useIndex('task_id_idx')
```

**Dependencies:** None

---

### Stream C4: Execution Plan Visualization

**Files:**
- `src/query/explain/planner.ts` - EXPLAIN implementation
- `src/query/explain/visualizer.ts` - Plan visualization
- `src/query/explain/planner.test.ts` - Unit tests

**Tasks:**
1. Implement EXPLAIN query mode
2. Add query plan JSON export
3. Add ASCII plan visualization
4. Add cost annotations
5. Add cardinality estimates

**API:**
```typescript
const explained = await query()
  .match(pattern('task'))
  .explain();

console.log(explained.plan);
// Output:
// Query (cost: 150ms)
//   ├─ Match (cost: 100ms, rows: 1000)
//   │  └─ IndexScan task_status_idx
//   └─ Filter (cost: 50ms, rows: 100)
```

**Dependencies:** None

---

**Parallel Strategy C:**
- Assign Stream C1 to Developer 1
- Assign Stream C2 to Developer 2
- Assign Stream C3 to Developer 3
- Assign Stream C4 to Developer 4
- Merge after 3-4 days

---

## Category D: Streaming Results

**Estimated effort:** 2-3 days
**Parallelization:** Limited (sequential work on streaming infrastructure)
**Dependencies:** None

### Stream D1: AsyncIterator Results

**Files:**
- `src/query/streaming/iterator.ts` - AsyncIterator implementation
- `src/query/streaming/backpressure.ts` - Backpressure handling
- `src/query/streaming/iterator.test.ts` - Unit tests

**Tasks:**
1. Implement AsyncIterator-based results
2. Add backpressure support
3. Add streaming aggregation
4. Add chunked result batching

**API:**
```typescript
const results = query()
  .match(pattern('tasks').label('Task'))
  .stream();

for await (const task of results) {
  console.log(task);
  // Process task without loading all into memory
}
```

**Dependencies:** None

---

### Stream D2: Result Pagination

**Files:**
- `src/query/streaming/pagination.ts` - Pagination support
- `src/query/streaming/cursor.ts` - Cursor-based pagination
- `src/query/streaming/pagination.test.ts` - Unit tests

**Tasks:**
1. Implement LIMIT/OFFSET pagination
2. Add cursor-based pagination
3. Add keyset pagination (stable sorting)
4. Add page size optimization

**API:**
```typescript
query()
  .match(pattern('tasks').label('Task'))
  .orderBy('createdAt', 'desc')
  .limit(100)
  .offset(200)

// Cursor-based
query()
  .match(pattern('tasks').label('Task'))
  .after(cursor)
  .limit(100)
```

**Dependencies:** Stream D1

---

**Parallel Strategy D:**
- Sequential execution (D1 → D2)
- D2 depends on D1 infrastructure

---

## Parallelization Summary

### Maximum Parallelization

**Phase 2A: Weeks 1-2 (Write Operations + Optimization)**

```
Developer 1: Stream A1 (CREATE) + Stream C1 (Join Opt)
Developer 2: Stream A2 (UPDATE) + Stream C2 (Pushdown)
Developer 3: Stream A3 (DELETE) + Stream C3 (Index Hints)
Developer 4: Stream C4 (EXPLAIN) + Stream D1 (Streaming)
```

**Phase 2B: Week 3 (Transactions + Streaming)**

```
Developer 1: Stream B1 (Transaction Manager)
Developer 2: Stream B2 (Batch Transactions)
Developer 3: Stream D2 (Pagination) + Integration tests
Developer 4: Documentation + E2E tests
```

### Serial Dependencies

```
B1 (Transaction Manager) → B2 (Batch Transactions)
D1 (Streaming) → D2 (Pagination)
A1/A2/A3 → B1 (Write ops before transactions)
```

### True Parallelization Opportunities

**Highly Parallel (4 streams):**
- Category A: Write Operations (3 streams)
- Category C: Optimization (4 streams)

**Sequential:**
- Category B: Transactions (2 sequential streams)
- Category D: Streaming (2 sequential streams)

**Optimal Team Size:** 3-4 developers
**Optimal Duration:** 3-4 weeks

---

## Risk Assessment

### High Risk

1. **Transaction Manager** - Complex distributed coordination
   - Mitigation: Start with local transactions, add distributed later
   - Fallback: Use optimistic locking instead of pessimistic

2. **Deadlock Detection** - Complex graph algorithms
   - Mitigation: Use timeout-based detection initially
   - Fallback: Manual deadlock resolution

### Medium Risk

1. **Join Optimization** - Cardinality estimation accuracy
   - Mitigation: Use heuristics initially, refine with profiling
   - Fallback: Use rule-based join ordering

2. **Streaming Backpressure** - Memory management complexity
   - Mitigation: Fixed buffer sizes initially
   - Fallback: Block until consumer ready

### Low Risk

- CRUD operations (well-defined semantics)
- Index hints (optional optimization)
- EXPLAIN plans (read-only introspection)
- Pagination (standard patterns)

---

## Testing Strategy

### Unit Tests

- Each stream has dedicated unit tests
- Coverage target: >90%
- Test isolation (no cross-stream dependencies)

### Integration Tests

- After each category complete
- Test write + read round-trips
- Test transaction rollback
- Test optimization effectiveness

### Performance Tests

- Benchmark query execution time
- Measure cache hit rates
- Test streaming memory usage
- Validate optimization improvements

### E2E Tests

- Full workflow scenarios
- Multi-query transactions
- Concurrent query execution
- Error handling and recovery

---

## Success Metrics

### Phase 2A (Write Operations + Optimization)

- ✅ CREATE/UPDATE/DELETE operations work
- ✅ >90% test coverage
- ✅ Query optimization reduces latency by 30%+
- ✅ EXPLAIN plans provide actionable insights

### Phase 2B (Transactions + Streaming)

- ✅ ACID transactions work correctly
- ✅ Streaming handles 100K+ results
- ✅ Backpressure prevents OOM
- ✅ Deadlock detection prevents hangs

### Overall Phase 2

- ✅ All 4 categories complete
- ✅ Comprehensive documentation
- ✅ Integration with WorkflowOrchestrator
- ✅ Production-ready query layer

---

## Deliverables

### Code

- `src/query/mutations/` - Write operations
- `src/query/transactions/` - Transaction support
- `src/query/optimizer/` - Query optimization
- `src/query/streaming/` - Result streaming
- `src/query/explain/` - Query introspection

### Tests

- Unit tests for all modules
- Integration tests for categories
- Performance benchmarks
- E2E workflow tests

### Documentation

- API reference updates
- Usage guides for new features
- Performance tuning guide
- Transaction isolation guide

---

## Timeline

### Aggressive (3 weeks, 4 developers)

- Week 1: Category A (Write) + Category C (Optimization)
- Week 2: Category B (Transactions) + Category D (Streaming)
- Week 3: Integration + Documentation + Testing

### Conservative (4 weeks, 4-5 developers)

- Week 1: Category A1 + A2 + A4 (CREATE nodes, UPDATE, CREATE relationships)
- Week 2: Category A3 + A5 + C1 + C2 (DELETE nodes/rels, Join, Pushdown)
- Week 3: Category C3 + C4 + D1 (Index, Explain, Streaming)
- Week 4: Category B + D2 (Transactions, Pagination) + Integration

### Realistic (4 weeks, 3-4 developers)

Mix of aggressive and conservative:
- Prioritize Category A (Write) and Category C (Optimization)
- De-prioritize Category B (Transactions) to Phase 3 if needed
- Category D (Streaming) optional for Phase 2

---

## Phase 3 Preview

If Phase 2 runs long, defer to Phase 3:

- **Advanced Transactions** - Distributed 2PC, saga patterns
- **Query Federation** - Cross-actor queries
- **Temporal Queries** - Time-travel queries (AS OF timestamp)
- **Full-Text Search** - Integrated search indexing
- **Graph Algorithms** - Shortest path, centrality, community detection

---

## Recommendations

### For Maximum Parallelization

1. **Start with Category A + Category C** (fully parallel)
2. **Defer Category B** (sequential) to Phase 3 if time-constrained
3. **Make Category D optional** for Phase 2 (nice-to-have)

### For Quality

1. **Focus on Category A** (CRUD is essential)
2. **Add Category C** (optimization is high-value)
3. **Thoroughly test** before moving to Category B

### For Time-to-Market

1. **Ship Category A** (write operations) first
2. **Iterate on Category C** (optimization) based on profiling
3. **Add Category B/D** in follow-up releases

---

## Conclusion

Phase 2 has **genuine parallelization opportunities**:

- **Category A:** 5 independent write operation streams
  - A1: CREATE nodes
  - A2: UPDATE nodes
  - A3: DELETE nodes
  - A4: CREATE relationships (graph edges)
  - A5: DELETE relationships (graph edges)
- **Category C:** 4 independent optimization streams
  - C1: Join optimization
  - C2: Predicate pushdown
  - C3: Index hints
  - C4: EXPLAIN plans

**Total: 9 truly parallel streams** (not artificial splitting)

Optimal team: **4-5 developers working 3-4 weeks**

Sequential work (transactions, streaming) can be deferred to Phase 3 if needed.

**Recommendation:** Prioritize A + C for Phase 2, defer B + D to Phase 3.

**Key insight:** Graph systems require both **node operations** (CREATE/UPDATE/DELETE entities) and **edge operations** (CREATE/DELETE relationships). All 5 Category A streams are independent and can run in parallel.
