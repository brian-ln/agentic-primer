# Pressure Test Scenarios for tk-agents Graph Coordination

## Executive Summary

This document identifies 12 pressure test scenarios designed to reveal coordination problems in tk-agents' actor-based task/knowledge graph system. The current implementation uses synchronous message passing (`await actor.send()`), which masks potential concurrency issues that would emerge in a truly asynchronous system.

**Key Insight**: The synchronous design prevents concurrent operations, which means we're not stress-testing the coordination mechanisms at all. These scenarios identify what *would* break if we introduce asynchronous mailboxes, concurrent actors, or distributed nodes.

---

## Scenario Classification

Scenarios are organized by failure mode:
- **Race Conditions**: Multiple actors reading/writing shared state
- **Deadlock**: Circular dependencies causing permanent blocking
- **Ordering**: Message sequence violations
- **Resource Exhaustion**: Queue overflow, memory pressure
- **Conflict Resolution**: Concurrent modifications to same entity

---

## Priority Matrix

| Priority | Likelihood | Severity | Scenario |
|----------|------------|----------|----------|
| **P0** | High | Critical | #1 Concurrent Task State Mutations |
| **P0** | High | Critical | #2 Knowledge Node Write Conflicts |
| **P0** | Medium | Critical | #3 Circular Task Dependencies |
| **P1** | High | High | #4 Mailbox Overflow |
| **P1** | Medium | High | #5 Parent-Child Task Races |
| **P1** | Medium | High | #6 Graph Edge Race Conditions |
| **P2** | Low | High | #7 Distributed Split Brain |
| **P2** | Medium | Medium | #8 Message Ordering Violations |
| **P2** | Medium | Medium | #9 Supervisor Restart Cascades |
| **P3** | Low | Medium | #10 Clock Skew in Progress Tracking |
| **P3** | Low | Medium | #11 Memory Leak from Stale Monitors |
| **P3** | Low | Low | #12 Priority Inversion Starvation |

---

## Detailed Scenarios

### P0: Critical Priority

---

#### Scenario 1: Concurrent Task State Mutations

**What It Tests**: Race conditions when multiple actors attempt to change task state simultaneously.

**Setup**:
```typescript
// Create task in 'active' state
const task = createTask({
  goal: "Deploy API",
  desiredDeliverables: ["Endpoints"],
  objectiveSuccessCriteria: [
    { criterion: "Tests pass", measure: "pass_rate", threshold: 1.0 }
  ]
}, graph);

graph.send(task.properties.id, "start", {});

// Concurrent actors attempt state transitions
const actor1 = async () => {
  await graph.send(task.properties.id, "complete", { result: "Done" });
};

const actor2 = async () => {
  await graph.send(task.properties.id, "block", { reason: "Missing deps" });
};

const actor3 = async () => {
  await graph.send(task.properties.id, "eval", {});
};

// Fire simultaneously
await Promise.all([actor1(), actor2(), actor3()]);
```

**Expected Behavior**:
- Only one state transition succeeds
- Others fail gracefully with clear errors
- Task ends in a valid terminal state (completed or blocked)
- No partial state updates

**What Can Go Wrong**:
- **Lost Update**: Actor1's `complete` overwrites Actor2's `block`
- **Invalid State**: Task ends in impossible state like `completed+blocked`
- **Partial Update**: `completedAt` is set but state is still `active`
- **Inconsistent Graph**: Task node says "completed" but edges say "blocked"

**Success Criteria**:
- [ ] Final state is valid according to state machine
- [ ] Exactly one transition succeeds
- [ ] Failed transitions return descriptive errors
- [ ] No orphaned properties (e.g., `blockReason` when not blocked)
- [ ] Graph edges match final state

**Why It Matters**: Tasks are the core entity. State corruption cascades to children, supervisors, and coordination logic.

---

#### Scenario 2: Knowledge Node Write Conflicts

**What It Tests**: Concurrent `append` operations to same KnowledgeNode, testing version conflicts and content ordering.

**Setup**:
```typescript
const kb = createKnowledge({
  title: "API Requirements",
  content: "Initial requirements...",
}, graph);

// 10 concurrent actors appending data
const actors = Array.from({ length: 10 }, (_, i) => async () => {
  await graph.send(kb.properties.id, "append", {
    data: `Requirement ${i}: ...`,
    source: `Actor-${i}`,
  });
});

await Promise.all(actors.map(a => a()));

// Verify result
const result = graph.send(kb.properties.id, "get", {});
```

**Expected Behavior**:
- All 10 appends are reflected in final content
- Content is coherent (no interleaved text)
- Version number increments correctly (initial=1, final=11)
- All 10 sources are tracked

**What Can Go Wrong**:
- **Lost Write**: Some appends don't appear in final content
- **Interleaved Text**: "Req1: fooReq2: barReq1: continued"
- **Version Conflict**: Version=6 instead of 11 (lost increments)
- **Duplicate Sources**: Same source recorded multiple times
- **Corrupted Content**: Partial sentences, missing newlines

**Success Criteria**:
- [ ] Content contains all 10 requirements in some order
- [ ] No interleaved text (each append is atomic)
- [ ] Version = 11
- [ ] Sources array has 10 unique entries
- [ ] Content separators are preserved (newlines)

**Why It Matters**: Knowledge nodes are shared across tasks. Corruption affects all consumers.

---

#### Scenario 3: Circular Task Dependencies (Deadlock)

**What It Tests**: Deadlock detection when tasks have circular `depends_on` edges.

**Setup**:
```typescript
const task1 = createTask({ goal: "Task 1", ... }, graph);
const task2 = createTask({ goal: "Task 2", ... }, graph);
const task3 = createTask({ goal: "Task 3", ... }, graph);

graph.send(task1.properties.id, "start", {});
graph.send(task2.properties.id, "start", {});
graph.send(task3.properties.id, "start", {});

// Create circular dependency: 1 -> 2 -> 3 -> 1
graph.send(task1.properties.id, "link", {
  toId: task2.properties.id,
  edgeType: "depends_on",
});
graph.send(task2.properties.id, "link", {
  toId: task3.properties.id,
  edgeType: "depends_on",
});
graph.send(task3.properties.id, "link", {
  toId: task1.properties.id,
  edgeType: "depends_on",
});

// Attempt to complete any task
await graph.send(task1.properties.id, "complete", { result: "Done" });
```

**Expected Behavior**:
- System detects circular dependency
- `complete` fails with clear error: "Circular dependency detected: task1 -> task2 -> task3 -> task1"
- Tasks remain in `active` state
- Alternative: Automatically transition all to `blocked` with reason "Deadlock"

**What Can Go Wrong**:
- **Silent Deadlock**: No error, tasks hang forever
- **Cascade Failure**: All tasks transition to `failed`
- **Partial Detection**: Detects cycle but doesn't identify all members
- **Livelock**: System repeatedly tries to resolve, consuming CPU

**Success Criteria**:
- [ ] Cycle is detected within 100ms
- [ ] Error message includes full cycle path
- [ ] No infinite loops or hangs
- [ ] Tasks are marked `blocked` with reason "Deadlock"
- [ ] Provides suggested resolution (remove one edge)

**Why It Matters**: Circular dependencies are a fundamental concurrency failure. Must be detected proactively.

---

### P1: High Priority

---

#### Scenario 4: Mailbox Overflow Under Load

**What It Tests**: Backpressure mechanisms when actor mailbox fills up.

**Setup**:
```typescript
// Create slow actor (100ms per message)
class SlowActor extends MailboxActor {
  protected async handleMessage(message: Message): Promise<Response> {
    await new Promise(resolve => setTimeout(resolve, 100));
    return { success: true, data: "Processed" };
  }
}

const slowActor = new SlowActor("slow-1", "deterministic", 100); // 100 msg max

// Flood with 1000 messages rapidly
const messages = Array.from({ length: 1000 }, (_, i) => ({
  id: `msg-${i}`,
  type: "work",
  payload: { index: i },
}));

// Try to send all at once
for (const msg of messages) {
  await slowActor.send(msg);
}
```

**Expected Behavior**:
- First 100 messages queue successfully
- Message 101+ fail with "Mailbox full" error
- Sender gets backpressure signal
- No messages are lost or corrupted
- Actor continues processing queued messages

**What Can Go Wrong**:
- **Silent Drop**: Messages 101-1000 silently discarded
- **Out of Memory**: Unlimited queue causes OOM crash
- **Starvation**: Actor never catches up, queue grows forever
- **Priority Inversion**: Important messages stuck behind 100 low-priority ones

**Success Criteria**:
- [ ] Mailbox enforces size limit (100 messages)
- [ ] Sender receives explicit backpressure error
- [ ] No messages lost from first 100
- [ ] Actor continues processing after backpressure
- [ ] Queue drains at expected rate (10 msg/sec)

**Why It Matters**: In async systems, fast senders can overwhelm slow receivers. Must have explicit flow control.

---

#### Scenario 5: Parent-Child Task Completion Race

**What It Tests**: Race condition when parent and child tasks complete simultaneously.

**Setup**:
```typescript
const parent = createTask({ goal: "Parent", ... }, graph);
graph.send(parent.properties.id, "start", {});

const childResponse = await graph.send(parent.properties.id, "spawn", {
  goal: "Child",
  deliverables: ["Artifact"],
  criteria: [{ criterion: "Done", measure: "complete", threshold: true }],
});
const childId = childResponse.childTaskId;

graph.send(childId, "start", {});

// Both attempt to complete concurrently
const completeParent = async () => {
  await graph.send(parent.properties.id, "complete", { result: "Parent done" });
};

const completeChild = async () => {
  await graph.send(childId, "complete", { result: "Child done" });
};

await Promise.all([completeParent(), completeChild()]);
```

**Expected Behavior**:
- Child completes successfully
- Parent completion either:
  - **Option A**: Succeeds because child is now completed
  - **Option B**: Fails initially, then succeeds on retry
- Final state: Both completed
- `eval` on parent sees child as completed

**What Can Go Wrong**:
- **Race Window**: Parent checks child (incomplete), child completes, parent fails permanently
- **Inconsistent View**: Parent's `childrenStatus` doesn't reflect child's actual state
- **Orphaned Child**: Parent completes, child state is ignored
- **Double Evaluation**: Both run eval simultaneously, get different results

**Success Criteria**:
- [ ] Both tasks end in `completed` state
- [ ] Parent's `eval` reflects child's completion
- [ ] No transient errors that require manual retry
- [ ] Progress calculation is consistent
- [ ] Edges reflect actual completion order

**Why It Matters**: Task hierarchies are central to decomposition. Race conditions break progress tracking.

---

#### Scenario 6: Graph Edge Race Conditions

**What It Tests**: Concurrent edge creation/deletion causing inconsistent graph structure.

**Setup**:
```typescript
const task = createTask({ goal: "Task", ... }, graph);
const kb1 = createKnowledge({ title: "KB1", ... }, graph);
const kb2 = createKnowledge({ title: "KB2", ... }, graph);

// Multiple actors manipulate edges concurrently
const link1 = async () => {
  await graph.send(task.properties.id, "link", {
    toId: kb1.properties.id,
    edgeType: "requires_knowledge",
  });
};

const unlink1 = async () => {
  const edges = graph.getEdgesFrom(task.properties.id);
  const edge = edges.find(e => e.toId === kb1.properties.id);
  if (edge) {
    await graph.send(task.properties.id, "unlink", { edgeId: edge.id });
  }
};

const link2 = async () => {
  await graph.send(task.properties.id, "link", {
    toId: kb2.properties.id,
    edgeType: "requires_knowledge",
  });
};

// Fire concurrently
await Promise.all([link1(), unlink1(), link2()]);
```

**Expected Behavior**:
- Edge operations are atomic
- Final graph state is consistent
- Either KB1 is linked or not (no half-state)
- KB2 is linked (independent operation)
- Edge IDs are unique

**What Can Go Wrong**:
- **Dangling Edge**: Edge exists but referenced node is deleted
- **Duplicate Edges**: Two edges between same nodes
- **Lost Edge**: KB2 link is silently dropped
- **Inconsistent Index**: `getEdgesFrom()` returns stale data
- **Edge ID Collision**: Two edges get same ID

**Success Criteria**:
- [ ] No duplicate edges between same nodes
- [ ] All edges have valid fromId/toId references
- [ ] KB2 edge exists in final state
- [ ] KB1 edge state is deterministic (linked XOR not linked)
- [ ] `getEdgesFrom()` matches actual edge table

**Why It Matters**: Graph integrity is foundational. Corrupted edges break queries and traversals.

---

### P2: Medium Priority

---

#### Scenario 7: Distributed Split Brain (Future-Proofing)

**What It Tests**: Network partition causing two registries to have conflicting views.

**Setup** (hypothetical multi-node):
```typescript
// Node A and Node B initially connected
const nodeA = new Registry();
const nodeB = new Registry();

const task = createTask({ goal: "Shared Task", ... }, nodeA.graph);
nodeB.graph.registerNode(task); // Replicate to B

// Network partition occurs
nodeA.disconnect(nodeB);

// Both nodes modify task independently
await nodeA.graph.send(task.properties.id, "update", {
  properties: { state: "completed" },
});

await nodeB.graph.send(task.properties.id, "update", {
  properties: { state: "blocked", blockReason: "Missing deps" },
});

// Network heals
nodeA.reconnect(nodeB);
```

**Expected Behavior**:
- System detects conflicting state
- Applies resolution strategy:
  - **Last Write Wins**: Use timestamp to pick winner
  - **Version Vectors**: Detect true conflict, escalate
  - **Merge**: Combine non-conflicting updates
- Final state is consistent across nodes

**What Can Go Wrong**:
- **Silent Overwrite**: NodeA's state overwrites NodeB's
- **Divergent State**: Nodes never reconcile, remain inconsistent
- **Lost Updates**: Both updates are discarded
- **Cascade**: Conflict propagates to dependent tasks

**Success Criteria**:
- [ ] Conflict is detected within 1 second of reconnection
- [ ] Resolution strategy is applied consistently
- [ ] No silent data loss
- [ ] Dependent tasks see consistent view
- [ ] Logs include conflict details for audit

**Why It Matters**: Future distributed deployment requires partition tolerance. Test now to inform design.

---

#### Scenario 8: Message Ordering Violations

**What It Tests**: Whether messages from same sender arrive in order.

**Setup**:
```typescript
const kb = createKnowledge({ title: "Counter", content: "0" }, graph);

// Send sequence of increments
await graph.send(kb.properties.id, "append", { data: "1" });
await graph.send(kb.properties.id, "append", { data: "2" });
await graph.send(kb.properties.id, "append", { data: "3" });
await graph.send(kb.properties.id, "append", { data: "4" });
await graph.send(kb.properties.id, "append", { data: "5" });

const result = graph.send(kb.properties.id, "get", {});
const content = result.properties.content;
```

**Expected Behavior**:
- Content is "0\n\n1\n\n2\n\n3\n\n4\n\n5"
- Appends appear in send order

**What Can Go Wrong**:
- **Out of Order**: Content is "0\n\n3\n\n1\n\n5\n\n2\n\n4"
- **Missing**: Some appends don't appear
- **Duplicates**: "0\n\n1\n\n1\n\n2\n\n3\n\n4\n\n5"

**Success Criteria**:
- [ ] Content matches expected sequence
- [ ] No missing or duplicate entries
- [ ] Order is preserved under load (test with 1000 messages)

**Why It Matters**: Erlang guarantees message ordering from same sender. If we violate this, many assumptions break.

---

#### Scenario 9: Supervisor Restart Cascades

**What It Tests**: Whether supervisor restarts propagate correctly through hierarchy.

**Setup**:
```typescript
// Supervision tree: SupervisorA -> [SupervisorB, TaskC]
//                   SupervisorB -> [TaskD, TaskE]
const supA = new SupervisorActor({
  id: "sup-a",
  strategy: "one_for_all",
  maxRestarts: 3,
  windowMs: 60000,
  children: [
    { id: "sup-b", factory: () => createSupervisorB(), restart: "permanent" },
    { id: "task-c", factory: () => createTaskC(), restart: "permanent" },
  ],
});

const supB = new SupervisorActor({
  id: "sup-b",
  strategy: "one_for_one",
  children: [
    { id: "task-d", factory: () => createTaskD(), restart: "permanent" },
    { id: "task-e", factory: () => createTaskE(), restart: "transient" },
  ],
});

// Cause TaskD to crash
await simulateCrash("task-d");
```

**Expected Behavior**:
- SupervisorB detects TaskD crash
- Restarts only TaskD (one_for_one strategy)
- TaskE continues running
- SupervisorA is not notified (child handled it)
- TaskC continues running

**What Can Go Wrong**:
- **Cascade**: SupervisorA restarts everything (TaskC, SupervisorB, TaskD, TaskE)
- **Restart Storm**: Repeated crashes cause infinite restart loop
- **Lost Child**: TaskD is not restarted, removed from registry
- **Zombie**: TaskD appears running but is actually dead

**Success Criteria**:
- [ ] Only TaskD is restarted
- [ ] TaskE, TaskC continue uninterrupted
- [ ] Restart completes within 500ms
- [ ] Registry reflects new TaskD instance
- [ ] Old TaskD is cleaned up (no memory leak)

**Why It Matters**: Fault isolation is key to supervision. Cascading restarts defeat the purpose.

---

### P3: Lower Priority

---

#### Scenario 10: Clock Skew in Progress Tracking

**What It Tests**: Progress calculation when actor clocks are skewed.

**Setup**:
```typescript
// Simulate clock skew by manipulating timestamps
const parent = createTask({ goal: "Parent", ... }, graph);
parent.properties.startedAt = new Date("2025-01-01T10:00:00Z");

const child = createTask({ goal: "Child", ... }, graph);
child.properties.startedAt = new Date("2025-01-01T09:00:00Z"); // 1 hour earlier
child.properties.completedAt = new Date("2025-01-01T09:30:00Z");

graph.send(parent.properties.id, "link", {
  toId: child.properties.id,
  edgeType: "spawned_by",
});

const status = graph.send(parent.properties.id, "query_status", {});
```

**Expected Behavior**:
- Progress calculation handles skewed timestamps gracefully
- Child appears completed (regardless of clock skew)
- No negative durations or NaN values

**What Can Go Wrong**:
- **Negative Duration**: Child duration = -30 minutes
- **Progress > 100%**: Child shows 150% complete
- **NaN Propagation**: Progress calculation returns NaN, breaks UI

**Success Criteria**:
- [ ] Progress is in range [0, 1]
- [ ] No negative durations
- [ ] No NaN or Infinity values
- [ ] Graceful handling of future timestamps

**Why It Matters**: Distributed systems have clock skew. Must be robust to timestamp inconsistencies.

---

#### Scenario 11: Memory Leak from Stale Monitors

**What It Tests**: Whether monitor cleanup happens correctly.

**Setup**:
```typescript
const registry = new MonitoringRegistry();

// Create and monitor 1000 actors
for (let i = 0; i < 1000; i++) {
  const actor = new MockActor(`actor-${i}`, "deterministic");
  registry.register(actor);

  const monitor = new MockActor(`monitor-${i}`, "deterministic");
  registry.register(monitor);

  registry.monitorActor(monitor.id, actor.id);
}

// Kill all monitored actors
for (let i = 0; i < 1000; i++) {
  const actor = registry.get(`actor-${i}`);
  await actor.stop();
}

// Check memory
const monitorRefs = registry.getMonitorCount();
```

**Expected Behavior**:
- All 1000 monitor references are cleaned up
- `monitorRefs === 0`
- Memory usage returns to baseline

**What Can Go Wrong**:
- **Memory Leak**: Monitor map retains 1000 references
- **Double Cleanup**: Attempting to clean twice causes crash
- **Zombie Monitors**: Monitors receive "down" event but aren't removed

**Success Criteria**:
- [ ] `monitorRefs === 0` after cleanup
- [ ] Memory usage within 10% of baseline
- [ ] No dangling references in reverseIndex map
- [ ] Repeated monitor/demonitor cycles don't leak

**Why It Matters**: Long-running systems must not leak memory. Monitors are a common source of leaks.

---

#### Scenario 12: Priority Inversion Starvation

**What It Tests**: Whether low-priority messages can starve high-priority ones.

**Setup**:
```typescript
const actor = new PriorityMailboxActor("worker", "deterministic");
await actor.start();

// Flood with 1000 low-priority messages
for (let i = 0; i < 1000; i++) {
  actor.mailbox.post(
    { id: `low-${i}`, type: "background", payload: {} },
    priority: 0
  );
}

// After 500ms, send urgent message
await new Promise(resolve => setTimeout(resolve, 500));
const urgentTime = Date.now();

actor.mailbox.post(
  { id: "urgent-1", type: "shutdown", payload: {} },
  priority: 10
);

// Measure time to handle urgent message
const urgentHandled = await waitForMessage("urgent-1");
const latency = urgentHandled - urgentTime;
```

**Expected Behavior**:
- Urgent message jumps to front of queue
- Handled within 100ms despite 1000 pending messages
- Low-priority messages resume after urgent completes

**What Can Go Wrong**:
- **Starvation**: Urgent message waits for all 1000 low-priority messages
- **Reordering**: Low-priority messages after urgent are handled first
- **Dropped**: Urgent message is lost due to queue full

**Success Criteria**:
- [ ] Urgent message latency < 100ms
- [ ] Low-priority messages are not dropped
- [ ] Priority order is maintained (10 before 0)
- [ ] No indefinite starvation of any priority level

**Why It Matters**: Systems need to handle critical messages quickly even under load.

---

## Testing Infrastructure Requirements

To implement these scenarios effectively:

### 1. Concurrency Primitives
```typescript
// Execute functions truly concurrently (not sequential await)
async function concurrent<T>(...fns: (() => Promise<T>)[]): Promise<T[]> {
  return Promise.all(fns.map(fn => fn()));
}

// Introduce random delays to expose race conditions
function withJitter<T>(fn: () => Promise<T>, maxMs: number = 10): Promise<T> {
  return new Promise(resolve => {
    setTimeout(async () => resolve(await fn()), Math.random() * maxMs);
  });
}
```

### 2. Chaos Engineering Tools
```typescript
// Randomly drop messages
function chaosMailbox(mailbox: Mailbox, dropRate: number = 0.1): Mailbox {
  const original = mailbox.post.bind(mailbox);
  mailbox.post = (msg, priority) => {
    if (Math.random() < dropRate) {
      throw new Error("Chaos: Message dropped");
    }
    return original(msg, priority);
  };
  return mailbox;
}

// Simulate network delay
async function withNetworkDelay<T>(fn: () => Promise<T>): Promise<T> {
  await new Promise(resolve => setTimeout(resolve, 50 + Math.random() * 100));
  return fn();
}
```

### 3. Verification Helpers
```typescript
// Assert no race condition occurred
function assertAtomicUpdate<T>(
  before: T,
  after: T,
  allowed: T[]
): void {
  if (!allowed.includes(after)) {
    throw new Error(`Invalid transition: ${before} -> ${after}`);
  }
}

// Detect deadlock with timeout
async function withDeadlockDetection<T>(
  fn: () => Promise<T>,
  timeoutMs: number = 5000
): Promise<T> {
  const timeout = new Promise<never>((_, reject) => {
    setTimeout(() => reject(new Error("Deadlock detected")), timeoutMs);
  });

  return Promise.race([fn(), timeout]);
}
```

### 4. Load Generation
```typescript
// Generate sustained load
async function sustainedLoad(
  actor: Actor,
  rps: number,
  durationMs: number
): Promise<void> {
  const interval = 1000 / rps;
  const endTime = Date.now() + durationMs;

  while (Date.now() < endTime) {
    actor.send({ type: "load", payload: {} });
    await new Promise(resolve => setTimeout(resolve, interval));
  }
}
```

---

## Implementation Roadmap

### Phase 1: Foundation (Week 1)
- [ ] Implement testing infrastructure (concurrency primitives, chaos tools)
- [ ] Set up scenario test suite structure
- [ ] Add instrumentation for race detection

### Phase 2: Critical Scenarios (Week 2)
- [ ] Scenario 1: Concurrent Task State Mutations
- [ ] Scenario 2: Knowledge Node Write Conflicts
- [ ] Scenario 3: Circular Task Dependencies

### Phase 3: High-Priority Scenarios (Week 3)
- [ ] Scenario 4: Mailbox Overflow
- [ ] Scenario 5: Parent-Child Task Completion Race
- [ ] Scenario 6: Graph Edge Race Conditions

### Phase 4: Medium-Priority Scenarios (Week 4)
- [ ] Scenario 7: Distributed Split Brain
- [ ] Scenario 8: Message Ordering Violations
- [ ] Scenario 9: Supervisor Restart Cascades

### Phase 5: Lower-Priority Scenarios (Week 5)
- [ ] Scenario 10: Clock Skew in Progress Tracking
- [ ] Scenario 11: Memory Leak from Stale Monitors
- [ ] Scenario 12: Priority Inversion Starvation

---

## Success Metrics

Track these metrics across all scenarios:

| Metric | Target | Critical Threshold |
|--------|--------|-------------------|
| Race Condition Detections | 0 | 0 (fail test) |
| Deadlocks | 0 | 0 (fail test) |
| Lost Updates | 0 | 0 (fail test) |
| Message Ordering Violations | 0 | 1% (warn) |
| Average Message Latency | < 10ms | < 100ms |
| 99th Percentile Latency | < 50ms | < 500ms |
| Memory Growth Rate | < 1MB/hour | < 10MB/hour |
| Restart Success Rate | > 99% | > 95% |

---

## References

This document draws on research and best practices from:

- [Actor-based Concurrency Patterns](https://berb.github.io/diploma-thesis/original/054_actors.html)
- [A Study of Concurrency Bugs in Actor Programs](https://link.springer.com/chapter/10.1007/978-3-030-00302-9_6)
- [Profiling Actor Utilization in Akka](https://dl.acm.org/doi/10.1145/2975969.2975972)
- [Understanding Actor Concurrency in Erlang](https://www.infoworld.com/article/2178134/understanding-actor-concurrency-part-1-actors-in-erlang.html)
- [Neo4j Concurrent Data Access](https://neo4j.com/docs/operations-manual/current/database-internals/concurrent-data-access/)
- [Deadlock Detection in Distributed Systems](https://www.cs.uic.edu/~ajayk/Chapter10.pdf)
- [Communication Deadlock Analysis for Actor Model](https://www.scirp.org/html/1-9302682_95870.htm)
- [Software Stress Testing: AI Trends 2026](https://blog.qasource.com/everything-you-need-to-know-about-stress-testing-your-software)

---

## Appendix: Known Limitations

### Current Architecture Constraints

1. **Synchronous Message Passing**: Current `await actor.send()` design prevents true concurrency testing. Must implement async mailboxes first.

2. **Single Process**: All actors share Node.js event loop. True isolation requires WebWorkers or multi-process architecture.

3. **No Distributed Coordination**: Registry is in-memory only. Scenario 7 (Split Brain) requires distributed registry implementation.

4. **Limited Supervision**: No SupervisorActor exists yet. Scenario 9 requires supervision hierarchy implementation.

### Testing Gaps

1. **Fault Injection**: Need ability to inject crashes, network delays, message drops at runtime.

2. **Observability**: Limited metrics on mailbox size, message latency, actor restarts.

3. **Reproducibility**: Race conditions are non-deterministic. Need deterministic scheduler for replay.

4. **Load Testing**: No current infrastructure for sustained high-throughput testing.

---

## Next Steps

1. **Review**: Share with team, prioritize scenarios based on roadmap
2. **Infrastructure**: Build concurrency testing primitives (Phase 1)
3. **Baseline**: Run scenarios against current synchronous implementation (expect passes)
4. **Refactor**: Introduce async mailboxes incrementally
5. **Validate**: Re-run scenarios, fix exposed issues
6. **Iterate**: Add new scenarios as system evolves

---

*Document Version: 1.0*
*Last Updated: 2026-01-15*
*Author: Research Analysis (tk-agents Project)*
