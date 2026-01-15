# Event Sourcing Exploration for tk-agents

## Executive Summary

This document explores event sourcing for tk-agents from an **exploration and learning perspective**, not production deployment. The critical analysis correctly identified that "time travel" and "event sourcing" are complexity bombs requiring significant infrastructure. This exploration reframes the question: **What's the minimal viable event sourcing that teaches us something valuable?**

**Key insight**: Event sourcing isn't binary. There's a spectrum from "append-only log" to "full CQRS/ES with snapshots and projections." We can explore the core concepts with minimal infrastructure.

**Recommendation**: Build a **learning prototype** focused on deterministic reconstruction, not production features. Estimated effort: 1-2 days. Expected learning value: High.

---

## 1. Reality Check: What Event Sourcing Actually Requires

### 1.1 Core Components (Minimum)

| Component | Purpose | Complexity | Current tk-agents Status |
|-----------|---------|------------|-------------------------|
| Event Store | Append-only log of events | Low | ❌ None - in-memory graph only |
| Event Schema | Versioned event definitions | Medium | ❌ None - messages are ad-hoc |
| Replay Engine | Reconstruct state from events | Low | ❌ None |
| Event Bus | Dispatch events to handlers | Low | ✅ Partial - `graph.send()` similar |

### 1.2 Production Features (Not Required for Exploration)

| Feature | Purpose | Why It's Complex | Skip for Learning? |
|---------|---------|------------------|-------------------|
| Snapshots | Avoid replaying thousands of events | Need snapshot strategy, versioning | ✅ Yes - keep replay fast |
| Projections | Materialized views for queries | Separate read models, consistency | ✅ Yes - query source directly |
| Schema Evolution | Handle event format changes | Upcasting, migration scripts | ✅ Yes - single schema version |
| CQRS | Separate read/write models | Coordination, eventual consistency | ✅ Yes - simple read = replay |
| Distributed Log | Scale event store horizontally | Partitioning, ordering guarantees | ✅ Yes - SQLite file |
| Compensation Events | Undo/rollback semantics | Event correlation, saga patterns | ⚠️ Maybe - interesting to explore |

**Critical Analysis Validation**: The analysis correctly identified that mentioning "time travel" without these concerns is fantasy planning. We acknowledge this and propose a learning-focused approach instead.

---

## 2. The Compression Insight: Deterministic Reconstruction

### 2.1 User's Key Observation

> "deterministic reconstruction from inputs + process"

This is the **heart of event sourcing** and connects to fundamental computer science:

```
Current State = f(Initial State, Event₁, Event₂, ..., Eventₙ)
```

This is analogous to:
- **Git**: `current_tree = replay(initial_commit, commit₁, ..., commitₙ)`
- **Redux**: `state = reducer(state₀, action₁, ..., actionₙ)`
- **Functional programming**: `fold/reduce` over immutable data
- **Database transactions**: Transaction log replay for recovery

### 2.2 What Makes Reconstruction "Deterministic"?

| Requirement | Explanation | tk-agents Challenge |
|-------------|-------------|---------------------|
| **Pure functions** | Same inputs → same outputs | ❌ ClaudeActor is non-deterministic (LLM calls) |
| **No side effects** | State changes only via events | ❌ BashActor runs commands with external side effects |
| **Total ordering** | Events have unambiguous sequence | ⚠️ Possible - add sequence numbers |
| **Immutable events** | Events never modified after creation | ✅ Easy - append-only log |
| **Idempotency** | Replaying events produces same result | ❌ LLM calls are not idempotent |

**Critical insight**: tk-agents has a **fundamental tension** between:
- **Deterministic actors** (BashActor, TaskNode state machine) - can be replayed
- **Non-deterministic actors** (ClaudeActor with LLM calls) - cannot be replayed

### 2.3 The Git Analogy

Git is event sourcing for files:

```bash
# Event log (commits)
commit abc123: "Add feature X"
commit def456: "Fix bug Y"
commit ghi789: "Refactor Z"

# Reconstruction (checkout)
git checkout ghi789  # Replay commits up to ghi789
```

**What git provides**:
- Time travel: `git checkout <commit>` or `git reset --hard <commit>`
- Branching: `git branch` creates alternate timelines
- Inspection: `git log`, `git diff`, `git blame` for history queries
- Compression: Git packs objects and uses deltas

**What git doesn't do**:
- Replay non-deterministic processes (build outputs, test runs)
- Guarantee reproducible builds without hermetic environments

**Lesson for tk-agents**: Event sourcing works well for **data changes** (like git tracks files), but not for **non-deterministic processes** (like Claude LLM responses).

---

## 3. Exploration Approach: Simplest Possible Event Sourcing

### 3.1 Learning Goals

1. **Understand event replay**: Does reconstructing state from events actually work?
2. **Identify determinism boundaries**: Which actors can be replayed? Which cannot?
3. **Explore time travel UX**: What's useful about viewing past states?
4. **Measure overhead**: How much does event logging cost in practice?
5. **Find emergent patterns**: What patterns appear when everything is an event?

### 3.2 Non-Goals (Out of Scope)

- Production-grade performance
- Distributed event store
- Schema evolution
- Snapshotting
- CQRS projections
- Event sourcing as primary architecture

This is an **experiment**, not a commitment.

---

## 4. Minimal Viable Implementation

### 4.1 Design

```typescript
// Event definition
interface Event {
  id: string;              // Unique event ID
  sequence: number;        // Global sequence number
  timestamp: Date;         // When event occurred
  nodeId: string;          // Which node this event affects
  type: string;            // Event type (e.g., "task_created", "task_started")
  payload: unknown;        // Event data
  causedBy?: string;       // Optional: parent event ID
}

// Event store (simplest possible)
class EventStore {
  private events: Event[] = [];
  private sequence = 0;

  append(nodeId: string, type: string, payload: unknown, causedBy?: string): Event {
    const event: Event = {
      id: `evt_${++this.sequence}`,
      sequence: this.sequence,
      timestamp: new Date(),
      nodeId,
      type,
      payload,
      causedBy,
    };
    this.events.push(event);
    return event;
  }

  getAllEvents(): Event[] {
    return [...this.events];
  }

  getEventsForNode(nodeId: string): Event[] {
    return this.events.filter(e => e.nodeId === nodeId);
  }

  getEventsSince(sequence: number): Event[] {
    return this.events.filter(e => e.sequence > sequence);
  }
}

// Modified Graph with event logging
class EventSourcedGraph extends Graph {
  private eventStore = new EventStore();

  send(nodeId: string, messageType: string, payload: Record<string, unknown> = {}): unknown {
    // Log event BEFORE processing
    const event = this.eventStore.append(nodeId, messageType, payload);

    // Process message normally
    const result = super.send(nodeId, messageType, payload);

    return result;
  }

  // Time travel: Replay events up to a sequence number
  replayUpTo(sequence: number): EventSourcedGraph {
    const newGraph = new EventSourcedGraph();
    const eventsToReplay = this.eventStore.getEventsSince(0).filter(e => e.sequence <= sequence);

    for (const event of eventsToReplay) {
      try {
        newGraph.send(event.nodeId, event.type, event.payload as Record<string, unknown>);
      } catch (e) {
        // Some events might fail on replay (e.g., node doesn't exist yet)
        console.warn(`Replay failed for event ${event.id}: ${e}`);
      }
    }

    return newGraph;
  }

  // Export event log
  exportEvents(): Event[] {
    return this.eventStore.getAllEvents();
  }
}
```

### 4.2 What This Gives Us

**Capabilities**:
1. **Event log**: Every state change is recorded
2. **Replay**: Reconstruct graph state at any point in history
3. **Inspection**: Query "what events led to this state?"
4. **Debugging**: "What was the state after event N?"

**Storage requirements**:
- Event: ~200 bytes (JSON serialized)
- 1000 events = ~200KB
- 10,000 events = ~2MB

For exploration, this is trivial. For production, you'd need SQLite or similar.

### 4.3 Persistence (Optional but Recommended)

```typescript
import Database from "bun:sqlite";

class SQLiteEventStore {
  private db: Database;

  constructor(path: string = ":memory:") {
    this.db = new Database(path);
    this.db.run(`
      CREATE TABLE IF NOT EXISTS events (
        id TEXT PRIMARY KEY,
        sequence INTEGER UNIQUE NOT NULL,
        timestamp TEXT NOT NULL,
        node_id TEXT NOT NULL,
        type TEXT NOT NULL,
        payload TEXT NOT NULL,
        caused_by TEXT
      )
    `);
    this.db.run(`CREATE INDEX IF NOT EXISTS idx_sequence ON events(sequence)`);
    this.db.run(`CREATE INDEX IF NOT EXISTS idx_node_id ON events(node_id)`);
  }

  append(nodeId: string, type: string, payload: unknown, causedBy?: string): Event {
    const sequence = this.getNextSequence();
    const event: Event = {
      id: `evt_${sequence}`,
      sequence,
      timestamp: new Date(),
      nodeId,
      type,
      payload,
      causedBy,
    };

    this.db.run(
      `INSERT INTO events (id, sequence, timestamp, node_id, type, payload, caused_by)
       VALUES (?, ?, ?, ?, ?, ?, ?)`,
      [event.id, event.sequence, event.timestamp.toISOString(), event.nodeId,
       event.type, JSON.stringify(event.payload), event.causedBy ?? null]
    );

    return event;
  }

  private getNextSequence(): number {
    const row = this.db.query(`SELECT MAX(sequence) as max_seq FROM events`).get() as { max_seq: number | null };
    return (row.max_seq ?? 0) + 1;
  }

  getAllEvents(): Event[] {
    return this.db.query(`SELECT * FROM events ORDER BY sequence`).all().map(this.rowToEvent);
  }

  private rowToEvent(row: any): Event {
    return {
      id: row.id,
      sequence: row.sequence,
      timestamp: new Date(row.timestamp),
      nodeId: row.node_id,
      type: row.type,
      payload: JSON.parse(row.payload),
      causedBy: row.caused_by,
    };
  }
}
```

**Why SQLite?**:
- Built into Bun (`bun:sqlite`)
- Single file persistence
- Fast queries with indexes
- No external dependencies
- Perfect for exploration

---

## 5. The Determinism Problem: What Can We Actually Replay?

### 5.1 Deterministic Actors (Can Replay)

| Actor | Events | Replayable? | Why |
|-------|--------|-------------|-----|
| TaskNode | `start`, `block`, `complete`, `spawn` | ✅ Yes | Pure state transitions |
| KnowledgeNode | `append` content | ✅ Yes | Append-only data |
| Graph | `add_node`, `add_edge`, `remove_edge` | ✅ Yes | Structural changes |

**Example replay**:
```
Event 1: task_created(id=task_1, goal="Deploy app")
Event 2: task_started(id=task_1)
Event 3: task_spawned(parent=task_1, child=task_2)
Event 4: task_completed(id=task_2, result="OK")
Event 5: task_completed(id=task_1, result="OK")

→ Replay these 5 events → Reconstruct full task tree with completion states
```

### 5.2 Non-Deterministic Actors (Cannot Replay)

| Actor | Events | Replayable? | Why Not |
|-------|--------|-------------|---------|
| ClaudeActor | `llm_query` | ❌ No | LLM response different each time |
| BashActor | `bash_exec` | ❌ No | Command output depends on system state |

**The problem**:
```
Event: llm_query(prompt="Explain task decomposition")
→ Replay: Gets DIFFERENT response from Claude API
→ State diverges from original execution
```

### 5.3 Solution: Event Enrichment (Record Responses)

Instead of just logging the **input event**, log the **input + output**:

```typescript
interface EnrichedEvent extends Event {
  result?: unknown;  // Store the result of the event
}

// Example
{
  type: "llm_query",
  payload: { prompt: "Explain task decomposition" },
  result: { response: "Task decomposition involves breaking down..." }
}
```

**Now replay is deterministic**:
- Replay reads the stored `result` instead of re-executing
- Non-deterministic operations become "cached" in the event log
- Replay is fast (no external calls) and consistent

**Trade-off**:
- Event size increases (store full LLM responses)
- Event log becomes the "source of truth" for non-deterministic results

---

## 6. Time Travel Use Cases: What's Actually Useful?

### 6.1 Debugging

**Scenario**: Task failed - why?

```typescript
// Find failure event
const failureEvent = events.find(e => e.type === "task_failed");

// Replay up to just before failure
const stateBeforeFailure = graph.replayUpTo(failureEvent.sequence - 1);

// Inspect state
console.log(stateBeforeFailure.dump());
```

**Learning value**: High - this is genuinely useful for debugging.

### 6.2 "What If" Experiments

**Scenario**: What if we hadn't spawned that child task?

```typescript
// Replay without the spawn event
const originalEvents = graph.exportEvents();
const modifiedEvents = originalEvents.filter(e => e.id !== "evt_42");

const alternateGraph = replayEvents(modifiedEvents);
```

**Learning value**: Medium - interesting but requires careful handling of dependent events.

### 6.3 Audit Trail

**Scenario**: Who changed this knowledge node and when?

```typescript
const history = eventStore.getEventsForNode("knowledge_1");
history.forEach(e => {
  console.log(`[${e.timestamp}] ${e.type}: ${JSON.stringify(e.payload)}`);
});
```

**Learning value**: High - this is a standard event sourcing use case.

### 6.4 Forking Timelines

**Scenario**: Branch execution at a decision point.

```typescript
// Replay up to decision point
const checkpoint = graph.replayUpTo(100);

// Fork 1: Choose option A
const fork1 = checkpoint.clone();
fork1.send("task_1", "choose_option", { option: "A" });

// Fork 2: Choose option B
const fork2 = checkpoint.clone();
fork2.send("task_1", "choose_option", { option: "B" });

// Compare outcomes
console.log("Fork A result:", fork1.getNode("task_1"));
console.log("Fork B result:", fork2.getNode("task_1"));
```

**Learning value**: Low for this project - this is speculative and adds significant complexity. Only useful if you're building a "planning agent" that evaluates multiple strategies.

---

## 7. Agent Specifications: If Agents Were to Build This

If we specify this for AI agents to implement:

### 7.1 Task 1: Basic Event Store

**Goal**: Implement append-only event log with SQLite persistence.

**Success Criteria**:
- [ ] Events stored with sequence numbers, timestamps, node IDs
- [ ] `append(nodeId, type, payload)` writes to SQLite
- [ ] `getAllEvents()` returns events in sequence order
- [ ] `getEventsForNode(nodeId)` filters by node
- [ ] Unit tests: append, query, persistence across restarts

**Deliverables**: `src/event-store.ts`, `src/event-store.test.ts`

**Estimated complexity**: Low (1-2 hours)

### 7.2 Task 2: Event Sourced Graph

**Goal**: Extend `Graph` to log all `send()` calls as events.

**Success Criteria**:
- [ ] Every `graph.send()` appends event to store
- [ ] `exportEvents()` returns full event log
- [ ] No changes to existing Graph behavior
- [ ] Unit tests: verify events logged for all message types

**Deliverables**: `src/event-graph.ts`, `src/event-graph.test.ts`

**Estimated complexity**: Low (1-2 hours)

### 7.3 Task 3: Replay Engine

**Goal**: Reconstruct graph state by replaying events.

**Success Criteria**:
- [ ] `replayUpTo(sequence)` creates new graph from events
- [ ] Replayed state matches original state for deterministic actors
- [ ] Handle missing nodes gracefully (log warnings, continue)
- [ ] Unit tests: create task tree, replay, verify structure

**Deliverables**: `replay()` function in `event-graph.ts`, tests

**Estimated complexity**: Medium (2-4 hours) - edge cases tricky

### 7.4 Task 4: Event Enrichment for Non-Deterministic Actors

**Goal**: Store results of non-deterministic operations in events.

**Success Criteria**:
- [ ] Events have optional `result` field
- [ ] ClaudeActor logs LLM response in event
- [ ] Replay uses stored result instead of re-calling LLM
- [ ] Unit tests: mock LLM call, replay, verify same result used

**Deliverables**: Modified `ClaudeActor`, enrichment tests

**Estimated complexity**: Medium (2-3 hours)

### 7.5 Task 5: Time Travel CLI

**Goal**: Interactive tool to explore event history.

**Success Criteria**:
- [ ] `bun run time-travel.ts` launches CLI
- [ ] Commands: `list`, `show <seq>`, `replay <seq>`, `diff <seq1> <seq2>`
- [ ] Pretty-print events and graph state

**Deliverables**: `time-travel.ts` CLI tool

**Estimated complexity**: Low (1-2 hours)

**Total estimated effort**: 8-15 hours (~1-2 days)

---

## 8. Success Criteria: How to Evaluate if Event Sourcing is Valuable

### 8.1 Technical Success

| Criterion | Measurement | Threshold |
|-----------|-------------|-----------|
| **Replay accuracy** | % of replays that match original state | >95% for deterministic actors |
| **Performance overhead** | Latency added by event logging | <10ms per event |
| **Storage efficiency** | Event log size vs graph state size | <5x graph state |
| **Replay speed** | Time to replay 1000 events | <100ms |

### 8.2 Learning Success

Did we answer these questions?

- [ ] Does event sourcing actually help debug task execution?
- [ ] What's the boundary between deterministic and non-deterministic actors?
- [ ] Is time travel UX useful or just novelty?
- [ ] What patterns emerge from treating everything as events?
- [ ] Does event sourcing make testing easier or harder?

### 8.3 Practical Success

Would we use this in production?

**Decision criteria**:
- **Yes** → If debugging value > implementation cost
- **No** → If it's just architectural complexity with no practical benefit

**Likely outcome**: Event logging is valuable. Full time travel is novelty. Keep the log, skip the fancy replay UI.

---

## 9. Recommendation: Experiment Design

### 9.1 Phase 1: Minimal Viable Event Log (Day 1)

**Goal**: Prove event logging works and is useful.

**Implementation**:
1. Add `EventStore` with SQLite persistence (Task 1)
2. Extend `Graph` to log all `send()` calls (Task 2)
3. Write tests to verify events are logged correctly

**Success check**: Can we export a complete event log of a task execution?

**Decision point**: If Phase 1 fails or is too complex, **stop here**. Event sourcing isn't a good fit.

### 9.2 Phase 2: Basic Replay (Day 1-2)

**Goal**: Prove reconstruction from events is possible.

**Implementation**:
1. Implement `replayUpTo(sequence)` (Task 3)
2. Test with deterministic actors (TaskNode)
3. Measure replay accuracy and performance

**Success check**: Can we replay a task tree and get identical structure?

**Decision point**: If replay is too buggy or slow, **stop here**. Focus on event logging for audit trails only.

### 9.3 Phase 3: Handle Non-Determinism (Day 2, Optional)

**Goal**: Make non-deterministic actors replayable.

**Implementation**:
1. Add event enrichment (Task 4)
2. Modify `ClaudeActor` to store LLM responses in events
3. Verify replay uses stored responses

**Success check**: Can we replay a task that called Claude and get identical results?

**Decision point**: If enrichment makes events too large or complex, **stop here**. Accept that some actors aren't replayable.

### 9.4 Phase 4: Time Travel CLI (Day 2, Optional)

**Goal**: Make event history explorable.

**Implementation**:
1. Build simple CLI tool (Task 5)
2. Commands to list, show, replay events
3. Diff two graph states

**Success check**: Is the CLI actually useful for debugging?

**Decision point**: If CLI is clunky or unused, **skip it**. Command-line event queries are enough.

### 9.5 Exit Criteria (Know When to Stop)

Stop if:
- [ ] Implementation takes >2 days (diminishing returns)
- [ ] Event logging adds >50ms latency (performance cost too high)
- [ ] Event log grows >10MB for realistic scenarios (storage cost too high)
- [ ] Replay fails >20% of the time (too brittle)
- [ ] We can't articulate a concrete use case for time travel

**Remember**: This is exploration, not production. The goal is to **learn**, not to ship.

---

## 10. Connections to Existing Architecture

### 10.1 How Event Sourcing Fits Current Design

**Current architecture** (from DESIGN.md):
- Everything is a node (actor)
- All interactions via `graph.send(nodeId, messageType, payload)`
- State stored in node properties

**With event sourcing**:
- `graph.send()` becomes event creation + dispatch
- Events are the "source of truth"
- Node properties become "projections" (derived from events)

**Key insight**: The message passing architecture is **already event-like**. We're just adding persistence and replay.

### 10.2 Synergy with Actor Model

Event sourcing + actor model = **Event-Driven Architecture**

| Concept | Actor Model | Event Sourcing | Combined |
|---------|-------------|----------------|----------|
| Communication | Messages | Events | Events are messages with history |
| State | Actor properties | Replay from events | Actor rebuilds state on replay |
| Time | Present only | All history | Time travel through actor states |
| Debugging | Current state | Event log | "How did we get here?" |

**Learning insight**: Event sourcing makes the actor model **auditable and reproducible**.

### 10.3 Impact on Testing

**Current testing** (from actors.test.ts):
```typescript
test("task lifecycle", () => {
  const task = new TaskNode({ ... });
  graph.registerNode(task);
  graph.send(task.properties.id, "start", {});
  expect(task.properties.state).toBe("active");
});
```

**With event sourcing**:
```typescript
test("task lifecycle", () => {
  const task = new TaskNode({ ... });
  graph.registerNode(task);
  graph.send(task.properties.id, "start", {});

  // Verify event was logged
  const events = graph.exportEvents();
  expect(events).toContainEqual(
    expect.objectContaining({ type: "start", nodeId: task.properties.id })
  );

  // Verify replay produces same state
  const replayed = graph.replayUpTo(events.length);
  const replayedTask = replayed.getNode(task.properties.id) as TaskNode;
  expect(replayedTask.properties.state).toBe("active");
});
```

**Benefit**: Tests become **regression tests** - if replay fails, state logic changed.

---

## 11. Alternatives to Full Event Sourcing

Before committing to event sourcing, consider simpler alternatives:

### 11.1 Alternative 1: Structured Logging

**What**: Log events as JSON lines to a file, but don't replay.

**Use case**: Audit trails, debugging post-mortem.

**Effort**: 1 hour (just append to file).

**Trade-off**: No replay, but gets 80% of debugging value.

```typescript
const log = Bun.file("events.jsonl", { create: true });
await log.write(JSON.stringify(event) + "\n");
```

### 11.2 Alternative 2: Snapshot + Deltas

**What**: Periodically save full graph state, log changes between snapshots.

**Use case**: Resume execution after crash.

**Effort**: 2-3 hours (snapshot to SQLite, apply deltas).

**Trade-off**: Not true event sourcing, but pragmatic.

### 11.3 Alternative 3: Git for State

**What**: Commit graph state to git after each operation.

**Use case**: Use git's time travel instead of building your own.

**Effort**: 1 hour (serialize graph to JSON, `git commit`).

**Trade-off**: Heavy-weight, but gets branching/diffing for free.

```typescript
await Bun.write("graph-state.json", JSON.stringify(graph.dump()));
await $`git add graph-state.json && git commit -m "State after event ${seq}"`;
```

**Recommendation**: Try Alternative 1 (structured logging) first. If that's insufficient, move to event sourcing.

---

## 12. Summary: Pragmatic Path Forward

### 12.1 What We Should Do

1. **Start with structured logging** (1 hour)
   - Log all `graph.send()` calls to `events.jsonl`
   - Use for debugging and audit trails
   - No replay yet

2. **If logging proves useful, add SQLite persistence** (2 hours)
   - Migrate from JSON lines to SQLite
   - Add queries: events by node, events in range, etc.

3. **If we need replay, implement minimal replay engine** (4 hours)
   - Focus on deterministic actors only
   - Accept that ClaudeActor isn't replayable (or add enrichment)

4. **If replay proves useful, add time travel CLI** (2 hours)
   - Simple tool to explore event history

**Total effort**: 1-9 hours, depending on where we stop.

### 12.2 What We Should NOT Do

- ❌ Build full CQRS/ES architecture with projections
- ❌ Implement snapshot optimization (not needed for exploration)
- ❌ Add event schema versioning (single version is fine)
- ❌ Build distributed event store (SQLite is enough)
- ❌ Implement compensation events / sagas (too complex)

### 12.3 What We'll Learn

**If successful**:
- Event sourcing makes debugging significantly easier
- Replay is useful for testing and "what if" analysis
- The boundary between deterministic and non-deterministic is clear
- Event log is the "source of truth" for audit trails

**If unsuccessful**:
- Event sourcing overhead isn't worth the benefit for this use case
- Replay is too brittle or too slow
- Time travel is a novelty without practical value

**Either outcome is valuable learning.**

---

## Appendix A: Event Sourcing Reading List

If you want to go deeper:

1. **Martin Fowler - Event Sourcing** (2005)
   - Canonical introduction
   - https://martinfowler.com/eaaDev/EventSourcing.html

2. **Greg Young - CQRS Documents** (2010)
   - Detailed CQRS/ES patterns
   - https://cqrs.files.wordpress.com/2010/11/cqrs_documents.pdf

3. **Eventstore.com - Event Sourcing Basics**
   - Practical guide with examples
   - https://www.eventstore.com/event-sourcing

4. **Redux Documentation - Time Travel Debugging**
   - Event sourcing in frontend context
   - https://redux.js.org/usage/implementing-undo-history

5. **Git Internals - How Git Works**
   - Event sourcing applied to version control
   - https://git-scm.com/book/en/v2/Git-Internals-Plumbing-and-Porcelain

---

## Appendix B: Example Event Log

What an event log might look like for a simple task execution:

```json
{"sequence":1,"type":"task_created","nodeId":"task_1","payload":{"goal":"Deploy app","deliverables":["docker-compose.yml"]}}
{"sequence":2,"type":"task_started","nodeId":"task_1","payload":{}}
{"sequence":3,"type":"task_spawned","nodeId":"task_1","payload":{"childId":"task_2","goal":"Build Docker image"}}
{"sequence":4,"type":"task_started","nodeId":"task_2","payload":{}}
{"sequence":5,"type":"bash_exec","nodeId":"task_2","payload":{"cmd":"docker build -t app ."},"result":{"stdout":"Successfully built abc123"}}
{"sequence":6,"type":"task_completed","nodeId":"task_2","payload":{"result":"Image built: abc123"}}
{"sequence":7,"type":"task_spawned","nodeId":"task_1","payload":{"childId":"task_3","goal":"Start containers"}}
{"sequence":8,"type":"task_started","nodeId":"task_3","payload":{}}
{"sequence":9,"type":"bash_exec","nodeId":"task_3","payload":{"cmd":"docker-compose up -d"},"result":{"stdout":"Started 3 containers"}}
{"sequence":10,"type":"task_completed","nodeId":"task_3","payload":{"result":"Containers running"}}
{"sequence":11,"type":"task_eval","nodeId":"task_1","payload":{}}
{"sequence":12,"type":"task_completed","nodeId":"task_1","payload":{"result":"Deployment successful"}}
```

**Replay**: Reprocessing events 1-12 reconstructs the full task tree with all completions.

**Time travel**: `replayUpTo(6)` shows state after image built but before containers started.

**Audit**: Query events for `task_2` shows exact commands run and their output.

---

## Appendix C: Comparison to Git

| Feature | Git | Event Sourcing |
|---------|-----|----------------|
| **Events** | Commits | Application events |
| **State** | File tree | Application state |
| **Replay** | `git checkout` | Replay events |
| **Branching** | `git branch` | Fork event streams |
| **History** | `git log` | Query event store |
| **Diff** | `git diff` | Compare projections |
| **Compression** | Git pack files | Snapshots |
| **Merge** | `git merge` | No standard approach |

**Key difference**: Git merges divergent histories. Event sourcing typically doesn't (single linear history per aggregate).

---

## Final Thoughts

Event sourcing is a powerful pattern, but it's **not a silver bullet**. For tk-agents:

**Pros**:
- Excellent audit trail for task/knowledge changes
- Debugging "how did we get here?" questions
- Testing via replay
- Natural fit with message passing architecture

**Cons**:
- Adds complexity (event store, replay logic)
- Performance overhead (logging every event)
- Storage overhead (events grow unbounded)
- Non-determinism challenges (LLM calls)

**Recommendation**: Start small (structured logging), prove value, then decide if full event sourcing is worth it. **Don't build infrastructure speculatively.**

The goal is to **learn by doing**, not to build a production event sourcing framework. Keep it simple, measure the value, and stop when you've learned enough.
