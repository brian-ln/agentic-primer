# Observable Pattern Design Review

**Date:** 2026-01-18
**Reviewer:** Review Agent
**Design Version:** 1.0
**Branch:** feature/actor-observables (commit f12558d)
**Status:** CHANGES REQUESTED

---

## Executive Summary

The Observable Pattern design is **comprehensive and well-reasoned**, with excellent subscription semantics and producer API. However, **the addressing pattern violates WORLDVIEW.md Principle 1 (Graph-Based Addressing)** and requires architectural refinement before implementation.

**Critical Issue:**
The design uses a separate namespace pattern (`primer.observables.*`) that creates parallel hierarchies and unclear ownership. This should be replaced with actor-scoped observables using the `.observe` capability pattern (`primer.tasks.observe.lifecycle`).

**Strengths:**
- Simple producer API (send to observable, done)
- Flexible subscription model with metadata filtering
- Clear separation of pure routing from effect actors
- Excellent use case examples demonstrating real-world application
- Thoughtful comparison with alternatives

**Changes Required:**
1. **Addressing pattern:** Adopt actor-scoped `.observe` capability (not separate namespace)
2. **Router pattern:** Each actor owns its `.observe` router (not global `primer.observables`)
3. **Discovery API:** List observables per actor (`primer.tasks.observe.list()`)
4. **Graph schema:** Observables as actors with hierarchical parent-child relationships
5. **Examples/use cases:** Update all code to use actor-scoped pattern

**Verdict:** CHANGES REQUESTED - Refactor addressing pattern to align with WORLDVIEW.md

---

## 🚨 CRITICAL ARCHITECTURAL ISSUE: Addressing Pattern

### Issue Summary

The design uses a **separate namespace** pattern for observables:
```
primer.observables.task_updates
primer.observables.agent_status
primer.observables.graph_changes
```

This pattern is **inconsistent with WORLDVIEW.md Principle 1 (Graph-Based Addressing)** and creates unnecessary coupling.

**Recommended pattern:** Actor-scoped observables using `.observe` capability:
```
primer.tasks.observe.lifecycle
primer.agents.task_55.observe.status
primer.graph.observe.changes
```

### Detailed Analysis

#### Current Design: Separate Namespace Pattern

**Addressing:**
```
primer (root)
├─ primer.observables (router)         ← Separate namespace
│   ├─ primer.observables.task_updates
│   ├─ primer.observables.agent_status
│   └─ primer.observables.graph_changes
├─ primer.tasks (router)
├─ primer.agents (router)
└─ primer.graph (actor)
```

**Discovery:**
```typescript
// List all observables (global)
const observables = await system.send("primer.observables", "list");
// Result: ["task_updates", "agent_status", "graph_changes"]

// Find task-related observables
const taskObservables = await system.query(`
  ?[name] := *observables[name, _, _], name ~ "task.*"
`);
```

**Problems:**

1. **Violates Graph Addressing Principle:**
   - Observables are SEPARATE from the actors they describe
   - Ownership unclear (who owns `task_updates`?)
   - Creates parallel hierarchy (tasks vs task_updates)

2. **Discovery is Disconnected:**
   - To find task observables: query separate namespace
   - Can't list capabilities of `primer.tasks` directly
   - Must know naming convention (`task_updates` relates to `tasks`)

3. **Namespace Pollution:**
   - All observables in one flat namespace
   - Name collisions likely (`status`, `updates`, `events`)
   - No scoping mechanism

4. **Violates Actor Ownership:**
   - `primer.tasks` doesn't "own" `primer.observables.task_updates`
   - Observable lifecycle independent of actor lifecycle
   - Unclear who manages observable metadata

#### Recommended: Actor-Scoped Pattern

**Addressing:**
```
primer (root)
├─ primer.tasks (router)
│   └─ primer.tasks.observe (router)        ← Capability of tasks
│       ├─ primer.tasks.observe.lifecycle
│       ├─ primer.tasks.observe.created
│       └─ primer.tasks.observe.completed
├─ primer.agents (router)
│   └─ primer.agents.task_55 (virtual actor)
│       └─ primer.agents.task_55.observe (router)
│           ├─ primer.agents.task_55.observe.status
│           └─ primer.agents.task_55.observe.progress
└─ primer.graph (actor)
    └─ primer.graph.observe (router)
        ├─ primer.graph.observe.mutations
        └─ primer.graph.observe.queries
```

**Discovery:**
```typescript
// List task observables (scoped to tasks actor)
const taskObservables = await system.send("primer.tasks.observe", "list");
// Result: ["lifecycle", "created", "completed"]

// List specific agent observables
const agentObservables = await system.send(
  "primer.agents.task_55.observe",
  "list"
);
// Result: ["status", "progress"]

// List ALL observables via graph query
const allObservables = await system.query(`
  ?[path] := *actors[path, type, _], type = "observable_router"
`);
// Result: [
//   "primer.tasks.observe",
//   "primer.agents.task_55.observe",
//   "primer.graph.observe"
// ]
```

**Benefits:**

1. **Aligns with Graph Addressing (WORLDVIEW.md Principle 1):**
   - `.observe` is a **capability of the actor**
   - Hierarchical addressing: actor owns its observables
   - Clear ownership: `primer.tasks` owns `primer.tasks.observe.*`

2. **Natural Discovery:**
   - List observables by asking the actor: `primer.tasks.observe.list()`
   - No separate namespace to search
   - Graph query finds all `.observe` routers

3. **Scoped Naming:**
   - Observable names scoped to actor (no global conflicts)
   - `primer.tasks.observe.status` vs `primer.agents.task_55.observe.status`
   - Clear context (status of what?)

4. **Actor Lifecycle Integration:**
   - Observable lifecycle tied to actor lifecycle
   - Virtual actors carry their observables
   - Passivation/reactivation includes observables

5. **Consistent with Actor Worldview:**
   - `.observe` similar to `.tasks`, `.knowledge`, etc. (capability routers)
   - Hierarchical addressing throughout
   - No special cases or separate namespaces

### Code Comparison

#### Current Design (Separate Namespace)

```typescript
// Producer: Task actor broadcasts lifecycle event
class TaskActor {
  async complete() {
    this.state = "completed";

    // Send to SEPARATE namespace
    await this.system.send(
      "primer.observables.task_updates",  // Different hierarchy!
      "broadcast",
      {
        event: "task_completed",
        task_id: this.id,
        timestamp: Date.now()
      }
    );
  }
}

// Subscriber: Dashboard subscribes to task updates
await system.send(
  "primer.observables.task_updates",  // Must know separate namespace
  "subscribe",
  { subscriber: "primer.dashboard" }
);
```

#### Recommended Design (Actor-Scoped)

```typescript
// Producer: Task actor broadcasts lifecycle event
class TaskActor {
  async complete() {
    this.state = "completed";

    // Send to OWN observable capability
    await this.system.send(
      "primer.tasks.observe.lifecycle",  // SAME hierarchy!
      "broadcast",
      {
        event: "task_completed",
        task_id: this.id,
        timestamp: Date.now()
      }
    );
  }
}

// Subscriber: Dashboard subscribes to task lifecycle
await system.send(
  "primer.tasks.observe.lifecycle",  // Clear: tasks' lifecycle observable
  "subscribe",
  { subscriber: "primer.dashboard" }
);
```

**Key difference:** Producer sends to its OWN `.observe` capability, not a separate namespace.

### Graph Schema Comparison

#### Current Design (Separate Namespace)

```datalog
# Observables in separate relation
*observables[name, schema, created_at] <-
  [["task_updates", {...}, "2026-01-18T10:00:00Z"],
   ["graph_changes", {...}, "2026-01-18T10:00:00Z"]]

# Actors in separate relation
*actors[path, type, metadata] <-
  [["primer.tasks", "router", {...}],
   ["primer.graph", "actor", {...}]]

# NO CONNECTION between actor and observable!
# Must rely on naming convention to link them
```

#### Recommended Design (Actor-Scoped)

```datalog
# Observables ARE actors (special type)
*actors[path, type, metadata] <-
  [["primer.tasks.observe", "observable_router", {...}],
   ["primer.tasks.observe.lifecycle", "observable", {...}],
   ["primer.graph.observe", "observable_router", {...}],
   ["primer.graph.observe.mutations", "observable", {...}]]

# Parent-child relationships explicit
*actor_hierarchy[parent, child] <-
  [["primer.tasks", "primer.tasks.observe"],
   ["primer.tasks.observe", "primer.tasks.observe.lifecycle"],
   ["primer.graph", "primer.graph.observe"],
   ["primer.graph.observe", "primer.graph.observe.mutations"]]

# CLEAR CONNECTION: observables are children of their owning actors
```

**Benefit:** Graph queries naturally discover actor → observables relationship.

### Worldview Principle 1 Violation

From WORLDVIEW.md:
> **Principle 1: Graph-Based Addressing**
> Actors are addressed via hierarchical paths forming a graph. The hierarchy serves as ONE index (primary key for direct addressing), while graph queries provide alternate access patterns.

**Current design violates this:**
- Observables in separate hierarchy (`primer.observables.*`)
- Parallel namespace to actual actors (`primer.tasks` vs `primer.observables.task_updates`)
- Ownership unclear (not hierarchical)

**Recommended design aligns:**
- `.observe` is a child of the actor (hierarchical)
- Actor owns its observable capability
- Single hierarchy, no parallel namespaces

### Migration Impact

**Changes required:**

1. **Addressing pattern:**
   - `primer.observables.task_updates` → `primer.tasks.observe.lifecycle`
   - `primer.observables.graph_changes` → `primer.graph.observe.mutations`

2. **Router pattern:**
   - Each actor has `.observe` router (not global `primer.observables`)
   - Routers are actor-scoped

3. **Discovery API:**
   - List observables per actor: `primer.tasks.observe.list()`
   - Not global: `primer.observables.list()`

4. **Graph schema:**
   - Observables as actors (with type "observable")
   - Parent-child hierarchy explicit

5. **Examples/use cases:**
   - Update all code examples to use actor-scoped pattern

**Effort:** Moderate (mostly find/replace + router refactoring)

### Recommendation

**ADOPT ACTOR-SCOPED PATTERN** (`.observe` capability)

**Rationale:**
1. ✅ Aligns with WORLDVIEW.md Principle 1 (graph addressing)
2. ✅ Clear ownership (actor owns observables)
3. ✅ Natural discovery (ask actor for capabilities)
4. ✅ Scoped naming (no global conflicts)
5. ✅ Lifecycle integration (observables tied to actors)
6. ✅ Consistent with existing patterns (`.tasks`, `.knowledge`, etc.)

**Next Steps:**
1. Update OBSERVABLES_DESIGN.md with actor-scoped pattern
2. Refactor all examples and use cases
3. Update graph schema (observables as actors)
4. Revise Phase 1 implementation tasks
5. Document migration from separate namespace pattern

### Sub-Tasks Created

Per user feedback, sub-tasks already created:
- `task_1768757702562_x4cad7whs`: Design refinement (P1)
- `task_1768757708913_c9hb0th1q`: Documentation (P1)

---

## Validation Results

### 1. Design Completeness ✅

**Observable pattern documented:**
- [x] Clear definition (virtual router actors with subscription semantics)
- [x] Observable addressing scheme (`primer.observables.<name>`)
- [x] State model (subscribers map, schema, metadata)
- [x] Message types (subscribe, unsubscribe, broadcast, describe)
- [x] Broadcast algorithm specified (fire-and-forget, filter-then-route)

**Discovery mechanism defined:**
- [x] List API (`primer.observables` → list all observables)
- [x] Describe API (get metadata for specific observable)
- [x] Graph query integration (Datalog queries for discovery)
- [x] Observable metadata schema (name, schema, subscribers, created_at)

**Subscription model specified:**
- [x] Subscribe message format (subscriber address, optional filter)
- [x] Unsubscribe message format
- [x] Subscription lifecycle (subscribe → routing table update → broadcast delivery)
- [x] Filter specification (exact, any, all, path patterns)
- [x] Filter matching semantics (with code example)

**Producer API simple and clear:**
- [x] Single broadcast message to observable
- [x] Fire-and-forget semantics (no waiting for subscribers)
- [x] System handles routing and delivery
- [x] Producer decoupled from subscriber knowledge

**Integration with actor worldview:**
- [x] Fits into hierarchical addressing (`primer.observables`)
- [x] Uses virtual actor pattern (lazy creation, single instance)
- [x] Router actor pattern (`primer.observables` manages collection)
- [x] System-managed delivery (location transparency)
- [x] Format-agnostic messages (serializable payloads)

**Assessment:** Design completeness is **excellent**. All required elements are documented with clear specifications and examples.

---

### 2. Consistency with Actor Worldview ⚠️ (1 violation)

Validating against seven principles from WORLDVIEW.md:

#### Principle 1: Graph-Based Addressing ❌ VIOLATION

**Design approach:**
- Observables use hierarchical addressing: `primer.observables.<name>`
- Router actor (`primer.observables`) manages collection
- Observable instances are virtual actors
- Graph queries support discovery

**Consistency check:**
- [x] Hierarchical paths for direct addressing ✅
- [❌] **VIOLATION:** Separate namespace creates parallel hierarchy ❌
- [❌] **VIOLATION:** Ownership unclear (observables not owned by actors) ❌
- [x] Query-based discovery supported ✅
- [x] Multiple access patterns (direct, query) ✅

**VIOLATION DETAILS:**

The separate namespace pattern (`primer.observables.*`) violates Principle 1 by creating **parallel hierarchies** rather than a unified actor hierarchy:

```
# Current (VIOLATES Principle 1):
primer.observables.task_updates     ← Separate namespace
primer.tasks                         ← Actor being described

# Should be (ALIGNS with Principle 1):
primer.tasks.observe.lifecycle       ← Capability of tasks actor
```

**Why this violates the principle:**
1. **Not truly hierarchical** - observables and actors in separate trees
2. **Ownership unclear** - who owns `primer.observables.task_updates`?
3. **Breaks single hierarchy** - creates parallel namespace instead

**See:** "🚨 CRITICAL ARCHITECTURAL ISSUE" section above for detailed analysis and recommended actor-scoped pattern.

#### Principle 2: System-Managed Placement ✅

**Design approach:**
- System handles observable delivery (local vs remote)
- Co-located actors may use in-memory optimization
- Remote actors use WebSocket/IPC
- System decides placement based on runtime conditions

**Consistency check:**
- [x] System-managed delivery ✅
- [x] Location transparency (producer doesn't know subscriber locations) ✅
- [x] Runtime optimization hints (co-location) ✅
- [x] No static placement requirements ✅

**Note:** Design explicitly states "system-managed placement" and allows for runtime optimization. Perfectly aligned.

#### Principle 3: Format-Agnostic Serialization ✅

**Design approach:**
- Messages must be serializable (stated requirement)
- No mandated format (JSON, MessagePack, etc.)
- Payload is "any serializable message"
- Transport decides format

**Consistency check:**
- [x] Serializable constraint specified ✅
- [x] No format mandate ✅
- [x] Transport flexibility ✅
- [x] Messages are data (no functions/closures) ✅

**Note:** Design correctly specifies "serializable" without mandating JSON. Aligns with format-agnostic principle.

#### Principle 4: Pragmatic Self-Description ✅

**Design approach:**
- Observable metadata (name, schema, subscriber count)
- Describe API for introspection
- Graph schema for observables
- No over-engineered homoiconicity

**Consistency check:**
- [x] System is inspectable (describe API) ✅
- [x] Metadata available (name, schema, subscribers) ✅
- [x] Graph integration (observable schema) ✅
- [x] Pragmatic approach (not over-engineered) ✅

**Note:** Design provides "good enough" introspection without forcing full homoiconicity. Pragmatic and aligned.

#### Principle 5: Virtual Actors ✅

**Design approach:**
- Observables use virtual actor pattern (Orleans-style)
- Lazy creation on first subscribe/broadcast
- Single instance per name
- Passivation when idle
- System-managed lifecycle

**Consistency check:**
- [x] Virtual actor pattern used appropriately ✅
- [x] Lazy creation specified ✅
- [x] Single instance semantics ✅
- [x] System-managed lifecycle ✅
- [x] Sender location-transparent to lifecycle ✅

**Note:** Virtual actor pattern is appropriate for observables (single instance per name). Design correctly applies principle 5.

#### Principle 6: External System Boundaries ✅

**Design approach:**
- Observables are PURE actors (no external side effects)
- No file I/O, database writes, network calls
- Pure message routing and filtering
- Effect actors can subscribe and perform side effects

**Consistency check:**
- [x] Pure actor boundary clear (observables are pure) ✅
- [x] Effect actors identified (subscribers can be effect actors) ✅
- [x] Side effects pushed to edges ✅
- [x] Functional Core / Imperative Shell pattern ✅

**Example from design:**
```typescript
// Observable broadcasts trigger effect actor side effects
actor CozoDBAdapter {
  receive(message) {
    if (message.type === "observable_broadcast") {
      // Side effect: Update CozoDB based on broadcast
      await this.client.query("...");
    }
  }
}
```

**Note:** Design correctly identifies observables as pure routing actors and delegates side effects to effect actor subscribers. Perfect alignment.

#### Principle 7: Design vs Implementation ✅

**Design approach:**
- Design phase document (status: "Design")
- Fitness function defined (latency, throughput, scalability goals)
- Optimization opportunities noted (inline delivery, batching)
- Implementation deferred to Phase 1-3

**Consistency check:**
- [x] Design → Fitness → Optimize → Validate framework followed ✅
- [x] Fitness function specified ✅
- [x] Optimization opportunities identified ✅
- [x] Implementation pragmatism acknowledged ✅

**Fitness function excerpt:**
```
Performance Goals:
- Latency: <10ms for local delivery, <100ms for remote
- Throughput: 1000+ broadcasts/sec for typical observables
- Scalability: 100+ subscribers per observable
- Failure isolation: One subscriber failure doesn't block others
```

**Note:** Design follows WORLDVIEW.md framework precisely. Design phase establishes clear goals; implementation phase can optimize pragmatically.

---

### 3. Actionability ✅

**Clear examples for each pattern:**

**Pattern 1: Simple Broadcast/Subscribe** ✅
- Complete code example with producer and subscriber
- Clear use case (event notifications)
- When to use guidance

**Pattern 2: Filtered Subscriptions** ✅
- Multiple filter examples (priority, labels)
- Real-world scenario (high-priority monitor)
- Performance optimization rationale

**Pattern 3: Sub-Actor Listener** ✅
- Full code showing parent/child structure
- Separation of concerns explanation
- Testing benefits highlighted

**Pattern 4: Fan-out Notification** ✅
- Multi-subscriber scenario
- Independent reactions demonstrated
- Failure isolation emphasized

**Pattern 5: Observables as Graph Edges** ✅
- Dependency graph example
- Event-driven workflow pattern
- Reactive cascade shown

**Use cases demonstrate real-world application:**

**Use Case 1: Task Lifecycle Notifications** ✅
- Dashboard real-time updates
- Complete code flow (subscribe → broadcast → receive → update UI)
- Practical and relatable

**Use Case 2: Agent Coordination** ✅
- Multiple agents monitoring task queue
- Filtered subscriptions for different priorities
- Realistic agent behavior

**Use Case 3: File Watcher Events** ✅
- Multiple components reacting to file changes
- Path pattern filtering
- Fan-out to graph updater and dashboard

**Use Case 4: Dependency Cascade** ✅
- Task dependency unblocking
- Observable-per-task pattern
- Reactive workflow trigger

**Design ready for implementation:**

**Phase 1: Core Implementation** ✅
- Tasks clearly listed (ObservableRouter, Observable, subscription, broadcast)
- Deliverables specified (TypeScript files, tests)
- Scoped appropriately

**Phase 2: Graph Integration** ✅
- Persistence tasks identified
- CozoDB schema required
- Discovery queries outlined

**Phase 3: Advanced Features** ✅
- Deferred appropriately (YAGNI)
- Complex filtering, policies, history
- Metrics and monitoring

**Assessment:** Actionability is **excellent**. Examples are clear, use cases are realistic, and implementation is properly decomposed into phases.

---

### 4. Integration Points Identified ✅

**Router pattern integration:**
- [x] `primer.observables` router actor specified
- [x] Manages collection of observable instances
- [x] Routes subscription/unsubscription messages
- [x] Provides discovery API (list, describe)
- [x] Spawns observables on-demand (virtual actor pattern)

**Virtual actor lifecycle:**
- [x] Lazy creation on first subscribe/broadcast
- [x] Single instance per name (deterministic)
- [x] Passivation when idle (memory efficiency)
- [x] System-managed lifecycle
- [x] State persistence in graph (subscriber lists)

**Graph addressing scheme:**
- [x] Hierarchical: `primer.observables.<observable-name>`
- [x] Router at `primer.observables`
- [x] Consistent with existing hierarchy (`primer.tasks`, `primer.knowledge`)
- [x] Graph queries for discovery (Datalog examples provided)

**Effect actor boundaries:**
- [x] Observables are PURE actors (no side effects)
- [x] Pure routing and filtering only
- [x] Effect actors subscribe and perform side effects
- [x] Clear separation (Functional Core / Imperative Shell)
- [x] Example: CozoDBAdapter subscribes, performs database writes

**Subscription metadata format:**
- [x] Subscribe message schema defined
- [x] Filter format specified (priority, event, labels_any, labels_all, path_pattern)
- [x] Unsubscribe message schema
- [x] Broadcast envelope schema
- [x] Filter matching semantics with code

**Assessment:** Integration points are **thoroughly identified and documented**. All touch points with existing actor worldview are clearly specified.

---

## Strengths

### 1. Elegant Integration with Actor Worldview

The observable pattern fits naturally into the existing actor hierarchy without introducing new concepts. Observables are simply "virtual router actors with subscription semantics" - reusing existing patterns (virtual actors, router actors) rather than inventing new ones.

```
primer (root)
├─ primer.tasks (router)          ← Existing
├─ primer.observables (router)     ← NEW, but same pattern
│   ├─ primer.observables.task_updates (virtual observable)
│   └─ primer.observables.graph_changes (virtual observable)
```

### 2. Simple Producer API

Producers have minimal coupling: send to observable address, done. System handles everything else (subscriber routing, filtering, delivery). This is exactly the right abstraction.

```typescript
// Producer: One line
await system.send("primer.observables.task_updates", "broadcast", {
  event: "task_completed",
  task_id: "task_42"
});
// No knowledge of subscribers, no waiting, no error handling
```

### 3. Flexible Subscription Model

Subscription API supports multiple sophistication levels:
- **Simple:** Subscribe with no filter (receive all messages)
- **Filtered:** Subscribe with metadata filter (priority, labels, event types)
- **Advanced:** Path patterns, custom predicates (Phase 3)

Designers can start simple and add filtering when needed. Good YAGNI adherence.

### 4. Clear Comparison with Alternatives

Design includes thoughtful comparison with:
- **Direct messaging:** When to use observables vs point-to-point
- **Event log:** When to use observables (ephemeral) vs event log (persistent)
- **Polling:** When to use observables (push) vs polling (pull)

This helps implementers choose the right tool for each use case.

### 5. Excellent Use Case Coverage

Four use cases cover different dimensions:
1. **Task Lifecycle:** Dashboard real-time updates
2. **Agent Coordination:** Filtered subscriptions for different priorities
3. **File Watcher:** Fan-out to multiple components
4. **Dependency Cascade:** Observables as graph edges

These demonstrate breadth of application (UI, coordination, events, dependencies).

### 6. Proper Phase Decomposition

Implementation split into three phases:
- **Phase 1:** Core functionality (MVP)
- **Phase 2:** Graph integration (persistence, discovery)
- **Phase 3:** Advanced features (complex filtering, policies, metrics)

This follows YAGNI and allows iterative validation.

---

## Issues Found

### Minor Issue 1: Performance Characteristics Under-Explored

**Issue:** Design states performance goals but doesn't deeply analyze:
- What happens with 1000+ subscribers on one observable?
- How does filter evaluation scale with complex predicates?
- What's the message queue backlog strategy?
- How are slow subscribers handled?

**Impact:** Low (acceptable for design phase)

**Recommendation:** During Phase 1 implementation, add performance tests and document actual characteristics. Consider:
- Subscriber limit per observable (soft limit?)
- Filter evaluation budget (timeout?)
- Backpressure strategy (drop messages? block producer?)
- Slow subscriber isolation (separate queue?)

**Status:** Note for implementation phase, not blocking for design approval.

### Minor Issue 2: Migration Path Deferred

**Issue:** Design states "Migration path (deferred to implementation phase)" in SHOULD deliverables.

**Impact:** Low (appropriate given YAGNI)

**Recommendation:** Before Phase 1 implementation, create brief migration document:
- Which existing patterns should migrate to observables?
- How to migrate polling loops to subscriptions?
- Backward compatibility strategy (if any)
- Rollback plan if observables don't meet fitness function

**Status:** Defer to Phase 1 kickoff, not blocking for design approval.

### Minor Issue 3: Observable Lifecycle Edge Cases

**Issue:** Design specifies lazy creation and passivation but doesn't address:
- What happens if observable passivated while subscribers exist?
- Are subscriptions restored on reactivation?
- How long does observable stay active with zero subscribers?
- Race condition: broadcast arrives during passivation?

**Impact:** Low (implementation detail)

**Recommendation:** During Phase 1, specify observable lifecycle state machine:
- States: Not Created → Active → Idle → Passivated
- Transitions: First message → Active, Last unsubscribe → Idle, Idle timeout → Passivated
- Reactivation: Restore subscribers from graph
- Race handling: Queue messages during passivation

**Status:** Implementation detail, not blocking for design approval.

---

## Recommendations

### For Phase 1 Implementation

**1. Start with Zero-Subscriber Observable Lifecycle**

Clarify behavior when observable has zero subscribers:
- Does observable passivate immediately?
- Grace period before passivation?
- Broadcasts with zero subscribers: log, drop, or error?

**Suggested approach:** Keep observable active for 5 minutes after last unsubscribe. This handles temporary unsubscribe/resubscribe patterns without churn.

**2. Add Observable Lifecycle Hooks**

Consider lifecycle hooks for debugging and monitoring:
```typescript
{
  onCreated: (observable) => log("Observable created: " + observable.name),
  onFirstSubscriber: (observable) => log("First subscriber"),
  onLastUnsubscribe: (observable) => log("Last unsubscribe"),
  onPassivated: (observable) => log("Observable passivated")
}
```

Useful for understanding observable lifecycle in production.

**3. Implement Basic Metrics from Day 1**

Track minimal metrics in Phase 1:
- Broadcast count per observable
- Subscriber count per observable
- Filter match rate (% of broadcasts delivered)
- Delivery latency (p50, p99)

Helps validate fitness function and identify optimization opportunities.

**4. Create Observable Testing Utilities**

Build test utilities for observables:
- Mock observable (in-memory, no system dependency)
- Subscriber spy (records broadcasts received)
- Broadcast recorder (captures all broadcasts)
- Filter validator (test filter matching logic)

Simplifies testing code that uses observables.

### For Phase 2 (Graph Integration)

**1. Design Observable Schema Carefully**

CozoDB schema should support:
- Observable metadata (name, schema, created_at)
- Subscriber list with filters
- Observable discovery queries
- Subscription history (for debugging)

Consider versioning observable schema (future compatibility).

**2. Optimize Discovery Queries**

Graph queries should be fast:
- Index observable names
- Index subscriber addresses
- Cache observable list (refresh periodically)

Discovery is likely frequent operation (dashboards, monitors).

**3. Plan for Observable Schema Evolution**

How do observables handle schema changes?
- Observable `task_updates` initially broadcasts `{ event, task_id }`
- Later adds `{ priority, labels }` fields
- Old subscribers should still work (backward compatibility)

Suggestion: Use optional fields, never remove fields.

### For Phase 3 (Advanced Features)

**1. Complex Filtering with Budget**

If implementing complex predicates (Phase 3):
- Set evaluation budget (max time, max operations)
- Timeout slow filters
- Log filter evaluation performance
- Suggest filter optimizations to users

Prevents one complex filter from degrading system.

**2. Subscription Policies as Graph Configuration**

Store subscription policies in graph:
```datalog
# Subscription policies
*subscription_policies[
  observable_name,
  max_subscribers,
  rate_limit_per_subscriber,
  backpressure_strategy
]
```

Allows per-observable tuning without code changes.

**3. Observable History (Replay) as Opt-In**

If adding replay (Phase 3), make it opt-in:
- Most observables: ephemeral (default)
- Specific observables: persistent (explicitly configured)

Avoids performance overhead for observables that don't need replay.

---

## Final Verdict

### CHANGES REQUESTED - Refactor Addressing Pattern

**Critical Issue Identified:**
The design violates **WORLDVIEW.md Principle 1 (Graph-Based Addressing)** by using a separate namespace pattern instead of actor-scoped observables.

**Reasoning:**
1. **Design completeness:** All required elements documented ✅
2. **Worldview consistency:** **VIOLATION** - Principle 1 (addressing) ❌
3. **Actionability:** Clear examples and phased implementation ✅
4. **Integration:** All touch points identified ✅

**Critical change required:**
- **Current:** Separate namespace (`primer.observables.task_updates`)
- **Required:** Actor-scoped pattern (`primer.tasks.observe.lifecycle`)

**Why this blocks approval:**
- Violates fundamental architectural principle (hierarchical addressing)
- Creates parallel hierarchies (bad design smell)
- Unclear ownership (who owns observables?)
- Inconsistent with existing patterns (`.tasks`, `.knowledge`)

**Additional notes (minor):**
- Performance characteristics: Add testing in Phase 1
- Migration path: Create brief guide before Phase 1
- Lifecycle edge cases: Specify state machine in Phase 1

### Required Changes Before Implementation

**Design document must be updated to:**

1. **Adopt actor-scoped addressing pattern:**
   - Replace `primer.observables.*` with `<actor-path>.observe.*`
   - Example: `primer.tasks.observe.lifecycle` (not `primer.observables.task_updates`)

2. **Refactor router pattern:**
   - Each actor owns its `.observe` router (not global router)
   - Example: `primer.tasks.observe` is a router actor owned by `primer.tasks`

3. **Update discovery API:**
   - List observables per actor: `system.send("primer.tasks.observe", "list")`
   - Not global: `system.send("primer.observables", "list")`

4. **Revise graph schema:**
   - Observables are actors (with type "observable" or "observable_router")
   - Parent-child hierarchy explicit in graph

5. **Update all examples and use cases:**
   - Replace separate namespace pattern with actor-scoped pattern
   - Show ownership (actor broadcasts to its own `.observe`)

**Effort estimate:** Moderate (mostly find/replace + router refactoring)

**Sub-tasks already created:**
- `task_1768757702562_x4cad7whs`: Design refinement (P1)
- `task_1768757708913_c9hb0th1q`: Documentation (P1)

### What Stays the Same (Approved Aspects)

The following design elements are **approved and do NOT need changes:**

✅ Subscription semantics (subscribe, unsubscribe, filter)
✅ Broadcast algorithm (fire-and-forget, filter-then-route)
✅ Producer API simplicity (send to observable, done)
✅ Virtual actor pattern (lazy creation, single instance)
✅ System-managed delivery (location transparency)
✅ Format-agnostic messages (serializable)
✅ Pure actor boundaries (observables are pure routing)
✅ Effect actor integration (subscribers can be effect actors)
✅ Phased implementation (Phase 1 MVP, Phase 2 persistence, Phase 3 advanced)
✅ Use case coverage (task lifecycle, agent coordination, file events, dependencies)
✅ Testing approach and utilities

**Only the addressing pattern needs refactoring.** The rest of the design is sound.

---

## Next Steps

**Immediate (Design Refinement - BLOCKING):**

1. **Refactor OBSERVABLES_DESIGN.md:**
   - Replace all `primer.observables.*` with actor-scoped pattern
   - Update addressing section (actor-scoped `.observe` capability)
   - Refactor all code examples
   - Update use cases
   - Revise graph schema (observables as actors)
   - Update discovery API documentation

2. **Update related documentation:**
   - Add `.observe` pattern to WORLDVIEW.md (if not present)
   - Document actor capability pattern
   - Create migration guide (separate namespace → actor-scoped)

3. **Submit revised design for re-review:**
   - Address all required changes
   - Ensure consistency with WORLDVIEW.md Principle 1
   - Validate examples match new pattern

**After Design Approval (Pre-Implementation):**

1. Create Phase 1 implementation tasks in task tracker
2. Assign implementers to actor-scoped observable routers
3. Write testing strategy document (test utilities, integration tests)
4. Specify observable lifecycle state machine

**Phase 1 (Core Implementation - AFTER design approval):**

1. Implement actor-scoped `.observe` routers (per actor)
2. Implement `Observable` virtual actor instances
3. Add subscription/unsubscription handling
4. Implement broadcast with filtering
5. Write integration tests
6. Document actual performance characteristics

**Phase 2 (Graph Integration):**

1. Design CozoDB schema for actor-scoped observables
2. Persist subscriber lists (with parent-child hierarchy)
3. Implement discovery queries (find all `.observe` routers)
4. Add observable metadata support

**Phase 3 (Advanced Features):**

1. Complex filtering with predicates
2. Subscription policies (rate limits, backpressure)
3. Optional observable history (replay)
4. Metrics dashboard

---

## Validation Checklist Summary

**1. Design Completeness:**
- [x] Observable pattern documented
- [x] Discovery mechanism defined
- [x] Subscription model specified
- [x] Producer API simple and clear
- [x] Integration with actor worldview explained

**2. Consistency with Actor Worldview:**
- [❌] Graph addressing (hierarchical paths) - **VIOLATION: Separate namespace** ❌
- [x] Virtual actor pattern (lazy creation) ✅
- [x] System-managed delivery (not actor concern) ✅
- [x] Format-agnostic messages (serializable) ✅
- [x] Fits Design → Fitness → Optimize → Validate framework ✅
- [x] Pure actors (routing only, no side effects) ✅
- [x] Effect boundaries clear ✅

**3. Actionability:**
- [x] Clear examples for each pattern ✅
- [x] Use cases demonstrate real-world application ✅
- [x] Design ready for implementation ✅
- [x] Implementation tasks properly decomposed ✅

**4. Integration Points Identified:**
- [x] Router pattern (primer.observables) ✅
- [x] Virtual actor lifecycle ✅
- [x] Graph addressing scheme ✅
- [x] Effect actor boundaries ✅
- [x] Subscription metadata format ✅

---

## Reviewer Signature

**Reviewed by:** Review Agent (Background Subagent)
**Date:** 2026-01-18
**Task:** task_1768757398967_tmd7xascr
**Verdict:** CHANGES REQUESTED - Addressing pattern violates WORLDVIEW.md Principle 1
**Confidence:** High (comprehensive review against worldview principles)

**Critical Issue:** Separate namespace pattern (`primer.observables.*`) must be replaced with actor-scoped pattern (`<actor>.observe.*`) to align with hierarchical addressing principle.

**Approved Aspects:** Subscription semantics, broadcast algorithm, producer API, virtual actors, system-managed delivery, pure actor boundaries, effect integration, phased implementation, use cases.

**Blocking Changes:** Refactor addressing pattern throughout OBSERVABLES_DESIGN.md before implementation can proceed.

---

**End of Review**
