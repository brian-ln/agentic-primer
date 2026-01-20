# Actor Worldview Analysis: Design Thinking, Not Implementation Dogma

**Agent:** Background subagent (worldview synthesis)
**Date:** 2026-01-18
**Status:** Complete - Integrated User Feedback
**Context:** Synthesis of ACTOR-WORLDVIEW.md with user corrections on addressing, placement, serialization, homoiconicity, virtual actors, external boundaries, and design vs implementation

---

## Executive Summary

The actor model is a **design tool for thinking about systems**, not an implementation straitjacket. This analysis clarifies seven critical corrections to the original worldview analysis:

1. **Graph-Based Addressing** - Hierarchical paths are one index; graph traversal provides alternate access
2. **System-Managed Placement** - System determines where actors run; configuration is runtime-adaptable
3. **Format-Agnostic Serialization** - Messages must be serializable; format matches target system needs
4. **Pragmatic Self-Description** - Self-describing with minimal syntax, not dogmatic homoiconicity
5. **Virtual Actors Optional** - One strategy among many; sender shouldn't care about lifecycle
6. **External System Boundaries** - Actors interact with non-actor systems (files, databases, networks)
7. **Design vs Implementation** - Model as actors (design), optimize for fitness function (implementation)

The actor model helps us **think clearly** about system structure, then we **optimize pragmatically** to meet real-world constraints.

---

## Part 1: Addressing - Graph, Not Just Hierarchy

### The Correction

**WRONG (original claim):**
> "Every actor has a hierarchical dotted-path address that fully describes its location."

**RIGHT (corrected understanding):**
> "Actors are nodes in a graph. Dotted paths are ONE index (like a primary key). Multiple traversal paths reach the same actor."

### Why This Matters

Hierarchical addressing is **convenient** but not **exclusive**. The system is a graph where:

- **Hierarchical paths** provide direct addressing: `primer.tasks.task_28`
- **Edge traversal** provides relationship-based access: "tasks blocked by task_28"
- **Router actors** act as graph indexes: `primer.tasks` routes to task collection
- **Queries** provide alternate discovery: "all tasks with priority P0"

**Example:**

```typescript
// Direct addressing (hierarchical)
await system.send("primer.tasks.task_28", "get", {});

// Relationship traversal (graph)
const blockedTasks = await graph.query(`
  ?[task] := *edges{from: "task_28", type: "blocks", to: task}
`);

// Router-based discovery (index)
// "primer.tasks" is both:
// - An actor that handles task collection operations
// - An index for discovering task_* actors
await system.send("primer.tasks", "list", { filter: { priority: "P0" } });
```

### Design Implication

**Don't force everything into a hierarchy.** Model relationships as graph edges, use hierarchical addressing for convenience, query the graph for complex access patterns.

Router actors like `primer.tasks` are **graph indexes** that provide curated views:
- They're actors (can receive messages)
- They maintain indexes over subgraphs
- They route messages to appropriate child actors
- They're one access strategy, not the only one

---

## Part 2: Placement - System-Managed, Not Static Config

### The Correction

**WRONG (original claim):**
> "Configuration determines where actors run. Change config file, change deployment."

**RIGHT (corrected understanding):**
> "System determines where actors run based on runtime conditions. Configuration is input, not law. Actors can be recovered elsewhere if unreachable."

### Why This Matters

**Static placement** (config says "actor X runs on server Y") breaks in real systems:
- Server Y crashes → actor X unavailable
- Network partition → can't reach server Y
- Load spike → server Y overloaded

**System-managed placement** enables:
- **Fault tolerance:** Actor becomes unreachable → system spawns replacement elsewhere
- **Load balancing:** High load on one node → system migrates actors
- **Adaptive optimization:** Message patterns change → system re-co-locates chatty actors

**Example:**

```typescript
// NOT THIS (static config):
const config = {
  "primer.tasks": { location: "server-2", url: "ws://server-2:3000" }
};
// Problem: Server-2 crashes → primer.tasks unavailable

// THIS (system-managed):
const placementHints = {
  "primer.tasks": {
    prefer: "local",  // Hint, not requirement
    fallback: "any-available-node",
    constraints: { maxLatency: "50ms" }
  }
};
// System decides actual placement based on:
// - Node availability
// - Current load
// - Network topology
// - Message rate profiling
```

### Design Implication

**Configuration provides hints and constraints, not commands.** The runtime system makes final placement decisions based on observed behavior and system health.

This is how Orleans, Akka Cluster, and Erlang/OTP work in production:
- Virtual actors spawn where needed
- Supervisors restart actors on healthy nodes
- Cluster rebalancing happens automatically

---

## Part 3: Serialization - Format-Agnostic, Not JSON-Only

### The Correction

**WRONG (original claim):**
> "All messages must be JSON-serializable."

**RIGHT (corrected understanding):**
> "Messages must be serializable. Format should match target system's needs. Project between formats at boundaries."

### Why This Matters

**JSON everywhere** is overly restrictive:
- **Inefficient** for binary data (base64 bloat)
- **Lossy** for some types (BigInt, Date precision)
- **Slow** for high-throughput systems

**Format-agnostic serialization** means:
- In-memory: Plain JavaScript objects (no serialization)
- WebSocket: JSON (human-debuggable)
- High-performance IPC: MessagePack or Cap'n Proto (efficient binary)
- Event log: JSON-LD (semantic web compatibility)
- GPU communication: Binary buffers (zero-copy)

**Example:**

```typescript
// Actor message (logical representation)
const message = {
  type: "update",
  targetId: "task_28",
  payload: { state: "complete", timestamp: Date.now() }
};

// Serialization depends on transport:

// Local (in-memory): No serialization
await localActor.send(message);  // Pass object reference

// WebSocket (remote): JSON
ws.send(JSON.stringify(message));

// High-throughput IPC: MessagePack
ipc.send(msgpack.encode(message));

// Event log: JSON-LD (with semantic context)
eventLog.append({
  "@context": "https://primer.dev/schema/v1",
  "@type": "TaskUpdateEvent",
  ...message
});
```

### Design Implication

**Design with serializable messages**, but don't mandate a specific format. Each transport/persistence layer chooses "good enough" format:
- **Local:** Objects (fastest, no copy)
- **Network:** JSON (debuggable) or binary (efficient)
- **Storage:** JSON-LD (queryable, semantic)

The key constraint: **Messages are data, not code.** No functions, no closures, no symbols. Everything else is negotiable.

---

## Part 4: Self-Description - Pragmatic, Not Dogmatic

### The Correction

**WRONG (original claim):**
> "Homoiconic representations (S-expressions) are essential. The system IS its own data."

**RIGHT (corrected understanding):**
> "Self-describing with minimal syntax rules. Systems don't have to use same representation. Good enough for the system that owns it."

### Why This Matters

**Homoiconicity** (code-as-data, like LISP S-expressions) is powerful but can become a dogma:
- Not every system needs full homoiconicity
- YAML, JSON, TOML are "good enough" for many use cases
- Over-engineering self-description adds complexity

**Pragmatic self-description** means:
- Actor topology is data (inspectable, transformable)
- Message logs are data (queryable, replayable)
- But we don't require LISP-style "eval" on system structure

**Example:**

```typescript
// Self-describing (but not homoiconic):

// System topology as data
const topology = {
  nodes: {
    "primer.tasks": { type: "router", children: ["task_28", "task_29"] },
    "task_28": { type: "task", properties: { goal: "...", state: "active" } }
  },
  edges: [
    { from: "task_28", to: "task_29", type: "blocks" }
  ]
};

// This is data, we can query it:
const blockers = topology.edges.filter(e => e.type === "blocks" && e.from === "task_28");

// But we don't need to "eval" it as code (no homoiconic eval)
```

### Design Implication

**Make the system inspectable** (topology, messages, state are data), but don't go overboard with full homoiconicity unless you need metaprogramming capabilities.

LISP's power comes from uniform representation (S-expressions). Actor systems benefit from **structured data** (JSON, YAML, graphs), but full homoiconicity is optional.

**The key: Self-describing with minimal syntax rules.** Each subsystem uses representations "good enough" for its needs. Project between formats at boundaries.

---

## Part 5: Virtual Actors - One Strategy, Not Mandatory

### The Correction

**WRONG (original claim):**
> "Virtual actors are THE way to manage actor lifecycle. Lazy creation, single instance per ID."

**RIGHT (corrected understanding):**
> "Virtual actors are ONE strategy. Message sender shouldn't care about lifecycle strategy. System manages virtualization per actor type."

### Why This Matters

**Virtual actors** (Orleans-style) are powerful:
- Lazy creation (spawn on first message)
- Single instance per ID (deterministic)
- Automatic passivation (garbage collect idle actors)

But they're not the **only** strategy:
- **Persistent actors** (always running, never passivated)
- **Transient actors** (spawn per request, die after response)
- **Pooled actors** (worker pool, any worker handles message)

**Example:**

```typescript
// Virtual actor (Orleans/Akka style)
// - Created on first message
// - Single instance per ID
// - Passivated if idle
await system.send("primer.tasks.task_28", "update", {...});
// System: Does "task_28" exist? No → create. Yes → route to existing.

// Persistent actor (Erlang/OTP style)
// - Created at system startup
// - Never passivated
// - Supervisor restarts if crashes
await system.send("primer.cozodb", "query", {...});
// System: CozoDB actor always running, route immediately.

// Transient actor (request-scoped)
// - Created per request
// - Dies after response
// - No identity
await system.send("primer.compute", "calculate_hash", { data: [...] });
// System: Spawn worker, calculate, send response, terminate worker.
```

### Design Implication

**Sender location-transparent to lifecycle.** Caller just sends to address; system handles:
- Virtual actors for stateful entities (tasks, knowledge)
- Persistent actors for singletons (database, event log)
- Transient actors for stateless computation (hashing, parsing)

Correct constraint: **"Caller is location-transparent to lifecycle strategy,"** not "All actors are virtual."

---

## Part 6: External System Boundaries - The Missing Section

### The Gap in Original Analysis

Original analysis focused on actors communicating with actors. Missing: **How do actors interact with non-actor systems?**

### Non-Deterministic External Systems

Actors are deterministic (same input → same output). But real systems interact with:
- **File I/O:** Files can change, disappear, or be locked
- **Network:** Requests timeout, servers go down
- **Databases:** Queries fail, transactions conflict
- **Time:** `Date.now()` is different every call
- **Randomness:** `Math.random()` is non-deterministic

### Effect Actors - Functional Core / Imperative Shell

**Pattern:** Separate pure computation (deterministic) from side effects (non-deterministic).

**Example:**

```typescript
// WRONG: Actor directly does file I/O
actor TaskActor:
  receive "save":
    const data = JSON.stringify(this.state);
    fs.writeFileSync(`task_${this.id}.json`, data);  // Side effect!
    reply "ok"

// RIGHT: Actor delegates to effect actor
actor TaskActor:
  receive "save":
    const data = JSON.stringify(this.state);  // Pure
    send data to FileSystemActor  // Delegate effect
    await response
    reply response

actor FileSystemActor:
  receive "write":
    try {
      fs.writeFileSync(message.path, message.data);  // Effect isolated here
      reply { success: true }
    } catch (error) {
      reply { success: false, error: error.message }
    }
```

**Benefit:**
- Pure actors (TaskActor) are deterministic and testable
- Effect actors (FileSystemActor) encapsulate non-determinism
- Supervision trees handle failures in effect actors

### Supervision Trees for Failure Handling

**Pattern:** Effect actors supervised by restart-on-crash policy.

**Example:**

```typescript
// Supervisor for external system actors
actor ExternalSystemSupervisor:
  children: [FileSystemActor, NetworkActor, DatabaseActor]
  strategy: "restart-on-crash"

  receive "child-crashed":
    log error
    spawn new child actor
    update routing table

// Effect actor crashes on transient failure
actor DatabaseActor:
  receive "query":
    const result = await db.query(message.sql);  // Might throw
    reply result
    // If query throws, actor crashes
    // Supervisor restarts actor
    // Sender retries or handles error
```

**Benefit:**
- "Let it crash" philosophy (Erlang)
- Supervisor provides fault tolerance
- System self-heals from transient failures

### Impedance Mismatch and Adapters

**Pattern:** Adapter actors translate between actor model and external APIs.

**Example:**

```typescript
// External API (not actor-based)
class CozoClient {
  query(sql: string): Promise<QueryResult> { ... }
}

// Adapter actor (wraps external API)
actor CozoDBAdapter:
  private client: CozoClient;

  receive "query":
    try {
      const result = await this.client.query(message.sql);
      reply { success: true, data: result }
    } catch (error) {
      reply { success: false, error: error.message }
    }

// Rest of system uses actor interface
await system.send("primer.cozodb", "query", { sql: "SELECT * FROM tasks" });
// CozoDBAdapter translates to CozoClient.query()
```

**Benefit:**
- External systems wrapped in actor boundary
- Rest of system uses uniform message interface
- Easy to mock adapter for testing

---

## Part 7: Design vs Implementation - THE CRITICAL SHIFT

### The Most Important Correction

**WRONG (original implication):**
> "Actor model is the implementation. Pure actors everywhere, no compromises."

**RIGHT (corrected understanding):**
> "Actor model is the DESIGN tool. Define fitness function, optimize implementation to meet it. May break actor purity if needed."

### The Design-to-Implementation Pipeline

```
1. MODEL system as actors (design phase)
   - Identify entities (tasks, knowledge, files)
   - Define messages (update, query, notify)
   - Draw actor graph (relationships, supervision)

2. DEFINE fitness function (goals)
   - Deliverables: What must system do?
   - Performance: Latency, throughput, resource usage
   - Success criteria: How do we know it works?

3. OPTIMIZE implementation (meet fitness function)
   - Profile: Where are bottlenecks?
   - Fuse: Merge actors with high message rate
   - Specialize: Inline actors on critical path
   - Bypass: Skip actor abstraction if justified by fitness function
```

### Example: CozoDB Integration

**Phase 1: Design (pure actors)**

```typescript
// Model CozoDB as actor
actor CozoDBQueryActor:
  receive "query":
    result = await database.query(message.sql)
    reply result

// Every query goes through actor
await system.send("primer.cozodb.query", "query", { sql: "..." });
```

**Phase 2: Define Fitness Function**

```
Goals:
- 10,000 task queries per second (throughput)
- <5ms p99 latency (performance)
- Event log for all writes (audit)
- Consistent reads (correctness)

Measurement:
- Actor message passing adds ~2ms overhead
- 10k qps requires <0.1ms per query
- Actor abstraction is 20x over budget
```

**Phase 3: Optimize Implementation**

```typescript
// OPTIMIZATION: Direct database access for reads (breaks actor purity!)
class TaskRepository {
  async getTask(id: string): Promise<Task> {
    return await cozoClient.query(`SELECT * FROM tasks WHERE id = $id`, { id });
    // Direct call, no actor message
    // Justified by fitness function (latency requirement)
  }
}

// But keep actor for WRITES (audit requirement)
actor CozoDBWriteActor:
  receive "update":
    await eventLog.append(message);  // Audit
    await database.query(message.sql);  // Write
    reply { success: true }

// Optimization breaks actor purity, but MEETS FITNESS FUNCTION
```

### Design vs Implementation - The Mindset

| Design (Actor Model) | Implementation (Optimized) |
|----------------------|----------------------------|
| Model everything as actors | Fuse actors where needed |
| Pure message passing | Inline calls on hot paths |
| Location transparent | Co-locate for performance |
| Supervision trees | Let crashes propagate if acceptable |
| Deterministic | Accept non-determinism if fitness allows |

**The key insight:** Actor model helps you **design clearly**, then you **implement pragmatically**.

**Constraints are design tools, not implementation laws.**

### Connection to Compilation Research

From ACTOR_COMPILATION_RESEARCH.md:

- **Actor fusion:** Merge actors with high message rate → eliminate overhead
- **Specialization:** Inline actor code for specific message types → remove dispatch
- **Dataflow compilation:** Identify pure dataflow subgraphs → compile to GPU kernels

**The design (actor model) enables optimization analysis.** You can't optimize what you can't understand. Pure actor design gives you:
- Clear message flow (dataflow graph)
- Independence analysis (which actors can fuse?)
- Critical path identification (where to optimize?)

Then you **compile away the abstraction** where fitness function demands it.

---

## Part 8: Synthesis - The Complete Worldview

### The Seven Corrections, Integrated

1. **Graph addressing:** Hierarchical paths + edge traversal + router indexes
2. **System-managed placement:** Runtime adaptation, not static config
3. **Format-agnostic serialization:** Messages are data; format matches transport
4. **Pragmatic self-description:** Inspectable structure, not dogmatic homoiconicity
5. **Virtual actors optional:** One lifecycle strategy among many
6. **External boundaries:** Effect actors, supervision, adapter pattern
7. **Design vs implementation:** Model as actors, optimize for fitness

### The Updated Worldview

**Actor model is a THINKING TOOL:**
- Design systems as actors and messages
- Draw graphs of entities and relationships
- Reason about supervision and fault tolerance
- Analyze message flow and dataflow

**Then OPTIMIZE PRAGMATICALLY:**
- Define fitness function (deliverables, performance, constraints)
- Profile to find bottlenecks
- Apply optimizations (fusion, specialization, bypasses)
- Validate against fitness function

**Constraints guide design, not implementation:**
- Message passing → helps identify interfaces
- Actor isolation → helps understand independence
- Supervision trees → helps structure fault tolerance
- Graph topology → helps analyze data flow

**But implementation may break constraints:**
- Fuse actors → eliminate message overhead
- Inline calls → remove actor dispatch
- Direct access → bypass actor for critical path
- Shared state → optimize hot paths

**As long as fitness function is met.**

### How This Changes Practice

**OLD approach (dogmatic):**
```
1. Everything MUST be an actor
2. ALL communication MUST be messages
3. NEVER bypass actor abstraction
4. Result: Over-engineered, slow, rigid
```

**NEW approach (pragmatic):**
```
1. DESIGN as actors (clear thinking)
2. DEFINE fitness function (clear goals)
3. OPTIMIZE to meet function (clear tradeoffs)
4. VALIDATE against design (clear verification)
```

### Example: Task System Design

**Design phase (pure actors):**
```
TaskCollection actor
  ├─ Task_28 actor
  │    ├─ Attachment actors
  │    └─ Comment actors
  ├─ Task_29 actor
  └─ ...

Messages:
- create_task
- update_task
- add_attachment
- query_blockers
```

**Fitness function:**
```
- 1000 task updates/sec (throughput)
- <10ms update latency (performance)
- Event log for all changes (audit)
- Task graph queries <50ms (analytics)
```

**Optimized implementation:**
```
- Task updates: Direct CozoDB writes (meets latency)
- Event log: Async append (meets audit without blocking)
- Graph queries: Separate read-optimized index (meets analytics)
- Attachments: Virtual actors (meets memory efficiency)

Result: Breaks actor purity in 3 places, but MEETS ALL FITNESS CRITERIA
```

### The Meta-Lesson

**Most complexity comes from CONFUSING design and implementation:**
- Design says "everything is an actor" → implementation tries to implement literally → over-engineered
- Design says "message passing" → implementation adds actors everywhere → slow
- Design says "location transparent" → implementation adds network everywhere → fragile

**Better approach:**
- Design: Pure actor model (clear thinking)
- Fitness: Explicit requirements (clear goals)
- Implementation: Optimized to meet fitness (clear tradeoffs)
- Validation: Does optimized implementation meet design intent? (clear verification)

**Actor model is not an implementation straitjacket. It's a design telescope.**

---

## Part 9: Implications for Primer

### Current State

Primer has:
- Graph-based storage (CozoDB)
- Task tracking (tasks.json)
- Message-passing CLI (daemon)
- WebSocket protocol (browser ↔ daemon)

But design is **implicit**, not **explicit actor model**.

### Applying the Worldview

**Phase 1: Design (model as actors)**

```
primer (root actor)
├─ primer.tasks (router actor, task collection)
│   ├─ primer.tasks.task_28 (virtual actor)
│   └─ primer.tasks.task_29 (virtual actor)
├─ primer.knowledge (router actor, knowledge collection)
├─ primer.graph (effect actor, CozoDB adapter)
├─ primer.eventlog (effect actor, append-only log)
└─ primer.watcher (supervisor, file watchers)
    ├─ primer.watcher.src (file watcher actor)
    └─ primer.watcher.docs (file watcher actor)

Messages:
- primer.tasks.task_28 ← "update" { properties: {...} }
- primer.graph ← "query" { sql: "..." }
- primer.eventlog ← "append" { event: {...} }
```

**Phase 2: Fitness Function**

```
Requirements:
- CLI commands <100ms response (interactive)
- 100+ concurrent tasks (scale)
- Event log for auditing (compliance)
- File watching <1s latency (responsiveness)
- Browser updates <500ms (UX)
- Daemon restart without data loss (reliability)

Measurement:
- Current: CLI via HTTP adds 50-100ms latency
- Current: No event log (violates compliance)
- Current: File watching not implemented
- Current: Browser polling, not push (UX issue)
```

**Phase 3: Optimized Implementation**

```
CLI (local):
- Inline local actors (no network, <10ms)
- Direct CozoDB reads (fast queries)
- Event log for writes (audit)

Daemon (distributed):
- WebSocket for browser (push updates)
- Virtual actors for tasks (memory efficiency)
- Persistent actor for CozoDB (singleton)
- Supervisor for file watchers (restart on crash)

Hybrid (best of both):
- CLI connects to daemon if running (shared state)
- CLI falls back to local mode (resilience)
- System decides placement (adaptive)
```

### Implementation Priorities

1. **Design actor topology** (graph of actors and messages)
2. **Implement effect actors** (CozoDB adapter, event log, file watchers)
3. **Add event log** (append-only audit trail)
4. **Profile hot paths** (where is latency?)
5. **Optimize selectively** (fusion, inlining, direct access)
6. **Validate fitness** (does it meet requirements?)

### Long-Term Vision

**Design layer (always actors):**
- System design documents use actor model
- Architecture diagrams show actor topology
- Message protocols defined formally
- Supervision trees explicit

**Implementation layer (optimized):**
- Hot paths may bypass actors
- Cold paths keep actor abstraction
- Compilation may fuse actors
- Profiling guides optimization

**Validation layer (fitness function):**
- Does implementation meet design intent?
- Are optimizations justified by metrics?
- Can we trace behavior back to design?

**Actor model as design tool, not implementation religion.**

---

## Conclusion

### The Corrected Worldview

Actor model is **powerful for design**, not **mandatory for implementation**:

1. **Graph addressing** - Multiple access paths, not rigid hierarchy
2. **System-managed placement** - Runtime adaptation, not static config
3. **Format-agnostic serialization** - Match transport, not mandate JSON
4. **Pragmatic self-description** - Inspectable, not dogmatic homoiconicity
5. **Virtual actors optional** - One strategy, not the only strategy
6. **External boundaries** - Effect actors wrap non-actor systems
7. **Design vs implementation** - Model as actors, optimize for fitness

### The Design Pipeline

```
DESIGN (actor model)
  ↓ (model system as actors, messages, supervision)
DEFINE (fitness function)
  ↓ (deliverables, performance, constraints)
OPTIMIZE (implementation)
  ↓ (fusion, specialization, selective bypasses)
VALIDATE (against design)
  ↓ (does implementation meet fitness? preserve design intent?)
SUCCESS
```

### The Meta-Insight

**Constraints are design tools, not implementation laws.**

Actor model helps you **think clearly** about:
- Entity boundaries
- Message protocols
- Fault tolerance
- Dataflow

Then you **implement pragmatically** to meet:
- Performance requirements
- Resource constraints
- Deployment realities
- Legacy system integration

**The design (actor model) enables optimization analysis.**
**The fitness function justifies optimization decisions.**
**The validation ensures design intent preserved.**

### Final Thought

From ACTOR-WORLDVIEW.md:

> "I'm willing to constrain the way I think of the world... such that it is simple and EASY and deterministic."

**The corrected understanding:**

Constrain your **design thinking** (actors and messages).
Free your **implementation optimization** (meet fitness function).
Validate that optimizations **preserve design intent**.

**Actor model is a design telescope, not an implementation cage.**

---

## Appendix: Quick Reference

### Design Checklist

When designing a new feature:

1. **Model as actors**
   - [ ] What entities are involved?
   - [ ] What messages do they exchange?
   - [ ] What relationships exist (graph edges)?
   - [ ] What supervision tree handles failures?

2. **Define fitness function**
   - [ ] What are the deliverables?
   - [ ] What performance is required?
   - [ ] What constraints must be met?
   - [ ] How do we measure success?

3. **Design external boundaries**
   - [ ] What non-actor systems are involved?
   - [ ] What effect actors wrap external APIs?
   - [ ] What supervision handles external failures?

### Implementation Checklist

When implementing a design:

1. **Start pure** (no premature optimization)
   - [ ] Implement actor model directly
   - [ ] All communication via messages
   - [ ] No bypasses or shortcuts

2. **Profile** (find bottlenecks)
   - [ ] Where is latency?
   - [ ] Where is memory usage?
   - [ ] Where is message overhead?

3. **Optimize selectively** (justify with fitness)
   - [ ] Fusion: Merge actors on hot paths
   - [ ] Inlining: Remove actor dispatch
   - [ ] Direct access: Bypass abstraction
   - [ ] Document: Why optimization needed

4. **Validate** (preserve design intent)
   - [ ] Does implementation meet fitness function?
   - [ ] Can we trace behavior to design?
   - [ ] Are optimizations justified by metrics?

### Red Flags

**Design phase:**
- ❌ "This doesn't fit actor model" → Rethink entity boundaries
- ❌ "Too many actors" → Maybe you're over-granular
- ❌ "Circular dependencies" → Rethink message flow

**Implementation phase:**
- ❌ "Actor overhead too high" → Profile first, optimize later
- ❌ "Let's skip actor abstraction" → Justify with fitness function
- ❌ "This is too slow" → Define "slow" quantitatively

**Validation phase:**
- ❌ "Optimization broke design" → Reconsider optimization
- ❌ "Can't trace behavior" → Add instrumentation
- ❌ "Doesn't meet fitness" → Revisit implementation

### Green Lights

**Design phase:**
- ✅ "Clear actor boundaries" → Good design
- ✅ "Simple message protocols" → Good design
- ✅ "Obvious supervision tree" → Good design

**Implementation phase:**
- ✅ "Optimization justified by metrics" → Good tradeoff
- ✅ "Design intent preserved" → Good validation
- ✅ "Meets fitness function" → Good success

---

**End of Analysis**

**Status:** Complete - User Feedback Integrated
**Deliverable:** ACTOR_WORLDVIEW_ANALYSIS_V2.md
**Next Steps:** User review, apply to Primer design
