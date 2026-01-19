# Actor Worldview: Design Thinking, Not Implementation Dogma

**Version:** 2.0 Consolidated
**Date:** 2026-01-18
**Status:** Authoritative - Consolidates all actor worldview research
**Purpose:** Actor model as design tool for system thinking

---

## Executive Summary

The actor model is a **design tool for thinking about systems**, not an implementation straitjacket.

**Core Insight:**
> Model systems as actors (design) → Define fitness function (goals) → Optimize implementation (pragmatically) → Validate against design (preserve intent)

**The actor model helps us think clearly about system structure, then we optimize pragmatically to meet real-world constraints.**

---

## The Complete Worldview: Seven Key Principles

### 1. Graph-Based Addressing

**Actors are nodes in a graph. Hierarchical paths are ONE index (like a primary key). Multiple traversal paths reach the same actor.**

- **Hierarchical paths** provide direct addressing: `primer.tasks.task_28`
- **Edge traversal** provides relationship-based access: "tasks blocked by task_28"
- **Router actors** act as graph indexes: `primer.tasks` routes to task collection
- **Queries** provide alternate discovery: "all tasks with priority P0"

**Don't force everything into a hierarchy.** Model relationships as graph edges, use hierarchical addressing for convenience, query the graph for complex access patterns.

### 2. System-Managed Placement

**System determines where actors run based on runtime conditions. Configuration is input, not law. Actors can be recovered elsewhere if unreachable.**

Static placement breaks in real systems:
- Server crashes → actor unavailable
- Network partition → can't reach server
- Load spike → server overloaded

System-managed placement enables:
- **Fault tolerance:** Actor unreachable → system spawns replacement elsewhere
- **Load balancing:** High load → system migrates actors
- **Adaptive optimization:** Message patterns change → system re-co-locates chatty actors

**Configuration provides hints and constraints, not commands.** Runtime system makes final placement decisions based on observed behavior and system health.

### 3. Format-Agnostic Serialization

**Messages must be serializable. Format should match target system's needs. Project between formats at boundaries.**

Different transports need different formats:
- In-memory: Plain JavaScript objects (no serialization)
- WebSocket: JSON (human-debuggable)
- High-performance IPC: MessagePack or Cap'n Proto (efficient binary)
- Event log: JSON-LD (semantic web compatibility)
- GPU communication: Binary buffers (zero-copy)

**Design with serializable messages**, but don't mandate a specific format. Each transport/persistence layer chooses "good enough" format.

The key constraint: **Messages are data, not code.** No functions, no closures, no symbols. Everything else is negotiable.

### 4. Pragmatic Self-Description

**Self-describing with minimal syntax rules. Systems don't have to use same representation. Good enough for the system that owns it.**

Homoiconicity (code-as-data, like LISP S-expressions) is powerful but can become dogma:
- Not every system needs full homoiconicity
- YAML, JSON, TOML are "good enough" for many use cases
- Over-engineering self-description adds complexity

**Make the system inspectable** (topology, messages, state are data), but don't go overboard with full homoiconicity unless you need metaprogramming capabilities.

**The key: Self-describing with minimal syntax rules.** Each subsystem uses representations "good enough" for its needs. Project between formats at boundaries.

### 5. Virtual Actors

**Virtual actors are ONE strategy. Message sender shouldn't care about lifecycle strategy. System manages virtualization per actor type.**

Actors have different lifecycle needs:
- **Virtual actors** (Orleans/Akka): Lazy creation, single instance per ID, passivation when idle
- **Persistent actors** (Erlang/OTP): Always running, never passivated, supervisor restarts if crashed
- **Transient actors**: Spawn per request, die after response, no identity
- **Pooled actors**: Worker pool, any worker handles message

**Sender location-transparent to lifecycle.** Caller just sends to address; system handles lifecycle strategy per actor type.

Correct constraint: **"Caller is location-transparent to lifecycle strategy,"** not "All actors are virtual."

### 6. External System Boundaries

**Actors interact with non-actor systems (files, databases, networks). Effect actors wrap non-deterministic external systems.**

Actors are deterministic (same input → same output). But real systems interact with:
- **File I/O:** Files can change, disappear, or be locked
- **Network:** Requests timeout, servers go down
- **Databases:** Queries fail, transactions conflict
- **Time:** `Date.now()` is different every call
- **Randomness:** `Math.random()` is non-deterministic

**Pattern: Functional Core / Imperative Shell**

Separate pure computation (deterministic) from side effects (non-deterministic):
- Pure actors are deterministic and testable
- Effect actors encapsulate non-determinism
- Supervision trees handle failures in effect actors

**Pattern: Adapter Actors**

Translate between actor model and external APIs:
- External systems wrapped in actor boundary
- Rest of system uses uniform message interface
- Easy to mock adapter for testing

### 7. Design vs Implementation

**THE CRITICAL SHIFT: Actor model is the DESIGN tool. Define fitness function, optimize implementation to meet it. May break actor purity if needed in implementation or optimization later on.**

The Design-to-Implementation Pipeline:

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

4. VALIDATE against design
   - Does implementation meet fitness function?
   - Can behavior be traced back to design?
   - Are optimizations justified by metrics?
```

**The key insight:** Actor model helps you **design clearly**, then you **implement pragmatically**.

**Constraints are design tools, not implementation laws.**

---

## The Design Framework

### Design → Fitness → Optimize → Validate

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
3. OPTIMIZE to meet function if and when necessary (clear tradeoffs, avoid premature optimization, YAGNI)
4. VALIDATE against design (clear verification)
```

---

## Application to Primer

### Design (Pure Actor Model)

```
primer (root actor)
├─ primer.tasks (router, task collection)
│   ├─ primer.tasks.task_28 (virtual actor)
│   └─ primer.tasks.task_29 (virtual actor)
├─ primer.knowledge (router, knowledge collection)
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

**Actor Types:**
- **Router actors** (`primer.tasks`, `primer.knowledge`): Manage collections, route to instances
- **Virtual actors** (`task_28`, `task_29`): Lazy creation, single instance per ID
- **Effect actors** (`primer.graph`, `primer.eventlog`): Wrap external systems
- **Supervisor** (`primer.watcher`): Restart child actors on failure

### Fitness Function

**Requirements:**
- CLI commands <100ms response (interactive)
- 100+ concurrent tasks (scale)
- Event log for auditing (compliance)
- File watching <1s latency (responsiveness)
- Browser updates <500ms (UX)
- Daemon restart without data loss (reliability)

### Optimized Implementation Strategies

**CLI (local mode):**
- Inline local actors (no network, <10ms)
- Direct CozoDB reads (fast queries)
- Event log for writes (audit)

**Daemon (distributed mode):**
- WebSocket for browser (push updates)
- Virtual actors for tasks (memory efficiency)
- Persistent actor for CozoDB (singleton)
- Supervisor for file watchers (restart on crash)

**Hybrid (best of both):**
- CLI connects to daemon if running (shared state)
- CLI falls back to local mode (resilience)
- System decides placement (adaptive)

---

## Design Patterns

### Virtual Actor Pattern (Orleans-style)

**On-demand instantiation. Actors created when first referenced.**

```typescript
// Virtual actor pattern: resolve creates instance
await system.send("primer.tasks.task_28", "update", {...});
// System: Does "task_28" exist? No → create. Yes → route to existing.
```

**Benefits:**
- No explicit lifecycle management
- Single instance per ID (deterministic)
- Memory efficient (passivate idle actors)

### Supervision Tree Pattern (Erlang-style)

**Hierarchical fault tolerance. Supervisors restart failed child actors.**

```typescript
// Supervisor for external system actors
actor ExternalSystemSupervisor:
  children: [FileSystemActor, NetworkActor, DatabaseActor]
  strategy: "restart-on-crash"

  receive "child-crashed":
    log error
    spawn new child actor
    update routing table
```

**Benefits:**
- "Let it crash" philosophy
- Fault isolation (failure doesn't cascade)
- Self-healing (automatic recovery)

### Effect Actor Pattern (Functional Core / Imperative Shell)

**Separate pure computation from side effects.**

```typescript
// Pure actor delegates to effect actor
actor TaskActor:
  receive "save":
    const data = JSON.stringify(this.state);  // Pure
    send data to FileSystemActor  // Delegate effect
    await response
    reply response

// Effect actor encapsulates I/O
actor FileSystemActor:
  receive "write":
    try {
      fs.writeFileSync(message.path, message.data);
      reply { success: true }
    } catch (error) {
      reply { success: false, error: error.message }
    }
```

**Benefits:**
- Pure actors are deterministic and testable
- Effect actors isolate non-determinism
- Clear separation of concerns

### Router Actor Pattern

**Index actor that manages collection and routes messages.**

```typescript
// Router actor manages collection
actor TaskCollectionActor:
  children: Map<string, Address>

  receive "create":
    const id = generateId();
    const address = spawnChild(id, message.payload);
    children.set(id, address);
    reply { id, address }

  receive "list":
    const tasks = [];
    for (const [id, address] of children) {
      const task = await send(address, "get");
      tasks.push(task);
    }
    reply tasks
```

**Benefits:**
- Collection-wide operations (list, search, filter)
- Lifecycle management for children
- Graph index for discovery

### Adapter Actor Pattern

**Wrap external APIs in actor interface.**

```typescript
// Adapter translates actor messages to external API
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
```

**Benefits:**
- External systems wrapped in actor boundary
- Uniform message interface
- Easy to mock for testing

---

## External System Boundaries

### Non-Deterministic External Systems

Actors should be deterministic, but real systems interact with:
- **Files:** Can change, disappear, or be locked
- **Networks:** Timeout, connection failures
- **Databases:** Query failures, transaction conflicts
- **Time:** `Date.now()` changes every call
- **Randomness:** `Math.random()` is non-deterministic

### Handling External Boundaries

**Strategy 1: Effect Actors**
- Wrap external system in actor boundary
- Isolate side effects from pure logic
- Supervision handles failures

**Strategy 2: Adapter Pattern**
- Translate external API to actor messages
- Provide mock adapters for testing
- Version adapters independently

**Strategy 3: Supervision Trees**
- Supervise effect actors with restart policies
- "Let it crash" for transient failures
- Log persistent failures for investigation

---

## Optimization Techniques

**Optimize When Necessary**. Make notes about potential optimizations in designs but defer until they are necessary. YAGNI principle applied. Assume that the system is maleable and changeable without high cost unless proven otherwise. You are writing the code or generating it from our models using generators. Human coding limits no longer apply.

### Actor Fusion

**Merge actors with high message rate to eliminate overhead.**

Before (pure actor):
```typescript
await system.send("actor1", "process", data);
await system.send("actor2", "validate", result);
// Two actor messages, 2x overhead
```

After (fused):
```typescript
await processAndValidate(data);
// Single function call, no actor overhead
// Justified by fitness function (latency requirement)
```

### Specialization

**Inline actor code for specific message types.**

Before (generic):
```typescript
actor.receive(message => {
  switch (message.type) {
    case "query": return handleQuery(message);
    case "update": return handleUpdate(message);
  }
});
```

After (specialized):
```typescript
// Hot path: inline query handler
function fastQuery(sql: string) {
  return cozoDB.query(sql);  // No actor dispatch
}

// Cold path: keep actor for updates (needs audit)
await system.send("primer.cozodb.write", "update", {...});
```

### Selective Bypasses

**Skip actor abstraction on critical paths if justified by fitness function.**

Example:
```typescript
// Fitness function requires <5ms p99 latency for reads
// Actor message passing adds ~2ms overhead
// Solution: Direct database access for reads

class TaskRepository {
  async getTask(id: string): Promise<Task> {
    return await cozoClient.query(`...`);
    // Direct call, breaks actor purity
    // Justified by latency requirement
  }
}

// But keep actor for writes (audit requirement)
await system.send("primer.cozodb.write", "update", {...});
```

**Rule:** Every optimization must be justified by fitness function and documented.

---

## The Meta-Insight

### Constraints as Design Tools

**Constraints are design tools, not implementation laws.**

Actor model constraints help you think:
- **Message passing** → identifies interfaces
- **Actor isolation** → understands independence
- **Supervision trees** → structures fault tolerance
- **Graph topology** → analyzes data flow

Then implementation may break constraints:
- **Fuse actors** → eliminate message overhead
- **Inline calls** → remove actor dispatch
- **Direct access** → bypass actor for critical path
- **Shared state** → optimize hot paths

**As long as fitness function is met and optimizations are documented.**

### The Design Telescope

**The actor model is a design telescope, not an implementation cage.**

Use actor model to:
- **See clearly** - Understand system structure
- **Reason formally** - Analyze message flow, dependencies, fault tolerance
- **Communicate design** - Share mental model with team
- **Identify optimization opportunities** - Profile message rates, find fusion candidates

Then optimize implementation:
- **Measure** - Profile to find bottlenecks
- **Optimize** - Apply techniques justified by fitness function
- **Validate** - Verify implementation meets design intent

### The Power of "Good Enough"

Most systems don't need:
- Perfect homoiconicity (YAML/JSON is good enough)
- Strict location transparency (local-first with fallback is good enough)
- Pure actor everywhere (actors for design, optimization for performance is good enough)

**Good enough beats perfect. Ship systems that work, not systems that are theoretically pure.**

---

## References

### Foundational Papers

- Hewitt, Carl (1973): "A Universal Modular ACTOR Formalism for Artificial Intelligence"
- Armstrong, Joe (2003): "Making reliable distributed systems in the presence of software errors" (Erlang/OTP)

### Actor Systems

- **Erlang/OTP**: Supervision trees, "let it crash" philosophy, process isolation
- **Orleans**: Virtual actors, grain directory, single-threaded grain execution
- **Akka**: Hierarchical addressing, typed actors, cluster sharding
- **Orleans.NET**: Location transparency, automatic state persistence

### Compilation & Optimization

- **StreamIt compiler** (MIT): Actor fusion, dataflow optimization
- **Halide**: Image processing pipelines, scheduling separation
- **TensorFlow XLA**: Dataflow compilation to hardware
- **Erlang BEAM JIT**: Runtime optimization of actor code

### Design Patterns

- **Functional Core / Imperative Shell**: Pure logic vs side effects
- **Effect actors**: Wrapper pattern for external systems
- **Supervision trees**: Hierarchical fault tolerance (Erlang)
- **Adapter pattern**: Impedance mismatch between systems

---

## Conclusion

### The Complete Worldview

Actor model is **powerful for design**, not **mandatory for implementation**:

1. **Graph addressing** - Multiple access paths, not rigid hierarchy
2. **System-managed placement** - Runtime adaptation, not static config
3. **Format-agnostic serialization** - Match transport, not mandate JSON
4. **Pragmatic self-description** - Inspectable, not dogmatic homoiconicity
5. **Virtual actors optional** - One strategy, not the only strategy
6. **External boundaries** - Effect actors wrap non-actor systems
7. **Design vs implementation** - Model as actors, optimize for fitness

### The Pragmatic Approach

```
DESIGN with actors → clear thinking
DEFINE fitness function → clear goals
OPTIMIZE implementation → clear tradeoffs
VALIDATE against design → clear verification
```

### The Actor Model Philosophy

**Use the actor model to:**
- Think clearly about system structure
- Reason formally about message flow
- Structure fault tolerance hierarchically
- Identify optimization opportunities

**Then implement pragmatically:**
- Profile to find bottlenecks
- Optimize where fitness function demands
- Document all optimizations
- Validate against design intent

**The actor model helps us design better systems, not force us into rigid implementations.**

---

**The actor model is a design telescope, not an implementation cage.**

