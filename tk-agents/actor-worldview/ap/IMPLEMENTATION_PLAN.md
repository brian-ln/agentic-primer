# SEAG MVP Implementation Plan

This plan follows a phased approach to building the **Self-Evolving Actor Graph**. Each phase is designed to be executed via a **Harness** (a minimal test/bootstrap script) that validates the objective and subjective success criteria.

---

## Phase 1: The Actor Kernel (The "Big Bang")
**Goal:** Establish the fundamental message-passing runtime and local addressing.

### Task 1.1: Minimal Actor Runtime [COMPLETED]
- **Outcome:** A Bun/TS base class and registry that can spawn actors and route messages.
- **Refinement:** Integrated **SERDE** (via `structuredClone` for memory isolation) and **Transports** (pluggable envelopes for boundaries).
- **Objective Success Criteria:** 
    - ✅ `system.send(id, msg)` reaches the target actor.
    - ✅ Registry successfully maps `seag://` URIs to in-memory references.
    - ✅ **SERDE Isolation:** Mutating the sender's object does not affect the receiver's copy.
- **Subjective Success Criteria:** 
    - ✅ The API feels "ergonomic" and requires minimal boilerplate.
- **Harness:** `tests/harness/phase1_kernel.test.ts`
- **Research:** Investigated `structuredClone` vs. shared memory for local optimization.

### Task 1.2: Root Supervisor & Self-Healing [COMPLETED]
- **Outcome:** The `RootSupervisor` from `ap/SYSTEM.model.lisp` is operational.
- **Objective Success Criteria:** 
    - ✅ Killing a "Permanent" actor triggers an automatic restart.
    - ✅ **Fault Isolation:** Errors in a child actor's `receive` are caught and reported without crashing the event loop.
- **Subjective Success Criteria:** 
    - ✅ The system feels resilient; "Let it crash" doesn't lead to system death.
- **Harness:** `tests/harness/phase1_kernel.test.ts`

### Task 1.3: Observability & Loop Avoidance [COMPLETED]
- **Outcome:** Messages carry `trace_id` and `hop_count`.
- **Objective Success Criteria:** 
    - ✅ Circular messages are dropped after 100 hops.
    - ✅ A `trace_id` can be used to follow a message across 3+ actors.
- **Harness:** `tests/harness/phase1_stability.test.ts`

---

## Phase 2: The Event Stream & Projection (The "Memory") [COMPLETED]
**Goal:** Implement the "Truth" layer and the Datalog-based view.

### Task 2.1: EventLog & Write-Ahead Logic [COMPLETED]
- **Outcome:** Every `patch` message results in an append-only log entry.
- **Objective Success Criteria:** 
    - ✅ System can reboot and reconstruct state by replaying the `events.jsonl`.
    - ✅ **Automatic Detection:** Mutators (patch/update/set) are logged automatically via regex pattern.
- **Subjective Success Criteria:** 
    - ✅ Performance overhead of logging is unnoticeable for manual interactions.
- **Harness:** `tests/harness/phase2_persistence.test.ts`

### Task 2.2: Graph Projection & Queryability [COMPLETED]
- **Outcome:** A `GraphProjector` actor that maintains a queryable view of the graph.
- **Approach:** Implemented a "Minimal Viable Index" using TS Maps/Sets.
- **Objective Success Criteria:** 
    - ✅ Queries for `linked(A, B)` and `reachable(A, B)` return correct results.
    - ✅ Projector updates the index in real-time as events arrive.
- **Subjective Success Criteria:** 
    - ✅ The query interface feels sufficiently "declarative".
- **Harness:** `tests/harness/phase2_projection.test.ts`

---

## Phase 3: The Virtual World (The "Spoon" manager) [COMPLETED]
**Goal:** Abstract the filesystem and web behind Effect Actors.

### Task 3.1: File & Web Effect Actors [COMPLETED]
- **Outcome:** `FileEffectActor` and `WebEffectActor` handle I/O.
- **Objective Success Criteria:** 
    - ✅ A domain actor can "read" a file by sending a message, without using `fs` directly.
    - ✅ **Security Hardening:** Security `Capability Tokens` are checked before I/O occurs.
- **Subjective Success Criteria:** 
    - ✅ The "No Filesystem" abstraction feels transparent.
- **Harness:** `tests/harness/phase3_world.test.ts`

### Task 3.2: Structural Destructuring (Shredder) [COMPLETED]
- **Outcome:** A `DocumentParser` that breaks Markdown/TS into graph nodes.
- **Objective Success Criteria:** 
    - ✅ **Decomposition:** A single file is represented as multiple `FragmentNode` actors.
    - ✅ **Live Linking:** The fragments are linked to the parent document in the graph.
    - ✅ **Back-Propagation:** Updating a fragment actor automatically re-assembles the document and updates the physical file.
- **Subjective Success Criteria:** 
    - ✅ Granular observation: An actor can watch a single section of a document.
- **Harness:** `tests/harness/phase3_shredder.test.ts`

### Task 3.3: Two-Way Sync (The Watcher) [COMPLETED]
- **Outcome:** External disk changes are reflected in the graph.
- **Objective Success Criteria:** 
    - ✅ Manually editing the physical file triggers an update in the `FragmentNode` actors.
    - ✅ **Reconciliation:** Idempotent spawns and no-op checks ensure minimal message propagation.
- **Subjective Success Criteria:** 
    - ✅ The graph feels "alive" and synchronized with the environment.
- **Harness:** `tests/harness/phase3_watcher.test.ts`

---

## Phase 4: The Agentic REPL (The "Thinking" Layer) [COMPLETED]
**Goal:** Connect a human user to the graph through an agent.

### Task 4.1: Gateway & UserProxy [COMPLETED]
- **Outcome:** A WebSocket gateway that bridges the Browser to the internal `UserProxy`.
- **Objective Success Criteria:** 
    - ✅ Messages sent from the browser console are captured in the `InteractionLog`.
    - ✅ **Full Causality:** WebSocket -> Gateway -> UserProxy -> Log chain verified.
- **Harness:** `tests/harness/phase4_gateway.test.ts`

### Task 4.2: The Brain (Inference Actor) [COMPLETED]
- **Outcome:** A `BrainAgent` that uses the Graph and Log to formulate a response.
- **Objective Success Criteria:** 
    - ✅ **Intent Parsing:** Agent handles `mount`, `explore`, and `set` commands.
    - ✅ **Full Loop:** Successfully updated a physical file via a REPL interaction with no native `fs` calls.
    - ✅ **Discovery:** Successfully queried the `GraphProjector` for reachable nodes.
- **Subjective Success Criteria:** 
    - ✅ The agent's "Thinking" signal feels responsive and informative.
- **Harness:** `tests/harness/phase4_brain.test.ts`

---

## 🚀 SEAG MVP STATUS: DELIVERED
All core architectural layers (Kernel, Memory, World, Thinking) are verified and operational.


## Research & Learning Backlog
- [ ] **WASM Actor Hosting:** Can we run the `AlgorithmicNode` in a restricted WASM sandbox?
- [ ] **Actor Fusion Metrics:** At what message volume does fusion become a net performance win?
- [ ] **Datalog Latency:** How does the query time scale with 10k+ nodes?
- [ ] **Advanced Mutation Detection:** Evaluate Protocol Registry vs. Schema-Driven detection to replace regex-based mutator detection.
- [ ] **Rope-based Actor Graphs:** Research using byte offsets and lengths in FragmentNodes to support random-access file updates (avoiding full-file re-writes).


## Execution Strategy (The Harness)
Each phase will begin with a `harness.ts` file that acts as a **Behavioral Specification**. We will not consider a phase "Complete" until the harness passes all objective tests and the human (you) signs off on the subjective experience.
