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

## Phase 3: The Virtual World (The "Spoon" manager)
...
## Research & Learning Backlog
- [ ] **WASM Actor Hosting:** Can we run the `AlgorithmicNode` in a restricted WASM sandbox?
- [ ] **Actor Fusion Metrics:** At what message volume does fusion become a net performance win?
- [ ] **Datalog Latency:** How does the query time scale with 10k+ nodes?
- [ ] **Advanced Mutation Detection:** Evaluate Protocol Registry vs. Schema-Driven detection to replace regex-based mutator detection.


## Execution Strategy (The Harness)
Each phase will begin with a `harness.ts` file that acts as a **Behavioral Specification**. We will not consider a phase "Complete" until the harness passes all objective tests and the human (you) signs off on the subjective experience.
