# SEAG MVP Implementation Plan

This plan follows a phased approach to building the **Self-Evolving Actor Graph**. Each phase is designed to be executed via a **Harness** (a minimal test/bootstrap script) that validates the objective and subjective success criteria.

---

## Phase 1: The Actor Kernel (The "Big Bang")
**Goal:** Establish the fundamental message-passing runtime and local addressing.

### Task 1.1: Minimal Actor Runtime
- **Outcome:** A Bun/TS base class and registry that can spawn actors and route messages.
- **Objective Success Criteria:** 
    - `system.send(id, msg)` reaches the target actor.
    - Registry successfully maps `seag://` URIs to in-memory references.
- **Subjective Success Criteria:** 
    - The API feels "ergonomic" and requires minimal boilerplate.
- **Harness:** `tests/harness/phase1_kernel.ts`
- **Research:** Optimal UUID vs. Human-readable ID strategies for the local registry.

### Task 1.2: Root Supervisor & Self-Healing
- **Outcome:** The `RootSupervisor` from `ap/SYSTEM.model.lisp` is operational.
- **Objective Success Criteria:** 
    - Killing a "Permanent" actor triggers an automatic restart.
- **Subjective Success Criteria:** 
    - The system feels resilient; "Let it crash" doesn't lead to system death.

---

## Phase 2: The Event Stream & Projection (The "Memory")
**Goal:** Implement the "Truth" layer and the Datalog-based view.

### Task 2.1: EventLog & Write-Ahead Logic
- **Outcome:** Every `patch` message results in an append-only log entry.
- **Objective Success Criteria:** 
    - System can reboot and reconstruct state by replaying the `events.jsonl`.
- **Subjective Success Criteria:** 
    - Performance overhead of logging is unnoticeable for manual interactions.
- **Harness:** `tests/harness/phase2_persistence.ts`

### Task 2.2: Datalog Projection Engine
- **Outcome:** The `GraphProjector` updates a local Datalog index.
- **Objective Success Criteria:** 
    - Querying the graph via Datalog returns accurate results for `reachable` and `stale_node` rules.
- **Research:** Benchmarking CozoDB vs. simple in-memory Datalog for the MVP scale.

---

## Phase 3: The Virtual World (The "Spoon" manager)
**Goal:** Abstract the filesystem and web behind Effect Actors.

### Task 3.1: File & Web Effect Actors
- **Outcome:** `FileEffectActor` and `WebEffectActor` handle I/O.
- **Objective Success Criteria:** 
    - A domain actor can "read" a file by sending a message, without using `fs` directly.
    - Security `Capability Tokens` are checked before I/O occurs.
- **Subjective Success Criteria:** 
    - The "No Filesystem" abstraction feels transparent; I don't miss the native `fs` module.
- **Harness:** `tests/harness/phase3_world.ts`

### Task 3.2: Structural Destructuring (Shredder)
- **Outcome:** A `DocumentParser` that breaks Markdown/TS into graph nodes.
- **Objective Success Criteria:** 
    - Updating a single paragraph in the graph updates the correct byte-range in the physical file.

---

## Phase 4: The Agentic REPL (The "Thinking" Layer)
**Goal:** Connect a human user to the graph through an agent.

### Task 4.1: Gateway & UserProxy
- **Outcome:** A WebSocket gateway that bridges the Browser to the internal `UserProxy`.
- **Objective Success Criteria:** 
    - Messages sent from the browser console are captured in the `InteractionLog`.

### Task 4.2: The Brain (Inference Actor)
- **Outcome:** A `BrainAgent` that uses the Graph and Log to formulate a response.
- **Objective Success Criteria:** 
    - Agent can successfully perform a "Search -> Synthesize -> Reply" loop.
- **Subjective Success Criteria:** 
    - The agent's "Thinking" signal feels responsive and informative.
- **Harness:** `tests/harness/phase4_repl.ts`

---

## Research & Learning Backlog
- [ ] **WASM Actor Hosting:** Can we run the `AlgorithmicNode` in a restricted WASM sandbox?
- [ ] **Actor Fusion Metrics:** At what message volume does fusion become a net performance win?
- [ ] **Datalog Latency:** How does the query time scale with 10k+ nodes?

## Execution Strategy (The Harness)
Each phase will begin with a `harness.ts` file that acts as a **Behavioral Specification**. We will not consider a phase "Complete" until the harness passes all objective tests and the human (you) signs off on the subjective experience.
