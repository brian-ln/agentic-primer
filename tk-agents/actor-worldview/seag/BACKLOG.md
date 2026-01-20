# SEAG Project Backlog

This document tracks the delta between our **Formal Models** and **Current Implementation**, as well as long-term research goals.

## I. The Consistency Gap (Modeled but Unimplemented)
*Identified by `bun run scripts/verify-consistency.ts`*

### 1. External Interface Actors (`ap/INTERFACES.model.lisp`)
- [ ] **ShellNode**: Execute shell commands and stream stdout/stderr directly to an actor log URI.
- [ ] **BrowserNode**: Manage a headless browser session as an actor; query the DOM via graph traversals.
- [ ] **TerminalNode**: Interactive TTY actor for REPL-in-REPL experiences.

### 2. Distribution & Routing (`ap/ROUTING.model.lisp`)
- [ ] **Transport**: Pluggable wire protocol actors (WebSocket, gRPC, Unix Sockets) to support cross-process messaging.
- [ ] **EntanglementManager**: Logic for managing "Shadow Actors" across physical boundaries (ensuring state consistency).

### 3. Core Graph Abstractions (`ap/ACTORS.model.lisp`)
- [ ] **InformationNode**: Formalize the base class for data-centric actors.
- [ ] **AlgorithmicNode**: Formalize the base class for compute/automata actors.
- [ ] **GraphEdge**: Implement edges as first-class actors to support edge-level metadata and behavior.

### 4. Log Intelligence (`ap/LOG.model.lisp`)
- [ ] **LogFolder**: A projector specialized in aggregating logs into state snapshots (Check-pointing).
- [ ] **SynthesisAgent**: Background agent that watches logs to derive higher-level insights or signals.

---

## II. Research & Technical Backlog
*Long-term architectural goals from `ap/IMPLEMENTATION_PLAN.md`*

### 1. Execution & Sandboxing
- [ ] **WASM Actor Hosting**: Capability to run `AlgorithmicNode` logic inside a restricted WASM sandbox for secure plugin support.

### 2. Optimization & Scaling
- [ ] **Actor Fusion Metrics**: Logic to detect high-latency actor hops and automatically "fuse" them into the same runtime.
- [ ] **Datalog Latency**: Benchmark and optimize CozoDB queries for graphs exceeding 10,000 nodes.
- [ ] **Rope-based Actor Graphs**: Support byte-offset and length updates in `FragmentNode` to avoid full-file re-writes for large documents.

### 3. Observability Refinements
- [ ] **OTel-style Spans**: Upgrade from "Structured Events" to full "Span Trees" (SpanID/ParentID) to support DAG-based waterfall visualizations.
- [ ] **Schema-Driven Mutation**: Replace regex-based mutator detection (`patch|update|set`) with a formal Protocol Registry.

---

## III. User Requests & UX
- [ ] **Trace Filtering**: Add the ability to filter the Trace Log UI by specific TraceIDs or Actor URIs.
- [ ] **Persistent Trace Log**: Add a `FileLogger` actor that subscribes to `seag://system/topic/trace` and writes to `data/trace.log`.
