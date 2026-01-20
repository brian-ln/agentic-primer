# Actor Worldview - Design Thinking Framework

**Status:** Complete - V2.0 Consolidated
**Date:** 2026-01-19
**Purpose:** Actor model as design tool for system thinking, not implementation dogma

---

## Overview

This folder contains the **consolidated actor worldview framework**, integrating all actor model research, user corrections, and practical guidance. The framework positions the actor model as a **design tool** rather than implementation straitjacket.

**Core Philosophy:**
> **Model systems as actors (design) → Define fitness function (goals) → Optimize implementation (pragmatically) → Validate against design (preserve intent)**

---

## Key Documents

### 🎯 Start Here: [WORLDVIEW.md](./WORLDVIEW.md)
**The authoritative actor worldview document**

Consolidated philosophy integrating:
- Seven key principles (addressing, placement, serialization, etc.)
- Design → Fitness → Optimize → Validate framework
- Application to Primer (with actor hierarchy)
- Design patterns and optimization techniques
- External system boundaries

**Read this first** for complete understanding of the actor model as a design tool.

### 📚 Navigation: [INDEX.md](./INDEX.md)
**Complete file index and reading paths**

Comprehensive guide to all documents:
- Quick navigation by topic
- Organized by category (guides, architectures, patterns, migration, topics)
- Reading paths (quick start, implementation, deep dive, historical)
- Document statistics and maintenance guide

**Use this** to find specific documents or plan your reading path.

### 📖 Organization
```
actor-worldview/
├── WORLDVIEW.md           # Consolidated philosophy (START HERE)
├── README.md              # This file (navigation guide)
├── INDEX.md               # Complete file index
│
├── seag/                  # SEAG MVP Implementation (Short Path)
├── ap/                    # Formal Models & Specs for SEAG
│
├── guides/                # How-to guides
│   ├── ADDRESSING_CONVENTIONS.md
│   └── IMPLEMENTATION_GUIDE.md
│
├── architectures/         # System architectures
│   ├── PURE_ACTOR_MODEL.md
│   ├── CLI_ACTOR_ARCHITECTURE.md
│   └── HIERARCHY_DESIGN.md
│
├── patterns/              # Design patterns
│   ├── ENTANGLED_ACTORS.md
│   └── PATTERN_CATALOG.md
│
├── migration/             # Migration guides
│   ├── MIGRATION_GUIDE.md
│   └── CLI_TRANSFORMATION.md
│
├── topics/                # Specialized topics
│   ├── COMPILATION_RESEARCH.md
│   └── deliverable-storage/
│
├── subsystems/            # Subsystem-specific worldviews
│   └── bg-workflow/       # Already consolidated
│
└── archive/               # Historical documents (timestamped)
```

---

## The Actor Worldview: Seven Key Principles

### 1. Graph-Based Addressing
Hierarchical paths are ONE index. Graph traversal provides alternate access.

### 2. System-Managed Placement
System determines where actors run. Configuration provides hints, not commands.

### 3. Format-Agnostic Serialization
Messages must be serializable. Format matches transport needs.

### 4. Pragmatic Self-Description
Self-describing with minimal syntax. Not dogmatic homoiconicity.

### 5. Virtual Actors Optional
One lifecycle strategy among many. Sender doesn't care about lifecycle.

### 6. External System Boundaries
Effect actors wrap non-actor systems (files, databases, networks).

### 7. Design vs Implementation
**THE CRITICAL SHIFT:** Model as actors, optimize for fitness function.

**See [WORLDVIEW.md](./WORLDVIEW.md) for complete details on each principle.**

---

## The Design Framework

```
┌─────────────────────────────────────────────────────────┐
│         DESIGN (Actor Model - Clear Thinking)           │
│  - Model entities as actors                            │
│  - Define messages and protocols                       │
│  - Draw actor graph (relationships, supervision)       │
└────────────────────┬────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────┐
│       DEFINE (Fitness Function - Clear Goals)           │
│  - Deliverables: What must system do?                  │
│  - Performance: Latency, throughput, resources         │
│  - Success criteria: How do we know it works?          │
└────────────────────┬────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────┐
│     OPTIMIZE (Implementation - Clear Tradeoffs)         │
│  - Profile: Where are bottlenecks?                     │
│  - Fuse: Merge actors with high message rate           │
│  - Specialize: Inline actors on critical path          │
│  - Bypass: Skip actor abstraction if justified         │
└────────────────────┬────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────┐
│      VALIDATE (Against Design - Clear Verification)     │
│  - Does implementation meet fitness function?          │
│  - Can behavior be traced back to design?              │
│  - Are optimizations justified by metrics?             │
└─────────────────────────────────────────────────────────┘
```

---

## Application to Primer

### Primer Actor Hierarchy

```
primer (root)
├─ primer.tasks (router, task collection)
│   ├─ task_28 (virtual actor)
│   └─ task_29 (virtual actor)
├─ primer.knowledge (router, knowledge collection)
├─ primer.graph (effect actor, CozoDB adapter)
├─ primer.eventlog (effect actor, append-only log)
└─ primer.watcher (supervisor, file watchers)
```

**Actor Types:**
- **Router actors**: Manage collections, route to instances
- **Virtual actors**: Lazy creation, single instance per ID
- **Effect actors**: Wrap external systems (CozoDB, files)
- **Supervisors**: Restart failed children

### Fitness Function Example

**Requirements:**
- CLI commands <100ms (interactive)
- 100+ concurrent tasks (scale)
- Event log for auditing (compliance)
- File watching <1s (responsiveness)
- Browser updates <500ms (UX)

**Implementation Strategies:**
- **Local mode**: Inline actors, direct reads, event log for writes
- **Daemon mode**: WebSocket push, virtual actors, persistent CozoDB
- **Hybrid**: System decides placement adaptively

**See [WORLDVIEW.md#application-to-primer](./WORLDVIEW.md#application-to-primer) for complete details.**

---

## Reading Order

### Quick Start (30 minutes)
1. **This README** - Understand structure
2. **[WORLDVIEW.md](./WORLDVIEW.md)** - Read Executive Summary + Seven Principles
3. **Application to Primer** section

**Outcome:** Grasp core philosophy and Primer application

### Implementation (2 hours)
1. **[guides/IMPLEMENTATION_GUIDE.md](./guides/IMPLEMENTATION_GUIDE.md)** - Learn patterns
2. **[guides/ADDRESSING_CONVENTIONS.md](./guides/ADDRESSING_CONVENTIONS.md)** - Naming rules
3. **[patterns/](./patterns/)** - Study specific patterns
4. **[architectures/CLI_ACTOR_ARCHITECTURE.md](./architectures/CLI_ACTOR_ARCHITECTURE.md)** - Example

**Outcome:** Ready to implement actors

### Deep Dive (4-6 hours)
1. Complete Quick Start and Implementation paths
2. **[WORLDVIEW.md](./WORLDVIEW.md)** - Read complete document
3. **[architectures/PURE_ACTOR_MODEL.md](./architectures/PURE_ACTOR_MODEL.md)** - Full architecture
4. **[topics/COMPILATION_RESEARCH.md](./topics/COMPILATION_RESEARCH.md)** - Optimization
5. **[migration/MIGRATION_GUIDE.md](./migration/MIGRATION_GUIDE.md)** - Migration strategy

**Outcome:** Complete mastery

**See [INDEX.md](./INDEX.md) for more reading paths.**

---

## Design Patterns

### Virtual Actor Pattern (Orleans-style)
On-demand instantiation. Actors created when first referenced.

### Supervision Tree Pattern (Erlang-style)
Hierarchical fault tolerance. Supervisors restart failed children.

### Effect Actor Pattern (Functional Core / Imperative Shell)
Separate pure computation from side effects.

### Router Actor Pattern
Index actor that manages collection and routes messages.

### Adapter Actor Pattern
Wrap external APIs in actor interface.

**See [patterns/](./patterns/) for complete catalog.**

---

## Optimization Techniques

### Actor Fusion
Merge actors with high message rate to eliminate overhead.

### Specialization
Inline actor code for specific message types.

### Selective Bypasses
Skip actor abstraction on critical paths if justified by fitness function.

**Rule:** Every optimization must be justified by fitness function and documented.

**See [topics/COMPILATION_RESEARCH.md](./topics/COMPILATION_RESEARCH.md) for research.**

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
- **Reason formally** - Analyze message flow, dependencies
- **Communicate design** - Share mental model
- **Identify optimization opportunities** - Profile, find fusion candidates

Then optimize implementation:
- **Measure** - Profile to find bottlenecks
- **Optimize** - Apply techniques justified by fitness
- **Validate** - Verify implementation meets design intent

---

## What Changed in V2.0 Consolidation

### Before (Scattered)
- 26 files in root directory
- Version conflicts (V1, V2, backups)
- Multiple overlapping documents
- No clear organization
- Duplicate content

### After (Organized)
- Single authoritative WORLDVIEW.md
- Organized into categories (guides/, architectures/, patterns/, etc.)
- Historical documents archived with timestamps
- Clear navigation (INDEX.md)
- No duplicates or version conflicts

### Changes from V1 to V2 Worldview

**V1 Analysis:** Constraint-based (hierarchical addressing, JSON serialization, everything is actor)

**V2 Analysis (Current):** Design-based with seven corrections:
1. Graph addressing (not just hierarchical)
2. System-managed placement (not static config)
3. Format-agnostic serialization (not JSON-only)
4. Pragmatic self-description (not dogmatic homoiconicity)
5. Virtual actors optional (not mandatory)
6. External boundaries (effect actors, supervision)
7. Design vs implementation (optimize for fitness)

**Mindset Shift:**
- **V1:** Actor model is implementation pattern
- **V2:** Actor model is design tool; implementation optimizes to meet fitness function

**See [archive/](./archive/) for historical documents.**

## Backlog & Future Goals
- [ ] **Advanced Capability Management**: Design a system for extending authorization and context across actor interactions (e.g., delegated capabilities).
- [ ] **Kafka-style Durable Topics**: Implement replayable streams by backing TopicNodes with an append-only log.
- [ ] **Actor Fusion Engine**: Automatically optimize topology by fusing high-frequency actor pairs.
- [ ] **Dynamic Transport Layer**: Support for LibP2P or WebRTC actor communication.

## References

### Foundational
- Hewitt Actor Model (1973)
- Erlang/OTP supervision trees
- Orleans virtual actors
- Akka clustering and sharding

### Compilation
- StreamIt compiler (MIT)
- Halide image processing compiler
- TensorFlow XLA dataflow compilation
- Erlang BEAM JIT

### Patterns
- Functional Core / Imperative Shell
- Effect actors (wrapper pattern)
- Supervision trees (let-it-crash)
- Adapter pattern (impedance mismatch)

**See [WORLDVIEW.md#references](./WORLDVIEW.md#references) for complete list.**

---

## Support

**Need help?**
- **Philosophy questions:** See [WORLDVIEW.md](./WORLDVIEW.md)
- **Implementation questions:** See [guides/](./guides/)
- **Specific subsystems:** See [architectures/](./architectures/) or [topics/](./topics/)
- **Migration questions:** See [migration/](./migration/)
- **Historical context:** See [archive/](./archive/)
- **Can't find something?:** Check [INDEX.md](./INDEX.md)

---

**The actor model is a design telescope, not an implementation cage.**

