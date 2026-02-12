# Beads as System of Record for Design Decisions

**Date:** 2026-01-26
**Status:** Analysis

---

## Verdict: Yes, With Structure

Using beads as the system of record for design decisions makes sense for this project. The approach is already working in practice (see `agentic-primer-737` for capability security, `agentic-primer-64k` for actor runtime), and the reasons go deeper than convenience.

---

## Why This Works

### 1. Design Decisions Are Graph Problems

Design decisions in UGS are interconnected:

- **Capability security (737)** affects how agents get credentials
- **Actor runtime (64k)** affects how ExecutionContext propagates
- **Both** affect how Sessions call Models through Providers

Traditional markdown docs represent this as cross-references that rot. Beads represent it as explicit edges that can be queried:

```
agentic-primer-737 (capability security)
  └── blocks: agentic-primer-64k (actor runtime)
      └── blocks: session-model-integration

# Query: "What decisions does changing 737 affect?"
bd deps agentic-primer-737 --downstream
```

The graph structure matches the problem structure.

### 2. Agent-Accessible Rationale

UGS is building agents that work on tasks. When an agent needs to understand constraints:

**Without beads:** Agent must read scattered markdown, hope docs are current, parse prose.

**With beads:** Agent queries the design graph:

```bash
# Agent working on session implementation
bd show agentic-primer-64k
# Returns: structured data with rationale, constraints, dependencies

# Agent checks if proposed change violates constraints
bd list --label security --status planned
# Returns: all security decisions that might affect approach
```

Agents can propose design changes as new beads, creating traceable evolution.

### 3. State-Aware Documentation

Beads have status (planned, in-progress, implemented, superseded). Documents do not.

Consider the harness reusability epic (`agentic-primer-rin`):

```
rin.1 (HARNESS_GUIDE.md)     - status: closed
rin.2 (HARNESS_TEMPLATE.md)  - status: closed
rin.3 (scripts/harness/)     - status: in-progress
rin.4 (examples/)            - status: blocked by rin.3
```

The bead graph shows what is done, what is blocked, and why. A markdown file would require manual status tracking that inevitably drifts from reality.

### 4. Traceability From Code to Design

The pattern emerging in this codebase:

```
git commit → references bead ID
  → bead has rationale, constraints, dependencies
    → dependencies link to other design decisions
      → full context reconstructable from ID
```

Example from `PROGRAM_TYPES_ARCHITECTURE.md`:

```
│                    Actor Runtime Layer                          │
│  (Future: agentic-primer-64k)                                   │
```

The bead ID in the architecture doc creates a queryable link. Six months from now, anyone (human or agent) can run `bd show agentic-primer-64k` to understand why the actor runtime is designed as planned.

### 5. The Self-Reference Insight

This is the meaningful meta-observation:

UGS is a system for storing, linking, and querying interconnected information as a graph. Beads store design decisions as interconnected issues with links and dependencies. Beads are a prototype of what UGS should do for design artifacts.

The experience of using beads to design UGS directly informs what UGS should provide. Pain points in beads become features in UGS. This is productive dogfooding.

Eventually, UGS should be able to subsume beads. Design decisions become `programType: "information"` nodes with `infoType: "design-decision"`. The graph that describes the system IS the system.

---

## Risks and Mitigations

### Risk 1: Query Overhead

**Problem:** Beads require CLI queries; markdown is `cat filename`.

**Mitigation:** This is acceptable friction. The overhead pays for itself when you need to answer "what depends on this?" or "what is the status of all security decisions?" - questions markdown cannot answer.

### Risk 2: Tool Lock-In

**Problem:** Beads depend on the `bd` tool. Markdown is universal.

**Mitigation:** Beads use a SQLite database with JSONL backup. Data is recoverable. More importantly, if beads stop working, the underlying design information still exists - just with degraded queryability.

### Risk 3: Over-Engineering Simple Decisions

**Problem:** Not every decision needs a bead.

**Mitigation:** Use beads for decisions that:
- Have dependencies on other decisions
- Affect multiple components
- Need status tracking (planned/implemented/superseded)
- Should be agent-queryable

Keep small, local decisions in code comments or focused docs.

---

## Recommended Structure

### Labels (Taxonomy)

| Label | Purpose |
|-------|---------|
| `architecture` | System-level design decisions |
| `security` | Capability, auth, secrets |
| `performance` | Scale, latency, resource usage |
| `api` | Interface contracts |
| `agent` | Agent behavior, harness, coordination |
| `storage` | Persistence, graph structure |
| `implementation` | Concrete work items |
| `research` | Exploratory analysis |

### Bead Types

| Type | Example | Notes |
|------|---------|-------|
| **Epic** | `agentic-primer-rin` | Multi-task initiatives |
| **Decision** | `agentic-primer-737` | Architectural choices with rationale |
| **Task** | `agentic-primer-rin.3` | Concrete work items |
| **Research** | `agentic-primer-fmp` | Analysis before decision |

### Conventions

1. **Reference beads in code:** Put bead IDs in relevant source files and docs
2. **Link dependencies:** Use `blocks`/`blocked-by` for design constraints
3. **Close with rationale:** When superseding a decision, record why
4. **Label consistently:** Use labels from the taxonomy above

### Example Design Bead

```bash
bd add "Actor runtime: AsyncLocalStorage for ExecutionContext propagation" \
  --label architecture,security \
  --blocks agentic-primer-session-impl \
  --depends-on agentic-primer-737

# In the bead description:
# - WHY this approach (not alternatives)
# - WHAT constraints it creates
# - WHERE it affects the codebase
```

---

## Conclusion

Beads as the system of record for design decisions is not just organizational convenience. It aligns the structure of design documentation with the structure of design problems (graphs of interconnected decisions). It makes design rationale agent-queryable. And it provides a working prototype of what UGS itself should eventually do for knowledge management.

The key insight: **The tool for describing the system should eventually BE the system.**

Start capturing architectural decisions in beads now. When UGS matures, migrate them to native UGS `information` nodes. The migration will be straightforward because the data model is already graph-shaped.
