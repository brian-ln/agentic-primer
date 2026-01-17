# Steve Yegge's Formulas/Molecules Implementation Analysis

## Executive Summary

Steve Yegge has built a comprehensive system for managing agent workflows through his Beads issue tracker and Gas Town orchestrator. The core innovation is a three-phase chemistry metaphor (Proto/Mol/Wisp) that enables reusable workflow templates (formulas) to be instantiated into persistent work graphs (molecules) that survive agent crashes, compaction, and multi-session work.

**Key Innovation**: Formulas are TOML-defined workflow templates that compile into protomolecules (reusable templates), which can be instantiated into molecules (persistent work) or wisps (ephemeral work). This enables compositional workflow design with macro expansion, variable substitution, and runtime bonding.

## Sources

This analysis is based on:
- [Beads GitHub Repository](https://github.com/steveyegge/beads)
- [Gas Town GitHub Repository](https://github.com/steveyegge/gastown)
- [Beads Molecules Documentation](https://github.com/steveyegge/beads/blob/main/docs/MOLECULES.md)
- [Beads Agent Instructions](https://github.com/steveyegge/beads/blob/main/AGENT_INSTRUCTIONS.md)
- [Gas Town Release Post (Welcome to Gas Town)](https://steve-yegge.medium.com/welcome-to-gas-town-4f25ee16dd04) - (Medium paywall prevented full access)
- Web search results about Beads and Gas Town from January 2026

## Core Concepts

### 1. The Chemistry Metaphor: Proto, Mol, and Wisp

Yegge uses a three-phase model for workflow lifecycle:

| Phase | Name | Storage | Git Synced | Purpose |
|-------|------|---------|------------|---------|
| **Solid** | Proto (Protomolecule) | `.beads/` | Yes | Frozen reusable template |
| **Liquid** | Mol (Molecule) | `.beads/` | Yes | Persistent active work with audit trail |
| **Vapor** | Wisp | `.beads/` | No | Ephemeral operations (patrols, one-shots) |

**Design Rationale**: This separation enables:
- **Reusability**: Protos are templates that can be instantiated multiple times
- **Persistence**: Mols survive agent crashes and can be resumed across sessions
- **Efficiency**: Wisps allow throwaway work without polluting git history

### 2. Formulas: TOML-Defined Workflows

Formulas are the **source form** for workflows, stored as TOML files in `.beads/formulas/`. They represent compile-time macro definitions that get "cooked" into protomolecules.

**Key Quote from Search Results**:
> "Formulas are TOML-defined, reusable workflow templates stored in `.beads/formulas/`. They represent predefined, repeatable processes that can be executed with parameterized variables."

**Formula Structure**:

```toml
# Metadata
description = "Release process for version X"
formula = "release"
version = "2.0"

# Variables section - parameterized inputs
[vars.version]
description = "The version to release"
required = true

# Steps array - sequential tasks with dependencies
[[steps]]
id = "bump-version"
title = "Bump version number"
description = "Update version to {{version}}"

[[steps]]
id = "run-tests"
title = "Run test suite"
description = "Execute full test suite"
needs = ["bump-version"]  # Dependency on previous step

[[steps]]
id = "build"
title = "Build artifacts"
description = "Build release artifacts"
needs = ["run-tests"]

[[steps]]
id = "publish"
title = "Publish release"
description = "Publish version {{version}}"
needs = ["build"]
```

**Key Features**:
- **Variable interpolation**: `{{version}}` syntax for template substitution
- **Dependency chains**: `needs` array controls execution order
- **Gate support**: `waits_for` field for fan-out/fan-in patterns
- **Conditional execution**: `conditional-blocks` for error-driven paths

### 3. Molecules: Work Graph Architecture

Molecules are "epics with workflow semantics" - parent issues containing child tasks connected by dependency relationships.

**Execution Model**:
- **Default parallelism**: All work executes concurrently unless explicitly blocked
- **Ready work**: Tasks with no open blocking dependencies
- **Blocked work**: Tasks waiting on unclosed dependencies
- Agents claim ready items, execute them, close them
- Workflow completes when all children close

**Design Philosophy** (from MOLECULES.md):
> "The system implements 'default parallelism' – work executes concurrently unless dependencies enforce sequence. This optimizes agent efficiency while maintaining explicit control over execution order through declarative dependency graphs rather than implicit ordering assumptions."

### 4. Dependency Types

Beads supports multiple blocking relationships:

| Type | Purpose | Blocking? |
|------|---------|-----------|
| `blocks` | Sequential constraint (B waits for A) | Yes |
| `parent-child` | Hierarchical inheritance | Yes (cascades) |
| `conditional-blocks` | Error-driven paths (B runs only if A fails) | Yes |
| `waits-for` | Fanout gates (B waits for all of A's children) | Yes |
| `related` | Organizational link | No |
| `discovered-from` | Audit trail | No |
| `replies-to` | Conversation threading | No |

**Critical Insight**: The `waits-for` dependency type enables **fan-out with gate** patterns - multiple children run in parallel, then a parent waits for all to complete before proceeding.

### 5. Bonding: Composing Work Graphs

Bonding is the operation that connects work graphs together at runtime.

**Definition from MOLECULES.md**:
> "Bond = create a dependency between two work graphs."

**Bonding Commands**:
```bash
bd mol bond A B                    # B depends on A (sequential)
bd mol bond A B --type parallel    # Organizational link, no blocking
bd mol bond A B --type conditional # B runs only if A fails
```

**Bonding Operands**:
- **Epic + Epic**: Creates dependency edge between existing work
- **Proto + Epic**: Spawns proto as new issues, attaches to epic
- **Proto + Proto**: Creates compound reusable template

**Key Innovation**: This enables **compound execution** where agents can traverse multiple bonded work graphs as a single logical unit across multiple sessions. This allows autonomous workflows spanning days.

**Pattern: Christmas Ornament** (from MOLECULES.md):
> "Discover work at runtime and bond arm molecules dynamically based on survey results."

This pattern enables dynamic workflow generation where the structure isn't known until execution time.

### 6. The Formula → Proto → Mol Pipeline

The architecture layers as:

```
Formulas (TOML files, compile-time macros)
  ↓ (bd cook)
Protos (template issues, frozen graphs)
  ↓ (bd mol pour)
Molecules (active work, persistent)
  ↓ (execution)
Epics (parent-child, dependencies)
  ↓ (storage)
Issues (JSONL, git-backed)
```

**Phase Transitions**:
- `bd cook <formula>`: Execute formula directly (ephemeral)
- `bd mol pour <proto>`: Template → persistent mol instance
- `bd mol wisp <proto>`: Template → ephemeral wisp instance
- `bd mol squash <id>`: Compress completed work to digest record
- `bd mol burn <id>`: Discard wisp without archival

**Design Insight**: Most users work only with the bottom two layers (Epics/Issues). Protos and formulas provide reusability for complex patterns - an optional advanced feature.

## Implementation Details

### Data Model: JSONL + Git

**Storage**: Issues stored as JSON Lines (JSONL) in `.beads/` directory, version controlled by git.

**Why JSONL?**:
- Line-based format reduces git merge conflicts
- Each issue is a single line
- Append-only friendly
- Easy to parse and process

**Architecture** (from ADVANCED.md):
> "Three-layer design—core beads storage layer handles logic; per-project daemons manage auto-sync; MCP server provides editor integration."

**Dual-Layer**:
- SQLite local cache for performance
- Background daemon for async synchronization
- Git repository as persistent truth

### Task ID Structure

**Hash-based IDs**: `bd-a1b2` prevent merge collisions
**Hierarchical support**:
- `bd-a3f8` (Epic)
- `bd-a3f8.1` (Task)
- `bd-a3f8.1.1` (Sub-task)

### Agent-Ready Query Pattern

```bash
bd ready --json
```

Returns tasks with zero open blockers, enabling agents to autonomously select next work items.

**Key Pattern**: This is the primary agent interface - agents don't need to understand the entire graph, they just query for ready work, execute it, and close it.

### Formulas and Macro Expansion

From search results:
> "Formulas (JSON compile-time macros) are optional, for complex composition. Protos and formulas are for reusable patterns and complex composition."

**Recent Formula Features** (from releases):
- Inline step expansion
- Expansion variable overrides
- Validation for expanded step IDs
- Gate-aware formula support (v2)
- `bd lint` command for template validation

**Macro Expansion Phase**:
> "A macro-expansion phase was needed in order to properly compose molecules with loops and gates."

This suggests formulas support control flow constructs (loops, gates) that expand at compile time into concrete dependency graphs.

### The Mol Mall

From Gas Town announcement:
> "The project includes a marketplace called the Mol Mall for sharing these formulas."

This indicates an ecosystem vision where formulas can be shared and reused across projects.

## Key Design Decisions

### 1. Default Parallelism

**Decision**: Work executes concurrently by default unless explicitly sequenced.

**Rationale**: Optimizes agent efficiency - agents can work on multiple tasks simultaneously without waiting for sequential bottlenecks.

**Implication**: Must use explicit dependencies (`needs`, `blocks`, `waits-for`) to enforce ordering.

### 2. Git-Backed Storage

**Decision**: Store issues as JSONL in git repository.

**Rationale**:
- Versioning and branching like code
- Merge/conflict resolution built-in
- Multi-agent collaboration through git operations
- Audit trail for all changes

**Implication**: Requires git hooks and sync mechanisms to maintain consistency.

### 3. Three-Phase Lifecycle (Proto/Mol/Wisp)

**Decision**: Separate templates, persistent work, and ephemeral work into distinct phases.

**Rationale**:
- Reusability without duplication
- Persistence without git pollution
- Efficiency for throwaway operations

**Implication**: Requires lifecycle management (pour, squash, burn operations).

### 4. Chemistry Metaphor

**Decision**: Use chemistry terms (molecules, protomolecules, wisps, bonding, cooking).

**Rationale**:
- Intuitive mental model for composition
- Distinguishes from generic "workflows" or "templates"
- Enables creative naming (Mol Mall)

**Fun Note** (from search results):
> "The naming is somewhat humorous - Claude insisted on The Expanse reference, ensuring they'll be sued by pretty much every major studio."

### 5. TOML for Formulas

**Decision**: Use TOML format for formula definitions (recent addition).

**Rationale**:
- Human-readable and writable
- Better for configuration than JSON
- Supports structured data without XML verbosity

**Evolution**: System originally used JSON for formulas, added TOML support later for better UX.

### 6. Bonding as Runtime Composition

**Decision**: Enable runtime bonding of work graphs.

**Rationale**:
- Workflows aren't always known statically
- Need to discover work dynamically
- Compose templates with runtime data

**Key Insight**: This is what distinguishes molecules from simple epics - the ability to stitch together workflow graphs at runtime based on discovered requirements.

## Critical Agent Pitfalls

From MOLECULES.md, Yegge documents common mistakes agents make:

### 1. Temporal Language Inversion

**Problem**: "Phase 1 comes before Phase 2" means `bd dep add phase2 phase1` (dependency points backward to what's needed).

**Why It's Wrong**: Natural language order is opposite to dependency direction.

**Correct Mental Model**: "Phase 2 depends on Phase 1" → `bd dep add phase2 phase1`

### 2. Order ≠ Sequence

**Problem**: Numbered task names (`task_1`, `task_2`, `task_3`) create no sequence without explicit dependencies.

**Why It's Wrong**: Names are just labels, not execution order.

**Correct Approach**: Add explicit `needs` or `blocks` dependencies.

### 3. Unclosed Blockers

**Problem**: If a blocker isn't closed, dependent work stays blocked indefinitely.

**Why It's Wrong**: Agents forget to close tasks after completion.

**Correct Pattern**: Always close tasks: `bd close <id> --reason "Done"`

### 4. Orphaned Wisps

**Problem**: Ephemeral instances accumulate without cleanup.

**Why It's Wrong**: Wisps aren't garbage collected automatically.

**Correct Pattern**: Use `bd mol squash` or `bd mol burn` for cleanup.

## Workflow Patterns

### Sequential Pipeline

**Pattern**: Chain steps with dependencies (A→B→C).

```toml
[[steps]]
id = "step-a"

[[steps]]
id = "step-b"
needs = ["step-a"]

[[steps]]
id = "step-c"
needs = ["step-b"]
```

### Parallel Fanout with Gate

**Pattern**: Multiple children run in parallel; `waits-for` blocks aggregation until all complete.

```toml
# Parent that spawns multiple children
[[steps]]
id = "discover-tasks"

# Multiple parallel tasks (no dependencies)
[[steps]]
id = "task-1"

[[steps]]
id = "task-2"

[[steps]]
id = "task-3"

# Gate that waits for all parallel tasks
[[steps]]
id = "aggregate"
waits_for = ["discover-tasks"]  # Waits for ALL children of discover-tasks
```

### Dynamic Bonding (Christmas Ornament)

**Pattern**: Discover work at runtime and bond arm molecules dynamically.

1. Create a survey/discovery task
2. Agent executes and discovers N work items
3. Agent creates N child tasks (or bonds N proto templates)
4. Parent task uses `waits-for` to wait for all children
5. Agent proceeds when all arms complete

### Compound Reusable Template

**Pattern**: Create templates that compose other templates.

```bash
# Create two proto templates
bd mol pour design-proto
bd mol pour implement-proto

# Bond them into a compound template
bd mol bond design-proto implement-proto --type proto
```

Result: A new proto that combines both workflows, can be reused.

## Integration with Gas Town

From Gas Town announcement:
> "Gas Town is Steve Yegge's project announced in January 2026, which is a coding agent orchestrator released on January 1."

**Gas Town's Role**:
- Multi-agent workspace manager
- Orchestrates multiple Claude Code instances
- Persistent work tracking through git-backed hooks
- "MEOW stack" - multi-agent orchestration framework

**Integration**:
- Gas Town uses Beads as its task tracking layer
- Formulas define multi-agent workflows
- Bonding enables agent coordination
- Wisps handle short-lived agent operations

**Vision**: "A new take on the IDE for 2026 that helps with the tedium of running lots of Claude Code instances."

## Key Insights for Our Graph System

### 1. Separation of Concerns

**Insight**: Separate workflow definition (formulas) from workflow execution (molecules) from workflow storage (issues).

**Application**: Our system should have:
- Template layer (formulas/protos)
- Execution layer (active work graphs)
- Storage layer (persistent state)

### 2. Default Parallelism

**Insight**: Make parallelism the default, sequence the exception.

**Application**:
- Don't assume task order
- Require explicit dependencies for sequencing
- Enable concurrent execution wherever possible

### 3. Git-Backed Persistence

**Insight**: Version control isn't just for code - it's perfect for structured work tracking.

**Application**:
- JSONL format for merge-friendly storage
- Git operations for multi-agent collaboration
- Hooks for automatic sync

### 4. Agent-First Design

**Insight**: Design APIs for programmatic consumption, not just humans.

**Application**:
- JSON output for all commands
- `bd ready` pattern for autonomous work discovery
- Clear blocked/ready state
- Dependency-aware queries

### 5. Composability through Bonding

**Insight**: Runtime composition is more powerful than static composition.

**Application**:
- Enable bonding operations at runtime
- Support dynamic workflow generation
- Allow templates to be composed with data

### 6. Lifecycle Management

**Insight**: Not all work is permanent - ephemeral work needs different handling.

**Application**:
- Support throwaway operations (wisps)
- Provide cleanup mechanisms (squash, burn)
- Separate git-synced from local-only work

### 7. Variable Substitution and Macro Expansion

**Insight**: Templates need parameterization and macro expansion for flexibility.

**Application**:
- Support template variables (`{{version}}`)
- Enable control flow in templates (loops, gates)
- Compile-time expansion into concrete graphs

### 8. Dependency Type Richness

**Insight**: Different blocking relationships enable different patterns.

**Application**:
- Not just "depends on" - support conditional, gate, organizational
- Enable error-driven workflows (conditional-blocks)
- Support fan-out/fan-in (waits-for)

### 9. Marketplace/Sharing Vision

**Insight**: Reusable workflow templates have network effects.

**Application**:
- Design formulas to be shareable
- Consider a marketplace/registry model
- Enable formula versioning and distribution

### 10. Clear Mental Models

**Insight**: The chemistry metaphor makes complex concepts intuitive.

**Application**:
- Use consistent metaphors throughout system
- Avoid overloading terms (epic vs molecule vs proto)
- Document pitfalls explicitly

## Implementation Checklist for Our System

Based on Yegge's approach, here's what we should implement:

### Core Infrastructure
- [ ] JSONL storage for work items
- [ ] Git-backed persistence with hooks
- [ ] Hash-based IDs for collision avoidance
- [ ] Hierarchical ID support (parent.child)
- [ ] SQLite cache layer for performance
- [ ] Dependency graph storage (blocks, waits-for, conditional)

### Formula System
- [ ] TOML parser for formula definitions
- [ ] Variable substitution (`{{var}}` syntax)
- [ ] Step dependency resolution (`needs`, `waits-for`)
- [ ] Macro expansion for control flow
- [ ] Formula validation (`lint` command)
- [ ] Template storage (`.beads/formulas/`)

### Molecule System
- [ ] Proto → Mol instantiation (`pour` operation)
- [ ] Proto → Wisp instantiation (ephemeral)
- [ ] Bonding operations (runtime composition)
- [ ] Lifecycle management (squash, burn)
- [ ] Ready work queries (zero blockers)
- [ ] Parallel execution model

### Agent Interface
- [ ] JSON output for all commands
- [ ] `ready` command for autonomous work discovery
- [ ] `create` command for dynamic work generation
- [ ] `close` command with reason tracking
- [ ] `dep add` for dynamic dependency creation
- [ ] Blocked/ready state queries

### Advanced Features
- [ ] Conditional execution (error-driven paths)
- [ ] Fan-out with gates (`waits-for` semantics)
- [ ] Dynamic bonding (Christmas Ornament pattern)
- [ ] Compound templates (proto + proto)
- [ ] Formula marketplace/sharing
- [ ] Multi-agent coordination hooks

### Documentation
- [ ] Common pitfall documentation
- [ ] Temporal language inversion guide
- [ ] Order vs sequence clarification
- [ ] Workflow pattern library
- [ ] Agent integration examples

## Open Questions

### 1. Formula Control Flow

**Question**: What control flow constructs do formulas support?

**Evidence**: "Macro-expansion phase was needed in order to properly compose molecules with loops and gates."

**Implication**: Formulas likely support:
- Loops (repeat steps N times?)
- Gates (fan-out/fan-in)
- Conditionals (if/else?)

**Need**: More documentation on control flow syntax.

### 2. Variable Scoping

**Question**: How do variables scope across bonded molecules?

**Evidence**: "Expansion variable overrides" feature mentioned.

**Implication**: Variables can be overridden when instantiating, but unclear:
- Can child molecules access parent variables?
- Can bonded molecules share variables?
- Are variables scoped per-molecule or global?

### 3. State Management

**Question**: How is state passed between steps?

**Evidence**: Not explicitly documented.

**Speculation**:
- Steps might be stateless (read from issue description?)
- State might be stored in issue body/notes
- State might be implicit (file system changes)

### 4. Error Handling

**Question**: How are errors propagated through the graph?

**Evidence**: `conditional-blocks` dependency type exists.

**Implication**:
- Steps can fail
- Failure triggers conditional paths
- But unclear: how are errors represented? logged?

### 5. Concurrent Execution Limits

**Question**: How many parallel tasks can agents execute?

**Evidence**: "Default parallelism" philosophy.

**Implication**:
- No built-in concurrency limits mentioned
- Agents might self-limit based on capacity
- Unclear: does the system enforce limits?

### 6. Formula Versioning

**Question**: How are formula versions managed?

**Evidence**: Formula metadata includes `version = "2.0"`.

**Implication**:
- Formulas are versioned
- But unclear: how are versions resolved?
- Can multiple versions coexist?
- How does `bd cook` choose versions?

### 7. Cleanup and Garbage Collection

**Question**: When are closed issues purged?

**Evidence**: "Semantic 'memory decay' summarizes old closed tasks to save context window."

**Implication**:
- Compaction mechanism exists
- But unclear: triggered how? manually? automatically?
- What's the retention policy?

## Comparison to Other Systems

### vs. GitHub Actions

**Similar**:
- Workflow definitions
- Dependency graphs
- Parallel execution

**Different**:
- Beads: Git-backed issue storage (persistent state)
- Beads: Runtime bonding (dynamic composition)
- Beads: Multi-session workflows (survives crashes)
- GitHub Actions: Event-driven triggers
- GitHub Actions: Stateless execution

### vs. Airflow/Dagster

**Similar**:
- DAG-based execution
- Task dependencies
- Scheduling

**Different**:
- Beads: Agent-first design (autonomous work discovery)
- Beads: Git-backed (multi-agent collaboration)
- Beads: Three-phase lifecycle (Proto/Mol/Wisp)
- Airflow: Cron-based scheduling
- Airflow: Centralized orchestration

### vs. Temporal/Durable Functions

**Similar**:
- Persistent workflow state
- Survives crashes and restarts
- Long-running workflows

**Different**:
- Beads: Git-backed (version controlled state)
- Beads: TOML formulas (declarative)
- Beads: Default parallelism (concurrent by default)
- Temporal: Code-first (workflows are functions)
- Temporal: Event sourcing (state reconstruction)

### vs. Epics/Stories in JIRA

**Similar**:
- Hierarchical work breakdown
- Parent-child relationships
- Status tracking

**Different**:
- Beads: Dependency graphs (explicit blocking)
- Beads: Formulas/templates (reusable workflows)
- Beads: Bonding (runtime composition)
- JIRA: Manual workflow management
- JIRA: No execution model (just tracking)

## Conclusion

Steve Yegge's formulas/molecules implementation represents a sophisticated approach to managing agent workflows with these key innovations:

1. **Three-Phase Lifecycle**: Proto/Mol/Wisp separation enables reusability, persistence, and efficiency
2. **TOML Formulas**: Declarative workflow templates with variable substitution and macro expansion
3. **Runtime Bonding**: Dynamic composition of work graphs based on discovered requirements
4. **Default Parallelism**: Optimize agent efficiency through concurrent execution by default
5. **Git-Backed Storage**: Version control for structured work with multi-agent collaboration
6. **Agent-First Design**: Autonomous work discovery through dependency-aware queries
7. **Rich Dependency Types**: Multiple blocking relationships enable diverse workflow patterns

The system is designed specifically for AI agents working on long-horizon, multi-session tasks with the ability to survive crashes, compaction, and interruptions. The chemistry metaphor (molecules, cooking, bonding) provides an intuitive mental model for a complex system.

For our graph system design, the key takeaways are:
- Separate definition from execution from storage
- Enable runtime composition through bonding
- Design for autonomy (ready work queries)
- Support both persistent and ephemeral work
- Make parallelism the default, sequence the exception
- Use version control for multi-agent collaboration

## References

### Primary Sources
- [Beads GitHub Repository](https://github.com/steveyegge/beads) - Main codebase
- [Gas Town GitHub Repository](https://github.com/steveyegge/gastown) - Multi-agent orchestrator
- [Molecules Documentation](https://github.com/steveyegge/beads/blob/main/docs/MOLECULES.md) - Core architecture
- [Agent Instructions](https://github.com/steveyegge/beads/blob/main/AGENT_INSTRUCTIONS.md) - Agent workflow patterns
- [Advanced Features](https://github.com/steveyegge/beads/blob/main/docs/ADVANCED.md) - Power user features
- [Changelog](https://github.com/steveyegge/beads/blob/main/CHANGELOG.md) - Feature evolution
- [Releases](https://github.com/steveyegge/beads/releases) - Version history

### Blog Posts (Medium - Paywall Limited Access)
- [Welcome to Gas Town](https://steve-yegge.medium.com/welcome-to-gas-town-4f25ee16dd04) - January 2026 announcement
- [Gas Town Emergency User Manual](https://steve-yegge.medium.com/gas-town-emergency-user-manual-cf0e4556d74b) - January 2026 guide
- [Beads Best Practices](https://steve-yegge.medium.com/beads-best-practices-2db636b9760c) - November 2025 guide
- [The Beads Revolution](https://steve-yegge.medium.com/the-beads-revolution-how-i-built-the-todo-system-that-ai-agents-actually-want-to-use-228a5f9be2a9) - Origin story
- [Introducing Beads](https://steve-yegge.medium.com/introducing-beads-a-coding-agent-memory-system-637d7d92514a) - Initial announcement

### Community
- [Beads for Blobfish](https://steve-yegge.medium.com/beads-for-blobfish-80c7a2977ffa) - Broader adoption
- [Beads Viewer](https://github.com/Dicklesworthstone/beads_viewer) - Third-party visualization tool
- [DoltHub: A Day in Gas Town](https://www.dolthub.com/blog/2026-01-15-a-day-in-gas-town/) - User experience report
- [Hacker News Discussion](https://news.ycombinator.com/item?id=46458936) - Community reactions

---

**Document Version**: 1.0
**Analysis Date**: 2026-01-16
**Analyst**: Claude Code Agent (task_20)
**Project**: tk-agents (agentic primer)
