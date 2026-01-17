# Beads Formulas, Molecules, and Workflow Templates

## Executive Summary

Beads implements a **layered architecture** for workflow management, using chemistry metaphors to express state transitions and composition. The system enables reusable, versionable workflow templates that can be instantiated, combined, and executed by AI agents across multiple sessions.

**Key Innovation**: Formulas provide compile-time composition of workflow templates, which are then instantiated into runtime molecules that maintain dependencies and execution state through git-backed storage.

## Architecture Layers (Top to Bottom)

```
┌─────────────────────────────────────────────────┐
│ FORMULAS (JSON/YAML compile-time macros)       │ ← Complex composition
│ - Optional layer for reusable patterns         │
│ - Variable definitions & substitution           │
│ - Composition rules (extends, compose)          │
└─────────────────────────────────────────────────┘
                    ↓ cook
┌─────────────────────────────────────────────────┐
│ PROTOS (template issues with "template" label) │ ← Reusable patterns
│ - Frozen templates in database                  │
│ - DAG structure with dependencies               │
│ - Variable placeholders {{key}}                 │
└─────────────────────────────────────────────────┘
                ↓ pour / wisp
┌─────────────────────────────────────────────────┐
│ MOLECULES (workflow operations)                 │ ← Workflow semantics
│ - bond: connect work graphs                     │
│ - squash: compress to digest                    │
│ - burn: discard ephemeral                       │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ EPICS (parent-child, dependencies)              │ ← DATA PLANE
│ - Parent issues with children                   │
│ - Dependency relationships                      │
│ - Execution semantics                           │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│ ISSUES (JSONL, git-backed)                      │ ← STORAGE
│ - Individual beads stored as JSONL              │
│ - Versioned, branched, merged like code         │
│ - Hash-based IDs prevent merge conflicts        │
└─────────────────────────────────────────────────┘
```

**Note**: Most users only need the bottom two layers (Epics + Issues). Formulas and Protos are for advanced workflows requiring reusability and composition.

---

## Phase Metaphor: State Transitions

Beads uses **chemistry terminology** to describe workflow state transitions:

### Solid → Liquid → Vapor

```
PROTO (Solid)          MOL (Liquid)         WISP (Vapor)
-------------          ------------         ------------
Frozen template        Active persistent    Ephemeral operation
Reusable pattern       Audit trail          No sync, no git
In .beads/             In .beads/           Local only
Synced across repos    Synced across repos  Auto-cleaned
```

### Phase Decision Matrix

| Use Case | Phase | Command | Characteristics |
|----------|-------|---------|----------------|
| Feature implementation | Liquid | `bd mol pour` | Persistent, audit trail, synced |
| Code review workflow | Liquid | `bd mol pour` | Needs history, multi-session |
| Release workflow | Vapor | `bd mol wisp` | One-time, no audit value |
| Patrol cycles | Vapor | `bd mol wisp` | Routine operational work |
| Health checks | Vapor | `bd mol wisp` | Diagnostic, temporary |
| Bug found during patrol | Liquid | `--pour` flag | Discovered work to persist |

**Key Principle**: Use vapor (wisp) by default for operational workflows; use liquid (mol) when you need audit trail or multi-session persistence.

---

## 1. Formulas: Compile-Time Workflow Definition

### What Are Formulas?

**Formulas** are YAML/JSON files that define workflow templates with:
- **Variable definitions** with defaults and validation
- **Step definitions** that become issue hierarchies
- **Composition rules** for combining formulas (extends, compose)
- **Inheritance** via extends mechanism

Formulas are the **source layer** - human-editable, version-controlled workflow definitions.

### Formula Lifecycle: Rig → Cook → Run

```
1. RIG:  Compose formulas (extends, compose)
2. COOK: Transform to proto (expand macros, apply aspects)
3. RUN:  Agents execute poured mols or wisps
```

### Search Paths (Priority Order)

When resolving formulas, beads searches:

1. `.beads/formulas/` (project-level)
2. `~/.beads/formulas/` (user-level)
3. `$GT_ROOT/.beads/formulas/` (orchestrator-level, if GT_ROOT set)

### Formula File Structure

Formulas are stored as:
- `.formula.json` (JSON format)
- `.formula.yaml` (YAML format)
- `.formula.toml` (TOML format)

Example: `mol-feature.formula.json`

```json
{
  "id": "mol-feature",
  "title": "Feature Implementation Workflow",
  "phase": "liquid",
  "variables": {
    "feature_name": {
      "default": "unnamed",
      "description": "Name of the feature being implemented"
    },
    "priority": {
      "default": "1",
      "description": "Priority level (0-3)"
    }
  },
  "steps": [
    {
      "id": "design",
      "title": "Design {{feature_name}}",
      "description": "Create design doc for {{feature_name}}"
    },
    {
      "id": "implement",
      "title": "Implement {{feature_name}}",
      "description": "Write code for {{feature_name}}",
      "depends_on": ["design"]
    },
    {
      "id": "test",
      "title": "Test {{feature_name}}",
      "description": "Write tests for {{feature_name}}",
      "depends_on": ["implement"]
    }
  ]
}
```

### Variable Substitution

Variables use `{{key}}` syntax and are substituted during:
- **Compile-time mode** (default): Variables kept as placeholders
- **Runtime mode**: Variables substituted with provided values

```bash
# Compile-time: preview template structure
bd cook mol-feature.formula.json

# Runtime: substitute variables
bd cook mol-feature --var feature_name=auth --var priority=0
```

---

## 2. Cooking: Formula → Proto Transformation

### The Cook Command

**`bd cook`** transforms a formula into a proto (template bead).

### Cooking Modes

#### Compile-Time Mode (Default)
- Produces proto with `{{variable}}` placeholders **intact**
- Use for: modeling, estimation, planning, contractor handoff
- Output shows template structure without substitution

```bash
bd cook mol-feature.formula.json
# Output: JSON with {{feature_name}}, {{priority}} placeholders
```

#### Runtime Mode
- Produces fully-resolved proto with variables **substituted**
- Use for: final validation before pour, seeing exact output
- Requires all variables to have values (via --var or defaults)

```bash
bd cook mol-feature --var feature_name=auth --var priority=0
# Output: JSON with "auth" and "0" substituted
```

### Output Options

#### Ephemeral (Default)
Outputs JSON to stdout, does not persist to database.

```bash
bd cook mol-feature.formula.json
# Prints JSON, no database write
```

#### Persistent (Legacy)
Writes proto to database with `--persist` flag.

```bash
bd cook mol-feature.formula.json --persist
# Creates proto bead in database with "template" label
```

**Modern Practice**: Prefer ephemeral cooking. `pour` and `wisp` commands accept formula names directly and cook inline, avoiding the need for persistent protos.

### Cook Examples

```bash
# Preview template structure (compile-time)
bd cook mol-feature.formula.json

# Substitute variables (runtime mode)
bd cook mol-feature --var name=auth

# Explicit runtime mode
bd cook mol-feature --mode=runtime --var name=auth

# Dry-run: preview steps without creating
bd cook mol-feature --dry-run

# Legacy: persist to database
bd cook mol-release.formula.json --persist

# Replace existing proto
bd cook mol-release.formula.json --persist --force

# Custom prefix for proto ID
bd cook mol-feature --persist --prefix "gt-"
# Creates: gt-mol-feature
```

---

## 3. Protos: Reusable Workflow Templates

### What Are Protos?

**Protos** (protomolecules) are template epics with:
- The `"template"` label for identification
- A DAG structure of child issues
- Variable placeholders `{{key}}`
- Dependencies matching `depends_on` relationships

Protos are **frozen templates** - reusable patterns stored in the database.

### Proto Characteristics

- **Uninstantiated**: Protos are templates, not active work
- **Reusable**: Same proto can spawn multiple molecules
- **Versionable**: Stored in `.beads/`, synced via git
- **Composable**: Can be bonded together (proto+proto → compound proto)

### Creating Protos

Two approaches:

#### 1. Cook from Formula (Forward Direction)
```bash
bd cook mol-feature.formula.json --persist
# Creates proto "mol-feature" with "template" label
```

#### 2. Distill from Epic (Reverse Direction)
```bash
bd mol distill bd-abc123 my-workflow --var feature_name=auth
# Extracts formula from existing epic
# Replaces concrete values with {{variable}} placeholders
```

---

## 4. Molecules: Runtime Workflow Instances

### What Are Molecules?

**Molecules** are active work instances - epics with workflow semantics.

**Key Insight**: Any epic with children functions as a molecule. No special type required. The system treats them as workflow graphs where **dependencies control execution**.

### Molecule Metaphor

From the chemistry analogy:
- **Proto** = molecular formula (template)
- **Molecule** = instantiated compound (active work)
- **Atoms** = individual beads (child issues)
- **Bonds** = dependencies between work graphs

### Pouring: Proto → Mol

**`bd mol pour`** instantiates a proto as a **persistent molecule**.

```bash
bd mol pour mol-feature --var name=auth --var priority=0
# Creates: bd-abc123 (hash ID)
#   ├─ bd-abc123.design (child)
#   ├─ bd-abc123.implement (child, depends on design)
#   └─ bd-abc123.test (child, depends on implement)
```

**Characteristics**:
- Persistent storage in `.beads/`
- Synced via git (part of JSONL export)
- Audit trail preserved
- Multi-session execution
- `Ephemeral=false` flag

**Use Cases**:
- Feature implementations spanning multiple sessions
- Work you may need to reference later
- Anything worth preserving in git history

### Wisping: Proto → Wisp

**`bd mol wisp`** instantiates a proto as an **ephemeral wisp**.

```bash
bd mol wisp beads-release --var version=1.0
# Creates: bd-xyz789 (hash ID, but Ephemeral=true)
```

**Characteristics**:
- Stored in database but **not exported to JSONL**
- Not synced via git
- Auto-cleaned by garbage collection
- `Ephemeral=true` flag

**Use Cases**:
- Release workflows (one-time execution)
- Patrol cycles (routine operational work)
- Health checks and diagnostics
- Any workflow without audit value

**Lifecycle**:
1. Create: `bd mol wisp <proto>` or `bd create --ephemeral`
2. Execute: Normal bd operations work on wisp issues
3. Squash: `bd mol squash <id>` (clears Ephemeral flag, promotes to persistent)
4. Or burn: `bd mol burn <id>` (deletes without creating digest)

### Phase Control: Pour vs Wisp

Formulas can specify recommended phase:

```json
{
  "id": "beads-release",
  "phase": "vapor",  // Recommends wisp usage
  ...
}
```

If you `pour` a vapor-phase formula, beads warns you. If you `wisp` a liquid-phase formula, beads warns you.

---

## 5. Bonding: Connecting Work Graphs

### The Bond Command

**`bd mol bond A B`** creates dependencies between work graphs.

### Polymorphic Bonding

Bond handles different operand types:

| A Type | B Type | Result | Description |
|--------|--------|--------|-------------|
| formula | formula | compound proto | Cook both, merge into template |
| formula | proto | compound proto | Cook formula, merge with proto |
| formula | mol | spawn + attach | Cook formula, spawn, attach to mol |
| proto | proto | compound proto | Merge into reusable template |
| proto | mol | spawn + attach | Spawn proto, attach to mol |
| mol | proto | spawn + attach | Spawn proto, attach to mol |
| mol | mol | compound mol | Join into compound molecule |

**Modern Practice**: Formula names (e.g., `mol-feature`) are cooked inline as ephemeral protos. This avoids needing pre-cooked proto beads in the database.

### Bond Types

Three types control execution relationship:

1. **Sequential** (default): B runs after A completes
2. **Parallel**: B runs alongside A
3. **Conditional**: B runs only if A fails

```bash
# Sequential bonding (default)
bd mol bond mol-feature mol-deploy

# Parallel bonding
bd mol bond mol-feature mol-deploy --type parallel

# Conditional bonding (run B if A fails)
bd mol bond mol-attempt mol-fallback --type conditional
```

### Phase Control in Bonding

When spawning protos during bonding, phase follows the target by default:

- Attaching to `mol` (Ephemeral=false) → spawns as persistent (Ephemeral=false)
- Attaching to wisp (Ephemeral=true) → spawns as ephemeral (Ephemeral=true)

**Override with flags**:
- `--pour`: Force spawn as liquid (persistent, Ephemeral=false)
- `--ephemeral`: Force spawn as vapor (ephemeral, Ephemeral=true)

```bash
# Found critical bug during patrol - persist it
bd mol bond mol-critical-bug wisp-patrol --pour

# Attach ephemeral diagnostic to persistent feature
bd mol bond mol-temp-check bd-feature --ephemeral
```

### Dynamic Bonding: Christmas Ornament Pattern

Use `--ref` for custom child references with variable substitution.

```bash
bd mol bond mol-polecat-arm bd-patrol --ref arm-{{polecat_name}} --var polecat_name=ace
# Creates: bd-patrol.arm-ace
#   └─ bd-patrol.arm-ace.capture (readable, not random hash)
```

**Benefits**:
- Readable IDs for per-worker arms on patrol
- Predictable structure for debugging
- Semantic naming instead of random hashes

### Bond Examples

```bash
# Compound proto (reusable template)
bd mol bond mol-feature mol-deploy

# Parallel execution
bd mol bond mol-feature mol-deploy --type parallel

# Attach proto to molecule
bd mol bond mol-feature bd-abc123

# Join two molecules
bd mol bond bd-abc123 bd-def456

# Persist found bug during patrol
bd mol bond mol-critical-bug wisp-patrol --pour

# Ephemeral diagnostic on persistent feature
bd mol bond mol-temp-check bd-feature --ephemeral

# Dynamic child ID with variables
bd mol bond mol-arm bd-patrol --ref arm-{{name}} --var name=ace
```

---

## 6. Execution Model

### Dependency-Driven Execution

**Key Principle**: Dependencies control execution. Children are **parallel by default**; only explicit dependencies create sequence.

```
Design Issue
  ├─ Frontend (parallel)
  ├─ Backend (parallel)
  └─ Integration (depends on Frontend + Backend)
```

### Agent Execution Flow

1. Agent queries for **ready work** (no blockers, open or in_progress)
2. Agent executes ready task
3. Agent marks task complete
4. Blockers resolve, new tasks become ready
5. Agent continues to newly unblocked tasks

**Cross-Molecule Execution**: When a molecule depends on another, the agent can seamlessly continue into the blocking molecule after completing prerequisites. This enables "compound execution" spanning multiple connected molecules.

### Multi-Session Persistence

- Molecules persist across sessions
- Agent resumes from last completed state
- Git-backed storage enables multi-agent collaboration
- Hash-based IDs prevent merge conflicts

---

## 7. Lifecycle Operations

### Squashing: Compress to Digest

**`bd mol squash <id>`** compresses a completed mol or wisp into a digest.

```bash
bd mol squash bd-abc123
# Creates: permanent summary record
# Clears Ephemeral flag if was wisp (promotes to persistent)
```

**Use Cases**:
- Preserve summary of completed work
- Promote valuable wisps to persistent record
- Reduce storage overhead while maintaining audit trail

### Burning: Discard Without Trace

**`bd mol burn <id>`** deletes a wisp without creating any record.

```bash
bd mol burn bd-xyz789
# Wisp deleted completely, no digest, no trace
```

**Use Cases**:
- Truly temporary scaffolding
- Failed or abandoned wisps
- Routine cycles with no audit value

### Distilling: Epic → Formula

**`bd mol distill <epic-id> [formula-name]`** extracts a reusable formula from an existing epic.

```bash
bd mol distill bd-abc123 my-workflow --var feature_name=auth
# Creates: .beads/formulas/my-workflow.formula.json
# Replaces "auth" with {{feature_name}} placeholder
```

**Use Cases**:
- Team develops good workflow organically, wants to reuse it
- Capture tribal knowledge as executable templates
- Create starting point for similar future work

**Variable Syntax** (both work):
- `--var branch=feature-auth` (Spawn-style, recommended)
- `--var feature-auth=branch` (Substitution-style)

Beads detects which side is the concrete value.

---

## 8. Key Design Patterns

### 1. Template Reusability

**Pattern**: Define formula once, instantiate many times with different variables.

```bash
# Define once
cat > .beads/formulas/mol-feature.formula.json << 'EOF'
{
  "id": "mol-feature",
  "variables": {"name": {"default": "unnamed"}},
  "steps": [
    {"id": "design", "title": "Design {{name}}"},
    {"id": "implement", "title": "Implement {{name}}", "depends_on": ["design"]},
    {"id": "test", "title": "Test {{name}}", "depends_on": ["implement"]}
  ]
}
EOF

# Instantiate many times
bd mol pour mol-feature --var name=auth
bd mol pour mol-feature --var name=payment
bd mol pour mol-feature --var name=search
```

### 2. Workflow Composition

**Pattern**: Combine multiple formulas into compound workflows.

```bash
# Bond formulas at proto level
bd mol bond mol-feature mol-deploy --as "Feature with Deploy"
# Creates: compound proto

# Pour compound proto
bd mol pour feature-with-deploy --var name=auth
```

### 3. Operational vs Persistent Work

**Pattern**: Use phase-appropriate commands for different work types.

```bash
# Persistent feature work (liquid)
bd mol pour mol-feature --var name=auth

# Ephemeral release workflow (vapor)
bd mol wisp beads-release --var version=1.0

# Found important bug during patrol? Persist it!
bd mol bond mol-bug-fix wisp-patrol --pour --var bug=auth-leak
```

### 4. Dynamic Work Attachment

**Pattern**: Attach work graphs dynamically during execution.

```bash
# Agent discovers need for additional work
bd mol bond mol-refactor bd-current-feature

# Agent spawns diagnostic check
bd mol bond mol-health-check bd-feature --ephemeral
```

### 5. Versioned Workflow Evolution

**Pattern**: Track formula changes in git, enabling versioned workflow templates.

```bash
# Commit formula changes
git add .beads/formulas/mol-feature.formula.json
git commit -m "Update feature workflow: add security review step"

# Different branches can have different workflow versions
git checkout release-1.0
bd mol pour mol-feature  # Uses 1.0 workflow

git checkout release-2.0
bd mol pour mol-feature  # Uses 2.0 workflow
```

---

## 9. Advanced Features

### Formula Inheritance

Formulas can extend other formulas:

```json
{
  "extends": "mol-base-feature",
  "id": "mol-security-feature",
  "steps": [
    {"id": "security-review", "title": "Security Review", "depends_on": ["design"]}
  ]
}
```

### Formula Composition

Multiple formulas can be composed:

```json
{
  "compose": ["mol-feature", "mol-security", "mol-deploy"],
  "id": "mol-secure-feature-deployment"
}
```

### Variable Validation

Formulas can define variable constraints:

```json
{
  "variables": {
    "priority": {
      "default": "1",
      "type": "number",
      "min": 0,
      "max": 3,
      "description": "Priority level (0=critical, 3=low)"
    }
  }
}
```

### Custom Prefixes

Use `--prefix` to namespace proto IDs:

```bash
bd cook mol-feature --persist --prefix "gt-"
# Creates: gt-mol-feature
```

Useful for orchestrator-level templates shared across projects.

---

## 10. Workflow Commands Summary

### Formula Management
```bash
bd formula list              # List available formulas
bd formula show <name>       # Show formula details
bd formula convert <file>    # Convert JSON to TOML
```

### Cooking (Formula → Proto)
```bash
bd cook <formula>                           # Compile-time (keep {{vars}})
bd cook <formula> --var k=v                 # Runtime (substitute vars)
bd cook <formula> --mode=compile|runtime    # Explicit mode
bd cook <formula> --persist                 # Write to database (legacy)
bd cook <formula> --dry-run                 # Preview steps
bd cook <formula> --prefix "gt-"            # Custom proto ID prefix
```

### Spawning (Proto → Mol/Wisp)
```bash
bd mol pour <proto> --var k=v               # Persistent molecule
bd mol wisp <proto> --var k=v               # Ephemeral wisp
```

### Bonding (Connecting Work)
```bash
bd mol bond <A> <B>                         # Sequential (default)
bd mol bond <A> <B> --type parallel         # Parallel
bd mol bond <A> <B> --type conditional      # Conditional
bd mol bond <A> <B> --pour                  # Force persistent spawn
bd mol bond <A> <B> --ephemeral             # Force ephemeral spawn
bd mol bond <A> <B> --ref child-{{var}}     # Custom child ref
```

### Lifecycle Operations
```bash
bd mol squash <id>                          # Compress to digest
bd mol burn <id>                            # Discard wisp
bd mol distill <epic-id> <name> --var k=v   # Extract formula
```

### Inspection
```bash
bd mol show <id>                            # Show molecule details
bd mol current                              # Show current position
bd mol progress <id>                        # Show progress summary
bd mol stale                                # Detect complete-but-unclosed
bd mol wisp list                            # List all wisps
bd mol wisp gc                              # Garbage collect wisps
```

---

## 11. Adapting to Our Graph System

### Key Concepts to Adopt

#### 1. Layered Architecture
Separate concerns into layers:
- **Storage**: Graph nodes (like Issues in beads)
- **Data Plane**: Task graph with dependencies (like Epics)
- **Workflow**: Operations on graph (like Molecules)
- **Templates**: Reusable patterns (like Protos)
- **Composition**: High-level macros (like Formulas)

#### 2. Phase Transitions
Use explicit state transitions:
- **Template → Active** (like Proto → Mol)
- **Active → Archived** (like Mol → Squashed)
- **Ephemeral → Persistent** (like Wisp → Mol via squash)

#### 3. Variable Substitution
Support parameterized templates:
- Define variables with defaults
- Substitute at instantiation time
- Enable compile-time (preview) vs runtime (execute) modes

#### 4. Composition Primitives
Provide operations to combine work:
- **Sequential**: B after A
- **Parallel**: B alongside A
- **Conditional**: B if A fails
- **Nested**: A contains B

#### 5. Versioning Strategy
Store templates in git-backed storage:
- Templates evolve over time
- Different branches = different versions
- Merge conflicts handled at file level

### Design Differences to Consider

| Beads Approach | Our System Consideration |
|----------------|--------------------------|
| Hash-based IDs (bd-abc123) | Hierarchical IDs (task_1.subtask_2) |
| JSONL storage per issue | Graph-based node storage |
| Git-backed issue tracker | Graph database with versioning |
| Chemistry metaphors | Task/graph terminology |
| CLI-first interface | Agent-first API |
| Issue-centric model | Graph-centric model |

### Features to Prioritize

1. **Template Instantiation**: Formula → Active task graph
2. **Variable Substitution**: Parameterized templates
3. **Composition**: Combine task graphs dynamically
4. **Phase Management**: Ephemeral vs persistent work
5. **Versioned Templates**: Git-backed formula storage

### Features to Defer

1. **Formula Inheritance**: Complex extends/compose rules
2. **Custom Prefixes**: Namespace management
3. **Multiple Search Paths**: Complex resolution logic
4. **Wisp Garbage Collection**: Ephemeral cleanup
5. **Distilling**: Epic → Formula extraction

---

## 12. References

### Primary Documentation
- [Beads GitHub Repository](https://github.com/steveyegge/beads) - Main project repository
- [Molecules Documentation](https://github.com/steveyegge/beads/blob/main/docs/MOLECULES.md) - Molecule system details
- [Better Stack Beads Guide](https://betterstack.com/community/guides/ai/beads-issue-tracker-ai-agents/) - Overview guide

### Steve Yegge's Articles
- [Introducing Beads: A Coding Agent Memory System](https://steve-yegge.medium.com/introducing-beads-a-coding-agent-memory-system-637d7d92514a)
- [The Beads Revolution](https://steve-yegge.medium.com/the-beads-revolution-how-i-built-the-todo-system-that-ai-agents-actually-want-to-use-228a5f9be2a9)
- [Beads Best Practices](https://steve-yegge.medium.com/beads-best-practices-2db636b9760c)
- [Welcome to Gas Town](https://steve-yegge.medium.com/welcome-to-gas-town-4f25ee16dd04)

### Community Resources
- [Beads Viewer](https://github.com/Dicklesworthstone/beads_viewer) - Visualization tool
- [DeepWiki Beads Docs](https://deepwiki.com/steveyegge/beads) - Aggregated documentation

---

## Appendix: CLI Command Reference

Complete command reference extracted from `bd --help`:

### Core Molecule Commands
```bash
bd mol pour <proto> [--var k=v] [--dry-run] [--assignee user]
bd mol wisp <proto> [--var k=v] [--dry-run]
bd mol bond <A> <B> [--type sequential|parallel|conditional] [--pour|--ephemeral] [--ref custom-{{var}}]
bd mol squash <id>
bd mol burn <id>
bd mol distill <epic-id> <name> [--var k=v] [--output dir]
bd mol show <id>
bd mol current
bd mol progress <id>
bd mol stale
bd mol wisp list
bd mol wisp gc
```

### Formula Commands
```bash
bd formula list
bd formula show <name>
bd formula convert <file>
```

### Cook Commands
```bash
bd cook <formula-file> [flags]
  --mode string           compile (keep placeholders) or runtime (substitute vars)
  --var k=v               Variable substitution, enables runtime mode
  --persist               Persist proto to database (legacy)
  --force                 Replace existing proto (requires --persist)
  --prefix string         Prefix for proto ID (e.g., 'gt-')
  --dry-run               Preview what would be created
  --search-path strings   Additional paths for formula inheritance
```

---

**Document Version**: 1.0
**Created**: 2026-01-16
**Author**: Background Research Agent
**Purpose**: Inform design decisions for tk-agents graph/formula/molecule system
