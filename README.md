# Agentic Primer

**Protocol-first convergence platform for multi-agent actor systems.**

## Current Focus (February 2026)

This project is building a **universal protocol layer** for convergent agent systems using the WASM Component Model:

- **@agentic-primer/protocols** - Published npm package with 63+ typed domain primitives (Address, Node, Edge, Agent, Task, etc.)
- **WIT Interface Definitions** - Language-agnostic protocol specifications using WebAssembly Interface Types
- **SEAG Integration** - Reference implementation of protocol-driven actor system (Simplify Environment for Agentic Growth)
- **Path-Based Addressing** - Hierarchical routing for actor systems (POC complete, performance validated)

**Key Components:**
- `packages/protocols/` - Protocol package (JSON Schema → TypeScript + Zod + WIT)
- `docs/protocols/` - WIT migration plans, integration strategy, validation reports
- `simplify/` - SEAG reference implementation (separate branch: feature/path-addressing)

**Recent Milestones:**
- Feb 6: Published `@agentic-primer/protocols@0.1.0` to npm
- Feb 5-6: Path-based addressing POC with hierarchical routing
- Late Jan: WIT protocol extraction and consolidation from UGS/Simplify
- Early Feb: Documentation reorganization (archive structure created)

See **CURRENT_STATE.md** for detailed project evolution and **docs/protocols/** for architecture documentation.

---

## Historical Context: Bootstrap Experiments (January 2026)

The project began with bootstrap simulation experiments (Jan 5-8) exploring self-optimizing git-native issue automation. This work is now archived in `docs/archive/bootstrap-phase/` and `docs/bootstrap/`. The focus has shifted to protocol-first actor systems.

## Quick Start

### To Use the Protocols Package:
```bash
npm install @agentic-primer/protocols
```

See `packages/protocols/README.md` for usage examples.

### To Explore WIT Protocols:
- Protocol definitions: `packages/protocols/wit/`
- Migration plans: `docs/protocols/WIT_PLATFORM_MIGRATION_PLAN.md`
- Integration strategy: `docs/protocols/INTEGRATION_STRATEGY.md`

### To Run Bootstrap Experiments (Historical):
1. Read `docs/bootstrap/BOOTSTRAP.md` - The 30-word prompt
2. Check `docs/bootstrap/SUCCESS_CRITERIA.md` - Observable outcomes
3. See `docs/simulation/` for simulation harness documentation

## Key Files

**Protocols & Architecture:**
- **packages/protocols/** - Published npm package with domain types
- **docs/protocols/** - WIT migration plans, integration strategy, validation reports
- **CURRENT_STATE.md** - Project evolution and current status
- **AGENTS.md** - Agent instructions (issue tracking, workflow)

**Activity Management:**
- **docs/activity/** - Worktree-based parallel activities system
- **scripts/activity** - Activity management CLI

**Historical (Bootstrap Experiments):**
- **docs/bootstrap/** - Bootstrap prompt and success criteria
- **docs/simulation/** - Simulation harness and testing framework
- **docs/archive/bootstrap-phase/** - Archived bootstrap-era documentation
- **docs/experiments/** - Completed meta-instruction and validation experiments

## Repository Structure

```text
agentic-primer/
├── packages/
│   └── protocols/            # @agentic-primer/protocols npm package
│       ├── schema/           # JSON Schema (source of truth)
│       ├── wit/              # WIT interface definitions
│       ├── src/              # Generated TypeScript + Zod
│       └── README.md         # Package documentation
│
├── docs/
│   ├── protocols/            # WIT migration, integration strategy
│   ├── activity/             # Activity system documentation
│   ├── bootstrap/            # Historical: bootstrap experiments
│   ├── simulation/           # Historical: simulation harness
│   ├── experiments/          # Historical: completed research findings
│   ├── knowledge/            # Session knowledge system
│   └── archive/              # Archived documentation
│       └── bootstrap-phase/  # Outdated bootstrap-era docs
│
├── simplify/                 # SEAG implementation (separate branch)
├── experiments/              # Historical experiment runs
├── scripts/                  # Utilities
│   ├── activity              # Activity management CLI
│   ├── know                  # Knowledge query CLI
│   └── [other scripts]
│
├── CURRENT_STATE.md          # Project evolution summary
├── AGENTS.md                 # Agent instructions
└── ARCHIVED_BRANCHES.md      # Documented archived work
```

## Key Learnings from Bootstrap Experiments (Historical)

From 9 bootstrap simulations (Jan 5-8, 2026):
- **Prompt length matters**: 30-35 words optimal for completeness
- **Model archetypes emerge**: Opus (analysis), Sonnet (research), Haiku (implementation)
- **Outcome-based criteria work better** than implementation requirements

See `docs/bootstrap/` and `docs/archive/bootstrap-phase/` for detailed findings.

## Usage

**Install the protocols package:**
```bash
npm install @agentic-primer/protocols
```

**Use domain types in TypeScript:**
```typescript
import { Address, Node, Agent, addressSchema } from '@agentic-primer/protocols';

const addr: Address = { id: "agent-123", scope: "node" };
const result = addressSchema.safeParse(addr);
```

**Manage parallel work:**
```bash
# Create activity worktree
./scripts/activity create feature-name "Description"

# Switch between activities
./scripts/activity list
./scripts/activity switch feature-name

# See docs/activity/ for full documentation
```

**Query session knowledge:**
```bash
./scripts/know decisions recent 10
./scripts/know learnings category technical
./scripts/know errors type NetworkError
```

**Track issues:**
```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress
bd close <id>         # Complete work
```

See `docs/` for detailed documentation and `AGENTS.md` for agent workflows.
