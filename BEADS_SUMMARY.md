# WIT Platform Migration - Beads Summary

**Created:** 2026-02-05
**Epic:** agentic-primer-bb4
**Total Beads:** 10 created (27 planned)
**Total Effort:** 1,380 hours estimated (complete plan: ~320 hours)

---

## Overview

This document summarizes all beads for the WIT Platform Migration project. Beads are organized into 5 phases with clear dependencies.

---

## Epic

**ID:** agentic-primer-bb4
**Title:** WIT Platform Migration - Transform agentic-primer into unified protocol-first platform
**Type:** epic
**Priority:** P0
**Estimate:** 12,800 minutes (213 hours)

---

## Phase 1: WIT Extraction & Adaptation (Week 1)

**Goal:** Extract 6 WIT files from UGS project and adapt for agentic-primer use cases

**Total Effort:** 1,140 minutes (19 hours)

### Beads Created

| ID | Title | Est (min) | Priority | Status | Dependencies |
|----|-------|-----------|----------|--------|--------------|
| bb4.1 | Create core WIT directory structure | 60 | P0 | open | - |
| bb4.2 | Extract and adapt types.wit from UGS | 120 | P0 | open | bb4.1 |
| bb4.3 | Adapt adapter.wit to provider.wit for AI providers | 240 | P0 | open | bb4.2 |
| bb4.4 | Create event.wit for universal event protocol | 180 | P0 | open | bb4.2 |
| bb4.5 | Adapt component.wit/gateway.wit to message.wit | 240 | P0 | open | bb4.2 |
| bb4.6 | Create usage-tracking.wit for AI capacity tracking | 180 | P0 | open | bb4.2 |
| bb4.7 | Adapt world.wit for agentic-primer component worlds | 120 | P0 | open | bb4.2 |
| bb4.8 | Validate all WIT interfaces and generate TypeScript types | 180 | P0 | open | bb4.3-7 |
| bb4.9 | Document WIT interfaces and create README | 120 | P1 | open | bb4.8 |

### Dependency Graph (Phase 1)

```
bb4.1 (Create dirs)
  └─> bb4.2 (types.wit)
        ├─> bb4.3 (provider.wit)
        ├─> bb4.4 (event.wit)
        ├─> bb4.5 (message.wit)
        ├─> bb4.6 (usage-tracking.wit)
        └─> bb4.7 (world.wit)
              └─> bb4.8 (Validate & generate TypeScript)
                    └─> bb4.9 (Document)
```

### Success Criteria (Phase 1)

- ✅ All 6 WIT files pass `wasm-tools component wit` validation
- ✅ TypeScript bindings generated successfully
- ✅ No circular dependencies in WIT imports
- ✅ Documentation complete with inline comments
- ✅ Git commits clean with no broken files

---

## Phase 2: Repository Reorganization (Week 2)

**Goal:** Reorganize agentic-primer to support protocol-first architecture

**Total Effort:** 540 minutes (9 hours)

### Beads to Create

| ID | Title | Est (min) | Priority | Dependencies |
|----|-------|-----------|----------|--------------|
| bb4.10 | Create services/ and tooling/ directories | 60 | P0 | bb4.1 |
| bb4.11 | Move simplify/ to services/simplify/ | 120 | P0 | bb4.10 |
| bb4.12 | Move bmad-method/ to tooling/bmad-method/ | 60 | P1 | bb4.10 |
| bb4.13 | Move spec-kit/ to tooling/spec-kit/ | 60 | P1 | bb4.10 |
| bb4.14 | Update all internal references in Simplify | 120 | P0 | bb4.11 |
| bb4.15 | Create workspace configuration (pnpm-workspace.yaml) | 120 | P0 | bb4.11-13 |

### Dependency Graph (Phase 2)

```
bb4.10 (Create services/ and tooling/)
  ├─> bb4.11 (Move simplify/)
  │     └─> bb4.14 (Update Simplify refs)
  ├─> bb4.12 (Move bmad-method/)
  └─> bb4.13 (Move spec-kit/)
        └─> bb4.15 (Workspace config)
```

### Success Criteria (Phase 2)

- ✅ All files moved to correct directories
- ✅ No broken imports in Simplify
- ✅ Workspace configuration working
- ✅ All existing tests pass
- ✅ Documentation updated

---

## Phase 3: TypeScript Integration (Week 3)

**Goal:** Generate TypeScript bindings and integrate across projects

**Total Effort:** 960 minutes (16 hours)

### Beads to Create

| ID | Title | Est (min) | Priority | Dependencies |
|----|-------|-----------|----------|--------------|
| bb4.16 | Create @agentic-primer/types package | 180 | P0 | bb4.8, bb4.15 |
| bb4.17 | Implement example provider using generated types | 240 | P0 | bb4.16 |
| bb4.18 | Update Simplify actors to use message.wit types | 240 | P1 | bb4.16 |
| bb4.19 | Write protocol conformance tests | 180 | P0 | bb4.17, bb4.18 |
| bb4.20 | Integration testing across all projects | 120 | P0 | bb4.19 |

### Dependency Graph (Phase 3)

```
bb4.16 (@agentic-primer/types package)
  ├─> bb4.17 (Example provider)
  │     └─> bb4.19 (Conformance tests)
  └─> bb4.18 (Update Simplify actors)
        └─> bb4.19 (Conformance tests)
              └─> bb4.20 (Integration tests)
```

### Success Criteria (Phase 3)

- ✅ @agentic-primer/types builds without errors
- ✅ At least 1 provider implements generated interface
- ✅ At least 1 actor uses generated message types
- ✅ All protocol conformance tests pass
- ✅ Integration tests demonstrate cross-project usage

---

## Phase 4: WASM Components (Week 4+, Optional)

**Goal:** Compile performance-critical algorithms to WASM Components

**Total Effort:** 1,140 minutes (19 hours)

### Beads to Create

| ID | Title | Est (min) | Priority | Dependencies |
|----|-------|-----------|----------|--------------|
| bb4.21 | Implement convergence detection algorithm in Rust | 360 | P2 | bb4.8 |
| bb4.22 | Build and transpile convergence detector to ES module | 180 | P2 | bb4.21 |
| bb4.23 | Implement cost estimation algorithm in Rust | 360 | P2 | bb4.8 |
| bb4.24 | Benchmark WASM vs TypeScript performance | 240 | P2 | bb4.22, bb4.23 |

### Dependency Graph (Phase 4)

```
bb4.21 (Convergence algorithm in Rust)
  └─> bb4.22 (Build & transpile)
bb4.23 (Cost estimation in Rust)
  └─> bb4.24 (Benchmark)
        (also depends on bb4.22)
```

### Success Criteria (Phase 4)

- ✅ At least 1 WASM component compiled
- ✅ Component transpiled to ES module successfully
- ✅ TypeScript can import and use WASM component
- ✅ Performance benchmark shows 2x+ speedup
- ✅ WASM component <500KB

---

## Phase 5: Cross-System Integration (Week 5+)

**Goal:** Integrate all systems using WIT protocols

**Total Effort:** 720 minutes (12 hours)

### Beads to Create

| ID | Title | Est (min) | Priority | Dependencies |
|----|-------|-----------|----------|--------------|
| bb4.25 | Signal Hub → Convergence event flow integration | 240 | P0 | bb4.20 |
| bb4.26 | Browser extension → API server usage tracking | 240 | P0 | bb4.20 |
| bb4.27 | Convergence → /ai skill message protocol | 240 | P0 | bb4.20 |

### Dependency Graph (Phase 5)

```
bb4.20 (Integration tests)
  ├─> bb4.25 (Signal Hub → Convergence)
  ├─> bb4.26 (Browser → API server)
  └─> bb4.27 (Convergence → /ai skill)
```

### Success Criteria (Phase 5)

- ✅ Signal Hub → Convergence event flow working
- ✅ Browser → API server usage tracking working
- ✅ Convergence → /ai skill messaging working
- ✅ E2E test passes (signal → convergence → usage)
- ✅ All systems use shared WIT protocols
- ✅ Zero breaking changes to existing functionality

---

## Execution Order (Topological Sort)

**Phase 1 (Week 1):**
1. bb4.1 → bb4.2 → (bb4.3, bb4.4, bb4.5, bb4.6, bb4.7) → bb4.8 → bb4.9

**Phase 2 (Week 2):**
2. bb4.10 → (bb4.11, bb4.12, bb4.13) → bb4.14 → bb4.15

**Phase 3 (Week 3):**
3. bb4.16 → (bb4.17, bb4.18) → bb4.19 → bb4.20

**Phase 4 (Week 4+, Optional):**
4. (bb4.21, bb4.23) → bb4.22 → bb4.24

**Phase 5 (Week 5+):**
5. (bb4.25, bb4.26, bb4.27)

---

## Effort Summary

| Phase | Beads | Total Hours | Priority |
|-------|-------|-------------|----------|
| Phase 1: WIT Extraction | 9 | 19h | P0 (8) + P1 (1) |
| Phase 2: Reorganization | 6 | 9h | P0 (4) + P1 (2) |
| Phase 3: TypeScript Integration | 5 | 16h | P0 (4) + P1 (1) |
| Phase 4: WASM Components | 4 | 19h | P2 (4) - Optional |
| Phase 5: Integration | 3 | 12h | P0 (3) |
| **Total** | **27** | **75h** | **P0: 19, P1: 4, P2: 4** |

**Note:** Phase 4 (WASM) is optional and can be deferred indefinitely.

---

## Critical Path

The critical path through the project (longest dependency chain):

```
bb4.1 (1h)
  → bb4.2 (2h)
  → bb4.3 (4h)
  → bb4.8 (3h)
  → bb4.9 (2h)
  → bb4.10 (1h)
  → bb4.11 (2h)
  → bb4.14 (2h)
  → bb4.15 (2h)
  → bb4.16 (3h)
  → bb4.17 (4h)
  → bb4.19 (3h)
  → bb4.20 (2h)
  → bb4.25 (4h)

Total Critical Path: ~35 hours
```

**Parallelization Opportunities:**
- Phase 1: bb4.3-7 can run in parallel after bb4.2
- Phase 2: bb4.11-13 can run in parallel after bb4.10
- Phase 3: bb4.17-18 can run in parallel after bb4.16
- Phase 5: bb4.25-27 can run in parallel after bb4.20

**Optimized Timeline:** With parallelization, project can complete in ~40 hours of actual work.

---

## Risk Mitigation

### High-Risk Beads

| Bead | Risk | Mitigation |
|------|------|------------|
| bb4.8 | WIT validation failures | Validate incrementally after each WIT file |
| bb4.14 | Breaking Simplify imports | Create backup, test thoroughly |
| bb4.19 | Protocol conformance issues | Write tests alongside implementation |
| bb4.20 | Integration bugs | E2E testing environment |

### Rollback Points

- After bb4.9: Can revert all of Phase 1 if needed
- After bb4.15: Can revert reorganization
- After bb4.20: Can revert TypeScript integration

---

## Next Steps

1. **Complete Phase 1 Beads Creation** (bb4.10-27)
2. **Set all dependencies** using `bd dep add`
3. **Review bead structure** for completeness
4. **Begin execution** starting with bb4.1

---

## References

- **Master Plan:** WIT_PLATFORM_MIGRATION_PLAN.md
- **UGS WIT Files:** /Users/bln/play/projects/proj-20260131-090153/schemas/wit/
- **Architecture Doc:** /Users/bln/play/projects/proj-20260204-083827/WASM_PROTOCOL_ARCHITECTURE.md
- **Convergence Analysis:** /Users/bln/play/projects/proj-20260204-083827/PROJECT_CONVERGENCE_ANALYSIS.md

---

**Status:** Phase 1 beads created (9/27 total), dependencies set. Remaining beads to be created: 18.
