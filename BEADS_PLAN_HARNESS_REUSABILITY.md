# Beads Plan: Harness Reusability (#2)

**Date:** 2026-01-06
**Epic:** agentic-primer-rin
**Context:** HARNESS_REUSABILITY_ANALYSIS.md

---

## Epic Summary

**agentic-primer-rin:** Create reusable simulation harness framework

Make SIMULATION_HARNESS.md reusable across projects with hybrid approach (skill + scripts + docs).

**Goals:**
- Enable other projects to use simulation harness methodology
- Balance ease-of-use (skill) with repeatability (scripts) and flexibility (docs)
- Version-control all components for reproducibility

**Deliverables:**
1. HARNESS_GUIDE.md (core documentation)
2. HARNESS_TEMPLATE.md (template with placeholders)
3. scripts/harness/ (reusable automation scripts)
4. examples/ directory (bootstrap + template)
5. /create-harness skill (optional Phase 2)

---

## Task Breakdown

### Phase 1: Core Foundation (P1 - Can work in parallel)

#### agentic-primer-rin.1: Create HARNESS_GUIDE.md
**Priority:** P1
**Labels:** documentation
**Status:** Ready to start (no blockers)

Comprehensive guide explaining how to create and customize simulation harnesses.

**Content sections:**
- Concepts: What is a simulation harness?
- Quick Start: Manual setup + skill path
- Config Schema: Document config.json structure
- Customization: Custom rubrics, non-standard matrices
- Examples: Link to bootstrap and other examples
- Troubleshooting: Common issues

**Success criteria:**
- Someone can create harness WITHOUT skill (docs are complete)
- Clear for first-time users
- Generic (not bootstrap-specific)

---

#### agentic-primer-rin.2: Create HARNESS_TEMPLATE.md
**Priority:** P1
**Labels:** documentation
**Status:** Ready to start (no blockers)

Template version of SIMULATION_HARNESS.md with clear placeholders.

**Features:**
- All project-specific content replaced with {PLACEHOLDERS}
- Comments explaining each section
- Reference to HARNESS_GUIDE.md
- Used by /create-harness skill as generation source

**Placeholders:**
- {PROJECT_NAME}, {DOMAIN_CONTEXT}
- {PROMPTS_COUNT}, {CRITERIA_COUNT}, {MODELS_COUNT}
- {P1_DESCRIPTION}, {S1_DESCRIPTION}, etc.
- {RUBRIC_DIMENSIONS}

---

#### agentic-primer-rin.3: Extract scripts/harness/
**Priority:** P1
**Labels:** automation
**Status:** Ready to start (no blockers)

Reusable automation scripts for harness execution.

**Scripts to create:**
1. **scaffold.sh** - Create directory structure from config
2. **run-batch.sh** - Execute simulation batch (Phase 1)
3. **evaluate-batch.sh** - Execute evaluation batch (Phase 2)
4. **aggregate-results.sh** - Aggregate analysis (Phase 3)
5. **helpers.sh** - Shared utilities

**Key design:**
- Scripts output JSON that Claude reads
- Version-controlled, testable, repeatable
- Don't directly invoke Task tool (can't from shell)

---

### Phase 1.5: Integration (P2 - Depends on Phase 1)

#### agentic-primer-rin.4: Create examples/ directory
**Priority:** P2
**Labels:** examples
**Status:** Blocked by agentic-primer-rin.3
**Dependencies:**
- Depends on: scripts/harness/ (rin.3)

Create examples directory with bootstrap reference and empty template.

**Structure:**
```
examples/
├── bootstrap/              # Working example
│   ├── config.json
│   ├── prompts/
│   ├── criteria/
│   └── README.md
└── template/              # Empty template for copying
    ├── config.template.json
    ├── prompts/README.md
    └── criteria/README.md
```

**Tasks:**
1. Copy experiments/iteration-2 to examples/bootstrap
2. Create clean template directory
3. Write README files with instructions
4. Validate config.json matches schema

---

#### agentic-primer-rin.5: Update existing docs
**Priority:** P2
**Labels:** documentation
**Status:** Blocked by agentic-primer-rin.1, agentic-primer-rin.2
**Dependencies:**
- Depends on: HARNESS_GUIDE.md (rin.1)
- Depends on: HARNESS_TEMPLATE.md (rin.2)

Update existing documentation to reference new harness system.

**Files to update:**
1. **SIMULATION_HARNESS.md** - Mark as bootstrap-specific, link to guide
2. **RUN_SIMULATION.md** - Reference scripts/harness/ scripts
3. **README.md** - Link to HARNESS_GUIDE.md
4. **QUICK_START_TONIGHT.md** - Update paths if needed

**Goal:** Ensure all docs are consistent and cross-referenced

---

### Phase 2: Optional Automation (P3 - Deferred)

#### agentic-primer-rin.6: Create /create-harness skill
**Priority:** P3
**Labels:** automation, skill, phase-2
**Status:** Blocked by agentic-primer-rin.1, rin.2, rin.3
**Dependencies:**
- Depends on: HARNESS_GUIDE.md (rin.1)
- Depends on: HARNESS_TEMPLATE.md (rin.2)
- Depends on: scripts/harness/ (rin.3)

Build interactive skill for guided harness setup. DEFERRED until scripts and docs are proven.

**Skill responsibilities:**
- Interactive Q&A for harness setup
- Validates inputs (test matrix math, file paths)
- Calls scripts/harness/scaffold.sh
- Generates config.json with user inputs
- Explains next steps

**Questions (one at a time):**
1. Project name
2. Domain context
3. Number of prompt variants (default: 3)
4. Number of criteria levels (default: 3)
5. Models to test (default: opus/sonnet/haiku)
6. Rubric dimensions (suggest standard, allow custom)

**What skill does NOT do:**
- Execute simulations (delegates to scripts)
- Replace documentation (points to HARNESS_GUIDE.md)

---

## Dependency Graph

```
Epic: agentic-primer-rin (harness reusability)
├─ Phase 1 (P1 - parallel, no blockers):
│  ├─ rin.1: HARNESS_GUIDE.md
│  ├─ rin.2: HARNESS_TEMPLATE.md
│  └─ rin.3: scripts/harness/
│
├─ Phase 1.5 (P2 - blocked by Phase 1):
│  ├─ rin.4: examples/
│  │  └─ depends on: rin.3 (scripts)
│  │
│  └─ rin.5: update docs
│     ├─ depends on: rin.1 (guide)
│     └─ depends on: rin.2 (template)
│
└─ Phase 2 (P3 - optional, blocked by Phase 1):
   └─ rin.6: /create-harness skill
      ├─ depends on: rin.1 (guide)
      ├─ depends on: rin.2 (template)
      └─ depends on: rin.3 (scripts)
```

---

## Execution Strategy

### Immediate Next Steps (Ready to work)

All Phase 1 tasks can be worked in parallel since they have no blockers:

1. **rin.1** (HARNESS_GUIDE.md) - Documentation work
2. **rin.2** (HARNESS_TEMPLATE.md) - Documentation work
3. **rin.3** (scripts/harness/) - Automation/scripting work

**Recommendation:** Start with **rin.3 (scripts)** first because:
- Proves the automation approach works
- Validates config.json schema design
- Informs documentation (rin.1 and rin.2 benefit from seeing working scripts)
- Unblocks rin.4 (examples) once complete

### After Phase 1 Complete

Once rin.1, rin.2, rin.3 are done:
- **rin.4** (examples/) becomes ready
- **rin.5** (doc updates) becomes ready
- **rin.6** (skill) becomes ready but is P3 (optional)

Work on rin.4 and rin.5 in parallel.

### Phase 2 Decision Point

After Phase 1 complete, evaluate:
- Are scripts working well?
- Is manual workflow (docs + scripts) sufficient?
- Is demand high enough for /create-harness skill?

If yes to all, proceed with rin.6. Otherwise, leave as P3/backlog.

---

## Reference Materials

**Primary Analysis:**
- `/Users/bln/play/agentic-primer/HARNESS_REUSABILITY_ANALYSIS.md`

**Current Harness Instance:**
- `/Users/bln/play/agentic-primer/SIMULATION_HARNESS.md` (bootstrap-specific)
- `/Users/bln/play/agentic-primer/experiments/iteration-2/` (current test setup)

**Existing Scripts to Extract From:**
- `scripts/create-experiment-run.sh`
- `scripts/finalize-experiment-run.sh`
- `scripts/analyze-simulation-agents.sh`

---

## Commands Reference

```bash
# View epic and all tasks
bd show agentic-primer-rin

# View ready tasks (no blockers)
bd ready

# View all tasks under epic
bd list --parent agentic-primer-rin

# Start working on a task
bd update agentic-primer-rin.1 --status in-progress

# Mark task complete
bd update agentic-primer-rin.1 --status closed

# View dependencies
bd show agentic-primer-rin.6  # Shows all blocking dependencies
```

---

## Success Metrics

**Phase 1 Complete When:**
- [ ] HARNESS_GUIDE.md exists and is comprehensive
- [ ] HARNESS_TEMPLATE.md has all placeholders clearly marked
- [ ] scripts/harness/ directory exists with 5 working scripts
- [ ] All Phase 1 tasks closed in beads

**Phase 1.5 Complete When:**
- [ ] examples/bootstrap/ mirrors current harness setup
- [ ] examples/template/ provides clean starting point
- [ ] All existing docs cross-reference HARNESS_GUIDE.md
- [ ] All Phase 1.5 tasks closed in beads

**Phase 2 Complete When:**
- [ ] /create-harness skill guides users through setup
- [ ] Skill calls scripts (doesn't duplicate logic)
- [ ] Skill validated with non-bootstrap project

**Project Success When:**
- [ ] Another project successfully uses harness framework
- [ ] No questions/confusion during setup
- [ ] Scripts work without modification
