# Quick Start: Bootstrap Validation Implementation

**Date:** 2026-01-11
**Epic:** agentic-primer-6jo
**Full Context:** IMPLEMENTATION_WORK_CONTEXT.md

---

## What We're Building

Production-ready validation framework for 27 AI bootstrap simulation scenarios.

**Goal:** Systematically evaluate which prompt + criteria + model combination produces the best issue automation system.

---

## Work Packages (6 Beads)

### Phase 1: Mock Infrastructure (P0) - ~6-9 hours

**1. Mock GitHub API Server** (agentic-primer-va1) - 2-3h
- Express.js server with 5 endpoints
- State tracking in JSON
- Enables functional testing of workflows

**2. Mock Copilot Agent** (agentic-primer-gem) - 1-2h
- Bash daemon watching task queue
- Simulates @copilot work
- Creates artifacts and commits

**3. Functional Test Harness** (agentic-primer-ser) - 3-4h
- Orchestrates mock infrastructure
- 4-phase testing (setup/execute/verify/teardown)
- End-to-end scenario validation

### Phase 2: Evaluation Automation (P1) - ~6-8 hours

**4. Automated Scoring System** (agentic-primer-945) - 4-5h
- Implements 80/120 points of Enhanced Rubric
- Scores all 27 scenarios in <5 min
- Reduces manual evaluation 83%

**5. Full Validation Pipeline** (agentic-primer-ocj) - 2-3h
- Single command validates all scenarios
- Syntax + Functional + Scoring + Report
- <2 hour execution for 27 scenarios

### Phase 3: Reusability (P2) - ~4-5 hours

**6. Reusable Harness Template** (agentic-primer-zwq) - 4-5h
- Extracts generic framework
- Configuration-driven approach
- Enables external adoption

---

## Dependency Graph

```
agentic-primer-6jo (Epic)
├── agentic-primer-va1 (Mock API)
├── agentic-primer-gem (Mock Agent)
├── agentic-primer-ser (Test Harness) [needs: va1, gem]
├── agentic-primer-945 (Scoring) [independent]
├── agentic-primer-ocj (Pipeline) [needs: ser, 945]
└── agentic-primer-zwq (Template) [needs: ocj complete]
```

**Parallel Execution:**
- Start: va1 + gem simultaneously
- Then: ser + 945 simultaneously
- Then: ocj
- Finally: zwq

---

## Quick Commands

### View Beads
```bash
bd show agentic-primer-6jo   # Epic overview
bd list --epic agentic-primer-6jo  # All child beads
bd ready  # See what's ready to work on
```

### Start Work
```bash
bd update agentic-primer-va1 --status in_progress
# Do the work
bd update agentic-primer-va1 --status closed
```

### Check Dependencies
```bash
bd dep list agentic-primer-ser  # What blocks this?
bd blocked  # What's blocked and why?
```

---

## Success Criteria at a Glance

**Must Have:**
- [x] All 27 scenarios validated (syntax)
- [ ] ≥5 scenarios pass functional testing
- [ ] Automated scoring operational (80/120 pts)
- [ ] Full pipeline runs in <2 hours
- [ ] Validation report with recommendations

**Nice to Have:**
- [ ] All 27 scenarios functionally tested
- [ ] Reusable template documented
- [ ] External team can adopt framework

---

## Key Files & Locations

**Target Scenarios:**
```
experiments/iteration-2/runs/run-20260106-003027-full-matrix/
├── P1-S1-haiku/
├── P1-S1-opus/
├── P1-S1-sonnet/
... (27 total directories)
```

**Outputs to Create:**
```
scripts/
├── mock-github-api.js        (va1)
├── mock-copilot-agent.sh     (gem)
├── test-harness.sh           (ser)
├── score-scenario.sh         (945)
└── validate-all-scenarios.sh (ocj)

harness-template/             (zwq)
├── README.md
├── config.schema.json
└── scripts/
```

**Documentation:**
```
IMPLEMENTATION_WORK_CONTEXT.md  (full details)
IMPLEMENTATION_QUICK_START.md   (this file)
experiments/.../VALIDATION_REPORT.md (output)
```

---

## Prerequisites

**Tools Needed:**
```bash
# Check installations
node --version      # Node.js for Express
jq --version        # JSON parsing
act --version       # GitHub Actions runner
yamllint --version  # YAML validation
shellcheck --version # Shell validation

# Install if missing (macOS)
brew install node jq act yamllint shellcheck
```

**Test Files:**
- Use P2-S2-sonnet as baseline (known-good scenario)
- Location: experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-sonnet/

---

## Time Estimates

**Phase 1 (P0):** 6-9 hours
- Critical path: va1 (2-3h) → ser (3-4h)
- Parallel: gem (1-2h) alongside va1

**Phase 2 (P1):** 6-8 hours
- Parallel: 945 (4-5h) while ser completes
- Then: ocj (2-3h)

**Phase 3 (P2):** 4-5 hours
- zwq (4-5h) after everything works

**Total:** 16-22 hours over 2-3 weeks

---

## Next Actions

### Immediate (Next Hour)
1. Read IMPLEMENTATION_WORK_CONTEXT.md (comprehensive details)
2. Verify all prerequisites installed
3. Review baseline scenario (P2-S2-sonnet)
4. Choose first task (recommend: va1 + gem in parallel)

### Background Task Assignments

**Assign to Worker 1:**
```
Bead: agentic-primer-va1 (Mock GitHub API)
Context: IMPLEMENTATION_WORK_CONTEXT.md "Task 1"
Output: scripts/mock-github-api.js
Success: Server starts, all endpoints work
Time: 2-3 hours
```

**Assign to Worker 2:**
```
Bead: agentic-primer-gem (Mock Copilot Agent)
Context: IMPLEMENTATION_WORK_CONTEXT.md "Task 2"
Output: scripts/mock-copilot-agent.sh
Success: Processes tasks, creates commits
Time: 1-2 hours
```

**Wait for Workers 1 & 2, then Worker 3:**
```
Bead: agentic-primer-ser (Test Harness)
Context: IMPLEMENTATION_WORK_CONTEXT.md "Task 3"
Dependencies: va1, gem
Output: scripts/test-harness.sh
Success: P2-S2-sonnet passes end-to-end
Time: 3-4 hours
```

**Parallel with Worker 3, Worker 4:**
```
Bead: agentic-primer-945 (Scoring System)
Context: IMPLEMENTATION_WORK_CONTEXT.md "Task 4"
Dependencies: None
Output: scripts/score-scenario.sh
Success: 27 scenarios scored in <5 min
Time: 4-5 hours
```

---

## Checkpoints

**Checkpoint 1: After Phase 1 (P0)**
- Mock API responds to curl requests
- Mock agent processes sample task
- Test harness runs full cycle on P2-S2-sonnet
- Decision: Proceed to Phase 2 or iterate

**Checkpoint 2: After Phase 2 (P1)**
- Scoring matches manual baseline
- Pipeline validates all 27 scenarios
- Report generated with recommendations
- Decision: Proceed to Phase 3 or refine

**Checkpoint 3: After Phase 3 (P2)**
- Template documented and tested
- External adoption possible
- Project complete

---

## Questions?

**For detailed context:** See IMPLEMENTATION_WORK_CONTEXT.md
**For bead details:** `bd show <bead-id>`
**For technical specs:** See EVALUATOR_REQUIREMENTS_AND_MOCK_SCAFFOLDING.md

---

**Version:** 1.0
**Status:** Ready for Implementation
**Last Updated:** 2026-01-11
