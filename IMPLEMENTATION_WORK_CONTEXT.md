# Implementation Work Context: Bootstrap Validation Framework

**Created:** 2026-01-11 06:52 EST
**Epic:** agentic-primer-6jo - Production-Ready Bootstrap Validation and Testing Framework
**Status:** Ready for Implementation

---

## Executive Summary

We are building production-ready validation and testing infrastructure for the AI Bootstrap Simulation Framework. This framework systematically evaluates 27 simulation scenarios (3 prompt lengths × 3 success criteria × 3 models) to identify the optimal bootstrap configuration for creating git-native issue automation systems.

**Current State:**
- 27 simulation scenarios completed (experiments/iteration-2/runs/run-20260106-003027-full-matrix/)
- Each scenario contains GitHub Actions workflows, templates, and documentation
- Basic syntax validation exists, but functional and quality testing is incomplete
- Manual evaluation takes 8-12 hours; automation will reduce to <2 hours

**Goal:**
Build complete testing infrastructure enabling systematic, automated evaluation of all scenarios through syntax validation, functional testing, and quality scoring.

---

## Project Background

### What We're Testing

The bootstrap simulation framework tests different approaches to creating issue automation:

- **Prompt Variants (P1-P3):** Minimal (10 words) → Moderate (14 words) → Detailed (35 words)
- **Success Criteria (S1-S3):** Minimal (1 req) → Moderate (3 reqs) → Comprehensive (7 outcomes)
- **Models:** Opus, Sonnet, Haiku

Each combination produces a complete implementation with:
- GitHub Actions workflows (.github/workflows/*.yml)
- Issue templates (.github/ISSUE_TEMPLATE/task.yml)
- Knowledge base structure (docs/knowledge/)
- Documentation (README.md, etc.)

### Why Validation Matters

**Without systematic validation:**
- Cannot identify which configuration works best
- Manual evaluation is slow, inconsistent, incomplete
- Cannot prove scenarios work end-to-end (only syntax checked)
- No automated quality benchmarking

**With this infrastructure:**
- Objective, reproducible evaluation across all 27 scenarios
- Functional testing proves implementations work (not just parse)
- Automated scoring identifies top performers
- Reusable framework for future iterations

---

## Work Breakdown Structure

### Phase 1: Mock Infrastructure (P0)

#### Task 1: Mock GitHub API Server (agentic-primer-va1)
**Goal:** Express.js server simulating GitHub REST API for functional testing

**What It Does:**
- Provides endpoints for workflow API calls (comments, labels, assignees, PRs)
- Maintains state in JSON format
- Logs all requests for verification
- Returns realistic GitHub-compatible responses

**Implementation:**
- Create scripts/mock-github-api.js (~100 LOC)
- 5 core endpoints (issues, labels, assignees, pulls, reviews)
- In-memory state with optional JSON persistence
- Request/response logging

**Success Criteria:**
- Server starts/stops programmatically
- All 5 endpoints return valid responses
- State persists across requests
- Logs enable verification of workflow behavior

**Dependencies:** None
**Estimated Effort:** 2-3 hours

---

#### Task 2: Mock Copilot Agent Stub (agentic-primer-gem)
**Goal:** Bash script simulating @copilot agent for workflow testing

**What It Does:**
- Watches copilot-queue/ directory for task assignments
- Processes tasks (JSON with issue data)
- Creates placeholder work artifacts
- Commits to git branches
- Signals completion

**Implementation:**
- Create scripts/mock-copilot-agent.sh (~50 LOC)
- inotify or polling-based file watch
- jq for JSON parsing
- Git operations (branch, commit)
- Daemon mode with clean startup/shutdown

**Success Criteria:**
- Processes task files from queue
- Creates expected workspace artifacts
- Commits to correct branch name
- Multiple tasks handled sequentially
- Clean background operation

**Dependencies:** None
**Estimated Effort:** 1-2 hours

---

#### Task 3: Functional Test Harness (agentic-primer-ser)
**Goal:** Orchestrate mock infrastructure to test scenarios end-to-end

**What It Does:**
- 4-phase testing: Setup → Execute → Verify → Teardown
- Coordinates mock API, mock agent, git remote
- Runs workflows with 'act' (local GitHub Actions runner)
- Verifies state transitions, file creation, git history
- Generates pass/fail reports

**Implementation:**
- Create scripts/test-harness.sh <scenario-dir> (~200 LOC)
- Setup: Start mocks, init state, create bare git remote
- Execute: Generate event payload, run workflow with act
- Verify: Check state.json, files, commits, logs
- Teardown: Stop mocks, cleanup, report results

**Success Criteria:**
- Single command tests one scenario
- All 4 phases execute successfully
- Verification checks 8+ conditions (labels, PRs, commits, etc.)
- Returns exit code 0 (pass) or 1 (fail)
- Structured markdown report generated

**Dependencies:** mock-github-api.js, mock-copilot-agent.sh
**Estimated Effort:** 3-4 hours

---

### Phase 2: Evaluation Automation (P1)

#### Task 4: Automated Evaluation Scoring (agentic-primer-945)
**Goal:** Implement Enhanced Rubric automated scoring (80/120 points)

**What It Does:**
- Scores 4 automatable dimensions:
  - Functional Verification (30 pts): Syntax, triggers, structure
  - Completeness Calibration (25 pts): File count vs optimal range
  - Actionability (15 pts): Placeholder density, documentation
  - Specificity (10 pts): Inappropriate placeholders
- Generates JSON scores per scenario
- Creates aggregate markdown reports

**Implementation:**
- Create scripts/score-scenario.sh <scenario-dir> <prompt-level> (~300 LOC)
- Scoring functions: syntax, structure, completeness, placeholders
- Use yamllint, shellcheck for syntax validation
- File counting with optimal ranges (P1: 6-10, P2: 6-12, P3: 8-15)
- Placeholder detection (grep for TODO/FIXME/@owner)
- JSON output format

**Success Criteria:**
- Scores match manual baseline (P2-S2-sonnet test case)
- All 27 scenarios scored in <5 minutes
- Report shows score distribution and rankings
- Identifies top/bottom performers correctly

**Dependencies:** None (uses standard tools)
**Estimated Effort:** 4-5 hours

---

#### Task 5: Full Validation Pipeline (agentic-primer-ocj)
**Goal:** Orchestrate all validation phases into single command

**What It Does:**
- Single command: validate-all-scenarios.sh <run-dir>
- Phase 1: Syntax validation (all 27 scenarios)
- Phase 2: Functional testing (3-5 representative scenarios)
- Phase 3: Automated scoring (all 27 scenarios)
- Phase 4: Report generation with recommendations
- Parallel execution where possible
- <2 hour total execution time

**Implementation:**
- Create scripts/validate-all-scenarios.sh (~200 LOC)
- Orchestrate existing scripts (validate-scenarios.sh, test-harness.sh, score-scenario.sh)
- Select representative subset for functional testing
- Generate comprehensive report:
  - Summary statistics (pass rates by model/prompt)
  - Top performers and failure analysis
  - Recommendations for next iteration
- Export results to JSON and Markdown

**Success Criteria:**
- All 27 scenarios processed
- Report contains all sections (syntax, functional, scoring, recommendations)
- Execution completes in <2 hours
- Clear pass/fail thresholds (≥80% for pass)
- Actionable recommendations provided

**Dependencies:** test-harness.sh, score-scenario.sh
**Estimated Effort:** 2-3 hours

---

### Phase 3: Reusability (P2)

#### Task 6: Reusable Harness Template (agentic-primer-zwq)
**Goal:** Extract generic framework for other AI evaluation projects

**What It Does:**
- Generic harness template with configuration-driven approach
- Parameterize project-specific values (agent name, domain, etc.)
- Comprehensive documentation for adoption
- Example instantiations (bootstrap + generic task)

**Implementation:**
- Create harness-template/ directory structure
- Replace hardcoded values with {{PLACEHOLDERS}}
- Configuration schema (JSON/YAML)
- CUSTOMIZATION_GUIDE.md with all touchpoints
- 2 working examples

**Success Criteria:**
- Template instantiable for new projects
- All placeholders documented
- Configuration drives behavior
- External user can adopt in <4 hours
- Complete epic agentic-primer-rin

**Dependencies:** None (uses completed infrastructure)
**Estimated Effort:** 4-5 hours

---

## Success Criteria & Measurement

### Overall Success Metrics

**Validation Coverage:**
- [x] Syntax validation: 100% of scenarios (27/27)
- [ ] Functional testing: ≥18% of scenarios (5/27 representative)
- [ ] Automated scoring: 100% of scenarios (27/27)
- [ ] Manual evaluation time reduction: 83% (12h → 2h)

**Quality Thresholds:**
- Pass rate: ≥80% scenarios pass validation
- Automated scoring accuracy: ≥90% agreement with manual baseline
- Functional test reliability: 100% reproducible results
- Pipeline execution time: ≤2 hours for full suite

**Deliverables:**
1. Working mock infrastructure (API + agent + harness)
2. Automated scoring system (80/120 points)
3. Full validation pipeline (single command)
4. Comprehensive validation report for run-20260106-003027-full-matrix
5. Reusable harness template
6. Documentation enabling external adoption

---

## Implementation Strategy

### Parallel Execution Plan

**Week 1 - Foundation (P0 Tasks):**
- Task 1 + Task 2 can run in parallel (independent)
- Task 3 waits for Task 1 & 2 to complete
- Focus: Get functional testing working end-to-end

**Week 2 - Automation (P1 Tasks):**
- Task 4 can run independently
- Task 5 waits for Task 3 & 4
- Focus: Automated evaluation at scale

**Week 3 - Polish (P2 Tasks):**
- Task 6 extracts learnings from completed infrastructure
- Focus: Documentation and reusability

### Recommended Approach for Background Workers

**High-Value Parallelization:**
1. Start Task 1 (Mock API) in background worker
2. Start Task 2 (Mock Agent) in different background worker
3. When both complete, start Task 4 (Scoring) in parallel with Task 3 (Harness)
4. When Task 3 & 4 complete, start Task 5 (Pipeline)
5. Task 6 can start anytime after Task 5

**Estimated Timeline:**
- Week 1: 6-9 hours (P0 tasks)
- Week 2: 6-8 hours (P1 tasks)
- Week 3: 4-5 hours (P2 tasks)
- **Total:** 16-22 hours

---

## Key Technical Details

### Mock Infrastructure Specifications

**GitHub API Mock (agentic-primer-va1):**
```javascript
// Endpoints
POST /repos/:owner/:repo/issues/:number/comments
POST /repos/:owner/:repo/issues/:number/labels
POST /repos/:owner/:repo/issues/:number/assignees
POST /repos/:owner/:repo/pulls
POST /repos/:owner/:repo/pulls/:number/reviews

// State format
{
  "issues": {
    "1": {
      "comments": ["Processing...", "Complete"],
      "labels": ["copilot", "processing"],
      "assignees": ["owner"]
    }
  },
  "prs": {
    "1": {
      "title": "feat: implement issue #1",
      "head": "copilot/issue-1",
      "base": "main"
    }
  }
}
```

**Copilot Agent Mock (agentic-primer-gem):**
```bash
# Task file format (copilot-queue/task-1.json)
{
  "issue": {
    "number": 1,
    "title": "Add unit tests",
    "body": "Create comprehensive tests..."
  }
}

# Agent actions
1. Read task from queue
2. Create branch: copilot/issue-{number}
3. Create files in copilot-workspace/issue-{number}/
4. Commit with message: "feat: implement issue #{number}"
5. Remove task file (signal completion)
```

### Scoring Rubric Details

**Automated Dimensions (80 points):**

1. **Functional Verification (30 pts):**
   - Syntax validity: yamllint + shellcheck (15 pts)
   - Workflow triggers: Correct event types (10 pts)
   - Structure correctness: Proper .github/ hierarchy (5 pts)

2. **Completeness Calibration (25 pts):**
   - File count vs optimal:
     - P1: 6-10 files = 100%, outside range = penalty
     - P2: 6-12 files = 100%, outside range = penalty
     - P3: 8-15 files = 100%, outside range = penalty
   - Penalizes over-engineering for minimal prompts

3. **Actionability (15 pts):**
   - Placeholder density: Count TODO/FIXME (10 pts)
   - Documentation quality: README exists and complete (5 pts)

4. **Specificity (10 pts):**
   - Inappropriate placeholders: @owner, YOUR_REPO, etc. (10 pts)

**Manual Dimensions (40 points):**
- Correctness (20 pts): Logic trace, semantic validity
- Research Quality (15 pts): WebSearch usage, current sources
- Insight Quality (5 pts): Novel approaches, explicit assumptions

### Test Harness Verification Checks

**8 Standard Verification Points:**
1. Workflow triggered on correct event (issues.opened or issues.labeled)
2. Labels added to issue (copilot, processing, etc.)
3. Comment posted to issue (acknowledgment)
4. Branch created with expected name (copilot/issue-N)
5. Commit made with expected message pattern
6. PR created with correct title/body
7. PR links to issue ("Closes #N")
8. Files created in expected locations

---

## Risk Assessment & Mitigation

### Technical Risks

**Risk 1: Mock infrastructure too complex**
- **Probability:** Medium
- **Impact:** High (blocks functional testing)
- **Mitigation:** Start minimal (~150 LOC), iterate based on actual needs
- **Fallback:** Static analysis covers 50% of evaluation without mocks

**Risk 2: 'act' compatibility issues**
- **Probability:** Medium
- **Impact:** Medium (some workflows may not run)
- **Mitigation:** Test against known-good scenario first (P2-S2-sonnet)
- **Fallback:** Document incompatibilities, recommend GitHub Actions live testing

**Risk 3: Automated scoring unreliable**
- **Probability:** Low
- **Impact:** Medium (loses trust in automation)
- **Mitigation:** Validate against manual baseline, tune thresholds
- **Acceptance:** 90% agreement with manual scores

### Schedule Risks

**Risk 4: Timeline too aggressive**
- **Probability:** Medium
- **Impact:** Low (P2 tasks are enhancements, not critical)
- **Mitigation:** P0/P1 provide core value, P2 can slip
- **Priority:** Phase 1 > Phase 2 > Phase 3

**Risk 5: Parallel execution conflicts**
- **Probability:** Low
- **Impact:** Low (just sequential fallback)
- **Mitigation:** Tasks 1 & 2 are independent, no shared state
- **Fallback:** Run sequentially if needed

---

## Dependencies & Prerequisites

### External Tools Required

**For Mock Infrastructure:**
- Node.js + Express.js (API server)
- jq (JSON parsing in bash)
- git (version control operations)

**For Functional Testing:**
- act (local GitHub Actions runner) - `brew install act`
- Docker (act runs workflows in containers)

**For Automated Scoring:**
- yamllint - `brew install yamllint`
- shellcheck - `brew install shellcheck`
- jq (JSON scoring output)

**For Validation Pipeline:**
- All of the above
- Parallel execution: GNU parallel (optional)

### Project Files Referenced

**Existing Documentation:**
- EVALUATOR_REQUIREMENTS_AND_MOCK_SCAFFOLDING.md (mock specs)
- SIMULATION_HARNESS.md (testing methodology)
- experiments/iteration-2/runs/run-20260106-003027-full-matrix/ENHANCED_RUBRIC.md (scoring system)
- PRODUCTION_READY_VALIDATION.md (quality pattern)

**Existing Scripts:**
- experiments/iteration-2/runs/run-20260106-003027-full-matrix/validate-scenarios.sh (syntax validation)

**Target Scenarios:**
- experiments/iteration-2/runs/run-20260106-003027-full-matrix/P*-S*-*/ (27 directories)

---

## Next Steps for Implementation

### Immediate Actions (Next 30 Minutes)

1. **Review all bead descriptions:**
   ```bash
   bd show agentic-primer-va1  # Mock API
   bd show agentic-primer-gem  # Mock Agent
   bd show agentic-primer-ser  # Test Harness
   bd show agentic-primer-945  # Scoring
   bd show agentic-primer-ocj  # Pipeline
   bd show agentic-primer-zwq  # Reusable Template
   ```

2. **Verify dependencies installed:**
   ```bash
   node --version     # Node.js for Express
   jq --version       # JSON parsing
   act --version      # GitHub Actions runner
   yamllint --version # YAML validation
   shellcheck --version # Shell script validation
   ```

3. **Identify baseline scenario for testing:**
   - Use P2-S2-sonnet as reference (known-good)
   - Verify files exist and workflows parse

### Background Task Assignments

**Worker 1 - Mock API Server (agentic-primer-va1):**
```bash
# Context: Build Express.js mock GitHub API
# Output: scripts/mock-github-api.js
# Success: Server starts, endpoints return valid responses
# Time: 2-3 hours
```

**Worker 2 - Mock Copilot Agent (agentic-primer-gem):**
```bash
# Context: Build bash daemon simulating @copilot
# Output: scripts/mock-copilot-agent.sh
# Success: Processes tasks, creates artifacts, commits
# Time: 1-2 hours
```

**Worker 3 - Wait for Workers 1 & 2, then Test Harness (agentic-primer-ser):**
```bash
# Context: Orchestrate mocks into end-to-end test
# Output: scripts/test-harness.sh
# Success: Full scenario test passes
# Time: 3-4 hours
```

**Worker 4 - Parallel with Worker 3, Scoring System (agentic-primer-945):**
```bash
# Context: Automate Enhanced Rubric scoring
# Output: scripts/score-scenario.sh
# Success: Scores 27 scenarios in <5 min
# Time: 4-5 hours
```

### Coordination Points

**Checkpoint 1 (After Workers 1 & 2):**
- Verify both mocks work independently
- Test mock API with curl
- Test mock agent with sample task file
- Decision: Proceed to Worker 3 or iterate

**Checkpoint 2 (After Workers 3 & 4):**
- Test harness passes on P2-S2-sonnet
- Scoring system validates against manual baseline
- Decision: Proceed to Worker 5 (pipeline) or refine

**Checkpoint 3 (After Worker 5):**
- Full pipeline runs on all 27 scenarios
- Report generated with recommendations
- Decision: Proceed to Worker 6 (reusability) or iterate

---

## Expected Outcomes

### Primary Deliverables

1. **Mock Infrastructure** (Phase 1):
   - scripts/mock-github-api.js (Express server)
   - scripts/mock-copilot-agent.sh (bash daemon)
   - scripts/test-harness.sh (orchestrator)

2. **Evaluation Automation** (Phase 2):
   - scripts/score-scenario.sh (rubric scorer)
   - scripts/validate-all-scenarios.sh (full pipeline)

3. **Validation Report** (Phase 2):
   - experiments/iteration-2/runs/run-20260106-003027-full-matrix/VALIDATION_REPORT.md
   - Clear recommendation on optimal configuration

4. **Reusable Template** (Phase 3):
   - harness-template/ directory structure
   - Documentation enabling external adoption

### Success Indicators

**Technical Success:**
- All 27 scenarios validated through automated pipeline
- ≥5 scenarios pass functional testing
- Automated scoring reduces evaluation time 83% (12h → 2h)
- Clear identification of top-performing configuration

**Process Success:**
- Framework reusable for iteration 3
- Documentation enables external team to adopt
- Infrastructure proven through actual use

**Business Success:**
- Recommendation on optimal bootstrap prompt + criteria
- Data-driven decision on next iteration focus
- Validated approach to AI evaluation at scale

---

## Questions & Clarifications

### For Human Review

1. **Priority Confirmation:**
   - Is P0 (mock infrastructure + functional testing) the right focus?
   - Should we deprioritize P2 (reusability) if timeline is tight?

2. **Scope Boundaries:**
   - Is 5/27 scenarios sufficient for functional testing?
   - Should we test all 27 functionally (more comprehensive but slower)?

3. **Quality Thresholds:**
   - Is ≥80% pass rate acceptable, or should we target 90%+?
   - What's minimum acceptable automated scoring accuracy?

4. **External Dependencies:**
   - Are all required tools (act, yamllint, etc.) available?
   - Any restrictions on using Docker for 'act'?

### For Implementation Team

1. **Technical Decisions:**
   - Express.js version for mock API?
   - State persistence: In-memory vs JSON file?
   - Logging format: JSON vs plain text?

2. **Testing Strategy:**
   - Should functional tests run in CI/CD?
   - Need for test fixtures/snapshots?

3. **Documentation:**
   - What level of detail for setup instructions?
   - Video walkthrough needed?

---

## References

**Key Documents:**
- PROJECT_OVERVIEW.md - Project vision and goals
- SIMULATION_HARNESS.md - Testing methodology (3-phase + evaluation)
- EVALUATOR_REQUIREMENTS_AND_MOCK_SCAFFOLDING.md - Mock infrastructure specs
- PRODUCTION_READY_VALIDATION.md - Quality pattern validation
- experiments/iteration-2/runs/run-20260106-003027-full-matrix/ENHANCED_RUBRIC.md - 120-point scoring system

**Beads:**
- agentic-primer-6jo (Epic) - Overall project tracker
- agentic-primer-va1 - Mock GitHub API
- agentic-primer-gem - Mock Copilot Agent
- agentic-primer-ser - Functional Test Harness
- agentic-primer-945 - Automated Scoring
- agentic-primer-ocj - Validation Pipeline
- agentic-primer-zwq - Reusable Template

**External Resources:**
- act documentation: https://github.com/nektos/act
- GitHub REST API: https://docs.github.com/en/rest
- Enhanced Rubric: experiments/iteration-2/runs/run-20260106-003027-full-matrix/ENHANCED_RUBRIC.md

---

**Document Version:** 1.0
**Last Updated:** 2026-01-11 06:52 EST
**Status:** Ready for Implementation
**Next Review:** After Phase 1 completion (Checkpoint 1)
