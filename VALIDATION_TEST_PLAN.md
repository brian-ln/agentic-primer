# Validation Test Plan: Production-Ready Prompt

**Date:** 2026-01-06
**Purpose:** Validate improved CONSTRAINTS prompt before full re-run
**Approach:** Option A - Test across prompt lengths with Sonnet

---

## Test Configuration

**Runs:** 3 simulations (P1/P2/P3 × S2 × Sonnet)
- **P1-S2-sonnet-VALIDATION** - Minimal prompt (10 words)
- **P2-S2-sonnet-VALIDATION** - Moderate prompt (14 words)
- **P3-S2-sonnet-VALIDATION** - Detailed prompt (35 words)

**Model:** Sonnet 4.5 (balanced quality/cost)
**Success Criteria:** S2-moderate (3 requirements)

---

## Prompt Details

### P1: Minimal (10 words)
```
Bootstrap @copilot issue automation with auto-review and knowledge base.
```

### P2: Moderate (14 words)
```
Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base.
```

### P3: Detailed (35 words)
```
Create issue-driven development system:
- Issue template (.github/ISSUE_TEMPLATE/task.yml) for @copilot tasks
- CODEOWNERS (* @owner) for PR auto-assignment
- Knowledge base (docs/knowledge/) with patterns/decisions/insights structure
- README with workflow: issue → @copilot → PR → review via web UI
```

### Success Criteria (S2-moderate)
```
Generated system must:
- Process test issue end-to-end without errors
- Pass syntax validation (yamllint, shellcheck)
- GitHub workflow triggers on issue creation
```

---

## CONSTRAINTS (Improved Pattern)

```
Production-ready, complete code, no placeholders.
Simulate GitHub operations (no APIs, git push, or state changes).
Output: experiments/iteration-2/runs/[run-name]/[P#]-[S#]-[model]/
```

**vs Old CONSTRAINTS:**
```
- Use web search and fetch and any other tools @copilot would typically use
- DO NOT ACTUALLY CALL ANY GITHUB APIS, PROGRAMS, or SCRIPTS to do the work
- Simulate them being called if necessary
```

**Key improvements:**
- 44% fewer tokens (18 vs 32 words)
- "Production-ready" sets quality bar
- Clear simulation boundary
- No ambiguity about WebSearch

---

## Validation Criteria

### Must-Have (Pass/Fail)

**1. WebSearch Usage:**
- [ ] P1-S2-sonnet: Used WebSearch (at least 1 query)
- [ ] P2-S2-sonnet: Used WebSearch (at least 1 query)
- [ ] P3-S2-sonnet: Used WebSearch (at least 1 query)

**2. Research Evidence:**
- [ ] P1: Cites sources or current best practices
- [ ] P2: Cites sources or current best practices
- [ ] P3: Cites sources or current best practices

**3. Completeness Calibration:**
- [ ] P1: ~65-70% complete (structure, minimal implementation)
- [ ] P2: ~70-75% complete (functional foundation)
- [ ] P3: ~75-85% complete (near production-ready)

**4. No Placeholders:**
- [ ] P1: 0 TODOs/FIXMEs/PLACEHOLDERs
- [ ] P2: 0 TODOs/FIXMEs/PLACEHOLDERs
- [ ] P3: 0 TODOs/FIXMEs/PLACEHOLDERs

### Nice-to-Have (Quality Indicators)

**5. Decision Documentation:**
- [ ] P1: Documented assumptions/rationale
- [ ] P2: ADRs or trade-off analysis
- [ ] P3: Comprehensive documentation

**6. Behavioral Consistency:**
- [ ] All 3 show similar research patterns
- [ ] Quality scales with prompt length
- [ ] No over-engineering in P1

---

## Execution Timeline

**Current Status:** Rate limited until Jan 8, 8:00 AM EST

**After Rate Limit Clears:**

1. **Prepare Output Directories (5 min)**
   ```bash
   mkdir -p experiments/iteration-2/runs/run-validation-20260108/
   mkdir -p experiments/iteration-2/runs/run-validation-20260108/P1-S2-sonnet-VALIDATION
   mkdir -p experiments/iteration-2/runs/run-validation-20260108/P2-S2-sonnet-VALIDATION
   mkdir -p experiments/iteration-2/runs/run-validation-20260108/P3-S2-sonnet-VALIDATION
   ```

2. **Run 3 Simulations in Parallel (10-15 min)**
   - Launch all 3 Task agents simultaneously
   - Track agent IDs
   - Wait for completion

3. **Verify Results (10 min)**
   - Check for WebSearch tool calls in outputs
   - Count TODOs/placeholders
   - Assess completeness levels
   - Review research citations

4. **Make Decision (5 min)**
   - **If validation passes:** Proceed with full re-run (18 runs)
   - **If validation fails:** Adjust CONSTRAINTS, re-test
   - **If mixed results:** Investigate and iterate

**Total Time:** ~30-40 minutes

---

## Task Prompts (Ready to Use)

### P1-S2-sonnet-VALIDATION

```
You are simulating @copilot the GitHub CoPilot agent that can do work autonomously when assigned issues.

PROMPT:
Bootstrap @copilot issue automation with auto-review and knowledge base.

SUCCESS CRITERIA:
Generated system must:
- Process test issue end-to-end without errors
- Pass syntax validation (yamllint, shellcheck)
- GitHub workflow triggers on issue creation

YOUR TASK:
Acting as @copilot and given the PROMPT and the SUCCESS CRITERIA design the solution, describe it in a single markdown file, then implement and verify it.

Remember, this is a simulation and you are pretending to act just like @copilot would. DO NOT ACTUALLY CALL ANY GITHUB APIS, PROGRAMS, or SCRIPTS to do the work. Simulate them being called if necessary.

List all files @copilot would create (file paths) as a part of the solution.

For each file provide:
- Purpose (1 sentence)
- Complete functional content (no placeholders)
- Assumptions made
- How and why @copilot decided it was necessary

OUTPUT: Markdown document with complete solution.

OUTPUT LOCATION: experiments/iteration-2/runs/run-validation-20260108/P1-S2-sonnet-VALIDATION/
Example: experiments/iteration-2/runs/run-20260106-001203-full-matrix/P1-S1-opus/

IMPORTANT: Put ALL files you create in the OUTPUT LOCATION directory above.

CONSTRAINTS:
Production-ready, complete code, no placeholders.
Simulate GitHub operations (no APIs, git push, or state changes).
Output: experiments/iteration-2/runs/run-validation-20260108/P1-S2-sonnet-VALIDATION/
```

### P2-S2-sonnet-VALIDATION

```
[Same structure, replace PROMPT with P2, update OUTPUT LOCATION to P2-S2-sonnet-VALIDATION/]
```

### P3-S2-sonnet-VALIDATION

```
[Same structure, replace PROMPT with P3, update OUTPUT LOCATION to P3-S2-sonnet-VALIDATION/]
```

---

## Success Metrics

### Validation PASSES if:
- **3/3 runs use WebSearch** (primary goal: fix emulation gap)
- **3/3 cite research sources** (evidence of real research)
- **Completeness calibrates:** P1 < P2 < P3 (not all over-engineered)
- **0 placeholders across all 3** (production-ready standard maintained)

### What This Proves:
✅ Improved CONSTRAINTS prompt achieves intended behavior change
✅ "Production-ready" drives quality without over-engineering
✅ Agents understand simulation boundaries (research OK, GitHub API not OK)
✅ Pattern works across prompt lengths

### If Validation Passes → Next Step:
**Run full re-run:** 18 simulations (9 P1 + 9 P2) + 9 P3 = 27 total
- Use same CONSTRAINTS pattern
- Expect consistent WebSearch usage
- Valid comparison of P1 vs P2 vs P3 effects

---

## Comparison Baseline

### Old Batch 1+2 (for reference):
- **WebSearch usage:** 0/18 runs (0%)
- **Completeness:** 85-95% (over-engineered)
- **Research citations:** Training data only, no current sources
- **Placeholders:** 0-3 per run (good)

### Expected Validation Results:
- **WebSearch usage:** 3/3 runs (100%)
- **Completeness:** 65-85% (calibrated to prompt length)
- **Research citations:** 2026 current sources
- **Placeholders:** 0 (maintained standard)

**Key improvement:** WebSearch 0% → 100%, Calibrated completeness

---

## Failure Scenarios & Mitigations

### Scenario 1: No WebSearch Usage
**Symptom:** Agents still don't use WebSearch despite new CONSTRAINTS
**Diagnosis:** Pattern still ambiguous or agents defaulting to training data
**Mitigation:** Add explicit "Research using WebSearch" instruction

### Scenario 2: Still Over-Engineering
**Symptom:** All 3 runs are 90%+ complete
**Diagnosis:** "Production-ready" interpreted as "deliver everything"
**Mitigation:** Add calibration guidance based on prompt length

### Scenario 3: Quality Drops
**Symptom:** Placeholders appear, syntax errors increase
**Diagnosis:** "Production-ready" not strong enough quality signal
**Mitigation:** Add "Pass syntax validation" to CONSTRAINTS

### Scenario 4: Mixed Results
**Symptom:** 2/3 pass, 1 fails
**Diagnosis:** Inconsistent behavior, pattern works but not reliably
**Mitigation:** Iterate on failed case, identify what differed

---

## Documentation of Results

After validation run, create:

**VALIDATION_RESULTS.md** containing:
1. Actual WebSearch usage (tool call counts)
2. Completeness percentages (file counts, line counts)
3. Research citations found
4. Placeholder counts
5. Pass/fail on each criterion
6. Recommendation (proceed or iterate)
7. Observations and insights

---

## Ready to Execute

**Waiting for:** Rate limit reset (Jan 8, 8:00 AM EST)

**Time remaining:** ~12 hours (as of Jan 6, 7:45 PM EST)

**When ready:**
1. Run the 3 validation tests
2. Verify results against criteria
3. Make go/no-go decision on full re-run

**If validation passes:**
- Proceed with confidence to full 27-run matrix
- Document methodology for future iterations

**If validation needs iteration:**
- Adjust CONSTRAINTS based on findings
- Re-test with 3 runs
- Iterate until validation passes
