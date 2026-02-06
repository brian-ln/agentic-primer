# Bootstrap Simulation Harness v2

Test framework for evaluating bootstrap prompts across models and success criteria variations.

---

## Test Matrix

Test 3 prompt lengths × 3 success criteria levels × 3 models = **27 simulations**

### Prompt Variants

Prompts are stored in `experiments/iteration-2/prompts/`:

- **P1** - `P1-minimal.txt` (10 words)
- **P2** - `P2-moderate.txt` (14 words)
- **P3** - `P3-detailed.txt` (35 words)

### Success Criteria Variants

Criteria are stored in `experiments/iteration-2/criteria/`:

- **S1** - `S1-minimal.txt` (single requirement)
- **S2** - `S2-moderate.txt` (3 requirements)
- **S3** - `S3-comprehensive.txt` (7 observable outcomes)

### Models

- **Opus** (claude-opus-4-5)
- **Sonnet** (claude-sonnet-4-5)
- **Haiku** (claude-haiku-4)

---

## Output File Organization

**Directory Structure for Runs:**

Each test configuration gets its own subdirectory in the run folder:

```
experiments/iteration-2/runs/run-YYYYMMDD-HHMMSS-full-matrix/
├── config.json
├── README.md
├── logs/
├── results/
├── P1-S1-opus/       # Simulation outputs for P1+S1+opus
├── P1-S1-sonnet/     # Simulation outputs for P1+S1+sonnet
├── P1-S1-haiku/      # Simulation outputs for P1+S1+haiku
├── P1-S2-opus/
├── P1-S2-sonnet/
... (27 total test directories)
```

**What Goes in Each Test Directory:**
- Markdown files created by simulation agents
- Solution design documents
- File manifests
- Any other artifacts generated during the simulation

**Important:** Agents should NOT create files in the main project directory. All simulation outputs belong in their respective test subdirectories.

---

## Three-Phase Methodology

### Phase 1: Simulation

Launch agent using Task tool:
```
Task(
  subagent_type="general-purpose",
  model=[opus/sonnet/haiku],
  description="[P#]+[S#]+[model] simulation"
)
```

**Prompt Template:**
```
You are simulating @copilot the GitHub CoPilot agent that can do work autonomously when assigned issues.

PROMPT:
[INSERT: P1, P2, or P3 content]

SUCCESS CRITERIA:
[INSERT: S1, S2, or S3 content]

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

OUTPUT LOCATION: experiments/iteration-2/runs/[run-name]/[P#]-[S#]-[model]/
Example: experiments/iteration-2/runs/run-20260106-001203-full-matrix/P1-S1-opus/

IMPORTANT: Put ALL files you create in the OUTPUT LOCATION directory above.

CONSTRAINTS:
Production-ready, complete code, no placeholders.
Simulate GitHub operations (no APIs, git push, or state changes).
Output: experiments/iteration-2/runs/[run-name]/[P#]-[S#]-[model]/
```

**Save agent ID** (e.g., ae913f4) - you'll need it for Phase 1b.

---

### Phase 1b: Self-Reflection (Resume)

Use Task tool with **resume** parameter:
```
Task(
  resume=[agent-id from Phase 1],
  description="[P#]+[S#]+[model] self-reflection"
)
```

**Resume Prompt:**
```
Now self-reflect on how well you think you did as @copilot and what could have produced better outcomes.

Analyze:
- Confidence level for each file (High/Medium/Low)
- Missing information from prompt needed to meet success criteria
- Research and findings:
  - What was researched
  - Why
  - What were findings
- What would you do differently?

OUTPUT: Self-reflection analysis in markdown format.
```

**Agent ID remains the same** (it's a continuation).

---

### Phase 2: Independent Evaluation

Launch NEW Sonnet agent using Task tool:
```
Task(
  subagent_type="general-purpose",
  model="sonnet",
  description="Evaluate [P#]+[S#]+[model]"
)
```

**Evaluation Prompt:**
```
Evaluate this simulation of how @copilot performed using the rubric below.

PROMPT: [Original prompt]
SUCCESS CRITERIA: [Original criteria]
SIMULATION OUTPUT: [Phase 1 agent's output]
SELF-REFLECTION: [Phase 1b agent's output]

RUBRIC (100 points):

1. COMPLETENESS (30 points)
   - File coverage: Expected vs actual files created
   - Content depth: Complete vs placeholder content

2. CORRECTNESS (25 points)
   - Syntax validity: yamllint, shellcheck, markdownlint pass
   - Semantic correctness: Would it actually work?

3. ACTIONABILITY (20 points)
   - Ready to use immediately? (20)
   - Minor tweaks needed? (15)
   - Significant rework required? (5)

4. SPECIFICITY (15 points)
   - Placeholder density (count TODOs, FIXMEs, etc.)
   - Contextual appropriateness for the prompt

5. INSIGHT QUALITY (10 points)
   - Novel approaches identified
   - Assumptions explicitly stated
   - Edge cases noted

SCORE EACH DIMENSION, PROVIDE TOTAL /100, GRADE (A/B/C/D/F)

SUMMARIZE:
- @copilot's approach (from compact agent session logs)
- Self-reflection insights
- Alignment between what was done vs what was reflected

COMMENTARY:
- 2-3 Strengths
- 2-3 Weaknesses
- Recommendations for improvement
```

**Save evaluator agent ID** (e.g., a0294d5).

---

### Phase 3: Aggregate Analysis (After all runs)

After completing all 27 simulations, launch ONE analysis agent:

**Aggregate Analysis Prompt:**
```
Analyze results across all 27 simulation runs.

RUNS:
[Include all evaluation results from Phase 2 agents - 27 evaluations]

ANALYSIS TASKS:
1. Score distribution by dimension (Completeness, Correctness, Actionability, Specificity, Insight)
2. Model performance comparison (opus vs sonnet vs haiku)
3. Prompt length impact (P1 vs P2 vs P3)
4. Success criteria impact (S1 vs S2 vs S3)
5. Identify optimal configuration (highest scoring combination)
6. Pattern analysis (what correlates with high scores?)
7. Recommendations for iteration 3

OUTPUT: Comprehensive comparison markdown with:
- Summary tables (scores by model, by prompt, by criteria)
- Best/worst performers
- Key findings
- Statistical insights
- Next iteration recommendations
```

---

## Running Single Test

**Step 1 - Create run directory:**
```bash
./scripts/create-experiment-run.sh iteration-2 P3-S2-sonnet
# Creates: experiments/iteration-2/runs/run-20260106-001234-P3-S2-sonnet/
```

**Step 2 - Phase 1 (Simulation):**
Launch Task with simulation prompt. Save agent ID.

**Step 3 - Phase 1b (Self-Reflection):**
Resume same agent with reflection prompt.

**Step 4 - Phase 2 (Evaluation):**
Launch new Sonnet evaluator. Save agent ID.

**Step 5 - Finalize:**
```bash
./scripts/finalize-experiment-run.sh iteration-2/runs/run-20260106-001234-P3-S2-sonnet <sim-id> <eval-id>
```

---

## Running Full Matrix (27 Tests)

**Batch Strategy:** 3 batches of 9 (grouped by prompt)

### Batch 1 - P1 × All Criteria × All Models

**Phase 1 - Launch 9 simulations in parallel:**
In a SINGLE message, launch 9 Task calls:
1. P1+S1+opus
2. P1+S1+sonnet
3. P1+S1+haiku
4. P1+S2+opus
5. P1+S2+sonnet
6. P1+S2+haiku
7. P1+S3+opus
8. P1+S3+sonnet
9. P1+S3+haiku

**Track all 9 agent IDs.**

**Phase 1b - Resume 9 agents for self-reflection:**
In a SINGLE message, resume all 9 agents with reflection prompt.

**Phase 2 - Launch 9 evaluators in parallel:**
In a SINGLE message, launch 9 new Sonnet evaluators.

**Track all 9 evaluator IDs.**

**Total for Batch 1:** 18 agent IDs (9 simulators + 9 evaluators)

### Batch 2 - P2 × All Criteria × All Models

Repeat with P2 combinations.
**Total for Batch 2:** 18 agent IDs

### Batch 3 - P3 × All Criteria × All Models

Repeat with P3 combinations.
**Total for Batch 3:** 18 agent IDs

**Grand Total:** 54 agent IDs (27 simulators + 27 evaluators)

### Phase 3 - Aggregate Analysis

After all 27 tests complete:
```bash
# Finalize all runs
./scripts/finalize-experiment-run.sh iteration-2/runs/run-20260106-* <all-54-agent-ids>

# Launch aggregate analysis agent
```

Provide all 27 evaluation results to aggregate analysis agent.

---

## Agent ID Tracking Template

```
BATCH 1 (P1):
Phase 1  (sims):     a1b2c3d a2b3c4d a3b4c5d a4b5c6d a5b6c7d a6b7c8d a7b8c9d a8b9cad a9bacbd
Phase 1b (reflect):  (same IDs, just resumed)
Phase 2  (evals):    aaabbb1 aaabbb2 aaabbb3 aaabbb4 aaabbb5 aaabbb6 aaabbb7 aaabbb8 aaabbb9

BATCH 2 (P2):
Phase 1  (sims):     [...9 more IDs...]
Phase 1b (reflect):  (same IDs)
Phase 2  (evals):    [...9 more IDs...]

BATCH 3 (P3):
Phase 1  (sims):     [...9 more IDs...]
Phase 1b (reflect):  (same IDs)
Phase 2  (evals):    [...9 more IDs...]
```

---

## Phase 4: Evaluation (Post-Simulation Quality Assessment)

After completing simulations, evaluate quality using automated and optional manual assessments.

### Step 1: Syntax Validation (30 minutes, automated)

Run automated syntax validation across all scenarios:

```bash
cd experiments/iteration-2/runs/[run-name]
./validate-scenarios.sh
```

**Generates:**
- `VALIDATION_REPORT.md` - Complete validation results (pass/fail for each file)
- `VALIDATION_SUMMARY.md` - Executive summary with pass rates by model/prompt
- `VALIDATION_INDEX.md` - Navigation guide

**What It Validates:**
- **YAML files:** Uses yamllint (GitHub issue forms, workflows, config)
- **Shell scripts:** Uses shellcheck (validation scripts, deployment scripts)
- **Markdown files:** Custom validation (structure, frontmatter, links)

**Pass Rates (from run-20260106-003027-full-matrix):**
- Overall: 88.4% (425/481 files pass)
- By Model: Opus 89.9%, Sonnet 88.4%, Haiku 84.8%
- By Prompt: P3 90.3%, P2 87.4%, P1 87.2%

---

### Step 2: Enhanced Rubric Scoring (optional, 2-12 hours)

Use the **Enhanced Rubric** (120-point system) for comprehensive evaluation:

**See:** `experiments/iteration-2/runs/[run-name]/ENHANCED_RUBRIC.md`

#### Automated Dimensions (~80 points, 30-60 min per scenario)

1. **Functional Verification (30 pts)**
   - Syntax validation (from Step 1)
   - Workflow trigger correctness (parse YAML)
   - Structure correctness (proper .github/ hierarchy vs flat)

2. **Completeness Calibration (25 pts)**
   - File count vs optimal range:
     - P1: 6-10 files optimal (70%)
     - P2: 6-12 files optimal (70-75%)
     - P3: 8-15 files optimal (75-85%)
   - Penalizes over-engineering for minimal prompts

3. **Actionability (15 pts)**
   - Placeholder density (count TODOs/FIXMEs)
   - Contextual appropriateness (templates OK, code not OK)
   - Documentation quality

4. **Specificity (10 pts)**
   - Inappropriate placeholders (YOUR_REPO, WORKFLOW_NAME)
   - Prompt reference specificity

#### Manual Dimensions (~40 points, requires human review)

5. **Correctness (20 pts)** - Logic trace, semantic correctness
6. **Research Quality (15 pts)** - WebSearch usage, 2026 sources
7. **Insight Quality (5 pts)** - Novel approaches, explicit assumptions

**Scoring Script Template:**
```bash
# Automated scoring (dimensions 1-4)
score_automated() {
  syntax_score=$(validate_syntax "$scenario")
  trigger_score=$(check_workflow_triggers "$scenario")
  structure_score=$(check_directory_structure "$scenario")
  completeness_score=$(calculate_completeness "$scenario" "$prompt_level")
  actionability_score=$(count_placeholders "$scenario")
  specificity_score=$(check_specificity "$scenario")

  echo "Automated Score: $(($syntax_score + $trigger_score + $structure_score + $completeness_score + $actionability_score + $specificity_score))/80"
}
```

---

### Step 3: Functional Testing (optional, 2-6 hours per scenario)

For deep validation of **representative scenarios**, perform functional testing.

**Approach: "Dry Run" Assessment (No Infrastructure Required)**

Analyzes implementations through static analysis and logic tracing:

1. **File Structure Analysis** - Verify proper placement
2. **Workflow Trigger Parsing** - Check YAML correctness
3. **GitHub API Usage Review** - Validate endpoint calls
4. **Logic Trace** - Follow execution path
5. **Automated Scoring** - Apply Enhanced Rubric dimensions

**Example Result (P2-S2-sonnet):**
- Automated Score: 80/80 (100%)
- Estimated Total: 89/120 (74%)
- Result: PASS
- Time: ~15 minutes

**See Example:**
- `experiments/iteration-2/runs/run-20260106-003027-full-matrix/P2-S2-sonnet-FUNCTIONAL-TEST.md`

**Alternative: Full Mock Infrastructure (Optional)**

For scenarios requiring runtime verification:

**See:** `EVALUATOR_REQUIREMENTS_AND_MOCK_SCAFFOLDING.md` for:
- Express.js mock GitHub API (~100 LOC)
- Bash @copilot agent stub (~50 LOC)
- Local bare git repository
- JSON state tracker

**Time Investment:** ~4-6 hours to build, ~30 min per test

---

### Step 4: Pattern Analysis (optional, 1-2 hours)

Analyze behavioral patterns across scenarios.

**See Existing Analyses:**
- `GITHUB_ACTIONS_ANALYSIS.md` - Automation adoption patterns (52% use GHA)
- `ANALYSIS.md` - Model behavior patterns (Opus minimal, Sonnet comprehensive, Haiku flat)
- `FINAL_ANALYSIS.md` - Cross-cutting insights and statistical summaries

**Analysis Dimensions:**
- GitHub Actions adoption rate
- File structure patterns (proper hierarchy vs flat)
- Knowledge base approaches (templates vs examples)
- Documentation styles
- Completeness trends (P1 vs P2 vs P3)

---

### Evaluation Documentation Structure

All evaluation assets stored in run directory:

```
experiments/iteration-2/runs/[run-name]/
├── ENHANCED_RUBRIC.md                      # 120-point evaluation system
├── VALIDATION_REPORT.md                    # Complete syntax validation
├── VALIDATION_SUMMARY.md                   # Executive summary
├── VALIDATION_INDEX.md                     # Navigation guide
├── VALIDATION_MANIFEST.md                  # Validation documentation
├── validate-scenarios.sh                   # Reusable validation script
├── GITHUB_ACTIONS_ANALYSIS.md              # Automation pattern analysis
├── ANALYSIS.md                             # Behavioral analysis (all scenarios)
├── FINAL_ANALYSIS.md                       # Cross-cutting insights
├── [Scenario]-FUNCTIONAL-TEST.md           # Optional functional test reports
└── EVALUATOR_REQUIREMENTS_AND_MOCK_...md   # Mock infrastructure guide
```

---

### Recommended Evaluation Workflows

#### Workflow A: Quick Validation (30 min)
```bash
# Syntax validation only
cd experiments/iteration-2/runs/[run-name]
./validate-scenarios.sh
# Review VALIDATION_SUMMARY.md
```

**Use when:** Quick pass/fail check needed

---

#### Workflow B: Standard Evaluation (2-4 hours)
```bash
# 1. Syntax validation (30 min)
./validate-scenarios.sh

# 2. Automated rubric scoring (2-3 hours for all 27 scenarios)
for scenario in P*-S*-*/; do
  score_automated "$scenario" >> AUTOMATED_SCORES.md
done

# 3. Review results
cat AUTOMATED_SCORES.md | sort -k2 -n
```

**Use when:** Comparing scenario quality, identifying patterns

---

#### Workflow C: Deep Evaluation (8-12 hours)
```bash
# 1. Syntax validation
./validate-scenarios.sh

# 2. Automated rubric scoring
score_all_scenarios.sh

# 3. Functional testing (1-3 representative scenarios)
functional_test.sh P2-S2-sonnet
functional_test.sh P3-S3-opus

# 4. Pattern analysis
analyze_github_actions.sh
analyze_model_behaviors.sh

# 5. Generate comprehensive report
generate_evaluation_report.sh
```

**Use when:** Production readiness assessment, research publication

---

### Evaluation Metrics Collected

| Metric | Source | Dimensions |
|--------|--------|------------|
| **Syntax Pass Rate** | validate-scenarios.sh | YAML, Shell, Markdown |
| **Structure Compliance** | Enhanced Rubric | Proper vs flat hierarchy |
| **Completeness Calibration** | File counts | Optimal vs over-engineered |
| **Placeholder Density** | grep analysis | Contextually appropriate vs problematic |
| **Workflow Correctness** | YAML parsing | Proper triggers, permissions |
| **GitHub Actions Adoption** | Pattern analysis | % using GHA, trigger types |
| **Model Characteristics** | Behavioral analysis | File counts, structure patterns |

---

### Pass/Fail Thresholds

**Syntax Validation:**
- PASS: ≥80% files pass validation
- WARNING: 60-79% pass rate
- FAIL: <60% pass rate

**Enhanced Rubric:**
- PASS: ≥80/120 points (67%)
- ACCEPTABLE: 60-79/120 (50-66%)
- FAIL: <60/120 (<50%)

**Functional Testing:**
- PASS: Logic trace shows it would work, all S# criteria satisfied
- WARNING: Works with caveats (hardcoded values, missing edge cases)
- FAIL: Would not execute or violates criteria

---

### Tools Reference

**Validation:**
- `validate-scenarios.sh` - Automated syntax validation
- `yamllint` - YAML linting (brew install yamllint)
- `shellcheck` - Shell script analysis (brew install shellcheck)

**Scoring:**
- `ENHANCED_RUBRIC.md` - Complete 120-point system with scoring guidelines
- Automated dimensions: bash scripts (syntax, structure, placeholders)
- Manual dimensions: human review (correctness, research, insights)

**Analysis:**
- Pattern analysis scripts (see experiments/iteration-2/runs/*/scripts/)
- Cross-scenario comparison tools
- Statistical aggregation

---

## Quick Reference

**Single test (P3+S2+sonnet):**
- Expected: ~3-5 minutes total
- Phase 1: ~2 min (simulation)
- Phase 1b: ~30 sec (reflection on same agent)
- Phase 2: ~2 min (evaluation)

**Full matrix (27 tests):**
- Expected: ~20-30 minutes total
- 3 batches × ~7-10 minutes per batch
- Phase 3 aggregate: ~5 minutes

**Evaluation (Phase 4):**
- Quick validation: ~30 min
- Standard evaluation: ~2-4 hours
- Deep evaluation: ~8-12 hours

**Time savings from parallelism:**
- Sequential: ~90-120 minutes
- Parallel batches: ~20-30 minutes
- **Speedup: 3-4x**
