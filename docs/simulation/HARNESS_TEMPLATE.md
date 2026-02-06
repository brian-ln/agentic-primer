# {PROJECT_NAME} Simulation Harness v2

<!-- TEMPLATE INSTRUCTIONS:
This is a reusable template for creating simulation harnesses.
Replace all {PLACEHOLDERS} with your project-specific values.
See HARNESS_GUIDE.md for detailed customization instructions.
-->

Test framework for evaluating {DOMAIN_CONTEXT} across models and success criteria variations.

---

## Test Matrix

<!-- Section Purpose: Define the dimensions of your test matrix (prompts × criteria × models) -->

Test {PROMPTS_COUNT} prompt lengths × {CRITERIA_COUNT} success criteria levels × {MODELS_COUNT} models = **{TOTAL_TESTS} simulations**

### Prompt Variants

<!-- Customization: Define your prompt variations. These should vary in length, detail, or style.
     Common patterns: minimal/moderate/detailed, terse/verbose, abstract/concrete -->

Prompts are stored in `experiments/{EXPERIMENT_PATH}/prompts/`:

- **P1** - `{P1_FILENAME}` ({P1_WORD_COUNT} words)
  <!-- Description: {P1_DESCRIPTION} -->
- **P2** - `{P2_FILENAME}` ({P2_WORD_COUNT} words)
  <!-- Description: {P2_DESCRIPTION} -->
- **P3** - `{P3_FILENAME}` ({P3_WORD_COUNT} words)
  <!-- Description: {P3_DESCRIPTION} -->

<!-- Add more prompt variants as needed: P4, P5, etc. -->

### Success Criteria Variants

<!-- Customization: Define your success criteria levels. These should vary in specificity or comprehensiveness.
     Common patterns: minimal/moderate/comprehensive, 1 requirement/3 requirements/7 requirements -->

Criteria are stored in `experiments/{EXPERIMENT_PATH}/criteria/`:

- **S1** - `{S1_FILENAME}` ({S1_REQUIREMENTS_COUNT} requirement)
  <!-- Description: {S1_DESCRIPTION} -->
- **S2** - `{S2_FILENAME}` ({S2_REQUIREMENTS_COUNT} requirements)
  <!-- Description: {S2_DESCRIPTION} -->
- **S3** - `{S3_FILENAME}` ({S3_REQUIREMENTS_COUNT} observable outcomes)
  <!-- Description: {S3_DESCRIPTION} -->

<!-- Add more criteria variants as needed: S4, S5, etc. -->

### Models

<!-- Customization: Choose which Claude models to test.
     Standard options: opus (most capable), sonnet (balanced), haiku (fastest)
     You can test fewer models (e.g., just sonnet) or add more -->

- **Opus** (claude-opus-4-5)
- **Sonnet** (claude-sonnet-4-5)
- **Haiku** (claude-haiku-4)

---

## Output File Organization

<!-- Section Purpose: Define where simulation artifacts are stored.
     This structure is standard across all harnesses - minimal customization needed. -->

**Directory Structure for Runs:**

Each test configuration gets its own subdirectory in the run folder:

```
experiments/{EXPERIMENT_PATH}/runs/run-YYYYMMDD-HHMMSS-{RUN_NAME}/
├── config.json
├── README.md
├── logs/
├── results/
├── P1-S1-opus/       # Simulation outputs for P1+S1+opus
├── P1-S1-sonnet/     # Simulation outputs for P1+S1+sonnet
├── P1-S1-haiku/      # Simulation outputs for P1+S1+haiku
├── P1-S2-opus/
├── P1-S2-sonnet/
... ({TOTAL_TESTS} total test directories)
```

**What Goes in Each Test Directory:**
- Markdown files created by simulation agents
- Solution design documents
- File manifests
- Any other artifacts generated during the simulation

**Important:** Agents should NOT create files in the main project directory. All simulation outputs belong in their respective test subdirectories.

---

## Three-Phase Methodology

<!-- Section Purpose: Core simulation workflow. This is UNIVERSAL - same across all harnesses.
     No customization needed except for domain-specific context in prompts. -->

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
You are simulating {AGENT_NAME} {AGENT_DESCRIPTION}.

PROMPT:
[INSERT: P1, P2, or P3 content]

SUCCESS CRITERIA:
[INSERT: S1, S2, or S3 content]

YOUR TASK:
{SIMULATION_TASK_INSTRUCTIONS}

{SIMULATION_CONSTRAINTS}

List all files {AGENT_NAME} would create (file paths) as a part of the solution.

For each file provide:
- Purpose (1 sentence)
- Complete functional content (no placeholders)
- Assumptions made
- How and why {AGENT_NAME} decided it was necessary

OUTPUT: Markdown document with complete solution.

OUTPUT LOCATION: experiments/{EXPERIMENT_PATH}/runs/[run-name]/[P#]-[S#]-[model]/
Example: experiments/{EXPERIMENT_PATH}/runs/run-20260106-001203-{RUN_NAME}/P1-S1-opus/

IMPORTANT: Put ALL files you create in the OUTPUT LOCATION directory above.

CONSTRAINTS:
Production-ready, complete code, no placeholders.
{DOMAIN_SPECIFIC_CONSTRAINTS}
Output: experiments/{EXPERIMENT_PATH}/runs/[run-name]/[P#]-[S#]-[model]/
```

<!-- Customization Points:
     {AGENT_NAME}: e.g., "@copilot", "the API designer", "the ML engineer"
     {AGENT_DESCRIPTION}: e.g., "the GitHub CoPilot agent that can do work autonomously when assigned issues"
     {SIMULATION_TASK_INSTRUCTIONS}: e.g., "Acting as @copilot and given the PROMPT and the SUCCESS CRITERIA design the solution, describe it in a single markdown file, then implement and verify it."
     {SIMULATION_CONSTRAINTS}: e.g., "Remember, this is a simulation and you are pretending to act just like @copilot would. DO NOT ACTUALLY CALL ANY GITHUB APIS, PROGRAMS, or SCRIPTS to do the work. Simulate them being called if necessary."
     {DOMAIN_SPECIFIC_CONSTRAINTS}: e.g., "Simulate GitHub operations (no APIs, git push, or state changes)."
-->

**Save agent ID** (e.g., ae913f4) - you'll need it for Phase 1b.

---

### Phase 1b: Self-Reflection (Resume)

<!-- Section Purpose: Agents reflect on their own performance.
     This is UNIVERSAL - minimal customization needed. -->

Use Task tool with **resume** parameter:
```
Task(
  resume=[agent-id from Phase 1],
  description="[P#]+[S#]+[model] self-reflection"
)
```

**Resume Prompt:**
```
Now self-reflect on how well you think you did as {AGENT_NAME} and what could have produced better outcomes.

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

<!-- Section Purpose: Independent evaluators score simulation performance.
     Customization: RUBRIC dimensions and weights are project-specific. -->

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
Evaluate this simulation of how {AGENT_NAME} performed using the rubric below.

PROMPT: [Original prompt]
SUCCESS CRITERIA: [Original criteria]
SIMULATION OUTPUT: [Phase 1 agent's output]
SELF-REFLECTION: [Phase 1b agent's output]

RUBRIC ({RUBRIC_TOTAL_POINTS} points):

<!-- Customization: Define rubric dimensions that matter for your domain.
     Standard dimensions: Completeness, Correctness, Actionability, Specificity, Insight Quality
     Custom dimensions: Domain-specific metrics (e.g., Security, Performance, Scalability) -->

1. {RUBRIC_DIMENSION_1_NAME} ({RUBRIC_DIMENSION_1_POINTS} points)
   - {RUBRIC_DIMENSION_1_CRITERIA_1}
   - {RUBRIC_DIMENSION_1_CRITERIA_2}

2. {RUBRIC_DIMENSION_2_NAME} ({RUBRIC_DIMENSION_2_POINTS} points)
   - {RUBRIC_DIMENSION_2_CRITERIA_1}
   - {RUBRIC_DIMENSION_2_CRITERIA_2}

3. {RUBRIC_DIMENSION_3_NAME} ({RUBRIC_DIMENSION_3_POINTS} points)
   - {RUBRIC_DIMENSION_3_CRITERIA_1}
   - {RUBRIC_DIMENSION_3_CRITERIA_2}

4. {RUBRIC_DIMENSION_4_NAME} ({RUBRIC_DIMENSION_4_POINTS} points)
   - {RUBRIC_DIMENSION_4_CRITERIA_1}
   - {RUBRIC_DIMENSION_4_CRITERIA_2}

5. {RUBRIC_DIMENSION_5_NAME} ({RUBRIC_DIMENSION_5_POINTS} points)
   - {RUBRIC_DIMENSION_5_CRITERIA_1}
   - {RUBRIC_DIMENSION_5_CRITERIA_2}

<!-- Add or remove dimensions as needed. Ensure points sum to {RUBRIC_TOTAL_POINTS}. -->

SCORE EACH DIMENSION, PROVIDE TOTAL /{RUBRIC_TOTAL_POINTS}, GRADE (A/B/C/D/F)

SUMMARIZE:
- {AGENT_NAME}'s approach (from compact agent session logs)
- Self-reflection insights
- Alignment between what was done vs what was reflected

COMMENTARY:
- 2-3 Strengths
- 2-3 Weaknesses
- Recommendations for improvement
```

<!-- Example Rubric (Bootstrap Project):
     1. COMPLETENESS (30 points) - File coverage: Expected vs actual files created
     2. CORRECTNESS (25 points) - Syntax validity: yamllint, shellcheck, markdownlint pass
     3. ACTIONABILITY (20 points) - Ready to use immediately vs rework required
     4. SPECIFICITY (15 points) - Placeholder density (count TODOs, FIXMEs, etc.)
     5. INSIGHT QUALITY (10 points) - Novel approaches, explicit assumptions, edge cases
-->

**Save evaluator agent ID** (e.g., a0294d5).

---

### Phase 3: Aggregate Analysis (After all runs)

<!-- Section Purpose: Cross-run analysis to identify patterns and optimal configurations.
     This is UNIVERSAL - minimal customization needed. -->

After completing all {TOTAL_TESTS} simulations, launch ONE analysis agent:

**Aggregate Analysis Prompt:**
```
Analyze results across all {TOTAL_TESTS} simulation runs.

RUNS:
[Include all evaluation results from Phase 2 agents - {TOTAL_TESTS} evaluations]

ANALYSIS TASKS:
1. Score distribution by dimension ({RUBRIC_DIMENSIONS_LIST})
2. Model performance comparison ({MODELS_LIST})
3. Prompt length impact ({PROMPTS_LIST})
4. Success criteria impact ({CRITERIA_LIST})
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

<!-- Customization Points:
     {RUBRIC_DIMENSIONS_LIST}: e.g., "Completeness, Correctness, Actionability, Specificity, Insight"
     {MODELS_LIST}: e.g., "opus vs sonnet vs haiku"
     {PROMPTS_LIST}: e.g., "P1 vs P2 vs P3"
     {CRITERIA_LIST}: e.g., "S1 vs S2 vs S3"
-->

---

## Running Single Test

<!-- Section Purpose: Step-by-step guide for running a single test (useful for testing setup).
     Standard workflow - minimal customization needed. -->

**Step 1 - Create run directory:**
```bash
./scripts/create-experiment-run.sh {EXPERIMENT_NAME} P3-S2-sonnet
# Creates: experiments/{EXPERIMENT_PATH}/runs/run-20260106-001234-P3-S2-sonnet/
```

**Step 2 - Phase 1 (Simulation):**
Launch Task with simulation prompt. Save agent ID.

**Step 3 - Phase 1b (Self-Reflection):**
Resume same agent with reflection prompt.

**Step 4 - Phase 2 (Evaluation):**
Launch new Sonnet evaluator. Save agent ID.

**Step 5 - Finalize:**
```bash
./scripts/finalize-experiment-run.sh {EXPERIMENT_PATH}/runs/run-20260106-001234-P3-S2-sonnet <sim-id> <eval-id>
```

---

## Running Full Matrix ({TOTAL_TESTS} Tests)

<!-- Section Purpose: Batch execution strategy for running all tests efficiently.
     Standard workflow - minimal customization needed. -->

**Batch Strategy:** {BATCH_COUNT} batches of {BATCH_SIZE} (grouped by prompt)

### Batch 1 - P1 × All Criteria × All Models

**Phase 1 - Launch {BATCH_SIZE} simulations in parallel:**
In a SINGLE message, launch {BATCH_SIZE} Task calls:
1. P1+S1+{MODEL_1}
2. P1+S1+{MODEL_2}
3. P1+S1+{MODEL_3}
4. P1+S2+{MODEL_1}
5. P1+S2+{MODEL_2}
6. P1+S2+{MODEL_3}
7. P1+S3+{MODEL_1}
8. P1+S3+{MODEL_2}
9. P1+S3+{MODEL_3}

<!-- Adjust numbering if you have different prompt/criteria/model counts -->

**Track all {BATCH_SIZE} agent IDs.**

**Phase 1b - Resume {BATCH_SIZE} agents for self-reflection:**
In a SINGLE message, resume all {BATCH_SIZE} agents with reflection prompt.

**Phase 2 - Launch {BATCH_SIZE} evaluators in parallel:**
In a SINGLE message, launch {BATCH_SIZE} new Sonnet evaluators.

**Track all {BATCH_SIZE} evaluator IDs.**

**Total for Batch 1:** {BATCH_1_AGENT_COUNT} agent IDs ({BATCH_SIZE} simulators + {BATCH_SIZE} evaluators)

### Batch 2 - P2 × All Criteria × All Models

Repeat with P2 combinations.
**Total for Batch 2:** {BATCH_2_AGENT_COUNT} agent IDs

### Batch 3 - P3 × All Criteria × All Models

Repeat with P3 combinations.
**Total for Batch 3:** {BATCH_3_AGENT_COUNT} agent IDs

<!-- Add more batches if you have more than 3 prompt variants -->

**Grand Total:** {TOTAL_AGENT_IDS} agent IDs ({TOTAL_TESTS} simulators + {TOTAL_TESTS} evaluators)

### Phase 3 - Aggregate Analysis

After all {TOTAL_TESTS} tests complete:
```bash
# Finalize all runs
./scripts/finalize-experiment-run.sh {EXPERIMENT_PATH}/runs/run-20260106-* <all-{TOTAL_AGENT_IDS}-agent-ids>

# Launch aggregate analysis agent
```

Provide all {TOTAL_TESTS} evaluation results to aggregate analysis agent.

---

## Agent ID Tracking Template

<!-- Section Purpose: Template for tracking agent IDs across batches.
     Copy and fill in as you run simulations. -->

```
BATCH 1 (P1):
Phase 1  (sims):     [ID1] [ID2] [ID3] [ID4] [ID5] [ID6] [ID7] [ID8] [ID9]
Phase 1b (reflect):  (same IDs, just resumed)
Phase 2  (evals):    [ID10] [ID11] [ID12] [ID13] [ID14] [ID15] [ID16] [ID17] [ID18]

BATCH 2 (P2):
Phase 1  (sims):     [ID19] [ID20] [ID21] [ID22] [ID23] [ID24] [ID25] [ID26] [ID27]
Phase 1b (reflect):  (same IDs)
Phase 2  (evals):    [ID28] [ID29] [ID30] [ID31] [ID32] [ID33] [ID34] [ID35] [ID36]

BATCH 3 (P3):
Phase 1  (sims):     [ID37] [ID38] [ID39] [ID40] [ID41] [ID42] [ID43] [ID44] [ID45]
Phase 1b (reflect):  (same IDs)
Phase 2  (evals):    [ID46] [ID47] [ID48] [ID49] [ID50] [ID51] [ID52] [ID53] [ID54]
```

<!-- Adjust for your batch count and size -->

---

## Phase 4: Evaluation (Post-Simulation Quality Assessment)

<!-- Section Purpose: Quality assessment framework for simulation outputs.
     This is UNIVERSAL - minimal customization needed, except for domain-specific validators. -->

After completing simulations, evaluate quality using automated and optional manual assessments.

### Step 1: Syntax Validation ({VALIDATION_TIME_ESTIMATE}, automated)

Run automated syntax validation across all scenarios:

```bash
cd experiments/{EXPERIMENT_PATH}/runs/[run-name]
./validate-scenarios.sh
```

**Generates:**
- `VALIDATION_REPORT.md` - Complete validation results (pass/fail for each file)
- `VALIDATION_SUMMARY.md` - Executive summary with pass rates by model/prompt
- `VALIDATION_INDEX.md` - Navigation guide

**What It Validates:**
<!-- Customization: Add domain-specific validators for your file types -->
- **{FILE_TYPE_1}:** Uses {VALIDATOR_1} ({VALIDATOR_1_PURPOSE})
- **{FILE_TYPE_2}:** Uses {VALIDATOR_2} ({VALIDATOR_2_PURPOSE})
- **{FILE_TYPE_3}:** Custom validation ({VALIDATOR_3_PURPOSE})

<!-- Example (Bootstrap):
     - YAML files: Uses yamllint (GitHub issue forms, workflows, config)
     - Shell scripts: Uses shellcheck (validation scripts, deployment scripts)
     - Markdown files: Custom validation (structure, frontmatter, links)
-->

**Pass Rates (reference from previous runs):**
- Overall: {OVERALL_PASS_RATE}%
- By Model: {MODEL_1} {MODEL_1_PASS_RATE}%, {MODEL_2} {MODEL_2_PASS_RATE}%, {MODEL_3} {MODEL_3_PASS_RATE}%
- By Prompt: {P1_PASS_RATE}%, {P2_PASS_RATE}%, {P3_PASS_RATE}%

---

### Step 2: Enhanced Rubric Scoring (optional, {ENHANCED_RUBRIC_TIME_ESTIMATE})

Use the **Enhanced Rubric** ({ENHANCED_RUBRIC_POINTS}-point system) for comprehensive evaluation:

**See:** `experiments/{EXPERIMENT_PATH}/runs/[run-name]/ENHANCED_RUBRIC.md`

#### Automated Dimensions (~{AUTOMATED_DIMENSIONS_POINTS} points, {AUTOMATED_TIME_PER_SCENARIO} per scenario)

<!-- Customization: Define automated metrics that can be computed from simulation outputs -->

1. **{AUTOMATED_DIMENSION_1_NAME} ({AUTOMATED_DIMENSION_1_POINTS} pts)**
   - {AUTOMATED_DIMENSION_1_CRITERIA}

2. **{AUTOMATED_DIMENSION_2_NAME} ({AUTOMATED_DIMENSION_2_POINTS} pts)**
   - {AUTOMATED_DIMENSION_2_CRITERIA}

3. **{AUTOMATED_DIMENSION_3_NAME} ({AUTOMATED_DIMENSION_3_POINTS} pts)**
   - {AUTOMATED_DIMENSION_3_CRITERIA}

<!-- Example (Bootstrap):
     1. Functional Verification (30 pts) - Syntax validation, workflow triggers, structure
     2. Completeness Calibration (25 pts) - File count vs optimal range
     3. Actionability (15 pts) - Placeholder density, documentation quality
-->

#### Manual Dimensions (~{MANUAL_DIMENSIONS_POINTS} points, requires human review)

<!-- Customization: Define qualitative metrics that require human judgment -->

4. **{MANUAL_DIMENSION_1_NAME} ({MANUAL_DIMENSION_1_POINTS} pts)** - {MANUAL_DIMENSION_1_CRITERIA}
5. **{MANUAL_DIMENSION_2_NAME} ({MANUAL_DIMENSION_2_POINTS} pts)** - {MANUAL_DIMENSION_2_CRITERIA}
6. **{MANUAL_DIMENSION_3_NAME} ({MANUAL_DIMENSION_3_POINTS} pts)** - {MANUAL_DIMENSION_3_CRITERIA}

<!-- Example (Bootstrap):
     4. Correctness (20 pts) - Logic trace, semantic correctness
     5. Research Quality (15 pts) - WebSearch usage, source currency
     6. Insight Quality (5 pts) - Novel approaches, explicit assumptions
-->

---

### Step 3: Functional Testing (optional, {FUNCTIONAL_TEST_TIME_ESTIMATE} per scenario)

For deep validation of **representative scenarios**, perform functional testing.

**Approach: "{FUNCTIONAL_TEST_APPROACH}"**

<!-- Customization: Define how to functionally test your domain.
     Options: Dry run analysis, mock infrastructure, actual deployment, etc. -->

{FUNCTIONAL_TEST_DESCRIPTION}

**Example Result (from previous runs):**
- {FUNCTIONAL_TEST_EXAMPLE_SCENARIO}
- {FUNCTIONAL_TEST_EXAMPLE_RESULT}

**See Example:**
- `experiments/{EXPERIMENT_PATH}/runs/[run-name]/{FUNCTIONAL_TEST_EXAMPLE_FILE}`

---

### Step 4: Pattern Analysis (optional, {PATTERN_ANALYSIS_TIME_ESTIMATE})

Analyze behavioral patterns across scenarios.

**See Existing Analyses:**
<!-- Customization: List domain-specific pattern analyses you want to perform -->
- `{PATTERN_ANALYSIS_1_FILE}` - {PATTERN_ANALYSIS_1_DESCRIPTION}
- `{PATTERN_ANALYSIS_2_FILE}` - {PATTERN_ANALYSIS_2_DESCRIPTION}
- `{PATTERN_ANALYSIS_3_FILE}` - {PATTERN_ANALYSIS_3_DESCRIPTION}

**Analysis Dimensions:**
<!-- Customization: Define what patterns to look for -->
- {PATTERN_1}
- {PATTERN_2}
- {PATTERN_3}

---

### Evaluation Documentation Structure

All evaluation assets stored in run directory:

```
experiments/{EXPERIMENT_PATH}/runs/[run-name]/
├── ENHANCED_RUBRIC.md                      # {ENHANCED_RUBRIC_POINTS}-point evaluation system
├── VALIDATION_REPORT.md                    # Complete syntax validation
├── VALIDATION_SUMMARY.md                   # Executive summary
├── VALIDATION_INDEX.md                     # Navigation guide
├── VALIDATION_MANIFEST.md                  # Validation documentation
├── validate-scenarios.sh                   # Reusable validation script
├── {PATTERN_ANALYSIS_1_FILE}               # Pattern analysis 1
├── {PATTERN_ANALYSIS_2_FILE}               # Pattern analysis 2
├── {PATTERN_ANALYSIS_3_FILE}               # Cross-cutting insights
├── [Scenario]-FUNCTIONAL-TEST.md           # Optional functional test reports
└── {MOCK_INFRASTRUCTURE_GUIDE_FILE}        # Mock infrastructure guide (if applicable)
```

---

### Recommended Evaluation Workflows

#### Workflow A: Quick Validation ({WORKFLOW_A_TIME_ESTIMATE})
```bash
# Syntax validation only
cd experiments/{EXPERIMENT_PATH}/runs/[run-name]
./validate-scenarios.sh
# Review VALIDATION_SUMMARY.md
```

**Use when:** Quick pass/fail check needed

---

#### Workflow B: Standard Evaluation ({WORKFLOW_B_TIME_ESTIMATE})
```bash
# 1. Syntax validation ({VALIDATION_TIME_ESTIMATE})
./validate-scenarios.sh

# 2. Automated rubric scoring ({AUTOMATED_SCORING_TIME_ESTIMATE} for all {TOTAL_TESTS} scenarios)
for scenario in P*-S*-*/; do
  score_automated "$scenario" >> AUTOMATED_SCORES.md
done

# 3. Review results
cat AUTOMATED_SCORES.md | sort -k2 -n
```

**Use when:** Comparing scenario quality, identifying patterns

---

#### Workflow C: Deep Evaluation ({WORKFLOW_C_TIME_ESTIMATE})
```bash
# 1. Syntax validation
./validate-scenarios.sh

# 2. Automated rubric scoring
score_all_scenarios.sh

# 3. Functional testing (1-3 representative scenarios)
functional_test.sh {REPRESENTATIVE_SCENARIO_1}
functional_test.sh {REPRESENTATIVE_SCENARIO_2}

# 4. Pattern analysis
{PATTERN_ANALYSIS_SCRIPT_1}
{PATTERN_ANALYSIS_SCRIPT_2}

# 5. Generate comprehensive report
generate_evaluation_report.sh
```

**Use when:** Production readiness assessment, research publication

---

### Evaluation Metrics Collected

| Metric | Source | Dimensions |
|--------|--------|------------|
| **{METRIC_1_NAME}** | {METRIC_1_SOURCE} | {METRIC_1_DIMENSIONS} |
| **{METRIC_2_NAME}** | {METRIC_2_SOURCE} | {METRIC_2_DIMENSIONS} |
| **{METRIC_3_NAME}** | {METRIC_3_SOURCE} | {METRIC_3_DIMENSIONS} |
| **{METRIC_4_NAME}** | {METRIC_4_SOURCE} | {METRIC_4_DIMENSIONS} |
| **{METRIC_5_NAME}** | {METRIC_5_SOURCE} | {METRIC_5_DIMENSIONS} |
| **{METRIC_6_NAME}** | {METRIC_6_SOURCE} | {METRIC_6_DIMENSIONS} |

<!-- Example (Bootstrap):
     | Metric | Source | Dimensions |
     | Syntax Pass Rate | validate-scenarios.sh | YAML, Shell, Markdown |
     | Structure Compliance | Enhanced Rubric | Proper vs flat hierarchy |
     | Completeness Calibration | File counts | Optimal vs over-engineered |
     | Placeholder Density | grep analysis | Contextually appropriate vs problematic |
     | Workflow Correctness | YAML parsing | Proper triggers, permissions |
     | Model Characteristics | Behavioral analysis | File counts, structure patterns |
-->

---

### Pass/Fail Thresholds

**Syntax Validation:**
- PASS: ≥{SYNTAX_PASS_THRESHOLD}% files pass validation
- WARNING: {SYNTAX_WARNING_LOW}-{SYNTAX_WARNING_HIGH}% pass rate
- FAIL: <{SYNTAX_FAIL_THRESHOLD}% pass rate

**Enhanced Rubric:**
- PASS: ≥{RUBRIC_PASS_THRESHOLD}/{ENHANCED_RUBRIC_POINTS} points ({RUBRIC_PASS_PERCENT}%)
- ACCEPTABLE: {RUBRIC_ACCEPTABLE_LOW}-{RUBRIC_ACCEPTABLE_HIGH}/{ENHANCED_RUBRIC_POINTS} ({RUBRIC_ACCEPTABLE_PERCENT}%)
- FAIL: <{RUBRIC_FAIL_THRESHOLD}/{ENHANCED_RUBRIC_POINTS} (<{RUBRIC_FAIL_PERCENT}%)

**Functional Testing:**
- PASS: {FUNCTIONAL_PASS_CRITERIA}
- WARNING: {FUNCTIONAL_WARNING_CRITERIA}
- FAIL: {FUNCTIONAL_FAIL_CRITERIA}

<!-- Example (Bootstrap):
     Syntax: PASS ≥80%, WARNING 60-79%, FAIL <60%
     Rubric: PASS ≥80/120 (67%), ACCEPTABLE 60-79 (50-66%), FAIL <60 (<50%)
     Functional: PASS = Logic trace shows it would work, all criteria satisfied
-->

---

### Tools Reference

**Validation:**
<!-- Customization: List tools used for validation -->
- `validate-scenarios.sh` - {VALIDATOR_SCRIPT_DESCRIPTION}
- `{VALIDATOR_1}` - {VALIDATOR_1_DESCRIPTION} ({VALIDATOR_1_INSTALL})
- `{VALIDATOR_2}` - {VALIDATOR_2_DESCRIPTION} ({VALIDATOR_2_INSTALL})

**Scoring:**
- `ENHANCED_RUBRIC.md` - Complete {ENHANCED_RUBRIC_POINTS}-point system with scoring guidelines
- Automated dimensions: bash scripts ({AUTOMATED_METRICS_LIST})
- Manual dimensions: human review ({MANUAL_METRICS_LIST})

**Analysis:**
- Pattern analysis scripts (see experiments/{EXPERIMENT_PATH}/runs/*/scripts/)
- Cross-scenario comparison tools
- Statistical aggregation

---

## Quick Reference

<!-- Section Purpose: Time estimates for planning purposes.
     Customize based on your domain complexity and model performance. -->

**Single test ({SINGLE_TEST_CONFIG}):**
- Expected: ~{SINGLE_TEST_TOTAL_TIME} total
- Phase 1: ~{SINGLE_TEST_PHASE1_TIME} (simulation)
- Phase 1b: ~{SINGLE_TEST_PHASE1B_TIME} (reflection on same agent)
- Phase 2: ~{SINGLE_TEST_PHASE2_TIME} (evaluation)

**Full matrix ({TOTAL_TESTS} tests):**
- Expected: ~{FULL_MATRIX_TOTAL_TIME} total
- {BATCH_COUNT} batches × ~{BATCH_TIME} per batch
- Phase 3 aggregate: ~{AGGREGATE_TIME}

**Evaluation (Phase 4):**
- Quick validation: ~{WORKFLOW_A_TIME_ESTIMATE}
- Standard evaluation: ~{WORKFLOW_B_TIME_ESTIMATE}
- Deep evaluation: ~{WORKFLOW_C_TIME_ESTIMATE}

**Time savings from parallelism:**
- Sequential: ~{SEQUENTIAL_TIME}
- Parallel batches: ~{PARALLEL_TIME}
- **Speedup: {SPEEDUP_FACTOR}x**

---

## Placeholder Reference

<!-- This section documents all placeholders used in this template for easy customization -->

### Project Identity
- `{PROJECT_NAME}` - Name of your project/experiment
- `{DOMAIN_CONTEXT}` - What domain you're simulating (e.g., "GitHub automation", "API design", "ML pipelines")
- `{EXPERIMENT_NAME}` - Short name for experiment directory
- `{EXPERIMENT_PATH}` - Path relative to experiments/ (e.g., "iteration-2")
- `{RUN_NAME}` - Run naming pattern (e.g., "full-matrix", "P3-S2-sonnet")

### Test Matrix Configuration
- `{PROMPTS_COUNT}` - Number of prompt variants (e.g., 3)
- `{CRITERIA_COUNT}` - Number of success criteria variants (e.g., 3)
- `{MODELS_COUNT}` - Number of models to test (e.g., 3)
- `{TOTAL_TESTS}` - Total simulations (prompts × criteria × models)
- `{BATCH_COUNT}` - Number of batches (usually = prompts count)
- `{BATCH_SIZE}` - Tests per batch (usually = criteria × models)

### Prompt Variants
- `{P1_FILENAME}`, `{P2_FILENAME}`, `{P3_FILENAME}` - Prompt filenames
- `{P1_DESCRIPTION}`, `{P2_DESCRIPTION}`, `{P3_DESCRIPTION}` - Prompt descriptions
- `{P1_WORD_COUNT}`, `{P2_WORD_COUNT}`, `{P3_WORD_COUNT}` - Word counts
- `{PROMPTS_LIST}` - Comma-separated list (e.g., "P1 vs P2 vs P3")

### Success Criteria Variants
- `{S1_FILENAME}`, `{S2_FILENAME}`, `{S3_FILENAME}` - Criteria filenames
- `{S1_DESCRIPTION}`, `{S2_DESCRIPTION}`, `{S3_DESCRIPTION}` - Criteria descriptions
- `{S1_REQUIREMENTS_COUNT}`, `{S2_REQUIREMENTS_COUNT}`, `{S3_REQUIREMENTS_COUNT}` - Requirement counts
- `{CRITERIA_LIST}` - Comma-separated list (e.g., "S1 vs S2 vs S3")

### Models
- `{MODEL_1}`, `{MODEL_2}`, `{MODEL_3}` - Model identifiers (e.g., "opus", "sonnet", "haiku")
- `{MODELS_LIST}` - Comma-separated list (e.g., "opus vs sonnet vs haiku")

### Simulation Context
- `{AGENT_NAME}` - Name of agent being simulated (e.g., "@copilot")
- `{AGENT_DESCRIPTION}` - Full description of agent role
- `{SIMULATION_TASK_INSTRUCTIONS}` - What the agent should do
- `{SIMULATION_CONSTRAINTS}` - Domain-specific simulation constraints
- `{DOMAIN_SPECIFIC_CONSTRAINTS}` - Additional constraints for output

### Rubric Configuration
- `{RUBRIC_TOTAL_POINTS}` - Total points (e.g., 100)
- `{RUBRIC_DIMENSION_1_NAME}` through `{RUBRIC_DIMENSION_5_NAME}` - Dimension names
- `{RUBRIC_DIMENSION_1_POINTS}` through `{RUBRIC_DIMENSION_5_POINTS}` - Points per dimension
- `{RUBRIC_DIMENSION_X_CRITERIA_Y}` - Criteria for each dimension
- `{RUBRIC_DIMENSIONS_LIST}` - Comma-separated dimension names

### Evaluation Configuration
- `{ENHANCED_RUBRIC_POINTS}` - Enhanced rubric total (e.g., 120)
- `{AUTOMATED_DIMENSIONS_POINTS}` - Points from automated metrics
- `{MANUAL_DIMENSIONS_POINTS}` - Points from manual review
- `{AUTOMATED_DIMENSION_X_NAME}` - Automated metric names
- `{MANUAL_DIMENSION_X_NAME}` - Manual metric names
- `{FILE_TYPE_X}` - File types to validate
- `{VALIDATOR_X}` - Validation tool names
- `{VALIDATOR_X_PURPOSE}` - What each validator checks

### Evaluation Metrics
- `{OVERALL_PASS_RATE}` - Overall validation pass rate
- `{MODEL_X_PASS_RATE}` - Pass rate by model
- `{PX_PASS_RATE}` - Pass rate by prompt
- `{METRIC_X_NAME}` - Metric names
- `{METRIC_X_SOURCE}` - Where metrics come from
- `{METRIC_X_DIMENSIONS}` - What metrics measure

### Time Estimates
- `{SINGLE_TEST_TOTAL_TIME}` - Total time for one test
- `{SINGLE_TEST_PHASE1_TIME}` - Phase 1 duration
- `{SINGLE_TEST_PHASE1B_TIME}` - Phase 1b duration
- `{SINGLE_TEST_PHASE2_TIME}` - Phase 2 duration
- `{FULL_MATRIX_TOTAL_TIME}` - Total time for all tests
- `{BATCH_TIME}` - Time per batch
- `{AGGREGATE_TIME}` - Phase 3 analysis time
- `{VALIDATION_TIME_ESTIMATE}` - Syntax validation time
- `{ENHANCED_RUBRIC_TIME_ESTIMATE}` - Enhanced rubric time
- `{FUNCTIONAL_TEST_TIME_ESTIMATE}` - Functional testing time
- `{PATTERN_ANALYSIS_TIME_ESTIMATE}` - Pattern analysis time
- `{WORKFLOW_A_TIME_ESTIMATE}` - Quick validation workflow time
- `{WORKFLOW_B_TIME_ESTIMATE}` - Standard evaluation workflow time
- `{WORKFLOW_C_TIME_ESTIMATE}` - Deep evaluation workflow time
- `{SEQUENTIAL_TIME}` - Time if run sequentially
- `{PARALLEL_TIME}` - Time with parallelism
- `{SPEEDUP_FACTOR}` - Speedup multiplier

### Thresholds
- `{SYNTAX_PASS_THRESHOLD}` - Syntax validation pass threshold (%)
- `{SYNTAX_WARNING_LOW}` - Warning range low (%)
- `{SYNTAX_WARNING_HIGH}` - Warning range high (%)
- `{SYNTAX_FAIL_THRESHOLD}` - Fail threshold (%)
- `{RUBRIC_PASS_THRESHOLD}` - Rubric pass threshold (points)
- `{RUBRIC_PASS_PERCENT}` - Pass threshold (%)
- `{RUBRIC_ACCEPTABLE_LOW}` - Acceptable range low (points)
- `{RUBRIC_ACCEPTABLE_HIGH}` - Acceptable range high (points)
- `{RUBRIC_ACCEPTABLE_PERCENT}` - Acceptable range (%)
- `{RUBRIC_FAIL_THRESHOLD}` - Fail threshold (points)
- `{RUBRIC_FAIL_PERCENT}` - Fail threshold (%)
- `{FUNCTIONAL_PASS_CRITERIA}` - Functional test pass criteria
- `{FUNCTIONAL_WARNING_CRITERIA}` - Functional test warning criteria
- `{FUNCTIONAL_FAIL_CRITERIA}` - Functional test fail criteria

### Documentation Artifacts
- `{PATTERN_ANALYSIS_X_FILE}` - Pattern analysis filenames
- `{PATTERN_ANALYSIS_X_DESCRIPTION}` - What each analysis covers
- `{PATTERN_ANALYSIS_SCRIPT_X}` - Analysis script names
- `{FUNCTIONAL_TEST_APPROACH}` - Testing approach description
- `{FUNCTIONAL_TEST_DESCRIPTION}` - How functional testing works
- `{FUNCTIONAL_TEST_EXAMPLE_SCENARIO}` - Example scenario tested
- `{FUNCTIONAL_TEST_EXAMPLE_RESULT}` - Example result
- `{FUNCTIONAL_TEST_EXAMPLE_FILE}` - Example report filename
- `{MOCK_INFRASTRUCTURE_GUIDE_FILE}` - Mock infrastructure guide (if applicable)
- `{REPRESENTATIVE_SCENARIO_X}` - Representative scenarios for testing

### Agent Tracking
- `{BATCH_X_AGENT_COUNT}` - Agent IDs per batch
- `{TOTAL_AGENT_IDS}` - Total agent IDs across all batches

---

## Customization Guide

**See HARNESS_GUIDE.md for detailed customization instructions.**

**Quick customization steps:**

1. **Replace project identity placeholders** (top section)
   - Search for `{PROJECT_NAME}`, `{DOMAIN_CONTEXT}`, `{EXPERIMENT_PATH}`

2. **Configure test matrix** (Test Matrix section)
   - Define your prompts (P1, P2, P3...)
   - Define your criteria (S1, S2, S3...)
   - Choose models to test

3. **Customize simulation prompts** (Phase 1 section)
   - Update `{AGENT_NAME}` and `{AGENT_DESCRIPTION}`
   - Tailor `{SIMULATION_TASK_INSTRUCTIONS}` to your domain
   - Add domain-specific constraints

4. **Define evaluation rubric** (Phase 2 section)
   - Choose rubric dimensions that matter for your domain
   - Set point values (ensure they sum to total)
   - Write clear criteria for each dimension

5. **Configure validators** (Phase 4 section)
   - List file types you'll validate
   - Specify validation tools
   - Set pass/fail thresholds

6. **Update time estimates** (Quick Reference section)
   - Run a few tests to calibrate estimates
   - Update based on your domain complexity

7. **Remove this section** when generating final harness
