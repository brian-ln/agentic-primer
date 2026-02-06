# HARNESS_TEMPLATE.md - Usage Guide

**Purpose:** This guide explains how to use HARNESS_TEMPLATE.md to create a custom simulation harness for your project.

**Reference:** See HARNESS_GUIDE.md for comprehensive harness concepts and methodology.

---

## Quick Start

### Option 1: Manual Customization (Immediate)

1. **Copy the template:**
   ```bash
   cp HARNESS_TEMPLATE.md experiments/my-project/SIMULATION_HARNESS.md
   ```

2. **Search and replace placeholders:**
   ```bash
   # In your editor, search for { and replace each placeholder
   # OR use sed for bulk replacement (carefully!)
   ```

3. **Validate completeness:**
   ```bash
   # Ensure no placeholders remain
   grep -n '{.*}' experiments/my-project/SIMULATION_HARNESS.md
   ```

4. **Create supporting files:**
   - Prompts: `experiments/my-project/prompts/P1.txt`, etc.
   - Criteria: `experiments/my-project/criteria/S1.txt`, etc.

### Option 2: Skill-Based Generation (Phase 2)

```
/create-harness
```

The skill will:
- Ask you questions about your project
- Generate a customized harness from the template
- Fill in all placeholders automatically
- Create directory structure and supporting files

---

## Placeholder Categories

### 1. Project Identity

These define what your project is about.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{PROJECT_NAME}` | "Bootstrap Simulation Harness" | Title | Human-readable project name |
| `{DOMAIN_CONTEXT}` | "GitHub automation" | Introduction | What domain you're testing |
| `{EXPERIMENT_NAME}` | "iteration-2" | Paths | Directory name |
| `{EXPERIMENT_PATH}` | "iteration-2" | File paths | Relative path from experiments/ |
| `{RUN_NAME}` | "full-matrix" | Run directories | Run naming pattern |

**Customization Notes:**
- `{PROJECT_NAME}` appears in the title and should be descriptive
- `{DOMAIN_CONTEXT}` should be 2-5 words describing what you're simulating
- `{EXPERIMENT_PATH}` must match your actual directory structure

---

### 2. Test Matrix Configuration

These define the dimensions of your test matrix.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{PROMPTS_COUNT}` | 3 | Test Matrix | Number of prompt variants |
| `{CRITERIA_COUNT}` | 3 | Test Matrix | Number of success criteria |
| `{MODELS_COUNT}` | 3 | Test Matrix | Number of models to test |
| `{TOTAL_TESTS}` | 27 | Throughout | prompts × criteria × models |
| `{BATCH_COUNT}` | 3 | Batching | Number of batches (usually = prompts) |
| `{BATCH_SIZE}` | 9 | Batching | Tests per batch (criteria × models) |

**Calculation:**
```
TOTAL_TESTS = PROMPTS_COUNT × CRITERIA_COUNT × MODELS_COUNT
BATCH_SIZE = CRITERIA_COUNT × MODELS_COUNT
BATCH_COUNT = PROMPTS_COUNT
```

**Example Matrix Sizes:**
- **2×3×2 = 12 tests** (quick exploration)
- **3×3×3 = 27 tests** (standard, balanced)
- **5×4×3 = 60 tests** (comprehensive, expensive)

---

### 3. Prompt Variants

These describe your prompt variations.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{P1_FILENAME}` | "P1-minimal.txt" | Prompt Variants | Filename for P1 |
| `{P1_DESCRIPTION}` | "10 words, minimal context" | Prompt Variants | What P1 is |
| `{P1_WORD_COUNT}` | 10 | Prompt Variants | Length of P1 |
| `{PROMPTS_LIST}` | "P1 vs P2 vs P3" | Analysis | List for reporting |

**Repeat for P2, P3, P4, etc.**

**Common Prompt Variation Patterns:**

**Pattern A: Length-based**
- P1: Minimal (5-10 words) - "Build GitHub automation"
- P2: Moderate (15-20 words) - "Build GitHub automation that responds to issues and creates solutions"
- P3: Detailed (30-50 words) - "Build GitHub automation using workflows that responds to issues, creates solutions, and verifies success"

**Pattern B: Specificity-based**
- P1: Abstract - "Design an API"
- P2: Structured - "Design a REST API with authentication"
- P3: Concrete - "Design a REST API with OAuth2, rate limiting, and versioning"

**Pattern C: Context-based**
- P1: No context - "Optimize the pipeline"
- P2: Partial context - "Optimize the ML training pipeline"
- P3: Full context - "Optimize the ML training pipeline for GPU utilization and data loading"

---

### 4. Success Criteria Variants

These describe your success criteria variations.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{S1_FILENAME}` | "S1-minimal.txt" | Criteria Variants | Filename for S1 |
| `{S1_DESCRIPTION}` | "single requirement" | Criteria Variants | What S1 is |
| `{S1_REQUIREMENTS_COUNT}` | 1 | Criteria Variants | Number of requirements |
| `{CRITERIA_LIST}` | "S1 vs S2 vs S3" | Analysis | List for reporting |

**Repeat for S2, S3, S4, etc.**

**Common Criteria Variation Patterns:**

**Pattern A: Count-based**
- S1: 1 requirement - "Must work"
- S2: 3 requirements - "Must work", "Must be secure", "Must be documented"
- S3: 7 requirements - Full checklist of observable outcomes

**Pattern B: Precision-based**
- S1: Vague - "Should be good"
- S2: Moderate - "Should pass tests and be maintainable"
- S3: Precise - "Must pass 95% of unit tests, have <10% code duplication, and follow style guide"

**Pattern C: Scope-based**
- S1: Core only - "Implement core functionality"
- S2: Core + docs - "Implement core functionality and documentation"
- S3: Production-ready - "Implement core functionality, docs, tests, CI/CD, monitoring"

---

### 5. Models

These specify which models to test.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{MODEL_1}` | "opus" | Throughout | Model identifier 1 |
| `{MODEL_2}` | "sonnet" | Throughout | Model identifier 2 |
| `{MODEL_3}` | "haiku" | Throughout | Model identifier 3 |
| `{MODELS_LIST}` | "opus vs sonnet vs haiku" | Analysis | List for reporting |

**Standard Model Configurations:**

**Full spectrum (3 models):**
- opus (claude-opus-4-5) - Most capable, slowest, most expensive
- sonnet (claude-sonnet-4-5) - Balanced capability and speed
- haiku (claude-haiku-4) - Fastest, least expensive, good for simple tasks

**Performance focus (2 models):**
- opus - Best performance
- sonnet - Production baseline

**Cost-optimized (2 models):**
- sonnet - Production baseline
- haiku - Cost-sensitive alternative

**Single model (1 model):**
- sonnet - Standard choice for most testing

---

### 6. Simulation Context

These define the agent being simulated and its task.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{AGENT_NAME}` | "@copilot" | Phase 1 | Name of simulated agent |
| `{AGENT_DESCRIPTION}` | "GitHub CoPilot agent..." | Phase 1 | Full description of role |
| `{SIMULATION_TASK_INSTRUCTIONS}` | "Acting as @copilot..." | Phase 1 | What agent should do |
| `{SIMULATION_CONSTRAINTS}` | "DO NOT ACTUALLY CALL..." | Phase 1 | Simulation guardrails |
| `{DOMAIN_SPECIFIC_CONSTRAINTS}` | "Simulate GitHub operations..." | Phase 1 | Domain limitations |

**Examples by Domain:**

**GitHub Automation:**
```
AGENT_NAME: "@copilot"
AGENT_DESCRIPTION: "the GitHub CoPilot agent that can do work autonomously when assigned issues"
SIMULATION_TASK_INSTRUCTIONS: "Acting as @copilot and given the PROMPT and the SUCCESS CRITERIA design the solution, describe it in a single markdown file, then implement and verify it."
SIMULATION_CONSTRAINTS: "Remember, this is a simulation and you are pretending to act just like @copilot would. DO NOT ACTUALLY CALL ANY GITHUB APIS, PROGRAMS, or SCRIPTS to do the work. Simulate them being called if necessary."
DOMAIN_SPECIFIC_CONSTRAINTS: "Simulate GitHub operations (no APIs, git push, or state changes)."
```

**API Design:**
```
AGENT_NAME: "the API architect"
AGENT_DESCRIPTION: "an experienced API designer who creates RESTful APIs following industry best practices"
SIMULATION_TASK_INSTRUCTIONS: "Given the PROMPT and SUCCESS CRITERIA, design a complete API including endpoints, schemas, authentication, and documentation."
SIMULATION_CONSTRAINTS: "This is a design exercise - create specifications, not implementations."
DOMAIN_SPECIFIC_CONSTRAINTS: "Use OpenAPI 3.0 format. Include example requests/responses."
```

**ML Pipeline:**
```
AGENT_NAME: "the ML engineer"
AGENT_DESCRIPTION: "a machine learning engineer who optimizes training pipelines for performance and cost"
SIMULATION_TASK_INSTRUCTIONS: "Given the PROMPT and SUCCESS CRITERIA, design an ML training pipeline with data loading, model training, and evaluation components."
SIMULATION_CONSTRAINTS: "Focus on pipeline design, not model architecture. Assume PyTorch/TensorFlow."
DOMAIN_SPECIFIC_CONSTRAINTS: "Optimize for GPU utilization and data throughput. Use distributed training patterns."
```

---

### 7. Rubric Configuration

These define how simulations are scored.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{RUBRIC_TOTAL_POINTS}` | 100 | Phase 2 | Total points available |
| `{RUBRIC_DIMENSION_1_NAME}` | "COMPLETENESS" | Phase 2 | First dimension name |
| `{RUBRIC_DIMENSION_1_POINTS}` | 30 | Phase 2 | Points for dimension 1 |
| `{RUBRIC_DIMENSION_X_CRITERIA_Y}` | "File coverage..." | Phase 2 | Criteria for scoring |
| `{RUBRIC_DIMENSIONS_LIST}` | "Completeness, Correctness..." | Analysis | List for reporting |

**Standard Rubric (100 points):**
```
1. COMPLETENESS (30 pts) - File coverage: Expected vs actual files created
2. CORRECTNESS (25 pts) - Syntax validity: yamllint, shellcheck, markdownlint pass
3. ACTIONABILITY (20 pts) - Ready to use immediately vs rework required
4. SPECIFICITY (15 pts) - Placeholder density (count TODOs, FIXMEs, etc.)
5. INSIGHT QUALITY (10 pts) - Novel approaches, explicit assumptions, edge cases
```

**Custom Rubric Examples:**

**Security-focused:**
```
1. SECURITY (30 pts) - Vulnerability prevention, secure defaults
2. CORRECTNESS (25 pts) - Logic correctness, edge case handling
3. COMPLETENESS (20 pts) - All security controls implemented
4. AUDITABILITY (15 pts) - Logging, monitoring, traceability
5. DOCUMENTATION (10 pts) - Security considerations documented
```

**Performance-focused:**
```
1. EFFICIENCY (30 pts) - Algorithmic complexity, resource usage
2. SCALABILITY (25 pts) - Handles load increases gracefully
3. CORRECTNESS (20 pts) - Produces correct results
4. OBSERVABILITY (15 pts) - Metrics, profiling, debugging
5. COMPLETENESS (10 pts) - All optimization opportunities addressed
```

**Domain-specific (ML):**
```
1. MODEL QUALITY (30 pts) - Architecture appropriateness, hyperparameters
2. DATA PIPELINE (25 pts) - Data loading, augmentation, preprocessing
3. TRAINING LOOP (20 pts) - Optimization, regularization, checkpointing
4. EVALUATION (15 pts) - Metrics, validation, testing
5. REPRODUCIBILITY (10 pts) - Seeds, versioning, documentation
```

---

### 8. Evaluation Configuration

These define automated and manual evaluation metrics.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{ENHANCED_RUBRIC_POINTS}` | 120 | Phase 4 | Enhanced rubric total |
| `{AUTOMATED_DIMENSIONS_POINTS}` | 80 | Phase 4 | Automated portion |
| `{MANUAL_DIMENSIONS_POINTS}` | 40 | Phase 4 | Manual review portion |
| `{AUTOMATED_DIMENSION_X_NAME}` | "Functional Verification" | Phase 4 | Automated metric name |
| `{MANUAL_DIMENSION_X_NAME}` | "Research Quality" | Phase 4 | Manual metric name |
| `{FILE_TYPE_X}` | "YAML files" | Phase 4 | File type to validate |
| `{VALIDATOR_X}` | "yamllint" | Phase 4 | Validation tool |
| `{VALIDATOR_X_PURPOSE}` | "GitHub workflows..." | Phase 4 | What validator checks |

**Example Enhanced Rubric (120 points):**

**Automated (80 points):**
1. Functional Verification (30 pts) - Syntax validation, workflow triggers, structure
2. Completeness Calibration (25 pts) - File count vs optimal range for prompt level
3. Actionability (15 pts) - Placeholder density, contextual appropriateness
4. Specificity (10 pts) - Inappropriate placeholders, prompt reference specificity

**Manual (40 points):**
5. Correctness (20 pts) - Logic trace, semantic correctness, would it work?
6. Research Quality (15 pts) - WebSearch usage, source currency (2026), relevance
7. Insight Quality (5 pts) - Novel approaches, explicit assumptions, edge cases noted

---

### 9. Time Estimates

These help with planning and scheduling.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{SINGLE_TEST_TOTAL_TIME}` | "3-5 minutes" | Quick Reference | Total for one test |
| `{SINGLE_TEST_PHASE1_TIME}` | "2 min" | Quick Reference | Simulation phase |
| `{SINGLE_TEST_PHASE1B_TIME}` | "30 sec" | Quick Reference | Reflection phase |
| `{SINGLE_TEST_PHASE2_TIME}` | "2 min" | Quick Reference | Evaluation phase |
| `{FULL_MATRIX_TOTAL_TIME}` | "20-30 minutes" | Quick Reference | All tests total |
| `{BATCH_TIME}` | "7-10 minutes" | Quick Reference | Time per batch |
| `{AGGREGATE_TIME}` | "5 minutes" | Quick Reference | Phase 3 analysis |
| `{SEQUENTIAL_TIME}` | "90-120 minutes" | Quick Reference | If run sequentially |
| `{PARALLEL_TIME}` | "20-30 minutes" | Quick Reference | With parallelism |
| `{SPEEDUP_FACTOR}` | "3-4" | Quick Reference | Speedup multiplier |

**How to Estimate:**

1. **Run a single test first** to calibrate
2. **Measure each phase** separately
3. **Account for model differences:**
   - Opus: 1.5-2x slower than Sonnet
   - Haiku: 0.5-0.7x Sonnet time
4. **Consider complexity:**
   - Simple tasks: Lower end of estimates
   - Complex tasks: Upper end or higher
5. **Parallelism benefits:**
   - Batch speedup: ~3-4x for 9 parallel agents
   - Diminishing returns beyond 9-12 parallel agents

**Example Time Estimates (Bootstrap Project):**
```
Single Test (P3+S2+sonnet):
- Phase 1: ~2 min (simulation)
- Phase 1b: ~30 sec (reflection)
- Phase 2: ~2 min (evaluation)
- Total: ~3-5 minutes

Full Matrix (27 tests):
- Batch 1 (P1): ~7-10 min (9 parallel sims + 9 parallel evals)
- Batch 2 (P2): ~7-10 min
- Batch 3 (P3): ~7-10 min
- Phase 3: ~5 min (aggregate analysis)
- Total: ~20-30 minutes

If Sequential: ~90-120 minutes
Speedup: 3-4x
```

---

### 10. Pass/Fail Thresholds

These define quality gates.

| Placeholder | Example | Where Used | Description |
|------------|---------|------------|-------------|
| `{SYNTAX_PASS_THRESHOLD}` | 80 | Phase 4 | % files passing syntax |
| `{SYNTAX_WARNING_LOW}` | 60 | Phase 4 | Warning range low |
| `{SYNTAX_WARNING_HIGH}` | 79 | Phase 4 | Warning range high |
| `{RUBRIC_PASS_THRESHOLD}` | 80 | Phase 4 | Points for PASS |
| `{RUBRIC_PASS_PERCENT}` | 67 | Phase 4 | % for PASS |
| `{RUBRIC_FAIL_THRESHOLD}` | 60 | Phase 4 | Points for FAIL |
| `{FUNCTIONAL_PASS_CRITERIA}` | "Logic trace shows..." | Phase 4 | Functional PASS criteria |

**Standard Thresholds:**

**Syntax Validation:**
- PASS: ≥80% files pass validation
- WARNING: 60-79% pass rate
- FAIL: <60% pass rate

**Rubric Scoring:**
- PASS: ≥67% of total points (e.g., ≥80/120)
- ACCEPTABLE: 50-66% (e.g., 60-79/120)
- FAIL: <50% (e.g., <60/120)

**Functional Testing:**
- PASS: Logic trace shows it would work, all criteria satisfied, no critical flaws
- WARNING: Works with caveats (hardcoded values, missing edge cases, minor issues)
- FAIL: Would not execute, violates criteria, critical logic errors

**Adjust Based on Domain:**
- **High-stakes domains** (security, healthcare): Raise thresholds (e.g., 90% syntax pass)
- **Exploratory work**: Lower thresholds (e.g., 60% syntax pass acceptable)
- **Production code**: Strict functional testing (PASS = would deploy as-is)

---

## Common Customization Scenarios

### Scenario 1: Smaller Matrix (2×3×2 = 12 tests)

**Changes needed:**
```
{PROMPTS_COUNT} = 2
{CRITERIA_COUNT} = 3
{MODELS_COUNT} = 2
{TOTAL_TESTS} = 12
{BATCH_COUNT} = 2
{BATCH_SIZE} = 6

Remove P3 section, keep P1 and P2
Remove {MODEL_3} references
Adjust batch strategy to 2 batches of 6
Update agent ID tracking template
```

**Why:** Faster exploration, lower cost, good for initial validation

---

### Scenario 2: Single Model Testing (3×3×1 = 9 tests)

**Changes needed:**
```
{MODELS_COUNT} = 1
{TOTAL_TESTS} = 9
{BATCH_SIZE} = 3

Keep only {MODEL_2} (sonnet) or your chosen model
Remove model comparison from Phase 3 analysis
Simplify batch strategy: 3 batches of 3
```

**Why:** Focus on prompt/criteria variations, not model differences. Cost-effective.

---

### Scenario 3: Custom Rubric (Security-focused)

**Changes needed:**
```
{RUBRIC_TOTAL_POINTS} = 100
{RUBRIC_DIMENSION_1_NAME} = "SECURITY"
{RUBRIC_DIMENSION_1_POINTS} = 30
{RUBRIC_DIMENSION_1_CRITERIA_1} = "No hardcoded secrets"
{RUBRIC_DIMENSION_1_CRITERIA_2} = "Input validation present"

Update all 5 dimensions for security focus
Update {RUBRIC_DIMENSIONS_LIST} = "Security, Correctness, Completeness, Auditability, Documentation"
```

**Why:** Domain requires specialized evaluation criteria

---

### Scenario 4: No Validation Phase (Conceptual Testing)

**Changes needed:**
```
Remove Phase 4 entirely
Update Quick Reference to only mention Phases 1-3
Remove validation-related placeholders
```

**Why:** Testing conceptual/design work that doesn't produce validatable code

---

### Scenario 5: Multi-Domain Testing

**Changes needed:**
```
{DOMAIN_CONTEXT} = "Multi-domain system design"
{AGENT_NAME} = "the system architect"

Define domain-specific constraints for each prompt variant:
P1: Focus on frontend
P2: Focus on backend
P3: Full-stack design

Adjust rubric to include domain coverage dimension
```

**Why:** Testing agent performance across different architectural layers

---

## Validation Checklist

After customizing your harness, verify:

- [ ] **No placeholders remain** (search for `{`)
- [ ] **Math is correct** (TOTAL_TESTS = PROMPTS × CRITERIA × MODELS)
- [ ] **Batch sizes match** (BATCH_SIZE = CRITERIA × MODELS)
- [ ] **Rubric points sum** (dimension points = RUBRIC_TOTAL_POINTS)
- [ ] **File paths are valid** (experiment paths exist)
- [ ] **Prompt files exist** (P1.txt, P2.txt, P3.txt in prompts/)
- [ ] **Criteria files exist** (S1.txt, S2.txt, S3.txt in criteria/)
- [ ] **Models are valid** (claude-opus-4-5, claude-sonnet-4-5, claude-haiku-4)
- [ ] **Time estimates are realistic** (run one test to calibrate)
- [ ] **Validators are installed** (yamllint, shellcheck, etc.)
- [ ] **Thresholds make sense** (not too strict or lenient for domain)
- [ ] **Agent context is clear** (AGENT_NAME and AGENT_DESCRIPTION are specific)
- [ ] **Simulation constraints prevent accidents** (no real API calls, etc.)

---

## Next Steps After Customization

1. **Create supporting files:**
   ```bash
   mkdir -p experiments/my-project/prompts
   mkdir -p experiments/my-project/criteria
   ```

2. **Write prompt variants:**
   ```bash
   vim experiments/my-project/prompts/P1.txt
   vim experiments/my-project/prompts/P2.txt
   vim experiments/my-project/prompts/P3.txt
   ```

3. **Write success criteria:**
   ```bash
   vim experiments/my-project/criteria/S1.txt
   vim experiments/my-project/criteria/S2.txt
   vim experiments/my-project/criteria/S3.txt
   ```

4. **Run a single test** to validate setup:
   ```bash
   # Follow "Running Single Test" section in your customized harness
   ```

5. **Review single test output** and adjust:
   - Are instructions clear to simulating agent?
   - Is evaluation rubric capturing what matters?
   - Are time estimates reasonable?

6. **Run full matrix** once validated

7. **Iterate** based on results

---

## Troubleshooting

### Problem: Agent confused about what to simulate

**Symptoms:**
- Agent asks clarifying questions
- Output doesn't match domain
- Agent refuses to simulate (tries to call real APIs)

**Solution:**
- Make `{AGENT_DESCRIPTION}` more specific
- Add more detail to `{SIMULATION_CONSTRAINTS}`
- Clarify `{DOMAIN_SPECIFIC_CONSTRAINTS}`
- Review prompt variants for clarity

---

### Problem: Rubric scores don't differentiate

**Symptoms:**
- All scenarios get similar scores
- Scores cluster at high or low end
- Evaluators can't distinguish quality

**Solution:**
- Add more specific rubric criteria
- Include examples of different score levels
- Split broad dimensions into narrower ones
- Adjust point distributions to emphasize what matters

---

### Problem: Validations all fail

**Symptoms:**
- <60% syntax pass rate across all scenarios
- Validators report many errors
- Files don't match expected format

**Solution:**
- Check validator configuration (too strict?)
- Review domain constraints (agent misunderstood?)
- Verify file type expectations match prompt
- Consider if validators are appropriate for domain

---

### Problem: Time estimates way off

**Symptoms:**
- Tests take 3-4x longer than estimated
- Agents timeout frequently
- Batch execution exceeds budget

**Solution:**
- Run single test to recalibrate
- Check if prompts are more complex than anticipated
- Consider reducing batch size (less parallelism)
- Adjust timeout_per_simulation in config
- Use faster model (sonnet instead of opus)

---

### Problem: Agent ID tracking breaks down

**Symptoms:**
- Lost track of which agent is which
- Can't resume reflection phase
- Evaluation mismatched to simulation

**Solution:**
- Use Agent ID Tracking Template consistently
- Name agents descriptively (P1-S1-opus-sim, P1-S1-opus-eval)
- Save IDs immediately after launching
- Consider using a spreadsheet for tracking

---

## Reference: Complete Example

**See:** `/Users/bln/play/agentic-primer/SIMULATION_HARNESS.md`

This is the Bootstrap Project harness - a complete, working example showing:
- All placeholders filled in
- Domain-specific customization (GitHub automation)
- Standard 3×3×3 matrix (27 tests)
- 100-point rubric (5 dimensions)
- Enhanced 120-point evaluation framework
- Comprehensive Phase 4 validation

Use it as a reference when customizing your own harness.

---

## See Also

- **HARNESS_GUIDE.md** - Comprehensive harness concepts and methodology (when created)
- **HARNESS_REUSABILITY_ANALYSIS.md** - Analysis of what makes harnesses reusable
- **SIMULATION_HARNESS.md** - Bootstrap project example (working reference)
- **experiments/iteration-2/** - Bootstrap project files (prompts, criteria, runs)
