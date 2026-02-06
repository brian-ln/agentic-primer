# Simulation Algorithm: Bootstrap Prompt Testing Framework

A systematic methodology for evaluating AI agent behavior across prompt variations, success criteria, and model architectures.

---

## Overview

```
INPUT SPACE (27 combinations)
├─ Prompts: P1 (10w), P2 (14w), P3 (35w)
├─ Criteria: S1 (minimal), S2 (moderate), S3 (comprehensive)
└─ Models: Opus, Sonnet, Haiku

METHODOLOGY (3 phases)
├─ Phase 1: Simulation (agent acts as @copilot)
├─ Phase 1b: Self-Reflection (agent analyzes its work)
└─ Phase 2: Evaluation (independent agent scores the output)

OUTPUT
└─ Scored behavioral data + learnings for next iteration
```

---

## The Algorithm

### High-Level Flow

```
FOR EACH combination (P × S × M):
    1. Launch simulation agent with (Prompt, Criteria, Model)
    2. Resume same agent for self-reflection
    3. Launch evaluator agent to score the work
    4. Export logs and collect metrics
    RETURN (simulation_output, reflection, score, metrics)

AGGREGATE all 27 results:
    5. Compare scores across dimensions
    6. Identify optimal configurations
    7. Extract behavioral patterns
    8. Generate learnings for iteration N+1
```

### Detailed Process Flow

```
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 1: SIMULATION (agent acts as @copilot)                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Input:                                                          │
│    - Prompt (P1/P2/P3)                                          │
│    - Success Criteria (S1/S2/S3)                                │
│    - Model (opus/sonnet/haiku)                                  │
│                                                                  │
│  Simulation Prompt Template:                                    │
│    "You are simulating @copilot..."                             │
│    PROMPT: [P#]                                                 │
│    SUCCESS CRITERIA: [S#]                                       │
│    YOUR TASK: Design, describe, implement, verify              │
│                                                                  │
│  Agent Behavior:                                                │
│    [Agent makes decisions autonomously]                         │
│    - May research (WebSearch)                                   │
│    - May read existing files (Read)                             │
│    - May create files (Write)                                   │
│    - May execute commands (Bash)                                │
│    OR may purely analyze (0 tool calls)                         │
│                                                                  │
│  Output:                                                         │
│    - Markdown document with solution design                     │
│    - File manifest (what @copilot would create)                 │
│    - Complete functional content (no placeholders)              │
│    - Assumptions documented                                     │
│                                                                  │
│  Save: agent_id (e.g., ae913f4)                                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 1b: SELF-REFLECTION (same agent analyzes itself)          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Resume: Same agent_id from Phase 1                             │
│                                                                  │
│  Reflection Prompt:                                             │
│    "Self-reflect on how well you did as @copilot..."            │
│                                                                  │
│  Agent Analyzes:                                                │
│    - Confidence level per file (High/Medium/Low)                │
│    - Missing information from prompt                            │
│    - Research conducted (what, why, findings)                   │
│    - What could be improved                                     │
│    - Gaps between what was built vs what works                  │
│                                                                  │
│  Output:                                                         │
│    - Self-reflection analysis in markdown                       │
│    - Honest assessment of limitations                           │
│    - Gap analysis (conceptual vs functional correctness)        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 2: EVALUATION (independent evaluator scores the work)     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Launch: NEW Sonnet agent (always Sonnet for consistency)       │
│                                                                  │
│  Input:                                                          │
│    - Original Prompt (P#)                                       │
│    - Original Criteria (S#)                                     │
│    - Phase 1 simulation output                                  │
│    - Phase 1b self-reflection                                   │
│                                                                  │
│  100-Point Rubric:                                              │
│    1. COMPLETENESS (30 pts)                                     │
│       - File coverage: Expected vs actual                       │
│       - Content depth: Complete vs placeholder                  │
│                                                                  │
│    2. CORRECTNESS (25 pts)                                      │
│       - Syntax validity: yamllint, shellcheck pass              │
│       - Semantic correctness: Would it work?                    │
│                                                                  │
│    3. ACTIONABILITY (20 pts)                                    │
│       - Ready to use immediately? (20)                          │
│       - Minor tweaks needed? (15)                               │
│       - Significant rework? (5)                                 │
│                                                                  │
│    4. SPECIFICITY (15 pts)                                      │
│       - Placeholder density (TODOs, FIXMEs)                     │
│       - Contextual appropriateness                              │
│                                                                  │
│    5. INSIGHT QUALITY (10 pts)                                  │
│       - Novel approaches                                        │
│       - Assumptions stated                                      │
│       - Edge cases noted                                        │
│                                                                  │
│  Output:                                                         │
│    - Score per dimension                                        │
│    - Total /100 + Grade (A/B/C/D/F)                            │
│    - 2-3 Strengths                                              │
│    - 2-3 Weaknesses                                             │
│    - Recommendations                                            │
│                                                                  │
│  Save: evaluator_agent_id (e.g., a0294d5)                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ METRICS EXTRACTION (analyze agent logs)                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Extract from agent-{id}.jsonl:                                 │
│    - Token usage (input, output, cached)                        │
│    - Tool calls (Read, Write, WebSearch, Bash)                  │
│    - Duration (first to last timestamp)                         │
│    - Efficiency (tokens/sec, tools/min)                         │
│    - Web searches performed (queries)                           │
│    - Files created (paths)                                      │
│                                                                  │
│  Classify Behavioral Archetype:                                 │
│    IF tools == 0:                                               │
│        "The Philosopher" (pure reasoning, no tools)             │
│    ELIF websearch > 0 AND write == 0:                           │
│        "The Researcher" (web research + exploration)            │
│    ELIF write > 0:                                              │
│        "The Builder" (creates files, iterates)                  │
│    ELSE:                                                         │
│        "Hybrid pattern"                                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## What Varies (Independent Variables)

### 1. Prompt Length (P1, P2, P3)

| Variant | Word Count | Specificity | Example |
|---------|-----------|-------------|---------|
| **P1-minimal** | 10 words | Minimal | "Bootstrap @copilot issue automation with auto-review and knowledge base." |
| **P2-moderate** | 14 words | Moderate | "Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base." |
| **P3-detailed** | 35 words | Comprehensive | "Create issue-driven development system: Issue template (.github/ISSUE_TEMPLATE/task.yml)..." |

**Hypothesis:** Longer prompts provide more clarity, reducing agent uncertainty and improving output quality.

### 2. Success Criteria (S1, S2, S3)

| Variant | Requirements | Observable Outcomes | Example |
|---------|-------------|---------------------|---------|
| **S1-minimal** | 1 | Functional test only | "System must process a test issue without errors." |
| **S2-moderate** | 3 | Functional + syntax + triggers | "Process test issue, pass syntax validation, GitHub workflow triggers." |
| **S3-comprehensive** | 7 | Full production readiness | "Functional test, syntax valid, observable behavior, 90% reliability, multi-agent, single-command, self-improvement." |

**Hypothesis:** More specific criteria drive more rigorous implementations.

### 3. Model Architecture (Opus, Sonnet, Haiku)

| Model | Archetype | Typical Behavior |
|-------|-----------|------------------|
| **Opus** | The Philosopher | Pure analysis, 0 tool calls, conceptual reasoning |
| **Sonnet** | The Researcher | WebSearch + Read, research-backed recommendations |
| **Haiku** | The Builder | Aggressive file creation, fast iteration |

**Hypothesis:** Model architecture determines behavior more than prompt content.

---

## What Stays Constant (Control Variables)

1. **Simulation Prompt Template** - Same structure for all tests
2. **Evaluation Rubric** - Same 100-point scoring system
3. **Evaluator Model** - Always Sonnet (for consistency)
4. **Methodology** - Three-phase process (simulate, reflect, evaluate)
5. **Output Location** - Structured directory per test
6. **Research Policy** - Web search allowed for all agents
7. **Constraints** - Production-ready code, no placeholders, simulated GitHub ops

---

## How We Measure Quality

### Quantitative Metrics (from rubric)

```
COMPLETENESS (30 pts)
├─ File coverage ratio: actual_files / expected_files
├─ Content depth score: complete_content / total_content
└─ Implementation gap count: missing_features

CORRECTNESS (25 pts)
├─ Syntax validity: yamllint pass/fail
├─ Semantic correctness: would_it_work (subjective)
└─ Integration issues: broken_references

ACTIONABILITY (20 pts)
└─ Ready-to-use scale: immediate (20) | tweaks (15) | rework (5)

SPECIFICITY (15 pts)
├─ Placeholder density: count(TODO, FIXME, placeholder)
└─ Contextual appropriateness: fits_prompt (subjective)

INSIGHT QUALITY (10 pts)
├─ Novel approaches: count(unique_insights)
├─ Assumptions documented: count(explicit_assumptions)
└─ Edge cases noted: count(edge_cases)

TOTAL = 100 points → Grade (A≥90, B≥80, C≥70, D≥60, F<60)
```

### Qualitative Metrics (from logs)

```
BEHAVIORAL SIGNATURE
├─ Archetype: Philosopher | Researcher | Builder | Hybrid
├─ Tool usage pattern: {Read: N, Write: M, WebSearch: K, Bash: J}
├─ Research depth: web_searches_performed
└─ Build velocity: files_created / duration

EFFICIENCY
├─ Tokens per tool: total_tokens / tool_calls
├─ Tokens per second: total_tokens / duration_sec
├─ Tools per minute: (tool_calls × 60) / duration_sec
└─ Cache efficiency: cache_read / (input_tokens + cache_read)

METACOGNITIVE ACCURACY
└─ Confidence vs correctness: self_reflection_confidence ~ evaluator_score
```

---

## The Feedback Loop (Iteration N → N+1)

### Loop Structure

```
ITERATION N:
    Run 27 simulations
    Collect scores + metrics
    Identify patterns
    ↓
    LEARNING:
    - What worked? (high scores)
    - What failed? (low scores)
    - What surprised us? (unexpected behaviors)
    ↓
    HYPOTHESIS REFINEMENT:
    - Update prompt templates
    - Adjust success criteria
    - Modify evaluation rubric
    ↓
ITERATION N+1:
    Test refined hypotheses
    Validate learnings
    Discover new patterns
    [REPEAT]
```

### Key Learnings from Iteration 2

#### 1. Production-Ready Pattern Discovery

**Finding:** Detailed prompts (P3) + moderate criteria (S2) + Sonnet = highest scores (70-85%)

**Evidence:**
- P3+S2+Sonnet: Research-backed, syntactically valid, functionally complete
- P1+S1+Opus: Conceptually correct but implementation gaps (~30% functional)
- P2+S3+Haiku: Over-builds without research, misses requirements

**Implication:** There's a "clarity sweet spot" where detail level matches model capability.

#### 2. WebSearch Usage Pattern

**Finding:** Agents that use WebSearch (Sonnet) produce more accurate implementations

**Evidence:**
- Sonnet agents: Average 3 web searches, 70-85% correctness
- Opus agents: 0 web searches, 30-60% correctness (outdated assumptions)
- Haiku agents: 0-1 web searches, 70% correctness (builds confidently, sometimes wrong)

**Implication:** Current documentation (2026) research improves accuracy significantly.

#### 3. Emulation Accuracy Gap

**Finding:** Simulated tests hide implementation gaps (false positives)

**Evidence from Self-Reflection:**
```
"Would this meet success criteria?"
- In simulation: Yes (100% pass)
- In reality: No (~25% functional)

Critical gaps:
- No @copilot trigger mechanism
- Missing copilot-setup-steps.yml
- Placeholder values (@owner)
- No error handling
```

**Implication:** Evaluation must distinguish "syntactically valid" from "functionally correct"

#### 4. Haiku Clarity Threshold

**Finding:** Haiku behavior flips at ~30-35 word prompts

**Evidence:**
- P1 (10w) + Haiku: Aggressive file creation (6+ files, 4-5 min)
- P2 (14w) + Haiku: Still builds (5-6 files, 3-4 min)
- P3 (35w) + Haiku: Switches to analysis mode (research, no builds, 2 min)

**Hypothesis:** Longer prompts overwhelm Haiku's context, triggering defensive "plan first" behavior

#### 5. Model Architecture > Prompt Length

**Finding:** Model determines behavior archetype regardless of prompt detail

**Evidence:**
- Opus: Always analyzes (0 tools) whether P1, P2, or P3
- Sonnet: Always researches (WebSearch) whether S1, S2, or S3
- Haiku: Builds until prompt exceeds threshold

**Implication:** Choose model for desired behavior, then tune prompt for quality.

---

## Aggregate Analysis Process

After all 27 simulations complete:

```python
# Pseudo-code for Phase 3 aggregate analysis

def aggregate_analysis(results: List[SimulationResult]):
    """
    Analyze 27 simulation results to identify patterns
    """

    # 1. Score Distribution by Dimension
    for dimension in [Completeness, Correctness, Actionability, Specificity, Insight]:
        plot_distribution(results, dimension)
        identify_outliers(results, dimension)

    # 2. Model Performance Comparison
    opus_avg = mean([r.score for r in results if r.model == "opus"])
    sonnet_avg = mean([r.score for r in results if r.model == "sonnet"])
    haiku_avg = mean([r.score for r in results if r.model == "haiku"])

    # 3. Prompt Length Impact (fix criteria + model)
    for model in [opus, sonnet, haiku]:
        for criteria in [S1, S2, S3]:
            compare_scores([P1, P2, P3], model, criteria)

    # 4. Success Criteria Impact (fix prompt + model)
    for model in [opus, sonnet, haiku]:
        for prompt in [P1, P2, P3]:
            compare_scores(prompt, [S1, S2, S3], model)

    # 5. Optimal Configuration
    best = max(results, key=lambda r: r.total_score)
    print(f"Highest scoring: {best.prompt} + {best.criteria} + {best.model} = {best.score}/100")

    # 6. Pattern Analysis (correlations)
    correlate(websearch_count, correctness_score)
    correlate(prompt_length, completion_time)
    correlate(tool_calls, actionability_score)

    # 7. Recommendations for Iteration 3
    learnings = extract_insights(results)
    hypotheses = generate_hypotheses(learnings)
    return next_iteration_plan(hypotheses)
```

### Example Output Tables

**Scores by Model:**

| Model | Avg Score | Completeness | Correctness | Actionability |
|-------|-----------|-------------|-------------|---------------|
| Opus | 48/100 | 18/30 | 12/25 | 8/20 |
| Sonnet | 78/100 | 26/30 | 21/25 | 17/20 |
| Haiku | 71/100 | 24/30 | 19/25 | 15/20 |

**Best Performers (Top 5):**

| Rank | Configuration | Score | Time | Archetype |
|------|--------------|-------|------|-----------|
| 1 | P3+S2+Sonnet | 85/100 | 3m27s | Researcher |
| 2 | P3+S3+Sonnet | 83/100 | 4m12s | Researcher |
| 3 | P2+S2+Haiku | 76/100 | 3m41s | Builder |
| 4 | P2+S2+Sonnet | 74/100 | 2m55s | Researcher |
| 5 | P1+S2+Haiku | 73/100 | 4m58s | Builder |

**Correlations:**

| Factor A | Factor B | Correlation | p-value |
|----------|----------|-------------|---------|
| WebSearch count | Correctness score | +0.82 | <0.001 |
| Prompt length (words) | Completion time | +0.61 | <0.01 |
| Tool calls | Actionability | +0.54 | <0.05 |
| Placeholder count | Specificity score | -0.91 | <0.001 |

---

## Parallel Execution Strategy

To reduce total runtime from ~90 minutes (sequential) to ~20 minutes (parallel):

### Batch Processing

```
BATCH 1: P1 × (S1,S2,S3) × (opus,sonnet,haiku) = 9 simulations
    Phase 1: Launch 9 agents in SINGLE message (parallel)
        Wait for all to complete → Save 9 agent IDs
    Phase 1b: Resume 9 agents in SINGLE message (parallel)
        Wait for all to complete → Same 9 agent IDs
    Phase 2: Launch 9 evaluators in SINGLE message (parallel)
        Wait for all to complete → Save 9 evaluator IDs

    Total: 18 agent IDs (9 simulation + 9 evaluation)
    Time: ~7-10 minutes

BATCH 2: P2 × (S1,S2,S3) × (opus,sonnet,haiku) = 9 simulations
    [Same structure]
    Time: ~7-10 minutes

BATCH 3: P3 × (S1,S2,S3) × (opus,sonnet,haiku) = 9 simulations
    [Same structure]
    Time: ~7-10 minutes

AGGREGATE: Phase 3 analysis across all 27 results
    Time: ~5 minutes

TOTAL: ~20-30 minutes (vs ~90 minutes sequential)
Speedup: 3-4x
```

### Dependency Management

```
PARALLEL (no dependencies):
    - All Phase 1 simulations within a batch
    - All Phase 1b reflections within a batch (resume operations)
    - All Phase 2 evaluations within a batch

SEQUENTIAL (dependencies):
    - Phase 1 MUST complete before Phase 1b
    - Phase 1b MUST complete before Phase 2
    - All Phase 2 MUST complete before Phase 3 aggregate
```

---

## Execution Checklist

### Single Test (P3+S2+Sonnet)

- [ ] 1. Create run directory: `./scripts/create-experiment-run.sh iteration-2 P3-S2-sonnet`
- [ ] 2. Launch Phase 1 (simulation): Task with P3+S2, model=sonnet
- [ ] 3. Save agent ID: `ae913f4`
- [ ] 4. Resume Phase 1b (reflection): Task resume `ae913f4`
- [ ] 5. Launch Phase 2 (evaluation): Task with rubric, model=sonnet
- [ ] 6. Save evaluator ID: `a0294d5`
- [ ] 7. Finalize: `./scripts/finalize-experiment-run.sh iteration-2/runs/run-{timestamp} ae913f4 a0294d5`
- [ ] 8. Review: Read `logs/analysis-*.md` for metrics

Expected time: ~3-5 minutes

### Full Matrix (27 Tests)

- [ ] 1. Create run directory: `./scripts/create-experiment-run.sh iteration-2 full-matrix`
- [ ] 2. Batch 1 (P1): Launch 9 simulations in parallel → Resume 9 reflections → Launch 9 evaluators
- [ ] 3. Batch 2 (P2): Launch 9 simulations in parallel → Resume 9 reflections → Launch 9 evaluators
- [ ] 4. Batch 3 (P3): Launch 9 simulations in parallel → Resume 9 reflections → Launch 9 evaluators
- [ ] 5. Track all 54 agent IDs (27 simulation + 27 evaluation)
- [ ] 6. Finalize: `./scripts/finalize-experiment-run.sh iteration-2/runs/run-{timestamp} <all-54-ids>`
- [ ] 7. Phase 3: Launch aggregate analysis agent with all 27 evaluation results
- [ ] 8. Review: Cross-run comparison, optimal configuration, pattern analysis

Expected time: ~20-30 minutes

---

## Validation Criteria

### For a Single Simulation

**Success indicators:**
- ✅ All 3 phases complete without errors
- ✅ Simulation produces markdown output in correct directory
- ✅ Self-reflection includes confidence scores and gap analysis
- ✅ Evaluation provides justified rubric scores (not just numbers)
- ✅ Logs exported successfully with complete metrics
- ✅ Analysis shows clear behavioral signature

**Failure indicators:**
- ❌ Agent errors or timeouts
- ❌ Self-evaluation instead of two-phase methodology
- ❌ Missing token/tool metrics in logs
- ❌ Evaluation scores without justification
- ❌ Simulation output in wrong location

### For Full Matrix

**Success indicators:**
- ✅ All 27 simulations complete
- ✅ Consistent evaluation methodology across all tests
- ✅ Clear score distribution patterns
- ✅ Statistically significant correlations identified
- ✅ Optimal configuration emerges from data
- ✅ Behavioral archetypes validated

**Failure indicators:**
- ❌ Incomplete test coverage (missing combinations)
- ❌ Inconsistent evaluation (different rubrics)
- ❌ No clear patterns (random noise)
- ❌ Evaluator bias (all scores similar)
- ❌ Missing aggregate analysis

---

## Visual Algorithm Summary

```
┌─────────────────────────────────────────────────────────────────┐
│                    SIMULATION ALGORITHM                          │
│                                                                  │
│  FOR EACH (Prompt, Criteria, Model) in 3×3×3 = 27:              │
│                                                                  │
│    ┌──────────────────────────────────────────────────┐         │
│    │ Phase 1: SIMULATE                                │         │
│    │   Input: P#, S#, Model                           │         │
│    │   Agent: Acts as @copilot                        │         │
│    │   Output: Solution design + files               │         │
│    │   Save: agent_id                                 │         │
│    └──────────────────────────────────────────────────┘         │
│                          │                                       │
│                          ▼                                       │
│    ┌──────────────────────────────────────────────────┐         │
│    │ Phase 1b: REFLECT                                │         │
│    │   Resume: Same agent_id                          │         │
│    │   Agent: Self-analyzes work                      │         │
│    │   Output: Confidence, gaps, learnings            │         │
│    └──────────────────────────────────────────────────┘         │
│                          │                                       │
│                          ▼                                       │
│    ┌──────────────────────────────────────────────────┐         │
│    │ Phase 2: EVALUATE                                │         │
│    │   Launch: NEW Sonnet evaluator                   │         │
│    │   Input: Phase 1 + 1b outputs                    │         │
│    │   Score: 100-point rubric                        │         │
│    │   Output: Scores + strengths + weaknesses        │         │
│    │   Save: evaluator_id                             │         │
│    └──────────────────────────────────────────────────┘         │
│                          │                                       │
│                          ▼                                       │
│    ┌──────────────────────────────────────────────────┐         │
│    │ EXTRACT METRICS                                  │         │
│    │   Parse: agent-{id}.jsonl logs                   │         │
│    │   Metrics: Tokens, tools, duration, efficiency   │         │
│    │   Classify: Behavioral archetype                 │         │
│    └──────────────────────────────────────────────────┘         │
│                                                                  │
│  NEXT combination                                                │
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐     │
│  │ Phase 3: AGGREGATE (after all 27)                     │     │
│  │   Compare: Scores by model, prompt, criteria          │     │
│  │   Identify: Optimal configuration                     │     │
│  │   Patterns: Correlations and insights                 │     │
│  │   Generate: Recommendations for iteration N+1         │     │
│  └────────────────────────────────────────────────────────┘     │
│                          │                                       │
│                          ▼                                       │
│                   LEARNINGS → Next Iteration                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## File Artifacts

After a complete run, the directory structure looks like:

```
experiments/iteration-2/runs/run-20260106-003027-full-matrix/
├── README.md                          # Run summary
├── config.json                        # Test configuration
│
├── P1-S1-opus/                        # Individual test outputs
│   ├── SOLUTION.md                    # Phase 1: Simulation output
│   ├── SELF_REFLECTION.md             # Phase 1b: Reflection
│   └── EVALUATION.md                  # Phase 2: Scores + commentary
│
├── P1-S1-sonnet/
│   └── ...
│
├── [... 25 more test directories ...]
│
├── logs/
│   ├── agent-ae913f4.compact.jsonl   # Compact log schema
│   ├── agent-a0294d5.compact.jsonl
│   └── analysis-20260106-120000.md   # Metrics analysis
│
├── results/
│   ├── scores-by-model.csv           # Aggregate scores
│   ├── correlations.csv              # Statistical analysis
│   └── behavioral-signatures.json    # Archetype classifications
│
└── comparison.md                      # Phase 3: Cross-run analysis
                                      # - Optimal configuration
                                      # - Key findings
                                      # - Recommendations
```

---

## Key Insights (Iteration 2 Learnings)

### 1. Clarity Sweet Spot

**Finding:** P3 (35 words) + S2 (moderate criteria) produces optimal results

**Why:**
- Enough detail to guide without overwhelming
- Moderate criteria balances rigor with achievability
- Detailed prompts reduce uncertainty and improve confidence

### 2. Model Archetypes are Stable

**Finding:** Model architecture determines behavior > prompt content

| Model | Archetype | Tool Pattern | Best Use Case |
|-------|-----------|-------------|---------------|
| Opus | Philosopher | 0 tools | Conceptual analysis, planning |
| Sonnet | Researcher | WebSearch + Read | Research-backed implementation |
| Haiku | Builder | Write + Bash | Rapid prototyping, iteration |

**Implication:** Choose model based on desired behavior, not just quality.

### 3. Research Improves Accuracy

**Finding:** Agents using WebSearch score 15-30% higher on correctness

**Evidence:**
- Sonnet (3 searches): 21/25 correctness
- Opus (0 searches): 12/25 correctness
- Haiku (0-1 searches): 19/25 correctness

**Why:** 2026 documentation reveals new APIs, deprecations, best practices

### 4. Simulation Hides Bugs

**Finding:** Simulated tests show 100% pass rate, but actual execution would fail 75%

**Critical gaps:**
- Syntactically valid but semantically broken
- Missing environment setup files
- Placeholder values not replaced
- No error handling

**Fix:** Evaluation must test "would this actually work?" not just "does it look right?"

### 5. Haiku Has a Clarity Threshold

**Finding:** Haiku switches from Builder to Researcher at ~30-35 word prompts

| Prompt Length | Haiku Behavior | Tools | Time |
|--------------|---------------|-------|------|
| P1 (10w) | Aggressive build | Write×6 | 5min |
| P2 (14w) | Build | Write×5 | 4min |
| P3 (35w) | Research/analyze | Read×7 | 2min |

**Hypothesis:** Longer prompts overwhelm smaller model, triggering defensive "plan first" mode

---

## Next Iteration Hypotheses

Based on Iteration 2 findings, test in Iteration 3:

1. **Test real execution** - Replace simulation with actual file creation + syntax validation
2. **Add validation gates** - yamllint, shellcheck, markdownlint in evaluation
3. **Explore Haiku threshold** - Test prompts at 15w, 20w, 25w, 30w to find exact flip point
4. **WebSearch ablation** - Test Sonnet with/without web access to quantify impact
5. **Multi-turn refinement** - Add Phase 1c where agent fixes issues from self-reflection
6. **Evaluator ensemble** - Use 3 evaluators (Opus+Sonnet+Haiku) and average scores

---

## References

- `/Users/bln/play/agentic-primer/SIMULATION_HARNESS.md` - Complete methodology
- `/Users/bln/play/agentic-primer/RUN_SIMULATION.md` - Execution instructions
- `/Users/bln/play/agentic-primer/SUCCESS_CRITERIA.md` - Observable outcomes
- `/Users/bln/play/agentic-primer/experiments/iteration-2/README.md` - Test matrix
- `/Users/bln/play/agentic-primer/scripts/analyze-simulation-agents.sh` - Metrics extraction
