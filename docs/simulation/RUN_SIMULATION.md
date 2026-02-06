# Run Simulation Instructions

**Give me these instructions after `/clear` to run a bootstrap simulation experiment.**

---

## Quick Start (Single Test)

```
I want to run a bootstrap simulation test.

Configuration:
- Prompt: P3 (experiments/iteration-2/prompts/P3-detailed.txt)
- Success Criteria: S2 (experiments/iteration-2/criteria/S2-moderate.txt)
- Model: sonnet
- Research Policy: web search allowed

EXECUTION:
1. Create experiment run: ./scripts/create-experiment-run.sh iteration-2 P3-S2-sonnet
   Note the run name (e.g., run-20260106-001234-P3-S2-sonnet)
   Creates: experiments/iteration-2/runs/run-20260106-001234-P3-S2-sonnet/

2. Execute Phase 1 (simulation):
   Launch 1 background agent using the Task tool:
   - subagent_type: "general-purpose"
   - model: "sonnet"
   - description: "Bootstrap simulation P3+S2"
   - prompt: [Full simulation template from SIMULATION_HARNESS.md Phase 1
             with P3 and S2 content substituted]

   Wait for completion. Note agent ID (e.g., ae913f4).

3. Execute Phase 2 (evaluation):
   Launch 1 background agent using the Task tool:
   - subagent_type: "general-purpose"
   - model: "sonnet"
   - description: "Evaluate P3+S2 simulation"
   - prompt: [Full evaluation template from SIMULATION_HARNESS.md Phase 2
             with Phase 1 output included]

   Apply 100-point rubric. Note agent ID (e.g., a0294d5).

4. Finalize:
   ./scripts/finalize-experiment-run.sh iteration-2/runs/run-20260106-001234-P3-S2-sonnet ae913f4 a0294d5

5. Review:
   experiments/iteration-2/runs/run-20260106-001234-P3-S2-sonnet/logs/analysis-*.md
```

---

## Full Test Matrix (27 Permutations)

```
I want to run the full simulation test matrix from SIMULATION_HARNESS.md.

Test all combinations:
- Prompts: P1 (10w), P2 (14w), P3 (35w)
- Success Criteria: S1 (minimal), S2 (moderate), S3 (comprehensive)
- Models: opus, sonnet, haiku
- Research Policy: web search allowed

That's 3 × 3 × 3 = 27 simulations.

EXECUTION STRATEGY (3 batches of 9):

BATCH 1 - P1 × All Criteria × All Models:
Launch 9 Task calls in parallel for simulations:
  P1+S1+opus, P1+S1+sonnet, P1+S1+haiku,
  P1+S2+opus, P1+S2+sonnet, P1+S2+haiku,
  P1+S3+opus, P1+S3+sonnet, P1+S3+haiku

Wait for all 9 to complete. Track agent IDs 1-9.

Launch 9 Task calls in parallel for evaluations.
Track agent IDs 10-18.

BATCH 2 - P2 × All Criteria × All Models:
Repeat with P2 combinations (9 sims + 9 evals).
Track agent IDs 19-36.

BATCH 3 - P3 × All Criteria × All Models:
Repeat with P3 combinations (9 sims + 9 evals).
Track agent IDs 37-54.

After all batches complete:
1. Finalize: ./scripts/finalize-experiment-run.sh run-{YYYYMMDD} <all-54-agent-ids>
2. Generate comparative analysis across all 27 runs
3. Identify optimal prompt/criteria/model combinations

Total time: ~15-20 minutes (vs ~60-90 minutes sequential)
```

### How to Launch Task Calls in Parallel

**Critical:** All Task calls must be in a SINGLE message to run in parallel.

**Example message for Batch 1 Phase 1:**
```
Launch 9 simulation agents in parallel using Task tool:

1. P1+S1+opus: subagent_type "general-purpose", model "opus", description "P1+S1+opus sim"
2. P1+S1+sonnet: subagent_type "general-purpose", model "sonnet", description "P1+S1+sonnet sim"
3. P1+S1+haiku: subagent_type "general-purpose", model "haiku", description "P1+S1+haiku sim"
[...6 more...]

Each gets the simulation prompt template with appropriate P1/S1/S2/S3 substituted.
```

All 9 agents launch simultaneously and run in parallel.

### What Happens When Agents Complete

**Each Task agent returns:**
- `agentId: a1b2c3d` - The unique agent ID for this simulation
- Output visible in the conversation

**You will see notifications like:**
```
Agent "Bootstrap simulation P3+S2" completed.
agentId: ae913f4
```

**Track these IDs** - you'll need all of them for the finalize script.

**Phase dependency:** Phase 2 (evaluation) agents MUST wait for Phase 1 (simulation) agents to complete, because evaluators need the simulation output.

---

## What I Need to Know

**From SIMULATION_HARNESS.md, I have:**

### Prompts (P1, P2, P3)
```
P1 (10w): "Bootstrap @copilot issue automation with auto-review and knowledge base."

P2 (14w): "Setup issue-driven development with @copilot. Auto-assign PRs to owner. Include knowledge base."

P3 (35w): "Create issue-driven development system:
- Issue template (.github/ISSUE_TEMPLATE/task.yml) for @copilot tasks
- CODEOWNERS (* @owner) for PR auto-assignment
- Knowledge base (docs/knowledge/) with patterns/decisions/insights structure
- README with workflow: issue → @copilot → PR → review via web UI"
```

### Success Criteria (S1, S2, S3)
```
S1 (Minimal): "System must process a test issue without errors."

S2 (Moderate): "Generated system must:
- Process test issue end-to-end without errors
- Pass syntax validation (yamllint, shellcheck)
- GitHub workflow triggers on issue creation"

S3 (Comprehensive): "Definition of Done (Observable Outcomes):
1. Functional Test: System processes test issue end-to-end without errors
2. Syntax Valid: All generated files pass automated validation
3. Observable Behavior: GitHub workflow actually triggers on issue creation
4. Reliability: 90%+ success rate across 20+ test runs
5. Multi-Agent: Works with ≥3 different AI agents
6. Single-Command: Bootstrap completes from bare repo with zero manual intervention
7. Self-Improvement: System creates ≥3 successful improvement PRs from logs"
```

### Two-Phase Methodology

**Phase 1: Simulation**
Use the prompt from SIMULATION_HARNESS.md Phase 1 template:
- Simulate @copilot behavior
- List files that would be created
- Provide complete functional content (no placeholders)
- DO NOT self-evaluate

**Phase 2: Evaluation**
Use the 100-point rubric from SIMULATION_HARNESS.md:
- Completeness (30 points)
- Correctness (25 points)
- Actionability (20 points)
- Specificity (15 points)
- Insight Quality (10 points)

---

## Expected Output Structure

After simulation(s) complete, create:

```
experiments/run-{YYYYMMDD-HHMMSS}/
├── config.json                     # Test configuration
├── logs/                           # Agent logs
│   ├── agent-{ID}.compact.jsonl   # Minimal schema
│   └── analysis-{timestamp}.md    # Metrics report
├── results/                        # Simulation outputs
│   ├── {model}-{prompt}-{criteria}.md    # Phase 1 output
│   └── eval-{model}-{prompt}-{criteria}.json  # Phase 2 scores
├── comparison.md                   # Cross-run analysis
└── README.md                       # Run summary
```

---

## Post-Simulation Analysis

After runs complete, I will:

1. Export logs: `./scripts/export-agent-logs.sh <agent-ids>`
2. Generate analysis: Comprehensive metrics across all runs
3. Create comparison: Identify optimal prompt/criteria combinations
4. Document findings: Update with key learnings

---

## Example Variations

**Test prompt length impact (fix criteria and model):**
```
Run 3 simulations:
- P1 + S2 + sonnet
- P2 + S2 + sonnet
- P3 + S2 + sonnet

Compare how prompt length affects completeness scores.
```

**Test criteria impact (fix prompt and model):**
```
Run 3 simulations:
- P3 + S1 + sonnet
- P3 + S2 + sonnet
- P3 + S3 + sonnet

Compare how success criteria specificity affects agent behavior.
```

**Test model behavior (fix prompt and criteria):**
```
Run 3 simulations:
- P2 + S2 + opus
- P2 + S2 + sonnet
- P2 + S2 + haiku

Compare the three archetypes (Philosopher, Researcher, Builder).
```

---

## Time Estimates

**Single simulation:**
- Opus: ~10 seconds (no tools)
- Sonnet: ~2-3 minutes (research + exploration)
- Haiku: ~2-5 minutes (depends on prompt length)

**Full 27-test matrix:**
- Sequential: ~60-90 minutes
- Parallel batches (9 at a time): ~15-20 minutes

---

## Success Indicators

✅ **Good run:**
- All phases complete without errors
- Logs exported successfully
- Analysis shows clear behavioral patterns
- Rubric scores are consistent and justified

❌ **Problem run:**
- Agent errors or timeouts
- Self-evaluation instead of two-phase methodology
- Missing token/tool metrics
- Scores without justification

---

## After First Run

Review the experiment output and tell me:
1. Do the rubric scores make sense?
2. Do we need to adjust prompt/criteria for next run?
3. Should we test additional permutations?
4. Any unexpected behavioral patterns to investigate?

This iterative approach lets us refine the experiment design based on real data.
