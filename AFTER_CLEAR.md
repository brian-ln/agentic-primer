# Instructions After /clear

**Copy/paste this after running `/clear` to start a simulation experiment.**

**NOTE:** Uses three-phase methodology from SIMULATION_HARNESS_V2.md:
- Phase 1: Simulation
- Phase 1b: Self-reflection (resume same agent)
- Phase 2: Independent evaluation (new Sonnet agent)

---

## Single Test (Recommended First)

```
I want to run a bootstrap simulation experiment.

Setup:
1. Create experiment directory: ./scripts/create-experiment-run.sh iteration-2 P3-S2-sonnet
2. Note the run name (e.g., run-20260106-001234-P3-S2-sonnet)

Configuration:
- Prompt: P3 (experiments/iteration-2/prompts/P3-detailed.txt)
- Success Criteria: S2 (experiments/iteration-2/criteria/S2-moderate.txt)
- Model: sonnet
- Research Policy: web search allowed

Execute THREE PHASES using SIMULATION_HARNESS_V2.md methodology:

PHASE 1 - SIMULATION:
Use the Task tool to launch a background agent with:
- subagent_type: "general-purpose"
- model: "sonnet"
- description: "P3+S2+sonnet simulation"
- prompt: [Use Phase 1 template from SIMULATION_HARNESS_V2.md with P3 and S2 content]

Wait for agent to complete. Note the agent ID (e.g., "agentId: ae913f4").

PHASE 1b - SELF-REFLECTION:
Use the Task tool with RESUME parameter:
- resume: [agent ID from Phase 1]
- description: "P3+S2+sonnet self-reflection"
- prompt: [Use Phase 1b template from SIMULATION_HARNESS_V2.md]

Same agent ID (it's a continuation). Wait for completion.

PHASE 2 - INDEPENDENT EVALUATION:
Use the Task tool to launch a NEW Sonnet agent:
- subagent_type: "general-purpose"
- model: "sonnet"
- description: "Evaluate P3+S2+sonnet"
- prompt: [Use Phase 2 template from SIMULATION_HARNESS_V2.md including both Phase 1 output AND Phase 1b reflection]

Apply the 100-point rubric. Note evaluator agent ID (e.g., "agentId: a0294d5").

After all three phases complete:
1. You'll have 2 agent IDs (simulation+reflection, evaluator)
2. Run: ./scripts/finalize-experiment-run.sh iteration-2/runs/<run-name> <sim-id> <eval-id>
3. Review: experiments/iteration-2/runs/<run-name>/logs/analysis-*.md
```

---

## Output File Organization

**IMPORTANT:** Each test configuration gets its own subdirectory in the run folder.

**Directory Structure:**
```
experiments/iteration-2/runs/run-20260106-001203-full-matrix/
├── P1-S1-opus/       # All files from P1+S1+opus simulation
├── P1-S1-sonnet/     # All files from P1+S1+sonnet simulation
├── P1-S1-haiku/      # All files from P1+S1+haiku simulation
├── P1-S2-opus/
... (27 total directories)
```

**What Goes in Each Directory:**
- Any markdown files created by simulation agents
- Agent output documents
- Solution designs
- File manifests
- Any other artifacts generated during simulation

**Why:** Keeps outputs organized and prevents 27 tests from dumping files in one location.

---

## How to Use the Task Tool for Simulations

**The Task tool launches background agents.** Each agent gets its own ID when it completes.

### Single Simulation Example

**Step 1 - Create experiment directory:**
```bash
./scripts/create-experiment-run.sh iteration-2 P3-S2-sonnet
# Creates: experiments/iteration-2/runs/run-20260106-001234-P3-S2-sonnet
```

**Step 2 - Launch Phase 1 (Simulation):**
Use Task tool with full prompt from SIMULATION_HARNESS.md, replacing `[INSERT: P3]` and `[INSERT: S2]` with actual content from:
- experiments/iteration-2/prompts/P3-detailed.txt
- experiments/iteration-2/criteria/S2-moderate.txt

**Agent returns:** `agentId: ae913f4`

**Step 3 - Launch Phase 2 (Evaluation):**
Use Task tool with evaluation prompt, including Phase 1's output.

**Agent returns:** `agentId: a0294d5`

**Step 4 - Finalize:**
```bash
./scripts/finalize-experiment-run.sh run-20260106-001234 ae913f4 a0294d5
```

### Parallel Execution Strategy

**Key insight:** Launch multiple Task calls in a SINGLE message for true parallelism.

**Example - Test 3 models in parallel:**
```
Launch 3 simulation agents in parallel using the Task tool:
1. P3+S2+opus simulation (returns agent ID 1)
2. P3+S2+sonnet simulation (returns agent ID 2)
3. P3+S2+haiku simulation (returns agent ID 3)

[All 3 agents run simultaneously]

After all complete, launch 3 evaluators in parallel:
1. Evaluate opus simulation (returns agent ID 4)
2. Evaluate sonnet simulation (returns agent ID 5)
3. Evaluate haiku simulation (returns agent ID 6)

Finalize with all 6 agent IDs.
```

**Why batches?** Each message can launch multiple Task calls in parallel, but phases must be sequential (can't evaluate until simulation completes).

---

## Full Test Matrix (All 27 Tests)

```
I want to run the full bootstrap simulation test matrix (27 tests).

Setup:
1. Create experiment directory: ./scripts/create-experiment-run.sh iteration-2 full-matrix
2. Note the run name (e.g., run-20260106-001234-full-matrix)

Test Matrix from SIMULATION_HARNESS_V2.md:
- Prompts: P1 (10w), P2 (14w), P3 (35w)
- Success Criteria: S1, S2, S3
- Models: opus, sonnet, haiku
- Total: 3 × 3 × 3 = 27 simulations

EXECUTION STRATEGY:
Run in 3 batches of 9 (grouped by prompt):

═══════════════════════════════════════════════════════
BATCH 1 - P1 × All Criteria × All Models
═══════════════════════════════════════════════════════

PHASE 1 - Launch 9 simulations in parallel:
In a SINGLE message, launch 9 Task calls:
1. P1+S1+opus, 2. P1+S1+sonnet, 3. P1+S1+haiku
4. P1+S2+opus, 5. P1+S2+sonnet, 6. P1+S2+haiku
7. P1+S3+opus, 8. P1+S3+sonnet, 9. P1+S3+haiku

Use Phase 1 template from SIMULATION_HARNESS_V2.md with P1 content.
Wait for all 9 to complete. Track agent IDs: [id1, id2, id3, id4, id5, id6, id7, id8, id9]

PHASE 1b - Resume 9 agents for self-reflection:
In a SINGLE message, resume all 9 agents:
1. Resume id1, 2. Resume id2, 3. Resume id3
4. Resume id4, 5. Resume id5, 6. Resume id6
7. Resume id7, 8. Resume id8, 9. Resume id9

Use Phase 1b template from SIMULATION_HARNESS_V2.md.
Wait for all 9 to complete. (Same agent IDs, just continued)

PHASE 2 - Launch 9 evaluators in parallel:
In a SINGLE message, launch 9 NEW Sonnet evaluators:
Each evaluates one P1 simulation (include both Phase 1 output AND Phase 1b reflection).

Wait for all 9 to complete. Track evaluator IDs: [eval1, eval2, eval3, eval4, eval5, eval6, eval7, eval8, eval9]

BATCH 1 TOTAL: 18 agent IDs (9 simulators + 9 evaluators)

═══════════════════════════════════════════════════════
BATCH 2 - P2 × All Criteria × All Models
═══════════════════════════════════════════════════════

Repeat same three-phase process with P2 content:
- Phase 1: 9 parallel simulations
- Phase 1b: Resume 9 for reflection
- Phase 2: 9 parallel evaluators

BATCH 2 TOTAL: 18 agent IDs

═══════════════════════════════════════════════════════
BATCH 3 - P3 × All Criteria × All Models
═══════════════════════════════════════════════════════

Repeat same three-phase process with P3 content:
- Phase 1: 9 parallel simulations
- Phase 1b: Resume 9 for reflection
- Phase 2: 9 parallel evaluators

BATCH 3 TOTAL: 18 agent IDs

GRAND TOTAL: 54 agent IDs (27 simulators + 27 evaluators)

After all batches complete:
1. Run: ./scripts/finalize-experiment-run.sh iteration-2/runs/<run-name> <all-54-agent-ids>
2. Launch Phase 3 aggregate analysis agent (include all 27 evaluation results)
3. Review comprehensive comparison and identify optimal configuration
```

### Full Matrix Execution Details

**Batch Size:** 9 simulations per batch (same prompt, all criteria/model combos)
**Why 9?** Balances parallelism with manageability. Can track 9 agent IDs easily.
**Total Time:** ~15-20 minutes (vs ~60-90 minutes if sequential)

**Phase Structure:**
1. Launch 9 simulations in parallel (one message with 9 Task calls)
2. Wait for all to complete
3. Launch 9 evaluations in parallel (one message with 9 Task calls)
4. Wait for all to complete
5. Move to next batch

**Tracking Agent IDs:**
Keep a running list as each batch completes:
- Batch 1 Phase 1: IDs 1-9
- Batch 1 Phase 2: IDs 10-18
- Batch 2 Phase 1: IDs 19-27
- Batch 2 Phase 2: IDs 28-36
- Batch 3 Phase 1: IDs 37-45
- Batch 3 Phase 2: IDs 46-54

---

## What You Have Access To

All files are in `/Users/bln/play/agentic-primer/`:

**Core Documents:**
- `SIMULATION_HARNESS.md` - Complete test framework with prompts, criteria, methodology
- `SUCCESS_CRITERIA.md` - Observable outcome definitions
- `BOOTSTRAP.md` - The 30-word production prompt
- `COMPACT_LOG_SCHEMA.md` - Minimal log format (90-95% compression)

**Scripts:**
- `scripts/create-experiment-run.sh` - Create experiment directory
- `scripts/finalize-experiment-run.sh` - Export logs and finalize
- `scripts/analyze-simulation-agents.sh` - Extract metrics
- `scripts/compact-agent-log.py` - Compress logs to minimal schema
- `scripts/export-agent-logs.sh` - Export and compact logs

**Reference:**
- `archive/` - Previous 9-agent simulation findings
- `AGENT_ANALYSIS_INDEX.md` - Summary of what we learned

---

## Expected Agent Behavior by Model

From archive analysis (9 previous simulations):

**Opus:**
- 0 tool calls, pure reasoning
- ~10 seconds
- Analysis of what WOULD be created
- Score: 30-60% (thoughtful but no validation)

**Sonnet:**
- 8-11 tool calls (WebSearch, Read, Bash)
- ~2-3 minutes
- Research-backed recommendations
- Score: 70-85% (validated, conservative)

**Haiku:**
- **Short prompts (P1/P2):** 21+ tool calls, creates 6 files, ~4-5 min, 70%+
- **Long prompts (P3):** 10 tool calls, analysis mode (behavioral flip!), ~2 min, 60-70%

---

## Success Indicators

✅ **Run succeeded if:**
- Both phases complete (simulation + evaluation)
- Agent logs exported successfully
- Analysis shows token/tool metrics
- Evaluation uses 100-point rubric with justification
- No self-assessment during simulation phase

❌ **Redo if:**
- Agent self-evaluated during simulation
- Missing token or tool metrics
- Scores given without rubric application
- Error or timeout

---

## After First Run

Tell me:
1. Agent ID(s) used
2. Did rubric scores make sense?
3. Should we adjust anything for next run?
4. Ready for full 27-test matrix?

Then I'll finalize the experiment and we can review findings together.
