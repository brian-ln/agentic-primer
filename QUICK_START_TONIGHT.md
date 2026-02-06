# Quick Start - Run Before Bed

**Copy/paste this entire block to Claude after `/clear`:**

---

I want to run a single bootstrap simulation test using the three-phase methodology.

Configuration:
- Prompt: P3 (experiments/iteration-2/prompts/P3-detailed.txt)
- Success Criteria: S2 (experiments/iteration-2/criteria/S2-moderate.txt)
- Model: sonnet

Setup:
```bash
./scripts/create-experiment-run.sh iteration-2 P3-S2-sonnet
```

PHASE 1 - SIMULATION:
Launch 1 Task agent with:
- subagent_type: "general-purpose"
- model: "sonnet"
- description: "P3+S2+sonnet simulation"
- prompt: Use Phase 1 template from SIMULATION_HARNESS_V2.md with P3 and S2 content substituted

Save agent ID (e.g., ae913f4).

PHASE 1b - SELF-REFLECTION:
Resume the same agent using Task tool:
- resume: [agent ID from Phase 1]
- description: "P3+S2+sonnet self-reflection"
- prompt: Use Phase 1b template from SIMULATION_HARNESS_V2.md

Agent ID stays the same.

PHASE 2 - INDEPENDENT EVALUATION:
Launch NEW Sonnet evaluator using Task tool:
- subagent_type: "general-purpose"
- model: "sonnet"
- description: "Evaluate P3+S2+sonnet"
- prompt: Use Phase 2 template from SIMULATION_HARNESS_V2.md with Phase 1 output and Phase 1b reflection included

Save evaluator agent ID (e.g., a0294d5).

FINALIZE:
```bash
./scripts/finalize-experiment-run.sh iteration-2/runs/run-[TIMESTAMP]-P3-S2-sonnet [sim-id] [eval-id]
```

REVIEW:
```bash
cat experiments/iteration-2/runs/run-[TIMESTAMP]-P3-S2-sonnet/logs/analysis-*.md
```

---

## Expected Timeline

- Phase 1: ~2-3 minutes (simulation with research)
- Phase 1b: ~30 seconds (reflection, quick)
- Phase 2: ~2 minutes (evaluation)
- **Total: ~5 minutes**

## Success Indicators

✅ Phase 1 completes with simulation output
✅ Phase 1b adds self-reflection (same agent ID)
✅ Phase 2 provides 100-point rubric score
✅ finalize script exports logs successfully
✅ Analysis shows 3 agents (sim, sim-continued, eval)

---

## If You Want to Run 3 Tests in Parallel Tonight

**Test: P3+S2 across all 3 models**

In a SINGLE message after `/clear`, launch 3 simulations in parallel:

```
Launch 3 simulation agents in parallel:
1. P3+S2+opus (Task with opus model)
2. P3+S2+sonnet (Task with sonnet model)
3. P3+S2+haiku (Task with haiku model)
```

Wait for all 3 to complete. Track IDs: [id1, id2, id3]

Then resume all 3 for reflection:

```
Resume 3 agents for self-reflection:
1. Resume id1 (opus reflection)
2. Resume id2 (sonnet reflection)
3. Resume id3 (haiku reflection)
```

Then launch 3 evaluators in parallel:

```
Launch 3 Sonnet evaluators in parallel:
1. Evaluate id1 output
2. Evaluate id2 output
3. Evaluate id3 output
```

Track eval IDs: [eval1, eval2, eval3]

Finalize with all 6 IDs:
```bash
./scripts/finalize-experiment-run.sh iteration-2/runs/run-[TIMESTAMP]-P3-S2-all-models id1 id2 id3 eval1 eval2 eval3
```

**Total time: ~5-7 minutes** (all running in parallel)

---

## Files You Need

- `SIMULATION_HARNESS_V2.md` - Has all three phase prompt templates
- `experiments/iteration-2/prompts/P3-detailed.txt` - The prompt
- `experiments/iteration-2/criteria/S2-moderate.txt` - The criteria
- `scripts/create-experiment-run.sh` - Creates run directory
- `scripts/finalize-experiment-run.sh` - Exports logs
