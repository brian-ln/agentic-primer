# Experiments Directory

Organized by iteration. Each iteration contains everything needed to run that set of experiments.

## Current Iterations

### Iteration 2 (Current)

Bootstrap simulation testing across models and success criteria variations.

**Location:** `iteration-2/`

**Test Matrix:** 3 prompts × 3 criteria × 3 models = 27 simulations

**Prompts:**
- P1-minimal.txt (10 words)
- P2-moderate.txt (14 words)
- P3-detailed.txt (35 words)

**Success Criteria:**
- S1-minimal.txt (single requirement)
- S2-moderate.txt (3 requirements)
- S3-comprehensive.txt (7 observable outcomes)

**Models:** Opus, Sonnet, Haiku

**Quick Start:**
```bash
cd iteration-2
cat prompts/P3-detailed.txt
cat criteria/S2-moderate.txt
# See iteration-2/README.md for complete instructions
```

## Directory Structure

```
experiments/
├── iteration-2/           # Current iteration
│   ├── prompts/           # P1, P2, P3 (simplified names)
│   ├── criteria/          # S1, S2, S3
│   ├── runs/              # Execution results (created during runs)
│   └── README.md          # Iteration-specific instructions
└── README.md (this file)  # Overall experiments guide
```

## Running Experiments

Each iteration directory is self-contained:

1. **Navigate to iteration:** `cd iteration-2`
2. **Review prompts and criteria:** `cat prompts/*.txt criteria/*.txt`
3. **Follow iteration README:** See `iteration-2/README.md` for specific instructions
4. **Run methodology:** Use `../SIMULATION_HARNESS.md` two-phase approach
5. **Results storage:** Results saved to `runs/run-YYYYMMDD-HHMMSS/`

## Methodology

All experiments follow the two-phase methodology from `SIMULATION_HARNESS.md`:

**Phase 1: Simulation**
- Agent executes bootstrap prompt
- No self-evaluation
- Complete working output

**Phase 2: Evaluation**
- Separate evaluator scores output
- 100-point rubric (Completeness 30, Correctness 25, Actionability 20, Specificity 15, Insight 10)
- Grade (A-F) with commentary

## Analysis Tools

```bash
# Export agent logs and generate analysis
./scripts/export-agent-logs.sh <agent-ids>

# Analyze behavioral patterns
./scripts/analyze-simulation-agents.sh <agent-ids>
```

## References

- `SIMULATION_HARNESS.md` - Complete test framework and methodology
- `AFTER_CLEAR.md` - Copy/paste instructions for starting experiments
- `SUCCESS_CRITERIA.md` - Observable outcome definitions
- `archive/` - Previous simulation findings and learnings
