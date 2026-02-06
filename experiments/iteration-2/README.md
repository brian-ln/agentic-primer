# Iteration 2: Bootstrap Simulation Experiments

This iteration tests bootstrap prompt effectiveness across models and success criteria variations.

## Test Matrix

3 prompts × 3 criteria × 3 models = **27 simulations**

### Prompts (in `prompts/`)

- **P1-minimal.txt** - 10 words, minimal specification
- **P2-moderate.txt** - 14 words, moderate detail
- **P3-detailed.txt** - 35 words, comprehensive specification

### Success Criteria (in `criteria/`)

- **S1-minimal.txt** - Single requirement (functional test only)
- **S2-moderate.txt** - 3 requirements (functional + syntax + triggers)
- **S3-comprehensive.txt** - 7 observable outcomes (full production readiness)

### Models

- **Opus** (claude-opus-4-5)
- **Sonnet** (claude-sonnet-4-5)
- **Haiku** (claude-haiku-4)

## Running an Iteration

### Quick Start (Single Test)

```bash
# 1. View the prompt and criteria you want to test
cat prompts/P3-detailed.txt
cat criteria/S2-moderate.txt

# 2. Run simulation using SIMULATION_HARNESS.md methodology
# See ../../SIMULATION_HARNESS.md for complete instructions

# 3. Results will be saved to runs/run-YYYYMMDD-HHMMSS/
```

### Full Matrix (27 Tests)

```bash
# For each combination of prompt (P1/P2/P3) × criteria (S1/S2/S3) × model (opus/sonnet/haiku):
# 1. Execute Phase 1: Simulation
# 2. Execute Phase 2: Evaluation
# 3. Save results to runs/ directory
# 4. Export agent logs for analysis
```

## Directory Structure

```
iteration-2/
├── prompts/
│   ├── P1-minimal.txt       (10 words)
│   ├── P2-moderate.txt      (14 words)
│   └── P3-detailed.txt      (35 words)
├── criteria/
│   ├── S1-minimal.txt       (single requirement)
│   ├── S2-moderate.txt      (3 requirements)
│   └── S3-comprehensive.txt (7 observable outcomes)
├── runs/                    (created during execution)
│   └── run-YYYYMMDD-HHMMSS/
│       ├── config.json
│       ├── logs/
│       ├── results/
│       └── README.md
└── README.md (this file)
```

## Expected Results

Based on initial 9-agent testing:

### Opus + Any Configuration
- Process: Pure analysis, 0 tool calls
- Output: Analysis of what WOULD be created
- Score: 30-60%
- Time: ~10 seconds

### Sonnet + Any Configuration
- Process: WebSearch → Read → Document
- Output: Research-backed recommendations
- Score: 70-85%
- Time: ~2-3 minutes

### Haiku + Short Prompt (P1/P2)
- Process: Build aggressively (6+ files)
- Output: Actual files created
- Score: 70%+
- Time: ~4-5 minutes

### Haiku + Detailed Prompt (P3)
- Process: **Behavior flip!** Analysis mode
- Output: Research without file creation
- Score: 60-70%
- Time: ~2 minutes

## Key Findings to Validate

1. **Model Architecture > Prompt Length** - Does Opus always analyze? Does Sonnet always research?
2. **Haiku Clarity Threshold** - Does 30-35 words flip Haiku from build to analyze mode?
3. **Success Criteria Impact** - Does S3 create more consistency than S1?
4. **Optimal Configuration** - Is P3 + S2 + Sonnet still the best combination?

## Methodology

See `../../SIMULATION_HARNESS.md` for complete two-phase methodology:
- Phase 1: Simulation (agent executes bootstrap)
- Phase 2: Evaluation (separate evaluator scores with 100-point rubric)

## Analysis

After running experiments:

```bash
# Export and analyze agent logs
../../scripts/export-agent-logs.sh <agent-ids>

# Results will show:
# - Tool usage patterns
# - Behavioral signatures
# - Score distributions
# - Optimal configurations
```

## References

- `../../SIMULATION_HARNESS.md` - Complete test framework
- `../../AFTER_CLEAR.md` - Copy/paste instructions
- `../../SUCCESS_CRITERIA.md` - Observable outcome definitions
- `../../archive/` - Previous simulation findings
