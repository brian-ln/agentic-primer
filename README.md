# Agentic Primer

**Turn any git repository into an AI-executable workspace that bootstraps itself and remembers everything.**

## Quick Start

### To Execute the Bootstrap:
1. Read `BOOTSTRAP.md` - The 30-word prompt that creates the system
2. Check `SUCCESS_CRITERIA.md` - Observable outcomes that define success
3. Execute the bootstrap with your preferred AI agent (@copilot, Claude, etc.)
4. Verify the system processes a test issue end-to-end

### To Run Simulation Experiments:
1. Read `RUN_SIMULATION.md` - Instructions for testing bootstrap variations
2. Create experiment run: `./scripts/create-experiment-run.sh`
3. Run simulations (see RUN_SIMULATION.md for prompts)
4. Finalize: `./scripts/finalize-experiment-run.sh <run-name> <agent-ids>`
5. Review findings in `experiments/<run-name>/`

## Files

**Core:**
- **BOOTSTRAP.md** - Production bootstrap prompt (30 words)
- **SUCCESS_CRITERIA.md** - Observable outcome definitions
- **AFTER_CLEAR.md** - Instructions to give me after `/clear` ⭐

**Testing:**
- **SIMULATION_HARNESS.md** - Test framework (3×3×3 = 27 permutations)
- **RUN_SIMULATION.md** - Detailed run instructions
- **COMPACT_LOG_SCHEMA.md** - Minimal log format (90-95% compression)

**Activity Management:**
- **ACTIVITY_WORKTREE_SYSTEM.md** - Worktree-based parallel activities design
- **ACTIVITY_QUICK_REFERENCE.md** - Quick command reference for users and Claude
- **CLAUDE_ACTIVITY_INTEGRATION.md** - How Claude Code uses activities reliably

## Structure

```
├── BOOTSTRAP.md              # Production prompt (30 words)
├── SUCCESS_CRITERIA.md       # Definition of done (observable outcomes)
├── SIMULATION_HARNESS.md     # Test framework (3×3×3 = 27 permutations)
├── COMPACT_LOG_SCHEMA.md     # Minimal agent log format (90-95% compression)
├── AFTER_CLEAR.md            # Instructions to give me after /clear ⭐
├── experiments/              # Experiment templates and runs
│   ├── prompts/             # P1, P2, P3 (10w, 14w, 35w)
│   ├── criteria/            # S1, S2, S3 (minimal, moderate, comprehensive)
│   ├── README.md            # Template usage guide
│   └── run-YYYYMMDD-*/      # Individual experiment runs
├── archive/                  # Analysis & exploration (48 files)
├── docs/                     # Generated knowledge base
└── scripts/                  # Utilities
    ├── activity                       # Activity management CLI
    ├── activity-lib.sh                # Activity library functions
    ├── activity-context               # Activity context helper
    ├── create-experiment-run.sh       # Create experiment directory
    ├── finalize-experiment-run.sh     # Export logs and finalize
    ├── analyze-simulation-agents.sh   # Extract metrics from logs
    ├── compact-agent-log.py           # Convert to minimal schema
    ├── export-agent-logs.sh           # Export & compact logs
    └── verify-bootstrap.sh            # Validate generated system
```

## Key Learnings

From 9 initial simulations (3 models × 3 prompt lengths):

### Model Archetypes
- **Opus "The Philosopher"**: Pure analysis, 0 tools, ~10 sec
- **Sonnet "The Researcher"**: Web research + validation, ~2-3 min
- **Haiku "The Builder"**: Creates files for short prompts, analyzes long ones

### Optimal Configuration
- **Prompt**: 30-35 words (sweet spot for 65-75% completeness)
- **Success Criteria**: Moderate detail (clear expectations without overwhelming)
- **Model**: Sonnet (balanced research + consistency)

### Critical Findings
- **Haiku flips behavior at ~35 words** (build mode → analysis mode)
- **Define success in first 10 minutes**, not after 2.5 hours
- **Outcome-based criteria** beat implementation requirements
- **Separate evaluation** from simulation (self-assessment unreliable)

## Usage

**To execute the bootstrap:**
```bash
# Use the 30-word prompt from BOOTSTRAP.md
# Verify with SUCCESS_CRITERIA.md
```

**To test variations:**
```bash
# Use SIMULATION_HARNESS.md framework
# Test different prompts, criteria, models
# Compare results with 100-point rubric
```

**To analyze agent behavior:**
```bash
# After running simulations, analyze what agents actually did
./scripts/analyze-simulation-agents.sh <agent-id-1> <agent-id-2> ...

# Example: Compare 3 models on same prompt
./scripts/analyze-simulation-agents.sh ad7d53c a7c3dfb a525bb6
```

**To manage parallel activities:**
```bash
# Initialize activity system (first time only)
./scripts/activity init

# Create new activity worktree
./scripts/activity create exploring-idea-x "Exploring new idea"

# List all activities
./scripts/activity list

# Switch between activities
./scripts/activity switch main
./scripts/activity switch exploring-idea-x

# Get current activity path (for scripts)
cd "$(./scripts/activity path)"

# See ACTIVITY_QUICK_REFERENCE.md for full usage
```

See `archive/` for detailed analysis and retrospective.
