#!/usr/bin/env bash
# create-experiment-run.sh - Create directory structure for experiment run
#
# Usage: ./create-experiment-run.sh [iteration] [config-suffix]
# Examples:
#   ./create-experiment-run.sh                    # iteration-2/runs/run-20260106-001234
#   ./create-experiment-run.sh iteration-2        # iteration-2/runs/run-20260106-001234
#   ./create-experiment-run.sh iteration-2 P3-S2-sonnet  # iteration-2/runs/run-20260106-001234-P3-S2-sonnet
#
# Creates: experiments/{iteration}/runs/{run-name}/ with proper structure

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXPERIMENTS_DIR="$PROJECT_ROOT/experiments"

# Default to iteration-2
ITERATION="${1:-iteration-2}"
CONFIG_SUFFIX="${2:-}"

# Generate timestamp
TIMESTAMP="$(date +%Y%m%d-%H%M%S)"

# Build run name with optional config suffix
if [ -n "$CONFIG_SUFFIX" ]; then
    RUN_NAME="run-${TIMESTAMP}-${CONFIG_SUFFIX}"
else
    RUN_NAME="run-${TIMESTAMP}"
fi

RUN_DIR="$EXPERIMENTS_DIR/$ITERATION/runs/$RUN_NAME"

# Check if already exists
if [ -d "$RUN_DIR" ]; then
    echo "Error: Run directory already exists: $RUN_DIR"
    exit 1
fi

# Create structure
mkdir -p "$RUN_DIR"/{logs,results}

# Create README template
cat > "$RUN_DIR/README.md" <<'EOF'
# Experiment Run: {RUN_NAME}

**Created:** {DATE}

## Configuration

- **Prompts Tested:** P1 (10w), P2 (14w), P3 (35w)
- **Success Criteria:** S1 (minimal), S2 (moderate), S3 (comprehensive)
- **Models:** opus, sonnet, haiku
- **Research Policy:** web search allowed

## Test Matrix

Total simulations: ___ (e.g., 3×3×3 = 27)

## Results Summary

[Fill in after runs complete]

### Key Findings

1.
2.
3.

### Optimal Configuration

- Best prompt length: ___
- Best criteria level: ___
- Best model: ___
- Score: ___/100

## Files

- `config.json` - Test configuration
- `logs/` - Compact agent logs and analysis
- `results/` - Simulation outputs and evaluations
- `comparison.md` - Cross-run analysis

## Next Steps

[What to test next based on these results]
EOF

sed -i '' "s/{RUN_NAME}/$RUN_NAME/g" "$RUN_DIR/README.md"
sed -i '' "s/{DATE}/$(date)/g" "$RUN_DIR/README.md"

# Create config template
cat > "$RUN_DIR/config.json" <<'EOF'
{
  "run_name": "{RUN_NAME}",
  "created": "{DATE}",
  "prompts": ["P1", "P2", "P3"],
  "criteria": ["S1", "S2", "S3"],
  "models": ["opus", "sonnet", "haiku"],
  "research_policy": "web",
  "total_simulations": 27,
  "notes": ""
}
EOF

sed -i '' "s/{RUN_NAME}/$RUN_NAME/g" "$RUN_DIR/config.json"
sed -i '' "s/{DATE}/$(date -Iseconds)/g" "$RUN_DIR/config.json"

echo "Created experiment run: $RUN_DIR"
echo ""
echo "Structure:"
echo "  $RUN_NAME/"
echo "  ├── config.json"
echo "  ├── README.md"
echo "  ├── logs/"
echo "  └── results/"
echo ""
echo "Next steps:"
echo "  1. Run simulations (see RUN_SIMULATION.md)"
echo "  2. Export logs: ./scripts/export-agent-logs.sh <agent-ids>"
echo "  3. Move logs to: $RUN_DIR/logs/"
echo "  4. Move results to: $RUN_DIR/results/"
echo "  5. Generate comparison.md in run directory"
echo ""
echo "Or use the helper:"
echo "  ./scripts/finalize-experiment-run.sh $RUN_NAME <agent-ids>"
