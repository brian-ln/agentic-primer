#!/usr/bin/env bash
# finalize-experiment-run.sh - Finalize experiment run with logs and analysis
#
# Usage: ./finalize-experiment-run.sh <run-name> <agent-id-1> [agent-id-2] ...
# Example: ./finalize-experiment-run.sh run-20260106-test ad7d53c a7c3dfb a525bb6

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXPERIMENTS_DIR="$PROJECT_ROOT/experiments"

if [ $# -lt 2 ]; then
    echo "Usage: $0 <run-name> <agent-id-1> [agent-id-2] ..."
    echo ""
    echo "Example:"
    echo "  $0 run-20260106-test ad7d53c a7c3dfb a525bb6"
    exit 1
fi

RUN_NAME="$1"
shift
AGENT_IDS=("$@")

RUN_DIR="$EXPERIMENTS_DIR/$RUN_NAME"

if [ ! -d "$RUN_DIR" ]; then
    echo "Error: Run directory not found: $RUN_DIR"
    echo ""
    echo "Create it first:"
    echo "  ./scripts/create-experiment-run.sh $RUN_NAME"
    exit 1
fi

echo "Finalizing experiment: $RUN_NAME"
echo "Agent IDs: ${AGENT_IDS[*]}"
echo ""

# Export logs
echo "Exporting agent logs..."
"$PROJECT_ROOT/scripts/export-agent-logs.sh" "${AGENT_IDS[@]}"

# Move logs to experiment directory
echo ""
echo "Moving logs to experiment directory..."
if [ -d "$PROJECT_ROOT/logs" ]; then
    mv "$PROJECT_ROOT/logs"/* "$RUN_DIR/logs/" 2>/dev/null || true
    rmdir "$PROJECT_ROOT/logs" 2>/dev/null || true
    echo "✓ Logs moved to $RUN_DIR/logs/"
fi

# Update README with agent count
AGENT_COUNT=${#AGENT_IDS[@]}
sed -i '' "s/Total simulations: ___/Total simulations: $AGENT_COUNT/" "$RUN_DIR/README.md"

# Update config with actual agent IDs
cat > "$RUN_DIR/agent_ids.txt" <<EOF
# Agent IDs for this run
# Generated: $(date)
${AGENT_IDS[@]}
EOF

echo ""
echo "✓ Experiment finalized: $RUN_DIR"
echo ""
echo "Contents:"
ls -lh "$RUN_DIR"
echo ""
echo "Logs:"
ls -lh "$RUN_DIR/logs" 2>/dev/null || echo "  (no logs found)"
echo ""
echo "Next steps:"
echo "  1. Review: cat $RUN_DIR/logs/analysis-*.md"
echo "  2. Add findings to: $RUN_DIR/README.md"
echo "  3. Generate comparison across runs if needed"
