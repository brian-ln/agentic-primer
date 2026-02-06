#!/usr/bin/env bash
# export-agent-logs.sh - Export agent logs and analysis to project directory
#
# Usage: ./export-agent-logs.sh <agent-id-1> <agent-id-2> ... [<agent-id-n>]
# Example: ./export-agent-logs.sh ad7d53c a7c3dfb a525bb6
#
# Creates:
#   logs/agent-{ID}.jsonl - Raw agent execution logs
#   logs/analysis-{timestamp}.md - Behavioral analysis report

set -euo pipefail

# Configuration
CLAUDE_PROJECTS_DIR="$HOME/.claude/projects"
PROJECT_ID="-Users-bln-play-agentic-primer"
AGENT_DIR="$CLAUDE_PROJECTS_DIR/$PROJECT_ID"
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOGS_DIR="$PROJECT_ROOT/logs"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

usage() {
    echo "Usage: $0 <agent-id-1> <agent-id-2> ... [<agent-id-n>]"
    echo ""
    echo "Exports agent logs and generates analysis report."
    echo ""
    echo "Example:"
    echo "  $0 ad7d53c a7c3dfb a525bb6"
    echo ""
    echo "Creates:"
    echo "  logs/agent-{ID}.jsonl - Raw agent logs"
    echo "  logs/analysis-{timestamp}.md - Analysis report"
    exit 1
}

if [ $# -eq 0 ]; then
    usage
fi

AGENT_IDS=("$@")
TIMESTAMP=$(date +%Y%m%d-%H%M%S)

# Create logs directory
mkdir -p "$LOGS_DIR"

echo -e "${BLUE}=== Exporting Agent Logs ===${NC}"
echo ""
echo "Destination: $LOGS_DIR"
echo ""

# Copy and compact agent logs
for agent_id in "${AGENT_IDS[@]}"; do
    src="$AGENT_DIR/agent-${agent_id}.jsonl"
    dst_raw="$LOGS_DIR/agent-${agent_id}.jsonl"
    dst_compact="$LOGS_DIR/agent-${agent_id}.compact.jsonl"

    if [[ -f "$src" ]]; then
        # Copy raw log
        cp "$src" "$dst_raw"
        size_raw=$(du -h "$dst_raw" | cut -f1)

        # Compact log
        "$PROJECT_ROOT/scripts/compact-agent-log.py" "$dst_raw" "$dst_compact" > /dev/null 2>&1
        if [[ -f "$dst_compact" ]]; then
            size_compact=$(du -h "$dst_compact" | cut -f1)
            echo -e "${GREEN}✓${NC} agent-${agent_id}.jsonl: ${size_raw} (raw) → ${size_compact} (compact)"
        else
            echo -e "${GREEN}✓${NC} agent-${agent_id}.jsonl: ${size_raw} (raw only)"
        fi
    else
        echo -e "${YELLOW}✗${NC} Not found: agent-${agent_id}.jsonl"
    fi
done

echo ""
echo -e "${BLUE}=== Generating Analysis Report ===${NC}"
echo ""

# Generate analysis report using the analyze script
REPORT_FILE="$LOGS_DIR/analysis-${TIMESTAMP}.md"

{
    echo "# Agent Simulation Analysis Report"
    echo ""
    echo "**Generated:** $(date)"
    echo "**Agents:** ${AGENT_IDS[*]}"
    echo ""
    echo "---"
    echo ""

    # Run analysis and capture output
    "$PROJECT_ROOT/scripts/analyze-simulation-agents.sh" "${AGENT_IDS[@]}" 2>&1 | sed 's/\x1b\[[0-9;]*m//g'

} > "$REPORT_FILE"

echo -e "${GREEN}✓${NC} Analysis report: $(basename "$REPORT_FILE")"
echo ""

# Summary
echo -e "${GREEN}=== Export Complete ===${NC}"
echo ""
echo "Files created:"
for agent_id in "${AGENT_IDS[@]}"; do
    if [[ -f "$LOGS_DIR/agent-${agent_id}.compact.jsonl" ]]; then
        echo "  • logs/agent-${agent_id}.jsonl (raw)"
        echo "  • logs/agent-${agent_id}.compact.jsonl (90-95% smaller)"
    elif [[ -f "$LOGS_DIR/agent-${agent_id}.jsonl" ]]; then
        echo "  • logs/agent-${agent_id}.jsonl"
    fi
done
echo "  • logs/$(basename "$REPORT_FILE")"
echo ""
echo "To view:"
echo "  cat logs/$(basename "$REPORT_FILE")"
echo "  ./scripts/view-session.py logs/agent-{ID}.jsonl"
echo "  cat logs/agent-{ID}.compact.jsonl  # Minimal schema"
