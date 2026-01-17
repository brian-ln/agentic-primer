#!/bin/bash
# Post-Tool-Use Hook: Update task status when agent completes
#
# This hook automatically updates the task status to "completed" when a
# background agent finishes execution.
#
# USAGE: Install to ~/.claude/hooks/ or .claude/hooks/ in project
#
# STATUS: Reference implementation (not yet enabled by default)
#
# Hook Event Data Available:
# - Tool execution results
# - Session context
# - Agent-to-task mapping from PreToolUse hook

set -euo pipefail

# Read hook event data from stdin
HOOK_DATA=$(cat)

# Parse JSON fields
TOOL_NAME=$(echo "$HOOK_DATA" | jq -r '.data.tool_name // ""')
RUN_IN_BACKGROUND=$(echo "$HOOK_DATA" | jq -r '.data.tool_input.run_in_background // false')
PROJECT_DIR=$(echo "$HOOK_DATA" | jq -r '.env.CLAUDE_PROJECT_DIR // ""')

# Only process background agents
if [[ "$TOOL_NAME" != "Bash" ]] || [[ "$RUN_IN_BACKGROUND" != "true" ]]; then
  echo '{"continue": true}'
  exit 0
fi

log() {
  echo "[hook:post-tool-use] $*" >&2
}

log "Background agent completed"

# Lookup task ID from mapping
AGENT_ID=$(echo "$HOOK_DATA" | jq -r '.data.session_id // "unknown"')
MAPPING_FILE="$PROJECT_DIR/.agent-task-map.txt"

if [[ ! -f "$MAPPING_FILE" ]]; then
  log "Warning: No agent-task mapping file found"
  echo '{"continue": true}'
  exit 0
fi

TASK_ID=$(grep "^$AGENT_ID=" "$MAPPING_FILE" 2>/dev/null | cut -d= -f2)

if [[ -z "$TASK_ID" ]]; then
  log "Warning: No task found for agent $AGENT_ID"
  echo '{"continue": true}'
  exit 0
fi

log "Found task $TASK_ID for agent $AGENT_ID"

# Update task status
cd "$PROJECT_DIR"

# Check if agent succeeded or failed (from tool output)
TOOL_SUCCESS=$(echo "$HOOK_DATA" | jq -r '.data.tool_output.success // true')

if [[ "$TOOL_SUCCESS" == "true" ]]; then
  log "Marking task $TASK_ID as completed"
  bun src/cli/task.ts update "$TASK_ID" complete 2>&1 | log
else
  log "Marking task $TASK_ID as failed"
  bun src/cli/task.ts update "$TASK_ID" block "Agent execution failed" 2>&1 | log
fi

# Clean up mapping entry
sed -i.bak "/^$AGENT_ID=/d" "$MAPPING_FILE"
log "Cleaned up mapping for agent $AGENT_ID"

echo '{"continue": true}'
exit 0
