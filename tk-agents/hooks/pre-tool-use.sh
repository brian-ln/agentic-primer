#!/bin/bash
# Pre-Tool-Use Hook: Auto-create tasks for background agents
#
# This hook automatically creates a task in tasks.json when a background
# agent is launched using the Bash tool with run_in_background: true
#
# USAGE: Install to ~/.claude/hooks/ or .claude/hooks/ in project
#
# STATUS: Reference implementation (not yet enabled by default)
#
# Hook Event Data Available:
# - CLAUDE_HOOK_TOOL_NAME: Name of tool being used
# - CLAUDE_HOOK_DESCRIPTION: Tool description
# - CLAUDE_HOOK_RUN_IN_BACKGROUND: "true" if background agent
# - CLAUDE_PROJECT_DIR: Project root directory
# - Session context in JSON format via stdin

set -euo pipefail

# Read hook event data from stdin
HOOK_DATA=$(cat)

# Parse JSON fields (requires jq)
TOOL_NAME=$(echo "$HOOK_DATA" | jq -r '.data.tool_name // ""')
RUN_IN_BACKGROUND=$(echo "$HOOK_DATA" | jq -r '.data.tool_input.run_in_background // false')
DESCRIPTION=$(echo "$HOOK_DATA" | jq -r '.data.tool_input.description // ""')
PROJECT_DIR=$(echo "$HOOK_DATA" | jq -r '.env.CLAUDE_PROJECT_DIR // ""')

# Only process background agents
if [[ "$TOOL_NAME" != "Bash" ]] || [[ "$RUN_IN_BACKGROUND" != "true" ]]; then
  # Not a background agent, continue normally
  echo '{"continue": true, "hookSpecificOutput": {"permissionDecision": "allow"}}'
  exit 0
fi

# Background agent detected
log() {
  echo "[hook:pre-tool-use] $*" >&2
}

log "Detected background agent: $DESCRIPTION"

# Check if tasks.json exists
TASK_FILE="$PROJECT_DIR/tasks.json"
if [[ ! -f "$TASK_FILE" ]]; then
  log "Warning: tasks.json not found, skipping task creation"
  echo '{"continue": true, "hookSpecificOutput": {"permissionDecision": "allow"}}'
  exit 0
fi

# Extract agent type from description (heuristic)
AGENT_TYPE="general"
if echo "$DESCRIPTION" | grep -iq "research"; then
  AGENT_TYPE="research"
elif echo "$DESCRIPTION" | grep -iq "implement\|code"; then
  AGENT_TYPE="implementation"
elif echo "$DESCRIPTION" | grep -iq "analyz\|review"; then
  AGENT_TYPE="analysis"
elif echo "$DESCRIPTION" | grep -iq "test"; then
  AGENT_TYPE="testing"
fi

# Create task using CLI
cd "$PROJECT_DIR"

# Use --json to get structured output
TASK_RESULT=$(bun src/cli/task.ts add "Agent: $DESCRIPTION" \
  --labels "agent,$AGENT_TYPE" \
  --priority P2 \
  --json 2>&1)

if [[ $? -eq 0 ]]; then
  TASK_ID=$(echo "$TASK_RESULT" | jq -r '.data.id')
  log "Created task $TASK_ID for agent"

  # Store agent-to-task mapping for PostToolUse hook
  AGENT_ID=$(echo "$HOOK_DATA" | jq -r '.data.session_id // "unknown"')
  MAPPING_FILE="$PROJECT_DIR/.agent-task-map.txt"
  echo "$AGENT_ID=$TASK_ID" >> "$MAPPING_FILE"

  log "Stored mapping: $AGENT_ID -> $TASK_ID"
else
  log "Error creating task: $TASK_RESULT"
fi

# Always allow the tool to continue
echo '{"continue": true, "hookSpecificOutput": {"permissionDecision": "allow"}}'
exit 0
