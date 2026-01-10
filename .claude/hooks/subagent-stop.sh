#!/usr/bin/env bash
# SubagentStop hook - Fires when background agents complete
# Purpose: Auto-check for newly-ready tasks and suggest next actions

set -euo pipefail

# Read hook input from stdin
read -r hook_input

# Parse session context
session_id=$(echo "$hook_input" | jq -r '.session_id // empty')
cwd=$(echo "$hook_input" | jq -r '.cwd // empty')

# Only activate in event-system directory
if [[ "$cwd" != *".wt/event-system"* ]]; then
  exit 0
fi

# Give brief feedback
echo ""
echo "🎉 Agent completed! Checking for newly-ready tasks..."
echo ""

# Check what's ready now
cd "$cwd"
ready_output=$(bd ready --format json 2>/dev/null || echo "[]")
ready_count=$(echo "$ready_output" | jq 'length')

if [[ "$ready_count" -eq 0 ]]; then
  echo "✅ No new tasks ready (waiting on dependencies)"
  exit 0
fi

# Show ready tasks
echo "📋 Ready tasks ($ready_count):"
bd ready | head -20

# Suggest parallelization opportunities
echo ""
echo "💡 Parallelization check:"

# Count event-system tasks that are ready
event_system_ready=$(echo "$ready_output" | jq '[.[] | select(.labels | contains(["event-system"]))] | length')

if [[ "$event_system_ready" -gt 1 ]]; then
  echo "   → $event_system_ready event-system tasks ready"
  echo "   → Consider spawning parallel agents with /bg"
fi

# Check for phase completion
for phase in phase-1a phase-1b phase-1c phase-1d phase-1e phase-1f; do
  phase_open=$(bd list --label "$phase" --status open --format json 2>/dev/null | jq 'length')
  if [[ "$phase_open" -eq 0 ]]; then
    phase_closed=$(bd list --label "$phase" --status closed --format json 2>/dev/null | jq 'length')
    if [[ "$phase_closed" -gt 0 ]]; then
      echo "   🎉 $phase complete!"
    fi
  fi
done

echo ""

# Inject context back into conversation (optional)
cat <<EOF
{
  "hookSpecificOutput": {
    "hookEventName": "SubagentStop",
    "additionalContext": "Agent completed. $ready_count tasks now ready. Check 'bd ready' for details."
  }
}
EOF

exit 0
