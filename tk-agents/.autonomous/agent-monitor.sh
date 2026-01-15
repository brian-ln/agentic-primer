#!/usr/bin/env bash
# Agent Monitor - Check status of all background agents

echo "=== Agent Status Monitor ==="
echo "Time: $(date)"
echo ""

# Active agents
agents=(
  "a89b813:Pressure test scenarios"
  "ace3a16:Error model exploration"
  "ac61ea1:Latency/locality tiers"
  "a55ded8:Category theory application"
  "a0e180d:Event sourcing reality check"
  "a3ce6a3:Meta conversation analysis"
  "ad21a4b:Cross-reference analysis"
  "a2f6bdc:Exploration roadmap"
)

completed=0
running=0
failed=0

for agent_info in "${agents[@]}"; do
  IFS=':' read -r agent_id agent_name <<< "$agent_info"
  output_file="/tmp/claude/-Users-bln-play-projects-proj-20260113-150839-agentic-primer-tk-agents/tasks/${agent_id}.output"

  if [ -f "$output_file" ]; then
    # Check if completed
    if tail -n 20 "$output_file" | grep -q "COMPLETION_REPORT\|completed successfully"; then
      echo "✓ $agent_name ($agent_id): COMPLETED"
      ((completed++))
    elif tail -n 20 "$output_file" | grep -q "error\|failed\|STOP_WORK"; then
      echo "✗ $agent_name ($agent_id): FAILED/BLOCKED"
      ((failed++))
    else
      echo "⏳ $agent_name ($agent_id): RUNNING"
      ((running++))
    fi
  else
    echo "❓ $agent_name ($agent_id): OUTPUT FILE NOT FOUND"
  fi
done

echo ""
echo "Summary: $completed completed, $running running, $failed failed"
echo "========================================="
