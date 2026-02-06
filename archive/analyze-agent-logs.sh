#!/bin/bash

# Analyze agent log files for tool usage patterns
# Usage: ./analyze-agent-logs.sh

AGENT_DIR="$HOME/.claude/projects/-Users-bln-play-agentic-primer"

# Map agent IDs to their configurations
declare -A AGENT_MAP=(
    ["ad7d53c"]="Opus 10-word"
    ["a7c3dfb"]="Sonnet 10-word"
    ["a525bb6"]="Haiku 10-word"
    ["a26a239"]="Opus 14-word"
    ["a4b876c"]="Sonnet 14-word"
    ["a8a4b15"]="Haiku 14-word"
    ["a907acb"]="Opus 35-word"
    ["abafbf0"]="Sonnet 35-word"
    ["ac9f33a"]="Haiku 35-word"
)

echo "# Agent Process Analysis - @copilot Simulation"
echo ""
echo "Analysis date: $(date)"
echo ""

for agent_id in ad7d53c a7c3dfb a525bb6 a26a239 a4b876c a8a4b15 a907acb abafbf0 ac9f33a; do
    log_file="$AGENT_DIR/agent-$agent_id.jsonl"

    if [[ ! -f "$log_file" ]]; then
        echo "## ${AGENT_MAP[$agent_id]} ($agent_id) - LOG NOT FOUND"
        echo ""
        continue
    fi

    echo "## ${AGENT_MAP[$agent_id]} ($agent_id)"
    echo ""

    # File size
    size=$(ls -lh "$log_file" | awk '{print $5}')
    echo "**Log size:** $size"

    # Total messages (each line is a message)
    total_msgs=$(wc -l < "$log_file")
    echo "**Total messages:** $total_msgs"

    # Tool usage patterns - extract tool names from the logs
    echo ""
    echo "**Tool usage:**"

    # Extract tool calls - looking for common patterns in JSONL
    # Tools appear in "name" fields or as function calls

    # Count Read tool calls
    read_count=$(grep -o '"name":"Read"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $read_count -gt 0 ]] && echo "- Read: $read_count"

    # Count Write tool calls
    write_count=$(grep -o '"name":"Write"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $write_count -gt 0 ]] && echo "- Write: $write_count"

    # Count WebSearch tool calls
    websearch_count=$(grep -o '"name":"WebSearch"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $websearch_count -gt 0 ]] && echo "- WebSearch: $websearch_count"

    # Count WebFetch tool calls
    webfetch_count=$(grep -o '"name":"WebFetch"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $webfetch_count -gt 0 ]] && echo "- WebFetch: $webfetch_count"

    # Count Grep tool calls
    grep_count=$(grep -o '"name":"Grep"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $grep_count -gt 0 ]] && echo "- Grep: $grep_count"

    # Count Glob tool calls
    glob_count=$(grep -o '"name":"Glob"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $glob_count -gt 0 ]] && echo "- Glob: $glob_count"

    # Count Bash tool calls
    bash_count=$(grep -o '"name":"Bash"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $bash_count -gt 0 ]] && echo "- Bash: $bash_count"

    # Count Edit tool calls
    edit_count=$(grep -o '"name":"Edit"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $edit_count -gt 0 ]] && echo "- Edit: $edit_count"

    # Count TodoWrite tool calls
    todo_count=$(grep -o '"name":"TodoWrite"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $todo_count -gt 0 ]] && echo "- TodoWrite: $todo_count"

    # Count Skill tool calls
    skill_count=$(grep -o '"name":"Skill"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    [[ $skill_count -gt 0 ]] && echo "- Skill: $skill_count"

    # Extract web search queries if any
    echo ""
    echo "**Web searches performed:**"
    grep '"name":"WebSearch"' "$log_file" -A 5 2>/dev/null | grep '"query"' | sed 's/.*"query":"\([^"]*\)".*/- \1/' | head -10

    # Check if no web searches
    if [[ $websearch_count -eq 0 && $webfetch_count -eq 0 ]]; then
        echo "- None"
    fi

    echo ""
    echo "---"
    echo ""
done

echo ""
echo "# Summary Statistics"
echo ""

# Create comparison table
echo "| Agent | Model | Prompt | Log Size | Messages | Read | Write | WebSearch | WebFetch | Total Tools |"
echo "|-------|-------|--------|----------|----------|------|-------|-----------|----------|-------------|"

for agent_id in ad7d53c a7c3dfb a525bb6 a26a239 a4b876c a8a4b15 a907acb abafbf0 ac9f33a; do
    log_file="$AGENT_DIR/agent-$agent_id.jsonl"

    if [[ ! -f "$log_file" ]]; then
        continue
    fi

    # Parse agent info
    config="${AGENT_MAP[$agent_id]}"
    model=$(echo "$config" | awk '{print $1}')
    prompt=$(echo "$config" | awk '{print $2}')

    # Get metrics
    size=$(ls -lh "$log_file" | awk '{print $5}')
    msgs=$(wc -l < "$log_file" | tr -d ' ')
    reads=$(grep -o '"name":"Read"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    writes=$(grep -o '"name":"Write"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    websearches=$(grep -o '"name":"WebSearch"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')
    webfetches=$(grep -o '"name":"WebFetch"' "$log_file" 2>/dev/null | wc -l | tr -d ' ')

    total_tools=$((reads + writes + websearches + webfetches))

    echo "| $agent_id | $model | $prompt | $size | $msgs | $reads | $writes | $websearches | $webfetches | $total_tools |"
done
