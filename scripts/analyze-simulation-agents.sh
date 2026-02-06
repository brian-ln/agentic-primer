#!/usr/bin/env bash
# analyze-simulation-agents.sh - Comprehensive agent execution analysis
#
# Usage: ./analyze-simulation-agents.sh <agent-id-1> <agent-id-2> ... [<agent-id-n>]
# Example: ./analyze-simulation-agents.sh ad7d53c a7c3dfb a525bb6
#
# Analyzes agent .jsonl logs to extract:
# - Token usage (input, output, cached)
# - Tool usage patterns (Read, Write, WebSearch, Bash)
# - Execution duration and timestamps
# - Efficiency metrics (tokens per tool, output per second)
# - Behavioral signatures and archetypes

set -euo pipefail

# Configuration
CLAUDE_PROJECTS_DIR="$HOME/.claude/projects"
PROJECT_ID="-Users-bln-play-agentic-primer"
AGENT_DIR="$CLAUDE_PROJECTS_DIR/$PROJECT_ID"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

usage() {
    echo "Usage: $0 <agent-id-1> <agent-id-2> ... [<agent-id-n>]"
    echo ""
    echo "Comprehensive analysis of simulation agent execution patterns."
    echo ""
    echo "Example:"
    echo "  $0 ad7d53c a7c3dfb a525bb6"
    echo ""
    echo "Metrics extracted:"
    echo "  • Token usage (input/output/cached)"
    echo "  • Tool usage and patterns"
    echo "  • Execution time and efficiency"
    echo "  • Behavioral signatures"
    exit 1
}

if [ $# -eq 0 ]; then
    usage
fi

AGENT_IDS=("$@")

echo -e "${BLUE}=== Comprehensive Agent Analysis ===${NC}"
echo ""
echo "Analyzing ${#AGENT_IDS[@]} agents..."
echo ""

# Enhanced analysis function
analyze_agent() {
    local agent_id="$1"
    local log_file="$AGENT_DIR/agent-${agent_id}.jsonl"

    if [[ ! -f "$log_file" ]]; then
        echo -e "${RED}ERROR: Agent log not found: $log_file${NC}"
        return 1
    fi

    # Basic counts
    local total_lines=$(wc -l < "$log_file" | tr -d ' ')
    local read_count=$(grep -c '"name":"Read"' "$log_file" 2>/dev/null || echo "0")
    local write_count=$(grep -c '"name":"Write"' "$log_file" 2>/dev/null || echo "0")
    local websearch_count=$(grep -c '"name":"WebSearch"' "$log_file" 2>/dev/null || echo "0")
    local bash_count=$(grep -c '"name":"Bash"' "$log_file" 2>/dev/null || echo "0")

    # Ensure single values (strip whitespace/newlines)
    read_count=$(echo "$read_count" | tr -d '\n\r ')
    write_count=$(echo "$write_count" | tr -d '\n\r ')
    websearch_count=$(echo "$websearch_count" | tr -d '\n\r ')
    bash_count=$(echo "$bash_count" | tr -d '\n\r ')

    local total_tools=$((read_count + write_count + websearch_count + bash_count))

    # Token usage (extract from usage fields in responses)
    local input_tokens=$(grep -o '"input_tokens":[0-9]*' "$log_file" | cut -d: -f2 | awk '{sum+=$1} END {print sum+0}')
    local output_tokens=$(grep -o '"output_tokens":[0-9]*' "$log_file" | cut -d: -f2 | awk '{sum+=$1} END {print sum+0}')
    local cache_read=$(grep -o '"cache_read_input_tokens":[0-9]*' "$log_file" | cut -d: -f2 | awk '{sum+=$1} END {print sum+0}')
    local cache_creation=$(grep -o '"cache_creation_input_tokens":[0-9]*' "$log_file" | cut -d: -f2 | awk '{sum+=$1} END {print sum+0}')

    # Total tokens
    local total_tokens=$((input_tokens + output_tokens))

    # Time analysis (extract timestamps)
    local first_ts=$(head -1 "$log_file" | grep -o '"timestamp":"[^"]*"' | cut -d'"' -f4 || echo "")
    local last_ts=$(tail -1 "$log_file" | grep -o '"timestamp":"[^"]*"' | cut -d'"' -f4 || echo "")

    # Calculate duration in seconds (if timestamps available)
    local duration_sec=0
    if [[ -n "$first_ts" && -n "$last_ts" ]]; then
        local first_epoch=$(date -j -f "%Y-%m-%dT%H:%M:%S" "${first_ts:0:19}" +%s 2>/dev/null || echo "0")
        local last_epoch=$(date -j -f "%Y-%m-%dT%H:%M:%S" "${last_ts:0:19}" +%s 2>/dev/null || echo "0")
        duration_sec=$((last_epoch - first_epoch))
    fi

    # Efficiency metrics
    local tokens_per_tool=0
    if [[ $total_tools -gt 0 ]]; then
        tokens_per_tool=$((total_tokens / total_tools))
    fi

    local tokens_per_sec=0
    if [[ $duration_sec -gt 0 ]]; then
        tokens_per_sec=$((total_tokens / duration_sec))
    fi

    # File size
    local file_size=$(du -h "$log_file" | cut -f1)

    # Output: pipe-separated for easy parsing
    echo "$agent_id|$total_lines|$read_count|$write_count|$websearch_count|$bash_count|$total_tools|$input_tokens|$output_tokens|$cache_read|$cache_creation|$total_tokens|$duration_sec|$tokens_per_tool|$tokens_per_sec|$file_size"
}

# Collect data
echo "Collecting metrics..."
declare -a RESULTS=()

for agent_id in "${AGENT_IDS[@]}"; do
    result=$(analyze_agent "$agent_id")
    if [[ $? -eq 0 ]]; then
        RESULTS+=("$result")
    fi
done

# Display comprehensive metrics table
echo ""
echo -e "${GREEN}=== Execution Metrics ===${NC}"
echo ""
printf "%-10s | %8s | %8s | %5s | %8s | %10s\n" \
    "Agent" "Messages" "Duration" "Tools" "Tokens" "Cache Read"
echo "-----------|----------|----------|-------|----------|------------"

for result in "${RESULTS[@]}"; do
    IFS='|' read -r agent msgs reads writes searches bashes tools in_tok out_tok cache_r cache_c total_tok dur tpt tps size <<< "$result"

    # Format duration
    dur_fmt="${dur}s"
    if [[ $dur -gt 60 ]]; then
        mins=$((dur / 60))
        secs=$((dur % 60))
        dur_fmt="${mins}m${secs}s"
    fi

    # Format cache read (KB)
    cache_kb=$((cache_r / 1000))
    cache_fmt="${cache_kb}K"
    if [[ $cache_r -eq 0 ]]; then
        cache_fmt="0"
    fi

    printf "%-10s | %8s | %8s | %5s | %8s | %10s\n" \
        "$agent" "$msgs" "$dur_fmt" "$tools" "$total_tok" "$cache_fmt"
done

# Tool usage breakdown
echo ""
echo -e "${GREEN}=== Tool Usage Breakdown ===${NC}"
echo ""
printf "%-10s | %4s | %5s | %9s | %4s | %8s\n" \
    "Agent" "Read" "Write" "WebSearch" "Bash" "Tokens/Tool"
echo "-----------|------|-------|-----------|------|----------"

for result in "${RESULTS[@]}"; do
    IFS='|' read -r agent msgs reads writes searches bashes tools in_tok out_tok cache_r cache_c total_tok dur tpt tps size <<< "$result"
    printf "%-10s | %4s | %5s | %9s | %4s | %8s\n" \
        "$agent" "$reads" "$writes" "$searches" "$bashes" "$tpt"
done

# Token economics
echo ""
echo -e "${GREEN}=== Token Economics ===${NC}"
echo ""
printf "%-10s | %8s | %9s | %10s | %11s | %8s\n" \
    "Agent" "Input" "Output" "Cached R" "Cached W" "Total"
echo "-----------|----------|-----------|------------|-------------|----------"

for result in "${RESULTS[@]}"; do
    IFS='|' read -r agent msgs reads writes searches bashes tools in_tok out_tok cache_r cache_c total_tok dur tpt tps size <<< "$result"

    # Format numbers with K suffix for readability
    in_k=$((in_tok / 1000))
    out_k=$((out_tok / 1000))
    cache_r_k=$((cache_r / 1000))
    cache_c_k=$((cache_c / 1000))
    total_k=$((total_tok / 1000))

    printf "%-10s | %7sK | %8sK | %9sK | %10sK | %7sK\n" \
        "$agent" "$in_k" "$out_k" "$cache_r_k" "$cache_c_k" "$total_k"
done

# Efficiency metrics
echo ""
echo -e "${GREEN}=== Efficiency Metrics ===${NC}"
echo ""
printf "%-10s | %12s | %12s | %8s\n" \
    "Agent" "Tokens/Sec" "Tools/Min" "Log Size"
echo "-----------|--------------|--------------|----------"

for result in "${RESULTS[@]}"; do
    IFS='|' read -r agent msgs reads writes searches bashes tools in_tok out_tok cache_r cache_c total_tok dur tpt tps size <<< "$result"

    # Tools per minute
    tools_per_min=0
    if [[ $dur -gt 0 ]]; then
        tools_per_min=$(awk "BEGIN {printf \"%.1f\", ($tools * 60.0) / $dur}")
    fi

    printf "%-10s | %12s | %12s | %8s\n" \
        "$agent" "$tps" "$tools_per_min" "$size"
done

# Behavioral signatures
echo ""
echo -e "${GREEN}=== Behavioral Signatures ===${NC}"
echo ""

extract_web_searches() {
    local agent_id="$1"
    local log_file="$AGENT_DIR/agent-${agent_id}.jsonl"
    [[ ! -f "$log_file" ]] && return 1
    grep '"name":"WebSearch"' "$log_file" | \
        grep -oE '"query":"[^"]*"' | \
        sed 's/"query":"//g' | \
        sed 's/"$//g'
}

extract_files_created() {
    local agent_id="$1"
    local log_file="$AGENT_DIR/agent-${agent_id}.jsonl"
    [[ ! -f "$log_file" ]] && return 1
    grep '"name":"Write"' "$log_file" | \
        grep -oE '"file_path":"[^"]*"' | \
        sed 's/"file_path":"//g' | \
        sed 's/"$//g' | \
        sed 's|.*/||'
}

for result in "${RESULTS[@]}"; do
    IFS='|' read -r agent msgs reads writes searches bashes tools in_tok out_tok cache_r cache_c total_tok dur tpt tps size <<< "$result"

    # Classify archetype
    archetype=""
    if [[ $tools -eq 0 ]]; then
        archetype="${YELLOW}The Philosopher${NC} (pure reasoning, no tools)"
    elif [[ $searches -gt 0 && $writes -eq 0 ]]; then
        archetype="${BLUE}The Researcher${NC} (web research + exploration)"
    elif [[ $writes -gt 0 ]]; then
        archetype="${GREEN}The Builder${NC} (creates files, iterates)"
    else
        archetype="Hybrid pattern"
    fi

    echo -e "Agent ${YELLOW}$agent${NC}: $archetype"
    echo "  Messages: $msgs | Tools: R:$reads W:$writes WS:$searches B:$bashes"
    echo "  Tokens: ${total_tok} (${in_tok} in, ${out_tok} out, ${cache_r} cached)"
    echo "  Duration: ${dur}s | Efficiency: ${tps} tok/s"

    if [[ $searches -gt 0 ]]; then
        echo "  Web Searches:"
        extract_web_searches "$agent" | sed 's/^/    - /'
    fi

    if [[ $writes -gt 0 ]]; then
        echo "  Files Created:"
        extract_files_created "$agent" | sed 's/^/    - /'
    fi

    echo ""
done

# Summary statistics
echo -e "${GREEN}=== Summary Statistics ===${NC}"
echo ""

total_tools=0
total_tokens=0
total_duration=0

for result in "${RESULTS[@]}"; do
    IFS='|' read -r agent msgs reads writes searches bashes tools in_tok out_tok cache_r cache_c total_tok dur tpt tps size <<< "$result"
    total_tools=$((total_tools + tools))
    total_tokens=$((total_tokens + total_tok))
    total_duration=$((total_duration + dur))
done

num_agents=${#RESULTS[@]}

echo "Agents analyzed: $num_agents"
echo "Total tokens: $total_tokens (avg: $((total_tokens / num_agents)))"
echo "Total tool calls: $total_tools (avg: $((total_tools / num_agents)))"
echo "Total duration: ${total_duration}s (avg: $((total_duration / num_agents))s)"
echo ""

# Efficiency ranking
echo -e "${GREEN}=== Efficiency Ranking ===${NC}"
echo ""
echo "Ranked by tokens/tool (lower = more efficient):"
echo ""

# Sort by tokens per tool
for result in "${RESULTS[@]}"; do
    IFS='|' read -r agent msgs reads writes searches bashes tools in_tok out_tok cache_r cache_c total_tok dur tpt tps size <<< "$result"
    echo "$tpt $agent"
done | sort -n | awk '{printf "  %2d. %s (%s tokens/tool)\n", NR, $2, $1}'

echo ""
echo -e "${BLUE}Analysis complete!${NC}"
echo ""
echo "To export logs and generate report:"
echo "  ./scripts/export-agent-logs.sh ${AGENT_IDS[*]}"
