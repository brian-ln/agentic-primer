#!/usr/bin/env bash
# Efficient session log analyzer

SESSION_DIR=~/.claude/projects/$(pwd | tr '/\\' '-')

# Function: Get session summary
session_summary() {
    local session_id=$1
    jq -r --arg sid "$session_id" '
        .entries[] |
        select(.sessionId == $sid) |
        "[\(.created)] \(.summary) (\(.messageCount) messages)"
    ' "$SESSION_DIR/sessions-index.json"
}

# Function: Extract conversation (deduplicated)
session_transcript() {
    local session_file=$1
    cat "$session_file" | jq -r '
        select(.type == "user" or .type == "assistant") |
        select(.message != null) |
        "\n## \(.type | ascii_upcase)\n\(.message.content[]? |
            if .type == "text" then .text
            elif .type == "tool_use" then "[Tool: \(.name)]"
            else ""
            end
        )"
    ' | awk '!seen[$0]++'  # Deduplicate identical lines
}

# Function: Extract tool usage timeline
session_tools() {
    local session_file=$1
    cat "$session_file" | jq -r '
        select(.message.content[]?.type == "tool_use") |
        "\(.timestamp | sub("T"; " ") | sub("\\..*"; ""))\t\(.message.content[] | select(.type == "tool_use") | .name)\t\(.message.content[] | select(.type == "tool_use") | .input.description // .input.file_path // .input.pattern // "")"
    ' | awk '!seen[$0]++' | head -20
}

# Function: Extract agent relationships
session_agents() {
    local session_id=$1
    if [ -d "$SESSION_DIR/$session_id/subagents" ]; then
        echo "Agents spawned:"
        ls "$SESSION_DIR/$session_id/subagents/" | sed 's/agent-//;s/.jsonl//' |
        while read agent_id; do
            # Get agent's first message to understand its task
            local task=$(head -20 "$SESSION_DIR/$session_id/subagents/agent-$agent_id.jsonl" |
                jq -r 'select(.type == "user") | .message.content' | head -1 | cut -c1-80)
            echo "  - $agent_id: ${task}..."
        done
    else
        echo "No agents spawned"
    fi
}

# Function: Get file modifications
session_files() {
    local session_file=$1
    cat "$session_file" | jq -r '
        select(.message.content[]? | select(.type == "tool_use") | .name == "Edit" or .name == "Write") |
        .message.content[] | select(.type == "tool_use") |
        .input.file_path
    ' | sort -u
}

# Main commands
case "${1:-help}" in
    list)
        echo "Recent sessions:"
        jq -r '.entries[] | "[\(.created | sub("T.*"; ""))] \(.sessionId | .[0:8])... \(.summary)"' \
            "$SESSION_DIR/sessions-index.json" | tail -20
        ;;

    summary)
        if [ -z "$2" ]; then
            echo "Usage: $0 summary <session-id>"
            exit 1
        fi
        session_summary "$2"
        ;;

    transcript)
        if [ -z "$2" ]; then
            echo "Usage: $0 transcript <session-id>"
            exit 1
        fi
        session_transcript "$SESSION_DIR/$2.jsonl"
        ;;

    tools)
        if [ -z "$2" ]; then
            echo "Usage: $0 tools <session-id>"
            exit 1
        fi
        echo "Tool usage timeline:"
        session_tools "$SESSION_DIR/$2.jsonl"
        ;;

    agents)
        if [ -z "$2" ]; then
            echo "Usage: $0 agents <session-id>"
            exit 1
        fi
        session_agents "$2"
        ;;

    files)
        if [ -z "$2" ]; then
            echo "Usage: $0 files <session-id>"
            exit 1
        fi
        echo "Files modified:"
        session_files "$SESSION_DIR/$2.jsonl"
        ;;

    full)
        if [ -z "$2" ]; then
            echo "Usage: $0 full <session-id>"
            exit 1
        fi
        echo "=== SESSION ANALYSIS ==="
        echo
        session_summary "$2"
        echo
        echo "=== AGENTS ==="
        session_agents "$2"
        echo
        echo "=== FILES MODIFIED ==="
        session_files "$SESSION_DIR/$2.jsonl"
        echo
        echo "=== TOOL TIMELINE ==="
        session_tools "$SESSION_DIR/$2.jsonl"
        ;;

    *)
        cat <<EOF
Usage: $0 <command> [session-id]

Commands:
  list              - Show recent sessions
  summary <sid>     - Show session summary
  transcript <sid>  - Extract deduplicated conversation
  tools <sid>       - Show tool usage timeline
  agents <sid>      - Show spawned agents
  files <sid>       - Show modified files
  full <sid>        - Full session analysis

Example:
  $0 list
  $0 full aeb742bc-5fb0-45b5-b343-74ac1f8b9a3b
EOF
        ;;
esac
