#!/usr/bin/env bash
# activity-lib.sh - Shared library functions for activity management

set -euo pipefail

# === Configuration ===
REPO_ROOT="/Users/bln/play/agentic-primer"
WORKTREE_DIR="$REPO_ROOT/.wt"
ACTIVITIES_JSON="$WORKTREE_DIR/.activities.json"
CURRENT_ACTIVITY_FILE="$WORKTREE_DIR/.current-activity"

# === Utility Functions ===

# Get ISO 8601 timestamp
now_iso8601() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# Convert ISO 8601 to human-readable "time ago"
time_ago() {
    local timestamp="$1"
    local now_epoch
    local then_epoch
    local diff

    now_epoch=$(date +%s)
    then_epoch=$(date -j -f "%Y-%m-%dT%H:%M:%SZ" "$timestamp" +%s 2>/dev/null || echo "0")
    diff=$((now_epoch - then_epoch))

    if [ "$diff" -lt 60 ]; then
        echo "${diff}s ago"
    elif [ "$diff" -lt 3600 ]; then
        echo "$((diff / 60))m ago"
    elif [ "$diff" -lt 86400 ]; then
        echo "$((diff / 3600))h ago"
    else
        echo "$((diff / 86400))d ago"
    fi
}

# === Validation Functions ===

# Validate activity name format
validate_activity_name() {
    local name="$1"

    # Check format: lowercase, numbers, dashes only
    if ! [[ "$name" =~ ^[a-z0-9-]+$ ]]; then
        echo "Error: Activity name must be lowercase letters, numbers, and dashes only" >&2
        echo "Examples: exploring-idea-x, feature-123, experiment-1" >&2
        return 1
    fi

    # Check reserved names
    if [[ "$name" == "list" || "$name" == "current" || "$name" == "path" || "$name" == "show" ]]; then
        echo "Error: '$name' is a reserved command name" >&2
        return 1
    fi

    # Check length
    if [ ${#name} -gt 50 ]; then
        echo "Error: Activity name too long (max 50 characters)" >&2
        return 1
    fi

    return 0
}

# Validate JSON structure
validate_activities_json() {
    local json_file="$1"

    if [ ! -f "$json_file" ]; then
        echo "Error: Activities metadata file not found: $json_file" >&2
        return 1
    fi

    # Check if valid JSON
    if ! jq . "$json_file" >/dev/null 2>&1; then
        echo "Error: Invalid JSON in $json_file" >&2
        return 1
    fi

    # Check required fields
    if ! jq -e '.schema_version' "$json_file" >/dev/null 2>&1; then
        echo "Error: Missing schema_version in metadata" >&2
        return 1
    fi

    if ! jq -e '.current_activity' "$json_file" >/dev/null 2>&1; then
        echo "Error: Missing current_activity in metadata" >&2
        return 1
    fi

    if ! jq -e '.activities | type == "object"' "$json_file" >/dev/null 2>&1; then
        echo "Error: activities must be an object" >&2
        return 1
    fi

    return 0
}

# === Metadata Functions ===

# Initialize activities.json if it doesn't exist
ensure_activities_json() {
    if [ ! -f "$ACTIVITIES_JSON" ]; then
        echo "Error: Activities not initialized. Run: ./scripts/activity init" >&2
        return 1
    fi
    return 0
}

# Read current activity name
get_current_activity() {
    ensure_activities_json || return 1
    jq -r '.current_activity' "$ACTIVITIES_JSON"
}

# Set current activity
set_current_activity() {
    local name="$1"

    ensure_activities_json || return 1

    # Update .activities.json
    local tmp_file
    tmp_file=$(mktemp)
    jq --arg name "$name" \
       --arg timestamp "$(now_iso8601)" \
       '.current_activity = $name |
        .activities[$name].last_accessed = $timestamp' \
       "$ACTIVITIES_JSON" > "$tmp_file"

    if validate_activities_json "$tmp_file"; then
        mv "$tmp_file" "$ACTIVITIES_JSON"
    else
        rm "$tmp_file"
        return 1
    fi

    # Update .current-activity file
    echo "$name" > "$CURRENT_ACTIVITY_FILE"

    return 0
}

# Get activity field value
get_activity_field() {
    local name="$1"
    local field="$2"

    ensure_activities_json || return 1

    jq -r --arg name "$name" --arg field "$field" \
       '.activities[$name][$field] // empty' \
       "$ACTIVITIES_JSON"
}

# Check if activity exists
activity_exists() {
    local name="$1"

    ensure_activities_json || return 1

    if jq -e --arg name "$name" '.activities[$name]' "$ACTIVITIES_JSON" >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Get activity worktree path
get_activity_path() {
    local name="$1"

    ensure_activities_json || return 1

    get_activity_field "$name" "worktree_path"
}

# List all activities
list_activities() {
    local status_filter="${1:-all}"

    ensure_activities_json || return 1

    jq -r --arg filter "$status_filter" \
       '.activities | to_entries[] |
        select(
            ($filter == "all") or
            (.value.status == $filter)
        ) |
        [.key, .value.branch, .value.status, .value.last_accessed] |
        @tsv' \
       "$ACTIVITIES_JSON"
}

# Add activity to metadata
add_activity() {
    local name="$1"
    local description="$2"
    local worktree_path="$3"
    local branch="$4"
    local parent_branch="${5:-main}"

    ensure_activities_json || return 1

    local tmp_file
    tmp_file=$(mktemp)

    jq --arg name "$name" \
       --arg desc "$description" \
       --arg path "$worktree_path" \
       --arg branch "$branch" \
       --arg parent "$parent_branch" \
       --arg created "$(now_iso8601)" \
       --arg accessed "$(now_iso8601)" \
       '.activities[$name] = {
           name: $name,
           description: $desc,
           worktree_path: $path,
           branch: $branch,
           is_main: false,
           created: $created,
           last_accessed: $accessed,
           status: "active",
           parent_branch: $parent,
           beads_issues: []
       }' \
       "$ACTIVITIES_JSON" > "$tmp_file"

    if validate_activities_json "$tmp_file"; then
        mv "$tmp_file" "$ACTIVITIES_JSON"
        return 0
    else
        rm "$tmp_file"
        return 1
    fi
}

# Remove activity from metadata
remove_activity() {
    local name="$1"

    ensure_activities_json || return 1

    local tmp_file
    tmp_file=$(mktemp)

    jq --arg name "$name" \
       'del(.activities[$name])' \
       "$ACTIVITIES_JSON" > "$tmp_file"

    if validate_activities_json "$tmp_file"; then
        mv "$tmp_file" "$ACTIVITIES_JSON"
        return 0
    else
        rm "$tmp_file"
        return 1
    fi
}

# Update activity status
update_activity_status() {
    local name="$1"
    local status="$2"

    ensure_activities_json || return 1

    local tmp_file
    tmp_file=$(mktemp)

    jq --arg name "$name" \
       --arg status "$status" \
       '.activities[$name].status = $status' \
       "$ACTIVITIES_JSON" > "$tmp_file"

    if validate_activities_json "$tmp_file"; then
        mv "$tmp_file" "$ACTIVITIES_JSON"
        return 0
    else
        rm "$tmp_file"
        return 1
    fi
}

# === Git Functions ===

# Get current git branch
get_current_branch() {
    git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD
}

# Get main/master branch name
get_main_branch() {
    # Try to detect main vs master
    if git -C "$REPO_ROOT" show-ref --verify --quiet refs/heads/main; then
        echo "main"
    elif git -C "$REPO_ROOT" show-ref --verify --quiet refs/heads/master; then
        echo "master"
    else
        # Fallback to current branch
        get_current_branch
    fi
}

# Check if worktree exists
worktree_exists() {
    local path="$1"
    git -C "$REPO_ROOT" worktree list | grep -q "$path"
}

# === Display Functions ===

# Print colored message
print_success() {
    echo -e "\033[32m✓\033[0m $*"
}

print_error() {
    echo -e "\033[31m✗\033[0m $*" >&2
}

print_warning() {
    echo -e "\033[33m!\033[0m $*" >&2
}

print_info() {
    echo -e "\033[34mℹ\033[0m $*"
}

# Print activity table header
print_activity_table_header() {
    printf "%-25s %-30s %-10s %-15s\n" "ACTIVITY" "BRANCH" "STATUS" "LAST ACCESSED"
    printf "%-25s %-30s %-10s %-15s\n" "--------" "------" "------" "-------------"
}

# Print activity table row
print_activity_row() {
    local name="$1"
    local branch="$2"
    local status="$3"
    local last_accessed="$4"
    local is_current="$5"

    local marker=" "
    if [ "$is_current" = "true" ]; then
        marker="*"
    fi

    local time_ago_str
    time_ago_str=$(time_ago "$last_accessed")

    printf "%s %-24s %-30s %-10s %-15s\n" \
           "$marker" "$name" "$branch" "$status" "$time_ago_str"
}

# Export functions for use in other scripts
export -f now_iso8601
export -f time_ago
export -f validate_activity_name
export -f validate_activities_json
export -f ensure_activities_json
export -f get_current_activity
export -f set_current_activity
export -f get_activity_field
export -f activity_exists
export -f get_activity_path
export -f list_activities
export -f add_activity
export -f remove_activity
export -f update_activity_status
export -f get_current_branch
export -f get_main_branch
export -f worktree_exists
export -f print_success
export -f print_error
export -f print_warning
export -f print_info
export -f print_activity_table_header
export -f print_activity_row
