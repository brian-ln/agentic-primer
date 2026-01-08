#!/bin/bash

##############################################################################
# process-issue.sh
#
# Parse GitHub issue body and extract task metadata for @copilot
#
# Usage:
#   ./scripts/process-issue.sh <issue_number> <issue_body> <issue_title>
#
# Environment Variables:
#   GITHUB_REPOSITORY - Owner/repo (from GitHub Actions)
#   GITHUB_SERVER_URL - GitHub server URL
#
# Output:
#   JSON with extracted fields
#
##############################################################################

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Arguments
ISSUE_NUMBER="${1:-0}"
ISSUE_BODY="${2:-}"
ISSUE_TITLE="${3:-}"

# GitHub context (from GitHub Actions environment)
REPO="${GITHUB_REPOSITORY:-}"
GITHUB_URL="${GITHUB_SERVER_URL:-https://github.com}"

##############################################################################
# Functions
##############################################################################

log() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $*" >&2
}

error() {
    log "ERROR: $*"
    exit 1
}

warn() {
    log "WARN: $*"
}

##############################################################################
# Validation
##############################################################################

validate_input() {
    if [[ $ISSUE_NUMBER -eq 0 ]]; then
        error "Issue number required"
    fi

    if [[ -z "$ISSUE_BODY" ]]; then
        error "Issue body required"
    fi

    if [[ -z "$ISSUE_TITLE" ]]; then
        error "Issue title required"
    fi
}

##############################################################################
# Parsing Functions
##############################################################################

# Extract field from issue body
# Handles both YAML-style and Markdown headers
extract_field() {
    local field_name="$1"
    local default="${2:-}"

    # Try YAML-style (field: value)
    local value
    value=$(echo "$ISSUE_BODY" | grep -i "^${field_name}:" | head -1 | cut -d':' -f2- | xargs || echo "")

    if [[ -z "$value" ]]; then
        # Try Markdown header style (## Field Name)
        value=$(echo "$ISSUE_BODY" | sed -n "/^##[[:space:]]*${field_name}/,/^##/p" | tail -n +2 | sed '$d' | xargs || echo "")
    fi

    if [[ -z "$value" ]]; then
        value="$default"
    fi

    echo "$value"
}

# Parse task description (everything before first ## marker)
parse_description() {
    echo "$ISSUE_BODY" | sed -n '1,/^##/p' | sed '$d' | xargs
}

# Validate that required fields are present
validate_required_fields() {
    local task_type="$1"
    local success_criteria="$2"
    local complexity="$3"

    if [[ -z "$task_type" ]]; then
        warn "Missing task-type field"
        return 1
    fi

    if [[ -z "$success_criteria" ]]; then
        warn "Missing success-criteria field"
        return 1
    fi

    if [[ -z "$complexity" ]]; then
        warn "Missing complexity field"
        return 1
    fi

    return 0
}

# Sanitize field value for JSON output
json_escape() {
    local str="$1"
    # Escape quotes and newlines
    str="${str//\\/\\\\}"
    str="${str//\"/\\\"}"
    str="${str//$'\n'/\\n}"
    echo "$str"
}

##############################################################################
# Main Processing
##############################################################################

main() {
    log "Processing issue #${ISSUE_NUMBER}"

    # Validate inputs
    validate_input

    # Extract fields
    local task_description
    task_description=$(parse_description)

    local task_type
    task_type=$(extract_field "Task Type" "feature")

    local success_criteria
    success_criteria=$(extract_field "Success Criteria" "")

    local complexity
    complexity=$(extract_field "Complexity" "3")

    local priority
    priority=$(extract_field "Priority" "Medium")

    local acceptance_notes
    acceptance_notes=$(extract_field "Additional Notes" "")

    local reference_links
    reference_links=$(extract_field "Reference Links" "")

    # Validate required fields
    if ! validate_required_fields "$task_type" "$success_criteria" "$complexity"; then
        log "Warning: Some required fields are missing, proceeding anyway"
    fi

    # Generate branch name
    local branch_name="copilot/task-${ISSUE_NUMBER}"

    # Count success criteria
    local criteria_count
    criteria_count=$(echo "$success_criteria" | grep -c "^\s*-\s*\[\s*\]" || echo "0")

    # Generate timestamp
    local timestamp
    timestamp=$(date -u +'%Y-%m-%dT%H:%M:%SZ')

    # Output as JSON
    cat <<EOF
{
  "issue_number": $ISSUE_NUMBER,
  "issue_title": "$(json_escape "$ISSUE_TITLE")",
  "issue_url": "${GITHUB_URL}/${REPO}/issues/${ISSUE_NUMBER}",
  "timestamp": "$timestamp",
  "task": {
    "description": "$(json_escape "$task_description")",
    "type": "$(json_escape "$task_type")",
    "complexity": "$(json_escape "$complexity")",
    "priority": "$(json_escape "$priority")",
    "success_criteria_count": $criteria_count,
    "success_criteria": "$(json_escape "$success_criteria")",
    "acceptance_notes": "$(json_escape "$acceptance_notes")",
    "reference_links": "$(json_escape "$reference_links")"
  },
  "workflow": {
    "branch_name": "$branch_name",
    "pr_title": "[${ISSUE_NUMBER}] ${ISSUE_TITLE}",
    "base_branch": "main"
  },
  "validation": {
    "required_fields_present": true,
    "title_valid": $(if [[ "$ISSUE_TITLE" =~ ^\[TASK\] ]]; then echo "true"; else echo "false"; fi),
    "complexity_valid": $(if [[ "$complexity" =~ ^[1-5]$ ]]; then echo "true"; else echo "false"; fi)
  }
}
EOF

    log "Issue #${ISSUE_NUMBER} processed successfully"
    log "Branch: $branch_name"
    log "Success criteria: $criteria_count items"
}

##############################################################################
# Execute
##############################################################################

main "$@"
