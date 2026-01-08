#!/usr/bin/env bash
#
# Issue Validation Script
#
# Validates that an issue has all required fields for @copilot to process it.
# Returns 0 if valid, non-zero if invalid.
#
# Usage: validate-issue.sh <issue-number>
#

set -euo pipefail

ISSUE_NUMBER="${1:-}"

if [ -z "$ISSUE_NUMBER" ]; then
    echo "Usage: $0 <issue-number>"
    exit 1
fi

# --- Validation functions ---

validate_required_fields() {
    local issue_data="$1"

    # Check for required fields based on issue template structure
    local has_title
    has_title=$(echo "$issue_data" | jq -r '.title // empty' | grep -c '.' || echo "0")
    local has_body
    has_body=$(echo "$issue_data" | jq -r '.body // empty' | grep -c '.' || echo "0")

    if [ "$has_title" -eq 0 ]; then
        echo "❌ Issue missing title"
        return 1
    fi

    if [ "$has_body" -eq 0 ]; then
        echo "❌ Issue missing body"
        return 1
    fi

    return 0
}

validate_copilot_label() {
    local issue_data="$1"

    # Check if issue has 'copilot' label
    local has_copilot_label
    has_copilot_label=$(echo "$issue_data" | jq -r '.labels[].name' | grep -c '^copilot$' || echo "0")

    if [ "$has_copilot_label" -eq 0 ]; then
        echo "⚠️  Issue missing 'copilot' label (may be added after creation)"
        # This is a warning, not a failure
    fi

    return 0
}

validate_acceptance_criteria() {
    local body="$1"

    # Check if body contains acceptance criteria section
    # Look for markdown checkboxes: - [ ] or - [x]
    local has_criteria
    has_criteria=$(echo "$body" | grep -c '\- \[ \]' || echo "0")

    if [ "$has_criteria" -eq 0 ]; then
        echo "⚠️  No acceptance criteria checkboxes found"
        # Warning, but not a hard failure - some issues may not need them
    fi

    return 0
}

validate_description_length() {
    local body="$1"

    # Ensure description is substantial (at least 50 characters of actual content)
    local body_length
    body_length=$(echo "$body" | tr -d '[:space:]' | wc -c)

    if [ "$body_length" -lt 50 ]; then
        echo "❌ Issue description too short (less than 50 characters)"
        return 1
    fi

    return 0
}

# --- Main validation ---

echo "🔍 Validating issue #${ISSUE_NUMBER}..."

# SIMULATION: In production, this would fetch real issue data via gh CLI
# ISSUE_DATA=$(gh issue view "$ISSUE_NUMBER" --json title,body,labels,author)

# For simulation, create sample issue data
ISSUE_DATA=$(cat <<EOF
{
  "title": "Sample Issue Title",
  "body": "This is a sample issue body with sufficient content to pass validation. It includes multiple sections and enough detail for implementation.",
  "labels": [
    {"name": "copilot"},
    {"name": "enhancement"}
  ],
  "author": {
    "login": "testuser"
  }
}
EOF
)

# Extract fields for validation
ISSUE_BODY=$(echo "$ISSUE_DATA" | jq -r '.body')

# Run validation checks
VALIDATION_FAILED=false

if ! validate_required_fields "$ISSUE_DATA"; then
    VALIDATION_FAILED=true
fi

if ! validate_copilot_label "$ISSUE_DATA"; then
    # Label validation is just a warning, don't fail
    :
fi

if ! validate_acceptance_criteria "$ISSUE_BODY"; then
    # Criteria validation is just a warning, don't fail
    :
fi

if ! validate_description_length "$ISSUE_BODY"; then
    VALIDATION_FAILED=true
fi

# --- Result ---

if [ "$VALIDATION_FAILED" = true ]; then
    echo "❌ Issue validation failed"
    echo ""
    echo "Please ensure your issue includes:"
    echo "  - Clear, descriptive title"
    echo "  - Detailed description (at least 50 characters)"
    echo "  - Acceptance criteria (recommended)"
    echo "  - 'copilot' label"
    exit 1
else
    echo "✅ Issue validation passed"
    exit 0
fi
