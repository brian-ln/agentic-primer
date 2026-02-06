#!/usr/bin/env bash
#
# extract-learnings.sh - Extract patterns and insights from agent execution logs
#
# Usage: ./scripts/extract-learnings.sh <pr-number>
#
# This script analyzes a merged PR and extracts:
#   - Code patterns that can be reused
#   - Architecture decisions and rationale
#   - Insights from review comments and discussions
#   - Common issues and their solutions
#
# The extracted learnings are stored in docs/knowledge/ for future reference
# by AI agents processing similar tasks.
#
# Exit codes:
#   0 - Learnings extracted successfully
#   1 - Invalid arguments or PR not found
#   2 - Extraction failed

set -euo pipefail

# Configuration
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Logging
log_info() { echo -e "${BLUE}ℹ${NC} $*"; }
log_success() { echo -e "${GREEN}✓${NC} $*"; }
log_warning() { echo -e "${YELLOW}⚠${NC} $*"; }
log_error() { echo -e "${RED}✗${NC} $*"; }

# Usage
usage() {
    cat << EOF
Usage: $0 <pr-number>

Extract learnings from a merged PR and add to knowledge base.

Arguments:
    pr-number    The number of the merged PR to analyze

Examples:
    $0 123       Extract learnings from PR #123

Environment Variables:
    GITHUB_TOKEN    GitHub API token (optional, uses gh CLI if available)

EOF
    exit 1
}

# Check prerequisites
check_prerequisites() {
    if ! command -v git &> /dev/null; then
        log_error "git is required but not installed"
        return 1
    fi

    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        log_error "Not in a git repository"
        return 1
    fi

    # Check for gh CLI (preferred) or GITHUB_TOKEN
    if ! command -v gh &> /dev/null && [ -z "${GITHUB_TOKEN:-}" ]; then
        log_warning "Neither gh CLI nor GITHUB_TOKEN found"
        log_warning "Some features may be limited"
    fi

    return 0
}

# Get PR details using gh CLI or git
get_pr_details() {
    local pr_number=$1

    log_info "Fetching details for PR #${pr_number}..."

    # Try using gh CLI first
    if command -v gh &> /dev/null; then
        local pr_json
        if pr_json=$(gh pr view "$pr_number" --json title,body,files,labels,reviews,comments 2>/dev/null); then
            echo "$pr_json"
            return 0
        else
            log_warning "Failed to fetch PR via gh CLI, falling back to git"
        fi
    fi

    # Fallback: use git to get basic info
    log_info "Using git to extract PR information..."

    # Find merge commit
    local merge_commit
    merge_commit=$(git log --oneline --merges --grep="Merge pull request #${pr_number}" -1 --format="%H" 2>/dev/null || true)

    if [ -z "$merge_commit" ]; then
        log_error "Could not find merge commit for PR #${pr_number}"
        return 1
    fi

    # Extract basic info from git
    local title
    title=$(git log -1 --format="%s" "$merge_commit")

    local files_changed
    files_changed=$(git diff --name-only "${merge_commit}^" "$merge_commit")

    # Create minimal JSON structure
    cat << EOF
{
    "number": $pr_number,
    "title": "$title",
    "body": "",
    "files": $(echo "$files_changed" | jq -R -s -c 'split("\n")[:-1] | map({filename: .})'),
    "labels": [],
    "reviews": [],
    "comments": []
}
EOF
}

# Categorize PR based on files and labels
categorize_pr() {
    local pr_data=$1

    local labels
    labels=$(echo "$pr_data" | jq -r '.labels[]?.name' 2>/dev/null || echo "")

    local files
    files=$(echo "$pr_data" | jq -r '.files[]?.filename' 2>/dev/null || echo "")

    # Determine category
    local category="general"

    if echo "$labels" | grep -qi "bug"; then
        category="bug-fixes"
    elif echo "$labels" | grep -qi "feature"; then
        category="features"
    elif echo "$labels" | grep -qi "refactor"; then
        category="refactoring"
    elif echo "$labels" | grep -qi "docs"; then
        category="documentation"
    elif echo "$labels" | grep -qi "ai-generated"; then
        category="ai-generated"
    elif echo "$files" | grep -q "\.github/workflows"; then
        category="ci-cd"
    elif echo "$files" | grep -q "test"; then
        category="testing"
    fi

    echo "$category"
}

# Extract code patterns
extract_patterns() {
    local pr_number=$1
    local pr_data=$2

    log_info "Extracting code patterns..."

    local patterns=()

    # Check for common patterns in file changes
    local files
    files=$(echo "$pr_data" | jq -r '.files[]?.filename' 2>/dev/null || echo "")

    # Pattern: Workflow additions
    if echo "$files" | grep -q "\.github/workflows"; then
        patterns+=("GitHub Actions workflow pattern")
    fi

    # Pattern: Test additions
    if echo "$files" | grep -q "test\|spec"; then
        patterns+=("Test coverage pattern")
    fi

    # Pattern: Documentation updates
    if echo "$files" | grep -q "README\|\.md$"; then
        patterns+=("Documentation update pattern")
    fi

    # Pattern: Configuration changes
    if echo "$files" | grep -q "\.yml$\|\.yaml$\|\.json$"; then
        patterns+=("Configuration management pattern")
    fi

    # Pattern: Script additions
    if echo "$files" | grep -q "\.sh$\|scripts/"; then
        patterns+=("Automation script pattern")
    fi

    printf '%s\n' "${patterns[@]}"
}

# Create insight document
create_insight_document() {
    local pr_number=$1
    local pr_data=$2
    local category=$3

    local timestamp
    timestamp=$(date +%Y%m%d-%H%M%S)

    local insight_file="${REPO_ROOT}/docs/knowledge/insights/pr-${pr_number}-${timestamp}.md"

    log_info "Creating insight document: $insight_file"

    local title
    title=$(echo "$pr_data" | jq -r '.title' 2>/dev/null || echo "PR #${pr_number}")

    local body
    body=$(echo "$pr_data" | jq -r '.body // ""' 2>/dev/null)

    local files_count
    files_count=$(echo "$pr_data" | jq -r '.files | length' 2>/dev/null || echo "0")

    local patterns
    patterns=$(extract_patterns "$pr_number" "$pr_data")

    # Create the insight document
    cat > "$insight_file" << EOF
# Learning from PR #${pr_number}

**Date**: $(date +%Y-%m-%d)
**Title**: ${title}
**Category**: ${category}

## Summary

This PR was merged and analyzed for reusable patterns and learnings.

## Changes Made

- **Files Changed**: ${files_count}
- **Category**: ${category}

## Patterns Identified

$(echo "$patterns" | sed 's/^/- /')

## Key Takeaways

${body}

## Implementation Details

<!-- Add specific implementation notes here -->

## Reusability

This learning can be applied to:
- Similar issues in category: ${category}
- Future AI agent tasks with related requirements
- Code review guidelines

## Best Practices

<!-- Extracted from review comments -->

## Related

- Original PR: #${pr_number}
- Merge date: $(date +%Y-%m-%d)

---
*Auto-generated by extract-learnings.sh*
*Last updated: $(date -u +%Y-%m-%dT%H:%M:%SZ)*
EOF

    log_success "Created insight document"
    echo "$insight_file"
}

# Update knowledge base index
update_knowledge_base_index() {
    local pr_number=$1
    local insight_file=$2
    local category=$3

    local index_file="${REPO_ROOT}/docs/knowledge/README.md"
    local relative_insight="${insight_file#$REPO_ROOT/}"

    log_info "Updating knowledge base index..."

    # Create index if it doesn't exist
    if [ ! -f "$index_file" ]; then
        cat > "$index_file" << 'EOF'
# Knowledge Base

This directory contains accumulated learnings from AI agent executions and code reviews.

## Structure

- **patterns/** - Reusable code patterns and solutions
- **decisions/** - Architecture decisions and rationale
- **insights/** - Learnings from merged PRs and agent logs

## Recent Updates

EOF
    fi

    # Add entry to recent updates
    local entry="- [PR #${pr_number}](${relative_insight}) - Category: ${category} ($(date +%Y-%m-%d))"

    # Check if entry already exists
    if grep -q "PR #${pr_number}" "$index_file"; then
        log_info "Entry already exists in index"
    else
        # Insert after "## Recent Updates" line
        if grep -q "## Recent Updates" "$index_file"; then
            sed -i.bak "/## Recent Updates/a\\
$entry
" "$index_file"
            rm -f "${index_file}.bak"
            log_success "Updated knowledge base index"
        else
            echo "" >> "$index_file"
            echo "## Recent Updates" >> "$index_file"
            echo "" >> "$index_file"
            echo "$entry" >> "$index_file"
            log_success "Added Recent Updates section and entry"
        fi
    fi
}

# Update category index
update_category_index() {
    local category=$1
    local pr_number=$2
    local insight_file=$3

    local category_index="${REPO_ROOT}/docs/knowledge/insights/README.md"

    log_info "Updating insights category index..."

    if [ ! -f "$category_index" ]; then
        cat > "$category_index" << 'EOF'
# Insights

Learnings extracted from merged PRs and agent executions.

## Index

EOF
    fi

    local relative_insight="${insight_file#$REPO_ROOT/docs/knowledge/insights/}"
    local entry="- [PR #${pr_number}]($relative_insight) - ${category}"

    if ! grep -q "PR #${pr_number}" "$category_index"; then
        echo "$entry" >> "$category_index"
        log_success "Updated category index"
    fi
}

# Main execution
main() {
    if [ $# -ne 1 ]; then
        usage
    fi

    local pr_number=$1

    # Validate PR number
    if ! [[ "$pr_number" =~ ^[0-9]+$ ]]; then
        log_error "Invalid PR number: $pr_number"
        usage
    fi

    echo ""
    log_info "Extracting learnings from PR #${pr_number}..."
    echo ""

    # Check prerequisites
    check_prerequisites || exit 1

    # Get PR details
    local pr_data
    pr_data=$(get_pr_details "$pr_number") || exit 1

    # Categorize PR
    local category
    category=$(categorize_pr "$pr_data")
    log_info "Category: $category"

    # Create knowledge base directories if needed
    mkdir -p "${REPO_ROOT}/docs/knowledge/insights"
    mkdir -p "${REPO_ROOT}/docs/knowledge/patterns"
    mkdir -p "${REPO_ROOT}/docs/knowledge/decisions"

    # Create insight document
    local insight_file
    insight_file=$(create_insight_document "$pr_number" "$pr_data" "$category")

    # Update indices
    update_knowledge_base_index "$pr_number" "$insight_file" "$category"
    update_category_index "$category" "$pr_number" "$insight_file"

    echo ""
    log_success "Learnings extracted successfully!"
    echo ""
    log_info "Insight document: ${insight_file#$REPO_ROOT/}"
    log_info "Category: $category"
    echo ""
    log_info "Next steps:"
    echo "  1. Review and enhance the generated insight document"
    echo "  2. Add specific code examples if applicable"
    echo "  3. Commit changes: git add docs/knowledge/ && git commit -m 'docs: add learnings from PR #${pr_number}'"
    echo ""
}

# Run main
main "$@"
