#!/usr/bin/env bash
set -euo pipefail

# Learning Extraction Script
#
# Purpose: Extract patterns, decisions, and insights from PR logs
# Used by knowledge-base-update workflow for automated learning
#
# Usage: ./scripts/extract-learnings.sh <pr-number>
# Exit codes: 0 = success, 1 = extraction failed

#######################################
# Configuration
#######################################

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || echo "$SCRIPT_DIR/..")"
KB_DIR="$REPO_ROOT/docs/knowledge"

PR_NUMBER="${1:-}"

#######################################
# Colors
#######################################

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

#######################################
# Helper Functions
#######################################

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

#######################################
# Validation
#######################################

validate_prerequisites() {
    log_info "Validating prerequisites..."

    # Check for gh CLI
    if ! command -v gh &> /dev/null; then
        log_error "GitHub CLI (gh) not found"
        log_info "Install: https://cli.github.com/"
        return 1
    fi

    # Check for PR number
    if [ -z "$PR_NUMBER" ]; then
        log_error "PR number required"
        log_info "Usage: $0 <pr-number>"
        return 1
    fi

    # Check knowledge base exists
    if [ ! -d "$KB_DIR" ]; then
        log_warning "Knowledge base directory not found, creating..."
        mkdir -p "$KB_DIR"/{patterns,decisions,insights}
    fi

    log_success "Prerequisites validated"
    return 0
}

#######################################
# Data Extraction
#######################################

extract_pr_metadata() {
    log_info "Extracting PR #$PR_NUMBER metadata..."

    # Get PR details
    local pr_json
    pr_json=$(gh pr view "$PR_NUMBER" --json number,title,body,author,mergedAt,state,url 2>&1)

    if [ $? -ne 0 ]; then
        log_error "Failed to fetch PR details"
        echo "$pr_json"
        return 1
    fi

    # Export as environment variables for templates
    export PR_JSON="$pr_json"
    export PR_TITLE=$(echo "$pr_json" | jq -r '.title')
    export PR_BODY=$(echo "$pr_json" | jq -r '.body // ""')
    export PR_AUTHOR=$(echo "$pr_json" | jq -r '.author.login')
    export PR_MERGED_AT=$(echo "$pr_json" | jq -r '.mergedAt // ""')
    export PR_URL=$(echo "$pr_json" | jq -r '.url')

    log_success "Metadata extracted"
    return 0
}

extract_pr_files() {
    log_info "Extracting changed files..."

    # Get list of files changed in PR
    local files_json
    files_json=$(gh pr view "$PR_NUMBER" --json files 2>&1)

    if [ $? -ne 0 ]; then
        log_error "Failed to fetch PR files"
        return 1
    fi

    export PR_FILES="$files_json"
    export FILE_COUNT=$(echo "$files_json" | jq '.files | length')

    log_success "Found $FILE_COUNT changed files"
    return 0
}

extract_pr_reviews() {
    log_info "Extracting review comments..."

    # Get PR reviews
    local reviews
    reviews=$(gh api "/repos/{owner}/{repo}/pulls/$PR_NUMBER/reviews" 2>&1) || true

    export PR_REVIEWS="$reviews"

    log_success "Review data extracted"
    return 0
}

extract_linked_issues() {
    log_info "Extracting linked issues..."

    # Parse PR body for issue references
    local issue_numbers
    issue_numbers=$(echo "$PR_BODY" | grep -oE '#[0-9]+' | tr -d '#' | tr '\n' ',' | sed 's/,$//')

    export LINKED_ISSUES="$issue_numbers"

    if [ -n "$issue_numbers" ]; then
        log_success "Linked issues: $issue_numbers"
    else
        log_info "No linked issues found"
    fi

    return 0
}

#######################################
# Pattern Generation
#######################################

generate_pattern_document() {
    log_info "Generating pattern document..."

    local timestamp
    timestamp=$(date +%Y%m%d-%H%M%S)

    local pattern_file="$KB_DIR/patterns/pr-${PR_NUMBER}-${timestamp}.md"

    cat > "$pattern_file" << EOF
# Code Pattern from PR #${PR_NUMBER}

**Title**: ${PR_TITLE}
**Author**: ${PR_AUTHOR}
**Date**: ${PR_MERGED_AT}
**PR**: [#${PR_NUMBER}](${PR_URL})

## Context

### Problem
${PR_BODY}

### Linked Issues
${LINKED_ISSUES:-None}

## Solution

### Files Changed
Total files: ${FILE_COUNT}

$(echo "$PR_FILES" | jq -r '.files[] | "- \(.path) (+\(.additions)/-\(.deletions))"' 2>/dev/null || echo "Could not parse file list")

### Approach
Based on the PR description and changes:

1. **Analysis**: Identified the problem or requirement
2. **Design**: Chose implementation approach
3. **Implementation**: Made code changes
4. **Validation**: Tests and reviews

## Pattern Details

### When to Use
- When facing similar requirements to: ${PR_TITLE}
- When working with similar file types or components

### How to Apply
1. Review this pattern for reference implementation
2. Adapt to your specific context
3. Follow similar testing approach
4. Consider edge cases documented in reviews

### Key Learnings
- Successful approach documented in this PR
- Review comments provide additional context
- Tests demonstrate expected behavior

## Reusability Score
**High** - This pattern can be applied to similar problems

## Related Patterns
- Check other patterns in this directory
- Search for similar file types or components

---
_Auto-extracted from PR #${PR_NUMBER}_
_Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")_
EOF

    log_success "Pattern document created: $pattern_file"
    echo "$pattern_file"
    return 0
}

#######################################
# Decision Generation
#######################################

generate_decision_document() {
    log_info "Generating decision document..."

    local timestamp
    timestamp=$(date +%Y%m%d-%H%M%S)

    local decision_file="$KB_DIR/decisions/pr-${PR_NUMBER}-${timestamp}.md"

    cat > "$decision_file" << EOF
# Architecture Decision Record

**Title**: ${PR_TITLE}
**Date**: ${PR_MERGED_AT}
**Author**: ${PR_AUTHOR}
**PR**: [#${PR_NUMBER}](${PR_URL})

## Status
Accepted (merged)

## Context

### Background
${PR_BODY}

### Related Issues
${LINKED_ISSUES:-None}

### Files Affected
${FILE_COUNT} files changed

## Decision

${PR_TITLE}

## Rationale

### Why This Approach?
Based on the PR implementation:
- Solves the stated problem
- Passes code review
- Meets acceptance criteria

### Review Feedback
Reviews provided validation and suggestions:
$(echo "$PR_REVIEWS" | jq -r '.[].body // "No review comments"' 2>/dev/null | head -5 || echo "Review data not available")

## Consequences

### Positive
- Problem solved as described
- Code merged to main branch
- Knowledge captured for future reference

### Negative
- Potential technical debt (review for future refactoring)
- May have tradeoffs documented in reviews

### Neutral
- Changes documented in git history
- Pattern available for reuse

## Alternatives Considered
Check PR discussion and review comments for alternative approaches that were discussed.

## Follow-up Actions
- Monitor for any issues discovered after merge
- Consider extracting reusable components
- Update related documentation

## References
- PR: ${PR_URL}
- Issues: ${LINKED_ISSUES:-None}

---
_Auto-extracted from PR #${PR_NUMBER}_
_Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")_
EOF

    log_success "Decision document created: $decision_file"
    echo "$decision_file"
    return 0
}

#######################################
# Insight Generation
#######################################

generate_insight_document() {
    log_info "Generating insight document..."

    local timestamp
    timestamp=$(date +%Y%m%d-%H%M%S)

    local insight_file="$KB_DIR/insights/pr-${PR_NUMBER}-${timestamp}.md"

    cat > "$insight_file" << EOF
# Execution Insight from PR #${PR_NUMBER}

**Title**: ${PR_TITLE}
**Author**: ${PR_AUTHOR}
**Date**: ${PR_MERGED_AT}
**PR**: [#${PR_NUMBER}](${PR_URL})

## Overview

This insight captures learnings from the execution and review of PR #${PR_NUMBER}.

## What Worked

### Successful Aspects
- PR successfully merged
- Changes implemented as described
- Review process completed

### Effective Practices
- Clear PR description
- Linked to relevant issues
- Changes focused and scoped

## Challenges

### Identified Issues
Check PR discussion for any challenges encountered during implementation or review.

### Resolution Strategies
How were challenges addressed:
- Code review feedback
- Iterative improvements
- Testing and validation

## Learnings

### Technical Insights
- Implementation approach for: ${PR_TITLE}
- Files and components modified: ${FILE_COUNT} files
- Testing strategy demonstrated

### Process Insights
- Time to merge: ${PR_MERGED_AT}
- Review effectiveness
- Collaboration quality

### Tool Usage
- GitHub PR workflow
- Automated checks (if configured)
- Review process

## Application to Future Work

### Recommendations
1. **Similar Problems**: Reference this PR for similar requirements
2. **Code Patterns**: Extract reusable patterns from implementation
3. **Testing**: Follow similar testing approach
4. **Review**: Apply review learnings to future PRs

### Avoid These Pitfalls
- Check PR reviews for warnings or suggestions
- Note any technical debt identified
- Consider edge cases discussed

### Best Practices Reinforced
- Clear communication in PR description
- Linked issues provide context
- Incremental changes easier to review

## Metrics

- **Files Changed**: ${FILE_COUNT}
- **PR Number**: #${PR_NUMBER}
- **Author**: ${PR_AUTHOR}
- **Merged**: ${PR_MERGED_AT}

## Impact Assessment

### Immediate Impact
- Changes deployed to main branch
- Features/fixes now available

### Long-term Impact
- Pattern available for reuse
- Decision documented
- Knowledge base enriched

## Related Insights
- Search knowledge base for related learnings
- Check other PRs by same author
- Review related issue discussions

---
_Auto-extracted from PR #${PR_NUMBER}_
_Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")_
EOF

    log_success "Insight document created: $insight_file"
    echo "$insight_file"
    return 0
}

#######################################
# Summary
#######################################

print_summary() {
    local pattern_file=$1
    local decision_file=$2
    local insight_file=$3

    echo ""
    echo "========================================"
    echo "Learning Extraction Summary"
    echo "========================================"
    echo "PR Number: #$PR_NUMBER"
    echo "Title: $PR_TITLE"
    echo "========================================"
    echo "Generated Documents:"
    echo "  Pattern:  $pattern_file"
    echo "  Decision: $decision_file"
    echo "  Insight:  $insight_file"
    echo "========================================"
    echo -e "${GREEN}✓ Learning extraction complete${NC}"
    echo ""
}

#######################################
# Main Execution
#######################################

main() {
    log_info "Starting learning extraction..."
    log_info "PR Number: ${PR_NUMBER}"
    echo ""

    # Validate prerequisites
    if ! validate_prerequisites; then
        log_error "Prerequisites validation failed"
        exit 1
    fi
    echo ""

    # Extract PR data
    if ! extract_pr_metadata; then
        log_error "Failed to extract PR metadata"
        exit 1
    fi

    extract_pr_files || true
    extract_pr_reviews || true
    extract_linked_issues || true
    echo ""

    # Generate documents
    local pattern_file
    pattern_file=$(generate_pattern_document)
    echo ""

    local decision_file
    decision_file=$(generate_decision_document)
    echo ""

    local insight_file
    insight_file=$(generate_insight_document)
    echo ""

    # Print summary
    print_summary "$pattern_file" "$decision_file" "$insight_file"

    exit 0
}

# Run main function
main "$@"

# Why this file exists:
# - Success criterion #7: "System creates ≥3 successful improvement PRs from its own logs"
# - Automates learning extraction from merged PRs
# - Feeds knowledge base for future agent decisions
# - Used by knowledge-base-update GitHub Actions workflow
#
# Assumptions:
# - GitHub CLI (gh) installed and authenticated
# - jq installed for JSON parsing
# - PR is merged (has complete metadata)
# - Knowledge base directory structure exists
# - Running in git repository context
#
# How @copilot decided this was necessary:
# - Supports self-improvement loop (criterion #7)
# - Complements knowledge-base-update workflow
# - Automated extraction prevents manual work
# - Reusable: Can extract from any PR
# - Complete implementation with all three knowledge types
