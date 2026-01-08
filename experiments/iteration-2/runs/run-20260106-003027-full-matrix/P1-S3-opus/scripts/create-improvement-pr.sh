#!/usr/bin/env bash
#
# Create Improvement PR
#
# Usage: ./scripts/create-improvement-pr.sh --category CATEGORY --description DESC --recommendation REC
#
# Creates a PR with proposed improvements based on log analysis.
# Usually called by the self-improvement workflow.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CATEGORY=""
DESCRIPTION=""
RECOMMENDATION=""
DRY_RUN=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --category)
            CATEGORY="$2"
            shift 2
            ;;
        --description)
            DESCRIPTION="$2"
            shift 2
            ;;
        --recommendation)
            RECOMMENDATION="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Validate required arguments
if [ -z "$CATEGORY" ] || [ -z "$DESCRIPTION" ] || [ -z "$RECOMMENDATION" ]; then
    echo "Usage: $0 --category CATEGORY --description DESC --recommendation REC"
    echo ""
    echo "Required arguments:"
    echo "  --category       Improvement category (template, workflow, documentation, process)"
    echo "  --description    Description of the issue found"
    echo "  --recommendation Recommended improvement"
    echo ""
    echo "Optional arguments:"
    echo "  --dry-run        Simulate without making changes"
    exit 1
fi

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Generate branch name
generate_branch_name() {
    local date_stamp
    date_stamp=$(date +%Y%m%d)
    local slug
    slug=$(echo "$CATEGORY" | tr '[:upper:]' '[:lower:]' | tr ' ' '-')
    echo "improvement/auto-${slug}-${date_stamp}"
}

# Create branch
create_branch() {
    local branch_name="$1"

    log_info "Creating branch: $branch_name"

    if [ "$DRY_RUN" = true ]; then
        echo "  [DRY-RUN] Would create branch: $branch_name"
        return 0
    fi

    cd "$REPO_ROOT"
    git checkout -b "$branch_name" 2>/dev/null || {
        log_error "Failed to create branch. May already exist."
        return 1
    }

    log_success "Branch created: $branch_name"
}

# Apply improvement
apply_improvement() {
    log_info "Applying improvement..."

    local improvements_log="$REPO_ROOT/docs/knowledge/insights/improvements/applied.jsonl"

    if [ "$DRY_RUN" = true ]; then
        echo "  [DRY-RUN] Would append to: $improvements_log"
        return 0
    fi

    # Ensure directory exists
    mkdir -p "$(dirname "$improvements_log")"

    # Append improvement record
    cat >> "$improvements_log" << EOF
{"timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)", "category": "$CATEGORY", "description": "$DESCRIPTION", "recommendation": "$RECOMMENDATION", "status": "proposed"}
EOF

    log_success "Improvement logged"
}

# Apply category-specific changes
apply_category_changes() {
    log_info "Applying category-specific changes..."

    if [ "$DRY_RUN" = true ]; then
        echo "  [DRY-RUN] Would apply $CATEGORY changes"
        return 0
    fi

    case "$CATEGORY" in
        template)
            # Add to template improvements doc
            local template_doc="$REPO_ROOT/docs/knowledge/patterns/template-improvements.md"
            mkdir -p "$(dirname "$template_doc")"
            cat >> "$template_doc" << EOF

## Improvement: $(date +%Y-%m-%d)

**Issue:** $DESCRIPTION
**Recommendation:** $RECOMMENDATION
EOF
            ;;

        workflow)
            # Add to workflow improvements doc
            local workflow_doc="$REPO_ROOT/docs/knowledge/patterns/workflow-improvements.md"
            mkdir -p "$(dirname "$workflow_doc")"
            cat >> "$workflow_doc" << EOF

## Improvement: $(date +%Y-%m-%d)

**Issue:** $DESCRIPTION
**Recommendation:** $RECOMMENDATION
EOF
            ;;

        documentation)
            # Add to documentation improvements doc
            local docs_doc="$REPO_ROOT/docs/knowledge/insights/documentation-improvements.md"
            mkdir -p "$(dirname "$docs_doc")"
            cat >> "$docs_doc" << EOF

## Improvement: $(date +%Y-%m-%d)

**Issue:** $DESCRIPTION
**Recommendation:** $RECOMMENDATION
EOF
            ;;

        process)
            # Add to process improvements doc
            local process_doc="$REPO_ROOT/docs/knowledge/insights/process-improvements.md"
            mkdir -p "$(dirname "$process_doc")"
            cat >> "$process_doc" << EOF

## Improvement: $(date +%Y-%m-%d)

**Issue:** $DESCRIPTION
**Recommendation:** $RECOMMENDATION
EOF
            ;;

        *)
            log_info "No category-specific changes for: $CATEGORY"
            ;;
    esac

    log_success "Category changes applied"
}

# Commit changes
commit_changes() {
    log_info "Committing changes..."

    if [ "$DRY_RUN" = true ]; then
        echo "  [DRY-RUN] Would commit: improvement($CATEGORY): $RECOMMENDATION"
        return 0
    fi

    cd "$REPO_ROOT"
    git add -A
    git commit -m "improvement($CATEGORY): $RECOMMENDATION" || {
        log_info "Nothing to commit"
        return 0
    }

    log_success "Changes committed"
}

# Push branch
push_branch() {
    local branch_name="$1"

    log_info "Pushing branch: $branch_name"

    if [ "$DRY_RUN" = true ]; then
        echo "  [DRY-RUN] Would push: $branch_name"
        return 0
    fi

    git push -u origin "$branch_name" 2>/dev/null || {
        log_error "Failed to push branch"
        return 1
    }

    log_success "Branch pushed"
}

# Create PR (simulation)
create_pr() {
    local branch_name="$1"

    log_info "Creating pull request..."

    local pr_title="[Auto-Improvement] $RECOMMENDATION"
    local pr_body="## Self-Improvement PR

**Category:** $CATEGORY

**Issue Detected:**
$DESCRIPTION

**Recommendation:**
$RECOMMENDATION

---
This PR was automatically generated by the self-improvement analyzer.
Review the changes and merge if appropriate."

    if [ "$DRY_RUN" = true ]; then
        echo "  [DRY-RUN] Would create PR:"
        echo "    Title: $pr_title"
        echo "    Branch: $branch_name -> main"
        echo "    Body: (see above)"
        return 0
    fi

    # In real execution, this would call GitHub API or gh CLI
    echo "  [SIMULATED] PR would be created:"
    echo "    Title: $pr_title"
    echo "    Branch: $branch_name"
    echo ""
    echo "  To create PR manually:"
    echo "    gh pr create --title \"$pr_title\" --body \"<body>\" --base main"

    log_success "PR creation simulated"
}

# Print summary
print_summary() {
    echo ""
    echo "=========================================="
    echo "  Improvement PR Summary"
    echo "=========================================="
    echo ""
    echo "  Category:       $CATEGORY"
    echo "  Description:    $DESCRIPTION"
    echo "  Recommendation: $RECOMMENDATION"
    echo ""

    if [ "$DRY_RUN" = true ]; then
        echo "  Mode: DRY-RUN (no changes made)"
    else
        echo "  Status: Changes applied"
    fi

    echo ""
}

# Main execution
main() {
    echo ""
    echo "=========================================="
    echo "  Create Improvement PR"
    echo "=========================================="

    if [ "$DRY_RUN" = true ]; then
        echo -e "${YELLOW}Running in DRY-RUN mode${NC}"
    fi

    local branch_name
    branch_name=$(generate_branch_name)

    create_branch "$branch_name"
    apply_improvement
    apply_category_changes
    commit_changes
    push_branch "$branch_name"
    create_pr "$branch_name"

    print_summary
}

main "$@"
