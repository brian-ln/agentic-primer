#!/usr/bin/env bash
#
# Pattern extraction script for knowledge base
#
# This script analyzes merged PRs, git commits, and code changes to extract
# reusable patterns, insights, and architectural decisions. Extracted knowledge
# is stored in the knowledge base for future reference.
#
# Usage: ./scripts/extract-patterns.sh [--pr NUMBER] [--since DATE] [--all]
#
# Options:
#   --pr NUMBER    Extract patterns from specific PR
#   --since DATE   Extract patterns from commits since date (YYYY-MM-DD)
#   --all          Extract patterns from all merged PRs
#
# Exit codes:
#   0 - Success
#   1 - Error during extraction
#   2 - Missing required tool

set -euo pipefail

# Parse command line arguments
PR_NUMBER=""
SINCE_DATE=""
EXTRACT_ALL=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --pr)
            PR_NUMBER="$2"
            shift 2
            ;;
        --since)
            SINCE_DATE="$2"
            shift 2
            ;;
        --all)
            EXTRACT_ALL=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--pr NUMBER] [--since DATE] [--all]"
            exit 1
            ;;
    esac
done

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[⚠]${NC} $*"
}

log_error() {
    echo -e "${RED}[✗]${NC} $*" >&2
}

# Check if required tools are available
check_tools() {
    if ! command -v git &> /dev/null; then
        log_error "git is not installed"
        return 2
    fi

    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        log_error "Not in a git repository"
        return 2
    fi

    return 0
}

# Create knowledge base directories if they don't exist
ensure_knowledge_dirs() {
    mkdir -p docs/knowledge/patterns
    mkdir -p docs/knowledge/decisions
    mkdir -p docs/knowledge/insights
}

# Extract patterns from a specific PR
extract_from_pr() {
    local pr_number=$1

    log_info "Extracting patterns from PR #$pr_number..."

    # Check if gh CLI is available
    if ! command -v gh &> /dev/null; then
        log_warning "GitHub CLI (gh) not installed, using git log instead"
        extract_from_commits "1"
        return 0
    fi

    # Get PR details
    local pr_data
    pr_data=$(gh pr view "$pr_number" --json title,author,body,files,additions,deletions,mergedAt,labels 2>/dev/null || echo "")

    if [ -z "$pr_data" ]; then
        log_error "Could not fetch PR #$pr_number"
        return 1
    fi

    local pr_title
    local pr_author
    local pr_merged_at
    pr_title=$(echo "$pr_data" | jq -r '.title')
    pr_author=$(echo "$pr_data" | jq -r '.author.login')
    pr_merged_at=$(echo "$pr_data" | jq -r '.mergedAt')

    # Create pattern file
    local pattern_file="docs/knowledge/patterns/pr-${pr_number}-patterns.md"
    cat > "$pattern_file" <<EOF
# Patterns from PR #${pr_number}

**Title**: ${pr_title}
**Author**: ${pr_author}
**Merged**: ${pr_merged_at}
**PR**: https://github.com/$(git remote get-url origin | sed 's/.*github.com[:/]\(.*\)\.git/\1/')/pull/${pr_number}

## Summary

$(echo "$pr_data" | jq -r '.body // "No description provided"')

## Files Changed

$(echo "$pr_data" | jq -r '.files[].path' | while read -r file; do echo "- \`$file\`"; done)

## Code Patterns

EOF

    # Analyze file types and extract patterns
    echo "$pr_data" | jq -r '.files[].path' | while read -r file; do
        local extension="${file##*.}"

        case "$extension" in
            js|ts|jsx|tsx)
                echo "### JavaScript/TypeScript Pattern: \`$file\`" >> "$pattern_file"
                echo "" >> "$pattern_file"
                echo "React/Node.js code modification. Consider extracting reusable components or utilities." >> "$pattern_file"
                echo "" >> "$pattern_file"
                ;;
            py)
                echo "### Python Pattern: \`$file\`" >> "$pattern_file"
                echo "" >> "$pattern_file"
                echo "Python code modification. Consider extracting reusable functions or classes." >> "$pattern_file"
                echo "" >> "$pattern_file"
                ;;
            sh)
                echo "### Shell Script Pattern: \`$file\`" >> "$pattern_file"
                echo "" >> "$pattern_file"
                echo "Automation script. Consider creating reusable functions or libraries." >> "$pattern_file"
                echo "" >> "$pattern_file"
                ;;
            yml|yaml)
                echo "### Configuration Pattern: \`$file\`" >> "$pattern_file"
                echo "" >> "$pattern_file"
                echo "YAML configuration. Consider documenting configuration patterns." >> "$pattern_file"
                echo "" >> "$pattern_file"
                ;;
        esac
    done

    # Create insight file
    local insight_file="docs/knowledge/insights/pr-${pr_number}-insights.md"
    cat > "$insight_file" <<EOF
# Insights from PR #${pr_number}

**Title**: ${pr_title}
**Author**: ${pr_author}
**Date**: ${pr_merged_at}

## Key Learnings

$(gh pr view "$pr_number" --json comments --jq '.comments[] | "- \(.body)"' 2>/dev/null || echo "No comments available")

## Statistics

- Files changed: $(echo "$pr_data" | jq -r '.files | length')
- Lines added: $(echo "$pr_data" | jq -r '.additions')
- Lines deleted: $(echo "$pr_data" | jq -r '.deletions')

## Tags

- author:${pr_author}
- pr:${pr_number}
$(echo "$pr_data" | jq -r '.labels[].name' | awk '{print "- label:" $0}')
EOF

    log_success "Extracted patterns to: $pattern_file"
    log_success "Extracted insights to: $insight_file"

    return 0
}

# Extract patterns from recent commits
extract_from_commits() {
    local limit=${1:-10}

    log_info "Extracting patterns from last $limit commits..."

    local commits
    if [ -n "$SINCE_DATE" ]; then
        commits=$(git log --since="$SINCE_DATE" --format="%H|%an|%ae|%s|%ai" --no-merges)
    else
        commits=$(git log -n "$limit" --format="%H|%an|%ae|%s|%ai" --no-merges)
    fi

    if [ -z "$commits" ]; then
        log_warning "No commits found"
        return 0
    fi

    local commit_count=0
    while IFS='|' read -r hash author email subject date; do
        commit_count=$((commit_count + 1))

        # Extract commit type from conventional commit format
        local commit_type="other"
        if [[ "$subject" =~ ^(feat|fix|docs|test|refactor|chore|style|perf): ]]; then
            commit_type=$(echo "$subject" | sed 's/:.*//')
        fi

        # Get files changed in this commit
        local files_changed
        files_changed=$(git diff-tree --no-commit-id --name-only -r "$hash")

        # Create pattern file for significant commits
        if [ "$commit_type" = "feat" ] || [ "$commit_type" = "fix" ] || [ "$commit_type" = "refactor" ]; then
            local pattern_file="docs/knowledge/patterns/commit-${hash:0:8}-${commit_type}.md"

            cat > "$pattern_file" <<EOF
# Pattern from Commit ${hash:0:8}

**Type**: ${commit_type}
**Author**: ${author}
**Date**: ${date}
**Subject**: ${subject}

## Files Modified

$(echo "$files_changed" | while read -r file; do echo "- \`$file\`"; done)

## Commit Details

\`\`\`
$(git show --stat "$hash")
\`\`\`

## Potential Patterns

EOF

            # Analyze file extensions for patterns
            echo "$files_changed" | while read -r file; do
                if [ -n "$file" ]; then
                    local ext="${file##*.}"
                    case "$ext" in
                        js|ts|jsx|tsx|py|sh|yml|yaml)
                            echo "- Pattern in \`$file\`: Consider extracting reusable components" >> "$pattern_file"
                            ;;
                    esac
                fi
            done

            log_info "Created pattern file: $pattern_file"
        fi

        # Create insight for all commits
        local insight_file="docs/knowledge/insights/commit-${hash:0:8}-insight.md"
        cat > "$insight_file" <<EOF
# Insight from Commit ${hash:0:8}

**Author**: ${author}
**Date**: ${date}
**Type**: ${commit_type}

## Change Summary

${subject}

## Files Changed

$(echo "$files_changed" | wc -l | tr -d ' ') files modified

## Tags

- author:${author}
- type:${commit_type}
- hash:${hash:0:8}
EOF

    done <<< "$commits"

    log_success "Processed $commit_count commits"

    return 0
}

# Update knowledge base indexes
update_indexes() {
    log_info "Updating knowledge base indexes..."

    # Update patterns index
    if [ -f "docs/knowledge/patterns/README.md" ]; then
        local pattern_count
        pattern_count=$(find docs/knowledge/patterns -name "*.md" ! -name "README.md" | wc -l | tr -d ' ')

        if grep -q "Total Patterns:" docs/knowledge/patterns/README.md; then
            sed -i.bak "s/Total Patterns: [0-9]*/Total Patterns: $pattern_count/" docs/knowledge/patterns/README.md
            rm -f docs/knowledge/patterns/README.md.bak
        fi

        log_success "Updated patterns index: $pattern_count patterns"
    fi

    # Update insights index
    if [ -f "docs/knowledge/insights/README.md" ]; then
        local insight_count
        insight_count=$(find docs/knowledge/insights -name "*.md" ! -name "README.md" | wc -l | tr -d ' ')

        if grep -q "Total Insights:" docs/knowledge/insights/README.md; then
            sed -i.bak "s/Total Insights: [0-9]*/Total Insights: $insight_count/" docs/knowledge/insights/README.md
            rm -f docs/knowledge/insights/README.md.bak
        fi

        log_success "Updated insights index: $insight_count insights"
    fi

    # Update main knowledge base README
    if [ -f "docs/knowledge/README.md" ]; then
        local total_docs
        total_docs=$(find docs/knowledge -name "*.md" ! -name "README.md" | wc -l | tr -d ' ')

        if grep -q "Total Documents:" docs/knowledge/README.md; then
            sed -i.bak "s/Total Documents: [0-9]*/Total Documents: $total_docs/" docs/knowledge/README.md
            rm -f docs/knowledge/README.md.bak
        fi

        log_success "Updated knowledge base index: $total_docs documents"
    fi
}

# Print summary
print_summary() {
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  Pattern Extraction Summary"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""

    local pattern_count
    local insight_count
    pattern_count=$(find docs/knowledge/patterns -name "*.md" ! -name "README.md" 2>/dev/null | wc -l | tr -d ' ')
    insight_count=$(find docs/knowledge/insights -name "*.md" ! -name "README.md" 2>/dev/null | wc -l | tr -d ' ')

    echo "  Patterns extracted: $pattern_count"
    echo "  Insights captured:  $insight_count"
    echo ""

    log_success "Knowledge base updated successfully!"
    echo ""
    echo "  Browse knowledge:"
    echo "    - Patterns: docs/knowledge/patterns/"
    echo "    - Insights: docs/knowledge/insights/"
    echo "    - Decisions: docs/knowledge/decisions/"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
}

# Main execution
main() {
    log_info "Starting pattern extraction..."
    echo ""

    check_tools || exit $?
    ensure_knowledge_dirs

    if [ -n "$PR_NUMBER" ]; then
        extract_from_pr "$PR_NUMBER" || exit 1
    elif [ "$EXTRACT_ALL" = true ]; then
        log_info "Extracting from all merged PRs..."
        if command -v gh &> /dev/null; then
            gh pr list --state merged --limit 100 --json number --jq '.[].number' | while read -r pr; do
                extract_from_pr "$pr" || true
            done
        else
            log_warning "GitHub CLI not available, falling back to commit analysis"
            extract_from_commits "50"
        fi
    elif [ -n "$SINCE_DATE" ]; then
        extract_from_commits "100"
    else
        # Default: extract from last 10 commits
        extract_from_commits "10"
    fi

    update_indexes
    print_summary

    exit 0
}

# Run main function
main "$@"
