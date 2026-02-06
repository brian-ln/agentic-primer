#!/usr/bin/env bash
#
# Analyze @copilot Automation Logs
#
# Usage: ./scripts/analyze-logs.sh [--days N] [--output FILE]
#
# Analyzes issue and PR logs to identify patterns and improvement opportunities.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DAYS=30
OUTPUT=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --days)
            DAYS="$2"
            shift 2
            ;;
        --output)
            OUTPUT="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_section() {
    echo ""
    echo "=== $1 ==="
}

# Check for jq
check_dependencies() {
    if ! command -v jq &> /dev/null; then
        echo "Warning: jq not installed. Using basic analysis."
        return 1
    fi
    return 0
}

# Count lines in JSONL file
count_entries() {
    local file="$1"
    if [ -f "$file" ]; then
        wc -l < "$file" | tr -d ' '
    else
        echo "0"
    fi
}

# Analyze issues log
analyze_issues() {
    log_section "Issue Analysis"

    local issues_log="$REPO_ROOT/docs/knowledge/insights/logs/issues.jsonl"
    local count
    count=$(count_entries "$issues_log")

    echo "Total issues logged: $count"

    if [ "$count" -eq 0 ]; then
        echo "No issues to analyze."
        return
    fi

    if check_dependencies; then
        # Status distribution
        echo ""
        echo "Status Distribution:"
        jq -r '.status' "$issues_log" 2>/dev/null | sort | uniq -c | sort -rn || echo "  Unable to parse"

        # Test vs real issues
        echo ""
        echo "Test vs Real Issues:"
        local test_count
        test_count=$(jq -r 'select(.test == true) | .issue_number' "$issues_log" 2>/dev/null | wc -l | tr -d ' ')
        local real_count=$((count - test_count))
        echo "  Test issues: $test_count"
        echo "  Real issues: $real_count"
    fi
}

# Analyze PRs log
analyze_prs() {
    log_section "PR Analysis"

    local prs_log="$REPO_ROOT/docs/knowledge/insights/logs/prs.jsonl"
    local count
    count=$(count_entries "$prs_log")

    echo "Total PRs logged: $count"

    if [ "$count" -eq 0 ]; then
        echo "No PRs to analyze."
        return
    fi

    if check_dependencies; then
        # Status distribution
        echo ""
        echo "Status Distribution:"
        jq -r '.status' "$prs_log" 2>/dev/null | sort | uniq -c | sort -rn || echo "  Unable to parse"

        # Linked vs unlinked
        echo ""
        echo "Issue Linking:"
        local linked
        linked=$(jq -r 'select(.linked_issue != "" and .linked_issue != null) | .pr_number' "$prs_log" 2>/dev/null | wc -l | tr -d ' ')
        local unlinked=$((count - linked))
        echo "  Linked PRs: $linked"
        echo "  Unlinked PRs: $unlinked"

        if [ "$count" -gt 0 ]; then
            local link_rate=$((linked * 100 / count))
            echo "  Link rate: ${link_rate}%"
        fi
    fi
}

# Analyze improvements
analyze_improvements() {
    log_section "Improvements Analysis"

    local improvements_log="$REPO_ROOT/docs/knowledge/insights/improvements/applied.jsonl"
    local count
    count=$(count_entries "$improvements_log")

    echo "Total improvements proposed: $count"

    if [ "$count" -eq 0 ]; then
        echo "No improvements to analyze."
        return
    fi

    if check_dependencies; then
        # Category distribution
        echo ""
        echo "By Category:"
        jq -r '.category' "$improvements_log" 2>/dev/null | sort | uniq -c | sort -rn || echo "  Unable to parse"

        # Status distribution
        echo ""
        echo "By Status:"
        jq -r '.status' "$improvements_log" 2>/dev/null | sort | uniq -c | sort -rn || echo "  Unable to parse"
    fi
}

# Identify patterns
identify_patterns() {
    log_section "Pattern Detection"

    local issues_log="$REPO_ROOT/docs/knowledge/insights/logs/issues.jsonl"

    if [ ! -f "$issues_log" ] || [ "$(count_entries "$issues_log")" -eq 0 ]; then
        echo "Insufficient data for pattern detection."
        return
    fi

    if check_dependencies; then
        # Find common words in titles
        echo "Common words in issue titles:"
        jq -r '.title // empty' "$issues_log" 2>/dev/null | \
            tr '[:upper:]' '[:lower:]' | \
            tr -cs '[:alpha:]' '\n' | \
            grep -E '^.{4,}$' | \
            sort | uniq -c | sort -rn | head -10 || echo "  Unable to analyze"
    fi
}

# Generate recommendations
generate_recommendations() {
    log_section "Recommendations"

    local issues_log="$REPO_ROOT/docs/knowledge/insights/logs/issues.jsonl"
    local prs_log="$REPO_ROOT/docs/knowledge/insights/logs/prs.jsonl"

    local issue_count
    issue_count=$(count_entries "$issues_log")

    local pr_count
    pr_count=$(count_entries "$prs_log")

    local recommendations=()

    # Check issue to PR ratio
    if [ "$issue_count" -gt 0 ] && [ "$pr_count" -gt 0 ]; then
        local ratio=$((pr_count * 100 / issue_count))
        if [ "$ratio" -lt 80 ]; then
            recommendations+=("Low issue-to-PR conversion rate (${ratio}%). Review issue quality.")
        fi
    fi

    # Check for unlinked PRs
    if check_dependencies && [ "$pr_count" -gt 0 ]; then
        local unlinked
        unlinked=$(jq -r 'select(.linked_issue == "" or .linked_issue == null) | .pr_number' "$prs_log" 2>/dev/null | wc -l | tr -d ' ')
        local unlink_rate=$((unlinked * 100 / pr_count))
        if [ "$unlink_rate" -gt 20 ]; then
            recommendations+=("${unlink_rate}% of PRs have no linked issue. Enforce issue linking.")
        fi
    fi

    # Check data volume
    if [ "$issue_count" -lt 5 ]; then
        recommendations+=("Low data volume ($issue_count issues). Need more activity for meaningful analysis.")
    fi

    if [ ${#recommendations[@]} -eq 0 ]; then
        echo "No specific recommendations at this time."
    else
        for rec in "${recommendations[@]}"; do
            echo "- $rec"
        done
    fi
}

# Print summary
print_summary() {
    log_section "Summary"

    local issues_count
    issues_count=$(count_entries "$REPO_ROOT/docs/knowledge/insights/logs/issues.jsonl")

    local prs_count
    prs_count=$(count_entries "$REPO_ROOT/docs/knowledge/insights/logs/prs.jsonl")

    local improvements_count
    improvements_count=$(count_entries "$REPO_ROOT/docs/knowledge/insights/improvements/applied.jsonl")

    echo "Analysis Period: Last $DAYS days"
    echo "Issues Logged: $issues_count"
    echo "PRs Logged: $prs_count"
    echo "Improvements Proposed: $improvements_count"
}

# Save output to file
save_output() {
    local file="$1"
    local content="$2"

    echo "$content" > "$file"
    echo ""
    echo "Output saved to: $file"
}

# Main execution
main() {
    echo ""
    echo "=========================================="
    echo "  @copilot Log Analysis"
    echo "=========================================="
    echo "  Analysis period: Last $DAYS days"
    echo "=========================================="

    local output=""

    output+=$(analyze_issues)
    output+=$(analyze_prs)
    output+=$(analyze_improvements)
    output+=$(identify_patterns)
    output+=$(generate_recommendations)
    output+=$(print_summary)

    echo "$output"

    if [ -n "$OUTPUT" ]; then
        save_output "$OUTPUT" "$output"
    fi
}

main "$@"
