#!/usr/bin/env bash

# auto-review.sh
# Automated PR quality validation for @copilot-generated pull requests
#
# This script runs syntax validation and quality checks before human review.
# It posts review comments on the PR with findings.
#
# Usage:
#   ./scripts/auto-review.sh <pr-number>
#
# Environment Variables:
#   GITHUB_TOKEN - GitHub API token for posting comments (required)
#   GITHUB_REPOSITORY - Repository in format "owner/repo" (required)
#
# Exit Codes:
#   0 - All checks passed
#   1 - Validation errors found
#   2 - Usage error or missing dependencies

set -euo pipefail

# Color output for readability
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Track check results
CHECKS_PASSED=0
CHECKS_FAILED=0
REVIEW_COMMENTS=""

# Usage information
usage() {
    cat <<EOF
Usage: $0 <pr-number>

Automated PR quality validation for @copilot-generated pull requests.

Arguments:
  pr-number       Pull request number to review

Environment Variables:
  GITHUB_TOKEN         GitHub API token for posting comments
  GITHUB_REPOSITORY    Repository in format "owner/repo"

Examples:
  $0 42
  GITHUB_TOKEN=ghp_xxx GITHUB_REPOSITORY=user/repo $0 123

EOF
    exit 2
}

# Print colored status messages
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $*"
}

# Check if required tools are installed
check_dependencies() {
    local missing_deps=()

    for cmd in git yamllint shellcheck markdownlint; do
        if ! command -v "$cmd" &>/dev/null; then
            missing_deps+=("$cmd")
        fi
    done

    if [ ${#missing_deps[@]} -gt 0 ]; then
        log_error "Missing required dependencies: ${missing_deps[*]}"
        log_info "Install with:"
        log_info "  - yamllint: pip install yamllint"
        log_info "  - shellcheck: apt-get install shellcheck (or brew install shellcheck)"
        log_info "  - markdownlint: npm install -g markdownlint-cli"
        return 1
    fi

    log_success "All dependencies available"
    return 0
}

# Validate YAML files
check_yaml_syntax() {
    log_info "Checking YAML syntax..."

    local yaml_files
    yaml_files=$(find "$PROJECT_ROOT" -type f \( -name "*.yml" -o -name "*.yaml" \) -not -path "*/node_modules/*" -not -path "*/.git/*")

    if [ -z "$yaml_files" ]; then
        log_warning "No YAML files found"
        return 0
    fi

    local yaml_errors=""
    while IFS= read -r file; do
        if ! yamllint -f parsable "$file" 2>&1; then
            yaml_errors+="YAML syntax error in $file\n"
            ((CHECKS_FAILED++))
        else
            ((CHECKS_PASSED++))
        fi
    done <<< "$yaml_files"

    if [ -n "$yaml_errors" ]; then
        log_error "YAML validation failed"
        REVIEW_COMMENTS+="## YAML Syntax Errors\n\n\`\`\`\n${yaml_errors}\`\`\`\n\n"
        return 1
    else
        log_success "YAML validation passed"
        return 0
    fi
}

# Validate shell scripts
check_shell_syntax() {
    log_info "Checking shell script syntax..."

    local shell_files
    shell_files=$(find "$PROJECT_ROOT" -type f \( -name "*.sh" -o -name "*.bash" \) -not -path "*/node_modules/*" -not -path "*/.git/*")

    if [ -z "$shell_files" ]; then
        log_warning "No shell scripts found"
        return 0
    fi

    local shell_errors=""
    while IFS= read -r file; do
        if ! shellcheck -f gcc "$file" 2>&1; then
            shell_errors+="Shell syntax error in $file\n"
            ((CHECKS_FAILED++))
        else
            ((CHECKS_PASSED++))
        fi
    done <<< "$shell_files"

    if [ -n "$shell_errors" ]; then
        log_error "Shell script validation failed"
        REVIEW_COMMENTS+="## Shell Script Errors\n\n\`\`\`\n${shell_errors}\`\`\`\n\n"
        return 1
    else
        log_success "Shell script validation passed"
        return 0
    fi
}

# Validate markdown files
check_markdown_syntax() {
    log_info "Checking Markdown syntax..."

    local md_files
    md_files=$(find "$PROJECT_ROOT" -type f -name "*.md" -not -path "*/node_modules/*" -not -path "*/.git/*")

    if [ -z "$md_files" ]; then
        log_warning "No Markdown files found"
        return 0
    fi

    # Create a permissive markdownlint config if it doesn't exist
    local mdl_config="$PROJECT_ROOT/.markdownlint.json"
    if [ ! -f "$mdl_config" ]; then
        cat > "$mdl_config" <<'MDLEOF'
{
  "default": true,
  "MD013": false,
  "MD033": false,
  "MD041": false
}
MDLEOF
    fi

    local md_errors=""
    # shellcheck disable=SC2086
    if ! markdownlint -c "$mdl_config" $md_files 2>&1; then
        md_errors="Markdown linting issues found\n"
        ((CHECKS_FAILED++))
    else
        ((CHECKS_PASSED++))
    fi

    if [ -n "$md_errors" ]; then
        log_warning "Markdown validation found issues (non-blocking)"
        REVIEW_COMMENTS+="## Markdown Linting\n\n\`\`\`\n${md_errors}\`\`\`\n\n"
        return 0  # Non-blocking for now
    else
        log_success "Markdown validation passed"
        return 0
    fi
}

# Run tests if test suite exists
check_tests() {
    log_info "Checking for test suite..."

    # Look for common test runners
    if [ -f "$PROJECT_ROOT/package.json" ] && grep -q '"test"' "$PROJECT_ROOT/package.json"; then
        log_info "Running npm tests..."
        if npm test; then
            log_success "Tests passed"
            ((CHECKS_PASSED++))
        else
            log_error "Tests failed"
            REVIEW_COMMENTS+="## Test Failures\n\nNPM test suite failed. Please review test output.\n\n"
            ((CHECKS_FAILED++))
            return 1
        fi
    elif [ -f "$PROJECT_ROOT/Makefile" ] && grep -q "^test:" "$PROJECT_ROOT/Makefile"; then
        log_info "Running make test..."
        if make test; then
            log_success "Tests passed"
            ((CHECKS_PASSED++))
        else
            log_error "Tests failed"
            REVIEW_COMMENTS+="## Test Failures\n\nMake test suite failed. Please review test output.\n\n"
            ((CHECKS_FAILED++))
            return 1
        fi
    else
        log_warning "No test suite found (skipping)"
    fi

    return 0
}

# Post review comment to GitHub PR
post_review_comment() {
    local pr_number="$1"

    if [ -z "${GITHUB_TOKEN:-}" ] || [ -z "${GITHUB_REPOSITORY:-}" ]; then
        log_warning "GITHUB_TOKEN or GITHUB_REPOSITORY not set, skipping comment posting"
        log_info "Review comment would be:"
        echo -e "$REVIEW_COMMENTS"
        return 0
    fi

    local review_body
    review_body=$(cat <<EOF
## 🤖 Auto-Review Results

**Checks Passed:** $CHECKS_PASSED
**Checks Failed:** $CHECKS_FAILED

$REVIEW_COMMENTS

---
*This automated review was generated by auto-review.sh*
*Human review is still required before merging*
EOF
    )

    # Post comment using GitHub API
    log_info "Posting review comment to PR #$pr_number..."

    # Escape JSON properly
    local json_body
    json_body=$(jq -n --arg body "$review_body" '{body: $body}')

    if curl -s -X POST \
        -H "Authorization: token $GITHUB_TOKEN" \
        -H "Accept: application/vnd.github.v3+json" \
        "https://api.github.com/repos/$GITHUB_REPOSITORY/issues/$pr_number/comments" \
        -d "$json_body" > /dev/null; then
        log_success "Review comment posted"
    else
        log_error "Failed to post review comment"
        return 1
    fi
}

# Main execution
main() {
    log_info "Starting automated PR review..."
    log_info "Project root: $PROJECT_ROOT"

    # Parse arguments
    if [ $# -ne 1 ]; then
        usage
    fi

    local pr_number="$1"

    # Validate PR number
    if ! [[ "$pr_number" =~ ^[0-9]+$ ]]; then
        log_error "Invalid PR number: $pr_number"
        usage
    fi

    log_info "Reviewing PR #$pr_number"

    # Check dependencies
    if ! check_dependencies; then
        exit 2
    fi

    # Run validation checks
    check_yaml_syntax || true
    check_shell_syntax || true
    check_markdown_syntax || true
    check_tests || true

    # Summary
    echo ""
    log_info "Review complete!"
    log_info "Checks passed: $CHECKS_PASSED"
    log_info "Checks failed: $CHECKS_FAILED"

    # Post review comment
    if [ -n "$REVIEW_COMMENTS" ]; then
        post_review_comment "$pr_number" || true
    else
        REVIEW_COMMENTS="All automated checks passed! ✅"
        post_review_comment "$pr_number" || true
    fi

    # Exit with appropriate code
    if [ $CHECKS_FAILED -gt 0 ]; then
        log_error "Validation failed"
        exit 1
    else
        log_success "All checks passed"
        exit 0
    fi
}

# Run main function
main "$@"
