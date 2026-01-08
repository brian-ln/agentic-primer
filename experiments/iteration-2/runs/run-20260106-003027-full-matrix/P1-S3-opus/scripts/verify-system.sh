#!/usr/bin/env bash
#
# Verify @copilot Automation System
#
# Usage: ./scripts/verify-system.sh [--verbose]
#
# Validates all system components:
# - File structure
# - YAML syntax
# - Shell script syntax
# - Markdown formatting

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERBOSE=false
ERRORS=0
WARNINGS=0

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

log_info() {
    if [ "$VERBOSE" = true ]; then
        echo -e "${BLUE}[INFO]${NC} $1"
    fi
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    ((WARNINGS++))
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((ERRORS++))
}

# Check file exists
check_file() {
    local file="$1"
    local description="$2"

    if [ -f "$REPO_ROOT/$file" ]; then
        log_success "$description: $file"
        return 0
    else
        log_error "$description missing: $file"
        return 1
    fi
}

# Validate YAML syntax
validate_yaml() {
    local file="$1"

    if ! command -v yamllint &> /dev/null; then
        if command -v python3 &> /dev/null; then
            # Fallback to Python YAML parser
            if python3 -c "import yaml; yaml.safe_load(open('$file'))" 2>/dev/null; then
                log_success "YAML valid: $file"
                return 0
            else
                log_error "YAML invalid: $file"
                return 1
            fi
        else
            log_warn "yamllint not installed, skipping: $file"
            return 0
        fi
    fi

    if yamllint -c "$REPO_ROOT/.yamllint.yml" "$file" 2>/dev/null; then
        log_success "YAML valid: $file"
        return 0
    else
        log_error "YAML invalid: $file"
        return 1
    fi
}

# Validate shell script syntax
validate_shell() {
    local file="$1"

    if ! command -v shellcheck &> /dev/null; then
        # Fallback to bash syntax check
        if bash -n "$file" 2>/dev/null; then
            log_success "Shell syntax valid: $file"
            return 0
        else
            log_error "Shell syntax error: $file"
            return 1
        fi
    fi

    if shellcheck "$file" 2>/dev/null; then
        log_success "ShellCheck passed: $file"
        return 0
    else
        log_warn "ShellCheck warnings: $file"
        return 0  # Warnings are acceptable
    fi
}

# Validate markdown
validate_markdown() {
    local file="$1"

    if ! command -v markdownlint &> /dev/null; then
        # Basic check: file is readable and has content
        if [ -s "$file" ]; then
            log_success "Markdown readable: $file"
            return 0
        else
            log_warn "Markdown empty: $file"
            return 0
        fi
    fi

    if markdownlint "$file" 2>/dev/null; then
        log_success "Markdown valid: $file"
        return 0
    else
        log_warn "Markdown warnings: $file"
        return 0  # Warnings are acceptable
    fi
}

# Check directory structure
check_directories() {
    echo ""
    echo "=== Directory Structure ==="

    local dirs=(
        ".github/ISSUE_TEMPLATE"
        ".github/workflows"
        "docs/knowledge/patterns"
        "docs/knowledge/decisions"
        "docs/knowledge/insights"
        "scripts"
    )

    for dir in "${dirs[@]}"; do
        if [ -d "$REPO_ROOT/$dir" ]; then
            log_success "Directory exists: $dir"
        else
            log_error "Directory missing: $dir"
        fi
    done
}

# Check required files
check_required_files() {
    echo ""
    echo "=== Required Files ==="

    check_file ".github/ISSUE_TEMPLATE/copilot-task.yml" "Issue template"
    check_file ".github/workflows/issue-copilot.yml" "Issue workflow"
    check_file ".github/workflows/pr-auto-review.yml" "PR review workflow"
    check_file ".github/workflows/self-improvement.yml" "Self-improvement workflow"
    check_file ".github/CODEOWNERS" "CODEOWNERS"
    check_file ".github/copilot-instructions.md" "Copilot instructions"
    check_file "docs/knowledge/README.md" "Knowledge base index"
    check_file "README.md" "Project README"
}

# Validate YAML files
check_yaml_files() {
    echo ""
    echo "=== YAML Validation ==="

    local yaml_files=(
        ".github/ISSUE_TEMPLATE/copilot-task.yml"
        ".github/workflows/issue-copilot.yml"
        ".github/workflows/pr-auto-review.yml"
        ".github/workflows/self-improvement.yml"
    )

    for file in "${yaml_files[@]}"; do
        if [ -f "$REPO_ROOT/$file" ]; then
            validate_yaml "$REPO_ROOT/$file"
        fi
    done
}

# Validate shell scripts
check_shell_scripts() {
    echo ""
    echo "=== Shell Script Validation ==="

    while IFS= read -r -d '' file; do
        validate_shell "$file"
    done < <(find "$REPO_ROOT/scripts" -name "*.sh" -print0 2>/dev/null || true)
}

# Validate markdown files
check_markdown_files() {
    echo ""
    echo "=== Markdown Validation ==="

    local md_files=(
        "README.md"
        "docs/knowledge/README.md"
        "docs/knowledge/patterns/README.md"
        "docs/knowledge/decisions/README.md"
        "docs/knowledge/insights/README.md"
        ".github/copilot-instructions.md"
    )

    for file in "${md_files[@]}"; do
        if [ -f "$REPO_ROOT/$file" ]; then
            validate_markdown "$REPO_ROOT/$file"
        fi
    done
}

# Check workflow triggers
check_workflow_triggers() {
    echo ""
    echo "=== Workflow Triggers ==="

    # Check issue workflow triggers on issues
    if grep -q "on:.*issues:" "$REPO_ROOT/.github/workflows/issue-copilot.yml" 2>/dev/null || \
       grep -q "issues:" "$REPO_ROOT/.github/workflows/issue-copilot.yml" 2>/dev/null; then
        log_success "Issue workflow triggers on issues"
    else
        log_error "Issue workflow missing issue trigger"
    fi

    # Check PR workflow triggers on pull_request
    if grep -q "on:.*pull_request:" "$REPO_ROOT/.github/workflows/pr-auto-review.yml" 2>/dev/null || \
       grep -q "pull_request:" "$REPO_ROOT/.github/workflows/pr-auto-review.yml" 2>/dev/null; then
        log_success "PR workflow triggers on pull_request"
    else
        log_error "PR workflow missing pull_request trigger"
    fi

    # Check self-improvement has schedule
    if grep -q "schedule:" "$REPO_ROOT/.github/workflows/self-improvement.yml" 2>/dev/null; then
        log_success "Self-improvement has schedule trigger"
    else
        log_warn "Self-improvement missing schedule trigger"
    fi
}

# Print summary
print_summary() {
    echo ""
    echo "=========================================="
    echo "  Verification Summary"
    echo "=========================================="
    echo ""
    echo "  Errors:   $ERRORS"
    echo "  Warnings: $WARNINGS"
    echo ""

    if [ $ERRORS -eq 0 ]; then
        echo -e "  ${GREEN}System verification PASSED${NC}"
        echo ""
        return 0
    else
        echo -e "  ${RED}System verification FAILED${NC}"
        echo ""
        return 1
    fi
}

# Main execution
main() {
    echo ""
    echo "=========================================="
    echo "  @copilot System Verification"
    echo "=========================================="

    check_directories
    check_required_files
    check_yaml_files
    check_shell_scripts
    check_markdown_files
    check_workflow_triggers

    print_summary
}

main "$@"
