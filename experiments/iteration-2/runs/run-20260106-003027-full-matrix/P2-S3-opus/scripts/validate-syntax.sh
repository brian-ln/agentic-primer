#!/usr/bin/env bash
# Syntax validation script for @copilot issue-driven development system
# Validates YAML, shell scripts, and markdown files

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
SKIPPED_CHECKS=0

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $1"; ((PASSED_CHECKS++)); ((TOTAL_CHECKS++)); }
log_error() { echo -e "${RED}[FAIL]${NC} $1"; ((FAILED_CHECKS++)); ((TOTAL_CHECKS++)); }
log_skip() { echo -e "${YELLOW}[SKIP]${NC} $1"; ((SKIPPED_CHECKS++)); }

# Header
echo ""
echo "=========================================="
echo " Syntax Validation"
echo "=========================================="
echo ""

# Check for validation tools
HAVE_YAMLLINT=false
HAVE_SHELLCHECK=false
HAVE_PYTHON=false

command -v yamllint &> /dev/null && HAVE_YAMLLINT=true
command -v shellcheck &> /dev/null && HAVE_SHELLCHECK=true
command -v python3 &> /dev/null && HAVE_PYTHON=true

log_info "Available tools: yamllint=$HAVE_YAMLLINT, shellcheck=$HAVE_SHELLCHECK, python3=$HAVE_PYTHON"
echo ""

# Validate YAML files
log_info "Validating YAML files..."

yaml_files=(
    ".github/workflows/copilot-issue-handler.yml"
    ".github/workflows/copilot-pr-review.yml"
    ".github/workflows/self-improvement-analyzer.yml"
    ".github/ISSUE_TEMPLATE/copilot-task.yml"
    ".github/ISSUE_TEMPLATE/self-improvement.yml"
)

for file in "${yaml_files[@]}"; do
    filepath="$PROJECT_ROOT/$file"
    if [[ ! -f "$filepath" ]]; then
        log_skip "$file (not found)"
        continue
    fi

    if $HAVE_YAMLLINT; then
        if yamllint -d "{extends: relaxed, rules: {line-length: disable}}" "$filepath" 2>/dev/null; then
            log_success "$file"
        else
            log_error "$file (yamllint failed)"
        fi
    elif $HAVE_PYTHON; then
        # Fallback: use Python's yaml module
        if python3 -c "import yaml; yaml.safe_load(open('$filepath'))" 2>/dev/null; then
            log_success "$file (python yaml check)"
        else
            log_error "$file (invalid YAML syntax)"
        fi
    else
        log_skip "$file (no YAML validator)"
    fi
done

echo ""

# Validate shell scripts
log_info "Validating shell scripts..."

shell_files=(
    "scripts/bootstrap.sh"
    "scripts/validate-syntax.sh"
    "scripts/test-issue-processing.sh"
)

for file in "${shell_files[@]}"; do
    filepath="$PROJECT_ROOT/$file"
    if [[ ! -f "$filepath" ]]; then
        log_skip "$file (not found)"
        continue
    fi

    if $HAVE_SHELLCHECK; then
        if shellcheck -x "$filepath" 2>/dev/null; then
            log_success "$file"
        else
            log_error "$file (shellcheck issues)"
        fi
    else
        # Fallback: basic syntax check
        if bash -n "$filepath" 2>/dev/null; then
            log_success "$file (bash -n check)"
        else
            log_error "$file (bash syntax error)"
        fi
    fi
done

echo ""

# Validate markdown files (basic structure check)
log_info "Validating markdown files..."

markdown_files=(
    ".github/copilot-instructions.md"
    "docs/knowledge/README.md"
    "docs/knowledge/patterns/README.md"
    "docs/knowledge/patterns/issue-to-pr-workflow.md"
    "docs/knowledge/decisions/README.md"
    "docs/knowledge/decisions/001-copilot-automation.md"
    "docs/knowledge/insights/README.md"
    "docs/knowledge/insights/multi-agent-compatibility.md"
    "SOLUTION.md"
)

for file in "${markdown_files[@]}"; do
    filepath="$PROJECT_ROOT/$file"
    if [[ ! -f "$filepath" ]]; then
        log_skip "$file (not found)"
        continue
    fi

    # Basic checks: file is not empty and has a heading
    if [[ -s "$filepath" ]]; then
        if head -10 "$filepath" | grep -q "^#"; then
            log_success "$file"
        else
            log_error "$file (missing heading)"
        fi
    else
        log_error "$file (empty file)"
    fi
done

echo ""

# Summary
echo "=========================================="
echo " Validation Summary"
echo "=========================================="
echo ""
log_info "Total checks: $TOTAL_CHECKS"
echo -e "${GREEN}Passed: $PASSED_CHECKS${NC}"
echo -e "${RED}Failed: $FAILED_CHECKS${NC}"
echo -e "${YELLOW}Skipped: $SKIPPED_CHECKS${NC}"
echo ""

if [[ $FAILED_CHECKS -eq 0 ]]; then
    echo -e "${GREEN}All validations passed!${NC}"
    exit 0
else
    echo -e "${RED}Some validations failed. Please fix the issues above.${NC}"
    exit 1
fi
