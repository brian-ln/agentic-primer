#!/usr/bin/env bash
#
# validate.sh - Validate syntax of all generated files
#
# Usage: ./scripts/validate.sh
#
# This script validates:
# - YAML files (using yamllint or yq)
# - Markdown files (using markdownlint or basic checks)
# - Shell scripts (using shellcheck)
#
# Exit codes:
#   0 - All validations passed
#   1 - One or more validations failed
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Print header
echo "=============================================="
echo "  Issue-Driven Development File Validator"
echo "=============================================="
echo ""
echo "Project root: $PROJECT_ROOT"
echo ""

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to validate YAML files
validate_yaml() {
    local file="$1"
    ((TOTAL++))

    echo -n "  Checking YAML: $(basename "$file")... "

    if command_exists yamllint; then
        if yamllint -d "{extends: relaxed, rules: {line-length: disable}}" "$file" >/dev/null 2>&1; then
            echo -e "${GREEN}PASS${NC}"
            ((PASSED++))
        else
            echo -e "${RED}FAIL${NC}"
            ((FAILED++))
            yamllint -d "{extends: relaxed, rules: {line-length: disable}}" "$file" 2>&1 | head -5
        fi
    elif command_exists yq; then
        if yq eval '.' "$file" >/dev/null 2>&1; then
            echo -e "${GREEN}PASS${NC}"
            ((PASSED++))
        else
            echo -e "${RED}FAIL${NC}"
            ((FAILED++))
        fi
    elif command_exists python3; then
        if python3 -c "import yaml; yaml.safe_load(open('$file'))" 2>/dev/null; then
            echo -e "${GREEN}PASS${NC}"
            ((PASSED++))
        else
            echo -e "${RED}FAIL${NC}"
            ((FAILED++))
        fi
    else
        echo -e "${YELLOW}SKIP (no YAML validator)${NC}"
        ((SKIPPED++))
    fi
}

# Function to validate Markdown files
validate_markdown() {
    local file="$1"
    ((TOTAL++))

    echo -n "  Checking Markdown: $(basename "$file")... "

    if command_exists markdownlint; then
        if markdownlint "$file" >/dev/null 2>&1; then
            echo -e "${GREEN}PASS${NC}"
            ((PASSED++))
        else
            echo -e "${YELLOW}WARN (lint issues)${NC}"
            ((PASSED++)) # Markdown lint warnings are not fatal
        fi
    else
        # Basic check: file exists and is not empty
        if [ -s "$file" ]; then
            echo -e "${GREEN}PASS (basic)${NC}"
            ((PASSED++))
        else
            echo -e "${RED}FAIL (empty)${NC}"
            ((FAILED++))
        fi
    fi
}

# Function to validate shell scripts
validate_shell() {
    local file="$1"
    ((TOTAL++))

    echo -n "  Checking Shell: $(basename "$file")... "

    if command_exists shellcheck; then
        if shellcheck "$file" >/dev/null 2>&1; then
            echo -e "${GREEN}PASS${NC}"
            ((PASSED++))
        else
            echo -e "${RED}FAIL${NC}"
            ((FAILED++))
            shellcheck "$file" 2>&1 | head -10
        fi
    else
        # Basic check: bash syntax
        if bash -n "$file" 2>/dev/null; then
            echo -e "${GREEN}PASS (basic)${NC}"
            ((PASSED++))
        else
            echo -e "${RED}FAIL${NC}"
            ((FAILED++))
        fi
    fi
}

# Validate GitHub configuration
echo "GitHub Configuration:"
echo "---------------------"

if [ -f "$PROJECT_ROOT/.github/ISSUE_TEMPLATE/task.yml" ]; then
    validate_yaml "$PROJECT_ROOT/.github/ISSUE_TEMPLATE/task.yml"
else
    echo -e "  ${RED}MISSING: .github/ISSUE_TEMPLATE/task.yml${NC}"
    ((FAILED++))
    ((TOTAL++))
fi

if [ -f "$PROJECT_ROOT/.github/workflows/copilot-issue.yml" ]; then
    validate_yaml "$PROJECT_ROOT/.github/workflows/copilot-issue.yml"
else
    echo -e "  ${RED}MISSING: .github/workflows/copilot-issue.yml${NC}"
    ((FAILED++))
    ((TOTAL++))
fi

if [ -f "$PROJECT_ROOT/.github/CODEOWNERS" ]; then
    ((TOTAL++))
    echo -n "  Checking CODEOWNERS... "
    # Basic validation: check for wildcard pattern
    if grep -q '^\*[[:space:]]' "$PROJECT_ROOT/.github/CODEOWNERS"; then
        echo -e "${GREEN}PASS${NC}"
        ((PASSED++))
    else
        echo -e "${YELLOW}WARN (no default owner)${NC}"
        ((PASSED++))
    fi
else
    echo -e "  ${RED}MISSING: .github/CODEOWNERS${NC}"
    ((FAILED++))
    ((TOTAL++))
fi

echo ""

# Validate Knowledge Base
echo "Knowledge Base:"
echo "---------------"

for readme in "$PROJECT_ROOT"/docs/knowledge/README.md \
              "$PROJECT_ROOT"/docs/knowledge/patterns/README.md \
              "$PROJECT_ROOT"/docs/knowledge/decisions/README.md \
              "$PROJECT_ROOT"/docs/knowledge/insights/README.md; do
    if [ -f "$readme" ]; then
        validate_markdown "$readme"
    else
        echo -e "  ${RED}MISSING: ${readme#$PROJECT_ROOT/}${NC}"
        ((FAILED++))
        ((TOTAL++))
    fi
done

# Validate pattern files
for pattern in "$PROJECT_ROOT"/docs/knowledge/patterns/*.md; do
    if [ -f "$pattern" ] && [ "$(basename "$pattern")" != "README.md" ]; then
        validate_markdown "$pattern"
    fi
done

# Validate decision files
for decision in "$PROJECT_ROOT"/docs/knowledge/decisions/*.md; do
    if [ -f "$decision" ] && [ "$(basename "$decision")" != "README.md" ]; then
        validate_markdown "$decision"
    fi
done

# Validate insight files
for insight in "$PROJECT_ROOT"/docs/knowledge/insights/*.md; do
    if [ -f "$insight" ] && [ "$(basename "$insight")" != "README.md" ]; then
        validate_markdown "$insight"
    fi
done

echo ""

# Validate Scripts
echo "Scripts:"
echo "--------"

for script in "$PROJECT_ROOT"/scripts/*.sh; do
    if [ -f "$script" ]; then
        validate_shell "$script"
    fi
done

echo ""

# Validate Root Documentation
echo "Documentation:"
echo "--------------"

if [ -f "$PROJECT_ROOT/README.md" ]; then
    validate_markdown "$PROJECT_ROOT/README.md"
else
    echo -e "  ${RED}MISSING: README.md${NC}"
    ((FAILED++))
    ((TOTAL++))
fi

echo ""

# Print summary
echo "=============================================="
echo "  Validation Summary"
echo "=============================================="
echo ""
echo -e "  Total:   $TOTAL"
echo -e "  ${GREEN}Passed:  $PASSED${NC}"
echo -e "  ${RED}Failed:  $FAILED${NC}"
echo -e "  ${YELLOW}Skipped: $SKIPPED${NC}"
echo ""

# Exit with appropriate code
if [ "$FAILED" -gt 0 ]; then
    echo -e "${RED}Validation FAILED${NC}"
    exit 1
else
    echo -e "${GREEN}Validation PASSED${NC}"
    exit 0
fi
