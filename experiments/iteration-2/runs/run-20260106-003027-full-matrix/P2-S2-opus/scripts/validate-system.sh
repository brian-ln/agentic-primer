#!/usr/bin/env bash
#
# validate-system.sh - Validate @copilot automation system files
#
# Validates:
# - YAML syntax (yamllint)
# - Shell script syntax (shellcheck)
# - File existence
#
# Usage: ./scripts/validate-system.sh
#
# Exit codes:
#   0 - All validations passed
#   1 - One or more validations failed

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track overall status
FAILED=0

echo "=========================================="
echo "  @copilot System Validation"
echo "=========================================="
echo ""

# Function to print status
print_status() {
    local status=$1
    local message=$2
    if [[ "$status" -eq 0 ]]; then
        echo -e "  ${GREEN}PASS${NC} $message"
    else
        echo -e "  ${RED}FAIL${NC} $message"
        FAILED=1
    fi
}

# Validate YAML files
echo "Validating YAML files..."
YAML_FILES=(
    ".github/ISSUE_TEMPLATE/copilot-task.yml"
    ".github/workflows/copilot-issue.yml"
    ".github/workflows/copilot-pr-assign.yml"
)

for file in "${YAML_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        if command -v yamllint &> /dev/null; then
            if yamllint -d relaxed "$file" &> /dev/null; then
                print_status 0 "$file"
            else
                print_status 1 "$file (yamllint errors)"
            fi
        else
            # Fallback: basic YAML syntax check with Python
            if python3 -c "import yaml; yaml.safe_load(open('$file'))" &> /dev/null; then
                print_status 0 "$file (python yaml check)"
            else
                print_status 1 "$file (invalid YAML)"
            fi
        fi
    else
        print_status 1 "$file (file not found)"
    fi
done

echo ""

# Validate shell scripts
echo "Validating shell scripts..."
SHELL_FILES=(
    "scripts/validate-system.sh"
)

for file in "${SHELL_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        if command -v shellcheck &> /dev/null; then
            if shellcheck "$file" &> /dev/null; then
                print_status 0 "$file"
            else
                print_status 1 "$file (shellcheck errors)"
            fi
        else
            # Fallback: basic syntax check
            if bash -n "$file" &> /dev/null; then
                print_status 0 "$file (bash syntax check)"
            else
                print_status 1 "$file (syntax error)"
            fi
        fi
    else
        print_status 1 "$file (file not found)"
    fi
done

echo ""

# Check required files exist
echo "Checking required files..."
REQUIRED_FILES=(
    ".github/CODEOWNERS"
    "docs/knowledge/README.md"
    "docs/knowledge/patterns/README.md"
    "docs/knowledge/decisions/README.md"
    "docs/knowledge/insights/README.md"
    "README.md"
)

for file in "${REQUIRED_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        print_status 0 "$file"
    else
        print_status 1 "$file (missing)"
    fi
done

echo ""
echo "=========================================="

if [[ "$FAILED" -eq 0 ]]; then
    echo -e "${GREEN}All validations passed!${NC}"
    exit 0
else
    echo -e "${RED}Some validations failed.${NC}"
    exit 1
fi
