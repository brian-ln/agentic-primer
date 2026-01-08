#!/usr/bin/env bash
#
# validate-all.sh - Comprehensive validation script for @copilot system
#
# Purpose: Validates all configuration files, scripts, and workflows
# Usage: ./tests/validate-all.sh
# Exit codes: 0 = all passed, 1 = validation failed

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "================================================"
echo "  @copilot System Validation"
echo "================================================"
echo ""

# Track results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Helper function to run test
run_test() {
    local test_name="$1"
    local test_command="$2"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -n "Testing: $test_name... "

    if eval "$test_command" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo -e "${RED}FAIL${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Helper function to check file exists
check_file() {
    local file_path="$1"
    local description="$2"

    run_test "$description" "test -f '$PROJECT_ROOT/$file_path'"
}

echo "1. FILE EXISTENCE CHECKS"
echo "------------------------"

# Issue templates
check_file ".github/ISSUE_TEMPLATE/config.yml" "Issue template config"
check_file ".github/ISSUE_TEMPLATE/copilot-feature.yml" "Feature template"
check_file ".github/ISSUE_TEMPLATE/copilot-bug.yml" "Bug template"
check_file ".github/ISSUE_TEMPLATE/copilot-refactor.yml" "Refactor template"

# Workflows
check_file ".github/workflows/copilot-issue-handler.yml" "Issue handler workflow"
check_file ".github/workflows/copilot-pr-assign.yml" "PR assign workflow"
check_file ".github/workflows/copilot-test.yml" "Test workflow"

# Scripts
check_file ".github/scripts/copilot-worker.sh" "Worker script"
check_file ".github/scripts/knowledge-query.sh" "KB query script"
check_file ".github/scripts/validate-issue.sh" "Issue validation script"

# Knowledge base
check_file "knowledge-base/index.yml" "KB index"
check_file "knowledge-base/architecture/patterns.yml" "Architecture patterns"
check_file "knowledge-base/architecture/components.yml" "Components"
check_file "knowledge-base/practices/coding-standards.yml" "Coding standards"
check_file "knowledge-base/practices/testing.yml" "Testing practices"
check_file "knowledge-base/context/tech-stack.yml" "Tech stack"
check_file "knowledge-base/context/dependencies.yml" "Dependencies"

# Documentation
check_file "docs/COPILOT_WORKFLOW.md" "Workflow documentation"
check_file "docs/KNOWLEDGE_BASE.md" "KB documentation"

# Tests
check_file "tests/test-issue.json" "Test issue"
check_file "tests/validate-all.sh" "Validation script (self)"

echo ""
echo "2. SHELL SCRIPT VALIDATION (shellcheck)"
echo "----------------------------------------"

if command -v shellcheck > /dev/null 2>&1; then
    for script in "$PROJECT_ROOT/.github/scripts"/*.sh "$PROJECT_ROOT/tests"/*.sh; do
        if [ -f "$script" ]; then
            script_name="$(basename "$script")"
            run_test "shellcheck: $script_name" "shellcheck '$script'"
        fi
    done
else
    echo -e "${YELLOW}SKIP${NC} - shellcheck not installed"
fi

echo ""
echo "3. YAML SYNTAX VALIDATION"
echo "-------------------------"

# Simple YAML syntax check using bash
# Function is used conditionally based on yamllint availability
# shellcheck disable=SC2329
validate_yaml_syntax() {
    local file="$1"
    # Basic check: file is readable and not empty
    if [ ! -s "$file" ]; then
        return 1
    fi
    # Check for basic YAML structure (starts with --- or has key: value)
    if grep -qE '^(---|[a-zA-Z_][a-zA-Z0-9_-]*:)' "$file"; then
        return 0
    fi
    return 1
}

if command -v yamllint > /dev/null 2>&1; then
    # Use yamllint if available
    for yaml_file in "$PROJECT_ROOT"/.github/**/*.yml "$PROJECT_ROOT"/knowledge-base/**/*.yml; do
        if [ -f "$yaml_file" ]; then
            yaml_name="$(basename "$yaml_file")"
            run_test "yamllint: $yaml_name" "yamllint -d relaxed '$yaml_file'"
        fi
    done
else
    # Fallback to basic syntax check
    echo -e "${YELLOW}INFO${NC} - yamllint not installed, using basic checks"
    for yaml_file in "$PROJECT_ROOT"/.github/ISSUE_TEMPLATE/*.yml \
                     "$PROJECT_ROOT"/.github/workflows/*.yml \
                     "$PROJECT_ROOT"/knowledge-base/**/*.yml; do
        if [ -f "$yaml_file" ]; then
            yaml_name="$(basename "$yaml_file")"
            run_test "YAML syntax: $yaml_name" "validate_yaml_syntax '$yaml_file'"
        fi
    done
fi

echo ""
echo "4. WORKFLOW STRUCTURE VALIDATION"
echo "---------------------------------"

# Check workflows have required fields
# Function is used in the loop below
# shellcheck disable=SC2329
check_workflow_field() {
    local workflow="$1"
    local field="$2"
    grep -q "^$field:" "$workflow"
}

for workflow in "$PROJECT_ROOT"/.github/workflows/*.yml; do
    if [ -f "$workflow" ]; then
        workflow_name="$(basename "$workflow")"
        run_test "$workflow_name has 'name'" "check_workflow_field '$workflow' 'name'"
        run_test "$workflow_name has 'on'" "check_workflow_field '$workflow' 'on'"
        run_test "$workflow_name has 'jobs'" "check_workflow_field '$workflow' 'jobs'"
    fi
done

echo ""
echo "5. SCRIPT EXECUTABILITY"
echo "-----------------------"

for script in "$PROJECT_ROOT"/.github/scripts/*.sh; do
    if [ -f "$script" ]; then
        script_name="$(basename "$script")"
        run_test "$script_name is executable" "test -x '$script'"
        run_test "$script_name has shebang" "grep -q '^#!/' '$script'"
    fi
done

echo ""
echo "6. KNOWLEDGE BASE STRUCTURE"
echo "---------------------------"

# Check KB has required sections
run_test "KB has categories" "grep -q 'categories:' '$PROJECT_ROOT/knowledge-base/index.yml'"
run_test "KB has metadata" "grep -q 'metadata:' '$PROJECT_ROOT/knowledge-base/index.yml'"

# Check each KB file has required fields
for kb_file in "$PROJECT_ROOT"/knowledge-base/**/*.yml; do
    if [ -f "$kb_file" ] && [ "$kb_file" != "$PROJECT_ROOT/knowledge-base/index.yml" ]; then
        kb_name="$(basename "$kb_file" .yml)"
        run_test "KB $kb_name has content" "test -s '$kb_file'"
    fi
done

echo ""
echo "7. DOCUMENTATION COMPLETENESS"
echo "-----------------------------"

# Check docs have required sections
run_test "Workflow doc has usage" "grep -qi 'usage' '$PROJECT_ROOT/docs/COPILOT_WORKFLOW.md'"
run_test "Workflow doc has examples" "grep -qi 'example' '$PROJECT_ROOT/docs/COPILOT_WORKFLOW.md'"
run_test "KB doc has maintenance" "grep -qi 'maintenance' '$PROJECT_ROOT/docs/KNOWLEDGE_BASE.md'"

echo ""
echo "================================================"
echo "  VALIDATION SUMMARY"
echo "================================================"
echo ""
echo "Total tests:  $TOTAL_TESTS"
echo -e "Passed:       ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed:       ${RED}$FAILED_TESTS${NC}"
echo ""

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}✓ All validation checks passed!${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}✗ Some validation checks failed${NC}"
    echo ""
    echo "Please review the failures above and fix the issues."
    exit 1
fi
