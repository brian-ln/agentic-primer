#!/bin/bash
# Validation script for issue-driven development system
# Verifies syntax, structure, and basic functionality

set -euo pipefail

OUTPUT_DIR="/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S2-sonnet"
cd "$OUTPUT_DIR"

echo "========================================="
echo "Issue-Driven Development System Validation"
echo "========================================="
echo ""

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASS=0
FAIL=0

# Test counter
test_num=0

# Helper function for tests
run_test() {
    local test_name="$1"
    local test_command="$2"

    test_num=$((test_num + 1))
    echo -n "[$test_num] $test_name ... "

    if eval "$test_command" > /tmp/test-output-$test_num.log 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        PASS=$((PASS + 1))
        return 0
    else
        echo -e "${RED}FAIL${NC}"
        echo "    Error output:"
        sed 's/^/    /' /tmp/test-output-$test_num.log
        FAIL=$((FAIL + 1))
        return 1
    fi
}

echo "=== File Structure Tests ==="
echo ""

run_test "Issue template exists" \
    "test -f .github/ISSUE_TEMPLATE/task.yml"

run_test "CODEOWNERS exists" \
    "test -f .github/CODEOWNERS"

run_test "Workflow exists" \
    "test -f .github/workflows/issue-assignment.yml"

run_test "README exists" \
    "test -f README.md"

run_test "Knowledge base - patterns" \
    "test -f docs/knowledge/patterns/README.md"

run_test "Knowledge base - decisions" \
    "test -f docs/knowledge/decisions/README.md"

run_test "Knowledge base - insights" \
    "test -f docs/knowledge/insights/README.md"

echo ""
echo "=== YAML Syntax Validation ==="
echo ""

# Check if yamllint is available
if command -v yamllint &> /dev/null; then
    run_test "Issue template YAML syntax" \
        "yamllint -d relaxed .github/ISSUE_TEMPLATE/task.yml"

    run_test "Workflow YAML syntax" \
        "yamllint -d relaxed .github/workflows/issue-assignment.yml"
else
    echo -e "${YELLOW}SKIP${NC} yamllint not installed (install with: pip install yamllint)"
    # Fallback: basic YAML check with python
    if command -v python3 &> /dev/null; then
        run_test "Issue template basic YAML check" \
            "python3 -c \"import yaml; yaml.safe_load(open('.github/ISSUE_TEMPLATE/task.yml'))\""

        run_test "Workflow basic YAML check" \
            "python3 -c \"import yaml; yaml.safe_load(open('.github/workflows/issue-assignment.yml'))\""
    fi
fi

echo ""
echo "=== Content Validation ==="
echo ""

run_test "Issue template has required fields" \
    "grep -q 'name:' .github/ISSUE_TEMPLATE/task.yml && \
     grep -q 'description:' .github/ISSUE_TEMPLATE/task.yml && \
     grep -q 'assignees:' .github/ISSUE_TEMPLATE/task.yml && \
     grep -q 'body:' .github/ISSUE_TEMPLATE/task.yml"

run_test "Issue template assigns to copilot" \
    "grep -q 'copilot' .github/ISSUE_TEMPLATE/task.yml"

run_test "CODEOWNERS has owner assignment" \
    "grep -q '@owner' .github/CODEOWNERS"

run_test "Workflow has proper triggers" \
    "grep -q 'issues:' .github/workflows/issue-assignment.yml && \
     grep -q 'types:' .github/workflows/issue-assignment.yml"

run_test "Workflow creates PR" \
    "grep -q 'pulls.create' .github/workflows/issue-assignment.yml"

run_test "README has workflow diagram" \
    "grep -q 'Workflow' README.md"

run_test "README has quick start" \
    "grep -q 'Quick Start' README.md"

echo ""
echo "=== Structure Validation ==="
echo ""

run_test "Knowledge base structure complete" \
    "test -d docs/knowledge/patterns && \
     test -d docs/knowledge/decisions && \
     test -d docs/knowledge/insights"

run_test "All knowledge READMEs non-empty" \
    "test -s docs/knowledge/patterns/README.md && \
     test -s docs/knowledge/decisions/README.md && \
     test -s docs/knowledge/insights/README.md"

echo ""
echo "=== Workflow Logic Validation ==="
echo ""

run_test "Workflow has conditional execution" \
    "grep -q 'if:' .github/workflows/issue-assignment.yml"

run_test "Workflow adds labels" \
    "grep -q 'addLabels' .github/workflows/issue-assignment.yml"

run_test "Workflow creates comments" \
    "grep -q 'createComment' .github/workflows/issue-assignment.yml"

run_test "Workflow extracts metadata" \
    "grep -q 'task-metadata' .github/workflows/issue-assignment.yml"

run_test "Workflow creates feature branch" \
    "grep -q 'copilot/issue-' .github/workflows/issue-assignment.yml"

echo ""
echo "=== Documentation Completeness ==="
echo ""

run_test "README has examples" \
    "grep -q 'Example' README.md"

run_test "README has troubleshooting" \
    "grep -q 'Troubleshooting' README.md"

run_test "README has validation section" \
    "grep -q 'Validation' README.md"

run_test "Patterns README has format template" \
    "grep -q 'Pattern Format' docs/knowledge/patterns/README.md"

run_test "Decisions README has ADR format" \
    "grep -q 'ADR Format' docs/knowledge/decisions/README.md"

run_test "Insights README has format guide" \
    "grep -q 'Insight Format' docs/knowledge/insights/README.md"

echo ""
echo "========================================="
echo "Validation Summary"
echo "========================================="
echo ""
echo -e "Tests passed: ${GREEN}$PASS${NC}"
echo -e "Tests failed: ${RED}$FAIL${NC}"
echo -e "Total tests:  $test_num"
echo ""

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}✓ All validation tests passed!${NC}"
    echo ""
    echo "System is ready for deployment."
    exit 0
else
    echo -e "${RED}✗ Some validation tests failed.${NC}"
    echo ""
    echo "Please review the errors above and fix before deployment."
    exit 1
fi
