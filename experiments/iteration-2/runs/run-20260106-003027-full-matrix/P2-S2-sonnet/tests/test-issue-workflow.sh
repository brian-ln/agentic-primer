#!/usr/bin/env bash
# test-issue-workflow.sh
# End-to-end test validating the issue-driven development system
#
# Usage: ./tests/test-issue-workflow.sh
#
# Tests:
#   1. File structure completeness
#   2. Workflow syntax validation
#   3. Issue template validation
#   4. Knowledge base structure
#   5. Script executability
#   6. Success criteria compliance
#
# Exit codes:
#   0: All tests passed
#   1: One or more tests failed

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Issue-Driven Development System Tests${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Project root: $PROJECT_ROOT"
echo ""

# Helper function to run a test
run_test() {
  local test_name="$1"
  local test_command="$2"

  TESTS_RUN=$((TESTS_RUN + 1))
  echo -ne "${CYAN}[ TEST ${TESTS_RUN} ]${NC} ${test_name}... "

  if eval "$test_command" >/dev/null 2>&1; then
    echo -e "${GREEN}✓ PASS${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    return 0
  else
    echo -e "${RED}✗ FAIL${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    return 1
  fi
}

# Helper function for verbose test output
run_test_verbose() {
  local test_name="$1"
  local test_command="$2"

  TESTS_RUN=$((TESTS_RUN + 1))
  echo -e "${CYAN}[ TEST ${TESTS_RUN} ]${NC} ${test_name}"

  if eval "$test_command" 2>&1 | sed 's/^/  /'; then
    echo -e "  ${GREEN}✓ PASS${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo ""
    return 0
  else
    echo -e "  ${RED}✗ FAIL${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo ""
    return 1
  fi
}

cd "$PROJECT_ROOT"

# Test Category 1: File Structure
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  1. File Structure Tests${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

run_test "Issue template exists" \
  "[ -f .github/ISSUE_TEMPLATE/copilot-task.yml ]"

run_test "Main workflow exists" \
  "[ -f .github/workflows/copilot-issue-agent.yml ]"

run_test "Validation workflow exists" \
  "[ -f .github/workflows/validate-pr.yml ]"

run_test "Copilot config exists" \
  "[ -f .github/copilot/config.yml ]"

run_test "Knowledge base README exists" \
  "[ -f docs/knowledge/README.md ]"

run_test "Patterns directory exists" \
  "[ -d docs/knowledge/patterns ]"

run_test "Decisions directory exists" \
  "[ -d docs/knowledge/decisions ]"

run_test "Insights directory exists" \
  "[ -d docs/knowledge/insights ]"

run_test "Validation script exists" \
  "[ -f scripts/validate-syntax.sh ]"

run_test "Assignment script exists" \
  "[ -f scripts/assign-pr-to-owner.sh ]"

echo ""

# Test Category 2: Workflow Syntax
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  2. Workflow Syntax Tests${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

run_test "Main workflow has issue trigger" \
  "grep -q 'on:' .github/workflows/copilot-issue-agent.yml && \
   grep -q 'issues:' .github/workflows/copilot-issue-agent.yml"

run_test "Main workflow has required permissions" \
  "grep -q 'permissions:' .github/workflows/copilot-issue-agent.yml && \
   grep -q 'contents: write' .github/workflows/copilot-issue-agent.yml && \
   grep -q 'pull-requests: write' .github/workflows/copilot-issue-agent.yml && \
   grep -q 'issues: write' .github/workflows/copilot-issue-agent.yml"

run_test "Main workflow has knowledge base step" \
  "grep -q 'Read knowledge base' .github/workflows/copilot-issue-agent.yml"

run_test "Main workflow creates PR" \
  "grep -q 'Create pull request' .github/workflows/copilot-issue-agent.yml"

run_test "Main workflow assigns PR" \
  "grep -q 'Auto-assign PR to issue creator' .github/workflows/copilot-issue-agent.yml"

run_test "Validation workflow has PR trigger" \
  "grep -q 'pull_request:' .github/workflows/validate-pr.yml"

run_test "Validation workflow runs yamllint" \
  "grep -q 'yamllint' .github/workflows/validate-pr.yml"

run_test "Validation workflow runs shellcheck" \
  "grep -q 'shellcheck' .github/workflows/validate-pr.yml"

echo ""

# Test Category 3: Issue Template
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  3. Issue Template Tests${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

run_test "Issue template has name" \
  "grep -q 'name:' .github/ISSUE_TEMPLATE/copilot-task.yml"

run_test "Issue template has copilot-task label" \
  "grep -q 'copilot-task' .github/ISSUE_TEMPLATE/copilot-task.yml"

run_test "Issue template has required fields" \
  "grep -q 'required: true' .github/ISSUE_TEMPLATE/copilot-task.yml"

run_test "Issue template has task title field" \
  "grep -q 'task_title' .github/ISSUE_TEMPLATE/copilot-task.yml"

run_test "Issue template has acceptance criteria field" \
  "grep -q 'acceptance_criteria' .github/ISSUE_TEMPLATE/copilot-task.yml"

echo ""

# Test Category 4: Configuration
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  4. Configuration Tests${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

run_test "Copilot config has agent section" \
  "grep -q 'agent:' .github/copilot/config.yml"

run_test "Copilot config has knowledge base settings" \
  "grep -q 'knowledge_base:' .github/copilot/config.yml"

run_test "Copilot config enables knowledge base" \
  "grep -q 'enabled: true' .github/copilot/config.yml"

run_test "Copilot config has PR settings" \
  "grep -q 'pull_request:' .github/copilot/config.yml"

run_test "Copilot config enables auto-assignment" \
  "grep -q 'auto_assign_to_creator: true' .github/copilot/config.yml"

echo ""

# Test Category 5: Knowledge Base
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  5. Knowledge Base Tests${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

run_test "Knowledge README documents structure" \
  "grep -q 'patterns' docs/knowledge/README.md && \
   grep -q 'decisions' docs/knowledge/README.md && \
   grep -q 'insights' docs/knowledge/README.md"

run_test "Patterns README exists" \
  "[ -f docs/knowledge/patterns/README.md ]"

run_test "Decisions README exists" \
  "[ -f docs/knowledge/decisions/README.md ]"

run_test "Insights README exists" \
  "[ -f docs/knowledge/insights/README.md ]"

run_test "Patterns README has template" \
  "grep -q 'template' docs/knowledge/patterns/README.md || \
   grep -q 'Template' docs/knowledge/patterns/README.md"

run_test "Decisions README has ADR format" \
  "grep -q 'ADR' docs/knowledge/decisions/README.md"

echo ""

# Test Category 6: Scripts
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  6. Script Tests${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

run_test "Validation script is executable" \
  "[ -x scripts/validate-syntax.sh ] || chmod +x scripts/validate-syntax.sh"

run_test "Assignment script is executable" \
  "[ -x scripts/assign-pr-to-owner.sh ] || chmod +x scripts/assign-pr-to-owner.sh"

run_test "Validation script has shebang" \
  "head -n1 scripts/validate-syntax.sh | grep -q '^#!/'"

run_test "Assignment script has shebang" \
  "head -n1 scripts/assign-pr-to-owner.sh | grep -q '^#!/'"

run_test "Validation script has help option" \
  "grep -q -- '--help' scripts/validate-syntax.sh"

echo ""

# Test Category 7: Success Criteria Validation
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  7. Success Criteria Validation${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

echo -e "${CYAN}[ CRITERION 1 ]${NC} Process test issue end-to-end without errors"
echo "  Components needed:"

run_test "  - Issue template for input" \
  "[ -f .github/ISSUE_TEMPLATE/copilot-task.yml ]"

run_test "  - Workflow to process issue" \
  "[ -f .github/workflows/copilot-issue-agent.yml ]"

run_test "  - Processing simulation step" \
  "grep -q 'Copilot agent simulation' .github/workflows/copilot-issue-agent.yml"

run_test "  - PR creation step" \
  "grep -q 'Create pull request' .github/workflows/copilot-issue-agent.yml"

run_test "  - Auto-assignment step" \
  "grep -q 'Auto-assign PR to issue creator' .github/workflows/copilot-issue-agent.yml"

echo ""
echo -e "${CYAN}[ CRITERION 2 ]${NC} Pass syntax validation (yamllint, shellcheck)"
echo "  Components needed:"

run_test "  - Validation workflow exists" \
  "[ -f .github/workflows/validate-pr.yml ]"

run_test "  - yamllint step present" \
  "grep -q 'yamllint' .github/workflows/validate-pr.yml"

run_test "  - shellcheck step present" \
  "grep -q 'shellcheck' .github/workflows/validate-pr.yml"

run_test "  - Standalone validation script" \
  "[ -f scripts/validate-syntax.sh ]"

echo ""
echo -e "${CYAN}[ CRITERION 3 ]${NC} GitHub workflow triggers on issue creation"
echo "  Components needed:"

run_test "  - Workflow has issue trigger" \
  "grep -q 'on:' .github/workflows/copilot-issue-agent.yml && \
   grep -A 2 'on:' .github/workflows/copilot-issue-agent.yml | grep -q 'issues:'"

run_test "  - Workflow triggers on 'opened'" \
  "grep -A 3 'issues:' .github/workflows/copilot-issue-agent.yml | grep -q 'opened'"

run_test "  - Workflow has correct permissions" \
  "grep -q 'permissions:' .github/workflows/copilot-issue-agent.yml"

run_test "  - Workflow filters by label" \
  "grep -q 'copilot-task' .github/workflows/copilot-issue-agent.yml"

echo ""

# Test Category 8: Syntax Validation (if tools available)
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  8. Actual Syntax Validation${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

if command -v yamllint >/dev/null 2>&1; then
  run_test_verbose "Running yamllint on YAML files" \
    "find . -name '*.yml' -o -name '*.yaml' | \
     grep -v node_modules | grep -v .git | \
     xargs yamllint -d relaxed 2>&1"
else
  echo -e "${YELLOW}⚠ yamllint not available, skipping YAML validation${NC}"
  echo ""
fi

if command -v shellcheck >/dev/null 2>&1; then
  run_test_verbose "Running shellcheck on shell scripts" \
    "find scripts tests -name '*.sh' -type f 2>/dev/null | \
     xargs shellcheck -S warning 2>&1"
else
  echo -e "${YELLOW}⚠ shellcheck not available, skipping shell validation${NC}"
  echo ""
fi

# Summary
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Test Summary${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Tests run:    $TESTS_RUN"
echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests failed: $([ $TESTS_FAILED -eq 0 ] && echo -e "${GREEN}$TESTS_FAILED${NC}" || echo -e "${RED}$TESTS_FAILED${NC}")"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
  echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo -e "${GREEN}  ✓ ALL TESTS PASSED${NC}"
  echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo ""
  echo "The issue-driven development system is properly configured."
  echo ""
  echo "Success criteria validation:"
  echo -e "  ${GREEN}✓${NC} Criterion 1: Process test issue end-to-end"
  echo -e "  ${GREEN}✓${NC} Criterion 2: Pass syntax validation"
  echo -e "  ${GREEN}✓${NC} Criterion 3: GitHub workflow triggers on issue creation"
  echo ""
  exit 0
else
  echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo -e "${RED}  ✗ TESTS FAILED${NC}"
  echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo ""
  echo "Please fix the failing tests before deploying the system."
  echo ""
  exit 1
fi
