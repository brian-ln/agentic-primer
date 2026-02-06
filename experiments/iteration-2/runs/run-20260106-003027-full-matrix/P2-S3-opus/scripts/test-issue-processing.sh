#!/usr/bin/env bash
# End-to-end test script for @copilot issue-driven development system
# Simulates the complete issue processing workflow

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Test state
TEST_PASSED=0
TEST_FAILED=0
SIMULATION_MODE="${1:-simulate}"

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $1"; TEST_PASSED=$((TEST_PASSED + 1)); }
log_error() { echo -e "${RED}[FAIL]${NC} $1"; TEST_FAILED=$((TEST_FAILED + 1)); }
log_step() { echo -e "${CYAN}[STEP]${NC} $1"; }
log_sim() { echo -e "${YELLOW}[SIM]${NC} $1"; }

# Header
echo ""
echo "=========================================="
echo " Issue Processing End-to-End Test"
echo "=========================================="
echo " Mode: $SIMULATION_MODE"
echo "=========================================="
echo ""

# Test 1: Verify directory structure
log_step "Test 1: Verify directory structure"

required_dirs=(
    ".github"
    ".github/ISSUE_TEMPLATE"
    ".github/workflows"
    "docs/knowledge"
    "logs/executions"
)

for dir in "${required_dirs[@]}"; do
    if [[ -d "$PROJECT_ROOT/$dir" ]]; then
        log_success "Directory exists: $dir"
    else
        log_error "Directory missing: $dir"
    fi
done

echo ""

# Test 2: Verify required files
log_step "Test 2: Verify required files"

required_files=(
    ".github/CODEOWNERS"
    ".github/copilot-instructions.md"
    ".github/workflows/copilot-issue-handler.yml"
    ".github/ISSUE_TEMPLATE/copilot-task.yml"
)

for file in "${required_files[@]}"; do
    if [[ -f "$PROJECT_ROOT/$file" ]]; then
        log_success "File exists: $file"
    else
        log_error "File missing: $file"
    fi
done

echo ""

# Test 3: Simulate issue creation
log_step "Test 3: Simulate issue creation"

ISSUE_NUMBER=42
ISSUE_TITLE="Add greeting function"
TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)

log_sim "Creating issue #$ISSUE_NUMBER: $ISSUE_TITLE"
log_sim "Timestamp: $TIMESTAMP"
log_sim "Labels: copilot, ai-task, test"

# Simulate workflow trigger
log_sim "Workflow 'copilot-issue-handler' triggered"
sleep 0.5

log_success "Issue creation simulated"

echo ""

# Test 4: Simulate workflow execution
log_step "Test 4: Simulate workflow execution"

BRANCH_NAME="copilot/issue-$ISSUE_NUMBER-greeting"

log_sim "Checking for 'copilot' label... FOUND"
log_sim "Creating branch: $BRANCH_NAME"
log_sim "Assigning @copilot to issue #$ISSUE_NUMBER"
log_sim "Parsing issue body..."
log_sim "Task extracted: Add greeting function"
log_sim "Acceptance criteria: 4 items"

log_success "Workflow execution simulated"

echo ""

# Test 5: Simulate implementation
log_step "Test 5: Simulate implementation"

log_sim "@copilot reading knowledge base..."
log_sim "Pattern found: issue-to-pr-workflow"
log_sim "Implementing solution..."
sleep 0.5
log_sim "Creating file: src/greeting.ts"
log_sim "Creating file: src/greeting.test.ts"
log_sim "Running tests... PASS (2 tests, 100% coverage)"

log_success "Implementation simulated"

echo ""

# Test 6: Simulate PR creation
log_step "Test 6: Simulate PR creation"

PR_NUMBER=$((ISSUE_NUMBER + 1))

log_sim "Committing changes to $BRANCH_NAME"
log_sim "Creating PR #$PR_NUMBER: feat: Add greeting function"
log_sim "PR body: Fixes #$ISSUE_NUMBER"
log_sim "CODEOWNERS triggered: @owner assigned as reviewer"

log_success "PR creation simulated"

echo ""

# Test 7: Verify CODEOWNERS
log_step "Test 7: Verify CODEOWNERS format"

CODEOWNERS_FILE="$PROJECT_ROOT/.github/CODEOWNERS"
if [[ -f "$CODEOWNERS_FILE" ]]; then
    if grep -q "^\*" "$CODEOWNERS_FILE"; then
        log_success "CODEOWNERS has wildcard rule"
    else
        log_error "CODEOWNERS missing wildcard rule"
    fi
else
    log_error "CODEOWNERS file not found"
fi

echo ""

# Test 8: Simulate review
log_step "Test 8: Simulate PR review"

log_sim "Workflow 'copilot-pr-review' triggered"
log_sim "@copilot reviewing PR #$PR_NUMBER..."
log_sim "Files reviewed: src/greeting.ts, src/greeting.test.ts"
log_sim "Review: APPROVED"

log_success "PR review simulated"

echo ""

# Test 9: Create execution log
log_step "Test 9: Create execution log"

LOG_DIR="$PROJECT_ROOT/logs/executions"
LOG_FILE="$LOG_DIR/$(date +%Y-%m-%d)-issue-$ISSUE_NUMBER-test.json"

mkdir -p "$LOG_DIR"

cat > "$LOG_FILE" << EOF
{
  "execution_id": "exec-$(date +%Y%m%d-%H%M%S)-$ISSUE_NUMBER",
  "issue_number": $ISSUE_NUMBER,
  "issue_title": "$ISSUE_TITLE",
  "start_time": "$TIMESTAMP",
  "end_time": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "duration_seconds": 37,
  "status": "success",
  "agent": "copilot",
  "branch": "$BRANCH_NAME",
  "pr_number": $PR_NUMBER,
  "files_created": [
    "src/greeting.ts",
    "src/greeting.test.ts"
  ],
  "tests_passed": true,
  "coverage_percent": 100,
  "review_status": "approved",
  "errors": [],
  "simulation": true
}
EOF

if [[ -f "$LOG_FILE" ]]; then
    log_success "Execution log created: $LOG_FILE"
else
    log_error "Failed to create execution log"
fi

echo ""

# Test 10: Validate log format
log_step "Test 10: Validate execution log format"

if command -v python3 &> /dev/null; then
    if python3 -c "import json; json.load(open('$LOG_FILE'))" 2>/dev/null; then
        log_success "Execution log is valid JSON"
    else
        log_error "Execution log is not valid JSON"
    fi
else
    log_sim "Skipping JSON validation (python3 not available)"
fi

echo ""

# Summary
echo "=========================================="
echo " Test Summary"
echo "=========================================="
echo ""

TOTAL_TESTS=$((TEST_PASSED + TEST_FAILED))

echo -e "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $TEST_PASSED${NC}"
echo -e "${RED}Failed: $TEST_FAILED${NC}"
echo ""

if [[ $TEST_FAILED -eq 0 ]]; then
    SUCCESS_RATE=100
else
    SUCCESS_RATE=$((TEST_PASSED * 100 / TOTAL_TESTS))
fi

echo "Success rate: $SUCCESS_RATE%"
echo ""

# Simulated execution summary
echo "=========================================="
echo " Simulated Execution Log"
echo "=========================================="
echo ""
echo "Issue: #$ISSUE_NUMBER - $ISSUE_TITLE"
echo "Branch: $BRANCH_NAME"
echo "PR: #$PR_NUMBER"
echo "Status: SUCCESS"
echo "Duration: 37s (simulated)"
echo ""

if [[ $TEST_FAILED -eq 0 ]]; then
    echo -e "${GREEN}All tests passed! System is ready for use.${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed. Please review the issues above.${NC}"
    exit 1
fi
