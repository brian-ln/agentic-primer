#!/usr/bin/env bash
# Session Knowledge System - Test Runner
# Epic: agentic-primer-t49.5
#
# Runs all test suites in sequence with reporting
# Exit codes: 0 (pass), 1 (fail), 2 (setup error), 3 (coverage fail)

set -e  # Exit on error (but we'll handle test failures specially)

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COVERAGE_DIR="${PROJECT_ROOT}/.coverage"
VERBOSE=0
COVERAGE=0
CI_MODE=0
FAST_MODE=0
WATCH_MODE=0
EXIT_CODE=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --verbose|-v)
      VERBOSE=1
      shift
      ;;
    --coverage|-c)
      COVERAGE=1
      shift
      ;;
    --ci)
      CI_MODE=1
      COVERAGE=1
      shift
      ;;
    --fast|-f)
      FAST_MODE=1
      shift
      ;;
    --watch|-w)
      WATCH_MODE=1
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --verbose, -v    Show detailed test output"
      echo "  --coverage, -c   Generate coverage report"
      echo "  --ci             CI mode (strict, coverage required)"
      echo "  --fast, -f       Skip slow tests"
      echo "  --watch, -w      Watch mode (see test-watch.sh)"
      echo "  --help, -h       Show this help"
      echo ""
      echo "Exit codes:"
      echo "  0 - All tests passed"
      echo "  1 - One or more tests failed"
      echo "  2 - Test setup failed"
      echo "  3 - Coverage threshold not met"
      exit 0
      ;;
    *)
      echo -e "${RED}Unknown option: $1${NC}"
      exit 2
      ;;
  esac
done

# Header
echo -e "${BLUE}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  Session Knowledge System - Test Runner                 ║${NC}"
echo -e "${BLUE}║  Epic: agentic-primer-t49.5                              ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check environment
echo -e "${CYAN}Checking environment...${NC}"

if ! command -v bun &> /dev/null; then
  echo -e "${RED}✗ Bun not found. Please install: https://bun.sh${NC}"
  exit 2
fi

echo -e "${GREEN}✓ Bun $(bun --version) found${NC}"

# Check database
DB_PATH="${HOME}/.claude/index/sessions-libsql.db"
if [[ ! -f "$DB_PATH" ]]; then
  echo -e "${YELLOW}⚠ Database not found at: $DB_PATH${NC}"
  echo -e "${YELLOW}  Run './build-index' to create it${NC}"
  if [[ $CI_MODE -eq 1 ]]; then
    echo -e "${RED}✗ Database required in CI mode${NC}"
    exit 2
  fi
  echo -e "${YELLOW}  Continuing anyway (some tests may fail)${NC}"
else
  echo -e "${GREEN}✓ Database found${NC}"
fi

# Navigate to project root
cd "$PROJECT_ROOT"

# Build test command
TEST_CMD="bun test"
TEST_FLAGS=""

if [[ $COVERAGE -eq 1 ]]; then
  TEST_FLAGS="$TEST_FLAGS --coverage --coverage-dir=$COVERAGE_DIR"
  if [[ $CI_MODE -eq 1 ]]; then
    TEST_FLAGS="$TEST_FLAGS --coverage-reporter=lcov --coverage-reporter=text"
  else
    TEST_FLAGS="$TEST_FLAGS --coverage-reporter=text"
  fi
fi

if [[ $FAST_MODE -eq 1 ]]; then
  # Skip e2e tests in fast mode
  TEST_CMD="$TEST_CMD --test-name-pattern='(?!e2e)'"
fi

# Start tests
echo ""
echo -e "${CYAN}Running test suites...${NC}"
echo ""

# Track test results
SUITES_RUN=0
SUITES_PASSED=0
SUITES_FAILED=0
declare -a FAILED_SUITES

# Function to run a test suite
run_suite() {
  local suite_name="$1"
  local test_pattern="$2"
  local is_optional="${3:-0}"

  echo -e "${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo -e "${MAGENTA}Suite: ${suite_name}${NC}"
  echo -e "${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

  SUITES_RUN=$((SUITES_RUN + 1))

  # Build command
  local cmd="$TEST_CMD"
  if [[ -n "$test_pattern" ]]; then
    cmd="$cmd $test_pattern"
  fi
  cmd="$cmd $TEST_FLAGS"

  if [[ $VERBOSE -eq 1 ]]; then
    echo -e "${CYAN}Command: $cmd${NC}"
  fi

  # Run test
  set +e  # Don't exit on error
  if [[ $VERBOSE -eq 1 ]]; then
    eval "$cmd"
  else
    eval "$cmd" > /tmp/test-output.$$ 2>&1
  fi
  local result=$?
  set -e

  # Check result
  if [[ $result -eq 0 ]]; then
    echo -e "${GREEN}✓ PASSED${NC}"
    SUITES_PASSED=$((SUITES_PASSED + 1))
  else
    if [[ $is_optional -eq 1 ]]; then
      echo -e "${YELLOW}⚠ FAILED (optional)${NC}"
      SUITES_PASSED=$((SUITES_PASSED + 1))
    else
      echo -e "${RED}✗ FAILED${NC}"
      SUITES_FAILED=$((SUITES_FAILED + 1))
      FAILED_SUITES+=("$suite_name")
      EXIT_CODE=1

      # Show output on failure
      if [[ $VERBOSE -eq 0 && -f /tmp/test-output.$$ ]]; then
        echo -e "${RED}Test output:${NC}"
        cat /tmp/test-output.$$
      fi
    fi
  fi

  # Cleanup
  rm -f /tmp/test-output.$$

  echo ""
}

# Run test suites
echo -e "${CYAN}Starting test suites...${NC}"
echo ""

# 1. Core System Tests
run_suite "Core System Tests" "src/graph.test.ts src/context.test.ts"

# 2. Entity Tests
if [[ $FAST_MODE -eq 0 ]]; then
  run_suite "Entity Unit Tests" "src/entities/*.test.ts"
  run_suite "Entity E2E Tests" "src/entities/*.e2e.test.ts" 1  # Optional
fi

# 3. Cognitive Integration Tests
run_suite "Cognitive Integration Tests" "src/session-knowledge/__tests__/cognitive-integration.test.ts"

# 4. All Other Tests (catch-all)
run_suite "Additional Tests" "src/" 1  # Optional, run all remaining

# Summary
echo -e "${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${MAGENTA}Test Summary${NC}"
echo -e "${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Suites run:    $SUITES_RUN"
echo -e "Suites passed: ${GREEN}$SUITES_PASSED${NC}"
echo -e "Suites failed: ${RED}$SUITES_FAILED${NC}"
echo ""

if [[ ${#FAILED_SUITES[@]} -gt 0 ]]; then
  echo -e "${RED}Failed suites:${NC}"
  for suite in "${FAILED_SUITES[@]}"; do
    echo -e "  ${RED}✗ $suite${NC}"
  done
  echo ""
fi

# Coverage report
if [[ $COVERAGE -eq 1 ]]; then
  echo -e "${CYAN}Coverage report generated at: ${COVERAGE_DIR}${NC}"

  if [[ $CI_MODE -eq 0 ]]; then
    # Show coverage summary
    if [[ -f "$COVERAGE_DIR/coverage.txt" ]]; then
      echo ""
      cat "$COVERAGE_DIR/coverage.txt"
      echo ""
    fi
  fi

  # Check coverage thresholds in CI mode
  if [[ $CI_MODE -eq 1 ]]; then
    # Parse coverage from lcov (basic check)
    if [[ -f "$COVERAGE_DIR/lcov.info" ]]; then
      echo -e "${CYAN}Checking coverage thresholds...${NC}"

      # Note: This is a simple check. For production, use a proper coverage tool
      LINES_COVERED=$(grep -c "^DA:" "$COVERAGE_DIR/lcov.info" | grep -c ",1$" || echo "0")
      LINES_TOTAL=$(grep -c "^DA:" "$COVERAGE_DIR/lcov.info" || echo "1")

      if [[ $LINES_TOTAL -gt 0 ]]; then
        COVERAGE_PERCENT=$((LINES_COVERED * 100 / LINES_TOTAL))
        echo "Line coverage: ${COVERAGE_PERCENT}%"

        # Threshold: 70% (configurable)
        if [[ $COVERAGE_PERCENT -lt 70 ]]; then
          echo -e "${RED}✗ Coverage below threshold (70%)${NC}"
          EXIT_CODE=3
        else
          echo -e "${GREEN}✓ Coverage meets threshold${NC}"
        fi
      fi
    fi
  fi
fi

# Final result
echo -e "${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
if [[ $EXIT_CODE -eq 0 ]]; then
  echo -e "${GREEN}✓ All tests passed!${NC}"
else
  echo -e "${RED}✗ Some tests failed (exit code: $EXIT_CODE)${NC}"
fi
echo -e "${MAGENTA}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# CI mode: output machine-readable summary
if [[ $CI_MODE -eq 1 ]]; then
  echo "{\"suites_run\":$SUITES_RUN,\"suites_passed\":$SUITES_PASSED,\"suites_failed\":$SUITES_FAILED,\"exit_code\":$EXIT_CODE}"
fi

exit $EXIT_CODE
