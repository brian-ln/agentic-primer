#!/usr/bin/env bash
# Background Test Runner for Concept Graph Web Server
# Runs Playwright tests in background with output capture

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Change to CONCEPT_GRAPH directory
cd "$(dirname "$0")"
CONCEPT_GRAPH_DIR="$(pwd)"
LOG_DIR="$CONCEPT_GRAPH_DIR/test-logs"
LOG_FILE="$LOG_DIR/test-output-$(date +%Y%m%d-%H%M%S).log"
LATEST_LINK="$LOG_DIR/latest.log"

# Create log directory
mkdir -p "$LOG_DIR"

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Concept Graph - Background Test Runner${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "${GREEN}Starting tests in background...${NC}"
echo -e "${YELLOW}Log file: $LOG_FILE${NC}"
echo ""
echo -e "Watch output with:"
echo -e "  ${BLUE}./watch-tests.sh${NC}"
echo ""

# Install Playwright browsers if needed (quietly)
if [ ! -d "$HOME/.cache/ms-playwright" ]; then
    echo -e "${BLUE}Installing Playwright browsers...${NC}"
    bunx playwright install chromium > "$LOG_FILE" 2>&1
fi

# Run tests in background
cd ..
{
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "Test Run Started: $(date)"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""

    bunx playwright test --config=CONCEPT_GRAPH/playwright.config.ts "$@"
    TEST_EXIT_CODE=$?

    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    if [ $TEST_EXIT_CODE -eq 0 ]; then
        echo "✓ All tests passed!"
    else
        echo "✗ Some tests failed (exit code: $TEST_EXIT_CODE)"
    fi
    echo "Test Run Completed: $(date)"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    exit $TEST_EXIT_CODE
} > "$LOG_FILE" 2>&1 &

TEST_PID=$!

# Create symlink to latest log
rm -f "$LATEST_LINK"
ln -s "$LOG_FILE" "$LATEST_LINK"

echo -e "${GREEN}✓ Tests running in background (PID: $TEST_PID)${NC}"
echo ""
echo -e "Commands:"
echo -e "  Watch output: ${BLUE}./watch-tests.sh${NC}"
echo -e "  View log:     ${BLUE}less $LOG_FILE${NC}"
echo -e "  Kill tests:   ${BLUE}kill $TEST_PID${NC}"
echo ""

# Store PID for reference
echo "$TEST_PID" > "$LOG_DIR/test-runner.pid"
