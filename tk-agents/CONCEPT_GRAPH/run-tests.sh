#!/usr/bin/env bash
# Test Runner for Concept Graph Web Server
# Runs Playwright tests with proper server setup

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Concept Graph Web Server - Test Suite${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# Change to CONCEPT_GRAPH directory
cd "$(dirname "$0")"

# Install Playwright browsers if needed
if [ ! -d "$HOME/.cache/ms-playwright" ]; then
    echo -e "${BLUE}Installing Playwright browsers...${NC}"
    bunx playwright install chromium
fi

# Run tests
echo -e "${GREEN}Running Playwright tests...${NC}"
echo ""

# Run Playwright test command from parent directory
cd ..
bunx playwright test --config=CONCEPT_GRAPH/playwright.config.ts "$@"

TEST_EXIT_CODE=$?

echo ""
if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
else
    echo -e "${RED}✗ Some tests failed${NC}"
    echo ""
    echo -e "${BLUE}View detailed report:${NC}"
    echo "  bunx playwright show-report"
fi

exit $TEST_EXIT_CODE
