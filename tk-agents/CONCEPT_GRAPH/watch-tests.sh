#!/usr/bin/env bash
# Watch Test Output for Concept Graph Web Server
# Follows the latest test run output in real-time

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Change to CONCEPT_GRAPH directory
cd "$(dirname "$0")"
LOG_DIR="./test-logs"
LATEST_LINK="$LOG_DIR/latest.log"
PID_FILE="$LOG_DIR/test-runner.pid"

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Concept Graph - Test Output Watcher${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# Check if log directory exists
if [ ! -d "$LOG_DIR" ]; then
    echo -e "${RED}✗ No test logs found${NC}"
    echo -e "  Run ${BLUE}./run-tests-bg.sh${NC} first to start background tests"
    exit 1
fi

# Check if latest log exists
if [ ! -f "$LATEST_LINK" ]; then
    echo -e "${YELLOW}⚠ No active test run found${NC}"
    echo ""
    echo -e "Available logs:"
    ls -lt "$LOG_DIR"/*.log 2>/dev/null | head -5 | while read -r line; do
        echo "  $line"
    done
    echo ""
    echo -e "Run ${BLUE}./run-tests-bg.sh${NC} to start a new test run"
    exit 1
fi

# Check if test runner is still running
if [ -f "$PID_FILE" ]; then
    TEST_PID=$(cat "$PID_FILE")
    if ps -p "$TEST_PID" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ Test runner is active (PID: $TEST_PID)${NC}"
    else
        echo -e "${YELLOW}⚠ Test runner completed${NC}"
    fi
else
    echo -e "${YELLOW}⚠ Test runner status unknown${NC}"
fi

echo -e "${BLUE}Watching: $LATEST_LINK${NC}"
echo -e "Press Ctrl+C to stop watching"
echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# Follow the log file
tail -f "$LATEST_LINK"
