#!/usr/bin/env bash
# Session Knowledge System - Test Watch Mode
# Epic: agentic-primer-t49.5
#
# Watch mode for development - runs tests on file changes
# Uses bun's built-in watch capability with inotify

set -e

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FILTER=""
CLEAR_SCREEN=1

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --filter|-f)
      FILTER="$2"
      shift 2
      ;;
    --no-clear)
      CLEAR_SCREEN=0
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --filter, -f <pattern>  Filter tests by name pattern"
      echo "  --no-clear              Don't clear screen on each run"
      echo "  --help, -h              Show this help"
      echo ""
      echo "Examples:"
      echo "  $0                             # Watch all tests"
      echo "  $0 --filter cognitive          # Watch cognitive tests only"
      echo "  $0 --filter ConfidenceDecay    # Watch specific tests"
      exit 0
      ;;
    *)
      echo -e "${RED}Unknown option: $1${NC}"
      exit 1
      ;;
  esac
done

# Navigate to project root
cd "$PROJECT_ROOT"

# Header
if [[ $CLEAR_SCREEN -eq 1 ]]; then
  clear
fi

echo -e "${BLUE}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  Session Knowledge System - Test Watch Mode             ║${NC}"
echo -e "${BLUE}║  Epic: agentic-primer-t49.5                              ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check environment
if ! command -v bun &> /dev/null; then
  echo -e "${RED}✗ Bun not found. Please install: https://bun.sh${NC}"
  exit 1
fi

echo -e "${GREEN}✓ Bun $(bun --version) found${NC}"
echo ""

# Build watch command
WATCH_CMD="bun test --watch"

if [[ -n "$FILTER" ]]; then
  WATCH_CMD="$WATCH_CMD --test-name-pattern=\"$FILTER\""
  echo -e "${CYAN}Watching tests matching: ${YELLOW}$FILTER${NC}"
else
  echo -e "${CYAN}Watching all tests${NC}"
fi

echo ""
echo -e "${CYAN}Watching for file changes...${NC}"
echo -e "${CYAN}Press Ctrl+C to stop${NC}"
echo ""
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# Alternative implementation: Manual watch using inotifywait (if bun --watch doesn't work)
# This is a fallback implementation

if command -v fswatch &> /dev/null; then
  # macOS/Linux with fswatch
  echo -e "${CYAN}Using fswatch for file monitoring${NC}"
  echo ""

  # Function to run tests
  run_tests() {
    if [[ $CLEAR_SCREEN -eq 1 ]]; then
      clear
    fi

    echo -e "${CYAN}[$(date '+%H:%M:%S')] Running tests...${NC}"
    echo ""

    # Build test command
    TEST_CMD="bun test"
    if [[ -n "$FILTER" ]]; then
      TEST_CMD="$TEST_CMD --test-name-pattern=\"$FILTER\""
    fi

    # Run tests
    eval "$TEST_CMD" || true

    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${CYAN}Waiting for changes...${NC}"
  }

  # Initial run
  run_tests

  # Watch for changes
  fswatch -r \
    --exclude="node_modules" \
    --exclude=".git" \
    --exclude=".coverage" \
    --exclude="*.md" \
    --event Created \
    --event Updated \
    --event Removed \
    src/ | while read -r file; do
      echo -e "${YELLOW}[$(date '+%H:%M:%S')] File changed: $file${NC}"
      run_tests
    done

elif command -v inotifywait &> /dev/null; then
  # Linux with inotify-tools
  echo -e "${CYAN}Using inotifywait for file monitoring${NC}"
  echo ""

  # Function to run tests
  run_tests() {
    if [[ $CLEAR_SCREEN -eq 1 ]]; then
      clear
    fi

    echo -e "${CYAN}[$(date '+%H:%M:%S')] Running tests...${NC}"
    echo ""

    # Build test command
    TEST_CMD="bun test"
    if [[ -n "$FILTER" ]]; then
      TEST_CMD="$TEST_CMD --test-name-pattern=\"$FILTER\""
    fi

    # Run tests
    eval "$TEST_CMD" || true

    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${CYAN}Waiting for changes...${NC}"
  }

  # Initial run
  run_tests

  # Watch for changes
  while true; do
    inotifywait -r -e modify,create,delete \
      --exclude '(node_modules|\.git|\.coverage|.*\.md)' \
      src/

    echo -e "${YELLOW}[$(date '+%H:%M:%S')] File changed${NC}"
    run_tests
  done

else
  # Fallback: Use bun's built-in watch (if available) or simple loop
  echo -e "${CYAN}Using bun's built-in watch${NC}"
  echo ""

  # Note: Bun test --watch may not be available in all versions
  # This will either work or show an error
  eval "$WATCH_CMD" || {
    echo ""
    echo -e "${RED}✗ Watch mode not available${NC}"
    echo ""
    echo -e "${YELLOW}Install fswatch (macOS) or inotify-tools (Linux):${NC}"
    echo -e "${YELLOW}  macOS:  brew install fswatch${NC}"
    echo -e "${YELLOW}  Linux:  apt-get install inotify-tools${NC}"
    echo ""
    echo -e "${YELLOW}Or use nodemon:${NC}"
    echo -e "${YELLOW}  npm install -g nodemon${NC}"
    echo -e "${YELLOW}  nodemon --exec 'bun test' --watch src/${NC}"
    exit 1
  }
fi
