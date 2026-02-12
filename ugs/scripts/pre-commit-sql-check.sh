#!/bin/sh
#
# SQL Injection Pre-commit Check
# Runs static analysis on staged TypeScript files to detect SQL injection vulnerabilities
# Epic: agentic-primer-0lg.2
#

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Get the root directory of the git repo
REPO_ROOT=$(git rev-parse --show-toplevel)

# Check if bun is available
if ! command -v bun >/dev/null 2>&1; then
    echo "${YELLOW}Warning: bun not found, skipping SQL injection checks${NC}" >&2
    exit 0
fi

# Check if sql-pattern-checker exists
CHECKER="$REPO_ROOT/src/session-knowledge/security/sql-pattern-checker.ts"
if [ ! -f "$CHECKER" ]; then
    echo "${YELLOW}Warning: sql-pattern-checker.ts not found, skipping SQL checks${NC}" >&2
    exit 0
fi

# Get list of staged TypeScript files
STAGED_TS_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(ts|tsx)$' || true)

if [ -z "$STAGED_TS_FILES" ]; then
    # No TypeScript files staged, skip check
    exit 0
fi

echo "🔍 Checking staged files for SQL injection vulnerabilities..."

# Create a temporary directory for checking
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Copy staged files to temp directory (preserving structure)
for file in $STAGED_TS_FILES; do
    if [ -f "$REPO_ROOT/$file" ]; then
        mkdir -p "$TEMP_DIR/$(dirname "$file")"
        cp "$REPO_ROOT/$file" "$TEMP_DIR/$file"
    fi
done

# Run the SQL pattern checker on temp directory
cd "$TEMP_DIR"
OUTPUT=$(bun run "$CHECKER" . 2>&1)
EXIT_CODE=$?

# Clean up
cd "$REPO_ROOT"

# Display results
if [ $EXIT_CODE -eq 0 ]; then
    echo "${GREEN}✅ No SQL injection vulnerabilities detected${NC}"
    exit 0
else
    echo "${RED}❌ SQL injection vulnerabilities detected in staged files:${NC}\n"
    echo "$OUTPUT"
    echo ""
    echo "${YELLOW}Please fix the issues above before committing.${NC}"
    echo "${YELLOW}See SECURITY_PATTERNS.md for guidance.${NC}"
    echo ""
    echo "To bypass this check (NOT RECOMMENDED):"
    echo "  git commit --no-verify"
    exit 1
fi
