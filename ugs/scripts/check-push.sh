#!/bin/bash
# Check code quality and doc sync
# Run manually: ./scripts/check-push.sh [--force]
# Also called by .git/hooks/pre-push

set -e
cd "$(dirname "$0")/.."

FORCE=false
if [ "$1" = "--force" ] || [ "$1" = "--force-review" ]; then
  FORCE=true
fi

MIN_COVERAGE=90

echo "=== Code Quality Checks ==="

# 1. Run tests with coverage
echo "→ Running tests with coverage..."
bun test --coverage 2>&1 | tee /tmp/test-output.txt | tail -20

# Check for failures
FAIL_COUNT=$(grep -oE '[0-9]+ fail' /tmp/test-output.txt | grep -oE '^[0-9]+' | head -1 || echo "0")
if [ "$FAIL_COUNT" != "0" ] && [ "$FAIL_COUNT" != "" ]; then
  echo "FAIL: $FAIL_COUNT tests failed"
  exit 1
fi

PASS_COUNT=$(grep -oE '[0-9]+ pass' /tmp/test-output.txt | grep -oE '[0-9]+' | tail -1 || echo "0")
SKIP_COUNT=$(grep -oE '[0-9]+ skip' /tmp/test-output.txt | grep -oE '[0-9]+' | tail -1 || echo "0")
echo "  ✓ $PASS_COUNT tests passing ($SKIP_COUNT skipped)"

# 2. Check coverage threshold
echo "→ Checking coverage..."
FUNC_COVERAGE=$(grep "All files" /tmp/test-output.txt | awk '{print $4}' || echo "0")
LINE_COVERAGE=$(grep "All files" /tmp/test-output.txt | awk '{print $6}' || echo "0")

# Compare as integers (strip decimal)
FUNC_INT=${FUNC_COVERAGE%.*}
LINE_INT=${LINE_COVERAGE%.*}

if [ "$FUNC_INT" -lt "$MIN_COVERAGE" ]; then
  echo "FAIL: Function coverage ${FUNC_COVERAGE}% < ${MIN_COVERAGE}%"
  grep -A100 "File.*% Funcs" /tmp/test-output.txt | head -20
  exit 1
fi

if [ "$LINE_INT" -lt "$MIN_COVERAGE" ]; then
  echo "FAIL: Line coverage ${LINE_COVERAGE}% < ${MIN_COVERAGE}%"
  grep -A100 "File.*% Funcs" /tmp/test-output.txt | head -20
  exit 1
fi

echo "  ✓ Coverage: ${FUNC_COVERAGE}% functions, ${LINE_COVERAGE}% lines (min: ${MIN_COVERAGE}%)"

# 3. Check test count matches spec
echo "→ Checking spec alignment..."
SPEC_COUNT=$(grep -oE '[0-9]+ (unit )?tests' docs/specifications/UGS_CURRENT_SPEC.md 2>/dev/null | grep -oE '[0-9]+' | head -1 || echo "0")
TOTAL_COUNT=$((PASS_COUNT + SKIP_COUNT))
if [ "$SPEC_COUNT" != "0" ] && [ "$SPEC_COUNT" != "$TOTAL_COUNT" ]; then
  echo "FAIL: Spec says $SPEC_COUNT tests but actual is $TOTAL_COUNT (pass: $PASS_COUNT, skip: $SKIP_COUNT)"
  exit 1
fi
echo "  ✓ Test count matches spec ($TOTAL_COUNT: $PASS_COUNT pass, $SKIP_COUNT skip)"

# 4. Optional AI review for deeper checks (only on --force or changes)
if ! command -v claude &> /dev/null; then
  echo "  ⚠ Claude CLI not found, skipping AI review"
  echo ""
  echo "=== All checks passed ==="
  exit 0
fi

FILES=$(git diff origin/main..HEAD --name-only 2>/dev/null || echo "")
if [ -z "$FILES" ] && [ "$FORCE" = false ]; then
  echo "  ✓ No changes to review"
  echo ""
  echo "=== All checks passed ==="
  exit 0
fi

echo "→ AI review (spec accuracy)..."
if [ "$FORCE" = true ]; then
  echo "  (forced full review)"
fi

# Only check spec accuracy with AI - coverage handles code→tests
SPEC_CURRENT=$(cat docs/specifications/UGS_CURRENT_SPEC.md 2>/dev/null || echo "")
CODE_EXPORTS=$(for f in src/entities/*.ts src/*.ts; do
  [ -f "$f" ] && echo "=== $f ===" && grep -E "^export (class|function|const)" "$f" 2>/dev/null | head -15
done)

REVIEW=$(echo "SPEC ACCURACY CHECK

Verify that UGS_CURRENT_SPEC.md accurately reflects the code.

SPEC (features claimed):
$SPEC_CURRENT

CODE EXPORTS:
$CODE_EXPORTS

CHECK:
1. Every ✅ feature in spec should have matching exports in code
2. Every ❌ feature should NOT have implementation code
3. No obvious mismatches between what spec claims and what code provides

Reply PASS or FAIL:<specific mismatch found>" | claude -p 2>/dev/null || echo "PASS")

if echo "$REVIEW" | grep -qi "^FAIL"; then
  echo "FAIL: AI review"
  echo "$REVIEW"
  exit 1
fi
echo "  ✓ AI review passed (spec matches code)"

echo ""
echo "=== All checks passed ==="
