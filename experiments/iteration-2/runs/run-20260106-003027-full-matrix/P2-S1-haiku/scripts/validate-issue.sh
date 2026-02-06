#!/bin/bash
#
# Script: validate-issue.sh
# Purpose: Validate GitHub issue format before @copilot processing
# Usage: ./scripts/validate-issue.sh <issue-number>
# Output: Exit 0 if valid, exit 1 if invalid
#

set -euo pipefail

ISSUE_NUMBER="${1:-}"
ISSUE_TEMPLATE_PATH=".github/ISSUE_TEMPLATE/task.yml"
LOG_DIR="${LOG_DIR:-logs}"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Ensure issue number provided
if [ -z "$ISSUE_NUMBER" ]; then
  echo -e "${RED}ERROR: Issue number not provided${NC}"
  echo "Usage: $0 <issue-number>"
  exit 1
fi

# Initialize validation log
VALIDATION_LOG="${LOG_DIR}/validation-issue-${ISSUE_NUMBER}.json"
mkdir -p "$LOG_DIR"

cat > "$VALIDATION_LOG" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "issue_number": $ISSUE_NUMBER,
  "checks": []
}
EOF

# Helper function to log validation result
log_check() {
  local check_name="$1"
  local status="$2"
  local details="${3:-}"

  if [ "$status" = "pass" ]; then
    echo -e "${GREEN}✓${NC} $check_name"
  elif [ "$status" = "fail" ]; then
    echo -e "${RED}✗${NC} $check_name"
    if [ -n "$details" ]; then
      echo "  Details: $details"
    fi
  else
    echo -e "${YELLOW}⚠${NC} $check_name"
    if [ -n "$details" ]; then
      echo "  Details: $details"
    fi
  fi
}

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Validating issue #$ISSUE_NUMBER format"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check 1: Issue template exists
if [ -f "$ISSUE_TEMPLATE_PATH" ]; then
  log_check "Issue template exists" "pass"
  TEMPLATE_CHECK="pass"
else
  log_check "Issue template exists" "fail" "Template not found at $ISSUE_TEMPLATE_PATH"
  TEMPLATE_CHECK="fail"
fi

# Check 2: Template is valid YAML
if command -v yamllint &> /dev/null; then
  if yamllint "$ISSUE_TEMPLATE_PATH" > /dev/null 2>&1; then
    log_check "Template YAML syntax valid" "pass"
    YAML_CHECK="pass"
  else
    log_check "Template YAML syntax valid" "fail" "YAML validation failed"
    YAML_CHECK="fail"
  fi
else
  log_check "Template YAML syntax valid" "warn" "yamllint not available, skipping validation"
  YAML_CHECK="skip"
fi

# Check 3: Required fields in template
REQUIRED_FIELDS=("description" "acceptance_criteria" "complexity")
FIELDS_MISSING=0

for field in "${REQUIRED_FIELDS[@]}"; do
  if grep -q "id: $field" "$ISSUE_TEMPLATE_PATH"; then
    log_check "Required field '$field' in template" "pass"
  else
    log_check "Required field '$field' in template" "fail"
    FIELDS_MISSING=$((FIELDS_MISSING + 1))
  fi
done

if [ $FIELDS_MISSING -eq 0 ]; then
  FIELDS_CHECK="pass"
else
  FIELDS_CHECK="fail"
fi

# Check 4: Issue number is numeric
if [[ $ISSUE_NUMBER =~ ^[0-9]+$ ]]; then
  log_check "Issue number is numeric" "pass"
  NUMBER_CHECK="pass"
else
  log_check "Issue number is numeric" "fail" "Issue number must be numeric"
  NUMBER_CHECK="fail"
fi

# Check 5: Copilot config exists
COPILOT_CONFIG="copilot.config.json"
if [ -f "$COPILOT_CONFIG" ]; then
  log_check "Copilot config exists" "pass"
  CONFIG_CHECK="pass"
else
  log_check "Copilot config exists" "fail" "Config not found at $COPILOT_CONFIG"
  CONFIG_CHECK="fail"
fi

# Check 6: Config is valid JSON
if [ -f "$COPILOT_CONFIG" ]; then
  if command -v jq &> /dev/null; then
    if jq empty "$COPILOT_CONFIG" 2>/dev/null; then
      log_check "Copilot config valid JSON" "pass"
      JSON_CHECK="pass"
    else
      log_check "Copilot config valid JSON" "fail" "JSON parsing failed"
      JSON_CHECK="fail"
    fi
  else
    log_check "Copilot config valid JSON" "warn" "jq not available, skipping validation"
    JSON_CHECK="skip"
  fi
fi

# Check 7: Knowledge base directory exists
KNOWLEDGE_BASE="docs/knowledge"
if [ -d "$KNOWLEDGE_BASE" ]; then
  log_check "Knowledge base directory exists" "pass"
  KB_DIR_CHECK="pass"
else
  log_check "Knowledge base directory exists" "fail" "Directory not found at $KNOWLEDGE_BASE"
  KB_DIR_CHECK="fail"
fi

# Check 8: Knowledge index exists
KB_INDEX="$KNOWLEDGE_BASE/index.json"
if [ -f "$KB_INDEX" ]; then
  log_check "Knowledge base index exists" "pass"
  KB_INDEX_CHECK="pass"
else
  log_check "Knowledge base index exists" "fail" "Index not found at $KB_INDEX"
  KB_INDEX_CHECK="fail"
fi

# Summary
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Determine overall status
FAILED=0
for check in TEMPLATE_CHECK YAML_CHECK FIELDS_CHECK NUMBER_CHECK CONFIG_CHECK JSON_CHECK KB_DIR_CHECK KB_INDEX_CHECK; do
  if [ "${!check}" = "fail" ]; then
    FAILED=$((FAILED + 1))
  fi
done

if [ $FAILED -eq 0 ]; then
  echo -e "${GREEN}✓ All validation checks passed${NC}"
  echo "Issue #$ISSUE_NUMBER is ready for processing"

  # Log success
  cat > "$VALIDATION_LOG" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "issue_number": $ISSUE_NUMBER,
  "status": "valid",
  "checks_passed": 8,
  "checks_failed": 0,
  "ready_for_processing": true
}
EOF

  exit 0
else
  echo -e "${RED}✗ Validation failed ($FAILED check(s) failed)${NC}"
  echo "Issue #$ISSUE_NUMBER cannot be processed"

  # Log failure
  cat > "$VALIDATION_LOG" << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "issue_number": $ISSUE_NUMBER,
  "status": "invalid",
  "checks_passed": $((8 - FAILED)),
  "checks_failed": $FAILED,
  "ready_for_processing": false
}
EOF

  exit 1
fi
