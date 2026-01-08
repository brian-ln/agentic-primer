#!/bin/bash
#
# verify-bootstrap.sh - Verify @copilot system is correctly configured
#
# Usage: ./scripts/verify-bootstrap.sh
# Exit codes: 0 = all checks pass, 1 = critical failures, 2 = warnings
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASS=0
WARN=0
FAIL=0

# Helper functions
log_pass() {
  echo -e "${GREEN}✓${NC} $1"
  ((PASS++))
}

log_warn() {
  echo -e "${YELLOW}⚠${NC} $1"
  ((WARN++))
}

log_fail() {
  echo -e "${RED}✗${NC} $1"
  ((FAIL++))
}

log_info() {
  echo -e "${BLUE}ℹ${NC} $1"
}

echo "=========================================="
echo "  @copilot Bootstrap Verification"
echo "=========================================="
echo ""

# Check 1: Issue template exists
echo "Checking issue template..."
TEMPLATE_FILE="${PROJECT_ROOT}/.github/ISSUE_TEMPLATE/task.yml"
if [[ -f "$TEMPLATE_FILE" ]]; then
  log_pass "Issue template found: $TEMPLATE_FILE"

  # Verify it has required fields
  if grep -q "name: Development Task" "$TEMPLATE_FILE"; then
    log_pass "Template name is correct"
  else
    log_warn "Template name missing or incorrect"
  fi

  if grep -q "id: title" "$TEMPLATE_FILE" && \
     grep -q "id: description" "$TEMPLATE_FILE" && \
     grep -q "id: acceptance_criteria" "$TEMPLATE_FILE"; then
    log_pass "Template has required fields (title, description, acceptance_criteria)"
  else
    log_warn "Template may be missing required fields"
  fi
else
  log_fail "Issue template not found at $TEMPLATE_FILE"
fi
echo ""

# Check 2: CODEOWNERS exists
echo "Checking CODEOWNERS..."
CODEOWNERS_FILE="${PROJECT_ROOT}/.github/CODEOWNERS"
if [[ -f "$CODEOWNERS_FILE" ]]; then
  log_pass "CODEOWNERS file found: $CODEOWNERS_FILE"

  # Check for placeholder
  if grep -q "@owner" "$CODEOWNERS_FILE"; then
    log_warn "CODEOWNERS contains placeholder '@owner' - must be replaced with actual GitHub username"
    log_info "Edit .github/CODEOWNERS and replace '@owner' with your GitHub username (e.g., '@alice')"
  else
    log_pass "CODEOWNERS appears to have actual usernames configured"
  fi

  # Check format
  if grep -E "^\*\s+@" "$CODEOWNERS_FILE" > /dev/null; then
    log_pass "CODEOWNERS has proper format"
  else
    log_warn "CODEOWNERS format may be incorrect"
  fi
else
  log_fail "CODEOWNERS not found at $CODEOWNERS_FILE"
fi
echo ""

# Check 3: Main workflow file
echo "Checking main workflow..."
WORKFLOW_FILE="${PROJECT_ROOT}/.github/workflows/copilot-task.yml"
if [[ -f "$WORKFLOW_FILE" ]]; then
  log_pass "Main workflow found: $WORKFLOW_FILE"

  # Verify workflow structure
  if grep -q "name: Copilot Task Automation" "$WORKFLOW_FILE"; then
    log_pass "Workflow name is correct"
  else
    log_warn "Workflow name may be incorrect"
  fi

  if grep -q "on:" "$WORKFLOW_FILE" && grep -q "issues:" "$WORKFLOW_FILE"; then
    log_pass "Workflow has triggers configured"
  else
    log_warn "Workflow triggers may be misconfigured"
  fi
else
  log_fail "Main workflow not found at $WORKFLOW_FILE"
fi
echo ""

# Check 4: Validation workflow
echo "Checking validation workflow..."
VALIDATE_WF="${PROJECT_ROOT}/.github/workflows/validate-system.yml"
if [[ -f "$VALIDATE_WF" ]]; then
  log_pass "Validation workflow found: $VALIDATE_WF"
else
  log_warn "Validation workflow not found at $VALIDATE_WF (optional but recommended)"
fi
echo ""

# Check 5: Knowledge base structure
echo "Checking knowledge base..."
KB_ROOT="${PROJECT_ROOT}/docs/knowledge"
if [[ -d "$KB_ROOT" ]]; then
  log_pass "Knowledge base directory exists: $KB_ROOT"

  # Check subdirectories
  for subdir in patterns decisions insights; do
    if [[ -d "$KB_ROOT/$subdir" ]]; then
      log_pass "Knowledge base has $subdir/ directory"
    else
      log_warn "Knowledge base missing $subdir/ directory"
      mkdir -p "$KB_ROOT/$subdir" 2>/dev/null || true
    fi
  done

  # Check README
  if [[ -f "$KB_ROOT/README.md" ]]; then
    log_pass "Knowledge base README exists"
  else
    log_warn "Knowledge base README not found (useful for guidance)"
  fi
else
  log_warn "Knowledge base directory not found at $KB_ROOT"
  log_info "Creating knowledge base structure..."
  mkdir -p "$KB_ROOT"/{patterns,decisions,insights} 2>/dev/null || true
fi
echo ""

# Check 6: PR template
echo "Checking pull request template..."
PR_TEMPLATE="${PROJECT_ROOT}/.github/pull_request_template.md"
if [[ -f "$PR_TEMPLATE" ]]; then
  log_pass "PR template found: $PR_TEMPLATE"
else
  log_warn "PR template not found at $PR_TEMPLATE (optional but recommended)"
fi
echo ""

# Check 7: README documentation
echo "Checking documentation..."
README="${PROJECT_ROOT}/README.md"
if [[ -f "$README" ]]; then
  log_pass "README found: $README"

  if grep -qi "copilot\|issue.*driven" "$README"; then
    log_pass "README documents the workflow"
  else
    log_warn "README may not adequately document the copilot workflow"
  fi
else
  log_fail "README not found at $README"
fi
echo ""

# Check 8: Shell scripts (if any)
echo "Checking scripts..."
SCRIPTS_DIR="${PROJECT_ROOT}/scripts"
if [[ -d "$SCRIPTS_DIR" ]]; then
  log_pass "Scripts directory exists: $SCRIPTS_DIR"

  # Check if scripts are executable
  for script in "$SCRIPTS_DIR"/*.sh; do
    if [[ -f "$script" ]]; then
      if [[ -x "$script" ]]; then
        log_pass "Script is executable: $(basename "$script")"
      else
        log_warn "Script not executable: $(basename "$script")"
      fi
    fi
  done
else
  log_warn "Scripts directory not found"
fi
echo ""

# Check 9: YAML syntax validation (if yamllint available)
echo "Checking YAML syntax..."
if command -v yamllint &> /dev/null; then
  log_pass "yamllint is available"

  # Check issue template
  if yamllint -d "{extends: relaxed}" "$TEMPLATE_FILE" 2>/dev/null; then
    log_pass "Issue template YAML is valid"
  else
    log_warn "Issue template has YAML issues (may still be functional)"
  fi

  # Check workflows
  if yamllint -d "{extends: relaxed}" "$WORKFLOW_FILE" 2>/dev/null; then
    log_pass "Main workflow YAML is valid"
  else
    log_warn "Main workflow has YAML issues"
  fi

  if [[ -f "$VALIDATE_WF" ]] && yamllint -d "{extends: relaxed}" "$VALIDATE_WF" 2>/dev/null; then
    log_pass "Validation workflow YAML is valid"
  fi
else
  log_info "yamllint not installed (install with: pip install yamllint)"
  log_info "Continuing without YAML validation..."
fi
echo ""

# Check 10: Shell script syntax (if shellcheck available)
echo "Checking shell scripts..."
if command -v shellcheck &> /dev/null; then
  log_pass "shellcheck is available"

  if [[ -f "$SCRIPT_DIR/verify-bootstrap.sh" ]]; then
    if shellcheck "$SCRIPT_DIR/verify-bootstrap.sh" 2>/dev/null; then
      log_pass "verify-bootstrap.sh has valid shell syntax"
    else
      log_warn "verify-bootstrap.sh has shell syntax issues"
    fi
  fi
else
  log_info "shellcheck not installed (install with: apt-get install shellcheck)"
fi
echo ""

# Check 11: .gitattributes for line endings
echo "Checking git configuration..."
GITATTRIBUTES="${PROJECT_ROOT}/.gitattributes"
if [[ -f "$GITATTRIBUTES" ]]; then
  log_pass ".gitattributes exists (ensures consistent line endings)"
else
  log_warn ".gitattributes not found (optional but recommended for cross-platform consistency)"
fi
echo ""

# Check 12: Repository structure
echo "Checking repository structure..."
if [[ -d "${PROJECT_ROOT}/.git" ]]; then
  log_pass "This is a git repository"

  # Check for main branch
  if git -C "$PROJECT_ROOT" rev-parse --verify main &>/dev/null; then
    log_pass "main branch exists"
  else
    log_warn "main branch not found (workflow assumes main as base branch)"
  fi
else
  log_warn "Not a git repository"
fi
echo ""

# Summary
echo "=========================================="
echo "  Verification Summary"
echo "=========================================="
echo -e "Passed:  ${GREEN}$PASS${NC}"
echo -e "Warnings: ${YELLOW}$WARN${NC}"
echo -e "Failed:   ${RED}$FAIL${NC}"
echo ""

# Determine exit code
if [[ $FAIL -gt 0 ]]; then
  echo -e "${RED}Status: FAILED${NC}"
  echo ""
  echo "Critical issues must be fixed:"
  echo "1. Ensure all .github files exist"
  echo "2. Ensure docs/knowledge/ directory structure"
  echo "3. Ensure README.md documents the workflow"
  exit 1
elif [[ $WARN -gt 0 ]]; then
  echo -e "${YELLOW}Status: PASSED WITH WARNINGS${NC}"
  echo ""
  echo "Before using in production:"
  echo "1. Replace '@owner' in .github/CODEOWNERS with your GitHub username"
  echo "2. Install yamllint and shellcheck for full validation"
  echo "3. Test with a sample issue"
  exit 0
else
  echo -e "${GREEN}Status: ALL CHECKS PASSED${NC}"
  echo ""
  echo "System is ready for use!"
  echo "Next steps:"
  echo "1. Create a test issue using the Development Task template"
  echo "2. Watch the workflow in the Actions tab"
  echo "3. Review the PR when created"
  echo "4. Merge and document any patterns discovered"
  exit 0
fi
