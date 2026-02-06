#!/bin/bash

##############################################################################
# Script: Verify @copilot System
# Purpose: Comprehensive validation that all components are in place
# Usage: ./scripts/verify-copilot-system.sh
##############################################################################

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
CHECKS_PASSED=0
CHECKS_FAILED=0
CHECKS_WARNING=0

# Helper functions
check_file() {
  local file=$1
  local description=$2

  if [ -f "$file" ]; then
    echo -e "${GREEN}✓${NC} File exists: $file"
    ((CHECKS_PASSED++))
    return 0
  else
    echo -e "${RED}✗${NC} File missing: $file - $description"
    ((CHECKS_FAILED++))
    return 1
  fi
}

check_dir() {
  local dir=$1
  local description=$2

  if [ -d "$dir" ]; then
    echo -e "${GREEN}✓${NC} Directory exists: $dir"
    ((CHECKS_PASSED++))
    return 0
  else
    echo -e "${RED}✗${NC} Directory missing: $dir - $description"
    ((CHECKS_FAILED++))
    return 1
  fi
}

check_yaml_syntax() {
  local file=$1

  if ! command -v yamllint &> /dev/null; then
    echo -e "${YELLOW}⊘${NC} yamllint not available (skipping: $file)"
    ((CHECKS_WARNING++))
    return 0
  fi

  if yamllint -d relaxed "$file" > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} YAML valid: $file"
    ((CHECKS_PASSED++))
    return 0
  else
    echo -e "${RED}✗${NC} YAML invalid: $file"
    yamllint -d relaxed "$file" || true
    ((CHECKS_FAILED++))
    return 1
  fi
}

check_shell_syntax() {
  local file=$1

  if ! command -v shellcheck &> /dev/null; then
    echo -e "${YELLOW}⊘${NC} shellcheck not available (skipping: $file)"
    ((CHECKS_WARNING++))
    return 0
  fi

  if shellcheck "$file" > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Shell valid: $file"
    ((CHECKS_PASSED++))
    return 0
  else
    echo -e "${RED}✗${NC} Shell invalid: $file"
    shellcheck "$file" || true
    ((CHECKS_FAILED++))
    return 1
  fi
}

check_markdown_syntax() {
  local file=$1

  if ! command -v markdownlint &> /dev/null; then
    echo -e "${YELLOW}⊘${NC} markdownlint not available (skipping: $file)"
    ((CHECKS_WARNING++))
    return 0
  fi

  if markdownlint "$file" > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Markdown valid: $file"
    ((CHECKS_PASSED++))
    return 0
  else
    echo -e "${RED}✗${NC} Markdown issues: $file"
    markdownlint "$file" || true
    ((CHECKS_WARNING++))
    return 1
  fi
}

check_git_repo() {
  if git rev-parse --git-dir > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Git repository initialized"
    ((CHECKS_PASSED++))
    return 0
  else
    echo -e "${RED}✗${NC} Not a git repository"
    ((CHECKS_FAILED++))
    return 1
  fi
}

check_codeowners() {
  if [ ! -f "CODEOWNERS" ]; then
    echo -e "${RED}✗${NC} CODEOWNERS file missing"
    ((CHECKS_FAILED++))
    return 1
  fi

  if grep -q "^\*" CODEOWNERS; then
    echo -e "${GREEN}✓${NC} CODEOWNERS has default owner"
    ((CHECKS_PASSED++))
  else
    echo -e "${YELLOW}⊘${NC} CODEOWNERS has no default owner (may be intentional)"
    ((CHECKS_WARNING++))
  fi

  return 0
}

check_workflow_config() {
  local workflow=".github/workflows/issue-agent.yml"

  if [ ! -f "$workflow" ]; then
    echo -e "${RED}✗${NC} Workflow file missing: $workflow"
    ((CHECKS_FAILED++))
    return 1
  fi

  # Check for required workflow sections
  if grep -q "on:" "$workflow" && \
     grep -q "issues:" "$workflow" && \
     grep -q "jobs:" "$workflow"; then
    echo -e "${GREEN}✓${NC} Workflow has required sections"
    ((CHECKS_PASSED++))
  else
    echo -e "${RED}✗${NC} Workflow missing required sections"
    ((CHECKS_FAILED++))
    return 1
  fi

  return 0
}

check_knowledge_base() {
  local kb_dir="docs/knowledge"

  if [ ! -d "$kb_dir" ]; then
    echo -e "${RED}✗${NC} Knowledge base directory missing: $kb_dir"
    ((CHECKS_FAILED++))
    return 1
  fi

  echo -e "${GREEN}✓${NC} Knowledge base directory exists"
  ((CHECKS_PASSED++))

  # Check subdirectories
  for subdir in patterns decisions insights; do
    if [ -d "$kb_dir/$subdir" ]; then
      local count=$(find "$kb_dir/$subdir" -type f | wc -l)
      echo -e "${GREEN}✓${NC} $kb_dir/$subdir/ exists ($count files)"
      ((CHECKS_PASSED++))
    else
      echo -e "${YELLOW}⊘${NC} $kb_dir/$subdir/ directory missing"
      ((CHECKS_WARNING++))
    fi
  done

  return 0
}

check_json_logs() {
  if [ ! -f "AGENT_LOG.jsonl" ]; then
    echo -e "${YELLOW}⊘${NC} AGENT_LOG.jsonl not created yet (will be created during execution)"
    ((CHECKS_WARNING++))
    return 0
  fi

  local lines=$(wc -l < AGENT_LOG.jsonl)
  echo -e "${GREEN}✓${NC} AGENT_LOG.jsonl exists ($lines log entries)"
  ((CHECKS_PASSED++))

  # Validate JSON lines
  if head -1 AGENT_LOG.jsonl | jq . > /dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} AGENT_LOG.jsonl has valid JSON format"
    ((CHECKS_PASSED++))
  else
    echo -e "${YELLOW}⊘${NC} AGENT_LOG.jsonl format warning (will validate on use)"
    ((CHECKS_WARNING++))
  fi

  return 0
}

print_header() {
  echo ""
  echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
  echo -e "${BLUE}  $1${NC}"
  echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
}

##############################################################################
# Main Verification
##############################################################################

echo -e "${BLUE}@copilot System Verification${NC}"
echo "Starting comprehensive checks..."
echo ""

# 1. Git Repository
print_header "1. Git Repository"
check_git_repo

# 2. GitHub Configuration Files
print_header "2. GitHub Configuration"
check_file ".github/ISSUE_TEMPLATE/task.yml" "Issue template for @copilot tasks"
check_yaml_syntax ".github/ISSUE_TEMPLATE/task.yml"
check_file ".github/workflows/issue-agent.yml" "GitHub Actions workflow"
check_yaml_syntax ".github/workflows/issue-agent.yml"
check_workflow_config

# 3. CODEOWNERS
print_header "3. Code Ownership Configuration"
check_file "CODEOWNERS" "Auto-assignment rules"
check_codeowners

# 4. Documentation
print_header "4. Agent Documentation"
check_file "docs/AGENT_INSTRUCTIONS.md" "Detailed agent instructions"
check_markdown_syntax "docs/AGENT_INSTRUCTIONS.md"

# 5. Knowledge Base
print_header "5. Knowledge Base"
check_knowledge_base

# 6. Script Validation
print_header "6. Scripts"
if [ -d "scripts" ]; then
  for script in scripts/*.sh; do
    if [ -f "$script" ]; then
      check_shell_syntax "$script"
    fi
  done
else
  echo -e "${YELLOW}⊘${NC} scripts/ directory not found (optional)"
  ((CHECKS_WARNING++))
fi

# 7. Execution Logs
print_header "7. Execution Logs"
check_json_logs

# 8. Optional: Test Files
print_header "8. Tests (Optional)"
if [ -d "tests" ]; then
  local test_count=$(find tests -name "*.sh" | wc -l)
  echo -e "${GREEN}✓${NC} Found $test_count test files"
  ((CHECKS_PASSED++))
else
  echo -e "${YELLOW}⊘${NC} tests/ directory not found (optional)"
  ((CHECKS_WARNING++))
fi

# Summary
print_header "Verification Summary"

TOTAL=$((CHECKS_PASSED + CHECKS_FAILED + CHECKS_WARNING))
echo ""
echo "Results:"
echo -e "  ${GREEN}Passed:${NC}   $CHECKS_PASSED"
echo -e "  ${RED}Failed:${NC}   $CHECKS_FAILED"
echo -e "  ${YELLOW}Warnings:${NC} $CHECKS_WARNING"
echo -e "  ${BLUE}Total:${NC}    $TOTAL"
echo ""

# Determine exit status
if [ $CHECKS_FAILED -eq 0 ]; then
  echo -e "${GREEN}✓ System verification PASSED${NC}"
  if [ $CHECKS_WARNING -gt 0 ]; then
    echo -e "${YELLOW}⊘ ($CHECKS_WARNING warnings - see above)${NC}"
  fi
  echo ""
  echo "The @copilot system is ready to process issues."
  echo ""
  echo "Next steps:"
  echo "1. Create a test issue with the 'ai-task' label"
  echo "2. Observe the GitHub Actions workflow trigger"
  echo "3. Review the created pull request"
  echo "4. Merge when satisfied"
  echo ""
  exit 0
else
  echo -e "${RED}✗ System verification FAILED${NC}"
  echo -e "${RED}($CHECKS_FAILED critical issues must be fixed)${NC}"
  echo ""
  echo "Please fix the issues above and run verification again."
  echo ""
  exit 1
fi
