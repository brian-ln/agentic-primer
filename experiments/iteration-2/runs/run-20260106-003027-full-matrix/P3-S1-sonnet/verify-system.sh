#!/bin/bash
# verify-system.sh - Validate the issue-driven development system
#
# This script checks that all required files exist and are valid.
# Simulates validation that would occur in a real deployment.

set -euo pipefail

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Base directory for this experiment
BASE_DIR="/Users/bln/play/agentic-primer/experiments/iteration-2/runs/run-20260106-003027-full-matrix/P3-S1-sonnet"

echo "============================================"
echo "Issue-Driven Development System Verification"
echo "============================================"
echo ""

# Track pass/fail
PASS_COUNT=0
FAIL_COUNT=0

# Helper functions
check_file() {
    local file="$1"
    local description="$2"

    if [[ -f "$BASE_DIR/$file" ]]; then
        echo -e "${GREEN}✓${NC} $description: $file"
        ((PASS_COUNT++))
        return 0
    else
        echo -e "${RED}✗${NC} $description: $file (MISSING)"
        ((FAIL_COUNT++))
        return 1
    fi
}

check_directory() {
    local dir="$1"
    local description="$2"

    if [[ -d "$BASE_DIR/$dir" ]]; then
        echo -e "${GREEN}✓${NC} $description: $dir"
        ((PASS_COUNT++))
        return 0
    else
        echo -e "${RED}✗${NC} $description: $dir (MISSING)"
        ((FAIL_COUNT++))
        return 1
    fi
}

check_yaml_syntax() {
    local file="$1"
    local full_path="$BASE_DIR/$file"

    if [[ ! -f "$full_path" ]]; then
        echo -e "${RED}✗${NC} YAML syntax check: $file (FILE MISSING)"
        ((FAIL_COUNT++))
        return 1
    fi

    # Basic YAML validation - check for common syntax errors
    # In real scenario would use yamllint
    if grep -q "^[[:space:]]*[a-zA-Z]" "$full_path" && \
       ! grep -q $'^\t' "$full_path"; then
        echo -e "${GREEN}✓${NC} YAML syntax valid: $file"
        ((PASS_COUNT++))
        return 0
    else
        echo -e "${RED}✗${NC} YAML syntax check: $file (POTENTIAL ISSUES)"
        ((FAIL_COUNT++))
        return 1
    fi
}

check_content() {
    local file="$1"
    local pattern="$2"
    local description="$3"
    local full_path="$BASE_DIR/$file"

    if [[ ! -f "$full_path" ]]; then
        echo -e "${RED}✗${NC} $description: $file (FILE MISSING)"
        ((FAIL_COUNT++))
        return 1
    fi

    if grep -q "$pattern" "$full_path"; then
        echo -e "${GREEN}✓${NC} $description"
        ((PASS_COUNT++))
        return 0
    else
        echo -e "${RED}✗${NC} $description (PATTERN NOT FOUND)"
        ((FAIL_COUNT++))
        return 1
    fi
}

echo "1. Checking File Structure"
echo "-----------------------------------"

# Issue template
check_file ".github/ISSUE_TEMPLATE/task.yml" "Issue template"

# CODEOWNERS
check_file ".github/CODEOWNERS" "CODEOWNERS file"

# Knowledge base structure
check_directory "docs/knowledge" "Knowledge base root"
check_file "docs/knowledge/README.md" "Knowledge base overview"

check_directory "docs/knowledge/patterns" "Patterns directory"
check_file "docs/knowledge/patterns/README.md" "Patterns README"
check_file "docs/knowledge/patterns/INDEX.md" "Patterns index"

check_directory "docs/knowledge/decisions" "Decisions directory"
check_file "docs/knowledge/decisions/README.md" "Decisions README"
check_file "docs/knowledge/decisions/INDEX.md" "Decisions index"

check_directory "docs/knowledge/insights" "Insights directory"
check_file "docs/knowledge/insights/README.md" "Insights README"
check_file "docs/knowledge/insights/INDEX.md" "Insights index"

# Documentation
check_file "README.md" "Workflow documentation"
check_file "DESIGN.md" "Design documentation"

echo ""
echo "2. Validating File Syntax"
echo "-----------------------------------"

# YAML validation
check_yaml_syntax ".github/ISSUE_TEMPLATE/task.yml"

# Check CODEOWNERS format
check_content ".github/CODEOWNERS" "^\\* @" "CODEOWNERS catch-all pattern"

echo ""
echo "3. Checking Content Completeness"
echo "-----------------------------------"

# Issue template must have required fields
check_content ".github/ISSUE_TEMPLATE/task.yml" "name:" "Issue template has name"
check_content ".github/ISSUE_TEMPLATE/task.yml" "description:" "Issue template has description"
check_content ".github/ISSUE_TEMPLATE/task.yml" "body:" "Issue template has body"

# README must document workflow
check_content "README.md" "Workflow" "README documents workflow"
check_content "README.md" "@copilot" "README mentions @copilot"
check_content "README.md" "pull request" "README mentions PRs"

# Knowledge base must explain structure
check_content "docs/knowledge/README.md" "patterns" "Knowledge base explains patterns"
check_content "docs/knowledge/README.md" "decisions" "Knowledge base explains decisions"
check_content "docs/knowledge/README.md" "insights" "Knowledge base explains insights"

echo ""
echo "4. Functional Checks"
echo "-----------------------------------"

# Check that issue template is properly structured
if [[ -f "$BASE_DIR/.github/ISSUE_TEMPLATE/task.yml" ]]; then
    if grep -q "validations:" "$BASE_DIR/.github/ISSUE_TEMPLATE/task.yml"; then
        echo -e "${GREEN}✓${NC} Issue template has validation rules"
        ((PASS_COUNT++))
    else
        echo -e "${YELLOW}⚠${NC} Issue template missing validation rules"
    fi
fi

# Check that CODEOWNERS has comment explaining usage
if [[ -f "$BASE_DIR/.github/CODEOWNERS" ]]; then
    if grep -q "^#" "$BASE_DIR/.github/CODEOWNERS"; then
        echo -e "${GREEN}✓${NC} CODEOWNERS has documentation comments"
        ((PASS_COUNT++))
    else
        echo -e "${YELLOW}⚠${NC} CODEOWNERS missing documentation"
    fi
fi

# Check that each knowledge subdirectory has both README and INDEX
for subdir in patterns decisions insights; do
    if [[ -f "$BASE_DIR/docs/knowledge/$subdir/README.md" ]] && \
       [[ -f "$BASE_DIR/docs/knowledge/$subdir/INDEX.md" ]]; then
        echo -e "${GREEN}✓${NC} Knowledge/$subdir has complete documentation"
        ((PASS_COUNT++))
    else
        echo -e "${RED}✗${NC} Knowledge/$subdir missing README or INDEX"
        ((FAIL_COUNT++))
    fi
done

echo ""
echo "5. Test Issue Validation"
echo "-----------------------------------"

if [[ -f "$BASE_DIR/TEST_ISSUE.md" ]]; then
    echo -e "${GREEN}✓${NC} Test issue documentation exists"
    ((PASS_COUNT++))

    # Check test issue has required sections
    check_content "TEST_ISSUE.md" "Task Description" "Test issue has task description"
    check_content "TEST_ISSUE.md" "Acceptance Criteria" "Test issue has acceptance criteria"
    check_content "TEST_ISSUE.md" "Success Criteria" "Test issue has success criteria"
else
    echo -e "${RED}✗${NC} Test issue documentation missing"
    ((FAIL_COUNT++))
fi

echo ""
echo "============================================"
echo "Verification Summary"
echo "============================================"
echo -e "Checks Passed: ${GREEN}$PASS_COUNT${NC}"
echo -e "Checks Failed: ${RED}$FAIL_COUNT${NC}"
echo ""

if [[ $FAIL_COUNT -eq 0 ]]; then
    echo -e "${GREEN}✓ ALL CHECKS PASSED${NC}"
    echo ""
    echo "The issue-driven development system is valid and ready for use."
    echo ""
    echo "Next steps:"
    echo "1. Update .github/CODEOWNERS with your actual GitHub username"
    echo "2. Deploy to a real repository"
    echo "3. Create a test issue using the template"
    echo "4. Verify @copilot can process the issue"
    echo ""
    exit 0
else
    echo -e "${RED}✗ VERIFICATION FAILED${NC}"
    echo ""
    echo "Some checks did not pass. Review the output above."
    echo ""
    exit 1
fi
