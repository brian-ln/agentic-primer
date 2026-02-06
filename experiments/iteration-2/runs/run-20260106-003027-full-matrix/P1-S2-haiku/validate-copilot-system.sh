#!/bin/bash
# Validate @copilot bootstrap system
# Checks all required files are present and valid
# Exit codes: 0 = all valid, 1 = missing files, 2 = invalid syntax

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="${SCRIPT_DIR}"
EXIT_CODE=0

echo "=========================================="
echo "Validating @copilot Bootstrap System"
echo "=========================================="
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counter
FILES_CHECKED=0
FILES_VALID=0
FILES_INVALID=0

# Check for required files
check_file() {
    local filepath="$1"
    local description="$2"

    FILES_CHECKED=$((FILES_CHECKED + 1))

    if [ -f "$REPO_ROOT/$filepath" ]; then
        echo -e "${GREEN}✅${NC} Found: $filepath"
        FILES_VALID=$((FILES_VALID + 1))
        return 0
    else
        echo -e "${RED}❌${NC} Missing: $filepath ($description)"
        FILES_INVALID=$((FILES_INVALID + 1))
        EXIT_CODE=1
        return 1
    fi
}

# Check YAML syntax
check_yaml_syntax() {
    local filepath="$1"
    local description="$2"

    FILES_CHECKED=$((FILES_CHECKED + 1))

    if [ ! -f "$REPO_ROOT/$filepath" ]; then
        echo -e "${RED}❌${NC} Missing: $filepath ($description)"
        FILES_INVALID=$((FILES_INVALID + 1))
        EXIT_CODE=1
        return 1
    fi

    if command -v yamllint &> /dev/null; then
        if yamllint "$REPO_ROOT/$filepath" > /dev/null 2>&1; then
            echo -e "${GREEN}✅${NC} Valid YAML: $filepath"
            FILES_VALID=$((FILES_VALID + 1))
            return 0
        else
            echo -e "${RED}❌${NC} Invalid YAML: $filepath"
            yamllint "$REPO_ROOT/$filepath" || true
            FILES_INVALID=$((FILES_INVALID + 1))
            EXIT_CODE=2
            return 1
        fi
    else
        echo -e "${YELLOW}⚠${NC}  YAML syntax not checked (yamllint not installed): $filepath"
        FILES_VALID=$((FILES_VALID + 1))
        return 0
    fi
}

# Check JSON syntax
check_json_syntax() {
    local filepath="$1"
    local description="$2"

    FILES_CHECKED=$((FILES_CHECKED + 1))

    if [ ! -f "$REPO_ROOT/$filepath" ]; then
        echo -e "${RED}❌${NC} Missing: $filepath ($description)"
        FILES_INVALID=$((FILES_INVALID + 1))
        EXIT_CODE=1
        return 1
    fi

    if command -v jq &> /dev/null; then
        if jq empty "$REPO_ROOT/$filepath" > /dev/null 2>&1; then
            echo -e "${GREEN}✅${NC} Valid JSON: $filepath"
            FILES_VALID=$((FILES_VALID + 1))
            return 0
        else
            echo -e "${RED}❌${NC} Invalid JSON: $filepath"
            FILES_INVALID=$((FILES_INVALID + 1))
            EXIT_CODE=2
            return 1
        fi
    else
        echo -e "${YELLOW}⚠${NC}  JSON syntax not checked (jq not installed): $filepath"
        FILES_VALID=$((FILES_VALID + 1))
        return 0
    fi
}

# Check shell syntax
check_shell_syntax() {
    local filepath="$1"
    local description="$2"

    FILES_CHECKED=$((FILES_CHECKED + 1))

    if [ ! -f "$REPO_ROOT/$filepath" ]; then
        echo -e "${RED}❌${NC} Missing: $filepath ($description)"
        FILES_INVALID=$((FILES_INVALID + 1))
        EXIT_CODE=1
        return 1
    fi

    if command -v shellcheck &> /dev/null; then
        if shellcheck "$REPO_ROOT/$filepath" > /dev/null 2>&1; then
            echo -e "${GREEN}✅${NC} Valid Shell: $filepath"
            FILES_VALID=$((FILES_VALID + 1))
            return 0
        else
            echo -e "${RED}❌${NC} Invalid Shell: $filepath"
            shellcheck "$REPO_ROOT/$filepath" || true
            FILES_INVALID=$((FILES_INVALID + 1))
            EXIT_CODE=2
            return 1
        fi
    else
        echo -e "${YELLOW}⚠${NC}  Shell syntax not checked (shellcheck not installed): $filepath"
        FILES_VALID=$((FILES_VALID + 1))
        return 0
    fi
}

echo "1. Checking Required Files"
echo "   ========================"
check_file ".github/ISSUE_TEMPLATE/task.yml" "Issue template"
check_file ".github/workflows/ai-process-issue.yml" "GitHub workflow"
check_file ".github/CODEOWNERS" "Code owners file"
check_file "docs/knowledge/README.md" "Knowledge base guide"
check_file "README.md" "User documentation"
check_file ".copilot-config.json" "Configuration file"
echo ""

echo "2. Validating Syntax"
echo "   ================="
check_yaml_syntax ".github/ISSUE_TEMPLATE/task.yml" "Issue template YAML"
check_yaml_syntax ".github/workflows/ai-process-issue.yml" "GitHub workflow YAML"
check_json_syntax ".copilot-config.json" "Configuration JSON"
echo ""

echo "3. Checking Directory Structure"
echo "   ============================"
# Check knowledge base directories
for dir in "docs/knowledge" "docs/knowledge/patterns" "docs/knowledge/decisions" "docs/knowledge/insights"; do
    FILES_CHECKED=$((FILES_CHECKED + 1))
    if [ -d "$REPO_ROOT/$dir" ]; then
        echo -e "${GREEN}✅${NC} Found directory: $dir"
        FILES_VALID=$((FILES_VALID + 1))
    else
        echo -e "${YELLOW}⚠${NC}  Directory not found (will be created by workflow): $dir"
        FILES_VALID=$((FILES_VALID + 1))
    fi
done
echo ""

echo "4. Checking File Permissions"
echo "   ========================="
FILES_CHECKED=$((FILES_CHECKED + 1))
if [ -f "$REPO_ROOT/.github/workflows/ai-process-issue.yml" ]; then
    echo -e "${GREEN}✅${NC} Workflow file is readable"
    FILES_VALID=$((FILES_VALID + 1))
else
    echo -e "${YELLOW}⚠${NC}  Cannot verify permissions"
    FILES_VALID=$((FILES_VALID + 1))
fi
echo ""

echo "5. Content Validation"
echo "   ==================="
# Check for required content in README
FILES_CHECKED=$((FILES_CHECKED + 1))
if [ -f "$REPO_ROOT/README.md" ]; then
    if grep -q "Issue-Driven Development" "$REPO_ROOT/README.md"; then
        echo -e "${GREEN}✅${NC} README contains expected content"
        FILES_VALID=$((FILES_VALID + 1))
    else
        echo -e "${RED}❌${NC} README missing expected content"
        FILES_INVALID=$((FILES_INVALID + 1))
        EXIT_CODE=1
    fi
fi

# Check for required fields in issue template
FILES_CHECKED=$((FILES_CHECKED + 1))
if [ -f "$REPO_ROOT/.github/ISSUE_TEMPLATE/task.yml" ]; then
    if grep -q "Task Description" "$REPO_ROOT/.github/ISSUE_TEMPLATE/task.yml" && \
       grep -q "Acceptance Criteria" "$REPO_ROOT/.github/ISSUE_TEMPLATE/task.yml"; then
        echo -e "${GREEN}✅${NC} Issue template contains required fields"
        FILES_VALID=$((FILES_VALID + 1))
    else
        echo -e "${RED}❌${NC} Issue template missing required fields"
        FILES_INVALID=$((FILES_INVALID + 1))
        EXIT_CODE=1
    fi
fi

# Check for required jobs in workflow
FILES_CHECKED=$((FILES_CHECKED + 1))
if [ -f "$REPO_ROOT/.github/workflows/ai-process-issue.yml" ]; then
    if grep -q "validate-issue" "$REPO_ROOT/.github/workflows/ai-process-issue.yml" && \
       grep -q "process-issue" "$REPO_ROOT/.github/workflows/ai-process-issue.yml" && \
       grep -q "log-metrics" "$REPO_ROOT/.github/workflows/ai-process-issue.yml"; then
        echo -e "${GREEN}✅${NC} Workflow contains all required jobs"
        FILES_VALID=$((FILES_VALID + 1))
    else
        echo -e "${RED}❌${NC} Workflow missing required jobs"
        FILES_INVALID=$((FILES_INVALID + 1))
        EXIT_CODE=1
    fi
fi

echo ""
echo "=========================================="
echo "Validation Summary"
echo "=========================================="
echo "Files checked: $FILES_CHECKED"
echo -e "Files valid:   ${GREEN}$FILES_VALID${NC}"
echo -e "Files invalid: ${RED}$FILES_INVALID${NC}"
echo ""

if [ $EXIT_CODE -eq 0 ]; then
    echo -e "${GREEN}✅ All validations passed!${NC}"
    echo ""
    echo "System is ready. Next steps:"
    echo "1. Verify files are committed to git:"
    echo "   git add ."
    echo "   git commit -m 'bootstrap: copilot auto-review and knowledge base'"
    echo ""
    echo "2. Create a test issue using '@copilot Task' template"
    echo ""
    echo "3. Watch the GitHub Actions workflow process the issue"
    echo ""
    echo "4. Review the generated PR and merge"
else
    echo -e "${RED}❌ Some validations failed${NC}"
    echo ""
    echo "Please fix the issues above and run validation again."
fi

exit $EXIT_CODE
