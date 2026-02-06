#!/usr/bin/env bash
#
# Syntax Validation Script
#
# Purpose: Pre-commit validation of all file types
# Usage: ./scripts/validate-syntax-v2.sh
# Exit Code: 0 if all pass, 1 if any fail

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0

echo ""
echo "═══════════════════════════════════════════════"
echo "  Syntax Validation"
echo "═══════════════════════════════════════════════"
echo ""

# Function to run a check
run_check() {
    local check_name="$1"
    local check_command="$2"

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    echo -ne "${BLUE}[CHECK ${TOTAL_CHECKS}]${NC} ${check_name}... "

    if eval "$check_command" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi
}

# Validate YAML files
echo "─── YAML Files ───"
echo ""

YAML_FILES=$(find . -type f \( -name "*.yml" -o -name "*.yaml" \) \
    ! -path "*/node_modules/*" \
    ! -path "*/.git/*" \
    ! -path "*/vendor/*" 2>/dev/null || true)

if [ -z "$YAML_FILES" ]; then
    echo -e "${YELLOW}No YAML files found${NC}"
else
    YAML_COUNT=$(echo "$YAML_FILES" | wc -l | tr -d ' ')
    echo "Found ${YAML_COUNT} YAML files"
    echo ""

    while IFS= read -r file; do
        if [ -f "$file" ]; then
            run_check "YAML: $file" "yamllint -f parsable '$file'" || {
                echo "  ${RED}Errors:${NC}"
                yamllint -f parsable "$file" 2>&1 | sed 's/^/    /'
                echo ""
            }
        fi
    done <<< "$YAML_FILES"
fi

echo ""

# Validate Shell scripts
echo "─── Shell Scripts ───"
echo ""

SHELL_FILES=$(find . -type f -name "*.sh" \
    ! -path "*/node_modules/*" \
    ! -path "*/.git/*" \
    ! -path "*/vendor/*" 2>/dev/null || true)

if [ -z "$SHELL_FILES" ]; then
    echo -e "${YELLOW}No shell scripts found${NC}"
else
    SHELL_COUNT=$(echo "$SHELL_FILES" | wc -l | tr -d ' ')
    echo "Found ${SHELL_COUNT} shell scripts"
    echo ""

    while IFS= read -r file; do
        if [ -f "$file" ]; then
            run_check "Shell: $file" "shellcheck --severity=warning '$file'" || {
                echo "  ${RED}Errors:${NC}"
                shellcheck --severity=warning --format=gcc "$file" 2>&1 | sed 's/^/    /'
                echo ""
            }
        fi
    done <<< "$SHELL_FILES"
fi

echo ""

# Validate Markdown files
echo "─── Markdown Files ───"
echo ""

MD_FILES=$(find . -type f -name "*.md" \
    ! -path "*/node_modules/*" \
    ! -path "*/.git/*" \
    ! -path "*/vendor/*" 2>/dev/null || true)

if [ -z "$MD_FILES" ]; then
    echo -e "${YELLOW}No markdown files found${NC}"
else
    MD_COUNT=$(echo "$MD_FILES" | wc -l | tr -d ' ')
    echo "Found ${MD_COUNT} markdown files"
    echo ""

    # Create default config if missing
    if [ ! -f ".markdownlint.json" ]; then
        echo '{"default": true, "MD013": {"line_length": 120}}' > .markdownlint.json
    fi

    while IFS= read -r file; do
        if [ -f "$file" ]; then
            # Markdown warnings don't fail the build
            if run_check "Markdown: $file" "markdownlint '$file'"; then
                :
            else
                echo "  ${YELLOW}Warnings:${NC}"
                markdownlint "$file" 2>&1 | sed 's/^/    /'
                echo ""
                # Adjust counters - markdown issues are warnings
                FAILED_CHECKS=$((FAILED_CHECKS - 1))
                PASSED_CHECKS=$((PASSED_CHECKS + 1))
            fi
        fi
    done <<< "$MD_FILES"
fi

echo ""

# Validate JSON files
echo "─── JSON Files ───"
echo ""

JSON_FILES=$(find . -type f -name "*.json" \
    ! -path "*/node_modules/*" \
    ! -path "*/.git/*" \
    ! -path "*/vendor/*" 2>/dev/null || true)

if [ -z "$JSON_FILES" ]; then
    echo -e "${YELLOW}No JSON files found${NC}"
else
    JSON_COUNT=$(echo "$JSON_FILES" | wc -l | tr -d ' ')
    echo "Found ${JSON_COUNT} JSON files"
    echo ""

    while IFS= read -r file; do
        if [ -f "$file" ]; then
            run_check "JSON: $file" "jq empty '$file'" || {
                echo "  ${RED}Errors:${NC}"
                jq empty "$file" 2>&1 | sed 's/^/    /'
                echo ""
            }
        fi
    done <<< "$JSON_FILES"
fi

echo ""

# Summary
echo "═══════════════════════════════════════════════"
echo "  Validation Summary"
echo "═══════════════════════════════════════════════"
echo ""

echo "Total Checks:  ${TOTAL_CHECKS}"
echo -e "Passed:        ${GREEN}${PASSED_CHECKS}${NC}"
echo -e "Failed:        ${RED}${FAILED_CHECKS}${NC}"
echo ""

# Create summary table
echo "┌────────────────────┬────────────┐"
echo "│ File Type          │ Status     │"
echo "├────────────────────┼────────────┤"

if [ -n "$YAML_FILES" ]; then
    YAML_STATUS="${GREEN}✓ Valid${NC}"
    [ $FAILED_CHECKS -gt 0 ] && YAML_STATUS="${RED}✗ Errors${NC}"
    echo -e "│ YAML               │ ${YAML_STATUS}    │"
fi

if [ -n "$SHELL_FILES" ]; then
    SHELL_STATUS="${GREEN}✓ Valid${NC}"
    [ $FAILED_CHECKS -gt 0 ] && SHELL_STATUS="${RED}✗ Errors${NC}"
    echo -e "│ Shell Scripts      │ ${SHELL_STATUS}    │"
fi

if [ -n "$MD_FILES" ]; then
    echo -e "│ Markdown           │ ${YELLOW}⚠ Warnings${NC} │"
fi

if [ -n "$JSON_FILES" ]; then
    JSON_STATUS="${GREEN}✓ Valid${NC}"
    [ $FAILED_CHECKS -gt 0 ] && JSON_STATUS="${RED}✗ Errors${NC}"
    echo -e "│ JSON               │ ${JSON_STATUS}    │"
fi

echo "└────────────────────┴────────────┘"
echo ""

# Exit with appropriate code
if [ $FAILED_CHECKS -gt 0 ]; then
    echo -e "${RED}Validation FAILED${NC}"
    echo "Fix the errors above before committing."
    exit 1
else
    echo -e "${GREEN}All validation checks PASSED${NC}"
    exit 0
fi
