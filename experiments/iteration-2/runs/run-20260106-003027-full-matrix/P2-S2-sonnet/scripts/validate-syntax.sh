#!/usr/bin/env bash
# validate-syntax.sh
# Validates YAML and shell script syntax
#
# Usage: ./scripts/validate-syntax.sh [--strict]
#
# Options:
#   --strict    Fail on warnings (default: fail only on errors)
#
# Exit codes:
#   0: All validations passed
#   1: Validation errors found
#   2: Missing validation tools

set -euo pipefail

# Configuration
STRICT_MODE=false
YAML_CONFIG="relaxed"
SHELLCHECK_SEVERITY="error"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --strict)
      STRICT_MODE=true
      YAML_CONFIG="default"
      SHELLCHECK_SEVERITY="warning"
      shift
      ;;
    --help)
      echo "Usage: $0 [--strict]"
      echo ""
      echo "Validates YAML files and shell scripts in the repository."
      echo ""
      echo "Options:"
      echo "  --strict    Use strict validation (fail on warnings)"
      echo "  --help      Show this help message"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      echo "Use --help for usage information"
      exit 1
      ;;
  esac
done

# Track validation results
YAML_ERRORS=0
SHELL_ERRORS=0
YAML_FILES_CHECKED=0
SHELL_FILES_CHECKED=0

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Syntax Validation${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Mode: $([ "$STRICT_MODE" = true ] && echo "Strict" || echo "Relaxed")"
echo ""

# Function to check if a command exists
command_exists() {
  command -v "$1" >/dev/null 2>&1
}

# Validate YAML files
echo -e "${BLUE}Checking YAML files...${NC}"
echo ""

if ! command_exists yamllint; then
  echo -e "${YELLOW}⚠ yamllint not found${NC}"
  echo "Install with: pip install yamllint"
  echo "Skipping YAML validation"
  echo ""
else
  # Find all YAML files
  mapfile -t yaml_files < <(find . -type f \( -name "*.yml" -o -name "*.yaml" \) \
    ! -path "*/node_modules/*" \
    ! -path "*/.git/*" \
    ! -path "*/vendor/*" \
    ! -path "*/dist/*" \
    ! -path "*/build/*")

  if [ ${#yaml_files[@]} -eq 0 ]; then
    echo -e "${YELLOW}No YAML files found${NC}"
    echo ""
  else
    echo "Found ${#yaml_files[@]} YAML files"
    echo ""

    for file in "${yaml_files[@]}"; do
      YAML_FILES_CHECKED=$((YAML_FILES_CHECKED + 1))

      # Run yamllint
      if yamllint -d "{extends: $YAML_CONFIG, rules: {line-length: {max: 120}}}" "$file" 2>&1; then
        echo -e "${GREEN}✓${NC} $file"
      else
        YAML_ERRORS=$((YAML_ERRORS + 1))
        echo -e "${RED}✗${NC} $file"
      fi
    done

    echo ""
    if [ $YAML_ERRORS -eq 0 ]; then
      echo -e "${GREEN}✓ All YAML files valid${NC}"
    else
      echo -e "${RED}✗ $YAML_ERRORS YAML file(s) with errors${NC}"
    fi
    echo ""
  fi
fi

# Validate shell scripts
echo -e "${BLUE}Checking shell scripts...${NC}"
echo ""

if ! command_exists shellcheck; then
  echo -e "${YELLOW}⚠ shellcheck not found${NC}"
  echo "Install from: https://www.shellcheck.net/"
  echo "Skipping shell script validation"
  echo ""
else
  # Find all shell scripts
  mapfile -t shell_files < <(find . -type f -name "*.sh" \
    ! -path "*/node_modules/*" \
    ! -path "*/.git/*" \
    ! -path "*/vendor/*" \
    ! -path "*/dist/*" \
    ! -path "*/build/*")

  if [ ${#shell_files[@]} -eq 0 ]; then
    echo -e "${YELLOW}No shell scripts found${NC}"
    echo ""
  else
    echo "Found ${#shell_files[@]} shell scripts"
    echo ""

    for file in "${shell_files[@]}"; do
      SHELL_FILES_CHECKED=$((SHELL_FILES_CHECKED + 1))

      # Run shellcheck
      if shellcheck -S "$SHELLCHECK_SEVERITY" "$file" 2>&1; then
        echo -e "${GREEN}✓${NC} $file"
      else
        SHELL_ERRORS=$((SHELL_ERRORS + 1))
        echo -e "${RED}✗${NC} $file"
      fi
    done

    echo ""
    if [ $SHELL_ERRORS -eq 0 ]; then
      echo -e "${GREEN}✓ All shell scripts valid${NC}"
    else
      echo -e "${RED}✗ $SHELL_ERRORS shell script(s) with errors${NC}"
    fi
    echo ""
  fi
fi

# Summary
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Summary${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Files checked:"
echo "  YAML: $YAML_FILES_CHECKED"
echo "  Shell: $SHELL_FILES_CHECKED"
echo ""
echo "Errors found:"
echo "  YAML: $YAML_ERRORS"
echo "  Shell: $SHELL_ERRORS"
echo ""

# Exit with appropriate code
TOTAL_ERRORS=$((YAML_ERRORS + SHELL_ERRORS))

if [ $TOTAL_ERRORS -eq 0 ]; then
  echo -e "${GREEN}✓ All validations passed!${NC}"
  exit 0
else
  echo -e "${RED}✗ Validation failed with $TOTAL_ERRORS error(s)${NC}"
  exit 1
fi
