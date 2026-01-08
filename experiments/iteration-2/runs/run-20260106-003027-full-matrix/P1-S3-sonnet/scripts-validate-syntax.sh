#!/usr/bin/env bash
#
# validate-syntax.sh - Comprehensive syntax validation for all file types
#
# Usage: ./scripts/validate-syntax.sh [--fix]
#
# This script validates:
#   - YAML files (GitHub workflows, issue templates, config files)
#   - Shell scripts (bash scripts in scripts/ directory)
#   - Markdown files (documentation)
#   - JSON files (configuration, package files)
#
# Options:
#   --fix    Automatically fix issues where possible
#
# Exit codes:
#   0 - All validations passed
#   1 - Validation errors found
#   2 - Required tools not installed

set -euo pipefail

# Configuration
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
readonly FIX_MODE="${1:-}"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Counters
TOTAL_FILES=0
PASSED_FILES=0
FAILED_FILES=0
SKIPPED_FILES=0

# Logging
log_info() { echo -e "${BLUE}ℹ${NC} $*"; }
log_success() { echo -e "${GREEN}✓${NC} $*"; }
log_warning() { echo -e "${YELLOW}⚠${NC} $*"; }
log_error() { echo -e "${RED}✗${NC} $*"; }

# Check if tool is installed
check_tool() {
    local tool=$1
    local install_cmd=$2

    if ! command -v "$tool" &> /dev/null; then
        log_warning "$tool not found"
        log_info "Install with: $install_cmd"
        return 1
    fi
    return 0
}

# Validate YAML files
validate_yaml() {
    log_info "Validating YAML files..."

    if ! check_tool yamllint "pip install yamllint"; then
        log_warning "Skipping YAML validation"
        return 0
    fi

    # Create yamllint config
    cat > "${REPO_ROOT}/.yamllint" << 'EOF'
extends: default
rules:
  line-length:
    max: 120
    level: warning
  indentation:
    spaces: 2
  comments:
    min-spaces-from-content: 1
  document-start: disable
  truthy:
    allowed-values: ['true', 'false', 'on', 'off']
  comments-indentation: disable
EOF

    local yaml_files
    yaml_files=$(find "$REPO_ROOT" \( -name "*.yml" -o -name "*.yaml" \) \
        ! -path "*/node_modules/*" \
        ! -path "*/.git/*" \
        ! -path "*/vendor/*" \
        2>/dev/null || true)

    if [ -z "$yaml_files" ]; then
        log_info "No YAML files found"
        return 0
    fi

    local yaml_errors=0
    while IFS= read -r file; do
        ((TOTAL_FILES++))
        local relative_path="${file#$REPO_ROOT/}"

        if yamllint -c "${REPO_ROOT}/.yamllint" "$file" > /dev/null 2>&1; then
            log_success "YAML: $relative_path"
            ((PASSED_FILES++))
        else
            log_error "YAML: $relative_path"
            if [ "$FIX_MODE" != "--fix" ]; then
                yamllint -c "${REPO_ROOT}/.yamllint" "$file" || true
            fi
            ((FAILED_FILES++))
            ((yaml_errors++))
        fi
    done <<< "$yaml_files"

    return $yaml_errors
}

# Validate shell scripts
validate_shell() {
    log_info "Validating shell scripts..."

    if ! check_tool shellcheck "apt-get install shellcheck (or brew install shellcheck)"; then
        log_warning "Skipping shell script validation"
        return 0
    fi

    local shell_files
    shell_files=$(find "$REPO_ROOT" -name "*.sh" \
        ! -path "*/node_modules/*" \
        ! -path "*/.git/*" \
        ! -path "*/vendor/*" \
        2>/dev/null || true)

    if [ -z "$shell_files" ]; then
        log_info "No shell scripts found"
        return 0
    fi

    local shell_errors=0
    while IFS= read -r file; do
        ((TOTAL_FILES++))
        local relative_path="${file#$REPO_ROOT/}"

        # Check if file is executable
        if [ ! -x "$file" ]; then
            log_warning "Not executable: $relative_path"
            if [ "$FIX_MODE" == "--fix" ]; then
                chmod +x "$file"
                log_info "Made executable: $relative_path"
            fi
        fi

        if shellcheck -x "$file" > /dev/null 2>&1; then
            log_success "Shell: $relative_path"
            ((PASSED_FILES++))
        else
            log_error "Shell: $relative_path"
            if [ "$FIX_MODE" != "--fix" ]; then
                shellcheck -x "$file" || true
            fi
            ((FAILED_FILES++))
            ((shell_errors++))
        fi
    done <<< "$shell_files"

    return $shell_errors
}

# Validate markdown files
validate_markdown() {
    log_info "Validating markdown files..."

    if ! check_tool markdownlint "npm install -g markdownlint-cli"; then
        log_warning "Skipping markdown validation"
        return 0
    fi

    # Create markdownlint config
    cat > "${REPO_ROOT}/.markdownlint.json" << 'EOF'
{
  "default": true,
  "MD013": {
    "line_length": 120,
    "code_blocks": false,
    "tables": false
  },
  "MD033": false,
  "MD041": false,
  "MD034": false,
  "MD024": {
    "siblings_only": true
  }
}
EOF

    local md_files
    md_files=$(find "$REPO_ROOT" -name "*.md" \
        ! -path "*/node_modules/*" \
        ! -path "*/.git/*" \
        ! -path "*/vendor/*" \
        2>/dev/null || true)

    if [ -z "$md_files" ]; then
        log_info "No markdown files found"
        return 0
    fi

    local md_errors=0
    while IFS= read -r file; do
        ((TOTAL_FILES++))
        local relative_path="${file#$REPO_ROOT/}"

        local cmd_args=(-c "${REPO_ROOT}/.markdownlint.json")
        if [ "$FIX_MODE" == "--fix" ]; then
            cmd_args+=(--fix)
        fi

        if markdownlint "${cmd_args[@]}" "$file" > /dev/null 2>&1; then
            log_success "Markdown: $relative_path"
            ((PASSED_FILES++))
        else
            log_error "Markdown: $relative_path"
            if [ "$FIX_MODE" != "--fix" ]; then
                markdownlint "${cmd_args[@]}" "$file" || true
            fi
            ((FAILED_FILES++))
            ((md_errors++))
        fi
    done <<< "$md_files"

    return $md_errors
}

# Validate JSON files
validate_json() {
    log_info "Validating JSON files..."

    local json_files
    json_files=$(find "$REPO_ROOT" -name "*.json" \
        ! -path "*/node_modules/*" \
        ! -path "*/.git/*" \
        ! -path "*/vendor/*" \
        2>/dev/null || true)

    if [ -z "$json_files" ]; then
        log_info "No JSON files found"
        return 0
    fi

    local json_errors=0
    while IFS= read -r file; do
        ((TOTAL_FILES++))
        local relative_path="${file#$REPO_ROOT/}"

        if python3 -m json.tool "$file" > /dev/null 2>&1; then
            log_success "JSON: $relative_path"
            ((PASSED_FILES++))
        else
            log_error "JSON: $relative_path"
            python3 -m json.tool "$file" || true
            ((FAILED_FILES++))
            ((json_errors++))
        fi
    done <<< "$json_files"

    return $json_errors
}

# Print summary
print_summary() {
    echo ""
    echo "=========================================="
    log_info "Validation Summary"
    echo "=========================================="
    echo "Total files checked: $TOTAL_FILES"
    echo -e "${GREEN}Passed: $PASSED_FILES${NC}"
    echo -e "${RED}Failed: $FAILED_FILES${NC}"
    echo -e "${YELLOW}Skipped: $SKIPPED_FILES${NC}"
    echo "=========================================="

    if [ $FAILED_FILES -gt 0 ]; then
        echo ""
        log_error "Validation failed with $FAILED_FILES error(s)"
        log_info "Run with --fix to automatically fix some issues"
        return 1
    else
        echo ""
        log_success "All validations passed!"
        return 0
    fi
}

# Main execution
main() {
    echo ""
    log_info "Starting syntax validation..."
    if [ "$FIX_MODE" == "--fix" ]; then
        log_info "Fix mode enabled - will attempt to fix issues"
    fi
    echo ""

    cd "$REPO_ROOT"

    local total_errors=0

    validate_yaml || ((total_errors++)) || true
    echo ""

    validate_shell || ((total_errors++)) || true
    echo ""

    validate_markdown || ((total_errors++)) || true
    echo ""

    validate_json || ((total_errors++)) || true
    echo ""

    print_summary
}

# Run main
main "$@"
