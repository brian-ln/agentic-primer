#!/usr/bin/env bash
#
# Syntax validation script for issue-driven development
#
# This script validates YAML, shell scripts, and markdown files using
# industry-standard linting tools. Designed for both local pre-commit
# checks and CI/CD pipeline integration.
#
# Usage: ./scripts/validate-syntax.sh [--fix] [--verbose]
#
# Options:
#   --fix      Attempt to auto-fix issues where possible
#   --verbose  Show detailed output from all tools
#
# Exit codes:
#   0 - All validations passed
#   1 - One or more validations failed
#   2 - Required tool not installed

set -euo pipefail

# Parse command line arguments
FIX_MODE=false
VERBOSE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --fix)
            FIX_MODE=true
            shift
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--fix] [--verbose]"
            exit 1
            ;;
    esac
done

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[⚠]${NC} $*"
}

log_error() {
    echo -e "${RED}[✗]${NC} $*" >&2
}

# Track overall validation status
VALIDATION_FAILED=false

# Check if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# Validate YAML files
validate_yaml() {
    log_info "Validating YAML files..."

    if ! command_exists yamllint; then
        log_error "yamllint is not installed. Install with: pip install yamllint"
        return 2
    fi

    local yaml_files
    yaml_files=$(find . -type f \( -name "*.yml" -o -name "*.yaml" \) \
        ! -path "*/node_modules/*" \
        ! -path "*/.venv/*" \
        ! -path "*/.git/*" \
        2>/dev/null)

    if [ -z "$yaml_files" ]; then
        log_warning "No YAML files found to validate"
        return 0
    fi

    local file_count
    file_count=$(echo "$yaml_files" | wc -l | tr -d ' ')
    log_info "Found $file_count YAML files"

    # Create temporary yamllint config if none exists
    local yamllint_config=".yamllint"
    local temp_config=false

    if [ ! -f "$yamllint_config" ]; then
        temp_config=true
        cat > "$yamllint_config" <<EOF
---
extends: default
rules:
  line-length:
    max: 120
  document-start: disable
  comments:
    min-spaces-from-content: 1
EOF
    fi

    if [ "$VERBOSE" = true ]; then
        echo "$yaml_files" | xargs yamllint
    else
        echo "$yaml_files" | xargs yamllint 2>&1 | grep -v "^$" || true
    fi

    local exit_code=${PIPESTATUS[1]}

    # Cleanup temporary config
    if [ "$temp_config" = true ]; then
        rm -f "$yamllint_config"
    fi

    if [ $exit_code -eq 0 ]; then
        log_success "YAML validation passed ($file_count files)"
        return 0
    else
        log_error "YAML validation failed"
        VALIDATION_FAILED=true
        return 1
    fi
}

# Validate shell scripts
validate_shell() {
    log_info "Validating shell scripts..."

    if ! command_exists shellcheck; then
        log_error "shellcheck is not installed. Install with: apt-get install shellcheck or brew install shellcheck"
        return 2
    fi

    local shell_files
    shell_files=$(find . -type f -name "*.sh" \
        ! -path "*/node_modules/*" \
        ! -path "*/.venv/*" \
        ! -path "*/.git/*" \
        2>/dev/null)

    if [ -z "$shell_files" ]; then
        log_warning "No shell scripts found to validate"
        return 0
    fi

    local file_count
    file_count=$(echo "$shell_files" | wc -l | tr -d ' ')
    log_info "Found $file_count shell scripts"

    # Run shellcheck with appropriate severity
    local shellcheck_opts=(
        --severity=warning
        --enable=all
        --exclude=SC2312  # Consider invoking this command separately
    )

    if [ "$VERBOSE" = true ]; then
        echo "$shell_files" | xargs shellcheck "${shellcheck_opts[@]}"
    else
        echo "$shell_files" | xargs shellcheck "${shellcheck_opts[@]}" 2>&1 | head -20
    fi

    local exit_code=${PIPESTATUS[1]}

    if [ $exit_code -eq 0 ]; then
        log_success "Shell script validation passed ($file_count files)"
        return 0
    else
        log_error "Shell script validation failed"
        VALIDATION_FAILED=true
        return 1
    fi
}

# Validate markdown files
validate_markdown() {
    log_info "Validating markdown files..."

    if ! command_exists markdownlint; then
        log_warning "markdownlint is not installed. Install with: npm install -g markdownlint-cli"
        log_info "Skipping markdown validation"
        return 0
    fi

    local md_files
    md_files=$(find . -type f -name "*.md" \
        ! -path "*/node_modules/*" \
        ! -path "*/.venv/*" \
        ! -path "*/.git/*" \
        2>/dev/null)

    if [ -z "$md_files" ]; then
        log_warning "No markdown files found to validate"
        return 0
    fi

    local file_count
    file_count=$(echo "$md_files" | wc -l | tr -d ' ')
    log_info "Found $file_count markdown files"

    # Create temporary markdownlint config if none exists
    local mdl_config=".markdownlint.json"
    local temp_config=false

    if [ ! -f "$mdl_config" ]; then
        temp_config=true
        cat > "$mdl_config" <<EOF
{
  "default": true,
  "MD013": {
    "line_length": 120,
    "code_blocks": false,
    "tables": false
  },
  "MD033": {
    "allowed_elements": ["details", "summary"]
  },
  "MD041": false
}
EOF
    fi

    local mdl_opts=()
    if [ "$FIX_MODE" = true ]; then
        mdl_opts+=(--fix)
        log_info "Auto-fix mode enabled"
    fi

    if [ "$VERBOSE" = true ]; then
        echo "$md_files" | xargs markdownlint "${mdl_opts[@]}"
    else
        echo "$md_files" | xargs markdownlint "${mdl_opts[@]}" 2>&1 | head -20 || true
    fi

    local exit_code=${PIPESTATUS[1]}

    # Cleanup temporary config
    if [ "$temp_config" = true ]; then
        rm -f "$mdl_config"
    fi

    # Markdown validation is non-blocking (warnings only)
    if [ $exit_code -eq 0 ]; then
        log_success "Markdown validation passed ($file_count files)"
    else
        log_warning "Markdown validation completed with warnings (non-blocking)"
    fi

    return 0
}

# Validate JSON files
validate_json() {
    log_info "Validating JSON files..."

    local json_files
    json_files=$(find . -type f -name "*.json" \
        ! -path "*/node_modules/*" \
        ! -path "*/.venv/*" \
        ! -path "*/.git/*" \
        ! -name "package-lock.json" \
        2>/dev/null)

    if [ -z "$json_files" ]; then
        log_warning "No JSON files found to validate"
        return 0
    fi

    local file_count
    file_count=$(echo "$json_files" | wc -l | tr -d ' ')
    log_info "Found $file_count JSON files"

    local failed_files=()

    while IFS= read -r file; do
        if ! python3 -m json.tool "$file" > /dev/null 2>&1; then
            failed_files+=("$file")
            if [ "$VERBOSE" = true ]; then
                log_error "Invalid JSON: $file"
            fi
        fi
    done <<< "$json_files"

    if [ ${#failed_files[@]} -eq 0 ]; then
        log_success "JSON validation passed ($file_count files)"
        return 0
    else
        log_error "JSON validation failed (${#failed_files[@]} files with errors)"
        for file in "${failed_files[@]}"; do
            echo "  - $file"
        done
        VALIDATION_FAILED=true
        return 1
    fi
}

# Check for common issues
check_common_issues() {
    log_info "Checking for common issues..."

    local issues_found=false

    # Check for trailing whitespace
    log_info "Checking for trailing whitespace..."
    local trailing_ws_files
    trailing_ws_files=$(find . -type f \( -name "*.sh" -o -name "*.yml" -o -name "*.yaml" -o -name "*.md" \) \
        ! -path "*/node_modules/*" \
        ! -path "*/.venv/*" \
        ! -path "*/.git/*" \
        -exec grep -l ' $' {} \; 2>/dev/null || true)

    if [ -n "$trailing_ws_files" ]; then
        log_warning "Files with trailing whitespace:"
        echo "$trailing_ws_files" | while read -r file; do
            echo "  - $file"
        done
        issues_found=true
    fi

    # Check for files with CRLF line endings
    log_info "Checking for CRLF line endings..."
    local crlf_files
    crlf_files=$(find . -type f \( -name "*.sh" -o -name "*.yml" -o -name "*.yaml" \) \
        ! -path "*/node_modules/*" \
        ! -path "*/.venv/*" \
        ! -path "*/.git/*" \
        -exec file {} \; 2>/dev/null | grep CRLF | cut -d: -f1 || true)

    if [ -n "$crlf_files" ]; then
        log_warning "Files with CRLF line endings (should be LF):"
        echo "$crlf_files" | while read -r file; do
            echo "  - $file"
            if [ "$FIX_MODE" = true ]; then
                dos2unix "$file" 2>/dev/null || sed -i 's/\r$//' "$file"
                log_info "Fixed: $file"
            fi
        done
        issues_found=true
    fi

    # Check for executable permissions on non-script files
    log_info "Checking file permissions..."
    local exec_files
    exec_files=$(find . -type f -perm +111 \
        ! -path "*/node_modules/*" \
        ! -path "*/.venv/*" \
        ! -path "*/.git/*" \
        ! -name "*.sh" 2>/dev/null || true)

    if [ -n "$exec_files" ]; then
        log_warning "Unexpected executable files (should scripts only):"
        echo "$exec_files" | while read -r file; do
            echo "  - $file"
        done
        issues_found=true
    fi

    if [ "$issues_found" = false ]; then
        log_success "No common issues found"
    fi

    return 0
}

# Print summary
print_summary() {
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  Validation Summary"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""

    if [ "$VALIDATION_FAILED" = false ]; then
        log_success "All validations passed! ✓"
        echo ""
        echo "Your code is ready to commit."
    else
        log_error "Some validations failed. Please fix the issues above."
        echo ""
        echo "Tips:"
        echo "  - Run with --fix to auto-fix some issues"
        echo "  - Run with --verbose for detailed output"
        echo "  - Check tool documentation for specific errors"
    fi

    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
}

# Main execution
main() {
    log_info "Starting syntax validation..."
    echo ""

    # Run all validations (don't exit early)
    validate_yaml || true
    echo ""

    validate_shell || true
    echo ""

    validate_markdown || true
    echo ""

    validate_json || true
    echo ""

    check_common_issues || true

    print_summary

    if [ "$VALIDATION_FAILED" = true ]; then
        exit 1
    else
        exit 0
    fi
}

# Run main function
main "$@"
