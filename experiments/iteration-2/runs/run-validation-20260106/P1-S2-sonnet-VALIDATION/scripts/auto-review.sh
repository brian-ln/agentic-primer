#!/usr/bin/env bash
#
# Auto-Review Script for @copilot PRs
#
# This script validates PR quality before human review by running:
# - YAML syntax validation (yamllint)
# - Shell script validation (shellcheck)
# - Markdown linting (markdownlint)
# - Test suite (if present)
#
# Exit codes:
#   0 - All checks passed
#   1 - One or more checks failed
#   2 - Script error (missing dependencies, etc.)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0

# Output functions
info() {
    echo -e "${BLUE}ℹ${NC} $*"
}

success() {
    echo -e "${GREEN}✓${NC} $*"
    ((PASSED_CHECKS++))
}

warning() {
    echo -e "${YELLOW}⚠${NC} $*"
}

error() {
    echo -e "${RED}✗${NC} $*"
    ((FAILED_CHECKS++))
}

section() {
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  $*${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    echo ""
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Validate YAML files
validate_yaml() {
    section "YAML Validation"

    if ! command_exists yamllint; then
        warning "yamllint not found, skipping YAML validation"
        warning "Install: pip install yamllint"
        return 0
    fi

    local yaml_files
    yaml_files=$(find . -type f \( -name "*.yml" -o -name "*.yaml" \) ! -path "*/node_modules/*" ! -path "*/.git/*" 2>/dev/null || true)

    if [ -z "$yaml_files" ]; then
        info "No YAML files found"
        return 0
    fi

    ((TOTAL_CHECKS++))

    local failed=0
    while IFS= read -r file; do
        if [ -f "$file" ]; then
            info "Checking: $file"
            if yamllint -d relaxed "$file" 2>&1; then
                success "Valid: $file"
            else
                error "Invalid YAML: $file"
                failed=1
            fi
        fi
    done <<< "$yaml_files"

    if [ $failed -eq 0 ]; then
        success "All YAML files are valid"
        return 0
    else
        error "YAML validation failed"
        return 1
    fi
}

# Validate shell scripts
validate_shell() {
    section "Shell Script Validation"

    if ! command_exists shellcheck; then
        warning "shellcheck not found, skipping shell validation"
        warning "Install: apt-get install shellcheck (or brew install shellcheck)"
        return 0
    fi

    local shell_files
    shell_files=$(find . -type f \( -name "*.sh" -o -name "*.bash" \) ! -path "*/node_modules/*" ! -path "*/.git/*" 2>/dev/null || true)

    if [ -z "$shell_files" ]; then
        info "No shell scripts found"
        return 0
    fi

    ((TOTAL_CHECKS++))

    local failed=0
    while IFS= read -r file; do
        if [ -f "$file" ]; then
            info "Checking: $file"
            # SC2086: Allow intentional word splitting for some cases
            # shellcheck disable=SC2086
            if shellcheck -x -e SC2086 "$file" 2>&1; then
                success "Valid: $file"
            else
                error "shellcheck failed: $file"
                failed=1
            fi
        fi
    done <<< "$shell_files"

    if [ $failed -eq 0 ]; then
        success "All shell scripts passed validation"
        return 0
    else
        error "Shell script validation failed"
        return 1
    fi
}

# Validate markdown files
validate_markdown() {
    section "Markdown Validation"

    if ! command_exists markdownlint; then
        if ! command_exists mdl; then
            warning "markdownlint or mdl not found, skipping markdown validation"
            warning "Install: npm install -g markdownlint-cli (or gem install mdl)"
            return 0
        fi
    fi

    local md_files
    md_files=$(find . -type f -name "*.md" ! -path "*/node_modules/*" ! -path "*/.git/*" 2>/dev/null || true)

    if [ -z "$md_files" ]; then
        info "No markdown files found"
        return 0
    fi

    ((TOTAL_CHECKS++))

    local failed=0

    # Try markdownlint-cli first
    if command_exists markdownlint; then
        info "Using markdownlint-cli"
        if markdownlint --ignore node_modules --ignore .git . 2>&1; then
            success "All markdown files are valid"
            return 0
        else
            error "Markdown validation failed"
            return 1
        fi
    elif command_exists mdl; then
        info "Using mdl (markdownlint ruby)"
        while IFS= read -r file; do
            if [ -f "$file" ]; then
                info "Checking: $file"
                if mdl "$file" 2>&1; then
                    success "Valid: $file"
                else
                    error "Markdown issues: $file"
                    failed=1
                fi
            fi
        done <<< "$md_files"

        if [ $failed -eq 0 ]; then
            success "All markdown files are valid"
            return 0
        else
            error "Markdown validation failed"
            return 1
        fi
    fi
}

# Run test suite
run_tests() {
    section "Test Suite"

    # Check for common test commands
    local test_cmd=""

    if [ -f "package.json" ]; then
        if command_exists npm; then
            if grep -q '"test"' package.json; then
                test_cmd="npm test"
            fi
        fi
    elif [ -f "Makefile" ]; then
        if grep -q "^test:" Makefile; then
            test_cmd="make test"
        fi
    elif [ -f "pytest.ini" ] || [ -f "setup.py" ]; then
        if command_exists pytest; then
            test_cmd="pytest"
        fi
    fi

    if [ -z "$test_cmd" ]; then
        info "No test suite detected, skipping"
        return 0
    fi

    ((TOTAL_CHECKS++))

    info "Running: $test_cmd"
    if $test_cmd 2>&1; then
        success "All tests passed"
        return 0
    else
        error "Test suite failed"
        return 1
    fi
}

# Check for common issues
check_common_issues() {
    section "Common Issues Check"

    ((TOTAL_CHECKS++))

    local issues_found=0

    # Check for TODO/FIXME in committed code
    info "Checking for TODO/FIXME markers..."
    if git diff --cached --name-only 2>/dev/null | xargs grep -Hn "TODO\|FIXME" 2>/dev/null; then
        warning "Found TODO/FIXME markers in staged code"
        warning "Consider creating issues for these items"
        issues_found=1
    fi

    # Check for console.log in JavaScript (if applicable)
    if [ -d "src" ] || [ -d "lib" ]; then
        info "Checking for console.log statements..."
        if find . -type f \( -name "*.js" -o -name "*.ts" \) ! -path "*/node_modules/*" ! -path "*/.git/*" -exec grep -Hn "console\.log" {} \; 2>/dev/null; then
            warning "Found console.log statements"
            warning "Consider using a proper logging library"
        fi
    fi

    # Check for large files
    info "Checking for large files (>1MB)..."
    if git diff --cached --name-only 2>/dev/null | while IFS= read -r file; do
        if [ -f "$file" ]; then
            size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null || echo 0)
            if [ "$size" -gt 1048576 ]; then
                echo "$file: $(( size / 1024 / 1024 ))MB"
            fi
        fi
    done | grep -q .; then
        warning "Found files larger than 1MB"
        warning "Consider using Git LFS for large files"
    fi

    if [ $issues_found -eq 0 ]; then
        success "No common issues found"
        return 0
    else
        warning "Some common issues detected (non-blocking)"
        return 0  # Don't fail build for warnings
    fi
}

# Generate summary
generate_summary() {
    section "Auto-Review Summary"

    echo "Total Checks: $TOTAL_CHECKS"
    echo "Passed: $PASSED_CHECKS"
    echo "Failed: $FAILED_CHECKS"
    echo ""

    if [ $FAILED_CHECKS -eq 0 ]; then
        success "All automated checks passed!"
        echo ""
        info "Ready for human review"
        return 0
    else
        error "Some checks failed"
        echo ""
        info "Please fix the issues above before requesting review"
        return 1
    fi
}

# Main execution
main() {
    echo ""
    echo "╔═══════════════════════════════════════════════════╗"
    echo "║                                                   ║"
    echo "║         @copilot PR Auto-Review                   ║"
    echo "║                                                   ║"
    echo "╚═══════════════════════════════════════════════════╝"
    echo ""

    info "Starting automated quality checks..."
    echo ""

    # Run all validation checks
    validate_yaml || true
    validate_shell || true
    validate_markdown || true
    run_tests || true
    check_common_issues || true

    # Generate and display summary
    if generate_summary; then
        exit 0
    else
        exit 1
    fi
}

# Run main function
main "$@"
