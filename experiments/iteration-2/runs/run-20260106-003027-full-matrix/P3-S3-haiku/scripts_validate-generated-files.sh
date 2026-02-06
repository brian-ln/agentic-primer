#!/bin/bash

##############################################################################
# validate-generated-files.sh
#
# Validate syntax of all generated files using appropriate tools
#
# Validates:
#   - YAML files (github actions workflows, issue templates)
#   - JSON configuration files
#   - Bash shell scripts
#   - Markdown documentation
#
# Usage:
#   ./scripts/validate-generated-files.sh [--verbose] [--fix]
#
# Options:
#   --verbose    Print detailed output
#   --fix        Attempt to fix issues (not implemented)
#
# Exit Codes:
#   0  - All validations passed
#   1  - One or more validations failed
#
##############################################################################

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VERBOSE=false
FIX=false

# Counters
PASS=0
WARN=0
FAIL=0

##############################################################################
# Helper Functions
##############################################################################

log() {
    echo "[INFO] $*" >&2
}

warn() {
    echo "[WARN] $*" >&2
    ((WARN++))
}

error() {
    echo "[FAIL] $*" >&2
    ((FAIL++))
}

success() {
    echo "[PASS] $*" >&2
    ((PASS++))
}

verbose() {
    if [[ "$VERBOSE" = true ]]; then
        log "$*"
    fi
}

##############################################################################
# Tool Detection
##############################################################################

check_tool() {
    local tool="$1"
    local install_hint="$2"

    if ! command -v "$tool" &> /dev/null; then
        warn "Tool not found: $tool"
        if [[ -n "$install_hint" ]]; then
            verbose "  Install: $install_hint"
        fi
        return 1
    fi
    return 0
}

##############################################################################
# Validation Functions
##############################################################################

# Validate YAML files (workflow, issue template)
validate_yaml_files() {
    log "Validating YAML files..."

    local yaml_files=(
        ".github/workflows/issue-to-pr.yml"
        ".github/ISSUE_TEMPLATE/task.yml"
    )

    for file in "${yaml_files[@]}"; do
        local full_path="${PROJECT_ROOT}/${file}"

        if [[ ! -f "$full_path" ]]; then
            warn "File not found: $file"
            continue
        fi

        # Try python3 YAML parser (most portable)
        if python3 -c "import yaml; yaml.safe_load(open('$full_path'))" 2>/dev/null; then
            success "YAML: $file"
        else
            error "YAML: $file (syntax error)"
        fi
    done
}

# Validate JSON files
validate_json_files() {
    log "Validating JSON files..."

    local json_files=(
        ".copilot/config.json"
    )

    for file in "${json_files[@]}"; do
        local full_path="${PROJECT_ROOT}/${file}"

        if [[ ! -f "$full_path" ]]; then
            warn "File not found: $file"
            continue
        fi

        if python3 -m json.tool "$full_path" > /dev/null 2>&1; then
            success "JSON: $file"
        else
            error "JSON: $file (syntax error)"
        fi
    done
}

# Validate Bash scripts
validate_bash_files() {
    log "Validating Bash scripts..."

    local bash_files=(
        "scripts/process-issue.sh"
        "scripts/validate-generated-files.sh"
    )

    for file in "${bash_files[@]}"; do
        local full_path="${PROJECT_ROOT}/${file}"

        if [[ ! -f "$full_path" ]]; then
            warn "File not found: $file"
            continue
        fi

        # Check syntax with bash -n
        if bash -n "$full_path" 2>/dev/null; then
            success "Bash: $file"
        else
            error "Bash: $file (syntax error)"
        fi

        # Check if executable (warning only)
        if [[ ! -x "$full_path" ]]; then
            warn "Not executable: $file (use: chmod +x $file)"
        fi
    done
}

# Validate Markdown files (optional, for documentation)
validate_markdown_files() {
    log "Validating Markdown files..."

    # Find all markdown files
    local md_files
    md_files=$(find "${PROJECT_ROOT}" -name "*.md" -type f 2>/dev/null || true)

    if [[ -z "$md_files" ]]; then
        verbose "No Markdown files found"
        return
    fi

    local file
    while IFS= read -r file; do
        # Basic markdown checks (not requiring external tools)

        # Check for valid headers
        if grep -q "^#" "$file"; then
            # Check for proper spacing
            if ! grep -q "^# [^#]" "$file"; then
                warn "Markdown: $file (headers should have space after #)"
                continue
            fi
        fi

        # Check for valid code blocks
        local code_blocks
        code_blocks=$(grep -c '```' "$file" || echo "0")
        if (( code_blocks % 2 != 0 )); then
            warn "Markdown: $file (unclosed code blocks)"
            continue
        fi

        success "Markdown: $(basename "$file")"
    done <<< "$md_files"
}

# Validate directory structure
validate_directory_structure() {
    log "Validating directory structure..."

    local required_dirs=(
        ".github/ISSUE_TEMPLATE"
        ".github/workflows"
        "docs/knowledge/decisions"
        "docs/knowledge/patterns"
        "docs/knowledge/insights"
        ".copilot"
        "scripts"
    )

    for dir in "${required_dirs[@]}"; do
        local full_path="${PROJECT_ROOT}/${dir}"

        if [[ -d "$full_path" ]]; then
            success "Directory: $dir"
        else
            error "Directory: $dir (not found)"
        fi
    done
}

# Validate required files
validate_required_files() {
    log "Validating required files..."

    local required_files=(
        ".github/ISSUE_TEMPLATE/task.yml"
        ".github/workflows/issue-to-pr.yml"
        "CODEOWNERS"
        "docs/knowledge/README.md"
        "docs/knowledge/decisions/ADR-001-event-driven.md"
        "docs/knowledge/patterns/issue-handling.md"
        "docs/knowledge/insights/bootstrap-log.md"
        ".copilot/config.json"
        ".copilot/bootstrap.md"
        "scripts/process-issue.sh"
        "scripts/validate-generated-files.sh"
    )

    for file in "${required_files[@]}"; do
        local full_path="${PROJECT_ROOT}/${file}"

        if [[ -f "$full_path" ]]; then
            success "File: $file"
        else
            error "File: $file (missing)"
        fi
    done
}

# Validate CODEOWNERS format
validate_codeowners() {
    log "Validating CODEOWNERS..."

    local codeowners_file="${PROJECT_ROOT}/CODEOWNERS"

    if [[ ! -f "$codeowners_file" ]]; then
        error "CODEOWNERS file not found"
        return
    fi

    # Check for valid patterns
    if grep -q "^\*" "$codeowners_file"; then
        success "CODEOWNERS: default pattern found"
    else
        warn "CODEOWNERS: no default pattern found"
    fi

    # Check for valid GitHub users/teams
    if grep -q "@" "$codeowners_file"; then
        success "CODEOWNERS: owner references found"
    else
        error "CODEOWNERS: no owner references found"
    fi
}

# Validate GitHub Actions workflow permissions
validate_workflow_permissions() {
    log "Validating GitHub Actions workflow..."

    local workflow_file="${PROJECT_ROOT}/.github/workflows/issue-to-pr.yml"

    if [[ ! -f "$workflow_file" ]]; then
        error "Workflow file not found"
        return
    fi

    # Check for required permissions
    if grep -q "permissions:" "$workflow_file"; then
        success "Workflow: permissions declared"
    else
        warn "Workflow: consider declaring permissions"
    fi

    # Check for trigger configuration
    if grep -q "on:" "$workflow_file"; then
        success "Workflow: trigger configured"
    else
        error "Workflow: missing trigger configuration"
    fi
}

##############################################################################
# Summary Report
##############################################################################

print_summary() {
    local total=$((PASS + WARN + FAIL))

    echo ""
    echo "============================================"
    echo "VALIDATION SUMMARY"
    echo "============================================"
    echo "PASS: $PASS"
    echo "WARN: $WARN"
    echo "FAIL: $FAIL"
    echo "TOTAL: $total"
    echo "============================================"

    if [[ $FAIL -eq 0 ]]; then
        echo "✓ All critical validations passed"
        return 0
    else
        echo "✗ $FAIL validation(s) failed - review errors above"
        return 1
    fi
}

##############################################################################
# Main Execution
##############################################################################

main() {
    # Parse options
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --verbose)
                VERBOSE=true
                shift
                ;;
            --fix)
                FIX=true
                shift
                ;;
            *)
                error "Unknown option: $1"
                exit 1
                ;;
        esac
    done

    log "Starting validation..."
    log "Project root: $PROJECT_ROOT"
    echo ""

    # Run validation checks
    validate_required_files
    validate_directory_structure
    validate_yaml_files
    validate_json_files
    validate_bash_files
    validate_markdown_files
    validate_codeowners
    validate_workflow_permissions

    echo ""

    # Print summary and return appropriate exit code
    if print_summary; then
        exit 0
    else
        exit 1
    fi
}

##############################################################################
# Execute
##############################################################################

main "$@"
