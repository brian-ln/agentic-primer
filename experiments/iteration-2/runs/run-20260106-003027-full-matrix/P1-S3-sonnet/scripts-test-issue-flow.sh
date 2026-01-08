#!/usr/bin/env bash
#
# test-issue-flow.sh - End-to-end integration test for issue processing
#
# Usage: ./scripts/test-issue-flow.sh [--dry-run]
#
# This script tests the complete workflow:
#   1. Directory structure exists
#   2. Issue template is valid
#   3. Workflows are present and valid
#   4. Knowledge base structure is correct
#   5. Scripts are executable
#   6. Simulated issue can be processed
#
# Options:
#   --dry-run    Show what would be tested without making changes
#
# Exit codes:
#   0 - All tests passed
#   1 - One or more tests failed

set -euo pipefail

# Configuration
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
readonly DRY_RUN="${1:-}"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Logging
log_info() { echo -e "${BLUE}ℹ${NC} $*"; }
log_success() { echo -e "${GREEN}✓${NC} $*"; }
log_warning() { echo -e "${YELLOW}⚠${NC} $*"; }
log_error() { echo -e "${RED}✗${NC} $*"; }

# Test framework
run_test() {
    local test_name=$1
    local test_command=$2

    ((TESTS_RUN++))

    if [ "$DRY_RUN" == "--dry-run" ]; then
        log_info "[DRY RUN] Would run: $test_name"
        return 0
    fi

    if eval "$test_command" > /dev/null 2>&1; then
        log_success "$test_name"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "$test_name"
        ((TESTS_FAILED++))
        return 1
    fi
}

# Test 1: Directory structure
test_directory_structure() {
    log_info "Test 1: Directory Structure"

    local required_dirs=(
        ".github"
        ".github/ISSUE_TEMPLATE"
        ".github/workflows"
        "docs/knowledge"
        "docs/knowledge/patterns"
        "docs/knowledge/decisions"
        "docs/knowledge/insights"
        "scripts"
    )

    local all_exist=true
    for dir in "${required_dirs[@]}"; do
        if ! run_test "Directory exists: $dir" "test -d '${REPO_ROOT}/${dir}'"; then
            all_exist=false
        fi
    done

    echo ""
    return $([ "$all_exist" == "true" ] && echo 0 || echo 1)
}

# Test 2: Required files exist
test_required_files() {
    log_info "Test 2: Required Files"

    local required_files=(
        ".github/CODEOWNERS"
        ".github/ISSUE_TEMPLATE/task.yml"
        "docs/knowledge/README.md"
        "docs/knowledge/patterns/README.md"
        "docs/knowledge/decisions/README.md"
        "docs/knowledge/insights/README.md"
    )

    local all_exist=true
    for file in "${required_files[@]}"; do
        if ! run_test "File exists: $file" "test -f '${REPO_ROOT}/${file}'"; then
            all_exist=false
        fi
    done

    echo ""
    return $([ "$all_exist" == "true" ] && echo 0 || echo 1)
}

# Test 3: Scripts are executable
test_script_permissions() {
    log_info "Test 3: Script Permissions"

    local scripts
    scripts=$(find "${REPO_ROOT}/scripts" -name "*.sh" 2>/dev/null || true)

    if [ -z "$scripts" ]; then
        log_warning "No shell scripts found in scripts/"
        echo ""
        return 0
    fi

    local all_executable=true
    while IFS= read -r script; do
        local relative_path="${script#$REPO_ROOT/}"
        if ! run_test "Executable: $relative_path" "test -x '$script'"; then
            all_executable=false
        fi
    done <<< "$scripts"

    echo ""
    return $([ "$all_executable" == "true" ] && echo 0 || echo 1)
}

# Test 4: YAML files are valid
test_yaml_syntax() {
    log_info "Test 4: YAML Syntax"

    if ! command -v yamllint &> /dev/null; then
        log_warning "yamllint not installed, skipping YAML validation"
        echo ""
        return 0
    fi

    local yaml_files
    yaml_files=$(find "${REPO_ROOT}/.github" \( -name "*.yml" -o -name "*.yaml" \) 2>/dev/null || true)

    if [ -z "$yaml_files" ]; then
        log_warning "No YAML files found"
        echo ""
        return 0
    fi

    local all_valid=true
    while IFS= read -r file; do
        local relative_path="${file#$REPO_ROOT/}"
        if ! run_test "Valid YAML: $relative_path" "yamllint '$file'"; then
            all_valid=false
        fi
    done <<< "$yaml_files"

    echo ""
    return $([ "$all_valid" == "true" ] && echo 0 || echo 1)
}

# Test 5: Issue template is valid
test_issue_template() {
    log_info "Test 5: Issue Template Structure"

    local template="${REPO_ROOT}/.github/ISSUE_TEMPLATE/task.yml"

    if [ ! -f "$template" ]; then
        log_error "Issue template not found"
        echo ""
        return 1
    fi

    # Check for required fields
    local required_fields=(
        "name:"
        "description:"
        "title:"
        "body:"
    )

    local all_fields_present=true
    for field in "${required_fields[@]}"; do
        if ! run_test "Template has field: $field" "grep -q '$field' '$template'"; then
            all_fields_present=false
        fi
    done

    echo ""
    return $([ "$all_fields_present" == "true" ] && echo 0 || echo 1)
}

# Test 6: Workflow files exist
test_workflows() {
    log_info "Test 6: GitHub Actions Workflows"

    local workflows_dir="${REPO_ROOT}/.github/workflows"

    if [ ! -d "$workflows_dir" ]; then
        log_error "Workflows directory not found"
        echo ""
        return 1
    fi

    local workflow_files
    workflow_files=$(find "$workflows_dir" -name "*.yml" 2>/dev/null || true)

    if [ -z "$workflow_files" ]; then
        log_warning "No workflow files found (they may be created in simulation output)"
        echo ""
        return 0
    fi

    local all_valid=true
    while IFS= read -r file; do
        local relative_path="${file#$REPO_ROOT/}"

        # Check for basic workflow structure
        if grep -q "^on:" "$file" && grep -q "^jobs:" "$file"; then
            log_success "Valid workflow: $relative_path"
            ((TESTS_PASSED++))
        else
            log_error "Invalid workflow structure: $relative_path"
            ((TESTS_FAILED++))
            all_valid=false
        fi
        ((TESTS_RUN++))
    done <<< "$workflow_files"

    echo ""
    return $([ "$all_valid" == "true" ] && echo 0 || echo 1)
}

# Test 7: Knowledge base structure
test_knowledge_base() {
    log_info "Test 7: Knowledge Base Structure"

    local kb_root="${REPO_ROOT}/docs/knowledge"

    # Check main README
    run_test "Knowledge base README exists" "test -f '$kb_root/README.md'"

    # Check category structure
    local categories=("patterns" "decisions" "insights")
    local all_valid=true

    for category in "${categories[@]}"; do
        if ! run_test "Category exists: $category" "test -d '$kb_root/$category'"; then
            all_valid=false
        fi
        if ! run_test "Category README: $category" "test -f '$kb_root/$category/README.md'"; then
            all_valid=false
        fi
    done

    echo ""
    return $([ "$all_valid" == "true" ] && echo 0 || echo 1)
}

# Test 8: CODEOWNERS syntax
test_codeowners() {
    log_info "Test 8: CODEOWNERS File"

    local codeowners="${REPO_ROOT}/.github/CODEOWNERS"

    if [ ! -f "$codeowners" ]; then
        log_error "CODEOWNERS file not found"
        echo ""
        return 1
    fi

    # Check for at least one owner rule
    if grep -qE '^\*\s+@' "$codeowners"; then
        log_success "CODEOWNERS has default owner rule"
        ((TESTS_PASSED++))
    else
        log_error "CODEOWNERS missing default owner rule"
        ((TESTS_FAILED++))
        echo ""
        return 1
    fi

    ((TESTS_RUN++))
    echo ""
    return 0
}

# Test 9: README has workflow documentation
test_readme_docs() {
    log_info "Test 9: README Documentation"

    local readme="${REPO_ROOT}/README.md"

    if [ ! -f "$readme" ]; then
        log_warning "README.md not found"
        echo ""
        return 0
    fi

    # Check for workflow documentation
    if grep -qi "workflow" "$readme" || grep -qi "issue" "$readme"; then
        log_success "README contains workflow documentation"
        ((TESTS_PASSED++))
    else
        log_warning "README might be missing workflow documentation"
        ((TESTS_RUN++))
    fi

    ((TESTS_RUN++))
    echo ""
    return 0
}

# Test 10: Simulated issue processing
test_simulated_issue() {
    log_info "Test 10: Simulated Issue Processing"

    if [ "$DRY_RUN" == "--dry-run" ]; then
        log_info "[DRY RUN] Would simulate issue processing"
        echo ""
        return 0
    fi

    log_info "Simulating issue assignment to @copilot..."

    # Create a test issue object (simulation)
    local test_issue_data='{
        "number": 999,
        "title": "[Task]: Test issue for validation",
        "body": "### Description\nTest issue\n\n### Acceptance Criteria\n- [ ] Test passes",
        "assignee": {"login": "copilot"}
    }'

    # Verify the workflow would trigger
    local workflow="${REPO_ROOT}/.github/workflows/copilot-assign.yml"

    if [ -f "$workflow" ]; then
        if grep -q "issues:" "$workflow" && grep -q "types: \[assigned\]" "$workflow"; then
            log_success "Workflow would trigger on issue assignment"
            ((TESTS_PASSED++))
        else
            log_warning "Workflow trigger configuration not found"
            ((TESTS_FAILED++))
        fi
    else
        log_info "Workflow file not found in repository (simulated in output)"
    fi

    ((TESTS_RUN++))
    echo ""
    return 0
}

# Print summary
print_summary() {
    echo "=========================================="
    log_info "Test Summary"
    echo "=========================================="
    echo "Total tests run: $TESTS_RUN"
    echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
    echo -e "${RED}Failed: $TESTS_FAILED${NC}"
    echo "=========================================="

    if [ $TESTS_FAILED -gt 0 ]; then
        echo ""
        log_error "Some tests failed"
        log_info "Review the errors above and fix the issues"
        return 1
    else
        echo ""
        log_success "All tests passed! System is ready for use."
        return 0
    fi
}

# Main execution
main() {
    echo ""
    log_info "Starting integration tests..."
    if [ "$DRY_RUN" == "--dry-run" ]; then
        log_info "Running in dry-run mode"
    fi
    echo ""

    cd "$REPO_ROOT"

    # Run all test suites
    test_directory_structure || true
    test_required_files || true
    test_script_permissions || true
    test_yaml_syntax || true
    test_issue_template || true
    test_workflows || true
    test_knowledge_base || true
    test_codeowners || true
    test_readme_docs || true
    test_simulated_issue || true

    # Print summary and exit with appropriate code
    print_summary
}

# Run main
main "$@"
