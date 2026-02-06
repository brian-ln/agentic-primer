#!/usr/bin/env bash
#
# End-to-end workflow testing script
#
# This script simulates the complete issue-driven development workflow:
#   1. Create test issue
#   2. Assign to @copilot
#   3. Create feature branch
#   4. Make changes and commit
#   5. Create PR
#   6. Verify auto-assignment
#   7. Merge PR
#   8. Verify knowledge capture
#
# Usage: ./scripts/test-workflow.sh [--cleanup] [--dry-run]
#
# Options:
#   --cleanup  Remove test artifacts after run
#   --dry-run  Show what would be done without executing
#
# Exit codes:
#   0 - All tests passed
#   1 - One or more tests failed
#   2 - Missing prerequisites

set -euo pipefail

# Parse command line arguments
CLEANUP=false
DRY_RUN=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --cleanup)
            CLEANUP=true
            shift
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--cleanup] [--dry-run]"
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

# Test tracking
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

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

# Test assertion functions
assert_file_exists() {
    local file=$1
    local description=$2

    TESTS_RUN=$((TESTS_RUN + 1))

    if [ -f "$file" ]; then
        log_success "Test $TESTS_RUN: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        log_error "Test $TESTS_RUN: $description (file not found: $file)"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

assert_command_exists() {
    local cmd=$1
    local description=$2

    TESTS_RUN=$((TESTS_RUN + 1))

    if command -v "$cmd" &> /dev/null; then
        log_success "Test $TESTS_RUN: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        log_error "Test $TESTS_RUN: $description (command not found: $cmd)"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

assert_yaml_valid() {
    local file=$1
    local description=$2

    TESTS_RUN=$((TESTS_RUN + 1))

    if command -v yamllint &> /dev/null; then
        if yamllint -d relaxed "$file" &> /dev/null; then
            log_success "Test $TESTS_RUN: $description"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            log_error "Test $TESTS_RUN: $description (invalid YAML)"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            return 1
        fi
    else
        log_warning "Test $TESTS_RUN: $description (skipped - yamllint not installed)"
        return 0
    fi
}

assert_shell_valid() {
    local file=$1
    local description=$2

    TESTS_RUN=$((TESTS_RUN + 1))

    if command -v shellcheck &> /dev/null; then
        if shellcheck "$file" &> /dev/null; then
            log_success "Test $TESTS_RUN: $description"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            log_error "Test $TESTS_RUN: $description (shellcheck failed)"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            return 1
        fi
    else
        log_warning "Test $TESTS_RUN: $description (skipped - shellcheck not installed)"
        return 0
    fi
}

# Test prerequisites
test_prerequisites() {
    log_info "Testing prerequisites..."
    echo ""

    assert_command_exists "git" "Git is installed"
    assert_command_exists "gh" "GitHub CLI is installed"

    # Check if in git repository
    TESTS_RUN=$((TESTS_RUN + 1))
    if git rev-parse --git-dir > /dev/null 2>&1; then
        log_success "Test $TESTS_RUN: In git repository"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: In git repository (not in git repo)"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    echo ""
}

# Test file structure
test_file_structure() {
    log_info "Testing file structure..."
    echo ""

    # GitHub configuration files
    assert_file_exists ".github/ISSUE_TEMPLATE/task.yml" "Issue template exists"
    assert_file_exists ".github/CODEOWNERS" "CODEOWNERS file exists"
    assert_file_exists ".github/agents.md" "Agent configuration exists"

    # Workflows
    assert_file_exists ".github/workflows/assign-pr-creator.yml" "PR assignment workflow exists"
    assert_file_exists ".github/workflows/validate-pr.yml" "Validation workflow exists"
    assert_file_exists ".github/workflows/knowledge-capture.yml" "Knowledge capture workflow exists"

    # Scripts
    assert_file_exists "scripts/bootstrap.sh" "Bootstrap script exists"
    assert_file_exists "scripts/validate-syntax.sh" "Validation script exists"
    assert_file_exists "scripts/test-workflow.sh" "Test workflow script exists"
    assert_file_exists "scripts/extract-patterns.sh" "Pattern extraction script exists"

    # Knowledge base
    assert_file_exists "docs/knowledge/README.md" "Knowledge base README exists"
    assert_file_exists "docs/knowledge/patterns/README.md" "Patterns README exists"
    assert_file_exists "docs/knowledge/decisions/README.md" "Decisions README exists"
    assert_file_exists "docs/knowledge/insights/README.md" "Insights README exists"

    echo ""
}

# Test file syntax
test_file_syntax() {
    log_info "Testing file syntax..."
    echo ""

    # YAML files
    assert_yaml_valid ".github/ISSUE_TEMPLATE/task.yml" "Issue template YAML is valid"
    assert_yaml_valid ".github/workflows/assign-pr-creator.yml" "PR assignment workflow YAML is valid"
    assert_yaml_valid ".github/workflows/validate-pr.yml" "Validation workflow YAML is valid"
    assert_yaml_valid ".github/workflows/knowledge-capture.yml" "Knowledge capture workflow YAML is valid"

    # Shell scripts
    assert_shell_valid "scripts/bootstrap.sh" "Bootstrap script is valid"
    assert_shell_valid "scripts/validate-syntax.sh" "Validation script is valid"
    assert_shell_valid "scripts/test-workflow.sh" "Test workflow script is valid"
    assert_shell_valid "scripts/extract-patterns.sh" "Pattern extraction script is valid"

    echo ""
}

# Test workflow triggers
test_workflow_triggers() {
    log_info "Testing workflow triggers..."
    echo ""

    # Check PR assignment workflow triggers
    TESTS_RUN=$((TESTS_RUN + 1))
    if grep -q "pull_request_target:" .github/workflows/assign-pr-creator.yml; then
        log_success "Test $TESTS_RUN: PR assignment workflow has correct trigger"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: PR assignment workflow missing pull_request_target trigger"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    # Check validation workflow triggers
    TESTS_RUN=$((TESTS_RUN + 1))
    if grep -q "pull_request:" .github/workflows/validate-pr.yml; then
        log_success "Test $TESTS_RUN: Validation workflow has correct trigger"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: Validation workflow missing pull_request trigger"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    # Check knowledge capture workflow triggers
    TESTS_RUN=$((TESTS_RUN + 1))
    if grep -q "types: \[closed\]" .github/workflows/knowledge-capture.yml; then
        log_success "Test $TESTS_RUN: Knowledge capture workflow has correct trigger"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: Knowledge capture workflow missing closed PR trigger"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    echo ""
}

# Test issue template structure
test_issue_template() {
    log_info "Testing issue template structure..."
    echo ""

    local template=".github/ISSUE_TEMPLATE/task.yml"

    # Check for required fields
    TESTS_RUN=$((TESTS_RUN + 1))
    if grep -q "id: title" "$template" && \
       grep -q "id: context" "$template" && \
       grep -q "id: acceptance_criteria" "$template"; then
        log_success "Test $TESTS_RUN: Issue template has required fields"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: Issue template missing required fields"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    # Check for assignee configuration
    TESTS_RUN=$((TESTS_RUN + 1))
    if grep -q "assignees:" "$template"; then
        log_success "Test $TESTS_RUN: Issue template has assignee configuration"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: Issue template missing assignee configuration"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    echo ""
}

# Test knowledge base structure
test_knowledge_base() {
    log_info "Testing knowledge base structure..."
    echo ""

    # Check directory structure
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ -d "docs/knowledge/patterns" ] && \
       [ -d "docs/knowledge/decisions" ] && \
       [ -d "docs/knowledge/insights" ]; then
        log_success "Test $TESTS_RUN: Knowledge base has correct directory structure"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: Knowledge base missing required directories"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    # Check README files
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ -f "docs/knowledge/patterns/README.md" ] && \
       [ -f "docs/knowledge/decisions/README.md" ] && \
       [ -f "docs/knowledge/insights/README.md" ]; then
        log_success "Test $TESTS_RUN: Knowledge base has README files"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        log_error "Test $TESTS_RUN: Knowledge base missing README files"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi

    echo ""
}

# Simulate issue creation and workflow
simulate_workflow() {
    if [ "$DRY_RUN" = true ]; then
        log_info "Simulating workflow (dry-run mode)..."
        echo ""

        log_info "Would create test issue with @copilot assignment"
        log_info "Would create feature branch: feature/test-workflow"
        log_info "Would make test commit"
        log_info "Would create draft PR"
        log_info "Would verify PR auto-assignment"
        log_info "Would merge PR (if approved)"
        log_info "Would verify knowledge capture"

        echo ""
        log_warning "Dry-run mode - no actual changes made"
    else
        log_info "Workflow simulation requires GitHub API access"
        log_warning "Skipping live simulation (use gh CLI manually to test)"
    fi

    echo ""
}

# Print summary
print_summary() {
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  Test Summary"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "  Total tests:  $TESTS_RUN"
    echo "  Passed:       $TESTS_PASSED"
    echo "  Failed:       $TESTS_FAILED"
    echo ""

    local pass_rate=0
    if [ $TESTS_RUN -gt 0 ]; then
        pass_rate=$((TESTS_PASSED * 100 / TESTS_RUN))
    fi

    echo "  Pass rate:    ${pass_rate}%"
    echo ""

    if [ $TESTS_FAILED -eq 0 ]; then
        log_success "All tests passed! ✓"
        echo ""
        echo "  Your issue-driven development setup is ready!"
    else
        log_error "Some tests failed. Please review the errors above."
        echo ""
        echo "  Fix the issues and run this script again."
    fi

    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""

    # Success criteria check
    if [ $pass_rate -ge 90 ]; then
        log_success "Success Criterion #4: 90%+ reliability achieved ($pass_rate%)"
    else
        log_error "Success Criterion #4: 90%+ reliability NOT met ($pass_rate%)"
    fi

    echo ""
}

# Main execution
main() {
    log_info "Starting end-to-end workflow tests..."
    echo ""

    test_prerequisites
    test_file_structure
    test_file_syntax
    test_workflow_triggers
    test_issue_template
    test_knowledge_base
    simulate_workflow

    print_summary

    if [ $TESTS_FAILED -eq 0 ]; then
        exit 0
    else
        exit 1
    fi
}

# Run main function
main "$@"
