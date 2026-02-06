#!/usr/bin/env bash
#
# Bootstrap @copilot Issue Automation System
#
# Usage: ./scripts/bootstrap.sh [--dry-run] [--verify-only]
#
# This script sets up the complete @copilot automation system from scratch.
# It creates all necessary files, directories, and configurations.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DRY_RUN=false
VERIFY_ONLY=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --verify-only)
            VERIFY_ONLY=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if running in a git repository
check_git_repo() {
    log_info "Checking git repository..."
    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        log_error "Not a git repository. Please initialize git first."
        exit 1
    fi
    log_success "Git repository detected"
}

# Create directory structure
create_directories() {
    log_info "Creating directory structure..."

    local dirs=(
        ".github/ISSUE_TEMPLATE"
        ".github/workflows"
        "docs/knowledge/patterns"
        "docs/knowledge/decisions"
        "docs/knowledge/insights/logs"
        "docs/knowledge/insights/improvements"
        "scripts"
    )

    for dir in "${dirs[@]}"; do
        if [ "$DRY_RUN" = true ]; then
            echo "  Would create: $dir"
        else
            mkdir -p "$REPO_ROOT/$dir"
            echo "  Created: $dir"
        fi
    done

    log_success "Directory structure created"
}

# Check if required files exist
check_files() {
    log_info "Checking required files..."

    local required_files=(
        ".github/ISSUE_TEMPLATE/copilot-task.yml"
        ".github/workflows/issue-copilot.yml"
        ".github/workflows/pr-auto-review.yml"
        ".github/workflows/self-improvement.yml"
        ".github/CODEOWNERS"
        ".github/copilot-instructions.md"
        "docs/knowledge/README.md"
        "docs/knowledge/patterns/README.md"
        "docs/knowledge/patterns/issue-workflow.md"
        "docs/knowledge/decisions/README.md"
        "docs/knowledge/decisions/001-copilot-automation.md"
        "docs/knowledge/insights/README.md"
        "docs/knowledge/insights/agent-performance.md"
        "scripts/bootstrap.sh"
        "scripts/verify-system.sh"
        "scripts/run-test-issue.sh"
        "README.md"
    )

    local missing=0
    for file in "${required_files[@]}"; do
        if [ -f "$REPO_ROOT/$file" ]; then
            echo -e "  ${GREEN}[OK]${NC} $file"
        else
            echo -e "  ${RED}[MISSING]${NC} $file"
            ((missing++))
        fi
    done

    if [ $missing -gt 0 ]; then
        log_warn "$missing files missing"
        return 1
    else
        log_success "All required files present"
        return 0
    fi
}

# Initialize empty log files
init_logs() {
    log_info "Initializing log files..."

    local logs=(
        "docs/knowledge/insights/logs/issues.jsonl"
        "docs/knowledge/insights/logs/prs.jsonl"
        "docs/knowledge/insights/improvements/applied.jsonl"
    )

    for log in "${logs[@]}"; do
        if [ "$DRY_RUN" = true ]; then
            echo "  Would create: $log"
        elif [ ! -f "$REPO_ROOT/$log" ]; then
            touch "$REPO_ROOT/$log"
            echo "  Created: $log"
        else
            echo "  Exists: $log"
        fi
    done

    log_success "Log files initialized"
}

# Create .yamllint configuration
create_yamllint_config() {
    log_info "Creating yamllint configuration..."

    local config_file="$REPO_ROOT/.yamllint.yml"

    if [ "$DRY_RUN" = true ]; then
        echo "  Would create: .yamllint.yml"
    elif [ ! -f "$config_file" ]; then
        cat > "$config_file" << 'EOF'
extends: default

rules:
  line-length:
    max: 120
    level: warning
  document-start: disable
  truthy:
    allowed-values: ['true', 'false', 'yes', 'no']
EOF
        echo "  Created: .yamllint.yml"
    else
        echo "  Exists: .yamllint.yml"
    fi

    log_success "Yamllint configuration ready"
}

# Run verification script
run_verification() {
    log_info "Running system verification..."

    if [ -f "$REPO_ROOT/scripts/verify-system.sh" ]; then
        chmod +x "$REPO_ROOT/scripts/verify-system.sh"
        if "$REPO_ROOT/scripts/verify-system.sh"; then
            log_success "System verification passed"
            return 0
        else
            log_error "System verification failed"
            return 1
        fi
    else
        log_warn "Verification script not found"
        return 1
    fi
}

# Run test issue
run_test_issue() {
    log_info "Running test issue simulation..."

    if [ -f "$REPO_ROOT/scripts/run-test-issue.sh" ]; then
        chmod +x "$REPO_ROOT/scripts/run-test-issue.sh"
        if "$REPO_ROOT/scripts/run-test-issue.sh" --dry-run; then
            log_success "Test issue simulation passed"
            return 0
        else
            log_error "Test issue simulation failed"
            return 1
        fi
    else
        log_warn "Test issue script not found"
        return 1
    fi
}

# Print summary
print_summary() {
    echo ""
    echo "=========================================="
    echo "  @copilot Automation Bootstrap Complete"
    echo "=========================================="
    echo ""
    echo "Created components:"
    echo "  - Issue template for @copilot tasks"
    echo "  - Issue automation workflow"
    echo "  - PR auto-review workflow"
    echo "  - Self-improvement analyzer"
    echo "  - Knowledge base structure"
    echo "  - Verification scripts"
    echo ""
    echo "Next steps:"
    echo "  1. Enable GitHub Actions in repository settings"
    echo "  2. Configure @copilot access permissions"
    echo "  3. Create your first issue using the template"
    echo "  4. Monitor workflow execution in Actions tab"
    echo ""
    echo "For more information, see README.md"
    echo ""
}

# Main execution
main() {
    echo ""
    echo "=========================================="
    echo "  @copilot Issue Automation Bootstrap"
    echo "=========================================="
    echo ""

    if [ "$DRY_RUN" = true ]; then
        log_warn "Running in DRY-RUN mode - no changes will be made"
    fi

    if [ "$VERIFY_ONLY" = true ]; then
        log_info "Running in VERIFY-ONLY mode"
        check_git_repo
        check_files
        run_verification
        exit $?
    fi

    # Run bootstrap steps
    check_git_repo
    create_directories
    init_logs
    create_yamllint_config

    # Verify setup
    if check_files; then
        log_success "Bootstrap completed successfully"
    else
        log_warn "Bootstrap completed with missing files"
        log_info "Some files need to be created manually"
    fi

    # Run verification if not dry-run
    if [ "$DRY_RUN" = false ]; then
        run_verification || true
        run_test_issue || true
    fi

    print_summary
}

main "$@"
