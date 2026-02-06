#!/usr/bin/env bash
# Bootstrap script for @copilot issue-driven development system
# This script sets up the complete automation system from a bare repository

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Header
echo ""
echo "=========================================="
echo " @copilot Issue-Driven Development Setup"
echo "=========================================="
echo ""

# Check prerequisites
log_info "Checking prerequisites..."

check_command() {
    if command -v "$1" &> /dev/null; then
        log_success "$1 found"
        return 0
    else
        log_warning "$1 not found (optional)"
        return 1
    fi
}

HAVE_YAMLLINT=false
HAVE_SHELLCHECK=false

if check_command yamllint; then HAVE_YAMLLINT=true; fi
if check_command shellcheck; then HAVE_SHELLCHECK=true; fi

# Create directory structure
log_info "Creating directory structure..."

directories=(
    ".github"
    ".github/ISSUE_TEMPLATE"
    ".github/workflows"
    "docs/knowledge"
    "docs/knowledge/patterns"
    "docs/knowledge/decisions"
    "docs/knowledge/insights"
    "logs/executions"
    "scripts"
)

for dir in "${directories[@]}"; do
    mkdir -p "$PROJECT_ROOT/$dir"
    log_success "Created $dir/"
done

# Create .gitkeep for empty directories
touch "$PROJECT_ROOT/logs/executions/.gitkeep"
log_success "Created logs/executions/.gitkeep"

# Verify core files exist
log_info "Verifying core files..."

required_files=(
    ".github/CODEOWNERS"
    ".github/copilot-instructions.md"
    ".github/ISSUE_TEMPLATE/copilot-task.yml"
    ".github/ISSUE_TEMPLATE/self-improvement.yml"
    ".github/workflows/copilot-issue-handler.yml"
    ".github/workflows/copilot-pr-review.yml"
    ".github/workflows/self-improvement-analyzer.yml"
    "docs/knowledge/README.md"
    "docs/knowledge/patterns/README.md"
    "docs/knowledge/patterns/issue-to-pr-workflow.md"
    "docs/knowledge/decisions/README.md"
    "docs/knowledge/decisions/001-copilot-automation.md"
    "docs/knowledge/insights/README.md"
    "docs/knowledge/insights/multi-agent-compatibility.md"
)

missing_files=()
for file in "${required_files[@]}"; do
    if [[ -f "$PROJECT_ROOT/$file" ]]; then
        log_success "Found $file"
    else
        log_warning "Missing $file"
        missing_files+=("$file")
    fi
done

if [[ ${#missing_files[@]} -gt 0 ]]; then
    log_warning "Some files are missing. They should be created from the SOLUTION.md template."
fi

# Run syntax validation if tools are available
log_info "Running syntax validation..."

validation_passed=true

if $HAVE_YAMLLINT; then
    log_info "Validating YAML files..."
    for file in "$PROJECT_ROOT"/.github/workflows/*.yml "$PROJECT_ROOT"/.github/ISSUE_TEMPLATE/*.yml; do
        if [[ -f "$file" ]]; then
            if yamllint -d relaxed "$file" 2>/dev/null; then
                log_success "YAML valid: $(basename "$file")"
            else
                log_error "YAML invalid: $(basename "$file")"
                validation_passed=false
            fi
        fi
    done
else
    log_warning "Skipping YAML validation (yamllint not installed)"
fi

if $HAVE_SHELLCHECK; then
    log_info "Validating shell scripts..."
    for file in "$PROJECT_ROOT"/scripts/*.sh; do
        if [[ -f "$file" ]]; then
            if shellcheck "$file" 2>/dev/null; then
                log_success "Shell valid: $(basename "$file")"
            else
                log_error "Shell invalid: $(basename "$file")"
                validation_passed=false
            fi
        fi
    done
else
    log_warning "Skipping shell script validation (shellcheck not installed)"
fi

# Summary
echo ""
echo "=========================================="
echo " Bootstrap Summary"
echo "=========================================="
echo ""

log_info "Directories created: ${#directories[@]}"
log_info "Required files checked: ${#required_files[@]}"
log_info "Missing files: ${#missing_files[@]}"

if $validation_passed; then
    log_success "Syntax validation: PASSED"
else
    log_error "Syntax validation: FAILED"
fi

echo ""
log_info "Next steps:"
echo "  1. Replace @owner in .github/CODEOWNERS with your GitHub username"
echo "  2. Commit and push the changes"
echo "  3. Create a test issue using the 'Copilot Task' template"
echo "  4. Watch the Actions tab for workflow execution"
echo ""

if [[ ${#missing_files[@]} -eq 0 ]] && $validation_passed; then
    log_success "Bootstrap complete! System is ready."
    exit 0
else
    log_warning "Bootstrap complete with warnings. Review issues above."
    exit 0
fi
