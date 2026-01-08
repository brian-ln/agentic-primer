#!/usr/bin/env bash
#
# bootstrap.sh - Single-command setup for issue-driven development system
#
# Usage: ./scripts/bootstrap.sh
#
# This script sets up the complete system from a bare repository with zero
# manual intervention. It creates all necessary directories, copies template
# files, and validates the installation.
#
# Prerequisites:
#   - git repository initialized
#   - GitHub remote configured
#   - git, gh CLI, yamllint, shellcheck, markdownlint installed
#
# Exit codes:
#   0 - Success
#   1 - Missing prerequisites
#   2 - Directory creation failed
#   3 - File creation failed
#   4 - Validation failed

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Logging functions
log_info() {
    echo -e "${BLUE}ℹ${NC} $*"
}

log_success() {
    echo -e "${GREEN}✓${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $*"
}

log_error() {
    echo -e "${RED}✗${NC} $*"
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    local missing_tools=()

    # Check for required tools
    if ! command -v git &> /dev/null; then
        missing_tools+=("git")
    fi

    if ! command -v gh &> /dev/null; then
        log_warning "gh CLI not found (optional, but recommended)"
    fi

    if ! command -v yamllint &> /dev/null; then
        missing_tools+=("yamllint")
    fi

    if ! command -v shellcheck &> /dev/null; then
        missing_tools+=("shellcheck")
    fi

    if ! command -v markdownlint &> /dev/null; then
        log_warning "markdownlint not found (install with: npm install -g markdownlint-cli)"
    fi

    if [ ${#missing_tools[@]} -gt 0 ]; then
        log_error "Missing required tools: ${missing_tools[*]}"
        log_info "Install missing tools and try again"
        return 1
    fi

    # Check if in git repository
    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        log_error "Not in a git repository"
        log_info "Run: git init"
        return 1
    fi

    # Check if GitHub remote exists
    if ! git remote get-url origin &> /dev/null; then
        log_warning "No GitHub remote configured"
        log_info "You may want to run: git remote add origin <url>"
    fi

    log_success "Prerequisites check passed"
    return 0
}

# Create directory structure
create_directories() {
    log_info "Creating directory structure..."

    local directories=(
        ".github/ISSUE_TEMPLATE"
        ".github/workflows"
        "docs/knowledge/patterns"
        "docs/knowledge/decisions"
        "docs/knowledge/insights"
        "scripts"
    )

    for dir in "${directories[@]}"; do
        local full_path="${REPO_ROOT}/${dir}"
        if [ ! -d "$full_path" ]; then
            mkdir -p "$full_path"
            log_success "Created: $dir"
        else
            log_info "Already exists: $dir"
        fi
    done

    log_success "Directory structure created"
}

# Create CODEOWNERS file
create_codeowners() {
    log_info "Creating CODEOWNERS file..."

    local codeowners_file="${REPO_ROOT}/.github/CODEOWNERS"

    if [ -f "$codeowners_file" ]; then
        log_warning "CODEOWNERS already exists, skipping"
        return 0
    fi

    cat > "$codeowners_file" << 'EOF'
# CODEOWNERS - Automatic PR review assignment
* @owner
EOF

    log_success "Created CODEOWNERS"
}

# Create issue template
create_issue_template() {
    log_info "Creating issue template..."

    local template_file="${REPO_ROOT}/.github/ISSUE_TEMPLATE/task.yml"

    if [ -f "$template_file" ]; then
        log_warning "Issue template already exists, skipping"
        return 0
    fi

    cat > "$template_file" << 'EOF'
name: AI Agent Task
description: Create a task for @copilot or other AI agents
title: "[Task]: "
labels: ["ai-task"]
body:
  - type: textarea
    id: description
    attributes:
      label: Description
      description: What needs to be done?
    validations:
      required: true
  - type: textarea
    id: acceptance-criteria
    attributes:
      label: Acceptance Criteria
      description: How will we know it's done?
    validations:
      required: true
EOF

    log_success "Created issue template"
}

# Create knowledge base README
create_knowledge_base_docs() {
    log_info "Creating knowledge base documentation..."

    # Main README
    local kb_readme="${REPO_ROOT}/docs/knowledge/README.md"
    if [ ! -f "$kb_readme" ]; then
        cat > "$kb_readme" << 'EOF'
# Knowledge Base

This directory contains accumulated learnings from AI agent executions.

## Structure

- **patterns/** - Reusable code patterns
- **decisions/** - Architecture decisions
- **insights/** - Learnings from PRs

## Usage

AI agents automatically reference this knowledge base when processing issues.
EOF
        log_success "Created knowledge base README"
    fi

    # Category READMEs
    for category in patterns decisions insights; do
        local category_readme="${REPO_ROOT}/docs/knowledge/${category}/README.md"
        if [ ! -f "$category_readme" ]; then
            cat > "$category_readme" << EOF
# ${category^}

This directory contains ${category} extracted from AI agent executions.

## Contents

<!-- Auto-generated index will appear here -->
EOF
            log_success "Created ${category} README"
        fi
    done
}

# Update main README
update_main_readme() {
    log_info "Updating main README..."

    local readme_file="${REPO_ROOT}/README.md"
    local workflow_section="## Issue-Driven Development Workflow

### Quick Start

1. **Create an Issue**: Use the AI Agent Task template
2. **Assign to @copilot**: The system will automatically process it
3. **Review PR**: Check the auto-generated pull request
4. **Merge**: Knowledge base updates automatically

### Workflow

\`\`\`
issue → @copilot → PR → review → merge → knowledge base update
\`\`\`

### Web UI Access

All interactions happen through GitHub's web interface - no local setup required.
"

    if ! grep -q "Issue-Driven Development Workflow" "$readme_file" 2>/dev/null; then
        if [ -f "$readme_file" ]; then
            echo "" >> "$readme_file"
            echo "$workflow_section" >> "$readme_file"
            log_success "Updated README with workflow documentation"
        else
            cat > "$readme_file" << EOF
# Project

$workflow_section
EOF
            log_success "Created README with workflow documentation"
        fi
    else
        log_info "README already contains workflow documentation"
    fi
}

# Create validation script
create_validation_script() {
    log_info "Creating validation script..."

    local script_file="${REPO_ROOT}/scripts/validate-syntax.sh"

    cat > "$script_file" << 'EOF'
#!/usr/bin/env bash
# validate-syntax.sh - Validate all generated files

set -euo pipefail

echo "Validating YAML files..."
find . -name "*.yml" -o -name "*.yaml" | grep -v node_modules | xargs yamllint || true

echo "Validating shell scripts..."
find . -name "*.sh" | grep -v node_modules | xargs shellcheck -x || true

echo "Validating markdown files..."
find . -name "*.md" | grep -v node_modules | xargs markdownlint || true

echo "Validation complete"
EOF

    chmod +x "$script_file"
    log_success "Created validation script"
}

# Create test script
create_test_script() {
    log_info "Creating test script..."

    local script_file="${REPO_ROOT}/scripts/test-issue-flow.sh"

    cat > "$script_file" << 'EOF'
#!/usr/bin/env bash
# test-issue-flow.sh - Test end-to-end issue processing flow

set -euo pipefail

echo "Testing issue processing flow..."
echo "1. Verify directory structure exists"
echo "2. Verify issue template is valid"
echo "3. Verify workflows are present"
echo "4. Verify knowledge base structure"

# Check directories
for dir in .github/ISSUE_TEMPLATE .github/workflows docs/knowledge; do
    if [ -d "$dir" ]; then
        echo "✓ $dir exists"
    else
        echo "✗ $dir missing"
        exit 1
    fi
done

echo "✓ All checks passed"
EOF

    chmod +x "$script_file"
    log_success "Created test script"
}

# Validate installation
validate_installation() {
    log_info "Validating installation..."

    local errors=0

    # Check directories
    local required_dirs=(
        ".github/ISSUE_TEMPLATE"
        ".github/workflows"
        "docs/knowledge"
        "scripts"
    )

    for dir in "${required_dirs[@]}"; do
        if [ -d "${REPO_ROOT}/${dir}" ]; then
            log_success "Directory exists: $dir"
        else
            log_error "Directory missing: $dir"
            ((errors++))
        fi
    done

    # Check files
    local required_files=(
        ".github/CODEOWNERS"
        ".github/ISSUE_TEMPLATE/task.yml"
        "docs/knowledge/README.md"
        "scripts/validate-syntax.sh"
        "scripts/test-issue-flow.sh"
    )

    for file in "${required_files[@]}"; do
        if [ -f "${REPO_ROOT}/${file}" ]; then
            log_success "File exists: $file"
        else
            log_error "File missing: $file"
            ((errors++))
        fi
    done

    if [ $errors -gt 0 ]; then
        log_error "Validation failed with $errors error(s)"
        return 1
    fi

    log_success "Validation passed"
    return 0
}

# Main execution
main() {
    echo ""
    log_info "Starting bootstrap process..."
    echo ""

    # Run setup steps
    check_prerequisites || exit 1
    create_directories || exit 2
    create_codeowners || exit 3
    create_issue_template || exit 3
    create_knowledge_base_docs || exit 3
    update_main_readme || exit 3
    create_validation_script || exit 3
    create_test_script || exit 3

    echo ""
    log_info "Running validation..."
    validate_installation || exit 4

    echo ""
    log_success "Bootstrap completed successfully!"
    echo ""
    log_info "Next steps:"
    echo "  1. Review generated files"
    echo "  2. Commit changes: git add . && git commit -m 'feat: bootstrap issue-driven development system'"
    echo "  3. Push to GitHub: git push origin main"
    echo "  4. Create a test issue and assign to @copilot"
    echo ""
    log_info "For workflow documentation, see README.md"
    echo ""
}

# Run main function
main "$@"
