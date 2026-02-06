#!/usr/bin/env bash
#
# Bootstrap Script for @copilot Issue Automation
#
# Purpose: Single-command setup from bare repository
# Usage: ./scripts/bootstrap-v2.sh
# Exit Codes:
#   0: Success
#   1: Missing prerequisites
#   2: Permission errors
#   3: Git repository not found

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Cleanup function
cleanup() {
    if [ $? -ne 0 ]; then
        log_error "Bootstrap failed. Please check errors above."
    fi
}

trap cleanup EXIT

# Banner
echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║         @copilot Issue Automation Bootstrap               ║"
echo "║         Single-Command Setup                               ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Step 1: Validate prerequisites
log_info "Step 1/7: Validating prerequisites..."

MISSING_DEPS=()

# Check Git
if ! command -v git &> /dev/null; then
    MISSING_DEPS+=("git")
else
    log_success "Git found: $(git --version | head -n1)"
fi

# Check if in a Git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    log_error "Not a Git repository. Run 'git init' first."
    exit 3
fi

# Check GitHub CLI
if ! command -v gh &> /dev/null; then
    log_warning "GitHub CLI (gh) not found - some features will be limited"
    log_info "Install from: https://cli.github.com/"
else
    log_success "GitHub CLI found: $(gh --version | head -n1)"
fi

# Check yamllint
if ! command -v yamllint &> /dev/null; then
    log_warning "yamllint not found - will attempt to install"
    MISSING_DEPS+=("yamllint")
else
    log_success "yamllint found: $(yamllint --version)"
fi

# Check shellcheck
if ! command -v shellcheck &> /dev/null; then
    log_warning "shellcheck not found - will attempt to install"
    MISSING_DEPS+=("shellcheck")
else
    log_success "shellcheck found: $(shellcheck --version | head -n1)"
fi

# Check markdownlint
if ! command -v markdownlint &> /dev/null; then
    log_warning "markdownlint not found - will attempt to install"
    MISSING_DEPS+=("markdownlint")
else
    log_success "markdownlint found"
fi

# Check jq
if ! command -v jq &> /dev/null; then
    log_warning "jq not found - will attempt to install"
    MISSING_DEPS+=("jq")
else
    log_success "jq found: $(jq --version)"
fi

# Step 2: Install missing dependencies
if [ ${#MISSING_DEPS[@]} -gt 0 ]; then
    log_info "Step 2/7: Installing missing dependencies..."

    for dep in "${MISSING_DEPS[@]}"; do
        case "$dep" in
            yamllint)
                log_info "Installing yamllint..."
                if command -v pip3 &> /dev/null; then
                    pip3 install --user yamllint
                    log_success "yamllint installed"
                elif command -v pip &> /dev/null; then
                    pip install --user yamllint
                    log_success "yamllint installed"
                else
                    log_error "Cannot install yamllint: pip not found"
                    exit 1
                fi
                ;;
            shellcheck)
                log_info "Installing shellcheck..."
                if [[ "$OSTYPE" == "darwin"* ]]; then
                    if command -v brew &> /dev/null; then
                        brew install shellcheck
                        log_success "shellcheck installed"
                    else
                        log_error "Cannot install shellcheck: Homebrew not found"
                        exit 1
                    fi
                elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
                    if command -v apt-get &> /dev/null; then
                        sudo apt-get update && sudo apt-get install -y shellcheck
                        log_success "shellcheck installed"
                    elif command -v yum &> /dev/null; then
                        sudo yum install -y shellcheck
                        log_success "shellcheck installed"
                    else
                        log_error "Cannot install shellcheck: No package manager found"
                        exit 1
                    fi
                else
                    log_error "Cannot install shellcheck: Unsupported OS"
                    exit 1
                fi
                ;;
            markdownlint)
                log_info "Installing markdownlint..."
                if command -v npm &> /dev/null; then
                    npm install -g markdownlint-cli
                    log_success "markdownlint installed"
                else
                    log_error "Cannot install markdownlint: npm not found"
                    exit 1
                fi
                ;;
            jq)
                log_info "Installing jq..."
                if [[ "$OSTYPE" == "darwin"* ]]; then
                    if command -v brew &> /dev/null; then
                        brew install jq
                        log_success "jq installed"
                    fi
                elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
                    if command -v apt-get &> /dev/null; then
                        sudo apt-get update && sudo apt-get install -y jq
                        log_success "jq installed"
                    elif command -v yum &> /dev/null; then
                        sudo yum install -y jq
                        log_success "jq installed"
                    fi
                fi
                ;;
        esac
    done
else
    log_success "Step 2/7: All dependencies already installed"
fi

# Step 3: Create directory structure
log_info "Step 3/7: Creating directory structure..."

DIRS=(
    ".github"
    ".github/ISSUE_TEMPLATE"
    ".github/workflows"
    "scripts"
    "docs/knowledge"
    "docs/knowledge/patterns"
    "docs/knowledge/decisions"
    "docs/knowledge/insights"
)

for dir in "${DIRS[@]}"; do
    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
        log_success "Created: $dir"
    else
        log_info "Already exists: $dir"
    fi
done

# Step 4: Copy/create configuration files
log_info "Step 4/7: Setting up configuration files..."

# Get repository owner for CODEOWNERS
REPO_OWNER=""
if command -v gh &> /dev/null && gh auth status &> /dev/null; then
    REPO_OWNER=$(gh api user --jq .login 2>/dev/null || echo "")
fi

if [ -z "$REPO_OWNER" ]; then
    REPO_OWNER=$(git config user.name || echo "REPO_OWNER")
    log_warning "Could not detect GitHub username, using: $REPO_OWNER"
    log_info "Update .github/CODEOWNERS manually with your GitHub username"
fi

# Create CODEOWNERS if it doesn't exist
if [ ! -f ".github/CODEOWNERS" ]; then
    cat > .github/CODEOWNERS << EOF
# CODEOWNERS
*       @${REPO_OWNER}
/.github/                       @${REPO_OWNER}
/scripts/*.sh                   @${REPO_OWNER}
/docs/knowledge/                @${REPO_OWNER}
EOF
    log_success "Created: .github/CODEOWNERS"
fi

# Create .markdownlint.json if it doesn't exist
if [ ! -f ".markdownlint.json" ]; then
    cat > .markdownlint.json << 'EOF'
{
  "default": true,
  "MD003": { "style": "atx" },
  "MD007": { "indent": 2 },
  "MD013": { "line_length": 120 },
  "MD024": { "siblings_only": true },
  "MD033": { "allowed_elements": ["details", "summary", "br"] },
  "MD034": false,
  "MD041": false
}
EOF
    log_success "Created: .markdownlint.json"
fi

# Step 5: Set file permissions
log_info "Step 5/7: Setting file permissions..."

if [ -d "scripts" ]; then
    for script in scripts/*.sh; do
        if [ -f "$script" ]; then
            chmod +x "$script"
            log_success "Made executable: $script"
        fi
    done
fi

# Step 6: Initialize knowledge base
log_info "Step 6/7: Initializing knowledge base..."

# Create knowledge base README files if they don't exist
if [ ! -f "docs/knowledge/README.md" ]; then
    cat > docs/knowledge/README.md << 'EOF'
# Knowledge Base

This directory contains institutional knowledge for AI agents working on this repository.

## Structure

- **patterns/**: Reusable code patterns and solutions
- **decisions/**: Architecture Decision Records (ADRs)
- **insights/**: Learnings from execution logs and PR analysis

## Usage

AI agents (@copilot) search this knowledge base before implementing features to:
- Reuse proven patterns
- Understand past decisions
- Avoid repeated mistakes

## Contributing

Knowledge base updates happen automatically via the `knowledge-base-update` workflow
when PRs are merged. Manual contributions are also welcome.

## Search

Use grep/ripgrep to search across all knowledge:

```bash
grep -r "authentication" docs/knowledge/
rg "error handling" docs/knowledge/
```
EOF
    log_success "Created: docs/knowledge/README.md"
fi

if [ ! -f "docs/knowledge/patterns/README.md" ]; then
    cat > docs/knowledge/patterns/README.md << 'EOF'
# Pattern Library

Catalog of reusable patterns discovered through PR analysis.

## Index

(Patterns will be added automatically as PRs are merged)

## Categories

- GitHub Actions workflows
- Shell scripting patterns
- Error handling
- Testing strategies
- Configuration management
EOF
    log_success "Created: docs/knowledge/patterns/README.md"
fi

if [ ! -f "docs/knowledge/decisions/README.md" ]; then
    cat > docs/knowledge/decisions/README.md << 'EOF'
# Architecture Decision Records

Documents key architectural decisions and their rationale.

## Index

(Decisions will be added as system evolves)

## Format

Each ADR follows this structure:
- Title
- Status (proposed, accepted, deprecated)
- Context
- Decision
- Consequences
EOF
    log_success "Created: docs/knowledge/decisions/README.md"
fi

if [ ! -f "docs/knowledge/insights/README.md" ]; then
    cat > docs/knowledge/insights/README.md << 'EOF'
# Insights

Learnings extracted from PR analysis and execution logs.

## Index

(Insights will be added automatically as PRs are merged)

## Format

- Date-stamped entries (YYYY-MM-DD-topic.md)
- Links to source PRs
- Key learnings and takeaways
EOF
    log_success "Created: docs/knowledge/insights/README.md"
fi

# Step 7: Health check
log_info "Step 7/7: Running health check..."

HEALTH_PASS=true

# Check directory structure
for dir in "${DIRS[@]}"; do
    if [ ! -d "$dir" ]; then
        log_error "Missing directory: $dir"
        HEALTH_PASS=false
    fi
done

# Check critical files
CRITICAL_FILES=(
    ".github/CODEOWNERS"
    ".markdownlint.json"
    "docs/knowledge/README.md"
)

for file in "${CRITICAL_FILES[@]}"; do
    if [ ! -f "$file" ]; then
        log_error "Missing file: $file"
        HEALTH_PASS=false
    fi
done

# Check if validation tools work
if ! yamllint --version &> /dev/null; then
    log_error "yamllint not working"
    HEALTH_PASS=false
fi

if ! shellcheck --version &> /dev/null; then
    log_error "shellcheck not working"
    HEALTH_PASS=false
fi

# Summary
echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║                    Bootstrap Summary                       ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

if [ "$HEALTH_PASS" = true ]; then
    log_success "Bootstrap completed successfully!"
    echo ""
    log_info "Next steps:"
    echo "  1. Review and update .github/CODEOWNERS with your GitHub username"
    echo "  2. Copy workflow files from templates to .github/workflows/"
    echo "  3. Run: scripts/validate-syntax.sh"
    echo "  4. Run: scripts/test-issue-flow.sh"
    echo "  5. Create your first @copilot issue!"
    echo ""
    log_info "Documentation: See README.md for usage guide"
    exit 0
else
    log_error "Bootstrap completed with errors - see above"
    exit 2
fi
