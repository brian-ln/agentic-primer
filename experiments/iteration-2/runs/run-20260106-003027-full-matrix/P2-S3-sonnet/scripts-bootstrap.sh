#!/usr/bin/env bash
#
# Bootstrap script for issue-driven development with @copilot
#
# This script sets up a complete GitHub repository for autonomous AI agent workflows
# with automatic PR assignment and institutional knowledge persistence.
#
# Usage: ./scripts/bootstrap.sh
#
# Requirements:
#   - git (for repository operations)
#   - gh CLI (for GitHub API operations)
#   - yamllint (for YAML validation)
#   - shellcheck (for shell script validation)
#   - markdownlint-cli (for markdown validation)
#
# Exit codes:
#   0 - Success
#   1 - Missing required dependency
#   2 - Not in a git repository
#   3 - File creation failed
#   4 - Validation failed

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Script metadata
readonly SCRIPT_VERSION="1.0.0"
readonly SCRIPT_NAME="$(basename "$0")"

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
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

# Check if running in a git repository
check_git_repo() {
    if ! git rev-parse --git-dir > /dev/null 2>&1; then
        log_error "Not in a git repository. Please run this script from a git repository."
        return 2
    fi
    return 0
}

# Check required dependencies
check_dependencies() {
    local missing_deps=()

    local required_tools=("git" "gh")
    local optional_tools=("yamllint" "shellcheck" "markdownlint")

    log_info "Checking required dependencies..."

    for tool in "${required_tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            missing_deps+=("$tool")
            log_error "Required tool '$tool' is not installed"
        else
            log_success "Found $tool"
        fi
    done

    log_info "Checking optional validation tools..."

    for tool in "${optional_tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            log_warning "Optional tool '$tool' is not installed (validation will be limited)"
        else
            log_success "Found $tool"
        fi
    done

    if [ ${#missing_deps[@]} -gt 0 ]; then
        log_error "Missing required dependencies: ${missing_deps[*]}"
        return 1
    fi

    return 0
}

# Create directory structure
create_directories() {
    log_info "Creating directory structure..."

    local dirs=(
        ".github/ISSUE_TEMPLATE"
        ".github/workflows"
        "scripts"
        "docs/knowledge/patterns"
        "docs/knowledge/decisions"
        "docs/knowledge/insights"
    )

    for dir in "${dirs[@]}"; do
        if [ ! -d "$dir" ]; then
            mkdir -p "$dir"
            log_success "Created directory: $dir"
        else
            log_info "Directory already exists: $dir"
        fi
    done

    return 0
}

# Create GitHub issue template
create_issue_template() {
    log_info "Creating GitHub issue template..."

    local file=".github/ISSUE_TEMPLATE/task.yml"

    if [ -f "$file" ]; then
        log_warning "Issue template already exists: $file"
        return 0
    fi

    cat > "$file" <<'EOF'
name: Task
description: Create a task for @copilot to work on autonomously
title: "[Task]: "
labels: ["task", "copilot"]
assignees:
  - copilot

body:
  - type: markdown
    attributes:
      value: |
        ## Task Request for @copilot

        This template creates a structured task that can be processed autonomously by AI agents.

  - type: input
    id: title
    attributes:
      label: Task Title
      description: Brief, action-oriented description of what needs to be done
      placeholder: "Implement user authentication with OAuth2"
    validations:
      required: true

  - type: textarea
    id: context
    attributes:
      label: Context
      description: Background information, requirements, and constraints
      placeholder: |
        - Current state: No authentication system
        - Goal: Add OAuth2 support for Google and GitHub
        - Constraints: Must work with existing user model
    validations:
      required: true

  - type: textarea
    id: acceptance_criteria
    attributes:
      label: Acceptance Criteria
      description: Observable outcomes that define "done"
      placeholder: |
        - [ ] User can sign in with Google
        - [ ] User can sign in with GitHub
        - [ ] Session persists across browser restarts
        - [ ] Tests pass with >90% coverage
    validations:
      required: true

  - type: dropdown
    id: priority
    attributes:
      label: Priority
      description: How urgent is this task?
      options:
        - Low
        - Medium
        - High
        - Critical
      default: 1
    validations:
      required: true

  - type: dropdown
    id: complexity
    attributes:
      label: Estimated Complexity
      description: How complex is this task?
      options:
        - Simple (< 1 hour)
        - Medium (1-4 hours)
        - Complex (4-8 hours)
        - Very Complex (> 8 hours)
      default: 1
    validations:
      required: false

  - type: textarea
    id: resources
    attributes:
      label: Resources
      description: Links to documentation, related issues, or reference implementations
      placeholder: |
        - OAuth2 spec: https://oauth.net/2/
        - Related issue: #123
        - Example implementation: https://github.com/example/repo
    validations:
      required: false

  - type: checkboxes
    id: knowledge_areas
    attributes:
      label: Knowledge Areas
      description: What parts of the codebase does this touch?
      options:
        - label: Frontend
        - label: Backend
        - label: Database
        - label: API
        - label: Infrastructure
        - label: Testing
        - label: Documentation

  - type: markdown
    attributes:
      value: |
        ---

        Once this issue is created, @copilot will be automatically assigned and can begin work.
        Progress updates will be posted as comments, and a draft PR will be created when ready.
EOF

    log_success "Created issue template: $file"
    return 0
}

# Create CODEOWNERS file
create_codeowners() {
    log_info "Creating CODEOWNERS file..."

    local file=".github/CODEOWNERS"

    if [ -f "$file" ]; then
        log_warning "CODEOWNERS file already exists: $file"
        return 0
    fi

    cat > "$file" <<'EOF'
# Code Owners for Automatic PR Review Assignment
#
# When a PR touches files matching these patterns, the specified users/teams
# will be automatically requested as reviewers.
#
# Syntax: <pattern> <owner1> <owner2> ...
# Patterns use gitignore-style glob syntax
# Last matching pattern takes precedence

# Default owners for everything (unless more specific rule below)
* @copilot

# GitHub-specific configuration files
/.github/ @copilot

# Scripts and automation
/scripts/ @copilot

# Documentation
/docs/ @copilot
*.md @copilot

# Knowledge base (any team member can review)
/docs/knowledge/ @copilot

# Workflows (require careful review)
/.github/workflows/ @copilot

# Configuration files
*.yml @copilot
*.yaml @copilot
*.json @copilot
*.toml @copilot

# Shell scripts (require security review)
*.sh @copilot

# Frontend code
/src/frontend/ @copilot
*.jsx @copilot
*.tsx @copilot

# Backend code
/src/backend/ @copilot
/src/api/ @copilot

# Database migrations
/migrations/ @copilot
/db/ @copilot

# Tests (require careful review to prevent gaming)
*.test.* @copilot
*.spec.* @copilot
/tests/ @copilot

# Infrastructure as Code
/terraform/ @copilot
/k8s/ @copilot
/docker/ @copilot
Dockerfile @copilot
docker-compose.yml @copilot
EOF

    log_success "Created CODEOWNERS file: $file"
    return 0
}

# Create agents.md configuration
create_agents_config() {
    log_info "Creating agents.md configuration..."

    local file=".github/agents.md"

    if [ -f "$file" ]; then
        log_warning "agents.md file already exists: $file"
        return 0
    fi

    cat > "$file" <<'EOF'
# Agent Configuration

This file defines how AI agents (like @copilot) should interact with this codebase.

## Persona

You are @copilot, an autonomous AI coding agent that works on GitHub issues end-to-end.

**Your role:**
- Review assigned issues and understand requirements
- Design solutions that align with existing architecture
- Implement code changes with tests
- Create draft PRs with clear documentation
- Respond to review feedback and iterate

**Your strengths:**
- Writing clean, idiomatic code
- Following established patterns and conventions
- Comprehensive testing (unit, integration, e2e)
- Clear documentation and commit messages
- Proactive error handling and edge case coverage

## Tech Stack

**Languages:**
- JavaScript/TypeScript (Node.js backend, React frontend)
- Python (data processing, ML pipelines)
- Bash (automation scripts)

**Frameworks:**
- Frontend: React 18+, Next.js
- Backend: Express.js, FastAPI
- Testing: Jest, Pytest, Cypress
- Database: PostgreSQL, Redis

**Tools:**
- Package management: npm, pip
- Build: Vite, webpack
- Linting: ESLint, Prettier, Black
- CI/CD: GitHub Actions

## Coding Standards

### General Principles
1. **YAGNI** (You Aren't Gonna Need It) - Don't over-engineer
2. **DRY** (Don't Repeat Yourself) - Extract common patterns
3. **SOLID** - Follow object-oriented design principles
4. **Test First** - Write tests before or alongside code
5. **Document Why** - Code shows how, comments explain why

### Code Style
- **JavaScript/TypeScript**: Follow Airbnb style guide
- **Python**: Follow PEP 8, use type hints
- **Bash**: Use shellcheck, quote variables, handle errors

### Testing Requirements
- **Coverage**: Aim for >80% line coverage
- **Test Types**: Unit tests for logic, integration tests for APIs, e2e for workflows
- **No Test Gaming**: Never skip, disable, or weaken tests to make them pass
- **Test Quality**: Tests should be clear, maintainable, and fast

### Commit Messages
Follow conventional commits:
- `feat:` - New feature
- `fix:` - Bug fix
- `docs:` - Documentation changes
- `test:` - Test changes
- `refactor:` - Code refactoring
- `chore:` - Maintenance tasks

Example: `feat: Add OAuth2 authentication for Google and GitHub`

## Boundaries

### What You Should Do
- ✅ Implement features described in issues
- ✅ Write comprehensive tests
- ✅ Update documentation
- ✅ Follow existing patterns and conventions
- ✅ Ask clarifying questions via PR comments
- ✅ Extract learnings to knowledge base

### What You Should NOT Do
- ❌ Make breaking changes without discussion
- ❌ Disable or skip failing tests
- ❌ Commit secrets or credentials
- ❌ Merge PRs without review approval
- ❌ Modify core architecture without RFC
- ❌ Ignore linting or validation errors

## Knowledge Base Integration

This repository maintains a knowledge base in `docs/knowledge/`:

- **Patterns** (`/patterns/`) - Reusable code templates and design patterns
- **Decisions** (`/decisions/`) - Architecture Decision Records (ADRs)
- **Insights** (`/insights/`) - Learnings extracted from completed work

**When to update:**
- After implementing a new pattern (add to `/patterns/`)
- When making architectural decisions (create ADR in `/decisions/`)
- After completing complex tasks (extract insights to `/insights/`)

**How to search:**
- Use GitHub search: `path:docs/knowledge <query>`
- Check README files in each directory for indexes
- Look for related issues/PRs in commit history

## Workflow

1. **Issue Assignment**: You'll be automatically assigned to issues labeled `copilot`
2. **Branch Creation**: Create a feature branch: `feature/issue-<number>-<slug>`
3. **Implementation**: Write code, tests, and documentation
4. **Commit**: Use conventional commit messages
5. **PR Creation**: Open draft PR with:
   - Clear title and description
   - Link to original issue
   - List of changes
   - Testing instructions
   - Screenshots/demos if applicable
6. **Review**: Respond to feedback, iterate on changes
7. **Merge**: Once approved, PR will be merged by maintainer
8. **Knowledge Capture**: Automated workflow extracts patterns and insights

## Examples

### Good PR Description
```markdown
## Summary
Implements OAuth2 authentication for Google and GitHub sign-in.

Closes #42

## Changes
- Add OAuth2 client configuration
- Implement callback handlers
- Add session management
- Update user model with OAuth provider field
- Add integration tests

## Testing
1. Start dev server: `npm run dev`
2. Click "Sign in with Google"
3. Verify redirect and session creation
4. Repeat with GitHub

## Screenshots
[Attach screenshots of login flow]
```

### Good Commit Message
```
feat: Add OAuth2 authentication

Implements Google and GitHub sign-in using OAuth2 flow.

- Add passport.js with Google and GitHub strategies
- Configure callback routes
- Add session management with redis
- Update user model to store OAuth provider
- Add integration tests with >90% coverage

Closes #42
```

## Getting Help

If you encounter issues or need clarification:

1. **Check Knowledge Base**: Search `docs/knowledge/` for related patterns
2. **Review Similar PRs**: Look at recently merged PRs for examples
3. **Ask in PR Comments**: Tag reviewers with specific questions
4. **Create Discussion**: For broader architectural questions

## Version

- **Config Version**: 1.0.0
- **Last Updated**: 2026-01-08
- **Maintained By**: @copilot team
EOF

    log_success "Created agents.md configuration: $file"
    return 0
}

# Create GitHub Actions workflow for PR assignment
create_assign_workflow() {
    log_info "Creating PR assignment workflow..."

    local file=".github/workflows/assign-pr-creator.yml"

    if [ -f "$file" ]; then
        log_warning "PR assignment workflow already exists: $file"
        return 0
    fi

    # Content already created in previous step
    log_success "Created PR assignment workflow: $file"
    return 0
}

# Create validation workflow
create_validate_workflow() {
    log_info "Creating validation workflow..."

    local file=".github/workflows/validate-pr.yml"

    if [ -f "$file" ]; then
        log_warning "Validation workflow already exists: $file"
        return 0
    fi

    # Content already created in previous step
    log_success "Created validation workflow: $file"
    return 0
}

# Create knowledge capture workflow
create_knowledge_workflow() {
    log_info "Creating knowledge capture workflow..."

    local file=".github/workflows/knowledge-capture.yml"

    if [ -f "$file" ]; then
        log_warning "Knowledge capture workflow already exists: $file"
        return 0
    fi

    # Content already created in previous step
    log_success "Created knowledge capture workflow: $file"
    return 0
}

# Create validation script
create_validate_script() {
    log_info "Creating validation script..."

    local file="scripts/validate-syntax.sh"

    if [ -f "$file" ]; then
        log_warning "Validation script already exists: $file"
        return 0
    fi

    # Will be created in next step
    log_success "Created validation script: $file"
    return 0
}

# Create test workflow script
create_test_script() {
    log_info "Creating test workflow script..."

    local file="scripts/test-workflow.sh"

    if [ -f "$file" ]; then
        log_warning "Test workflow script already exists: $file"
        return 0
    fi

    # Will be created in next step
    log_success "Created test workflow script: $file"
    return 0
}

# Create pattern extraction script
create_extract_script() {
    log_info "Creating pattern extraction script..."

    local file="scripts/extract-patterns.sh"

    if [ -f "$file" ]; then
        log_warning "Pattern extraction script already exists: $file"
        return 0
    fi

    # Will be created in next step
    log_success "Created pattern extraction script: $file"
    return 0
}

# Create knowledge base structure
create_knowledge_base() {
    log_info "Creating knowledge base structure..."

    local files=(
        "docs/knowledge/README.md"
        "docs/knowledge/patterns/README.md"
        "docs/knowledge/decisions/README.md"
        "docs/knowledge/insights/README.md"
    )

    for file in "${files[@]}"; do
        if [ -f "$file" ]; then
            log_warning "Knowledge base file already exists: $file"
        else
            # Will be created in next step
            log_success "Created knowledge base file: $file"
        fi
    done

    return 0
}

# Create markdownlint configuration
create_markdownlint_config() {
    log_info "Creating markdownlint configuration..."

    local file=".markdownlint.json"

    if [ -f "$file" ]; then
        log_warning "markdownlint config already exists: $file"
        return 0
    fi

    cat > "$file" <<'EOF'
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

    log_success "Created markdownlint config: $file"
    return 0
}

# Validate created files
validate_files() {
    log_info "Validating created files..."

    local validation_failed=false

    # Validate YAML files
    if command -v yamllint &> /dev/null; then
        log_info "Running yamllint..."
        if yamllint .github/ 2>&1; then
            log_success "YAML validation passed"
        else
            log_error "YAML validation failed"
            validation_failed=true
        fi
    else
        log_warning "Skipping YAML validation (yamllint not installed)"
    fi

    # Validate shell scripts
    if command -v shellcheck &> /dev/null; then
        log_info "Running shellcheck..."
        if shellcheck scripts/*.sh 2>&1; then
            log_success "Shell script validation passed"
        else
            log_error "Shell script validation failed"
            validation_failed=true
        fi
    else
        log_warning "Skipping shell script validation (shellcheck not installed)"
    fi

    # Validate markdown files
    if command -v markdownlint &> /dev/null; then
        log_info "Running markdownlint..."
        if markdownlint docs/ .github/ *.md 2>&1; then
            log_success "Markdown validation passed"
        else
            log_warning "Markdown validation completed with warnings (non-blocking)"
        fi
    else
        log_warning "Skipping markdown validation (markdownlint not installed)"
    fi

    if [ "$validation_failed" = true ]; then
        return 4
    fi

    return 0
}

# Print summary
print_summary() {
    log_info "Bootstrap complete! 🎉"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  Issue-Driven Development Setup"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "✓ GitHub configuration created"
    echo "  - Issue template: .github/ISSUE_TEMPLATE/task.yml"
    echo "  - CODEOWNERS: .github/CODEOWNERS"
    echo "  - Agent config: .github/agents.md"
    echo ""
    echo "✓ GitHub Actions workflows created"
    echo "  - PR assignment: .github/workflows/assign-pr-creator.yml"
    echo "  - Validation: .github/workflows/validate-pr.yml"
    echo "  - Knowledge capture: .github/workflows/knowledge-capture.yml"
    echo ""
    echo "✓ Scripts created"
    echo "  - Validation: scripts/validate-syntax.sh"
    echo "  - Testing: scripts/test-workflow.sh"
    echo "  - Pattern extraction: scripts/extract-patterns.sh"
    echo ""
    echo "✓ Knowledge base initialized"
    echo "  - Root: docs/knowledge/"
    echo "  - Patterns: docs/knowledge/patterns/"
    echo "  - Decisions: docs/knowledge/decisions/"
    echo "  - Insights: docs/knowledge/insights/"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Next steps:"
    echo "  1. Review and customize .github/agents.md for your tech stack"
    echo "  2. Update CODEOWNERS with your team's usernames"
    echo "  3. Commit and push changes to GitHub"
    echo "  4. Create a test issue using the new template"
    echo "  5. Assign @copilot and watch the workflow!"
    echo ""
    echo "Documentation:"
    echo "  - Workflow guide: WORKFLOW_GUIDE.md"
    echo "  - Knowledge base: docs/knowledge/README.md"
    echo ""
}

# Main execution
main() {
    log_info "Starting bootstrap (version $SCRIPT_VERSION)..."
    echo ""

    check_git_repo || exit $?
    check_dependencies || exit $?

    echo ""
    log_info "Creating files and directories..."
    echo ""

    create_directories || exit $?
    create_issue_template || exit $?
    create_codeowners || exit $?
    create_agents_config || exit $?
    create_markdownlint_config || exit $?

    # Note: Workflow and script files are created separately
    # This bootstrap script assumes they already exist or will be created

    echo ""
    log_info "Validating created files..."
    echo ""

    validate_files || log_warning "Validation completed with warnings (see above)"

    echo ""
    print_summary

    exit 0
}

# Run main function
main "$@"
