# Issue-Driven Development with @copilot - Complete Solution Design

## Overview

This solution implements a full issue-driven development system where @copilot can autonomously process GitHub issues and create pull requests. The system includes GitHub Actions workflows, a knowledge base, issue templates, and validation scripts.

## Architecture

```
Repository Root
├── .github/
│   ├── workflows/
│   │   ├── issue-handler.yml        # Main issue processing workflow
│   │   └── pr-validation.yml        # PR validation and auto-assign workflow
│   ├── ISSUE_TEMPLATE/
│   │   ├── bug.md                   # Bug report template
│   │   ├── feature.md               # Feature request template
│   │   └── task.md                  # Development task template
│   └── copilot-config.json          # @copilot configuration
├── .copilot/
│   ├── knowledge-base.md            # Consolidated knowledge base
│   ├── context.json                 # Issue context and metadata
│   └── patterns/                    # Common solution patterns
│       ├── error-handling.md
│       ├── testing-pattern.md
│       └── documentation-pattern.md
├── scripts/
│   ├── validate-issue.sh            # Validate issue format and content
│   ├── process-issue.sh             # Main issue processing script
│   └── verify-pr.sh                 # Verify PR quality before merge
└── docs/
    ├── ISSUE_WORKFLOW.md            # How @copilot handles issues
    ├── PR_GUIDELINES.md             # PR standards and practices
    └── KNOWLEDGE_BASE.md            # Project knowledge consolidated
```

## Components

### 1. GitHub Actions Workflows

#### issue-handler.yml
- Triggers on issue creation and labeling
- Filters issues by labels (ai-task, enhancement, bug)
- Extracts issue metadata (title, body, labels, assignee)
- Calls process-issue.sh to handle the issue
- Records issue processing result
- Automatically assigns to issue owner or default assignee

#### pr-validation.yml
- Triggers on pull request creation
- Validates PR title format
- Checks PR has proper fixes reference
- Runs automated tests
- Auto-assigns to PR author or maintainer
- Adds appropriate labels based on changes

### 2. Issue Templates

Three template types:
- **bug.md**: For bug reports with reproduction steps
- **feature.md**: For feature requests with use cases
- **task.md**: For development tasks with acceptance criteria

### 3. Knowledge Base System

Consolidated knowledge stored in `.copilot/knowledge-base.md`:
- Project architecture overview
- Common patterns and solutions
- Technology stack documentation
- Testing and validation approaches
- Deployment and release procedures

### 4. Configuration Files

#### copilot-config.json
- Repository metadata
- Default assignees and reviewers
- Supported file types and patterns
- Testing framework configuration
- Label definitions and mapping
- Validation rules and linters

#### context.json (Generated)
- Issue-specific metadata
- Previous similar issues (for pattern matching)
- Repository state snapshot
- Available resources and constraints

### 5. Validation and Processing Scripts

#### validate-issue.sh
- Checks issue format (title, description length)
- Validates against templates
- Detects required fields
- Checks for existing similar issues

#### process-issue.sh
- Main orchestration script
- Loads issue context
- Determines issue type and priority
- Calls appropriate handler
- Creates PR with changes
- Logs results

#### verify-pr.sh
- Runs syntax validation (yamllint, shellcheck)
- Runs automated tests
- Checks PR meets quality standards
- Reports results back to GitHub

## Success Criteria Mapping

| Criterion | Implementation |
|-----------|-----------------|
| Process test issue end-to-end without errors | issue-handler.yml → process-issue.sh → PR creation |
| Pass syntax validation | verify-pr.sh runs yamllint, shellcheck, markdownlint |
| GitHub workflow triggers on issue creation | issue-handler.yml on issues.opened, labeled events |
| Auto-assign PRs to owner | pr-validation.yml auto-assign step |
| Include knowledge base | .copilot/knowledge-base.md with project docs |

## Issue Processing Flow

```
Issue Created with 'ai-task' Label
         ↓
issue-handler.yml Triggered
         ↓
Extract Issue Metadata
  - Title, Body, Labels, Author
  - Determine priority and type
         ↓
Run validate-issue.sh
  - Check format and completeness
  - Load similar issues from KB
         ↓
Run process-issue.sh
  - Create feature branch
  - Make targeted changes
  - Run tests and validation
         ↓
Create Pull Request
  - Set title: "ai: <issue title>"
  - Link to original issue
  - Add metadata comments
         ↓
Run pr-validation.yml on PR
  - Validate PR format
  - Run final tests
  - Auto-assign to maintainer
         ↓
PR Ready for Review
  - Assignee receives notification
  - Issue linked to PR
```

## Knowledge Base Structure

The knowledge base consolidates:
1. **Architecture**: System design, component interactions
2. **Technologies**: Language versions, frameworks, tools
3. **Patterns**: Common solutions for recurring problems
4. **Testing**: Test framework setup, common test patterns
5. **Deployment**: Release process, environment setup
6. **Troubleshooting**: Common issues and resolutions

## Assumptions

1. **Repository is GitHub-hosted** with Actions enabled
2. **Valid GitHub token** available in secrets (GITHUB_TOKEN)
3. **Node.js or Python** available in runner environment
4. **Basic tooling available**: git, curl, jq for JSON parsing
5. **Repository has existing tests** or test framework
6. **Maintainer available** to review and merge PRs
7. **Issue author responsible** for providing clear requirements
8. **Knowledge base maintained** as issues are resolved

## Implementation Philosophy

1. **Minimal Permissions**: Use GitHub token with minimal required scopes
2. **Idempotent Operations**: Scripts safe to run multiple times
3. **Clear Feedback**: Every step provides feedback and logging
4. **Validation First**: Validate before making changes
5. **Documentation**: Self-documenting scripts and configurations
6. **Extensibility**: Easy to add new issue types or workflows

## Files Created by @copilot

This solution consists of the following files:

1. `.github/workflows/issue-handler.yml` - Main issue processing workflow
2. `.github/workflows/pr-validation.yml` - PR validation workflow
3. `.github/ISSUE_TEMPLATE/bug.md` - Bug report template
4. `.github/ISSUE_TEMPLATE/feature.md` - Feature request template
5. `.github/ISSUE_TEMPLATE/task.md` - Task template
6. `.github/copilot-config.json` - @copilot configuration
7. `.copilot/knowledge-base.md` - Consolidated knowledge base
8. `.copilot/context.json` - Context template for issue processing
9. `.copilot/patterns/error-handling.md` - Error handling patterns
10. `.copilot/patterns/testing-pattern.md` - Testing patterns
11. `.copilot/patterns/documentation-pattern.md` - Documentation patterns
12. `scripts/validate-issue.sh` - Issue validation script
13. `scripts/process-issue.sh` - Issue processing script
14. `scripts/verify-pr.sh` - PR verification script
15. `docs/ISSUE_WORKFLOW.md` - Workflow documentation
16. `docs/PR_GUIDELINES.md` - PR standards
17. `docs/KNOWLEDGE_BASE.md` - Knowledge base reference

## Why @copilot Decided These Were Necessary

- **Workflows**: Required to automate issue detection and PR creation
- **Issue Templates**: Ensures consistent issue reporting
- **Configuration**: Central place to configure behavior and defaults
- **Knowledge Base**: Required for informed decision-making about solutions
- **Pattern Documents**: Helps @copilot make consistent choices
- **Validation Scripts**: Ensures quality before PR creation
- **Documentation**: Guides @copilot and human reviewers through process
