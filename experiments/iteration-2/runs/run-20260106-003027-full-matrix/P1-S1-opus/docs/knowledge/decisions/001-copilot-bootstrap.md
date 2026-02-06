# ADR-001: Copilot Automation Bootstrap

## Status

Accepted

## Context

We need a system that enables @copilot to process GitHub issues autonomously while:
- Maintaining code quality through automated and human review
- Capturing knowledge for continuous improvement
- Providing clear guidance to both @copilot and human contributors

The prompt was: "Bootstrap @copilot issue automation with auto-review and knowledge base."

Success criterion: "System must process a test issue without errors."

## Options Considered

### Option A: Minimal Setup
- Single workflow file
- No structured templates
- No knowledge capture
- Pro: Fast to implement
- Con: No guidance for @copilot, no learning loop

### Option B: Full Automation
- Programmatic @copilot triggering
- Auto-merge on passing checks
- Automated knowledge extraction
- Pro: Zero manual steps
- Con: Not possible (no @copilot API), security concerns

### Option C: Assisted Automation (Chosen)
- Structured issue templates
- Workflows for preparation and review
- Manual @copilot assignment
- Knowledge base for manual curation
- Pro: Works with current GitHub capabilities, maintains human oversight
- Con: Requires manual steps

## Decision

We chose Option C (Assisted Automation) because:

1. **No @copilot API exists** - @copilot assignment must be manual via GitHub UI
2. **Security model** - Bot-triggered workflows require manual approval
3. **Human oversight** - CODEOWNERS ensures review before merge
4. **Knowledge capture** - Manual curation produces higher quality insights

### Implementation

1. **Issue Template** (`.github/ISSUE_TEMPLATE/copilot-task.yml`)
   - Structured fields for task, acceptance criteria, context
   - Automatic `copilot` label
   - Guidance for users

2. **Copilot Instructions** (`.github/copilot-instructions.md`)
   - Code style guidelines
   - Testing requirements
   - PR format expectations

3. **Setup Steps** (`.github/copilot-setup-steps.yml`)
   - Environment configuration
   - Dependency installation
   - Verification commands

4. **Issue Workflow** (`.github/workflows/copilot-issue.yml`)
   - Triggers on `copilot` label
   - Adds guidance comment
   - Checks for @copilot assignment

5. **Review Workflow** (`.github/workflows/copilot-review.yml`)
   - Runs linting, tests, security audit
   - Adds review summary to PR
   - Human review still required via CODEOWNERS

6. **Knowledge Base** (`docs/knowledge/`)
   - Patterns for reusable solutions
   - Decisions (ADRs) for architectural choices
   - Insights for lessons learned

## Consequences

### Positive

- Clear structure for @copilot tasks
- Automated feedback on code quality
- Human oversight maintained
- Knowledge accumulates over time
- Works with current GitHub capabilities

### Negative

- Manual @copilot assignment required
- Manual approval for Actions runs
- Knowledge base needs curation
- Placeholder values need configuration

### Neutral

- Learning curve for issue template usage
- Additional files to maintain
