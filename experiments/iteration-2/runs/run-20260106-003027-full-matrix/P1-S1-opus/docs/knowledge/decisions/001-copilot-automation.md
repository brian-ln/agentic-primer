# ADR-001: Copilot Automation Bootstrap

## Status
Accepted

## Context
We need a system that enables @copilot to process issues autonomously while maintaining code quality through automated reviews and capturing knowledge for continuous improvement.

## Decision
We will implement:

1. **Issue Template** (`.github/ISSUE_TEMPLATE/copilot-task.yml`)
   - Structured YAML form for task details
   - Automatic `copilot` label assignment
   - Required fields: task, acceptance criteria

2. **GitHub Actions Workflows**
   - `copilot-issue.yml`: Triggers on issue creation with `copilot` label
   - `copilot-review.yml`: Auto-reviews PRs before human review

3. **CODEOWNERS**
   - All files require owner review
   - Ensures human oversight of @copilot work

4. **Knowledge Base** (`docs/knowledge/`)
   - Patterns, decisions, and insights structure
   - Captures learnings for future @copilot sessions

## Consequences

### Positive
- Issues are processed consistently
- Code quality maintained via automated + human review
- Knowledge accumulates over time
- Low friction for users creating tasks

### Negative
- Requires GitHub Actions minutes
- @copilot availability dependent on GitHub
- Knowledge base requires curation
