# @copilot Instructions

## Overview

This repository uses @copilot for issue-driven development. When assigned an issue with the `copilot-ready` label, follow these instructions.

## Workflow

### 1. Issue Analysis

1. Read the issue title and description carefully
2. Review all acceptance criteria
3. Check for constraints and context
4. Identify files that need modification

### 2. Implementation

1. Create a new branch: `copilot/<issue-number>-<short-description>`
2. Make minimal, focused changes
3. Follow existing code patterns
4. Add or update tests as needed
5. Update documentation if required

### 3. Pull Request

1. Create PR with clear title referencing issue
2. Use "Closes #<issue-number>" in PR body
3. Fill out PR template completely
4. Ensure all checks pass

## Code Standards

### General
- Follow existing code style
- Use meaningful variable names
- Add comments for complex logic
- Keep functions small and focused

### Commits
- Use conventional commit format: `type(scope): description`
- Types: feat, fix, docs, style, refactor, test, chore
- Keep commits atomic and focused

### Testing
- Add tests for new functionality
- Ensure existing tests pass
- Aim for meaningful coverage

## File Locations

| Type | Location |
|------|----------|
| Source code | `src/` |
| Tests | `tests/` or `__tests__/` |
| Documentation | `docs/` |
| Scripts | `scripts/` |
| Config | Root directory |

## Knowledge Base

When completing tasks, update the knowledge base:

- **Patterns**: `docs/knowledge/patterns/`
- **Decisions**: `docs/knowledge/decisions/`
- **Insights**: `docs/knowledge/insights/`

## Agent Compatibility

This system works with multiple AI agents:

| Agent | Notes |
|-------|-------|
| GitHub Copilot | Native integration |
| Claude Opus | Full capability |
| Claude Sonnet | Balanced performance |
| Claude Haiku | Fast, lightweight tasks |

## Error Handling

If you encounter issues:

1. Document the error in the PR description
2. Add `needs-human-review` label
3. Create comment explaining the blocker
4. Assign to human reviewer

## Success Metrics

Target performance:
- Issue to PR: < 1 hour
- All checks passing: Required
- Human review time: < 30 minutes
- Successful merge rate: > 90%
