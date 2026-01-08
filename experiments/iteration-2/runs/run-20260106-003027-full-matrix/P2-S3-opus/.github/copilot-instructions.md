# Copilot Instructions

## Overview

This repository uses issue-driven development with @copilot. When assigned an issue, follow these instructions to ensure consistent, high-quality implementations.

## Workflow

### When Assigned an Issue

1. **Read the issue carefully** - Parse the task description and acceptance criteria
2. **Check the knowledge base** - Look in `docs/knowledge/` for relevant patterns and decisions
3. **Create a feature branch** - Use format `copilot/issue-{number}-{slug}`
4. **Implement the solution** - Follow project conventions
5. **Add tests** - Every implementation should include appropriate tests
6. **Create a PR** - Link to the issue with "Fixes #N"

### Code Standards

- Follow existing code style in the repository
- Use TypeScript types when available
- Include JSDoc comments for public APIs
- Run linting before committing
- Ensure all tests pass

### Commit Messages

Use conventional commits format:
- `feat:` for new features
- `fix:` for bug fixes
- `docs:` for documentation
- `test:` for test additions/changes
- `refactor:` for code refactoring

Example: `feat: implement greeting function (#42)`

### Pull Request Template

PRs should include:
1. Summary of changes
2. Link to issue (`Fixes #N`)
3. List of files changed
4. Test coverage information

## Knowledge Base Usage

### Before Starting Work

Check these locations for relevant context:
- `docs/knowledge/patterns/` - Reusable implementation patterns
- `docs/knowledge/decisions/` - Architecture decisions that may affect your work
- `docs/knowledge/insights/` - Lessons learned from previous implementations

### After Completing Work

Consider adding to the knowledge base:
- New patterns discovered during implementation
- Decisions made and their rationale
- Insights that could help future implementations

## Multi-Agent Compatibility

This repository supports multiple AI agents:
- **Opus** - Best for complex, multi-step tasks
- **Sonnet** - Balanced for most tasks
- **Haiku** - Quick for simple tasks

When working on this repository:
- Use explicit, structured formats (YAML, JSON) over prose
- Include clear acceptance criteria checkboxes
- Document assumptions in PR descriptions

## Error Handling

If you encounter errors:
1. Log the error details in the execution log
2. Comment on the issue explaining the blocker
3. Suggest alternative approaches if possible
4. Mark the execution as failed with reason

## Testing Requirements

All implementations must include:
- Unit tests for new functions
- Integration tests for API changes
- Edge case coverage
- Minimum 80% code coverage for new code

## Documentation

Update documentation when:
- Adding new public APIs
- Changing existing behavior
- Adding new configuration options
- Introducing new patterns

## Execution Logging

Log all executions to `logs/executions/`:
```json
{
  "execution_id": "exec-YYYYMMDD-HHMMSS-{issue_number}",
  "issue_number": 42,
  "issue_title": "Task title",
  "start_time": "ISO8601",
  "end_time": "ISO8601",
  "status": "success|failure",
  "agent": "opus|sonnet|haiku",
  "branch": "copilot/issue-42-slug",
  "pr_number": 43,
  "files_created": ["path/to/file.ts"],
  "tests_passed": true,
  "errors": []
}
```

## Security

- Never commit secrets or credentials
- Use environment variables for sensitive data
- Review for security vulnerabilities before PR
- Flag any security concerns in PR description

## Questions?

If unclear about requirements:
1. Comment on the issue asking for clarification
2. Document assumptions you're making
3. Proceed with best judgment and note in PR
