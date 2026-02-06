# Copilot Instructions

This file provides behavioral guidance for @copilot when processing issues in this repository.

## Code Style

- Use modern JavaScript (ES6+) syntax
- Follow consistent naming conventions:
  - camelCase for variables and functions
  - PascalCase for classes
  - SCREAMING_SNAKE_CASE for constants
- Include JSDoc comments for public functions
- Keep functions small and focused (under 30 lines preferred)
- Use descriptive variable names over comments

## File Organization

- Place source code in `src/` directory
- Place tests alongside source files with `.test.js` suffix
- Keep related functionality in the same module
- Export functions explicitly, avoid default exports

## Testing Requirements

- Write unit tests for all new functions
- Use Jest as the testing framework
- Test both happy path and error cases
- Aim for meaningful coverage, not 100% line coverage
- Include edge cases (null, undefined, empty strings)

## Error Handling

- Validate input parameters at function boundaries
- Throw descriptive Error objects with context
- Use early returns for validation failures
- Never swallow errors silently

## Pull Request Guidelines

When creating a PR:

1. Reference the issue number in the title: "Add feature X (closes #123)"
2. Include a description of changes made
3. List acceptance criteria that were addressed
4. Note any assumptions or decisions made
5. Ensure all tests pass before marking ready

## Commit Message Format

Use conventional commit format:

```
type(scope): description

- detail 1
- detail 2
```

Types: feat, fix, docs, test, refactor, chore

## Dependencies

- Minimize external dependencies
- Prefer built-in Node.js modules when possible
- If adding a dependency, justify the need
- Use exact versions in package.json

## Security

- Never commit secrets or credentials
- Sanitize user input before processing
- Use parameterized queries for database operations
- Audit dependencies for vulnerabilities
