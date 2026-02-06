# Code Patterns Library

This directory contains reusable code patterns, templates, and design solutions extracted from the codebase.

## Purpose

Patterns serve as:

- **Templates** - Starting points for common tasks
- **Best Practices** - Proven solutions to recurring problems
- **Reference** - Examples of idiomatic code
- **Teaching Material** - How to implement features correctly

## Pattern Categories

### Frontend Patterns

React components, hooks, state management, and UI patterns.

**Examples:**
- Authentication flows
- Form validation
- Data fetching
- Error boundaries
- Component composition

### Backend Patterns

API design, data access, middleware, and service architecture.

**Examples:**
- RESTful endpoints
- Database queries
- Authentication middleware
- Error handling
- Background jobs

### Testing Patterns

Test strategies, fixtures, mocks, and assertions.

**Examples:**
- Unit test structure
- Integration test setup
- E2E test scenarios
- Mock data factories
- Test utilities

### DevOps Patterns

CI/CD, deployment, monitoring, and infrastructure.

**Examples:**
- GitHub Actions workflows
- Docker configurations
- Deployment scripts
- Health checks
- Monitoring setup

## Pattern Template

When creating a new pattern, use this template:

```markdown
# Pattern: [Descriptive Name]

**Category**: Frontend | Backend | Testing | DevOps | Other
**Difficulty**: Beginner | Intermediate | Advanced
**Last Updated**: YYYY-MM-DD

## Context

When should you use this pattern? What problem does it solve?

## Problem

Describe the specific challenge this pattern addresses.

## Solution

### Overview

High-level explanation of the approach.

### Implementation

\`\`\`language
// Code example showing the pattern
function exampleImplementation() {
  // Clear, commented code
}
\`\`\`

### Key Points

- Important consideration 1
- Important consideration 2
- Important consideration 3

## Examples

### Example 1: [Scenario Name]

\`\`\`language
// Real-world usage from the codebase
\`\`\`

**Source**: Link to PR or file

### Example 2: [Another Scenario]

\`\`\`language
// Another real-world example
\`\`\`

**Source**: Link to PR or file

## Variations

Alternative implementations or adaptations:

- **Variation 1**: When to use and how it differs
- **Variation 2**: Trade-offs and considerations

## Pitfalls

Common mistakes to avoid:

- ❌ **Anti-pattern 1**: Why this doesn't work
- ❌ **Anti-pattern 2**: What to do instead

## Testing

How to test code using this pattern:

\`\`\`language
// Test example
test('pattern behavior', () => {
  // Assertions
});
\`\`\`

## Related Patterns

- [Pattern Name](./pattern-file.md) - How they relate
- [Another Pattern](./another-pattern.md) - When to choose each

## References

- [Documentation](https://example.com) - Official docs
- [Article](https://example.com) - Explanatory article
- [ADR-123](../decisions/adr-123.md) - Related decision

## Tags

- category:frontend
- language:javascript
- difficulty:intermediate
- status:active

## Changelog

- **2026-01-08**: Initial pattern documented
- **YYYY-MM-DD**: Updated with new examples
```

## How to Use Patterns

### Finding Patterns

1. **Browse by category**: Look in relevant section above
2. **Search by keyword**: `grep -r "authentication" .`
3. **Filter by tag**: `grep -r "category:frontend" .`

### Applying Patterns

1. Read the full pattern document
2. Understand the context and problem
3. Adapt the solution to your needs
4. Follow implementation guidelines
5. Include tests as shown
6. Reference the pattern in your PR

### Creating New Patterns

1. Identify a reusable solution in your work
2. Document using the template above
3. Include real examples from the codebase
4. Add appropriate tags
5. Link to source PR
6. Submit for review

## Pattern Index

Total Patterns: 0

### By Category

- **Frontend**: 0 patterns
- **Backend**: 0 patterns
- **Testing**: 0 patterns
- **DevOps**: 0 patterns
- **Other**: 0 patterns

### Recently Added

(No patterns yet)

### Most Referenced

(Track which patterns are most often linked in PRs)

## Pattern Lifecycle

### Active Patterns

Currently recommended and maintained.

**Status**: `status:active`

### Deprecated Patterns

Still documented but no longer recommended.

**Status**: `status:deprecated`

### Experimental Patterns

Under evaluation, may change.

**Status**: `status:experimental`

## Quality Checklist

Before submitting a pattern, ensure:

- [ ] Clear problem statement
- [ ] Working code examples
- [ ] Real-world usage from codebase
- [ ] Test examples included
- [ ] Pitfalls documented
- [ ] Related patterns linked
- [ ] Appropriate tags added
- [ ] Proper markdown formatting

## Anti-Patterns

Document what NOT to do:

### Anti-Pattern: Skipping Tests

**Problem**: Code without tests breaks easily.

**Why It's Wrong**: No safety net for refactoring.

**Better Approach**: Write tests first (TDD).

### Anti-Pattern: Global State

**Problem**: Shared mutable state causes bugs.

**Why It's Wrong**: Hard to reason about, test, and debug.

**Better Approach**: Use local state or context.

## Pattern Evolution

Patterns evolve over time:

1. **Initial Discovery** - Pattern identified in code
2. **Documentation** - Pattern written up
3. **Validation** - Pattern used successfully 3+ times
4. **Refinement** - Pattern improved based on feedback
5. **Standardization** - Pattern becomes team standard
6. **Deprecation** - Better pattern supersedes it

## Contributing

### Extraction from PRs

Patterns are automatically extracted from merged PRs via GitHub Actions.

Review auto-generated patterns and:
- Add missing context
- Improve code examples
- Link to related patterns
- Add test examples

### Manual Creation

For patterns not auto-extracted:

1. Create new file: `pattern-{name}.md`
2. Use template above
3. Submit PR with label `documentation`
4. Request review from team

### Improving Existing Patterns

Found a better way?

1. Update the pattern document
2. Add to changelog
3. Link to PR that prompted improvement
4. Update related patterns

## Tools

- **`scripts/extract-patterns.sh`** - Auto-extract from PRs/commits
- **`scripts/search-patterns.sh`** - Advanced pattern search
- **GitHub Actions** - Automatic extraction on merge

## Examples of Good Patterns

(Once patterns are created, link to best examples here)

## Questions?

- Ask in PR comments
- Create issue with label `patterns`
- Discuss in team chat

---

**Maintainer**: Development Team
**Last Updated**: 2026-01-08
**Version**: 1.0.0
