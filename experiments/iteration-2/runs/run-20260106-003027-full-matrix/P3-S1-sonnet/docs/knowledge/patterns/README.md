# Patterns

Reusable design patterns, code templates, and proven approaches for solving common problems.

## What Goes Here?

Document patterns when you:
- Solve a problem that will recur across the codebase
- Create a reusable template or boilerplate
- Establish a coding convention or style
- Find an elegant solution worth preserving

## Pattern Template

```markdown
# Pattern Name

## Problem
What problem does this pattern solve?

## Solution
How does the pattern address the problem?

## Example
Code example demonstrating the pattern

## When to Use
- Use case 1
- Use case 2

## When NOT to Use
- Anti-pattern 1
- Anti-pattern 2

## Trade-offs
Benefits vs costs of using this pattern

## Related Patterns
Links to related patterns in this knowledge base
```

## Examples of Good Patterns

- **Error Handling Pattern**: How to handle and propagate errors consistently
- **API Response Format**: Standard structure for API responses
- **Test Setup Pattern**: How to set up and tear down test fixtures
- **Configuration Loading**: How to load and validate configuration
- **Database Migration Pattern**: How to write reversible migrations
- **Logging Pattern**: What and how to log for debugging

## Organization

Each pattern is a separate markdown file:
- Use kebab-case filenames: `error-handling-pattern.md`
- Start with a descriptive title
- Include code examples
- Update INDEX.md when adding new patterns

## Cross-References

Link patterns to:
- Related patterns in this directory
- Decisions that led to the pattern (in `/decisions/`)
- Insights learned from using the pattern (in `/insights/`)

## Evolution

Patterns should evolve as the codebase grows:
- Update patterns when you find better approaches
- Keep old versions in git history
- Document why the pattern changed
- Consider creating a new decision record for significant changes

## Getting Started

1. Read existing patterns to understand the style
2. When solving a problem, check if a pattern exists
3. If no pattern exists, consider creating one
4. Keep patterns focused on a single concept
5. Make patterns actionable with clear examples
