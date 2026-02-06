# Patterns

Reusable code patterns and solutions discovered during development.

## What Belongs Here

Patterns should be:
- **Reusable** - Applicable to multiple situations
- **Proven** - Tested in at least one production context
- **Complete** - Include working code examples
- **Documented** - Explain when and why to use

## Template

When adding a new pattern, create a file named `NNN-pattern-name.md`:

```markdown
# Pattern Name

## Problem

What problem does this pattern solve? Be specific.

## Solution

How does this pattern solve it? Explain the approach.

## Example

\`\`\`javascript
// Working code example
function example() {
  // Implementation
}
\`\`\`

## When to Use

- Scenario 1: Description
- Scenario 2: Description

## When NOT to Use

- Anti-scenario: Why this pattern doesn't fit

## Trade-offs

| Pro | Con |
|-----|-----|
| Benefit 1 | Drawback 1 |

## Related

- Link to related patterns
- Link to relevant ADRs
```

## Index

| ID | Pattern | Problem Solved |
|----|---------|----------------|
| - | - | No patterns documented yet |

## Adding Patterns

1. Identify a reusable solution during development
2. Create a new file following the template
3. Update this index
4. Link to related patterns and decisions
