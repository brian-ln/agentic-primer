# Patterns Catalog

Reusable code patterns for consistent implementation across the codebase.

## Overview

Patterns are proven solutions to common problems. @copilot references these when implementing features to ensure consistency.

## Pattern Template

When adding a new pattern, use this template:

```markdown
## Pattern Name

**Problem:** What problem does this solve?

**Solution:** How does this pattern solve it?

**Example:**
\`\`\`typescript
// Code example here
\`\`\`

**When to Use:**
- Scenario 1
- Scenario 2

**When NOT to Use:**
- Anti-scenario 1
```

## Patterns Index

| Pattern | Description | Status |
|---------|-------------|--------|
| (none yet) | Add patterns as they emerge | - |

## How to Add Patterns

1. Create a new file: `docs/knowledge/patterns/<pattern-name>.md`
2. Follow the pattern template above
3. Update this index
4. Reference in relevant issues

## Example Pattern Categories

- **Error Handling** - How to handle and propagate errors
- **API Design** - REST/GraphQL endpoint conventions
- **Testing** - Test structure and mocking patterns
- **Logging** - Logging format and levels
- **Configuration** - Environment and config management
