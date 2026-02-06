# Patterns

Reusable implementation patterns discovered through @copilot execution and team experience.

## What are Patterns?

Patterns are proven solutions to recurring problems. They capture:
- The problem being solved
- The solution approach
- When to apply (and when not to)
- Example implementation
- Trade-offs and considerations

## Pattern Index

*Patterns will be added here as they are discovered and documented.*

**Current patterns:** 0

---

## How to Document a Pattern

### Template

Create a new file: `pattern-name.md`

```markdown
# Pattern: [Pattern Name]

## Problem

What problem does this pattern solve?

## Solution

How does this pattern solve the problem?

## Example

\`\`\`javascript
// Code example showing the pattern in action
\`\`\`

## When to Use

- Scenario 1
- Scenario 2
- Scenario 3

## When NOT to Use

- Scenario A
- Scenario B

## Trade-offs

| Benefit | Cost |
|---------|------|
| Benefit 1 | Cost 1 |
| Benefit 2 | Cost 2 |

## Related

- Link to related patterns
- Link to related decisions
- Link to example implementations

## References

- External resources
- Documentation links
```

### Example Pattern Structure

```
patterns/
├── README.md                           # This file
├── error-handling-with-retry.md       # Retry logic pattern
├── api-client-with-caching.md         # API caching pattern
├── async-batch-processing.md          # Batch processing pattern
└── config-with-validation.md          # Configuration validation
```

## Pattern Categories

As patterns accumulate, organize them by category:

### Architecture Patterns
- System design patterns
- Component structure
- Data flow patterns

### Code Patterns
- Error handling
- Async operations
- Data transformation
- Validation

### Testing Patterns
- Test structure
- Mocking strategies
- Test data management

### Workflow Patterns
- CI/CD patterns
- Deployment strategies
- Rollback procedures

## Quality Checklist

Before adding a pattern, ensure it:

- ✅ Solves a real, recurring problem
- ✅ Has been used successfully at least once
- ✅ Includes working code examples
- ✅ Explains when to use and when NOT to use
- ✅ Documents trade-offs clearly
- ✅ Links to actual implementations (PRs/commits)

## Pattern Lifecycle

1. **Discovered** - Pattern identified during implementation
2. **Documented** - Pattern written up and added to knowledge base
3. **Validated** - Pattern used successfully in multiple places
4. **Refined** - Pattern updated based on experience
5. **Superseded** - Better pattern found, mark as deprecated

## Using Patterns

### For @copilot

When implementing a feature:
1. **Search patterns** for similar problems
2. **Apply pattern** if it fits the use case
3. **Adapt pattern** to specific context (don't copy blindly)
4. **Document variations** if you improve the pattern

### For Humans

When reviewing PRs:
1. **Check for pattern usage** - is @copilot using known patterns?
2. **Suggest patterns** if a better one exists
3. **Extract new patterns** from good implementations
4. **Update patterns** when you find improvements

## Anti-Patterns

Document what NOT to do by creating anti-pattern files:

**File naming:** `antipattern-name.md`

Anti-patterns explain:
- Why it's problematic
- Common mistakes
- How to fix it
- The better alternative (link to proper pattern)

## Examples of Good Patterns

### Good Pattern Documentation

```markdown
# Pattern: Error Handling with Exponential Backoff

## Problem

API calls may fail temporarily due to rate limits or network issues.
Immediate retries can worsen the problem.

## Solution

Retry failed requests with exponentially increasing delays:
1st retry: 1s, 2nd retry: 2s, 3rd retry: 4s, etc.

## Example

\`\`\`javascript
async function fetchWithRetry(url, maxRetries = 3) {
  for (let i = 0; i < maxRetries; i++) {
    try {
      return await fetch(url);
    } catch (error) {
      if (i === maxRetries - 1) throw error;
      await sleep(Math.pow(2, i) * 1000);
    }
  }
}
\`\`\`

## When to Use

- External API calls
- Database operations
- Any transient failures

## When NOT to Use

- Permanent failures (404, 401)
- User input validation errors
- Local operations

## Trade-offs

| Benefit | Cost |
|---------|------|
| Handles transient failures gracefully | Increases total request time |
| Reduces load during outages | May delay error visibility |

## Related

- Decision: 003-api-retry-strategy.md
- Implementation: PR #456
```

This pattern is:
- ✅ Specific and actionable
- ✅ Includes working code
- ✅ Explains context clearly
- ✅ Documents trade-offs
- ✅ Links to related work

### Poor Pattern Documentation

```markdown
# Pattern: Handle Errors

## Problem

Errors happen.

## Solution

Use try-catch.

\`\`\`javascript
try {
  doSomething();
} catch (e) {
  console.log(e);
}
\`\`\`
```

This pattern is:
- ❌ Too vague
- ❌ No context on when to use
- ❌ Missing trade-offs
- ❌ No links to examples

## Contributing Patterns

Patterns can come from:
1. **@copilot implementations** - Extracted during PR review
2. **Human implementations** - Documented by developers
3. **External sources** - Adapted from industry best practices
4. **Incident learnings** - Solutions to production issues

All patterns should be reviewed before merging to ensure quality.

## Pattern Metrics

Track pattern effectiveness:
- **Usage count** - How many times applied
- **Success rate** - How often it solved the problem
- **Adaptations** - How often it needed modification
- **Related issues** - Problems it helped prevent

Update patterns with metrics to guide future usage.

## Getting Started

To add your first pattern:

1. Identify a solution you've used successfully
2. Copy the template above
3. Fill in all sections completely
4. Link to the actual implementation
5. Get it reviewed
6. Add to this index

Start small - even documenting one pattern helps the team.
