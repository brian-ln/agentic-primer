# Patterns

This directory contains reusable code patterns and solutions that have proven effective across the codebase.

## What is a Pattern?

A pattern is a **reusable solution to a commonly occurring problem**. Patterns in this directory should be:

- **Proven**: Successfully used in production code
- **Reusable**: Applicable to multiple situations
- **Well-documented**: Clear examples and usage guidelines
- **Tested**: Validated through real implementations

## Categories

Patterns are organized by domain:

### API Design
- Request/response formats
- Authentication and authorization
- Error handling
- Versioning strategies
- Rate limiting

### Testing
- Unit test structures
- Integration test patterns
- Mocking strategies
- Test data management
- Coverage approaches

### Performance
- Caching strategies
- Query optimization
- Lazy loading
- Pagination
- Resource management

### Error Handling
- Error response formats
- Logging patterns
- Retry strategies
- Circuit breakers
- Graceful degradation

### Data Processing
- Stream processing
- Batch operations
- Data transformation
- Validation patterns
- Serialization

### Security
- Input validation
- Output encoding
- Secrets management
- CSRF protection
- Rate limiting

## How to Use Patterns

### 1. Search for Relevant Patterns

```bash
# Search all patterns
grep -r "authentication" docs/knowledge/patterns/

# List all patterns
ls docs/knowledge/patterns/

# Find patterns by category
grep "Category:" docs/knowledge/patterns/*.md
```

### 2. Reference in Code

```javascript
// Good: Reference the pattern in comments
/**
 * Authentication middleware following the pattern documented in:
 * docs/knowledge/patterns/api-authentication.md
 */
function authenticate(req, res, next) {
    // Implementation following pattern...
}
```

### 3. Link in PRs

When implementing a pattern, reference it in your PR description:

```markdown
## Implementation Notes

This PR follows established patterns:
- [API Error Handling](docs/knowledge/patterns/api-error-handling.md)
- [Request Validation](docs/knowledge/patterns/request-validation.md)
```

## Pattern Template

Use this template when creating new patterns:

```markdown
# Pattern: [Name]

**Category**: [API Design / Testing / Performance / Security / etc.]
**Confidence**: [High / Medium / Low]
**First Used**: [PR #123 or Date]
**Last Updated**: [Date]

## Problem

[What problem does this pattern solve?]
[What symptoms indicate you need this pattern?]

## Solution

[The pattern itself - high-level description]

### Code Example

```language
// Complete, working example
```

### Key Components

- **Component 1**: [Description]
- **Component 2**: [Description]

## When to Use

- ✅ [Situation 1 where pattern applies]
- ✅ [Situation 2 where pattern applies]

## When NOT to Use

- ❌ [Situation where pattern is inappropriate]
- ❌ [Anti-patterns or common misuses]

## Trade-offs

**Pros:**
- [Benefit 1]
- [Benefit 2]

**Cons:**
- [Limitation 1]
- [Limitation 2]

## Examples in Codebase

- [File path 1] - [Brief description]
- [File path 2] - [Brief description]

## Variations

### Variation 1: [Name]
[When and how to use this variation]

### Variation 2: [Name]
[When and how to use this variation]

## Common Mistakes

1. **Mistake**: [Description]
   **Fix**: [How to avoid or fix it]

2. **Mistake**: [Description]
   **Fix**: [How to avoid or fix it]

## Testing

[How to test code using this pattern]
[Example test cases]

## Performance Considerations

[Performance characteristics]
[Optimization tips]

## Related Patterns

- [Related Pattern 1] - [Relationship]
- [Related Pattern 2] - [Relationship]

## References

- [External documentation]
- [Related PRs: #123, #456]
- [Related decisions: docs/knowledge/decisions/001-xyz.md]

---

**Maintained by**: [Team or individual]
**Confidence**: [High/Medium/Low - based on production usage]
**Last Review**: [Date]
```

## Adding New Patterns

### Automatic Extraction

Patterns are automatically identified from merged PRs when:
- Same solution appears in 3+ places
- Code review suggests "this should be a pattern"
- PR contains label `extract-pattern`

### Manual Addition

1. **Identify the pattern** in production code
2. **Create markdown file** following template above
3. **Add examples** from actual codebase
4. **Update this index** (below)
5. **Submit PR** with label `knowledge-base`

## Pattern Index

<!-- Auto-generated index - do not edit manually -->
<!-- Patterns will be listed here as they are added -->

### API Design
- (No patterns yet - will be auto-populated)

### Testing
- (No patterns yet - will be auto-populated)

### Performance
- (No patterns yet - will be auto-populated)

### Error Handling
- (No patterns yet - will be auto-populated)

### Security
- (No patterns yet - will be auto-populated)

### Data Processing
- (No patterns yet - will be auto-populated)

## Pattern Lifecycle

```
Identified → Documented → Reviewed → Published → Evolved → Deprecated
```

### States

- **Draft**: Pattern identified but not fully documented
- **Active**: Recommended for use
- **Evolved**: Superseded by better pattern (keep for reference)
- **Deprecated**: No longer recommended (archived)

## Quality Guidelines

Good patterns should:
- ✅ Solve a real problem from production code
- ✅ Include working code examples
- ✅ Document when NOT to use the pattern
- ✅ Link to actual usages in codebase
- ✅ Include test examples
- ✅ Note performance implications

Bad patterns:
- ❌ Theoretical solutions never used in production
- ❌ Copy-pasted from external sources without adaptation
- ❌ Missing examples or usage guidelines
- ❌ Overly complex for the problem they solve

## Metrics

**Pattern Reuse Rate**: [Auto-calculated - how often patterns are referenced in PRs]
**Pattern Age**: [Auto-calculated - average time since creation]
**Most Referenced**: [Auto-calculated - top 5 most linked patterns]

Last updated: [Auto-populated]

## Contributing

Found a pattern worth documenting? Open an issue with:
- Label: `pattern-proposal`
- Title: "Pattern: [Name]"
- Body: Link to examples in codebase + brief description

The community will review and help refine the pattern before adding to knowledge base.

---

**Questions?** Open an issue with label `knowledge-base`
