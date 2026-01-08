# Solution Patterns

Repository of reusable solution patterns discovered during issue processing.

## What is a Pattern?

A pattern documents a proven solution approach to a common problem. Each pattern includes:

- **Problem Statement**: What problem does this solve?
- **Solution Approach**: How does the solution work?
- **Code Example**: Working reference implementation
- **Applicability**: When should you use this pattern?
- **Limitations**: When not to use it
- **Related Issues**: Links to issues using this pattern

## Naming Convention

Pattern files are named: `pattern-YYYYMMDD-HHmmss-pattern-name.md`

Example: `pattern-20260106-143022-async-error-handler.md`

## How @copilot Uses Patterns

### Before Generating Solution
1. @copilot searches patterns folder for similar problems
2. If found, uses documented pattern as starting point
3. Adapts pattern to specific issue requirements
4. Saves time by reusing proven solutions

### After Generating Solution
1. If solution uses a new pattern not in KB, documents it
2. Creates pattern file with problem, solution, and code example
3. Links pattern to the issue that discovered it
4. Future issues can reference this pattern

## Pattern Templates

### Complete Pattern Template

```markdown
# Pattern: [Pattern Name]

**Date Discovered:** YYYY-MM-DD HH:MM UTC
**Author:** @copilot
**Status:** [Active / Deprecated / Experimental]

## Overview

[1-2 sentence summary of the pattern]

## Problem Context

Describe the problem this pattern solves. Include:
- What is the problem?
- When does it occur?
- Why is it difficult without this pattern?

### Example Problem

```
// Problem example
code here
```

## Solution Approach

Explain the solution strategy:
- What is the key insight?
- How does the pattern work?
- Why does this approach work?

## Implementation

### Code Example

Complete, working code example that implements the pattern:

```javascript
// Example: Express Async Error Handler
const asyncHandler = fn => (req, res, next) => {
  Promise.resolve(fn(req, res, next)).catch(next);
};

app.get('/users', asyncHandler(async (req, res) => {
  const users = await db.getUsers();
  res.json(users);
}));
```

### Variations

If pattern has variations for different contexts:

**Variation 1: Async/Await**
```javascript
// Code here
```

**Variation 2: Promise Chains**
```javascript
// Code here
```

## Applicability Criteria

### Use This Pattern When

- [ ] You need to handle async errors in Express.js
- [ ] You're using async/await syntax
- [ ] You want centralized error handling
- [ ] You need stack traces in development

### Don't Use When

- [ ] Using callbacks instead of promises
- [ ] Already have error handling middleware
- [ ] Using a framework with built-in async error handling
- [ ] Writing synchronous-only code

### Works Best With

- Express.js 4.0+
- Node.js 12.0+ (async/await support)
- Modern JavaScript (ES2017+)
- Unit tests for error cases

## Limitations

1. **Limitation 1**: [Description]
   - Impact: [How it affects implementation]
   - Workaround: [How to handle this]

2. **Limitation 2**: [Description]
   - Impact: [How it affects implementation]
   - Workaround: [How to handle this]

## Testing

### Test Strategy

```javascript
describe('Async Error Handler', () => {
  it('should catch async errors', async () => {
    const handler = asyncHandler(async (req, res) => {
      throw new Error('Test error');
    });
    // Test passes error to next()
  });
});
```

### Coverage Goals

- 90%+ code coverage
- All error paths tested
- Both sync and async errors covered

## Performance Considerations

- **Memory**: Minimal overhead
- **CPU**: Single wrapper function
- **Scalability**: Works with high request volumes
- **Optimization**: No additional optimization needed

## Related Patterns

Links to related patterns that work well together:

- [Error Response Format Pattern](./pattern-error-response-format.md)
- [Request Logging Pattern](./pattern-request-logging.md)
- [Input Validation Pattern](./pattern-input-validation.md)

## Related Issues

Issues that used this pattern:

- #42 - Add error handling to user API
- #98 - Handle async errors in product API
- #156 - Implement error middleware

## Alternatives Considered

### Alternative 1: Express Error Handling Middleware

```javascript
app.use((err, req, res, next) => {
  // Handle error
});
```

**Why not chosen:**
- Doesn't catch async errors automatically
- Requires manual try/catch in route handlers

### Alternative 2: Promises Library (Bluebird)

```javascript
const Promise = require('bluebird');
router.get(..., Promise.promisify(handler));
```

**Why not chosen:**
- Extra dependency
- Native promises sufficient for modern Node.js

## Evolution

- **v1** (2026-01-06): Initial pattern discovered
- **v1.1** (TBD): Add TypeScript support
- **v2** (TBD): Add async middleware support

## See Also

- [Express.js Documentation](https://expressjs.com/)
- [Node.js Error Handling](https://nodejs.org/en/docs/guides/nodejs-error-handling/)
- [Async/Await Best Practices](https://javascript.info/async-await)

---

**Last Updated:** [auto-generated timestamp]
**Usage Count:** [auto-counted from related issues]
```

## Category Examples

### API Design Patterns
- REST endpoint structure
- Request/response format
- Error handling
- Pagination
- Versioning

### Error Handling Patterns
- Async error catching
- Error response format
- Error logging
- Retry logic
- Circuit breakers

### Testing Patterns
- Unit test structure
- Mock strategies
- Fixture management
- Test organization
- Coverage goals

### Data Patterns
- Database queries
- Caching strategy
- Data transformation
- Validation
- Normalization

### Performance Patterns
- Query optimization
- Caching layers
- Batch operations
- Connection pooling
- Resource limits

### Security Patterns
- Authentication flow
- Authorization checks
- Input validation
- Sanitization
- Rate limiting

## Discovery Process

When @copilot processes a new issue:

1. **Search Phase**: Look for similar patterns in KB
2. **Decision Phase**: Use existing pattern or create new?
3. **Implementation Phase**: Apply pattern to solution
4. **Documentation Phase**: If new pattern, document it

## Maintenance

### Monthly Review

- [ ] Remove obsolete patterns
- [ ] Update documentation
- [ ] Fix broken code examples
- [ ] Consolidate duplicates

### Quarterly Consolidation

- [ ] Merge similar patterns
- [ ] Update related links
- [ ] Archive old patterns
- [ ] Update statistics

### Annual Assessment

- [ ] Evaluate pattern effectiveness
- [ ] Check against new technologies
- [ ] Update best practices
- [ ] Plan next version

## Statistics

Auto-updated after each issue:

```
Total Patterns: [auto-counted]
Active Patterns: [auto-counted]
Deprecated Patterns: [auto-counted]
Most Used Pattern: [auto-tracked]
Last Pattern Added: [auto-timestamp]
```

---

**Version:** 1.0
**Last Updated:** [auto-generated]
**Maintained by:** @copilot
