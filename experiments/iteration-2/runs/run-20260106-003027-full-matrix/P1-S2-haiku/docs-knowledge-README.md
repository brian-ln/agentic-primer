# Knowledge Base

Structured repository of patterns, decisions, and insights discovered during issue processing.

## Organization

### `/patterns/`

Reusable solution patterns discovered during issue resolution. Each pattern documents:

- **Problem Context**: What problem does this pattern solve?
- **Solution Approach**: How does this pattern solve it?
- **Code Example**: Reference implementation
- **Applicability Criteria**: When should this pattern be used?
- **Limitations**: When not to use this pattern
- **Related Issues**: Links to issues using this pattern

**Example:** `pattern-20260106-handle-async-errors.md`

### `/decisions/`

Architecture Decision Records (ADRs) documenting why certain design choices were made. Each decision records:

- **Decision Title**: What is being decided?
- **Context and Problem**: Why is this decision needed?
- **Solution Chosen**: What approach was selected?
- **Alternatives Considered**: What other options were evaluated?
- **Consequences**: What are the positive and negative impacts?
- **Status**: Accepted, Rejected, Superseded, etc.
- **Related Decisions**: Links to other related ADRs

**Example:** `adr-20260106-error-handling-strategy.md`

### `/insights/`

Learnings and observations from issue processing. Each insight captures:

- **Observation**: What was observed?
- **Implication**: Why does this matter?
- **Supporting Evidence**: What data or evidence supports this?
- **Lessons Learned**: What should we remember?
- **Future Applications**: How can we apply this learning?
- **Date Captured**: When was this insight recorded?

**Example:** `insight-20260106-async-patterns.md`

## File Format

### Pattern File (`.md`)

```markdown
# [Pattern Name]

**Date Discovered:** YYYY-MM-DD
**Author:** @copilot
**Related Issues:** #123, #456

## Problem Context

Describe the problem this pattern solves.

## Solution Approach

Explain the solution strategy.

## Code Example

```python
# Reference implementation
def example():
    pass
```

## Applicability Criteria

- Use this pattern when...
- Don't use when...
- Works best with...

## Limitations

- Limitation 1
- Limitation 2

## Related Patterns

- [Other Pattern Name](./pattern-xxx.md)
```

### Decision File (`.md`)

```markdown
# ADR: [Decision Title]

**Date:** YYYY-MM-DD
**Status:** Accepted / Rejected / Superseded
**Author:** @copilot

## Context

What problem or question does this decision address?

## Problem Statement

What are the constraints and requirements?

## Alternatives Considered

1. **Option A**: Description, pros, cons
2. **Option B**: Description, pros, cons
3. **Option C**: Description, pros, cons

## Decision

We chose **Option X** because...

## Consequences

### Positive
- Consequence 1
- Consequence 2

### Negative
- Trade-off 1
- Trade-off 2

## Related Decisions

- [ADR-001](./adr-20260106-xxx.md)
```

### Insight File (`.md`)

```markdown
# Insight: [Title]

**Date:** YYYY-MM-DD
**Author:** @copilot
**Confidence:** High / Medium / Low

## Observation

What did we observe?

## Evidence

- Data point 1
- Data point 2
- Metric: X vs Y

## Implication

Why does this matter?

## Lessons Learned

What should we remember?

## Application

How can we apply this learning in future work?

## Related Insights

- [Other Insight](./insight-xxx.md)
```

## Usage by @copilot

### When Processing New Issues

1. **Search KB for Similar Patterns**: Before generating a solution, check if similar problems were solved before
2. **Reuse Patterns**: If found, use the documented approach
3. **Document Approach**: If new pattern discovered, record it

### After Solution Creation

1. **Analyze What Was Built**: What was the pattern in the solution?
2. **Document Pattern**: Create pattern file if novel approach
3. **Record Decision**: Document why design choices were made
4. **Capture Insights**: Record any learnings

### When Making Architectural Decisions

1. **Check ADRs**: Review previous decisions
2. **Document Rationale**: Create new ADR for this decision
3. **Link to Context**: Reference related issues and decisions

### Continuous Learning

1. **Review KB Weekly**: Remove obsolete patterns
2. **Archive Superseded Items**: Keep history but mark as old
3. **Cross-Reference**: Link related patterns, decisions, insights
4. **Update Metrics**: Track knowledge base growth and usage

## Maintenance

### Regular Reviews

- **Monthly**: Review for obsolescence, archive old patterns
- **Quarterly**: Aggregate insights into higher-level learnings
- **Annually**: Evaluate decision consequences, update ADRs

### Archival

Superseded patterns are moved to `/archive/`:
- Keep historical record
- Mark supersession reason
- Link to replacement pattern

### Quality Standards

- **Complete**: All required sections filled
- **Clear**: Written for future readers unfamiliar with context
- **Actionable**: Can be directly applied or referenced
- **Linked**: Cross-references to related items

## Search and Discovery

### By Category

| Category | Purpose | Search By |
|----------|---------|-----------|
| Patterns | Reusable solutions | Problem type, tech stack |
| Decisions | Design rationale | Architecture area, context |
| Insights | Learnings | Topic, date, confidence |

### By Domain

- **API Design**: Patterns for endpoint structure, auth, versioning
- **Error Handling**: Patterns for error types, recovery, logging
- **Testing**: Patterns for test organization, mocking, coverage
- **Documentation**: Patterns for API docs, README, examples
- **Performance**: Patterns for optimization, caching, scaling

## Metrics

Automatically tracked and updated:

- **Total Patterns**: Number of solution patterns documented
- **Total Decisions**: Number of architectural decisions recorded
- **Total Insights**: Number of learnings captured
- **Usage Frequency**: How often patterns are referenced
- **Pattern Age**: When patterns were last updated
- **Coverage**: Percentage of issue types with documented patterns

## Example Entries

### Pattern: Express Async Error Handler

```markdown
# Pattern: Express Async Error Handler

**Date Discovered:** 2026-01-03
**Author:** @copilot
**Related Issues:** #42, #98, #156

## Problem Context

Express middleware doesn't automatically catch async errors,
causing unhandled promise rejections.

## Solution Approach

Wrap async route handlers with a try/catch wrapper function
that passes errors to next(err).

## Code Example

```javascript
const asyncHandler = fn => (req, res, next) => {
  Promise.resolve(fn(req, res, next)).catch(next);
};

app.get('/users', asyncHandler(async (req, res) => {
  const users = await db.getUsers();
  res.json(users);
}));
```

## Applicability Criteria

- Use when: Building Express.js APIs with async/await
- Don't use when: Using callbacks or promise chains exclusively
- Works best with: Modern Node.js (14+), ES2017+

## Related Patterns

- [Error Middleware](./pattern-error-middleware.md)
- [Validation Pipeline](./pattern-validation.md)
```

### Decision: Use JSON for API Responses

```markdown
# ADR: JSON-Only API Responses

**Date:** 2026-01-02
**Status:** Accepted
**Author:** @copilot

## Context

Our API needs a standard response format that works with
all client types (web, mobile, CLI).

## Problem Statement

- Support multiple client types
- Consistent error format
- Clear versioning path
- Negotiable content types

## Alternatives Considered

1. **JSON Only**: Simple, widely supported, no negotiation overhead
2. **JSON + XML**: Support both, more complex negotiation logic
3. **Content Negotiation**: Support any format, complex handling

## Decision

We chose **JSON Only** because:
- Universal client support
- Simpler implementation and testing
- Easier versioning
- Standard for modern APIs

## Consequences

### Positive
- All clients speak JSON (web, mobile, CLI)
- Simpler API contract testing
- Easier versioning and deprecation
- Standard tooling support

### Negative
- Some legacy systems may need adapters
- Cannot serve HTML directly (acceptable, separate concern)
- Lost flexibility for future content types (minimal value)
```

### Insight: Structured Logging Reduces Debug Time

```markdown
# Insight: Structured Logging Reduces Debug Time

**Date:** 2026-01-05
**Author:** @copilot
**Confidence:** High

## Observation

On issues with structured JSON logs (timestamp, request_id, service, level),
@copilot solved issues 3x faster than with unstructured logs.

## Evidence

- Issue #42: Structured logs, solved in 12 min
- Issue #98: Unstructured logs, solved in 38 min
- Issue #156: Structured logs, solved in 15 min
- Sample: 5/5 structured vs 2/5 unstructured solved on first attempt

## Implication

Structured logging is worth the upfront investment as it
dramatically improves troubleshooting velocity.

## Lessons Learned

- Include request_id in all logs
- Include timestamp and level fields
- Normalize field names across services
- Use JSON format for parsing

## Application

Recommend structured logging for all new services.
Consider migration path for existing services.

## Related Insights

- [Error Context Matters](./insight-error-context.md)
```

## Statistics

Auto-updated after each issue processed:

**Last Updated:** [auto-generated timestamp]

```
Patterns: 12
Decisions: 8
Insights: 15
Total KB Size: ~180 KB
Most Recent: [auto-filled]
Most Used Pattern: [auto-filled]
```

---

**Version:** 1.0
**Maintained by:** @copilot
**Last Reviewed:** [auto-generated]
