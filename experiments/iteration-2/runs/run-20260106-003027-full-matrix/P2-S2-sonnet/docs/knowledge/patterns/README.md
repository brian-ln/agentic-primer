# Patterns

This directory contains reusable patterns and solutions to recurring problems in the codebase.

## What is a Pattern?

A pattern is a proven solution to a common problem. It describes:
- The problem context
- The solution approach
- When to use it (and when not to)
- Example implementation
- Trade-offs and considerations

## When to Document a Pattern

Add a pattern when:
- You've solved a problem that will likely recur
- You've created a code structure worth replicating
- You've established a best practice for the team
- You find yourself explaining the same solution repeatedly

## Pattern Template

Use this template when creating new pattern documentation:

```markdown
# Pattern Name

**Status**: Active | Deprecated | Superseded
**Category**: Architecture | Code | Testing | Deployment | Other
**Last Updated**: YYYY-MM-DD

## Problem

What problem does this pattern solve? Describe the context and the challenge.

Example:
- When do developers encounter this problem?
- What are the symptoms?
- Why is it important to solve?

## Solution

How does this pattern solve the problem? Provide clear explanation.

### Key Components

List the main parts of the solution:
1. Component A: What it does
2. Component B: What it does
3. Component C: What it does

### Implementation Steps

Step-by-step guide:
1. First, do this...
2. Then, do this...
3. Finally, do this...

## Example

Provide a concrete example (code, configuration, or diagram).

\`\`\`javascript
// Example code showing the pattern in action
function exampleImplementation() {
  // Clear, working code that demonstrates the pattern
}
\`\`\`

## When to Use

List scenarios where this pattern is appropriate:
- ✅ Use when: situation A
- ✅ Use when: situation B
- ❌ Don't use when: situation C
- ❌ Don't use when: situation D

## Trade-offs

Discuss pros and cons:

**Advantages**:
- Benefit 1
- Benefit 2

**Disadvantages**:
- Limitation 1
- Limitation 2

**Alternatives**:
- Alternative approach A: when to use it instead
- Alternative approach B: when to use it instead

## Related

Link to related knowledge:
- Related patterns: [Pattern Name](./other-pattern.md)
- Relevant decisions: [Decision Name](../decisions/decision.md)
- Applicable insights: [Insight Name](../insights/insight.md)

## References

- Code examples: [Link to actual implementation]
- External resources: [Documentation, articles, books]
- Related issues/PRs: #123, #456

## History

Track major changes to this pattern:
- **2026-01-08**: Initial documentation
- **2026-02-15**: Updated with new best practices
```

## Pattern Categories

Organize patterns by category:

### Architecture Patterns
- System design and component organization
- Service boundaries and communication
- Data flow and state management

### Code Patterns
- Language-specific idioms
- Design patterns (Factory, Observer, etc.)
- Common algorithms and data structures

### Testing Patterns
- Test organization and structure
- Mocking and stubbing approaches
- Integration test patterns

### Deployment Patterns
- CI/CD pipeline structures
- Environment configuration
- Release strategies

### Integration Patterns
- API client design
- External service integration
- Event handling

## Naming Convention

Name pattern files as: `{number}-{descriptive-name}.md`

Examples:
- `001-auth-pattern.md`
- `002-error-handling.md`
- `003-api-client-structure.md`

Number patterns sequentially as they're added.

## Pattern Lifecycle

Patterns can have different statuses:

- **Active**: Current and recommended
- **Deprecated**: Still in use but not recommended for new code
- **Superseded**: Replaced by a better pattern (link to replacement)
- **Experimental**: Being evaluated, not yet standard

## Quality Guidelines

Good pattern documentation:

1. **Is specific**: Concrete examples, not vague descriptions
2. **Is actionable**: Clear steps to implement
3. **Explains why**: Context and rationale, not just what
4. **Shows trade-offs**: Honest about limitations
5. **Links to real code**: Points to actual implementations
6. **Stays current**: Updated when practices change

## Examples of Good Patterns

### Example 1: Error Handling Pattern

Shows:
- Clear problem statement
- Multiple error types and how to handle each
- Code examples in context
- When to use vs when to use alternatives

### Example 2: API Client Pattern

Shows:
- Consistent structure for all API clients
- Authentication handling
- Error mapping
- Retry logic
- Example implementation

### Example 3: Test Organization Pattern

Shows:
- Directory structure
- Naming conventions
- Setup/teardown patterns
- How to organize test data

## Maintaining Patterns

### Regular Review

Quarterly review checklist:
- [ ] Is this pattern still relevant?
- [ ] Are the examples current with latest code?
- [ ] Do links still work?
- [ ] Has a better pattern emerged?
- [ ] Should this be deprecated?

### Updating Patterns

When updating a pattern:
1. Note the change in History section
2. Update "Last Updated" date
3. If significant change, bump version number
4. Notify team of important changes

### Deprecating Patterns

When a pattern is no longer recommended:
1. Change status to "Deprecated"
2. Explain why it's deprecated
3. Point to the replacement pattern
4. Don't delete (keep for historical context)

## Getting Help

- Unsure if something is a pattern? Ask: "Will this solution be used multiple times?"
- Need help writing? Use the template above and fill in what you know
- Want feedback? Create a PR and request review
- Have questions? Open an issue with the "knowledge-base" label

---

**Note**: Start simple. Even a basic pattern is better than no documentation. You can always enhance it later.
