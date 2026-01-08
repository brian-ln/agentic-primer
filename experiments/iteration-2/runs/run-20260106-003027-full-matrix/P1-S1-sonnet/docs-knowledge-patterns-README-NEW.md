# Patterns

Reusable design patterns and code templates for common problems.

## What Are Patterns?

Patterns are proven solutions to recurring problems in software design and implementation. They provide:
- Reusable code templates
- Consistent approaches across the codebase
- Reduced decision-making overhead
- Knowledge transfer to new team members

## When to Add a Pattern

Add a pattern when:
- You solve a problem that will likely recur
- You create a code template worth reusing
- You establish a standard approach for the team
- You want to ensure consistency across features

## Pattern Template

Each pattern should follow this structure:

```markdown
# Pattern: [Pattern Name]

**Category:** Patterns
**Domain:** [Area of application, e.g., "API Design", "Data Access", "Error Handling"]
**Last Updated:** YYYY-MM-DD
**Status:** [Active | Deprecated | Superseded]

## Problem

[Describe the problem this pattern solves]

**Symptoms:**
- [List observable symptoms of the problem]

## Solution

[High-level description of the solution]

## Implementation

[Detailed implementation with code examples]

### Code Example

[Include working code examples in the relevant language]

## Benefits

[List advantages of using this pattern]

## Trade-offs

**Pros:**
- [Positive aspects]

**Cons:**
- [Limitations or drawbacks]

## When to Use

✅ **Use this pattern when:**
- [Conditions that make this pattern appropriate]

❌ **Consider alternatives when:**
- [Conditions that make this pattern inappropriate]

## Testing

[Example tests for this pattern]

## Related Knowledge

- **Decisions:** [Links to related ADRs]
- **Insights:** [Links to related insights]
- **Other Patterns:** [Links to related patterns]

## References

- [External documentation, articles, specifications]

## Changelog

- **YYYY-MM-DD:** [Description of changes]
```

## Current Patterns

### API Design
- [api-error-handling.md](api-error-handling.md) - Standardized error response format

### Data Access
(Coming soon)

### Authentication & Authorization
(Coming soon)

### Caching
(Coming soon)

### Testing
(Coming soon)

## How to Use Patterns

### For Developers

1. **Before starting work:**
   - Check if a pattern exists for your problem
   - Review implementation details
   - Follow the pattern's guidelines

2. **When creating an issue for @copilot:**
   - Reference relevant patterns in issue description
   - Example: "Follow patterns/api-error-handling.md for error responses"

3. **During code review:**
   - Verify pattern is applied correctly
   - Suggest pattern improvements if needed

### For @copilot

When processing issues, @copilot:
1. Scans patterns directory for relevant solutions
2. Applies pattern templates to generated code
3. Maintains consistency with established practices

### For Code Reviewers

1. **Check pattern adherence:**
   - Does code follow referenced patterns?
   - Are patterns applied correctly?
   - Should a new pattern be created?

2. **Suggest patterns:**
   - If code solves a common problem, suggest creating a pattern
   - If similar code exists elsewhere, suggest extracting a pattern

## Contributing Patterns

### Creating a New Pattern

1. **Identify the pattern:**
   - Solve a problem at least twice
   - Notice others facing the same problem
   - Have a clear, reusable solution

2. **Write the pattern:**
   - Use the template above
   - Include concrete code examples
   - Document trade-offs honestly
   - Add tests that demonstrate usage

3. **Name the file:**
   - Use kebab-case: `pattern-name.md`
   - Be specific: `api-pagination.md` not `pagination.md`
   - Include domain if helpful: `react-component-composition.md`

4. **Submit for review:**
   - Create PR with the new pattern
   - Get team feedback
   - Update based on review
   - Add to index below

### Updating Existing Patterns

1. **When to update:**
   - Better implementation discovered
   - New edge cases identified
   - Tools or libraries changed
   - Team standards evolved

2. **How to update:**
   - Preserve original intent
   - Update changelog with date and reason
   - Notify team of significant changes
   - Update related knowledge references

### Deprecating Patterns

1. **Mark as deprecated:**
   - Update status to "Deprecated"
   - Explain why it's deprecated
   - Link to replacement pattern if applicable

2. **Migration plan:**
   - Document how to migrate from old to new
   - Provide timeline for migration
   - Identify code that needs updating

## Pattern Categories

Organize patterns by domain for easy discovery:

### API Design
- Error handling
- Request validation
- Response formatting
- Pagination
- Filtering and sorting
- Versioning

### Data Access
- Database querying
- Transaction management
- Connection pooling
- ORM usage patterns

### Authentication & Authorization
- JWT token handling
- Permission checking
- Session management
- OAuth flows

### Performance
- Caching strategies
- Database optimization
- Query batching
- Lazy loading

### Testing
- Unit test structure
- Integration test patterns
- Mock data creation
- Test fixtures

### Error Handling
- Exception handling
- Logging patterns
- Error recovery
- Retry logic

### Code Organization
- Module structure
- Dependency injection
- Configuration management
- Code splitting

## Best Practices

### Writing Good Patterns

1. **Be specific:** Generic advice isn't a pattern
2. **Show code:** Examples are more valuable than prose
3. **Explain trade-offs:** No pattern is perfect for everything
4. **Keep it current:** Update as the codebase evolves
5. **Link liberally:** Connect to related knowledge

### Naming Patterns

- Use descriptive names: `api-error-handling` not `errors`
- Be consistent: All API patterns start with `api-`
- Avoid jargon: Use terms the whole team understands
- Be specific: `jwt-authentication` not `auth`

### Organizing Patterns

- Group by domain or feature area
- Use subdirectories if >10 patterns in a category
- Maintain this README as the index
- Cross-reference related patterns

## Metrics

Track pattern effectiveness:

- **Adoption rate:** % of code following patterns
- **Consistency:** % of similar code using same pattern
- **Defect rate:** Bugs per LOC in pattern-following code
- **Review time:** Time to approve pattern-following PRs

## Questions?

- How do I know if something should be a pattern vs a decision?
  - Patterns are "how to implement"
  - Decisions are "why we chose this approach"
  - Example: Decision says "use REST", Pattern shows "how to design REST endpoints"

- Can patterns change?
  - Yes! Update changelog and notify team
  - Consider versioning for major changes
  - Deprecate old patterns gracefully

- What if I disagree with a pattern?
  - Discuss with team
  - Propose alternative in PR
  - Document trade-offs of both approaches
  - Let team decide

## See Also

- [Knowledge Base README](../README.md) - Overview of entire knowledge system
- [Decisions](../decisions/README.md) - Why we made certain choices
- [Insights](../insights/README.md) - What we've learned from experience
