# Patterns

This directory contains reusable design patterns and code templates that solve common problems in our codebase.

## What are Patterns?

Patterns are proven solutions to recurring problems. They help us:
- **Consistency:** Apply the same solution across the codebase
- **Quality:** Use tested, reliable approaches
- **Velocity:** Avoid reinventing the wheel
- **Communication:** Share a common vocabulary

## Pattern Structure

Each pattern document should include:

1. **Name:** Clear, descriptive name
2. **Context:** When to use this pattern
3. **Problem:** What problem does it solve?
4. **Solution:** How to implement it
5. **Example:** Working code example
6. **Consequences:** Trade-offs and considerations
7. **Related Patterns:** Links to related patterns

## Example Patterns

- **API Error Handling:** Consistent error responses across all endpoints
- **Database Migrations:** Safe schema evolution strategy
- **Configuration Management:** Environment-specific config handling
- **Logging Standards:** Structured logging with context
- **Authentication Flow:** OAuth2/JWT implementation pattern

## How to Use Patterns

### For Developers

1. **Search first:** Check if a pattern exists before solving a problem
2. **Follow the pattern:** Use the provided code templates
3. **Adapt if needed:** Patterns are guidelines, not strict rules
4. **Improve patterns:** Submit PRs if you find better approaches

### For @copilot

Reference patterns in issue descriptions:

```markdown
Implement user authentication following the pattern in:
- docs/knowledge/patterns/authentication-flow.md
```

@copilot will load the pattern and use it as a template for implementation.

## Contributing a Pattern

### When to Create a Pattern

Create a pattern when:
- You've solved the same problem 3+ times
- The solution is non-obvious or requires specific knowledge
- The approach should be consistent across the codebase
- You want to share a best practice with the team

### Pattern Template

```markdown
# Pattern Name

## Context

When do you use this pattern? What's the situation?

## Problem

What problem does this solve? What are the symptoms?

## Solution

How do you implement this pattern?

### Implementation Steps

1. Step one
2. Step two
3. Step three

### Code Example

\`\`\`javascript
// Working code example here
\`\`\`

## Consequences

### Benefits
- What are the advantages?

### Trade-offs
- What are the costs or limitations?

## Related Patterns

- Link to related patterns
- Link to relevant decisions
```

### Contribution Process

1. Create pattern file: `patterns/your-pattern-name.md`
2. Follow the template above
3. Include working code examples
4. Submit PR with clear description
5. Request review from team leads

## Pattern Categories

We organize patterns by domain:

- **API Patterns:** REST endpoints, GraphQL resolvers, API design
- **Data Patterns:** Database access, caching, data validation
- **Integration Patterns:** External services, message queues, webhooks
- **UI Patterns:** Component structure, state management, forms
- **Infrastructure Patterns:** Deployment, monitoring, error tracking

## Maintenance

- **Review quarterly:** Are patterns still relevant?
- **Update examples:** Keep code examples current with latest syntax
- **Deprecate carefully:** Mark obsolete patterns as deprecated before deleting
- **Link to code:** Reference pattern in actual implementation comments

## Resources

- [Design Patterns (Gang of Four)](https://en.wikipedia.org/wiki/Design_Patterns)
- [Enterprise Integration Patterns](https://www.enterpriseintegrationpatterns.com/)
- [Cloud Design Patterns](https://learn.microsoft.com/en-us/azure/architecture/patterns/)
