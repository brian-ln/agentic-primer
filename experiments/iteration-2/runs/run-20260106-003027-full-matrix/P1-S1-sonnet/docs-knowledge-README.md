# Knowledge Base

This directory contains organizational knowledge that helps GitHub Copilot and team members make better decisions.

## Purpose

The knowledge base provides context that improves code quality by:
- Documenting proven solutions (Patterns)
- Recording architectural decisions (Decisions)
- Capturing operational learnings (Insights)

## Structure

```
knowledge/
├── patterns/       # Reusable design patterns and code templates
├── decisions/      # Architecture Decision Records (ADRs)
└── insights/       # Lessons learned and best practices
```

## Categories

### Patterns

**What:** Reusable solutions to common problems.

**Examples:**
- Error handling patterns
- API design templates
- Data validation approaches
- Caching strategies

**When to add:**
- You solve a problem that will likely recur
- You create a code template worth reusing
- You establish a standard approach for the team

**See:** [patterns/README.md](patterns/README.md)

---

### Decisions

**What:** Architecture Decision Records (ADRs) documenting the "why" behind choices.

**Examples:**
- Why we chose REST over GraphQL
- Why we use PostgreSQL instead of MongoDB
- Why we structure directories this way
- Why we selected a particular library

**When to add:**
- You make a significant architectural choice
- You select between competing alternatives
- Future team members will ask "why did we do it this way?"
- The decision has long-term implications

**See:** [decisions/README.md](decisions/README.md)

---

### Insights

**What:** Empirical learnings from production, testing, and operations.

**Examples:**
- Performance optimization discoveries
- Production incident lessons
- Testing strategies that work
- Debugging techniques
- Gotchas and pitfalls

**When to add:**
- You solve a difficult production issue
- You discover non-obvious behavior
- You learn from an incident or outage
- You find a better way to do something

**See:** [insights/README.md](insights/README.md)

---

## How @copilot Uses This

When processing issues, GitHub Copilot:
1. Scans this knowledge base for relevant context
2. Applies patterns that match the task
3. Respects architectural decisions
4. Avoids pitfalls documented in insights

**Result:** More consistent, higher-quality code that aligns with team standards.

## Contributing

### Adding Knowledge

1. **Choose the right category:**
   - Reusable solution? → Patterns
   - Architectural choice? → Decisions
   - Lesson learned? → Insights

2. **Use the template:**
   - See README in each category for format
   - Follow naming conventions
   - Include examples and context

3. **Keep it actionable:**
   - Focus on "what" and "why"
   - Provide concrete examples
   - Link to related knowledge

4. **Review and iterate:**
   - Knowledge evolves with the project
   - Update entries as you learn more
   - Deprecate outdated information

### Finding Knowledge

**By browsing:**
- Check category READMEs for indexes
- Scan filenames for keywords
- Use GitHub's file finder (press `t` in repo)

**By searching:**
```bash
# Search all knowledge
grep -r "authentication" docs/knowledge/

# Search specific category
grep -r "error handling" docs/knowledge/patterns/
```

**By @copilot:**
- Copilot automatically considers relevant knowledge
- Reference specific entries in issue descriptions
- Tag issues with knowledge categories

## Maintenance

### Regular Reviews

- **Quarterly:** Review for outdated information
- **After incidents:** Capture new insights
- **During architecture reviews:** Update decisions
- **When patterns emerge:** Document for reuse

### Quality Standards

- **Accurate:** Information should be current and correct
- **Concise:** Focus on essential points
- **Searchable:** Use clear filenames and headings
- **Linked:** Cross-reference related knowledge
- **Examples:** Include code samples where helpful

## Getting Started

1. **Read existing knowledge:**
   - Browse patterns for code examples
   - Review decisions to understand "why"
   - Check insights for gotchas

2. **Reference in issues:**
   - Mention relevant knowledge when creating issues
   - Help @copilot find the right context
   - Example: "See patterns/api-error-handling.md"

3. **Contribute your learnings:**
   - When you solve a problem, document it
   - Share insights from incidents
   - Record architectural decisions

## Best Practices

### For Knowledge Authors

- **Write for future you:** Assume you'll forget the details
- **Include rationale:** Explain "why", not just "what"
- **Provide examples:** Code speaks louder than words
- **Link to sources:** Reference docs, articles, discussions
- **Date your knowledge:** Include "Last updated" for time-sensitive info

### For Knowledge Consumers

- **Search before creating:** Someone may have solved this already
- **Update as you learn:** Found a better approach? Document it
- **Share what works:** Success stories belong in insights
- **Question outdated info:** Challenge assumptions respectfully

## Examples

### Pattern Example
```markdown
# API Error Handling Pattern

## Problem
Inconsistent error responses across API endpoints.

## Solution
Use standardized error response format with status codes.

## Implementation
[Code example...]
```

### Decision Example
```markdown
# ADR 001: Use REST API Instead of GraphQL

## Context
Need to design API for mobile and web clients.

## Decision
Use REST with JSON instead of GraphQL.

## Rationale
- Team experience with REST
- Simpler for our use case
- Better tooling support

## Consequences
- More endpoints to maintain
- Less flexible for clients
```

### Insight Example
```markdown
# Copilot Best Practices

## Observation
Copilot performs better with structured issue descriptions.

## Evidence
PRs from detailed issues had 80% fewer review cycles.

## Recommendation
Always use the issue template when assigning to @copilot.
```

## Questions?

- Check category READMEs for detailed guidelines
- Search existing knowledge for examples
- Ask the team in pull request reviews
- Update this README with common questions

---

**Remember:** Knowledge is most valuable when it's shared, searchable, and actionable.
