# Code Patterns

This directory contains reusable code patterns, conventions, and best practices for this project.

## What is a Pattern?

A pattern is a proven solution to a common problem. Patterns in this directory should be:

- **Reusable:** Applicable in multiple places throughout the codebase
- **Proven:** Successfully used at least once in production
- **Clear:** Well-documented with examples and rationale
- **Contextual:** Specific to this project's needs and constraints

## When to Add a Pattern

Add a new pattern when you:

- Solve a problem in a way worth replicating
- Establish a convention the team should follow
- Discover a technique that improves code quality or maintainability
- Want to prevent others from reinventing the wheel

## Pattern Template

```markdown
# Pattern Name

## Problem

What problem does this pattern solve?

## Context

When is this pattern applicable? What are the preconditions?

## Solution

How does this pattern solve the problem?

### Code Example

[Include a clear, working code example]

### Explanation

Walk through the example, explaining key decisions.

## Alternatives Considered

What other approaches were tried? Why was this pattern chosen?

## Trade-offs

What are the pros and cons of this pattern?

## Related Patterns

Link to related patterns, decisions, or insights.

## References

Links to external resources, documentation, or inspiration.

## Metadata

- **Author:** [Name/Agent]
- **Created:** YYYY-MM-DD
- **Last Updated:** YYYY-MM-DD
- **Status:** Active | Deprecated | Superseded
```

## Naming Convention

Use descriptive, lowercase filenames with hyphens:

- `error-handling-with-results.md`
- `async-task-queue-pattern.md`
- `dependency-injection-setup.md`

## Current Patterns

### General Patterns

- *(No patterns added yet - this is a new knowledge base)*

### Domain-Specific Patterns

- *(To be added as team discovers useful patterns)*

## Pattern Categories

As the knowledge base grows, patterns may be organized into subdirectories:

```
patterns/
├── README.md (this file)
├── api/
│   └── [API-specific patterns]
├── data/
│   └── [Data handling patterns]
├── testing/
│   └── [Testing patterns]
└── infrastructure/
    └── [Infrastructure patterns]
```

## Contributing a Pattern

1. **Write the pattern** using the template above
2. **Include a working example** with real code from the project
3. **Get feedback** by opening a PR
4. **Link it** from this README
5. **Keep it updated** as the pattern evolves

## Quality Guidelines

Good patterns should:

- Explain **why**, not just **what**
- Include **concrete examples** from this codebase
- Acknowledge **trade-offs** and limitations
- Link to **related patterns** or decisions
- Be **discoverable** via GitHub search

Avoid:

- Copying generic patterns from the internet without adaptation
- Including patterns that are only used once
- Writing patterns without real-world validation
- Creating overly abstract or theoretical patterns

## Pattern Lifecycle

- **Draft:** Pattern is being developed and tested
- **Active:** Pattern is proven and recommended for use
- **Deprecated:** Pattern is no longer recommended (explain why and link to replacement)
- **Superseded:** A better pattern has replaced this one (link to new pattern)

Update the status metadata when a pattern's lifecycle changes.

## Examples from Other Projects

For inspiration, see:

- [Architecture Decision Records](https://adr.github.io/)
- [Design Patterns (Gang of Four)](https://en.wikipedia.org/wiki/Design_Patterns)
- [Cloud Design Patterns](https://learn.microsoft.com/en-us/azure/architecture/patterns/)
- [Refactoring.Guru](https://refactoring.guru/design-patterns)

Adapt these to your project's specific needs and context.

## Metadata

- **Created:** 2026-01-06
- **Last Updated:** 2026-01-06
- **Status:** Active
