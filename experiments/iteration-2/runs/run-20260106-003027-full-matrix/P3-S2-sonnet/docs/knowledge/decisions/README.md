# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant technical decisions made during development.

## Purpose

ADRs capture:
- **What** decision was made
- **Why** it was chosen over alternatives
- **Context** that influenced the decision
- **Consequences** (positive and negative)

This creates an auditable decision trail and helps future team members understand the reasoning behind the current architecture.

## ADR Format

We follow the lightweight ADR format:

```markdown
# ADR-NNN: Title

**Date:** YYYY-MM-DD
**Status:** Proposed | Accepted | Deprecated | Superseded by ADR-XXX
**Decider(s):** @copilot, @owner

## Context

What is the issue we're facing?
What factors constrain our decision?

## Decision

What did we decide to do?

## Consequences

### Positive
- Benefit 1
- Benefit 2

### Negative
- Trade-off 1
- Limitation 2

### Neutral
- Other impacts

## Alternatives Considered

### Alternative 1: Name
- Pros: ...
- Cons: ...
- Why not chosen: ...

## References

- Related ADRs
- External documentation
- Discussions
```

## Naming Convention

- Files: `ADR-001-descriptive-title.md`
- Sequential numbering
- Lowercase with hyphens
- Descriptive but concise titles

## When to Create an ADR

Create an ADR for decisions that:
- Impact system architecture
- Affect multiple components
- Have significant trade-offs
- Need to be reversible later
- Will be questioned by future developers

## Index

<!-- ADRs will be listed here chronologically -->
*No decisions recorded yet. ADRs will be created as architectural decisions are made.*

## See Also

- [Patterns](/docs/knowledge/patterns/) - Reusable solution approaches
- [Insights](/docs/knowledge/insights/) - Lessons from experience
