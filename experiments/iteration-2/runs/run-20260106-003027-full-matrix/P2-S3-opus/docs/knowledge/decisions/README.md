# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records for the @copilot automation system.

## Index

| ADR | Title | Status | Date |
|-----|-------|--------|------|
| [001](001-copilot-automation.md) | Copilot Automation Architecture | Accepted | 2026-01-06 |

## What is an ADR?

An Architecture Decision Record captures an important architectural decision along with its context and consequences.

## ADR Template

```markdown
# ADR-NNN: Title

## Status
[Proposed | Accepted | Deprecated | Superseded by ADR-XXX]

## Date
YYYY-MM-DD

## Context
What is the issue that we're seeing that is motivating this decision?

## Decision
What is the change that we're proposing and/or doing?

## Consequences
What becomes easier or more difficult to do because of this change?

## Alternatives Considered
What other options were evaluated?

## Related
Links to related ADRs, issues, or documentation.
```

## Lifecycle

1. **Proposed** - ADR is drafted and under discussion
2. **Accepted** - Decision is approved and implemented
3. **Deprecated** - Decision is no longer relevant
4. **Superseded** - Replaced by a newer ADR

## When to Write an ADR

Write an ADR when:

- Making a significant architectural choice
- Choosing between multiple viable options
- The decision will be hard to reverse
- Future maintainers need to understand why

## Naming Convention

```
NNN-short-description.md
```

Where NNN is a zero-padded sequential number.

## Contributing

1. Create a new ADR using the template
2. Number it sequentially
3. Set status to "Proposed"
4. Discuss in a PR or issue
5. Update status to "Accepted" when approved
