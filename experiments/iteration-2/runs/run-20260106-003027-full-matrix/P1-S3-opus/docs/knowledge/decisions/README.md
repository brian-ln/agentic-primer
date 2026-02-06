# Architecture Decision Records

This directory contains Architecture Decision Records (ADRs) documenting significant technical decisions.

## Contents

| ADR | Title | Status |
|-----|-------|--------|
| [001](001-copilot-automation.md) | Copilot Automation System | Accepted |

## ADR Template

When adding new ADRs:

1. Create file `NNN-title-slug.md`
2. Use the template below
3. Update this README

### Template

```markdown
# ADR-NNN: Title

## Status

[Proposed | Accepted | Deprecated | Superseded]

## Context

What is the issue that we're seeing that is motivating this decision or change?

## Decision

What is the change that we're proposing and/or doing?

## Consequences

What becomes easier or more difficult to do because of this change?

### Positive
- List positive outcomes

### Negative
- List negative outcomes

### Neutral
- List neutral outcomes

## Alternatives Considered

What other options were evaluated?

## Related

Links to related ADRs, issues, or documents.
```

## Guidelines

1. **One decision per ADR** - Keep focused
2. **Immutable once accepted** - Create new ADR to change
3. **Document context** - Future readers need background
4. **List alternatives** - Show what was considered
