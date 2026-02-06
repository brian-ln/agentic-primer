# Architecture Decision Records

This directory contains Architecture Decision Records (ADRs) documenting significant technical decisions.

## Why ADRs?

ADRs capture the "why" behind architectural choices:
- **Context** - What situation prompted the decision
- **Options** - What alternatives were considered
- **Decision** - What was chosen and why
- **Consequences** - What trade-offs were accepted

## Template

Create a new file named `NNN-decision-title.md`:

```markdown
# ADR-NNN: Title

## Status

Proposed | Accepted | Deprecated | Superseded by ADR-XXX

## Context

What situation or problem motivated this decision?
What constraints exist?

## Options Considered

### Option A: Name
- Pro: Benefit
- Con: Drawback

### Option B: Name
- Pro: Benefit
- Con: Drawback

## Decision

What did we decide and why?

## Consequences

### Positive
- What becomes easier

### Negative
- What becomes harder

### Neutral
- What changes without clear benefit or cost
```

## Index

| ADR | Title | Status | Date |
|-----|-------|--------|------|
| 001 | [Copilot Automation Bootstrap](001-copilot-bootstrap.md) | Accepted | 2026-01-08 |

## Lifecycle

1. **Proposed** - Under discussion
2. **Accepted** - Approved and in effect
3. **Deprecated** - No longer recommended
4. **Superseded** - Replaced by newer ADR
