# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant design choices.

## Purpose

ADRs capture the context, options considered, and rationale behind architectural decisions. They provide a historical record for understanding why the system is designed as it is.

## ADR Template

Use this structure for new ADRs:

```markdown
# ADR-NNN: Title

## Status
Proposed | Accepted | Deprecated | Superseded by ADR-XXX

## Context
What is the issue we're addressing?

## Decision
What is the change we're making?

## Consequences

### Positive
- Benefit 1
- Benefit 2

### Negative
- Tradeoff 1
- Tradeoff 2

## Alternatives Considered
- Option A: Description, why rejected
- Option B: Description, why rejected
```

## Decisions Index

| ADR | Title | Status | Date |
|-----|-------|--------|------|
| [001](001-issue-driven-workflow.md) | Issue-Driven Workflow with @copilot | Accepted | 2026-01-06 |

## Guidelines

1. **One decision per ADR** - Keep records focused
2. **Include alternatives** - Show what was considered
3. **Document rationale** - Explain why, not just what
4. **Link related ADRs** - Build a connected history
5. **Date your decisions** - Context changes over time
