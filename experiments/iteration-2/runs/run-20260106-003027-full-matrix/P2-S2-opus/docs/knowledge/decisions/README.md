# Architecture Decision Records (ADRs)

Documentation of significant architectural and design decisions.

## What is an ADR?

An Architecture Decision Record captures:

- **Context**: The situation and constraints
- **Decision**: What was decided
- **Consequences**: Impact of the decision
- **Status**: Proposed, Accepted, Deprecated, Superseded

## ADR Template

Use this template for new decisions:

```markdown
# ADR-XXX: Title

## Status
Proposed | Accepted | Deprecated | Superseded by ADR-YYY

## Context
What is the situation? What are the constraints?

## Decision
What decision was made?

## Consequences
What are the results of this decision?

### Positive
- Benefit 1
- Benefit 2

### Negative
- Trade-off 1
- Trade-off 2

## Related
- Related ADRs
- Related issues/PRs
```

## Decisions Index

| ID | Title | Status | Date |
|----|-------|--------|------|
| ADR-001 | [Use @copilot for Issue Automation](./ADR-001-copilot-automation.md) | Accepted | 2026-01-06 |

## ADR-001: Use @copilot for Issue Automation

### Status
Accepted

### Context
The team needs to automate repetitive development tasks while maintaining code quality.

### Decision
Use GitHub @copilot with workflow automation to process issues autonomously.

### Consequences

**Positive:**
- Faster issue resolution
- Consistent code patterns
- Reduced manual work

**Negative:**
- Requires clear issue specifications
- May need human review for complex tasks
