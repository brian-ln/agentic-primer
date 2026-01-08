# Architecture Decisions

Record of significant architecture and design decisions.

## What is an ADR?

An Architecture Decision Record (ADR) captures:
- The context and problem
- The decision made
- The consequences and trade-offs

## ADR Template

```markdown
# ADR-NNN: Title

## Status
[Proposed | Accepted | Deprecated | Superseded]

## Context
What is the issue that we're seeing that is motivating this decision?

## Decision
What is the change that we're proposing and/or doing?

## Consequences
What becomes easier or more difficult to do because of this change?

## References
Links to related discussions, issues, or documentation.
```

## Decisions Index

### ADR-001: Use GitHub Actions for @copilot Automation

**Status:** Accepted

**Context:** Need to trigger @copilot processing when issues are created.

**Decision:** Use GitHub Actions workflow triggered by issue events.

**Consequences:**
- Native GitHub integration
- No external services required
- Limited to GitHub-hosted runners

---

No additional decisions recorded yet.
