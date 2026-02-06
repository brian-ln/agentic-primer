# Architecture Decision Records (ADRs)

Documents key architectural decisions and their rationale.

## Purpose

ADRs provide context for future maintainers and AI agents, answering:
- **Why** was this decision made?
- **What** alternatives were considered?
- **What** are the consequences?

## ADR Index

| # | Title | Status | Date |
|---|-------|--------|------|
| 001 | [Core Architecture](001-architecture-v2.md) | Accepted | 2026-01-08 |
| 002 | Use GitHub Actions for Automation | Accepted | 2026-01-08 |
| 003 | Structured Knowledge Base | Accepted | 2026-01-08 |

## ADR Format

Each ADR follows this structure:

```markdown
# ADR-NNN: Title

**Status**: Proposed | Accepted | Deprecated | Superseded
**Date**: YYYY-MM-DD
**Deciders**: @username, @copilot

## Context

What is the issue we're seeing that is motivating this decision?
What forces are at play (technical, political, social, project)?

## Decision

What is the change we're proposing and/or have agreed to implement?

## Alternatives Considered

1. **Alternative A**
   - Pros: ...
   - Cons: ...
   - Reason rejected: ...

2. **Alternative B**
   - Pros: ...
   - Cons: ...
   - Reason rejected: ...

## Consequences

What becomes easier or more difficult to do because of this change?

### Positive

- Benefit 1
- Benefit 2

### Negative

- Cost 1
- Cost 2

### Neutral

- Change 1
- Change 2

## Implementation Notes

(Optional) How this decision was implemented.

## Related Decisions

- ADR-XXX (supersedes/superseded by/related to)

## References

- Links to relevant documentation
- Research papers or blog posts
- Discussion threads
```

## When to Write an ADR

Create an ADR when:

1. **Significant Architectural Choice**: Impact lasts > 6 months
2. **Multiple Valid Options**: Non-obvious trade-offs
3. **Team Disagreement**: Document consensus reached
4. **External Constraints**: Vendor lock-in, compliance, budget
5. **Technology Adoption**: New framework, language, or tool

## ADR Lifecycle

```
Draft → Proposed → Accepted → Implemented
                      ↓
                 Deprecated (if superseded)
```

- **Draft**: Initial proposal, open for feedback
- **Proposed**: Team has reviewed, awaiting decision
- **Accepted**: Decision finalized, ready to implement
- **Implemented**: Decision is in production
- **Deprecated**: Decision no longer valid, see superseding ADR

## ADR Status Definitions

| Status | Meaning | Next Step |
|--------|---------|-----------|
| **Proposed** | Under consideration | Review and decide |
| **Accepted** | Decision approved | Implement |
| **Implemented** | In production | Monitor consequences |
| **Deprecated** | No longer valid | Migrate to new approach |
| **Superseded** | Replaced by newer ADR | Reference new ADR |

## Creating a New ADR

### Step 1: Choose a Number

Find the next available ADR number:

```bash
ls docs/knowledge/decisions/ | grep -E '^[0-9]{3}' | tail -1
```

### Step 2: Use the Template

Copy the format above and fill in all sections.

### Step 3: Gather Input

- Share with team for feedback
- Document all alternatives considered
- Capture trade-offs honestly

### Step 4: Update Index

Add your ADR to this README's table.

### Step 5: Reference

Link to your ADR from relevant patterns and code comments.

## Reviewing Existing ADRs

### Periodic Review

- **Quarterly**: Review recent ADRs for accuracy
- **Annually**: Review all ADRs, deprecate outdated ones

### Review Checklist

- [ ] Decision still valid?
- [ ] Consequences materialized as expected?
- [ ] Alternatives still less favorable?
- [ ] Implementation notes accurate?
- [ ] Related decisions linked?

## Common Decision Categories

### Technology Choices
- Programming languages
- Frameworks and libraries
- Databases
- Cloud providers

### Architecture Patterns
- Microservices vs monolith
- Event-driven vs request-response
- Synchronous vs asynchronous

### Process Decisions
- Git workflow (trunk-based, git-flow)
- Review process
- Deployment strategy

### Tool Selections
- CI/CD platform
- Monitoring and alerting
- Issue tracking

## Anti-Patterns to Avoid

1. **Decision Without Context**: Must explain "why"
2. **Missing Alternatives**: Always document what you didn't choose
3. **Retroactive ADRs**: Write ADR before implementing
4. **Stale ADRs**: Deprecate outdated decisions
5. **ADR Overload**: Not every decision needs an ADR

## Search and Navigation

**By status**:
```bash
rg "Status: Accepted" docs/knowledge/decisions/
```

**By date**:
```bash
ls -lt docs/knowledge/decisions/*.md
```

**By keyword**:
```bash
rg "GitHub Actions" docs/knowledge/decisions/
```

## Benefits of ADRs

1. **Historical Context**: Understand past decisions
2. **Onboarding**: New team members learn rationale
3. **Consistency**: Avoid revisiting settled questions
4. **Learning**: See what worked and what didn't
5. **AI Context**: @copilot can reference decisions

## Integration with Other Knowledge

- **Patterns**: ADRs justify why certain patterns are preferred
- **Insights**: Insights may trigger new ADRs
- **Code**: Link to ADRs in architecture-sensitive code

---

**Last Updated**: January 8, 2026
**Status**: Active
**Maintainer**: Team + @copilot
