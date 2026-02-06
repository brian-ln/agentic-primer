# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant architectural and design decisions made in this project.

## Purpose

ADRs provide:

- **Historical Context** - Why decisions were made
- **Rationale** - Trade-offs and alternatives considered
- **Consequences** - Impact on the codebase
- **Stability** - Prevent re-litigation of settled decisions

## What to Document

Record decisions that:

- ✅ Affect project structure or architecture
- ✅ Impact multiple components or teams
- ✅ Are hard to reverse or change later
- ✅ Set precedents for future work
- ✅ Resolve contentious discussions

Don't record:

- ❌ Implementation details of single features
- ❌ Routine code choices
- ❌ Temporary workarounds
- ❌ Obvious or uncontroversial decisions

## ADR Template

Use this template for new ADRs:

```markdown
# ADR-{number}: [Decision Title]

**Status**: Proposed | Accepted | Deprecated | Superseded by ADR-{number}

**Date**: YYYY-MM-DD

**Deciders**: @username1, @username2

**Tags**: architecture, database, api, frontend, backend, infrastructure

## Context

What is the issue we're addressing? What factors led to this decision?

### Background

Provide background information relevant to the decision.

### Constraints

- Technical constraints (existing systems, performance, compatibility)
- Business constraints (timeline, budget, resources)
- Organizational constraints (team skills, compliance, security)

## Decision

What are we doing about it? State the decision clearly and concisely.

### Approach

Describe the chosen approach in detail.

### Rationale

Why this approach over alternatives?

## Alternatives Considered

### Alternative 1: [Name]

**Description**: Brief explanation

**Pros**:
- Advantage 1
- Advantage 2

**Cons**:
- Disadvantage 1
- Disadvantage 2

**Why Not Chosen**: Specific reason

### Alternative 2: [Name]

**Description**: Brief explanation

**Pros**: ...

**Cons**: ...

**Why Not Chosen**: Specific reason

## Consequences

### Positive

What becomes easier or better as a result?

- Benefit 1
- Benefit 2
- Benefit 3

### Negative

What becomes harder or worse?

- Trade-off 1
- Trade-off 2
- Trade-off 3

### Neutral

Other impacts that are neither clearly positive nor negative.

- Impact 1
- Impact 2

## Implementation

### Action Items

- [ ] Task 1
- [ ] Task 2
- [ ] Task 3

### Migration Path

If changing existing systems, describe the migration approach.

### Timeline

Expected timeline for implementation and adoption.

## Compliance

How does this decision align with:

- Security requirements
- Performance requirements
- Accessibility standards
- Coding standards
- Organizational policies

## Validation

How will we know if this decision was correct?

**Success Metrics**:
- Metric 1: Target value
- Metric 2: Target value

**Review Date**: YYYY-MM-DD

## Related Decisions

- [ADR-001: Related Decision](./adr-001.md) - How they relate
- [ADR-002: Another Decision](./adr-002.md) - Dependencies

## Related Patterns

- [Pattern: Implementation Pattern](../patterns/pattern-name.md)

## References

- [Documentation](https://example.com)
- [Research Article](https://example.com)
- [Discussion](https://github.com/org/repo/issues/123)
- [RFC](https://github.com/org/repo/pull/456)

## Changelog

- **YYYY-MM-DD**: Decision proposed
- **YYYY-MM-DD**: Decision accepted
- **YYYY-MM-DD**: First implementation merged
- **YYYY-MM-DD**: Reviewed and updated
```

## Naming Convention

ADR files should be named:

```
adr-{number}-{slug}.md
```

**Examples:**
- `adr-001-use-postgresql.md`
- `adr-002-adopt-react-hooks.md`
- `adr-003-monorepo-structure.md`

## Status Values

### Proposed

Decision is under discussion. Not yet implemented.

**Use when**: Gathering feedback, evaluating options.

### Accepted

Decision is approved and being implemented.

**Use when**: Team consensus reached, implementation starting.

### Deprecated

Decision is no longer recommended but still in use.

**Use when**: Better approach found but migration not complete.

### Superseded

Decision has been replaced by a newer decision.

**Use when**: Fully migrated to new approach. Link to new ADR.

## ADR Index

Total Decisions: 0

### By Status

- **Proposed**: 0
- **Accepted**: 0
- **Deprecated**: 0
- **Superseded**: 0

### By Category

- **Architecture**: 0
- **Database**: 0
- **API**: 0
- **Frontend**: 0
- **Backend**: 0
- **Infrastructure**: 0
- **Security**: 0
- **Testing**: 0

### Recent Decisions

(No decisions yet)

## Decision Process

### 1. Identify Need

When should you create an ADR?

- Major architectural change proposed
- Multiple approaches debated
- Precedent-setting choice needed
- Stakeholder alignment required

### 2. Research

Before proposing:

- Research alternatives thoroughly
- Consult with experts
- Prototype if uncertain
- Gather data and benchmarks

### 3. Draft ADR

Create ADR document:

- Use template above
- Include all alternatives
- Document trade-offs honestly
- Add supporting evidence

### 4. Review

Submit for review:

- Create PR with ADR
- Tag relevant stakeholders
- Schedule discussion if needed
- Incorporate feedback

### 5. Decide

Reach consensus:

- Discussion in PR comments
- Meeting if complex/contentious
- Document final decision
- Update status to "Accepted"

### 6. Implement

Execute decision:

- Create implementation issues
- Link to ADR in PRs
- Track progress
- Update ADR as needed

### 7. Review

Periodic review:

- Check if decision still valid
- Measure success metrics
- Consider deprecation if superseded
- Update or create new ADR

## Examples of Good ADRs

### ADR-001: Database Selection

**Good because**:
- Clear problem statement
- Multiple alternatives evaluated
- Quantitative comparison (benchmarks)
- Honest about trade-offs
- Actionable consequences

### ADR-015: Migration Strategy

**Good because**:
- Detailed migration path
- Risk mitigation plan
- Rollback procedure
- Success metrics defined

## Common Pitfalls

### Pitfall: Too Detailed

**Problem**: ADR reads like implementation guide.

**Solution**: Focus on "what" and "why", not "how". Implementation details go in code or separate docs.

### Pitfall: No Alternatives

**Problem**: Only presents one option.

**Solution**: Always document at least 2-3 alternatives, even if obvious.

### Pitfall: Missing Context

**Problem**: Decision seems arbitrary without context.

**Solution**: Explain the situation that prompted this decision.

### Pitfall: Outdated Status

**Problem**: ADR marked "Accepted" but approach changed.

**Solution**: Update status to "Deprecated" or "Superseded" when things change.

## ADR Relationships

### Supersedes

When a new ADR replaces an old one:

1. Update old ADR status to "Superseded by ADR-{number}"
2. Link new ADR in old one
3. Explain why new approach is better

### Depends On

When an ADR builds on others:

1. Link to foundational ADRs
2. Explain dependency
3. Consider impact if foundational ADR changes

### Related To

When ADRs affect similar areas:

1. Cross-reference in both ADRs
2. Explain relationship
3. Note any conflicts or synergies

## Tools and Automation

- **Manual Creation**: Copy template and fill in
- **GitHub Actions**: Auto-generate from RFC PRs
- **Search**: `grep -r "database" .` to find related ADRs

## Best Practices

### Writing

- ✅ Use clear, concise language
- ✅ Include concrete examples
- ✅ Quantify trade-offs when possible
- ✅ Link to supporting evidence
- ✅ Keep it focused on one decision

### Reviewing

- ✅ Challenge assumptions politely
- ✅ Ask for clarification
- ✅ Suggest additional alternatives
- ✅ Check for missing consequences
- ✅ Verify alignment with existing ADRs

### Maintaining

- ✅ Review ADRs every 6 months
- ✅ Update status as things change
- ✅ Archive superseded decisions
- ✅ Link from code to relevant ADRs

## Resources

### Templates

- [ADR Template](./templates/adr-template.md) - Copy this
- [RFC Template](./templates/rfc-template.md) - For proposals

### Examples

- [Michael Nygard's ADR repo](https://github.com/joelparkerhenderson/architecture-decision-record)
- [ADR GitHub organization](https://adr.github.io/)

### Articles

- [Documenting Architecture Decisions](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)
- [Why Write ADRs](https://engineering.atspotify.com/2020/04/when-should-i-write-an-architecture-decision-record/)

## Questions?

- Create issue with label `adr`
- Ask in architecture discussions
- Ping @architecture-team

---

**Maintainer**: Architecture Team
**Last Updated**: 2026-01-08
**Version**: 1.0.0
