# Architecture Decision Records (ADRs)

Documentation of significant technical decisions and their rationale.

## What Goes Here?

Document decisions when you:
- Choose a technology, framework, or library
- Decide on an architectural approach or pattern
- Establish a process or workflow
- Choose between competing alternatives
- Make a trade-off with long-term implications

## Decision Record Template

```markdown
# ADR-NNN: Decision Title

## Status
Accepted | Rejected | Deprecated | Superseded by ADR-XXX

## Context
What is the issue we're facing? What factors are in play?

## Decision
What decision did we make?

## Consequences
What are the positive and negative outcomes of this decision?

### Positive
- Benefit 1
- Benefit 2

### Negative
- Cost 1
- Cost 2

### Neutral
- Note 1
- Note 2

## Alternatives Considered
What other options did we evaluate?

### Option A
Brief description and why we didn't choose it

### Option B
Brief description and why we didn't choose it

## Related Decisions
- ADR-XXX: Related decision
- Links to issues, PRs, discussions
```

## Numbering Scheme

Use sequential numbering:
- ADR-001, ADR-002, etc.
- Number in filename: `001-decision-title.md`
- Gaps in numbering are okay (deleted/deprecated ADRs)

## When to Write an ADR

### Do write an ADR when:
- Decision has long-term impact (>6 months)
- Decision affects multiple components/teams
- Decision involves significant cost/effort
- Decision is reversible but expensive to reverse
- Future you will ask "why did we do this?"

### Don't write an ADR when:
- Decision is trivial or obvious
- Decision is easily reversible
- Decision only affects a single function/file
- Better documented in code comments

## Status Lifecycle

1. **Proposed**: Decision under consideration
2. **Accepted**: Decision approved and implemented
3. **Deprecated**: Decision still in effect but being phased out
4. **Superseded**: Replaced by a newer decision (link to it)
5. **Rejected**: Considered but not adopted

## Examples of Good Decisions to Document

- "Why we use SQLite instead of PostgreSQL"
- "Why we chose REST over GraphQL"
- "Why we use TypeScript instead of JavaScript"
- "Why we structure folders by feature, not by type"
- "Why we use GitHub Issues instead of Jira"
- "Why we run tests in Docker containers"

## Organization

Each decision record:
- Separate markdown file
- Sequential numbering in filename
- Clear, descriptive title
- Update INDEX.md when adding new ADRs

## Cross-References

Link decisions to:
- Related decisions (supersedes, relates to, conflicts with)
- Patterns that implement the decision
- Insights learned from the decision
- Issues and PRs where decision was discussed

## Evolution

Decisions evolve:
- Update status as circumstances change
- Don't delete old ADRs, mark as superseded
- Create new ADR to reverse or update old decision
- Reference old ADR in new ADR

## Getting Started

1. Read existing ADRs to understand decision history
2. Before making significant decisions, check if ADR exists
3. If decision requires discussion, create ADR as proposal
4. After decision is made, update status to "Accepted"
5. Reference ADRs in code comments and documentation
