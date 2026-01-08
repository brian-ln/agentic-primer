# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant technical decisions made in this project.

## What is an ADR?

An ADR captures an important architectural decision along with its context and consequences. ADRs help teams:

- **Remember why** decisions were made
- **Avoid revisiting** settled questions
- **Onboard new members** with decision context
- **Track evolution** of the architecture over time

## When to Write an ADR

Create an ADR when you:

- Make a significant architectural choice (frameworks, databases, deployment strategy)
- Choose between multiple viable alternatives
- Need to explain "why we did it this way" to future maintainers
- Want to prevent future bikeshedding on a settled decision

## ADR Template

```markdown
# ADR-NNNN: Decision Title

**Status:** Proposed | Accepted | Deprecated | Superseded

**Date:** YYYY-MM-DD

**Deciders:** [Names or roles of people who made this decision]

## Context

What is the issue we're facing? What factors are driving this decision?

## Decision

What is the change we're proposing and/or doing?

## Rationale

Why is this the right decision? What makes this better than alternatives?

## Alternatives Considered

### Alternative 1: [Name]

- **Pros:** [Benefits]
- **Cons:** [Drawbacks]
- **Why not chosen:** [Reason]

### Alternative 2: [Name]

- **Pros:** [Benefits]
- **Cons:** [Drawbacks]
- **Why not chosen:** [Reason]

## Consequences

### Positive

What becomes easier or better because of this decision?

### Negative

What becomes harder or worse? What trade-offs did we accept?

### Neutral

What changes in a neutral way (neither better nor worse)?

## Implementation Notes

How will this decision be implemented? Are there migration steps?

## Related Decisions

- ADR-XXXX: [Related decision]
- [Links to related patterns or insights]

## References

- [External links, documentation, blog posts that influenced this decision]

## Metadata

- **Author:** [Name/Agent]
- **Created:** YYYY-MM-DD
- **Last Updated:** YYYY-MM-DD
- **Status:** [Current status]
```

## Naming Convention

Use this format: `YYYYMMDD-decision-title.md`

Examples:
- `20260106-use-github-actions-for-ci.md`
- `20260106-choose-postgresql-over-mongodb.md`
- `20260106-adopt-microservices-architecture.md`

## Numbering (Optional)

Some teams prefer sequential numbering: `ADR-0001-decision-title.md`

Choose one convention and stick with it.

## Current Decisions

### Infrastructure

- *(No decisions recorded yet - this is a new knowledge base)*

### Architecture

- *(To be added as significant decisions are made)*

### Tooling

- *(To be added as significant decisions are made)*

## Decision Status

ADRs can have the following statuses:

- **Proposed:** Under discussion, not yet accepted
- **Accepted:** Decision has been made and is in effect
- **Deprecated:** No longer recommended, but still in use (explain why)
- **Superseded:** Replaced by a newer decision (link to the new ADR)

## Contributing an ADR

1. **Identify the decision** that needs documentation
2. **Write the ADR** using the template above
3. **Include alternatives** with honest pros/cons
4. **Open a PR** for team discussion
5. **Update status** to "Accepted" after approval
6. **Link it** from this README

## Quality Guidelines

Good ADRs should:

- Explain the **context** and **constraints** clearly
- Present **alternatives** fairly (not strawman arguments)
- Acknowledge **trade-offs** honestly
- Be **specific** to this project (not generic advice)
- Include **consequences** (positive and negative)

Avoid:

- Documenting trivial decisions
- Justifying decisions after the fact without real context
- Presenting only one option (at least consider alternatives)
- Being vague or overly abstract

## When to Update an ADR

Update an ADR when:

- The decision's status changes (accepted → deprecated)
- New consequences are discovered
- Implementation reveals important details
- A better alternative emerges (create a new superseding ADR)

Never delete ADRs. Mark them as deprecated/superseded instead. History matters.

## Examples from Other Projects

For inspiration, see:

- [Michael Nygard's ADR Template](https://github.com/joelparkerhenderson/architecture-decision-record)
- [AWS Architecture Decision Records](https://docs.aws.amazon.com/prescriptive-guidance/latest/architectural-decision-records/welcome.html)
- [Spotify's RFC Process](https://engineering.atspotify.com/2020/04/spotifys-rfcs/)
- [ADR GitHub Organization](https://adr.github.io/)

## Integration with @copilot

AI agents can:

- **Propose ADRs** for decisions they make during development
- **Reference ADRs** when making implementation choices
- **Update ADRs** when consequences change
- **Summarize ADRs** for quick context retrieval

Encourage agents to document their reasoning in ADR format.

## Metadata

- **Created:** 2026-01-06
- **Last Updated:** 2026-01-06
- **Status:** Active
