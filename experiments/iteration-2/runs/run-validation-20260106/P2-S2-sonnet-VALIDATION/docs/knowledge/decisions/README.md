# Architectural Decision Records (ADRs)

This directory contains records of significant architectural and technical decisions made for this project.

## What are ADRs?

Architectural Decision Records document important decisions about the project's architecture, technology choices, and design approach. They help us:
- **Remember why:** Capture context and rationale for future reference
- **Onboard faster:** New team members understand decisions without archaeology
- **Avoid revisiting:** Prevent rehashing settled decisions
- **Learn from history:** Understand what worked and what didn't

## ADR Structure

Each ADR follows this format:

1. **Title:** Short, descriptive title with number (e.g., "001 - Use REST API")
2. **Status:** Proposed, Accepted, Deprecated, Superseded
3. **Context:** What situation led to this decision?
4. **Decision:** What did we decide to do?
5. **Consequences:** What are the impacts (positive and negative)?
6. **Alternatives Considered:** What other options were evaluated?

## ADR Template

```markdown
# ADR-### - Title

**Status:** Proposed | Accepted | Deprecated | Superseded by ADR-###

**Date:** YYYY-MM-DD

**Decision Makers:** Names or roles

## Context

What is the issue we're trying to address? What factors are influencing this decision?

- Business requirement
- Technical constraint
- Team capability
- Timeline pressure

## Decision

We will [decision statement].

### Implementation Details

How will this be implemented?

## Consequences

### Positive

- Benefit 1
- Benefit 2

### Negative

- Trade-off 1
- Trade-off 2

### Neutral

- Side effect 1

## Alternatives Considered

### Option A: [Alternative name]

**Pros:**
- Advantage 1
- Advantage 2

**Cons:**
- Disadvantage 1
- Disadvantage 2

**Why rejected:** Reason

### Option B: [Alternative name]

**Pros:**
- Advantage 1

**Cons:**
- Disadvantage 1

**Why rejected:** Reason

## References

- Link to relevant documentation
- Related ADRs
- External resources
```

## Numbering Convention

- **Format:** `###-descriptive-name.md`
- **Examples:**
  - `001-github-copilot-automation.md`
  - `002-use-postgresql.md`
  - `003-monorepo-vs-polyrepo.md`

Numbers are sequential and never reused, even if an ADR is deprecated.

## When to Create an ADR

Create an ADR for decisions that:
- **Affect architecture:** Service boundaries, data flow, deployment model
- **Technology choices:** Frameworks, databases, languages, tools
- **Process changes:** Development workflow, testing strategy, deployment process
- **Have significant impact:** Will affect multiple teams or be expensive to change

Don't create ADRs for:
- Minor implementation details
- Easily reversible decisions
- Team-specific preferences without technical impact

## ADR Lifecycle

### 1. Proposed

Initial draft, seeking feedback.

```markdown
**Status:** Proposed
```

### 2. Accepted

Team has agreed, implementation may proceed.

```markdown
**Status:** Accepted
**Date:** 2026-01-06
```

### 3. Deprecated

No longer recommended, but not forbidden.

```markdown
**Status:** Deprecated
**Reason:** Better alternative found (see ADR-042)
**Date:** 2026-03-15
```

### 4. Superseded

Replaced by a newer ADR.

```markdown
**Status:** Superseded by ADR-042
**Date:** 2026-03-15
```

## Contributing an ADR

### Process

1. **Create draft:**
   ```bash
   # Find next number
   ls docs/knowledge/decisions/ | grep "^[0-9]" | sort -n | tail -1

   # Create new ADR
   cp docs/knowledge/decisions/template.md docs/knowledge/decisions/042-your-decision.md
   ```

2. **Fill out template:**
   - Provide context and decision rationale
   - Document alternatives considered
   - List consequences (positive and negative)

3. **Share for feedback:**
   - Create PR with "ADR: " prefix
   - Request review from stakeholders
   - Discuss in team meeting if needed

4. **Mark as Accepted:**
   - Update status to "Accepted"
   - Add decision date
   - Merge PR

### Tips for Good ADRs

- **Be specific:** "Use PostgreSQL 14+" not "Use a database"
- **Capture context:** Explain the situation that led to the decision
- **Be honest:** Document negative consequences too
- **Show alternatives:** Demonstrate you considered options
- **Keep it concise:** 1-2 pages max, link to detailed docs
- **Write for future:** Explain things that seem obvious now

## Using ADRs with @copilot

Reference ADRs in issue descriptions to ensure @copilot follows established decisions:

```markdown
Implementation should follow:
- docs/knowledge/decisions/001-github-copilot-automation.md
- docs/knowledge/decisions/005-api-versioning-strategy.md
```

## Maintenance

- **Review regularly:** Are old ADRs still relevant?
- **Update status:** Mark deprecated/superseded decisions
- **Link from code:** Reference ADRs in code comments
- **Keep history:** Never delete ADRs, only deprecate them

## Resources

- [ADR GitHub Organization](https://adr.github.io/)
- [Michael Nygard's ADR Post](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)
- [ADR Tools](https://github.com/npryce/adr-tools)
