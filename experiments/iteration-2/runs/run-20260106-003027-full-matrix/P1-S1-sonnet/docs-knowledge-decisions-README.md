# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant technical decisions made for this project.

## Purpose

ADRs capture the **why** behind technical choices:
- **Historical Context** - Understand past decisions
- **Knowledge Transfer** - Onboard new team members
- **Avoid Rehashing** - Don't re-debate settled decisions
- **Change Rationale** - Document when/why decisions change

## What Belongs Here

Record decisions that are:
- **Significant** - Impact architecture, tooling, or processes
- **Consequential** - Affect multiple components or teams
- **Non-obvious** - Require explanation of trade-offs
- **Reversible** - May need revisiting later

**Examples:**
- Technology choices (database, framework, cloud provider)
- Architectural patterns (microservices, monolith, event-driven)
- API design (REST vs GraphQL, versioning strategy)
- Security approaches (auth method, encryption)
- Deployment strategies (blue-green, canary, rolling)

**Not ADRs:**
- Implementation details (belongs in code comments)
- Temporary experiments (use spike branch docs)
- Personal preferences without rationale

## ADR Format

We use [MADR](https://adr.github.io/madr/) (Markdown Architectural Decision Records) format:

```markdown
# {Number}. {Title}

Date: YYYY-MM-DD
Status: {Proposed | Accepted | Deprecated | Superseded}
Deciders: {List people involved}

## Context and Problem Statement

What is the issue we're trying to solve?

## Decision Drivers

- {Driver 1, e.g., cost, performance, developer experience}
- {Driver 2}
- {Driver 3}

## Considered Options

1. {Option 1}
2. {Option 2}
3. {Option 3}

## Decision Outcome

Chosen option: "{Option X}", because {justification}

### Positive Consequences

- {Benefit 1}
- {Benefit 2}

### Negative Consequences

- {Drawback 1}
- {Drawback 2}

## Pros and Cons of the Options

### {Option 1}

- **Good**: {Pro 1}
- **Good**: {Pro 2}
- **Bad**: {Con 1}
- **Bad**: {Con 2}

### {Option 2}

- **Good**: {Pro 1}
- **Bad**: {Con 1}

## Links

- {Related ADRs}
- {External references}
```

## Naming Convention

Format: `NNN-{kebab-case-title}.md`

Examples:
- `001-use-rest-api.md`
- `002-postgres-over-mongodb.md`
- `003-adopt-github-copilot.md`

Numbers are sequential and never reused (even for superseded ADRs).

## Lifecycle of an ADR

1. **Proposed** - Under discussion, not yet implemented
2. **Accepted** - Decision made, implementation in progress or complete
3. **Deprecated** - Still in use but scheduled for removal
4. **Superseded** - Replaced by newer ADR (link to replacement)

## How to Use

### Creating a New ADR

1. Copy the template above
2. Assign next sequential number
3. Fill in all sections (no placeholders!)
4. Discuss with team before setting status to "Accepted"
5. Update this README's index

### Referencing ADRs

In code comments, PRs, or issues:
```
// See ADR-001 for why we use REST instead of GraphQL
```

In other documentation:
```markdown
As decided in [ADR-001: Use REST API](./decisions/001-use-rest-api.md)...
```

### For GitHub Copilot

When creating issues or PRs, reference relevant ADRs in the "Knowledge References" field. Copilot will use these to align implementations with architectural decisions.

## Decision Index

| Number | Title | Status | Date |
|--------|-------|--------|------|
| [001](./001-use-rest-api.md) | Use REST API for external interface | Accepted | 2026-01-06 |

---

**Resources:**
- [ADR GitHub Organization](https://adr.github.io/)
- [MADR Template](https://adr.github.io/madr/)
- [When to Write an ADR](https://github.com/joelparkerhenderson/architecture-decision-record#suggestions-for-writing-good-adrs)

**Last Updated:** 2026-01-06
**Maintainer:** @owner
