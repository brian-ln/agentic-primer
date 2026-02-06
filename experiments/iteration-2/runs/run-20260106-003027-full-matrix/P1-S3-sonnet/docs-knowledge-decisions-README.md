# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant architectural and technical decisions made for this project.

## What is an ADR?

An Architecture Decision Record (ADR) captures:
- **Context**: The forces at play (technical, political, social, project)
- **Decision**: The response to these forces
- **Consequences**: The resulting context after applying the decision

ADRs are **immutable records** - we don't change past decisions, we supersede them with new ADRs.

## Why ADRs?

- **Preserve context**: Future developers understand *why* decisions were made
- **Enable informed changes**: Know what trade-offs were considered
- **Reduce repeated debates**: Document decisions to avoid rehashing
- **Onboard faster**: New team members learn project history
- **Support AI agents**: Provide context for autonomous decision-making

## ADR Format

Each ADR follows this structure:

```markdown
# ADR [Number]: [Title]

**Date**: YYYY-MM-DD
**Status**: [Proposed / Accepted / Deprecated / Superseded by ADR-XXX]
**Deciders**: [Who was involved in the decision]
**Tags**: [relevant, tags, for, searching]

## Context

What is the issue we're facing? What are the constraints and requirements?

## Decision

What did we decide to do? Be specific and actionable.

## Rationale

Why did we make this decision? What factors influenced us?

## Alternatives Considered

1. **Alternative 1**
   - Pros: [Benefits]
   - Cons: [Drawbacks]
   - Why not chosen: [Reason]

2. **Alternative 2**
   - Pros: [Benefits]
   - Cons: [Drawbacks]
   - Why not chosen: [Reason]

## Consequences

### Positive
- [Benefit 1]
- [Benefit 2]

### Negative
- [Trade-off 1]
- [Trade-off 2]

### Neutral
- [Side effect 1]

## Implementation

How will this decision be implemented? What changes are needed?

## Validation

How will we know this decision was correct? What metrics or outcomes?

## References

- [External documentation]
- [Related PRs: #123]
- [Related ADRs: ADR-001]
- [Discussion threads]

---

**Reviewed**: [Date of last review]
**Next Review**: [When should this be reviewed again]
```

## ADR Lifecycle

```
Proposed → Under Review → Accepted → Implemented → [Superseded/Deprecated]
```

### Status Definitions

- **Proposed**: Decision suggested but not yet finalized
- **Accepted**: Decision approved and being/will be implemented
- **Implemented**: Decision is in production
- **Deprecated**: No longer relevant (kept for historical record)
- **Superseded**: Replaced by a newer ADR (reference the new one)

## Naming Convention

```
NNN-short-title.md
```

- **NNN**: Three-digit sequential number (001, 002, etc.)
- **short-title**: Lowercase with hyphens, descriptive

Examples:
- `001-use-github-actions-for-ci.md`
- `002-adopt-rest-over-graphql.md`
- `003-knowledge-base-structure.md`

## When to Create an ADR

Create an ADR when making decisions about:

### Architecture & Design
- ✅ Choosing frameworks or libraries
- ✅ Defining system boundaries
- ✅ Selecting architectural patterns
- ✅ Database schema design

### Technology Choices
- ✅ Programming languages
- ✅ Build tools and CI/CD
- ✅ Deployment strategies
- ✅ Third-party services

### Processes & Standards
- ✅ Code review policies
- ✅ Testing strategies
- ✅ Branching strategies
- ✅ Documentation standards

### Don't Create ADRs For
- ❌ Routine bug fixes
- ❌ Minor refactoring
- ❌ Temporary workarounds
- ❌ Implementation details (use patterns instead)

## How to Create an ADR

### 1. Choose a Number

```bash
# Find next available number
ls docs/knowledge/decisions/ | grep "^[0-9]" | sort | tail -1
# If last is 005-xyz.md, use 006
```

### 2. Create File from Template

```bash
cp docs/knowledge/decisions/000-template.md \
   docs/knowledge/decisions/006-your-decision.md
```

### 3. Fill in All Sections

- Be specific and concrete
- Include code examples where relevant
- Document ALL alternatives considered
- Be honest about trade-offs

### 4. Get Feedback

- Open PR with label `adr`
- Request review from relevant stakeholders
- Discuss in PR comments
- Iterate until consensus

### 5. Merge and Reference

- Merge when accepted
- Reference in related PRs: "See ADR-006"
- Link from implementation code

## Searching ADRs

```bash
# Search all ADRs
grep -r "search-term" docs/knowledge/decisions/

# List all ADRs
ls -1 docs/knowledge/decisions/

# Find ADRs by tag
grep "Tags:.*database" docs/knowledge/decisions/*.md

# Find active ADRs
grep "Status: Accepted" docs/knowledge/decisions/*.md
```

## ADR Index

<!-- Auto-maintained index -->

### Active Decisions

(No ADRs yet - will be auto-populated as decisions are added)

### Superseded Decisions

(None yet)

### Proposed (Under Review)

(None yet)

## Example ADRs

### ADR-000: Use Architecture Decision Records

**Status**: Accepted
**Summary**: We will document significant architectural decisions using ADRs.
**Rationale**: Preserve context and enable informed future changes.

See: `000-template.md` (serves as both template and example)

## Best Practices

### Writing Good ADRs

✅ **Do:**
- Write in active voice ("We will use X")
- Be specific about the decision
- Document the context thoroughly
- List all alternatives seriously considered
- Be honest about trade-offs
- Include measurable success criteria
- Link to external resources
- Set a review date

❌ **Don't:**
- Make vague decisions ("Improve performance")
- Skip alternatives section
- Ignore negative consequences
- Write implementation guides (that's for patterns)
- Change old ADRs (create new ones instead)
- Make decisions in isolation

### Reviewing ADRs

When reviewing an ADR PR:

1. **Context**: Is the problem clearly stated?
2. **Decision**: Is it specific and actionable?
3. **Alternatives**: Were reasonable options considered?
4. **Consequences**: Are trade-offs honestly documented?
5. **References**: Are sources cited?

### Superseding ADRs

When a decision is no longer valid:

1. Create new ADR documenting the new decision
2. Reference the old ADR in "Alternatives Considered"
3. Update old ADR's status to "Superseded by ADR-XXX"
4. Explain why the original decision is no longer valid

Example:
```markdown
# ADR 002: Use REST API

**Status**: Superseded by ADR-015
**Date**: 2024-01-15

[Original decision content...]

---

**Update 2025-03-20**: This decision has been superseded by ADR-015
which adopts GraphQL for improved client flexibility. See ADR-015 for
details on why this decision was reversed.
```

## AI Agent Integration

AI agents should:

1. **Search ADRs** before making architectural decisions
2. **Reference ADRs** in PR descriptions when following decisions
3. **Propose ADRs** when encountering decisions not yet documented
4. **Flag conflicts** if implementation contradicts an ADR

Example agent behavior:
```
Agent: I need to add a new API endpoint. Searching for API design decisions...
Agent: Found ADR-002: Use REST API. I will follow REST conventions.
Agent: Creating endpoint following ADR-002 guidelines.
```

## Metrics

**Total ADRs**: [Auto-calculated]
**Active**: [Auto-calculated]
**Superseded**: [Auto-calculated]
**Average Time to Acceptance**: [Auto-calculated]
**Most Referenced**: [Auto-calculated - top 5]

Last updated: [Auto-populated]

## Templates

### Quick Decision Template

For simpler decisions, use this condensed format:

```markdown
# ADR [Number]: [Title]

**Date**: YYYY-MM-DD
**Status**: Accepted

## Decision

[What we decided]

## Rationale

[Why we decided this]

## Consequences

- [Key consequence 1]
- [Key consequence 2]
```

### Major Decision Template

For complex decisions, use the full template in `000-template.md`

## Migration from Other Formats

If you have existing architecture documentation:

1. **Identify key decisions** in existing docs
2. **Extract into individual ADRs** (one decision per ADR)
3. **Backdate appropriately** (use original decision date)
4. **Number sequentially** in chronological order
5. **Keep original docs** as references

## FAQs

**Q: Should every technical decision be an ADR?**
A: No. Only decisions with significant long-term impact.

**Q: Can we change an old ADR?**
A: Only to fix typos or add references. For decision changes, create a new ADR.

**Q: Who can create ADRs?**
A: Anyone (including AI agents). All ADRs should be reviewed before merging.

**Q: How detailed should ADRs be?**
A: Detailed enough that someone in 2 years understands the decision and context.

**Q: What if we disagree on a decision?**
A: Document the disagreement in the ADR and make a time-boxed decision. Review later.

## Resources

- [Architecture Decision Records (adr.github.io)](https://adr.github.io/)
- [Joel Parker Henderson's ADR examples](https://github.com/joelparkerhenderson/architecture-decision-record)
- [Documenting Architecture Decisions by Michael Nygard](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)

---

**Maintained by**: All team members
**Format**: Markdown (ADR format)
**Questions?**: Open issue with label `adr`
