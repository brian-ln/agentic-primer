# Architecture Decisions

Record of significant decisions made in this project, with context and rationale.

## Purpose

Architecture Decision Records (ADRs) capture:
- **What** was decided
- **Why** that choice was made
- **What** alternatives were considered
- **What** the consequences are

This helps future contributors (human and AI) understand the reasoning behind the codebase structure.

## What Goes Here

### Good Decision Records

✅ **Technology choices**: "Use PostgreSQL for primary database"
✅ **Architecture patterns**: "Adopt microservices architecture"
✅ **API designs**: "Use GraphQL instead of REST for client API"
✅ **Process decisions**: "Require PR reviews before merge"
✅ **Security policies**: "Implement OAuth 2.0 for authentication"

### Not Decision Records

❌ **Implementation details**: Put in `patterns/` instead
❌ **Bug fixes**: Normal PR documentation is sufficient
❌ **Temporary workarounds**: Document in code comments
❌ **Process learnings**: Put in `insights/` instead

## Decision Template

```markdown
# Decision: [Short Title]

**Status**: [Proposed / Accepted / Deprecated / Superseded]
**Date**: YYYY-MM-DD
**Deciders**: @username, @team
**Source**: PR #XXX or Issue #YYY

## Context and Problem Statement

[Describe the context and problem that needs a decision]

### Business Context
[Why this matters to the project/business]

### Technical Context
[Current system state, constraints]

### Stakeholders
[Who is affected by this decision]

## Decision Drivers

- [Driver 1: e.g., "Performance requirements"]
- [Driver 2: e.g., "Team expertise"]
- [Driver 3: e.g., "Cost constraints"]

## Considered Options

### Option 1: [Name]
**Description**: [What this option entails]

**Pros**:
- Benefit 1
- Benefit 2

**Cons**:
- Drawback 1
- Drawback 2

**Estimated effort**: [High/Medium/Low]

### Option 2: [Name]
[Same structure as Option 1]

### Option 3: [Name]
[Same structure as Option 1]

## Decision Outcome

**Chosen option**: Option [X] - [Name]

### Rationale

[Explain why this option was selected over alternatives]

Key factors:
1. [Primary reason]
2. [Secondary reason]
3. [Supporting reason]

### Consequences

**Positive**:
- [Benefit 1]
- [Benefit 2]

**Negative**:
- [Tradeoff 1]
- [Tradeoff 2]

**Neutral**:
- [Other impact 1]
- [Other impact 2]

### Implementation

- [What needs to be done]
- [Timeline if applicable]
- [Responsible parties]

## Validation

How we'll know this decision was correct:
- [Metric 1]
- [Metric 2]
- [Success criteria]

## Follow-up

- [ ] Action item 1
- [ ] Action item 2
- [ ] Review date: YYYY-MM-DD

## References

- [Link to research]
- [External documentation]
- [Related decisions]
- PR: #XXX
- Issues: #YYY, #ZZZ

---
_Documented in PR #XXX_
```

## Status Values

### Proposed
Decision under discussion, not yet accepted.

### Accepted
Decision approved and in effect.

### Deprecated
Decision no longer recommended but still in use (plan migration).

### Superseded
Decision replaced by newer decision (link to replacement).

## Categories

Common decision categories:

### Technology Stack
- Programming languages
- Frameworks and libraries
- Databases
- Infrastructure

### Architecture
- System design patterns
- Service boundaries
- Communication patterns
- Data flow

### Process
- Development workflow
- Review requirements
- Release process
- Testing strategy

### Security
- Authentication methods
- Authorization model
- Data protection
- Compliance requirements

### Performance
- Caching strategies
- Optimization approaches
- Scaling decisions

## Search Tips

### Find decisions by topic
```bash
grep -r "database" decisions/
```

### Find recent decisions
```bash
ls -lt decisions/*.md | head -10
```

### Find decisions by status
```bash
grep -l "Status: Accepted" decisions/*.md
```

### Find superseded decisions
```bash
grep -l "Superseded" decisions/*.md
```

## Usage Guide

### For AI Agents

When implementing features:

1. **Search** for related decisions
2. **Understand** the rationale
3. **Align** implementation with decision
4. **Flag** if implementation conflicts with decision
5. **Propose** new decision if requirements changed

### For Human Developers

1. Review decisions before major changes
2. Document new architectural choices
3. Update decisions when context changes
4. Link PRs to relevant decisions

## Example Decisions

### Example 1: Database Choice

```markdown
# Decision: Use PostgreSQL for Primary Database

**Status**: Accepted
**Date**: 2025-12-15
**Deciders**: @tech-lead, @backend-team
**Source**: Issue #45

## Context and Problem Statement

Need to select primary database for new application. Requirements include:
- Complex queries with joins
- ACID transactions
- JSON data support
- Good community support
- Cost-effective hosting

## Decision Drivers

- Data integrity requirements (financial data)
- Complex query needs (reporting)
- Team has PostgreSQL experience
- Need good tooling and hosting options

## Considered Options

### Option 1: PostgreSQL
**Pros**:
- ACID compliant
- Excellent JSON support (JSONB)
- Rich query capabilities
- Mature, stable
- Great tooling
- Many hosting options

**Cons**:
- Vertical scaling limits
- Setup complexity

### Option 2: MongoDB
**Pros**:
- Flexible schema
- Horizontal scaling
- Simple to start

**Cons**:
- Weaker consistency guarantees
- Complex query limitations
- Team less familiar

### Option 3: MySQL
**Pros**:
- Team familiarity
- Good performance
- Wide adoption

**Cons**:
- Weaker JSON support
- Less advanced features
- Oracle ownership concerns

## Decision Outcome

**Chosen option**: PostgreSQL

### Rationale

1. ACID requirements critical for financial data
2. JSON support needed for flexible metadata
3. Team already experienced with PostgreSQL
4. Excellent query optimizer for reporting needs
5. Many cost-effective hosting options

### Consequences

**Positive**:
- Strong data consistency
- Rich query capabilities
- Familiar to team
- Excellent documentation

**Negative**:
- Need to plan for scaling (sharding complex)
- Initial setup more complex than MongoDB

**Neutral**:
- Industry-standard choice (not innovative but proven)

## Validation

Success metrics:
- All data consistency tests pass
- Query performance meets SLA (<100ms for 95% queries)
- No data loss incidents
- Team productive within 2 weeks

## References

- PostgreSQL docs: https://postgresql.org/docs/
- Benchmark comparison: [internal doc]
- PR implementing schema: #78

---
_Documented in PR #48_
```

### Example 2: API Design

```markdown
# Decision: Use GraphQL for Client API

**Status**: Accepted
**Date**: 2025-11-20
**Deciders**: @frontend-team, @backend-team
**Source**: Issue #23

## Context and Problem Statement

Mobile and web clients need efficient data fetching. Current REST API requires multiple round trips for common screens.

[... rest of decision following template]
```

## Contributing

### Creating New Decisions

1. Identify significant architectural choice
2. Use template above
3. Document all considered options
4. Explain rationale clearly
5. Link to related PRs/issues

### Updating Decisions

When context changes:
1. Update the decision record
2. Add version history section
3. Document what changed and why
4. Consider if status should change

### Deprecating Decisions

When decision no longer applicable:
1. Change status to "Deprecated" or "Superseded"
2. Link to replacement decision
3. Document migration path
4. Keep for historical reference

## Review Process

### New Decisions

1. Create as "Proposed"
2. Discuss in PR or issue
3. Update based on feedback
4. Change to "Accepted" when approved

### Existing Decisions

Review quarterly:
- Are decisions still valid?
- Has context changed?
- Should any be deprecated?
- Are consequences accurate?

## Maintenance

### Quality Checks

- All options documented with pros/cons
- Clear rationale for chosen option
- Consequences identified
- References included
- Searchable keywords

### Metrics

- Number of decisions by status
- Average decision age
- Decisions reviewed vs total
- Decisions referenced in PRs

---

**Decision Count**: Auto-updated by workflow
**Last Reviewed**: 2026-01-06
**Maintainers**: @owner, automated workflows
