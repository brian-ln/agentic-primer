# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant technical and architectural choices.

## What is an ADR?

An Architecture Decision Record (ADR) documents an important decision made about the software architecture, including:
- The context and problem
- Options considered
- The decision made
- Consequences of that decision

ADRs preserve the reasoning behind decisions, helping future maintainers understand **why** something was done, not just **what** was done.

## When to Create an ADR

Create an ADR when you make decisions about:

**Architecture & Design**:
- System architecture and component boundaries
- Technology stack choices
- Framework or library selection
- Database or storage decisions
- API design approaches

**Technical Standards**:
- Coding conventions and patterns
- Testing strategies
- Deployment approaches
- Security practices
- Performance optimization strategies

**Process & Tools**:
- Development workflow changes
- CI/CD pipeline design
- Monitoring and observability approach
- Documentation standards

## ADR Template

Use this template for all new ADRs:

```markdown
# ADR-{number}: {Decision Title}

**Status**: Proposed | Accepted | Deprecated | Superseded
**Date**: YYYY-MM-DD
**Decision Makers**: @username1, @username2
**Consulted**: @username3, @username4
**Last Updated**: YYYY-MM-DD

## Context and Problem Statement

What is the issue we're facing? What question needs to be answered?

Describe:
- The current situation
- The problem or opportunity
- Why this decision is needed
- Constraints or requirements

Example: "We need to choose a database for our new microservice. The service will handle high read load (10k reads/sec) with moderate writes (500 writes/sec). Data is primarily relational with some JSON fields."

## Decision Drivers

What factors are important for this decision?

- Factor 1: [e.g., "Performance at scale"]
- Factor 2: [e.g., "Team familiarity"]
- Factor 3: [e.g., "Cost constraints"]
- Factor 4: [e.g., "Integration with existing systems"]

## Options Considered

List all options evaluated:

1. Option A
2. Option B
3. Option C

### Option A: {Name}

**Description**: Brief explanation of this option.

**Pros**:
- Advantage 1
- Advantage 2
- Advantage 3

**Cons**:
- Disadvantage 1
- Disadvantage 2
- Disadvantage 3

**Estimated Effort**: [Small | Medium | Large]
**Risk Level**: [Low | Medium | High]

### Option B: {Name}

**Description**: Brief explanation of this option.

**Pros**:
- Advantage 1
- Advantage 2

**Cons**:
- Disadvantage 1
- Disadvantage 2

**Estimated Effort**: [Small | Medium | Large]
**Risk Level**: [Low | Medium | High]

### Option C: {Name}

**Description**: Brief explanation of this option.

**Pros**:
- Advantage 1
- Advantage 2

**Cons**:
- Disadvantage 1
- Disadvantage 2

**Estimated Effort**: [Small | Medium | Large]
**Risk Level**: [Low | Medium | High]

## Decision

**Chosen Option**: Option [X]

**Rationale**: Explain why this option was chosen. What made it better than the alternatives?

Key reasons:
1. Reason 1
2. Reason 2
3. Reason 3

## Consequences

What becomes easier or harder as a result of this decision?

### Positive Consequences

- Benefit 1
- Benefit 2
- Benefit 3

### Negative Consequences

- Trade-off 1 (and how we'll mitigate it)
- Trade-off 2 (and how we'll mitigate it)

### Risks and Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Risk 1 | Low/Med/High | Low/Med/High | How we'll handle it |
| Risk 2 | Low/Med/High | Low/Med/High | How we'll handle it |

## Implementation

How will this decision be implemented?

**Action Items**:
1. [ ] Action 1
2. [ ] Action 2
3. [ ] Action 3

**Timeline**: Expected implementation date or duration

**Owner**: @username responsible for implementation

## Validation

How will we know if this decision was correct?

**Success Metrics**:
- Metric 1: Target value
- Metric 2: Target value

**Review Date**: YYYY-MM-DD (when to evaluate this decision)

## Related Decisions

Link to related ADRs:
- Related to: [ADR-123: Other Decision](./123-other-decision.md)
- Supersedes: [ADR-045: Old Decision](./045-old-decision.md)
- Superseded by: (if deprecated)

## References

Supporting materials:
- Documentation: [Link]
- Research: [Link to spike, POC, or analysis]
- External resources: [Articles, benchmarks, case studies]
- Issues/PRs: #123, #456

## Notes

Additional context, discussion notes, or considerations.

## History

Track changes to this ADR:
- **YYYY-MM-DD**: Initial proposal
- **YYYY-MM-DD**: Accepted after team review
- **YYYY-MM-DD**: Updated with implementation learnings
```

## Naming Convention

Name ADR files as: `{number}-{decision-topic}.md`

Examples:
- `001-database-choice.md`
- `002-api-rest-vs-graphql.md`
- `003-monorepo-structure.md`

Number ADRs sequentially. Don't reuse numbers.

## ADR Lifecycle

### Statuses

- **Proposed**: Decision under discussion, not yet made
- **Accepted**: Decision made and being implemented
- **Deprecated**: No longer recommended but still in use
- **Superseded**: Replaced by a newer decision (link to it)

### Creating an ADR

1. Copy the template above
2. Fill in context and options
3. Create PR with "Status: Proposed"
4. Discuss and refine in PR comments
5. Once team agrees, change status to "Accepted"
6. Merge and implement

### Updating an ADR

ADRs should be immutable once accepted. If the decision changes:
1. Don't edit the original ADR
2. Create a new ADR that supersedes it
3. Link them together
4. Change old ADR status to "Superseded"

Exception: Typos, clarifications, adding references can be updated in place.

### When to Revisit

Review ADRs when:
- Technology landscape changes significantly
- Original assumptions no longer hold
- Problems emerge from the decision
- Better alternatives become available
- At scheduled review date (if specified)

## Decision-Making Process

### Collaborative Decisions

For significant decisions:

1. **Propose**: Author creates ADR with "Proposed" status
2. **Discuss**: Team reviews and provides input
3. **Refine**: Author incorporates feedback
4. **Decide**: Team consensus or designated decision maker
5. **Accept**: Status changed to "Accepted"
6. **Implement**: Action items executed
7. **Review**: Evaluate outcomes against success metrics

### Solo Decisions

For smaller decisions, a single person can:
1. Create ADR
2. Mark as "Accepted" immediately
3. Merge
4. Notify team

But prefer collaborative process for anything significant.

## Tips for Writing Good ADRs

### Do:
- ✅ Write clearly and concisely
- ✅ Document all serious options considered
- ✅ Be honest about trade-offs
- ✅ Include enough context for future readers
- ✅ Link to supporting materials
- ✅ Specify success metrics

### Don't:
- ❌ Overexplain obvious decisions
- ❌ Include implementation details (those go in code/docs)
- ❌ Make it too long (aim for 1-2 pages)
- ❌ Skip the "why" behind the decision
- ❌ Forget to update status as decision evolves

## Examples of Good ADRs

### Example: Database Selection

- Clear context (requirements, constraints)
- Three options compared (PostgreSQL, MongoDB, DynamoDB)
- Honest pros/cons for each
- Decision explained with metrics
- Implementation timeline specified

### Example: API Design

- Problem: Need API for mobile and web clients
- Options: REST, GraphQL, gRPC
- Decision drivers: Client diversity, query flexibility, team experience
- Chosen: GraphQL with rationale
- Migration plan from existing REST API

### Example: Testing Strategy

- Context: Growing codebase, insufficient test coverage
- Options: Unit-only, Unit+Integration, Unit+Integration+E2E
- Trade-offs: Speed vs confidence vs maintenance
- Decision: All three layers with specific ratios
- Success metrics: Coverage %, test runtime, bug escape rate

## ADR Anti-Patterns

Avoid these common mistakes:

**Decision After Implementation**:
- Writing ADR after already building something
- ADRs should inform decisions, not justify them retroactively

**Too Much Detail**:
- Including implementation specifics
- ADR is "why", code/docs are "how"

**Analysis Paralysis**:
- Considering too many options
- Focus on 2-4 serious candidates

**No Trade-offs**:
- Only listing positives of chosen option
- Every decision has trade-offs; be honest

**No Success Metrics**:
- Can't evaluate if decision was correct
- Define how you'll measure success

## Finding ADRs

### By Topic
Browse files in this directory, organized numerically.

### By Status
- Active decisions: Status "Accepted"
- Historical context: Status "Superseded"
- In discussion: Status "Proposed"

### By Date
Check "Date" field in each ADR.

## Benefits of ADRs

1. **Preserve Context**: Future maintainers understand why decisions were made
2. **Avoid Repeating Mistakes**: Learn from past trade-offs
3. **Onboard Faster**: New team members ramp up on architectural thinking
4. **Facilitate Debate**: Structured format for discussing options
5. **Track Evolution**: See how system design evolved over time

## Getting Help

- First ADR? Start with a simple one to learn the process
- Unsure if something needs an ADR? Ask: "Will future me wonder why we did this?"
- Need feedback? Create PR and request specific reviewers
- Want examples? Look at existing ADRs in this directory
- Questions? Open an issue with "knowledge-base" label

## Resources

Learn more about ADRs:
- [ADR GitHub Organization](https://adr.github.io/)
- [Michael Nygard's ADR article](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)
- [ADR Tools](https://github.com/npryce/adr-tools)

---

**Remember**: ADRs are for significant decisions. Not every choice needs an ADR. If in doubt, consider: "Will this decision impact the project long-term?"
