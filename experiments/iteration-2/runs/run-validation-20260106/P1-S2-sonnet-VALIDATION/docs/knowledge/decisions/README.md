# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records documenting significant choices made during development.

## What is an ADR?

An Architecture Decision Record captures:
- **What** decision was made
- **Why** it was chosen over alternatives
- **What** the consequences are (both positive and negative)

ADRs answer the question: "Why did we build it this way?"

## When to Create an ADR

Create an ADR when:

1. **Architectural Impact:** Decision affects system structure, performance, or maintainability
2. **Multiple Alternatives:** More than one viable approach existed
3. **Future Questions:** Future team members will likely ask "why did we do this?"
4. **Non-Obvious:** The rationale isn't immediately clear from the code

**Examples of ADR-worthy decisions:**
- Database selection (PostgreSQL vs MongoDB vs DynamoDB)
- Authentication strategy (JWT vs sessions vs OAuth)
- API design (REST vs GraphQL vs gRPC)
- Deployment platform (AWS vs GCP vs Azure)
- Testing framework (Jest vs Mocha vs Vitest)
- State management (Redux vs Context vs Zustand)

**Not ADR-worthy:**
- Variable naming choices
- Code formatting preferences (use linter config)
- Trivial library selections with obvious replacements

## ADR Template

Use this template for all new ADRs:

```markdown
# ADR-NNN: Decision Title

**Status:** [Proposed | Accepted | Deprecated | Superseded by ADR-XXX]

**Date:** YYYY-MM-DD

**Deciders:** @username1, @username2

**Related:** [Links to related ADRs, issues, PRs]

## Context

What is the issue we're trying to solve? What are the constraints and requirements?

Be specific about:
- The problem or need driving this decision
- Relevant facts about the project context
- Forces at play (business needs, technical constraints, team skills)

Example: "Our application needs to store user data persistently. We expect
100K+ users within 6 months, with complex relational queries for analytics.
The team has experience with SQL databases but limited NoSQL experience.
We need ACID guarantees for financial transactions."

## Decision

What is the change we're proposing or have agreed to?

State the decision clearly and concisely.

Example: "We will use PostgreSQL as our primary database, running on AWS RDS
with automatic backups and read replicas for scaling."

## Alternatives Considered

What other options were evaluated? Why were they not chosen?

Present alternatives fairly - no strawman arguments.

### Alternative 1: [Name]

**Pros:**
- List advantages

**Cons:**
- List disadvantages

**Why not chosen:**
- Specific reasons

### Alternative 2: [Name]

**Pros:**
- List advantages

**Cons:**
- List disadvantages

**Why not chosen:**
- Specific reasons

Example:

### Alternative 1: MongoDB

**Pros:**
- Flexible schema for rapid iteration
- Strong JSON support
- Good horizontal scaling

**Cons:**
- Team lacks NoSQL experience (training needed)
- No native ACID transactions across documents (pre-4.0)
- Analytics queries more complex than SQL

**Why not chosen:**
We need ACID guarantees for financial transactions, and complex relational
queries are central to our analytics features. The learning curve and
transaction limitations outweigh schema flexibility benefits.

### Alternative 2: DynamoDB

**Pros:**
- Fully managed (no ops overhead)
- Excellent scaling
- Pay-per-request pricing

**Cons:**
- Complex query patterns require careful schema design
- No support for complex joins or analytics queries
- Vendor lock-in to AWS

**Why not chosen:**
Our analytics requirements involve complex multi-table queries that don't
map well to DynamoDB's key-value model. SQL databases are better suited
for our access patterns.

## Consequences

What becomes easier or harder as a result of this decision?

Be honest about trade-offs - no decision is perfect.

**Positive:**
- List benefits and what becomes easier
- Include both immediate and long-term gains

**Negative:**
- List costs and what becomes harder
- Include both immediate and long-term challenges

**Neutral:**
- List changes that are neither clearly positive nor negative

Example:

**Positive:**
- Rich SQL query support enables complex analytics
- ACID transactions ensure data consistency for financial operations
- Team can be productive immediately (existing SQL skills)
- Excellent tooling and monitoring ecosystem

**Negative:**
- Vertical scaling limits (though RDS supports large instances)
- Schema migrations require careful planning
- More expensive than some NoSQL alternatives at scale

**Neutral:**
- Need to manage database schema versioning
- Must design indexes thoughtfully for query performance

## Implementation Notes

Practical details about putting this decision into practice.

Example:
- Database version: PostgreSQL 15
- Hosting: AWS RDS with Multi-AZ deployment
- Migration tool: Prisma for schema management
- Backup: Automated daily snapshots, 7-day retention
- Monitoring: CloudWatch + pgBadger for query analysis

## References

Links to research, documentation, or discussions that informed this decision.

- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [Team discussion in Slack](https://example.slack.com/archives/...)
- [Comparison benchmark results](https://example.com/benchmark)
- [Related GitHub issue #123](https://github.com/owner/repo/issues/123)

## History

Track changes to this ADR over time.

- **2026-01-06:** ADR created and accepted by team
- **2025-12-15:** Initial proposal drafted
```

## Naming Convention

ADRs use sequential numbering with descriptive slugs:

```
001-database-selection.md
002-authentication-strategy.md
003-api-design-rest-vs-graphql.md
004-deployment-platform.md
```

Start numbering at 001 and increment for each new ADR.

## ADR Lifecycle

### Proposed

Decision is under consideration but not yet approved.

**Use when:**
- Gathering feedback on a proposed decision
- Alternatives still being evaluated
- Team discussion ongoing

**Next steps:**
- Discuss with team
- Evaluate alternatives
- Move to Accepted or revise

### Accepted

Decision has been approved and is now the official approach.

**Use when:**
- Team has agreed on the decision
- Implementation is underway or complete
- This is the current standard

**Next steps:**
- Implement the decision
- Reference in code/docs
- Monitor outcomes

### Deprecated

Decision is no longer recommended but still in use.

**Use when:**
- Better approach identified
- Migration planned but incomplete
- Context has changed

**Required information:**
- Why deprecated
- What to use instead
- Migration timeline

### Superseded

Decision has been replaced and is no longer in use.

**Use when:**
- New ADR has replaced this one
- Old approach no longer found in codebase
- Kept for historical reference

**Required information:**
- Link to superseding ADR
- Date of supersession
- Migration completion date

## Contribution Guidelines

### Before Creating an ADR

Ask yourself:

1. **Is this decision significant enough?** Will people ask "why" in 6 months?
2. **Are there real alternatives?** Or is this the only reasonable option?
3. **Do I understand the trade-offs?** Can I honestly assess pros and cons?

If yes to all three, create an ADR.

### Writing Good ADRs

**Do:**
- Present alternatives fairly and honestly
- Acknowledge trade-offs and downsides
- Include enough context for future readers
- Link to supporting research and discussions
- Update status as situations change

**Don't:**
- Present strawman alternatives to justify your preferred choice
- Hide or downplay negative consequences
- Use ADRs for trivial decisions
- Leave ADRs stale when decisions change
- Make decisions in isolation (involve the team)

### ADR Review Process

All ADRs should be:

1. **Drafted** by the decision owner
2. **Reviewed** by relevant stakeholders
3. **Discussed** in team meeting or async
4. **Approved** by consensus or decision authority
5. **Merged** with status "Accepted"

**Review checklist:**
- [ ] Context clearly explains the problem and constraints
- [ ] Decision is stated unambiguously
- [ ] At least 2 alternatives presented fairly
- [ ] Trade-offs documented honestly
- [ ] Consequences include both positive and negative
- [ ] Status and date are set correctly

## AI Agent Integration

When @copilot makes significant architectural decisions, it:

1. **Checks existing ADRs** to understand past decisions
2. **Creates new ADR** for major architectural choices
3. **References ADRs** in implementation code and PR descriptions
4. **Updates ADRs** when decisions evolve or are superseded

This ensures architectural context is preserved and accessible.

## Getting Started

1. **Read existing ADRs:** Understand past decisions and rationale
2. **Reference ADRs:** When asking "why did we do this?", check here first
3. **Propose new ADRs:** Document significant decisions as you make them
4. **Update status:** Mark ADRs as deprecated or superseded when appropriate

ADRs are most valuable when they're actively maintained and trusted as the source of truth.

---

**Last Updated:** 2026-01-06
**ADR Count:** 0 (initial setup)
