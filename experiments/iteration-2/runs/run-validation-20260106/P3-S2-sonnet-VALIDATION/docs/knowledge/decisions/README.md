# Architecture Decision Records (ADRs)

Document important architectural and design decisions with their context and consequences.

## What are ADRs?

Architecture Decision Records capture the "why" behind significant choices:
- Why we chose technology X over Y
- Why we structured the system this way
- Why we rejected alternative approaches
- What trade-offs we accepted

ADRs preserve decision context over time, helping future contributors understand the reasoning.

## ADR Index

*Decisions will be added here as they are made and documented.*

**Current decisions:** 0

---

## ADR Format

### Template

Create numbered file: `NNN-decision-title.md` (e.g., `001-use-github-actions.md`)

```markdown
# ADR NNN: [Decision Title]

**Status:** [Proposed | Accepted | Deprecated | Superseded]

**Date:** YYYY-MM-DD

**Deciders:** [List of people involved]

## Context

What is the issue we're facing? What factors are driving this decision?

Include:
- Background information
- Current situation
- Forces at play (constraints, requirements, etc.)

## Decision

What is the change that we're proposing and/or doing?

State the decision clearly and concisely.

## Consequences

What becomes easier or harder as a result of this decision?

### Positive Consequences

- Benefit 1
- Benefit 2
- Benefit 3

### Negative Consequences

- Cost 1
- Cost 2
- Cost 3

### Neutral Consequences

- Impact 1
- Impact 2

## Alternatives Considered

What other options did we evaluate?

### Alternative 1: [Name]

**Pros:**
- Pro 1
- Pro 2

**Cons:**
- Con 1
- Con 2

**Why rejected:** Explanation

### Alternative 2: [Name]

**Pros:**
- Pro 1

**Cons:**
- Con 1

**Why rejected:** Explanation

## References

- Related issues: #123, #456
- Related PRs: #789
- External docs: [Link]
- Related ADRs: ADR 002, ADR 005

## Notes

Any additional context, follow-up actions, or future considerations.
```

## ADR Lifecycle

### Status Values

| Status | Meaning |
|--------|---------|
| **Proposed** | Under discussion, not yet decided |
| **Accepted** | Decision made and currently in effect |
| **Deprecated** | No longer recommended, but not replaced |
| **Superseded** | Replaced by another ADR (link to new ADR) |

### Lifecycle Flow

```
Proposed → Accepted → [Deprecated or Superseded]
            ↓
    (Implementation happens)
```

## Example ADR

### Good ADR Documentation

```markdown
# ADR 001: Use JWT for API Authentication

**Status:** Accepted

**Date:** 2024-01-15

**Deciders:** @owner, @copilot

## Context

Our API needs authentication to:
- Identify users making requests
- Protect sensitive endpoints
- Support mobile and web clients
- Enable stateless scaling

We evaluated session-based auth and token-based auth.

Constraints:
- Must work across multiple services (microservices architecture)
- Must support mobile apps (long-lived sessions)
- Must scale horizontally without shared state

## Decision

We will use JWT (JSON Web Tokens) for API authentication.

Implementation:
- Access tokens expire after 15 minutes
- Refresh tokens expire after 30 days
- Tokens signed with RS256 (asymmetric keys)
- Public key distributed to all services

## Consequences

### Positive Consequences

- **Stateless**: Servers don't need session storage
- **Scalable**: No shared state required
- **Cross-service**: Works across microservices
- **Mobile-friendly**: Long-lived refresh tokens

### Negative Consequences

- **Cannot revoke**: Tokens valid until expiration
- **Token size**: Larger than session IDs
- **Key management**: Must distribute public keys
- **Complexity**: More complex than sessions

### Neutral Consequences

- Need refresh token rotation strategy
- Need token blacklist for logout (mitigates revocation issue)

## Alternatives Considered

### Alternative 1: Session-based Authentication

**Pros:**
- Simple to implement
- Easy to revoke (delete session)
- Small session IDs

**Cons:**
- Requires shared session store (Redis)
- Harder to scale horizontally
- Doesn't work well across services
- Mobile apps would need constant re-auth

**Why rejected:** Doesn't meet stateless and cross-service requirements.

### Alternative 2: OAuth 2.0 with External Provider

**Pros:**
- Delegates authentication
- Industry standard
- Reduced responsibility

**Cons:**
- Dependency on external service
- Vendor lock-in
- Overkill for our use case
- Users want our own accounts

**Why rejected:** Too complex for current needs, want control over auth.

## References

- Implementation: PR #123
- JWT library: https://github.com/auth0/node-jsonwebtoken
- Token storage pattern: patterns/token-storage.md
- Related: ADR 002 (API Rate Limiting)

## Notes

- Consider implementing token blacklist for early revocation
- Monitor token size impact on request overhead
- Plan key rotation strategy (quarterly)
- Review decision in 6 months based on actual usage
```

This ADR is:
- ✅ Complete context provided
- ✅ Decision clearly stated
- ✅ Consequences documented (positive, negative, neutral)
- ✅ Alternatives explored and explained
- ✅ Links to related work

### Poor ADR Documentation

```markdown
# ADR 001: Use JWT

**Status:** Accepted

## Decision

We're using JWT for auth.

## Why

It's better than sessions.
```

This ADR is:
- ❌ Missing context
- ❌ No explanation of trade-offs
- ❌ No alternatives considered
- ❌ No references or links

## Numbering Scheme

ADRs are numbered sequentially:
- `001-first-decision.md`
- `002-second-decision.md`
- `003-third-decision.md`

**Never reuse numbers** - even for superseded decisions.

Gaps in numbering are OK (if decisions are deleted/moved).

## When to Write an ADR

Write an ADR when you:
- Choose between multiple technical approaches
- Make a decision with long-term impact
- Select tools, frameworks, or libraries
- Establish architectural patterns
- Make trade-offs that affect the system

**Not every decision needs an ADR** - focus on significant, hard-to-reverse choices.

### ADR vs Pattern vs Insight

| Type | Purpose | Example |
|------|---------|---------|
| **ADR** | Why we made a choice | "Why JWT over sessions" |
| **Pattern** | How to solve a problem | "How to implement retries" |
| **Insight** | What we learned | "API rate limits are stricter than documented" |

## Using ADRs

### For @copilot

When implementing:
1. **Read relevant ADRs** to understand context
2. **Follow established decisions** unless there's a good reason not to
3. **Propose new ADRs** for significant choices during implementation
4. **Link to ADRs** in PR descriptions to explain approach

### For Humans

When reviewing:
1. **Check alignment** with existing ADRs
2. **Challenge decisions** that contradict ADRs without explanation
3. **Suggest ADRs** for significant choices made during implementation
4. **Update ADRs** when better information emerges

## Superseding ADRs

When replacing a decision:

1. **Create new ADR** with updated decision
2. **Update old ADR** status to "Superseded by ADR NNN"
3. **Link between them** in both directions
4. **Don't delete** the old ADR - history matters

Example:

```markdown
# ADR 001: Use MySQL

**Status:** Superseded by ADR 015

**Date:** 2024-01-01

**Superseded Date:** 2024-06-15

[Original content...]

## Superseding Note

This decision was superseded by ADR 015 (Use PostgreSQL) due to:
- Need for better JSON support
- Performance issues with current queries
- Team expertise shifted to PostgreSQL
```

## Quality Checklist

Before creating an ADR, ensure:

- ✅ Decision is significant and has long-term impact
- ✅ Context is clearly explained
- ✅ Alternatives were actually considered
- ✅ Consequences are thoroughly documented
- ✅ References link to related work
- ✅ Status and date are included
- ✅ Title clearly states what was decided

## ADR Maintenance

### Regular Reviews

- **Monthly**: Review proposed ADRs, move to accepted or reject
- **Quarterly**: Review accepted ADRs for continued relevance
- **Annually**: Identify deprecated or superseded decisions

### Red Flags

Watch for:
- ADRs that are frequently violated in PRs
- ADRs that block progress without good reason
- ADRs based on outdated information
- ADRs that contradict each other

These signal a need to revisit and possibly supersede the ADR.

## Getting Started

To add your first ADR:

1. Identify a significant decision recently made
2. Copy the template above
3. Fill in all sections completely
4. Review with team
5. Assign sequential number
6. Add to this index

Even documenting decisions retroactively helps - capture the "why" before it's forgotten.

## ADR Categories

As ADRs accumulate, organize them:

### Architecture
- System structure
- Service boundaries
- Data flow

### Technology
- Language choices
- Framework selection
- Tool adoption

### Process
- Development workflow
- Deployment strategy
- Testing approach

### Security
- Authentication method
- Authorization model
- Data protection

## Benefits of ADRs

1. **Preserve context** - Future developers understand "why"
2. **Speed decisions** - Similar situations reference existing ADRs
3. **Reduce conflicts** - Explicit decisions prevent re-litigation
4. **Onboard faster** - New team members learn architectural reasoning
5. **Enable evolution** - Clear record of what changed and why

ADRs are documentation that actually gets used - they answer "why did we do it this way?"
