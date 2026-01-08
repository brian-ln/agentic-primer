# Decisions

Architecture Decision Records (ADRs) documenting significant technical decisions and their rationale.

## What Are ADRs?

Architecture Decision Records capture important decisions about the software architecture and design. They document:
- **What** decision was made
- **Why** it was made (context and rationale)
- **When** it was made
- **Who** made it
- **What** the consequences are

## Why Document Decisions?

1. **Historical context:** Future developers understand why things are the way they are
2. **Prevent revisiting:** Don't re-debate settled decisions
3. **Onboarding:** New team members learn the reasoning behind choices
4. **Change management:** Know what to reconsider if circumstances change
5. **Alignment:** Ensure team shares understanding of architectural direction

## When to Create an ADR

Create an ADR when:
- Making a significant architectural choice
- Selecting between competing alternatives (frameworks, databases, patterns)
- Establishing team standards or conventions
- Making decisions with long-term implications
- Future team members will ask "why did we do it this way?"

## ADR Template

Each ADR should follow this structure:

```markdown
# ADR [Number]: [Title]

**Status:** [Proposed | Accepted | Deprecated | Superseded]
**Date:** YYYY-MM-DD
**Decision Makers:** [Who was involved]
**Category:** Architecture Decision Record

## Context

[Describe the situation, problem, or need that prompted this decision]

## Decision

We will [clear statement of the decision].

## Alternatives Considered

### Option 1: [Name]
**Pros:**
- [List advantages]

**Cons:**
- [List disadvantages]

### Option 2: [Name]
**Pros:**
- [List advantages]

**Cons:**
- [List disadvantages]

[Add more options as needed]

## Rationale

[Explain why this decision was made over alternatives]

## Consequences

### Positive Consequences
[Benefits of this decision]

### Negative Consequences
[Drawbacks or limitations]

### Mitigation Strategies
[How to address negative consequences]

## Implementation Guidelines

[Practical guidance for implementing this decision]

## Review and Reassessment

[When and under what conditions this decision should be reconsidered]

## Related Decisions

[Links to related ADRs]

## Related Patterns

[Links to patterns that implement this decision]

## References

[External sources, documentation, articles]

## Approval

- **Proposed by:** [Name]
- **Reviewed by:** [Names]
- **Approved by:** [Name/Role]
- **Date:** YYYY-MM-DD

## Changelog

- **YYYY-MM-DD:** [Description of changes]
```

## Current Decisions

### ADR Index

1. [001-use-rest-api.md](001-use-rest-api.md) - Use REST API instead of GraphQL

### By Category

**API Architecture:**
- ADR 001: REST vs GraphQL

**Data Storage:**
(Coming soon)

**Authentication:**
(Coming soon)

**Deployment:**
(Coming soon)

## ADR Lifecycle

### Statuses

- **Proposed:** Decision under consideration
- **Accepted:** Decision approved and in effect
- **Deprecated:** Decision no longer recommended but not yet replaced
- **Superseded:** Replaced by a newer decision (link to replacement)

### Workflow

1. **Proposal:**
   - Identify decision to be made
   - Create ADR with "Proposed" status
   - Document context and alternatives
   - Share with team for feedback

2. **Review:**
   - Team discusses alternatives
   - Evaluates trade-offs
   - Considers long-term implications
   - Reaches consensus or escalates

3. **Acceptance:**
   - Update status to "Accepted"
   - Record approval and date
   - Communicate decision to team
   - Update related documentation

4. **Implementation:**
   - Follow decision in new code
   - Create patterns that implement decision
   - Update coding standards if needed
   - Monitor for issues

5. **Review:**
   - Periodically reassess decision
   - Update ADR if circumstances change
   - Deprecate if no longer valid
   - Supersede if replaced

## How to Use ADRs

### For Developers

1. **Before making architectural decisions:**
   - Check if an ADR already exists
   - Follow existing decisions
   - Propose new ADR if needed

2. **When creating issues for @copilot:**
   - Reference relevant ADRs
   - Example: "Follow decisions/001-use-rest-api.md for API design"

3. **During code review:**
   - Verify code aligns with ADRs
   - Question deviations from documented decisions
   - Suggest updating ADRs if circumstances changed

### For @copilot

When processing issues, @copilot:
1. Reviews relevant ADRs for context
2. Generates code consistent with decisions
3. Applies architectural principles documented in ADRs

### For Code Reviewers

1. **Check ADR alignment:**
   - Does code follow architectural decisions?
   - Are deviations justified?
   - Should an ADR be updated?

2. **Identify missing ADRs:**
   - Notice undocumented decisions
   - Suggest creating ADRs for significant choices
   - Help maintain decision documentation

## Numbering ADRs

- Use sequential numbering: 001, 002, 003, etc.
- Don't reuse numbers (even if an ADR is superseded)
- Pad with zeros: 001 not 1 (for sorting)
- Include number in filename: `001-use-rest-api.md`

## Writing Good ADRs

### Best Practices

1. **Be clear and concise:**
   - State the decision upfront
   - Explain "why" not just "what"
   - Use simple language

2. **Document alternatives:**
   - Show what was considered
   - Explain why alternatives were rejected
   - Be fair to all options

3. **Be honest about trade-offs:**
   - Every decision has pros and cons
   - Document negative consequences
   - Provide mitigation strategies

4. **Include context:**
   - What was the situation?
   - What constraints existed?
   - What were the requirements?

5. **Link to related knowledge:**
   - Reference related ADRs
   - Link to implementing patterns
   - Connect to relevant insights

6. **Plan for review:**
   - Specify when to reassess
   - List conditions that would invalidate the decision
   - Make it easy to revisit

### Common Mistakes

❌ **Don't:**
- Make decisions without documenting them
- Write ADRs after implementation (capture decisions as made)
- Hide negative consequences
- Ignore alternatives
- Use jargon without explanation
- Make it too long (aim for 1-2 pages)

✅ **Do:**
- Write ADRs when decisions are made
- Be objective about trade-offs
- Keep it scannable with clear headings
- Update ADRs when circumstances change
- Link to supporting documentation

## ADR Categories

Organize ADRs by domain:

### Technology Selection
- Languages and frameworks
- Libraries and dependencies
- Tools and platforms

### Architecture
- System architecture patterns
- Component structure
- Integration approaches

### Data
- Database choices
- Data modeling
- Storage strategies

### Security
- Authentication approaches
- Authorization models
- Encryption standards

### Operations
- Deployment strategies
- Monitoring and logging
- Infrastructure choices

### Process
- Development workflows
- Testing strategies
- Release processes

## Reviewing and Updating ADRs

### When to Review

- **Quarterly:** Check if circumstances have changed
- **During incidents:** Assess if decisions contributed to issues
- **Before major projects:** Verify decisions still apply
- **When new technology emerges:** Reevaluate choices

### How to Update

1. **Minor updates:**
   - Fix typos or clarify wording
   - Add references or examples
   - Update changelog

2. **Significant changes:**
   - Add "Revisited on [date]" section
   - Document changed circumstances
   - Update rationale if needed

3. **Deprecation:**
   - Change status to "Deprecated"
   - Explain why decision is deprecated
   - Link to replacement if available
   - Provide migration guidance

4. **Superseding:**
   - Create new ADR
   - Reference old ADR
   - Explain what changed and why
   - Update old ADR status to "Superseded by ADR XXX"

## Examples

### Good ADR Title
- "Use PostgreSQL for primary database"
- "Implement authentication with JWT tokens"
- "Deploy on Kubernetes instead of EC2"

### Poor ADR Title
- "Database choice" (not specific)
- "Use the best approach" (not a decision)
- "Technical decision" (too vague)

### Good Decision Statement
> "We will use REST API with JSON instead of GraphQL for our public API."

### Poor Decision Statement
> "We will use a good API approach that works well."

## Metrics

Track ADR effectiveness:

- **Coverage:** % of architectural decisions documented
- **Adherence:** % of code following documented decisions
- **Age:** Time since last review of each ADR
- **Supersession rate:** How often decisions are revised

## Questions?

**Q: How detailed should an ADR be?**
A: Detailed enough that someone unfamiliar with the decision can understand the reasoning. Aim for 1-2 pages.

**Q: Can we disagree with an ADR?**
A: Yes! Propose updating it with new information or changing circumstances. Bring evidence and rationale.

**Q: What if we violate an ADR?**
A: Either update the code to follow the ADR, or update/supersede the ADR if circumstances changed.

**Q: How long do ADRs remain valid?**
A: Until circumstances change significantly. Review periodically and update as needed.

**Q: Should we create ADRs for small decisions?**
A: No. ADRs are for significant architectural decisions. Small tactical choices don't need ADRs.

## See Also

- [Knowledge Base README](../README.md) - Overview of entire knowledge system
- [Patterns](../patterns/README.md) - How to implement decisions
- [Insights](../insights/README.md) - What we've learned from decisions
- [ADR Tools](https://adr.github.io/) - Community resources for ADRs
