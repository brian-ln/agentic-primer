# Insights

This directory contains lessons learned, gotchas, performance tips, and unexpected behaviors discovered during development.

## What is an Insight?

An insight is practical knowledge gained from experience:
- Unexpected behaviors or quirks
- Performance discoveries
- Integration challenges and solutions
- "I wish I'd known this earlier" moments
- Tips and tricks that save time

Insights capture the tribal knowledge that's usually shared verbally or learned the hard way.

## When to Document an Insight

Record an insight when you:
- Discover unexpected behavior in a library or framework
- Find a performance optimization that makes a significant difference
- Encounter a subtle bug that took time to debug
- Learn something that would help others avoid the same issue
- Solve an integration problem with an external service
- Find a workaround for a known limitation

## Insight Template

Use this template when capturing insights:

```markdown
# Insight: {Short Descriptive Title}

**Date**: YYYY-MM-DD
**Author**: @username
**Category**: Performance | Bug | Integration | Configuration | Best Practice | Other
**Severity**: Info | Warning | Critical
**Status**: Active | Resolved | Obsolete

## Summary

One-sentence summary of the insight.

Example: "Database connection pooling reduced API response time by 80% under high load."

## Context

What situation led to this discovery?

- What were you trying to do?
- What system/component was involved?
- What version of the software/library?

## The Discovery

What did you learn? What was unexpected?

Describe:
- What you expected to happen
- What actually happened
- Why the difference matters

## Details

Provide technical details:

### Reproduction (if applicable)

Steps to reproduce the behavior:
1. Step 1
2. Step 2
3. Step 3

### Example Code

```language
// Code demonstrating the insight
// Including before/after if applicable
```

### Metrics (if applicable)

If performance-related, include measurements:
- Before: [measurement]
- After: [measurement]
- Improvement: [percentage or absolute]

## Impact

Who/what is affected?

- **Scope**: Specific component | Entire system | All environments | Production only
- **Frequency**: Common | Occasional | Rare
- **Impact Level**: High | Medium | Low

## Recommendation

What should developers do with this information?

- ✅ **Do**: Recommended action
- ❌ **Don't**: What to avoid
- ⚠️ **Watch out for**: Related gotchas

### Action Items

If this insight requires changes:
- [ ] Action 1
- [ ] Action 2
- [ ] Action 3

## Root Cause

What's the underlying reason for this behavior?

Explain the "why" behind the insight. Link to:
- Documentation that explains it
- Source code if you've investigated
- Related issues or bug reports

## Workaround / Solution

How to handle this issue?

### Temporary Workaround
(If a permanent fix isn't available)

```language
// Workaround code
```

### Permanent Solution
(If one exists)

```language
// Solution code
```

## Related Knowledge

Link to related information:
- Related patterns: [Pattern Name](../patterns/pattern.md)
- Related decisions: [ADR Name](../decisions/adr.md)
- Related insights: [Insight Name](./other-insight.md)
- Issues/PRs: #123, #456
- External resources: [Links to documentation, blog posts, etc.]

## History

Track updates to this insight:
- **YYYY-MM-DD**: Initial discovery
- **YYYY-MM-DD**: Added workaround
- **YYYY-MM-DD**: Updated with permanent solution
- **YYYY-MM-DD**: Marked as resolved/obsolete
```

## Naming Convention

Name insight files as: `{date}-{topic}.md`

Examples:
- `2026-01-08-api-rate-limiting.md`
- `2026-01-15-postgres-connection-pool.md`
- `2026-02-03-safari-css-quirk.md`

Use ISO date format (YYYY-MM-DD) for easy chronological sorting.

## Insight Categories

### Performance
- Optimization discoveries
- Bottleneck identification
- Caching strategies
- Resource usage patterns

### Bug
- Subtle bugs and their solutions
- Edge cases
- Error patterns
- Debugging techniques

### Integration
- External service quirks
- API behavior
- Third-party library issues
- Cross-system challenges

### Configuration
- Settings that matter
- Environment-specific behavior
- Deployment gotchas
- Infrastructure quirks

### Best Practice
- Lessons learned
- What works well
- Pitfalls to avoid
- Tips and tricks

### Security
- Security vulnerabilities discovered
- Security best practices
- Authentication/authorization gotchas
- Data protection insights

## Insight Severity

**Info**: Good to know, but not critical
- Tips and tricks
- Minor optimizations
- Interesting behavior

**Warning**: Should be aware of this
- Performance concerns
- Known limitations
- Workarounds needed

**Critical**: Must know to avoid problems
- Data loss risks
- Security vulnerabilities
- System failures
- Breaking changes

## Insight Lifecycle

### Statuses

- **Active**: Current and relevant
- **Resolved**: Issue has been fixed upstream
- **Obsolete**: No longer applicable (e.g., library updated, component removed)

### Updating Insights

When status changes:
1. Update the "Status" field
2. Add note to "History" section
3. If resolved, note the solution
4. If obsolete, explain why

Don't delete insights; they provide historical context.

## Writing Effective Insights

### Do:
- ✅ Be specific and concrete
- ✅ Include examples and code
- ✅ Provide context and impact
- ✅ Suggest actionable recommendations
- ✅ Link to related resources
- ✅ Keep it concise but complete

### Don't:
- ❌ Be vague or abstract
- ❌ Skip the "why" behind the insight
- ❌ Forget to include reproduction steps
- ❌ Leave out version information
- ❌ Write without testing your claims

## Examples of Good Insights

### Example 1: Performance Optimization

**Title**: "Database Connection Pooling Reduces API Latency by 80%"

Shows:
- Specific metrics (80% improvement)
- Before/after measurements
- Code example of implementation
- When to apply this optimization
- Configuration recommendations

### Example 2: Library Quirk

**Title**: "Library X Memory Leak with Config Option Y"

Shows:
- Clear reproduction steps
- Affected versions
- Workaround code
- Link to upstream issue
- When it's expected to be fixed

### Example 3: Browser Compatibility

**Title**: "CSS Grid Not Supported in Safari < 14"

Shows:
- Specific version information
- Impact (which features affected)
- Fallback approach
- Browser testing recommendations
- Link to compatibility table

### Example 4: Integration Discovery

**Title**: "Payment API Rate Limit Resets at Midnight UTC"

Shows:
- Unexpected behavior
- Impact on retry logic
- How we discovered it
- Code adjustment needed
- Monitoring recommendation

## Maintaining Insights

### Regular Review

Quarterly review checklist:
- [ ] Are active insights still relevant?
- [ ] Have any issues been resolved upstream?
- [ ] Do version numbers need updating?
- [ ] Are workarounds still necessary?
- [ ] Should any be marked obsolete?

### Triaging New Insights

When someone discovers something:
1. Quick Slack message → If impacts others immediately
2. Insight document → If useful long-term
3. Both → If critical knowledge

### Cross-Referencing

When you create/update:
- Patterns: Check if insights influenced them
- Decisions: Note insights that informed choice
- Code: Link insights in comments for context

## Finding Insights

### By Date
Files are named chronologically (YYYY-MM-DD prefix).

### By Category
Use the "Category" field to filter.

### By Severity
Check "Severity" field for critical issues.

### By Status
- Active: Current and applicable
- Resolved: Fixed but documented
- Obsolete: Historical context only

## Benefits of Capturing Insights

1. **Avoid Repeated Mistakes**: Learn from experience once, apply forever
2. **Faster Debugging**: Similar issues? Check insights first
3. **Onboarding**: New team members learn gotchas upfront
4. **Knowledge Retention**: Tribal knowledge becomes explicit
5. **Better Decisions**: Inform future choices with real experience

## Insight vs Pattern vs Decision

**Insight**: "Here's something unexpected we learned"
- Discovery-focused
- Specific finding
- Usually emerged from real work

**Pattern**: "Here's a proven solution to a recurring problem"
- Solution-focused
- Reusable approach
- Intentionally designed

**Decision**: "Here's a choice we made and why"
- Choice-focused
- Options evaluated
- Rationale documented

Some insights become patterns once we formalize the solution. Some inform decisions by providing data.

## Quick Capture Format

In a hurry? Minimum viable insight:

```markdown
# Insight: {Title}

**Date**: YYYY-MM-DD
**Author**: @username

## What We Learned
[One paragraph explaining the insight]

## Why It Matters
[One paragraph on impact]

## What to Do
[Bullet points of recommendations]
```

Enhance it later with full template details.

## Getting Help

- Not sure if something is insight-worthy? Ask: "Would this help someone else?"
- Need help writing? Start with the quick format above
- Want feedback? Create PR and tag relevant team members
- Have questions? Open issue with "knowledge-base" label

## Contributing

Insights are most valuable when fresh. Document them as soon as you discover them, while details are clear.

**Remember**: Your "obvious" discovery might be someone else's hour-saving insight.

## Resources

Learn more about knowledge capture:
- [The Pragmatic Programmer: Tips & Tricks](https://pragprog.com/)
- [Software Development Lessons Learned](https://dev.to/t/lessons)
- [Post-Incident Reviews](https://sre.google/sre-book/postmortem-culture/)

---

**Pro tip**: Add a calendar reminder to review insights quarterly. Active maintenance keeps the knowledge base healthy.
