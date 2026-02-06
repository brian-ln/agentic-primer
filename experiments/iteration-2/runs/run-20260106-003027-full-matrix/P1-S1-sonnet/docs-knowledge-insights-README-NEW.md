# Insights

Empirical learnings from production, testing, and operations.

## What Are Insights?

Insights capture knowledge gained from real-world experience with the system:
- Lessons learned from production incidents
- Performance optimization discoveries
- Testing strategies that work (or don't)
- Non-obvious behavior and gotchas
- Best practices discovered through trial and error

## Difference from Patterns and Decisions

- **Patterns:** Reusable solution templates ("how to implement")
- **Decisions:** Architectural choices and rationale ("why we chose this")
- **Insights:** Experiential knowledge and lessons learned ("what we discovered")

## When to Add an Insight

Create an insight when:
- You solve a difficult production issue
- You discover non-obvious behavior
- You learn from an incident or outage
- You find a better way to do something
- You identify a common pitfall or gotcha
- You validate or invalidate an assumption
- You discover performance characteristics

## Insight Template

Each insight should follow this structure:

```markdown
# Insight: [Insight Title]

**Category:** Insights
**Domain:** [Area, e.g., "Performance", "Security", "Testing"]
**Last Updated:** YYYY-MM-DD
**Source:** [How this was discovered, e.g., "Production Incident", "Performance Testing"]

## Summary

[One-paragraph overview of the insight]

## Key Findings

### [Finding 1]

**Observation:**
[What was observed]

**Data:**
[Quantitative evidence if available]

**Evidence:**
[How this was validated]

**Recommendation:**
[Actionable guidance]

**Example:**
[Code example or scenario]

[Repeat for each finding]

## Anti-Patterns to Avoid

[Common mistakes related to this insight]

## Related Knowledge

- **Patterns:** [Links to related patterns]
- **Decisions:** [Links to related ADRs]
- **Other Insights:** [Links to related insights]

## References

[External sources, documentation, tools used]

## Contributors

[People who contributed to this insight]

## Changelog

- **YYYY-MM-DD:** [Description of changes]
```

## Current Insights

### By Domain

**AI-Assisted Development:**
- [copilot-best-practices.md](copilot-best-practices.md) - GitHub Copilot usage patterns

**Performance:**
(Coming soon)

**Security:**
(Coming soon)

**Testing:**
(Coming soon)

**Operations:**
(Coming soon)

## Types of Insights

### Production Incidents

**What to capture:**
- Root cause analysis
- What went wrong and why
- How it was detected
- How it was fixed
- How to prevent recurrence

**Example:**
```markdown
# Insight: Database Connection Pool Exhaustion Under Load

## Summary
During Black Friday traffic spike, API became unresponsive due to connection
pool exhaustion caused by long-running queries blocking connections.

## Root Cause
- Default pool size: 10 connections
- Long-running analytics queries: 30-45 seconds
- High traffic: 100+ req/sec
- Result: Pool exhausted in <1 second

## Fix
- Increased pool size to 50
- Moved analytics to read replica
- Added connection pool monitoring

## Prevention
- Monitor pool utilization
- Separate read/write workloads
- Set query timeout limits
```

### Performance Discoveries

**What to capture:**
- Performance characteristics observed
- Optimization techniques that worked
- Benchmarks and measurements
- Trade-offs made

**Example:**
```markdown
# Insight: API Response Time Optimization

## Observation
GET /api/users endpoint had p95 response time of 800ms.

## Investigation
- 600ms: Database query (N+1 problem)
- 150ms: JSON serialization
- 50ms: Authentication

## Optimizations Applied
1. Fixed N+1 with eager loading: 600ms → 50ms
2. Added caching: 50ms → 10ms (cache hit)
3. Result: 800ms → 60ms (p95)

## Recommendation
Always use eager loading for relations in list endpoints.
```

### Testing Learnings

**What to capture:**
- Testing strategies that work
- Common testing pitfalls
- Test maintenance discoveries
- Coverage vs quality insights

### Debugging Techniques

**What to capture:**
- Effective debugging approaches
- Tools that helped
- Common debugging traps
- Investigation methodologies

### Tool and Library Experiences

**What to capture:**
- Gotchas with specific tools
- Library limitations discovered
- Integration challenges
- Workarounds developed

## How to Use Insights

### For Developers

1. **Before implementing:**
   - Check insights for related learnings
   - Avoid known pitfalls
   - Apply proven techniques

2. **When creating issues:**
   - Reference relevant insights
   - Help @copilot avoid known problems
   - Example: "Avoid patterns from insights/connection-pooling.md"

3. **After incidents:**
   - Document learnings as insights
   - Share with team
   - Update related patterns/decisions

### For @copilot

When processing issues, @copilot:
1. Reviews insights to avoid known pitfalls
2. Applies best practices discovered through experience
3. Incorporates performance learnings

### For Code Reviewers

1. **Check against known issues:**
   - Does code avoid documented pitfalls?
   - Are insights from similar work applied?
   - Should new insights be captured?

2. **Identify learning opportunities:**
   - If review reveals an insight, document it
   - Update existing insights with new data
   - Cross-reference related knowledge

## Contributing Insights

### From Production Incidents

1. **During incident:**
   - Document timeline and observations
   - Capture metrics and logs
   - Note what worked/didn't in debugging

2. **Post-incident:**
   - Conduct root cause analysis
   - Identify preventive measures
   - Create insight document
   - Update related patterns/decisions

3. **Follow-up:**
   - Implement preventive measures
   - Update monitoring and alerts
   - Share learning with team

### From Development Experience

1. **Notice a pattern:**
   - Solve same problem multiple times
   - Discover non-obvious behavior
   - Find better approach than documented

2. **Validate with data:**
   - Measure improvement if applicable
   - Test across different scenarios
   - Confirm with team members

3. **Document and share:**
   - Create insight document
   - Link to related knowledge
   - Present in team meeting

### From Performance Work

1. **Benchmark before and after:**
   - Record baseline metrics
   - Document optimization applied
   - Measure improvement
   - Note trade-offs

2. **Create actionable insight:**
   - What was the problem?
   - What did you try?
   - What worked and why?
   - How to apply elsewhere?

## Organizing Insights

### File Naming

- Use descriptive names: `copilot-best-practices.md`
- Include domain if helpful: `api-performance-optimization.md`
- Be specific: `postgres-connection-pooling.md` not `database.md`

### Categories

Group insights by domain:

**Development Practices:**
- AI-assisted development
- Code review learnings
- Refactoring discoveries

**Performance:**
- Database optimization
- API response time
- Caching strategies
- Query optimization

**Security:**
- Vulnerability discoveries
- Security testing
- Authentication gotchas
- Input validation

**Operations:**
- Deployment learnings
- Monitoring insights
- Incident response
- Scaling strategies

**Testing:**
- Test strategy effectiveness
- Coverage vs quality
- Integration testing
- E2E testing

**Tools and Libraries:**
- Framework gotchas
- Library limitations
- Tool effectiveness
- Integration challenges

## Best Practices

### Writing Good Insights

1. **Be specific:** Include concrete examples and data
2. **Be honest:** Document failures as well as successes
3. **Be actionable:** Provide clear recommendations
4. **Be quantitative:** Include metrics when possible
5. **Be current:** Update as you learn more

### Validating Insights

Before documenting:
- ✓ Is this based on real experience?
- ✓ Do you have data or evidence?
- ✓ Is this reproducible?
- ✓ Will this help others?
- ✓ Is the recommendation clear?

### Maintaining Insights

- **Regular reviews:** Quarterly check if insights are still valid
- **Update with new data:** Add findings as you learn more
- **Deprecate if outdated:** Mark old insights as obsolete
- **Cross-reference:** Link to related knowledge

## Metrics

Track insight effectiveness:

- **Incident recurrence:** Rate of similar incidents
- **Application rate:** How often insights are referenced
- **Prevention:** Issues avoided by applying insights
- **Time to resolution:** Faster debugging with insights

## Common Questions

**Q: How is an insight different from a pattern?**
A: Patterns are prescriptive (how to do X). Insights are descriptive (we learned Y).

**Q: Should every production incident become an insight?**
A: Only if there's a learning that will help prevent or debug similar issues in the future.

**Q: Can insights contradict decisions or patterns?**
A: Yes! Real-world experience may reveal that a decision or pattern needs updating. Document the insight and propose updating the decision/pattern.

**Q: How detailed should insights be?**
A: Detailed enough to be actionable. Include data, examples, and clear recommendations.

**Q: Who can contribute insights?**
A: Anyone on the team! Insights come from all roles: developers, QA, DevOps, product, etc.

## Examples

### Good Insight

```markdown
# Insight: Copilot Generates Better Code with Structured Issues

## Data
- Structured issues: 80% acceptance rate
- Free-form issues: 35% acceptance rate
- Sample size: 50 PRs over 3 months

## Recommendation
Always use issue templates when assigning to @copilot

## Evidence
[Details of study methodology and results]
```

### Poor Insight

```markdown
# Insight: Write Good Code

## Summary
Writing good code is important.

## Recommendation
Write code that is good.
```

(Too vague, no data, not actionable)

## See Also

- [Knowledge Base README](../README.md) - Overview of entire knowledge system
- [Patterns](../patterns/README.md) - Reusable solution templates
- [Decisions](../decisions/README.md) - Architectural choices
- [Post-Mortem Templates](https://github.com/dastergon/postmortem-templates) - Incident documentation
