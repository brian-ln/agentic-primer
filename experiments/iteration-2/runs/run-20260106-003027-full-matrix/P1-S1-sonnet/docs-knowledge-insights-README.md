# Engineering Insights

This directory contains empirical learnings, best practices, and post-mortems from real-world experience with this project.

## Purpose

Insights capture **what we've learned**:
- **Lessons Learned** - What worked, what didn't, why
- **Best Practices** - Proven approaches from production
- **Performance Findings** - Optimization learnings
- **Incident Post-Mortems** - Root cause analysis
- **Tool Evaluations** - Experience with libraries, services, tools

## Insights vs Patterns vs Decisions

| Type | Focus | Example |
|------|-------|---------|
| **Pattern** | Reusable solution | "Standard error response format" |
| **Decision** | Why we chose X over Y | "Use REST API instead of GraphQL" |
| **Insight** | What we learned doing it | "Copilot works better with structured issues" |

Insights are **empirical and contextual** - they're based on our specific experience and may not apply universally.

## Format

Each insight document should include:

```markdown
# Insight: {Title}

**Category:** {Performance | Process | Tooling | Incident | Testing}
**Date:** YYYY-MM-DD
**Contributors:** @username1, @username2

## Summary
One-paragraph summary of the insight.

## Context
What was the situation? What were we trying to do?

## What We Learned
The key learning or discovery.

## Evidence
Data, metrics, or observations supporting the insight.

## Recommendations
What should we do based on this learning?

## Related
- Links to patterns, decisions, code, or docs
```

## Insight Categories

### Performance
- Database query optimizations
- Caching strategies that worked/failed
- API response time improvements
- Frontend bundle size reductions

### Process
- Development workflow improvements
- Code review learnings
- Deployment process changes
- Team collaboration insights

### Tooling
- Library/framework evaluations
- CI/CD pipeline improvements
- Development tool experiences
- Monitoring and observability

### Incidents
- Post-mortems from outages
- Security incident learnings
- Data loss prevention
- Recovery procedures

### Testing
- Test strategy effectiveness
- Flaky test resolutions
- Coverage improvements
- Testing tool evaluations

## How to Use

### Writing a New Insight

1. **After** significant events: outages, launches, experiments
2. **During** retrospectives or post-mortems
3. **When** discovering non-obvious learnings
4. **Before** forgetting the details!

**Template:** Copy format above, fill in all sections

### Reading Insights

- Browse before starting similar work
- Reference in ADRs when making decisions
- Share with new team members during onboarding
- Include in post-incident reviews

### For GitHub Copilot

Link insights in issue descriptions:
```markdown
Knowledge References:
- docs/knowledge/insights/copilot-best-practices.md
```

Copilot will consider these learnings when implementing solutions.

## Insight Index

| Date | Title | Category | Contributors |
|------|-------|----------|--------------|
| 2026-01-06 | [Copilot Best Practices](./copilot-best-practices.md) | Tooling | @copilot, @owner |

---

**Contributing:**
- Insights should be **specific and actionable**
- Include **evidence** (metrics, logs, screenshots)
- Update within **72 hours** of incident/learning
- Review quarterly to archive outdated insights

**Last Updated:** 2026-01-06
**Maintainer:** @owner
