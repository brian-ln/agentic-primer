# Insights and Learnings

This directory captures lessons learned, observations, and reflections from development experience.

## What is an Insight?

An insight is a valuable lesson or observation discovered through experience. Unlike patterns (reusable solutions) or decisions (architectural choices), insights are:

- **Experiential:** Learned by doing, often the hard way
- **Contextual:** Specific to this project's journey
- **Reflective:** "I wish I'd known this earlier" moments
- **Educational:** Help others avoid mistakes or discover opportunities

## When to Add an Insight

Capture an insight when you:

- Complete a retrospective or post-mortem
- Discover a non-obvious truth about the codebase
- Learn something surprising or counterintuitive
- Want to preserve context that isn't obvious from code
- Have a "next time, we should..." realization

## Insight Template

```markdown
# Insight: [Brief Title]

**Date:** YYYY-MM-DD

**Author:** [Name/Agent]

**Context:** [What was happening when this insight emerged?]

## The Insight

[What did you learn? State it clearly and concisely.]

## Background

[What led to this realization? What was the situation?]

## Evidence

[What concrete evidence supports this insight?]

- Metrics, logs, or data
- Specific incidents or examples
- Team feedback or observations

## Implications

[What does this insight mean for the project?]

### What We Should Do

- Actionable recommendations
- Process changes to consider
- Patterns to adopt or avoid

### What We Should Stop Doing

- Practices that don't work
- Assumptions that proved false
- Anti-patterns to avoid

## Related

- [Links to related patterns, decisions, or other insights]
- [References to issues, PRs, or external resources]

## Metadata

- **Created:** YYYY-MM-DD
- **Last Updated:** YYYY-MM-DD
- **Status:** Active | Resolved | Obsolete
```

## Naming Convention

Use this format: `YYYYMMDD-brief-title.md`

Examples:
- `20260106-test-coverage-alone-insufficient.md`
- `20260106-async-debugging-requires-structured-logging.md`
- `20260106-migrations-need-rollback-plans.md`

## Current Insights

### Development Process

- *(No insights recorded yet - this is a new knowledge base)*

### Technical Lessons

- *(To be added as team discovers insights)*

### Team Learnings

- *(To be added as team discovers insights)*

## Insight Categories

Common categories that may emerge:

- **Performance:** Lessons about optimization and scalability
- **Debugging:** Insights from troubleshooting difficult issues
- **Process:** Learnings about development workflow and practices
- **Architecture:** Discoveries about system design and structure
- **Collaboration:** Insights about team communication and coordination
- **Tooling:** Lessons about tools, libraries, and frameworks
- **Operations:** Learnings from deployment, monitoring, and incidents

Tag insights with categories as they're added.

## Contributing an Insight

1. **Reflect** on what you learned
2. **Write the insight** using the template above
3. **Include evidence** (metrics, examples, incidents)
4. **Propose actions** that follow from the insight
5. **Open a PR** for discussion
6. **Link it** from this README

## Quality Guidelines

Good insights should:

- Be **specific** to real experiences in this project
- Include **evidence** rather than just opinions
- Suggest **actionable** changes or improvements
- Acknowledge **context** (when does this apply?)
- Be **honest** about what didn't work

Avoid:

- Generic advice copied from the internet
- Insights based on hearsay rather than experience
- Blame or finger-pointing
- Overgeneralizing from single incidents

## Insight Lifecycle

- **Active:** Relevant to current development
- **Resolved:** Issue addressed, kept for historical context
- **Obsolete:** No longer applicable (explain why in the document)

Update the status as the project evolves.

## Examples of Good Insights

### Example 1: Specific and Actionable

> **Insight:** Our integration tests fail intermittently because they share database state.
>
> **Evidence:** 37% of test failures in December were due to race conditions. Logs show table locks and constraint violations.
>
> **Action:** Implement test database isolation using transactions or separate schemas per test.

### Example 2: Counterintuitive Learning

> **Insight:** Adding more caching actually slowed down our API.
>
> **Evidence:** P99 latency increased from 250ms to 890ms after Redis caching was added. Cache misses trigger two network calls instead of one.
>
> **Action:** Remove caching for endpoints with high cache miss rates (>70%). Focus caching on read-heavy, stable data.

### Example 3: Process Improvement

> **Insight:** Post-mortems work better when written within 24 hours.
>
> **Evidence:** Incidents from >3 days ago had vague timelines and "I don't remember" responses. Recent incidents had detailed, accurate timelines.
>
> **Action:** Schedule post-mortem meeting within 24 hours of incident resolution.

## Integration with @copilot

AI agents can:

- **Propose insights** from patterns they observe in logs or metrics
- **Analyze incidents** and suggest learnings
- **Reference insights** when making recommendations
- **Update insights** with new evidence or outcomes

Encourage agents to capture their observations as insights.

## Post-Mortem to Insight

After an incident or major event:

1. **Conduct post-mortem** (what happened, why, what to do differently)
2. **Extract key learnings** from the discussion
3. **Create insights** for each significant lesson
4. **Track action items** in the issue tracker
5. **Update insights** when actions are completed

## Retrospective to Insight

After a sprint or milestone:

1. **Run retrospective** (what went well, what didn't, what to try)
2. **Identify themes** across team feedback
3. **Create insights** for important patterns
4. **Propose experiments** to test improvements
5. **Revisit insights** in future retros to see if changes helped

## Metadata

- **Created:** 2026-01-06
- **Last Updated:** 2026-01-06
- **Status:** Active
