# Insights

Learnings captured from AI agent execution, including successes, failures, and observations.

## What is an Insight?

An insight is a documented learning from actual execution. Unlike patterns (which are prescriptive) or decisions (which are choices), insights are observations that may inform future work.

## Insight Categories

- **Success**: What worked well and why
- **Failure**: What failed and root cause analysis
- **Observation**: Neutral observations about agent behavior
- **Improvement**: Suggested improvements based on experience

## Insight Template

```markdown
# Insight: Title

## Category
Success | Failure | Observation | Improvement

## Date
YYYY-MM-DD

## Context
What was the agent trying to do?

## Observation
What happened?

## Analysis
Why did this happen?

## Recommendation
What should we do differently?

## Related Issues/PRs
- Issue #N
- PR #M
```

## Current Insights

| ID | Title | Category |
|----|-------|----------|
| 001 | Bootstrap Learnings | Observation |

## Adding New Insights

1. Create a new file: `NNN-insight-title.md`
2. Use the template above
3. Add an entry to the table in this README
4. Link to relevant issues or PRs

## Auto-Generated Insights

The GitHub workflow automatically captures insights from:

- Failed workflow runs (root cause analysis)
- Successful PRs (pattern detection)
- Review comments (feedback incorporation)

These auto-generated insights are marked with `[AUTO]` in their title.
