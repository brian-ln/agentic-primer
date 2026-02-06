# Insights

Learnings from completed work, discoveries, gotchas, and non-obvious knowledge.

## What Goes Here?

Document insights when you:
- Complete a challenging or educational task
- Discover a non-obvious behavior or edge case
- Learn from a mistake or debugging session
- Find a performance issue or optimization
- Encounter a gotcha worth sharing
- Discover useful tool/technique/approach

## Insight Template

```markdown
# Insight: Brief Title

## Date
YYYY-MM-DD

## Context
What were you working on? What were you trying to accomplish?

## Discovery
What did you learn? What was the insight?

## Why It Matters
Why is this knowledge valuable? Who benefits from knowing this?

## Example
Concrete example demonstrating the insight (code, command, scenario)

## Action Items
- What should we do differently based on this insight?
- What patterns/decisions does this inform?

## Related
- Links to issues, PRs, patterns, decisions
- Related insights
```

## Types of Insights

### Success Insights
"We tried X and it worked great because Y"
- What worked well
- Why it succeeded
- When to use this approach again

### Failure Insights
"We tried X and it failed because Y"
- What didn't work
- Why it failed
- How we fixed it
- What to do instead

### Discovery Insights
"We learned that X behaves as Y in situation Z"
- Non-obvious behavior
- Edge cases
- Tool capabilities
- Performance characteristics

### Process Insights
"We found that doing X before Y saves time/effort"
- Workflow improvements
- Efficiency gains
- Best practices

## Examples of Good Insights

- "GitHub Actions cache keys must be branch-specific"
- "Always validate input before database writes to avoid SQL injection"
- "Using batch operations reduced API calls by 90%"
- "Error messages with context cut debugging time in half"
- "Running tests in parallel saved 3 minutes per CI run"
- "Lazy loading images improved page load from 3s to 0.8s"

## Organization

Each insight:
- Separate markdown file
- Date-prefixed filename: `YYYY-MM-DD-insight-title.md`
- Clear, descriptive title
- Update INDEX.md when adding new insights

## Cross-References

Link insights to:
- Patterns created based on this insight
- Decisions informed by this insight
- Related insights (similar discoveries)
- Issues/PRs where insight was discovered

## From Insight to Action

Good insights lead to:
1. **New patterns**: If insight reveals a reusable approach
2. **New decisions**: If insight changes our technical direction
3. **Documentation updates**: If insight reveals a gap
4. **Code improvements**: If insight shows a better way

## Avoiding Insight Bloat

Don't document:
- Obvious behaviors (what the documentation already says)
- One-time occurrences with no general lesson
- Insights that belong in code comments
- Information better suited for patterns or decisions

## Getting Started

1. Keep a "learned something" mindset while working
2. When you debug something non-trivial, document it
3. When you discover a gotcha, share it
4. When you optimize something, explain what worked
5. Review insights periodically for pattern/decision promotion
