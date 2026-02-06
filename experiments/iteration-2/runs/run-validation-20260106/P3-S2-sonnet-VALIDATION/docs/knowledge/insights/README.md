# Insights

Learnings, observations, and discoveries made during @copilot execution and team experience.

## What are Insights?

Insights capture emergent knowledge that doesn't fit into patterns or decisions:
- Unexpected behaviors discovered
- Performance observations
- Tool-specific quirks
- Gotchas and edge cases
- Improvement opportunities

Insights are less structured than patterns but equally valuable - they're the raw observations that may later become patterns or inform decisions.

## Insight Index

*Insights will be added here as they are discovered and documented.*

**Current insights:** 0

---

## How to Document an Insight

### Template

Create file: `descriptive-name.md`

```markdown
# Insight: [Descriptive Title]

**Date:** YYYY-MM-DD

**Discovered by:** [@copilot | @username]

**Context:** [Where/when this was observed]

## Observation

What did you notice? What happened?

Be specific and factual.

## Context

What were you doing when you discovered this?

- Task: [What task/issue]
- Environment: [Dev/staging/production]
- Conditions: [Specific circumstances]

## Implications

What does this mean for future work?

### Immediate Impact

- Impact 1
- Impact 2

### Future Considerations

- Consideration 1
- Consideration 2

## Reproducibility

Can this be reproduced?

- [ ] Consistently reproducible
- [ ] Intermittent
- [ ] One-time occurrence
- [ ] Unknown

**Steps to reproduce:**
1. Step 1
2. Step 2
3. Step 3

## Related Work

- Related issues: #123
- Related PRs: #456
- Related patterns: pattern-name.md
- Related decisions: ADR 001

## Potential Actions

What might we do with this knowledge?

- [ ] Document as pattern
- [ ] Create ADR for related decision
- [ ] File issue to investigate
- [ ] Update documentation
- [ ] No action needed (just awareness)

## Notes

Any additional context or follow-up thoughts.
```

## Insight Categories

### Tool Insights
Observations about @copilot, GitHub, or other tools

**Examples:**
- "@copilot prefers explicit file paths over relative paths"
- "GitHub Actions has 6-hour timeout limit"
- "Workflow triggers don't fire for bot-created issues"

### Performance Insights
Observations about speed, efficiency, or resource usage

**Examples:**
- "API response time degrades with >1000 records"
- "Database query slow without index on user_id"
- "Batch processing 50% faster than individual calls"

### Behavior Insights
Unexpected or interesting system behaviors

**Examples:**
- "Rate limit resets at midnight UTC, not per-request"
- "Webhook retries use exponential backoff"
- "Cache invalidation doesn't cascade to dependencies"

### Process Insights
Learnings about workflow and development process

**Examples:**
- "PRs with >500 lines rarely get thorough review"
- "Tests fail intermittently on CI but not locally (timing issue)"
- "@copilot works better with acceptance criteria than implementation details"

## Example Insights

### Good Insight Documentation

```markdown
# Insight: @copilot Works Better with Outcome-Based Tasks

**Date:** 2024-01-20

**Discovered by:** @owner

**Context:** Comparing @copilot performance across 15 issues

## Observation

When issues describe WHAT to achieve (outcomes) rather than HOW to implement (steps), @copilot:
- Completes tasks 30% faster
- Produces more maintainable code
- Makes better architectural choices
- Updates documentation more thoroughly

## Context

Analyzed 15 @copilot tasks over 2 weeks:
- 8 issues with outcome-based descriptions
- 7 issues with step-by-step instructions

Task environment:
- Same repository
- Similar complexity levels
- Same @copilot version

## Implications

### Immediate Impact

- Update issue template to emphasize outcomes over steps
- Train team on writing outcome-based issues
- Review existing issues and refactor if needed

### Future Considerations

- This might apply to AI agents generally (test with other agents)
- Could create pattern for "outcome-based task specification"
- May want ADR on task specification format

## Reproducibility

- [x] Consistently reproducible
- [ ] Intermittent
- [ ] One-time occurrence
- [ ] Unknown

**Steps to reproduce:**
1. Create two similar issues
2. Issue A: "Add JWT authentication with login/logout endpoints and token validation"
3. Issue B: "Implement auth using JWT library, create auth.js file, add middleware"
4. Assign both to @copilot
5. Compare completion time and code quality

**Results across 8 pairs:**
- Outcome-based: avg 45 min, high quality
- Step-based: avg 65 min, more refactoring needed

## Related Work

- Issue template: .github/ISSUE_TEMPLATE/task.yml
- Related pattern: (to be created) outcome-based-specification.md
- Example issues: #45 (outcome), #47 (steps)

## Potential Actions

- [x] Update issue template to emphasize acceptance criteria
- [x] Document this insight
- [ ] Create pattern for outcome-based specifications
- [ ] Train team on this approach
- [ ] Create ADR for task specification format

## Notes

This aligns with best practices for human delegation too - specify the goal, not the method. Might be a general principle for AI-human collaboration.

Consider follow-up experiment with varying levels of detail:
- Minimal (just goal)
- Moderate (goal + constraints)
- Detailed (goal + constraints + suggested approach)
- Prescriptive (step-by-step instructions)

Hypothesis: Sweet spot is "Moderate" - clear goal with constraints but freedom in approach.
```

This insight is:
- ✅ Specific observation with data
- ✅ Clear context provided
- ✅ Reproducible with steps
- ✅ Actionable implications
- ✅ Links to related work

### Poor Insight Documentation

```markdown
# Insight: @copilot is smart

**Date:** 2024-01-20

## Observation

@copilot did a really good job on the last issue.

## Implications

We should use @copilot more.
```

This insight is:
- ❌ Too vague
- ❌ No specific observation
- ❌ No reproducibility information
- ❌ No actionable implications

## When to Document an Insight

Document insights when you discover:

### Definitely Document

- Unexpected tool behavior
- Performance patterns (good or bad)
- Edge cases or gotchas
- Workarounds for known issues
- Surprising successes or failures

### Consider Documenting

- One-time unusual occurrences
- Patterns that might be coincidence
- Observations needing more investigation

### Don't Document

- Expected behavior (goes in main docs)
- Obvious information
- Duplicate of existing insights

## Insight Lifecycle

```
Discovery → Documentation → Validation → Action

Action might be:
- Promote to Pattern
- Inform new ADR
- Update documentation
- File issue to investigate
- Archive as one-time event
```

## Using Insights

### For @copilot

When working on a task:
1. **Search insights** for related observations
2. **Avoid known pitfalls** documented in insights
3. **Apply learnings** from similar situations
4. **Document new insights** discovered during execution

### For Humans

When reviewing PRs:
1. **Check for patterns** in @copilot's work
2. **Extract insights** from interesting behaviors
3. **Connect insights** to broader patterns or decisions
4. **Promote insights** that warrant formal patterns or ADRs

## Insight Quality

### High-Quality Insights

- Specific and detailed
- Reproducible (or explains why not)
- Actionable implications
- Links to evidence (issues, PRs, logs)
- Suggests next steps

### Low-Quality Insights

- Vague observations
- No context
- Unverifiable
- No implications
- Dead ends

## Insight to Pattern Pipeline

Some insights evolve into patterns:

1. **Insight**: "API calls fail randomly, succeeding on retry"
2. **Investigation**: Confirm it's transient network issues
3. **Solution**: Implement retry logic with backoff
4. **Pattern**: "Error Handling with Exponential Backoff"

Track this evolution:
- Link insight to resulting pattern
- Keep insight for historical context
- Update insight with "Promoted to pattern/XYZ.md"

## Insight to ADR Pipeline

Some insights drive architectural decisions:

1. **Insight**: "Database queries slow with large result sets"
2. **Investigation**: Confirm it's pagination issue
3. **Evaluation**: Consider pagination vs. cursor-based vs. limit queries
4. **ADR**: "ADR 005: Use Cursor-Based Pagination for Large Lists"

Track this evolution:
- Link insight to resulting ADR
- Reference insight in ADR context
- Keep insight as evidence for decision

## Common Insight Types

### "It works better if..."

Observations about what produces better results.

**Example:** "It works better if we provide acceptance criteria in bullet points rather than paragraphs"

### "Watch out for..."

Gotchas and edge cases to avoid.

**Example:** "Watch out for rate limits - they reset daily at midnight UTC, not per-hour"

### "Surprisingly..."

Unexpected behaviors worth noting.

**Example:** "Surprisingly, parallel API calls are slower than sequential due to connection pooling limits"

### "We discovered..."

Novel findings that might inform future work.

**Example:** "We discovered that @copilot generates better tests when given example test cases"

## Insight Metrics

Track insight value:
- **Referenced**: How often linked in issues/PRs
- **Promoted**: Became pattern or ADR
- **Impact**: Prevented bugs or improved efficiency
- **Obsolete**: No longer relevant

Review insights quarterly and archive obsolete ones.

## Anti-Insights

Document what turned out to be wrong:

```markdown
# Anti-Insight: Database Indexing Always Speeds Queries

**Date:** 2024-01-15

**Discovered:** This is actually FALSE

## Original Observation

"Adding indexes always makes queries faster"

## Reality

Indexes have trade-offs:
- Slow down writes
- Take disk space
- Can slow queries if database chooses wrong index

## Correct Approach

Profile first, index based on data, not assumptions.

## Lessons

- Don't generalize from single examples
- Measure before optimizing
- Database performance is complex

## Related

- Pattern: performance-profiling.md
- ADR 008: Database Indexing Strategy
```

Anti-insights prevent others from making the same wrong assumptions.

## Contributing Insights

All team members should contribute:

### Sources of Insights

1. **During development** - Notice something unusual
2. **During debugging** - Discover root causes
3. **During review** - Spot patterns in PRs
4. **During retros** - Reflect on sprint learnings
5. **During incidents** - Learn from production issues

### Quality Over Quantity

Better to have 10 high-quality, actionable insights than 100 vague observations.

Focus on:
- Reproducible observations
- Clear implications
- Actionable next steps

## Getting Started

To add your first insight:

1. Notice something interesting or unexpected
2. Copy the template above
3. Document what you observed with specific details
4. Describe implications and potential actions
5. Link to related work (issues, PRs, commits)
6. Add to this index

Start capturing the "huh, that's interesting" moments - they're often the most valuable.

## Insight Value

The best insights are those that:

1. **Save time** - Prevent others from repeating investigations
2. **Prevent bugs** - Warn about gotchas before they cause problems
3. **Improve quality** - Share what produces better results
4. **Enable innovation** - Spark ideas for new patterns or decisions

An insight that saves the team 2 hours once has paid for itself. An insight that prevents a production bug is priceless.

Keep documenting. Keep learning. Keep sharing.
