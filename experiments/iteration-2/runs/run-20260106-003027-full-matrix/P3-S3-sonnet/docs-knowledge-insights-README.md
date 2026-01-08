# Execution Insights

Meta-learnings extracted from agent execution logs and development process.

## Purpose

Insights capture **how we work** in this project:
- What approaches are effective
- What practices improve quality
- What patterns lead to problems
- What processes work well

Unlike patterns (what to build) or decisions (why we chose), insights describe **how we learned**.

## What Goes Here

### Good Insight Entries

✅ **Process learnings**: "TDD reduces PR review cycles by 40%"
✅ **Effectiveness metrics**: "Breaking PRs into <300 LOC improves review quality"
✅ **Tool discoveries**: "Using TypeScript strict mode caught 23% more bugs"
✅ **Workflow improvements**: "Pre-commit hooks reduced CI failures by 60%"
✅ **Team practices**: "Pairing on complex features halves debugging time"

### Not Insight Entries

❌ **Code patterns**: Put in `patterns/` instead
❌ **Architecture choices**: Put in `decisions/` instead
❌ **Individual bug fixes**: Normal PR documentation
❌ **Vague observations**: Must have specific learnings

## Insight Template

```markdown
# Insight: [Clear, Specific Title]

**Category**: [Process / Quality / Performance / Team / Tools]
**Date**: YYYY-MM-DD
**Source**: [PR #XXX / Sprint Retro / Manual Observation]
**Confidence**: [High / Medium / Low]

## Context

[Describe the situation where this insight was discovered]

### Time Period
[When this observation was made]

### Sample Size
[How many instances observed]

## Observation

[What was noticed or measured]

### Specific Finding
[Detailed description of what happened]

### Data
[Quantitative metrics if available]
- Metric 1: [value]
- Metric 2: [value]

## Analysis

### Why This Happened
[Explain the underlying cause]

### Contributing Factors
- Factor 1
- Factor 2

### Exceptions
[Cases where this doesn't apply]

## Application

### How to Use This Learning

1. [Actionable step 1]
2. [Actionable step 2]
3. [Actionable step 3]

### When to Apply
- Use when [situation 1]
- Use when [situation 2]
- Don't use when [exception]

### Expected Impact
[What improvement to expect]

## Evidence

### Examples
- PR #XXX: [How this applied]
- PR #YYY: [Another example]

### Metrics
[Before/after data if available]

## Related Insights

- [Link to related insight 1]
- [Link to related insight 2]

## Review

**Last Validated**: YYYY-MM-DD
**Still Relevant**: [Yes / Needs Update / Deprecated]
**Updates Since Creation**: [List changes]

---
_Extracted from [source]_
```

## Categories

### Process Insights
- Development workflow effectiveness
- Review process improvements
- Release process learnings
- Planning accuracy

### Quality Insights
- Testing strategies that work
- Bug prevention techniques
- Code review effectiveness
- Documentation practices

### Performance Insights
- Optimization approaches
- Bottleneck patterns
- Scaling learnings
- Resource usage

### Team Insights
- Communication patterns
- Collaboration effectiveness
- Knowledge sharing
- Onboarding improvements

### Tool Insights
- Tool effectiveness
- Integration benefits
- Automation wins
- Productivity gains

## Confidence Levels

### High Confidence
- Multiple data points (10+ instances)
- Consistent pattern
- Clear causation
- Validated over time

### Medium Confidence
- Some data points (3-10 instances)
- General trend observed
- Likely correlation
- Needs more validation

### Low Confidence
- Few data points (1-2 instances)
- Initial observation
- Possible correlation
- Hypothesis to test

## Search Tips

### Find insights by category
```bash
grep -r "Category: Process" insights/
```

### Find high-confidence insights
```bash
grep -l "Confidence: High" insights/*.md
```

### Find recent insights
```bash
ls -lt insights/*.md | head -10
```

### Find insights with metrics
```bash
grep -l "Metric" insights/*.md
```

## Usage Guide

### For AI Agents

When processing issues:

1. **Search** for relevant process insights
2. **Apply** proven approaches
3. **Avoid** anti-patterns documented
4. **Measure** outcomes for validation
5. **Update** insights with new learnings

### For Human Developers

1. Review insights before starting work
2. Apply proven practices
3. Document new learnings
4. Validate/update existing insights
5. Share insights in retros

## Example Insights

### Example 1: Test-Driven Development

```markdown
# Insight: TDD Reduces PR Review Cycles

**Category**: Process
**Date**: 2025-12-01
**Source**: Analysis of 50 PRs
**Confidence**: High

## Context

Compared PR review cycles for TDD vs non-TDD PRs over 3 months (50 PRs total).

### Time Period
October - December 2025

### Sample Size
- TDD PRs: 25
- Non-TDD PRs: 25

## Observation

PRs with tests written first required significantly fewer review iterations.

### Specific Finding
- TDD PRs: Average 1.4 review rounds
- Non-TDD PRs: Average 2.8 review rounds
- 50% reduction in review cycles

### Data
- Average time to merge (TDD): 2.3 days
- Average time to merge (non-TDD): 4.1 days
- Review comments (TDD): 3.2 per PR
- Review comments (non-TDD): 7.8 per PR

## Analysis

### Why This Happened

1. Tests clarify requirements upfront
2. Edge cases discovered during test writing
3. Implementation guided by test cases
4. Fewer bugs in initial submission

### Contributing Factors
- Tests document expected behavior
- Test failures catch issues before review
- Reviewers can focus on design vs finding bugs

### Exceptions
- Very simple PRs (<50 LOC) show minimal difference
- Exploratory work benefits less from TDD

## Application

### How to Use This Learning

1. Write tests before implementation
2. Use tests to clarify requirements
3. Run tests locally before creating PR
4. Include test results in PR description

### When to Apply
- Use for all feature development
- Use for bug fixes
- Use for refactoring

### Expected Impact
- 40-50% reduction in review cycles
- 30-40% faster time to merge
- Fewer post-merge bugs

## Evidence

### Examples
- PR #145: TDD approach, merged in 1.5 days with 2 minor comments
- PR #147: Non-TDD, 4 review rounds, 5 days to merge
- PR #152: TDD for bug fix, 0 review rounds (auto-approved)

### Metrics
**Before TDD adoption** (3 months):
- Average review rounds: 3.1
- Average days to merge: 4.5
- Post-merge bugs: 12

**After TDD adoption** (3 months):
- Average review rounds: 1.6
- Average days to merge: 2.4
- Post-merge bugs: 5

## Related Insights

- insight-test-coverage.md
- insight-pr-size.md
- insight-review-effectiveness.md

## Review

**Last Validated**: 2026-01-06
**Still Relevant**: Yes
**Updates Since Creation**: None

---
_Extracted from manual analysis_
```

### Example 2: PR Size Impact

```markdown
# Insight: Small PRs Get Better Reviews

**Category**: Quality
**Date**: 2025-11-15
**Source**: PR analysis
**Confidence**: High

## Context

Analyzed relationship between PR size and review quality metrics.

### Sample Size
100 PRs analyzed

## Observation

PRs under 300 lines receive more thorough reviews and faster approvals.

### Data
- <150 LOC: 92% approval rate, 1.2 days avg
- 150-300 LOC: 85% approval rate, 2.1 days avg
- 300-500 LOC: 68% approval rate, 3.8 days avg
- 500+ LOC: 45% approval rate, 6.2 days avg

[... rest of insight following template]
```

### Example 3: Type Safety Benefits

```markdown
# Insight: TypeScript Strict Mode Catches 23% More Bugs

**Category**: Quality
**Date**: 2025-10-20
**Source**: Bug tracking analysis
**Confidence**: Medium

## Context

Compared bug discovery rates before/after enabling TypeScript strict mode.

[... rest of insight following template]
```

## Contributing

### Adding New Insights

1. Observe pattern or trend
2. Collect data (quantitative if possible)
3. Use template above
4. Include evidence and examples
5. Mark confidence level appropriately

### Updating Existing Insights

When new data available:
1. Add to evidence section
2. Update metrics
3. Adjust confidence if warranted
4. Note in review section

### Deprecating Insights

When no longer relevant:
1. Mark as deprecated in review section
2. Explain why it's no longer applicable
3. Link to superseding insight if exists
4. Keep for historical reference

## Validation

### Regular Review

Review insights quarterly:
- Are they still valid?
- Do metrics need updating?
- Has context changed?
- Should confidence level change?

### Quality Checks

- Specific, measurable observations
- Clear application guidance
- Evidence included
- Appropriate confidence level
- Regular validation dates

### Metrics

Track insight effectiveness:
- Number of insights by category
- Average confidence level
- Validation frequency
- Application rate in PRs

## Best Practices

### Quantify When Possible

✅ "Reduced review time by 40%" (specific)
❌ "Made reviews faster" (vague)

### Include Context

✅ "In React components over 200 LOC" (specific)
❌ "In components" (too broad)

### Show Evidence

✅ "Analyzed 50 PRs, found..." (data-driven)
❌ "I think..." (opinion)

### Be Actionable

✅ "Run linter before commit to catch..." (actionable)
❌ "Code quality matters" (too abstract)

---

**Insight Count**: Auto-updated by workflow
**Last Reviewed**: 2026-01-06
**Maintainers**: @owner, automated workflows
