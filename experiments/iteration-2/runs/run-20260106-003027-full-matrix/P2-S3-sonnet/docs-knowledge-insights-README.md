# Insights Repository

This directory contains learnings, observations, and insights extracted from development activities, retrospectives, and post-mortems.

## Purpose

Insights capture:

- **Lessons Learned** - What we discovered during development
- **Observations** - Patterns in our workflow or codebase
- **Post-Mortems** - Analysis of incidents and failures
- **Improvements** - Opportunities identified for optimization
- **Team Knowledge** - Tacit knowledge made explicit

## Insight Categories

### Technical Insights

Learnings about code, architecture, and tools.

**Examples:**
- "React hooks reduce boilerplate by 40%"
- "PostgreSQL JSONB queries perform better than joins for nested data"
- "GitHub Actions cache reduces CI time from 10min to 3min"

### Process Insights

Learnings about workflow, collaboration, and practices.

**Examples:**
- "Pairing on complex features reduces review cycles"
- "Small, frequent PRs merge 2x faster than large ones"
- "Pre-commit hooks catch 80% of linting issues"

### Team Insights

Learnings about communication, skills, and dynamics.

**Examples:**
- "Documentation prevents repeated questions"
- "Retrospectives surface blockers early"
- "Cross-training improves bus factor"

### Customer Insights

Learnings about users, requirements, and feedback.

**Examples:**
- "Users prefer keyboard shortcuts over mouse navigation"
- "Mobile-first design reduces desktop complexity"
- "Error messages with suggestions reduce support tickets"

## Insight Template

When documenting an insight, use this template:

```markdown
# Insight: [Key Learning]

**Category**: Technical | Process | Team | Customer
**Source**: PR #123, Issue #456, Retrospective, Post-Mortem, User Research
**Date**: YYYY-MM-DD
**Author**: @username
**Impact**: High | Medium | Low

## Context

What was the situation? What were we working on?

## Observation

What did we notice? What pattern or issue emerged?

### Supporting Data

Quantitative evidence:
- Metric 1: Before → After
- Metric 2: Comparison
- Benchmark: Results

Qualitative evidence:
- Quotes from team members
- User feedback
- Code examples

## Analysis

### Why This Happened

Root cause or explanation of the observation.

### Contributing Factors

- Factor 1: How it contributed
- Factor 2: How it contributed
- Factor 3: How it contributed

### Implications

What does this mean for:
- Our codebase?
- Our process?
- Our team?
- Our users?

## Action Items

What should we do based on this insight?

### Immediate Actions

- [ ] Action 1: Who, When
- [ ] Action 2: Who, When

### Long-Term Actions

- [ ] Action 1: Who, When
- [ ] Action 2: Who, When

### Knowledge Capture

- [ ] Document pattern: Where?
- [ ] Create ADR: Which decision?
- [ ] Update runbook: What process?

## Related Insights

- [Insight: Related Learning](./insight-related.md)
- [Insight: Another Learning](./insight-another.md)

## Related Decisions

- [ADR-123: Relevant Decision](../decisions/adr-123.md)

## Related Patterns

- [Pattern: Implementation](../patterns/pattern-name.md)

## Tags

- category:technical
- source:pr-123
- impact:high
- language:python
- component:api

## Changelog

- **YYYY-MM-DD**: Initial insight documented
- **YYYY-MM-DD**: Action items completed, updated with results
```

## How to Use Insights

### Capturing Insights

**During development:**
1. Notice something surprising or valuable
2. Document immediately (while fresh)
3. Include context and evidence
4. Share with team for feedback

**After completing work:**
1. Review PR or issue for learnings
2. Extract key insights
3. Quantify impact if possible
4. Create action items

**From retrospectives:**
1. Facilitate discussion
2. Identify patterns and themes
3. Document consensus insights
4. Assign action items

### Applying Insights

**Before starting work:**
1. Search insights for related learnings
2. Review action items from similar work
3. Apply recommendations
4. Update insight with new data

**When stuck:**
1. Search for similar challenges
2. Review how others solved it
3. Adapt approach
4. Document your experience

## Insight Index

Total Insights: 0

### By Category

- **Technical**: 0
- **Process**: 0
- **Team**: 0
- **Customer**: 0

### By Impact

- **High**: 0
- **Medium**: 0
- **Low**: 0

### Recent Insights

(No insights yet)

### Most Actionable

(Track which insights led to most improvements)

## Insight Sources

### From PRs

Automatically extracted when PR is merged:

- Code changes analyzed
- Commit messages reviewed
- PR comments mined
- Metrics collected

**Example**: "PR #42 showed that async/await reduces callback complexity"

### From Issues

Manually extracted from issue discussions:

- Problem-solving approaches
- Debugging strategies
- Workarounds discovered

**Example**: "Issue #15 revealed that caching API responses improves UX"

### From Retrospectives

Captured during team retrospectives:

- What went well
- What could improve
- Action items

**Example**: "Retro 2026-01: Pair programming on complex features speeds delivery"

### From Post-Mortems

Documented after incidents:

- Root cause analysis
- Timeline of events
- Prevention measures

**Example**: "Incident 2026-01: Missing index caused database slowdown"

### From User Research

Captured from user interactions:

- Usability testing
- Customer feedback
- Support tickets

**Example**: "Users struggle with multi-step forms, prefer single page"

## Insight Quality

### High-Quality Insights

- ✅ Specific and actionable
- ✅ Backed by evidence (quantitative or qualitative)
- ✅ Include context and background
- ✅ Lead to clear action items
- ✅ Transferable to other situations

### Low-Quality Insights

- ❌ Vague or generic
- ❌ Opinion without evidence
- ❌ Missing context
- ❌ No actionable recommendations
- ❌ Too narrow/specific to generalize

### Improving Insight Quality

Before submitting:

1. **Add Evidence**: Include metrics, quotes, examples
2. **Clarify Context**: Explain situation and constraints
3. **Be Specific**: Avoid generalizations
4. **Action-Oriented**: What should change?
5. **Peer Review**: Get feedback from team

## Insight Lifecycle

### 1. Capture

Notice and document insight.

**Output**: Draft insight document

### 2. Validate

Verify insight with data or team input.

**Output**: Confirmed insight with evidence

### 3. Act

Implement action items from insight.

**Output**: Changes to code, process, or docs

### 4. Measure

Track impact of changes.

**Output**: Metrics showing improvement (or not)

### 5. Refine

Update insight based on results.

**Output**: Updated insight with outcomes

### 6. Share

Communicate insight to broader audience.

**Output**: Blog post, presentation, or training

## Tools and Automation

### Automatic Extraction

GitHub Actions automatically extract insights from:

- Merged PRs (via knowledge-capture workflow)
- Issue comments (via issue-analysis workflow)
- Commit messages (pattern detection)

### Manual Capture

Use scripts for manual extraction:

```bash
# Extract insights from specific PR
./scripts/extract-patterns.sh --pr 123

# Extract from date range
./scripts/extract-patterns.sh --since 2026-01-01

# Extract from all merged PRs
./scripts/extract-patterns.sh --all
```

### Search and Analysis

```bash
# Search insights by keyword
grep -r "performance" docs/knowledge/insights/

# Find high-impact insights
grep -r "impact:high" docs/knowledge/insights/

# Insights from specific author
grep -r "author:alice" docs/knowledge/insights/
```

## Examples of Good Insights

### Example: Performance Optimization

**Insight**: "Adding database indexes reduced query time by 85%"

**Why Good**:
- Specific metric (85%)
- Clear cause and effect
- Actionable (add more indexes)
- Transferable (applies to other queries)

### Example: Process Improvement

**Insight**: "Small PRs (<200 lines) get reviewed 3x faster"

**Why Good**:
- Quantified improvement (3x)
- Actionable (keep PRs small)
- Team-wide application
- Easy to measure

## Anti-Patterns

### Anti-Pattern: Opinion as Insight

**Bad**: "React is better than Vue"

**Good**: "React hooks reduced our component boilerplate by 40%, measured across 50 components"

### Anti-Pattern: No Evidence

**Bad**: "Users don't like the new design"

**Good**: "5 out of 8 usability test participants struggled with navigation, taking 2x longer to complete tasks"

### Anti-Pattern: Too Vague

**Bad**: "Code quality improved"

**Good**: "Linting rules reduced code review comments about formatting by 70%"

## Contributing

### Creating New Insights

1. Use template above
2. Include evidence and context
3. Add relevant tags
4. Submit PR with label `insight`
5. Share in team chat

### Updating Existing Insights

1. Add new evidence or data
2. Update action items
3. Add to changelog
4. Note in PR description

### Insight Review

Team members should:

- Validate evidence
- Suggest improvements
- Add related insights
- Challenge assumptions

## Metrics

Track insight effectiveness:

- **Capture Rate**: Insights per month
- **Action Rate**: % with action items completed
- **Reference Rate**: How often insights are cited
- **Impact Rate**: Measured improvements from insights

## Questions?

- Create issue with label `insights`
- Ask in retrospectives
- Discuss in team chat

---

**Maintainer**: Development Team
**Last Updated**: 2026-01-08
**Version**: 1.0.0
