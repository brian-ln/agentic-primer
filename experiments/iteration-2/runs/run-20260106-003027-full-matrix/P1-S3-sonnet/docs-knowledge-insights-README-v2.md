# Insights

Learnings extracted from PR analysis and execution logs.

## Purpose

This directory captures real-world learnings from the system in action:
- What worked well
- What challenges were encountered
- What improvements are needed
- Patterns discovered in practice

## Format

Insights are date-stamped and linked to specific PRs:

**Filename**: `YYYY-MM-DD-pr-NNN.md` or `YYYY-MM-DD-topic.md`

**Contents**:
- Summary of the PR or event
- Statistics (files changed, lines modified)
- Learnings (successes and failures)
- Patterns identified
- Improvement opportunities
- Tags for search

## Index

### Recent Insights

(Auto-populated as PRs are merged)

### Improvement Tracking

- [Improvement Opportunities](improvements.md) - Tracked suggestions from PR analysis

### Categorized Insights

#### Workflow Insights
- Issues with GitHub Actions execution
- Performance bottlenecks in CI/CD

#### Code Quality Insights
- Validation failures and fixes
- Testing gaps discovered

#### Knowledge Base Insights
- Pattern reuse effectiveness
- Documentation quality issues

#### Multi-Agent Insights
- Model-specific behaviors (Opus, Sonnet, Haiku)
- Success rate comparisons

## Auto-Generation Process

When a PR is merged to main:

1. **Workflow Triggered**: `knowledge-base-update.yml` runs
2. **PR Analyzed**: Extract metadata, diff, linked issues
3. **Insight Created**: Generate markdown summary
4. **Index Updated**: Add to this README
5. **Patterns Extracted**: Update pattern library if applicable

## Manual Enhancement

Auto-generated insights benefit from human review:

### What to Add

- **Context**: Why was this PR needed?
- **Challenges**: What was difficult?
- **Learnings**: What would you do differently?
- **Connections**: How does this relate to other work?

### Enhancement Workflow

1. Review auto-generated insight
2. Add context from PR discussion
3. Link to related patterns/decisions
4. Tag appropriately
5. Commit updates

## Insight Categories

### 1. Success Stories

Document what went well:
- Pattern reuse success
- Automation working smoothly
- Quick turnaround times

### 2. Challenges

Document difficulties:
- Validation failures
- Workflow errors
- Knowledge base gaps

### 3. Discoveries

New patterns or techniques:
- Novel solutions
- Unexpected behaviors
- Performance optimizations

### 4. Improvements

Opportunities identified:
- Repeated bugs (add checks)
- Missing documentation
- Process inefficiencies

## Search and Navigation

**By date**:
```bash
ls -lt docs/knowledge/insights/*.md | head -10
```

**By PR**:
```bash
find docs/knowledge/insights -name "*pr-123*"
```

**By topic**:
```bash
rg "workflow" docs/knowledge/insights/
```

**By tag**:
```bash
rg "tag:performance" docs/knowledge/insights/
```

## Insight Metrics

Track system learning:

| Metric | Target | Current |
|--------|--------|---------|
| Insights per month | 10+ | TBD |
| Human-enhanced insights | 50%+ | TBD |
| Improvement suggestions | 5+/month | TBD |
| Insights referenced in PRs | 20%+ | TBD |

## Insight Lifecycle

```
Auto-generated → Human-enhanced → Archived (if stale)
```

- **Auto-generated**: Created by workflow, basic facts only
- **Human-enhanced**: Context and learnings added
- **Archived**: Moved to archive/ after 12 months (if not frequently referenced)

## Archiving Old Insights

### When to Archive

- Insight is > 12 months old
- Content is outdated (superseded by newer learnings)
- Not referenced in past 6 months

### Archiving Process

```bash
mkdir -p docs/knowledge/insights/archive/2025/
mv docs/knowledge/insights/2025-*.md docs/knowledge/insights/archive/2025/
```

Update this README to remove archived entries from index.

## Integration with Patterns and Decisions

Insights often lead to:
- **New Patterns**: Successful technique → documented pattern
- **New Decisions**: Repeated issue → architectural decision
- **Pattern Updates**: Insight reveals pattern improvement

### Promotion Workflow

1. Identify insight worth promoting
2. Extract reusable pattern
3. Create pattern document
4. Link back to original insight
5. Tag insight as "promoted"

## Example Insight Structure

```markdown
# Insight: Improved Validation Performance

**PR**: #45
**Author**: @copilot
**Merged**: 2026-01-15
**Labels**: performance, validation

## Summary

Optimized validation workflow to run checks in parallel,
reducing PR validation time from 5min to 2min.

## Statistics

- Files changed: 3
- Lines added: 120
- Lines deleted: 45

## Learnings

### What Worked Well

- Matrix strategy for parallel validation
- Caching validation tool installations
- Early exit on first failure

### Challenges

- Coordinating job dependencies
- Aggregating results from parallel jobs

### Improvements

- Consider adding smoke tests before full validation
- Add timeout protection for runaway jobs

## Patterns Identified

- [Parallel CI/CD](../patterns/github-actions.md#matrix-builds)
- [Fail-Fast Strategy](../patterns/error-handling.md)

## Tags

`performance` `validation` `github-actions` `optimization`

---

**Status**: Human-enhanced
**Last Updated**: 2026-01-16
```

## Future Enhancements

1. **Semantic Analysis**: Use AI to extract deeper insights
2. **Visualization**: Charts showing trends over time
3. **Cross-Repository**: Share insights across organization
4. **Automated Tagging**: ML-based categorization
5. **Impact Tracking**: Measure effect of applied improvements

---

**Last Updated**: January 8, 2026
**Status**: Active
**Maintainer**: @copilot + human reviewers
