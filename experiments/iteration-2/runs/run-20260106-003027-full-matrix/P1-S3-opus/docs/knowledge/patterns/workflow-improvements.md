# Pattern: Workflow Improvements

## Context

Use this pattern to continuously improve the @copilot automation system.

## Problem

Systems degrade over time without intentional improvement:
- Common issues repeat
- Inefficiencies accumulate
- Knowledge is lost
- Performance declines

## Solution

Automated improvement loop that:
1. Collects metrics
2. Identifies patterns
3. Proposes improvements
4. Applies changes

## Improvement Loop

```
Collect Logs → Analyze Patterns → Propose Changes → Review & Merge → Monitor
      ↑                                                                  │
      └──────────────────────────────────────────────────────────────────┘
```

## Implementation

### 1. Log Collection

Automatic logging in `docs/knowledge/insights/logs/`:

**issues.jsonl**
```json
{"timestamp": "...", "issue_number": 42, "title": "...", "status": "ready"}
```

**prs.jsonl**
```json
{"timestamp": "...", "pr_number": 15, "linked_issue": "42", "status": "reviewed"}
```

### 2. Pattern Analysis

`self-improvement.yml` workflow runs daily:

```python
# Analyze for common patterns
- Repeated task types → suggest templates
- Unlinked PRs → suggest enforcement
- Slow reviews → suggest automation
```

### 3. Change Proposal

Automatic PR creation with:
- Clear problem description
- Recommended solution
- Confidence score
- Category label

### 4. Review & Merge

Human reviews improvement PRs:
- Verify recommendation makes sense
- Check implementation is correct
- Merge or close with feedback

### 5. Monitor

Track improvement effectiveness:
- Did the change help?
- Any new issues?
- Update metrics

## Improvement Categories

| Category | Examples |
|----------|----------|
| Template | New issue types, better prompts |
| Workflow | Faster validation, better labels |
| Documentation | Clearer instructions, examples |
| Process | Review guidelines, approval rules |

## Examples

### Template Improvement

```
Finding: "API" appears in 40% of issue titles
Recommendation: Create specialized API task template
PR: Adds .github/ISSUE_TEMPLATE/api-task.yml
Result: Faster issue processing for API tasks
```

### Workflow Improvement

```
Finding: 30% of PRs lack linked issues
Recommendation: Add PR template with required issue link
PR: Updates PR template, adds validation
Result: 95% of PRs now link issues
```

## Success Metrics

| Metric | Target | How Measured |
|--------|--------|--------------|
| Improvement PRs/month | >= 3 | Count of auto-PRs |
| Merge rate | >= 50% | Merged / Created |
| Issue resolution time | Decreasing | Log timestamps |
| Error rate | Decreasing | Failed validations |

## Applied Improvements

Recent improvements are logged in:
`docs/knowledge/insights/improvements/applied.jsonl`

## Related

- [Agent Performance](../insights/agent-performance.md)
- [Self-Improvement Workflow](/.github/workflows/self-improvement.yml)
