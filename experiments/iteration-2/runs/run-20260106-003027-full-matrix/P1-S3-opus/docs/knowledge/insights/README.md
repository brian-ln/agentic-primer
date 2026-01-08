# Insights

Performance metrics and learned insights from @copilot automation.

## Contents

| File | Description |
|------|-------------|
| [agent-performance.md](agent-performance.md) | Overall performance metrics |
| `logs/` | Raw automation logs |
| `improvements/` | Applied improvement records |

## Log Files

### issues.jsonl

Records issue processing events:

```json
{"timestamp": "2026-01-06T12:00:00Z", "issue_number": 42, "title": "Add feature", "status": "ready", "labels": ["copilot-task", "copilot-ready"]}
```

### prs.jsonl

Records PR processing events:

```json
{"timestamp": "2026-01-06T12:30:00Z", "pr_number": 15, "title": "feat: Add feature", "linked_issue": "42", "status": "reviewed", "author": "copilot[bot]"}
```

## Improvement Records

### applied.jsonl

Records applied improvements:

```json
{"timestamp": "2026-01-06T06:00:00Z", "category": "workflow", "description": "30% of PRs unlinked", "recommendation": "Enforce issue linking", "status": "proposed"}
```

## Metrics Dashboard

See [agent-performance.md](agent-performance.md) for current metrics.

Key metrics tracked:
- Issue-to-PR time
- PR success rate
- Review time
- Improvement PR count

## Analysis

To analyze logs:

```bash
# Count issues by status
jq -s 'group_by(.status) | map({status: .[0].status, count: length})' logs/issues.jsonl

# Find slow PRs
jq -s 'map(select(.status == "reviewed"))' logs/prs.jsonl

# Recent improvements
tail -10 improvements/applied.jsonl
```

## Maintenance

Logs grow over time. Periodically:

1. Archive old logs (> 90 days)
2. Aggregate statistics
3. Clean up applied improvements
