# Knowledge Base

Central repository for patterns, decisions, and insights learned from @copilot automation.

## Structure

```
docs/knowledge/
├── README.md           # This file
├── patterns/           # Reusable patterns and best practices
├── decisions/          # Architecture Decision Records (ADRs)
└── insights/           # Learned insights and metrics
    ├── logs/           # Automated logs (issues, PRs)
    └── improvements/   # Applied improvements
```

## Sections

### Patterns (`patterns/`)

Documented patterns for common tasks:

- **issue-workflow.md** - How issues flow through the system
- **pr-review.md** - PR review patterns and criteria
- **error-handling.md** - Common errors and solutions

### Decisions (`decisions/`)

Architecture Decision Records following ADR format:

- **001-copilot-automation.md** - Initial automation decision
- **002-knowledge-base-structure.md** - This structure
- **003-self-improvement.md** - Automated improvement system

### Insights (`insights/`)

Performance metrics and learned insights:

- **agent-performance.md** - Agent performance metrics
- **logs/** - Raw logs from automation
- **improvements/** - Applied improvement records

## Usage

### Reading

Browse the directories for relevant information. Start with:
1. `patterns/` for how-to guidance
2. `decisions/` for understanding why things work this way
3. `insights/` for performance data

### Contributing

When completing tasks, consider adding:

1. **New patterns** for reusable approaches
2. **Decisions** when making architectural choices
3. **Insights** when discovering performance issues

### Automatic Updates

Some files are automatically updated:

- `insights/logs/issues.jsonl` - Issue processing log
- `insights/logs/prs.jsonl` - PR processing log
- `insights/agent-performance.md` - Daily metrics update
- `insights/improvements/applied.jsonl` - Applied improvements

## Search

To find relevant knowledge:

```bash
# Search patterns
grep -r "keyword" docs/knowledge/patterns/

# Search decisions
grep -r "keyword" docs/knowledge/decisions/

# View recent insights
tail -20 docs/knowledge/insights/logs/issues.jsonl
```

## Maintenance

The self-improvement workflow (`self-improvement.yml`) automatically:

1. Analyzes logs daily
2. Identifies improvement opportunities
3. Creates improvement PRs
4. Updates metrics

Review improvement PRs to maintain quality.
