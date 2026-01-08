# Knowledge Base

This knowledge base captures patterns, decisions, and insights from @copilot automation executions.

## Structure

```
knowledge/
|-- README.md           # This file - knowledge base index
|-- patterns/           # Reusable implementation patterns
|-- decisions/          # Architecture Decision Records (ADRs)
+-- insights/           # Lessons learned and agent-specific notes
```

## How to Use

### Before Starting Work

1. **Check patterns/** for existing solutions to similar problems
2. **Review decisions/** for architectural constraints that may affect your work
3. **Read insights/** for tips from previous implementations

### After Completing Work

Consider contributing back:

1. **New pattern discovered?** Add to `patterns/`
2. **Architectural decision made?** Create ADR in `decisions/`
3. **Lesson learned?** Document in `insights/`

## Categories

### Patterns

Reusable solutions to common problems:

- Workflow patterns (issue-to-PR flows)
- Code patterns (error handling, testing)
- Integration patterns (API, database)

### Decisions

Architecture Decision Records (ADRs) following the format:

- Title and date
- Status (proposed, accepted, deprecated)
- Context and problem
- Decision and rationale
- Consequences

### Insights

Lessons learned from executions:

- Agent-specific behaviors and preferences
- Common pitfalls and solutions
- Performance observations

## Contributing

When adding to the knowledge base:

1. Use clear, descriptive filenames
2. Include metadata (date, author/agent, related issues)
3. Keep entries focused and actionable
4. Link related entries where applicable

## Search Tips

- Use grep to search across all knowledge:
  ```bash
  grep -r "keyword" docs/knowledge/
  ```
- Check git history for context on when entries were added
- Look at related PRs for implementation details

## Maintenance

- Review quarterly for outdated entries
- Archive deprecated patterns rather than deleting
- Update insights as agent capabilities evolve
