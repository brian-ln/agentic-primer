# Knowledge Base

This knowledge base provides context and guidance for @copilot when processing issues. It contains reusable patterns, architecture decisions, and insights learned from development.

## Structure

```
docs/knowledge/
├── index.md          # This file - knowledge base overview
├── patterns/         # Reusable code patterns
│   └── README.md     # Pattern catalog
├── decisions/        # Architecture Decision Records (ADRs)
│   └── README.md     # Decision log
└── insights/         # Learned insights from development
    └── README.md     # Insights catalog
```

## How @copilot Uses This Knowledge

When processing an issue, @copilot:

1. **Reads relevant patterns** to ensure consistent code style
2. **Checks decisions** to understand architectural constraints
3. **Reviews insights** to avoid known pitfalls

## Referencing Knowledge in Issues

When creating an issue for @copilot, reference relevant knowledge:

```markdown
### Knowledge References
- See docs/knowledge/patterns/error-handling.md for error patterns
- Follow docs/knowledge/decisions/ADR-001-api-design.md for API conventions
```

## Contributing to Knowledge

After @copilot completes a task, consider adding:

- **New patterns** discovered during implementation
- **Decisions** made that affect future development
- **Insights** learned from the task

## Quick Links

- [Patterns Catalog](patterns/README.md)
- [Decisions Log](decisions/README.md)
- [Insights Catalog](insights/README.md)
