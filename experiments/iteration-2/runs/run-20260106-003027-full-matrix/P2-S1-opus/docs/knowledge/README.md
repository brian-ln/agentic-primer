# Knowledge Base

This knowledge base captures patterns, decisions, and insights from issue-driven development with @copilot.

## Purpose

The knowledge base serves as institutional memory for the repository, enabling:

1. **Patterns** - Reusable solutions that @copilot and developers can reference
2. **Decisions** - Architecture Decision Records (ADRs) documenting choices and rationale
3. **Insights** - Lessons learned from @copilot interactions and automated development

## Structure

```
docs/knowledge/
├── README.md           # This file - knowledge base index
├── patterns/           # Reusable code patterns and solutions
│   └── README.md       # Pattern documentation guidelines
├── decisions/          # Architecture Decision Records (ADRs)
│   ├── README.md       # ADR format and guidelines
│   └── 001-*.md        # Individual decision records
└── insights/           # Lessons learned and observations
    └── README.md       # Insights documentation guidelines
```

## Navigation

- [Patterns](patterns/README.md) - Reusable implementation patterns
- [Decisions](decisions/README.md) - Architecture Decision Records
- [Insights](insights/README.md) - Lessons learned from @copilot

## Usage

### For Developers

1. **Check patterns** before implementing common functionality
2. **Review decisions** to understand architectural context
3. **Document insights** when completing @copilot-driven work

### For @copilot

When processing issues, @copilot should:
1. Reference patterns for consistent implementation
2. Follow established decisions
3. Capture new insights in the appropriate directory

## Contributing

When adding to the knowledge base:
1. Follow the templates in each subdirectory
2. Use clear, descriptive titles
3. Include context and rationale
4. Link related documents
