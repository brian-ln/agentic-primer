# Knowledge Base

A structured repository of organizational knowledge captured from @copilot-assisted development.

## Purpose

This knowledge base serves as the persistent memory for the development workflow. It captures:

- **Patterns**: Reusable solutions to common problems
- **Decisions**: Architectural choices and their rationale
- **Insights**: Observations and learnings from @copilot interactions

## Structure

```
knowledge/
├── README.md           # This file - index and guide
├── patterns/           # Reusable code and design patterns
│   └── README.md
├── decisions/          # Architecture Decision Records (ADRs)
│   └── README.md
└── insights/           # Learnings from @copilot sessions
    └── README.md
```

## How to Use

### Adding Knowledge

1. **After completing an issue:** Document any reusable patterns discovered
2. **When making architectural choices:** Create an ADR in decisions/
3. **After @copilot sessions:** Record useful insights

### Finding Knowledge

- Browse directories by category
- Use GitHub search within the docs/ directory
- Reference ADRs by number (e.g., ADR-001)

## Categories

| Category | Purpose | Example |
|----------|---------|---------|
| [Patterns](./patterns/) | Reusable solutions | "Error handling pattern for API calls" |
| [Decisions](./decisions/) | Why we chose X | "ADR-001: Choose REST over GraphQL" |
| [Insights](./insights/) | What we learned | "Insight: @copilot works best with small issues" |

## Contributing

When adding to the knowledge base:

1. Use clear, descriptive titles
2. Include context and rationale
3. Add examples when possible
4. Link to related issues or PRs
