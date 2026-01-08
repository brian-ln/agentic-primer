# Knowledge Base

Welcome to the project knowledge base. This directory serves as a living repository for patterns, decisions, and insights accumulated during development.

## Purpose

The knowledge base helps teams:
- **Remember context** that would otherwise be lost in chat logs
- **Document decisions** with rationale for future reference
- **Share patterns** that work well in this codebase
- **Capture insights** from retrospectives and post-mortems
- **Onboard new contributors** with searchable, versioned documentation

## Structure

```
docs/knowledge/
├── README.md (this file)
├── patterns/
│   ├── README.md
│   └── [pattern-name].md
├── decisions/
│   ├── README.md
│   └── [YYYYMMDD-decision-title].md
└── insights/
    ├── README.md
    └── [YYYYMMDD-insight-title].md
```

### Patterns

Code patterns, best practices, and conventions used in this project.

**When to add:**
- Discovered a useful approach worth reusing
- Solved a tricky problem with a clean solution
- Established a convention the team should follow

**Format:** See [patterns/README.md](patterns/README.md)

### Decisions

Architecture Decision Records (ADRs) documenting important technical choices.

**When to add:**
- Made a significant architectural decision
- Chose between multiple viable alternatives
- Need to explain "why we did it this way" for future maintainers

**Format:** See [decisions/README.md](decisions/README.md)

### Insights

Lessons learned, observations, and reflections from development experience.

**When to add:**
- Post-mortem from an incident or outage
- Retrospective findings worth preserving
- "I wish I'd known this earlier" moments

**Format:** See [insights/README.md](insights/README.md)

## How to Use

### Adding Content

1. **Choose the right category** (patterns, decisions, or insights)
2. **Create a new markdown file** following the naming convention
3. **Use the template** from the category's README
4. **Commit and open a PR** for team review
5. **Update this README** if adding new categories

### Finding Content

- **GitHub Search:** Use the repository search with keywords
- **Browse by Category:** Navigate to patterns/, decisions/, or insights/
- **Read READMEs:** Each category README indexes its contents
- **Git History:** Use `git log` to see evolution of documents

### Maintaining Quality

- **Keep it current:** Update documents when context changes
- **Link liberally:** Connect related patterns, decisions, and insights
- **Be specific:** Include examples, code snippets, and context
- **Explain why:** Document rationale, not just what was done

## Integration with @copilot

This knowledge base is designed to work seamlessly with GitHub Copilot and other AI agents:

- **Context for Agents:** Agents can read this documentation to understand project conventions
- **Agent-Generated Insights:** Agents can propose new patterns/decisions based on their work
- **Version Control:** Git tracks all changes, making it easy to review agent contributions
- **Searchable:** Markdown format enables both human and AI search

## Examples

See existing entries for reference:

- **Pattern Example:** [patterns/example-pattern.md](patterns/example-pattern.md) (if it exists)
- **Decision Example:** [decisions/example-decision.md](decisions/example-decision.md) (if it exists)
- **Insight Example:** [insights/example-insight.md](insights/example-insight.md) (if it exists)

## Contributing

Everyone is encouraged to contribute to the knowledge base:

1. Share patterns you've found useful
2. Document decisions as you make them
3. Capture insights while they're fresh
4. Review and improve existing entries

The knowledge base is only valuable if it's kept up to date. When you discover something worth remembering, write it down here!

## Metadata

- **Created:** 2026-01-06
- **Maintainers:** All project contributors
- **Last Updated:** 2026-01-06
- **Status:** Active
