# Knowledge Base

This directory contains the project's knowledge base, organized to provide context and guidance for both human developers and AI agents like @copilot.

## Purpose

The knowledge base serves as the project's institutional memory:
- **For Humans:** Reference documentation, design rationale, best practices
- **For AI Agents:** Context for autonomous code generation and decision-making
- **For Teams:** Shared understanding of patterns, decisions, and learnings

## Organization

The knowledge base is organized into three categories:

### 📐 Patterns (`patterns/`)

Reusable solutions to common problems. **What** we do.

- Design patterns
- Code templates
- Architectural patterns
- Integration patterns

Examples: Error handling, API design, database access, caching strategies

### 🎯 Decisions (`decisions/`)

Architectural Decision Records (ADRs). **Why** we do it this way.

- Technology choices
- Architecture decisions
- Trade-off analysis
- Decision context and consequences

Examples: Why REST over GraphQL, why PostgreSQL, why microservices

### 💡 Insights (`insights/`)

Empirical learnings and best practices. **What we learned**.

- Lessons learned
- Best practices
- Performance optimizations
- Team workflows

Examples: Testing strategies, deployment lessons, debugging tips

## Usage

### For Developers

Browse the knowledge base to:
- Understand why decisions were made
- Find reusable patterns for common tasks
- Learn from past experiences
- Contribute your own learnings

### For @copilot

Reference knowledge in issue descriptions:

```markdown
Knowledge References:
- docs/knowledge/patterns/api-error-handling.md
- docs/knowledge/decisions/001-github-copilot-automation.md
```

The automation workflow loads relevant knowledge and provides it to @copilot as context.

### Contributing

See the README in each category for contribution guidelines:
- `patterns/README.md` - How to document patterns
- `decisions/README.md` - How to write ADRs
- `insights/README.md` - How to capture insights

## Maintenance

- **Keep it current:** Update patterns and insights as the project evolves
- **Link from code:** Reference knowledge files in code comments
- **Review regularly:** Quarterly review to archive outdated content
- **Quality over quantity:** Better to have 10 excellent entries than 100 mediocre ones

## File Naming Conventions

- **Patterns:** Descriptive names (e.g., `api-error-handling.md`, `database-migrations.md`)
- **Decisions:** Numbered ADRs (e.g., `001-use-rest-api.md`, `002-choose-postgresql.md`)
- **Insights:** Topic-based names (e.g., `deployment-best-practices.md`, `testing-strategies.md`)

All files use lowercase with hyphens, `.md` extension.
