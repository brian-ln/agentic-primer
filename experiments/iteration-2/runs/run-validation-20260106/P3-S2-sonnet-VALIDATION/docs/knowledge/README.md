# Knowledge Base

This directory contains git-tracked knowledge accumulated through @copilot executions and team experience.

## Purpose

The knowledge base serves as persistent memory for the automation system, capturing:
- **Patterns** - Reusable solutions and best practices
- **Decisions** - Architecture choices and rationale
- **Insights** - Learnings and observations from execution

All knowledge is version-controlled, making it easy to track evolution and share across the team.

## Structure

```
docs/knowledge/
├── README.md           # This file
├── patterns/           # Reusable implementation patterns
│   └── README.md
├── decisions/          # Architecture Decision Records (ADRs)
│   └── README.md
└── insights/           # Learnings and observations
    └── README.md
```

## Quick Reference

### When to Add Content

| Type | When to Add | Example |
|------|-------------|---------|
| **Pattern** | Solved a problem that might recur | "Error handling with retry logic" |
| **Decision** | Made an important architectural choice | "Why we chose JWT over sessions" |
| **Insight** | Discovered something unexpected | "@copilot handles async better with Promises" |

### How to Add Content

#### Adding a Pattern

```bash
# Create pattern file
touch docs/knowledge/patterns/error-handling-with-retry.md

# Document:
# - Problem statement
# - Solution approach
# - Code example
# - When to use/avoid
```

#### Recording a Decision

```bash
# Create numbered ADR
touch docs/knowledge/decisions/001-jwt-authentication.md

# Follow ADR format:
# - Status (Accepted/Deprecated/Superseded)
# - Context
# - Decision
# - Consequences
```

#### Capturing an Insight

```bash
# Create insight file
touch docs/knowledge/insights/copilot-async-patterns.md

# Document:
# - Observation
# - Context
# - Implications
# - Related work
```

## Usage by @copilot

@copilot can read this knowledge base to:
1. **Learn from past solutions** before implementing new features
2. **Understand context** behind architectural decisions
3. **Avoid repeating mistakes** documented in insights
4. **Apply proven patterns** to similar problems

The knowledge base grows with each execution, making the system progressively smarter.

## Best Practices

### For Humans

- **Review @copilot's knowledge updates** during PR review
- **Keep content concise** - prefer clarity over completeness
- **Link related content** - cross-reference patterns, decisions, and insights
- **Update outdated content** - mark deprecated decisions, evolve patterns

### For @copilot

When completing a task:
1. **Check existing knowledge** before implementing
2. **Document new patterns** discovered during implementation
3. **Record decisions** made during task execution
4. **Capture insights** from unexpected behaviors or learnings

## Knowledge Evolution

### Lifecycle

1. **Creation** - @copilot or human adds new knowledge
2. **Refinement** - Updates based on usage and feedback
3. **Deprecation** - Mark outdated content (don't delete)
4. **Archiving** - Move superseded content to archive/

### Quality Guidelines

**Good knowledge entries:**
- Specific and actionable
- Include examples
- Explain "why" not just "what"
- Link to related code/issues/PRs

**Avoid:**
- Vague generalizations
- Copy-pasting code without context
- Opinion without rationale
- Duplicate content

## Navigation

- **[Patterns](patterns/README.md)** - Reusable implementation patterns
- **[Decisions](decisions/README.md)** - Architecture Decision Records
- **[Insights](insights/README.md)** - Learnings and observations

## Maintenance

Regular maintenance keeps the knowledge base valuable:

- **Monthly**: Review recent additions for quality
- **Quarterly**: Update patterns with better approaches
- **Annually**: Archive outdated decisions and insights

The knowledge base is a living document - it should evolve with the project.

## Contributing

All team members (human and AI) should contribute to the knowledge base:

1. **During implementation** - Document patterns as you create them
2. **During review** - Suggest knowledge base updates in PR feedback
3. **During retrospectives** - Extract insights from sprint learnings
4. **During incident resolution** - Capture what was learned

Knowledge sharing is knowledge multiplying.
