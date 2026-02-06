# Knowledge Base

This knowledge base provides context and guidance for the Copilot agent when processing issues and generating implementations.

## Purpose

The knowledge base serves three key functions:

1. **Patterns** - Reusable code patterns, conventions, and best practices
2. **Decisions** - Architectural decisions and their rationale (ADR-style)
3. **Insights** - Learnings from past implementations and retrospectives

## Structure

```
docs/knowledge/
├── README.md                    # This file
├── patterns/                    # Reusable "how-to" templates
│   └── api-design.md           # RESTful API conventions
├── decisions/                   # Historical "why" context
│   └── workflow-architecture.md # Workflow design decisions
└── insights/                    # Empirical "lessons learned"
    └── automation-learnings.md  # Automation insights
```

## Usage

### For Copilot Agent

The workflow automatically scans this directory structure and provides a summary to the Copilot agent:
- Counts of available patterns, decisions, and insights
- File names for quick reference
- Content is available for detailed consultation

### For Humans

Browse these directories to understand:
- How we approach common problems (patterns)
- Why we made certain architectural choices (decisions)
- What we've learned from past work (insights)

## Adding Knowledge

### When to Add

Add knowledge when you:
- Solve a problem that might recur
- Make an architectural decision worth documenting
- Learn something valuable from an implementation or bug fix

### How to Add

1. **Choose the right category:**
   - **Pattern** if it's a reusable solution template
   - **Decision** if it's an architectural choice with rationale
   - **Insight** if it's a lesson learned from experience

2. **Create a new markdown file:**
   - Use kebab-case naming: `api-design.md`, `database-choice.md`
   - Include frontmatter with metadata (title, date, tags)
   - Write clear, concise content

3. **Submit via PR:**
   - Knowledge updates should be reviewed like code
   - Ensure accuracy and clarity
   - Link to related issues or PRs

### Template: Pattern

```markdown
---
title: Pattern Name
category: pattern
tags: [tag1, tag2]
created: YYYY-MM-DD
---

# Pattern Name

## Context
When and why to use this pattern.

## Solution
How to implement this pattern (with code examples).

## Examples
Real-world usage in this codebase.

## Tradeoffs
Advantages and disadvantages.
```

### Template: Decision

```markdown
---
title: Decision Title
category: decision
status: accepted
date: YYYY-MM-DD
---

# Decision: Title

## Context
What problem or question prompted this decision?

## Options Considered
- Option A: pros/cons
- Option B: pros/cons
- Option C: pros/cons

## Decision
What we chose and why.

## Consequences
What this means for future work (positive and negative).
```

### Template: Insight

```markdown
---
title: Insight Title
category: insight
date: YYYY-MM-DD
source: Issue #X or PR #Y
---

# Insight: Title

## What We Learned
Brief summary of the insight.

## Context
What situation led to this learning?

## Impact
How does this change our approach going forward?

## Related
Links to issues, PRs, or other knowledge base items.
```

## Evolution

This knowledge base should evolve organically:
- **Add** when you learn something valuable
- **Update** when understanding deepens or changes
- **Deprecate** when patterns become obsolete (mark with status)
- **Refactor** when organization needs improvement

## Integration

### Workflow Integration

The Copilot agent workflow reads this knowledge base during issue processing:

```yaml
- name: Read knowledge base
  run: |
    find docs/knowledge/patterns -name "*.md"
    find docs/knowledge/decisions -name "*.md"
    find docs/knowledge/insights -name "*.md"
```

The summary is included in:
- Copilot agent context
- Pull request descriptions
- Issue completion comments

### Future Enhancements

Potential improvements:
- Full-text search during issue processing
- Automatic knowledge extraction from merged PRs
- Knowledge base statistics and analytics
- Integration with advanced systems (if using enterprise)

## Maintenance

- **Owner:** Team responsibility (not individual)
- **Review:** Knowledge updates reviewed like code
- **Gardening:** Periodic cleanup and reorganization
- **Quality:** Accuracy and clarity over quantity

---

**Started:** 2026-01-08
**Last Updated:** 2026-01-08
**Status:** Active
