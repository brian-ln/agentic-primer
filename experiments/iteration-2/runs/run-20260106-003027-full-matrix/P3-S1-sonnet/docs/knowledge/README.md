# Knowledge Base

This knowledge base captures institutional knowledge for AI agents and human developers working on this project.

## Purpose

As @copilot and other agents complete tasks, they accumulate knowledge about:
- **What works** (patterns)
- **Why decisions were made** (decisions)
- **What was learned** (insights)

This knowledge base preserves that understanding across sessions, team changes, and time.

## Structure

### `/patterns/` - How to Solve Problems
Reusable design patterns, code templates, and proven approaches.

**When to add:**
- You solve a problem that will recur
- You create a reusable template
- You establish a coding convention

**Examples:**
- API endpoint structure
- Error handling patterns
- Testing strategies
- Database migration patterns

### `/decisions/` - Why We Chose This
Architecture Decision Records (ADRs) documenting significant technical choices.

**When to add:**
- You make a technology choice (framework, library, tool)
- You decide on an architectural approach
- You establish a process or workflow
- You choose between competing alternatives

**Examples:**
- Why we use SQLite vs PostgreSQL
- Why we chose REST over GraphQL
- Why we structure folders this way

### `/insights/` - What We Learned
Learnings from completed work, both successes and failures.

**When to add:**
- You complete a challenging task
- You encounter a gotcha or edge case
- You discover a performance issue
- You learn something non-obvious

**Examples:**
- "Learned: Always validate input before database writes"
- "GitHub Actions cache keys must be unique per branch"
- "Error messages should include context for debugging"

## Usage

### For AI Agents (@copilot)
1. **Before starting work**: Read relevant patterns and decisions
2. **During work**: Reference patterns for implementation guidance
3. **After completing work**: Add new insights or patterns discovered
4. **When making decisions**: Document significant choices

### For Human Developers
1. Browse the knowledge base to understand project conventions
2. Add entries when you make decisions or discover patterns
3. Update entries as understanding evolves
4. Reference entries in code reviews and discussions

## File Format

All entries use Markdown with this structure:

```markdown
# Title

## Context
Brief background on why this exists

## Content
The actual pattern/decision/insight

## Related
Links to other knowledge base entries, issues, or PRs
```

## Maintenance

- Keep entries focused and concise
- Update INDEX.md when adding new entries
- Archive outdated entries rather than deleting them
- Link related entries together

## Getting Started

Each subdirectory (patterns/, decisions/, insights/) has:
- **README.md** - Detailed usage guide
- **INDEX.md** - Catalog of all entries

Start by reading those files to understand the structure.
