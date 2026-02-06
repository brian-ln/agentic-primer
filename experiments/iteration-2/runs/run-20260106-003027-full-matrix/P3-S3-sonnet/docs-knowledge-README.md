# Knowledge Base

This directory contains institutional knowledge for AI coding agents working in this repository.

## Purpose

The knowledge base provides:
- **Context**: Historical decisions and their rationale
- **Patterns**: Reusable solutions for common problems
- **Insights**: Meta-learnings from past execution

AI agents search this knowledge base before implementing solutions, enabling context-aware decisions based on project history.

## Structure

```
knowledge/
├── README.md          # This file - navigation guide
├── patterns/          # Code patterns and solutions
│   └── README.md
├── decisions/         # Architecture decisions
│   └── README.md
└── insights/          # Execution learnings
    └── README.md
```

### Patterns

**What**: Reusable code patterns, templates, and solutions

**When to use**: When implementing features similar to past work

**Location**: `patterns/`

**Example**: "Authentication pattern using JWT tokens" with reference implementation

### Decisions

**What**: Architecture decisions with context and rationale

**When to use**: When understanding why the codebase is structured a certain way

**Location**: `decisions/`

**Example**: "Decision to use PostgreSQL over MongoDB" with tradeoffs documented

### Insights

**What**: Meta-learnings from agent execution logs

**When to use**: When understanding what approaches work well in this project

**Location**: `insights/`

**Example**: "Test-driven development reduces PR review cycles by 40%"

## How It Works

### For AI Agents

When assigned an issue, agents:

1. **Parse issue** requirements and acceptance criteria
2. **Search knowledge base** for related patterns/decisions/insights
3. **Apply learnings** to implementation
4. **Create PR** with context-aware solution

### Automatic Updates

After PR merge:

1. **Extract patterns** from code changes
2. **Document decisions** from PR description and reviews
3. **Capture insights** from execution metadata
4. **Commit to knowledge base** automatically

## Search Strategies

### By Topic

```bash
# Find authentication-related patterns
grep -r "authentication" patterns/

# Find database decisions
grep -r "database" decisions/

# Find performance insights
grep -r "performance" insights/
```

### By File Type

```bash
# Find React component patterns
grep -r "React" patterns/

# Find API design decisions
grep -r "API" decisions/
```

### By Date

```bash
# Recent patterns (last 30 days)
find patterns/ -type f -mtime -30

# Decisions from specific time period
ls -lt decisions/ | head -20
```

### By PR Number

```bash
# Find learnings from specific PR
grep -r "PR #123" .
```

## Contributing

### Manual Additions

You can manually add knowledge:

```bash
# Create pattern
cp patterns/template.md patterns/my-new-pattern.md
# Edit with details

# Create decision
cp decisions/template.md decisions/my-decision.md
# Edit with context and rationale

# Create insight
cp insights/template.md insights/my-insight.md
# Edit with learnings
```

### Automatic Extraction

The knowledge base updates automatically:

1. Merge a PR
2. Workflow extracts learnings
3. New files appear in knowledge base
4. Commit pushed to main

## Document Templates

### Pattern Template

```markdown
# Pattern Name

**Context**: When to use this pattern
**Problem**: What problem it solves
**Solution**: Implementation approach
**Example**: Code reference
**Reusability**: How to apply elsewhere
```

### Decision Template

```markdown
# Decision Title

**Status**: Accepted/Superseded
**Context**: Background and constraints
**Decision**: What was decided
**Rationale**: Why this choice
**Consequences**: Positive/negative/neutral impacts
**Alternatives**: Other options considered
```

### Insight Template

```markdown
# Insight Title

**Date**: When discovered
**Context**: Situation
**Observation**: What was learned
**Application**: How to use this learning
**Metrics**: Quantitative data if available
```

## Quality Guidelines

### Good Knowledge Entries

✅ **Specific**: References actual code, PRs, issues
✅ **Actionable**: Clear how to apply the learning
✅ **Contextual**: Explains why, not just what
✅ **Searchable**: Good keywords and tags
✅ **Dated**: Timestamp for relevance assessment

### Poor Knowledge Entries

❌ **Vague**: "Be careful with database queries"
❌ **Obvious**: "Write tests for your code"
❌ **Outdated**: References deprecated approaches
❌ **Context-free**: No explanation of why
❌ **Duplicate**: Same information as existing entry

## Maintenance

### Regular Review

Quarterly review of knowledge base:
- Archive outdated entries
- Consolidate duplicates
- Update with new learnings
- Refactor for clarity

### Size Management

As knowledge base grows:
- Consolidate similar patterns
- Archive old decisions
- Summarize insights
- Extract to external documentation

### Metrics

Track knowledge base effectiveness:
- Number of entries (patterns/decisions/insights)
- Search frequency (agent references)
- PR improvement rate (before/after knowledge base)
- Review cycle time reduction

## Advanced Usage

### Knowledge Graph

Build relationships between entries:

```markdown
## Related
- Pattern: auth-jwt.md
- Decision: oauth-provider.md
- Insight: security-review-2024.md
```

### Tagging System

Add tags for better search:

```markdown
**Tags**: #authentication #security #jwt #api
```

### Version Tracking

Track when knowledge changes:

```markdown
**Version**: 2.0
**Updated**: 2026-01-06
**Changes**: Updated to reflect new OAuth flow
**Supersedes**: auth-pattern-v1.md
```

## Integration with Workflows

### Copilot Assignment Workflow

1. Issue assigned to @copilot
2. Workflow searches knowledge base
3. Relevant patterns passed to agent context
4. Agent implements using learned patterns

### Knowledge Base Update Workflow

1. PR merged to main
2. Workflow extracts metadata
3. Generates pattern/decision/insight documents
4. Commits to knowledge base
5. Available for next issue

## FAQ

**Q: How often is the knowledge base updated?**
A: Automatically after every merged PR. Manual additions can be made anytime.

**Q: Can I delete outdated knowledge?**
A: Yes, review and remove obsolete entries. Consider archiving instead of deleting.

**Q: How do agents know which knowledge to use?**
A: Agents search by keywords from issue description and related files.

**Q: What if knowledge conflicts?**
A: Newer entries generally supersede older ones. Review and consolidate conflicts.

**Q: Can I use this without AI agents?**
A: Yes! The knowledge base is valuable documentation for human developers too.

## Examples

### Example Pattern Reference

See `patterns/README.md` for examples of well-documented code patterns.

### Example Decision Record

See `decisions/README.md` for examples of architecture decision records.

### Example Insight

See `insights/README.md` for examples of execution learnings.

---

**Last Updated**: 2026-01-06
**Maintainers**: @owner, automated workflows
**Feedback**: Create an issue with suggestions for improvement
