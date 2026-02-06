# Knowledge Base

This directory contains institutional knowledge extracted from development activities, including code patterns, architectural decisions, and insights from completed work.

## Purpose

The knowledge base serves as:

1. **Pattern Library** - Reusable code templates and design patterns
2. **Decision Log** - Historical record of architectural choices and rationale
3. **Insight Repository** - Learnings extracted from PRs, issues, and retrospectives
4. **Context for AI Agents** - Reference material for @copilot and other agents

## Structure

```
docs/knowledge/
├── README.md           # This file
├── patterns/           # Reusable code patterns and templates
│   ├── README.md       # Pattern catalog and index
│   └── *.md            # Individual pattern documents
├── decisions/          # Architecture Decision Records (ADRs)
│   ├── README.md       # ADR log and index
│   └── *.md            # Individual decision records
└── insights/           # Extracted learnings and observations
    ├── README.md       # Insight catalog and index
    └── *.md            # Individual insight documents
```

## Quick Navigation

### By Category

- **[Patterns](./patterns/README.md)** - Browse code patterns and templates
- **[Decisions](./decisions/README.md)** - Review architectural decisions
- **[Insights](./insights/README.md)** - Explore learnings and observations

### By Source

- **From PRs** - Files named `pr-{number}-*.md`
- **From Commits** - Files named `commit-{hash}-*.md`
- **Manual Entries** - Files named descriptively

## How to Use

### For Developers

**Before implementing a feature:**
1. Search patterns for similar solutions: `grep -r "authentication" patterns/`
2. Check decisions for architectural guidance: `grep -r "API design" decisions/`
3. Review insights from related work: `grep -r "OAuth" insights/`

**After completing work:**
1. Extract reusable patterns: `./scripts/extract-patterns.sh --pr 123`
2. Document decisions: Create ADR in `decisions/`
3. Capture learnings: Add insights to `insights/`

### For AI Agents

**When assigned an issue:**
1. Query knowledge base for relevant patterns
2. Review decisions affecting your implementation
3. Apply insights to avoid known pitfalls

**After completing a PR:**
1. Pattern extraction happens automatically (via GitHub Actions)
2. Review extracted patterns for accuracy
3. Suggest improvements via comments

## Search Strategies

### Full-Text Search

```bash
# Search all knowledge documents
grep -r "search term" docs/knowledge/

# Search specific category
grep -r "React hooks" docs/knowledge/patterns/

# Case-insensitive search
grep -ri "authentication" docs/knowledge/
```

### GitHub Search

Use GitHub's search interface with path filters:

- `path:docs/knowledge authentication`
- `path:docs/knowledge/patterns React`
- `path:docs/knowledge/decisions API`

### Tag-Based Search

Documents include tags for filtering:

```bash
# Find patterns by author
grep -r "author:alice" docs/knowledge/

# Find insights by PR
grep -r "pr:42" docs/knowledge/

# Find documents by type
grep -r "type:feat" docs/knowledge/
```

## Statistics

- **Total Documents**: 0
- **Last Updated**: 2026-01-08

### By Category

- Patterns: 0
- Decisions: 0
- Insights: 0

### Recent Additions

(Documents added in the last 30 days)

- None yet

## Maintenance

### Automatic Updates

The knowledge base is automatically updated via GitHub Actions:

- **After PR merge**: Patterns and insights extracted from changes
- **Weekly**: Indexes rebuilt and statistics updated
- **Monthly**: Stale documents reviewed and archived

### Manual Curation

Team members should:

- Review auto-generated content for accuracy
- Merge duplicate patterns
- Update outdated decisions
- Archive obsolete knowledge

### Quality Standards

Knowledge documents should:

- ✅ Have clear, descriptive titles
- ✅ Include context and examples
- ✅ Link to source PRs or issues
- ✅ Use proper markdown formatting
- ✅ Include relevant tags
- ❌ Avoid sensitive information (passwords, keys)
- ❌ Not include generated code without context

## Contributing

### Creating Patterns

See [patterns/README.md](./patterns/README.md) for template and guidelines.

**Quick template:**

```markdown
# Pattern: [Descriptive Name]

## Context
When and why to use this pattern.

## Implementation
Code example or template.

## Examples
Real-world usage from codebase.

## Related
Links to similar patterns or decisions.
```

### Creating Decisions

See [decisions/README.md](./decisions/README.md) for ADR template.

**Quick template:**

```markdown
# ADR-{number}: [Decision Title]

**Status**: Proposed | Accepted | Deprecated | Superseded

**Date**: YYYY-MM-DD

**Context**: What is the issue we're addressing?

**Decision**: What are we doing about it?

**Consequences**: What becomes easier or harder as a result?
```

### Creating Insights

See [insights/README.md](./insights/README.md) for guidelines.

**Quick template:**

```markdown
# Insight: [Key Learning]

**Source**: PR #123, Issue #456, Retrospective

**Date**: YYYY-MM-DD

## Observation
What did we notice?

## Analysis
Why did this happen?

## Action Items
What should we do differently?

## Tags
- category:testing
- author:alice
```

## Tools and Scripts

- **`scripts/extract-patterns.sh`** - Extract patterns from PRs or commits
- **`scripts/search-knowledge.sh`** - Advanced search across knowledge base
- **GitHub Actions** - Automatic extraction on PR merge

## Related Documentation

- [Workflow Guide](../../WORKFLOW_GUIDE.md) - Development workflow overview
- [Agent Configuration](../../.github/agents.md) - AI agent setup
- [Contributing Guide](../../CONTRIBUTING.md) - Contribution guidelines

## Questions?

- Open an issue with label `documentation`
- Ask in team chat or discussions
- Review recent PRs for examples

---

**Version**: 1.0.0
**Maintained by**: Development Team
**Last reviewed**: 2026-01-08
