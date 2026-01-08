# Knowledge Base

This directory contains institutional knowledge for AI agents (@copilot) working on this repository.

## Purpose

The knowledge base serves as "memory" for the automated system, enabling:

1. **Pattern Reuse**: Avoid reinventing solutions that have already worked
2. **Decision Context**: Understand why architectural choices were made
3. **Continuous Learning**: Capture insights from successes and failures
4. **Consistency**: Maintain coding standards and best practices across PRs

## Structure

```
docs/knowledge/
├── README.md                    # This file
├── patterns/
│   ├── README.md               # Pattern catalog
│   ├── github-actions.md       # Workflow patterns
│   ├── scripting.md            # Shell script patterns
│   ├── error-handling.md       # Error handling strategies
│   └── testing.md              # Test patterns
├── decisions/
│   ├── README.md               # Decision index
│   ├── 001-architecture.md     # Core architecture decisions
│   ├── 002-tooling.md          # Tool selection rationale
│   └── 003-workflows.md        # Workflow design decisions
└── insights/
    ├── README.md               # Insights index
    ├── improvements.md         # Tracked improvement opportunities
    └── [YYYY-MM-DD-pr-NNN].md  # Date-stamped PR insights
```

## Usage by AI Agents

When @copilot receives an issue assignment, it:

1. **Searches patterns** for similar problems already solved
2. **Reviews decisions** to understand architectural constraints
3. **Learns from insights** to avoid repeated mistakes
4. **Applies patterns** to the current task

### Example Search Queries

```bash
# Find authentication patterns
grep -r "authentication" docs/knowledge/patterns/

# Find error handling strategies
rg "error handling" docs/knowledge/

# Review recent insights
ls -lt docs/knowledge/insights/ | head -10
```

## Automatic Updates

The knowledge base is automatically updated via the `knowledge-base-update` workflow when PRs are merged:

1. **PR merged** to main branch
2. **Workflow triggered** to analyze changes
3. **Patterns extracted** from code/config changes
4. **Insight created** with summary and learnings
5. **Indexes updated** for easy navigation

## Manual Contributions

While the system auto-populates knowledge, human contributions are valuable:

- **Enhance insights**: Add context to auto-generated entries
- **Create patterns**: Document complex patterns not auto-detected
- **Write ADRs**: Formalize major architectural decisions
- **Review quality**: Ensure auto-generated content is accurate

### Contributing Guidelines

1. **Patterns**: Focus on reusable solutions, not one-off fixes
2. **Decisions**: Include context, alternatives considered, and consequences
3. **Insights**: Be specific - link to PRs, issues, and code
4. **Format**: Use Markdown, follow existing structure
5. **Index**: Update relevant README.md files when adding content

## Knowledge Base Metrics

Track the health of the knowledge base:

| Metric | Target | Current |
|--------|--------|---------|
| Pattern library size | 10+ patterns | TBD |
| Decision records | 5+ ADRs | TBD |
| Insight frequency | Weekly | TBD |
| Search usage | Tracked in logs | TBD |

## Search and Navigation

### Quick Links

- [All Patterns](patterns/README.md)
- [All Decisions](decisions/README.md)
- [Recent Insights](insights/README.md)
- [Improvements](insights/improvements.md)

### Search Tips

**By topic**:
```bash
rg "authentication" docs/knowledge/
```

**By date**:
```bash
ls -lt docs/knowledge/insights/ | head -20
```

**By PR**:
```bash
find docs/knowledge -name "*pr-123*"
```

**By pattern type**:
```bash
rg "workflow" docs/knowledge/patterns/
```

## Maintenance

### Periodic Tasks

- **Monthly**: Review and archive outdated insights (>6 months)
- **Quarterly**: Consolidate similar patterns
- **Annually**: Major structure refactoring if needed

### Quality Gates

- All documents must be valid Markdown (checked by CI)
- Cross-references should use relative paths
- Code snippets should be syntax-highlighted
- Each document should have a clear purpose

## Future Enhancements

1. **Semantic Search**: Use vector embeddings for better search
2. **Visualization**: Generate knowledge graphs
3. **Multi-Repo**: Share knowledge across organization
4. **Versioning**: Track how patterns evolve over time
5. **Metrics**: Measure pattern reuse and effectiveness

---

**Last Updated**: January 8, 2026
**Maintainer**: @copilot automation + human reviewers
**Status**: Active
