# Knowledge Base

This knowledge base stores patterns, decisions, and insights learned from issue-driven development with AI agents.

## Purpose

The knowledge base serves three functions:

1. **Patterns** - Reusable implementation solutions
2. **Decisions** - Architectural decision records (ADRs)
3. **Insights** - Learnings from execution successes and failures

## Structure

```
docs/knowledge/
├── README.md           # This file
├── patterns/           # Reusable implementation patterns
│   └── *.md           # Pattern documentation
├── decisions/          # Architectural decision records
│   └── *.md           # ADR documentation
└── insights/           # Execution learnings
    └── *.md           # Insight documentation
```

## Usage

### For AI Agents

When processing an issue:
1. Check `patterns/` for existing solutions to similar problems
2. Check `decisions/` for constraints and guidelines
3. After completion, add learnings to `insights/`

### For Humans

When reviewing PRs:
1. Reference relevant patterns in review comments
2. Add new ADRs when architectural decisions are made
3. Review insights to understand agent behavior

## File Naming Convention

All files use numeric prefixes for ordering:

- `001-pattern-name.md`
- `002-another-pattern.md`

## Template

Each file should follow this structure:

```markdown
# Title

## Context
Why this exists

## Content
The actual pattern/decision/insight

## References
Links to related issues, PRs, or files
```

## Self-Improvement

This knowledge base enables self-improvement by:

1. Detecting repeated patterns in successful PRs
2. Capturing failure modes and their solutions
3. Auto-generating pattern documentation

The GitHub workflow scans completed PRs and creates improvement PRs to update this knowledge base.
