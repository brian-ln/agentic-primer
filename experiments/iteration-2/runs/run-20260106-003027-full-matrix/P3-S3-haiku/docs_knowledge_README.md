# Knowledge Base

This directory contains the institutional knowledge of the project: architectural decisions, reusable patterns, and learning logs from autonomous task execution.

## Structure

```
docs/knowledge/
├── decisions/       # Architecture Decision Records (ADRs)
├── patterns/        # Reusable patterns and solutions
└── insights/        # Learning logs and execution history
```

## How to Use This Knowledge Base

### 1. Finding Architectural Decisions

All major decisions are documented in `decisions/` using the ADR format.

**File naming:** `ADR-NNN-{decision-title}.md`

**To find a decision:**
```bash
# List all decisions
ls docs/knowledge/decisions/

# Search by topic
grep -r "GitHub Actions" docs/knowledge/decisions/

# Read a specific ADR
cat docs/knowledge/decisions/ADR-001-event-driven-architecture.md
```

### 2. Learning from Patterns

Reusable patterns are in `patterns/` - these are battle-tested solutions @copilot has used successfully.

**Common patterns:**
- `issue-handling.md` - How to process issues end-to-end
- `pr-generation.md` - Techniques for creating quality PRs
- `knowledge-capture.md` - How to document learnings

### 3. Understanding Execution History

Every task @copilot executes creates a log in `insights/`. These show:
- What was tried
- What succeeded
- What failed and why
- How to improve

**Files:**
- `execution-log.md` - Chronological task history
- `success-log.md` - What worked (for replication)
- `failure-log.md` - What didn't work (to avoid)

## For @copilot (AI Agents)

When processing a new task:

1. **Check decisions first** - Are there any ADRs relevant to this task?
2. **Find patterns** - Is there a pattern in `patterns/` that applies?
3. **Review insights** - Did similar tasks succeed or fail before?
4. **Log results** - Update `execution-log.md` with what you did

## For Humans (Team Members)

When assigning a task:

1. **Review patterns** - Is there a pattern we should follow?
2. **Check decisions** - Are there architectural constraints?
3. **Read insights** - Have we tried this before?

When reviewing @copilot PRs:

1. **Understand the context** - Read the original issue + related ADRs
2. **Check if patterns were followed** - Compare implementation to `patterns/`
3. **Approve or suggest changes** - Point to relevant docs

## Adding New Content

### Creating an ADR

When making an architectural decision, create a new file:

```bash
cat > docs/knowledge/decisions/ADR-002-yaml-issue-format.md << 'EOF'
# ADR-002: Use YAML for Issue Template Format

## Status
ACCEPTED

## Decision
We use YAML frontmatter in GitHub issue templates for structured data capture.

## Context
- Need to parse issues reliably
- GitHub supports YAML in issue forms
- Parseable by both humans and machines

## Consequences
- Issues are more structured
- Parsing is simpler and more reliable
- Team needs to follow the format

## Alternatives Considered
- Markdown with fixed headers (rejected - less reliable parsing)
- Custom JSON format (rejected - not GitHub-native)
- Free-form text (rejected - too ambiguous)
EOF
```

### Creating a Pattern

Document patterns you've used successfully:

```bash
cat > docs/knowledge/patterns/github-actions-pattern.md << 'EOF'
# GitHub Actions Pattern

## When to Use
When you need to automate workflows triggered by GitHub events.

## Structure
1. Trigger (issue opened, PR created, etc.)
2. Input parsing (extract relevant data)
3. Validation (check preconditions)
4. Execution (do the work)
5. Output (create PR, comment, etc.)
6. Logging (record results)

## Example
[See issue-to-pr.yml workflow]

## Gotchas
- Workflow permissions must be explicitly set
- Token expiry for long-running jobs
- Branch creation needs proper auth
EOF
```

### Logging Results

After @copilot completes a task, update the insight logs:

```bash
# Append to execution log
echo "
## Task #123 - Add Feature X
- Status: SUCCESS
- Time: 2 hours
- Files: 5 created
- Tests: All passing
- Learnings: Could reuse pattern from Task #45
" >> docs/knowledge/insights/execution-log.md
```

## Search Tips

```bash
# Find all mentions of "performance"
grep -r "performance" docs/knowledge/

# List decisions by recency
ls -lt docs/knowledge/decisions/

# Show all success logs
cat docs/knowledge/insights/success-log.md | grep "✓"

# Find failures with category
grep "ERROR\|FAIL" docs/knowledge/insights/failure-log.md | grep "category:*"
```

## Knowledge Base Growth

This knowledge base grows with every task:

```
Week 1:  3 ADRs, 2 patterns, 1 execution log
Week 2:  6 ADRs, 5 patterns, 10 task entries
Week 3:  9 ADRs, 8 patterns, 25 task entries
Month 1: 20+ ADRs, 20+ patterns, 100+ learnings
```

Over time, this becomes your project's institutional memory - a resource more valuable than any single PR.

## Key Metrics

- **Decision coverage**: % of major decisions that are documented
- **Pattern reuse**: # of times patterns are cited in PR comments
- **Knowledge freshness**: Recency of last ADR/pattern update
- **Learnings velocity**: # of new insights per week

---

**Last Updated:** Automatically maintained by @copilot
**Total Entries:** [auto-counted]
**Last Task:** [auto-updated]
