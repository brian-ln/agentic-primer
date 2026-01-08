# Insights

Lessons learned from development sessions and @copilot interactions.

## Purpose

Insights capture observations that don't fit into patterns (how) or decisions (why), but are valuable for future work:
- Unexpected behaviors
- Process improvements
- Tool discoveries
- Workflow optimizations

## Template

Create entries in this file or separate files for major insights:

```markdown
## Insight Title

**Date:** YYYY-MM-DD
**Context:** Issue #NNN or general observation

### Observation

What did we notice? Be specific.

### Impact

How does this affect future work?

### Recommendation

What should we do differently?
```

## Index

### Bootstrap Insights (2026-01-08)

#### 1. Structured Templates Improve @copilot Accuracy

**Observation:** Issue templates with explicit fields (task, acceptance criteria, context) help @copilot understand requirements better than free-form descriptions.

**Impact:** Higher quality first-attempt implementations, fewer revision cycles.

**Recommendation:** Always use structured templates for @copilot tasks.

#### 2. Manual Assignment is Currently Required

**Observation:** There is no API to programmatically assign @copilot. Assignment must be done via GitHub UI.

**Impact:** "Automation" requires human initiation step.

**Recommendation:** Document this clearly; don't promise full automation.

#### 3. Auto-Review Complements Human Review

**Observation:** Automated checks (linting, tests, security) catch common issues before human review.

**Impact:** Human reviewers can focus on logic and design, not formatting.

**Recommendation:** Keep automated checks fast and focused on objective criteria.

#### 4. Knowledge Base Requires Active Curation

**Observation:** Knowledge doesn't capture itself. Without deliberate effort, the knowledge base becomes stale.

**Impact:** Stale knowledge is worse than no knowledge (misleading).

**Recommendation:** Schedule periodic review; delete rather than keep outdated content.

## Processed Issues Log

| Date | Issue | Outcome | Notes |
|------|-------|---------|-------|
| - | - | - | No issues processed yet |
