# Issue-Driven Development System for @copilot

## Design Overview

This document describes an issue-driven development system that enables GitHub Copilot (@copilot) to autonomously work on issues, create PRs, and integrate with a knowledge base for continuous learning.

## Architecture

```
Issue Created (with label: copilot)
         │
         ▼
GitHub Actions Workflow Triggered
         │
         ▼
@copilot processes issue body
         │
         ├─── Reads knowledge base (docs/knowledge/)
         │
         ▼
Creates branch + commits changes
         │
         ▼
Opens PR (auto-assigned via CODEOWNERS)
         │
         ▼
Human reviews via web UI
         │
         ▼
Knowledge base updated with insights
```

## Files Created

| File Path | Purpose |
|-----------|---------|
| `.github/ISSUE_TEMPLATE/copilot-task.yml` | Issue template for @copilot tasks |
| `.github/CODEOWNERS` | Auto-assignment of PRs to owner |
| `.github/workflows/copilot-agent.yml` | Workflow triggered on issue creation |
| `docs/knowledge/patterns/README.md` | Reusable code patterns |
| `docs/knowledge/decisions/README.md` | Architecture decision records |
| `docs/knowledge/insights/README.md` | Learned insights from development |
| `docs/knowledge/index.md` | Knowledge base index |
| `README.md` | Workflow documentation |

## Decision Log

### D1: Separate template from existing task.yml
**Why:** The existing `task.yml` is generic. A `copilot-task.yml` template specifically tailored for @copilot includes fields for context, constraints, and knowledge references.

### D2: CODEOWNERS uses wildcard with single owner
**Why:** Simplest configuration that ensures all PRs get assigned. Can be extended later.

### D3: Knowledge base uses markdown
**Why:** Human-readable, version-controlled, and can be parsed by AI agents.

### D4: Workflow uses issue labeled trigger
**Why:** Allows filtering to only process issues labeled `copilot`. Prevents accidental processing of regular issues.

## Validation Plan

1. **YAML Syntax**: All YAML files pass `yamllint` validation
2. **Workflow Trigger**: Workflow fires on issue creation with `copilot` label
3. **End-to-End**: Test issue flows through system to PR creation
4. **Knowledge Base**: Structure is navigable and parseable

## Simulated Execution

### Step 1: Issue Created
```
Title: [Copilot Task]: Add logging utility
Labels: copilot
Body:
  - Context: Need a simple logging utility
  - Acceptance: Function exists, tests pass
```

### Step 2: Workflow Triggered
```
[Simulated] GitHub Actions workflow copilot-agent.yml triggered
[Simulated] Checkout repository
[Simulated] @copilot reads issue body
[Simulated] @copilot reads docs/knowledge/patterns/
[Simulated] @copilot creates branch: copilot/issue-42
[Simulated] @copilot commits: feat: add logging utility
[Simulated] @copilot creates PR #43 (closes #42)
```

### Step 3: PR Created
```
Title: feat: Add logging utility (closes #42)
Assignee: @owner (via CODEOWNERS)
Labels: copilot-generated
```

### Step 4: Human Review
```
Reviewer opens PR in GitHub web UI
Reviews changes, provides feedback
Approves and merges
```

## Implementation Notes

- All files are self-contained with no external dependencies
- Knowledge base structure is extensible
- Workflow can be enhanced with additional steps (linting, testing)
- CODEOWNERS can be extended with team-specific patterns
