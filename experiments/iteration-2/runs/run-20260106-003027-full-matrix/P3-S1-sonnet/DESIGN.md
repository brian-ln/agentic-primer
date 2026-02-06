# Issue-Driven Development System Design

## Overview

This system enables autonomous AI agents (like @copilot) to receive work via GitHub issues, complete tasks, and submit pull requests for human review.

## Architecture

### 1. Issue Templates (.github/ISSUE_TEMPLATE/)
- **task.yml**: Structured issue template for @copilot tasks
- Captures: description, acceptance criteria, context, priority
- Auto-labels issues with "copilot-task"

### 2. Code Ownership (.github/CODEOWNERS)
- Routes all PRs to repository owner for review
- Pattern: `* @owner` (catch-all assignment)
- Ensures human oversight on all changes

### 3. Knowledge Base (docs/knowledge/)
Structure:
```
docs/knowledge/
├── patterns/     # Reusable design patterns
├── decisions/    # Architecture decision records (ADRs)
└── insights/     # Learnings from completed work
```

Each directory contains:
- README.md explaining purpose
- Index file listing all entries
- Individual markdown files for each item

### 4. Workflow Documentation (README.md)
Clear process:
1. Create issue using task template
2. Assign to @copilot
3. @copilot analyzes, implements, tests
4. @copilot creates PR
5. Human reviews via GitHub web UI
6. Merge or request changes

## File Manifest

All files to be created:

1. `.github/ISSUE_TEMPLATE/task.yml` - Issue template for copilot tasks
2. `.github/CODEOWNERS` - PR auto-assignment
3. `docs/knowledge/README.md` - Knowledge base overview
4. `docs/knowledge/patterns/README.md` - Patterns documentation
5. `docs/knowledge/patterns/INDEX.md` - Pattern catalog
6. `docs/knowledge/decisions/README.md` - Decision records documentation
7. `docs/knowledge/decisions/INDEX.md` - Decision catalog
8. `docs/knowledge/insights/README.md` - Insights documentation
9. `docs/knowledge/insights/INDEX.md` - Insights catalog
10. `README.md` - Workflow documentation (appended to existing)

## Decision Rationale

### Why YAML issue template?
- Structured data extraction for automation
- Required fields prevent incomplete tasks
- GitHub native support, no external tools

### Why CODEOWNERS?
- Zero-config PR routing
- GitHub built-in feature
- Works with web UI (no CLI needed)

### Why three-part knowledge structure?
- **Patterns**: How to solve recurring problems
- **Decisions**: Why we chose specific approaches
- **Insights**: What we learned (success + failure)
- Separates "how" from "why" from "learned"

### Why docs/knowledge/ location?
- Clear separation from code
- Standard documentation path
- Easy to .gitignore if needed
- AI agents know to look here

## Test Strategy

### Verification Test Issue
Create test issue with:
- Title: "Test: Verify issue-driven workflow"
- Task: Add simple hello.txt file
- Expected: @copilot creates PR with file

### Success Criteria
1. Issue template renders correctly in GitHub UI
2. CODEOWNERS assigns PRs automatically
3. Knowledge base structure is navigable
4. README workflow is clear and actionable
5. Test issue processes without errors

## Implementation Order

1. Create directory structure
2. Implement issue template (validates YAML)
3. Implement CODEOWNERS (simple format)
4. Build knowledge base scaffolding
5. Document workflow in README
6. Create verification test issue

## Assumptions

- Repository owner GitHub username is "owner" (must be updated)
- Agent has write access to create files
- Git repository already initialized
- Agent can simulate GitHub API calls for verification
- Web UI access available for human review

## Edge Cases Handled

- Missing acceptance criteria in issue (template makes required)
- Multiple CODEOWNERS patterns (catch-all at end)
- Empty knowledge directories (README explains usage)
- Existing README (append workflow section)
- Invalid YAML (structured validation)

## Not Included (Out of Scope)

- GitHub Actions workflows (not in 30-word prompt)
- Automated testing CI/CD (not required for MVP)
- Issue automation bots (copilot is the agent)
- Branch protection rules (repository settings)
- Label automation (manual for now)

## Success Definition

System succeeds when:
1. All files created and valid (syntax check)
2. Test issue can be processed conceptually
3. Workflow is documented and clear
4. No manual setup required
5. Human can review PRs via web UI
