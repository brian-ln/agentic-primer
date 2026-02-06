# Bootstrap Seed v2.0

Create a minimal issue-driven development system using @copilot.

## Required Files

### 1. Issue Template: .github/ISSUE_TEMPLATE/task.yml
Create issue template:
- Name: "Development Task"
- Description: "Task for @copilot to execute"
- Fields:
  - title (input, required)
  - description (textarea, required): "What needs to be done"
  - acceptance_criteria (textarea, required): "How to verify completion"
- Auto-assign labels: ["copilot-task"]

### 2. Code Owners: .github/CODEOWNERS
Create file with content:
```
# All PRs auto-request review from repository owner
* @OWNER
```
(Replace @OWNER with the repository owner's GitHub username)

### 3. Documentation: README.md
Create README with:
- Title: "Issue-Driven Development"
- Description: "Development workflow using GitHub issues and @copilot"
- How it works:
  1. Create issue using "Development Task" template
  2. Mention @copilot in issue or assign issue to @copilot
  3. @copilot analyzes and creates PR with implementation
  4. PR auto-assigned for review (via CODEOWNERS)
  5. Review and merge via GitHub web UI
- Quick start example
- Requirements: GitHub Copilot subscription

### 4. Knowledge Base: docs/knowledge/README.md
Create knowledge base structure:
- Create directory: docs/knowledge/
- Create README explaining purpose: repository for patterns, decisions, learnings
- Create three subdirectories:
  - docs/knowledge/patterns/ (reusable patterns discovered)
  - docs/knowledge/decisions/ (architecture decision records)
  - docs/knowledge/insights/ (learnings and observations)

## Success Criteria
- All 4 files created
- Issue template appears in GitHub "New Issue" UI
- CODEOWNERS file triggers automatic review assignment on PRs
- README provides clear usage instructions

## Usage After Bootstrap
1. Create new issue using the template
2. Mention @copilot in a comment or assign issue to @copilot
3. Wait for PR (usually < 5 minutes)
4. Review PR (auto-assigned to you)
5. Merge via web UI
