# Bootstrap Seed v1.0

Create a minimal git-native issue automation system.

## Required Files

### 1. Workflow: .github/workflows/issue-agent.yml
Create workflow that:
- Triggers on: issues opened, issues labeled
- Runs when: issue has label "ai-task"
- Actions: checkout repo, setup environment, process issue, create PR
- Permissions: contents:write, pull-requests:write, issues:write

### 2. Template: .github/ISSUE_TEMPLATE/task.yml
Create issue template with:
- Name: "AI Task"
- Fields: title (text), description (textarea), acceptance_criteria (textarea)
- Labels: ["ai-task"]

### 3. Documentation: README.md
Create README with:
- Project title: "Git-Native Issue Automation"
- Description: "AI agents execute GitHub issues as work items"
- Quick start: How to create an issue that triggers automation
- Requirements: List required GitHub secrets

### 4. Knowledge Base: docs/knowledge/README.md
Create knowledge base structure:
- Directory: docs/knowledge/
- README explaining: how to contribute patterns, decisions, insights
- Create three subdirectories: docs/knowledge/patterns/, docs/knowledge/decisions/, docs/knowledge/insights/

### 5. Verification: scripts/verify-bootstrap.sh
Create bash script that:
- Checks all 4 files above exist
- Validates YAML syntax of workflow and template
- Checks workflow has required fields (on, jobs, permissions)
- Checks template has required fields (name, body)
- Exits 0 if all pass, exits 1 if any fail
- Makes script executable (chmod +x)

## Success Criteria
Run: `./scripts/verify-bootstrap.sh`
Expected: Exit code 0, all checks pass
