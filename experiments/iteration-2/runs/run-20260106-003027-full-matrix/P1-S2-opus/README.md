# @copilot Issue Automation

Automated issue-driven development with @copilot, auto-review, and knowledge base.

## Quick Start

### Create a Task

1. Go to **Issues** tab
2. Click **New Issue**
3. Select **Copilot Task** template
4. Fill in the required fields:
   - Task Summary
   - Detailed Description
   - Acceptance Criteria
   - Priority
   - Task Type

### What Happens Next

1. **Workflow Triggers**: GitHub Action detects the new issue
2. **@copilot Assigned**: Issue is labeled and @copilot receives assignment
3. **PR Created**: @copilot analyzes and creates a pull request
4. **Auto-Review**: CODEOWNERS assigns reviewer automatically
5. **Merge**: Approve and merge via web UI

## Workflow Diagram

```
Issue Created     Workflow Triggers    @copilot Processes
     |                  |                    |
     v                  v                    v
[Task Template] --> [Auto-Label] --> [Create PR]
                                          |
                                          v
                                    [CODEOWNERS]
                                          |
                                          v
                                    [Auto-Review]
                                          |
                                          v
                                    [Merge PR]
```

## Repository Structure

```
.
├── .github/
│   ├── ISSUE_TEMPLATE/
│   │   └── task.yml              # Task template for @copilot
│   ├── workflows/
│   │   └── copilot-automation.yml # Automation workflow
│   └── CODEOWNERS                 # Auto-reviewer assignment
├── docs/
│   └── knowledge/
│       ├── patterns/              # Reusable solutions
│       ├── decisions/             # Architecture decisions
│       └── insights/              # Learnings and observations
└── README.md                      # This file
```

## Features

### Issue Template

Structured form for creating @copilot tasks:
- Task summary and description
- Acceptance criteria (checklist)
- Priority selection (Low/Medium/High/Critical)
- Task type (Feature/Bug Fix/Refactor/Docs/Test/Chore)

### Automation Workflow

GitHub Actions workflow that:
- Triggers on issue creation
- Auto-labels issues mentioning @copilot
- Posts acknowledgment comments
- Logs issue details for tracking

### Auto-Review

CODEOWNERS file assigns reviewers based on:
- File paths modified
- Default repository owners
- Team-specific ownership rules

### Knowledge Base

Captures organizational learning:
- **Patterns**: Solutions that work
- **Decisions**: Why we chose approaches
- **Insights**: Observations from @copilot

## Usage Examples

### Example: Add Error Handling

```markdown
Title: [Task]: Add error handling to API client

Summary: Add error handling to the API client

Description:
The API client currently doesn't handle network errors gracefully.
Add try/catch blocks and meaningful error messages.

Acceptance Criteria:
- [ ] Network errors are caught
- [ ] User-friendly error messages shown
- [ ] Tests cover error scenarios

Priority: Medium
Type: Bug Fix
```

### Example: Add Documentation

```markdown
Title: [Task]: Document the authentication flow

Summary: Add documentation for auth flow

Description:
New developers need documentation explaining how
authentication works in this application.

Acceptance Criteria:
- [ ] Auth flow diagram created
- [ ] Step-by-step guide written
- [ ] Code examples included

Priority: Low
Type: Documentation
```

## Configuration

### Customize CODEOWNERS

Edit `.github/CODEOWNERS` to assign team-specific reviewers:

```
# Frontend team owns UI code
src/ui/ @frontend-team

# Backend team owns API code
src/api/ @backend-team
```

### Modify Workflow

Edit `.github/workflows/copilot-automation.yml` to customize:
- Trigger conditions
- Label assignments
- Notification messages

## Troubleshooting

### Issue Not Processed

1. Check if issue has `copilot` label
2. Verify workflow is enabled in Actions tab
3. Review workflow run logs for errors

### Reviewer Not Assigned

1. Verify CODEOWNERS syntax
2. Check that reviewers exist in repository
3. Ensure branch protection rules allow auto-assignment

## Contributing to Knowledge Base

When you discover something useful:

1. **Patterns**: Add to `docs/knowledge/patterns/`
2. **Decisions**: Add to `docs/knowledge/decisions/`
3. **Insights**: Add to `docs/knowledge/insights/`

See each directory's README for templates.

## License

MIT
