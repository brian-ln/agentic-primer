# Issue-Driven Development with @copilot

This repository uses issue-driven development with GitHub Copilot for automated task processing.

## Quick Start

### Creating a Task for @copilot

1. Go to **Issues** > **New Issue**
2. Select the **Copilot Task** template
3. Fill out:
   - **Task Description** - What should @copilot accomplish?
   - **Acceptance Criteria** - How do we know it's complete?
   - **Context** - Related files, dependencies, background
   - **Priority** - How urgent is this?
4. Submit the issue

The `copilot` label is automatically applied, triggering @copilot processing.

### What Happens Next

1. **Workflow triggers** - `issue-copilot.yml` detects the labeled issue
2. **@copilot acknowledges** - Comment posted confirming processing started
3. **Work begins** - @copilot Workspace creates branch and implements solution
4. **PR created** - Pull request opened, referencing the original issue
5. **Review assigned** - CODEOWNERS auto-assigns repository owner
6. **Merge & close** - Owner reviews, merges, issue auto-closes

## Workflow

```
Issue Created          PR Created           Review & Merge
    │                      │                      │
    ▼                      ▼                      ▼
┌─────────────┐      ┌─────────────┐      ┌─────────────┐
│  Template   │      │  @copilot   │      │  CODEOWNERS │
│  + copilot  │ ───► │  Workspace  │ ───► │  auto-assign│
│   label     │      │             │      │             │
└─────────────┘      └─────────────┘      └─────────────┘
    │                      │                      │
    ▼                      ▼                      ▼
issue-copilot.yml    Branch created        Owner reviews
validates & acks     PR references issue   Merge closes issue
```

## Project Structure

```
.github/
├── ISSUE_TEMPLATE/
│   └── copilot-task.yml       # Task template for @copilot
├── CODEOWNERS                  # Auto-assigns PRs to owner
└── workflows/
    ├── issue-copilot.yml      # Issue processing automation
    └── pr-auto-assign.yml     # PR assignment logging

docs/
└── knowledge/
    ├── README.md              # Knowledge base index
    ├── patterns/              # Reusable code patterns
    ├── decisions/             # Architecture Decision Records
    └── insights/              # Lessons learned
```

## Knowledge Base

The [knowledge base](docs/knowledge/README.md) captures learnings from @copilot development:

- **[Patterns](docs/knowledge/patterns/README.md)** - Reusable solutions
- **[Decisions](docs/knowledge/decisions/README.md)** - Architecture Decision Records
- **[Insights](docs/knowledge/insights/README.md)** - Lessons learned

## Configuration

### CODEOWNERS

Edit `.github/CODEOWNERS` to set the repository owner:

```
# Replace @owner with actual GitHub username
* @your-github-username
```

### Issue Template

The issue template includes:
- Task Description (required)
- Acceptance Criteria (required)
- Context (optional)
- Priority (optional)

## Requirements

- GitHub repository with Actions enabled
- GitHub Copilot Workspace access
- CODEOWNERS configured with valid username

## Troubleshooting

### Issue not being processed

1. Verify the `copilot` label is applied
2. Check Actions tab for workflow runs
3. Review workflow logs for errors

### PR not assigned

1. Verify CODEOWNERS syntax
2. Confirm username exists and has access
3. Check branch protection settings

### @copilot not responding

1. Verify Copilot Workspace is enabled for repo
2. Check issue template completeness
3. Review Copilot usage limits

## Contributing

1. Create an issue using the Copilot Task template
2. Let @copilot process it
3. Review the resulting PR
4. Document insights in the knowledge base
