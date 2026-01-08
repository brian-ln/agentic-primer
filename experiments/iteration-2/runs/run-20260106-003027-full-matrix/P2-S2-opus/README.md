# Issue-Driven Development with @copilot

Automated issue-to-PR workflow using GitHub @copilot with auto-assignment and knowledge capture.

## Quick Start

1. **Create an Issue**
   - Go to Issues > New Issue
   - Select "Copilot Task" template
   - Fill in task details and submit

2. **Watch Automation**
   - GitHub Action triggers automatically
   - @copilot is assigned to the issue
   - @copilot creates a PR with the solution

3. **Review and Merge**
   - PR is auto-assigned to repository owner
   - Review the changes
   - Merge when approved

## How It Works

```
Issue Created → Workflow Triggers → @copilot Assigned → PR Created → Owner Reviews → Merged
```

### Components

| Component | Purpose |
|-----------|---------|
| Issue Template | Structured input for @copilot tasks |
| copilot-issue.yml | Triggers on issue creation, assigns @copilot |
| copilot-pr-assign.yml | Auto-assigns PRs to owner |
| CODEOWNERS | Requests review from owner |
| Knowledge Base | Captures patterns and learnings |

## Repository Structure

```
.github/
├── ISSUE_TEMPLATE/
│   └── copilot-task.yml      # Task template for @copilot
├── workflows/
│   ├── copilot-issue.yml     # Issue processing workflow
│   └── copilot-pr-assign.yml # PR auto-assignment workflow
└── CODEOWNERS                # Review assignment

docs/
└── knowledge/
    ├── README.md             # Knowledge base index
    ├── patterns/             # Reusable code patterns
    ├── decisions/            # Architecture decisions
    └── insights/             # Learnings from @copilot

scripts/
└── validate-system.sh        # System validation
```

## Creating a Task

Use the issue template to create well-structured tasks:

1. **Task Summary**: Brief description (1-2 sentences)
2. **Detailed Description**: Implementation context
3. **Acceptance Criteria**: Checklist of requirements
4. **Priority**: Low / Medium / High / Critical
5. **Task Type**: Feature / Bug Fix / Refactor / Documentation / Test / Chore

## Knowledge Base

The knowledge base captures organizational learning:

- **[Patterns](docs/knowledge/patterns/)**: Reusable solutions
- **[Decisions](docs/knowledge/decisions/)**: Architecture choices (ADRs)
- **[Insights](docs/knowledge/insights/)**: Learnings from @copilot

After each completed issue, consider documenting:
- Patterns that could be reused
- Decisions that should be recorded
- Insights from the @copilot interaction

## Validation

Run the validation script to check system integrity:

```bash
./scripts/validate-system.sh
```

This validates:
- YAML syntax (yamllint)
- Shell script syntax (shellcheck)
- Required files exist

## Labels

| Label | Meaning |
|-------|---------|
| `copilot` | Task is eligible for @copilot processing |
| `in-progress` | @copilot is actively working on this |
| `enhancement` | Default label for new features |

## Workflow Details

### Issue Processing (copilot-issue.yml)

Triggers: `issues.opened`, `issues.edited`, `issues.labeled`

Actions:
1. Check for "copilot" label
2. Post acknowledgment comment
3. Add "in-progress" label
4. Log issue details

### PR Auto-Assignment (copilot-pr-assign.yml)

Triggers: `pull_request.opened`

Actions:
1. Get repository owner
2. Assign PR to owner
3. Post assignment comment

### CODEOWNERS

Automatically requests review from owner for:
- All files (`*`)
- GitHub configuration (`.github/`)
- Documentation (`docs/`)
- Scripts (`scripts/`)

## Contributing

1. Use the issue template for all @copilot tasks
2. Keep issues focused and specific
3. Document patterns discovered during development
4. Record architectural decisions as ADRs
5. Share insights from @copilot interactions
