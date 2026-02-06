# Issue-Driven Development with @copilot

A system for autonomous issue processing using GitHub Copilot.

## Workflow Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Issue-Driven Development                 │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. CREATE ISSUE           2. @COPILOT PROCESSES           │
│  ┌──────────────┐          ┌──────────────────┐            │
│  │ Use template │──────────│ Workflow runs    │            │
│  │ Add 'copilot'│          │ Reads knowledge  │            │
│  │ label        │          │ Generates code   │            │
│  └──────────────┘          └────────┬─────────┘            │
│                                     │                       │
│  4. REVIEW & MERGE         3. PR CREATED                   │
│  ┌──────────────┐          ┌──────────────────┐            │
│  │ Review in    │◄─────────│ Auto-assigned    │            │
│  │ GitHub UI    │          │ via CODEOWNERS   │            │
│  │ Approve/Merge│          │ Links to issue   │            │
│  └──────────────┘          └──────────────────┘            │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### 1. Create an Issue

1. Go to **Issues** > **New Issue**
2. Select **"Copilot Task"** template
3. Fill out the form:
   - **Context**: Background and motivation
   - **Task Description**: What needs to be done
   - **Acceptance Criteria**: How to verify completion
   - **Constraints**: Limitations (optional)
   - **Knowledge References**: Relevant patterns/decisions (optional)
4. Submit the issue

### 2. @copilot Processes the Issue

The workflow automatically:
- Triggers on issues with the `copilot` label
- Checks out the repository
- Reads the knowledge base for context
- Generates code changes
- Creates a branch and commits

### 3. Review the Pull Request

1. Navigate to **Pull Requests**
2. Find the PR linked to your issue
3. Review the changes in GitHub's web UI
4. Provide feedback via comments if needed
5. Approve and merge when satisfied

### 4. Update Knowledge Base (Optional)

After merging, consider adding:
- New patterns to `docs/knowledge/patterns/`
- Decisions to `docs/knowledge/decisions/`
- Insights to `docs/knowledge/insights/`

## Directory Structure

```
.github/
├── ISSUE_TEMPLATE/
│   └── copilot-task.yml     # Issue template for @copilot tasks
├── CODEOWNERS               # Auto-assignment for PRs
└── workflows/
    └── copilot-agent.yml    # Workflow triggered on issue creation

docs/knowledge/
├── index.md                 # Knowledge base overview
├── patterns/                # Reusable code patterns
│   └── README.md
├── decisions/               # Architecture decisions (ADRs)
│   └── README.md
└── insights/                # Learned insights
    └── README.md
```

## Configuration

### CODEOWNERS

Edit `.github/CODEOWNERS` to set PR reviewers:

```
# Default owner for all files
* @your-username

# Team-specific ownership
/src/api/ @api-team
/docs/ @docs-team
```

### Issue Labels

The system uses these labels:
- `copilot` - Triggers @copilot processing
- `copilot-generated` - Applied to PRs created by @copilot

### Workflow Customization

Edit `.github/workflows/copilot-agent.yml` to:
- Add pre-processing steps (linting, tests)
- Customize PR labels and title format
- Add notification integrations

## Best Practices

### Writing Good Issues

1. **Be specific**: Include exact file paths and function names
2. **Provide context**: Explain why the change is needed
3. **Define acceptance**: List testable criteria
4. **Reference knowledge**: Link to relevant patterns/decisions

### Reviewing @copilot PRs

1. **Check alignment**: Does the PR match the issue requirements?
2. **Verify tests**: Are there appropriate tests?
3. **Review patterns**: Does it follow project conventions?
4. **Update knowledge**: Add insights for future reference

## Troubleshooting

### Issue not being processed
- Verify the `copilot` label is applied
- Check workflow runs in Actions tab
- Review workflow logs for errors

### PR not created
- Check if there were actual code changes
- Review the workflow step outputs
- Verify GitHub token permissions

### Wrong assignee on PR
- Update CODEOWNERS with correct patterns
- Ensure usernames are valid GitHub handles
