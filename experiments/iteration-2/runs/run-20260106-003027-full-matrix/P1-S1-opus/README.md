# @copilot Issue Automation

Enable @copilot to process GitHub issues with automated code review and knowledge capture.

## Prerequisites

Before using this system:

1. **GitHub Copilot Enterprise** subscription required for repository
2. **Configure CODEOWNERS** - Replace `@OWNER` placeholder with actual GitHub username
3. **Enable branch protection** - Require CODEOWNERS review for PRs
4. **Node.js project** - Workflows expect `package.json` with `lint` and `test` scripts

## Quick Start

### Creating a Task for @copilot

1. Go to **Issues** > **New Issue**
2. Select **Copilot Task** template
3. Fill in:
   - **Task Description**: What should @copilot do?
   - **Acceptance Criteria**: How do we know it's done?
   - **Context** (optional): Related files or dependencies
4. Click **Submit new issue**
5. **Manually assign @copilot** as assignee to begin processing

### Workflow

```
Issue Created ──► Label 'copilot' ──► Workflow adds comment
                                              │
                                              ▼
                                     Manual: Assign @copilot
                                              │
                                              ▼
                                     @copilot processes issue
                                              │
                                              ▼
                                        Creates PR
                                              │
                                              ▼
                       ┌──────────────────────┴──────────────────────┐
                       ▼                                             ▼
              Auto-Review Workflow                           CODEOWNERS assigns
              (lint, test, audit)                            human reviewer
                       │                                             │
                       └──────────────────┬──────────────────────────┘
                                          ▼
                                    Human Review
                                          │
                                          ▼
                                        Merge
```

## Components

### Issue Template

`.github/ISSUE_TEMPLATE/copilot-task.yml`

Structured form with:
- Task description (required)
- Acceptance criteria (required, with defaults)
- Context (optional)
- Priority and complexity dropdowns

### Copilot Configuration

`.github/copilot-instructions.md` - Behavioral guidance for @copilot
`.github/copilot-setup-steps.yml` - Environment configuration

### GitHub Actions

**Issue Preparation** (`.github/workflows/copilot-issue.yml`)
- Triggers when issue has `copilot` label
- Adds guidance comment with next steps

**Auto-Review** (`.github/workflows/copilot-review.yml`)
- Triggers on PR creation/update
- Runs linting, tests, security audit
- Adds review summary with check results

### CODEOWNERS

`.github/CODEOWNERS`

Assigns human reviewers to PRs. **You must replace `@OWNER` with your GitHub username.**

### Knowledge Base

`docs/knowledge/`

```
docs/knowledge/
├── README.md        # Index and quick start
├── patterns/        # Reusable code patterns
├── decisions/       # Architecture Decision Records
└── insights/        # Lessons learned
```

## Known Limitations

1. **@copilot assignment is manual** - No API to programmatically assign @copilot
2. **Actions require approval** - Bot-triggered workflows need manual approval
3. **CODEOWNERS placeholder** - Must configure with real username
4. **Knowledge base is manual** - Requires human curation

## Usage Examples

### Example: Add a new feature

```markdown
## Task Description
Add a `/health` endpoint that returns JSON status.

## Acceptance Criteria
- [ ] GET /health returns 200
- [ ] Response includes `{"status": "ok", "timestamp": "..."}`
- [ ] Unit test covers endpoint

## Context
API is in `src/api/` using Express.js
```

### Example: Fix a bug

```markdown
## Task Description
Fix null pointer exception in user lookup when email is undefined.

## Acceptance Criteria
- [ ] Handle undefined email gracefully
- [ ] Return appropriate error message
- [ ] Add regression test

## Context
Bug reported in #42, occurs in `src/services/user.js`
```

## Knowledge Base

After completing tasks, capture learnings in `docs/knowledge/`:

| Section | Purpose | When to Add |
|---------|---------|-------------|
| **Patterns** | Reusable solutions | Found a generalizable approach |
| **Decisions** | ADRs | Made architectural choice |
| **Insights** | Lessons learned | Discovered something unexpected |

Browse the knowledge base before starting new work to avoid re-solving problems.
