# Pattern: Issue Workflow

## Context

Use this pattern when processing issues through the @copilot automation system.

## Problem

Issues need consistent handling from creation to completion. Without a defined workflow:
- Issues may be missed or duplicated
- Status is unclear
- Handoffs between human and agent are messy
- Learning opportunities are lost

## Solution

A structured workflow with clear states, triggers, and transitions.

## Workflow States

```
Created → Validated → Ready → In Progress → PR Created → Reviewed → Completed
   │          │         │         │             │           │
   └──────────┴─────────┴─────────┴─────────────┴───────────┘
                    (can fail/return at any point)
```

### State Descriptions

| State | Label | Description |
|-------|-------|-------------|
| Created | none | Issue just opened |
| Validated | `copilot-task` | Issue uses correct template |
| Ready | `copilot-ready` | All required fields present |
| In Progress | `copilot-in-progress` | Agent working on issue |
| PR Created | `copilot-pr-created` | PR exists for this issue |
| Reviewed | `copilot-reviewed` | PR has been reviewed |
| Completed | `copilot-completed` | Issue closed via PR merge |

## Implementation

### 1. Issue Creation

User creates issue using the `copilot-task.yml` template:

```markdown
## Task Description
[What needs to be done]

## Acceptance Criteria
- [ ] Criterion 1
- [ ] Criterion 2

## Additional Context
[Any relevant context]
```

### 2. Automatic Validation

`issue-copilot.yml` workflow triggers and:

1. Checks for required sections
2. Validates template structure
3. Adds appropriate labels
4. Comments with status

### 3. Agent Processing

When issue has `copilot-ready` label:

1. Agent analyzes issue
2. Creates implementation branch
3. Makes changes
4. Opens PR with "Closes #N" reference

### 4. Auto-Review

`pr-auto-review.yml` workflow:

1. Runs syntax validation
2. Executes tests
3. Generates review summary
4. Updates knowledge base logs

### 5. Human Review

Reviewer:

1. Reviews PR changes
2. Checks acceptance criteria
3. Approves or requests changes
4. Merges when ready

### 6. Completion

On merge:

1. Issue auto-closes (via "Closes #N")
2. `copilot-completed` label added
3. Logs updated
4. Metrics recorded

## Examples

### Successful Flow

```
1. User creates: [Task]: Add retry logic to API client
2. Workflow validates → adds labels
3. @copilot picks up → creates branch copilot/42-retry-logic
4. @copilot opens PR → references issue
5. Auto-review runs → syntax valid, tests pass
6. Human reviews → approves
7. Merge → issue closes
```

### Failed Validation

```
1. User creates: [Task]: Fix bug (missing template)
2. Workflow validates → fails
3. Comment added with required sections
4. User edits issue → re-validates
5. Labels added → continues normal flow
```

## Related

- [ADR-001: Copilot Automation](../decisions/001-copilot-automation.md)
- [PR Review Patterns](pr-review.md)
