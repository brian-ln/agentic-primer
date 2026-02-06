# ADR-001: Event-Driven Architecture for Autonomous Task Processing

## Status
ACCEPTED

## Context

The goal is to create an AI-executable workspace where humans create GitHub issues and @copilot autonomously generates pull requests with implementations.

We need a trigger mechanism that:
1. Detects when a new task issue is created
2. Invokes an AI agent to process the task
3. Creates a PR with the implementation
4. Maintains an audit trail

### Options Considered

1. **Event-Driven (GitHub Actions)** - Trigger on issue creation, invoke @copilot API
2. **Polling** - Periodically check for new issues
3. **Webhook** - External service listens for GitHub webhooks
4. **Manual** - Humans run a command to process issues

## Decision

We chose **Event-Driven Architecture** using GitHub Actions as the orchestration layer.

### Why This Decision

#### Advantages
- **Automatic**: No manual intervention needed
- **Scalable**: GitHub Actions handles concurrency
- **Native**: Built into GitHub, no external services
- **Auditable**: Full execution logs in GitHub Actions tab
- **Secure**: Uses GitHub's native auth (no external credentials)
- **Cost-effective**: Free tier supports substantial workloads
- **Observable**: Easy to see what the workflow did via logs

#### Disadvantages
- **Limited to 15 minutes**: Long-running tasks may timeout
- **GitHub-dependent**: If GitHub Actions is down, system is down
- **Workflow YAML syntax**: Can be verbose
- **Debugging**: Limited to GitHub's UI (no local execution)

## Architecture

```
GitHub Issue Created
  ↓
[GitHub Actions Triggered by 'issues.opened' event]
  ↓
[Parse Issue Template YAML]
  ↓
[Invoke @copilot Agent API]
  ↓
[Agent processes task & generates implementation]
  ↓
[Workflow commits changes to new branch]
  ↓
[Workflow creates Pull Request]
  ↓
[Post comment on original issue]
  ↓
[Update knowledge base with execution log]
```

## Implementation Details

### Workflow File: `.github/workflows/issue-to-pr.yml`

**Trigger**: `on: issues: types: [opened, reopened]`

**Key Steps**:
1. Parse issue body as YAML
2. Validate required fields
3. Create feature branch
4. Generate implementation files
5. Commit to branch
6. Create PR
7. Log execution to knowledge base

### Issue Template: `.github/ISSUE_TEMPLATE/task.yml`

**Structure**: YAML form with:
- Task description (required)
- Task type enum (required)
- Success criteria (required)
- Complexity level (required)
- Priority enum (required)
- Notes (optional)
- Reference links (optional)

This ensures consistent, machine-readable task definitions.

### @copilot Integration

The workflow invokes @copilot (simulated) with:
- Issue title + body
- Task type
- Success criteria
- Reference links

@copilot responds with:
- Pull request content (files + code)
- Test cases
- Documentation
- Learning logs

## Consequences

### Positive
- ✓ Fully autonomous task processing
- ✓ No manual PR creation needed
- ✓ Audit trail built in
- ✓ Consistent workflow for all tasks
- ✓ Works with any AI model (Opus, Sonnet, Haiku)
- ✓ Knowledge base grows automatically

### Negative
- ✗ 15-minute timeout limit (long tasks need chunking)
- ✗ Depends on GitHub Actions availability
- ✗ YAML workflow files can be complex
- ✗ Debugging requires checking GitHub UI

### Mitigation
- Split large tasks into smaller subtasks
- Add retry logic for transient failures
- Document workflow YAML extensively
- Add issue comments explaining failures

## Related Decisions

- ADR-002: YAML Issue Template Format
- ADR-003: Knowledge Base Structure

## Alternatives Rejected

### A: Polling Service
```
Rejected because:
- Requires external infrastructure
- More complex to maintain
- Higher latency (check interval)
- Less reliable than event-driven
```

### B: Webhook Service
```
Rejected because:
- Requires external server
- More security surface area
- Overkill for current scale
- GitHub Actions is sufficient
```

### C: Manual Command
```
Rejected because:
- Defeats purpose of autonomy
- Requires human intervention
- Not scalable to many tasks
```

## Tests / Verification

To verify this architecture works:

1. **Create test issue** via `.github/ISSUE_TEMPLATE/task.yml` template
2. **Observe workflow execution** in GitHub Actions tab
3. **Verify PR created** with correct branch name and content
4. **Check knowledge logs** updated in `docs/knowledge/insights/`
5. **Run syntax validation** (yamllint, shellcheck) on generated files

All 5 checks must pass to consider architecture verified.

## References

- GitHub Actions Workflow Syntax: https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions
- GitHub Issue Templates: https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/configuring-issue-template-routing
- YAML Specification: https://yaml.org/spec/

## Author
@copilot (AI Agent)

## Date
2026-01-06

## Review Status
Pending team review
