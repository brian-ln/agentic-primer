# ADR-001: GitHub Actions Workflow for Issue Processing

**Status**: Accepted
**Date**: 2026-01-06
**Deciders**: @copilot Architecture Team
**Affects**: All issue-driven development workflows

---

## Context

The system needs to automatically process GitHub issues and create pull requests without manual intervention. Multiple approaches are possible:

1. **GitHub Actions** (chosen): Event-driven, integrated, no additional infrastructure
2. **External CI/CD**: Jenkins, GitLab CI, CircleCI
3. **Webhooks + Lambda**: Serverless, but requires cloud account
4. **Manual Process**: Human processes issues
5. **Bot Framework**: Probot, GitHub App

### Requirements
- Automatic detection of task issues
- Immediate processing (within seconds)
- Full audit trail
- Works with multiple AI agents
- No additional infrastructure cost
- Works on fresh repositories

### Constraints
- No external services allowed
- Must work in open-source context
- No special GitHub permissions beyond write access

## Decision

**Use GitHub Actions as the primary event trigger and orchestration engine.**

### Workflow Design
```
Issue Created/Labeled
    ↓
GitHub Actions Triggered (.github/workflows/issue-agent.yml)
    ↓
Checkout Repo + Setup Environment
    ↓
Parse Issue (title, body, labels)
    ↓
Create Feature Branch (ai/issue-<number>)
    ↓
Agent Processing (simulated or API call)
    ↓
Create Pull Request
    ↓
Log Execution Metrics
    ↓
CODEOWNERS Routes PR to Owner
```

### Key Decisions Within This Architecture

**1. Trigger Event: Issues opened/labeled/edited**
```yaml
on:
  issues:
    types: [opened, labeled, edited]
```

Rationale:
- `opened`: Detect new issues immediately
- `labeled`: Allow manual triggering by adding `ai-task` label
- `edited`: Support retrying if issue is updated
- Alternative considered: Push trigger (rejected - less intuitive)

**2. Filtering: Only process `ai-task` labeled issues**
```yaml
if: contains(github.event.issue.labels.*.name, 'ai-task')
```

Rationale:
- Explicit opt-in (doesn't process every issue)
- Label-based filtering is visible in issue UI
- Easy for humans to control
- Alternative: Comment trigger (rejected - less discoverable)

**3. Feature Branches: Isolate changes per issue**
```
ai/issue-<number>
```

Rationale:
- Clear naming convention
- Multiple issues can be processed in parallel
- Easy to identify which issue each PR belongs to
- Alternatives considered:
  - Single branch: Too fragile (concurrent issues conflict)
  - Random names: Hard to trace (violates auditability)

**4. Pull Request Auto-Assignment: Via CODEOWNERS**
```
CODEOWNERS file routing based on file paths
```

Rationale:
- Automatic without requiring team setup
- Works with both individuals and teams
- Respects repository structure
- Alternative: GitHub teams (requires more setup)

**5. Logging Strategy: Append-only JSONL format**
```
AGENT_LOG.jsonl
{"timestamp":"...", "event":"...", "issue_number":..., "status":"..."}
```

Rationale:
- Immutable history (append-only)
- Compact format (JSONL is 80-90% smaller than JSON arrays)
- Analyzable by simple scripts
- Alternatives considered:
  - CSV: Less expressive
  - Database: Too heavyweight
  - Text log: Not machine-readable

## Consequences

### Positive
- **Zero infrastructure**: No external services needed
- **Fast**: Issues processed within seconds
- **Audit trail**: Full execution history in git
- **Scalable**: Handles any number of concurrent issues
- **Compatible**: Works with any Claude model
- **Observable**: Actions tab shows execution status
- **Reversible**: Can disable workflow by removing label
- **No costs**: GitHub Actions free tier sufficient

### Negative
- **GitHub-specific**: Tightly coupled to GitHub platform
  - Mitigation: Could adapt to GitLab CI (similar capability)
- **Limited execution time**: 6-hour timeout per workflow
  - Mitigation: Not a practical limit for code tasks
- **Secrets management**: Need to configure API credentials
  - Mitigation: Documented in bootstrap process
- **Learning curve**: Agents need to understand GitHub Actions
  - Mitigation: Provided in AGENT_INSTRUCTIONS.md

### Trade-offs

| Option | Cost | Speed | Complexity | Auditability |
|--------|------|-------|-----------|--------------|
| GitHub Actions | Free | 5sec | Low | Excellent |
| External CI | $$ | 10sec | Medium | Good |
| Webhooks+Lambda | $$ | 2sec | High | Fair |
| Manual | Free | Hours | Very Low | Excellent |

**Chosen: GitHub Actions** balances all factors.

## Implementation Notes

### Workflow File Location
```
.github/workflows/issue-agent.yml
```

Why this location:
- Standard GitHub convention
- Automatically discovered by GitHub
- Version controlled with repository
- Human-readable for auditing

### Execution Context Provided to Agent

The workflow passes:
```yaml
env:
  ISSUE_BODY: ${{ github.event.issue.body }}
  ISSUE_NUMBER: ${{ github.event.issue.number }}
  ISSUE_TITLE: ${{ github.event.issue.title }}
  ISSUE_AUTHOR: ${{ github.event.issue.user.login }}
```

Agent can also access:
- `.git/config` - repository configuration
- `docs/knowledge/` - knowledge base
- `AGENT_LOG.jsonl` - execution history
- All source files - entire repository context

### Failure Handling

If workflow fails:
1. Execution stops
2. Error logged to AGENT_LOG.jsonl
3. Issue remains open (not auto-closed)
4. Maintainer can review action logs
5. Can retry by re-labeling issue

### Concurrent Execution

Multiple issues can be processed simultaneously:
- Each issue gets unique branch: `ai/issue-123`, `ai/issue-124`
- Each issue creates separate PR: #456, #457
- No conflicts (different files/branches)
- Parallelization benefit: 3 issues → 3x throughput

## Related Decisions

- **ADR-002**: Agent selection strategy (which model to use)
- **ADR-003**: Knowledge base structure (patterns/decisions/insights)
- **Pattern**: Issue processing pattern (how agents execute)

## Alternatives Considered & Rejected

### 1. External CI System (Jenkins, CircleCI)
**Status**: Rejected
**Reason**: Requires infrastructure setup, adds operational burden, costs money
**Use if**: Team already has CI infrastructure in place

### 2. GitHub App + Webhooks
**Status**: Rejected
**Reason**: More complex setup, requires separate hosting, overkill for this use case
**Use if**: Need sophisticated authentication or private data access

### 3. GitOps (Deploy on merge)
**Status**: Rejected (considered as alternative to workflow)
**Reason**: Doesn't fit issue-triggered model, better for code-change triggers
**Use if**: Processing code changes (not issues) triggers tasks

### 4. Scheduled Workflow (Cron)
**Status**: Rejected
**Reason**: Would need polling mechanism, higher latency, not event-driven
**Use if**: Need batch processing of multiple issues

## Success Metrics

This decision is successful if:
- [ ] Workflow triggers within 5 seconds of issue creation
- [ ] Issue processing completes within 10 minutes
- [ ] 95%+ of workflows complete successfully
- [ ] Execution logs are complete and analyzable
- [ ] Works consistently across multiple test runs
- [ ] New team members understand flow quickly

## Review Schedule

Revisit this decision when:
- GitHub Changes Actions pricing model
- New CI/CD platform becomes significantly better
- Team size exceeds 50+ simultaneous issues
- Execution time consistently exceeds 30 minutes

---

**Last Updated**: 2026-01-06
**Next Review**: 2026-02-01 (30-day check-in)
**Author**: @copilot Architecture
**Approval**: Accepted and implemented
