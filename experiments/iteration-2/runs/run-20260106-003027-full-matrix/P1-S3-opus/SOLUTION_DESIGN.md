# @copilot Issue Automation Solution Design

## Prompt Analysis

**Original Prompt (P1 - 10 words):**
> Bootstrap @copilot issue automation with auto-review and knowledge base.

**Success Criteria (S3 - Comprehensive - 7 Observable Outcomes):**
1. Functional Test: System processes test issue end-to-end without errors
2. Syntax Valid: All generated files pass automated validation (yamllint, shellcheck, markdownlint)
3. Observable Behavior: GitHub workflow actually triggers on issue creation
4. Reliability: 90%+ success rate across 20+ test runs
5. Multi-Agent: Works with >=3 different AI agents (Opus, Sonnet, Haiku)
6. Single-Command: Bootstrap completes from bare repo with zero manual intervention
7. Self-Improvement: System creates >=3 successful improvement PRs from its own logs

---

## Solution Architecture

### System Overview

```
                    +------------------+
                    |   GitHub Issue   |
                    |  (with @copilot) |
                    +--------+---------+
                             |
                             v
                    +------------------+
                    | Issue Workflow   |
                    | (on: issues)     |
                    +--------+---------+
                             |
            +----------------+----------------+
            |                                 |
            v                                 v
   +------------------+            +------------------+
   | @copilot Agent   |            | Knowledge Base   |
   | (via label/      |            | Auto-Update      |
   |  assignment)     |            +------------------+
   +--------+---------+
            |
            v
   +------------------+
   | Pull Request     |
   | (auto-created)   |
   +--------+---------+
            |
            v
   +------------------+
   | Auto-Review      |
   | Workflow         |
   +--------+---------+
            |
            v
   +------------------+
   | Self-Improvement |
   | Analyzer         |
   +------------------+
```

### Core Components

1. **Issue Template** - Structured input for @copilot tasks
2. **Issue Workflow** - Triggers on issue creation, labels for @copilot
3. **CODEOWNERS** - Auto-assigns PRs to maintainers
4. **Auto-Review Workflow** - Validates PRs and runs checks
5. **Knowledge Base** - Stores patterns, decisions, insights
6. **Self-Improvement Analyzer** - Creates improvement PRs from logs
7. **Bootstrap Script** - Single-command setup from bare repo

---

## Files to Create

### GitHub Configuration Files

| File | Purpose |
|------|---------|
| `.github/ISSUE_TEMPLATE/copilot-task.yml` | Structured issue template for @copilot |
| `.github/workflows/issue-copilot.yml` | Main issue automation workflow |
| `.github/workflows/pr-auto-review.yml` | Automatic PR review workflow |
| `.github/CODEOWNERS` | PR auto-assignment |
| `.github/copilot-instructions.md` | Agent-specific instructions |

### Knowledge Base Structure

| File | Purpose |
|------|---------|
| `docs/knowledge/README.md` | Knowledge base index |
| `docs/knowledge/patterns/README.md` | Reusable patterns |
| `docs/knowledge/patterns/issue-workflow.md` | Issue processing pattern |
| `docs/knowledge/decisions/README.md` | Architecture decisions |
| `docs/knowledge/decisions/001-copilot-automation.md` | First ADR |
| `docs/knowledge/insights/README.md` | Learned insights |
| `docs/knowledge/insights/agent-performance.md` | Performance tracking |

### Scripts

| File | Purpose |
|------|---------|
| `scripts/bootstrap.sh` | Single-command bootstrap |
| `scripts/analyze-logs.sh` | Extract insights from logs |
| `scripts/create-improvement-pr.sh` | Generate improvement PRs |
| `scripts/verify-system.sh` | Validate all components |
| `scripts/run-test-issue.sh` | End-to-end test runner |

### Configuration

| File | Purpose |
|------|---------|
| `README.md` | Project overview with workflow |
| `.copilot-config.yml` | Agent configuration |

---

## Verification Mapping to Success Criteria

| Success Criterion | Verification Method | Files Involved |
|-------------------|---------------------|----------------|
| 1. Functional Test | `scripts/run-test-issue.sh` | All workflows |
| 2. Syntax Valid | `scripts/verify-system.sh` | All YAML, Shell, MD |
| 3. Observable Behavior | GitHub Actions log | issue-copilot.yml |
| 4. Reliability | Multiple test runs | run-test-issue.sh |
| 5. Multi-Agent | Agent config matrix | .copilot-config.yml |
| 6. Single-Command | `scripts/bootstrap.sh` | bootstrap.sh |
| 7. Self-Improvement | analyze-logs.sh + PR | analyze-logs.sh, create-improvement-pr.sh |

---

## Implementation Details

### Issue Processing Flow

1. User creates issue using `copilot-task.yml` template
2. `issue-copilot.yml` workflow triggers
3. Workflow validates issue structure
4. Adds `copilot-ready` label
5. @copilot agent picks up issue (via GitHub Copilot or external agent)
6. Agent creates PR
7. `pr-auto-review.yml` validates PR
8. CODEOWNERS auto-assigns reviewers
9. On merge, knowledge base updated

### Auto-Review Checks

- Syntax validation (yamllint, shellcheck, markdownlint)
- Test execution
- Knowledge base consistency
- Link validation

### Self-Improvement Loop

1. `analyze-logs.sh` runs on schedule (daily)
2. Extracts patterns from:
   - Issue completion times
   - Common failure modes
   - Agent performance metrics
3. Generates improvement proposals
4. `create-improvement-pr.sh` creates PRs for:
   - Issue template refinements
   - Workflow optimizations
   - Knowledge base updates

---

## Agent Compatibility

System designed to work with:

| Agent | Integration Method |
|-------|-------------------|
| GitHub Copilot | Native issue assignment |
| Claude (Opus/Sonnet/Haiku) | Via copilot-instructions.md |
| Other AI Agents | Standard issue/PR interface |

---

## Bootstrap Sequence

```bash
./scripts/bootstrap.sh
```

Performs:
1. Create directory structure
2. Generate all configuration files
3. Initialize knowledge base
4. Run verification suite
5. Create test issue (dry-run)
6. Report success/failure

---

## Assumptions

1. Repository has GitHub Actions enabled
2. @copilot has write access to repository
3. GitHub API token available for automation
4. Repository uses standard branch protection

---

## Next Steps After Bootstrap

1. Enable GitHub Actions in repository settings
2. Configure @copilot access permissions
3. Create first real issue
4. Monitor workflow execution
5. Review and merge first PR
