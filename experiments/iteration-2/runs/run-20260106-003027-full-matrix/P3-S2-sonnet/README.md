# Issue-Driven Development with @copilot

An autonomous development workflow powered by GitHub Issues and @copilot agent.

## Overview

This system enables task automation through a simple workflow:

1. **Create Issue** using structured template
2. **@copilot processes** autonomously
3. **Pull Request** auto-created for review
4. **Human reviews** via GitHub web UI
5. **Merge and learn** - knowledge accumulates

### Key Benefits

- **Zero-friction task creation** - Structured forms guide input
- **Autonomous execution** - @copilot handles implementation
- **Human-in-the-loop quality** - All work reviewed before merge
- **Knowledge accumulation** - Patterns, decisions, and insights captured
- **Full audit trail** - GitHub tracks everything

## Quick Start

### 1. Create a Task

1. Go to **Issues** > **New Issue**
2. Select **"Task for @copilot"** template
3. Fill out the form:
   - Task summary and description
   - Acceptance criteria (specific, testable)
   - Priority and task type
   - Any related files or context
4. Click **Submit new issue**

The issue is automatically assigned to @copilot and processing begins immediately.

### 2. @copilot Processes

@copilot automatically:
- Analyzes task requirements
- Creates feature branch (`copilot/issue-N`)
- Implements solution
- Runs tests and validation
- Creates pull request
- Requests review from @owner (via CODEOWNERS)

You can monitor progress through issue comments and labels.

### 3. Review Pull Request

1. Navigate to the PR created by @copilot
2. Review changes in GitHub web UI:
   - Check code quality and correctness
   - Verify tests are adequate
   - Ensure documentation is updated
3. Request changes if needed or approve
4. Merge when satisfied

### 4. Knowledge Captured

After merge, @copilot may extract:
- **Patterns** - Reusable solutions in `docs/knowledge/patterns/`
- **Decisions** - Architecture choices in `docs/knowledge/decisions/`
- **Insights** - Lessons learned in `docs/knowledge/insights/`

## Workflow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     Developer Creates Issue                  │
│                  (GitHub Issue Template Form)                │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            │ Auto-assigned to @copilot
                            │ Workflow triggered
                            v
┌─────────────────────────────────────────────────────────────┐
│                   @copilot Processes Task                    │
│  • Parse requirements                                        │
│  • Create feature branch                                     │
│  • Implement solution                                        │
│  • Run tests                                                 │
│  • Update documentation                                      │
└───────────────────────────┬─────────────────────────────────┘
                            │
                            │ Creates PR
                            │ Requests review (CODEOWNERS)
                            v
┌─────────────────────────────────────────────────────────────┐
│                    Human Reviews PR                          │
│  • Code quality check                                        │
│  • Test coverage verification                                │
│  • Documentation review                                      │
└───────────────────────────┬─────────────────────────────────┘
                            │
                ┌───────────┴───────────┐
                │                       │
         Approve & Merge         Request Changes
                │                       │
                v                       v
    ┌────────────────────┐    ┌──────────────────┐
    │  Close Issue       │    │  @copilot Updates │
    │  Merge PR          │    │  Additional Work  │
    │  Extract Knowledge │    └────────┬─────────┘
    └────────────────────┘             │
                                       │
                                 (Loop back to review)
```

## System Components

### Issue Template (`.github/ISSUE_TEMPLATE/task.yml`)

Structured YAML form that captures:
- Task summary and detailed description
- Acceptance criteria (testable conditions)
- Priority (P0-P3)
- Task type (feature, bug, refactor, etc.)
- Related files and additional context
- Pre-submission validation checklist

### Workflow Automation (`.github/workflows/issue-assignment.yml`)

GitHub Actions workflow that:
- Triggers on issue open/assign with `copilot-task` label
- Adds status labels and progress comments
- Creates feature branch
- Simulates @copilot processing (calls copilot service in production)
- Commits work and creates PR
- Auto-assigns reviewers via CODEOWNERS

### CODEOWNERS (`.github/CODEOWNERS`)

Automatic PR review assignment:
- `* @owner` routes all PRs to designated reviewer
- Ensures human-in-the-loop without manual assignment
- Can be refined with path-specific owners

### Knowledge Base (`docs/knowledge/`)

Structured documentation that grows over time:

- **`patterns/`** - Reusable solution approaches
  - When to apply, trade-offs, examples
  - Templates for common problems

- **`decisions/`** - Architecture Decision Records (ADRs)
  - What was decided and why
  - Alternatives considered
  - Consequences and trade-offs

- **`insights/`** - Lessons learned
  - Gotchas and surprises
  - Performance findings
  - Best practices discovered

## Configuration

### Prerequisites

- GitHub repository with Actions enabled
- @copilot user/bot account created
- @owner user/team designated for reviews

### Setup Steps

1. **Copy files to repository root:**
   ```bash
   cp -r .github /path/to/your/repo/
   cp -r docs /path/to/your/repo/
   ```

2. **Update usernames in:**
   - `.github/CODEOWNERS` - Replace `@owner` with your reviewer
   - `.github/ISSUE_TEMPLATE/task.yml` - Replace `copilot` assignee if needed

3. **Enable workflow:**
   - Go to **Settings** > **Actions** > **General**
   - Allow workflow permissions: Read and write
   - Allow GitHub Actions to create PRs

4. **Test with sample issue:**
   - Create issue using template
   - Verify workflow triggers
   - Check PR creation

## Examples

### Example 1: Simple Feature Task

**Issue Title:** Add health check endpoint

**Description:**
```
Add a /health endpoint to the API that returns service status.
This is needed for load balancer health monitoring.
```

**Acceptance Criteria:**
```markdown
- [ ] GET /health returns 200 status
- [ ] Response includes {"status": "ok", "timestamp": "..."}
- [ ] Endpoint added to API documentation
- [ ] Integration test added
```

**Result:** @copilot creates endpoint, tests, docs, and PR in ~2 minutes.

### Example 2: Bug Fix Task

**Issue Title:** Fix date parsing timezone issue

**Description:**
```
Dates from API are being parsed incorrectly in different timezones.
User-reported issue: timestamps show wrong day for UTC-8 users.
```

**Acceptance Criteria:**
```markdown
- [ ] All dates consistently use UTC
- [ ] Timezone conversion happens at display layer only
- [ ] Existing date fields maintain backward compatibility
- [ ] Test cases cover multiple timezones (UTC, UTC-8, UTC+5)
```

**Result:** @copilot fixes parsing, adds tests, updates 3 affected components.

## Troubleshooting

### Workflow doesn't trigger

**Check:**
- Issue has `copilot-task` label OR is assigned to `copilot`
- Actions are enabled in repository settings
- Workflow file has correct YAML syntax (run `yamllint`)

**Fix:**
```bash
yamllint .github/workflows/issue-assignment.yml
```

### PR not auto-assigned for review

**Check:**
- CODEOWNERS file exists in repository root
- Username in CODEOWNERS matches GitHub account exactly
- File has no syntax errors

**Fix:**
```bash
# Verify CODEOWNERS is recognized
gh api repos/{owner}/{repo}/codeowners/errors
```

### @copilot processing fails

**Check:**
- Issue template fields are all filled out
- Acceptance criteria are specific and testable
- No conflicting branches exist

**Debug:**
- View workflow run logs: **Actions** > **Copilot Issue Assignment**
- Check issue comments for error details

## Advanced Usage

### Custom Labels

Add labels to issues for automated routing:
- `priority-critical` - Expedited processing
- `needs-review-architecture` - Tag architecture team
- `backend`, `frontend`, `infra` - Component-specific routing

### Multi-step Tasks

For complex tasks:
1. Create epic issue as parent
2. Create sub-tasks as separate issues
3. Reference parent in description: "Part of #123"
4. @copilot processes each independently

### Knowledge Base Integration

After completing related tasks:
1. Review PRs for patterns
2. Extract to `docs/knowledge/patterns/pattern-name.md`
3. Link from pattern README
4. @copilot can reference in future tasks

## Validation

### Syntax Validation

```bash
# Validate YAML syntax
yamllint .github/ISSUE_TEMPLATE/task.yml
yamllint .github/workflows/issue-assignment.yml

# Validate CODEOWNERS syntax
gh api repos/{owner}/{repo}/codeowners/errors
```

### Functional Testing

1. Create test issue with simple task
2. Verify workflow triggers and completes
3. Check PR is created with correct reviewers
4. Test approval and merge flow

### Success Metrics

- ✅ Issue-to-PR time < 5 minutes (for simple tasks)
- ✅ PR auto-assigned to correct reviewers
- ✅ All tests pass in @copilot PRs
- ✅ Knowledge base grows over time
- ✅ Zero manual intervention for standard tasks

## FAQ

**Q: Can @copilot handle any task?**
A: @copilot works best on well-defined, scoped tasks with clear acceptance criteria. Complex architectural decisions should involve humans.

**Q: What if @copilot makes mistakes?**
A: The PR review process catches errors. Request changes, @copilot will update the PR.

**Q: How do I stop @copilot from processing an issue?**
A: Remove the `copilot-task` label and unassign @copilot.

**Q: Can multiple @copilot instances work in parallel?**
A: Yes, as long as they work on different issues/branches. Use branch naming convention to avoid conflicts.

**Q: How is the knowledge base used?**
A: @copilot references patterns and decisions when implementing similar tasks. It helps maintain consistency and avoid repeating mistakes.

## Contributing

### Improving Templates

Issue template improvements:
1. Add fields that help @copilot understand tasks better
2. Add validation for common mistakes
3. Update based on user feedback

### Extending Workflows

Add workflow steps:
- Code quality checks (linting, formatting)
- Security scanning
- Performance testing
- Deployment previews

### Growing Knowledge Base

Contribute patterns, decisions, and insights:
1. Create file in appropriate directory
2. Follow the documented format
3. Link from directory README
4. Submit PR for review

## License

[Specify your license here]

## Support

- **Issues:** [GitHub Issues](https://github.com/your-org/your-repo/issues)
- **Discussions:** [GitHub Discussions](https://github.com/your-org/your-repo/discussions)
- **Documentation:** `docs/knowledge/`

---

**Status:** Production Ready ✅
**Last Updated:** 2026-01-06
**Maintained by:** @owner, @copilot
