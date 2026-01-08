# Issue-Driven Development with @copilot

**Autonomous issue-driven development system where GitHub Issues become executable work items.**

Turn GitHub issues into pull requests automatically. Create an issue → @copilot analyzes it → PR appears for review.

---

## Quick Start

### 1. Create an Issue

Go to **Issues** → **New Issue** and select the **"Development Task"** template:

1. **Task Title:** Clear description of what needs to be done
2. **Description:** Detailed explanation with context
3. **Acceptance Criteria:** Specific, observable criteria for success
4. **Priority:** P0-P3 urgency level
5. **Effort:** Estimated time to complete
6. **Skills Required:** Type of work (Frontend, Backend, etc.)
7. **Dependencies:** Any blocking issues
8. **Additional Notes:** Links, context, constraints

**Example:**
```
Title: Add caching to API responses

Description:
## Background
API responses are slow, hitting database on every request

## What needs to be done
Implement Redis caching layer for GET requests with 5-minute TTL

## Context
See #42 for performance analysis

Acceptance Criteria:
- [ ] GET /api/users caches successfully
- [ ] Cache invalidates after 5 minutes
- [ ] Tests pass with 95%+ coverage
- [ ] Documentation updated

Priority: P1 (High)
Effort: 3-8 hours
Skills: Backend
```

### 2. Trigger @copilot

Choose one of these to activate @copilot:

- **Automatic:** Create with `copilot-task` label (enabled by default)
- **Manual:** Comment `@copilot analyze` on any issue
- **Assign:** Assign the issue to @copilot

### 3. Watch the Workflow

@copilot will:

1. ✓ Acknowledge receipt (comment posted)
2. ✓ Parse acceptance criteria
3. ✓ Check knowledge base for patterns
4. ✓ Create feature branch: `copilot/issue-NNN`
5. ✓ Implement solution (in production)
6. ✓ Create pull request with auto-populated description
7. ✓ Tag reviewers from CODEOWNERS

**Status updates** posted as comments throughout.

### 4. Review and Merge

1. Check the PR in the **Pull Requests** tab
2. Review changes and tests
3. Request changes or approve
4. Merge when ready

---

## How It Works

### Workflow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│ 1. Issue Created (using Development Task template)          │
│    - Title, description, acceptance criteria, etc.          │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ 2. GitHub Actions Triggered                                 │
│    - on: [opened, labeled, assigned, comment]               │
│    - Checks for copilot-task label or @copilot mention      │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ 3. @copilot Receives Task                                   │
│    - Posts acknowledgment comment                           │
│    - Adds "in-progress" label                               │
│    - Creates feature branch: copilot/issue-NNN              │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ 4. Analysis Phase                                           │
│    - Parse acceptance criteria                              │
│    - Search docs/knowledge/ for relevant patterns           │
│    - Estimate complexity                                    │
│    - Plan implementation                                    │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ 5. Implementation Phase                                     │
│    - Write code on feature branch                           │
│    - Run tests                                              │
│    - Update documentation                                   │
│    - Commit changes                                         │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ 6. Create Pull Request                                      │
│    - Title: Implementation of: [issue title]                │
│    - Body: Auto-populated from issue description            │
│    - Links to issue: Fixes #NNN                             │
│    - Assignees: From CODEOWNERS                             │
│    - Labels: From issue                                     │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ 7. Review Phase (Manual)                                    │
│    - Repository owner (from CODEOWNERS) notified            │
│    - Reviews code in web UI                                 │
│    - Requests changes or approves                           │
│    - @copilot resolves feedback (if iterative)              │
└────────────────────┬────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────────────────┐
│ 8. Merge & Close                                            │
│    - Merge PR to main branch                                │
│    - Close associated issue                                 │
│    - Opportunity to document patterns discovered            │
└─────────────────────────────────────────────────────────────┘
```

### Key Components

**Issue Template** (`.github/ISSUE_TEMPLATE/task.yml`)
- Standardizes how issues are written
- Captures priority, effort, skills required
- Auto-labels with `copilot-task`

**Workflow** (`.github/workflows/copilot-task.yml`)
- Listens for issue events
- Orchestrates the complete pipeline
- Posts status updates
- Creates branch and PR

**CODEOWNERS** (`.github/CODEOWNERS`)
- Routes PRs to appropriate reviewers
- Ensures code review doesn't get lost
- Configurable by team/path

**Knowledge Base** (`docs/knowledge/`)
- Captures patterns from completed tasks
- ADRs for architectural decisions
- Insights for lessons learned
- Referenced during task analysis

---

## Requirements

✅ GitHub repository with Actions enabled
✅ Basic GitHub knowledge (issues, PRs, labels)
✅ Repository owner configured in CODEOWNERS

**No external subscriptions or APIs required** - this runs entirely on GitHub Actions.

---

## Setup Checklist

Verify these files exist before using:

- [ ] `.github/ISSUE_TEMPLATE/task.yml` - Issue template
- [ ] `.github/CODEOWNERS` - Review assignment (edit with your username!)
- [ ] `.github/workflows/copilot-task.yml` - Main workflow
- [ ] `.github/workflows/validate-system.yml` - Health checks
- [ ] `.github/pull_request_template.md` - PR auto-population
- [ ] `docs/knowledge/README.md` - Knowledge base guide
- [ ] `docs/knowledge/{patterns,decisions,insights}/` - KB directories
- [ ] `README.md` - This documentation
- [ ] `scripts/verify-bootstrap.sh` - Verification script

Run this to verify:
```bash
./scripts/verify-bootstrap.sh
```

---

## Important Configuration

### Set Your Username in CODEOWNERS

Edit `.github/CODEOWNERS` and replace `@owner` with your GitHub username:

**Before:**
```
* @owner
```

**After:**
```
* @alice
```

This ensures PRs are assigned to you for review.

---

## First Test Issue

Ready to test? Create an issue using the template:

**Title:** Test: Verify Copilot Workflow

**Description:**
```
## Background
Testing that the @copilot automation system works end-to-end.

## What needs to be done
Create a simple verification that the workflow triggered correctly.

## Context
This is a test of the issue-driven development system.
```

**Acceptance Criteria:**
- [ ] @copilot acknowledges task
- [ ] Feature branch created
- [ ] PR created for review

**Priority:** P2
**Effort:** 1-2 hours
**Skills:** Testing

---

## Using the Workflow

### Typical Day-to-Day

```bash
# 1. Team member creates issue
#    (Goes to GitHub Issues → New Issue → Development Task)

# 2. @copilot processes automatically
#    (Workflow runs, posts status in issue)

# 3. You get notified about PR
#    (GitHub sends notification, or check Pull Requests tab)

# 4. Review in GitHub web UI
#    - Check "Files changed" tab
#    - Leave comments or approve
#    - Merge when ready

# 5. @copilot closes the issue
#    (Happens automatically when PR merges)
```

### Triggering On-Demand

If you want to trigger @copilot on an existing issue:

**Option 1: Add label**
```bash
# In GitHub UI: Issues → [issue] → Labels → copilot-task
```

**Option 2: Comment mention**
```
@copilot analyze
```

**Option 3: Assign to copilot**
```bash
# In GitHub UI: Issues → [issue] → Assignees → copilot
```

---

## Knowledge Base Integration

### Referencing Patterns in Issues

When creating an issue, reference relevant patterns:

```markdown
## References
- Pattern: See `docs/knowledge/patterns/github-actions-caching.md`
- Decision: See `docs/knowledge/decisions/ADR-003-api-design.md`
- Constraint: See `docs/knowledge/insights/rate-limit-gotchas.md`
```

This helps @copilot understand context.

### Documenting Solutions

After closing an issue (post-merge), create patterns:

1. Did you solve a reusable problem?
   → Create `docs/knowledge/patterns/NAME.md`

2. Did you make an architectural choice?
   → Create `docs/knowledge/decisions/ADR-NNN.md`

3. Did you learn something important?
   → Create `docs/knowledge/insights/TOPIC.md`

See `docs/knowledge/README.md` for templates and detailed guidance.

---

## Troubleshooting

### @copilot doesn't respond

**Check:**
1. Is the issue labeled with `copilot-task`?
2. Did you mention @copilot in a comment?
3. Is the issue assigned to @copilot?
4. Are GitHub Actions enabled? (Settings → Actions → General)

**Debug:**
1. Go to **Actions** tab
2. Click "Copilot Task Automation" workflow
3. View the latest run
4. Check logs for errors

### PR not created

**Check:**
1. Did the workflow run? (check Actions tab)
2. Did it complete successfully or fail?
3. Are there error messages in the logs?

**Common issues:**
- CODEOWNERS still has `@owner` placeholder → Update with real username
- Workflow file has YAML syntax error → Run `yamllint .github/workflows/copilot-task.yml`
- Branch not pushed → Check "Commits" section of workflow logs

### Issue appears stuck

**Check:**
1. Look for comments posted by @copilot
2. Check the feature branch was created (`copilot/issue-NNN`)
3. Search for PR with title "Implementation of: [issue title]"

**Common issues:**
- PR was created but not assigned to you → Check all open PRs
- Workflow is still running → Check Actions tab for duration
- Knowledge base validation failed → Check `docs/knowledge/` structure exists

---

## Customization

### Different Reviewers by Code Area

Edit `.github/CODEOWNERS` to route reviews:

```
# Frontend code
client/**  @frontend-team

# Backend code
server/**  @backend-team

# Configuration
.github/**  @infra-team

# Everything else
*  @repo-owner
```

### Different Issue Types

Create additional templates in `.github/ISSUE_TEMPLATE/`:
- `bug-report.yml` - For bug reports
- `feature-request.yml` - For feature requests
- `documentation.yml` - For doc updates

Copy `task.yml` as a starting point.

### Workflow Customization

Edit `.github/workflows/copilot-task.yml` to:
- Change branch naming convention
- Add additional validation steps
- Integrate with external tools
- Change notification behavior

---

## Advanced: Understanding the Automation

### What Each File Does

**Issue Template** (`task.yml`)
- Guides users to provide consistent information
- Auto-labels issues with `copilot-task`
- Captures priority, effort, skills needed
- GitHub automatically offers this when creating issues

**Workflow** (`copilot-task.yml`)
- Triggers on: issue created/labeled/assigned
- Steps:
  1. Extract issue metadata
  2. Post acknowledgment comment
  3. Create feature branch
  4. Analyze requirements (planning)
  5. Check knowledge base
  6. Create pull request (in production)
  7. Assign reviewers from CODEOWNERS

**CODEOWNERS** (GitHub standard)
- GitHub reads this file automatically
- Routes PR approval requests based on file patterns
- Only person listed can approve PRs to that area

**Knowledge Base**
- Optional but recommended
- Three sections: Patterns (reusable), Decisions (ADRs), Insights (lessons)
- Workflow checks it during analysis phase
- Should be updated after completing tasks

---

## FAQ

**Q: Can @copilot work on any type of task?**
A: Yes! Tasks can be features, bugs, docs, refactoring, anything. The workflow is generic.

**Q: What if the PR needs changes after review?**
A: @copilot can iterate based on feedback (requires API integration). Otherwise, reviewer can push changes directly or ask for revision.

**Q: Is this just for @copilot, or can I use it for team members?**
A: Can be adapted for any autonomous agent or team member. Just change CODEOWNERS.

**Q: What if we don't have GitHub Actions enabled?**
A: This system requires Actions. Check Settings → Actions → General and enable if needed.

**Q: Can we integrate this with Slack/Discord?**
A: Yes, by extending the workflow with notification steps. See GitHub Actions marketplace.

**Q: Who can create issues?**
A: Anyone with write access to the repository.

**Q: Can @copilot be disabled?**
A: Remove `copilot-task` label from issues or disable the workflow in GitHub Actions.

---

## System Components

```
.github/
├── ISSUE_TEMPLATE/
│   └── task.yml                    # Issue template
├── CODEOWNERS                      # Review assignment
├── workflows/
│   ├── copilot-task.yml           # Main execution
│   └── validate-system.yml        # Health checks
└── pull_request_template.md       # PR structure

docs/
└── knowledge/
    ├── README.md                  # KB structure & guide
    ├── patterns/                  # Reusable solutions
    ├── decisions/                 # Architecture decisions
    └── insights/                  # Lessons learned

scripts/
└── verify-bootstrap.sh            # System verification

README.md                           # This file
```

---

## Workflow Lifecycle States

Issues processed by @copilot follow this state machine:

```
created
  ↓
in-progress ← @copilot adds label
  ↓
(PR created) ← copilot/issue-NNN branch
  ↓
under-review ← Reviewer assigned from CODEOWNERS
  ↓
changes-requested ← If feedback needed
  │   ↓
  └→ in-progress ← @copilot iterates
  ↓
approved ← Reviewer approves
  ↓
merged ← PR merged to main
  ↓
closed ← Issue closed
  ↓
documented ← Patterns added to KB (optional)
```

---

## Monitoring & Health

Run the validation workflow to check system health:

**Manual:** Go to **Actions** → **Validate Copilot System** → **Run workflow**

**Automatic:** Runs daily at midnight UTC

**Checks:**
- All required files exist
- YAML syntax is valid
- CODEOWNERS has proper format
- Knowledge base structure is correct
- Bash scripts are executable
- Documentation is present

---

## Next Steps

1. **Setup:** Edit `.github/CODEOWNERS` with your GitHub username
2. **Test:** Create a simple issue using the template
3. **Monitor:** Watch the workflow run in Actions tab
4. **Review:** Check the created PR in Pull Requests tab
5. **Learn:** Check `docs/knowledge/` for pattern examples
6. **Integrate:** Add patterns to KB from completed tasks

---

## Support

**For questions about:**
- This automation → Create issue with label `infra` or `docs`
- GitHub Copilot features → See https://github.com/features/copilot
- GitHub Actions → See https://docs.github.com/actions
- This repository → Create issue using Development Task template

---

## See Also

- `.github/CODEOWNERS` - Configure review assignment
- `.github/ISSUE_TEMPLATE/task.yml` - Issue template format
- `docs/knowledge/README.md` - Knowledge base guide
- `.github/workflows/copilot-task.yml` - Workflow implementation
- `scripts/verify-bootstrap.sh` - Validation script

---

**Last Updated:** 2026-01-06
**System Status:** Ready for production use
**Documentation Version:** 1.0
