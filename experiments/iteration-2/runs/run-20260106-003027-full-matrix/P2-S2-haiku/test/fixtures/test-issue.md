# Test Copilot Automation

## Description

This is a test issue to validate the Copilot issue-driven development workflow. It exercises all major workflow components:

- ✅ Issue creation and labeling
- ✅ Knowledge base consultation
- ✅ Implementation generation
- ✅ Syntax validation
- ✅ PR creation and assignment
- ✅ Issue completion notification

## Acceptance Criteria

1. Issue is automatically assigned to the creator
2. Issue receives `copilot-processing` label
3. Workflow reads knowledge base and finds available patterns, decisions, and insights
4. Copilot agent generates implementation file: `src/features/issue-N-implementation.md`
5. Test script is created: `test/features/issue-N-test.sh`
6. Syntax validation completes (YAML and shell)
7. Feature branch created: `copilot/issue-N`
8. Pull request created with comprehensive summary
9. PR is assigned to the issue creator
10. PR body includes knowledge base summary
11. Issue receives completion comment with PR link
12. `copilot-processing` label is removed
13. `copilot-completed` label is added
14. Workflow completes without errors

## Test Procedures

### Manual Setup

```bash
# 1. Create issue with label in GitHub UI
# Title: "Test Copilot Automation"
# Body: (copy content from this file)
# Labels: copilot-task

# 2. Monitor workflow in Actions tab
# 3. Verify all criteria above are met
```

### Automated Validation

```bash
# Check knowledge base structure
find docs/knowledge -name "*.md" -type f

# Verify workflow syntax
yamllint .github/workflows/copilot-issue-driven.yml

# Validate test script if created
shellcheck test/features/issue-*.sh
```

### Expected Workflow Timeline

1. **Trigger** (0 sec): Issue labeled with `copilot-task`
2. **Setup** (1 sec): Environment variables configured
3. **Assignment** (2 sec): Issue assigned to creator
4. **KB Scan** (3 sec): Knowledge base scanned (should find 3 files)
5. **Implementation** (5 sec): Files generated
6. **Validation** (10 sec): YAML and shell validation
7. **Git Operations** (15 sec): Branch created, changes committed
8. **PR Creation** (20 sec): Pull request created
9. **Assignment** (21 sec): PR assigned to creator
10. **Notification** (22 sec): Issue commented with completion status
11. **Complete** (25 sec): Workflow finished

## Success Indicators

### Logs Should Show

```
Processing issue #XXX
Title: Test Copilot Automation
Creator: @username

Found 1 patterns:
   - api-design.md
Found 1 decisions:
   - workflow-architecture.md
Found 1 insights:
   - automation-learnings.md

Copilot agent processing complete
Generated: src/features/issue-XXX-implementation.md
Generated: test/features/issue-XXX-test.sh

Running Validation Checks
yamllint found, running validation...
shellcheck found, running validation...
Validation phase complete

Branch created successfully
Changes committed
Branch pushed successfully

Created PR #YYY
URL: https://github.com/...
Assigned PR #YYY to: @username

Workflow Completed Successfully
```

### GitHub Should Show

- **Issue**: Assigned to creator, labeled `copilot-processing` → `copilot-completed`
- **Comment**: "Copilot Agent - Task Complete" with PR link
- **Branch**: `copilot/issue-XXX` with commit
- **PR**: Titled "feat: Test Copilot Automation", assigned to creator
- **PR Body**: Includes knowledge base summary, checklist, validation results

## Troubleshooting Test Failures

### Workflow Didn't Trigger

- [ ] Issue has `copilot-task` label
- [ ] GitHub Actions enabled in settings
- [ ] Workflow file in `.github/workflows/copilot-issue-driven.yml`
- [ ] Workflow syntax is valid

### Issue Not Assigned

- [ ] Check workflow logs for "Could not assign issue" error
- [ ] Verify user has permission to be assigned
- [ ] Check if assignee limit reached

### PR Not Created

- [ ] Check if branch already exists (delete and retry)
- [ ] Verify `main` branch exists (not `master`)
- [ ] Check permissions in repo settings

### Knowledge Base Not Found

- [ ] Verify `docs/knowledge/patterns/` directory exists
- [ ] Verify `docs/knowledge/decisions/` directory exists
- [ ] Verify `docs/knowledge/insights/` directory exists
- [ ] Verify each directory has at least one `.md` file
- [ ] Should show "Found 0 X" for empty directories (not an error)

### Validation Failed

- [ ] Check if yamllint/shellcheck installed on runner
- [ ] Review validation warnings in workflow logs
- [ ] Note: validation warnings are non-blocking (workflow continues)

## Files Created During Test

When workflow completes successfully, these files should exist:

1. `src/features/issue-XXX-implementation.md` - Implementation document
2. `test/features/issue-XXX-test.sh` - Test script

Both should be:
- [ ] Committed to feature branch
- [ ] Included in pull request
- [ ] Syntactically valid

## Cleanup After Test

If testing locally and need to clean up:

```bash
# Delete feature branch
git branch -D copilot/issue-XXX

# Delete from remote
git push origin --delete copilot/issue-XXX

# Delete PR (in GitHub UI)
# Close and delete PR #YYY

# Delete issue (in GitHub UI or mark as duplicate)
# Close issue #XXX

# Remove generated files if on main
rm -f src/features/issue-XXX-implementation.md
rm -f test/features/issue-XXX-test.sh
```

## Notes

- This is a realistic test case that exercises the entire workflow
- It serves as documentation of expected behavior
- Can be reused for regression testing
- Adapt to your repository structure if needed

---

**Created**: 2026-01-08
**Purpose**: Workflow validation and regression testing
**Status**: Ready for use
