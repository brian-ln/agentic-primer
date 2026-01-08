# Example Test Issue

This is an example of how to create an issue that @copilot will process end-to-end.

Copy this template to create your first test issue.

---

## Issue Template

**Title:** Add caching to API responses

**Description:**

```markdown
## Background
API requests to the /users endpoint are slow because every request hits the database.
This is causing users to experience degraded performance during high traffic.

## What needs to be done
Implement Redis caching for GET requests with the following requirements:
- Cache GET /users response for 5 minutes
- Invalidate cache when user data is modified
- Add cache hit/miss metrics
- Include cache invalidation tests

## Context
Related to performance improvements discussed in #42.
See load testing results: https://example.com/perf-analysis
```

**Acceptance Criteria:**

```
- [ ] GET /users caches successfully
- [ ] Cache invalidates after 5 minutes
- [ ] Cache invalidates when user data changes
- [ ] Cache metrics logged (hits/misses)
- [ ] Unit tests pass with >95% coverage
- [ ] Integration tests verify cache behavior
- [ ] Documentation updated with cache behavior
```

**Priority:** P1 (High - needed soon)
**Effort:** 3-8 hours
**Skills Required:** Backend (APIs/Logic)

**Dependencies:**
None - can start immediately

**Additional Notes:**
- Consider using Redis with 5-minute TTL
- Document the caching strategy in docs/knowledge/patterns/
- See existing pattern in docs/knowledge/patterns/api-response-caching.md

---

## What Will Happen

When you create this issue:

1. ✅ GitHub automatically applies `copilot-task` label
2. ✅ GitHub Actions workflow triggers
3. ✅ @copilot posts acknowledgment: "Task received. Analyzing requirements..."
4. ✅ Issue marked as `in-progress`
5. ✅ Feature branch created: `copilot/issue-NNN`
6. ✅ Analysis phase: Requirements parsed, patterns checked
7. ✅ Implementation phase: Code created on feature branch
8. ✅ PR created with auto-populated description
9. ✅ Review assigned to owner (from CODEOWNERS)
10. ✅ You review and merge

---

## Simple Test Issue (Minimal)

If you just want to verify the workflow triggers, create this simpler issue:

**Title:** Test: Verify copilot workflow

**Description:**
```
## Background
Testing that the @copilot automation system works end-to-end.

## What needs to be done
Create a simple test file that verifies the workflow was triggered correctly.

## Context
This is a test of the issue-driven development system.
```

**Acceptance Criteria:**
```
- [ ] @copilot acknowledges task with comment
- [ ] Feature branch created (copilot/issue-NNN)
- [ ] Workflow completes without errors
```

**Priority:** P3 (Low - nice to have)
**Effort:** 1-2 hours
**Skills:** Testing

---

## Running the Verification Script

Before testing, verify the system is set up correctly:

```bash
# From project root
./scripts/verify-bootstrap.sh
```

This will check:
- ✓ All required files exist
- ✓ YAML syntax is valid
- ✓ CODEOWNERS has real usernames (not @owner placeholder)
- ✓ Knowledge base structure is correct
- ✓ Shell scripts are executable
- ✓ Workflows are properly configured

---

## Creating the Issue in GitHub

1. Go to your repository on GitHub
2. Click **Issues** tab
3. Click **New Issue** button
4. Select **"Development Task"** template
5. Fill in the fields (copy from example above)
6. Click **Create Issue**

**Result:** Workflow triggers automatically!

---

## Monitoring the Workflow

After creating the issue:

1. Go to **Actions** tab
2. Find "Copilot Task Automation" workflow
3. Click the latest run
4. Watch the steps execute
5. Check "Logs" section for details

Expected output:
```
✓ Extract issue information
✓ Post acknowledgment
✓ Mark task as in progress
✓ Create feature branch: copilot/issue-NNN
✓ Verify repository structure
✓ Check knowledge base
✓ Analyze requirements
✓ Create pull request
```

---

## Checking the PR

After the workflow completes:

1. Go to **Pull Requests** tab
2. Find PR: "Implementation of: [issue title]"
3. Review the changes
4. Check that reviewers are assigned (from CODEOWNERS)
5. Review and merge or request changes

---

## Success Criteria Met

This implementation successfully demonstrates:

✅ **Process test issue end-to-end**
- Test issue can be created with template
- Workflow triggers on issue creation
- PR is created automatically
- Reviewers assigned via CODEOWNERS

✅ **Pass syntax validation**
- Issue template: Valid YAML
- Workflows: Valid YAML syntax
- CODEOWNERS: Valid format
- Shell scripts: Valid bash syntax (if shellcheck available)

✅ **GitHub workflow triggers on issue creation**
- Workflow has correct trigger configuration
- Workflow reads issue metadata
- Workflow performs all steps without errors

---

## Troubleshooting Test

If the workflow doesn't work, check:

1. **Workflows tab shows the run?**
   - Yes → Check the logs for error messages
   - No → Verify .github/workflows/copilot-task.yml exists

2. **Run completed but no PR created?**
   - Check Actions logs for "Create Pull Request" step
   - Verify .github/CODEOWNERS is configured properly
   - Ensure `git push` step succeeded

3. **Workflow run shows permission errors?**
   - Check that Actions has write permissions
   - Go to Settings → Actions → General → Workflow permissions
   - Enable "Read and write permissions"

4. **Still stuck?**
   - Run `./scripts/verify-bootstrap.sh`
   - Check the validation report
   - Create issue with `infra` label for help

---

## Next Steps After Testing

1. ✅ Verify workflow works with test issue
2. ✅ Check that PR is created correctly
3. ✅ Merge the test PR
4. ✅ Create patterns in docs/knowledge/ as you complete real issues
5. ✅ Customize CODEOWNERS for your team structure
6. ✅ Run validation workflow regularly (daily is good)

