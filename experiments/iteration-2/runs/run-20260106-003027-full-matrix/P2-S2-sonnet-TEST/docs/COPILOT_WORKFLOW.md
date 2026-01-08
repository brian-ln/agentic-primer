# Copilot Workflow Guide

Complete guide for using the @copilot issue-driven development system.

## Table of Contents

- [Quick Start](#quick-start)
- [Creating Issues](#creating-issues)
- [Monitoring Progress](#monitoring-progress)
- [Reviewing PRs](#reviewing-prs)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)

---

## Quick Start

### For Issue Creators

1. **Navigate to Issues** → Click "New Issue"
2. **Choose Template** → Select "Copilot Feature Request", "Copilot Bug Fix", or "Copilot Refactoring"
3. **Fill Required Fields** → Complete all required sections
4. **Submit** → Issue is automatically labeled with `copilot` and workflow triggers

### What Happens Next

1. ✅ Issue validation runs (checks for required fields)
2. 📚 Knowledge base is queried for relevant context
3. 🤖 Copilot worker creates branch and implements changes
4. 🔀 Draft PR is opened and linked to your issue
5. 👤 You're automatically assigned as PR reviewer
6. ✔️ Review and approve when satisfied

---

## Creating Issues

### Issue Templates

We provide three templates optimized for @copilot:

#### 1. Feature Request (`copilot-feature.yml`)

Use for new features, enhancements, or additions to the codebase.

**Required Fields:**
- **Feature Title:** Clear, concise name (e.g., "Add OAuth login with GitHub")
- **Description:** Detailed explanation of what and why
- **Acceptance Criteria:** Checklist of conditions for "done" (use `- [ ]` syntax)
- **Priority:** Low, Medium, High, or Critical
- **Technical Context:** Relevant files, frameworks, patterns

**Optional but Recommended:**
- **Knowledge Base References:** Link to KB entries (architecture patterns, standards)
- **Constraints:** Security concerns, performance requirements, compatibility needs
- **Related Issues:** Dependencies, blockers, related work

**Example:**

```markdown
Feature Title: Add OAuth login with GitHub

Description:
Users should be able to log in using their GitHub account instead of creating
a new password. This improves onboarding and reduces password management overhead.

Acceptance Criteria:
- [ ] "Sign in with GitHub" button on login page
- [ ] OAuth flow redirects to GitHub and back successfully
- [ ] User profile created/updated with GitHub data
- [ ] Existing sessions preserved across OAuth login
- [ ] Unit tests cover OAuth flow
- [ ] Integration tests verify end-to-end flow

Priority: High

Technical Context:
- Current auth: JWT tokens in HTTP-only cookies
- Framework: Next.js 14 with App Router
- Database: PostgreSQL with Prisma ORM
- Related files: src/app/api/auth/*, src/lib/auth.ts

Knowledge Base References:
- Architecture: authentication-patterns
- Standards: api-design-guide
```

#### 2. Bug Fix (`copilot-bug.yml`)

Use for bugs, errors, or unexpected behavior.

**Required Fields:**
- **Bug Title:** Short description of the bug
- **Description:** Clear explanation of what's wrong
- **Reproduction Steps:** Step-by-step instructions (numbered list)
- **Expected Behavior:** What should happen
- **Actual Behavior:** What actually happens
- **Severity:** Low, Medium, High, or Critical
- **Environment:** Browser, OS, app version, environment
- **Technical Context:** Relevant files, functions, components
- **Acceptance Criteria:** Checklist to confirm the fix

**Optional but Recommended:**
- **Error Logs:** Console errors, stack traces
- **Suggested Fix:** Ideas on how to fix

**Example:**

```markdown
Bug Title: Login form doesn't validate email format

Description:
The login form accepts invalid email addresses and attempts to authenticate
with them, causing 400 errors from the API.

Reproduction Steps:
1. Navigate to /login
2. Enter "invalid@" in email field
3. Enter any password
4. Click "Sign In" button
5. Observe error in console

Expected Behavior:
Form should validate email format client-side and show error message before
submitting to API.

Actual Behavior:
Form submits invalid email to API, which returns 400 error. User sees generic
"Login failed" message instead of helpful validation feedback.

Severity: Medium

Environment:
- Browser: Chrome 120, Firefox 121
- OS: macOS 14.2
- App version: 2.3.1
- Environment: Production

Technical Context:
- File: src/components/LoginForm.tsx
- Function: handleSubmit(), line 45
- Related: src/lib/validation.ts (email validation utility)
- API endpoint: POST /api/auth/login

Acceptance Criteria:
- [ ] Email field validates format on blur
- [ ] Invalid emails show clear error message
- [ ] Valid emails pass validation and submit
- [ ] API only called with valid email formats
- [ ] Unit tests cover validation edge cases
```

#### 3. Refactoring (`copilot-refactor.yml`)

Use for code improvements, restructuring, or tech debt reduction.

**Required Fields:**
- **Refactoring Title:** What needs refactoring
- **Current State:** Describe the code that needs improvement
- **Desired State:** What it should look like after refactoring
- **Refactoring Type:** Extract, rename, move, simplify, deduplicate, etc.
- **Motivation:** Why this refactoring is needed
- **Scope:** Files/components affected
- **Constraints:** What must not change (API contracts, behavior, etc.)
- **Acceptance Criteria:** Checklist to confirm completion
- **Priority:** Low, Medium, High, or Critical

**Example:**

```markdown
Refactoring Title: Extract authentication logic into separate service

Current State:
Authentication logic is scattered across multiple components:
- LoginForm.tsx has OAuth logic
- Dashboard.tsx has session checking
- Profile.tsx has token refresh logic

This makes it hard to maintain and test consistently.

Desired State:
All authentication logic centralized in src/lib/auth-service.ts with:
- Single source of truth for auth state
- Reusable functions for login, logout, token refresh
- Clear separation of concerns
- Easier to test and maintain

Refactoring Type: Extract function/class

Motivation:
- Code is duplicated in 5 different components
- Hard to update authentication logic consistently
- New OAuth providers need to be added (blocked by current structure)
- Test coverage is low due to tight coupling

Scope:
Files to refactor:
- src/components/LoginForm.tsx
- src/components/Dashboard.tsx
- src/components/Profile.tsx

Files to create:
- src/lib/auth-service.ts

Constraints:
- API contract must remain unchanged
- User-facing behavior must stay exactly the same
- Existing tests must continue to pass
- No breaking changes to exported interfaces

Acceptance Criteria:
- [ ] All authentication logic centralized in auth-service.ts
- [ ] Components use auth service instead of inline logic
- [ ] Code duplication eliminated
- [ ] All existing tests pass
- [ ] New unit tests for auth service (80%+ coverage)
- [ ] No change to user-facing behavior
```

### Writing Effective Issues

Based on GitHub's **WRAP framework** (Write effective issues, Refine instructions, And get the most out of coding agent):

#### 1. Provide Context

@copilot needs context like a new team member would:
- What is the feature/bug/refactoring?
- Why is it needed?
- What constraints exist?
- What patterns should be followed?

#### 2. Be Specific

Vague: "Make the login better"
Specific: "Add email validation to login form that checks format on blur and displays inline error messages"

#### 3. Include Acceptance Criteria

Use checkbox syntax for clear, testable conditions:

```markdown
- [ ] Email field validates format on blur
- [ ] Invalid emails show error message
- [ ] Unit tests verify validation logic
```

#### 4. Reference Knowledge Base

Link to relevant KB entries so @copilot uses project patterns:

```markdown
Knowledge Base References:
- Architecture: authentication-patterns
- Standards: coding-standards
- Testing: testing-practices
```

#### 5. Add Technical Context

Help @copilot understand the codebase:

```markdown
Technical Context:
- Framework: Next.js 14 with App Router
- Files to modify: src/app/api/auth/route.ts
- Related components: src/components/LoginForm.tsx
- Database: PostgreSQL with Prisma
```

---

## Monitoring Progress

### Issue Comments

@copilot posts updates as comments on your issue:

1. **Validation Status**
   ```
   ✅ Issue validation passed
   ```

2. **Knowledge Base Context**
   ```
   🤖 @copilot starting work

   📚 Knowledge Base Context:
   - Architecture: Service Layer pattern
   - Standards: TypeScript best practices
   ```

3. **Implementation Complete**
   ```
   ✅ Implementation complete!

   🔀 Pull Request: #123
   🌿 Branch: copilot/issue-45-add-oauth-login
   ```

4. **Errors** (if any)
   ```
   ❌ Implementation failed

   Error: Could not parse issue body
   ```

### GitHub Actions

View detailed logs in the Actions tab:

1. Navigate to **Actions** → **Copilot Issue Handler**
2. Click on the workflow run for your issue
3. Expand steps to see detailed logs

---

## Reviewing PRs

### PR Assignment

When @copilot opens a PR:
- ✅ You're automatically assigned as the PR assignee
- ✅ You're added as a reviewer (if you're not the PR author)
- ✅ PR is linked to your issue with "Closes #123"
- ✅ PR is opened as a **draft** (prevents accidental merge)

### Review Process

1. **Read PR Description**
   - Review changes summary
   - Check that acceptance criteria are addressed
   - Review knowledge base context used

2. **Review Code Changes**
   - Click "Files changed" tab
   - Review implementation line-by-line
   - Check for:
     - Correct logic
     - Code quality and style
     - Test coverage
     - Documentation

3. **Test Locally (Optional)**
   ```bash
   # Checkout the PR branch
   gh pr checkout <PR-NUMBER>

   # Run tests
   npm test

   # Run the app
   npm run dev
   ```

4. **Request Changes or Approve**
   - **If issues found:** Add review comments, request changes
   - **If looks good:** Approve the PR

5. **Convert from Draft**
   - Click "Ready for review" button
   - Merge when all checks pass

### Common Issues to Watch For

- **Missing tests:** Ensure implementation includes unit/integration tests
- **Breaking changes:** Check that existing functionality still works
- **Code style:** Should follow project conventions (ESLint, Prettier)
- **Documentation:** Complex logic should have comments
- **Security:** Validate user input, protect sensitive data

---

## Troubleshooting

### Issue Validation Failed

**Error:** "Issue validation failed"

**Cause:** Issue is missing required fields

**Solution:**
1. Edit the issue
2. Add missing required fields (title, description, acceptance criteria)
3. Ensure description is at least 50 characters
4. Re-add `copilot` label to trigger workflow again

### No PR Created

**Error:** Workflow completes but no PR appears

**Possible Causes:**
1. Branch creation failed (check logs)
2. No changes were made (check worker logs)
3. GitHub token permissions issue

**Solution:**
1. Check Actions logs for errors
2. Ensure repository allows Actions to create PRs
3. Verify `GITHUB_TOKEN` has `pull_requests: write` permission

### PR Not Assigned to Me

**Error:** PR created but not assigned to issue creator

**Possible Causes:**
1. Branch name doesn't follow pattern `copilot/issue-<number>-<slug>`
2. Issue number extraction failed
3. GitHub API error

**Solution:**
1. Manually assign yourself to the PR
2. Check workflow logs for issue number extraction
3. Re-run the PR assign workflow

### Knowledge Base Context Not Used

**Error:** Implementation doesn't follow project patterns

**Possible Causes:**
1. Knowledge base is empty or not indexed
2. Query didn't match any KB entries
3. Issue description lacks relevant keywords

**Solution:**
1. Update knowledge base with relevant patterns
2. Add explicit KB references in issue description
3. Use keywords that match KB entries

### Implementation Doesn't Match Requirements

**Error:** PR implements something different than requested

**Possible Causes:**
1. Vague or ambiguous issue description
2. Missing acceptance criteria
3. Unclear technical context

**Solution:**
1. Close the PR with explanation
2. Update the issue with clearer requirements
3. Re-add `copilot` label to trigger new implementation

---

## Best Practices

### For Issue Creators

#### DO:
✅ **Be specific and detailed** in descriptions
✅ **Include acceptance criteria** as checkboxes
✅ **Add technical context** (files, frameworks, patterns)
✅ **Reference knowledge base** entries
✅ **Specify constraints** (security, performance, compatibility)
✅ **Link related issues** (dependencies, blockers)
✅ **Use clear, descriptive titles**

#### DON'T:
❌ **Be vague** ("make it better")
❌ **Skip acceptance criteria** (how do we know it's done?)
❌ **Assume context** (@copilot needs explicit info)
❌ **Mix multiple requests** (create separate issues)
❌ **Use jargon without explanation**

### For Reviewers

#### DO:
✅ **Review promptly** (within 24-48 hours)
✅ **Test the implementation** if possible
✅ **Check test coverage** and quality
✅ **Verify acceptance criteria** are met
✅ **Leave constructive feedback**
✅ **Approve and merge** when satisfied

#### DON'T:
❌ **Ignore the PR** (provide feedback or approval)
❌ **Merge without review** (defeats the purpose)
❌ **Nitpick style** (use automated linting instead)
❌ **Request changes without explanation**

### For Maintainers

#### DO:
✅ **Keep knowledge base updated** with project changes
✅ **Monitor worker success rate** and improve templates
✅ **Review and merge PRs promptly**
✅ **Collect feedback** on @copilot effectiveness
✅ **Update issue templates** based on patterns

#### DON'T:
❌ **Let knowledge base go stale**
❌ **Ignore failed workflows** (investigate and fix)
❌ **Skip validation tests** (ensure system health)

---

## Advanced Usage

### Custom Labels

Add labels to issues for categorization:
- `priority:high` - High priority work
- `good-first-issue` - Simple tasks for onboarding
- `tech-debt` - Technical debt reduction
- `performance` - Performance improvements

### Multiple Issue Types

Create custom issue templates for specific scenarios:
- Database migrations
- API endpoint additions
- UI component creation
- Documentation updates

### Integration with Project Boards

Link issues to project boards for tracking:
1. Create project board (e.g., "Copilot Work")
2. Add automation rules to move cards
3. Track progress visually

### Metrics and Analytics

Track @copilot effectiveness:
- Success rate (PRs merged / issues created)
- Cycle time (issue created → PR merged)
- Review time (PR opened → approved)
- Rework rate (changes requested / total PRs)

---

## FAQs

### Q: Can I assign issues to @copilot manually?

**A:** Yes, just add the `copilot` label to any issue (even if not created from template). Ensure it has sufficient context for implementation.

### Q: What if I need to modify @copilot's implementation?

**A:** You can push commits to the PR branch or request changes. @copilot won't automatically update the PR (yet), so manual changes are fine.

### Q: Can I use @copilot for urgent bug fixes?

**A:** Yes, use the Bug Fix template and set severity to "Critical". However, for very urgent fixes, manual implementation may be faster.

### Q: What types of work is @copilot good at?

**A:** @copilot excels at:
- Well-defined features with clear requirements
- Bug fixes with reproduction steps
- Refactoring with specific goals
- Boilerplate code generation
- Following established patterns

**Not ideal for:**
- Exploratory work with unclear requirements
- Complex architectural decisions
- Cross-cutting changes affecting many files
- Work requiring human judgment or creativity

### Q: How do I improve @copilot's accuracy?

**A:**
1. **Update the knowledge base** with project-specific patterns
2. **Refine issue templates** based on what works
3. **Provide detailed context** in issues
4. **Give feedback** on PRs (what worked, what didn't)

---

## Support

### Getting Help

- **Documentation:** Read this guide and `KNOWLEDGE_BASE.md`
- **Issues:** File bugs/feature requests for the @copilot system itself
- **Discussions:** Ask questions in GitHub Discussions

### Feedback

We want to improve @copilot! Please share:
- What worked well
- What didn't work
- Ideas for improvement
- Example issues that succeeded or failed

Create a discussion or issue with the `copilot-feedback` label.

---

**Last Updated:** 2026-01-06
**Version:** 1.0
