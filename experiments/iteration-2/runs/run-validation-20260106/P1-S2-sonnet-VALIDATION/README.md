# @copilot Issue Automation System

**Bootstrap Date:** 2026-01-06
**System Version:** 1.0
**Agent:** @copilot (simulated)

---

## Overview

This repository implements an AI-executable workspace with GitHub-native issue automation, auto-review, and knowledge base integration. The @copilot agent processes tasks autonomously while maintaining mandatory human oversight.

## Quick Start

### Create a Task for @copilot

1. **Go to Issues tab** in GitHub web UI
2. **Click "New Issue"**
3. **Select "@copilot Task" template**
4. **Fill in the form:**
   - Task Summary (one-line description)
   - Detailed Description (requirements)
   - Context and Background (related files, dependencies)
   - Success Criteria (observable outcomes)
   - Priority (Low/Medium/High/Critical)
5. **Click "Submit new issue"**

### What Happens Next

```
Issue Created
    ↓
GitHub Actions Workflow Triggers
    ↓
@copilot Agent Processes Task
    ↓
Creates Branch: copilot/issue-{number}
    ↓
Implements Solution
    ↓
Commits Changes
    ↓
Opens Pull Request
    ↓
Auto-Review Runs (syntax, tests, linting)
    ↓
CODEOWNERS Assigns Reviewer (YOU)
    ↓
Human Reviews and Approves
    ↓
PR Merged → Knowledge Base Updated
```

## System Components

### 1. Issue Template (`.github/ISSUE_TEMPLATE/copilot-task.yml`)

Structured YAML form that captures:
- Task summary and detailed description
- Context and background information
- Observable success criteria
- Priority level
- Additional notes

**Why:** Structured input improves agent success rate and ensures complete task specifications.

### 2. GitHub Actions Workflow (`.github/workflows/copilot-agent.yml`)

Automation workflow that:
- Triggers on issue creation with `copilot-task` label
- Simulates @copilot agent execution
- Creates branch `copilot/issue-{number}`
- Commits implementation
- Opens pull request
- Runs auto-review checks
- Posts status comments

**Why:** Enables autonomous task processing without manual intervention.

### 3. CODEOWNERS File (`.github/CODEOWNERS`)

Review enforcement that:
- Auto-assigns all PRs to repository owner
- Ensures mandatory human review
- Prevents unchecked agent changes

**Why:** GitHub Copilot agents cannot approve their own PRs - human oversight required for safety.

### 4. Knowledge Base (`docs/knowledge/`)

Structured repository for institutional memory:

```
docs/knowledge/
├── README.md           # Knowledge base overview
├── patterns/           # Reusable code patterns
│   └── README.md      # Pattern documentation guide
├── decisions/          # Architecture Decision Records (ADRs)
│   └── README.md      # ADR template and guide
└── insights/           # Lessons learned from retrospectives
    └── README.md      # Insight capture guide
```

**Why:** Preserves context, patterns, and learnings across time and team changes.

### 5. Auto-Review Script (`scripts/auto-review.sh`)

Automated quality validation that runs:
- YAML syntax validation (`yamllint`)
- Shell script validation (`shellcheck`)
- Markdown linting (`markdownlint`)
- Test suite (if present)
- Common issues check (TODOs, large files, etc.)

**Why:** Catches trivial errors before human review, improves efficiency.

### 6. Documentation

This README provides:
- Quick start guide
- Workflow explanation
- Usage examples
- Troubleshooting
- Best practices

**Why:** System is useless if users don't understand how to use it.

## Usage Examples

### Example 1: Add New Feature

**Issue Title:** `[TASK]: Add user profile page`

**Detailed Description:**
```
Create a user profile page that displays:
- User avatar and name
- Bio/description
- Recent activity (last 10 posts)
- Edit profile button (if viewing own profile)

Technical requirements:
- React component in src/pages/Profile.js
- Fetch user data from /api/users/:id
- Use existing Avatar and Card components
- Responsive design (mobile + desktop)
```

**Success Criteria:**
```
- [ ] Profile page renders correctly at /profile/:username
- [ ] All user data displays accurately
- [ ] Edit button only shows for authenticated user viewing own profile
- [ ] Page is responsive on mobile and desktop
- [ ] All tests pass
```

**What @copilot does:**
1. Creates `copilot/issue-42` branch
2. Implements Profile.js component
3. Adds API integration
4. Writes tests
5. Opens PR with implementation
6. Auto-review validates syntax and tests
7. You review and approve

### Example 2: Fix Bug

**Issue Title:** `[TASK]: Fix login form validation`

**Detailed Description:**
```
Login form allows submission with empty email field.

Current behavior:
- User can click "Login" without entering email
- API returns 400 error
- No user-friendly error message shown

Expected behavior:
- Disable submit button when email is empty
- Show validation error: "Email is required"
- Validate email format before submission
```

**Success Criteria:**
```
- [ ] Submit button disabled when email empty
- [ ] Validation message displays for empty email
- [ ] Email format validated (basic regex check)
- [ ] Existing tests still pass
- [ ] New test covers empty email case
```

**What @copilot does:**
1. Identifies login form component
2. Adds validation logic
3. Updates UI to show errors
4. Writes test for edge case
5. Opens PR
6. You verify the fix works

### Example 3: Improve Documentation

**Issue Title:** `[TASK]: Document API authentication flow`

**Detailed Description:**
```
Create documentation for API authentication process.

Topics to cover:
- How to obtain JWT token
- Token format and expiration
- How to include token in requests
- Token refresh process
- Error handling for expired/invalid tokens

Include code examples in JavaScript and curl.
```

**Success Criteria:**
```
- [ ] Documentation created in docs/api/authentication.md
- [ ] Covers all 5 topics listed above
- [ ] Includes working code examples
- [ ] Examples tested and verified
- [ ] Links added to main API docs
```

**What @copilot does:**
1. Creates authentication.md
2. Documents flow with examples
3. Tests curl examples
4. Updates doc index
5. Opens PR
6. You review for accuracy

## Web UI Workflow

All interactions happen in GitHub web interface:

### Creating Issues

1. Navigate to **Issues** tab
2. Click **New Issue** (green button, top right)
3. Select **"@copilot Task"** template
4. Fill form fields
5. Click **Submit new issue**
6. Wait for @copilot to comment with status

### Reviewing PRs

1. Navigate to **Pull Requests** tab
2. Click on PR created by @copilot
3. Review **Files changed** tab
4. Check auto-review comments
5. Add your own review comments
6. Click **Review changes** → **Approve** (or Request changes)
7. Click **Merge pull request** when ready

### Checking Workflow Status

1. Navigate to **Actions** tab
2. Click on workflow run
3. View logs for each step
4. Debug if workflow fails

## Troubleshooting

### Issue: Workflow doesn't trigger

**Check:**
- Issue has `copilot-task` label (should auto-apply from template)
- GitHub Actions enabled in repository settings
- Workflow file is valid YAML

**Fix:**
```bash
# Validate workflow syntax
yamllint .github/workflows/copilot-agent.yml

# Manually add label to issue if missing
# (via GitHub web UI: Labels → copilot-task)
```

### Issue: Auto-review fails

**Check:**
- Linters installed (yamllint, shellcheck, markdownlint)
- Files pass individual validation
- Test suite is runnable

**Fix:**
```bash
# Install linters
pip install yamllint
apt-get install shellcheck  # or: brew install shellcheck
npm install -g markdownlint-cli

# Run manually to see errors
./scripts/auto-review.sh
```

### Issue: PR not assigned to reviewer

**Check:**
- CODEOWNERS file exists
- `@owner` replaced with actual GitHub username
- Branch protection configured (optional but recommended)

**Fix:**
```bash
# Edit CODEOWNERS
# Replace: * @owner
# With: * @yourusername

# Commit and push
git add .github/CODEOWNERS
git commit -m "fix: Update CODEOWNERS with actual username"
git push
```

### Issue: @copilot implementation is incorrect

**Action:**
- Add review comments on PR
- Request changes with specific feedback
- @copilot will iterate (in production; manual in simulation)
- Alternative: Close PR and create new issue with clearer requirements

## Best Practices

### Writing Effective Task Descriptions

**Good:**
```
Add pagination to /api/users endpoint:
- Implement cursor-based pagination (not offset)
- Accept ?cursor= and ?limit= query params
- Default limit: 50, max limit: 100
- Return nextCursor in response for subsequent requests
- Update API docs with pagination examples
```

**Bad:**
```
Add pagination
```

**Why:** Specific requirements lead to better implementations. Vague tasks lead to mismatched expectations.

### Defining Success Criteria

**Good:**
```
- [ ] API endpoint returns 200 status for valid requests
- [ ] Response includes data array and nextCursor field
- [ ] Invalid cursor returns 400 with error message
- [ ] All existing tests pass
- [ ] New tests cover pagination edge cases
```

**Bad:**
```
- [ ] Make it work
- [ ] No bugs
```

**Why:** Observable, testable criteria enable verification. Vague criteria lead to ambiguity.

### Reviewing PRs

**Check:**
- Does implementation match success criteria?
- Are edge cases handled?
- Is code readable and maintainable?
- Are tests comprehensive?
- Is documentation updated?

**Don't just:**
- Trust that tests pass
- Approve without reading code
- Skip checking edge cases

**Why:** Human review catches logic issues, design problems, and missing requirements that automated checks can't.

## Knowledge Base Usage

### When to Add Patterns

After solving the same problem 2+ times with the same approach:

```bash
# Create pattern document
cat > docs/knowledge/patterns/cursor-based-pagination.md << 'EOF'
# Cursor-Based Pagination Pattern

## Problem
API endpoints returning large lists need efficient pagination...

## Solution
[Include code example from your implementation]

## Trade-offs
[Discuss pros/cons vs offset pagination]
EOF

git add docs/knowledge/patterns/
git commit -m "docs: Add cursor-based pagination pattern"
```

### When to Create ADRs

After making a significant architectural decision:

```bash
# Create ADR
cat > docs/knowledge/decisions/001-pagination-strategy.md << 'EOF'
# ADR-001: Cursor-Based Pagination Strategy

**Status:** Accepted
**Date:** 2026-01-06

## Context
Our API needs to paginate large result sets efficiently...

## Decision
We will use cursor-based pagination instead of offset-based...

## Alternatives Considered
[Document offset-based, GraphQL connections, etc.]
EOF

git add docs/knowledge/decisions/
git commit -m "docs: Add pagination strategy ADR"
```

### When to Capture Insights

After retrospectives or discoveries:

```bash
# Create insight
cat > docs/knowledge/insights/2026-01-06-pagination-performance.md << 'EOF'
# Insight: Cursor Pagination Improved Query Performance 10x

**Date:** 2026-01-06
**Category:** Performance

## Summary
Switching from offset to cursor pagination reduced query time from 2.3s to 0.2s...

## Evidence
[Include metrics, graphs, benchmarks]

## Learnings
[What we learned and how to apply it]
EOF

git add docs/knowledge/insights/
git commit -m "docs: Add pagination performance insight"
```

## Configuration

### Customizing Auto-Review

Edit `scripts/auto-review.sh` to add checks:

```bash
# Add custom validation
validate_custom() {
    section "Custom Validation"

    # Your custom checks here
    # Return 0 for pass, 1 for fail
}

# Call in main()
main() {
    # ... existing checks ...
    validate_custom || true
    # ... rest of main ...
}
```

### Customizing Workflow

Edit `.github/workflows/copilot-agent.yml`:

```yaml
# Add custom steps
- name: "Custom Step"
  run: |
    echo "Your custom logic here"
```

### Customizing Issue Template

Edit `.github/ISSUE_TEMPLATE/copilot-task.yml`:

```yaml
# Add new fields
- type: input
  id: custom-field
  attributes:
    label: "Custom Field"
    description: "Description of custom field"
  validations:
    required: false
```

## System Architecture

### Design Decisions

**Why GitHub-Native?**
- No external dependencies
- Works on any repository
- Free for public repos
- Integrated with existing workflow

**Why YAML Issue Templates?**
- Structured data input
- Required fields enforcement
- Better UX than markdown templates
- Auto-labeling support

**Why CODEOWNERS?**
- Automatic PR assignment
- Fail-safe (can't be bypassed)
- Zero maintenance
- Branch protection integration

**Why Markdown Knowledge Base?**
- Git version control
- GitHub search integration
- No additional tools needed
- Renders in web UI

**Why Shell Auto-Review Script?**
- Universal (works everywhere)
- No runtime dependencies
- Easy to audit and customize
- Standard linting tools

## Success Metrics

This system is considered successful when:

1. **Functional Test:** Processes test issue end-to-end without errors
2. **Syntax Valid:** All files pass automated validation
3. **Workflow Triggers:** GitHub Actions runs on issue creation
4. **Human Review:** All PRs receive review before merge
5. **Knowledge Growth:** Patterns, decisions, and insights accumulate over time

## Validation

### Syntax Validation

```bash
# Validate all generated files
yamllint .github/ISSUE_TEMPLATE/copilot-task.yml
yamllint .github/workflows/copilot-agent.yml
shellcheck scripts/auto-review.sh
markdownlint docs/knowledge/**/*.md README.md
```

### End-to-End Test

1. Create test issue using template
2. Verify workflow triggers
3. Check branch creation
4. Verify PR creation
5. Confirm auto-review runs
6. Validate CODEOWNERS assignment

### Expected Results

All checks pass:
- YAML files are syntactically valid
- Shell scripts pass shellcheck
- Markdown files are properly formatted
- Workflow triggers on labeled issues
- Auto-review provides useful feedback
- Human reviewer is assigned automatically

## Maintenance

### Regular Tasks

**Weekly:**
- Review new patterns, decisions, insights
- Update stale knowledge base entries
- Check workflow run success rate

**Monthly:**
- Audit CODEOWNERS accuracy
- Review auto-review effectiveness
- Update documentation for changes

**Quarterly:**
- Evaluate agent success rate
- Gather user feedback
- Plan improvements

### Updating the System

All updates should:
1. Be tested in a branch first
2. Pass auto-review checks
3. Receive human review
4. Document changes in knowledge base

## Security Considerations

**GitHub Actions Security:**
- Workflow runs in isolated sandbox
- Limited to repository scope
- No access to secrets unless explicitly granted

**Code Review Security:**
- CODEOWNERS enforces mandatory review
- Human must approve all agent changes
- Branch protection recommended (not required)

**Knowledge Base Security:**
- Version controlled (audit trail)
- Review process for all changes
- No secrets or credentials stored

## License and Attribution

**Created by:** @copilot agent (simulated)
**Date:** 2026-01-06
**System Version:** 1.0
**Bootstrap Prompt:** "Bootstrap @copilot issue automation with auto-review and knowledge base."

This system is designed to work on any git repository with GitHub Actions enabled.

---

## Next Steps

1. **Test the System:** Create your first @copilot task issue
2. **Review the PR:** Verify the workflow creates a PR correctly
3. **Update CODEOWNERS:** Replace `@owner` with your GitHub username
4. **Customize:** Adapt templates and workflows to your needs
5. **Contribute:** Add patterns, decisions, and insights as you learn

Ready to get started? [Create your first issue](../../issues/new/choose)!
