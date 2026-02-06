# Issue-Driven Development Workflow Guide

This guide explains how to use the issue-driven development system with @copilot for autonomous AI agent workflows.

## Table of Contents

- [Overview](#overview)
- [Quick Start](#quick-start)
- [Detailed Workflow](#detailed-workflow)
- [For Developers](#for-developers)
- [For AI Agents](#for-ai-agents)
- [Configuration](#configuration)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)

## Overview

### What is Issue-Driven Development?

Issue-driven development is a workflow where:

1. **Issues define work** - Each task starts as a GitHub issue
2. **Agents execute autonomously** - @copilot works on assigned issues
3. **PRs document progress** - Changes are submitted as pull requests
4. **Knowledge accumulates** - Learnings are extracted automatically

### Key Components

```
Issue → Assignment → Branch → Implementation → PR → Review → Merge → Knowledge
```

- **Issue Template** - Structured task definition
- **Auto-Assignment** - PR creator automatically assigned
- **GitHub Actions** - Automation for validation and knowledge capture
- **Knowledge Base** - Patterns, decisions, and insights

### Benefits

- ✅ **Autonomous execution** - AI agents work independently
- ✅ **Consistent process** - Every task follows same workflow
- ✅ **Automatic documentation** - Knowledge captured from PRs
- ✅ **Quality gates** - Validation runs on every PR
- ✅ **Team alignment** - Clear visibility into work

## Quick Start

### 1. Bootstrap the Repository

```bash
# Clone repository
git clone <repo-url>
cd <repo-directory>

# Run bootstrap script
./scripts/bootstrap.sh

# Verify setup
./scripts/test-workflow.sh
```

### 2. Create Your First Issue

1. Go to **Issues** → **New Issue**
2. Select **Task** template
3. Fill in required fields:
   - **Task Title**: Brief description
   - **Context**: Background and requirements
   - **Acceptance Criteria**: Definition of done
   - **Priority**: Low/Medium/High/Critical
4. Assign to **@copilot**
5. Click **Submit new issue**

### 3. Watch the Workflow

1. **Issue Created** - @copilot is automatically assigned
2. **Branch Created** - Feature branch for the work
3. **Implementation** - @copilot makes changes and commits
4. **PR Opened** - Draft PR with changes and description
5. **Validation** - GitHub Actions run tests and checks
6. **Review** - Team reviews and approves
7. **Merge** - PR merged to main branch
8. **Knowledge Capture** - Patterns extracted automatically

## Detailed Workflow

### Phase 1: Issue Creation

**Purpose**: Define what needs to be done

**Steps:**

1. **Choose Template**
   - Use "Task" template for @copilot work
   - Use other templates for bugs, features, etc.

2. **Fill Required Fields**
   - **Title**: Action-oriented (e.g., "Implement user authentication")
   - **Context**: Background, current state, constraints
   - **Acceptance Criteria**: Checkboxes defining "done"
   - **Priority**: Impact and urgency
   - **Resources**: Links to docs, examples, related issues

3. **Set Metadata**
   - **Assignee**: @copilot (auto-populated if using template)
   - **Labels**: Added automatically by template
   - **Projects**: Optional project board assignment
   - **Milestone**: Optional milestone

**Example:**

```markdown
Title: Implement user authentication with OAuth2

Context:
- No authentication system currently exists
- Need to support Google and GitHub OAuth
- Must work with existing user model in database

Acceptance Criteria:
- [ ] User can sign in with Google
- [ ] User can sign in with GitHub
- [ ] Session persists across browser restarts
- [ ] Tests pass with >90% coverage
- [ ] Documentation updated

Priority: High
```

### Phase 2: Assignment and Planning

**Purpose**: Agent prepares to work on the issue

**Automatic:**
- Issue assigned to @copilot via template
- Labels added: `task`, `copilot`
- Notifications sent

**Agent Actions:**
1. Review issue details
2. Search knowledge base for related patterns
3. Check related issues and PRs
4. Identify dependencies
5. Plan implementation approach

### Phase 3: Implementation

**Purpose**: Agent implements the solution

**Branch Strategy:**

```bash
# Agent creates feature branch
git checkout -b feature/issue-42-oauth-authentication
```

**Development Process:**

1. **Code Changes**
   - Follow patterns from knowledge base
   - Apply coding standards from agents.md
   - Write tests alongside code

2. **Commits**
   - Use conventional commit format
   - Reference issue number
   - Clear, descriptive messages

   ```bash
   git commit -m "feat: Add OAuth2 authentication for Google

   Implements Google sign-in flow with passport.js.

   - Add Google OAuth strategy
   - Configure callback route
   - Add session management

   Refs #42"
   ```

3. **Testing**
   - Run tests locally
   - Fix any failures
   - Ensure coverage targets met

4. **Documentation**
   - Update README if needed
   - Add code comments
   - Update API docs

### Phase 4: Pull Request

**Purpose**: Submit changes for review

**PR Creation:**

```bash
# Push branch
git push -u origin feature/issue-42-oauth-authentication

# Create PR (via gh CLI or GitHub UI)
gh pr create --title "Implement OAuth2 authentication" \
  --body "$(cat <<EOF
## Summary
Implements OAuth2 authentication for Google and GitHub sign-in.

Closes #42

## Changes
- Add OAuth2 client configuration
- Implement callback handlers
- Add session management with Redis
- Update user model with OAuth provider field
- Add integration tests (>90% coverage)

## Testing
1. Start dev server: \`npm run dev\`
2. Navigate to /auth/google
3. Complete OAuth flow
4. Verify session persistence
5. Repeat with GitHub

## Screenshots
[Attach screenshots of login flow]
EOF
)"
```

**Auto-Assignment:**

GitHub Actions automatically:
- Assigns PR to creator (@copilot)
- Adds `auto-assigned` label
- Requests reviews from CODEOWNERS

### Phase 5: Validation

**Purpose**: Ensure quality and correctness

**Automatic Checks:**

1. **Syntax Validation** (`.github/workflows/validate-pr.yml`)
   - YAML files: `yamllint`
   - Shell scripts: `shellcheck`
   - Markdown: `markdownlint`
   - JSON: `python -m json.tool`

2. **Test Suite** (if `[test]` in commit message)
   - Unit tests
   - Integration tests
   - E2E tests
   - Coverage check

3. **Security Scan**
   - Check for exposed secrets
   - Validate file permissions
   - Scan dependencies

**Status Checks:**

All checks must pass before merge:
- ✅ Syntax validation
- ✅ Tests passed
- ✅ No security issues
- ✅ Code review approved

### Phase 6: Review

**Purpose**: Human verification and feedback

**Reviewer Actions:**

1. **Code Review**
   - Check logic and correctness
   - Verify tests are comprehensive
   - Ensure standards followed
   - Look for edge cases

2. **Testing**
   - Pull branch locally
   - Run application
   - Test acceptance criteria
   - Verify documentation

3. **Feedback**
   - Request changes if needed
   - Suggest improvements
   - Approve when ready

**Agent Response:**

1. Address feedback
2. Make requested changes
3. Respond to comments
4. Re-request review

### Phase 7: Merge

**Purpose**: Integrate changes into main branch

**Merge Strategy:**

- **Squash and Merge** (default)
  - Combines commits into one
  - Cleaner history
  - Preserves PR link

- **Merge Commit**
  - Keeps all commits
  - Full history
  - Use for complex features

- **Rebase and Merge**
  - Linear history
  - Individual commits preserved
  - Use for clean commit history

**Post-Merge:**

1. Delete feature branch
2. Close related issue
3. Trigger knowledge capture workflow

### Phase 8: Knowledge Capture

**Purpose**: Extract and preserve learnings

**Automatic Extraction** (`.github/workflows/knowledge-capture.yml`)

1. **Pattern Extraction**
   - Analyze changed files
   - Identify code patterns
   - Create pattern documents
   - Store in `docs/knowledge/patterns/`

2. **Insight Capture**
   - Extract commit messages
   - Mine PR comments
   - Collect metrics
   - Store in `docs/knowledge/insights/`

3. **Index Updates**
   - Update pattern count
   - Update insight count
   - Refresh search indexes

4. **Improvement Issues**
   - Create follow-up issues for `enhancement` label
   - Assign to @copilot
   - Link to extracted knowledge

**Manual Curation:**

Team reviews auto-extracted knowledge:
- Improve descriptions
- Add examples
- Link related patterns
- Merge duplicates

## For Developers

### Creating Issues for @copilot

**Good Issue:**

```markdown
Title: Add rate limiting to API endpoints

Context:
- Current API has no rate limiting
- Seeing abuse from some clients
- Need to protect against DoS
- Should be configurable per endpoint

Acceptance Criteria:
- [ ] Rate limiting middleware implemented
- [ ] Configurable limits per endpoint
- [ ] Clear error messages when limited
- [ ] Redis-backed for distributed systems
- [ ] Tests with >90% coverage
- [ ] Documentation for configuration

Priority: High
Complexity: Medium (1-4 hours)

Resources:
- Express rate-limit: https://www.npmjs.com/package/express-rate-limit
- Redis integration: (link)
```

**Bad Issue:**

```markdown
Title: Fix API

Context:
API is broken

Acceptance Criteria:
- [ ] Make it work

Priority: High
```

### Reviewing AI-Generated PRs

**What to Check:**

1. **Correctness**
   - Does it solve the issue?
   - Are there bugs or edge cases?
   - Is the logic sound?

2. **Tests**
   - Comprehensive coverage?
   - Tests actually test behavior?
   - No test gaming (skipped, weakened)?

3. **Code Quality**
   - Follows coding standards?
   - Clear and maintainable?
   - Appropriate abstractions?

4. **Security**
   - No exposed secrets?
   - Input validation?
   - Proper error handling?

5. **Documentation**
   - Code comments for complex logic?
   - README updated?
   - API docs current?

**Providing Feedback:**

```markdown
## Code Review

### Strengths
- Clean implementation of OAuth flow
- Comprehensive test coverage
- Good error handling

### Requested Changes
1. **Security**: Add CSRF protection to OAuth callback
2. **Testing**: Add test for expired tokens
3. **Documentation**: Update API docs with OAuth endpoints

### Suggestions (optional)
- Consider extracting OAuth config to separate file
- Could add logging for failed login attempts
```

## For AI Agents

### Processing Assigned Issues

**Workflow:**

1. **Parse Issue**
   - Read title, context, acceptance criteria
   - Identify requirements and constraints
   - Note priority and complexity

2. **Research**
   - Search knowledge base: `docs/knowledge/`
   - Review related issues/PRs
   - Check coding standards: `.github/agents.md`

3. **Plan**
   - Break down into subtasks
   - Identify dependencies
   - Choose implementation approach

4. **Implement**
   - Create feature branch
   - Write code with tests
   - Follow conventions
   - Commit incrementally

5. **Document**
   - Add code comments
   - Update documentation
   - Write clear PR description

6. **Submit**
   - Create draft PR
   - Link to issue
   - Request reviews

7. **Iterate**
   - Respond to feedback
   - Make requested changes
   - Re-request review

### Knowledge Base Integration

**Before implementing:**

```bash
# Search for authentication patterns
grep -r "authentication" docs/knowledge/patterns/

# Check for related decisions
grep -r "OAuth" docs/knowledge/decisions/

# Review similar insights
grep -r "auth" docs/knowledge/insights/
```

**After completing:**

Patterns are extracted automatically, but you can:
- Review extracted patterns for accuracy
- Suggest improvements via PR
- Create ADR for significant decisions

### Coding Standards

Follow standards in `.github/agents.md`:

- **Code Style**: Language-specific guides (Airbnb, PEP 8, etc.)
- **Testing**: >80% coverage, no test gaming
- **Commits**: Conventional commit format
- **Documentation**: Code comments explain "why"

## Configuration

### agents.md

Customize `.github/agents.md` for your project:

```markdown
## Tech Stack

**Languages:**
- (Your primary languages)

**Frameworks:**
- (Your frameworks)

**Tools:**
- (Your tooling)

## Coding Standards

(Your standards)

## Boundaries

(What agents should/shouldn't do)
```

### CODEOWNERS

Update `.github/CODEOWNERS` with your team:

```
# Default reviewers
* @team-lead @senior-dev

# Frontend
/src/frontend/ @frontend-team

# Backend
/src/backend/ @backend-team

# Infrastructure
/terraform/ @devops-team
```

### Issue Template

Customize `.github/ISSUE_TEMPLATE/task.yml`:

- Add/remove fields
- Change validation rules
- Update default assignees
- Modify labels

## Troubleshooting

### Issue Not Assigned

**Problem**: Issue created but @copilot not assigned

**Solution:**
- Check issue template has `assignees: - copilot`
- Verify @copilot user exists in org
- Manually assign via issue UI

### PR Not Auto-Assigned

**Problem**: PR created but creator not assigned

**Solution:**
- Check workflow: `.github/workflows/assign-pr-creator.yml`
- Verify workflow has `pull-requests: write` permission
- Check Actions tab for errors

### Validation Failing

**Problem**: Syntax validation fails on PR

**Solution:**
- Run locally: `./scripts/validate-syntax.sh`
- Fix reported issues
- Push fixes
- Re-run workflow

### Knowledge Not Captured

**Problem**: PR merged but no patterns extracted

**Solution:**
- Check workflow: `.github/workflows/knowledge-capture.yml`
- Verify runs on `pull_request.closed` with `merged == true`
- Check Actions tab for errors
- Run manually: `./scripts/extract-patterns.sh --pr 123`

## Best Practices

### For Issues

- ✅ **Be specific** - Clear, actionable tasks
- ✅ **Include context** - Background and constraints
- ✅ **Define done** - Observable acceptance criteria
- ✅ **Link resources** - Docs, examples, related issues
- ❌ **Don't be vague** - "Fix the thing" is not helpful

### For PRs

- ✅ **Small and focused** - One feature or fix per PR
- ✅ **Comprehensive tests** - Don't game the tests
- ✅ **Clear description** - What, why, how
- ✅ **Link to issue** - Use "Closes #123"
- ❌ **Don't mix concerns** - Separate refactoring from features

### For Reviews

- ✅ **Be constructive** - Suggest improvements
- ✅ **Test locally** - Don't just read code
- ✅ **Check tests** - Ensure quality not quantity
- ✅ **Document decisions** - Create ADRs for significant choices
- ❌ **Don't rubber-stamp** - Actually review

### For Knowledge

- ✅ **Review auto-generated** - Improve pattern descriptions
- ✅ **Link related** - Connect patterns, decisions, insights
- ✅ **Keep current** - Archive obsolete knowledge
- ✅ **Use it** - Reference in PRs and issues
- ❌ **Don't hoard** - Delete duplicates and outdated content

## Success Metrics

Track workflow effectiveness:

- **Issue Resolution Time** - Time from creation to closure
- **PR Merge Time** - Time from creation to merge
- **Review Cycles** - Average rounds of review
- **Test Reliability** - % of PRs passing first validation
- **Knowledge Growth** - Patterns/insights added per month
- **Knowledge Usage** - References to knowledge base

## Resources

- **Bootstrap Script**: `scripts/bootstrap.sh`
- **Validation Script**: `scripts/validate-syntax.sh`
- **Test Script**: `scripts/test-workflow.sh`
- **Pattern Extraction**: `scripts/extract-patterns.sh`
- **Knowledge Base**: `docs/knowledge/`
- **Agent Config**: `.github/agents.md`

## Support

Need help?

- **Documentation Issues**: Create issue with `documentation` label
- **Workflow Problems**: Create issue with `workflow` label
- **Agent Behavior**: Review `.github/agents.md` and create issue
- **General Questions**: Ask in discussions or team chat

---

**Version**: 1.0.0
**Last Updated**: 2026-01-08
**Maintained By**: Development Team
