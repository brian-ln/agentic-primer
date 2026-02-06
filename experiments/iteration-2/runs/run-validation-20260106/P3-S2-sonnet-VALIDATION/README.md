# Issue-Driven Development with @copilot

**Turn GitHub issues into automated execution with human oversight.**

This repository uses @copilot to autonomously execute tasks defined in GitHub issues, creating a workflow where humans provide direction and review while @copilot handles implementation.

## Quick Start

### Creating a Task for @copilot

1. **Go to GitHub Issues** → Click "New Issue"
2. **Select "@copilot Task" template**
3. **Fill in the form:**
   - Task title and description
   - Acceptance criteria (how you'll know it's done)
   - Priority and complexity estimates
   - Additional context (optional)
4. **Create issue** → @copilot is automatically notified
5. **Wait for PR** → @copilot will implement and create a pull request
6. **Review and merge** → Use GitHub web UI to review and merge

### Reviewing @copilot's Work

1. **PR notification** → You'll be auto-assigned as reviewer (via CODEOWNERS)
2. **Review changes** → Check code, tests, and documentation
3. **Review knowledge base updates** → Check `docs/knowledge/` for new patterns/insights
4. **Request changes or approve** → Use GitHub's review interface
5. **Merge when ready** → PR merges, issue closes automatically

## Workflow Diagram

```
┌─────────────────┐
│  Human creates  │
│  GitHub Issue   │
│  (@copilot tag) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ GitHub Action   │
│ notifies        │
│ @copilot        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  @copilot       │
│  - Reads issue  │
│  - Creates      │
│    branch       │
│  - Implements   │
│  - Runs tests   │
│  - Updates docs │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  @copilot       │
│  creates PR     │
│  (auto-assigned │
│   to @owner)    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Human reviews  │
│  via GitHub UI  │
│  - Code         │
│  - Tests        │
│  - Knowledge    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Human approves │
│  and merges     │
│  (issue closes) │
└─────────────────┘
```

## System Components

### 1. Issue Template (`.github/ISSUE_TEMPLATE/task.yml`)

Structured form for creating @copilot tasks with:
- Task description and acceptance criteria
- Priority and complexity estimates
- Context and related work
- Required aspects (tests, docs, knowledge base)

**Why:** Ensures @copilot has all necessary information to execute successfully.

### 2. CODEOWNERS (`.github/CODEOWNERS`)

Automatically assigns all PRs to repository owner for review.

**Why:** Ensures human oversight on all @copilot changes before merging.

### 3. Workflow Trigger (`.github/workflows/copilot-notify.yml`)

GitHub Action that triggers when issues are created with `@copilot` label.

**Why:** Automates the handoff from issue creation to @copilot execution.

### 4. Knowledge Base (`docs/knowledge/`)

Git-tracked memory that accumulates over time:

- **`patterns/`** - Reusable implementation patterns
- **`decisions/`** - Architecture Decision Records (ADRs)
- **`insights/`** - Learnings and observations

**Why:** @copilot learns from past work, avoiding mistakes and reusing solutions.

## Example: Creating an Authentication Feature

### Step 1: Create Issue

```
Title: [Task]: Add user authentication

Description:
Implement JWT-based authentication system with login/logout functionality.

Acceptance Criteria:
- [ ] Users can log in with email/password
- [ ] JWT tokens generated on successful login
- [ ] Protected routes verify valid tokens
- [ ] Users can log out (invalidate token)
- [ ] Tests achieve 80%+ coverage
- [ ] API documentation updated

Priority: High
Complexity: Medium
```

### Step 2: @copilot Executes

```bash
# @copilot automatically:
1. Creates branch: feature/issue-42-user-authentication
2. Implements authentication:
   - src/auth/jwt.js (token generation)
   - src/auth/middleware.js (route protection)
   - src/routes/auth.js (login/logout endpoints)
3. Adds tests:
   - tests/auth/jwt.test.js
   - tests/auth/middleware.test.js
4. Updates documentation:
   - docs/api/authentication.md
5. Adds knowledge:
   - docs/knowledge/patterns/jwt-authentication.md
   - docs/knowledge/decisions/001-jwt-vs-sessions.md
6. Creates PR #43 linked to issue #42
```

### Step 3: Review and Merge

```
PR #43: Implement user authentication (resolves #42)
Reviewer: @owner (auto-assigned via CODEOWNERS)

Files changed:
+ src/auth/jwt.js
+ src/auth/middleware.js
+ src/routes/auth.js
+ tests/auth/*.test.js
+ docs/api/authentication.md
+ docs/knowledge/patterns/jwt-authentication.md
+ docs/knowledge/decisions/001-jwt-vs-sessions.md

Review → Approve → Merge → Issue #42 closes
```

## Knowledge Base

The knowledge base in `docs/knowledge/` is the system's memory:

### Patterns

Reusable solutions to common problems.

**Example:** `docs/knowledge/patterns/error-handling-with-retry.md`

```markdown
# Pattern: Error Handling with Exponential Backoff

Problem: API calls may fail temporarily
Solution: Retry with increasing delays (1s, 2s, 4s, ...)
When to use: External APIs, network operations
When NOT to use: User input errors, permanent failures
```

### Decisions

Important architectural choices and their rationale.

**Example:** `docs/knowledge/decisions/001-jwt-authentication.md`

```markdown
# ADR 001: Use JWT for API Authentication

Status: Accepted
Context: Need stateless auth for microservices
Decision: Use JWT with 15-min access tokens
Consequences:
  + Stateless, scalable
  - Cannot revoke until expiry
Alternatives: Sessions (requires shared state)
```

### Insights

Learnings and observations from execution.

**Example:** `docs/knowledge/insights/copilot-testing-patterns.md`

```markdown
# Insight: @copilot Generates Better Tests with Examples

Observation: When issues include example test cases,
@copilot produces more comprehensive tests.

Implication: Update issue template to encourage examples.
```

### Using Knowledge

**For @copilot:**
- Read patterns before implementing similar features
- Follow decisions unless there's good reason not to
- Learn from insights to avoid repeating mistakes
- Add new knowledge during task execution

**For Humans:**
- Review knowledge base updates during PR review
- Suggest patterns when reviewing @copilot's code
- Document insights from @copilot's behavior
- Evolve patterns based on what works

## Best Practices

### Writing Good Issues for @copilot

**DO:**
- ✅ Describe WHAT to achieve (outcomes), not HOW (implementation)
- ✅ Provide clear acceptance criteria
- ✅ Include context and constraints
- ✅ Link to related issues and docs
- ✅ Give examples when helpful

**DON'T:**
- ❌ Prescribe implementation details (let @copilot decide)
- ❌ Be vague about success criteria
- ❌ Assume @copilot knows implicit context
- ❌ Skip priority/complexity estimates

### Example: Good vs Bad Issues

**Good Issue:**
```
Title: Add user authentication

Description: Users need to log in to access protected features.
Implement authentication using industry-standard practices.

Acceptance Criteria:
- Users can log in with email/password
- Protected routes require authentication
- Tokens expire after reasonable time
- Tests cover happy path and error cases

Constraints:
- Must work with existing user model
- Must support mobile and web clients
```

**Bad Issue:**
```
Title: Do auth

Description: Add authentication. Use JWT. Create auth.js file
in src/auth/. Use bcrypt for passwords. Set token expiry to
900 seconds. Add middleware to app.js.

Acceptance Criteria: It works
```

The good issue specifies WHAT (outcomes), the bad issue specifies HOW (implementation).

### Reviewing @copilot's PRs

**Check:**
1. **Functionality** - Does it meet acceptance criteria?
2. **Code quality** - Is it maintainable and well-structured?
3. **Tests** - Are edge cases covered? Do tests actually test?
4. **Documentation** - Is usage clear? Are changes documented?
5. **Knowledge** - Did @copilot document patterns/decisions/insights?
6. **No regressions** - Did existing tests pass?

**Don't:**
- Don't approve without actually reviewing
- Don't merge failing tests
- Don't skip knowledge base review

## Troubleshooting

### @copilot Didn't Respond to Issue

**Check:**
1. Does issue have `@copilot` label?
2. Did GitHub Action run? (Check Actions tab)
3. Is @copilot system operational?

### PR Doesn't Meet Acceptance Criteria

**Options:**
1. Request changes via PR review
2. Add clarifying comments
3. Close PR and update issue with more details
4. Collaborate with @copilot in PR comments

### Knowledge Base Getting Cluttered

**Maintenance:**
1. Review patterns quarterly - archive outdated ones
2. Mark superseded decisions clearly
3. Consolidate related insights
4. Keep content concise and actionable

## System Requirements

- GitHub repository (public or private)
- GitHub Actions enabled
- @copilot integration configured
- Repository owner has review permissions

## Files and Directories

```
.github/
├── ISSUE_TEMPLATE/
│   └── task.yml                    # @copilot task template
├── CODEOWNERS                      # Auto-assign reviewers
└── workflows/
    └── copilot-notify.yml          # Issue → @copilot trigger

docs/
└── knowledge/                      # Knowledge base
    ├── README.md                   # Knowledge base guide
    ├── patterns/                   # Reusable patterns
    │   └── README.md
    ├── decisions/                  # Architecture decisions
    │   └── README.md
    └── insights/                   # Learnings & observations
        └── README.md

README.md                           # This file
```

## Advanced Usage

### Multiple @copilot Instances

Use labels to route to different @copilot instances:
- `@copilot-frontend` - Frontend tasks
- `@copilot-backend` - Backend tasks
- `@copilot-infra` - Infrastructure tasks

Update workflow trigger to filter by specific labels.

### Custom Workflows

Extend `copilot-notify.yml` to:
- Run preliminary checks before @copilot starts
- Notify team channels (Slack, Discord)
- Trigger additional automation
- Collect metrics on @copilot performance

### Integration with CI/CD

@copilot PRs automatically trigger:
- Test suites
- Linting and formatting checks
- Security scans
- Build validation

Require all checks to pass before allowing merge.

## Success Metrics

Track system effectiveness:

**For @copilot:**
- Task completion rate (PRs merged / issues created)
- Time to PR (issue created → PR opened)
- Review cycles (changes requested per PR)
- Test coverage in generated code

**For Knowledge Base:**
- Pattern reuse (how often patterns are referenced)
- Decision relevance (how often ADRs inform new work)
- Insight value (insights that prevent bugs or save time)

**For Team:**
- Time saved (manual implementation vs @copilot)
- Code quality (bugs in @copilot code vs human code)
- Onboarding speed (new team members learning from knowledge base)

## Getting Help

**For issues with:**
- **Issue template** - See `.github/ISSUE_TEMPLATE/task.yml`
- **Workflow** - See `.github/workflows/copilot-notify.yml`
- **Knowledge base** - See `docs/knowledge/README.md`
- **@copilot behavior** - Check `docs/knowledge/insights/` for known quirks

## Contributing

Contributions welcome! To contribute:

1. Create issue using @copilot task template
2. Let @copilot implement and create PR
3. Review PR and provide feedback
4. Merge when ready

For knowledge base contributions:
- Add patterns for reusable solutions
- Document decisions for important choices
- Capture insights from interesting observations

## License

[Your license here]

## Acknowledgments

This system was bootstrapped from a 30-word prompt and iteratively improved through @copilot execution and human review. The knowledge base grows with each task completed.

---

**Ready to start?** Create your first issue using the @copilot task template!
