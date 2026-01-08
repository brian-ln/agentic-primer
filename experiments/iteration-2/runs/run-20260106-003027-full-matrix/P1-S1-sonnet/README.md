# Issue-Driven Development with GitHub Copilot

This repository uses **GitHub Copilot coding agent** for autonomous issue-to-PR workflows.

## Quick Start

### 1. Create an Issue

Use the **Copilot Task** template:
- Navigate to Issues → New Issue
- Select "Copilot Task" template
- Fill in all required fields
- Assign to `@copilot`

### 2. Copilot Works

When you assign the issue to `@copilot`:
1. ✅ GitHub Actions workflow triggers automatically
2. 🔍 Copilot analyzes issue + knowledge base
3. 🌿 Copilot creates branch: `copilot/issue-{number}`
4. 💻 Copilot implements changes with tests
5. 📝 Copilot opens draft PR for review

### 3. Review & Merge

The PR is automatically assigned to reviewers via `CODEOWNERS`:
1. Review Copilot's implementation
2. Request changes if needed (Copilot can iterate)
3. Approve and merge when satisfied

## Workflow Diagram

```
┌─────────────────┐
│  Create Issue   │
│  (use template) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Assign @copilot │
└────────┬────────┘
         │
         ▼
┌─────────────────────┐
│ GitHub Actions      │
│ - Add label         │
│ - Load knowledge    │
│ - Trigger Copilot   │
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│ Copilot Works       │
│ - Create branch     │
│ - Write code        │
│ - Add tests         │
│ - Push commits      │
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│ Open Draft PR       │
│ - Auto-assign       │
│   reviewers (owner) │
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│ Code Review         │
│ - Request changes?  │
│ - Approve?          │
└────────┬────────────┘
         │
         ▼
┌─────────────────────┐
│ Merge to main       │
│ - Issue auto-closes │
└─────────────────────┘
```

## Repository Structure

```
.
├── .github/
│   ├── ISSUE_TEMPLATE/
│   │   └── copilot-task.yml          # Structured issue template
│   ├── workflows/
│   │   └── copilot-automation.yml    # Auto-trigger on assignment
│   └── CODEOWNERS                    # Auto-assign reviewers
│
├── docs/
│   └── knowledge/                    # AI-accessible knowledge base
│       ├── patterns/                 # Reusable design patterns
│       ├── decisions/                # Architecture decision records
│       └── insights/                 # Lessons learned
│
├── src/                              # Application code
├── tests/                            # Test suites
└── README.md                         # This file
```

## Knowledge Base

The `docs/knowledge/` directory provides context for Copilot:

### Patterns (`docs/knowledge/patterns/`)
Reusable solutions to common problems:
- API error handling
- Authentication flows
- Database query patterns
- Testing strategies

**When to use:** Reference in issues when you want Copilot to follow existing patterns.

### Decisions (`docs/knowledge/decisions/`)
Architecture Decision Records (ADRs) explaining **why** we chose specific approaches:
- Technology selections (REST vs GraphQL)
- Framework choices
- Security approaches

**When to use:** Reference when implementing features related to past architectural decisions.

### Insights (`docs/knowledge/insights/`)
Empirical learnings from production experience:
- Performance optimizations
- Tool best practices
- Post-mortem learnings
- Process improvements

**When to use:** Reference when working on similar problems to avoid past mistakes.

## Writing Good Issues for Copilot

### ✅ Do This

```yaml
Title: Add user authentication endpoint

Description: |
  Implement JWT-based authentication for the API.
  Users should be able to log in with email/password
  and receive a token for subsequent requests.

Acceptance Criteria:
  - [ ] POST /api/auth/login endpoint accepts email and password
  - [ ] Valid credentials return JWT token (1 hour expiry)
  - [ ] Invalid credentials return 401 with error message
  - [ ] Password is never logged or exposed
  - [ ] Tests cover success and failure cases
  - [ ] All existing tests still pass

Priority: High

Files to Modify:
  - src/api/auth/login.js
  - tests/api/auth.test.js
  - docs/api/authentication.md

Knowledge References:
  - docs/knowledge/patterns/api-error-handling.md
  - docs/knowledge/decisions/001-use-rest-api.md
```

### ❌ Don't Do This

```yaml
Title: Fix the login

Description: |
  The login thing isn't working right. Make it better.

Acceptance Criteria:
  - [ ] It should work
```

## Best Practices

### For Issue Authors

1. **Be specific** - Clear requirements = better results
2. **Use checkboxes** - Testable acceptance criteria
3. **Suggest files** - Guide Copilot to relevant code
4. **Link knowledge** - Reference patterns/decisions/insights
5. **Set priority** - Helps Copilot scope effort

### For Knowledge Base Maintainers

1. **Keep patterns updated** - Copilot uses latest content
2. **Include code examples** - Working code > descriptions
3. **Use clear filenames** - Helps discoverability
4. **Cross-reference** - Link related docs

### For Reviewers

1. **Check knowledge alignment** - Did Copilot follow patterns?
2. **Verify acceptance criteria** - All checkboxes satisfied?
3. **Test thoroughly** - Copilot can miss edge cases
4. **Update knowledge** - Add new insights from reviews

## GitHub Actions Workflow

The automation workflow (`.github/workflows/copilot-automation.yml`) triggers when:
- **Event:** Issue is assigned
- **Condition:** Assignee is `@copilot`

**Steps:**
1. Add `copilot-processing` label
2. Post acknowledgment comment
3. Load knowledge base context
4. Trigger Copilot coding agent (in production)
5. Create branch and push initial commit
6. Update issue with status

**Permissions required:**
- `issues: write` - Add labels, post comments
- `pull-requests: write` - Create PRs
- `contents: write` - Push to branches

## CODEOWNERS Auto-Review

The `.github/CODEOWNERS` file automatically requests reviews:

```
# Pattern                  Reviewer(s)
*                          @owner          # Default: all files
/.github/                  @owner          # GitHub configs
/docs/knowledge/           @owner          # Knowledge base
*.yml                      @owner          # YAML configs
```

**How it works:**
1. Copilot opens PR with changes
2. GitHub compares changed files to CODEOWNERS patterns
3. Matching patterns → auto-request review from those users
4. Reviewers receive notification

**Customization:**
- Replace `@owner` with actual GitHub usernames or team names (e.g., `@myorg/backend-team`)
- Add specific patterns for different file types or directories
- Limit to 3 MB file size

## Troubleshooting

### Issue assigned but workflow didn't trigger

**Check:**
1. Is assignee exactly `@copilot`? (case-sensitive)
2. Is workflow file in `.github/workflows/` on default branch?
3. Do repository settings allow Actions to run?

**Fix:** Verify workflow syntax with `yamllint .github/workflows/copilot-automation.yml`

### Copilot created PR but wrong reviewers assigned

**Check:**
1. Is `.github/CODEOWNERS` on the base branch (usually `main`)?
2. Do usernames in CODEOWNERS match actual GitHub users?
3. Is file under 3 MB?

**Fix:** Update CODEOWNERS and merge to main before next PR.

### Knowledge base not being used

**Check:**
1. Are markdown files in `docs/knowledge/` subdirectories?
2. Did you reference them in "Knowledge References" field?
3. Are filenames descriptive (helps Copilot find them)?

**Fix:** Explicitly link knowledge docs in issue description.

### Copilot's implementation doesn't match expectations

**Check:**
1. Are acceptance criteria specific and testable?
2. Did you provide file hints?
3. Is there a relevant pattern Copilot should follow?

**Fix:** Request changes on PR with specific guidance. Copilot can iterate.

## Configuration Files

### Issue Template (`.github/ISSUE_TEMPLATE/copilot-task.yml`)

**Purpose:** Structured YAML form for consistent issue creation

**Fields:**
- `title` - Task title
- `description` - Detailed requirements
- `acceptance-criteria` - Testable checkboxes
- `priority` - Urgency level
- `files-to-modify` - File hints
- `knowledge-references` - Links to docs
- `additional-context` - Screenshots, links

**Customization:** Add project-specific fields (e.g., `epic`, `team`, `milestone`)

### Workflow (`.github/workflows/copilot-automation.yml`)

**Purpose:** Automate Copilot invocation on issue assignment

**Trigger:** `issues.assigned` + `assignee == 'copilot'`

**Actions:**
- `actions/checkout@v4` - Clone repository
- `actions/github-script@v7` - Add labels, comments
- Custom steps - Load knowledge, trigger Copilot

**Customization:** Add custom setup steps (e.g., install dependencies, run linters)

### CODEOWNERS (`.github/CODEOWNERS`)

**Purpose:** Auto-assign reviewers based on changed files

**Syntax:** `{pattern} {owner1} {owner2} ...`

**Examples:**
```
*.js @javascript-team
/api/ @backend-team @security-team
docs/ @technical-writers
```

**Customization:** Replace `@owner` with your team's usernames/teams

## Advanced Usage

### Custom Instructions

Create `.github/copilot-instructions.md` with project-specific guidance:

```markdown
# Project Context for Copilot

## Code Style
- Use ESLint with Airbnb config
- Prefer functional components in React
- All functions must have JSDoc comments

## Testing Requirements
- Jest for unit tests (coverage > 80%)
- Playwright for E2E tests
- Test file naming: `*.test.js`

## Architecture
- Follow repository pattern for data access
- Services layer for business logic
- Controllers are thin (routing only)
```

Copilot will read this before working on issues.

### Setup Steps

Create `.github/copilot-setup-steps.yml` to pre-install dependencies:

```yaml
steps:
  - name: Install dependencies
    run: npm ci

  - name: Setup database
    run: npm run db:setup:test

  - name: Run linter
    run: npm run lint
```

Runs before Copilot starts coding.

### Multiple Issue Templates

Create additional templates for different task types:

```
.github/ISSUE_TEMPLATE/
├── copilot-task.yml        # General tasks
├── copilot-bugfix.yml      # Bug fixes
├── copilot-refactor.yml    # Refactoring
└── copilot-docs.yml        # Documentation
```

Each can have different fields and default labels.

## Metrics and Success

Track these metrics to measure effectiveness:

### Issue-to-PR Time
Time from assignment to PR creation. **Target:** < 10 minutes

### First-Time Success Rate
PRs merged without requesting changes. **Target:** > 70%

### Review Rounds
Average number of review iterations. **Target:** < 2

### Test Coverage
Coverage of Copilot-generated code. **Target:** > 80%

### Knowledge Base Usage
% of issues referencing knowledge docs. **Target:** > 50%

## Contributing

### Adding Patterns

1. Create `docs/knowledge/patterns/{name}.md`
2. Follow pattern template (see `patterns/README.md`)
3. Include working code examples
4. Link from related decisions/insights
5. Update `patterns/README.md` index

### Adding Decisions

1. Create `docs/knowledge/decisions/{NNN-title}.md`
2. Use MADR format (see `decisions/README.md`)
3. Fill all sections (no placeholders!)
4. Discuss with team before marking "Accepted"
5. Update `decisions/README.md` index

### Adding Insights

1. Create `docs/knowledge/insights/{name}.md`
2. Include evidence (metrics, logs, data)
3. Make recommendations actionable
4. Link related patterns/decisions
5. Update `insights/README.md` index

## Resources

### GitHub Copilot Documentation
- [Copilot coding agent overview](https://code.visualstudio.com/docs/copilot/copilot-coding-agent)
- [Best practices for using Copilot](https://docs.github.com/copilot/how-tos/agents/copilot-coding-agent/best-practices-for-using-copilot-to-work-on-tasks)
- [Assigning issues to Copilot](https://github.blog/ai-and-ml/github-copilot/assigning-and-completing-issues-with-coding-agent-in-github-copilot/)

### Issue Templates
- [GitHub issue forms documentation](https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/syntax-for-issue-forms)
- [Creating issue templates](https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/configuring-issue-templates-for-your-repository)

### CODEOWNERS
- [About code owners](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)
- [CODEOWNERS syntax](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners#codeowners-syntax)

### Knowledge Engineering
- [Architecture Decision Records](https://adr.github.io/)
- [MADR (Markdown ADR)](https://adr.github.io/madr/)
- [Pattern Libraries](https://link.springer.com/chapter/10.1007/978-3-540-24750-0_10)

## License

[Your License Here]

## Support

**Questions?** Open an issue with label `question`

**Problems?** Check [Troubleshooting](#troubleshooting) section

**Feature requests?** Open an issue with label `enhancement`

---

**Last Updated:** 2026-01-06
**Maintained By:** @owner
**Copilot Version:** GitHub Copilot coding agent (2025+)
