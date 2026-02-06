# Issue-Driven Development with @copilot

A streamlined workflow for autonomous task execution by GitHub Copilot.

## Quick Start

1. **Create an issue** using the [Copilot Task template](.github/ISSUE_TEMPLATE/task.yml)
2. **@copilot picks it up** automatically (labeled `copilot`)
3. **Review the PR** via GitHub web UI
4. **Merge** and the issue closes automatically

## Workflow

```
┌─────────────────┐
│  1. CREATE      │ Use "Copilot Task" template
│     ISSUE       │ Fill in: type, description, acceptance criteria
└────────┬────────┘
         │
         v
┌─────────────────┐
│  2. @COPILOT    │ Automatically assigned via label
│     WORKS       │ Implements solution, runs tests
└────────┬────────┘
         │
         v
┌─────────────────┐
│  3. PR CREATED  │ CODEOWNERS assigns reviewer
│                 │ Links back to issue
└────────┬────────┘
         │
         v
┌─────────────────┐
│  4. REVIEW      │ Human reviews via web UI
│     & MERGE     │ Merge closes linked issue
└─────────────────┘
```

## Components

### Issue Template

Location: `.github/ISSUE_TEMPLATE/task.yml`

Creates structured tasks with:
- **Task Type**: feature, bugfix, refactor, test, docs, chore
- **Description**: What needs to be done
- **Acceptance Criteria**: Verifiable success conditions
- **Technical Context**: File locations, dependencies, constraints
- **Priority**: low, medium, high, critical

### CODEOWNERS

Location: `.github/CODEOWNERS`

Automatically assigns reviewers to PRs. Configure with your GitHub usernames:

```
# Default reviewer for all files
* @your-username

# Team-specific overrides
/docs/ @docs-team
/src/api/ @backend-team
```

### Knowledge Base

Location: `docs/knowledge/`

Structured repository of institutional knowledge:

| Directory | Purpose | Example |
|-----------|---------|---------|
| `patterns/` | Reusable code patterns | Error handling, API responses |
| `decisions/` | Architecture Decision Records | Why we chose X over Y |
| `insights/` | Lessons learned, gotchas | "Don't use X because..." |

## Writing Good Tasks

### Do

- Be specific about expected behavior
- Include file paths when relevant
- List testable acceptance criteria
- Provide context from related issues

### Don't

- Leave acceptance criteria vague
- Require human decisions mid-task
- Include credentials or secrets
- Assume knowledge not in the issue

### Example Task

```markdown
Title: Add rate limiting to API endpoints

Type: feature

Description:
Implement rate limiting for all /api/* endpoints.
- 100 requests per minute per IP
- Return 429 Too Many Requests when exceeded
- Include Retry-After header

Acceptance Criteria:
- [ ] Rate limiter middleware added to /api routes
- [ ] Limit of 100 req/min per IP address
- [ ] 429 response with Retry-After header when exceeded
- [ ] Unit tests for rate limiting logic
- [ ] Integration test for end-to-end behavior

Technical Context:
- Express.js application
- Use express-rate-limit package
- Config in src/config/rateLimits.ts
```

## Reviewing Copilot PRs

When reviewing a PR from @copilot:

1. **Verify acceptance criteria** - Did it complete all checkboxes?
2. **Check test coverage** - Are edge cases handled?
3. **Review code quality** - Does it follow project patterns?
4. **Validate security** - No exposed secrets or vulnerabilities?
5. **Test locally** - If uncertain, pull and verify

## Troubleshooting

| Issue | Solution |
|-------|----------|
| @copilot not picking up issue | Verify `copilot` label is applied |
| PR not assigned reviewer | Check CODEOWNERS syntax |
| Task too complex | Break into smaller issues |
| Missing context | Add to Technical Context field |

## Contributing to Knowledge Base

After completing tasks, capture learnings:

1. **New patterns** -> `docs/knowledge/patterns/`
2. **Architecture decisions** -> `docs/knowledge/decisions/`
3. **Gotchas and tips** -> `docs/knowledge/insights/`

This keeps the team's knowledge growing with each task.
