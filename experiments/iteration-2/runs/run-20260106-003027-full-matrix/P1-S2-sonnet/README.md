# Project with @copilot Issue Automation

This repository is equipped with GitHub Copilot agent automation for issue-driven development, complete with auto-review and knowledge base integration.

## Quick Start

### Assign a Task to @copilot

1. **Create a new issue** using the [Copilot Task template](.github/ISSUE_TEMPLATE/copilot-task.yml)
2. **Fill in the details:**
   - Task summary and description
   - Context and background
   - Success criteria
   - Priority
3. **Submit the issue** - The @copilot agent will automatically start working on it

### What Happens Next

```
Issue Created → Workflow Triggered → Agent Works → PR Opened → Auto-Review Runs → Human Reviews → Merge
```

1. **Issue Labeled:** Your issue is automatically labeled with `copilot-task`
2. **Workflow Triggers:** GitHub Actions detects the new issue and starts the agent
3. **Agent Works:** @copilot analyzes the task, explores the codebase, and implements changes
4. **PR Created:** Agent opens a pull request with proposed changes
5. **Auto-Review:** Automated quality checks run (syntax, tests, linting)
6. **Human Review:** Repository owner is assigned to review the PR
7. **Merge:** After approval, changes are merged into the main branch

## Workflow Details

### Issue Template

Use the structured issue template to provide clear, complete task descriptions:

- **Task Summary:** One-line description
- **Detailed Description:** What needs to be built/fixed/improved
- **Context:** Related issues, affected files, dependencies
- **Success Criteria:** How to verify the task is complete
- **Priority:** Urgency level (Low/Medium/High/Critical)

Good task descriptions help the agent produce better results on the first try.

### GitHub Actions Workflow

The [copilot-agent.yml](.github/workflows/copilot-agent.yml) workflow:

- Triggers on issue creation with `copilot-task` label
- Creates a branch named `copilot/issue-{number}`
- Simulates agent work (exploring, implementing, testing)
- Commits changes and pushes to the branch
- Opens a pull request linking to the original issue
- Posts status updates as comments

### Auto-Review Script

The [auto-review.sh](scripts/auto-review.sh) script validates PRs before human review:

- **YAML Syntax:** Validates all `.yml` and `.yaml` files with yamllint
- **Shell Scripts:** Checks all `.sh` files with shellcheck
- **Markdown:** Lints all `.md` files with markdownlint
- **Tests:** Runs test suite if available (npm test, make test)
- **Comments:** Posts findings as PR comments

This ensures reviewers can focus on logic and design rather than syntax errors.

### Code Ownership

The [CODEOWNERS](.github/CODEOWNERS) file ensures:

- All PRs are automatically assigned to the repository owner
- Human review is required before merging (agent cannot self-approve)
- Clear responsibility for code quality and maintenance

### Knowledge Base

The [docs/knowledge/](docs/knowledge/) directory captures institutional knowledge:

- **Patterns:** Reusable code patterns and best practices
- **Decisions:** Architecture Decision Records (ADRs) documenting important choices
- **Insights:** Lessons learned from retrospectives and post-mortems

This knowledge base is:
- Version controlled (tracked in git)
- Searchable (via GitHub search)
- AI-accessible (agents can read and contribute)
- Human-friendly (markdown format)

## Usage Examples

### Example 1: Add a New Feature

```markdown
Issue Title: [TASK] Add user authentication to API

Task Summary: Implement JWT-based authentication for API endpoints

Detailed Description:
Add JWT token authentication to protect API endpoints. Users should be able to:
- Register with email/password
- Login and receive a JWT token
- Access protected endpoints with valid token
- Handle token expiration and refresh

Success Criteria:
- [ ] User registration endpoint (/api/register)
- [ ] User login endpoint (/api/login)
- [ ] JWT middleware protects existing endpoints
- [ ] Tests cover authentication flow
- [ ] Documentation updated with auth examples
```

### Example 2: Fix a Bug

```markdown
Issue Title: [TASK] Fix pagination bug in user list endpoint

Task Summary: User list endpoint returns wrong page when offset is used

Detailed Description:
The /api/users endpoint has a pagination bug. When offset > 0, it returns
duplicate users from previous pages. This breaks client-side pagination.

Context:
- Affected file: src/api/users.js
- Related issue: #456
- User report: https://example.com/bug-report

Success Criteria:
- [ ] Pagination returns correct users for all offset values
- [ ] No duplicate users in paginated results
- [ ] Existing pagination tests still pass
- [ ] New test covers offset edge cases
```

### Example 3: Improve Documentation

```markdown
Issue Title: [TASK] Add API documentation with examples

Task Summary: Create comprehensive API documentation with request/response examples

Detailed Description:
Add documentation for all API endpoints including:
- Endpoint URLs and HTTP methods
- Request parameters and body format
- Response format and status codes
- Example requests with curl
- Error handling examples

Success Criteria:
- [ ] Documentation covers all endpoints
- [ ] Examples are copy-paste ready
- [ ] Error codes explained
- [ ] Documentation linked from README
```

## Web UI Workflow

### Creating Issues via GitHub Web Interface

1. Navigate to **Issues** tab
2. Click **New issue**
3. Select **Copilot Task** template
4. Fill in the form fields
5. Click **Submit new issue**
6. Watch the Actions tab for workflow progress

### Reviewing PRs via GitHub Web Interface

1. Navigate to **Pull requests** tab
2. Find the PR created by @copilot
3. Review the **Files changed** tab
4. Check **auto-review** comments for validation results
5. Add review comments if changes needed
6. Click **Approve** or **Request changes**
7. Merge when satisfied

## Configuration

### Required Setup

1. **GitHub Actions:** Must be enabled on the repository
2. **CODEOWNERS:** Update `.github/CODEOWNERS` with actual owner username
3. **Permissions:** Workflow needs `contents: write` and `pull-requests: write`

### Optional Enhancements

1. **Branch Protection:** Require PR reviews before merging
2. **Status Checks:** Require auto-review to pass before merge
3. **Notifications:** Configure GitHub notifications for issue/PR activity
4. **Custom Actions:** Add project-specific validation to auto-review.sh

## Troubleshooting

### Workflow Doesn't Trigger

- Verify issue has `copilot-task` label
- Check GitHub Actions is enabled (Settings → Actions)
- Review workflow permissions (Settings → Actions → General)

### Auto-Review Fails

- Install required dependencies (yamllint, shellcheck, markdownlint)
- Check syntax errors in generated files
- Review auto-review script logs in Actions tab

### PR Not Assigned to Owner

- Update `.github/CODEOWNERS` with valid GitHub username
- Ensure owner has write access to repository
- Check CODEOWNERS file syntax

## Dependencies

### Required for Auto-Review

- **yamllint:** `pip install yamllint`
- **shellcheck:** `apt-get install shellcheck` or `brew install shellcheck`
- **markdownlint:** `npm install -g markdownlint-cli`

### Optional Integrations

- **GitHub CLI:** `gh` command for PR creation (workflow uses GitHub API)
- **jq:** JSON processing for API responses

## Security

### Agent Permissions

- Agent can only push to `copilot/*` branches
- Agent cannot merge its own PRs
- All PRs require human approval via CODEOWNERS
- Workflows require manual approval in public repos

### Review Requirements

- Never merge without reviewing changes
- Verify agent understood the task correctly
- Check for security issues (secrets, vulnerabilities)
- Test the changes locally if critical

## Knowledge Base

### Adding to Knowledge Base

**Patterns:** When you discover a reusable solution
```bash
# Create new pattern
vim docs/knowledge/patterns/my-pattern.md
# Follow template from docs/knowledge/patterns/README.md
```

**Decisions:** When you make an important architectural choice
```bash
# Create new ADR
vim docs/knowledge/decisions/$(date +%Y%m%d)-decision-title.md
# Follow template from docs/knowledge/decisions/README.md
```

**Insights:** After retrospectives or lessons learned
```bash
# Create new insight
vim docs/knowledge/insights/$(date +%Y%m%d)-insight-title.md
# Follow template from docs/knowledge/insights/README.md
```

### Searching Knowledge Base

```bash
# Via git grep
git grep "authentication" docs/knowledge/

# Via GitHub web search
# Navigate to repository → Click search bar → Type query

# Via command line (if gh CLI installed)
gh search code "authentication" --repo owner/repo --path docs/knowledge/
```

## Contributing

### For Humans

1. Create issues for @copilot using the task template
2. Review PRs carefully before approving
3. Add patterns, decisions, and insights to knowledge base
4. Provide clear success criteria in issues

### For Agents

1. Read knowledge base before making changes
2. Document decisions in ADR format
3. Capture insights from your work
4. Propose new patterns when you discover them

## License

*(Add your license information here)*

## Support

For questions or issues:
1. Check this README
2. Review knowledge base documentation
3. Open an issue with the bug or question template
4. Contact repository maintainer

---

**Powered by GitHub Copilot Agent Automation**

*This system was bootstrapped with a 10-word prompt and validates itself end-to-end.*
