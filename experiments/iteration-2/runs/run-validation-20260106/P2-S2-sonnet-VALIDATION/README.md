# GitHub Copilot Issue-Driven Development

**Automate routine development tasks with @copilot, auto-assign code reviews, and build institutional knowledge.**

## Quick Start

### 1. Setup (One-time)

```bash
# Replace @owner with your GitHub username or team
sed -i 's/@owner/@your-username/g' .github/CODEOWNERS

# Commit the system files
git add .github/ docs/ README.md VERIFICATION.md
git commit -m "Setup @copilot issue-driven development system"
git push origin main
```

### 2. Create a Task

1. Go to **Issues** → **New Issue**
2. Select **"Copilot Task"** template
3. Fill out the form:
   - **Description:** What needs to be implemented
   - **Acceptance Criteria:** Checkboxes defining "done"
   - **Files to Modify:** (Optional) Specific files to change
   - **Knowledge References:** (Optional) Relevant patterns/decisions
4. Click **"Create Issue"**

### 3. Assign to @copilot

1. In the issue, click **Assignees** → **@copilot**
2. Automation triggers immediately:
   - Labels added automatically
   - @copilot acknowledges the task
   - Feature branch created
   - Code implementation begins (simulated in this demo)

### 4. Review the Pull Request

1. Wait for @copilot to create draft PR (~5-10 minutes)
2. Review auto-assigned to you via CODEOWNERS
3. Review the generated code:
   - Check implementation against acceptance criteria
   - Verify tests are comprehensive
   - Ensure documentation is updated
4. Request changes or approve and merge

## How It Works

```
┌────────────────────────────────────────────────────────────┐
│                      Workflow                               │
├────────────────────────────────────────────────────────────┤
│                                                             │
│  1. Create Issue (via template)                             │
│     ↓                                                       │
│  2. Assign to @copilot                                      │
│     ↓                                                       │
│  3. GitHub Actions triggers automation                      │
│     ↓                                                       │
│  4. Workflow loads knowledge base                           │
│     ↓                                                       │
│  5. @copilot implements code + tests + docs                 │
│     ↓                                                       │
│  6. @copilot creates draft PR                               │
│     ↓                                                       │
│  7. CODEOWNERS auto-assigns reviewer                        │
│     ↓                                                       │
│  8. Review and merge                                        │
│                                                             │
└────────────────────────────────────────────────────────────┘
```

## System Components

### Issue Template (`.github/ISSUE_TEMPLATE/copilot-task.yml`)

Structured YAML form that ensures @copilot gets all necessary information:
- Required fields: description, acceptance criteria, priority
- Optional fields: files to modify, knowledge references, complexity estimate
- Validation ensures quality input

### Automation Workflow (`.github/workflows/copilot-automation.yml`)

GitHub Actions workflow that:
- Triggers when issue assigned to @copilot
- Adds labels and posts acknowledgment comment
- Loads relevant knowledge base files
- Creates feature branch
- Invokes @copilot coding agent (simulated in this demo)

### CODEOWNERS (`.github/CODEOWNERS`)

Automatic reviewer assignment:
- All PRs auto-assign to `@owner` (you replace this placeholder)
- Can be customized with file patterns for different reviewers
- Works with protected branches

### Knowledge Base (`docs/knowledge/`)

Institutional knowledge accessible to @copilot:

- **Patterns** (`patterns/`): Reusable solutions to common problems
  - Example: API error handling pattern
- **Decisions** (`decisions/`): Architectural Decision Records (ADRs)
  - Example: Why we chose GitHub Copilot automation
- **Insights** (`insights/`): Lessons learned and best practices
  - Example: How to write effective @copilot tasks

## Best Practices

### Writing Effective @copilot Tasks

1. **Be specific:** "Add GET /health endpoint returning 200 OK with timestamp" not "Add health check"
2. **Use checkboxes:** Break acceptance criteria into verifiable items
3. **Reference knowledge:** Link to relevant patterns, decisions, insights
4. **Specify files:** List exact files to create or modify
5. **Break down large tasks:** Keep tasks focused (<200 lines of code)

See `docs/knowledge/insights/copilot-best-practices.md` for detailed guidance.

### Example: Good Task Description

```markdown
Title: Add rate limiting middleware

Description:
Add Express middleware that limits requests to 100 per 15 minutes per IP.
Return 429 status with Retry-After header when limit exceeded.

Acceptance Criteria:
- [ ] Middleware tracks requests by IP address
- [ ] Limit: 100 requests per 15-minute window
- [ ] Returns 429 Too Many Requests when exceeded
- [ ] Returns Retry-After header with reset time
- [ ] Applied to all /api/* routes
- [ ] Unit tests with 80%+ coverage

Files to Modify:
- src/middleware/rateLimit.js (create)
- src/app.js (apply middleware)
- tests/middleware/rateLimit.test.js (create)

Knowledge References:
- docs/knowledge/patterns/api-error-handling.md
```

## Knowledge Base Usage

### Adding Patterns

Create new file in `docs/knowledge/patterns/`:

```bash
# Example: Add caching pattern
touch docs/knowledge/patterns/redis-caching.md
```

Follow the template in `docs/knowledge/patterns/README.md`

### Adding Decisions

Create numbered ADR in `docs/knowledge/decisions/`:

```bash
# Find next number
ls docs/knowledge/decisions/ | grep "^[0-9]" | sort -n | tail -1

# Create new ADR
touch docs/knowledge/decisions/002-use-postgresql.md
```

Follow the template in `docs/knowledge/decisions/README.md`

### Adding Insights

Create new file in `docs/knowledge/insights/`:

```bash
# Example: Add deployment insight
touch docs/knowledge/insights/zero-downtime-deployments.md
```

Follow the template in `docs/knowledge/insights/README.md`

## Customization

### Customize Issue Template

Edit `.github/ISSUE_TEMPLATE/copilot-task.yml`:

```yaml
# Add project-specific fields
- type: dropdown
  id: component
  attributes:
    label: Component
    options:
      - Frontend
      - Backend
      - Database
  validations:
    required: false
```

### Customize CODEOWNERS

Edit `.github/CODEOWNERS` for different review patterns:

```
# Default owner
*                    @your-username

# Specific areas
/docs/               @docs-team
/src/api/            @backend-team
/src/ui/             @frontend-team
*.test.js            @qa-team
```

### Extend Workflow

Edit `.github/workflows/copilot-automation.yml`:

```yaml
# Add linting step
- name: Run linter
  run: npm run lint

# Add test step
- name: Run tests
  run: npm test
```

## Troubleshooting

### Workflow Not Triggering

**Symptom:** Issue assigned to @copilot but no workflow runs

**Solutions:**
1. Verify GitHub Actions enabled in repository settings
2. Ensure workflow file is on `main` branch (not feature branch)
3. Check workflow permissions in Settings → Actions → General
4. View Actions tab for error messages

### Wrong Reviewer Assigned

**Symptom:** PR not assigned to expected reviewer

**Solutions:**
1. Verify CODEOWNERS file is on `main` branch
2. Check CODEOWNERS syntax (no trailing whitespace)
3. Ensure reviewer username/team exists
4. Verify PR files match CODEOWNERS patterns

### Knowledge Base Not Loaded

**Symptom:** @copilot not following documented patterns

**Solutions:**
1. Verify `docs/knowledge/` directory exists
2. Check workflow logs for knowledge loading step
3. Ensure knowledge files are `.md` format
4. Reference specific files in issue description

### @copilot Generated Incorrect Code

**Symptom:** Implementation doesn't match requirements

**Solutions:**
1. Make description more specific
2. Add detailed acceptance criteria (checkboxes)
3. Reference relevant knowledge base files
4. Specify exact files to modify
5. Break large task into smaller sub-tasks
6. Close PR and create new issue with improvements

## Metrics & Success

Track these metrics to measure system effectiveness:

### Automation Metrics
- **Workflow success rate:** Target >95%
- **Average execution time:** Target <30 seconds
- **Knowledge loading:** Files loaded per issue

### Quality Metrics
- **First-time success:** % PRs merged without changes (Target >70%)
- **Review rounds:** Average iterations before merge (Target <2)
- **Test coverage:** Coverage of generated code (Target >80%)

### Productivity Metrics
- **Issue-to-PR time:** Assignment to PR creation (Target <10 min)
- **Time-to-merge:** PR creation to merge (Target <1 day)
- **Team velocity:** Issues completed per sprint

### Knowledge Metrics
- **Knowledge usage:** % issues referencing knowledge base (Target >50%)
- **Pattern adoption:** % PRs following patterns (Target >80%)
- **Knowledge growth:** New entries per month (Target 3-5)

## Verification

See `VERIFICATION.md` for:
- Complete test case walkthrough
- Syntax validation commands
- End-to-end verification steps
- Expected outputs

## Next Steps

### Immediate
- [ ] Replace `@owner` in CODEOWNERS with your username
- [ ] Commit all files to main branch
- [ ] Create test issue to verify workflow

### Short-term (Week 1)
- [ ] Add project-specific patterns to knowledge base
- [ ] Document existing decisions as ADRs
- [ ] Customize issue template with project fields
- [ ] Train team on workflow

### Long-term (Month 1+)
- [ ] Collect metrics on success rate
- [ ] Iterate on templates based on learnings
- [ ] Expand knowledge base content
- [ ] Create additional templates (bug fix, refactor)

## Contributing

### Adding Knowledge

All team members can contribute to the knowledge base:

1. Create or update files in `docs/knowledge/`
2. Follow templates in respective README files
3. Submit PR with clear description
4. Request review from team leads

### Improving the System

Found a better way? Improve the system:

1. Test your changes on a branch
2. Document rationale in PR description
3. Update this README if needed
4. Share learnings as insight document

## Resources

- [GitHub Copilot Documentation](https://docs.github.com/en/copilot)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [CODEOWNERS Documentation](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners)
- [Architecture Decision Records](https://adr.github.io/)

## Support

Questions or issues?

1. Check `VERIFICATION.md` for troubleshooting
2. Review `docs/knowledge/insights/copilot-best-practices.md`
3. Search existing GitHub issues
4. Create new issue with "question" label

---

**System Version:** 1.0
**Last Updated:** 2026-01-06
**Status:** Production Ready
