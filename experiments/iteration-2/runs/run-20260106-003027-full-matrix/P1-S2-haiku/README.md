# Issue-Driven Development with @copilot

Autonomous issue processing system that bootstraps and continuously improves itself.

## How It Works

```
Issue Created
    ↓
GitHub Workflow Triggered
    ↓
@copilot Analyzes Requirements
    ↓
Solution Generated & Tested
    ↓
PR Created with Auto-Review
    ↓
Manual Review (if needed)
    ↓
Merge & Knowledge Update
```

## Quick Start

### 1. Create a Task Issue

Click "New Issue" and select the **"@copilot Task"** template.

Fill in:
- **Task Description**: What needs to be done
- **Acceptance Criteria**: How to verify completion
- **Additional Context**: Links, examples, related issues (optional)
- **Priority**: Low/Medium/High/Critical (optional)
- **Allow Auto-Merge**: Let @copilot auto-merge if it passes all gates (optional)

### 2. Let @copilot Process It

The GitHub workflow automatically:
- ✅ Validates issue format
- ✅ Generates solution
- ✅ Runs syntax validation
- ✅ Executes test suite
- ✅ Updates knowledge base
- ✅ Creates pull request with auto-review

### 3. Review and Merge

Review the PR that @copilot created:
- Check solution matches acceptance criteria
- Review code quality and test coverage
- Approve and merge via GitHub web UI

If you enabled **auto-merge**, @copilot may merge automatically if all gates pass.

## Features

### Auto-Review Quality Gates

Every @copilot-generated PR passes through:

1. **Syntax Validation**: YAML, shell, markdown linting
2. **Code Quality**: Linting, formatting checks
3. **Test Suite**: Automated test execution
4. **Knowledge Base**: Pattern/decision documentation
5. **Reviewer Assignment**: CODEOWNERS auto-assigns reviewers

### Knowledge Base

@copilot learns from every issue and captures:

- **Patterns** (`docs/knowledge/patterns/`): Reusable solutions
- **Decisions** (`docs/knowledge/decisions/`): Why design choices were made
- **Insights** (`docs/knowledge/insights/`): Learnings and observations

Next issue? @copilot checks the knowledge base first for similar patterns.

### Metrics & Logging

Every processed issue generates metrics:

- Processing time
- Gates passed/failed
- PR number
- Links to related issues

Logs available in `.copilot-logs/`

## Example Task

### Create Issue

**Title:** [copilot] Add error handling middleware

**Description:**
```
Create a global error handling middleware for Express.js that:
- Catches all async errors
- Logs errors with context
- Returns standardized error responses
- Includes stack traces in dev mode
```

**Acceptance Criteria:**
```
1. Middleware function created in src/middleware/error-handler.js
2. Handles both sync and async errors
3. Returns 500 status with error details
4. Tests pass (90%+ coverage)
5. No linting errors
6. Documentation updated in docs/MIDDLEWARE.md
```

### What @copilot Does

1. **Analyze**: Read issue, understand acceptance criteria
2. **Design**: Check knowledge base for similar patterns
3. **Implement**: Generate middleware code
4. **Test**: Create comprehensive test suite
5. **Validate**: Run syntax checks and tests
6. **Document**: Update docs and knowledge base
7. **Submit**: Create PR with auto-review

### Expected Result

PR created with:
- ✅ Implementation in src/middleware/error-handler.js
- ✅ Tests in tests/error-handler.test.js
- ✅ Documentation in docs/MIDDLEWARE.md
- ✅ Knowledge base entry created
- ✅ Auto-review checklist provided
- ✅ Reviewer assigned via CODEOWNERS

---

## Issue Template Fields

| Field | Required | Purpose |
|-------|----------|---------|
| Task Description | Yes | What needs to be done? |
| Acceptance Criteria | Yes | How to verify completion? |
| Additional Context | No | Links, examples, related issues |
| Priority | No | Low/Medium/High/Critical |
| Allow Auto-Merge | No | Auto-merge if all gates pass |

### Tips for Writing Good Issues

1. **Be Specific**: "Add user authentication" vs "Implement OAuth2 with JWT tokens"
2. **List Criteria**: Use numbered list for acceptance criteria
3. **Link Context**: Include issue/PR/doc links if helpful
4. **Set Priority**: Helps @copilot prioritize if handling multiple issues
5. **Enable Auto-Merge**: Only for low-risk issues (docs, small features)

## Configuration

### Repository Setup

1. Copy issue template: `.github/ISSUE_TEMPLATE/task.yml`
2. Copy CODEOWNERS: `.github/CODEOWNERS`
3. Copy workflow: `.github/workflows/ai-process-issue.yml`
4. Create knowledge base: `docs/knowledge/` with subdirectories

### Customize Workflow

Edit `.github/workflows/ai-process-issue.yml`:

```yaml
# Change who gets assigned for review
reviewers: your-github-username

# Add additional validation gates
- name: Custom validation
  run: npm run validate:custom

# Modify PR template
body: |
  Custom PR template here
```

### Customize Issue Template

Edit `.github/ISSUE_TEMPLATE/task.yml`:

```yaml
# Add fields specific to your project
- type: dropdown
  id: component
  attributes:
    label: "Component"
    options:
      - "Backend"
      - "Frontend"
      - "DevOps"
```

## Workflow Triggers

The AI processing workflow triggers on:

1. **Issue Opened**: When new issue is created with `copilot-task` label
2. **Issue Labeled**: When issue is labeled with `copilot-task`

### Manual Trigger

To manually trigger for existing issue:
1. Go to issue
2. Click "Labels" → add "copilot-task"
3. Workflow triggers automatically

## Troubleshooting

### Issue Not Processing?

**Check:**
- ✅ Repository has Actions enabled (Settings → Actions)
- ✅ Workflow file exists: `.github/workflows/ai-process-issue.yml`
- ✅ Issue uses "@copilot Task" template
- ✅ Issue has "copilot-task" label
- ✅ Required fields filled (description, acceptance criteria)

**Fix:**
- Go to "Actions" tab, check workflow run logs
- Look for validation errors in logs
- Ensure issue format matches template

### PR Not Created?

**Check:**
- ✅ Workflow completed successfully (no errors in logs)
- ✅ Issue format is valid (required fields filled)
- ✅ GitHub token has write access
- ✅ Repository isn't in read-only mode

**Fix:**
- Check workflow run logs for specific error
- Manual fix: Create PR manually and reference issue

### Tests Failing in PR?

**How to Handle:**
1. Review test failures in PR checks
2. Understand what @copilot generated
3. Either:
   - Fix implementation (edit files in PR)
   - Request changes (comment on PR)
   - Close and create new issue with clarification

### Knowledge Base Not Updating?

**Check:**
- ✅ `docs/knowledge/` directory exists
- ✅ Subdirectories created: `patterns/`, `decisions/`, `insights/`
- ✅ Workflow has write permissions
- ✅ Branch protection doesn't block writes

**Fix:**
- Create directories manually
- Check workflow permissions in GitHub settings

## Success Metrics

Track @copilot's effectiveness:

| Metric | How to Calculate | Target |
|--------|------------------|--------|
| Processing Time | Time from issue create to PR | < 5 min |
| Validation Pass Rate | % of PRs passing all gates | > 95% |
| Acceptance Rate | % of PRs merged without major changes | > 85% |
| KB Utilization | % of new issues matching KB patterns | Increasing |
| Code Quality | % passing lint, tests, review | > 90% |

## Architecture

### Core Components

1. **Issue Template** (`.github/ISSUE_TEMPLATE/task.yml`)
   - Structured input format
   - Ensures consistent data
   - Auto-labels issues

2. **GitHub Workflow** (`.github/workflows/ai-process-issue.yml`)
   - Validates issue format
   - Generates solution
   - Runs quality gates
   - Creates PR
   - Logs metrics

3. **CODEOWNERS** (`.github/CODEOWNERS`)
   - Auto-assigns reviewers
   - Ensures accountability
   - Tracks review history

4. **Knowledge Base** (`docs/knowledge/`)
   - Patterns: Solution templates
   - Decisions: Design rationale
   - Insights: Learnings

## Workflow Jobs

### validate-issue
- Checks issue format
- Validates required fields
- Generates task ID

### process-issue
- Extracts metadata
- Creates solution branch
- Generates solution
- Validates syntax
- Runs tests
- Updates knowledge base
- Creates PR

### log-metrics
- Records processing metrics
- Creates summary
- Updates statistics

## Environment Variables

Available in workflow:

```bash
ISSUE_BODY           # Full issue body text
ISSUE_TITLE          # Issue title
ISSUE_NUMBER         # Issue number (1, 2, 3...)
ISSUE_AUTHOR         # Who created issue
ISSUE_ASSIGNEE       # Who issue is assigned to
```

## Best Practices

### For Issue Creators

1. **Use the Template**: Always start with "@copilot Task" template
2. **Be Specific**: Vague issues → vague solutions
3. **List Criteria**: Make success measurable
4. **Set Priority**: Important for multi-issue queues
5. **Link Context**: Help @copilot understand scope
6. **Review PRs**: Don't auto-merge without review (except trivial)

### For Code Owners

1. **Review Promptly**: Don't let PRs sit unreviewed
2. **Give Feedback**: Comment on improvements
3. **Update KB**: Provide patterns for future issues
4. **Monitor Metrics**: Track @copilot's effectiveness
5. **Iterate**: Update workflow based on learnings

### For System Maintenance

1. **Regular Backups**: Keep `.copilot-logs/` organized
2. **KB Cleanup**: Archive superseded patterns monthly
3. **Workflow Updates**: Keep GitHub Actions dependencies current
4. **Permissions**: Review access regularly
5. **Documentation**: Keep README and KB in sync

## Integration with CI/CD

### Run Existing Tests

```yaml
- name: Run tests
  run: npm test  # or pytest, cargo test, etc.
```

### Add Lint Checks

```yaml
- name: Lint code
  run: npm run lint  # or eslint, pylint, etc.
```

### Build Project

```yaml
- name: Build
  run: npm run build  # or make build, etc.
```

### Deploy to Staging

```yaml
- name: Deploy to staging
  run: npm run deploy:staging
```

## Performance

Typical @copilot workflow:

| Step | Time |
|------|------|
| Issue validation | < 10 sec |
| Solution generation | 1-3 min |
| Syntax validation | < 30 sec |
| Test execution | 1-2 min |
| PR creation | < 30 sec |
| **Total** | **~5 min** |

Scaling to multiple concurrent issues:
- Each issue processed in parallel
- No blocking dependencies
- Linear scaling with issue count

## Security

### GitHub Token Permissions

Required permissions in workflow:

```yaml
permissions:
  contents: write        # Needed to commit changes
  pull-requests: write   # Needed to create PRs
  issues: write          # Needed to comment on issues
```

### Branch Protection

Recommended settings:

- ✅ Require pull request reviews
- ✅ Require status checks to pass (tests, lint)
- ✅ Dismiss stale PR approvals
- ✅ Require code owner review for CODEOWNERS changes
- ❌ Allow force pushes (security risk)

### Sensitive Data

Best practices:

1. **Secrets**: Use GitHub Secrets for API keys, tokens
2. **Logs**: Don't log sensitive data (passwords, keys)
3. **Commits**: Use `.gitignore` for secrets files
4. **PRs**: Review for credential leaks

## Roadmap

### Phase 1 (Current)
- ✅ Issue template
- ✅ GitHub workflow
- ✅ CODEOWNERS
- ✅ Knowledge base structure
- ✅ Metrics logging

### Phase 2 (Future)
- [ ] Web dashboard for metrics
- [ ] KB search interface
- [ ] Auto-merge for trusted issues
- [ ] Machine learning for pattern matching
- [ ] Multi-agent coordination
- [ ] Dependency management

### Phase 3 (Future)
- [ ] IDE integration (VS Code extension)
- [ ] Slack/Discord notifications
- [ ] GitHub app for multi-repo coordination
- [ ] Pattern marketplace
- [ ] Custom validation rules

---

## Support

### Getting Help

- **Workflow Issues**: Check `.github/workflows/ai-process-issue.yml` logs
- **Issue Questions**: See "Troubleshooting" section
- **Feature Requests**: Create issue with "[proposal]" tag

### Contributing

To improve @copilot system:

1. Create issue: "[improvement] What could be better"
2. Let @copilot suggest changes
3. Review and merge improvements
4. Knowledge base captures learning

---

## License

This bootstrapping system is provided as-is for use within your repository.

---

**Version:** 1.0
**Last Updated:** [auto-generated]
**Maintained by:** @copilot
