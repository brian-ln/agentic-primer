# @copilot Issue Automation System

Autonomous AI agent system for processing GitHub issues, creating pull requests, and continuously improving through a knowledge base feedback loop.

## Quick Start

Bootstrap the entire system in one command:

```bash
./scripts/bootstrap-v2.sh
```

This will:
- Validate prerequisites (git, gh CLI, linters)
- Create directory structure
- Install missing dependencies
- Initialize knowledge base
- Set up configuration files

## Overview

This system enables @copilot (GitHub Copilot agent) to:

1. **Receive Issues**: Automatically process when assigned
2. **Generate Code**: Create implementation based on requirements
3. **Create PRs**: Open pull requests with changes
4. **Validate**: Run syntax checks, tests, security scans
5. **Learn**: Extract patterns and insights after merge
6. **Improve**: Identify and create improvement issues

### System Flow

```
┌──────────────┐
│ Create Issue │
│ Assign       │
│ @copilot     │
└──────┬───────┘
       │
       ↓
┌─────────────────┐
│ Workflow        │
│ Triggers        │
│ - Parse issue   │
│ - Search KB     │
│ - Generate code │
│ - Create PR     │
└─────┬───────────┘
      │
      ↓
┌────────────────┐
│ Validation     │
│ - Syntax       │
│ - Security     │
│ - Tests        │
└────┬───────────┘
     │
     ↓ (merge)
┌────────────────┐
│ Knowledge Base │
│ Update         │
│ - Extract      │
│ - Document     │
│ - Improve      │
└────────────────┘
```

## Architecture

### Components

#### 1. Issue Templates (`.github/ISSUE_TEMPLATE/`)

Structured issue forms for consistent input:
- Task title
- Description
- Acceptance criteria
- Context
- Priority and complexity

#### 2. Workflows (`.github/workflows/`)

##### `copilot-assign.yml`
- **Trigger**: Issue assigned to @copilot OR labeled "copilot"
- **Actions**:
  - Parse issue body
  - Search knowledge base
  - Generate implementation
  - Create feature branch
  - Open pull request

##### `validate-pr.yml`
- **Trigger**: PR opened or updated
- **Checks**:
  - YAML validation (yamllint)
  - Shell script validation (shellcheck)
  - Markdown validation (markdownlint)
  - Security scan (secret detection)
  - Test execution

##### `knowledge-base-update.yml`
- **Trigger**: PR merged to main
- **Actions**:
  - Extract PR metadata
  - Analyze changes
  - Update pattern library
  - Create insight document
  - Identify improvement opportunities

#### 3. Scripts (`scripts/`)

- **`bootstrap-v2.sh`**: Single-command setup
- **`validate-syntax-v2.sh`**: Pre-commit validation
- **`test-issue-flow-v2.sh`**: Integration test
- **`extract-learnings-v2.sh`**: Manual learning extraction

#### 4. Knowledge Base (`docs/knowledge/`)

Hierarchical documentation structure:

```
docs/knowledge/
├── README.md                 # Navigation guide
├── patterns/
│   ├── github-actions.md    # Workflow patterns
│   ├── scripting.md         # Shell script patterns
│   └── ...
├── decisions/
│   ├── 001-architecture.md  # ADRs
│   └── ...
└── insights/
    ├── improvements.md      # Improvement tracking
    └── [date]-pr-[num].md   # PR learnings
```

## Installation

### Prerequisites

- Git
- GitHub CLI (`gh`)
- Python 3.11+ (for yamllint)
- Node.js 20+ (for markdownlint)
- Bash-compatible shell

### Setup

1. Clone repository:
```bash
git clone <repo-url>
cd <repo>
```

2. Run bootstrap:
```bash
chmod +x scripts/bootstrap-v2.sh
./scripts/bootstrap-v2.sh
```

3. Copy workflow files:
```bash
# If workflows are stored with different names
cp github-workflows-*.yml .github/workflows/
```

4. Update CODEOWNERS:
```bash
# Replace REPO_OWNER with your GitHub username
sed -i 's/@REPO_OWNER/@yourusername/g' .github/CODEOWNERS
```

5. Verify installation:
```bash
./scripts/validate-syntax-v2.sh
./scripts/test-issue-flow-v2.sh
```

## Usage

### Creating an Issue for @copilot

1. Go to Issues → New Issue
2. Select "Copilot Task" template
3. Fill in all fields:
   - Task title
   - Detailed description
   - Acceptance criteria (checklist)
   - Any relevant context
4. Assign to @copilot OR add label "copilot"
5. Submit

### Expected Timeline

- **0-2 minutes**: Workflow triggers and processes issue
- **2-5 minutes**: PR created with implementation
- **5-10 minutes**: Validation checks complete
- **Variable**: Human review and merge
- **Post-merge**: Knowledge base update (automatic)

### Monitoring

**Check workflow status**:
```bash
gh run list --limit 10
```

**View workflow logs**:
```bash
gh run view <run-id> --log
```

**Check PR validation**:
```bash
gh pr checks <pr-number>
```

## Configuration

### Model Selection

Set AI model via repository variable:

```bash
gh variable set COPILOT_MODEL --body "sonnet-4.5"
# Options: opus-4.5, sonnet-4.5, haiku-3.5
```

### Workflow Customization

Edit workflow files in `.github/workflows/` to customize:
- Validation rules
- Notification preferences
- Knowledge base update logic

### Knowledge Base

Search for relevant patterns before implementing:

```bash
# Search for authentication patterns
rg "authentication" docs/knowledge/

# Find recent insights
ls -lt docs/knowledge/insights/ | head -10

# Review decisions
cat docs/knowledge/decisions/README-v2.md
```

## Testing

### Validation Tests

Run syntax validation locally:

```bash
./scripts/validate-syntax-v2.sh
```

### Integration Tests

Test full issue → PR flow:

```bash
./scripts/test-issue-flow-v2.sh
```

This creates a test issue, monitors workflow execution, and verifies PR creation.

### Manual Testing

1. Create test issue with label "test"
2. Monitor Actions tab
3. Review generated PR
4. Check validation results
5. Clean up: close PR and issue

## Troubleshooting

### Workflow Not Triggering

**Check**:
- Issue assigned to @copilot OR labeled "copilot"
- GitHub Actions enabled in repository settings
- Workflow files in `.github/workflows/`

**Debug**:
```bash
gh run list --limit 5
gh workflow view copilot-assign
```

### Validation Failing

**Common causes**:
- YAML syntax errors (check indentation)
- Shell script issues (run shellcheck locally)
- Missing files or directories

**Debug**:
```bash
./scripts/validate-syntax-v2.sh
yamllint .github/workflows/*.yml
shellcheck scripts/*.sh
```

### PR Not Created

**Check**:
- Workflow completed successfully
- Branch protection rules not blocking
- GitHub token has write permissions

**Debug**:
```bash
gh run view <run-id> --log
gh auth status
```

### Knowledge Base Not Updating

**Check**:
- PR was merged (not just closed)
- Workflow has write permissions
- No errors in workflow logs

**Debug**:
```bash
gh run list --workflow=knowledge-base-update
```

## Development

### Adding New Patterns

1. Create pattern document in `docs/knowledge/patterns/`
2. Follow standard format (see existing patterns)
3. Update `patterns/README.md` index
4. Test pattern application

### Adding New Workflows

1. Create workflow in `.github/workflows/`
2. Test in feature branch first
3. Document in knowledge base
4. Update this README

### Enhancing Knowledge Base

1. Review auto-generated insights
2. Add context from PR discussions
3. Extract reusable patterns
4. Link related documents

## Success Metrics

Track system effectiveness:

| Metric | Target | Measurement |
|--------|--------|-------------|
| Issue processing time | < 5 min | Workflow duration |
| PR creation success | 90%+ | Successful runs / total |
| Validation pass rate | 95%+ | Green checks / total |
| Knowledge base growth | 5+ insights/month | File count |
| Pattern reuse | 20%+ of PRs | Manual review |

## Maintenance

### Weekly

- Review failed workflows
- Check for GitHub Actions updates
- Monitor knowledge base size

### Monthly

- Review improvement opportunities
- Update deprecated patterns
- Audit CODEOWNERS accuracy

### Quarterly

- Review ADRs for relevance
- Archive old insights (>6 months)
- Update this README

## Security

### Secret Management

- Never commit secrets to repository
- Use GitHub Secrets for credentials
- Scan PRs for leaked secrets (automated)

### Permissions

- Workflows use minimal permissions
- CODEOWNERS enforces review
- Actions pinned to commit SHAs

### Updates

- Dependabot monitors GitHub Actions
- Review and merge dependency PRs regularly

## Multi-Agent Support

System supports multiple AI models:

- **Claude Opus 4.5**: Highest quality, slower
- **Claude Sonnet 4.5**: Balanced (default)
- **Claude Haiku 3.5**: Fast, cost-effective

Configure via `COPILOT_MODEL` repository variable.

## Roadmap

### Near-Term (Q1 2026)

- [ ] Add semantic search for knowledge base
- [ ] Implement metrics dashboard
- [ ] Support for multiple languages

### Mid-Term (Q2-Q3 2026)

- [ ] Multi-repository knowledge sharing
- [ ] Advanced pattern mining from logs
- [ ] Chat interface (Slack/Discord bot)

### Long-Term (Q4 2026+)

- [ ] Self-optimizing workflows
- [ ] Predictive issue analysis
- [ ] Cross-organization learning

## Contributing

### Reporting Issues

- Use issue template
- Provide workflow logs
- Include reproduction steps

### Pull Requests

- Run validation locally first
- Update knowledge base if adding patterns
- Link to related issues

### Knowledge Base

- Enhance auto-generated insights
- Document novel patterns
- Keep ADRs up-to-date

## License

[Specify license]

## Support

- **Issues**: Use GitHub Issues
- **Documentation**: See `docs/knowledge/`
- **Logs**: Check GitHub Actions tab

## Acknowledgments

- Inspired by GitHub Copilot Workspace
- Built with GitHub Actions
- Powered by Claude AI models

---

**Version**: 1.0.0
**Last Updated**: January 8, 2026
**Maintainer**: @copilot + human reviewers
