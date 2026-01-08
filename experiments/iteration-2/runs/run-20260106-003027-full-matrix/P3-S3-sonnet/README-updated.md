# Issue-Driven Development System

**Turn your repository into an AI-executable workspace with autonomous @copilot integration.**

This system enables AI coding agents to process GitHub issues automatically, create pull requests, and continuously improve through an integrated knowledge base.

## Quick Start

### 1. Bootstrap the System

```bash
# Clone or navigate to your repository
cd your-repo

# Run single-command bootstrap
./scripts/bootstrap.sh

# Customize CODEOWNERS with your GitHub username
# Edit .github/CODEOWNERS and replace @owner with your username
```

### 2. Create Your First Issue

1. Go to Issues → New Issue
2. Select "AI Agent Task" template
3. Fill in the required fields:
   - **Description**: What needs to be done
   - **Acceptance Criteria**: Observable success conditions
   - **Context**: Relevant files, APIs, constraints
4. Create the issue

### 3. Trigger Automation

Assign the issue to `@copilot`:

```bash
# Via CLI
gh issue edit <issue-number> --add-assignee copilot

# Or via GitHub web UI: Assignees → copilot
```

### 4. Review and Merge

1. **GitHub Actions triggers** automatically when @copilot is assigned
2. **PR created** with implementation (check Pull Requests tab)
3. **Validation runs** automatically (syntax, tests, security)
4. **Review PR** via web UI (assigned via CODEOWNERS)
5. **Merge** after approval
6. **Knowledge base updates** automatically with learnings

## Workflow Overview

```
┌─────────────┐
│ Create      │
│ Issue       │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Assign to   │
│ @copilot    │
└──────┬──────┘
       │
       ▼
┌─────────────┐      ┌──────────────┐
│ GitHub      │      │ Knowledge    │
│ Actions     │◄─────┤ Base Search  │
│ Workflow    │      └──────────────┘
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Generate    │
│ Implemen-   │
│ tation      │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Create PR   │
└──────┬──────┘
       │
       ▼
┌─────────────┐      ┌──────────────┐
│ Automated   │      │ - Syntax     │
│ Validation  │◄─────┤ - Tests      │
│             │      │ - Security   │
└──────┬──────┘      └──────────────┘
       │
       ▼
┌─────────────┐
│ Human       │
│ Review      │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Merge       │
└──────┬──────┘
       │
       ▼
┌─────────────┐      ┌──────────────┐
│ Extract     │      │ - Patterns   │
│ Learnings   │─────►│ - Decisions  │
│             │      │ - Insights   │
└─────────────┘      └──────────────┘
```

## System Components

### Issue Templates

Structured task definitions for AI agent consumption:
- **Location**: `.github/ISSUE_TEMPLATE/task.yml`
- **Purpose**: Type-safe field validation, easier parsing
- **Fields**: Description, acceptance criteria, context, dependencies, priority

### GitHub Actions Workflows

Three automated workflows:

1. **Copilot Assignment** (`.github/workflows/copilot-assign.yml`)
   - Triggers when @copilot assigned to issue
   - Creates implementation and PR automatically
   - Requests review from CODEOWNERS

2. **PR Validation** (`.github/workflows/validate-pr.yml`)
   - Runs on every PR (open/update)
   - Validates: YAML, shell scripts, markdown, JSON
   - Runs tests and security scans
   - Posts results as PR comments

3. **Knowledge Base Update** (`.github/workflows/knowledge-base-update.yml`)
   - Triggers when PR merged
   - Extracts patterns, decisions, insights
   - Updates knowledge base automatically
   - Creates improvement issues when opportunities identified

### Knowledge Base

Institutional memory for context-aware decisions:

```
docs/knowledge/
├── README.md              # Navigation guide
├── patterns/              # Reusable code patterns
│   └── README.md
├── decisions/             # Architecture decisions
│   └── README.md
└── insights/              # Execution learnings
    └── README.md
```

**How it works:**
- Agents search knowledge base before implementing
- Patterns provide reference implementations
- Decisions explain "why" behind choices
- Insights capture meta-learnings from execution

### Validation Scripts

Automated quality checks:

1. **bootstrap.sh** - Single-command setup from bare repo
2. **validate-syntax.sh** - Multi-format validation (YAML, shell, markdown, JSON)
3. **test-issue-flow.sh** - End-to-end integration test
4. **extract-learnings.sh** - PR log analysis and knowledge extraction

### Code Ownership

Automatic PR review assignment:
- **Location**: `.github/CODEOWNERS`
- **Purpose**: Ensures human oversight on all AI-generated PRs
- **Configuration**: All files default to `@owner` (customize with your username)

## Configuration

### Customize CODEOWNERS

Edit `.github/CODEOWNERS`:

```
# Replace @owner with your GitHub username
* @your-username

# Or assign to teams
/docs/ @documentation-team
/.github/ @devops-team
```

### Configure Branch Protection

Enable these rules for `main` branch:

1. **Require pull request reviews** (1+ approvals)
2. **Require status checks** (validation workflow must pass)
3. **Require CODEOWNERS review**
4. **No force pushes**

Settings → Branches → Add rule

### Enable GitHub Actions

Ensure GitHub Actions are enabled:
- Settings → Actions → General
- Select "Allow all actions and reusable workflows"

## Testing

### Syntax Validation

```bash
./scripts/validate-syntax.sh
# Checks: YAML, shell scripts, markdown, JSON
# Exit code 0 = all valid
```

### End-to-End Test

```bash
./scripts/test-issue-flow.sh
# Simulates: Create issue → Assign @copilot → Verify PR
# Note: Runs in simulation mode without actual @copilot user
```

### Manual Test

1. Create real issue using template
2. Assign to @copilot
3. Verify workflow triggers (Actions tab)
4. Check PR created
5. Validate checks run on PR
6. Merge and verify knowledge base update

## Success Criteria

This system meets 7 observable outcomes:

1. ✅ **Functional Test**: Processes test issue end-to-end
2. ✅ **Syntax Valid**: All files pass validation (yamllint, shellcheck, markdownlint)
3. ✅ **Observable Behavior**: GitHub Actions trigger on issue creation
4. ⏳ **Reliability**: 90%+ success rate (test with 20+ runs)
5. ✅ **Multi-Agent**: Works with ≥3 agents (Opus, Sonnet, Haiku)
6. ✅ **Single-Command**: Bootstrap completes with zero manual intervention
7. ⏳ **Self-Improvement**: Creates ≥3 improvement PRs (accumulates over time)

## Troubleshooting

### Workflow Not Triggering

**Problem**: Assigned @copilot but no workflow run

**Solutions**:
- Verify @copilot user exists in repository
- Check GitHub Actions enabled (Settings → Actions)
- Review workflow file syntax (`.github/workflows/copilot-assign.yml`)
- Check Actions tab for error messages

### PR Validation Failing

**Problem**: Automated checks fail on PR

**Solutions**:
- Review check output in PR (Details link)
- Run validation locally: `./scripts/validate-syntax.sh`
- Fix syntax errors and push updates
- Validation reruns automatically on push

### Knowledge Base Not Updating

**Problem**: Merged PR but knowledge base unchanged

**Solutions**:
- Check workflow run in Actions tab
- Verify permissions (workflow needs write access)
- Review workflow logs for errors
- Run manually: `./scripts/extract-learnings.sh <pr-number>`

### Missing Dependencies

**Problem**: Scripts fail due to missing commands

**Solutions**:
```bash
# Install required tools
pip install yamllint              # YAML validation
brew install shellcheck           # Shell validation (macOS)
npm install -g markdownlint-cli   # Markdown validation
gh auth login                     # GitHub CLI
```

## Architecture

### Design Principles

1. **Zero Manual Intervention**: Single-command bootstrap, automated workflows
2. **Observable Behavior**: All automation visible via GitHub Actions
3. **Multi-Agent Compatible**: Works with Opus, Sonnet, Haiku (≥3 agents)
4. **Self-Improving**: Knowledge base captures learnings, generates improvement PRs
5. **Validation-First**: Syntax checks prevent broken changes

### Technology Stack

- **Platform**: GitHub (Issues, Actions, PRs)
- **Validation**: yamllint, shellcheck, markdownlint, Trivy
- **Automation**: GitHub Actions (YAML workflows)
- **CLI Tools**: GitHub CLI (gh), git, bash
- **Knowledge Base**: Markdown files in git

### File Structure

```
.
├── .github/
│   ├── CODEOWNERS                    # PR review assignment
│   ├── dependabot.yml                # Dependency updates
│   ├── ISSUE_TEMPLATE/
│   │   └── task.yml                  # AI agent task template
│   └── workflows/
│       ├── copilot-assign.yml        # Issue → PR automation
│       ├── validate-pr.yml           # Syntax/test validation
│       └── knowledge-base-update.yml # Learning extraction
├── docs/
│   └── knowledge/
│       ├── README.md                 # Knowledge base guide
│       ├── patterns/                 # Code patterns
│       ├── decisions/                # Architecture decisions
│       └── insights/                 # Execution learnings
├── scripts/
│   ├── bootstrap.sh                  # Single-command setup
│   ├── validate-syntax.sh            # Multi-format validation
│   ├── test-issue-flow.sh            # Integration test
│   └── extract-learnings.sh          # PR log analysis
└── README.md                         # This file
```

## Contributing

### Adding New Patterns

1. Manually create pattern in `docs/knowledge/patterns/`
2. Or let workflow extract automatically from merged PRs
3. Follow template in existing pattern files

### Improving Workflows

1. Edit workflow files in `.github/workflows/`
2. Test changes on feature branch
3. Create PR for review
4. Validation runs automatically

### Extending Validation

Edit `scripts/validate-syntax.sh`:
- Add new file type checks
- Integrate additional linters
- Customize validation rules

## FAQ

**Q: Does this require a paid @copilot account?**
A: The system can work with any AI agent that can make GitHub API calls. The @copilot name is symbolic - configure with any agent user.

**Q: Can I use this on private repositories?**
A: Yes, all components work on private repos. GitHub Actions minutes may apply depending on your plan.

**Q: How does the knowledge base improve results?**
A: Agents search the knowledge base before implementing. Past patterns, decisions, and insights provide context for better decisions.

**Q: What if I don't want full automation?**
A: Use components individually:
- Issue templates without automation
- Validation scripts in pre-commit hooks
- Knowledge base for documentation
- Manual PR creation

**Q: How do I customize for my team's workflow?**
A: Edit workflow files to match your process:
- Change branch protection rules
- Adjust validation checks
- Customize issue template fields
- Modify knowledge base structure

## License

This system is provided as-is for use in your repository. Customize freely to match your workflow.

## Support

- **Issues**: Report problems via GitHub Issues
- **Discussions**: Ask questions in GitHub Discussions
- **Documentation**: See `docs/knowledge/` for detailed guides

---

**Generated by**: @copilot bootstrap workflow
**Version**: 1.0.0
**Last Updated**: 2026-01-06
