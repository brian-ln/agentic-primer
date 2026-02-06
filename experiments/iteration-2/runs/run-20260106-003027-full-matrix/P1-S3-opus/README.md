# @copilot Issue Automation

Turn any git repository into an AI-executable workspace that bootstraps itself and remembers everything.

## Quick Start

### Bootstrap (Single Command)

```bash
./scripts/bootstrap.sh
```

This will:
1. Create all required directories
2. Initialize log files
3. Verify system configuration
4. Run a test issue simulation

### Verify Installation

```bash
./scripts/verify-system.sh
```

### Run Test Issue

```bash
./scripts/run-test-issue.sh --dry-run
```

## Workflow

```
1. Create Issue      → Use the @copilot task template
2. Automatic         → Issue validated, labeled, logged
3. @copilot Works    → Agent picks up ready issues
4. PR Created        → Auto-reviewed, checked, assigned
5. Human Review      → Final approval before merge
6. Knowledge Saved   → Patterns and insights captured
7. Self-Improve      → System creates improvement PRs
```

### Creating a Task

1. Go to **Issues** > **New Issue**
2. Select **@copilot Task** template
3. Fill out required fields:
   - Task Description
   - Acceptance Criteria
   - Task Type and Priority
4. Submit the issue
5. Wait for validation and labeling

### Issue Labels

| Label | Meaning |
|-------|---------|
| `copilot-task` | Issue uses correct template |
| `copilot-ready` | Ready for @copilot processing |
| `copilot-in-progress` | Agent working on issue |
| `copilot-completed` | Task finished, PR merged |

### PR Review Flow

1. @copilot creates PR with "Closes #N" reference
2. Automatic validation runs:
   - YAML syntax (yamllint)
   - Shell scripts (shellcheck)
   - Markdown (markdownlint)
   - Test suite
3. Auto-review comment added
4. CODEOWNERS assigned
5. Human reviews and merges

## Components

### GitHub Configuration

| File | Description |
|------|-------------|
| `.github/ISSUE_TEMPLATE/copilot-task.yml` | Structured task template |
| `.github/workflows/issue-copilot.yml` | Issue automation workflow |
| `.github/workflows/pr-auto-review.yml` | PR review workflow |
| `.github/workflows/self-improvement.yml` | Daily improvement analyzer |
| `.github/CODEOWNERS` | PR auto-assignment |
| `.github/copilot-instructions.md` | Agent instructions |

### Knowledge Base

| Directory | Contents |
|-----------|----------|
| `docs/knowledge/patterns/` | Reusable workflow patterns |
| `docs/knowledge/decisions/` | Architecture Decision Records |
| `docs/knowledge/insights/` | Performance metrics and logs |

### Scripts

| Script | Purpose |
|--------|---------|
| `scripts/bootstrap.sh` | Single-command setup |
| `scripts/verify-system.sh` | Validate all components |
| `scripts/run-test-issue.sh` | End-to-end test runner |
| `scripts/analyze-logs.sh` | Extract insights from logs |
| `scripts/create-improvement-pr.sh` | Generate improvement PRs |

## Success Criteria

| Criterion | Requirement | Status |
|-----------|-------------|--------|
| Functional Test | Process test issue end-to-end | Pending |
| Syntax Valid | yamllint, shellcheck, markdownlint pass | Ready |
| Observable Behavior | Workflow triggers on issue creation | Ready |
| Reliability | 90%+ success across 20+ runs | Pending |
| Multi-Agent | Works with Opus, Sonnet, Haiku | Ready |
| Single-Command | Bootstrap with zero manual steps | Ready |
| Self-Improvement | Create 3+ improvement PRs | Pending |

## Agent Compatibility

This system works with multiple AI agents:

| Agent | Method | Status |
|-------|--------|--------|
| GitHub Copilot | Native issue assignment | Supported |
| Claude Opus | Via copilot-instructions.md | Supported |
| Claude Sonnet | Via copilot-instructions.md | Supported |
| Claude Haiku | Via copilot-instructions.md | Supported |

## Self-Improvement

The system automatically improves itself:

1. **Daily Analysis**: Logs analyzed for patterns
2. **Pattern Detection**: Common issues identified
3. **PR Creation**: Improvement proposals generated
4. **Human Review**: Changes reviewed before merge

To trigger manually:

```bash
./scripts/analyze-logs.sh
./scripts/create-improvement-pr.sh \
  --category workflow \
  --description "Issue detected" \
  --recommendation "Proposed fix"
```

## Directory Structure

```
.
├── .github/
│   ├── ISSUE_TEMPLATE/
│   │   └── copilot-task.yml
│   ├── workflows/
│   │   ├── issue-copilot.yml
│   │   ├── pr-auto-review.yml
│   │   └── self-improvement.yml
│   ├── CODEOWNERS
│   └── copilot-instructions.md
├── docs/
│   └── knowledge/
│       ├── README.md
│       ├── patterns/
│       ├── decisions/
│       └── insights/
├── scripts/
│   ├── bootstrap.sh
│   ├── verify-system.sh
│   ├── run-test-issue.sh
│   ├── analyze-logs.sh
│   └── create-improvement-pr.sh
└── README.md
```

## Troubleshooting

### Workflow Not Triggering

1. Check GitHub Actions is enabled
2. Verify workflow files are valid YAML
3. Check branch protection rules

### Validation Failing

1. Run `./scripts/verify-system.sh --verbose`
2. Check specific error messages
3. Fix and re-run

### Agent Not Processing Issues

1. Verify `copilot-ready` label is applied
2. Check @copilot has repository access
3. Review agent instructions in `.github/copilot-instructions.md`

## License

MIT
