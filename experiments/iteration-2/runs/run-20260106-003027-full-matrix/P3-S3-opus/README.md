# Issue-Driven Development System

A GitHub-native system for autonomous AI agent task processing with human oversight.

## Overview

This system enables AI agents (GitHub Copilot, Claude, etc.) to autonomously process development tasks defined in GitHub issues and create pull requests for human review.

```
Issue (Template) --> @copilot (Workflow) --> PR (Review) --> Merge
       |                    |                    |
       v                    v                    v
  [Structured       [Automated            [Human
   Input]            Processing]           Oversight]
```

## Quick Start

### Setup (One Command)

```bash
./scripts/bootstrap.sh your-github-username
```

### Create a Task

1. Go to **Issues** > **New Issue**
2. Select **Copilot Task** template
3. Fill in:
   - Task description
   - Success criteria
   - Context files (optional)
   - Constraints (optional)
   - Select agent (copilot, claude-opus, claude-sonnet, claude-haiku)
4. Click **Submit new issue**
5. Add the `copilot` label to trigger processing

### Review the PR

1. Wait for the agent to create a PR (usually minutes)
2. Review changes in the web UI
3. Request changes or approve
4. Merge when satisfied

## Workflow

```
1. User creates issue using task template
         |
         v
2. User adds 'copilot' label
         |
         v
3. GitHub Action triggers
         |
         v
4. Agent reads issue, creates branch
         |
         v
5. Agent implements solution
         |
         v
6. Agent creates PR (auto-assigned via CODEOWNERS)
         |
         v
7. Human reviews PR
         |
         v
8. System captures learnings
         |
         v
9. PR merged, issue closed
```

## Components

### Issue Template

Location: `.github/ISSUE_TEMPLATE/task.yml`

Provides structured input with required fields:
- Task Description (required)
- Success Criteria (required)
- Context Files
- Constraints
- Agent Selection
- Priority

### CODEOWNERS

Location: `.github/CODEOWNERS`

Automatically assigns all PRs to the repository owner for review.

### GitHub Workflow

Location: `.github/workflows/copilot-issue.yml`

Triggers on issue labeling and orchestrates:
1. Issue validation
2. Agent invocation (Copilot or Claude)
3. PR creation
4. Insight capture
5. Self-improvement checks

### Knowledge Base

Location: `docs/knowledge/`

Stores learnings for continuous improvement:

```
docs/knowledge/
├── README.md           # Overview
├── patterns/           # Reusable solutions
├── decisions/          # Architectural decisions (ADRs)
└── insights/           # Execution learnings
```

## Multi-Agent Support

The system works with multiple AI agents:

| Agent | Selection | Notes |
|-------|-----------|-------|
| GitHub Copilot | `copilot (default)` | Native GitHub integration |
| Claude Opus | `claude-opus` | Highest capability |
| Claude Sonnet | `claude-sonnet` | Balanced performance |
| Claude Haiku | `claude-haiku` | Fastest response |

For Claude agents, add your API key to repository secrets:
- Secret name: `ANTHROPIC_API_KEY`

## Self-Improvement

The system automatically improves over time by:

1. **Pattern Detection**: Identifies common solutions in successful PRs
2. **Insight Capture**: Logs successes and failures with root cause
3. **Improvement PRs**: Creates PRs to update the knowledge base

After 10+ processed issues, the system begins generating improvement PRs.

## Validation

Run syntax validation on all files:

```bash
./scripts/validate.sh
```

Validates:
- YAML syntax (yamllint)
- Markdown structure (markdownlint)
- Shell scripts (shellcheck)

## File Structure

```
.
├── .github/
│   ├── CODEOWNERS                    # PR auto-assignment
│   ├── ISSUE_TEMPLATE/
│   │   └── task.yml                  # Task template
│   └── workflows/
│       └── copilot-issue.yml         # Automation workflow
├── docs/
│   └── knowledge/
│       ├── README.md                 # Knowledge base overview
│       ├── patterns/                 # Reusable patterns
│       │   ├── README.md
│       │   └── 001-issue-driven-development.md
│       ├── decisions/                # ADRs
│       │   ├── README.md
│       │   └── 001-use-github-native-features.md
│       └── insights/                 # Learnings
│           ├── README.md
│           └── 001-bootstrap-learnings.md
├── scripts/
│   ├── bootstrap.sh                  # Setup script
│   └── validate.sh                   # Validation script
├── README.md                         # This file
└── SOLUTION.md                       # Design documentation
```

## Success Criteria

This system meets the following success criteria:

| Criterion | Status | Implementation |
|-----------|--------|----------------|
| Functional Test | Ready | Workflow processes issues end-to-end |
| Syntax Valid | Ready | validate.sh checks all files |
| Observable Behavior | Ready | Workflow triggers on issue label |
| Reliability | Ready | Retry logic, knowledge base patterns |
| Multi-Agent | Ready | Supports Copilot + 3 Claude variants |
| Single-Command | Ready | bootstrap.sh sets up everything |
| Self-Improvement | Ready | Insight capture + improvement PRs |

## Requirements

- GitHub repository with Actions enabled
- For Claude agents: `ANTHROPIC_API_KEY` secret

## Troubleshooting

### Workflow not triggering

1. Verify the issue has the `copilot` label
2. Check Actions are enabled in repository settings
3. Review workflow run logs in the Actions tab

### PR not created

1. Check workflow run for errors
2. Verify agent has write permissions
3. Review the issue format matches template

### Validation failures

1. Run `./scripts/validate.sh` locally
2. Install missing tools: yamllint, shellcheck, markdownlint
3. Fix reported issues before pushing

## License

MIT

## Contributing

1. Create an issue using the task template
2. Let @copilot implement it
3. Review and merge the PR

Meta: This system can improve itself through the same workflow it provides.
