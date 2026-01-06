# Git-Native Issue Automation

AI agents execute GitHub issues as work items

## Overview

This repository implements a git-native issue automation system where GitHub Issues become executable work items. AI agents (Claude, Copilot, Gemini, Aider) can process issues labeled as tasks and automatically create pull requests with implementations.

## Quick Start

### 1. Create an Issue

Use the "AI Task" issue template to create a new task:

1. Go to Issues > New Issue
2. Select "AI Task" template
3. Fill in:
   - **Title**: Brief description of the task
   - **Description**: Detailed explanation of what needs to be done
   - **Acceptance Criteria**: Checklist of requirements
4. Submit the issue

The issue will automatically receive the `ai-task` label.

### 2. Automation Triggers

When an issue is created with or labeled `ai-task`, the workflow triggers and:
- Checks out the repository
- Sets up the environment
- Processes the issue (calls AI agent)
- Creates a pull request with changes

### 3. Review and Merge

Review the automatically created PR and merge when ready.

## Requirements

### Required GitHub Secrets

Depending on which AI agent you use, configure one or more of these secrets:

- `ANTHROPIC_API_KEY` - For Claude Code
- `GITHUB_TOKEN` - Automatically provided by GitHub Actions
- `OPENAI_API_KEY` - For OpenAI-based agents
- `GOOGLE_API_KEY` - For Gemini

### Setup Instructions

1. Go to Settings > Secrets and variables > Actions
2. Add your API key(s) as repository secrets
3. Update `.github/workflows/issue-agent.yml` to use your preferred AI agent

## Architecture

- `.github/workflows/issue-agent.yml` - Main automation workflow
- `.github/ISSUE_TEMPLATE/task.yml` - Issue template for AI tasks
- `docs/knowledge/` - Git-tracked knowledge base for patterns and decisions
- `scripts/verify-bootstrap.sh` - Validation script

## Knowledge Base

The `docs/knowledge/` directory stores:
- **patterns/** - Reusable patterns and best practices
- **decisions/** - Architecture decision records (ADRs)
- **insights/** - Learnings and observations

See `docs/knowledge/README.md` for contribution guidelines.

## Verification

To verify the bootstrap setup is correct:

```bash
./scripts/verify-bootstrap.sh
```

This checks that all required files exist and are properly configured.

## Learn More

- See `BOOTLOADER.md` for setup instructions with different AI agents
- See `ARCHITECTURE.md` for system design details
- See `ROADMAP.md` for development phases and future enhancements
