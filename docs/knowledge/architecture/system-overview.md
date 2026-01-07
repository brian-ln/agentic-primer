# System Architecture

## Overview

The agentic-primer project implements a git-native issue automation system where issues are executable and drive development workflows.

## Components

### 1. GitHub Actions Workflows

Located in `.github/workflows/`:

- **auto-assign.yml** - Automatically assigns PRs to repository owner
- **copilot-swe-agent** (dynamic) - Processes issues with GitHub Copilot

### 2. Issue Templates

Located in `.github/ISSUE_TEMPLATE/`:

- **research.yml** - Research and investigation tasks
- **implementation.yml** - Development and implementation tasks
- **test.yml** - System validation tasks
- **config.yml** - Template configuration

### 3. Knowledge Base

Located in `docs/knowledge/`:

- **conventions/** - Coding standards and best practices
- **architecture/** - System design documentation
- **workflows/** - Process and workflow guides
- **decisions/** - Architecture Decision Records

## Data Flow

```
Issue Created
    ↓
Auto-labeled and Assigned
    ↓
Copilot Mentioned (@copilot)
    ↓
Workflow Triggered
    ↓
Branch Created
    ↓
Changes Implemented
    ↓
PR Created and Assigned
    ↓
Review and Merge
    ↓
Issue Closed
```

## Key Principles

1. **Git-Native** - Everything is tracked in git
2. **Automated** - Workflows reduce manual work
3. **Traceable** - Full history and audit trail
4. **Collaborative** - Humans and AI work together
5. **Incremental** - Small, focused changes

## Integration Points

- **GitHub API** - Issue and PR management
- **GitHub Actions** - Workflow automation
- **GitHub Copilot** - AI-powered development
- **Git** - Version control and history
