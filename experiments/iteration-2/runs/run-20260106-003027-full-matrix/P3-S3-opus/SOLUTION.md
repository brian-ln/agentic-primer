# Issue-Driven Development System for @copilot

## Overview

This document describes an issue-driven development system that enables autonomous AI agents (like GitHub Copilot) to process issues and create pull requests with minimal human intervention.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         Issue-Driven Development                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────┐     ┌──────────────┐     ┌──────────────┐             │
│  │   Issue      │     │   @copilot   │     │     PR       │             │
│  │  Template    │────▶│   Workflow   │────▶│   Review     │             │
│  │  (task.yml)  │     │  (Actions)   │     │  (Web UI)    │             │
│  └──────────────┘     └──────────────┘     └──────────────┘             │
│         │                    │                    │                      │
│         ▼                    ▼                    ▼                      │
│  ┌──────────────────────────────────────────────────────────┐           │
│  │                    Knowledge Base                         │           │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐                │           │
│  │  │ patterns │  │decisions │  │ insights │                │           │
│  │  └──────────┘  └──────────┘  └──────────┘                │           │
│  └──────────────────────────────────────────────────────────┘           │
│                              │                                           │
│                              ▼                                           │
│                    ┌──────────────┐                                     │
│                    │ CODEOWNERS   │                                     │
│                    │ (auto-assign)│                                     │
│                    └──────────────┘                                     │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## Components

### 1. Issue Template (.github/ISSUE_TEMPLATE/task.yml)
**Purpose**: Provides structured input for @copilot tasks with required fields.

Fields:
- Task title (auto from issue title)
- Task description (what to implement)
- Success criteria (observable outcomes)
- Context files (relevant existing code)
- Constraints (limitations, do-nots)
- Agent assignment (which AI to use)

### 2. CODEOWNERS (.github/CODEOWNERS)
**Purpose**: Auto-assigns all PRs to repository owner for review.

Rule: `* @owner` assigns all files to the designated owner.

### 3. Knowledge Base (docs/knowledge/)
**Purpose**: Stores patterns, decisions, and insights for continuous learning.

Structure:
```
docs/knowledge/
├── README.md           # Knowledge base overview
├── patterns/           # Reusable implementation patterns
│   ├── README.md
│   └── *.md           # Individual pattern files
├── decisions/          # Architectural decision records (ADRs)
│   ├── README.md
│   └── *.md           # Individual decision files
└── insights/           # Learned insights from execution
    ├── README.md
    └── *.md           # Individual insight files
```

### 4. GitHub Workflow (.github/workflows/copilot-issue.yml)
**Purpose**: Triggers @copilot when issues are labeled.

Trigger: Issue labeled with `copilot`
Actions:
1. Parse issue template fields
2. Invoke @copilot agent
3. Create branch and PR
4. Update issue with progress

### 5. Bootstrap Script (scripts/bootstrap.sh)
**Purpose**: Single-command setup from bare repository.

Actions:
1. Create directory structure
2. Copy template files
3. Initialize knowledge base
4. Validate all syntax
5. Report success/failure

### 6. Validation Script (scripts/validate.sh)
**Purpose**: Validates syntax of all generated files.

Checks:
- YAML syntax (yamllint)
- Markdown syntax (markdownlint)
- Shell script syntax (shellcheck)

## Workflow

```
1. User creates issue using task.yml template
         │
         ▼
2. User adds 'copilot' label to issue
         │
         ▼
3. GitHub workflow triggers
         │
         ▼
4. @copilot reads issue, creates branch
         │
         ▼
5. @copilot implements solution
         │
         ▼
6. @copilot creates PR (auto-assigned via CODEOWNERS)
         │
         ▼
7. Human reviews PR in web UI
         │
         ▼
8. @copilot records learnings in knowledge base
         │
         ▼
9. PR merged, issue closed
```

## Success Criteria Mapping

| Criterion | Implementation |
|-----------|---------------|
| Functional Test | Workflow processes issues end-to-end |
| Syntax Valid | validate.sh checks all files |
| Observable Behavior | Workflow triggers on issue label |
| Reliability | Retry logic in workflow, knowledge base for patterns |
| Multi-Agent | Agent selector in issue template |
| Single-Command | bootstrap.sh sets up everything |
| Self-Improvement | Insights stored in knowledge base, workflow creates improvement PRs |

## Files Created

1. `.github/ISSUE_TEMPLATE/task.yml` - Issue template for @copilot tasks
2. `.github/CODEOWNERS` - PR auto-assignment rules
3. `.github/workflows/copilot-issue.yml` - Automation workflow
4. `docs/knowledge/README.md` - Knowledge base overview
5. `docs/knowledge/patterns/README.md` - Patterns directory
6. `docs/knowledge/decisions/README.md` - Decisions directory
7. `docs/knowledge/insights/README.md` - Insights directory
8. `docs/knowledge/patterns/001-issue-driven-development.md` - First pattern
9. `docs/knowledge/decisions/001-use-github-native-features.md` - First ADR
10. `docs/knowledge/insights/001-bootstrap-learnings.md` - First insight
11. `scripts/bootstrap.sh` - Single-command setup
12. `scripts/validate.sh` - Syntax validation
13. `README.md` - Complete workflow documentation

## Design Decisions

### Why GitHub Issue Templates?
- Native GitHub feature, no external dependencies
- Structured input reduces parsing errors
- Form-based UI improves user experience

### Why CODEOWNERS?
- Automatic PR assignment without workflow complexity
- Integrates with GitHub's review process
- Single line maintains simplicity

### Why Knowledge Base in docs/?
- Markdown is version-controlled and diffable
- Standard location (docs/) is discoverable
- Three-tier structure (patterns/decisions/insights) covers learning taxonomy

### Why Separate Workflow File?
- Decouples automation from issue template
- Enables independent testing and updates
- Clear separation of concerns

## Agent Compatibility

The system works with multiple AI agents:

| Agent | How It Works |
|-------|-------------|
| GitHub Copilot (@copilot) | Native issue assignment |
| Claude (Opus/Sonnet/Haiku) | Via GitHub Actions + API |
| Custom Agents | Via webhook + workflow dispatch |

The issue template includes an `agent` field to specify which agent should handle the task.

## Self-Improvement Mechanism

The system creates improvement PRs through:

1. **Pattern Detection**: Workflow scans completed PRs for common solutions
2. **Insight Capture**: Failures are logged with root cause in insights/
3. **Auto-PR Creation**: Workflow creates PRs to update knowledge base

Example improvement cycle:
```
Issue #1: "Add validation to user input"
  └─> PR #1: Adds input validation
       └─> Insight: "Input validation pattern identified"
            └─> PR #2: Adds pattern to docs/knowledge/patterns/
```

## Simulation Notes

This design was created by simulating @copilot's behavior:

1. **Analysis**: Read the prompt requirements and success criteria
2. **Research**: Considered GitHub native features vs. external tools
3. **Design**: Chose minimal, native approach for reliability
4. **Implementation**: Created all necessary files with complete content
5. **Validation**: Ensured syntax correctness and coverage of all criteria

The simulation assumes:
- Repository owner is specified as `@owner` (placeholder)
- GitHub Actions are enabled on the repository
- Agents have appropriate permissions for PR creation
