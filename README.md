# Agentic Primer

A git-native issue automation system where issues are executable.

## Overview

This repository implements an automated workflow system that transforms GitHub issues into actionable tasks with automated execution. Issues labeled appropriately trigger workflows that handle research, planning, and implementation tasks automatically.

## Key Features

- **🔬 Research Automation** - Investigate topics and document findings
- **📋 Planning Automation** - Create architecture docs and implementation plans
- **🔨 Implementation Automation** - Execute implementations with tracking
- **📚 Knowledge Base** - Git-tracked documentation of all work
- **🤖 Automated Workflows** - Label-based issue routing and execution

## How It Works

### 1. Create an Issue

Use one of the issue templates:
- **Research Task** - For investigation and analysis
- **Planning Task** - For architecture and design
- **Implementation Task** - For coding and execution

### 2. Automatic Processing

When you create an issue with the appropriate label:
- The **Issue Router** identifies the task type
- The corresponding automation workflow starts
- A branch is created automatically
- Documentation is generated in `docs/knowledge/`
- A pull request is opened with the work

### 3. Review and Merge

- Review the automated work in the PR
- Enhance or refine as needed
- Merge when complete
- The knowledge base is updated automatically

## Issue Types

### 🔬 Research Tasks

Use the `research` label to trigger research automation:
- Investigates research questions
- Documents findings in `docs/knowledge/research/`
- Provides recommendations
- Creates structured research documents

**Example uses:**
- "Research best practices for CI/CD"
- "Investigate alternative authentication methods"
- "Analyze performance optimization strategies"

### 📋 Planning Tasks

Use the `planning` label to trigger planning automation:
- Creates architecture documents
- Breaks down implementation into tasks
- Generates timelines and milestones
- Produces detailed plans in `docs/knowledge/planning/`

**Example uses:**
- "Plan the API redesign"
- "Design the new feature architecture"
- "Create implementation roadmap"

### 🔨 Implementation Tasks

Use the `implementation` label to trigger implementation automation:
- Creates implementation branches
- Tracks progress with documentation
- Manages code changes
- Opens PRs with work in `docs/knowledge/implementation/`

**Example uses:**
- "Implement user authentication"
- "Add new API endpoint"
- "Refactor database layer"

## Project Structure

```
.github/
├── workflows/           # GitHub Actions automation
│   ├── issue-router.yml
│   ├── research-automation.yml
│   ├── planning-automation.yml
│   └── implementation-automation.yml
└── ISSUE_TEMPLATE/      # Issue templates
    ├── config.yml
    ├── research.yml
    ├── planning.yml
    └── implementation.yml

docs/
└── knowledge/           # Git-tracked knowledge base
    ├── research/        # Research findings
    ├── planning/        # Architecture and plans
    └── implementation/  # Implementation tracking
```

## Quick Start

### Creating a Research Issue

1. Go to **Issues** → **New Issue**
2. Select **🔬 Research Task**
3. Fill in the research question and context
4. Submit the issue
5. The automation creates a research document and PR

### Creating a Planning Issue

1. Go to **Issues** → **New Issue**
2. Select **📋 Planning Task**
3. Describe the objective and requirements
4. Submit the issue
5. The automation creates a planning document and PR

### Creating an Implementation Issue

1. Go to **Issues** → **New Issue**
2. Select **🔨 Implementation Task**
3. Describe what needs to be implemented
4. Submit the issue
5. The automation creates an implementation branch and PR

## Workflows

### Issue Router (`issue-router.yml`)

Routes issues based on labels and provides feedback:
- Detects issue type from labels
- Comments with automation status
- Guides users on next steps

### Research Automation (`research-automation.yml`)

Automates research tasks:
- Creates research branch
- Generates research document template
- Opens PR with findings structure

### Planning Automation (`planning-automation.yml`)

Automates planning tasks:
- Creates planning branch
- Generates architecture document
- Structures implementation plans
- Opens PR with planning documents

### Implementation Automation (`implementation-automation.yml`)

Automates implementation tasks:
- Creates implementation branch
- Sets up tracking document
- Opens PR for code changes
- Tracks progress and testing

## Knowledge Base

The `docs/knowledge/` directory serves as a git-tracked knowledge base:

- **Permanent Record** - All decisions and rationale are documented
- **Searchable History** - Git history provides full traceability
- **Linked Documents** - Issues, PRs, and docs are interconnected
- **Structured Information** - Consistent templates for all document types

### Why Git-Native?

- **Version Control** - Full history of all knowledge
- **Branching** - Work in progress is isolated
- **Pull Requests** - Review process for knowledge
- **GitHub Integration** - Seamless with existing workflows
- **No External Tools** - Everything in the repository

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed information on:
- How to contribute
- Workflow guidelines
- Best practices
- Development setup

## Architecture

The system is built on:
- **GitHub Actions** - Workflow automation
- **GitHub Issues** - Task management
- **Issue Templates** - Structured input
- **Git Branches** - Work isolation
- **Pull Requests** - Review and merge
- **Markdown** - Documentation format

## Benefits

### For Teams
- **Automated Documentation** - Knowledge captured automatically
- **Consistent Process** - Same workflow for all tasks
- **Historical Record** - Git tracks everything
- **Reduced Overhead** - Less manual work

### For Projects
- **Scalable** - Handles any number of issues
- **Transparent** - All work is visible
- **Reproducible** - Clear process for all tasks
- **Maintainable** - Knowledge base grows with project

### For Individuals
- **Clear Structure** - Templates guide the work
- **Automated Setup** - Branches and PRs created automatically
- **Focus on Content** - Less process management
- **Learning Resource** - Past work is documented

## Examples

### Research Example

**Issue:** "Research authentication best practices"

**Automation creates:**
- Branch: `research/issue-5`
- Document: `docs/knowledge/research/issue-5.md`
- PR: "Research: authentication best practices"

### Planning Example

**Issue:** "Plan API v2 architecture"

**Automation creates:**
- Branch: `planning/issue-10`
- Document: `docs/knowledge/planning/issue-10.md`
- PR: "Planning: API v2 architecture"

### Implementation Example

**Issue:** "Implement user registration"

**Automation creates:**
- Branch: `implementation/issue-15`
- Document: `docs/knowledge/implementation/issue-15.md`
- PR: "Implementation: user registration"

## Customization

You can customize the system by:

1. **Modifying Templates** - Edit `.github/ISSUE_TEMPLATE/*.yml`
2. **Adjusting Workflows** - Update `.github/workflows/*.yml`
3. **Changing Structure** - Reorganize `docs/knowledge/`
4. **Adding Labels** - Create new automation triggers

## License

This project is open source. Feel free to use, modify, and distribute as needed.

## Support

- **Issues** - Report bugs or request features
- **Discussions** - Ask questions and share ideas
- **Pull Requests** - Contribute improvements

---

Built with ❤️ for automated, git-native workflows
