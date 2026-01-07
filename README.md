# Agentic Primer

A git-native issue automation system where issues are executable and drive development workflows through GitHub Copilot integration.

## Overview

This repository demonstrates issue-driven development with automated workflows. Issues become the primary unit of work, with GitHub Copilot handling implementation tasks.

## Features

- ✅ **Auto-assign PRs** - PRs automatically assigned to repository owner
- ✅ **Issue Templates** - Structured templates for research, implementation, and testing
- ✅ **Knowledge Base** - Git-tracked documentation and conventions
- ✅ **Copilot Integration** - AI-powered issue processing
- ✅ **Automated Workflows** - GitHub Actions for automation

## Quick Start

### Creating an Issue

1. Go to [Issues](../../issues/new/choose)
2. Select a template:
   - **Research Task** - Investigation and analysis
   - **Implementation Task** - Development work
   - **Test Issue** - System validation
3. Fill in the details
4. Submit the issue

### Triggering Copilot

Mention Copilot in a comment:
```
@copilot can you take this one?
```

Copilot will:
- Create a branch
- Implement the changes
- Create a pull request
- Assign it to you

### Review and Merge

1. Review the PR
2. Test the changes
3. Provide feedback if needed
4. Merge when ready

## Documentation

- [Issue-Driven Development Workflow](docs/knowledge/workflows/issue-driven-development.md)
- [System Architecture](docs/knowledge/architecture/system-overview.md)
- [Knowledge Base](docs/knowledge/README.md)

## Repository Structure

```
.
├── .github/
│   ├── workflows/          # GitHub Actions workflows
│   └── ISSUE_TEMPLATE/     # Issue templates
├── docs/
│   └── knowledge/          # Knowledge base
│       ├── conventions/    # Coding standards
│       ├── architecture/   # Design docs
│       ├── workflows/      # Process guides
│       └── decisions/      # ADRs
└── README.md
```

## Contributing

This is an experimental system for issue-driven development. Contributions are welcome through the standard issue → PR workflow.

## License

MIT
