# Issue-Driven Development Workflow

## Overview

This project uses issue-driven development where issues are the primary unit of work. Each issue represents a discrete task that can be processed by GitHub Copilot or human contributors.

## Workflow Steps

### 1. Create Issue

Create an issue using one of the available templates:
- **Research Task** - For investigation and analysis
- **Implementation Task** - For development work
- **Test Issue** - For system validation

### 2. Assignment

When an issue is created:
- It is automatically labeled based on the template
- It is assigned to the repository owner by default
- Copilot can be mentioned with `@copilot` to trigger automated processing

### 3. Processing

For Copilot-enabled issues:
1. Copilot creates a branch named `copilot/<issue-description>`
2. Copilot implements the required changes
3. Copilot creates a pull request
4. PR is automatically assigned to the repository owner

### 4. Review and Merge

1. Review the PR and provide feedback
2. Copilot can address feedback if mentioned
3. Merge the PR when complete
4. The issue is automatically closed

## Best Practices

- **One Issue, One Task** - Keep issues focused on a single objective
- **Clear Descriptions** - Provide clear, actionable descriptions
- **Reference Knowledge Base** - Link to relevant documentation
- **Update Documentation** - Keep knowledge base current
- **Small Changes** - Prefer small, incremental changes

## Copilot Integration

To trigger Copilot processing:
```
@copilot can you take this one?
```

Or:
```
@copilot please implement this feature
```

Copilot will:
- Analyze the issue
- Create a plan
- Implement changes
- Create a PR
- Respond to feedback
