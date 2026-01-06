# Contributing to Agentic Primer

Thank you for your interest in contributing! This guide will help you understand our workflow and how to contribute effectively.

## Table of Contents

- [Getting Started](#getting-started)
- [Workflow Overview](#workflow-overview)
- [Using Issue Templates](#using-issue-templates)
- [Understanding Automation](#understanding-automation)
- [Contributing Code](#contributing-code)
- [Documentation](#documentation)
- [Best Practices](#best-practices)

## Getting Started

1. **Fork the repository** (for external contributors)
2. **Clone your fork** or the main repository
3. **Explore the structure** to understand the organization
4. **Read the README** for an overview of the system

## Workflow Overview

Our workflow is built around automated issue processing:

```
Issue Created → Automation Triggered → Branch Created → PR Opened → Review → Merge
```

### The Cycle

1. **Create Issue** - Use appropriate template
2. **Automation Runs** - Workflows process the issue
3. **Branch Created** - Work is isolated automatically
4. **PR Opened** - Ready for review and enhancement
5. **Review & Refine** - Collaborate on the work
6. **Merge** - Knowledge base updated

## Using Issue Templates

We have three main issue templates:

### 🔬 Research Tasks

**When to use:**
- Investigating new technologies
- Analyzing alternatives
- Documenting best practices
- Conducting feasibility studies

**Template fields:**
- Research Question (required)
- Context
- Scope
- Expected Deliverables
- Priority

**Automation creates:**
- Research document in `docs/knowledge/research/`
- Structured findings template
- Pull request for review

**Example:**
> **Title:** [Research] Best practices for GitHub Actions optimization
>
> **Research Question:** What are the most effective ways to optimize GitHub Actions workflows?
>
> **Context:** Our workflows are taking longer to run, impacting development velocity.

### 📋 Planning Tasks

**When to use:**
- Designing new features
- Planning architecture changes
- Breaking down large projects
- Creating implementation roadmaps

**Template fields:**
- Objective (required)
- Requirements
- Context
- Expected Deliverables
- Priority

**Automation creates:**
- Planning document in `docs/knowledge/planning/`
- Architecture overview
- Implementation plan with phases
- Task breakdown structure

**Example:**
> **Title:** [Planning] Multi-tenant architecture design
>
> **Objective:** Design a scalable multi-tenant architecture for the application
>
> **Requirements:**
> - Support 1000+ tenants
> - Data isolation
> - Performance at scale

### 🔨 Implementation Tasks

**When to use:**
- Building new features
- Fixing bugs
- Refactoring code
- Adding tests

**Template fields:**
- Description (required)
- Acceptance Criteria
- Technical Details
- Related Work
- Priority
- Complexity

**Automation creates:**
- Implementation branch
- Tracking document in `docs/knowledge/implementation/`
- Pull request for code changes

**Example:**
> **Title:** [Implementation] Add rate limiting to API
>
> **Description:** Implement rate limiting to prevent API abuse
>
> **Acceptance Criteria:**
> - [ ] Rate limiting middleware implemented
> - [ ] Tests pass
> - [ ] Documentation updated

## Understanding Automation

### Issue Router

The issue router (`issue-router.yml`) is the entry point:

- **Triggers:** When issues are opened or labeled
- **Actions:**
  - Detects issue type from labels
  - Comments with automation status
  - Provides guidance

### Research Automation

The research automation (`research-automation.yml`):

1. **Extracts** issue details (question, context, scope)
2. **Creates** research branch: `research/issue-{number}`
3. **Generates** research document template
4. **Opens** PR with the research structure
5. **Comments** on the issue with status

### Planning Automation

The planning automation (`planning-automation.yml`):

1. **Extracts** issue details (objective, requirements)
2. **Creates** planning branch: `planning/issue-{number}`
3. **Generates** planning document with sections
4. **Opens** PR with the planning structure
5. **Comments** on the issue with status

### Implementation Automation

The implementation automation (`implementation-automation.yml`):

1. **Extracts** issue details (description, acceptance criteria)
2. **Creates** implementation branch: `implementation/issue-{number}`
3. **Generates** tracking document
4. **Opens** PR for implementation
5. **Comments** on the issue with status

## Contributing Code

### Manual Contributions

If you prefer to work without automation:

1. **Create a branch** manually
2. **Make your changes**
3. **Open a PR** directly
4. **Link to related issues**

### Enhancing Automated Work

After automation creates a PR:

1. **Check out the branch**
   ```bash
   git fetch origin
   git checkout research/issue-5  # or planning/issue-10, etc.
   ```

2. **Review the generated documents**
   - Check `docs/knowledge/research/issue-5.md`
   - Or `docs/knowledge/planning/issue-10.md`
   - Or `docs/knowledge/implementation/issue-15.md`

3. **Enhance the content**
   - Add research findings
   - Complete planning details
   - Implement features
   - Add tests

4. **Commit your changes**
   ```bash
   git add .
   git commit -m "Enhance research findings"
   git push origin research/issue-5
   ```

5. **Update the PR** as needed

### Code Standards

- **Style** - Follow existing code patterns
- **Comments** - Document complex logic
- **Tests** - Add tests for new features
- **Documentation** - Update relevant docs

## Documentation

### Knowledge Base

All significant work should be documented in `docs/knowledge/`:

- **Research** - `docs/knowledge/research/`
- **Planning** - `docs/knowledge/planning/`
- **Implementation** - `docs/knowledge/implementation/`

### Document Structure

Follow the template structure for consistency:

**Research documents:**
```markdown
# Research: [Title]
**Issue:** #XX
**Date:** YYYY-MM-DD
**Status:** In Progress/Complete

## Research Question
## Context
## Scope
## Methodology
## Findings
## Recommendations
## References
## Next Steps
```

**Planning documents:**
```markdown
# Planning: [Title]
**Issue:** #XX
**Date:** YYYY-MM-DD
**Status:** In Progress/Complete

## Objective
## Requirements
## Architecture Overview
## Implementation Plan
## Task Breakdown
## Timeline
## Risk Assessment
## Success Criteria
```

**Implementation documents:**
```markdown
# Implementation: [Title]
**Issue:** #XX
**Date:** YYYY-MM-DD
**Status:** In Progress/Complete

## Description
## Acceptance Criteria
## Implementation Progress
## Code Changes
## Testing
## Documentation Updates
## Review Checklist
```

### Linking Documents

Always link related work:
- Reference issues: `#42`
- Link documents: `See docs/knowledge/research/issue-42.md`
- Cross-reference: `Implements planning from #38`

## Best Practices

### Issue Creation

- **Be specific** - Clear, detailed descriptions
- **Use templates** - They guide automation
- **Add context** - Explain why, not just what
- **Link related work** - Reference other issues/PRs

### Working with Automation

- **Review generated content** - Don't assume it's complete
- **Enhance documents** - Add your expertise
- **Update status** - Keep tracking accurate
- **Follow structure** - Maintain template format

### Pull Requests

- **Descriptive titles** - Clear and concise
- **Complete descriptions** - What, why, how
- **Link issues** - Use "Closes #XX" or "Fixes #XX"
- **Request reviews** - Tag relevant people
- **Respond to feedback** - Engage with reviewers

### Documentation

- **Update as you go** - Don't defer documentation
- **Be thorough** - Future you will thank you
- **Use examples** - Show, don't just tell
- **Keep it current** - Update outdated docs

### Knowledge Base

- **Consistent format** - Follow templates
- **Complete sections** - Don't leave TBD forever
- **Link liberally** - Connect related documents
- **Update status** - Mark completed work

## Workflow Examples

### Example 1: Research Workflow

```bash
# 1. Create research issue via GitHub UI
# 2. Automation creates branch and PR
# 3. Check out the branch
git fetch origin
git checkout research/issue-5

# 4. Enhance the research document
vim docs/knowledge/research/issue-5.md

# 5. Add findings, references, recommendations
# 6. Commit and push
git add docs/knowledge/research/issue-5.md
git commit -m "Complete research on authentication best practices"
git push origin research/issue-5

# 7. PR is updated automatically
# 8. Request review
# 9. Merge when approved
```

### Example 2: Planning Workflow

```bash
# 1. Create planning issue via GitHub UI
# 2. Automation creates branch and PR
# 3. Check out the branch
git fetch origin
git checkout planning/issue-10

# 4. Complete the planning document
vim docs/knowledge/planning/issue-10.md

# 5. Add architecture, tasks, timeline
# 6. Commit and push
git add docs/knowledge/planning/issue-10.md
git commit -m "Complete API v2 architecture plan"
git push origin planning/issue-10

# 7. Create implementation issues from the plan
# 8. Request review
# 9. Merge when approved
```

### Example 3: Implementation Workflow

```bash
# 1. Create implementation issue via GitHub UI
# 2. Automation creates branch and PR
# 3. Check out the branch
git fetch origin
git checkout implementation/issue-15

# 4. Implement the feature
# 5. Update tracking document
vim docs/knowledge/implementation/issue-15.md

# 6. Run tests
npm test  # or your test command

# 7. Commit and push
git add .
git commit -m "Implement user registration with tests"
git push origin implementation/issue-15

# 8. Mark items complete in tracking doc
# 9. Request review
# 10. Merge when approved
```

## Customizing the System

Want to adapt the system to your needs?

### Modify Templates

Edit `.github/ISSUE_TEMPLATE/*.yml` to:
- Add new fields
- Change required fields
- Update descriptions
- Add new templates

### Adjust Workflows

Edit `.github/workflows/*.yml` to:
- Change automation behavior
- Add new steps
- Modify document templates
- Add notifications

### Extend Structure

Modify `docs/knowledge/` to:
- Add new categories
- Change organization
- Add indexes
- Include examples

## Getting Help

- **Questions?** Open a discussion
- **Bug?** Create an issue
- **Feature idea?** Create a planning issue
- **Contribution?** Open a PR

## Code of Conduct

Be respectful, collaborative, and constructive. We're building something together!

---

Thank you for contributing to Agentic Primer! 🚀
