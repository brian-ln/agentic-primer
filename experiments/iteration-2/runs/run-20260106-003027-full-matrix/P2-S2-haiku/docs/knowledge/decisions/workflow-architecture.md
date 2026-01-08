---
title: Decision - GitHub Actions for Issue-Driven Automation
category: decision
status: accepted
date: 2026-01-08
---

# Decision: GitHub Actions for Issue-Driven Automation

## Context

We needed to implement automated issue processing and pull request generation for a GitHub Copilot agent. This required choosing a platform for orchestrating the workflow:

- How to trigger automation on issue events?
- How to automate repository operations (branching, commits, PRs)?
- How to manage GitHub API authentication and permissions?
- How to ensure the solution is maintainable and cost-effective?

## Options Considered

### Option A: GitHub Actions (Selected)

**Implementation:**
- Use GitHub Actions workflows triggered on issue events
- GitHub Script action for GitHub API interactions
- Standard runner (Ubuntu-latest) with shell scripting

**Pros:**
- Native GitHub integration, no external service needed
- Built-in event system (issues, labels, pull_request)
- GitHub Script provides direct API access with proper authentication
- Permissions are managed through repository settings
- Free tier generous (2000 minutes/month for private repos)
- Logs are stored in repository for audit trail
- No additional infrastructure to maintain

**Cons:**
- Limited to GitHub-hosted runners (without self-hosted)
- Workflow files are verbose YAML
- Error handling is shell-based (less elegant)
- Limited environment variables and context

### Option B: GitHub App + Custom Server

**Implementation:**
- Build custom GitHub App
- Run server to receive webhooks
- Server implements business logic and GitHub API calls

**Pros:**
- More control over logic and error handling
- Can implement sophisticated decision trees
- Can integrate with other systems easily
- Can use any programming language

**Cons:**
- Requires external hosting and maintenance
- More complex setup and configuration
- Higher operational cost
- Potential security concerns with webhook validation
- Harder to debug without centralized logging
- Need to manage authentication tokens

### Option C: Probot (GitHub App Framework)

**Implementation:**
- Use Probot framework for GitHub App development
- Deploy to external platform (Heroku, AWS, etc.)
- Simpler than raw GitHub App

**Pros:**
- Framework reduces boilerplate
- Good documentation and examples
- Community-maintained

**Cons:**
- Still requires external hosting
- Operational overhead
- Vendor lock-in (framework-specific)
- More moving parts than GitHub Actions

### Option D: Third-Party Automation Service

**Implementation:**
- Use service like CircleCI, Travis CI, or similar
- Configure to process GitHub issues

**Pros:**
- May have built-in GitHub integrations
- Can run on your own runners

**Cons:**
- Adds external dependency
- Learning curve for new platform
- Potential cost at scale
- May have limitations for issue-specific workflows

## Decision

**We chose: GitHub Actions (Option A)**

### Rationale

1. **Native Integration**: GitHub Actions is built into GitHub, no external services needed
2. **Simplicity**: Straightforward setup - just add workflow file to repository
3. **Cost**: Generous free tier covers most use cases
4. **Reliability**: GitHub-hosted, no infrastructure to maintain
5. **Auditability**: Workflow runs and logs stored in repository
6. **Discoverability**: Other developers can easily find and understand workflow
7. **Permissions Model**: Repository permissions naturally map to workflow capabilities
8. **GitHub Script**: Excellent library for GitHub API operations

### Implementation Details

- **Trigger**: `on: issues: types: [opened, labeled]`
- **Condition**: `if: contains(github.event.issue.labels.*.name, 'copilot-task')`
- **Language**: Shell scripts with GitHub Script for API calls
- **Permissions**:
  - `contents: write` - for git operations
  - `pull-requests: write` - for PR creation
  - `issues: write` - for issue management

## Consequences

### Positive

- **Easy to understand**: New team members can read the workflow file
- **Easy to modify**: Changes are code-reviewed like any other code
- **Built-in observability**: Logs are available in Actions tab
- **No infrastructure**: No server to manage, monitor, or pay for
- **Secure by default**: Permissions enforced through repository settings
- **Scalable**: GitHub handles running at any scale

### Negative

- **Vendor lock-in**: Tightly coupled to GitHub (but that's where we're hosting)
- **Limited environment**: Only what's available on Ubuntu runners
- **Verbose YAML**: Workflow files can get long
- **Debugging**: Need to push to see results (can't test locally easily)
- **Concurrent runs**: Only one workflow per branch/tag by default

### Mitigation Strategies

- **Shell script complexity**: Keep complex logic in external scripts committed to repo
- **Debugging**: Add comprehensive logging at each step
- **Maintenance**: Document each step clearly with comments
- **Future changes**: Make workflow modular so pieces can be updated independently

## Related Decisions

- [Knowledge Base Structure](./knowledge-base-structure.md) - How information is stored for Copilot agent
- [PR Creation Strategy](./pr-creation.md) - How PRs are generated and assigned

## Timeline

- **Decision Date**: 2026-01-08
- **Status**: Accepted
- **Implementation Date**: 2026-01-08
- **Review Cycle**: Quarterly (January)

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [GitHub Script Action](https://github.com/actions/github-script)
- [GitHub Issue Events](https://docs.github.com/en/webhooks-and-events/webhooks/webhook-events-and-payloads#issues)

---

**Last Updated:** 2026-01-08
**Owner:** Engineering Team
**Status:** Active
