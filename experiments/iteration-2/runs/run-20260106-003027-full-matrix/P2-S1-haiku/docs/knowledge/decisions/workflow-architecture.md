# Decision: GitHub Actions + GitHub Script Workflow Architecture

## Status

Accepted

## Context

We needed to design an automation system that:
- Processes GitHub issues automatically when labeled
- Creates pull requests without external hosting
- Auto-assigns PRs to issue creators
- Integrates with GitHub's native features
- Works in simulated and real environments

We considered several approaches:
1. **Webhooks + External Service**: Requires separate hosting, authentication, environment management
2. **GitHub Apps**: Overkill for single-repo automation, adds complexity
3. **Probot (Node.js + GitHub App)**: Better for plugins, still requires hosting
4. **GitHub Actions + GitHub Script**: Native GitHub integration, no external hosting needed

## Decision

We chose **GitHub Actions with GitHub Script** for our issue-driven automation:

### Architecture Components

1. **GitHub Actions Workflow** (`.github/workflows/copilot-issue-driven.yml`)
   - Triggered on issue creation/labeling
   - Orchestrates all workflow steps
   - Runs on Ubuntu-latest runner

2. **GitHub Script Action** (built-in actions/github-script)
   - Auto-assigns issues to creator
   - Creates pull requests programmatically
   - Updates issue labels and comments
   - No external dependencies

3. **Knowledge Base** (`docs/knowledge/`)
   - File-based, version-controlled
   - Scanned by workflow at runtime
   - Provides context to agent

4. **CODEOWNERS**
   - Automatic PR assignment
   - GitHub native mechanism
   - No action needed

## Consequences

### Positive

- **No external hosting**: Reduces operational burden
- **Native GitHub integration**: Uses platform features directly
- **Audit trail**: All actions in workflow logs
- **Permissions model**: Built-in GitHub permissions system
- **Secrets management**: GitHub Actions native secrets
- **Cost-effective**: Uses included GitHub Actions minutes
- **Simulated easily**: Can be tested without real GitHub API calls
- **Future-proof**: Works with real Copilot API when available

### Negative

- **Workflow complexity**: Single file gets large with many steps
- **Debugging**: Require reading workflow logs in GitHub UI
- **Rate limiting**: Subject to GitHub API rate limits
- **Branch cleanup**: Manual deletion or separate action needed
- **Error handling**: GitHub Script sometimes terse error messages

### Mitigations

- Comprehensive logging at each step
- Continue-on-error for optional steps (validation tools)
- Clear variable names and step comments
- Separate workflow file makes changes easy
- Document all assumptions

## Alternatives Considered

### 1. Webhooks + External Service

**Approach**: GitHub sends webhook to our server which processes issues

**Pros**: Maximum flexibility, custom business logic

**Cons**:
- Requires hosting (serverless or server)
- Authentication/authorization complexity
- Environment setup needed
- Debugging across systems harder
- Not simulated easily

**Rejected**: Adds infrastructure burden

### 2. GitHub Apps

**Approach**: Create GitHub App for issue automation

**Pros**: More powerful, can be published

**Cons**:
- Complex setup (must be registered with GitHub)
- Installation step required
- App permissions to manage
- Overkill for single-repo use

**Rejected**: Too complex for current scope

### 3. Probot (Node.js Framework)

**Approach**: Use Probot framework for GitHub automation

**Pros**: Good for plugin development

**Cons**:
- Requires Node.js hosting
- Additional framework to learn
- Deployment complexity
- Debugging across services

**Rejected**: Too heavy for issue-driven dev

## Related Decisions

- **Knowledge Base Structure**: File-based markdown (not Copilot Spaces)
- **Label-based Triggering**: Explicit `copilot-task` label prevents accidents
- **PR Auto-assignment**: CODEOWNERS instead of custom logic

## Implementation Notes

The workflow is implemented in `.github/workflows/copilot-issue-driven.yml` with:

- 10+ steps covering full automation flow
- Error handling for each critical step
- Environment variables for configuration
- GitHub Script for all API calls
- Comprehensive logging and summary

## Testing

To validate this architecture:

1. Create test issue with `copilot-task` label
2. Observe workflow run in Actions tab
3. Verify PR created and assigned
4. Check issue comments updated
5. Confirm labels changed correctly

## Future Evolution

If we need to migrate from GitHub Actions:

1. **Phase 1**: Keep current workflow, add new action alongside
2. **Phase 2**: Gradually move logic to new system
3. **Phase 3**: Deprecate old workflow
4. **Phase 4**: Clean up

Current architecture makes this migration smooth since logic is contained in single workflow file.

---

**Last Updated:** 2026-01-08
**Version:** 1.0
**Maintainer:** Development team
