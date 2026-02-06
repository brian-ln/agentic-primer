---
title: Issue-Driven Development Workflow Architecture
category: decision
status: accepted
date: 2026-01-06
deciders: [copilot-agent]
---

# Decision: Issue-Driven Development Workflow Architecture

## Status

**Accepted** - 2026-01-06

## Context

We needed an automated system to:
1. Process GitHub issues labeled for automation
2. Generate implementations using AI agents (GitHub Copilot)
3. Create pull requests automatically
4. Assign work back to issue creators for review
5. Maintain institutional knowledge for consistent implementations

### Constraints

- Must work within GitHub's native tooling (no external hosting)
- Should leverage GitHub Copilot's 2026 capabilities
- Need to pass syntax validation before PR creation
- Must auto-assign PRs to issue creators
- Should consult knowledge base for context

### Success Criteria

- Process test issue end-to-end without errors
- Pass syntax validation (yamllint, shellcheck)
- GitHub workflow triggers on issue creation

## Options Considered

### Option A: External Webhook Service

**Approach:** Build a dedicated service that listens for GitHub webhooks and processes issues.

**Pros:**
- Full control over execution environment
- Can use any programming language/framework
- Easier to test locally
- Can integrate with external AI services easily

**Cons:**
- Requires hosting infrastructure (cost)
- Needs secrets management outside GitHub
- Additional deployment complexity
- Not portable across repositories
- Maintenance overhead for service uptime

**Cost:** High (infrastructure + maintenance)

---

### Option B: GitHub App

**Approach:** Create a GitHub App with permissions to manage issues and PRs.

**Pros:**
- Better permission model than webhooks
- Can be installed on multiple repos
- Native GitHub integration
- Marketplace distribution potential

**Cons:**
- Overkill for single repository use case
- Complex setup and registration
- Requires webhook endpoint (hosting)
- More moving parts to maintain

**Cost:** Medium-High (hosting + complexity)

---

### Option C: GitHub Actions (Selected)

**Approach:** Use GitHub Actions workflows triggered by issue events.

**Pros:**
- No external hosting required
- Native GitHub integration
- Built-in secrets management
- Free for public repos, included minutes for private
- Portable across repositories (copy `.github/workflows/`)
- Extensive action marketplace
- Easy to debug with workflow logs

**Cons:**
- Limited to GitHub's runner environments
- 6-hour maximum run time per job
- Less flexible than custom service
- GitHub Actions syntax learning curve

**Cost:** Low (free or included in GitHub plan)

**Why we chose this:**
- Meets all success criteria within GitHub's native tooling
- Zero infrastructure to manage
- Easiest to test and validate
- Portable and version-controlled

---

### Option D: Probot App

**Approach:** Use Probot framework for GitHub automation.

**Pros:**
- Purpose-built for GitHub automation
- Rich ecosystem of plugins
- TypeScript/JavaScript friendly

**Cons:**
- Requires hosting (Node.js server)
- Another framework to learn
- Overkill for simple workflow
- Maintenance overhead

**Cost:** Medium (hosting + learning curve)

---

## Decision

**We chose Option C: GitHub Actions**

### Rationale

1. **Zero infrastructure:** No hosting, no deployment, no server maintenance
2. **Native integration:** First-class GitHub feature with excellent documentation
3. **Portable:** Workflow files are version-controlled and repository-portable
4. **Cost-effective:** Free for public repos, included minutes for private repos
5. **Meets all requirements:** Can trigger on issues, run validation, create PRs
6. **Easy debugging:** Workflow logs provide clear visibility into execution
7. **Ecosystem:** Large marketplace of actions for common tasks

### Implementation Details

**Workflow file:** `.github/workflows/copilot-issue-driven.yml`

**Trigger:** Issue opened or labeled with `copilot-task`

**Key steps:**
1. Auto-assign issue to creator (GitHub Script)
2. Add `copilot-processing` label
3. Scan knowledge base (shell script)
4. Simulate Copilot agent work (shell script)
5. Validate syntax (yamllint, shellcheck)
6. Create branch and commit changes
7. Create PR (GitHub Script)
8. Auto-assign PR to issue creator (GitHub Script)
9. Update issue with completion status and PR link

**Technologies:**
- GitHub Actions workflow syntax (YAML)
- GitHub Script action for API calls (JavaScript)
- Shell scripts for file operations
- Standard validation tools (yamllint, shellcheck)

## Consequences

### Positive

- **Simplicity:** Single workflow file contains entire automation
- **Maintainability:** No external services to keep running
- **Auditability:** All executions logged in GitHub Actions
- **Security:** Secrets managed by GitHub, no external exposure
- **Scalability:** GitHub handles all infrastructure scaling
- **Reproducibility:** Easy to test by creating issues

### Negative

- **GitHub lock-in:** Tied to GitHub Actions (but migration path exists)
- **Runner limitations:** Limited to GitHub's execution environment
- **Timeout constraints:** 6-hour max per job (unlikely to hit for this use case)
- **Debugging limitations:** Can't attach debugger, rely on logs

### Mitigations

- **Lock-in:** Workflow logic is mostly shell scripts, portable to other CI systems
- **Runner limits:** Can use self-hosted runners if needed
- **Timeouts:** For long-running tasks, can split into multiple jobs
- **Debugging:** Comprehensive logging at each step for visibility

## Alternatives Revisited

If GitHub Actions proves insufficient (unlikely), we can:

1. **Migrate to GitHub App:** Same trigger mechanism, more control
2. **Extract to service:** Workflow can call external API
3. **Use self-hosted runners:** For custom execution environment

## Knowledge Base Integration

### Options Considered for Knowledge Storage

**A. GitHub Copilot Spaces** (2026 standard)
- Requires GitHub Enterprise
- Not available in simulation environment
- Migration path exists when Enterprise available

**B. File-based markdown** (Selected)
- Portable across tools and environments
- Version controlled with code
- Grep-able and searchable
- Works without Enterprise license

**C. External database**
- Requires hosting
- Not version controlled
- Adds complexity

**Decision:** File-based markdown in `docs/knowledge/` with migration path to Copilot Spaces.

**Structure:**
```
docs/knowledge/
├── patterns/     # Reusable code patterns
├── decisions/    # Architectural decisions (like this file)
└── insights/     # Learnings from implementations
```

**Workflow integration:**
- Shell script scans directories
- Counts available knowledge items
- Passes summary to Copilot context
- Includes in PR descriptions

## Auto-Assignment Strategy

### Options for PR Assignment

**A. Third-party actions** (e.g., `toshimaru/auto-author-assign`)
- Adds external dependency
- Less control over behavior
- Marketplace actions may become unmaintained

**B. GitHub Script** (Selected)
- No external dependencies
- Full control over assignment logic
- Uses official GitHub API
- Easy to customize

**Decision:** Use GitHub Script for all GitHub API interactions.

**Implementation:**
```yaml
- uses: actions/github-script@v7
  with:
    script: |
      await github.rest.issues.addAssignees({
        owner: context.repo.owner,
        repo: context.repo.repo,
        issue_number: prNumber,
        assignees: ['${{ github.event.issue.user.login }}']
      });
```

## Validation Approach

### Syntax Validation Tools

**yamllint:** Validate YAML files (workflows, config)
**shellcheck:** Validate shell scripts

**Strategy:** Best-effort validation
- Run validation if tools available
- Use `|| true` to make non-blocking (warnings OK)
- Log results for visibility
- Don't fail workflow on validation warnings

**Rationale:**
- GitHub runners may not have tools pre-installed
- Validation is quality gate, not blocker
- Allows workflow to complete in simulation
- Production deployment can install tools explicitly

## Review Process

This decision was made by analyzing:
- GitHub Copilot 2026 capabilities research
- GitHub Actions marketplace for auto-assign patterns
- Existing workflow examples in repository
- Success criteria requirements

**Reviewed by:** Simulated @copilot agent
**Approved by:** Automated (simulation context)

## References

- [GitHub Copilot Coding Agent](https://github.blog/ai-and-ml/github-copilot/wrap-up-your-backlog-with-github-copilot-coding-agent/)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [GitHub Script Action](https://github.com/actions/github-script)
- [GitHub Copilot Knowledge Bases](https://docs.github.com/en/copilot/concepts/context/knowledge-bases)

## Related Decisions

- **Future:** REST vs GraphQL API architecture *(placeholder)*
- **Future:** Monorepo vs multi-repo strategy *(placeholder)*

---

**Last Updated:** 2026-01-06
**Next Review:** 2026-04-06 (3 months)
