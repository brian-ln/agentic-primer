# ADR-001: Core System Architecture

**Status**: Accepted
**Date**: 2026-01-08
**Deciders**: @copilot (simulated)

## Context

We need to build an automated issue processing system that:
- Enables AI agents (@copilot) to process GitHub issues autonomously
- Validates generated code before merge
- Learns from past implementations (knowledge base)
- Works with multiple AI models (Opus, Sonnet, Haiku)
- Requires zero manual intervention after bootstrap

The system must be reliable (90%+ success rate), observable (all actions visible), and self-improving.

## Decision

We will implement a GitHub Actions-based automation system with three core workflows:

1. **Issue Assignment Workflow**: Triggers when @copilot is assigned to an issue
   - Parses structured issue data
   - Searches knowledge base for relevant patterns
   - Generates code/configuration
   - Creates pull request

2. **PR Validation Workflow**: Runs on all PRs
   - Syntax validation (YAML, shell, markdown)
   - Security scanning (secrets detection)
   - Test execution
   - Quality gates for merge

3. **Knowledge Base Update Workflow**: Post-merge learning
   - Extracts patterns from merged PRs
   - Updates documentation automatically
   - Identifies improvement opportunities

Supporting components:
- **Issue Templates**: Structured input via GitHub issue forms
- **Knowledge Base**: Hierarchical documentation (patterns/decisions/insights)
- **Bootstrap Script**: Single-command setup
- **Validation Scripts**: Pre-commit checks

## Alternatives Considered

### Alternative A: External CI/CD Platform (CircleCI, Jenkins)

**Pros**:
- More powerful compute options
- Advanced caching and parallelization
- Existing organizational knowledge

**Cons**:
- Requires external account and billing
- Additional complexity for users
- Not integrated with GitHub UI
- Setup friction (authentication, webhooks)

**Reason rejected**: GitHub Actions provides native integration, zero setup friction, and visible audit trail.

### Alternative B: Serverless Functions (AWS Lambda, Cloud Functions)

**Pros**:
- Highly scalable
- Pay-per-use pricing
- Can run arbitrary code

**Cons**:
- Requires cloud account
- Complex cold start issues
- Vendor lock-in
- Debugging is harder
- Not observable in GitHub UI

**Reason rejected**: Too much operational complexity for the benefits gained.

### Alternative C: Self-Hosted Runners with Custom Service

**Pros**:
- Full control over environment
- Can use proprietary tools
- No GitHub Actions minute limits

**Cons**:
- Infrastructure management burden
- Security risks (need to secure runners)
- Higher barrier to adoption
- Not portable across organizations

**Reason rejected**: Violates "single-command setup" requirement.

### Alternative D: Manual Workflow with GitHub CLI

**Pros**:
- Simple to understand
- Full human control
- Easy to debug

**Cons**:
- Not automated (human in the loop)
- Slow turnaround time
- Doesn't scale
- Violates "zero manual intervention" requirement

**Reason rejected**: Defeats the purpose of automation.

## Consequences

### Positive

1. **Zero Setup Friction**: GitHub Actions works on any repository immediately
2. **Observable**: All automation visible in GitHub Actions tab
3. **Integrated**: Issue → PR flow entirely within GitHub UI
4. **Portable**: Solution works on any GitHub repository
5. **Maintainable**: YAML workflows are readable and versionable
6. **Secure**: Leverages GitHub's token and secret management
7. **Auditable**: Every run logged and traceable

### Negative

1. **GitHub Lock-In**: Difficult to migrate to other platforms
2. **Minute Limits**: Free tier has 2000 minutes/month (manageable for this use case)
3. **Workflow Language**: YAML + bash scripting has learning curve
4. **Debugging**: Workflow failures require inspecting GitHub logs
5. **Concurrency Limits**: Free tier has job concurrency limits

### Neutral

1. **Performance**: Adequate for this use case (not high-frequency)
2. **Cost**: Free for public repos, reasonable for private repos
3. **Community**: Large ecosystem of pre-built actions

## Implementation Notes

### Workflow Design Principles

1. **Idempotent**: Safe to re-run workflows
2. **Fast Feedback**: Fail fast on validation errors
3. **Error Handling**: All workflows have failure handlers
4. **Simulation Mode**: Test without side effects

### Key Technical Choices

- **GitHub Script Action**: JavaScript for GitHub API interaction
- **Bash Scripts**: Portable across environments
- **Markdown**: Human and AI readable documentation
- **YAML**: Structured configuration and validation

### Security Considerations

- All actions pinned to commit SHAs (not tags)
- Minimal permissions (principle of least privilege)
- Secret scanning in PR validation
- No secrets stored in repository

### Scalability

Current design supports:
- ~100 issues/month (well within GitHub Actions limits)
- Multiple concurrent workflows
- Growing knowledge base (hundreds of documents)

If scaling needs increase:
- Consider self-hosted runners
- Implement workflow queuing
- Add caching for knowledge base search

## Related Decisions

- ADR-002: Use GitHub Actions (this decision)
- ADR-003: Structured Knowledge Base

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [GitHub Copilot Workspace](https://githubnext.com/projects/copilot-workspace) - Inspiration
- [Qodo AI Code Review](https://www.qodo.ai/) - Knowledge base patterns
- Research: "AI Coding Agents" (2026) - Multi-agent compatibility

---

**Review History**:
- 2026-01-08: Initial decision
- (Future reviews will be logged here)

**Implementation Status**: ✅ Implemented
**Success Metrics**: Track in knowledge base insights
