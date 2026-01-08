# ADR-001: Use GitHub Native Features

## Status

Accepted

## Context

When building an issue-driven development system for AI agents, we need to decide between:

1. Using GitHub's native features (Issues, Actions, CODEOWNERS)
2. Building custom tooling with external services
3. A hybrid approach

Key considerations:
- Reliability and maintainability
- Ease of adoption
- Integration with existing workflows
- Support for multiple AI agents

## Decision

We will use GitHub native features wherever possible:

- **Issue Templates** instead of external forms
- **GitHub Actions** instead of external CI/CD
- **CODEOWNERS** instead of custom assignment logic
- **Labels** instead of external tracking
- **Markdown files** instead of external knowledge bases

## Consequences

### Positive

- **Zero external dependencies**: Everything runs within GitHub
- **Native integration**: Works with existing GitHub tools and workflows
- **Familiar UX**: Users already know GitHub's interface
- **Automatic updates**: GitHub maintains the infrastructure
- **Free for public repos**: No additional cost

### Negative

- **Limited customization**: Bound by GitHub's feature set
- **Vendor lock-in**: Harder to migrate to other platforms
- **Rate limits**: GitHub API has rate limits that may affect high-volume usage
- **Feature gaps**: Some advanced features may require workarounds

### Neutral

- **Learning curve**: Users need to understand GitHub's features
- **Documentation**: Need to document GitHub-specific workflows

## Alternatives Considered

### External Orchestration (Rejected)

Using tools like n8n, Zapier, or custom webhooks:
- More flexible but adds complexity
- External dependencies reduce reliability
- Additional cost and maintenance

### Hybrid Approach (Partially Adopted)

Using GitHub for core features with external knowledge base:
- Considered using separate wiki or docs site
- Rejected because Markdown in repo is simpler
- Knowledge base in `/docs/knowledge/` provides versioning

### Custom Bot (Rejected)

Building a custom GitHub bot application:
- Maximum flexibility
- High maintenance burden
- Requires hosting and security management
- Overkill for the stated requirements

## References

- GitHub Issue Templates: https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests
- GitHub Actions: https://docs.github.com/en/actions
- CODEOWNERS: https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners
