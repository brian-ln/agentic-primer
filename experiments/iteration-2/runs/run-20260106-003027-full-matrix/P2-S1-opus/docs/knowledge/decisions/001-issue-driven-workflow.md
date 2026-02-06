# ADR-001: Issue-Driven Workflow with @copilot

## Status

Accepted

## Date

2026-01-06

## Context

We need to establish a systematic approach for @copilot to process work requests. The goals are:

1. Structured intake of work requests
2. Automated processing by @copilot
3. Consistent ownership of resulting PRs
4. Knowledge capture from automated development

Without a defined workflow, @copilot usage becomes ad-hoc and results are inconsistent.

## Decision

Implement an issue-driven development workflow where:

1. **Issues are the source of truth** - All @copilot work originates from GitHub Issues
2. **Labels trigger automation** - The `copilot` label activates processing workflows
3. **CODEOWNERS routes reviews** - All PRs auto-assign to repository owner
4. **Knowledge base captures learnings** - Patterns, decisions, and insights documented

### Implementation

- Issue template: `.github/ISSUE_TEMPLATE/copilot-task.yml`
- Issue workflow: `.github/workflows/issue-copilot.yml`
- PR workflow: `.github/workflows/pr-auto-assign.yml`
- Review routing: `.github/CODEOWNERS`
- Knowledge base: `docs/knowledge/`

## Consequences

### Positive

- **Traceability** - Every @copilot PR links to an issue
- **Consistency** - Structured templates ensure complete task definitions
- **Ownership** - CODEOWNERS guarantees review assignment
- **Learning** - Knowledge base accumulates institutional memory
- **Auditability** - Workflow logs provide visibility

### Negative

- **Overhead** - Requires issue creation before @copilot work
- **Rigidity** - Template structure may not fit all use cases
- **Maintenance** - Workflows need updates as GitHub features evolve

## Alternatives Considered

### 1. Direct @copilot mentions (no issues)

Invoke @copilot directly in code comments or chat.

**Rejected because:**
- No structured intake
- Poor traceability
- No ownership chain

### 2. PR-first workflow

Create PRs directly, use issues only for tracking.

**Rejected because:**
- Loses issue-PR linkage
- Less context for @copilot
- Harder to track work before implementation

### 3. External task management

Use Jira, Linear, or similar tools as source of truth.

**Rejected because:**
- Adds tooling complexity
- Requires sync mechanisms
- GitHub Issues sufficient for this use case

## Related

- [Knowledge Base README](../README.md)
- Issue template: `.github/ISSUE_TEMPLATE/copilot-task.yml`
