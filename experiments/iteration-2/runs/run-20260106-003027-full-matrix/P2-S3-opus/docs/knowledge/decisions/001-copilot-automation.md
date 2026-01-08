# ADR-001: Copilot Automation Architecture

## Status

Accepted

## Date

2026-01-06

## Context

We need to enable issue-driven development where @copilot can autonomously:

1. Process GitHub issues assigned to it
2. Create branches and implement solutions
3. Open pull requests with proper structure
4. Auto-assign PRs to repository owners for review
5. Maintain a knowledge base that grows over time
6. Generate self-improvement suggestions from execution logs

The solution must:
- Work with multiple AI agents (Opus, Sonnet, Haiku)
- Bootstrap from a bare repository with a single command
- Achieve 90%+ reliability across executions
- Support self-improvement through log analysis

## Decision

We will implement a GitHub-native automation system using:

### 1. GitHub Actions for Workflow Automation

Three workflows handle the core automation:

- **copilot-issue-handler.yml**: Triggers on issue creation/labeling, creates branches, assigns @copilot, and initiates PRs
- **copilot-pr-review.yml**: Auto-reviews PRs created by @copilot
- **self-improvement-analyzer.yml**: Weekly analysis of execution logs to generate improvement issues

### 2. CODEOWNERS for PR Assignment

```
* @owner
```

Native GitHub mechanism for automatic review assignment without additional tooling.

### 3. Issue Templates for Structured Input

YAML-based templates ensure @copilot receives parseable task descriptions:
- `copilot-task.yml`: Primary task template
- `self-improvement.yml`: Generated improvement suggestions

### 4. Knowledge Base Structure

```
docs/knowledge/
|-- patterns/   # Reusable implementation patterns
|-- decisions/  # ADRs like this one
+-- insights/   # Agent-specific learnings
```

### 5. Execution Logging

JSON logs in `logs/executions/` enable:
- Success rate tracking
- Duration analysis
- Error pattern detection
- Self-improvement input

### 6. Bootstrap Script

Single `scripts/bootstrap.sh` that:
- Creates directory structure
- Generates all configuration files
- Validates syntax
- Runs initial tests

## Consequences

### Positive

1. **Zero External Dependencies**: Everything runs on GitHub infrastructure
2. **Multi-Agent Compatible**: Same templates work for all agent types
3. **Observable**: All actions logged for debugging and improvement
4. **Self-Improving**: Analyzer workflow generates improvement issues
5. **Single-Command Setup**: Bootstrap enables rapid deployment

### Negative

1. **GitHub Lock-in**: Tightly coupled to GitHub Actions and APIs
2. **Rate Limits**: High activity may hit GitHub API limits
3. **Stateless Workflows**: Each workflow run is independent
4. **Template Rigidity**: Structured templates may be constraining

### Neutral

1. **Learning Curve**: Users must adopt issue template format
2. **Maintenance**: Workflows require periodic updates
3. **Customization**: Patterns may need adaptation per project

## Alternatives Considered

### External CI/CD (Jenkins, CircleCI)

- **Pros**: More compute power, custom runtimes
- **Cons**: Additional infrastructure, complexity
- **Decision**: Rejected - GitHub Actions sufficient

### Webhook-based System

- **Pros**: Real-time processing, flexible handling
- **Cons**: Requires hosting, maintenance burden
- **Decision**: Rejected - Adds operational complexity

### Manual Assignment Flow

- **Pros**: Simple to understand
- **Cons**: Defeats automation purpose
- **Decision**: Rejected - Core requirement is automation

### Database-backed State

- **Pros**: Rich querying, persistent state
- **Cons**: Infrastructure overhead
- **Decision**: Rejected - File-based logs sufficient

## Related

- [Issue-to-PR Workflow Pattern](../patterns/issue-to-pr-workflow.md)
- [Multi-Agent Compatibility Insights](../insights/multi-agent-compatibility.md)

## Metadata

- **Author**: @copilot
- **Reviewed By**: (pending)
- **Implementation PR**: (pending)
