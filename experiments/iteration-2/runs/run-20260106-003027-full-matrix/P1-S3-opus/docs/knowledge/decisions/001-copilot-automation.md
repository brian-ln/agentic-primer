# ADR-001: Copilot Automation System

## Status

Accepted

## Context

We need to automate issue-driven development to enable:
- Faster task completion
- Consistent quality
- Reduced manual overhead
- Learning from experience

The system should work with multiple AI agents (GitHub Copilot, Claude Opus, Claude Sonnet, Claude Haiku) and bootstrap from a bare repository with minimal manual setup.

## Decision

Implement a GitHub Actions-based automation system with the following components:

1. **Issue Template** - Structured input via `copilot-task.yml`
2. **Issue Workflow** - Automatic validation and labeling
3. **PR Auto-Review** - Syntax validation and test execution
4. **Self-Improvement** - Daily log analysis and improvement PRs
5. **Knowledge Base** - Patterns, decisions, and insights storage

### Architecture

```
Issue → Workflow → Agent → PR → Review → Merge → Knowledge Base
                                  ↓
                           Self-Improvement
```

### Key Design Choices

1. **Label-based routing** - Simple, visible, debuggable
2. **JSONL logs** - Append-only, easy to analyze
3. **Markdown knowledge base** - Human and agent readable
4. **Daily improvement cycle** - Balanced frequency

## Consequences

### Positive

- **Consistency** - All issues processed the same way
- **Visibility** - Labels and comments show status
- **Learning** - Knowledge base grows over time
- **Multi-agent** - Works with any agent that can read/write GitHub
- **Self-improving** - System gets better automatically

### Negative

- **Complexity** - Multiple workflows to maintain
- **GitHub dependency** - Tightly coupled to GitHub Actions
- **Storage** - Logs grow over time (need rotation)
- **Bootstrap time** - Initial setup takes ~10 minutes

### Neutral

- **Human review required** - Keeps human in loop
- **Label management** - New labels to maintain
- **Metrics** - Need to define what success looks like

## Alternatives Considered

### 1. External Orchestration (Rejected)

Using external tool (Zapier, n8n) for orchestration.

**Why rejected:** Adds external dependency, more complex setup.

### 2. Single Workflow (Rejected)

One monolithic workflow for all automation.

**Why rejected:** Too complex, hard to maintain, slower execution.

### 3. Database-backed State (Rejected)

Using database for issue state instead of labels.

**Why rejected:** Adds infrastructure, labels are simpler and visible.

### 4. Real-time Improvement (Rejected)

Analyzing and improving after every issue.

**Why rejected:** Too noisy, daily batch is sufficient.

## Related

- [Issue Workflow Pattern](../patterns/issue-workflow.md)
- [Workflow Improvements Pattern](../patterns/workflow-improvements.md)
- Issue: Initial bootstrap implementation
