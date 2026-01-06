# Knowledge Base

This directory contains git-tracked knowledge accumulated through the use of this automation system.

## Purpose

The knowledge base serves as a persistent memory for the system, capturing:
- Patterns that work well
- Decisions and their rationale
- Insights discovered during execution

All knowledge is version-controlled alongside the code, making it easy to track evolution and share across the team.

## Structure

### patterns/
Reusable patterns and best practices discovered through automation.

**What to store:**
- Successful implementation patterns
- Common solutions to recurring problems
- Code templates and snippets
- Workflow optimizations

**Example:**
```
patterns/error-handling-pattern.md
patterns/testing-strategy.md
```

### decisions/
Architecture Decision Records (ADRs) documenting important choices.

**What to store:**
- Why certain technologies were chosen
- Trade-offs considered
- Context at time of decision
- Consequences and outcomes

**Format:** Follow ADR format (Context, Decision, Consequences)

**Example:**
```
decisions/001-use-github-actions.md
decisions/002-choice-of-ai-agent.md
```

### insights/
Learnings, observations, and discoveries made during execution.

**What to store:**
- Unexpected behaviors
- Performance observations
- Agent-specific quirks
- Improvement opportunities

**Example:**
```
insights/claude-prefers-explicit-paths.md
insights/workflow-trigger-timing.md
```

## Contributing

### Adding a Pattern

1. Create a new markdown file in `patterns/`
2. Use descriptive filename: `pattern-name.md`
3. Include:
   - Problem statement
   - Solution
   - Example usage
   - When to apply/avoid

### Recording a Decision

1. Create numbered file in `decisions/`: `NNN-decision-title.md`
2. Follow ADR format:
   - **Status**: Accepted/Deprecated/Superseded
   - **Context**: What led to this decision
   - **Decision**: What was decided
   - **Consequences**: Impact and trade-offs

### Capturing an Insight

1. Create file in `insights/` with clear name
2. Include:
   - Observation
   - Context when discovered
   - Implications
   - Related issues/PRs if applicable

## Usage by AI Agents

AI agents can read this knowledge base to:
- Learn from past patterns before implementing new features
- Understand why certain decisions were made
- Avoid repeating past mistakes
- Apply proven solutions to similar problems

The knowledge base grows over time, making the system smarter with each execution.

## Maintenance

- Review and update patterns as better approaches emerge
- Mark decisions as "Superseded" when replaced
- Archive outdated insights
- Keep content concise and actionable
