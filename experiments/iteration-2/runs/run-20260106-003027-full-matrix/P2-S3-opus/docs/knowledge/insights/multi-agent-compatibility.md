# Multi-Agent Compatibility

## Summary

Guidelines and observations for ensuring @copilot automation works effectively across Opus, Sonnet, and Haiku AI agents.

## Observation

Different AI agents have varying capabilities and response patterns. A system designed for one agent may not work optimally with others. The issue-driven development workflow must accommodate these differences.

## Context

This insight emerged during initial system design when considering the success criterion: "Multi-Agent: Works with >=3 different AI agents (Opus, Sonnet, Haiku)".

## Agent Characteristics

### Claude Opus 4.5

**Strengths:**
- Complex reasoning and multi-step tasks
- Nuanced understanding of context
- Code quality and architecture decisions
- Long-form documentation

**Best For:**
- Architectural changes
- Complex refactoring
- Multi-file implementations
- Detailed code reviews

**Considerations:**
- Higher latency per request
- Best used for high-value tasks

### Claude Sonnet

**Strengths:**
- Balanced speed and capability
- Good code generation
- Efficient for most tasks
- Reliable output quality

**Best For:**
- Standard feature implementation
- Bug fixes with clear scope
- Test writing
- Documentation updates

**Considerations:**
- Default choice for typical tasks
- Good cost/performance ratio

### Claude Haiku

**Strengths:**
- Fast response times
- Efficient for simple tasks
- Low resource usage
- High throughput

**Best For:**
- Simple code changes
- Template-based generation
- Quick fixes
- Repetitive tasks

**Considerations:**
- May struggle with complex logic
- Best for well-defined, narrow tasks

## Compatibility Strategies

### 1. Structured Input

Use explicit, parseable formats instead of prose:

```yaml
# Good: Structured
## Task Description
Create a function that validates email addresses.

## Acceptance Criteria
- [ ] Function validateEmail(email: string): boolean
- [ ] Returns true for valid emails
- [ ] Returns false for invalid emails
- [ ] Unit tests with edge cases

# Avoid: Vague prose
Please add email validation somewhere in the auth flow
```

### 2. Explicit Acceptance Criteria

Checkboxes provide clear, verifiable goals:

```markdown
## Acceptance Criteria
- [ ] Function exists and is exported
- [ ] TypeScript types are correct
- [ ] Unit tests pass
- [ ] Edge cases handled
- [ ] Documentation added
```

### 3. Context Limiting

Provide focused context to avoid overwhelming simpler agents:

```markdown
## Context
Related file: src/utils/validation.ts
Pattern to follow: See existing validatePhone function
Constraint: Must be backwards compatible
```

### 4. Error Handling Hints

Suggest how to handle blockers:

```markdown
## If Blocked
- Comment on issue explaining the blocker
- Provide partial solution if possible
- Suggest alternative approaches
```

## Issue Template Design

The issue templates are designed to work across all agents:

1. **Required Fields** - Task description, acceptance criteria
2. **Optional Fields** - Context, priority, agent preference
3. **Validation** - Pre-submission checklist

## Workflow Adaptations

### For Complex Tasks (Opus-preferred)

```yaml
labels: ["copilot", "complex", "opus-preferred"]
```

The workflow can route to specific agents based on labels.

### For Simple Tasks (Haiku-acceptable)

```yaml
labels: ["copilot", "simple", "quick-fix"]
```

### For Standard Tasks (Sonnet-default)

```yaml
labels: ["copilot", "ai-task"]
```

## Performance Expectations

| Agent | Avg. Execution | Success Rate | Best Use |
|-------|----------------|--------------|----------|
| Opus | 60-120s | 95%+ | Complex |
| Sonnet | 30-60s | 90%+ | Standard |
| Haiku | 10-30s | 85%+ | Simple |

*Note: These are estimates and may vary based on task complexity.*

## Implications

1. **Template Design**: Keep templates structured and explicit
2. **Task Routing**: Consider agent-appropriate task assignment
3. **Fallback Strategy**: If one agent fails, retry with another
4. **Logging**: Track agent-specific metrics for improvement

## Recommendations

1. **Default to Sonnet** for unknown complexity
2. **Use Opus** for architectural or multi-file changes
3. **Use Haiku** for templated or repetitive tasks
4. **Monitor metrics** to refine agent selection
5. **Update insights** as agent capabilities evolve

## Evidence

- Initial system design analysis
- Agent capability documentation
- Success criteria requirement: multi-agent support

## Metadata

- **Created**: 2026-01-06
- **Author**: @copilot
- **Status**: Initial observations (to be updated with execution data)
