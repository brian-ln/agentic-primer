# Pattern Library

Catalog of reusable patterns discovered through PR analysis and manual documentation.

## Purpose

This library captures proven solutions to common problems, enabling @copilot to:
- Avoid reinventing solutions
- Apply best practices consistently
- Learn from past successes

## Index

### GitHub Actions Patterns
- [Workflow Patterns](github-actions.md) - CI/CD workflow design patterns

### Scripting Patterns
- [Shell Scripting](scripting.md) - Bash scripting best practices
- [Error Handling](error-handling.md) - Robust error handling strategies

### Code Patterns
- [Testing Strategies](testing.md) - Test design and implementation patterns

### Configuration Patterns
- [YAML Best Practices](#) - Configuration file patterns
- [Environment Management](#) - Environment variable handling

## Pattern Format

Each pattern document follows this structure:

```markdown
# Pattern Name

## Problem

Clear description of the problem this pattern solves.

## Solution

Step-by-step solution with code examples.

## Example

Real-world usage example.

## Considerations

- When to use this pattern
- When NOT to use this pattern
- Trade-offs and alternatives

## Related Patterns

- Links to related patterns
- Links to decisions that led to this pattern
```

## Contributing New Patterns

### When to Create a Pattern

Create a pattern when you've:
1. Solved a problem that will likely recur
2. Used the same solution in 3+ places
3. Found a best practice worth codifying
4. Discovered a non-obvious solution

### Pattern Checklist

- [ ] Problem clearly stated
- [ ] Solution is reusable (not project-specific)
- [ ] Code examples included
- [ ] Trade-offs documented
- [ ] Related patterns linked
- [ ] Added to this index

## Pattern Categories

### 1. GitHub Actions Workflows

Patterns for CI/CD automation:
- Issue-triggered workflows
- PR validation pipelines
- Scheduled jobs
- Matrix builds
- Workflow composition

### 2. Shell Scripting

Patterns for robust shell scripts:
- Error handling (`set -euo pipefail`)
- Logging and output formatting
- Argument parsing
- Idempotent operations
- Cross-platform compatibility

### 3. Error Handling

Patterns for graceful failure:
- Retry with exponential backoff
- Circuit breakers
- Graceful degradation
- Error reporting and alerting

### 4. Testing

Patterns for comprehensive testing:
- Unit testing strategies
- Integration test design
- End-to-end test orchestration
- Test data management
- Mocking and stubbing

### 5. Configuration Management

Patterns for configuration:
- Environment-specific configs
- Secret management
- Feature flags
- Validation of config files

## Pattern Evolution

Patterns evolve through:

1. **Auto-Generation**: New patterns extracted from merged PRs
2. **Manual Addition**: Engineers document novel solutions
3. **Refinement**: Patterns improved based on usage feedback
4. **Deprecation**: Outdated patterns marked and archived

## Usage Metrics

Track pattern effectiveness:

| Pattern | Times Used | Last Used | Success Rate |
|---------|-----------|-----------|--------------|
| TBD | - | - | - |

## Pattern Lifecycle

```
Proposed → Accepted → Mature → Deprecated
```

- **Proposed**: New pattern, under evaluation
- **Accepted**: Proven in 3+ cases, recommended for use
- **Mature**: Battle-tested, default choice for this problem
- **Deprecated**: Superseded by better pattern, avoid for new code

## Anti-Patterns

Document what NOT to do:

1. **Over-Complication**: Adding complexity without clear benefit
2. **Premature Optimization**: Optimizing before measuring
3. **Copy-Paste Programming**: Duplicating code instead of abstracting
4. **Silent Failures**: Ignoring errors without handling
5. **Magic Numbers**: Hard-coding values without explanation

## Search Tips

**By technology**:
```bash
rg "GitHub Actions" docs/knowledge/patterns/
```

**By problem domain**:
```bash
rg "error handling" docs/knowledge/patterns/
```

**Recently added**:
```bash
ls -lt docs/knowledge/patterns/*.md | head -5
```

## Future Enhancements

1. **Pattern Templates**: Scaffolding for new patterns
2. **Pattern Validation**: Automated checks for completeness
3. **Usage Tracking**: Measure which patterns are most valuable
4. **Visual Diagrams**: Architecture diagrams for complex patterns
5. **Pattern Language**: Define relationships between patterns

---

**Last Updated**: January 8, 2026
**Status**: Active
**Maintainer**: @copilot + human reviewers
