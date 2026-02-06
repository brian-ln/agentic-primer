# Knowledge Base Structure

This directory contains shared knowledge, patterns, and decisions that guide @copilot's work. The structure ensures consistency across all tasks and serves as a reference for implementation approaches.

## Organization

### `patterns/` - Recurring Implementation Solutions

This directory contains proven solutions for common implementation tasks. @copilot references these patterns to avoid recreating approaches from scratch.

**When to use**: Before implementing a feature, check if a pattern exists. If the approach matches the pattern, follow it. If the approach differs, create a new pattern or update the existing one.

**Pattern anatomy**:
```
# Pattern Name

## When to Use
[Conditions that apply to this pattern]

## Implementation Steps
[Step-by-step approach]

## Code Example
[Concrete code example]

## Considerations
[Edge cases, performance, trade-offs]
```

### `decisions/` - Architecture Decision Records (ADRs)

This directory contains decisions about significant technical choices, trade-offs, and constraints. ADRs prevent repeated debates and document the reasoning behind decisions.

**When to use**: When a task involves architectural implications or when making a decision that affects multiple parts of the system.

**ADR anatomy**:
```
# ADR-XXX: [Decision Title]

## Status
[Proposed | Accepted | Deprecated | Superseded by ADR-YYY]

## Context
[Why this decision was needed]

## Decision
[What was decided]

## Consequences
[Trade-offs and implications]

## Alternatives Considered
[Other options evaluated]
```

### `insights/` - Project-Specific Learnings

This directory captures domain-specific knowledge about the project, team practices, and lessons learned.

## Navigation

### Quick Start for @copilot
1. Parse the issue using `.github/ISSUE_TEMPLATE/task.yml`
2. Search `patterns/` for relevant implementation patterns
3. Check `decisions/` for architectural context
4. Review `insights/` for project-specific practices
5. Implement following patterns and decisions
6. Update patterns/decisions if new learning is discovered

### File Naming
- Patterns: `[domain]-[feature]-pattern.md` (e.g., `api-deprecation-pattern.md`)
- Decisions: `adr-XXX-[title].md` (e.g., `adr-001-monorepo-structure.md`)
- Insights: `[topic]-insights.md` (e.g., `testing-insights.md`)

## Maintenance

### When to Add a Pattern
- Solution appears in multiple issues
- Implementation has non-obvious steps
- Trade-offs should be documented
- New team members would benefit from the guidance

### When to Create an ADR
- Decision affects multiple systems
- Significant trade-offs exist
- Decision should be revisited periodically
- Team needs to understand the "why" later

### When to Add an Insight
- Lesson learned during implementation
- Project-specific practice differs from industry standard
- Useful context for future work
- Pitfall to avoid

## Examples

The knowledge base includes example patterns and decisions to demonstrate the structure. These can be extended or replaced as the project evolves.
