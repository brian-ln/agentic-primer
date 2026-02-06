# Knowledge Base

This directory serves as the institutional memory for this project, capturing patterns, decisions, and insights discovered during development.

## Purpose

The knowledge base provides:
- **Version-controlled documentation** of architectural decisions
- **Searchable patterns** for common problems and solutions
- **Lessons learned** from retrospectives and post-mortems
- **Context preservation** that survives team changes

## Structure

```
knowledge/
├── patterns/       # Reusable code patterns and best practices
├── decisions/      # Architecture Decision Records (ADRs)
└── insights/       # Lessons learned and retrospective findings
```

### Patterns

**What:** Proven, reusable solutions to recurring problems in this codebase

**When to add:**
- You've solved the same problem 2+ times
- Solution is specific to this project (not generic advice)
- Pattern has been validated in production

**Examples:**
- Error handling patterns
- Database query optimization techniques
- API response formatting standards
- Test fixture patterns

See [patterns/README.md](patterns/README.md) for contribution guide.

### Decisions

**What:** Architecture Decision Records (ADRs) documenting significant choices

**When to add:**
- You make a non-trivial architectural decision
- Multiple viable alternatives existed
- Future team members will ask "why did we do it this way?"

**Examples:**
- Why we chose PostgreSQL over MongoDB
- Why we use JWT instead of session cookies
- Why we structured the API as REST vs GraphQL
- Why we adopted TypeScript

See [decisions/README.md](decisions/README.md) for ADR template.

### Insights

**What:** Experiential learnings from retrospectives, incidents, and "aha!" moments

**When to add:**
- After retrospectives or post-mortems
- When you discover something you wish you'd known earlier
- When you solve a non-obvious problem
- When you identify process improvements

**Examples:**
- "Database connection pooling prevented production outage"
- "Code review checklist reduced bug escape rate by 40%"
- "Feature flags enabled safer deployments"
- "Pair programming accelerated onboarding"

See [insights/README.md](insights/README.md) for contribution guide.

## Usage

### For Humans

**Search:** Use GitHub search to find relevant knowledge:
```
repo:owner/repo path:docs/knowledge <search terms>
```

**Browse:** Navigate directories in GitHub web UI - all markdown renders automatically

**Contribute:** Create new markdown files following the templates in each directory's README

### For AI Agents

This knowledge base is designed for both human and AI consumption:

- **@copilot tasks:** Agent reads knowledge base before implementation to understand project patterns
- **Auto-review:** Scripts reference patterns and decisions during PR validation
- **Self-improvement:** Agents contribute insights back to knowledge base after task completion

## Contribution Guidelines

### Quality Over Quantity

- **Be specific:** "In our Express API, we handle authentication errors by..." not "Handle errors properly"
- **Include evidence:** Link to commits, PRs, or metrics that support the pattern/decision/insight
- **Update stale content:** Mark outdated patterns as deprecated rather than deleting them

### Lifecycle Management

Each knowledge category has lifecycle states:

- **Patterns:** Active → Deprecated → Superseded
- **Decisions:** Proposed → Accepted → Superseded
- **Insights:** Fresh → Validated → Resolved

See individual category READMEs for lifecycle details.

### Naming Conventions

**Patterns:** Descriptive name of the pattern
```
database-connection-retry.md
api-error-response-format.md
test-fixture-builder.md
```

**Decisions:** Sequential numbering with descriptive slug
```
001-database-selection.md
002-authentication-strategy.md
003-deployment-platform.md
```

**Insights:** Date-based with topic
```
2026-01-06-feature-flag-rollout.md
2025-12-15-code-review-improvements.md
2025-11-20-performance-optimization.md
```

## Integration with @copilot Workflow

The @copilot agent automatically:

1. **Reads knowledge base** before implementing tasks (understands context)
2. **References patterns** in implementation (consistency)
3. **Creates ADRs** for significant architectural choices (documentation)
4. **Contributes insights** after task completion (learning)

This creates a feedback loop: knowledge informs implementation, implementation creates new knowledge.

## Getting Started

1. **Explore existing knowledge:** Read through patterns, decisions, and insights
2. **Reference during work:** Check knowledge base before solving problems
3. **Contribute liberally:** When you learn something valuable, document it
4. **Keep it current:** Update or deprecate outdated information

The knowledge base is only valuable if it's actively maintained and trusted.

---

**Last Updated:** 2026-01-06
**Maintained By:** @copilot agent + human contributors
