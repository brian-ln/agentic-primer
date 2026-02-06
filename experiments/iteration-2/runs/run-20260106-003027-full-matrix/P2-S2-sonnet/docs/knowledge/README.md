# Knowledge Base

This directory contains the knowledge base for the Copilot agent. The agent consults this knowledge before processing issues to ensure consistency with established patterns, decisions, and learnings.

## Structure

The knowledge base is organized into three categories:

### 📐 Patterns (`patterns/`)

**What**: Reusable solutions to recurring problems.

**When to add**:
- You've solved a problem that appears frequently
- You've created a code structure worth replicating
- You've established a best practice

**Examples**:
- Authentication flow pattern
- Error handling pattern
- API client structure
- Database migration pattern

**Format**: See `patterns/README.md`

### 🎯 Decisions (`decisions/`)

**What**: Architecture Decision Records (ADRs) documenting important technical choices.

**When to add**:
- You've made a significant architectural decision
- You've chosen between multiple technology options
- You've evaluated trade-offs and need to document rationale

**Examples**:
- Choosing React over Vue
- Selecting PostgreSQL database
- Deciding on monorepo vs multi-repo
- API design: REST vs GraphQL

**Format**: See `decisions/README.md`

### 💡 Insights (`insights/`)

**What**: Lessons learned, gotchas, performance tips, and unexpected behaviors.

**When to add**:
- You've discovered unexpected behavior
- You've found a performance optimization
- You've encountered a subtle bug or limitation
- You wish you'd known something earlier

**Examples**:
- "Database connection pooling reduced load by 80%"
- "Library X has a memory leak with setting Y"
- "Feature Z doesn't work in Safari < 14"
- "Always await cleanup in tests or they'll be flaky"

**Format**: See `insights/README.md`

## Usage by Copilot Agent

When processing an issue, the Copilot agent:

1. **Reads** all knowledge files in relevant categories
2. **Summarizes** applicable patterns, decisions, and insights
3. **Applies** this knowledge to the implementation
4. **References** knowledge in PR description
5. **Updates** knowledge base if new learnings emerge

## Contributing to Knowledge Base

### Adding New Knowledge

1. Identify which category fits best (pattern, decision, or insight)
2. Use the README template in that category
3. Fill in all sections completely
4. Use descriptive filename: `{number}-{short-description}.md`
5. Create PR with your knowledge addition
6. Link to relevant code/issues

### Maintaining Knowledge

- **Review quarterly**: Check if knowledge is still current
- **Archive outdated**: Move obsolete knowledge to `archive/`
- **Update when needed**: Keep knowledge accurate as project evolves
- **Link related items**: Cross-reference patterns, decisions, insights

### Naming Conventions

**Patterns**: `{number}-{pattern-name}.md`
- Example: `001-auth-pattern.md`

**Decisions**: `{number}-{decision-topic}.md`
- Example: `001-database-choice.md`

**Insights**: `{date}-{insight-topic}.md`
- Example: `2026-01-08-api-rate-limiting.md`

## Benefits of Knowledge Base

1. **Consistency**: New code follows established patterns
2. **Onboarding**: New team members learn from documented decisions
3. **Context**: Future maintainers understand why choices were made
4. **Efficiency**: Don't reinvent solutions to solved problems
5. **Quality**: Learn from past mistakes and successes

## Knowledge Base Metrics

Track these metrics to measure knowledge base health:

- **Growth rate**: New entries added per month
- **Reference rate**: How often knowledge is referenced in PRs
- **Accuracy**: Percentage of knowledge still current
- **Coverage**: Areas with good vs sparse documentation

## Tips for Effective Knowledge

1. **Be specific**: Concrete examples better than abstract descriptions
2. **Include context**: Explain the "why" not just the "what"
3. **Keep updated**: Outdated knowledge is worse than no knowledge
4. **Link generously**: Reference related code, issues, PRs
5. **Write for future you**: Assume you'll forget why you did this

## Getting Help

- Questions about knowledge format? See category READMEs
- Unsure which category? Patterns = solutions, Decisions = choices, Insights = learnings
- Need examples? Look at existing knowledge in each category
- Want to improve knowledge base? Open an issue with suggestions

---

**Last Updated**: 2026-01-08
**Maintainer**: Copilot Agent
**Version**: 1.0.0
