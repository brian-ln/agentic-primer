# Knowledge Base for @copilot

This directory contains organizational knowledge that informs @copilot's decision-making when processing issues. The knowledge base serves as the agent's "experience database" for patterns, architectural decisions, and lessons learned.

## Structure

```
docs/knowledge/
├── README.md                           # This file
├── patterns/                           # Reusable solutions and templates
│   └── api-design.md                  # RESTful API design conventions
├── decisions/                          # Architectural Decision Records (ADRs)
│   └── workflow-architecture.md       # Why we chose this automation approach
└── insights/                           # Lessons learned and best practices
    └── automation-learnings.md        # Automation successes and failures
```

## How @copilot Uses This Knowledge Base

When processing an issue labeled `copilot-task`:

1. **Scan Phase**: Workflow counts available patterns, decisions, and insights
2. **Context Phase**: Counts passed to @copilot agent for issue analysis
3. **Decision Phase**: Agent references relevant files when generating implementation
4. **Documentation Phase**: PR includes summary of knowledge applied
5. **Learning Phase**: After merge, team can add new patterns/decisions/insights

## Document Types

### Patterns (`patterns/`)
**Purpose**: Reusable solutions and best practices

**Format**:
```markdown
# Pattern Name

## Problem
What problem does this solve?

## Solution
How to implement it

## Example
Code or configuration example

## When to Use
When is this pattern applicable?

## Related Patterns
Links to related patterns
```

**Examples**: API design, testing strategies, deployment procedures, error handling

### Decisions (`decisions/`)
**Purpose**: Record architectural choices and tradeoffs

**Format**: Architecture Decision Records (ADRs)
```markdown
# Decision Title

## Status
Accepted / Proposed / Deprecated

## Context
What factors led to this decision?

## Decision
What was decided and why?

## Consequences
What are the tradeoffs?

## Alternatives Considered
What other options were evaluated?

## Related Decisions
Links to related decisions
```

**Examples**: Technology choices (GitHub Actions vs webhooks), architectural patterns, integration approaches

### Insights (`insights/`)
**Purpose**: Lessons learned from real experience

**Format**:
```markdown
# Insight Title

## What We Learned
What did we discover?

## How We Learned It
What situation or issue led to this insight?

## Application
How should this inform future decisions?

## Examples
Real examples of when this applied

## Counter-Examples
When this insight does NOT apply

## Related Insights
Links to related insights
```

**Examples**: Performance lessons, security findings, workflow optimizations, team processes

## Adding to the Knowledge Base

When you want to contribute:

1. **Determine the type**: Is this a pattern (how-to), decision (why), or insight (lesson)?
2. **Create the file**: Follow the template above
3. **Use clear examples**: Include code snippets, config examples, or concrete cases
4. **Link to related items**: Reference other patterns/decisions/insights
5. **Keep it current**: Mark decisions as deprecated if they change; update insights with new findings
6. **Make it searchable**: Use clear headings and keywords that @copilot will recognize

## Searching the Knowledge Base

### Manual Search
```bash
# Find all patterns
find docs/knowledge/patterns -name "*.md" -type f

# Search for keyword in all documents
grep -r "keyword" docs/knowledge/

# View specific decision
cat docs/knowledge/decisions/workflow-architecture.md
```

### Within @copilot Workflow
The workflow automatically:
1. Counts files in each directory
2. Passes counts to @copilot context
3. References files during implementation
4. Documents knowledge used in PR summary

## Evolution Over Time

As your team grows and learns:

- **Month 1-2**: Seed with core patterns (API design, testing, deployment)
- **Month 3-6**: Add architectural decisions as system matures
- **Month 6+**: Extract insights from completed projects
- **Ongoing**: Deprecate old patterns, update decisions as practices evolve

## Integration with @copilot Workflow

The GitHub Actions workflow in `.github/workflows/copilot-issue-driven.yml`:

1. **Scan step** (`Scan knowledge base`):
   ```bash
   find docs/knowledge/patterns -name "*.md" 2>/dev/null | wc -l
   find docs/knowledge/decisions -name "*.md" 2>/dev/null | wc -l
   find docs/knowledge/insights -name "*.md" 2>/dev/null | wc -l
   ```

2. **Output step** (passes to @copilot):
   ```yaml
   Knowledge Base Context:
     - Patterns available: $PATTERNS
     - Decisions available: $DECISIONS
     - Insights available: $INSIGHTS
   ```

3. **Documentation step** (includes in PR):
   - Lists what knowledge was available
   - Helps reviewers understand context
   - Creates audit trail of knowledge usage

## Best Practices

### Writing Effective Patterns
- ✓ Include working code examples
- ✓ Explain when to use and when not to
- ✓ Link to related patterns
- ✗ Don't make assumptions about environment

### Writing Effective Decisions
- ✓ Explain the context that led to the decision
- ✓ Document tradeoffs explicitly
- ✓ Make it easy to understand why this was chosen
- ✗ Don't hide alternatives considered
- ✗ Don't assume future readers know the history

### Writing Effective Insights
- ✓ Ground in real experience
- ✓ Provide specific examples
- ✓ Include counter-examples (when insight doesn't apply)
- ✓ Update as understanding evolves
- ✗ Don't present hypotheticals as lessons
- ✗ Don't make universal claims from limited data

## Examples in This Repository

### Pattern: API Design (`api-design.md`)
Covers how to design RESTful endpoints, versioning, error responses, and documentation standards that @copilot should follow when creating APIs.

### Decision: Workflow Architecture (`workflow-architecture.md`)
Explains why this system uses GitHub Actions + GitHub Script (not webhooks, serverless, etc.) and the tradeoffs made.

### Insight: Automation Learnings (`automation-learnings.md`)
Documents empirical findings about what makes automation reliable: idempotency, proper logging, error handling patterns, etc.

## Maintenance

- **Review quarterly**: Check if patterns are still current
- **Archive deprecated**: Move old patterns to `archive/` with explanation
- **Version with code**: Keep knowledge base in sync with actual implementation
- **Measure usage**: Track which patterns @copilot references most often

## Questions?

If you have questions about:
- **How to contribute**: See "Adding to the Knowledge Base" section above
- **Specific patterns/decisions**: Check individual files
- **@copilot's decision-making**: Review the workflow logs in GitHub Actions
- **Knowledge base search**: Ask in team channels or create an issue

Remember: The knowledge base is the team's collective memory. Invest in it, and @copilot will make better decisions.
