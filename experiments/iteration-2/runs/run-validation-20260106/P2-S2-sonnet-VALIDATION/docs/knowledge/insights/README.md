# Insights

This directory contains empirical learnings, best practices, and lessons learned from working on this project.

## What are Insights?

Insights are knowledge gained through experience. They capture:
- **Lessons learned:** What we discovered by doing
- **Best practices:** What works well in practice
- **Pitfalls to avoid:** Common mistakes and how to prevent them
- **Performance optimizations:** What we learned about speed and efficiency
- **Workflow improvements:** Better ways to work together

Unlike patterns (which are formal solutions) and decisions (which explain choices), insights are **experiential knowledge** from the trenches.

## Insight Structure

Each insight document should include:

1. **Title:** Clear, actionable title
2. **Context:** What situation does this apply to?
3. **The Insight:** What did we learn?
4. **Evidence:** How do we know this is true?
5. **Action Items:** What should you do with this knowledge?
6. **Related Content:** Links to patterns, decisions, or other insights

## Example Insights

- **Copilot Best Practices:** How to write effective tasks for @copilot
- **Deployment Lessons:** What we learned from production incidents
- **Testing Strategies:** Approaches that improved quality and speed
- **Performance Optimizations:** Specific changes that improved latency
- **Code Review Learnings:** What makes reviews effective vs wasteful

## How to Use Insights

### For Developers

1. **Browse before starting:** Check for insights related to your task
2. **Learn from others:** Avoid mistakes others already made
3. **Apply best practices:** Use proven approaches
4. **Contribute your learnings:** Share what you discover

### For @copilot

Reference insights in issue descriptions:

```markdown
Follow best practices from:
- docs/knowledge/insights/copilot-best-practices.md
- docs/knowledge/insights/api-performance-tips.md
```

@copilot will incorporate these learnings into implementation.

## Contributing an Insight

### When to Create an Insight

Create an insight when:
- **You learned something valuable** that others should know
- **You solved a tricky problem** that isn't obvious
- **You found a better way** to do something
- **You made a mistake** that others can avoid
- **You optimized something** with measurable results

### Insight Template

```markdown
# Insight Title

## Context

What situation does this insight apply to?

## The Insight

What did you learn? State it clearly and concisely.

## Evidence

How do we know this is true?

- Metrics (before/after)
- Specific examples
- Test results
- Production data

## Action Items

What should developers do with this knowledge?

1. Action one
2. Action two
3. Action three

## Examples

### Good Example

\`\`\`javascript
// Code showing the right approach
\`\`\`

### Bad Example (Anti-pattern)

\`\`\`javascript
// Code showing what to avoid
\`\`\`

## Related Content

- Patterns: Link to relevant patterns
- Decisions: Link to relevant ADRs
- Insights: Link to related insights
```

### Contribution Process

1. Create insight file: `insights/your-insight-name.md`
2. Follow the template above
3. Provide specific evidence (metrics, examples)
4. Make action items concrete and actionable
5. Submit PR with clear description

## Insight Categories

We organize insights by domain:

- **Development:** Coding practices, debugging, testing
- **Performance:** Speed, efficiency, resource usage
- **Security:** Vulnerabilities discovered, security improvements
- **Operations:** Deployment, monitoring, incident response
- **Workflow:** Team processes, collaboration, tools

## Quality Guidelines

### Good Insights

- **Specific:** "Reduce API response time by caching user sessions" not "Make things faster"
- **Evidenced:** Include metrics, examples, or data
- **Actionable:** Clear steps developers can take
- **Honest:** Include context about when it doesn't apply

### Avoid

- **Obvious statements:** "Write tests" without specific guidance
- **Opinion without evidence:** "Framework X is better" without data
- **One-time fixes:** Very specific to a unique situation
- **Rants:** Complaints without constructive solutions

## Maintenance

- **Keep current:** Update insights as practices evolve
- **Retire outdated:** Move obsolete insights to archive
- **Consolidate duplicates:** Merge similar insights
- **Link from code:** Reference insights in code comments

## Using Insights with @copilot

### In Issue Descriptions

```markdown
Task: Optimize database queries for user dashboard

Insights to apply:
- docs/knowledge/insights/database-performance.md
- docs/knowledge/insights/caching-strategies.md
```

### In PR Reviews

When reviewing @copilot PRs, check if insights are being followed:
- Did it apply the relevant best practices?
- Should we create a new insight from this PR?
- Do we need to update existing insights?

## Resources

- [Failure is an Option (SRE)](https://sre.google/sre-book/postmortem-culture/)
- [Lessons Learned in Software Testing](https://www.satisfice.com/blog/archives/1928)
- [Incident Review Best Practices](https://www.atlassian.com/incident-management/postmortem)
