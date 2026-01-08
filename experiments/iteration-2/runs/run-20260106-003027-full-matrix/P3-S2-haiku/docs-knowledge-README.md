# Knowledge Base

Repository of patterns, decisions, and insights discovered during development.

## Purpose

This knowledge base serves as organizational memory:

- **Capture:** Document patterns and lessons as we discover them
- **Reuse:** Reference patterns in future issues to avoid re-solving problems
- **Decide:** Record why we made architectural choices
- **Learn:** Share observations and constraints discovered

The knowledge base is actively integrated into the issue workflow - solutions discovered while completing tasks should be documented here.

---

## Structure

### patterns/

Reusable solutions to common problems. These are high-value, low-friction entries.

**Format:** One pattern per file (Markdown)
**Naming:** `PATTERN-NAME.md` (kebab-case, descriptive)

**Examples:**
- `github-actions-caching.md` - How to cache dependencies to speed up workflows
- `error-handling-http.md` - Best practice for HTTP error handling
- `database-migration-safety.md` - How to deploy DB changes without downtime
- `testing-with-mocks.md` - Mocking patterns for unit tests
- `api-rate-limiting.md` - Implementing rate limiting cleanly

**Template:**

```markdown
# [Pattern Name]

## Problem
What problem does this pattern solve? When would you use it?

## Solution
How do you implement this pattern? Step-by-step or code example.

## Example
Concrete code or configuration example that can be copied and adapted.

## Trade-offs
What are the costs/limitations? When would you NOT use this?

## Variations
Alternative approaches or related patterns.

## References
Links to external resources, blog posts, or documentation.

## Related Patterns
Links to similar patterns in this knowledge base.

## Discovered In
Link to the issue where this pattern was discovered and documented.
```

**Contribution Trigger:**
When closing an issue, if you discovered a reusable solution, document it as a pattern.

---

### decisions/

Architecture Decision Records (ADRs) - formal record of why we chose X over Y.

**Format:** Numbered, one decision per file
**Naming:** `ADR-001-TITLE.md`, `ADR-002-TITLE.md` (sequential)

**Examples:**
- `ADR-001-use-postgresql-over-mongodb.md` - Why we chose PostgreSQL
- `ADR-002-rest-api-over-graphql.md` - Why REST instead of GraphQL
- `ADR-003-monorepo-structure.md` - How we organize code across services

**Template:**

```markdown
# ADR-001: [Short Decision Title]

## Status
One of: Proposed | Accepted | Deprecated | Superseded by ADR-NNN

## Date
ISO format: YYYY-MM-DD

## Context
Why was this decision needed? What drove the need to choose?

## Decision
What did we choose and why? Be specific.

## Rationale
Why is this the best choice given the context?

## Consequences

### Positive
- Benefit 1
- Benefit 2
- Benefit 3

### Negative
- Cost 1
- Cost 2
- Risk 1

### Trade-offs
What are we giving up? What becomes harder?

## Alternatives Considered
- Option A: Why not this?
- Option B: Why not this?
- Option C (Chosen): Why this one?

## Related ADRs
Links to related decisions (supersedes, superseded by, related to).

## Implementation Notes
Any important notes for implementing this decision.
```

**Contribution Trigger:**
When making architectural choices during a task, create an ADR to explain the reasoning.

---

### insights/

Learnings and observations - less formal than ADRs, more evolutionary.

**Format:** Freeform markdown, organized by date or topic
**Naming:** `YYYY-MM-DD-TITLE.md` (date-based) or `TOPIC-TITLE.md` (topic-based)

**Examples:**
- `2026-01-05-scaling-caching-performance.md` - "We observed that in-memory cache hits 100MB"
- `2026-01-03-github-api-rate-limits.md` - "API rate limits during parallel jobs"
- `api-versioning-strategies.md` - "Lessons learned from versioning APIs"
- `deployment-rollback-patterns.md` - "What we've learned about safe rollbacks"

**Characteristics:**
- More casual tone than ADRs
- Can be observations, gotchas, lessons
- Can evolve as we learn more
- May lead to formal ADRs later
- Good place for warnings and constraints

**Template:**

```markdown
# [Insight Title]

**Date:** YYYY-MM-DD
**Author:** @username
**Context:** Brief context about where this learning came from

## Observation
What did we observe or learn?

## Implication
Why does this matter? What should we do differently?

## Constraints
Any constraints or gotchas we discovered?

## Related Issues
Links to issues where this was discovered.

## Follow-up Actions
Any actions we should take based on this insight?
- [ ] Create issue for X
- [ ] Add pattern to knowledge base
- [ ] Create ADR if this becomes a decision
```

**Contribution Trigger:**
During development, if you notice something important that other developers should know, document it here.

---

## How to Contribute

### When to Add to Knowledge Base

**Add a PATTERN when:**
- You solve a problem that will likely come up again
- The solution is reusable across multiple contexts
- You want to help future developers solve the same problem faster

**Add a DECISION (ADR) when:**
- You make a significant architectural choice
- You evaluate multiple options and choose one
- The decision affects how the team builds features going forward

**Add an INSIGHT when:**
- You discover a constraint or limitation
- You observe unexpected behavior in a system
- You want to warn other developers about a gotcha
- You learn something that doesn't fit patterns/decisions but is valuable

### How to Contribute

1. **Complete a task** (Issue → PR → Merge)

2. **Ask yourself:**
   - Did I solve a reusable problem? → Create `patterns/NAME.md`
   - Did I make an architectural choice? → Create `decisions/ADR-NNN.md`
   - Did I learn something important? → Create `insights/TOPIC.md`

3. **Create the file** (see templates above)

4. **Link it:**
   - Reference the issue/PR that prompted it: `Discovered in: #NNN`
   - Link related patterns/decisions
   - Update this README if adding major patterns

5. **Make it discoverable:**
   - Use clear, searchable filenames
   - Add comments that someone would search for
   - Cross-reference related documents

### Review Checklist for KB Entries

- [ ] Clear, descriptive title
- [ ] Problem/context is explained
- [ ] Solution is concrete and actionable
- [ ] Example code (if applicable) works
- [ ] Trade-offs are documented
- [ ] Related documents are linked
- [ ] No duplicate of existing entry
- [ ] Filename follows conventions

---

## Discovery & Search

### By Keyword

```bash
# Find all patterns about caching
grep -r "caching" docs/knowledge/patterns/

# Find all ADRs about API design
grep -r "API" docs/knowledge/decisions/

# Find insights from January
ls docs/knowledge/insights/2026-01-*.md
```

### By Type

```bash
# List all patterns
ls docs/knowledge/patterns/

# List all ADRs (in order)
ls docs/knowledge/decisions/ADR-*.md

# List all insights
ls docs/knowledge/insights/
```

### When Creating Issues

**Reference knowledge base in issue description:**

```markdown
## References
This task relates to:
- Pattern: `patterns/github-actions-caching.md` for build speed
- Decision: `decisions/ADR-003-api-design.md` - the reasoning
- Insight: `insights/rate-limiting-gotchas.md` - important constraint
```

This helps @copilot (or any developer) understand the context without re-solving old problems.

---

## Maintenance

### Quarterly Review (Every 3 months)

1. **Archive outdated patterns**
   - Move to `archive/` if no longer relevant
   - Create new pattern if approach has evolved

2. **Update ADRs if context changes**
   - Mark as superseded if newer decision made
   - Update consequences if implementation changed

3. **Consolidate insights**
   - Convert important insights to patterns
   - Archive lessons no longer relevant

4. **Link related documents**
   - Patterns → related patterns
   - ADRs → related ADRs
   - Insights → relevant patterns/ADRs

### Organization Best Practices

- Keep descriptions concise
- Make examples copy-paste ready
- Link generously to related docs
- Update when implementation changes
- Note discovery date to understand currency

---

## Using the Knowledge Base in Your Work

### As a Developer

**Before implementing:**
```bash
# Search for similar problems already solved
grep -r "keyword" docs/knowledge/

# Read related patterns to understand approach
cat docs/knowledge/patterns/RELEVANT-PATTERN.md

# Check decisions to understand architectural constraints
cat docs/knowledge/decisions/ADR-NNN.md
```

**After implementing:**
```bash
# Did you solve a reusable problem?
# Create docs/knowledge/patterns/YOUR-PATTERN.md

# Did you make a design choice?
# Create docs/knowledge/decisions/ADR-NNN.md

# Did you learn something valuable?
# Create docs/knowledge/insights/DATE-TITLE.md
```

### As @copilot

**When processing an issue:**
1. Search knowledge base for related patterns
2. Reference relevant decisions in analysis
3. Note constraints from insights
4. After implementation, add new patterns discovered

**In PR descriptions:**
```markdown
## Knowledge Base
- Implements pattern from: `docs/knowledge/patterns/X.md`
- Respects constraint from ADR: `docs/knowledge/decisions/ADR-NNN.md`
- Discovered new pattern: See `docs/knowledge/patterns/Y.md` (new)
```

---

## Examples of Good KB Entries

### Pattern Example

**File:** `docs/knowledge/patterns/github-actions-caching.md`

```markdown
# GitHub Actions Dependency Caching

## Problem
GitHub Actions workflows are slow because they reinstall dependencies on every run.

## Solution
Use `actions/cache@v3` to cache dependency directories between runs.

## Example
# .github/workflows/test.yml
- name: Cache npm dependencies
  uses: actions/cache@v3
  with:
    path: ~/.npm
    key: ${{ runner.os }}-npm-${{ hashFiles('**/package-lock.json') }}
    restore-keys: |
      ${{ runner.os }}-npm-

## Trade-offs
- Pros: 5-10x faster workflows
- Cons: Cache can become stale (strategy: weekly refresh)

## Discovered In
Issue #42: Speed up CI pipeline
```

### ADR Example

**File:** `docs/knowledge/decisions/ADR-003-rest-api-over-graphql.md`

```markdown
# ADR-003: REST API Over GraphQL

## Status: Accepted

## Date: 2026-01-05

## Context
We're building an API. Our team is familiar with REST. GraphQL would be more flexible.

## Decision
Use REST API for v1, with clear versioning strategy.

## Rationale
- Team already knows REST
- REST is simpler to cache
- Fewer edge cases in early product

## Alternatives Considered
- GraphQL: more flexible, but unfamiliar to team
- GRPC: wrong use case for web clients

## Related ADRs
None yet. Will create ADR-NNN for versioning strategy later.
```

---

## FAQ

**Q: Should I document every small change?**
A: No. Document things you'd want other developers to know. If it's specific to one issue, probably not KB-worthy.

**Q: When do insights become patterns?**
A: When you've seen the insight apply multiple times across different contexts.

**Q: Who approves KB entries?**
A: During code review, reviewers check KB entries just like code. Should be accurate and helpful.

**Q: How do we search the KB?**
A: `grep -r` for now. Future: could build a search UI.

---

## See Also

- `.github/ISSUE_TEMPLATE/task.yml` - How issues reference the KB
- `.github/pull_request_template.md` - PR checklist includes KB updates
- `.github/workflows/copilot-task.yml` - Workflow checks KB during task processing

