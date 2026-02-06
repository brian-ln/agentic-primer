# Code Patterns

This directory contains proven, reusable solutions to recurring problems in this codebase.

## What is a Pattern?

A pattern is a **proven solution** to a **recurring problem** that is **specific to this project**.

**Not patterns:**
- Generic advice ("write clean code")
- One-off solutions used once
- Untested ideas or proposals

**Are patterns:**
- Solutions used successfully 2+ times
- Project-specific implementations
- Validated approaches that work

## When to Document a Pattern

Document a pattern when:

1. **Recurrence:** You've solved the same problem multiple times
2. **Validation:** The solution has proven effective in production
3. **Specificity:** It's particular to this codebase (not generic wisdom)
4. **Teachability:** New team members would benefit from knowing it

## Pattern Template

Use this template when creating new pattern documentation:

```markdown
# Pattern Name

## Problem

What problem does this pattern solve? Be specific.

Example: "When making database queries that might return large result sets,
we need a consistent way to handle pagination without loading all records into memory."

## Context

When should this pattern be used? What are the conditions or constraints?

Example: "This pattern applies to all API endpoints that return lists of resources.
It's especially important for tables with >1000 records where full-table scans
would cause performance issues."

## Solution

How do you implement this pattern? Provide concrete code examples.

Example:
\`\`\`javascript
// Use cursor-based pagination with limit
async function getUsers(cursor = null, limit = 50) {
  const query = {
    limit: limit,
    orderBy: 'created_at',
    ...(cursor && { where: { created_at: { gt: cursor } } })
  };

  const results = await db.users.findMany(query);

  return {
    data: results,
    nextCursor: results.length === limit
      ? results[results.length - 1].created_at
      : null
  };
}
\`\`\`

## Trade-offs

What are the advantages and disadvantages of this pattern?

**Advantages:**
- Efficient for large datasets
- Consistent memory usage
- Stateless (cursor in response, not server)

**Disadvantages:**
- Can't jump to arbitrary pages (no "page 5")
- Requires stable sort order
- New records may shift results between requests

## Alternatives Considered

What other solutions were tried or considered?

1. **Offset-based pagination:** Works for small datasets, but performance degrades
   with large offsets (OFFSET 10000 LIMIT 50 scans all 10000 records)

2. **Load all records:** Simple but causes memory issues and slow API responses
   for large tables

3. **GraphQL connections:** Good standard but adds complexity for simple REST API

## Examples in Codebase

Where is this pattern used?

- `src/api/users.js` - User listing endpoint
- `src/api/posts.js` - Blog post pagination
- `src/api/comments.js` - Comment threads

## Related Patterns

Links to related patterns in this directory.

- [Error Response Format](error-response-format.md)
- [Database Connection Retry](database-connection-retry.md)

## Status

Current status of this pattern:

- **Active** - Recommended for use
- **Deprecated** - Avoid in new code, migrate away from in existing code
- **Superseded** - Replaced by [new pattern link]

**Current Status:** Active

## History

- **2026-01-06:** Pattern documented based on implementation in users and posts APIs
- **2025-12-10:** Initial implementation in users endpoint
```

## Pattern Lifecycle

### Active

Pattern is current and recommended for use.

**When to mark Active:**
- Pattern has been validated in production
- No known significant issues
- Recommended for new implementations

### Deprecated

Pattern has known issues or better alternatives exist, but is still in use.

**When to mark Deprecated:**
- Better pattern discovered
- Significant trade-offs identified
- Migration planned but not complete

**Required information:**
- Why it's deprecated
- What to use instead
- Migration timeline if any

### Superseded

Pattern has been replaced by a better approach and should not be used.

**When to mark Superseded:**
- Better pattern documented and in use
- Old pattern no longer found in codebase
- Kept for historical reference only

**Required information:**
- Link to replacement pattern
- Completion date of migration

## Contribution Guidelines

### Before Creating a Pattern

Ask yourself:

1. **Is this truly recurring?** Have you used this solution 2+ times?
2. **Is it proven?** Has it worked in production?
3. **Is it specific?** Is it unique to this project, not generic advice?
4. **Will it help others?** Would new team members benefit?

If yes to all four, document it.

### Writing Good Patterns

**Do:**
- Include concrete code examples from the actual codebase
- Explain trade-offs honestly (no perfect solutions)
- Link to actual usage in the codebase
- Describe the problem before the solution
- Update when the pattern evolves

**Don't:**
- Copy generic patterns from books/internet
- Document untested ideas or proposals
- Leave out trade-offs or disadvantages
- Use theoretical examples instead of real code
- Let patterns go stale without updates

### Pattern Review

All new patterns should be reviewed by at least one other team member before merging.

**Review checklist:**
- [ ] Problem is clearly stated and specific
- [ ] Solution includes concrete code examples
- [ ] Trade-offs are honestly documented
- [ ] Links to actual usage in codebase
- [ ] Alternative approaches considered and documented
- [ ] Pattern is truly recurring (used 2+ times)

## AI Agent Integration

When @copilot processes a task, it:

1. **Reads all active patterns** before implementation
2. **Applies relevant patterns** to maintain consistency
3. **References patterns** in PR description
4. **Proposes new patterns** when discovering recurring solutions

This ensures AI-generated code follows project conventions.

## Getting Started

1. **Browse existing patterns:** Understand current practices
2. **Apply patterns in your work:** Reference them during implementation
3. **Propose new patterns:** Document solutions you've validated
4. **Keep patterns current:** Update when approaches evolve

Patterns are living documentation - they should evolve with the codebase.

---

**Last Updated:** 2026-01-06
**Pattern Count:** 0 (initial setup)
