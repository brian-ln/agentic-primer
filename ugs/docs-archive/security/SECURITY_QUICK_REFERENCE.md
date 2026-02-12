# SQL Security - Quick Reference Card

**Keep this handy when writing database queries.**

---

## The Golden Rule

**EVERY LIKE query needs TWO things:**
1. `sanitizeLikePattern(userInput)` - Escape wildcards
2. `ESCAPE '\\'` - Tell SQL how to interpret escapes

**Easier option:** Use query builder (handles both automatically)

---

## Quick Examples

### ❌ NEVER Do This

```typescript
// Direct interpolation - VULNERABLE
db.query(`SELECT * WHERE name LIKE '%${input}%'`)

// Missing ESCAPE - INCOMPLETE
db.query(`SELECT * WHERE name LIKE ?`).all(`%${input}%`)

// String concatenation - VULNERABLE
const pattern = '%' + input + '%';
db.query(`SELECT * WHERE name LIKE ?`).all(pattern)
```

### ✅ Always Do This

```typescript
// Option 1: Query Builder (RECOMMENDED)
import { queryBuilder } from './security/query-builder';

const query = queryBuilder()
  .select('*')
  .from('table')
  .whereLike('name', userInput, 'contains')
  .build();

db.query(query.sql).all(...query.params);

// Option 2: Manual Sanitization
import { sanitizeLikePattern } from './security/input-validation';

const sanitized = sanitizeLikePattern(userInput);
db.query(`SELECT * WHERE name LIKE ? ESCAPE '\\'`)
  .all(`%${sanitized}%`);
```

---

## LIKE Modes

| Mode | Pattern | Use Case |
|------|---------|----------|
| `'contains'` | `%text%` | Search anywhere (most common) |
| `'starts'` | `text%` | Prefix search (e.g., session IDs) |
| `'ends'` | `%text` | Suffix search (e.g., file extensions) |
| `'exact'` | `text` | Exact match |

---

## Imports You Need

```typescript
// Query builder (easiest)
import { queryBuilder } from './security/query-builder';

// Manual sanitization
import { sanitizeLikePattern } from './security/input-validation';

// Rate limiting
import { withRateLimit, dbRateLimiter } from './security/rate-limiter';
```

---

## Common Patterns

### Single Column Search

```typescript
const query = queryBuilder()
  .select('*')
  .from('sessions')
  .whereLike('title', searchTerm, 'contains')
  .build();
```

### Multi-Column Search

```typescript
const query = queryBuilder()
  .select('*')
  .from('sessions')
  .whereOrLike(['title', 'description'], searchTerm, 'contains')
  .build();
```

### With Filters

```typescript
const query = queryBuilder()
  .select('*')
  .from('sessions')
  .where('timestamp >= ?', startTime)
  .whereLike('title', searchTerm, 'contains')
  .orderBy('timestamp DESC')
  .limit(50)
  .build();
```

### With Rate Limiting

```typescript
const results = await withRateLimit(async () => {
  return await db.execute({ sql: query.sql, args: query.params });
}, dbRateLimiter);
```

---

## Security Checklist

Before committing code with LIKE queries:

- [ ] Import `sanitizeLikePattern` or `queryBuilder`
- [ ] Sanitize user input OR use query builder
- [ ] Include `ESCAPE '\\'` in SQL OR use query builder
- [ ] Use parameterized queries with `?` placeholders
- [ ] Add rate limiting for database operations
- [ ] Add test for malicious input (e.g., `'%'`)
- [ ] Run `bun run src/session-knowledge/security/sql-pattern-checker.ts`

---

## Testing Pattern

```typescript
test('prevents SQL injection', () => {
  const malicious = '%';
  const query = queryBuilder()
    .select('*')
    .from('table')
    .whereLike('col', malicious, 'contains')
    .build();

  expect(query.params[0]).toBe('%\\%%'); // Escaped
});
```

---

## Pre-Commit Check

```bash
# Install hook (one time)
./scripts/install-pre-commit-hook.sh

# Manual check
./scripts/pre-commit-sql-check.sh

# Run tests
bun test src/session-knowledge/__tests__/security.test.ts
```

---

## What Gets Sanitized?

```typescript
sanitizeLikePattern("test%")      // → "test\\%"
sanitizeLikePattern("file_name")  // → "file\\_name"
sanitizeLikePattern("path\\to")   // → "path\\\\to"
sanitizeLikePattern("%%%")        // → "\\%\\%\\%"
```

---

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| No ESCAPE clause | SQL interprets backslashes differently | Add `ESCAPE '\\'` |
| Sanitize after pattern | Wildcards not escaped | Sanitize BEFORE adding `%` |
| Template literals | Bypasses parameterization | Use `?` placeholders |
| Double sanitization | Breaks the pattern | Sanitize only once |

---

## Help & Resources

- **Full Guide:** SECURITY_PATTERNS.md
- **Examples:** src/session-knowledge/security/EXAMPLES.md
- **Tools:** src/session-knowledge/security/README.md
- **Fixed Code:** src/session-knowledge/cli/*.ts

---

## Remember

**Using query builder = Secure by default**

```typescript
// This is all you need:
import { queryBuilder } from './security/query-builder';

const query = queryBuilder()
  .select('*')
  .from('table')
  .whereLike('column', userInput, 'contains')
  .build();

db.query(query.sql).all(...query.params);
```

**Security is not optional. When in doubt, use the query builder.**
