# SQL Injection Prevention Patterns

**Last Updated:** 2026-02-04
**Security Grade:** A
**Epic:** agentic-primer-0lg.2

This document provides clear patterns and examples for preventing SQL injection vulnerabilities in the Session Knowledge System. These patterns are derived from the comprehensive security fixes implemented in Phase 2.

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [LIKE Queries - The Main Risk](#like-queries---the-main-risk)
3. [Pattern Sanitization](#pattern-sanitization)
4. [Type-Safe Query Builder](#type-safe-query-builder)
5. [Rate Limiting](#rate-limiting)
6. [Security Checklist](#security-checklist)
7. [Common Mistakes](#common-mistakes)
8. [Testing Your Code](#testing-your-code)

---

## Quick Reference

### ✅ GOOD Patterns

```typescript
// Pattern 1: Use sanitizeLikePattern() + ESCAPE
import { sanitizeLikePattern } from '../security/input-validation';

const sanitized = sanitizeLikePattern(userInput);
const results = db.query(`
  SELECT * FROM table
  WHERE name LIKE ? ESCAPE '\\'
`).all(`%${sanitized}%`);

// Pattern 2: Use query builder (recommended)
import { queryBuilder } from '../security/query-builder';

const query = queryBuilder()
  .select('*')
  .from('session_decisions')
  .whereLike('decision', userInput, 'contains')
  .build();

const results = db.query(query.sql).all(...query.params);

// Pattern 3: Parameterized queries (non-LIKE)
const results = db.query(`
  SELECT * FROM table WHERE id = ?
`).all(userId);
```

### ❌ BAD Patterns

```typescript
// NEVER: Direct string interpolation in LIKE
const query = `SELECT * FROM table WHERE name LIKE '%${userInput}%'`; // ❌ VULNERABLE

// NEVER: Template literal interpolation
const query = db.query(`SELECT * FROM table WHERE name LIKE '%${input}%'`); // ❌ VULNERABLE

// NEVER: Missing ESCAPE clause
const sanitized = sanitizeLikePattern(input);
const query = db.query(`SELECT * FROM table WHERE name LIKE ?`).all(`%${sanitized}%`); // ❌ INCOMPLETE

// NEVER: String concatenation
const pattern = '%' + userInput + '%';
const query = db.query(`SELECT * FROM table WHERE name LIKE ?`).all(pattern); // ❌ VULNERABLE
```

---

## LIKE Queries - The Main Risk

### Why LIKE Queries Are Dangerous

SQL LIKE patterns use wildcards:
- `%` matches any sequence of characters
- `_` matches any single character
- `\` escapes the next character

**Attack Example:**

```typescript
// User provides input: "%"
const userInput = "%";

// Vulnerable code
const results = db.query(`
  SELECT * FROM session_decisions
  WHERE decision LIKE '%${userInput}%'
`).all();

// Generated SQL: SELECT * FROM session_decisions WHERE decision LIKE '%%'
// Result: Returns ALL decisions in the database! 💀
```

### The Two-Part Solution

**1. Sanitize the pattern** to escape wildcards:

```typescript
function sanitizeLikePattern(input: string): string {
  return input.replace(/[%_\\]/g, '\\$&');
}

// Input: "Use % wildcard"
// Output: "Use \\% wildcard"
```

**2. Add ESCAPE clause** to tell SQL how to interpret backslashes:

```sql
WHERE decision LIKE '%Use \\% wildcard%' ESCAPE '\\'
```

Without `ESCAPE '\\'`, the database might interpret `\\%` differently depending on the SQL dialect.

---

## Pattern Sanitization

### When to Use sanitizeLikePattern()

Use `sanitizeLikePattern()` **every time** user input is used in a LIKE query:

```typescript
import { sanitizeLikePattern } from '../security/input-validation';

// ✅ CLI commands
const sessionId = args[0];
const sanitized = sanitizeLikePattern(sessionId);
db.query(`SELECT * FROM decisions WHERE session_id LIKE ? ESCAPE '\\'`)
  .all(`${sanitized}%`);

// ✅ Search functions
async function searchDecisions(query: string) {
  const sanitized = sanitizeLikePattern(query);
  return db.query(`
    SELECT * FROM session_decisions
    WHERE decision LIKE ? ESCAPE '\\'
       OR reasoning LIKE ? ESCAPE '\\'
  `).all(`%${sanitized}%`, `%${sanitized}%`);
}

// ✅ Temporal queries
async queryAtTime(query: string, asOf: Date) {
  const sanitizedQuery = sanitizeLikePattern(query);
  const decisions = await db.execute({
    sql: `SELECT * WHERE decision LIKE ? ESCAPE '\\'`,
    args: [`%${sanitizedQuery}%`]
  });
}
```

### What Gets Sanitized

```typescript
sanitizeLikePattern("Test % pattern")    // → "Test \\% pattern"
sanitizeLikePattern("File_name.txt")     // → "File\\_name.txt"
sanitizeLikePattern("Path\\to\\file")    // → "Path\\\\to\\\\file"
sanitizeLikePattern("%%%")               // → "\\%\\%\\%"
```

### ESCAPE Clause Position

The `ESCAPE '\\'` clause **must** come after the LIKE pattern:

```sql
-- ✅ Correct
WHERE name LIKE ? ESCAPE '\\'

-- ❌ Wrong (syntax error)
WHERE name ESCAPE '\\' LIKE ?

-- ✅ Correct (multiple LIKE conditions)
WHERE (decision LIKE ? ESCAPE '\\' OR reasoning LIKE ? ESCAPE '\\')
```

---

## Type-Safe Query Builder

### Why Use Query Builder?

The query builder provides:
- **Type safety** - catches errors at compile time
- **Automatic sanitization** - handles escaping for you
- **Parameterized queries** - prevents injection by design
- **Cleaner code** - easier to read and maintain

### Basic Usage

```typescript
import { queryBuilder } from '../security/query-builder';

// Simple query
const query = queryBuilder()
  .select('*')
  .from('session_decisions')
  .whereLike('decision', userInput, 'contains')
  .build();

// Execute
const results = db.query(query.sql).all(...query.params);
```

### LIKE Modes

```typescript
// Contains (default) - %pattern%
.whereLike('decision', 'auth', 'contains')
// SQL: decision LIKE '%auth%' ESCAPE '\\'

// Starts with - pattern%
.whereLike('session_id', 'abc123', 'starts')
// SQL: session_id LIKE 'abc123%' ESCAPE '\\'

// Ends with - %pattern
.whereLike('file_name', '.txt', 'ends')
// SQL: file_name LIKE '%.txt' ESCAPE '\\'

// Exact match - pattern
.whereLike('category', 'decision', 'exact')
// SQL: category LIKE 'decision' ESCAPE '\\'
```

### Complex Queries

```typescript
import { queryBuilder } from '../security/query-builder';

const query = queryBuilder()
  .select('id', 'decision', 'reasoning', 'timestamp')
  .from('session_decisions')
  .where('timestamp >= ?', startDate.getTime())
  .where('timestamp < ?', endDate.getTime())
  .whereLike('decision', searchTerm, 'contains')
  .orderBy('timestamp DESC')
  .limit(100)
  .build();

const results = db.query(query.sql).all(...query.params);
```

### Multi-Column Search

```typescript
// Search across multiple columns
const query = queryBuilder()
  .select('*')
  .from('session_decisions')
  .whereOrLike(['decision', 'reasoning', 'alternatives'], userSearch, 'contains')
  .build();

// Generated SQL:
// SELECT * FROM session_decisions
// WHERE (decision LIKE ? ESCAPE '\\' OR reasoning LIKE ? ESCAPE '\\' OR alternatives LIKE ? ESCAPE '\\')
```

### Convenience Functions

```typescript
import { buildLikeQuery, buildSearchQuery } from '../security/query-builder';

// Simple LIKE query
const query1 = buildLikeQuery(
  'session_decisions',
  'decision',
  userInput,
  'contains',
  100
);

// Multi-column search
const query2 = buildSearchQuery(
  'session_decisions',
  ['decision', 'reasoning', 'alternatives'],
  searchTerm,
  'contains',
  50
);

const results = db.query(query1.sql).all(...query1.params);
```

---

## Rate Limiting

### Why Rate Limiting?

Rate limiting prevents:
- **Denial of Service (DoS)** attacks
- **Resource exhaustion** from rapid queries
- **Database overload** from automated attacks

### Using withRateLimit

```typescript
import { withRateLimit, dbRateLimiter } from '../security/rate-limiter';

// Wrap database operations
const results = await withRateLimit(async () => {
  return await db.execute({
    sql: `SELECT * FROM session_decisions WHERE decision LIKE ? ESCAPE '\\'`,
    args: [`%${sanitizedQuery}%`]
  });
}, dbRateLimiter);
```

### Rate Limiter Configuration

```typescript
import { RateLimiter } from '../security/rate-limiter';

// Database operations (fast, high volume)
const dbRateLimiter = new RateLimiter({
  maxRequestsPerSecond: 50,
  minDelayMs: 20,
  maxConcurrent: 10,
  backoffMultiplier: 1.5,
  maxBackoffMs: 5_000,
});

// LLM API calls (slow, expensive)
const llmRateLimiter = new RateLimiter({
  maxRequestsPerSecond: 5,
  minDelayMs: 200,
  maxConcurrent: 5,
  backoffMultiplier: 2,
  maxBackoffMs: 30_000,
});
```

### Adding Rate Limiting to Class Methods

```typescript
import { RateLimiter } from '../security/rate-limiter';

export class QueryEngine {
  private db: Database;
  private rateLimiter: RateLimiter;

  constructor() {
    this.db = new Database(DB_PATH, { readonly: true });
    this.rateLimiter = new RateLimiter({
      maxRequestsPerSecond: 50,
      minDelayMs: 20,
      maxConcurrent: 10,
    });
  }

  async search(query: string): Promise<Result[]> {
    await this.rateLimiter.throttle();  // ✅ Rate limit
    const sanitized = sanitizeLikePattern(query);
    return this.db.query(`
      SELECT * FROM table WHERE text LIKE ? ESCAPE '\\'
    `).all(`%${sanitized}%`);
  }
}
```

---

## Security Checklist

Use this checklist when writing database query code:

### For Every LIKE Query

- [ ] Import `sanitizeLikePattern` from `../security/input-validation`
- [ ] Call `sanitizeLikePattern()` on user input **before** building pattern
- [ ] Add `ESCAPE '\\'` after **every** LIKE clause
- [ ] Use parameterized queries with `?` placeholders
- [ ] Verify pattern is built correctly (`%${sanitized}%`, `${sanitized}%`, etc.)

### For Query Functions

- [ ] Add rate limiting with `withRateLimit()` or `rateLimiter.throttle()`
- [ ] Validate input length with `validateLength()`
- [ ] Validate input format (session IDs, dates, etc.)
- [ ] Sanitize error messages with `sanitizeErrorMessage()`
- [ ] Add appropriate tests for injection attempts

### For New Features

- [ ] Review all database queries for SQL injection risks
- [ ] Consider using `queryBuilder()` for complex queries
- [ ] Document any security assumptions or requirements
- [ ] Add security tests for attack scenarios
- [ ] Run `bun run src/session-knowledge/security/sql-pattern-checker.ts` before committing

---

## Common Mistakes

### Mistake 1: Forgetting ESCAPE Clause

```typescript
// ❌ Missing ESCAPE
const sanitized = sanitizeLikePattern(input);
db.query(`SELECT * WHERE name LIKE ?`).all(`%${sanitized}%`);

// ✅ Correct
const sanitized = sanitizeLikePattern(input);
db.query(`SELECT * WHERE name LIKE ? ESCAPE '\\'`).all(`%${sanitized}%`);
```

### Mistake 2: Sanitizing After Pattern Building

```typescript
// ❌ Wrong order - sanitize after building pattern
const pattern = `%${input}%`;
const sanitized = sanitizeLikePattern(pattern);

// ✅ Correct - sanitize before building pattern
const sanitized = sanitizeLikePattern(input);
const pattern = `%${sanitized}%`;
```

### Mistake 3: Using Template Literals

```typescript
// ❌ Template literal (vulnerable even with sanitization)
const sanitized = sanitizeLikePattern(input);
db.query(`SELECT * WHERE name LIKE '%${sanitized}%' ESCAPE '\\'`);

// ✅ Parameterized query
const sanitized = sanitizeLikePattern(input);
db.query(`SELECT * WHERE name LIKE ? ESCAPE '\\'`).all(`%${sanitized}%`);
```

### Mistake 4: Multiple Sanitization

```typescript
// ❌ Double sanitization breaks the pattern
const sanitized1 = sanitizeLikePattern(input);
const sanitized2 = sanitizeLikePattern(sanitized1); // Wrong!

// ✅ Sanitize once
const sanitized = sanitizeLikePattern(input);
```

### Mistake 5: Wrong ESCAPE Character

```typescript
// ❌ Different escape characters
db.query(`SELECT * WHERE name LIKE ? ESCAPE '/'`);  // Wrong
db.query(`SELECT * WHERE name LIKE ? ESCAPE '|'`);  // Wrong

// ✅ Always use backslash
db.query(`SELECT * WHERE name LIKE ? ESCAPE '\\'`);
```

---

## Testing Your Code

### Security Test Patterns

```typescript
import { describe, test, expect } from 'bun:test';
import { sanitizeLikePattern } from '../security/input-validation';

describe('SQL Injection Prevention', () => {
  test('prevents wildcard data exfiltration', () => {
    const maliciousInput = '%';
    const sanitized = sanitizeLikePattern(maliciousInput);

    const results = db.query(`
      SELECT * FROM session_decisions
      WHERE decision LIKE ? ESCAPE '\\'
    `).all(`%${sanitized}%`);

    // Should match only literal "%" character, not all records
    expect(results.length).toBeLessThan(totalRecords);
  });

  test('escapes underscore wildcards', () => {
    const maliciousInput = '_______________'; // 15 underscores
    const sanitized = sanitizeLikePattern(maliciousInput);

    const results = db.query(`
      SELECT * FROM session_decisions
      WHERE decision LIKE ? ESCAPE '\\'
    `).all(`%${sanitized}%`);

    // Should match only literal underscores, not arbitrary 15-char strings
    expect(results.every(r => r.decision.includes('_______________'))).toBe(true);
  });

  test('escapes backslash characters', () => {
    const maliciousInput = '\\%test';
    const sanitized = sanitizeLikePattern(maliciousInput);

    expect(sanitized).toBe('\\\\\\%test');
    // Backslash → \\\\, percent → \\%
  });

  test('prevents SQL injection attempts', () => {
    const maliciousInput = "'; DROP TABLE session_decisions; --";
    const sanitized = sanitizeLikePattern(maliciousInput);

    // Should search for literal string, not execute SQL
    const results = db.query(`
      SELECT * FROM session_decisions
      WHERE decision LIKE ? ESCAPE '\\'
    `).all(`%${sanitized}%`);

    // Table should still exist
    expect(() => db.query('SELECT COUNT(*) FROM session_decisions').get()).not.toThrow();
  });
});
```

### Running Security Checks

```bash
# Run security tests
bun test src/session-knowledge/__tests__/security.test.ts

# Run static analysis
bun run src/session-knowledge/security/sql-pattern-checker.ts src/

# Run all tests
bun test
```

### Pre-Commit Checks

The pre-commit hook automatically runs security checks:

```bash
# Manual pre-commit check
.git/hooks/pre-commit

# Check specific files
bun run src/session-knowledge/security/sql-pattern-checker.ts src/session-knowledge/cli/
```

---

## Migration Guide

### Converting Existing Code

**Before:**
```typescript
const sessionId = args[0];
const results = db.query(`
  SELECT * FROM session_decisions
  WHERE session_id LIKE ?
`).all(`${sessionId}%`);
```

**After (Option 1: Manual sanitization):**
```typescript
import { sanitizeLikePattern } from '../security/input-validation';

const sessionId = args[0];
const sanitized = sanitizeLikePattern(sessionId);
const results = db.query(`
  SELECT * FROM session_decisions
  WHERE session_id LIKE ? ESCAPE '\\'
`).all(`${sanitized}%`);
```

**After (Option 2: Query builder):**
```typescript
import { queryBuilder } from '../security/query-builder';

const sessionId = args[0];
const query = queryBuilder()
  .select('*')
  .from('session_decisions')
  .whereLike('session_id', sessionId, 'starts')
  .build();

const results = db.query(query.sql).all(...query.params);
```

### Adding Rate Limiting

**Before:**
```typescript
export class QueryEngine {
  search(query: string) {
    return db.query(`SELECT * WHERE text LIKE ?`).all(`%${query}%`);
  }
}
```

**After:**
```typescript
import { RateLimiter } from '../security/rate-limiter';
import { sanitizeLikePattern } from '../security/input-validation';

export class QueryEngine {
  private rateLimiter = new RateLimiter({
    maxRequestsPerSecond: 50,
    minDelayMs: 20,
    maxConcurrent: 10,
  });

  async search(query: string) {
    await this.rateLimiter.throttle();
    const sanitized = sanitizeLikePattern(query);
    return db.query(`
      SELECT * WHERE text LIKE ? ESCAPE '\\'
    `).all(`%${sanitized}%`);
  }
}
```

---

## Additional Resources

- **SECURITY_FIXES.md** - Complete implementation report with all security fixes
- **SECURITY_REVIEW.md** - Original security assessment and vulnerability analysis
- **src/session-knowledge/security/input-validation.ts** - Input validation utilities
- **src/session-knowledge/security/query-builder.ts** - Type-safe query builder
- **src/session-knowledge/security/rate-limiter.ts** - Rate limiting implementation
- **src/session-knowledge/__tests__/security.test.ts** - Comprehensive security tests (94 tests)

---

## Questions?

If you encounter a pattern not covered in this guide:
1. Check existing code in `src/session-knowledge/cli/` for examples
2. Review security tests in `__tests__/security.test.ts`
3. Run the static analyzer to detect vulnerabilities
4. When in doubt, use the query builder - it handles sanitization automatically

**Security is not optional. Every LIKE query must be sanitized.**
