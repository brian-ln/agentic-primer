# SQL Injection Prevention Tools

This directory contains security utilities for preventing SQL injection vulnerabilities in the Session Knowledge System.

## Tools Overview

### 1. Input Validation (`input-validation.ts`)

Core sanitization and validation functions:

```typescript
import { sanitizeLikePattern } from './input-validation';

const sanitized = sanitizeLikePattern(userInput);
// "Use % wildcard" → "Use \\% wildcard"
```

**Functions:**
- `sanitizeLikePattern(input)` - Escapes SQL LIKE wildcards (%, _, \)
- `validateSessionId(id)` - Validates UUID format
- `validateLength(input, max, name)` - Enforces length limits
- `validateFilePath(path, base)` - Prevents directory traversal
- `sanitizeErrorMessage(error)` - Removes sensitive info from errors

### 2. Query Builder (`query-builder.ts`)

Type-safe SQL query construction with automatic sanitization:

```typescript
import { queryBuilder } from './query-builder';

const query = queryBuilder()
  .select('*')
  .from('session_decisions')
  .whereLike('decision', userInput, 'contains')
  .orderBy('timestamp DESC')
  .limit(100)
  .build();

const results = db.query(query.sql).all(...query.params);
```

**Features:**
- Automatic LIKE pattern sanitization
- Type-safe column/table name validation
- Parameterized query generation
- Multi-column search support
- ESCAPE clause handling

**API:**
- `.select(...columns)` - Specify columns
- `.from(table)` - Specify table
- `.where(condition, ...params)` - Add WHERE condition
- `.whereLike(column, pattern, mode)` - Add safe LIKE condition
- `.whereOrLike(columns, pattern, mode)` - Multi-column LIKE
- `.orderBy(clause)` - Add ORDER BY
- `.limit(n)` - Add LIMIT
- `.build()` - Generate SafeQuery

**LIKE Modes:**
- `'contains'` - `%pattern%` (default)
- `'starts'` - `pattern%`
- `'ends'` - `%pattern`
- `'exact'` - `pattern`

### 3. Rate Limiter (`rate-limiter.ts`)

Prevent DoS attacks and resource exhaustion:

```typescript
import { withRateLimit, dbRateLimiter } from './rate-limiter';

const results = await withRateLimit(async () => {
  return await db.execute({ sql: '...', args: [...] });
}, dbRateLimiter);
```

**Features:**
- Token bucket algorithm
- Exponential backoff on failures
- Concurrent request limiting
- Configurable thresholds

**Global Instances:**
- `dbRateLimiter` - For database queries (50 req/s, 10 concurrent)
- `llmRateLimiter` - For LLM API calls (5 req/s, 5 concurrent)

### 4. SQL Pattern Checker (`sql-pattern-checker.ts`)

Static analysis tool for detecting SQL injection vulnerabilities:

```bash
# Check entire project
bun run src/session-knowledge/security/sql-pattern-checker.ts src/

# Check specific directory
bun run src/session-knowledge/security/sql-pattern-checker.ts src/session-knowledge/cli/
```

**Detects:**
- Template literal interpolation in SQL
- LIKE queries without ESCAPE clauses
- Raw string concatenation in patterns
- Unsafe parameter usage
- SQL injection attempts

**Exit Codes:**
- `0` - No errors found
- `1` - Errors detected (fails pre-commit)

## Usage Patterns

### Basic LIKE Query

```typescript
import { sanitizeLikePattern } from './input-validation';

const sanitized = sanitizeLikePattern(userInput);
const results = db.query(`
  SELECT * FROM table
  WHERE name LIKE ? ESCAPE '\\'
`).all(`%${sanitized}%`);
```

### Using Query Builder

```typescript
import { queryBuilder } from './query-builder';

const query = queryBuilder()
  .select('id', 'decision', 'timestamp')
  .from('session_decisions')
  .where('timestamp >= ?', startTime)
  .whereLike('decision', searchTerm, 'contains')
  .orderBy('timestamp DESC')
  .limit(50)
  .build();

const results = db.query(query.sql).all(...query.params);
```

### Multi-Column Search

```typescript
import { buildSearchQuery } from './query-builder';

const query = buildSearchQuery(
  'session_decisions',
  ['decision', 'reasoning', 'alternatives'],
  searchTerm,
  'contains',
  100
);

const results = db.query(query.sql).all(...query.params);
```

### Rate Limited Operations

```typescript
import { withRateLimit, dbRateLimiter } from './rate-limiter';

const results = await withRateLimit(async () => {
  return await db.execute({
    sql: 'SELECT * WHERE name LIKE ? ESCAPE \'\\\'',
    args: [`%${sanitized}%`]
  });
}, dbRateLimiter);
```

## Pre-Commit Integration

Install the pre-commit hook to automatically check for SQL injection vulnerabilities:

```bash
# Install enhanced pre-commit hook
./scripts/install-pre-commit-hook.sh

# Manual check before commit
./scripts/pre-commit-sql-check.sh
```

The hook will:
1. Scan staged TypeScript files for SQL injection patterns
2. Block commits if vulnerabilities are detected
3. Provide fix suggestions for each issue

## Testing

Run security tests:

```bash
# All security tests (94 tests)
bun test src/session-knowledge/__tests__/security.test.ts

# Query builder tests (47 tests)
bun test src/session-knowledge/__tests__/query-builder.test.ts

# Run static analysis
bun run src/session-knowledge/security/sql-pattern-checker.ts src/
```

## Security Checklist

When writing database query code:

- [ ] Import `sanitizeLikePattern` from `./input-validation`
- [ ] Call `sanitizeLikePattern()` on user input before building pattern
- [ ] Add `ESCAPE '\\'` after every LIKE clause
- [ ] Use parameterized queries with `?` placeholders
- [ ] Add rate limiting with `withRateLimit()` or `rateLimiter.throttle()`
- [ ] Validate input lengths and formats
- [ ] Sanitize error messages
- [ ] Add security tests for attack scenarios
- [ ] Run SQL pattern checker before committing

## Documentation

- **SECURITY_PATTERNS.md** - Comprehensive guide with examples and common mistakes
- **SECURITY_FIXES.md** - Complete implementation report
- **SECURITY_REVIEW.md** - Original security assessment

## Support

For questions or issues:
1. Review SECURITY_PATTERNS.md for examples
2. Check existing code in `src/session-knowledge/cli/` for patterns
3. Run tests to verify security properties
4. Use static analyzer to detect vulnerabilities

**Remember: Security is not optional. Every LIKE query must be sanitized.**

---

**Version**: 2.0.0 (with Query Builder & Static Analysis)
**Last Updated**: 2026-02-04
**Epic**: agentic-primer-0lg.2
