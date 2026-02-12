# SQL Security - Practical Examples

Real-world examples showing how to use the security tools correctly.

## Example 1: CLI Command with Search

**Scenario:** Implementing a search command that finds sessions matching a user query.

**Bad Implementation (VULNERABLE):**

```typescript
#!/usr/bin/env bun
import { Database } from 'bun:sqlite';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');
const db = new Database(DB_PATH, { readonly: true });

// ❌ VULNERABLE - Direct interpolation
const searchTerm = process.argv[2];
const results = db.query(`
  SELECT * FROM sessions
  WHERE title LIKE '%${searchTerm}%'
`).all();

console.log(results);
```

**Attack:** User provides `%` as search term → dumps all sessions

**Good Implementation (SECURE - Manual):**

```typescript
#!/usr/bin/env bun
import { Database } from 'bun:sqlite';
import { sanitizeLikePattern } from '../security/input-validation';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');
const db = new Database(DB_PATH, { readonly: true });

// ✅ SECURE - Sanitize + ESCAPE
const searchTerm = process.argv[2];
const sanitized = sanitizeLikePattern(searchTerm);
const results = db.query(`
  SELECT * FROM sessions
  WHERE title LIKE ? ESCAPE '\\'
`).all(`%${sanitized}%`);

console.log(results);
```

**Good Implementation (SECURE - Query Builder - RECOMMENDED):**

```typescript
#!/usr/bin/env bun
import { Database } from 'bun:sqlite';
import { queryBuilder } from '../security/query-builder';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');
const db = new Database(DB_PATH, { readonly: true });

// ✅ SECURE - Query builder handles everything
const searchTerm = process.argv[2];
const query = queryBuilder()
  .select('*')
  .from('sessions')
  .whereLike('title', searchTerm, 'contains')
  .build();

const results = db.query(query.sql).all(...query.params);
console.log(results);
```

---

## Example 2: Multi-Column Search

**Scenario:** Search across decision, reasoning, and alternatives fields.

**Implementation with Query Builder:**

```typescript
import { buildSearchQuery } from '../security/query-builder';
import { Database } from 'bun:sqlite';

export async function searchDecisions(searchTerm: string, limit: number = 50) {
  const db = new Database(DB_PATH, { readonly: true });

  const query = buildSearchQuery(
    'session_decisions',
    ['decision', 'reasoning', 'alternatives'],
    searchTerm,
    'contains',
    limit
  );

  const results = db.query(query.sql).all(...query.params);
  db.close();

  return results;
}

// Usage
const results = await searchDecisions('authentication', 100);
```

**What It Generates:**

```sql
SELECT * FROM session_decisions
WHERE (
  decision LIKE ? ESCAPE '\\'
  OR reasoning LIKE ? ESCAPE '\\'
  OR alternatives LIKE ? ESCAPE '\\'
)
LIMIT 100
```

**Parameters:** `['%authentication%', '%authentication%', '%authentication%']`

---

## Example 3: Complex Query with Filters

**Scenario:** Search decisions from specific time range with pattern matching.

**Implementation:**

```typescript
import { queryBuilder } from '../security/query-builder';
import { Database } from 'bun:sqlite';

interface SearchOptions {
  searchTerm?: string;
  startDate?: Date;
  endDate?: Date;
  sessionId?: string;
  limit?: number;
}

export async function searchDecisionsAdvanced(options: SearchOptions) {
  const db = new Database(DB_PATH, { readonly: true });

  const builder = queryBuilder()
    .select('id', 'decision', 'reasoning', 'timestamp', 'session_id')
    .from('session_decisions');

  // Add time range filter
  if (options.startDate) {
    builder.where('timestamp >= ?', options.startDate.getTime());
  }
  if (options.endDate) {
    builder.where('timestamp < ?', options.endDate.getTime());
  }

  // Add session filter
  if (options.sessionId) {
    builder.whereLike('session_id', options.sessionId, 'starts');
  }

  // Add search term
  if (options.searchTerm) {
    builder.whereOrLike(
      ['decision', 'reasoning'],
      options.searchTerm,
      'contains'
    );
  }

  // Add ordering and limit
  builder.orderBy('timestamp DESC').limit(options.limit || 50);

  const query = builder.build();
  const results = db.query(query.sql).all(...query.params);
  db.close();

  return results;
}

// Usage examples
const results1 = await searchDecisionsAdvanced({
  searchTerm: 'security',
  startDate: new Date('2026-01-01'),
  endDate: new Date('2026-02-01'),
  limit: 100,
});

const results2 = await searchDecisionsAdvanced({
  sessionId: 'abc123',
  searchTerm: 'database',
});
```

---

## Example 4: Class-Based Query Engine with Rate Limiting

**Scenario:** Query engine class with rate limiting on all methods.

**Implementation:**

```typescript
import { Database } from 'bun:sqlite';
import { RateLimiter } from '../security/rate-limiter';
import { queryBuilder } from '../security/query-builder';

export class DecisionQueryEngine {
  private db: Database;
  private rateLimiter: RateLimiter;

  constructor(dbPath: string) {
    this.db = new Database(dbPath, { readonly: true });
    this.rateLimiter = new RateLimiter({
      maxRequestsPerSecond: 50,
      minDelayMs: 20,
      maxConcurrent: 10,
    });
  }

  async search(searchTerm: string, limit: number = 50): Promise<Decision[]> {
    await this.rateLimiter.throttle();

    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereOrLike(['decision', 'reasoning'], searchTerm, 'contains')
      .orderBy('timestamp DESC')
      .limit(limit)
      .build();

    return this.db.query(query.sql).all(...query.params) as Decision[];
  }

  async findBySession(sessionId: string): Promise<Decision[]> {
    await this.rateLimiter.throttle();

    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .whereLike('session_id', sessionId, 'starts')
      .orderBy('timestamp DESC')
      .build();

    return this.db.query(query.sql).all(...query.params) as Decision[];
  }

  async recent(limit: number = 10): Promise<Decision[]> {
    await this.rateLimiter.throttle();

    const query = queryBuilder()
      .select('*')
      .from('session_decisions')
      .orderBy('timestamp DESC')
      .limit(limit)
      .build();

    return this.db.query(query.sql).all(...query.params) as Decision[];
  }

  close() {
    this.db.close();
  }
}

// Usage
const engine = new DecisionQueryEngine(DB_PATH);
const results = await engine.search('authentication');
engine.close();
```

---

## Example 5: Temporal Query with Rate Limiting

**Scenario:** Query knowledge as it existed at a specific point in time.

**Implementation:**

```typescript
import { createClient, type Client } from '@libsql/client';
import { withRateLimit, dbRateLimiter } from '../security/rate-limiter';
import { sanitizeLikePattern } from '../security/input-validation';

export class TemporalQueryEngine {
  private db: Client;

  constructor(dbPath: string) {
    this.db = createClient({ url: `file:${dbPath}` });
  }

  async queryAtTime(searchTerm: string, asOf: Date): Promise<Decision[]> {
    const asOfMs = asOf.getTime();
    const sanitized = sanitizeLikePattern(searchTerm);

    const result = await withRateLimit(async () => {
      return await this.db.execute({
        sql: `
          SELECT *
          FROM session_decisions
          WHERE (valid_from IS NULL OR valid_from <= ?)
            AND (valid_to IS NULL OR valid_to > ?)
            AND (transaction_from IS NULL OR transaction_from <= ?)
            AND (transaction_to IS NULL OR transaction_to > ?)
            AND (decision LIKE ? ESCAPE '\\' OR reasoning LIKE ? ESCAPE '\\')
          ORDER BY timestamp DESC
        `,
        args: [asOfMs, asOfMs, asOfMs, asOfMs, `%${sanitized}%`, `%${sanitized}%`],
      });
    }, dbRateLimiter);

    return result.rows as Decision[];
  }
}

// Usage
const temporal = new TemporalQueryEngine(DB_PATH);
const pastDecisions = await temporal.queryAtTime(
  'security',
  new Date('2026-01-01')
);
```

---

## Example 6: Testing Security

**Scenario:** Write tests to verify SQL injection prevention.

**Implementation:**

```typescript
import { describe, test, expect } from 'bun:test';
import { searchDecisions } from './search';
import { Database } from 'bun:sqlite';

describe('Search Security', () => {
  test('prevents wildcard data exfiltration', async () => {
    const maliciousInput = '%';
    const results = await searchDecisions(maliciousInput);

    // Should search for literal "%", not match all records
    expect(results.every(r =>
      r.decision.includes('%') || r.reasoning?.includes('%')
    )).toBe(true);
  });

  test('prevents underscore wildcard attacks', async () => {
    const maliciousInput = '_______________'; // 15 underscores
    const results = await searchDecisions(maliciousInput);

    // Should match only literal underscores
    expect(results.every(r =>
      r.decision.includes('_______________') || r.reasoning?.includes('_______________')
    )).toBe(true);
  });

  test('prevents backslash escape bypass', async () => {
    const maliciousInput = '\\%test';
    const results = await searchDecisions(maliciousInput);

    // Should search for literal "\%test"
    expect(results.every(r =>
      r.decision.includes('\\%test') || r.reasoning?.includes('\\%test')
    )).toBe(true);
  });

  test('handles SQL injection attempts safely', async () => {
    const maliciousInput = "'; DROP TABLE session_decisions; --";

    // Should not crash or execute SQL
    const results = await searchDecisions(maliciousInput);

    // Table should still exist
    const db = new Database(DB_PATH);
    expect(() => {
      db.query('SELECT COUNT(*) FROM session_decisions').get();
    }).not.toThrow();
    db.close();
  });

  test('handles empty search term', async () => {
    const results = await searchDecisions('');

    // Should return results (matches everything)
    expect(Array.isArray(results)).toBe(true);
  });

  test('handles unicode in search', async () => {
    const results = await searchDecisions('emoji 🔒 test');

    // Should not crash on unicode
    expect(Array.isArray(results)).toBe(true);
  });
});
```

---

## Example 7: Error Handling with Sanitization

**Scenario:** Handle errors safely without leaking sensitive information.

**Implementation:**

```typescript
import { sanitizeErrorMessage } from '../security/input-validation';

export async function searchDecisions(searchTerm: string) {
  try {
    const query = buildSearchQuery(
      'session_decisions',
      ['decision', 'reasoning'],
      searchTerm
    );

    const db = new Database(DB_PATH);
    const results = db.query(query.sql).all(...query.params);
    db.close();

    return results;
  } catch (error) {
    // ✅ Sanitize error before logging/displaying
    const sanitized = sanitizeErrorMessage(error);
    console.error(`Search failed: ${sanitized}`);

    // Return empty results instead of crashing
    return [];
  }
}

// If error contains: "Failed to read /home/user/.claude/secrets.json"
// Sanitized version: "Failed to read [PATH]"
```

---

## Example 8: Pre-Commit Workflow

**Scenario:** Developer writes new code and commits.

**Workflow:**

```bash
# 1. Write code using query builder
cat > src/session-knowledge/cli/search.ts << 'EOF'
import { queryBuilder } from '../security/query-builder';

const query = queryBuilder()
  .select('*')
  .from('sessions')
  .whereLike('title', searchTerm, 'contains')
  .build();
EOF

# 2. Add tests
cat > src/session-knowledge/__tests__/search.test.ts << 'EOF'
test('prevents SQL injection', () => {
  const malicious = '%';
  const query = buildSearchQuery('table', ['col'], malicious);
  expect(query.params[0]).toBe('%\\%%');
});
EOF

# 3. Run tests locally
bun test src/session-knowledge/__tests__/search.test.ts

# 4. Run static analysis
bun run src/session-knowledge/security/sql-pattern-checker.ts src/

# 5. Stage and commit (pre-commit hook runs automatically)
git add .
git commit -m "feat: add session search"

# Pre-commit hook output:
# 🔍 Checking staged files for SQL injection vulnerabilities...
# ✅ No SQL injection vulnerabilities detected
# [main abc1234] feat: add session search
```

**If vulnerabilities detected:**

```bash
git commit -m "feat: add search"

# Pre-commit hook output:
# 🔍 Checking staged files for SQL injection vulnerabilities...
# ❌ SQL injection vulnerabilities detected:
#
# 📄 src/session-knowledge/cli/search.ts
#   ❌ Line 12 - LIKE query missing ESCAPE clause
#      💡 Fix: Add ESCAPE '\\' after the LIKE pattern parameter
#
# Please fix the issues above before committing.
# See SECURITY_PATTERNS.md for guidance.

# Fix the issue
vim src/session-knowledge/cli/search.ts

# Try again
git add .
git commit -m "feat: add search"
# ✅ No SQL injection vulnerabilities detected
```

---

## Example 9: Migration of Legacy Code

**Scenario:** Converting old vulnerable code to use new security tools.

**Before (VULNERABLE):**

```typescript
export function findSessions(pattern: string) {
  const db = new Database(DB_PATH);
  const results = db.query(`
    SELECT * FROM sessions WHERE title LIKE '%${pattern}%'
  `).all();
  db.close();
  return results;
}
```

**After (Step 1: Add sanitization):**

```typescript
import { sanitizeLikePattern } from '../security/input-validation';

export function findSessions(pattern: string) {
  const db = new Database(DB_PATH);
  const sanitized = sanitizeLikePattern(pattern);
  const results = db.query(`
    SELECT * FROM sessions WHERE title LIKE ? ESCAPE '\\'
  `).all(`%${sanitized}%`);
  db.close();
  return results;
}
```

**After (Step 2: Use query builder - RECOMMENDED):**

```typescript
import { queryBuilder } from '../security/query-builder';

export function findSessions(pattern: string) {
  const db = new Database(DB_PATH);

  const query = queryBuilder()
    .select('*')
    .from('sessions')
    .whereLike('title', pattern, 'contains')
    .build();

  const results = db.query(query.sql).all(...query.params);
  db.close();
  return results;
}
```

**After (Step 3: Add rate limiting):**

```typescript
import { queryBuilder } from '../security/query-builder';
import { RateLimiter } from '../security/rate-limiter';

const rateLimiter = new RateLimiter({
  maxRequestsPerSecond: 50,
  minDelayMs: 20,
  maxConcurrent: 10,
});

export async function findSessions(pattern: string) {
  await rateLimiter.throttle();

  const db = new Database(DB_PATH);

  const query = queryBuilder()
    .select('*')
    .from('sessions')
    .whereLike('title', pattern, 'contains')
    .build();

  const results = db.query(query.sql).all(...query.params);
  db.close();
  return results;
}
```

---

## Example 10: Custom Query Builder Extension

**Scenario:** Extend query builder for specific use case.

**Implementation:**

```typescript
import { queryBuilder, type SafeQuery } from '../security/query-builder';

/**
 * Build a query to search decisions by time range and pattern
 */
export function buildTimeRangeSearchQuery(
  searchTerm: string,
  startDate: Date,
  endDate: Date,
  limit: number = 50
): SafeQuery {
  return queryBuilder()
    .select('id', 'decision', 'reasoning', 'timestamp')
    .from('session_decisions')
    .where('timestamp >= ?', startDate.getTime())
    .where('timestamp < ?', endDate.getTime())
    .whereOrLike(['decision', 'reasoning', 'alternatives'], searchTerm, 'contains')
    .orderBy('timestamp DESC')
    .limit(limit)
    .build();
}

/**
 * Build a query to find decisions by session prefix
 */
export function buildSessionPrefixQuery(
  sessionIdPrefix: string,
  limit: number = 100
): SafeQuery {
  return queryBuilder()
    .select('*')
    .from('session_decisions')
    .whereLike('session_id', sessionIdPrefix, 'starts')
    .orderBy('timestamp DESC')
    .limit(limit)
    .build();
}

/**
 * Build a query for exact decision matching
 */
export function buildExactDecisionQuery(decision: string): SafeQuery {
  return queryBuilder()
    .select('*')
    .from('session_decisions')
    .whereLike('decision', decision, 'exact')
    .build();
}

// Usage
const query1 = buildTimeRangeSearchQuery(
  'security',
  new Date('2026-01-01'),
  new Date('2026-02-01')
);

const query2 = buildSessionPrefixQuery('abc123', 50);

const query3 = buildExactDecisionQuery('Use libSQL for vector storage');
```

---

## Summary

These examples demonstrate:

1. **Query Builder** - Type-safe, secure by default
2. **Manual Sanitization** - When you need more control
3. **Rate Limiting** - Prevent DoS attacks
4. **Multi-Column Search** - Complex query patterns
5. **Error Handling** - Sanitize sensitive information
6. **Testing** - Verify security properties
7. **Pre-Commit Workflow** - Automated enforcement
8. **Migration** - Converting legacy code
9. **Extensions** - Custom query patterns

**Key Takeaways:**
- Use query builder for most cases (easiest, safest)
- Always sanitize LIKE patterns
- Always include ESCAPE '\\' clause
- Add rate limiting to prevent abuse
- Write security tests
- Let static analyzer catch mistakes

For more patterns, see **SECURITY_PATTERNS.md**.
