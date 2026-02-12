# Security Best Practices - Session Knowledge System

This document outlines security best practices for developers working on the Session Knowledge System.

## Table of Contents

1. [Input Validation](#input-validation)
2. [SQL Security](#sql-security)
3. [File System Security](#file-system-security)
4. [Rate Limiting](#rate-limiting)
5. [Error Handling](#error-handling)
6. [Testing Requirements](#testing-requirements)

---

## Input Validation

### Always Validate User Input

**Rule**: Every user input must be validated before use.

```typescript
// ❌ BAD: No validation
const sessionId = args[0];
await processSession(sessionId);

// ✅ GOOD: Validate format
import { validateSessionId } from './security/input-validation';
const sessionId = validateSessionId(args[0]);
await processSession(sessionId);
```

### Enforce Length Limits

**Rule**: All text inputs must have maximum length constraints.

```typescript
// ❌ BAD: Unlimited input
const query = args[0];

// ✅ GOOD: Enforce limits
import { validateLength, INPUT_LIMITS } from './security/input-validation';
const query = validateLength(args[0], INPUT_LIMITS.MAX_QUERY_LENGTH, 'query');
```

### Available Validation Functions

```typescript
import {
  validateLength,           // String length validation
  validateSessionId,        // UUID format validation
  validateFilePath,         // Path traversal prevention
  validateProjectName,      // Safe directory names
  validateInteger,          // Integer range validation
  validateDateString,       // Date format validation
  validateCategory,         // Category validation
  INPUT_LIMITS,            // Predefined limits
} from './security/input-validation';
```

---

## SQL Security

### Always Use Parameterized Queries

**Rule**: NEVER concatenate user input into SQL queries.

```typescript
// ❌ BAD: String concatenation (SQL injection!)
const sql = `SELECT * FROM sessions WHERE id = '${userId}'`;

// ✅ GOOD: Parameterized query
const sql = 'SELECT * FROM sessions WHERE id = ?';
const result = await db.execute({ sql, args: [userId] });
```

### Sanitize LIKE Patterns

**Rule**: Escape wildcards in LIKE queries.

```typescript
// ❌ BAD: Unsanitized LIKE (allows wildcard injection)
const sql = `SELECT * FROM sessions WHERE summary LIKE ?`;
await db.execute({ sql, args: [`%${query}%`] });

// ✅ GOOD: Sanitized LIKE pattern
import { sanitizeLikePattern } from './security/input-validation';
const sanitized = sanitizeLikePattern(query);
const sql = `SELECT * FROM sessions WHERE summary LIKE ? ESCAPE '\\'`;
await db.execute({ sql, args: [`%${sanitized}%`] });
```

### Use Transactions for Multi-Step Operations

```typescript
// ✅ GOOD: Atomic transactions
await db.execute('BEGIN');
try {
  await db.execute({ sql: 'INSERT INTO sessions ...', args: [...] });
  await db.execute({ sql: 'INSERT INTO session_files ...', args: [...] });
  await db.execute('COMMIT');
} catch (err) {
  await db.execute('ROLLBACK');
  throw err;
}
```

---

## File System Security

### Prevent Directory Traversal

**Rule**: Validate all file paths before accessing the file system.

```typescript
// ❌ BAD: Unchecked path (directory traversal!)
const projectDir = join(baseDir, userInput);

// ✅ GOOD: Validated path
import { validateFilePath } from './security/input-validation';
const projectDir = validateFilePath(join(baseDir, userInput), baseDir);
```

### Validate Project Names

**Rule**: Sanitize directory names derived from user paths.

```typescript
// ❌ BAD: Direct path conversion
const projectName = process.cwd().replace(/\//g, '-');

// ✅ GOOD: Validated project name
import { validateProjectName } from './security/input-validation';
const projectName = validateProjectName(process.cwd());
const projectDir = resolve(baseDir, '.claude/projects', projectName);
```

### Use Absolute Paths

**Rule**: Always resolve to absolute paths and verify they're within allowed directories.

```typescript
import { resolve, relative } from 'path';

// ✅ GOOD: Verify path is within base
const resolved = resolve(basePath, userPath);
const rel = relative(basePath, resolved);
if (rel.startsWith('..')) {
  throw new Error('Invalid path: directory traversal detected');
}
```

---

## Rate Limiting

### Apply Rate Limiting to API Calls

**Rule**: All LLM API calls must be rate-limited.

```typescript
// ❌ BAD: No rate limiting
for (const item of items) {
  await llm.classify(item);
}

// ✅ GOOD: Rate-limited API calls
import { llmRateLimiter } from './security/rate-limiter';

for (const item of items) {
  await llmRateLimiter.throttle();
  try {
    await llm.classify(item);
    llmRateLimiter.recordSuccess();
  } catch (error) {
    llmRateLimiter.recordFailure(); // Triggers exponential backoff
    throw error;
  }
}
```

### Use Batch Processing

**Rule**: Process items in batches to limit concurrent requests.

```typescript
// ✅ GOOD: Batch processing with rate limiting
const batchSize = 5;
for (let i = 0; i < items.length; i += batchSize) {
  const batch = items.slice(i, i + batchSize);
  await Promise.all(batch.map(async item => {
    await llmRateLimiter.throttle();
    return processItem(item);
  }));
}
```

### Available Rate Limiters

```typescript
import {
  RateLimiter,        // Rate limiter class
  llmRateLimiter,     // For LLM API calls
  dbRateLimiter,      // For database operations
  withRateLimit,      // Helper function
} from './security/rate-limiter';
```

---

## Error Handling

### Sanitize Error Messages

**Rule**: Remove sensitive information from error messages.

```typescript
// ❌ BAD: Raw error (exposes paths, keys)
catch (error) {
  console.error('Error:', error.message);
}

// ✅ GOOD: Sanitized error
import { sanitizeErrorMessage } from './security/input-validation';
catch (error) {
  console.error('Error:', sanitizeErrorMessage(error));
}
```

### What Gets Sanitized

The `sanitizeErrorMessage` function removes:
- File paths (`/home/user/.claude/...` → `[PATH]`)
- API keys (`ABCDEF123456...` → `[KEY]`)
- Credentials in URLs (`user:pass@host` → `[CREDENTIALS]@host`)
- Passwords (`password=secret` → `password=[REDACTED]`)
- Tokens (`token=abc123` → `token=[REDACTED]`)

### Graceful Degradation

```typescript
// ✅ GOOD: Fail safely
try {
  const result = await riskyOperation();
  return result;
} catch (error) {
  console.error('Operation failed:', sanitizeErrorMessage(error));
  return defaultValue; // Or throw after logging
}
```

---

## Testing Requirements

### Security Tests Required

Every security-sensitive function must have tests covering:

1. **Valid inputs**: Normal operation
2. **Invalid inputs**: Error handling
3. **Edge cases**: Empty strings, nulls, extremes
4. **Attack vectors**: Malicious inputs

### Example Test Structure

```typescript
describe('validateSessionId', () => {
  test('accepts valid UUID', () => {
    const valid = 'f03b3b54-ca47-46d3-be1f-20ccfc82f9de';
    expect(validateSessionId(valid)).toBe(valid);
  });

  test('rejects SQL injection attempt', () => {
    expect(() => validateSessionId("' OR '1'='1")).toThrow();
  });

  test('rejects directory traversal', () => {
    expect(() => validateSessionId('../../../etc/passwd')).toThrow();
  });

  test('handles empty input', () => {
    expect(() => validateSessionId('')).toThrow();
  });
});
```

### Run Security Tests

```bash
# Run all security tests
bun test src/session-knowledge/__tests__/security.test.ts

# Run with coverage
bun test --coverage src/session-knowledge/__tests__/security.test.ts
```

---

## Code Review Checklist

Before submitting code, verify:

- [ ] All user inputs are validated
- [ ] Length limits are enforced
- [ ] SQL queries use parameterized queries
- [ ] LIKE patterns are sanitized
- [ ] File paths are validated
- [ ] Rate limiting is applied to API calls
- [ ] Error messages are sanitized
- [ ] Security tests are included
- [ ] No shell commands with user input
- [ ] No `eval()` or `Function()` constructor

---

## Common Vulnerabilities to Avoid

### 1. SQL Injection

```typescript
// ❌ NEVER do this
const sql = `SELECT * FROM table WHERE name = '${userInput}'`;

// ✅ Always use parameterized queries
const sql = 'SELECT * FROM table WHERE name = ?';
await db.execute({ sql, args: [userInput] });
```

### 2. Path Traversal

```typescript
// ❌ NEVER trust user paths directly
const file = join(baseDir, userPath);

// ✅ Always validate
const file = validateFilePath(join(baseDir, userPath), baseDir);
```

### 3. Command Injection

```typescript
// ❌ NEVER execute shell commands with user input
exec(`ls ${userInput}`);

// ✅ Use library functions instead
import { readdir } from 'fs/promises';
await readdir(validatedPath);
```

### 4. Unbounded Resource Consumption

```typescript
// ❌ No limits
const items = userInput.split(',');

// ✅ Enforce limits
const MAX_ITEMS = 1000;
const items = userInput.split(',').slice(0, MAX_ITEMS);
```

---

## Incident Response

If you discover a security vulnerability:

1. **Do not commit** the vulnerable code
2. **Report immediately** to the security team
3. **Document** the issue in SECURITY_AUDIT.md
4. **Create a fix** following these best practices
5. **Add tests** to prevent regression
6. **Update documentation** if needed

---

## Resources

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [Node.js Security Best Practices](https://nodejs.org/en/docs/guides/security/)
- [SQL Injection Prevention Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/SQL_Injection_Prevention_Cheat_Sheet.html)

---

**Last Updated**: 2026-02-03
**Version**: 1.0.0
