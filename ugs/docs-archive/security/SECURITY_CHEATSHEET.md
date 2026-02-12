# Security Cheatsheet - Quick Reference

Quick reference for common security tasks in the Session Knowledge System.

---

## Import Security Functions

```typescript
// Input validation
import {
  validateLength,
  validateSessionId,
  validateFilePath,
  validateProjectName,
  validateInteger,
  validateDateString,
  sanitizeLikePattern,
  sanitizeErrorMessage,
  INPUT_LIMITS,
} from './security/input-validation';

// Rate limiting
import {
  RateLimiter,
  llmRateLimiter,
  dbRateLimiter,
  withRateLimit,
} from './security/rate-limiter';
```

---

## Common Validation Patterns

### Validate Text Input
```typescript
const text = validateLength(userInput, INPUT_LIMITS.MAX_TEXT_LENGTH, 'text');
```

### Validate Search Query
```typescript
const query = validateLength(userInput, INPUT_LIMITS.MAX_QUERY_LENGTH, 'query');
```

### Validate Session ID
```typescript
const sessionId = validateSessionId(args[0]); // Accepts UUID or keywords
```

### Validate Date
```typescript
const date = validateDateString('2026-02-03'); // YYYY-MM-DD format
```

### Validate Integer
```typescript
const limit = validateInteger(args[0], 1, 100, 'limit');
```

### Validate File Path
```typescript
const safePath = validateFilePath(userPath, baseDirectory);
```

### Validate Project Name
```typescript
const projectName = validateProjectName(process.cwd());
```

---

## SQL Security

### Parameterized Queries (ALWAYS)
```typescript
// ✅ CORRECT
const result = await db.execute({
  sql: 'SELECT * FROM sessions WHERE id = ?',
  args: [sessionId]
});
```

### LIKE Queries with Sanitization
```typescript
// ✅ CORRECT
const sanitized = sanitizeLikePattern(userInput);
const result = await db.execute({
  sql: `SELECT * FROM sessions WHERE summary LIKE ? ESCAPE '\\'`,
  args: [`%${sanitized}%`]
});
```

---

## Rate Limiting

### Simple Rate Limiting
```typescript
for (const item of items) {
  await llmRateLimiter.throttle();
  try {
    const result = await llm.classify(item);
    llmRateLimiter.recordSuccess();
  } catch (error) {
    llmRateLimiter.recordFailure();
    throw error;
  }
}
```

### With Helper Function
```typescript
const result = await withRateLimit(
  async () => llm.classify(text),
  llmRateLimiter,
  3 // max retries
);
```

### Custom Rate Limiter
```typescript
const customLimiter = new RateLimiter({
  maxRequestsPerSecond: 10,
  minDelayMs: 100,
  maxConcurrent: 5,
});
```

---

## Error Handling

### Sanitize Error Messages
```typescript
catch (error) {
  console.error('Error:', sanitizeErrorMessage(error));
}
```

### What Gets Sanitized
- File paths → `[PATH]`
- API keys → `[KEY]`
- Credentials → `[CREDENTIALS]`
- Passwords → `[REDACTED]`
- Tokens → `[REDACTED]`

---

## Input Limits

```typescript
INPUT_LIMITS.MAX_TEXT_LENGTH     // 10,000 chars
INPUT_LIMITS.MAX_QUERY_LENGTH    // 1,000 chars
INPUT_LIMITS.MAX_SESSION_ID_LENGTH // 100 chars
INPUT_LIMITS.MAX_FILE_PATH_LENGTH // 1,000 chars
```

---

## Common Mistakes to Avoid

### ❌ DON'T: String concatenation in SQL
```typescript
const sql = `SELECT * FROM table WHERE name = '${userInput}'`;
```

### ✅ DO: Parameterized queries
```typescript
const sql = 'SELECT * FROM table WHERE name = ?';
await db.execute({ sql, args: [userInput] });
```

---

### ❌ DON'T: Unsanitized LIKE patterns
```typescript
const sql = 'SELECT * FROM table WHERE col LIKE ?';
await db.execute({ sql, args: [`%${userInput}%`] });
```

### ✅ DO: Sanitized LIKE patterns
```typescript
const sanitized = sanitizeLikePattern(userInput);
const sql = `SELECT * FROM table WHERE col LIKE ? ESCAPE '\\'`;
await db.execute({ sql, args: [`%${sanitized}%`] });
```

---

### ❌ DON'T: Unchecked file paths
```typescript
const file = join(baseDir, userInput);
```

### ✅ DO: Validated file paths
```typescript
const file = validateFilePath(join(baseDir, userInput), baseDir);
```

---

### ❌ DON'T: Unlimited API calls
```typescript
await Promise.all(items.map(item => llm.classify(item)));
```

### ✅ DO: Rate-limited API calls
```typescript
for (const item of items) {
  await llmRateLimiter.throttle();
  await llm.classify(item);
}
```

---

### ❌ DON'T: Raw error messages
```typescript
catch (error) {
  console.error(error.message);
}
```

### ✅ DO: Sanitized error messages
```typescript
catch (error) {
  console.error(sanitizeErrorMessage(error));
}
```

---

## Quick Security Checklist

Before committing code:

- [ ] All user inputs validated
- [ ] SQL queries use parameterized queries
- [ ] LIKE patterns sanitized
- [ ] File paths validated
- [ ] Rate limiting on API calls
- [ ] Error messages sanitized
- [ ] Length limits enforced
- [ ] Tests include security cases

---

## Testing

### Run Security Tests
```bash
bun test src/session-knowledge/__tests__/security.test.ts
```

### Test Specific Function
```bash
bun test src/session-knowledge/__tests__/security.test.ts -t "validateSessionId"
```

---

## More Information

- Full Audit: `SECURITY_AUDIT.md`
- Best Practices: `SECURITY_BEST_PRACTICES.md`
- Implementation: `SECURITY_IMPLEMENTATION_SUMMARY.md`

---

**Quick Help**: If in doubt, validate the input and sanitize the output!
