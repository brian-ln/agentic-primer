# Security Audit Report - Know CLI

**Date**: 2026-02-03
**System**: Session Knowledge System (know CLI)
**Auditor**: Claude Sonnet 4.5
**Scope**: P0 Security Hardening

## Executive Summary

This audit reviewed the Session Knowledge System for critical security vulnerabilities before high-scale deployment. The system handles user input through a CLI interface and stores data in SQLite databases (libSQL). Overall, the codebase demonstrates good security practices with **parameterized queries throughout**, but several areas require hardening.

**Risk Level**: MODERATE
**Blockers for Production**: 2 P0 issues
**Recommended Improvements**: 5 P1 issues

---

## Findings

### P0 Issues (Must Fix Before Production)

#### 1. SQL Injection in QueryEngine.search() - LIKE wildcards
**File**: `src/session-knowledge/index/QueryEngine.ts:111-118`
**Severity**: HIGH
**Status**: VULNERABLE

**Issue**: The `search()` method uses user input directly in LIKE patterns without escaping special characters:

```typescript
search(query: string, options: QueryOptions = {}): SessionResult[] {
  return this.db.query(`
    SELECT id, created, summary, cost, message_count as messageCount
    FROM sessions
    WHERE summary LIKE ?
    ORDER BY created DESC
    LIMIT ?
  `).all(`%${query}%`, options.limit || 20) as SessionResult[];
}
```

While the query uses parameterized queries (good!), the `%${query}%` string concatenation happens before parameter binding. A malicious user could inject:
- `%` to match all records
- `_` as single character wildcards
- Escape sequences to bypass filters

**Attack Vector**:
```bash
know sessions search "%"  # Returns ALL sessions
know sessions search "a%" # Unintended wildcard behavior
```

**Fix**: Sanitize LIKE wildcards before concatenation:
```typescript
function sanitizeLikePattern(input: string): string {
  return input.replace(/[%_\\]/g, '\\$&');
}

search(query: string, options: QueryOptions = {}): SessionResult[] {
  const sanitized = sanitizeLikePattern(query);
  return this.db.query(`
    SELECT id, created, summary, cost, message_count as messageCount
    FROM sessions
    WHERE summary LIKE ? ESCAPE '\\'
    ORDER BY created DESC
    LIMIT ?
  `).all(`%${sanitized}%`, options.limit || 20) as SessionResult[];
}
```

---

#### 2. Path Traversal in File Operations
**File**: `src/session-knowledge/cli.ts:316-332`
**Severity**: HIGH
**Status**: VULNERABLE

**Issue**: The `runAdd()` function derives project directory from `process.cwd()` and constructs file paths without validation:

```typescript
const cwd = process.cwd();
const homeDir = process.env.HOME || '';
const projectName = cwd.replace(/\//g, '-');
const projectDir = join(homeDir, '.claude/projects', projectName);
```

An attacker could:
1. Execute from a malicious directory: `/tmp/../../etc/passwd`
2. Create directory traversal via `cd` before running command
3. Access files outside `.claude/projects/`

**Attack Vector**:
```bash
cd "/tmp/../../etc"
./know add decision "pwned"
```

**Fix**: Validate and canonicalize paths:
```typescript
import { resolve, normalize } from 'path';

function validateProjectPath(cwd: string, baseDir: string): string {
  const normalized = normalize(cwd);
  const projectName = normalized.replace(/\//g, '-').replace(/^-+/, '');
  const projectDir = resolve(baseDir, '.claude/projects', projectName);

  // Ensure resolved path is within base directory
  if (!projectDir.startsWith(resolve(baseDir, '.claude/projects'))) {
    throw new Error('Invalid project path: directory traversal detected');
  }

  return projectDir;
}
```

---

### P1 Issues (Should Fix Soon)

#### 3. Input Length Limits Missing
**Files**: Multiple CLI commands
**Severity**: MEDIUM
**Status**: NEEDS IMPROVEMENT

**Issue**: No length limits on user inputs. Long inputs can:
- Cause DoS through memory exhaustion
- Break UI rendering
- Waste LLM API credits

**Examples**:
- `know add decision "<10MB of text>"` - No limit
- `know search "<100KB query>"` - Excessive API cost
- Session IDs, file paths - No validation

**Fix**: Add input validation:
```typescript
const MAX_TEXT_LENGTH = 10_000; // 10KB
const MAX_QUERY_LENGTH = 1_000;  // 1KB
const MAX_SESSION_ID_LENGTH = 100;

function validateInput(text: string, maxLength: number, fieldName: string): string {
  if (text.length > maxLength) {
    throw new Error(`${fieldName} exceeds maximum length of ${maxLength} characters`);
  }
  return text;
}
```

---

#### 4. UUID Format Validation Missing
**Files**: Multiple CLI commands accepting session IDs
**Severity**: MEDIUM
**Status**: NEEDS IMPROVEMENT

**Issue**: Session IDs are accepted without validation. While parameterized queries prevent SQL injection, invalid formats waste resources:

```typescript
// Current: No validation
await runExtract(['not-a-uuid']); // Wastes DB query

// Should validate:
const UUID_REGEX = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

function validateSessionId(id: string): void {
  if (!UUID_REGEX.test(id) && id !== 'all' && id !== 'today' && id !== 'yesterday') {
    throw new Error(`Invalid session ID format: ${id}`);
  }
}
```

---

#### 5. XSS in Console Output (Low Risk but Best Practice)
**Files**: All CLI output functions
**Severity**: LOW
**Status**: ADVISORY

**Issue**: User-controlled content is displayed without sanitization. While terminal environments are generally safe, best practice is to escape control characters:

```typescript
// Current:
console.log(`  ${result.content}`);

// Better:
function sanitizeOutput(text: string): string {
  return text
    .replace(/\x1b/g, '\\x1b')  // ANSI escape codes
    .replace(/\r/g, '\\r')
    .replace(/\n/g, '\\n')
    .replace(/\t/g, '\\t');
}
```

---

#### 6. Rate Limiting Not Implemented
**Files**: `src/session-knowledge/extraction/KnowledgeExtractor.ts`
**Severity**: MEDIUM
**Status**: NEEDS IMPROVEMENT

**Issue**: The extraction pipeline has hardcoded batch size but no rate limiting:

```typescript
// Current: Fixed batch size, no rate limiting
const batchSize = 5;
for (let i = 0; i < candidates.length; i += batchSize) {
  const batch = candidates.slice(i, i + batchSize);
  await Promise.all(batch.map(async (candidate) => {
    await this.classifier.classifyDecision(candidate.content);
  }));
}
```

**Issues**:
- No delay between batches
- No backoff on API failures
- No concurrent request limits
- Could overwhelm LLM API endpoints

**Fix**: Implement rate limiter:
```typescript
class RateLimiter {
  private lastRequestTime = 0;
  private requestCount = 0;
  private readonly minDelayMs = 100;
  private readonly maxRequestsPerSecond = 10;

  async throttle(): Promise<void> {
    const now = Date.now();
    const timeSinceLastRequest = now - this.lastRequestTime;

    if (timeSinceLastRequest < this.minDelayMs) {
      await new Promise(resolve =>
        setTimeout(resolve, this.minDelayMs - timeSinceLastRequest)
      );
    }

    this.lastRequestTime = Date.now();
    this.requestCount++;
  }
}
```

---

#### 7. Error Messages Leak Internal Information
**Files**: Multiple CLI commands
**Severity**: LOW
**Status**: ADVISORY

**Issue**: Error messages expose internal details:

```typescript
// Current:
catch (error) {
  console.error('Error:', error instanceof Error ? error.message : error);
  process.exit(1);
}
```

**Problems**:
- Stack traces leak file paths
- Database errors expose schema details
- API errors expose credentials in URLs

**Fix**: Sanitize error messages:
```typescript
function sanitizeError(error: unknown): string {
  if (error instanceof Error) {
    // Remove file paths, API keys, connection strings
    return error.message
      .replace(/\/[^\s]+/g, '[PATH]')
      .replace(/\b[A-Z0-9]{20,}\b/g, '[KEY]')
      .replace(/\/\/[^@]+@/g, '//[CREDENTIALS]@');
  }
  return 'An unexpected error occurred';
}
```

---

## Positive Security Practices Found

### ✅ Parameterized Queries Throughout
**ALL** database queries use parameterized queries or prepared statements:

```typescript
// Excellent example from cli.ts:408-412
await db.execute({
  sql: `INSERT INTO session_decisions (id, session_id, message_id, timestamp, decision, reasoning, alternatives, context)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
  args: [id, sessionId, messageId, timestamp, text, reasoning, alternatives, context]
});
```

**Impact**: Complete protection against SQL injection via user content.

### ✅ No Shell Command Execution
The codebase does NOT use `child_process.exec()` or similar with user input. All operations use direct library calls.

### ✅ No eval() or Function() Constructor
No dynamic code evaluation found.

### ✅ Type Safety
TypeScript provides compile-time type checking, reducing runtime errors.

---

## Test Coverage for Security

Current coverage: **91% functions, 91% lines**

### Missing Security Tests

1. **SQL Injection Tests**
   - LIKE wildcard injection
   - Unicode exploitation
   - Boolean-based blind injection attempts

2. **Path Traversal Tests**
   - Directory traversal via cwd
   - Symlink attacks
   - Absolute path injection

3. **Input Validation Tests**
   - Oversized inputs
   - Special characters
   - Unicode edge cases
   - Null bytes

4. **Rate Limiting Tests**
   - Concurrent request handling
   - API failure recovery
   - Resource exhaustion prevention

---

## Remediation Plan

### Phase 1: Critical Fixes (P0) - Must Complete Before Production

1. **Fix QueryEngine LIKE injection** (2 hours)
   - Add `sanitizeLikePattern()` function
   - Update all LIKE queries
   - Add unit tests

2. **Fix path traversal vulnerabilities** (3 hours)
   - Add `validateProjectPath()` function
   - Validate all file operations
   - Add integration tests

3. **Create security test suite** (4 hours)
   - SQL injection test cases
   - Path traversal test cases
   - Input validation test cases

### Phase 2: Important Improvements (P1) - Complete Within 2 Weeks

4. **Add input validation** (4 hours)
   - Length limits on all text inputs
   - UUID format validation
   - Session ID validation

5. **Implement rate limiting** (6 hours)
   - Rate limiter class
   - Backoff strategy
   - Concurrent request limits

6. **Sanitize error messages** (2 hours)
   - Error message sanitizer
   - Remove sensitive details
   - User-friendly messages

7. **Add output sanitization** (2 hours)
   - ANSI escape code handling
   - Control character filtering

### Phase 3: Security Hardening (Nice to Have)

8. **Security documentation**
   - Security best practices guide
   - Incident response plan
   - Security checklist for contributors

9. **Automated security scanning**
   - npm audit integration
   - SAST tool integration
   - Dependency vulnerability scanning

---

## Deployment Checklist

Before deploying to production:

- [ ] P0 Issue #1: LIKE injection fixed
- [ ] P0 Issue #2: Path traversal fixed
- [ ] Security test suite passing (100% P0 tests)
- [ ] Input validation implemented
- [ ] Rate limiting implemented
- [ ] Error message sanitization
- [ ] Security documentation complete
- [ ] Penetration testing completed
- [ ] Code review by security team
- [ ] Incident response plan documented

---

## Testing Strategy

### Unit Tests
- Input validation functions
- Path sanitization functions
- LIKE pattern escaping
- Error message sanitization

### Integration Tests
- End-to-end CLI commands with malicious inputs
- Database operations with attack payloads
- File system operations with traversal attempts

### Fuzzing
- Random input generation
- Boundary condition testing
- Unicode edge cases

---

## Conclusion

The Session Knowledge System demonstrates good security fundamentals with consistent use of parameterized queries and no command injection vectors. However, **two P0 issues must be resolved before production deployment**:

1. LIKE wildcard injection in search operations
2. Path traversal in file operations

Additional improvements in input validation, rate limiting, and error handling will further strengthen the system's security posture.

**Recommendation**: Implement P0 fixes immediately, followed by P1 improvements within 2 weeks.

---

**Audit Completed**: 2026-02-03
**Next Review**: After P0 fixes implemented
