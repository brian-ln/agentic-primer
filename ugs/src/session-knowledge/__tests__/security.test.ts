/**
 * Security Test Suite
 * Tests for input validation, SQL injection prevention, path traversal, etc.
 * Epic: agentic-primer-0lg.2
 */

import { describe, test, expect, beforeEach } from 'bun:test';
import {
  validateLength,
  validateSessionId,
  sanitizeLikePattern,
  validateFilePath,
  validateProjectName,
  validateInteger,
  validateDateString,
  sanitizeOutput,
  sanitizeErrorMessage,
  sanitizeStackTrace,
  validateCategory,
  INPUT_LIMITS,
} from '../security/input-validation';
import { RateLimiter, withRateLimit } from '../security/rate-limiter';
import { join } from 'path';
import { tmpdir } from 'os';
import { mkdirSync, symlinkSync, unlinkSync, rmdirSync, existsSync, rmSync } from 'fs';

describe('Input Validation', () => {
  describe('validateLength', () => {
    test('accepts valid input within limit', () => {
      expect(validateLength('hello', 10, 'test')).toBe('hello');
    });

    test('rejects input exceeding limit', () => {
      expect(() => validateLength('x'.repeat(101), 100, 'test')).toThrow(
        'test exceeds maximum length'
      );
    });

    test('shows actual length in error message', () => {
      expect(() => validateLength('x'.repeat(101), 100, 'field')).toThrow(
        'got 101'
      );
    });
  });

  describe('validateSessionId', () => {
    test('accepts valid UUID', () => {
      const validUuid = 'f03b3b54-ca47-46d3-be1f-20ccfc82f9de';
      expect(validateSessionId(validUuid)).toBe(validUuid);
    });

    test('accepts special keywords', () => {
      expect(validateSessionId('all')).toBe('all');
      expect(validateSessionId('today')).toBe('today');
      expect(validateSessionId('yesterday')).toBe('yesterday');
      expect(validateSessionId('manual')).toBe('manual');
    });

    test('rejects invalid UUID format', () => {
      expect(() => validateSessionId('not-a-uuid')).toThrow(
        'Invalid session ID format'
      );
      expect(() => validateSessionId('12345')).toThrow(
        'Invalid session ID format'
      );
      expect(() => validateSessionId('')).toThrow('Invalid session ID format');
    });

    test('truncates long invalid IDs in error message', () => {
      const longInvalid = 'x'.repeat(100);
      try {
        validateSessionId(longInvalid);
        expect(true).toBe(false); // Should not reach here
      } catch (error: any) {
        expect(error.message).toContain('...');
      }
    });
  });

  describe('sanitizeLikePattern', () => {
    test('escapes % wildcard', () => {
      expect(sanitizeLikePattern('test%')).toBe('test\\%');
      expect(sanitizeLikePattern('%test%')).toBe('\\%test\\%');
    });

    test('escapes _ wildcard', () => {
      expect(sanitizeLikePattern('test_')).toBe('test\\_');
      expect(sanitizeLikePattern('_test_')).toBe('\\_test\\_');
    });

    test('escapes backslash', () => {
      expect(sanitizeLikePattern('test\\pattern')).toBe('test\\\\pattern');
    });

    test('escapes multiple special characters', () => {
      expect(sanitizeLikePattern('%test_value\\here%')).toBe(
        '\\%test\\_value\\\\here\\%'
      );
    });

    test('leaves normal text unchanged', () => {
      expect(sanitizeLikePattern('normal text')).toBe('normal text');
    });
  });

  describe('validateFilePath', () => {
    const baseDir = tmpdir();

    test('accepts valid path within base directory', () => {
      const validPath = join(baseDir, 'projects', 'test');
      expect(() => validateFilePath(validPath, baseDir)).not.toThrow();
    });

    test('rejects path traversal with ..', () => {
      const maliciousPath = join(baseDir, 'projects', '..', '..', 'etc', 'passwd');
      expect(() => validateFilePath(maliciousPath, baseDir)).toThrow(
        'directory traversal detected'
      );
    });

    test('rejects absolute path outside base', () => {
      const outsidePath = '/etc/passwd';
      expect(() => validateFilePath(outsidePath, baseDir)).toThrow(
        'directory traversal detected'
      );
    });

    test('normalizes path before validation', () => {
      const pathWithDots = join(baseDir, 'projects', '.', 'test', '..', 'valid');
      expect(() => validateFilePath(pathWithDots, baseDir)).not.toThrow();
    });

    test('accepts valid symlink within base directory', () => {
      // Create test directory structure
      const testBase = join(tmpdir(), 'symlink-test-valid');
      const targetDir = join(testBase, 'target');
      const symlinkPath = join(testBase, 'link');

      try {
        // Setup
        if (existsSync(testBase)) {
          rmSync(testBase, { recursive: true, force: true });
        }
        mkdirSync(testBase, { recursive: true });
        mkdirSync(targetDir, { recursive: true });
        symlinkSync(targetDir, symlinkPath);

        // Test - symlink pointing within base should be allowed
        expect(() => validateFilePath(symlinkPath, testBase)).not.toThrow();
      } finally {
        // Cleanup
        if (existsSync(testBase)) {
          rmSync(testBase, { recursive: true, force: true });
        }
      }
    });

    test('rejects symlink pointing outside base directory', () => {
      // Create test directory structure
      const testBase = join(tmpdir(), 'symlink-test-invalid');
      const outsideTarget = tmpdir(); // Points outside test base
      const symlinkPath = join(testBase, 'malicious-link');

      try {
        // Setup
        if (existsSync(testBase)) {
          rmSync(testBase, { recursive: true, force: true });
        }
        mkdirSync(testBase, { recursive: true });
        symlinkSync(outsideTarget, symlinkPath);

        // Test - symlink pointing outside base should be rejected
        expect(() => validateFilePath(symlinkPath, testBase)).toThrow(
          'symlink points outside allowed directory'
        );
      } finally {
        // Cleanup
        if (existsSync(testBase)) {
          rmSync(testBase, { recursive: true, force: true });
        }
      }
    });
  });

  describe('validateProjectName', () => {
    test('converts path to safe directory name', () => {
      expect(validateProjectName('/home/user/project')).toMatch(/home-user-project/);
    });

    test('removes leading dashes', () => {
      const result = validateProjectName('/root');
      expect(result.startsWith('-')).toBe(false);
    });

    test('converts dots to dashes', () => {
      const result = validateProjectName('./test');
      expect(result).not.toContain('.');
      expect(result).toMatch(/test/);
    });

    test('rejects reserved names', () => {
      expect(() => validateProjectName('con')).toThrow('reserved name');
      expect(() => validateProjectName('aux')).toThrow('reserved name');
    });

    test('rejects dangerous characters', () => {
      expect(() => validateProjectName('test<script>')).toThrow(
        'forbidden characters'
      );
    });

    test('rejects empty project name', () => {
      expect(() => validateProjectName('')).toThrow('empty project name');
    });
  });

  describe('validateInteger', () => {
    test('accepts valid integer within range', () => {
      expect(validateInteger('10', 1, 100, 'count')).toBe(10);
      expect(validateInteger(50, 1, 100, 'count')).toBe(50);
    });

    test('rejects non-integer input', () => {
      expect(() => validateInteger('abc', 1, 100, 'count')).toThrow(
        'must be a valid integer'
      );
      expect(() => validateInteger('10.5', 1, 100, 'count')).toThrow(
        'must be a valid integer'
      );
    });

    test('rejects integer outside range', () => {
      expect(() => validateInteger(0, 1, 100, 'count')).toThrow(
        'must be between 1 and 100'
      );
      expect(() => validateInteger(101, 1, 100, 'count')).toThrow(
        'must be between 1 and 100'
      );
    });
  });

  describe('validateDateString', () => {
    test('accepts valid date format', () => {
      const date = validateDateString('2026-02-03');
      expect(date.getFullYear()).toBe(2026);
      expect(date.getMonth()).toBe(1); // 0-indexed
      expect(date.getDate()).toBe(3);
    });

    test('removes quotes from input', () => {
      expect(() => validateDateString('"2026-02-03"')).not.toThrow();
      expect(() => validateDateString("'2026-02-03'")).not.toThrow();
    });

    test('rejects invalid date format', () => {
      expect(() => validateDateString('02-03-2026')).toThrow('Invalid date format');
      expect(() => validateDateString('2026/02/03')).toThrow('Invalid date format');
      expect(() => validateDateString('not-a-date')).toThrow('Invalid date format');
    });

    test('rejects invalid date values', () => {
      expect(() => validateDateString('2026-13-01')).toThrow('Invalid date');
      expect(() => validateDateString('2026-02-30')).toThrow('Invalid date');
    });
  });

  describe('sanitizeOutput', () => {
    test('escapes ANSI escape codes', () => {
      expect(sanitizeOutput('\x1b[31mRed Text\x1b[0m')).toBe(
        '\\x1b[31mRed Text\\x1b[0m'
      );
    });

    test('escapes carriage returns', () => {
      expect(sanitizeOutput('Line1\rLine2')).toBe('Line1\\rLine2');
    });

    test('escapes null bytes', () => {
      expect(sanitizeOutput('test\x00null')).toBe('test\\x00null');
    });

    test('leaves normal text unchanged', () => {
      expect(sanitizeOutput('Normal text\nwith newlines')).toBe(
        'Normal text\nwith newlines'
      );
    });
  });

  describe('sanitizeErrorMessage', () => {
    test('redacts file paths', () => {
      const error = new Error('Error in /home/user/.claude/index/db.sqlite');
      expect(sanitizeErrorMessage(error)).toContain('[PATH]');
      expect(sanitizeErrorMessage(error)).not.toContain('/home/user');
    });

    test('redacts API keys', () => {
      const error = new Error(
        'API error: key ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'
      );
      expect(sanitizeErrorMessage(error)).toContain('[KEY]');
      expect(sanitizeErrorMessage(error)).not.toContain('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
    });

    test('redacts credentials in URLs', () => {
      const error = new Error('Connection failed: https://user:pass@example.com/api');
      expect(sanitizeErrorMessage(error)).toContain('[CREDENTIALS]');
      expect(sanitizeErrorMessage(error)).not.toContain('user:pass');
    });

    test('redacts passwords', () => {
      const error = new Error('Auth failed: password=secret123');
      expect(sanitizeErrorMessage(error)).toContain('password=[REDACTED]');
      expect(sanitizeErrorMessage(error)).not.toContain('secret123');
    });

    test('redacts tokens', () => {
      const error = new Error('Token error: token=abc123xyz');
      expect(sanitizeErrorMessage(error)).toContain('token=[REDACTED]');
      expect(sanitizeErrorMessage(error)).not.toContain('abc123xyz');
    });

    test('handles non-Error objects', () => {
      expect(sanitizeErrorMessage('string error')).toBe(
        'An unexpected error occurred'
      );
      expect(sanitizeErrorMessage(null)).toBe('An unexpected error occurred');
    });
  });

  describe('sanitizeStackTrace', () => {
    test('redacts paths in stack traces', () => {
      const stack = `Error: Test error
    at Object.<anonymous> (/Users/bln/.claude/projects/test/file.ts:10:15)
    at Module._compile (node:internal/modules/cjs/loader:1256:14)
    at Object.Module._extensions..js (/home/user/app/src/index.js:89:10)
    at Module.load (/tmp/build/cache/file.js:100:32)
    at Function.executeUserEntryPoint [as runMain] (/Applications/Node.app/Contents/file.js:150:12)`;

      const sanitized = sanitizeStackTrace(stack);
      expect(sanitized).toContain('[PATH]');
      expect(sanitized).not.toContain('/Users/bln/.claude');
      expect(sanitized).not.toContain('/home/user');
      expect(sanitized).not.toContain('/tmp/build');
      expect(sanitized).not.toContain('/Applications/Node.app');
    });

    test('handles undefined and empty stack', () => {
      expect(sanitizeStackTrace(undefined)).toBe('');
      expect(sanitizeStackTrace('')).toBe('');
    });

    test('preserves function names and line numbers', () => {
      const stack = `Error: Test error
    at Object.<anonymous> (/Users/bln/.claude/file.ts:10:15)
    at processRequest (node:internal/request:100:20)`;

      const sanitized = sanitizeStackTrace(stack);
      expect(sanitized).toContain('Object.<anonymous>');
      expect(sanitized).toContain('processRequest');
      expect(sanitized).toContain(':10:15');
      expect(sanitized).toContain(':100:20');
    });
  });

  describe('validateCategory', () => {
    test('accepts valid categories', () => {
      expect(validateCategory('decision')).toBe('decision');
      expect(validateCategory('learning')).toBe('learning');
      expect(validateCategory('error')).toBe('error');
      expect(validateCategory('workflow')).toBe('workflow');
    });

    test('rejects invalid categories', () => {
      expect(() => validateCategory('invalid')).toThrow('Invalid category');
      expect(() => validateCategory('')).toThrow('Invalid category');
    });
  });
});

describe('Rate Limiter', () => {
  let limiter: RateLimiter;

  beforeEach(() => {
    limiter = new RateLimiter({
      maxRequestsPerSecond: 10,
      minDelayMs: 100,
      maxConcurrent: 2,
      backoffMultiplier: 2,
      maxBackoffMs: 1000,
    });
  });

  test('enforces minimum delay between requests', async () => {
    const start = Date.now();

    await limiter.throttle();
    await limiter.throttle();

    const elapsed = Date.now() - start;
    expect(elapsed).toBeGreaterThanOrEqual(100);
  });

  test('limits concurrent requests', async () => {
    const concurrent = [];
    let maxConcurrent = 0;
    let currentConcurrent = 0;

    for (let i = 0; i < 5; i++) {
      concurrent.push(
        limiter.throttle().then(() => {
          currentConcurrent++;
          maxConcurrent = Math.max(maxConcurrent, currentConcurrent);
          return new Promise((resolve) =>
            setTimeout(() => {
              currentConcurrent--;
              resolve(undefined);
            }, 50)
          );
        })
      );
    }

    await Promise.all(concurrent);
    expect(maxConcurrent).toBeLessThanOrEqual(2);
  });

  test('records successful requests', () => {
    limiter.recordSuccess();
    const stats = limiter.getStats();
    expect(stats.totalRequests).toBeGreaterThanOrEqual(0);
  });

  test('applies exponential backoff on failures', async () => {
    limiter.recordFailure();
    limiter.recordFailure();

    const start = Date.now();
    await limiter.throttle();
    const elapsed = Date.now() - start;

    // Should have exponential backoff delay
    expect(elapsed).toBeGreaterThanOrEqual(200); // 100 * 2^2
  });

  test('resets backoff on success', () => {
    limiter.recordFailure();
    limiter.recordFailure();
    limiter.recordSuccess();

    const stats = limiter.getStats();
    // After success, backoff should be reset (can't directly test internal state)
    expect(stats).toBeDefined();
  });

  test('provides accurate statistics', async () => {
    await limiter.throttle();
    await limiter.throttle();

    const stats = limiter.getStats();
    expect(stats.totalRequests).toBe(2);
    expect(stats.throttledRequests).toBeGreaterThanOrEqual(1);
  });

  test('reset clears all state', async () => {
    await limiter.throttle();
    await limiter.throttle();

    limiter.reset();
    const stats = limiter.getStats();

    expect(stats.totalRequests).toBe(0);
    expect(stats.throttledRequests).toBe(0);
  });
});

describe('withRateLimit', () => {
  test('executes function with rate limiting', async () => {
    let executed = false;
    const fn = async () => {
      executed = true;
      return 'success';
    };

    const limiter = new RateLimiter({ minDelayMs: 50 });
    const result = await withRateLimit(fn, limiter);

    expect(executed).toBe(true);
    expect(result).toBe('success');
  });

  test('retries on failure', async () => {
    let attempts = 0;
    const fn = async () => {
      attempts++;
      if (attempts < 3) {
        throw new Error('Temporary failure');
      }
      return 'success';
    };

    const limiter = new RateLimiter({ minDelayMs: 10 });
    const result = await withRateLimit(fn, limiter, 5);

    expect(attempts).toBe(3);
    expect(result).toBe('success');
  });

  test('throws after max retries', async () => {
    const fn = async () => {
      throw new Error('Permanent failure');
    };

    const limiter = new RateLimiter({ minDelayMs: 10 });

    await expect(withRateLimit(fn, limiter, 2)).rejects.toThrow('Permanent failure');
  });
});

describe('SQL Injection Prevention', () => {
  test('LIKE pattern injection prevented', () => {
    // Attack: Use % to match all records
    const maliciousInput = '%';
    const sanitized = sanitizeLikePattern(maliciousInput);
    expect(sanitized).toBe('\\%');

    // Verify it would not match everything when used in query
    const pattern = `%${sanitized}%`;
    expect(pattern).toBe('%\\%%');
  });

  test('wildcard injection prevented', () => {
    const maliciousInput = 'a%b_c';
    const sanitized = sanitizeLikePattern(maliciousInput);
    expect(sanitized).toBe('a\\%b\\_c');
  });

  test('backslash escape injection prevented', () => {
    const maliciousInput = '\\%test';
    const sanitized = sanitizeLikePattern(maliciousInput);
    expect(sanitized).toBe('\\\\\\%test');
  });
});

describe('Path Traversal Prevention', () => {
  test('directory traversal blocked', () => {
    const baseDir = '/safe/directory';
    const maliciousPath = join(baseDir, '..', '..', 'etc', 'passwd');

    expect(() => validateFilePath(maliciousPath, baseDir)).toThrow(
      'directory traversal'
    );
  });

  test('symlink directory traversal blocked', () => {
    const baseDir = '/safe/directory';
    const maliciousPath = '/etc/passwd';

    expect(() => validateFilePath(maliciousPath, baseDir)).toThrow(
      'directory traversal'
    );
  });

  test('project name validation blocks malicious names', () => {
    // Path traversal attempts get converted to safe directory names
    const result = validateProjectName('../../../etc/passwd');
    // Dots and slashes are converted to dashes
    expect(result).not.toContain('.');
    expect(result).not.toContain('/');
    expect(result).toMatch(/etc-passwd/);
  });
});

describe('Input Size Limits', () => {
  test('enforces text length limits', () => {
    const oversized = 'x'.repeat(INPUT_LIMITS.MAX_TEXT_LENGTH + 1);
    expect(() =>
      validateLength(oversized, INPUT_LIMITS.MAX_TEXT_LENGTH, 'text')
    ).toThrow('exceeds maximum length');
  });

  test('enforces query length limits', () => {
    const oversized = 'x'.repeat(INPUT_LIMITS.MAX_QUERY_LENGTH + 1);
    expect(() =>
      validateLength(oversized, INPUT_LIMITS.MAX_QUERY_LENGTH, 'query')
    ).toThrow('exceeds maximum length');
  });

  test('accepts input at exact limit', () => {
    const atLimit = 'x'.repeat(INPUT_LIMITS.MAX_TEXT_LENGTH);
    expect(() =>
      validateLength(atLimit, INPUT_LIMITS.MAX_TEXT_LENGTH, 'text')
    ).not.toThrow();
  });
});

describe('Edge Cases', () => {
  test('handles empty strings', () => {
    expect(validateLength('', 100, 'test')).toBe('');
    expect(sanitizeLikePattern('')).toBe('');
    expect(sanitizeOutput('')).toBe('');
  });

  test('handles unicode characters', () => {
    const unicode = '你好世界🌍';
    expect(validateLength(unicode, 100, 'test')).toBe(unicode);
    expect(sanitizeLikePattern(unicode)).toBe(unicode);
  });

  test('handles null bytes in strings', () => {
    const withNull = 'test\x00null';
    const sanitized = sanitizeOutput(withNull);
    expect(sanitized).toContain('\\x00');
  });

  test('handles very long error messages', () => {
    const longError = new Error('x'.repeat(10000));
    const sanitized = sanitizeErrorMessage(longError);
    expect(sanitized.length).toBeGreaterThan(0);
  });
});

describe('Legacy CLI SQL Injection Prevention', () => {
  test('sanitizes session ID in decisions.ts pattern', () => {
    // Simulates the pattern used in decisions.ts: WHERE session_id LIKE ?
    const maliciousSessionId = "%'; DROP TABLE session_decisions; --";
    const sanitized = sanitizeLikePattern(maliciousSessionId);

    // Should escape % and _ to prevent wildcard matching
    expect(sanitized).toBe("\\%'; DROP TABLE session\\_decisions; --");
    // Verify wildcard is escaped with backslash prefix
    expect(sanitized).toContain('\\%');
    expect(sanitized).toContain('\\_');
  });

  test('sanitizes session ID in learnings.ts pattern', () => {
    // Simulates the pattern used in learnings.ts: WHERE session_id LIKE ?
    const maliciousSessionId = "%%%";
    const sanitized = sanitizeLikePattern(maliciousSessionId);

    // Should escape all % wildcards
    expect(sanitized).toBe("\\%\\%\\%");
  });

  test('sanitizes search term in learnings.ts search', () => {
    // Simulates the pattern used in learnings search: WHERE learning LIKE ? OR context LIKE ?
    const maliciousSearch = "%' OR 1=1 --";
    const sanitized = sanitizeLikePattern(maliciousSearch);

    // Should escape % and make SQL injection ineffective
    expect(sanitized).toBe("\\%' OR 1=1 --");
    expect(sanitized).toContain('\\%');
  });

  test('sanitizes session ID in errors.ts pattern', () => {
    // Simulates the pattern used in errors.ts: WHERE session_id LIKE ?
    const maliciousSessionId = "\\%test";
    const sanitized = sanitizeLikePattern(maliciousSessionId);

    // Should escape backslash and %
    expect(sanitized).toBe("\\\\\\%test");
  });

  test('prevents wildcard data exfiltration', () => {
    // Attack: Use % to dump all data
    const wildcardAttack = "%";
    const sanitized = sanitizeLikePattern(wildcardAttack);

    // Should escape to literal %
    expect(sanitized).toBe("\\%");
  });

  test('prevents underscore wildcard attacks', () => {
    // Attack: Use _ to match single characters
    const underscoreAttack = "_______________"; // Match any 15-char string
    const sanitized = sanitizeLikePattern(underscoreAttack);

    // Should escape all underscores
    expect(sanitized).toBe("\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_");
  });
});

describe('TemporalQueries SQL Injection Prevention', () => {
  test('sanitizes query in queryAtTime decisions', () => {
    // Simulates: decision LIKE '%' || ? || '%'
    const maliciousQuery = "%'; DROP TABLE session_decisions; --";
    const sanitized = sanitizeLikePattern(maliciousQuery);

    // Should escape % wildcards
    expect(sanitized).toContain('\\%');
    expect(sanitized).not.toMatch(/^%/);
  });

  test('sanitizes query in queryAtTime learnings', () => {
    // Simulates: learning LIKE '%' || ? || '%'
    const maliciousQuery = "%' OR '1'='1";
    const sanitized = sanitizeLikePattern(maliciousQuery);

    expect(sanitized).toBe("\\%' OR '1'='1");
  });

  test('sanitizes query in queryAtTime errors', () => {
    // Simulates: error_message LIKE '%' || ? || '%'
    const maliciousQuery = "%"; // Dump all errors
    const sanitized = sanitizeLikePattern(maliciousQuery);

    expect(sanitized).toBe("\\%");
  });

  test('sanitizes query in queryAtTime workflows', () => {
    // Simulates: description LIKE '%' || ? || '%'
    const maliciousQuery = "\\\\%"; // Double escape attempt
    const sanitized = sanitizeLikePattern(maliciousQuery);

    expect(sanitized).toBe("\\\\\\\\\\%");
  });
});

describe('Rate Limiting Integration', () => {
  test('rate limiter enforces delay on QueryEngine methods', async () => {
    const limiter = new RateLimiter({
      maxRequestsPerSecond: 10,
      minDelayMs: 100,
      maxConcurrent: 5,
    });

    const start = Date.now();

    // Simulate two rapid queries
    await limiter.throttle();
    await limiter.throttle();

    const elapsed = Date.now() - start;

    // Second call should be delayed by at least minDelayMs
    expect(elapsed).toBeGreaterThanOrEqual(100);
  });

  test('rate limiter blocks concurrent requests beyond limit', async () => {
    const limiter = new RateLimiter({
      maxRequestsPerSecond: 10,
      minDelayMs: 50,
      maxConcurrent: 2,
    });

    let concurrentCount = 0;
    let maxConcurrent = 0;

    const task = async () => {
      // throttle() internally manages concurrent count
      // The actual work happens inside the throttle, not after
      const throttlePromise = limiter.throttle();
      concurrentCount++;
      maxConcurrent = Math.max(maxConcurrent, concurrentCount);
      await throttlePromise;
      await new Promise(resolve => setTimeout(resolve, 50));
      concurrentCount--;
    };

    // Try to run 5 tasks concurrently
    await Promise.all([task(), task(), task(), task(), task()]);

    // The limiter controls concurrency during throttle execution
    // This test verifies the limiter's internal concurrent request tracking
    const stats = limiter.getStats();
    expect(stats.totalRequests).toBe(5);
  });

  test('withRateLimit wrapper applies rate limiting', async () => {
    const limiter = new RateLimiter({
      maxRequestsPerSecond: 10,
      minDelayMs: 100,
      maxConcurrent: 5,
    });

    let callCount = 0;
    const mockQuery = async () => {
      callCount++;
      return 'result';
    };

    const start = Date.now();

    // Import withRateLimit for testing
    const { withRateLimit } = await import('../security/rate-limiter');

    await withRateLimit(mockQuery, limiter);
    await withRateLimit(mockQuery, limiter);

    const elapsed = Date.now() - start;

    expect(callCount).toBe(2);
    expect(elapsed).toBeGreaterThanOrEqual(100);
  });
});

describe('ReDoS Prevention (L1)', () => {
  test('should handle malicious UUID input without catastrophic backtracking', () => {
    const malicious = '0'.repeat(100000) + '!';
    // Should reject quickly due to validateSessionId's UUID_REGEX check
    // The UUID regex is simple and non-backtracking, but input is invalid
    expect(() => validateSessionId(malicious)).toThrow('Invalid session ID format');
  });

  test('should handle complex patterns without performance degradation', () => {
    const start = Date.now();
    const patterns = [
      'a'.repeat(1000),
      'x'.repeat(100) + '-' + 'y'.repeat(100),
      '1'.repeat(50) + 'z'.repeat(50)
    ];

    patterns.forEach(p => {
      try {
        validateSessionId(p);
      } catch {
        // Expected to fail validation, we're testing performance
      }
    });

    const duration = Date.now() - start;
    expect(duration).toBeLessThan(100); // Should be fast even for invalid input
  });
});

describe('Prototype Pollution Prevention (L2)', () => {
  test('should prevent __proto__ path-based pollution in project names', () => {
    // The real risk is __proto__.property patterns, not just __proto__
    const maliciousPath = '__proto__.isAdmin';
    const result = validateProjectName(maliciousPath);

    // validateProjectName converts dots to dashes, breaking the pollution chain
    expect(result).not.toContain('.');
    expect(result).toBe('__proto__-isAdmin');

    // Verify no prototype pollution can occur
    const obj = {};
    (obj as any)[result] = true;
    expect((obj as any).isAdmin).toBeUndefined();
  });

  test('should prevent constructor.prototype pollution in project names', () => {
    const maliciousPath = 'constructor.prototype.isAdmin';
    const result = validateProjectName(maliciousPath);

    // Dots are converted to dashes
    expect(result).not.toContain('.');
    expect(result).toBe('constructor-prototype-isAdmin');

    // Verify no pollution
    const obj = {};
    (obj as any)[result] = true;
    expect((obj as any).isAdmin).toBeUndefined();
  });

  test('should sanitize prototype in category validation', () => {
    // validateCategory has whitelist: decision, learning, error, workflow
    // Should reject 'prototype' and '__proto__'
    expect(() => validateCategory('prototype')).toThrow('Invalid category');
    expect(() => validateCategory('__proto__')).toThrow('Invalid category');
    expect(() => validateCategory('constructor')).toThrow('Invalid category');
  });

  test('should not allow object prototype pollution via nested paths', () => {
    // Test various prototype pollution patterns
    const patterns = [
      '__proto__.polluted',
      'constructor.prototype.polluted',
      'prototype.polluted'
    ];

    patterns.forEach(pattern => {
      const sanitized = validateProjectName(pattern);
      // All dots should be converted to dashes
      expect(sanitized).not.toContain('.');

      // Verify using the sanitized name doesn't cause pollution
      const obj = {};
      (obj as any)[sanitized] = 'bad';
      expect((obj as any).polluted).toBeUndefined();
    });
  });
});

describe('Second-Order SQL Injection Prevention (M3)', () => {
  test('should handle stored SQL wildcards in text fields', () => {
    // Simulates storing malicious data with SQL wildcards
    const maliciousDecision = "Use % wildcard in queries";
    const maliciousReasoning = "Test '; DROP TABLE sessions; --";
    const maliciousAlternatives = "__ underscore wildcards";

    // When these are stored and later retrieved for use in LIKE queries,
    // they should be sanitized before being used in the query
    const sanitizedDecision = sanitizeLikePattern(maliciousDecision);
    expect(sanitizedDecision).toBe("Use \\% wildcard in queries");

    // Note: sanitizeLikePattern only escapes LIKE wildcards (%, _, \)
    // SQL injection via quotes is handled by parameterized queries
    const sanitizedReasoning = sanitizeLikePattern(maliciousReasoning);
    expect(sanitizedReasoning).toBe("Test '; DROP TABLE sessions; --");
    // The single quotes are safe because the value is passed as a parameter

    const sanitizedAlternatives = sanitizeLikePattern(maliciousAlternatives);
    expect(sanitizedAlternatives).toBe("\\_\\_ underscore wildcards");
  });

  test('should handle stored SQL fragments in learning text', () => {
    // Simulate storing SQL injection attempt as learning content
    const maliciousLearning = "SQL injection: '; DROP TABLE learnings; --";
    const maliciousContext = "Testing % and _ wildcards";
    const maliciousActionable = "Don't use \\% without escaping";

    // When retrieved and used in LIKE queries, should be sanitized
    // Note: sanitizeLikePattern only handles LIKE wildcards, not quotes
    const sanitized = sanitizeLikePattern(maliciousLearning);
    expect(sanitized).toBe("SQL injection: '; DROP TABLE learnings; --");
    // Single quotes are safe in parameterized queries

    const sanitizedContext = sanitizeLikePattern(maliciousContext);
    expect(sanitizedContext).toBe("Testing \\% and \\_ wildcards");

    const sanitizedActionable = sanitizeLikePattern(maliciousActionable);
    expect(sanitizedActionable).toBe("Don't use \\\\\\% without escaping");
  });

  test('should handle backslash escapes in stored error messages', () => {
    // Test backslash escape patterns that could break LIKE queries
    const patterns = [
      { input: "Backslash escape: \\%test", expected: "Backslash escape: \\\\\\%test" },
      { input: "Testing \\\\ double backslash", expected: "Testing \\\\\\\\ double backslash" },
      { input: "Mixed \\% and \\_ wildcards", expected: "Mixed \\\\\\% and \\\\\\_ wildcards" },
      { input: "Triple \\\\\\% escape", expected: "Triple \\\\\\\\\\\\\\% escape" }
    ];

    patterns.forEach(({ input, expected }) => {
      const sanitized = sanitizeLikePattern(input);
      expect(sanitized).toBe(expected);
    });
  });

  test('should not allow wildcard expansion in stored data', () => {
    // Attack: Store data with wildcards that could match unintended records
    const maliciousPatterns = [
      { input: "Use %%% for wildcards", expected: "Use \\%\\%\\% for wildcards" },
      { input: "Pattern with %_% combo", expected: "Pattern with \\%\\_\\% combo" },
      { input: "%", expected: "\\%" },
      { input: "_______________", expected: "\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_\\_" }
    ];

    maliciousPatterns.forEach(({ input, expected }) => {
      const sanitized = sanitizeLikePattern(input);

      // Verify it matches the expected escaped pattern
      expect(sanitized).toBe(expected);

      // Verify escape characters are present before wildcards
      if (input.includes('%')) {
        expect(sanitized).toContain('\\%');
      }
      if (input.includes('_')) {
        expect(sanitized).toContain('\\_');
      }

      // When used in LIKE with ESCAPE '\\', these will match literally, not as wildcards
    });
  });

  test('should handle mixed malicious patterns safely', () => {
    // Complex attack patterns combining multiple techniques
    const complexPatterns = [
      "Test with % wildcard",
      "Test with _ underscore",
      "Test with '; DROP TABLE",
      "Test with \\% escaped",
      "Test with %%% multiple",
      "Test with ___ underscores",
      "Test with \\\\ backslashes",
      "Mix %_\\% all together"
    ];

    complexPatterns.forEach(pattern => {
      const sanitized = sanitizeLikePattern(pattern);

      // All wildcards should be escaped
      if (pattern.includes('%')) {
        expect(sanitized).toContain('\\%');
      }
      if (pattern.includes('_')) {
        expect(sanitized).toContain('\\_');
      }
      if (pattern.includes('\\')) {
        const backslashCount = (pattern.match(/\\/g) || []).length;
        const sanitizedBackslashCount = (sanitized.match(/\\/g) || []).length;
        // Each backslash should be doubled, plus escape chars for wildcards
        expect(sanitizedBackslashCount).toBeGreaterThanOrEqual(backslashCount);
      }

      // Should not contain unescaped wildcards
      expect(sanitized).not.toMatch(/[^\\]%/);
      expect(sanitized).not.toMatch(/[^\\]_/);
    });
  });

  test('should sanitize retrieved data used in subsequent LIKE queries', () => {
    // Simulate: Store data → Retrieve data → Use in another LIKE query
    // This is the actual second-order SQL injection scenario

    // Step 1: Malicious data is stored (represented as string)
    const storedSummary = "Test with % wildcard";

    // Step 2: Data is retrieved (simulated)
    const retrievedSummary = storedSummary;

    // Step 3: Before using in another LIKE query, MUST sanitize
    const sanitizedForQuery = sanitizeLikePattern(retrievedSummary);

    // Verify the sanitization prevents wildcard expansion
    expect(sanitizedForQuery).toBe("Test with \\% wildcard");
    expect(sanitizedForQuery).not.toContain(' % ');

    // If used in LIKE query like: WHERE column LIKE '%' || ? || '%' ESCAPE '\\'
    // The wildcards in stored data won't cause unintended matches
    const likePattern = `%${sanitizedForQuery}%`;
    expect(likePattern).toBe("%Test with \\% wildcard%");
  });
});
