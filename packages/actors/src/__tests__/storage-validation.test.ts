/**
 * Tests for storage layer validation.
 *
 * Coverage:
 * - SQL query validation (dangerous patterns, length limits)
 * - SQL parameter validation (count, size limits)
 * - Batch size validation
 * - KV key validation (size, path traversal, control chars)
 * - KV value validation (size limits)
 * - Edge cases (empty strings, null, undefined, Unicode)
 * - Valid operations pass through
 * - Mock storage verification
 */

import { describe, it, expect, mock } from 'bun:test';
import {
  validateSqlQuery,
  validateSqlParams,
  validateBatchSize,
  validateKvKey,
  validateKvValue,
} from '../validation/storage-validation.ts';
import { ValidatedSqlStorage } from '../validation/validated-sql-storage.ts';
import { ValidatedKvStorage } from '../validation/validated-kv-storage.ts';
import type { ISqlStorage, IKeyValueStorage, SqlValue, SqlResult } from '../interfaces.ts';

// ============================================================================
// SQL Query Validation Tests
// ============================================================================

describe('validateSqlQuery', () => {
  it('allows valid SELECT queries', () => {
    expect(() => validateSqlQuery('SELECT * FROM users WHERE id = ?')).not.toThrow();
    expect(() => validateSqlQuery('SELECT name, email FROM users')).not.toThrow();
  });

  it('allows valid INSERT queries', () => {
    expect(() => validateSqlQuery('INSERT INTO users (name, email) VALUES (?, ?)')).not.toThrow();
  });

  it('allows valid UPDATE queries', () => {
    expect(() => validateSqlQuery('UPDATE users SET name = ? WHERE id = ?')).not.toThrow();
  });

  it('allows valid DELETE queries', () => {
    expect(() => validateSqlQuery('DELETE FROM users WHERE id = ?')).not.toThrow();
  });

  it('rejects DROP TABLE', () => {
    expect(() => validateSqlQuery('DROP TABLE users')).toThrow('dangerous pattern: DROP');
  });

  it('rejects DROP DATABASE', () => {
    expect(() => validateSqlQuery('DROP DATABASE mydb')).toThrow('dangerous pattern: DROP');
  });

  it('rejects EXEC', () => {
    expect(() => validateSqlQuery('EXEC sp_executesql @sql')).toThrow('dangerous pattern: EXEC');
  });

  it('rejects xp_cmdshell', () => {
    // Note: EXEC is caught first in this example, but xp_cmdshell is still a dangerous pattern
    expect(() => validateSqlQuery('EXEC xp_cmdshell "dir"')).toThrow('dangerous pattern');
  });

  it('rejects TRUNCATE', () => {
    expect(() => validateSqlQuery('TRUNCATE TABLE logs')).toThrow('dangerous pattern: TRUNCATE');
  });

  it('is case-insensitive for dangerous patterns', () => {
    expect(() => validateSqlQuery('drop table users')).toThrow('dangerous pattern');
    expect(() => validateSqlQuery('DrOp TaBlE users')).toThrow('dangerous pattern');
  });

  it('rejects oversized queries', () => {
    const huge = 'SELECT * FROM ' + 'x'.repeat(10_000);
    expect(() => validateSqlQuery(huge)).toThrow('exceeds maximum length (10000 chars)');
  });

  it('accepts queries at the size limit', () => {
    const maxSize = 'SELECT * FROM ' + 'x'.repeat(9_986); // Total 10000 chars
    expect(() => validateSqlQuery(maxSize)).not.toThrow();
  });

  it('handles empty strings', () => {
    // Empty query should be allowed (might be validated by underlying storage)
    expect(() => validateSqlQuery('')).not.toThrow();
  });

  it('handles Unicode characters', () => {
    expect(() => validateSqlQuery('SELECT * FROM users WHERE name = "François"')).not.toThrow();
    expect(() => validateSqlQuery('SELECT * FROM 用户表 WHERE 名字 = ?')).not.toThrow();
  });
});

// ============================================================================
// SQL Parameter Validation Tests
// ============================================================================

describe('validateSqlParams', () => {
  it('allows undefined params', () => {
    expect(() => validateSqlParams(undefined)).not.toThrow();
  });

  it('allows empty params array', () => {
    expect(() => validateSqlParams([])).not.toThrow();
  });

  it('allows valid params', () => {
    expect(() => validateSqlParams([1, 'test', null])).not.toThrow();
  });

  it('allows numbers', () => {
    expect(() => validateSqlParams([42, 3.14, -100, 0])).not.toThrow();
  });

  it('allows strings', () => {
    expect(() => validateSqlParams(['hello', 'world', ''])).not.toThrow();
  });

  it('allows null values', () => {
    expect(() => validateSqlParams([null, null])).not.toThrow();
  });

  it('allows Uint8Array', () => {
    const blob = new Uint8Array([1, 2, 3, 4]);
    expect(() => validateSqlParams([blob])).not.toThrow();
  });

  it('rejects excessive parameter count', () => {
    const tooMany = new Array(101).fill(1);
    expect(() => validateSqlParams(tooMany)).toThrow('parameter count exceeds maximum (100)');
  });

  it('allows max parameter count', () => {
    const maxParams = new Array(100).fill(1);
    expect(() => validateSqlParams(maxParams)).not.toThrow();
  });

  it('rejects oversized string parameters', () => {
    const huge = 'x'.repeat(10_001);
    expect(() => validateSqlParams([huge])).toThrow('exceeds maximum string length (10000 bytes)');
  });

  it('allows string parameters at size limit', () => {
    const maxSize = 'x'.repeat(10_000);
    expect(() => validateSqlParams([maxSize])).not.toThrow();
  });

  it('rejects oversized blob parameters', () => {
    const hugeBlob = new Uint8Array(1_048_577); // 1MB + 1 byte
    expect(() => validateSqlParams([hugeBlob])).toThrow('exceeds maximum blob size (1MB)');
  });

  it('allows blob parameters at size limit', () => {
    const maxBlob = new Uint8Array(1_048_576); // Exactly 1MB
    expect(() => validateSqlParams([maxBlob])).not.toThrow();
  });

  it('validates Unicode string sizes correctly', () => {
    // Unicode characters can be multiple bytes
    const unicode = 'François'.repeat(1250); // Each char ~8 bytes = ~10KB
    expect(() => validateSqlParams([unicode])).toThrow('exceeds maximum string length');
  });

  it('provides parameter index in error messages', () => {
    const huge = 'x'.repeat(10_001);
    expect(() => validateSqlParams(['ok', huge])).toThrow('SQL parameter 1');
  });
});

// ============================================================================
// Batch Size Validation Tests
// ============================================================================

describe('validateBatchSize', () => {
  it('allows valid batch sizes', () => {
    expect(() => validateBatchSize(1)).not.toThrow();
    expect(() => validateBatchSize(10)).not.toThrow();
    expect(() => validateBatchSize(25)).not.toThrow();
  });

  it('rejects batch size > 25', () => {
    expect(() => validateBatchSize(26)).toThrow('exceeds maximum size (25 statements): got 26');
  });

  it('rejects batch size = 0', () => {
    expect(() => validateBatchSize(0)).toThrow('must contain at least 1 statement');
  });

  it('rejects negative batch sizes', () => {
    expect(() => validateBatchSize(-1)).toThrow('must contain at least 1 statement');
  });
});

// ============================================================================
// KV Key Validation Tests
// ============================================================================

describe('validateKvKey', () => {
  it('allows valid keys', () => {
    expect(() => validateKvKey('user:123')).not.toThrow();
    expect(() => validateKvKey('session/abc-def-ghi')).not.toThrow();
    expect(() => validateKvKey('cache_key_v1')).not.toThrow();
  });

  it('allows empty keys', () => {
    // Edge case - let underlying storage decide
    expect(() => validateKvKey('')).not.toThrow();
  });

  it('allows Unicode keys', () => {
    expect(() => validateKvKey('user:François')).not.toThrow();
    expect(() => validateKvKey('用户:123')).not.toThrow();
  });

  it('rejects oversized keys', () => {
    const huge = 'x'.repeat(513);
    expect(() => validateKvKey(huge)).toThrow('exceeds maximum size (512 bytes)');
  });

  it('allows keys at size limit', () => {
    const maxSize = 'x'.repeat(512);
    expect(() => validateKvKey(maxSize)).not.toThrow();
  });

  it('validates Unicode key sizes correctly', () => {
    // Unicode can exceed byte limit while staying under char limit
    const unicode = '用'.repeat(171); // Each char is 3 bytes = 513 bytes
    expect(() => validateKvKey(unicode)).toThrow('exceeds maximum size (512 bytes)');
  });

  it('rejects path traversal with ../', () => {
    expect(() => validateKvKey('../etc/passwd')).toThrow('path traversal sequence');
    expect(() => validateKvKey('user/../admin')).toThrow('path traversal sequence');
    expect(() => validateKvKey('../../secret')).toThrow('path traversal sequence');
  });

  it('rejects path traversal with ..\\', () => {
    expect(() => validateKvKey('..\\windows\\system32')).toThrow('path traversal sequence');
    expect(() => validateKvKey('user\\..\\admin')).toThrow('path traversal sequence');
  });

  it('rejects control characters', () => {
    expect(() => validateKvKey('key\x00null')).toThrow('contains control characters');
    expect(() => validateKvKey('key\x01soh')).toThrow('contains control characters');
    expect(() => validateKvKey('key\x1Funit')).toThrow('contains control characters');
    expect(() => validateKvKey('key\x7Fdel')).toThrow('contains control characters');
  });

  it('allows normal special characters', () => {
    expect(() => validateKvKey('user:123-abc_def.json')).not.toThrow();
    expect(() => validateKvKey('path/to/key')).not.toThrow();
    expect(() => validateKvKey('key@example.com')).not.toThrow();
  });
});

// ============================================================================
// KV Value Validation Tests
// ============================================================================

describe('validateKvValue', () => {
  it('allows valid string values', () => {
    expect(() => validateKvValue('hello world')).not.toThrow();
    expect(() => validateKvValue(JSON.stringify({ user: 'test' }))).not.toThrow();
  });

  it('allows valid ArrayBuffer values', () => {
    const buffer = new ArrayBuffer(1024);
    expect(() => validateKvValue(buffer)).not.toThrow();
  });

  it('allows ReadableStream values without validation', () => {
    const stream = new ReadableStream({
      start(controller) {
        controller.enqueue(new Uint8Array([1, 2, 3]));
        controller.close();
      },
    });
    expect(() => validateKvValue(stream)).not.toThrow();
  });

  it('rejects oversized string values', () => {
    const huge = 'x'.repeat(25 * 1024 * 1024 + 1); // > 25MB
    expect(() => validateKvValue(huge)).toThrow('exceeds maximum size (25MB)');
  });

  it('rejects oversized ArrayBuffer values', () => {
    const huge = new ArrayBuffer(25 * 1024 * 1024 + 1); // > 25MB
    expect(() => validateKvValue(huge)).toThrow('exceeds maximum size (25MB)');
  });

  it('allows values at size limit', () => {
    const maxString = 'x'.repeat(25 * 1024 * 1024); // Exactly 25MB
    const maxBuffer = new ArrayBuffer(25 * 1024 * 1024);
    expect(() => validateKvValue(maxString)).not.toThrow();
    expect(() => validateKvValue(maxBuffer)).not.toThrow();
  });

  it('allows empty values', () => {
    expect(() => validateKvValue('')).not.toThrow();
    expect(() => validateKvValue(new ArrayBuffer(0))).not.toThrow();
  });
});

// ============================================================================
// ValidatedSqlStorage Tests
// ============================================================================

describe('ValidatedSqlStorage', () => {
  const mockResult: SqlResult = {
    columns: ['id', 'name'],
    rows: [[1, 'test']],
    rowsAffected: 1,
  };

  function createMockStorage(): ISqlStorage {
    return {
      execute: mock(() => Promise.resolve(mockResult)),
      batch: mock(() => Promise.resolve([mockResult])),
    };
  }

  describe('execute', () => {
    it('validates and forwards valid queries', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const result = await validated.execute('SELECT * FROM users WHERE id = ?', [123]);

      expect(result).toBe(mockResult);
      expect(mock.execute).toHaveBeenCalledWith('SELECT * FROM users WHERE id = ?', [123]);
    });

    it('rejects dangerous queries before calling inner storage', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      await expect(validated.execute('DROP TABLE users')).rejects.toThrow('dangerous pattern');
      expect(mock.execute).not.toHaveBeenCalled();
    });

    it('rejects oversized queries before calling inner storage', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const huge = 'SELECT * FROM ' + 'x'.repeat(10_000);
      await expect(validated.execute(huge)).rejects.toThrow('exceeds maximum length');
      expect(mock.execute).not.toHaveBeenCalled();
    });

    it('rejects invalid parameters before calling inner storage', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const tooMany = new Array(101).fill(1);
      await expect(validated.execute('SELECT * FROM users', tooMany)).rejects.toThrow('parameter count');
      expect(mock.execute).not.toHaveBeenCalled();
    });

    it('allows queries without parameters', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      await validated.execute('SELECT * FROM users');
      expect(mock.execute).toHaveBeenCalledWith('SELECT * FROM users', undefined);
    });
  });

  describe('batch', () => {
    it('validates and forwards valid batches', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const statements = [
        { sql: 'INSERT INTO users (name) VALUES (?)', params: ['Alice' as SqlValue] },
        { sql: 'INSERT INTO users (name) VALUES (?)', params: ['Bob' as SqlValue] },
      ];

      const result = await validated.batch(statements);

      expect(result).toEqual([mockResult]);
      expect(mock.batch).toHaveBeenCalledWith(statements);
    });

    it('rejects oversized batches', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const statements = Array(26).fill({ sql: 'SELECT 1' });
      await expect(validated.batch(statements)).rejects.toThrow('exceeds maximum size (25 statements)');
      expect(mock.batch).not.toHaveBeenCalled();
    });

    it('rejects batches with dangerous queries', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const statements = [
        { sql: 'SELECT * FROM users' },
        { sql: 'DROP TABLE users' },
      ];

      await expect(validated.batch(statements)).rejects.toThrow('dangerous pattern');
      expect(mock.batch).not.toHaveBeenCalled();
    });

    it('rejects batches with invalid parameters', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const statements = [
        { sql: 'SELECT * FROM users', params: [1] as SqlValue[] },
        { sql: 'SELECT * FROM posts', params: new Array(101).fill(1) as SqlValue[] },
      ];

      await expect(validated.batch(statements)).rejects.toThrow('parameter count');
      expect(mock.batch).not.toHaveBeenCalled();
    });

    it('allows empty parameter arrays', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const statements = [{ sql: 'SELECT * FROM users', params: [] as SqlValue[] }];
      await validated.batch(statements);
      expect(mock.batch).toHaveBeenCalledWith(statements);
    });

    it('allows statements without params field', async () => {
      const mock = createMockStorage();
      const validated = new ValidatedSqlStorage(mock);

      const statements = [{ sql: 'SELECT * FROM users' }];
      await validated.batch(statements);
      expect(mock.batch).toHaveBeenCalledWith(statements);
    });
  });
});

// ============================================================================
// ValidatedKvStorage Tests
// ============================================================================

describe('ValidatedKvStorage', () => {
  function createMockKvStorage(): IKeyValueStorage {
    return {
      get: mock(() => Promise.resolve('value')),
      put: mock(() => Promise.resolve(undefined)),
      delete: mock(() => Promise.resolve(undefined)),
      list: mock(() => Promise.resolve({ keys: [] })),
    };
  }

  describe('get', () => {
    it('validates and forwards valid keys', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      const result = await validated.get('user:123');

      expect(result).toBe('value');
      expect(mock.get).toHaveBeenCalledWith('user:123', undefined);
    });

    it('rejects invalid keys before calling inner storage', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await expect(validated.get('../etc/passwd')).rejects.toThrow('path traversal');
      expect(mock.get).not.toHaveBeenCalled();
    });

    it('forwards options correctly', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await validated.get('key', { type: 'json' });
      expect(mock.get).toHaveBeenCalledWith('key', { type: 'json' });
    });
  });

  describe('put', () => {
    it('validates and forwards valid operations', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await validated.put('user:123', 'value');

      expect(mock.put).toHaveBeenCalledWith('user:123', 'value', undefined);
    });

    it('rejects invalid keys', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await expect(validated.put('../etc/passwd', 'value')).rejects.toThrow('path traversal');
      expect(mock.put).not.toHaveBeenCalled();
    });

    it('rejects oversized values', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      const huge = 'x'.repeat(25 * 1024 * 1024 + 1);
      await expect(validated.put('key', huge)).rejects.toThrow('exceeds maximum size');
      expect(mock.put).not.toHaveBeenCalled();
    });

    it('allows ReadableStream values', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      const stream = new ReadableStream();
      await validated.put('key', stream);
      expect(mock.put).toHaveBeenCalledWith('key', stream, undefined);
    });

    it('forwards options correctly', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      const options = { expirationTtl: 3600, metadata: { version: 1 } };
      await validated.put('key', 'value', options);
      expect(mock.put).toHaveBeenCalledWith('key', 'value', options);
    });
  });

  describe('delete', () => {
    it('validates and forwards valid keys', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await validated.delete('user:123');

      expect(mock.delete).toHaveBeenCalledWith('user:123');
    });

    it('rejects invalid keys', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await expect(validated.delete('../etc/passwd')).rejects.toThrow('path traversal');
      expect(mock.delete).not.toHaveBeenCalled();
    });
  });

  describe('list', () => {
    it('forwards list operations without prefix', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      const result = await validated.list();

      expect(result).toEqual({ keys: [] });
      expect(mock.list).toHaveBeenCalledWith(undefined);
    });

    it('validates prefix when provided', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await validated.list({ prefix: 'user:' });
      expect(mock.list).toHaveBeenCalledWith({ prefix: 'user:' });
    });

    it('rejects invalid prefix', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      await expect(validated.list({ prefix: '../etc/' })).rejects.toThrow('path traversal');
      expect(mock.list).not.toHaveBeenCalled();
    });

    it('forwards all options correctly', async () => {
      const mock = createMockKvStorage();
      const validated = new ValidatedKvStorage(mock);

      const options = { prefix: 'user:', limit: 100, cursor: 'abc123' };
      await validated.list(options);
      expect(mock.list).toHaveBeenCalledWith(options);
    });
  });
});
