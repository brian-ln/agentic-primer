/**
 * Integration tests for storage adapters (D1Storage, KVStorage, R2Storage).
 *
 * These tests run inside the Workers runtime via vitest-pool-workers,
 * using miniflare-provided bindings from wrangler.toml:
 * - env.DB (D1Database)
 * - env.KV (KVNamespace)
 * - env.BUCKET (R2Bucket)
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { env } from 'cloudflare:test';
import { D1Storage } from '../storage/d1-storage.ts';
import { KVStorage } from '../storage/kv-storage.ts';
import { R2Storage } from '../storage/r2-storage.ts';

// ---------- D1Storage ----------

describe('D1Storage', () => {
  let storage: D1Storage;

  beforeEach(async () => {
    storage = new D1Storage(env.DB);
    // Create a fresh test table
    await storage.execute(
      'CREATE TABLE IF NOT EXISTS test_items (id TEXT PRIMARY KEY, value TEXT, count INTEGER)'
    );
    // Clean up any leftover data
    await storage.execute('DELETE FROM test_items');
  });

  describe('execute', () => {
    it('inserts and selects a row', async () => {
      await storage.execute(
        'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
        ['1', 'hello', 42]
      );

      const result = await storage.execute(
        'SELECT * FROM test_items WHERE id = ?',
        ['1']
      );
      expect(result.rows).toHaveLength(1);
      expect(result.rows[0]).toEqual({ id: '1', value: 'hello', count: 42 });
    });

    it('returns changes count for mutations', async () => {
      const insertResult = await storage.execute(
        'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
        ['1', 'hello', 1]
      );
      expect(insertResult.changes).toBe(1);
      expect(insertResult.rows).toEqual([]);
    });

    it('returns 0 changes for SELECTs', async () => {
      const result = await storage.execute('SELECT * FROM test_items');
      expect(result.changes).toBe(0);
    });

    it('selects multiple rows', async () => {
      await storage.execute(
        'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
        ['a', 'alpha', 1]
      );
      await storage.execute(
        'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
        ['b', 'beta', 2]
      );
      await storage.execute(
        'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
        ['c', 'gamma', 3]
      );

      const result = await storage.execute(
        'SELECT * FROM test_items ORDER BY id'
      );
      expect(result.rows).toHaveLength(3);
      expect(result.rows[0].id).toBe('a');
      expect(result.rows[1].id).toBe('b');
      expect(result.rows[2].id).toBe('c');
    });

    it('handles UPDATE statements', async () => {
      await storage.execute(
        'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
        ['1', 'original', 10]
      );

      const updateResult = await storage.execute(
        'UPDATE test_items SET value = ?, count = ? WHERE id = ?',
        ['updated', 20, '1']
      );
      expect(updateResult.changes).toBe(1);

      const result = await storage.execute(
        'SELECT * FROM test_items WHERE id = ?',
        ['1']
      );
      expect(result.rows[0].value).toBe('updated');
      expect(result.rows[0].count).toBe(20);
    });

    it('handles DELETE statements', async () => {
      await storage.execute(
        'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
        ['1', 'delete-me', 0]
      );

      const deleteResult = await storage.execute(
        'DELETE FROM test_items WHERE id = ?',
        ['1']
      );
      expect(deleteResult.changes).toBe(1);

      const result = await storage.execute(
        'SELECT * FROM test_items WHERE id = ?',
        ['1']
      );
      expect(result.rows).toHaveLength(0);
    });

    it('works with no params', async () => {
      const result = await storage.execute('SELECT COUNT(*) as cnt FROM test_items');
      expect(result.rows).toHaveLength(1);
      expect(result.rows[0].cnt).toBe(0);
    });
  });

  describe('batch', () => {
    it('executes multiple statements atomically', async () => {
      await storage.batch([
        {
          sql: 'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
          params: ['1', 'first', 1],
        },
        {
          sql: 'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
          params: ['2', 'second', 2],
        },
        {
          sql: 'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
          params: ['3', 'third', 3],
        },
      ]);

      const result = await storage.execute('SELECT COUNT(*) as cnt FROM test_items');
      expect(result.rows[0].cnt).toBe(3);
    });

    it('handles statements with and without params', async () => {
      await storage.batch([
        {
          sql: 'INSERT INTO test_items (id, value, count) VALUES (?, ?, ?)',
          params: ['x', 'val', 99],
        },
        {
          sql: "INSERT INTO test_items (id, value, count) VALUES ('y', 'inline', 0)",
        },
      ]);

      const result = await storage.execute(
        'SELECT * FROM test_items ORDER BY id'
      );
      expect(result.rows).toHaveLength(2);
      expect(result.rows[0].id).toBe('x');
      expect(result.rows[1].id).toBe('y');
    });
  });
});

// ---------- KVStorage ----------

describe('KVStorage', () => {
  let kv: KVStorage;

  beforeEach(() => {
    kv = new KVStorage(env.KV);
  });

  describe('get and put', () => {
    it('puts and gets a text value', async () => {
      await kv.put('key1', 'value1');
      const result = await kv.get('key1');
      expect(result).toBe('value1');
    });

    it('returns null for missing key', async () => {
      const result = await kv.get('nonexistent');
      expect(result).toBeNull();
    });

    it('puts and gets JSON data', async () => {
      const data = { name: 'test', count: 42 };
      await kv.put('json-key', JSON.stringify(data));

      const result = await kv.get<{ name: string; count: number }>('json-key', {
        type: 'json',
      });
      expect(result).toEqual(data);
    });

    it('overwrites existing values', async () => {
      await kv.put('key1', 'original');
      await kv.put('key1', 'updated');
      const result = await kv.get('key1');
      expect(result).toBe('updated');
    });
  });

  describe('delete', () => {
    it('deletes an existing key', async () => {
      await kv.put('to-delete', 'value');
      await kv.delete('to-delete');
      const result = await kv.get('to-delete');
      expect(result).toBeNull();
    });

    it('does not throw when deleting non-existent key', async () => {
      await expect(kv.delete('nonexistent')).resolves.not.toThrow();
    });
  });

  describe('list', () => {
    it('lists keys with prefix filtering', async () => {
      await kv.put('prefix:a', '1');
      await kv.put('prefix:b', '2');
      await kv.put('other:c', '3');

      const result = await kv.list({ prefix: 'prefix:' });
      const names = result.keys.map((k) => k.name);
      expect(names).toContain('prefix:a');
      expect(names).toContain('prefix:b');
      expect(names).not.toContain('other:c');
    });

    it('lists all keys when no prefix', async () => {
      await kv.put('list-all-a', '1');
      await kv.put('list-all-b', '2');

      const result = await kv.list({ prefix: 'list-all-' });
      expect(result.keys.length).toBeGreaterThanOrEqual(2);
    });

    it('respects limit parameter', async () => {
      await kv.put('lim:1', '1');
      await kv.put('lim:2', '2');
      await kv.put('lim:3', '3');

      const result = await kv.list({ prefix: 'lim:', limit: 2 });
      expect(result.keys.length).toBeLessThanOrEqual(2);
    });
  });
});

// ---------- R2Storage ----------

describe('R2Storage', () => {
  let r2: R2Storage;

  beforeEach(() => {
    r2 = new R2Storage(env.BUCKET);
  });

  describe('put and get', () => {
    it('puts and gets a string', async () => {
      await r2.put('test-key', 'hello world');

      const obj = await r2.get('test-key');
      expect(obj).not.toBeNull();

      const text = await new Response(obj!.body).text();
      expect(text).toBe('hello world');
    });

    it('returns null for missing key', async () => {
      const obj = await r2.get('nonexistent');
      expect(obj).toBeNull();
    });

    it('puts and gets with custom metadata', async () => {
      await r2.put('meta-key', 'data', {
        customMetadata: { source: 'test', version: '1' },
      });

      const obj = await r2.get('meta-key');
      expect(obj).not.toBeNull();
      expect(obj!.metadata).toEqual({ source: 'test', version: '1' });
      // Consume the body stream to avoid R2 storage isolation issues
      await obj!.body.cancel();
    });

    it('puts and gets an ArrayBuffer', async () => {
      const data = new Uint8Array([1, 2, 3, 4, 5]);
      await r2.put('binary-key', data.buffer);

      const obj = await r2.get('binary-key');
      expect(obj).not.toBeNull();

      const buf = await new Response(obj!.body).arrayBuffer();
      expect(new Uint8Array(buf)).toEqual(data);
    });

    it('overwrites existing objects', async () => {
      await r2.put('overwrite-key', 'original');
      await r2.put('overwrite-key', 'updated');

      const obj = await r2.get('overwrite-key');
      const text = await new Response(obj!.body).text();
      expect(text).toBe('updated');
    });
  });

  describe('delete', () => {
    it('deletes an existing object', async () => {
      await r2.put('del-key', 'value');
      await r2.delete('del-key');

      const obj = await r2.get('del-key');
      expect(obj).toBeNull();
    });

    it('does not throw when deleting non-existent key', async () => {
      await expect(r2.delete('nonexistent')).resolves.not.toThrow();
    });
  });

  describe('head', () => {
    it('returns metadata for an existing object', async () => {
      await r2.put('head-key', 'some content');

      const meta = await r2.head('head-key');
      expect(meta).not.toBeNull();
      expect(meta!.size).toBe('some content'.length);
      expect(meta!.uploaded).toBeInstanceOf(Date);
    });

    it('returns null for missing key', async () => {
      const meta = await r2.head('nonexistent');
      expect(meta).toBeNull();
    });

    it('reflects httpMetadata when set', async () => {
      await r2.put('head-http-key', 'data', {
        httpMetadata: { contentType: 'application/json' },
      });

      const meta = await r2.head('head-http-key');
      expect(meta).not.toBeNull();
      expect(meta!.httpMetadata?.contentType).toBe('application/json');
    });
  });

  describe('list', () => {
    it('lists objects with prefix filtering', async () => {
      await r2.put('r2list:a', '1');
      await r2.put('r2list:b', '2');
      await r2.put('r2other:c', '3');

      const result = await r2.list({ prefix: 'r2list:' });
      const keys = result.objects.map((o) => o.key);
      expect(keys).toContain('r2list:a');
      expect(keys).toContain('r2list:b');
      expect(keys).not.toContain('r2other:c');
    });

    it('returns object metadata in list results', async () => {
      await r2.put('r2meta:obj', 'test data');

      const result = await r2.list({ prefix: 'r2meta:' });
      expect(result.objects).toHaveLength(1);
      expect(result.objects[0].key).toBe('r2meta:obj');
      expect(result.objects[0].size).toBe('test data'.length);
      expect(result.objects[0].uploaded).toBeInstanceOf(Date);
    });

    it('respects limit parameter', async () => {
      await r2.put('r2lim:1', '1');
      await r2.put('r2lim:2', '2');
      await r2.put('r2lim:3', '3');

      const result = await r2.list({ prefix: 'r2lim:', limit: 2 });
      expect(result.objects.length).toBeLessThanOrEqual(2);
    });

    it('returns empty list for non-matching prefix', async () => {
      const result = await r2.list({ prefix: 'no-match-prefix:' });
      expect(result.objects).toEqual([]);
    });
  });
});
