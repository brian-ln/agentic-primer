/**
 * Integration tests for MiniflareProvisioner.
 *
 * Runs in Node.js (not pool-workers). Tests that provisioning creates
 * working Miniflare instances with D1, KV, R2, and Durable Object bindings.
 */
import { describe, it, expect, afterEach } from 'vitest';
import { MiniflareProvisioner } from '../provisioner/miniflare-provisioner.ts';
import type { SystemManifest } from '../provisioner/types.ts';

// Minimal worker module that exports a fetch handler and a test DO
const MINIMAL_WORKER = `
export default {
  async fetch(request, env) {
    return new Response('ok');
  }
};
`;

// Worker with a simple Durable Object that stores/retrieves values via SQLite
const DO_WORKER = `
import { DurableObject } from 'cloudflare:workers';

export class TestDO extends DurableObject {
  async fetch(request) {
    const url = new URL(request.url);

    if (request.method === 'POST') {
      const body = await request.json();
      this.ctx.storage.sql.exec(
        'CREATE TABLE IF NOT EXISTS data (key TEXT PRIMARY KEY, value TEXT)'
      );
      this.ctx.storage.sql.exec(
        'INSERT OR REPLACE INTO data (key, value) VALUES (?, ?)',
        body.key, body.value
      );
      return new Response(JSON.stringify({ ok: true }), {
        headers: { 'Content-Type': 'application/json' },
      });
    }

    if (request.method === 'GET') {
      const key = url.searchParams.get('key');
      this.ctx.storage.sql.exec(
        'CREATE TABLE IF NOT EXISTS data (key TEXT PRIMARY KEY, value TEXT)'
      );
      const rows = [...this.ctx.storage.sql.exec(
        'SELECT value FROM data WHERE key = ?',
        key
      )];
      if (rows.length === 0) {
        return new Response('null', {
          headers: { 'Content-Type': 'application/json' },
        });
      }
      return new Response(JSON.stringify(rows[0].value), {
        headers: { 'Content-Type': 'application/json' },
      });
    }

    return new Response('Method not allowed', { status: 405 });
  }
}

export default {
  async fetch(request, env) {
    const url = new URL(request.url);

    if (url.pathname.startsWith('/do/')) {
      const id = env.TEST_DO.idFromName('singleton');
      const stub = env.TEST_DO.get(id);
      // Forward the request to the DO
      return stub.fetch(request);
    }

    return new Response('ok');
  }
};
`;

describe('MiniflareProvisioner', () => {
  let provisioner: MiniflareProvisioner;

  afterEach(async () => {
    if (provisioner) {
      await provisioner.dispose();
    }
  });

  describe('provision and getBinding', () => {
    it('provisions D1 database', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-d1',
        namespace: 'test',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          { type: 'd1', binding: 'DB' },
        ],
      };

      const resources = await provisioner.provision(manifest);
      expect(resources).toHaveLength(1);
      expect(resources[0]).toMatchObject({
        type: 'd1',
        binding: 'DB',
        namespace: 'test',
      });

      // Get D1 binding and use it
      const db = await provisioner.getBinding<D1Database>('DB');
      await db.exec('CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT)');
      await db.exec("INSERT INTO test (name) VALUES ('hello')");
      const result = await db.prepare('SELECT name FROM test').first<{ name: string }>();
      expect(result?.name).toBe('hello');
    });

    it('provisions KV namespace', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-kv',
        namespace: 'test',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          { type: 'kv', binding: 'KV' },
        ],
      };

      const resources = await provisioner.provision(manifest);
      expect(resources).toHaveLength(1);
      expect(resources[0]).toMatchObject({ type: 'kv', binding: 'KV' });

      const kv = await provisioner.getBinding<KVNamespace>('KV');
      await kv.put('greeting', 'world');
      const value = await kv.get('greeting');
      expect(value).toBe('world');
    });

    it('provisions R2 bucket', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-r2',
        namespace: 'test',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          { type: 'r2', binding: 'BUCKET' },
        ],
      };

      const resources = await provisioner.provision(manifest);
      expect(resources).toHaveLength(1);
      expect(resources[0]).toMatchObject({ type: 'r2', binding: 'BUCKET' });

      const bucket = await provisioner.getBinding<R2Bucket>('BUCKET');
      await bucket.put('test-key', 'test-data');
      const obj = await bucket.get('test-key');
      expect(obj).not.toBeNull();
      const text = await obj!.text();
      expect(text).toBe('test-data');
    });

    it('provisions multiple resources in one manifest', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-multi',
        namespace: 'test',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          { type: 'd1', binding: 'DB' },
          { type: 'kv', binding: 'KV' },
          { type: 'r2', binding: 'BUCKET' },
        ],
      };

      const resources = await provisioner.provision(manifest);
      expect(resources).toHaveLength(3);

      // All bindings should work
      const db = await provisioner.getBinding<D1Database>('DB');
      const kv = await provisioner.getBinding<KVNamespace>('KV');
      const bucket = await provisioner.getBinding<R2Bucket>('BUCKET');

      await db.exec('CREATE TABLE t (id INTEGER PRIMARY KEY)');
      await kv.put('k', 'v');
      await bucket.put('f', 'data');

      const dbResult = await db.prepare('SELECT COUNT(*) as c FROM t').first<{ c: number }>();
      expect(dbResult?.c).toBe(0);
      expect(await kv.get('k')).toBe('v');
      expect(await (await bucket.get('f'))!.text()).toBe('data');
    });
  });

  describe('Durable Objects', () => {
    it('provisions a Durable Object with SQLite', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-do',
        namespace: 'test',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: DO_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          {
            type: 'do',
            binding: 'TEST_DO',
            config: { className: 'TestDO', useSQLite: true },
          },
        ],
      };

      const resources = await provisioner.provision(manifest);
      expect(resources).toHaveLength(1);
      expect(resources[0]).toMatchObject({
        type: 'do',
        binding: 'TEST_DO',
        id: 'TestDO',
      });

      // Use the worker's fetch handler to interact with the DO
      const worker = await provisioner.getWorker('test-do');

      // Store a value in the DO
      const storeRes = await worker.fetch('http://localhost/do/test', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ key: 'name', value: 'actor-system' }),
      });
      expect(storeRes.ok).toBe(true);

      // Read it back
      const readRes = await worker.fetch('http://localhost/do/test?key=name');
      const value = await readRes.json();
      expect(value).toBe('actor-system');
    });

    it('DO state persists across requests to the same instance', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-do-persist',
        namespace: 'test',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: DO_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          {
            type: 'do',
            binding: 'TEST_DO',
            config: { className: 'TestDO', useSQLite: true },
          },
        ],
      };

      await provisioner.provision(manifest);
      const worker = await provisioner.getWorker('test-do-persist');

      // Write multiple values
      await worker.fetch('http://localhost/do/test', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ key: 'a', value: '1' }),
      });
      await worker.fetch('http://localhost/do/test', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ key: 'b', value: '2' }),
      });

      // Both values should be readable
      const resA = await worker.fetch('http://localhost/do/test?key=a');
      expect(await resA.json()).toBe('1');

      const resB = await worker.fetch('http://localhost/do/test?key=b');
      expect(await resB.json()).toBe('2');
    });
  });

  describe('resource tracking', () => {
    it('tracks provisioned resources', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-tracking',
        namespace: 'ns1',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          { type: 'd1', binding: 'DB' },
          { type: 'kv', binding: 'CACHE' },
        ],
      };

      await provisioner.provision(manifest);

      const all = provisioner.resources();
      expect(all).toHaveLength(2);
      expect(all.map((r) => r.binding).sort()).toEqual(['CACHE', 'DB']);

      const ns1 = provisioner.resources('ns1');
      expect(ns1).toHaveLength(2);

      const ns2 = provisioner.resources('ns2');
      expect(ns2).toHaveLength(0);
    });

    it('deprovisioning removes resources by namespace', async () => {
      provisioner = new MiniflareProvisioner();

      // Provision first worker
      await provisioner.provision({
        name: 'worker-a',
        namespace: 'ns-a',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [{ type: 'kv', binding: 'KV_A' }],
      });

      expect(provisioner.resources()).toHaveLength(1);

      // Deprovision — since it's the only worker, this disposes miniflare
      await provisioner.deprovision('ns-a');
      expect(provisioner.resources()).toHaveLength(0);
    });
  });

  describe('error handling', () => {
    it('throws on DO without className', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-err',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
        },
        resources: [
          { type: 'do', binding: 'BAD_DO' },
        ],
      };

      await expect(provisioner.provision(manifest)).rejects.toThrow(
        "DO resource 'BAD_DO' requires config.className"
      );
    });

    it('throws on getBinding before provision', async () => {
      provisioner = new MiniflareProvisioner();
      await expect(provisioner.getBinding('DB')).rejects.toThrow(
        'No Miniflare instance'
      );
    });
  });

  describe('idempotent provisioning', () => {
    it('re-provisioning the same manifest updates the instance', async () => {
      provisioner = new MiniflareProvisioner();
      const manifest: SystemManifest = {
        name: 'test-idem',
        namespace: 'test',
        worker: {
          modules: [{ type: 'ESModule', path: 'index.mjs', contents: MINIMAL_WORKER }],
          compatibilityDate: '2024-12-01',
        },
        resources: [
          { type: 'kv', binding: 'KV' },
        ],
      };

      // First provision
      await provisioner.provision(manifest);
      const kv1 = await provisioner.getBinding<KVNamespace>('KV');
      await kv1.put('key', 'first');

      // Re-provision (same manifest)
      await provisioner.provision(manifest);
      // Note: in-memory state is reset by setOptions, but that's expected
      // The point is it doesn't throw
      expect(provisioner.resources()).toHaveLength(1);
    });
  });
});
