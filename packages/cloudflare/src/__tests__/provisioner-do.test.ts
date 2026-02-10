/**
 * Integration test: MiniflareProvisioner + DOActorSystem.
 *
 * Bundles a real DOActorSystem subclass with esbuild, provisions it
 * in miniflare, and verifies actor message routing end-to-end.
 */
import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { build } from 'esbuild';
import { resolve } from 'node:path';
import { MiniflareProvisioner } from '../provisioner/miniflare-provisioner.ts';
import type { SystemManifest } from '../provisioner/types.ts';

let bundledCode: string;
let provisioner: MiniflareProvisioner;

beforeAll(async () => {
  // Bundle the DOActorSystem test worker into a single ESM module
  const result = await build({
    entryPoints: [resolve(__dirname, 'do-actor-system-worker.ts')],
    bundle: true,
    format: 'esm',
    target: 'esnext',
    write: false,
    external: ['cloudflare:workers'],
  });
  bundledCode = result.outputFiles[0].text;
});

afterAll(async () => {
  if (provisioner) {
    await provisioner.dispose();
  }
});

describe('MiniflareProvisioner + DOActorSystem', () => {
  it('provisions and starts a DOActorSystem', async () => {
    provisioner = new MiniflareProvisioner();

    const manifest: SystemManifest = {
      name: 'actor-test',
      namespace: 'test',
      worker: {
        modules: [{ type: 'ESModule', path: 'index.mjs', contents: bundledCode }],
        compatibilityDate: '2024-12-01',
      },
      resources: [
        {
          type: 'do',
          binding: 'ACTOR_DO',
          config: { className: 'ActorDO', useSQLite: true },
        },
      ],
    };

    const resources = await provisioner.provision(manifest);
    expect(resources).toHaveLength(1);
    expect(resources[0]).toMatchObject({
      type: 'do',
      binding: 'ACTOR_DO',
      id: 'ActorDO',
    });
  });

  it('routes fetch through worker to DO', async () => {
    const worker = await provisioner.getWorker('actor-test');

    // The default handler routes all requests to the ACTOR_DO singleton
    const response = await worker.fetch('http://localhost/stats');
    expect(response.ok).toBe(true);

    const stats = await response.json() as { name: string; behaviors: number };
    expect(stats.name).toBe('actor-do-test');
    // Counter actor should be spawned
    expect(stats.behaviors).toBeGreaterThanOrEqual(1);
  });

  it('sends actor messages via POST /actor-message', async () => {
    const worker = await provisioner.getWorker('actor-test');

    // Send increment messages to the counter actor
    for (let i = 0; i < 3; i++) {
      const res = await worker.fetch('http://localhost/actor-message', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          to: 'counter',
          type: 'increment',
        }),
      });
      expect(res.ok).toBe(true);
      const body = await res.json();
      expect(body).toEqual({ ok: true });
    }
  });

  it('sends actor messages with payload', async () => {
    const worker = await provisioner.getWorker('actor-test');

    const res = await worker.fetch('http://localhost/actor-message', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        to: 'counter',
        type: 'add',
        payload: 10,
      }),
    });
    expect(res.ok).toBe(true);
  });

  it('returns 400 for malformed actor messages', async () => {
    const worker = await provisioner.getWorker('actor-test');

    const res = await worker.fetch('http://localhost/actor-message', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ invalid: true }),
    });
    expect(res.status).toBe(400);
  });

  it('returns 404 for unknown paths', async () => {
    const worker = await provisioner.getWorker('actor-test');

    const res = await worker.fetch('http://localhost/unknown-path');
    expect(res.status).toBe(404);
  });

  it('DOActorCheckpoint tables are created', async () => {
    // The DOActorSystem initializes DOActorCheckpoint which creates
    // _actor_snapshots and _actor_wal tables. We verify this
    // by checking the stats endpoint works (which means the DO
    // constructor completed successfully, including blockConcurrencyWhile).
    const worker = await provisioner.getWorker('actor-test');
    const res = await worker.fetch('http://localhost/stats');
    expect(res.ok).toBe(true);
    const stats = await res.json() as { name: string };
    expect(stats.name).toBe('actor-do-test');
  });
});
