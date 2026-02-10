/**
 * Test worker entry point for vitest-pool-workers.
 *
 * Exports a Durable Object class (TestDO) that exposes DOPersistence
 * methods via RPC for testing from within the Workers runtime.
 */
import { DurableObject } from 'cloudflare:workers';
import { DOPersistence } from '../do-persistence.ts';

export interface Env {
  DB: D1Database;
  KV: KVNamespace;
  BUCKET: R2Bucket;
  TEST_DO: DurableObjectNamespace<TestDO>;
}

/**
 * Test Durable Object that wraps DOPersistence and exposes its methods
 * for integration testing via RPC (Durable Object stubs in vitest-pool-workers
 * support calling public methods directly).
 */
export class TestDO extends DurableObject<Env> {
  private persistence: DOPersistence;

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    this.persistence = new DOPersistence(ctx.storage);
  }

  async initialize(): Promise<void> {
    await this.persistence.initialize();
  }

  async saveSnapshot(key: string, data: number[]): Promise<void> {
    await this.persistence.saveSnapshot(key, new Uint8Array(data));
  }

  async loadSnapshot(key: string): Promise<number[] | null> {
    const result = await this.persistence.loadSnapshot(key);
    if (result === null) return null;
    return Array.from(result);
  }

  async appendWAL(key: string, entry: number[]): Promise<void> {
    await this.persistence.appendWAL(key, new Uint8Array(entry));
  }

  async replayWAL(key: string): Promise<number[][]> {
    const entries = await this.persistence.replayWAL(key);
    return entries.map((e) => Array.from(e));
  }
}

export default {
  async fetch(_request: Request, _env: Env): Promise<Response> {
    return new Response('Test worker');
  },
};
