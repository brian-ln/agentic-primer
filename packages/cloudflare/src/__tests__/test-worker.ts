/**
 * Test worker entry point for vitest-pool-workers.
 *
 * Exports a Durable Object class (TestDO) that exposes DOActorCheckpoint
 * methods via RPC for testing from within the Workers runtime.
 */
import { DurableObject } from 'cloudflare:workers';
import { DOActorCheckpoint } from '../do-actor-checkpoint.ts';

export interface Env {
  DB: D1Database;
  KV: KVNamespace;
  BUCKET: R2Bucket;
  TEST_DO: DurableObjectNamespace<TestDO>;
}

/**
 * Test Durable Object that wraps DOActorCheckpoint and exposes its methods
 * for integration testing via RPC (Durable Object stubs in vitest-pool-workers
 * support calling public methods directly).
 */
export class TestDO extends DurableObject<Env> {
  private checkpoint: DOActorCheckpoint;

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    this.checkpoint = new DOActorCheckpoint(ctx.storage);
  }

  async initialize(): Promise<void> {
    await this.checkpoint.initialize();
  }

  async saveSnapshot(key: string, data: number[]): Promise<void> {
    await this.checkpoint.saveSnapshot(key, new Uint8Array(data));
  }

  async loadSnapshot(key: string): Promise<number[] | null> {
    const result = await this.checkpoint.loadSnapshot(key);
    if (result === null) return null;
    return Array.from(result);
  }

  async appendWAL(key: string, entry: number[]): Promise<void> {
    await this.checkpoint.appendWAL(key, new Uint8Array(entry));
  }

  async replayWAL(key: string): Promise<number[][]> {
    const entries = await this.checkpoint.replayWAL(key);
    return entries.map((e) => Array.from(e));
  }
}

export default {
  async fetch(_request: Request, _env: Env): Promise<Response> {
    return new Response('Test worker');
  },
};
