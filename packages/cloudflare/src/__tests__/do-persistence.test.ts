/**
 * Integration tests for DOPersistence.
 *
 * These tests run inside the Workers runtime via vitest-pool-workers.
 * They use a TestDO Durable Object that wraps DOPersistence and exposes
 * its methods via RPC stubs.
 */
import {
  describe,
  it,
  expect,
  beforeEach,
} from 'vitest';
import { env } from 'cloudflare:test';
import type { TestDO } from './test-worker.ts';

function getStub(): DurableObjectStub<TestDO> {
  const id = env.TEST_DO.newUniqueId();
  return env.TEST_DO.get(id);
}

describe('DOPersistence', () => {
  let stub: DurableObjectStub<TestDO>;

  beforeEach(() => {
    // Each test gets a fresh DO instance with a unique ID
    stub = getStub();
  });

  describe('initialize', () => {
    it('creates tables without error', async () => {
      await stub.initialize();
    });

    it('is idempotent (can be called multiple times)', async () => {
      await stub.initialize();
      await stub.initialize();
    });
  });

  describe('saveSnapshot + loadSnapshot', () => {
    it('round-trips a snapshot', async () => {
      await stub.initialize();

      const data = [1, 2, 3, 4, 5];
      await stub.saveSnapshot('actor-1', data);

      const loaded = await stub.loadSnapshot('actor-1');
      expect(loaded).toEqual(data);
    });

    it('returns null for a missing key', async () => {
      await stub.initialize();

      const loaded = await stub.loadSnapshot('nonexistent');
      expect(loaded).toBeNull();
    });

    it('overwrites an existing snapshot', async () => {
      await stub.initialize();

      await stub.saveSnapshot('actor-1', [1, 2, 3]);
      await stub.saveSnapshot('actor-1', [4, 5, 6]);

      const loaded = await stub.loadSnapshot('actor-1');
      expect(loaded).toEqual([4, 5, 6]);
    });

    it('stores different keys independently', async () => {
      await stub.initialize();

      await stub.saveSnapshot('actor-1', [10, 20]);
      await stub.saveSnapshot('actor-2', [30, 40]);

      expect(await stub.loadSnapshot('actor-1')).toEqual([10, 20]);
      expect(await stub.loadSnapshot('actor-2')).toEqual([30, 40]);
    });

    it('handles empty data', async () => {
      await stub.initialize();

      await stub.saveSnapshot('empty', []);
      const loaded = await stub.loadSnapshot('empty');
      expect(loaded).toEqual([]);
    });

    it('handles large payloads', async () => {
      await stub.initialize();

      // 10KB of data
      const largeData = Array.from({ length: 10000 }, (_, i) => i % 256);
      await stub.saveSnapshot('large', largeData);

      const loaded = await stub.loadSnapshot('large');
      expect(loaded).toEqual(largeData);
    });
  });

  describe('appendWAL + replayWAL', () => {
    it('replays entries in order', async () => {
      await stub.initialize();

      await stub.appendWAL('actor-1', [1]);
      await stub.appendWAL('actor-1', [2]);
      await stub.appendWAL('actor-1', [3]);

      const entries = await stub.replayWAL('actor-1');
      expect(entries).toEqual([[1], [2], [3]]);
    });

    it('returns empty array for missing key', async () => {
      await stub.initialize();

      const entries = await stub.replayWAL('nonexistent');
      expect(entries).toEqual([]);
    });

    it('isolates WAL entries by key', async () => {
      await stub.initialize();

      await stub.appendWAL('actor-1', [10]);
      await stub.appendWAL('actor-2', [20]);
      await stub.appendWAL('actor-1', [11]);

      const entries1 = await stub.replayWAL('actor-1');
      expect(entries1).toEqual([[10], [11]]);

      const entries2 = await stub.replayWAL('actor-2');
      expect(entries2).toEqual([[20]]);
    });

    it('handles multiple entries with varied data sizes', async () => {
      await stub.initialize();

      const small = [1];
      const medium = Array.from({ length: 100 }, (_, i) => i % 256);
      const large = Array.from({ length: 1000 }, (_, i) => i % 256);

      await stub.appendWAL('actor-1', small);
      await stub.appendWAL('actor-1', medium);
      await stub.appendWAL('actor-1', large);

      const entries = await stub.replayWAL('actor-1');
      expect(entries).toHaveLength(3);
      expect(entries[0]).toEqual(small);
      expect(entries[1]).toEqual(medium);
      expect(entries[2]).toEqual(large);
    });
  });

  describe('snapshot and WAL independence', () => {
    it('snapshots and WAL entries do not interfere', async () => {
      await stub.initialize();

      await stub.saveSnapshot('actor-1', [100, 200]);
      await stub.appendWAL('actor-1', [1]);
      await stub.appendWAL('actor-1', [2]);

      // Snapshot should be unchanged
      expect(await stub.loadSnapshot('actor-1')).toEqual([100, 200]);

      // WAL should have its entries
      expect(await stub.replayWAL('actor-1')).toEqual([[1], [2]]);
    });
  });
});
