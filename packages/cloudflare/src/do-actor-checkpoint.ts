/**
 * DO SQLite-based actor checkpoint for the actor system.
 *
 * Implements IActorCheckpoint from @agentic-primer/actors using Durable Object
 * storage's embedded SQLite database (ctx.storage.sql).
 *
 * Uses two tables:
 * - `_actor_snapshots`: Key-value store for serialized actor state snapshots
 * - `_actor_wal`: Write-ahead log for event-sourcing replay
 *
 * All SQL operations via storage.sql.exec() are synchronous within the DO's
 * single-threaded execution model, but the interface returns Promises for
 * compatibility with the portable IActorCheckpoint contract.
 */

import type { IActorCheckpoint } from '@agentic-primer/actors';

export class DOActorCheckpoint implements IActorCheckpoint {
  private readonly storage: DurableObjectStorage;

  constructor(storage: DurableObjectStorage) {
    this.storage = storage;
  }

  /**
   * Create the snapshot and WAL tables if they don't exist.
   * Called once during DOActorSystem initialization inside blockConcurrencyWhile.
   */
  async initialize(): Promise<void> {
    this.storage.sql.exec(`
      CREATE TABLE IF NOT EXISTS _actor_snapshots (
        key TEXT PRIMARY KEY,
        data BLOB NOT NULL
      )
    `);

    this.storage.sql.exec(`
      CREATE TABLE IF NOT EXISTS _actor_wal (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        key TEXT NOT NULL,
        seq INTEGER NOT NULL,
        entry BLOB NOT NULL
      )
    `);

    // Index for efficient WAL replay by key
    this.storage.sql.exec(`
      CREATE INDEX IF NOT EXISTS idx_actor_wal_key_seq
      ON _actor_wal (key, seq ASC)
    `);
  }

  /**
   * Load the most recent snapshot for the given key.
   * Returns null if no snapshot exists.
   */
  async loadSnapshot(key: string): Promise<Uint8Array | null> {
    const cursor = this.storage.sql.exec(
      'SELECT data FROM _actor_snapshots WHERE key = ?',
      key
    );
    const rows = [...cursor];
    if (rows.length === 0) {
      return null;
    }
    const data = rows[0].data as ArrayBuffer;
    return new Uint8Array(data);
  }

  /**
   * Save a snapshot for the given key, replacing any existing snapshot.
   */
  async saveSnapshot(key: string, data: Uint8Array): Promise<void> {
    // Convert Uint8Array to ArrayBuffer for DO SQLite BLOB storage
    const buffer = data.buffer.slice(
      data.byteOffset,
      data.byteOffset + data.byteLength
    );
    this.storage.sql.exec(
      'INSERT OR REPLACE INTO _actor_snapshots (key, data) VALUES (?, ?)',
      key,
      buffer
    );
  }

  /**
   * Append a WAL entry for the given key.
   * Sequence numbers auto-increment per key using MAX(seq).
   */
  async appendWAL(key: string, entry: Uint8Array): Promise<void> {
    const buffer = entry.buffer.slice(
      entry.byteOffset,
      entry.byteOffset + entry.byteLength
    );
    // Get next sequence number for this key
    const seqCursor = this.storage.sql.exec(
      'SELECT COALESCE(MAX(seq), 0) + 1 AS next_seq FROM _actor_wal WHERE key = ?',
      key
    );
    const seqRows = [...seqCursor];
    const nextSeq = (seqRows[0].next_seq as number) || 1;

    this.storage.sql.exec(
      'INSERT INTO _actor_wal (key, seq, entry) VALUES (?, ?, ?)',
      key,
      nextSeq,
      buffer
    );
  }

  /**
   * Replay all WAL entries for the given key in sequence order.
   * Returns an empty array if no entries exist.
   */
  async replayWAL(key: string): Promise<Uint8Array[]> {
    const cursor = this.storage.sql.exec(
      'SELECT entry FROM _actor_wal WHERE key = ? ORDER BY seq ASC',
      key
    );
    const entries: Uint8Array[] = [];
    for (const row of cursor) {
      entries.push(new Uint8Array(row.entry as ArrayBuffer));
    }
    return entries;
  }
}
