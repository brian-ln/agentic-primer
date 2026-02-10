/**
 * LibSqlVectorStore - IVectorStore adapter for LibSQL native vectors
 *
 * Uses LibSQL's F32_BLOB type, vector_distance_cos(), and DiskANN indexes
 * for efficient similarity search. Stores vectors in a single
 * `vector_store` table with id, vector, and optional metadata.
 *
 * The table and index are auto-created on first use via ensureTable().
 */

import type { Client } from '@libsql/client';
import type { IVectorStore } from '@agentic-primer/actors';

export interface LibSqlVectorStoreConfig {
  /** Table name to use (default: 'vector_store') */
  tableName?: string;
  /** Vector dimensions (default: 768) */
  dimensions?: number;
}

export class LibSqlVectorStore implements IVectorStore {
  private readonly tableName: string;
  private readonly dimensions: number;
  private initialized = false;

  constructor(
    private readonly client: Client,
    config?: LibSqlVectorStoreConfig
  ) {
    this.tableName = config?.tableName ?? 'vector_store';
    this.dimensions = config?.dimensions ?? 768;
  }

  private async ensureTable(): Promise<void> {
    if (this.initialized) return;

    await this.client.batch([
      {
        sql: `CREATE TABLE IF NOT EXISTS ${this.tableName} (
          id TEXT PRIMARY KEY,
          embedding F32_BLOB(${this.dimensions}),
          metadata TEXT
        )`,
        args: [],
      },
      {
        sql: `CREATE INDEX IF NOT EXISTS idx_${this.tableName}_vec
              ON ${this.tableName} (libsql_vector_idx(embedding))`,
        args: [],
      },
    ]);

    this.initialized = true;
  }

  async index(
    id: string,
    vector: number[],
    metadata?: Record<string, string>
  ): Promise<void> {
    await this.ensureTable();

    const vectorJson = `[${vector.join(',')}]`;
    const metaJson = metadata ? JSON.stringify(metadata) : null;

    await this.client.execute({
      sql: `INSERT INTO ${this.tableName} (id, embedding, metadata)
            VALUES (?, vector(?), ?)
            ON CONFLICT(id) DO UPDATE SET
              embedding = excluded.embedding,
              metadata = excluded.metadata`,
      args: [id, vectorJson, metaJson],
    });
  }

  async query(
    vector: number[],
    options?: {
      limit?: number;
      minSimilarity?: number;
      filter?: Record<string, string>;
    }
  ): Promise<Array<{ id: string; similarity: number; metadata?: Record<string, string> }>> {
    await this.ensureTable();

    const limit = options?.limit ?? 10;
    const minSimilarity = options?.minSimilarity ?? 0;
    const vectorJson = `[${vector.join(',')}]`;

    // vector_distance_cos returns distance (0 = identical), convert to similarity (1 - distance)
    const result = await this.client.execute({
      sql: `SELECT id, metadata,
                   vector_distance_cos(embedding, vector(?)) as distance
            FROM ${this.tableName}
            WHERE embedding IS NOT NULL
            ORDER BY distance ASC
            LIMIT ?`,
      args: [vectorJson, limit * 2], // fetch extra for post-filter
    });

    const results: Array<{ id: string; similarity: number; metadata?: Record<string, string> }> = [];

    for (const row of result.rows) {
      const distance = row.distance as number;
      const similarity = 1 - distance;

      if (similarity < minSimilarity) continue;

      // Apply metadata filter if specified
      if (options?.filter) {
        const metaStr = row.metadata as string | null;
        if (!metaStr) continue;
        try {
          const meta = JSON.parse(metaStr) as Record<string, string>;
          const matches = Object.entries(options.filter).every(
            ([k, v]) => meta[k] === v
          );
          if (!matches) continue;
        } catch {
          continue;
        }
      }

      let parsedMeta: Record<string, string> | undefined;
      if (row.metadata) {
        try {
          parsedMeta = JSON.parse(row.metadata as string);
        } catch {
          // ignore malformed metadata
        }
      }

      results.push({
        id: row.id as string,
        similarity,
        metadata: parsedMeta,
      });

      if (results.length >= limit) break;
    }

    return results;
  }

  async delete(id: string): Promise<void> {
    await this.ensureTable();

    await this.client.execute({
      sql: `DELETE FROM ${this.tableName} WHERE id = ?`,
      args: [id],
    });
  }

  /** Close the underlying libsql client connection. */
  close(): void {
    this.client.close();
  }
}
