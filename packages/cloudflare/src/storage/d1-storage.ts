/**
 * D1 storage adapter implementing the portable ISqlStorage interface.
 *
 * Wraps Cloudflare D1's API into the WIT-aligned ISqlStorage interface
 * from @agentic-primer/actors. Column names and positional row values
 * are extracted from D1's result shape.
 *
 * Key design considerations:
 * - D1 uses SQLite dialect (same as LibSQL)
 * - D1 batch() provides transaction-like atomicity
 * - D1 does not support BEGIN/COMMIT directly; use batch() instead
 */

import type { ISqlStorage, SqlResult, SqlValue } from '@agentic-primer/actors';

/**
 * D1 adapter implementing ISqlStorage.
 *
 * Usage:
 * ```typescript
 * const storage = new D1Storage(env.DB);
 * const result = await storage.execute(
 *   'SELECT * FROM signals WHERE id = ?',
 *   [signalId]
 * );
 * // result.columns = ['id', 'name', ...]
 * // result.rows = [['abc', 'my-signal', ...], ...]
 * ```
 */
export class D1Storage implements ISqlStorage {
  constructor(private readonly db: D1Database) {}

  async execute(
    sql: string,
    params: SqlValue[] = []
  ): Promise<SqlResult> {
    const stmt = this.db.prepare(sql);
    const bound = params.length > 0 ? stmt.bind(...params) : stmt;

    // Detect SELECT vs mutation to use the appropriate D1 method
    const isSelect = sql.trimStart().toUpperCase().startsWith('SELECT');
    if (isSelect) {
      const result = await bound.all();
      const records = result.results as Record<string, unknown>[];
      const columns = records.length > 0 ? Object.keys(records[0]) : [];
      const rows = records.map((r) => columns.map((c) => r[c] as SqlValue));
      return {
        columns,
        rows,
        rowsAffected: 0,
      };
    } else {
      const result = await bound.run();
      return {
        columns: [],
        rows: [],
        rowsAffected: result.meta.changes ?? 0,
        lastInsertRowid: result.meta.last_row_id ?? undefined,
      };
    }
  }

  async batch(
    statements: Array<{ sql: string; params?: SqlValue[] }>
  ): Promise<SqlResult[]> {
    const prepared = statements.map((s) => {
      const stmt = this.db.prepare(s.sql);
      return s.params && s.params.length > 0 ? stmt.bind(...s.params) : stmt;
    });
    const d1Results = await this.db.batch(prepared);
    return d1Results.map((d1Result, i) => {
      const sql = statements[i].sql;
      const isSelect = sql.trimStart().toUpperCase().startsWith('SELECT');
      if (isSelect) {
        const records = d1Result.results as Record<string, unknown>[];
        const columns = records.length > 0 ? Object.keys(records[0]) : [];
        const rows = records.map((r) => columns.map((c) => r[c] as SqlValue));
        return {
          columns,
          rows,
          rowsAffected: 0,
        };
      } else {
        return {
          columns: [],
          rows: [],
          rowsAffected: d1Result.meta.changes ?? 0,
          lastInsertRowid: d1Result.meta.last_row_id ?? undefined,
        };
      }
    });
  }
}
