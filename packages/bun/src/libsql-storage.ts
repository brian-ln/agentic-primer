/**
 * LibSqlStorage - ISqlStorage adapter for @libsql/client
 *
 * Thin wrapper that maps the libsql Client interface to the portable
 * ISqlStorage interface from @agentic-primer/actors.
 */

import type { Client } from '@libsql/client';
import type { ISqlStorage, SqlValue, SqlResult } from '@agentic-primer/actors';

/** Convert ISqlStorage SqlValue[] to libsql InArgs. */
function toLibsqlArgs(params: SqlValue[]): (null | number | string | ArrayBuffer)[] {
  return params.map((v) => {
    if (v instanceof Uint8Array) return v.buffer as ArrayBuffer;
    return v;
  });
}

/** Map a libsql ResultSet to our SqlResult. */
function toSqlResult(rs: { columns: string[]; rows: any[][]; rowsAffected: number; lastInsertRowid?: bigint | number | undefined }): SqlResult {
  return {
    columns: rs.columns,
    rows: rs.rows.map((row) =>
      row.map((cell) => {
        if (cell === null || cell === undefined) return null;
        if (typeof cell === 'bigint') return Number(cell);
        return cell as SqlValue;
      })
    ),
    rowsAffected: rs.rowsAffected,
    lastInsertRowid:
      rs.lastInsertRowid != null ? Number(rs.lastInsertRowid) : undefined,
  };
}

export class LibSqlStorage implements ISqlStorage {
  constructor(private readonly client: Client) {}

  async execute(sql: string, params: SqlValue[] = []): Promise<SqlResult> {
    const rs = await this.client.execute({
      sql,
      args: toLibsqlArgs(params),
    });
    return toSqlResult(rs as any);
  }

  async batch(
    statements: Array<{ sql: string; params?: SqlValue[] }>
  ): Promise<SqlResult[]> {
    const rs = await this.client.batch(
      statements.map((s) => ({
        sql: s.sql,
        args: toLibsqlArgs(s.params ?? []),
      }))
    );
    return rs.map((r) => toSqlResult(r as any));
  }

  /** Close the underlying libsql client connection. */
  close(): void {
    this.client.close();
  }
}
