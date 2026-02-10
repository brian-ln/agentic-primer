/**
 * StorageActor - Database access with table-level scoping
 *
 * Accepts any ISqlStorage implementation, allowing it to work with
 * bun:sqlite, D1, libsql, or any other SQL backend.
 *
 * Features:
 * - Table-level access control (allowedTables)
 * - Operation-level permissions (read, write, delete, admin)
 * - Transaction support via ISqlStorage.batch()
 * - SQL parameter binding
 */

import { Database } from 'bun:sqlite';
import {
  Actor,
  type Message,
  type MessageResponse,
  type IMessageRouter,
  type ISqlStorage,
  type SqlValue,
  type SqlResult,
  createResponse,
  createErrorResponse,
} from '@agentic-primer/actors';

export type StorageOperation = 'read' | 'write' | 'delete' | 'admin';

export interface StorageActorConfig {
  /** Provide an ISqlStorage instance directly. Takes precedence over dbPath. */
  storage?: ISqlStorage;
  /** Path to a bun:sqlite database file. Used only if `storage` is not provided. */
  dbPath?: string;
  allowedTables: string[];
  operations: StorageOperation[];
}

/**
 * BunSqliteStorage - ISqlStorage adapter for bun:sqlite.
 *
 * Used internally when StorageActor is configured with a dbPath
 * instead of an explicit ISqlStorage instance.
 */
class BunSqliteStorage implements ISqlStorage {
  constructor(private readonly db: Database) {}

  async execute(sql: string, params: SqlValue[] = []): Promise<SqlResult> {
    const isSelect = sql.trimStart().toUpperCase().startsWith('SELECT');
    if (isSelect) {
      const stmt = this.db.prepare(sql);
      const records = stmt.all(...params) as Record<string, unknown>[];
      const columns = records.length > 0 ? Object.keys(records[0]) : [];
      const rows = records.map((r) =>
        columns.map((c) => r[c] as SqlValue)
      );
      return { columns, rows, rowsAffected: 0 };
    } else {
      const stmt = this.db.prepare(sql);
      const info = stmt.run(...params);
      return {
        columns: [],
        rows: [],
        rowsAffected: info.changes,
        lastInsertRowid:
          typeof info.lastInsertRowid === 'number'
            ? info.lastInsertRowid
            : Number(info.lastInsertRowid),
      };
    }
  }

  async batch(
    statements: Array<{ sql: string; params?: SqlValue[] }>
  ): Promise<SqlResult[]> {
    const results: SqlResult[] = [];
    const transaction = this.db.transaction(() => {
      for (const s of statements) {
        results.push(
          // execute is async in the interface but sync here; we await in the outer method
          this.executeSync(s.sql, s.params ?? [])
        );
      }
    });
    transaction();
    return results;
  }

  /** Synchronous execute for use inside bun:sqlite transactions. */
  private executeSync(sql: string, params: SqlValue[]): SqlResult {
    const isSelect = sql.trimStart().toUpperCase().startsWith('SELECT');
    if (isSelect) {
      const stmt = this.db.prepare(sql);
      const records = stmt.all(...params) as Record<string, unknown>[];
      const columns = records.length > 0 ? Object.keys(records[0]) : [];
      const rows = records.map((r) =>
        columns.map((c) => r[c] as SqlValue)
      );
      return { columns, rows, rowsAffected: 0 };
    } else {
      const stmt = this.db.prepare(sql);
      const info = stmt.run(...params);
      return {
        columns: [],
        rows: [],
        rowsAffected: info.changes,
        lastInsertRowid:
          typeof info.lastInsertRowid === 'number'
            ? info.lastInsertRowid
            : Number(info.lastInsertRowid),
      };
    }
  }

  close(): void {
    this.db.close();
  }
}

export class StorageActor extends Actor {
  private storage: ISqlStorage;
  private ownedDb: BunSqliteStorage | null = null;
  private allowedTables: Set<string>;
  private operations: Set<StorageOperation>;

  constructor(id: string, router: IMessageRouter, config: StorageActorConfig) {
    super(id, router);
    if (config.storage) {
      this.storage = config.storage;
    } else if (config.dbPath) {
      const bunStorage = new BunSqliteStorage(new Database(config.dbPath));
      this.storage = bunStorage;
      this.ownedDb = bunStorage;
    } else {
      throw new Error('StorageActorConfig requires either `storage` or `dbPath`');
    }
    this.allowedTables = new Set(config.allowedTables);
    this.operations = new Set(config.operations);
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'storage.query': return await this.handleQuery(message, payload);
        case 'storage.execute': return await this.handleExecute(message, payload);
        case 'storage.transaction': return await this.handleTransaction(message, payload);
        default:
          return createErrorResponse(message, `Unknown message type: ${type}`);
      }
    } catch (error: any) {
      this.logError('Storage operation failed', { type, error: error.message });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleQuery(
    message: Message,
    payload: { sql: string; params?: SqlValue[] }
  ): Promise<MessageResponse> {
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    const tables = this.extractTables(payload.sql);
    const violation = this.validateTables(tables);
    if (violation) return createErrorResponse(message, violation);

    const result = await this.storage.execute(payload.sql, payload.params ?? []);
    return createResponse(message, result);
  }

  private async handleExecute(
    message: Message,
    payload: { sql: string; params?: SqlValue[] }
  ): Promise<MessageResponse> {
    const operation = this.inferOperation(payload.sql);
    if (!this.operations.has(operation)) {
      return createErrorResponse(message, `Operation '${operation}' not permitted`);
    }

    const tables = this.extractTables(payload.sql);
    const violation = this.validateTables(tables);
    if (violation) return createErrorResponse(message, violation);

    const result = await this.storage.execute(payload.sql, payload.params ?? []);
    return createResponse(message, {
      changes: result.rowsAffected,
      lastInsertRowid: result.lastInsertRowid,
    });
  }

  private async handleTransaction(
    message: Message,
    payload: { statements: Array<{ sql: string; params?: SqlValue[] }> }
  ): Promise<MessageResponse> {
    for (const stmt of payload.statements) {
      const operation = this.inferOperation(stmt.sql);
      if (!this.operations.has(operation)) {
        return createErrorResponse(
          message,
          `Transaction statement requires '${operation}' permission`
        );
      }

      const tables = this.extractTables(stmt.sql);
      const violation = this.validateTables(tables);
      if (violation) return createErrorResponse(message, violation);
    }

    await this.storage.batch(payload.statements);
    return createResponse(message, { committed: true });
  }

  private validateTables(tables: string[]): string | null {
    if (this.allowedTables.has('*')) return null;

    for (const table of tables) {
      if (!this.allowedTables.has(table)) {
        return `Access denied: table '${table}' not in allowedTables: [${Array.from(this.allowedTables).join(', ')}]`;
      }
    }
    return null;
  }

  private extractTables(sql: string): string[] {
    const tables = new Set<string>();

    const fromMatch = sql.match(/FROM\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (fromMatch) {
      fromMatch.forEach(match => tables.add(match.replace(/FROM\s+/i, '').trim()));
    }

    const intoMatch = sql.match(/INTO\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (intoMatch) {
      intoMatch.forEach(match => tables.add(match.replace(/INTO\s+/i, '').trim()));
    }

    const updateMatch = sql.match(/UPDATE\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (updateMatch) {
      updateMatch.forEach(match => tables.add(match.replace(/UPDATE\s+/i, '').trim()));
    }

    return Array.from(tables);
  }

  private inferOperation(sql: string): StorageOperation {
    const trimmed = sql.trim().toUpperCase();
    if (trimmed.startsWith('SELECT')) return 'read';
    if (trimmed.startsWith('INSERT') || trimmed.startsWith('UPDATE')) return 'write';
    if (trimmed.startsWith('DELETE')) return 'delete';
    if (trimmed.startsWith('ALTER') || trimmed.startsWith('DROP') || trimmed.startsWith('CREATE')) return 'admin';
    throw new Error(`Cannot infer operation from SQL: ${sql.substring(0, 50)}...`);
  }

  async shutdown(): Promise<void> {
    if (this.ownedDb) {
      this.ownedDb.close();
    }
  }
}
