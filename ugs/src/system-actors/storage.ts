#!/usr/bin/env bun
/**
 * StorageActor - Database access with table-level scoping
 *
 * Pure actor model: StorageActor IS the capability.
 * Access control happens through routing, not helper objects.
 *
 * Features:
 * - Table-level access control (allowedTables)
 * - Operation-level permissions (read, write, delete, admin)
 * - Transaction support
 * - SQL parameter binding
 */

import { Database } from 'bun:sqlite';
import { Actor, createResponse, createErrorResponse } from '@agentic-primer/actors';
import type { Message, MessageResponse, MessageRouter } from '@agentic-primer/actors';

/**
 * Storage operations
 */
export type StorageOperation = 'read' | 'write' | 'delete' | 'admin';

/**
 * StorageActor configuration
 */
export interface StorageActorConfig {
  /** Database path */
  dbPath: string;

  /** Allowed tables (use '*' for all tables) */
  allowedTables: string[];

  /** Allowed operations */
  operations: StorageOperation[];
}

/**
 * StorageActor - Provides database access with internal table-level scoping
 *
 * @example
 * ```typescript
 * const storage = new StorageActor('storage', router, {
 *   dbPath: './data/app.db',
 *   allowedTables: ['tasks', 'workflows'],
 *   operations: ['read', 'write', 'delete']
 * });
 * router.registerActor('/workflows/system/storage', storage);
 * ```
 */
export class StorageActor extends Actor {
  private db: Database;
  private allowedTables: Set<string>;
  private operations: Set<StorageOperation>;

  constructor(id: string, router: MessageRouter, config: StorageActorConfig) {
    super(id, router);
    this.db = new Database(config.dbPath);
    this.allowedTables = new Set(config.allowedTables);
    this.operations = new Set(config.operations);
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      if (type === 'storage.query') {
        return await this.handleQuery(message, payload);
      }

      if (type === 'storage.execute') {
        return await this.handleExecute(message, payload);
      }

      if (type === 'storage.transaction') {
        return await this.handleTransaction(message, payload);
      }

      return createErrorResponse(message, `Unknown message type: ${type}`);
    } catch (error: any) {
      this.logError('Storage operation failed', {
        type,
        error: error.message,
      });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleQuery(
    message: Message,
    payload: { sql: string; params?: any[] }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    // Validate tables
    const tables = this.extractTables(payload.sql);
    const violation = this.validateTables(tables);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Execute query
    const stmt = this.db.prepare(payload.sql);
    const rows = stmt.all(payload.params || []);

    return createResponse(message, { rows });
  }

  private async handleExecute(
    message: Message,
    payload: { sql: string; params?: any[] }
  ): Promise<MessageResponse> {
    // Determine operation type
    const operation = this.inferOperation(payload.sql);

    // Check operation permission
    if (!this.operations.has(operation)) {
      return createErrorResponse(
        message,
        `Operation '${operation}' not permitted`
      );
    }

    // Validate tables
    const tables = this.extractTables(payload.sql);
    const violation = this.validateTables(tables);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Execute statement
    const stmt = this.db.prepare(payload.sql);
    const info = stmt.run(payload.params || []);

    return createResponse(message, {
      changes: info.changes,
      lastInsertRowid: info.lastInsertRowid,
    });
  }

  private async handleTransaction(
    message: Message,
    payload: { statements: Array<{ sql: string; params?: any[] }> }
  ): Promise<MessageResponse> {
    // Validate all statements first
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
      if (violation) {
        return createErrorResponse(message, violation);
      }
    }

    // Execute transaction
    const transaction = this.db.transaction(() => {
      for (const stmt of payload.statements) {
        const prepared = this.db.prepare(stmt.sql);
        prepared.run(stmt.params || []);
      }
    });

    transaction();

    return createResponse(message, { committed: true });
  }

  /**
   * Validate tables against allowedTables
   * @returns Error message if validation fails, null if OK
   */
  private validateTables(tables: string[]): string | null {
    if (this.allowedTables.has('*')) {
      return null; // Wildcard = all tables allowed
    }

    for (const table of tables) {
      if (!this.allowedTables.has(table)) {
        return `Access denied: table '${table}' not in allowedTables: [${Array.from(
          this.allowedTables
        ).join(', ')}]`;
      }
    }

    return null;
  }

  /**
   * Extract table names from SQL
   */
  private extractTables(sql: string): string[] {
    const tables = new Set<string>();

    // Match FROM clause
    const fromMatch = sql.match(/FROM\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (fromMatch) {
      fromMatch.forEach((match) => {
        const table = match.replace(/FROM\s+/i, '').trim();
        tables.add(table);
      });
    }

    // Match INTO clause (INSERT)
    const intoMatch = sql.match(/INTO\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (intoMatch) {
      intoMatch.forEach((match) => {
        const table = match.replace(/INTO\s+/i, '').trim();
        tables.add(table);
      });
    }

    // Match UPDATE clause
    const updateMatch = sql.match(/UPDATE\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (updateMatch) {
      updateMatch.forEach((match) => {
        const table = match.replace(/UPDATE\s+/i, '').trim();
        tables.add(table);
      });
    }

    return Array.from(tables);
  }

  /**
   * Infer operation from SQL statement
   */
  private inferOperation(sql: string): StorageOperation {
    const trimmed = sql.trim().toUpperCase();

    if (trimmed.startsWith('SELECT')) return 'read';
    if (trimmed.startsWith('INSERT') || trimmed.startsWith('UPDATE'))
      return 'write';
    if (trimmed.startsWith('DELETE')) return 'delete';
    if (
      trimmed.startsWith('ALTER') ||
      trimmed.startsWith('DROP') ||
      trimmed.startsWith('CREATE')
    ) {
      return 'admin';
    }

    throw new Error(
      `Cannot infer operation from SQL: ${sql.substring(0, 50)}...`
    );
  }

  /**
   * Close database connection
   */
  async shutdown(): Promise<void> {
    this.db.close();
  }
}
