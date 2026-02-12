#!/usr/bin/env bun
/**
 * StorageCapability - Scoped database access
 *
 * Provides table-level and operation-level scoped access to storage.
 * Enforces restrictions before routing to system storage actors.
 *
 * @example
 * ```typescript
 * const storage = new StorageCapability(router, {
 *   namespace: '/workflows',
 *   allowedTables: ['tasks', 'workflows'],
 *   operations: ['read', 'write']
 * });
 *
 * // ✅ Allowed
 * await storage.query('SELECT * FROM tasks WHERE status = ?', ['open']);
 *
 * // ❌ Denied
 * await storage.query('SELECT * FROM users'); // Table not in allowedTables
 * ```
 */

import type { MessageRouter } from '../router.ts';
import { type Address, address, createMessage } from '../message.ts';

/**
 * StorageCapability - Scoped database access
 */
export class StorageCapability {
  private router: MessageRouter;
  private storageAddress: Address;
  private config: StorageCapabilityConfig;

  constructor(router: MessageRouter, config: StorageCapabilityConfig) {
    this.router = router;
    this.config = config;
    this.storageAddress = address(`${config.namespace}/system/storage`);
  }

  /**
   * Execute a SELECT query
   * Requires: 'read' operation
   * Enforces: allowedTables restriction
   */
  async query<T = any>(sql: string, params?: any[]): Promise<T[]> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Extract tables from SQL and validate
    const tables = this.extractTables(sql);
    this.validateTables(tables);

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(
        this.storageAddress,
        'storage.query',
        { sql, params },
        { pattern: 'ask', from: address('capability/storage') }
      )
    );

    if (!response.success) {
      throw new Error(`Storage query failed: ${response.error}`);
    }

    return response.payload as T[];
  }

  /**
   * Execute an INSERT/UPDATE statement
   * Requires: 'write' operation
   * Enforces: allowedTables restriction
   */
  async execute(
    sql: string,
    params?: any[]
  ): Promise<{ changes: number; lastInsertRowid?: number }> {
    // Check operation permission
    if (!this.config.operations.includes('write')) {
      throw new CapabilityError('write', 'Operation not permitted by capability');
    }

    // Extract tables and validate
    const tables = this.extractTables(sql);
    this.validateTables(tables);

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(
        this.storageAddress,
        'storage.execute',
        { sql, params },
        { pattern: 'ask', from: address('capability/storage') }
      )
    );

    if (!response.success) {
      throw new Error(`Storage execute failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Execute a DELETE statement
   * Requires: 'delete' operation
   * Enforces: allowedTables restriction
   */
  async delete(sql: string, params?: any[]): Promise<{ changes: number }> {
    // Check operation permission
    if (!this.config.operations.includes('delete')) {
      throw new CapabilityError('delete', 'Operation not permitted by capability');
    }

    // Extract tables and validate
    const tables = this.extractTables(sql);
    this.validateTables(tables);

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(
        this.storageAddress,
        'storage.execute',
        { sql, params },
        { pattern: 'ask', from: address('capability/storage') }
      )
    );

    if (!response.success) {
      throw new Error(`Storage delete failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Execute multiple statements in a transaction
   * Requires: operations matching each statement
   * Enforces: allowedTables and operations for each statement
   */
  async transaction(statements: Array<{ sql: string; params?: any[] }>): Promise<void> {
    // Validate all statements first
    for (const stmt of statements) {
      const tables = this.extractTables(stmt.sql);
      this.validateTables(tables);

      const operation = this.inferOperation(stmt.sql);
      if (!this.config.operations.includes(operation)) {
        throw new CapabilityError(
          operation,
          `Operation '${operation}' not permitted by capability`
        );
      }
    }

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(
        this.storageAddress,
        'storage.transaction',
        { statements },
        { pattern: 'ask', from: address('capability/storage') }
      )
    );

    if (!response.success) {
      throw new Error(`Storage transaction failed: ${response.error}`);
    }
  }

  /**
   * Subscribe to table changes (reactive queries)
   * Requires: 'subscribe' operation
   * Enforces: allowedTables restriction
   */
  async subscribe<T = any>(
    table: string,
    options: {
      where?: Record<string, any>;
      onMatch: (rows: T[]) => void;
    }
  ): Promise<{ unsubscribe: () => void }> {
    // Check operation permission
    if (!this.config.operations.includes('subscribe')) {
      throw new CapabilityError('subscribe', 'Operation not permitted by capability');
    }

    // Validate table
    this.validateTables([table]);

    // Route to storage actor
    // Note: In real implementation, need to create callback mechanism
    const response = await this.router.ask(
      createMessage(
        this.storageAddress,
        'storage.subscribe',
        {
          table,
          where: options.where,
          // Callback mechanism TBD in Phase 3
        },
        { pattern: 'ask', from: address('capability/storage') }
      )
    );

    if (!response.success) {
      throw new Error(`Storage subscription failed: ${response.error}`);
    }

    return {
      unsubscribe: () => {
        // Send unsubscribe message
        this.router
          .tell(
            createMessage(
              this.storageAddress,
              'storage.unsubscribe',
              { subscriptionId: response.payload.subscriptionId },
              { pattern: 'tell', from: address('capability/storage') }
            )
          )
          .catch(() => {}); // Ignore errors
      },
    };
  }

  /**
   * Extract table names from SQL
   * Simple regex-based extraction
   *
   * Note: This is a simplified implementation. A production system
   * should use a proper SQL parser for accuracy.
   */
  private extractTables(sql: string): string[] {
    const tables = new Set<string>();

    // Match FROM clause
    const fromMatch = sql.match(/FROM\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (fromMatch) {
      fromMatch.forEach(match => {
        const table = match.replace(/FROM\s+/i, '').trim();
        tables.add(table);
      });
    }

    // Match INTO clause (INSERT)
    const intoMatch = sql.match(/INTO\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (intoMatch) {
      intoMatch.forEach(match => {
        const table = match.replace(/INTO\s+/i, '').trim();
        tables.add(table);
      });
    }

    // Match UPDATE clause
    const updateMatch = sql.match(/UPDATE\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (updateMatch) {
      updateMatch.forEach(match => {
        const table = match.replace(/UPDATE\s+/i, '').trim();
        tables.add(table);
      });
    }

    return Array.from(tables);
  }

  /**
   * Validate tables against allowedTables
   */
  private validateTables(tables: string[]): void {
    const { allowedTables } = this.config;

    for (const table of tables) {
      if (!allowedTables.includes(table) && !allowedTables.includes('*')) {
        throw new CapabilityError(
          'table-access',
          `Table '${table}' not in allowedTables: [${allowedTables.join(', ')}]`
        );
      }
    }
  }

  /**
   * Infer operation from SQL statement
   */
  private inferOperation(sql: string): StorageOperation {
    const trimmed = sql.trim().toUpperCase();

    if (trimmed.startsWith('SELECT')) return 'read';
    if (trimmed.startsWith('INSERT') || trimmed.startsWith('UPDATE')) return 'write';
    if (trimmed.startsWith('DELETE')) return 'delete';
    if (
      trimmed.startsWith('ALTER') ||
      trimmed.startsWith('DROP') ||
      trimmed.startsWith('CREATE')
    )
      return 'admin';

    throw new Error(`Cannot infer operation from SQL: ${sql.substring(0, 50)}...`);
  }
}

/**
 * StorageCapability configuration
 */
export interface StorageCapabilityConfig {
  /** Actor namespace (e.g., '/workflows') */
  namespace: string;

  /** Allowed tables (use '*' for all tables) */
  allowedTables: string[];

  /** Allowed operations */
  operations: StorageOperation[];
}

export type StorageOperation = 'read' | 'write' | 'delete' | 'admin' | 'subscribe';

/**
 * Capability error
 */
export class CapabilityError extends Error {
  constructor(public operation: string, message: string) {
    super(message);
    this.name = 'CapabilityError';
  }
}
