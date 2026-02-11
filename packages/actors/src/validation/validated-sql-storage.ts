/**
 * Validated SQL storage wrapper.
 *
 * Provides defense-in-depth validation for SQL operations.
 * Wraps any ISqlStorage implementation with input validation.
 *
 * Pattern: Decorator - maintains ISqlStorage interface while adding validation.
 *
 * Usage:
 * ```typescript
 * const rawStorage = new D1Storage(env.DB);
 * const validatedStorage = new ValidatedSqlStorage(rawStorage);
 * await validatedStorage.execute('SELECT * FROM users WHERE id = ?', [userId]);
 * ```
 */

import type { ISqlStorage, SqlValue, SqlResult } from '../interfaces.ts';
import {
  validateSqlQuery,
  validateSqlParams,
  validateBatchSize,
} from './storage-validation.ts';

/**
 * Validated SQL storage wrapper.
 *
 * Validates all inputs before delegating to the inner storage implementation.
 * Provides an independent security layer that does not trust upstream validation.
 */
export class ValidatedSqlStorage implements ISqlStorage {
  constructor(private inner: ISqlStorage) {}

  /**
   * Execute a single SQL statement with validation.
   *
   * Validates:
   * - SQL query (size, dangerous patterns)
   * - Parameters (count, size)
   *
   * @throws {Error} if validation fails
   */
  async execute(sql: string, params?: SqlValue[]): Promise<SqlResult> {
    validateSqlQuery(sql);
    validateSqlParams(params);
    return this.inner.execute(sql, params);
  }

  /**
   * Execute a batch of SQL statements with validation.
   *
   * Validates:
   * - Batch size (max 25 statements for D1)
   * - Each SQL query (size, dangerous patterns)
   * - Each parameter set (count, size)
   *
   * @throws {Error} if validation fails for any statement
   */
  async batch(statements: Array<{ sql: string; params?: SqlValue[] }>): Promise<SqlResult[]> {
    validateBatchSize(statements.length);

    for (const stmt of statements) {
      validateSqlQuery(stmt.sql);
      validateSqlParams(stmt.params);
    }

    return this.inner.batch(statements);
  }
}
