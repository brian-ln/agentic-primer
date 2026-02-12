/**
 * Type-Safe SQL Query Builder
 * Prevents SQL injection through parameterized queries and safe pattern handling
 * Epic: agentic-primer-0lg.2
 *
 * This module provides utilities for safely constructing SQL queries with LIKE patterns,
 * ensuring all user input is properly sanitized and parameterized.
 */

import { sanitizeLikePattern } from './input-validation';

/**
 * Represents a parameterized SQL query with safe parameter binding
 */
export interface SafeQuery {
  sql: string;
  params: any[];
}

/**
 * LIKE pattern matching modes
 */
export type LikeMode = 'contains' | 'starts' | 'ends' | 'exact';

/**
 * Safe LIKE pattern builder
 *
 * @example
 * ```typescript
 * // Contains search (most common)
 * const query = buildLikePattern('user input', 'contains');
 * // Returns: { pattern: '%sanitized%', escape: true }
 *
 * // Starts with search
 * const query = buildLikePattern('prefix', 'starts');
 * // Returns: { pattern: 'sanitized%', escape: true }
 *
 * // Exact match
 * const query = buildLikePattern('exact', 'exact');
 * // Returns: { pattern: 'sanitized', escape: true }
 * ```
 */
export function buildLikePattern(
  input: string,
  mode: LikeMode = 'contains'
): { pattern: string; escape: boolean } {
  const sanitized = sanitizeLikePattern(input);

  switch (mode) {
    case 'contains':
      return { pattern: `%${sanitized}%`, escape: true };
    case 'starts':
      return { pattern: `${sanitized}%`, escape: true };
    case 'ends':
      return { pattern: `%${sanitized}`, escape: true };
    case 'exact':
      return { pattern: sanitized, escape: true };
  }
}

/**
 * Query builder for safe SELECT queries with LIKE patterns
 *
 * @example
 * ```typescript
 * // Simple query
 * const query = queryBuilder()
 *   .select('*')
 *   .from('session_decisions')
 *   .whereLike('decision', 'authentication', 'contains')
 *   .build();
 *
 * // Complex query with multiple conditions
 * const query = queryBuilder()
 *   .select('id', 'decision', 'timestamp')
 *   .from('session_decisions')
 *   .where('timestamp >= ?', startTime)
 *   .whereLike('decision', userSearch, 'contains')
 *   .orderBy('timestamp DESC')
 *   .limit(100)
 *   .build();
 * ```
 */
export class QueryBuilder {
  private selectColumns: string[] = [];
  private fromTable: string = '';
  private whereConditions: string[] = [];
  private orderByClause: string = '';
  private limitValue: number | null = null;
  private params: any[] = [];

  /**
   * Specify columns to select
   */
  select(...columns: string[]): this {
    this.selectColumns = columns;
    return this;
  }

  /**
   * Specify table to select from
   */
  from(table: string): this {
    // Validate table name (alphanumeric + underscore only)
    if (!/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(table)) {
      throw new Error(`Invalid table name: ${table}`);
    }
    this.fromTable = table;
    return this;
  }

  /**
   * Add a WHERE condition with parameterized value
   *
   * @example
   * ```typescript
   * .where('id = ?', userId)
   * .where('timestamp >= ?', startTime)
   * ```
   */
  where(condition: string, ...params: any[]): this {
    this.whereConditions.push(condition);
    this.params.push(...params);
    return this;
  }

  /**
   * Add a safe LIKE WHERE condition
   * Automatically sanitizes the pattern and adds ESCAPE clause
   *
   * @example
   * ```typescript
   * // Search for decisions containing "auth"
   * .whereLike('decision', 'auth', 'contains')
   *
   * // Search for sessions starting with specific ID
   * .whereLike('session_id', 'abc123', 'starts')
   * ```
   */
  whereLike(
    column: string,
    pattern: string,
    mode: LikeMode = 'contains'
  ): this {
    // Validate column name (alphanumeric + underscore only)
    if (!/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(column)) {
      throw new Error(`Invalid column name: ${column}`);
    }

    const { pattern: safePattern } = buildLikePattern(pattern, mode);
    this.whereConditions.push(`${column} LIKE ? ESCAPE '\\\\'`);
    this.params.push(safePattern);
    return this;
  }

  /**
   * Add an OR-based LIKE condition across multiple columns
   * Useful for searching across multiple text fields
   *
   * @example
   * ```typescript
   * .whereOrLike(['decision', 'reasoning'], 'authentication', 'contains')
   * // Generates: (decision LIKE ? ESCAPE '\\' OR reasoning LIKE ? ESCAPE '\\')
   * ```
   */
  whereOrLike(
    columns: string[],
    pattern: string,
    mode: LikeMode = 'contains'
  ): this {
    if (columns.length === 0) {
      throw new Error('At least one column required for whereOrLike');
    }

    // Validate all column names
    for (const column of columns) {
      if (!/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(column)) {
        throw new Error(`Invalid column name: ${column}`);
      }
    }

    const { pattern: safePattern } = buildLikePattern(pattern, mode);
    const conditions = columns.map(col => `${col} LIKE ? ESCAPE '\\\\'`).join(' OR ');
    this.whereConditions.push(`(${conditions})`);

    // Add the same pattern for each column
    for (let i = 0; i < columns.length; i++) {
      this.params.push(safePattern);
    }

    return this;
  }

  /**
   * Add ORDER BY clause
   *
   * @example
   * ```typescript
   * .orderBy('timestamp DESC')
   * .orderBy('decision ASC, timestamp DESC')
   * ```
   */
  orderBy(clause: string): this {
    // Basic validation: only allow alphanumeric, underscore, space, comma, ASC, DESC
    if (!/^[a-zA-Z0-9_,\s]+(?:ASC|DESC)?(?:\s*,\s*[a-zA-Z0-9_]+\s*(?:ASC|DESC)?)*$/i.test(clause)) {
      throw new Error(`Invalid ORDER BY clause: ${clause}`);
    }
    this.orderByClause = clause;
    return this;
  }

  /**
   * Add LIMIT clause
   */
  limit(value: number): this {
    if (!Number.isInteger(value) || value < 1) {
      throw new Error(`Invalid LIMIT value: ${value}`);
    }
    this.limitValue = value;
    return this;
  }

  /**
   * Build the final parameterized query
   *
   * @returns SafeQuery object with SQL and parameters
   */
  build(): SafeQuery {
    if (this.selectColumns.length === 0) {
      throw new Error('No columns specified (use .select())');
    }
    if (!this.fromTable) {
      throw new Error('No table specified (use .from())');
    }

    let sql = `SELECT ${this.selectColumns.join(', ')} FROM ${this.fromTable}`;

    if (this.whereConditions.length > 0) {
      sql += ` WHERE ${this.whereConditions.join(' AND ')}`;
    }

    if (this.orderByClause) {
      sql += ` ORDER BY ${this.orderByClause}`;
    }

    if (this.limitValue !== null) {
      sql += ` LIMIT ${this.limitValue}`;
    }

    return {
      sql,
      params: this.params,
    };
  }
}

/**
 * Create a new query builder instance
 *
 * @example
 * ```typescript
 * const query = queryBuilder()
 *   .select('*')
 *   .from('session_decisions')
 *   .whereLike('decision', userInput, 'contains')
 *   .build();
 *
 * const results = db.query(query.sql).all(...query.params);
 * ```
 */
export function queryBuilder(): QueryBuilder {
  return new QueryBuilder();
}

/**
 * Convenience function for building a simple LIKE query
 *
 * @example
 * ```typescript
 * const query = buildLikeQuery(
 *   'session_decisions',
 *   'decision',
 *   userInput,
 *   'contains'
 * );
 * const results = db.query(query.sql).all(...query.params);
 * ```
 */
export function buildLikeQuery(
  table: string,
  column: string,
  pattern: string,
  mode: LikeMode = 'contains',
  limit?: number
): SafeQuery {
  const builder = queryBuilder()
    .select('*')
    .from(table)
    .whereLike(column, pattern, mode);

  if (limit) {
    builder.limit(limit);
  }

  return builder.build();
}

/**
 * Convenience function for building a multi-column search query
 *
 * @example
 * ```typescript
 * const query = buildSearchQuery(
 *   'session_decisions',
 *   ['decision', 'reasoning', 'alternatives'],
 *   userSearchTerm,
 *   'contains',
 *   50
 * );
 * const results = db.query(query.sql).all(...query.params);
 * ```
 */
export function buildSearchQuery(
  table: string,
  columns: string[],
  pattern: string,
  mode: LikeMode = 'contains',
  limit?: number
): SafeQuery {
  const builder = queryBuilder()
    .select('*')
    .from(table)
    .whereOrLike(columns, pattern, mode);

  if (limit) {
    builder.limit(limit);
  }

  return builder.build();
}
