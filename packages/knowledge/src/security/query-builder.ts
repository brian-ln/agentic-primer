/**
 * Type-Safe SQL Query Builder
 * Prevents SQL injection through parameterized queries and safe pattern handling.
 */

import { sanitizeLikePattern } from './input-validation.ts';

export interface SafeQuery {
  sql: string;
  params: any[];
}

export type LikeMode = 'contains' | 'starts' | 'ends' | 'exact';

export function buildLikePattern(
  input: string,
  mode: LikeMode = 'contains'
): { pattern: string; escape: boolean } {
  const sanitized = sanitizeLikePattern(input);
  switch (mode) {
    case 'contains': return { pattern: `%${sanitized}%`, escape: true };
    case 'starts':   return { pattern: `${sanitized}%`, escape: true };
    case 'ends':     return { pattern: `%${sanitized}`, escape: true };
    case 'exact':    return { pattern: sanitized, escape: true };
  }
}

export class QueryBuilder {
  private selectColumns: string[] = [];
  private fromTable: string = '';
  private whereConditions: string[] = [];
  private orderByClause: string = '';
  private limitValue: number | null = null;
  private params: any[] = [];

  select(...columns: string[]): this {
    this.selectColumns = columns;
    return this;
  }

  from(table: string): this {
    if (!/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(table)) {
      throw new Error(`Invalid table name: ${table}`);
    }
    this.fromTable = table;
    return this;
  }

  where(condition: string, ...params: any[]): this {
    this.whereConditions.push(condition);
    this.params.push(...params);
    return this;
  }

  whereLike(column: string, pattern: string, mode: LikeMode = 'contains'): this {
    if (!/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(column)) {
      throw new Error(`Invalid column name: ${column}`);
    }
    const { pattern: safePattern } = buildLikePattern(pattern, mode);
    this.whereConditions.push(`${column} LIKE ? ESCAPE '\\\\'`);
    this.params.push(safePattern);
    return this;
  }

  whereOrLike(columns: string[], pattern: string, mode: LikeMode = 'contains'): this {
    if (columns.length === 0) throw new Error('At least one column required for whereOrLike');
    for (const column of columns) {
      if (!/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(column)) {
        throw new Error(`Invalid column name: ${column}`);
      }
    }
    const { pattern: safePattern } = buildLikePattern(pattern, mode);
    const conditions = columns.map(col => `${col} LIKE ? ESCAPE '\\\\'`).join(' OR ');
    this.whereConditions.push(`(${conditions})`);
    for (let i = 0; i < columns.length; i++) {
      this.params.push(safePattern);
    }
    return this;
  }

  orderBy(clause: string): this {
    if (!/^[a-zA-Z0-9_,\s]+(?:ASC|DESC)?(?:\s*,\s*[a-zA-Z0-9_]+\s*(?:ASC|DESC)?)*$/i.test(clause)) {
      throw new Error(`Invalid ORDER BY clause: ${clause}`);
    }
    this.orderByClause = clause;
    return this;
  }

  limit(value: number): this {
    if (!Number.isInteger(value) || value < 1) throw new Error(`Invalid LIMIT value: ${value}`);
    this.limitValue = value;
    return this;
  }

  build(): SafeQuery {
    if (this.selectColumns.length === 0) throw new Error('No columns specified (use .select())');
    if (!this.fromTable) throw new Error('No table specified (use .from())');

    let sql = `SELECT ${this.selectColumns.join(', ')} FROM ${this.fromTable}`;
    if (this.whereConditions.length > 0) {
      sql += ` WHERE ${this.whereConditions.join(' AND ')}`;
    }
    if (this.orderByClause) sql += ` ORDER BY ${this.orderByClause}`;
    if (this.limitValue !== null) sql += ` LIMIT ${this.limitValue}`;

    return { sql, params: this.params };
  }
}

export function queryBuilder(): QueryBuilder {
  return new QueryBuilder();
}

export function buildLikeQuery(
  table: string, column: string, pattern: string,
  mode: LikeMode = 'contains', limit?: number
): SafeQuery {
  const builder = queryBuilder().select('*').from(table).whereLike(column, pattern, mode);
  if (limit) builder.limit(limit);
  return builder.build();
}

export function buildSearchQuery(
  table: string, columns: string[], pattern: string,
  mode: LikeMode = 'contains', limit?: number
): SafeQuery {
  const builder = queryBuilder().select('*').from(table).whereOrLike(columns, pattern, mode);
  if (limit) builder.limit(limit);
  return builder.build();
}
