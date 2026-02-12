#!/usr/bin/env bun
/**
 * Path Pattern Compilation
 *
 * Compiles path patterns (with wildcards) to SQL WHERE clauses.
 * Leverages PathResolver for pattern matching logic.
 *
 * Part of Phase 6: Query Layer Integration for Path-Based Addressing
 */

import { matchPattern, parsePath } from '@agentic-primer/actors';

/**
 * SQL compilation result for path filters
 */
export interface PathFilterSQL {
  /** SQL WHERE clause fragment */
  sql: string;

  /** Parameters for prepared statement */
  params: Record<string, any>;

  /** Whether this uses an index-friendly query */
  indexOptimized: boolean;

  /** Strategy used for compilation */
  strategy: 'exact' | 'prefix' | 'like' | 'pattern';
}

/**
 * Compile path filter to SQL WHERE clause
 *
 * Optimizes for different filter types:
 * - **Exact match**: `path = ?` (index scan)
 * - **Prefix match**: `path LIKE 'prefix%'` (index range scan)
 * - **Single wildcard**: `path LIKE 'pattern'` (index scan if prefix is literal)
 * - **Multi wildcard**: Runtime pattern matching (requires full scan)
 *
 * @param filterType - Type of path filter
 * @param value - Filter value (path, prefix, or pattern)
 * @param paramPrefix - Prefix for SQL parameter names (for uniqueness)
 * @returns Compiled SQL and parameters
 *
 * @example
 * ```typescript
 * // Exact match (most efficient)
 * compilePathFilter('exact', 'domain/inference', 'p1')
 * // => { sql: "path = :p1_path", params: { p1_path: 'domain/inference' }, indexOptimized: true }
 *
 * // Prefix match (index range scan)
 * compilePathFilter('prefix', 'workflows/build/', 'p1')
 * // => { sql: "path LIKE :p1_path", params: { p1_path: 'workflows/build/%' }, indexOptimized: true }
 *
 * // Pattern with wildcard (partial index use)
 * compilePathFilter('pattern', 'workflows/star/tasks', 'p1')
 * // => { sql: "path LIKE :p1_path", params: { p1_path: 'workflows/%/tasks' }, indexOptimized: true }
 *
 * // Recursive wildcard (full scan - least efficient)
 * compilePathFilter('pattern', 'services/**', 'p1')
 * // => { sql: "path LIKE :p1_path", params: { p1_path: 'services/%' }, indexOptimized: false }
 * ```
 */
export function compilePathFilter(
  filterType: 'exact' | 'prefix' | 'pattern',
  value: string,
  paramPrefix: string = 'path'
): PathFilterSQL {
  const paramKey = `${paramPrefix}_path`;

  switch (filterType) {
    case 'exact':
      // Exact match: path = 'domain/inference'
      // Most efficient: uses index for equality
      return {
        sql: `path = :${paramKey}`,
        params: { [paramKey]: value },
        indexOptimized: true,
        strategy: 'exact',
      };

    case 'prefix':
      // Prefix match: path LIKE 'workflows/build/%'
      // Efficient: uses index for range scan
      const prefixValue = value.endsWith('/') ? value : `${value}/`;
      return {
        sql: `path LIKE :${paramKey}`,
        params: { [paramKey]: `${prefixValue}%` },
        indexOptimized: true,
        strategy: 'prefix',
      };

    case 'pattern':
      return compilePattern(value, paramKey);

    default:
      throw new Error(`Unknown path filter type: ${filterType}`);
  }
}

/**
 * Compile path pattern with wildcards to SQL
 *
 * Strategies:
 * 1. **No wildcards** → Exact match (use = operator)
 * 2. **Prefix + wildcards** → LIKE with prefix (partial index)
 * 3. **Recursive wildcard** → LIKE + runtime filter (full scan)
 * 4. **Complex pattern** → Runtime pattern matching (full scan)
 *
 * @internal
 */
function compilePattern(pattern: string, paramKey: string): PathFilterSQL {
  const segments = parsePath(pattern);

  // No wildcards: treat as exact match
  if (!segments.some(s => s === '*' || s === '**')) {
    return {
      sql: `path = :${paramKey}`,
      params: { [paramKey]: pattern },
      indexOptimized: true,
      strategy: 'exact',
    };
  }

  // Find first wildcard position
  const firstWildcardIndex = segments.findIndex(s => s === '*' || s === '**');

  // If pattern starts with wildcard, full scan required
  if (firstWildcardIndex === 0) {
    return compileWildcardPattern(segments, paramKey);
  }

  // Extract literal prefix before first wildcard
  const literalPrefix = segments.slice(0, firstWildcardIndex).join('/');

  // Simple prefix + trailing wildcard: optimize as prefix match
  if (firstWildcardIndex === segments.length - 1 && segments[firstWildcardIndex] === '**') {
    // Pattern: "services/**" → LIKE "services/%"
    return {
      sql: `path LIKE :${paramKey}`,
      params: { [paramKey]: `${literalPrefix}/%` },
      indexOptimized: true,
      strategy: 'prefix',
    };
  }

  // Pattern with prefix + wildcards: use LIKE with prefix filter + runtime check
  const likePattern = convertToLikePattern(segments);

  return {
    sql: `path LIKE :${paramKey}`,
    params: { [paramKey]: likePattern },
    indexOptimized: literalPrefix.length > 0, // Partial index use
    strategy: 'like',
  };
}

/**
 * Compile pattern with leading wildcards (full scan required)
 *
 * @internal
 */
function compileWildcardPattern(segments: string[], paramKey: string): PathFilterSQL {
  const likePattern = convertToLikePattern(segments);

  return {
    sql: `path LIKE :${paramKey}`,
    params: { [paramKey]: likePattern },
    indexOptimized: false, // Full table scan
    strategy: 'pattern',
  };
}

/**
 * Convert path pattern to SQL LIKE pattern
 *
 * Rules:
 * - `*` → `%` (matches one segment: becomes [^/]+)
 * - `**` → `%` (matches multiple segments)
 * - Literal segments → unchanged
 *
 * Note: SQL LIKE is not sufficient for exact segment matching.
 * Runtime filtering with PathResolver.matchPattern() is required.
 *
 * @internal
 */
function convertToLikePattern(segments: string[]): string {
  const converted = segments.map(segment => {
    if (segment === '*' || segment === '**') {
      return '%';
    }
    // Escape SQL LIKE special characters
    return segment.replace(/%/g, '\\%').replace(/_/g, '\\_');
  });

  return converted.join('/');
}

/**
 * Check if a path matches a pattern (runtime validation)
 *
 * Used after SQL filtering to verify exact pattern match.
 * Delegates to PathResolver.matchPattern() for accuracy.
 *
 * @param path - Path to test
 * @param pattern - Pattern with wildcards
 * @returns True if path matches pattern
 *
 * @example
 * ```typescript
 * // SQL LIKE might return false positives for single-segment wildcards
 * // Runtime check ensures accuracy:
 *
 * // LIKE 'channels/%' matches: channels/logs/errors
 * // But pattern 'channels/*' should NOT match (too deep)
 * runtimePathMatch('channels/logs/errors', 'channels/*') // => false
 * runtimePathMatch('channels/logs', 'channels/*') // => true
 * ```
 */
export function runtimePathMatch(path: string, pattern: string): boolean {
  return matchPattern(path, pattern);
}

/**
 * Generate optimized SQL query for path filters
 *
 * Combines SQL filtering with optional runtime validation.
 *
 * @param filterType - Type of path filter
 * @param value - Filter value
 * @param tableName - SQL table name (default: 'actors')
 * @param paramPrefix - Parameter prefix for uniqueness
 * @returns Complete SQL query fragment
 *
 * @example
 * ```typescript
 * generatePathQuery('prefix', 'workflows/build/', 'actors', 'q1')
 * // Returns: {
 * //   sql: "actors.path LIKE :q1_path",
 * //   params: { q1_path: 'workflows/build/%' },
 * //   requiresRuntimeFilter: false
 * // }
 * ```
 */
export function generatePathQuery(
  filterType: 'exact' | 'prefix' | 'pattern',
  value: string,
  tableName: string = 'actors',
  paramPrefix: string = 'path'
): {
  sql: string;
  params: Record<string, any>;
  requiresRuntimeFilter: boolean;
  runtimePattern?: string;
} {
  const compiled = compilePathFilter(filterType, value, paramPrefix);

  // Add table prefix to path column
  const sql = compiled.sql.replace(/^path/, `${tableName}.path`);

  // Determine if runtime filtering is needed
  const requiresRuntimeFilter =
    filterType === 'pattern' &&
    value.includes('*') &&
    !compiled.indexOptimized;

  return {
    sql,
    params: compiled.params,
    requiresRuntimeFilter,
    runtimePattern: requiresRuntimeFilter ? value : undefined,
  };
}
