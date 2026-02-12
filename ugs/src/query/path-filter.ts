#!/usr/bin/env bun
/**
 * Path-Based Filter API
 *
 * Provides fluent API for path-based actor filtering in queries.
 * Supports prefix matching and wildcard patterns (* and **).
 *
 * Part of Phase 6: Query Layer Integration for Path-Based Addressing
 */

import type { FilterExpression } from './types.ts';

/**
 * Path filter options for queries
 */
export interface PathFilterOptions {
  /** Exact path prefix match (e.g., 'workflows/build-pipeline/') */
  pathPrefix?: string;

  /** Path pattern with wildcards (e.g., 'channels/*', 'services/**') */
  pathPattern?: string;

  /** Path must match exactly (no wildcards, strict equality) */
  pathExact?: string;
}

/**
 * Path filter builder for constructing path-based queries
 *
 * @example
 * ```typescript
 * // Prefix matching
 * pathFilter().prefix('workflows/build-pipeline/tasks/')
 *
 * // Single-level wildcard
 * pathFilter().pattern('channels/*')
 *
 * // Multi-level wildcard
 * pathFilter().pattern('services/**')
 *
 * // Exact match
 * pathFilter().exact('domain/inference')
 * ```
 */
export class PathFilterBuilder {
  private options: PathFilterOptions = {};

  /**
   * Match actors under a specific path prefix
   *
   * @param prefix - Path prefix to match (e.g., 'workflows/build/')
   * @returns This builder for chaining
   *
   * @example
   * ```typescript
   * pathFilter().prefix('workflows/build-pipeline/tasks/')
   * // Matches: workflows/build-pipeline/tasks/compile
   * // Matches: workflows/build-pipeline/tasks/test/unit
   * // Does not match: workflows/build-pipeline/deploy
   * ```
   */
  prefix(prefix: string): this {
    this.options.pathPrefix = prefix;
    return this;
  }

  /**
   * Match actors using wildcard patterns
   *
   * Supports:
   * - `*` - matches exactly one path segment
   * - `**` - matches zero or more path segments (recursive)
   *
   * @param pattern - Pattern with wildcards
   * @returns This builder for chaining
   *
   * @example
   * ```typescript
   * // Single-level wildcard
   * pathFilter().pattern('channels/*')
   * // Matches: channels/logs, channels/metrics
   * // Does not match: channels/logs/errors (too deep)
   *
   * // Recursive wildcard
   * pathFilter().pattern('services/**')
   * // Matches: services/llm, services/stable/inference, services/a/b/c
   *
   * // Combined patterns
   * pathFilter().pattern('workflows/star/tasks/star')
   * // Matches: workflows/build/tasks/compile
   * // Matches: workflows/deploy/tasks/publish
   * ```
   */
  pattern(pattern: string): this {
    this.options.pathPattern = pattern;
    return this;
  }

  /**
   * Match actors with exact path (strict equality)
   *
   * @param path - Exact path to match
   * @returns This builder for chaining
   *
   * @example
   * ```typescript
   * pathFilter().exact('domain/inference')
   * // Matches only: domain/inference
   * // Does not match: domain/inference/task-1
   * ```
   */
  exact(path: string): this {
    this.options.pathExact = path;
    return this;
  }

  /**
   * Build the filter options
   * @internal
   */
  build(): PathFilterOptions {
    return this.options;
  }

  /**
   * Convert to FilterExpression for query integration
   * @internal
   */
  toFilterExpression(variable: string): FilterExpression {
    if (this.options.pathExact) {
      return {
        type: 'comparison',
        operator: '=',
        variable,
        property: 'path',
        value: this.options.pathExact,
      };
    }

    if (this.options.pathPrefix) {
      return {
        type: 'comparison',
        operator: 'STARTS_WITH',
        variable,
        property: 'path',
        value: this.options.pathPrefix,
      };
    }

    if (this.options.pathPattern) {
      return {
        type: 'predicate',
        operator: 'PATH_MATCH',
        variable,
        property: 'path',
        value: this.options.pathPattern,
      };
    }

    throw new Error('PathFilterBuilder: No filter criteria specified');
  }
}

/**
 * Create a path filter builder
 *
 * @example
 * ```typescript
 * // In pattern.where()
 * pattern('task').where({
 *   path_prefix: 'workflows/build/',
 *   status: 'active'
 * })
 *
 * // Or with builder
 * const filter = pathFilter().pattern('services/**');
 * ```
 */
export function pathFilter(): PathFilterBuilder {
  return new PathFilterBuilder();
}

/**
 * Convenience helper: create path prefix filter
 *
 * @example
 * ```typescript
 * pathPrefix('workflows/build-pipeline/tasks/')
 * ```
 */
export function pathPrefix(prefix: string): PathFilterBuilder {
  return new PathFilterBuilder().prefix(prefix);
}

/**
 * Convenience helper: create path pattern filter
 *
 * @example
 * ```typescript
 * pathPattern('channels/*')
 * pathPattern('services/**')
 * ```
 */
export function pathPattern(pattern: string): PathFilterBuilder {
  return new PathFilterBuilder().pattern(pattern);
}

/**
 * Convenience helper: create exact path filter
 *
 * @example
 * ```typescript
 * pathExact('domain/inference')
 * ```
 */
export function pathExact(path: string): PathFilterBuilder {
  return new PathFilterBuilder().exact(path);
}
