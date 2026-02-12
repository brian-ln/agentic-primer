#!/usr/bin/env bun
/**
 * Pattern Matching API
 *
 * Provides fluent API for constructing graph patterns with type safety.
 * Inspired by Cypher-style pattern matching but as TypeScript builders.
 */

import type {
  PatternSpec,
  RelationshipConstraint,
  FilterExpression,
} from './types.ts';

/**
 * Pattern builder for type-safe pattern construction
 */
export class PatternBuilder<T = any> {
  private spec: PatternSpec;

  constructor(variable: string) {
    this.spec = {
      variable,
      labels: [],
      where: {},
      relationships: [],
      notExists: [],
    };
  }

  /**
   * Add label constraint
   *
   * @example
   * pattern('task').label('Task')
   */
  label(label: string): this {
    if (!this.spec.labels) {
      this.spec.labels = [];
    }
    this.spec.labels.push(label);
    return this;
  }

  /**
   * Add property constraints
   *
   * Supports special path filter properties:
   * - `path_prefix: 'workflows/build/'` - Match by path prefix
   * - `path_pattern: 'channels/*'` - Match by wildcard pattern
   * - `path_exact: 'domain/inference'` - Match exact path
   *
   * @example
   * ```typescript
   * // Regular property filters
   * pattern('task').where({ status: 'open', priority: 'high' })
   *
   * // Path prefix filter
   * pattern('task').where({
   *   path_prefix: 'workflows/build-pipeline/tasks/',
   *   status: 'active'
   * })
   *
   * // Path pattern with wildcards
   * pattern('actor').where({
   *   path_pattern: 'channels/*'
   * })
   *
   * // Recursive wildcard
   * pattern('service').where({
   *   path_pattern: 'services/**'
   * })
   * ```
   */
  where(properties: Partial<T>): this {
    this.spec.where = { ...this.spec.where, ...properties };
    return this;
  }

  /**
   * Add relationship constraint
   *
   * @example
   * pattern('blocker').relatedTo('task', {
   *   type: 'requires',
   *   direction: 'inbound'
   * })
   */
  relatedTo(
    target: string,
    options: {
      type?: string;
      direction: 'outbound' | 'inbound' | 'both';
      properties?: Record<string, any>;
    }
  ): this {
    const constraint: RelationshipConstraint = {
      target,
      type: options.type,
      direction: options.direction,
      properties: options.properties,
    };

    if (!this.spec.relationships) {
      this.spec.relationships = [];
    }
    this.spec.relationships.push(constraint);
    return this;
  }

  /**
   * Add NOT EXISTS constraint
   *
   * @example
   * pattern('task').notExists(
   *   pattern('blocker')
   *     .label('Task')
   *     .where({ status: 'open' })
   *     .relatedTo('task', { type: 'requires', direction: 'inbound' })
   * )
   */
  notExists(...patterns: PatternBuilder[]): this {
    if (!this.spec.notExists) {
      this.spec.notExists = [];
    }
    this.spec.notExists.push(...patterns.map((p) => p.build()));
    return this;
  }

  /**
   * Build the pattern specification
   */
  build(): PatternSpec {
    return this.spec;
  }

  /**
   * Get the variable name
   */
  getVariable(): string {
    return this.spec.variable;
  }
}

/**
 * Create a pattern builder
 *
 * @example
 * const taskPattern = pattern<Task>('task')
 *   .label('Task')
 *   .where({ status: 'open' })
 */
export function pattern<T = any>(variable: string): PatternBuilder<T> {
  return new PatternBuilder<T>(variable);
}

/**
 * Filter expression builder
 */
export class FilterBuilder {
  private expr: FilterExpression;

  private constructor(expr: FilterExpression) {
    this.expr = expr;
  }

  /**
   * Create comparison filter
   *
   * @example
   * filter('task', 'priority').eq('high')
   */
  static comparison(
    variable: string,
    property: string
  ): ComparisonBuilder {
    return new ComparisonBuilder(variable, property);
  }

  /**
   * Create logical AND
   *
   * @example
   * filter.and(
   *   filter('task', 'status').eq('open'),
   *   filter('task', 'priority').eq('high')
   * )
   */
  static and(...filters: FilterBuilder[]): FilterBuilder {
    return new FilterBuilder({
      type: 'logical',
      operator: 'AND',
      expressions: filters.map((f) => f.build()),
    });
  }

  /**
   * Create logical OR
   */
  static or(...filters: FilterBuilder[]): FilterBuilder {
    return new FilterBuilder({
      type: 'logical',
      operator: 'OR',
      expressions: filters.map((f) => f.build()),
    });
  }

  /**
   * Create logical NOT
   */
  static not(filter: FilterBuilder): FilterBuilder {
    return new FilterBuilder({
      type: 'logical',
      operator: 'NOT',
      expressions: [filter.build()],
    });
  }

  /**
   * Build the filter expression
   */
  build(): FilterExpression {
    return this.expr;
  }
}

/**
 * Comparison builder for property comparisons
 */
class ComparisonBuilder {
  constructor(
    private variable: string,
    private property: string
  ) {}

  /**
   * Equality comparison
   */
  eq(value: any): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: '=',
      variable: this.variable,
      property: this.property,
      value,
    });
  }

  /**
   * Inequality comparison
   */
  neq(value: any): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: '!=',
      variable: this.variable,
      property: this.property,
      value,
    });
  }

  /**
   * Greater than
   */
  gt(value: any): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: '>',
      variable: this.variable,
      property: this.property,
      value,
    });
  }

  /**
   * Less than
   */
  lt(value: any): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: '<',
      variable: this.variable,
      property: this.property,
      value,
    });
  }

  /**
   * Greater than or equal
   */
  gte(value: any): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: '>=',
      variable: this.variable,
      property: this.property,
      value,
    });
  }

  /**
   * Less than or equal
   */
  lte(value: any): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: '<=',
      variable: this.variable,
      property: this.property,
      value,
    });
  }

  /**
   * Contains (for strings/arrays)
   */
  contains(value: any): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: 'CONTAINS',
      variable: this.variable,
      property: this.property,
      value,
    });
  }

  /**
   * Starts with (for strings)
   */
  startsWith(value: string): FilterBuilder {
    return new FilterBuilder({
      type: 'comparison',
      operator: 'STARTS_WITH',
      variable: this.variable,
      property: this.property,
      value,
    });
  }
}

/**
 * Helper function to create filter expressions
 *
 * @example
 * filter('task', 'status').eq('open')
 */
export function filter(
  variable: string,
  property: string
): ComparisonBuilder {
  return FilterBuilder.comparison(variable, property);
}

/**
 * Logical operators for combining filters
 */
export const logic = {
  and: FilterBuilder.and,
  or: FilterBuilder.or,
  not: FilterBuilder.not,
};
