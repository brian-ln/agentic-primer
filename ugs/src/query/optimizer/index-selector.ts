#!/usr/bin/env bun
/**
 * Index Selector - Automatic Index Hint Selection
 *
 * Analyzes query patterns and historical statistics to recommend
 * optimal indexes for query execution. Implements adaptive learning
 * based on execution history.
 *
 * Key capabilities:
 * - Pattern-based index selection (high cardinality, equality filters)
 * - Historical performance analysis
 * - Confidence scoring
 * - Index effectiveness tracking
 */

import type {
  QueryDefinition,
  PatternSpec,
  FilterExpression,
  IndexHint,
  QueryStatistics,
  IndexEffectiveness,
} from '../types.ts';

/**
 * Index selection strategy
 */
export interface IndexSelectionStrategy {
  /** Strategy name */
  name: string;

  /** Selection function */
  select: (
    query: QueryDefinition,
    statistics?: QueryStatistics[]
  ) => IndexHint[];

  /** Priority (higher = preferred) */
  priority: number;
}

/**
 * Index selector - recommends optimal indexes
 */
export class IndexSelector {
  private strategies: IndexSelectionStrategy[];

  constructor() {
    this.strategies = [
      this.createPatternBasedStrategy(),
      this.createHistoricalStrategy(),
      this.createCardinalityStrategy(),
      this.createCompositeStrategy(),
    ];

    // Sort by priority
    this.strategies.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Select optimal indexes for a query
   */
  selectIndexes(
    query: QueryDefinition,
    statistics?: QueryStatistics[]
  ): IndexHint[] {
    const allHints: IndexHint[] = [];

    // Apply each strategy
    for (const strategy of this.strategies) {
      const hints = strategy.select(query, statistics);
      allHints.push(...hints);
    }

    // Merge and deduplicate hints
    const merged = this.mergeHints(allHints);

    // Sort by confidence (highest first)
    merged.sort((a, b) => (b.confidence || 0) - (a.confidence || 0));

    return merged;
  }

  /**
   * Pattern-based strategy: Select indexes based on query patterns
   *
   * High confidence for:
   * - Equality filters on single properties
   * - Range queries on ordered properties
   * - Common query patterns (status, type, timestamps)
   */
  private createPatternBasedStrategy(): IndexSelectionStrategy {
    return {
      name: 'pattern-based',
      priority: 100,
      select: (query: QueryDefinition) => {
        const hints: IndexHint[] = [];

        for (const pattern of query.patterns) {
          // Check WHERE clause for indexable properties
          if (pattern.where) {
            const indexableProps = this.findIndexableProperties(pattern.where);

            for (const prop of indexableProps) {
              hints.push({
                variable: pattern.variable,
                index: prop.name,
                source: 'automatic',
                confidence: prop.confidence,
                reason: `${prop.reason} on ${prop.name}`,
              });
            }
          }

          // Check relationship constraints
          if (pattern.relationships) {
            for (const rel of pattern.relationships) {
              hints.push({
                variable: pattern.variable,
                index: `rel_${rel.type || 'any'}_${rel.direction}`,
                source: 'automatic',
                confidence: 0.7,
                reason: `Relationship index for ${rel.direction} ${rel.type || 'any'} traversal`,
              });
            }
          }
        }

        // Check filters for additional indexing opportunities
        if (query.filters) {
          const filterHints = this.analyzeFilters(query.filters);
          hints.push(...filterHints);
        }

        return hints;
      },
    };
  }

  /**
   * Historical strategy: Use past execution statistics
   *
   * Learns from previous query executions to recommend
   * indexes that have proven effective.
   */
  private createHistoricalStrategy(): IndexSelectionStrategy {
    return {
      name: 'historical',
      priority: 90,
      select: (query: QueryDefinition, statistics?: QueryStatistics[]) => {
        if (!statistics || statistics.length === 0) {
          return [];
        }

        const hints: IndexHint[] = [];

        // Find similar queries in statistics
        for (const stats of statistics) {
          if (!stats.indexEffectiveness) continue;

          // Iterate over index effectiveness data
          for (const [indexName, effectiveness] of Array.from(stats.indexEffectiveness.entries())) {
            // Only recommend if index has proven effective
            if (
              effectiveness.avgImprovement > 0.1 &&
              effectiveness.successRate > 0.8 &&
              effectiveness.useCount >= 3
            ) {
              // Try to match to a pattern variable
              const variable = this.inferVariableForIndex(query, indexName);

              if (variable) {
                const confidence = Math.min(
                  effectiveness.avgImprovement * effectiveness.successRate,
                  0.95
                );

                hints.push({
                  variable,
                  index: indexName,
                  source: 'automatic',
                  confidence,
                  reason: `Historical data: ${(effectiveness.avgImprovement * 100).toFixed(0)}% improvement over ${effectiveness.useCount} executions`,
                });
              }
            }
          }
        }

        return hints;
      },
    };
  }

  /**
   * Cardinality strategy: Prefer indexes on high-cardinality columns
   *
   * High cardinality = more selective = better index performance
   */
  private createCardinalityStrategy(): IndexSelectionStrategy {
    return {
      name: 'cardinality',
      priority: 80,
      select: (query: QueryDefinition) => {
        const hints: IndexHint[] = [];

        // Known high-cardinality properties
        const highCardinalityProps = new Set([
          'id',
          'uuid',
          'email',
          'address',
          'url',
          'path',
          'hash',
        ]);

        // Known low-cardinality properties (avoid indexing)
        const lowCardinalityProps = new Set([
          'active',
          'enabled',
          'visible',
          'archived',
        ]);

        for (const pattern of query.patterns) {
          if (!pattern.where) continue;

          for (const [key] of Object.entries(pattern.where)) {
            const lowerKey = key.toLowerCase();

            // High cardinality: strong recommendation
            if (
              highCardinalityProps.has(lowerKey) ||
              lowerKey.endsWith('_id') ||
              lowerKey.endsWith('id')
            ) {
              hints.push({
                variable: pattern.variable,
                index: key,
                source: 'automatic',
                confidence: 0.85,
                reason: `High cardinality property`,
              });
            }

            // Medium cardinality: moderate recommendation
            if (
              lowerKey.includes('name') ||
              lowerKey.includes('type') ||
              lowerKey.includes('category')
            ) {
              // Only if not already in high cardinality
              if (!highCardinalityProps.has(lowerKey)) {
                hints.push({
                  variable: pattern.variable,
                  index: key,
                  source: 'automatic',
                  confidence: 0.6,
                  reason: `Medium cardinality property`,
                });
              }
            }
          }
        }

        return hints;
      },
    };
  }

  /**
   * Composite strategy: Recommend composite indexes for multi-property queries
   */
  private createCompositeStrategy(): IndexSelectionStrategy {
    return {
      name: 'composite',
      priority: 70,
      select: (query: QueryDefinition) => {
        const hints: IndexHint[] = [];

        for (const pattern of query.patterns) {
          if (!pattern.where) continue;

          const props = Object.keys(pattern.where);

          // If multiple properties, consider composite index
          if (props.length >= 2 && props.length <= 4) {
            // Sort properties for consistent naming
            const sortedProps = [...props].sort();
            const compositeIndex = sortedProps.join('_');

            hints.push({
              variable: pattern.variable,
              index: `composite_${compositeIndex}`,
              source: 'automatic',
              confidence: 0.65,
              reason: `Composite index for multi-property query (${sortedProps.join(', ')})`,
            });
          }
        }

        return hints;
      },
    };
  }

  /**
   * Find indexable properties in WHERE clause
   */
  private findIndexableProperties(
    where: Record<string, any>
  ): Array<{ name: string; confidence: number; reason: string }> {
    const indexable: Array<{ name: string; confidence: number; reason: string }> =
      [];

    for (const [key, value] of Object.entries(where)) {
      // Equality filters: highest confidence
      if (typeof value === 'string' || typeof value === 'number') {
        indexable.push({
          name: key,
          confidence: 0.8,
          reason: 'Equality filter',
        });
      }

      // Range queries (if value is an object with operators)
      if (
        typeof value === 'object' &&
        value !== null &&
        ('$gt' in value || '$lt' in value || '$gte' in value || '$lte' in value)
      ) {
        indexable.push({
          name: key,
          confidence: 0.75,
          reason: 'Range query',
        });
      }

      // Array membership ($in operator)
      if (Array.isArray(value) || (typeof value === 'object' && '$in' in value)) {
        indexable.push({
          name: key,
          confidence: 0.7,
          reason: 'Array membership query',
        });
      }
    }

    return indexable;
  }

  /**
   * Analyze filters for indexing opportunities
   */
  private analyzeFilters(filters: FilterExpression[]): IndexHint[] {
    const hints: IndexHint[] = [];

    for (const filter of filters) {
      if (filter.type === 'comparison' && filter.variable && filter.property) {
        let confidence = 0.6;
        let reason = 'Filter comparison';

        // Higher confidence for equality
        if (filter.operator === '=' || filter.operator === '==') {
          confidence = 0.75;
          reason = 'Equality filter';
        }

        // Lower confidence for inequality
        if (filter.operator === '!=' || filter.operator === '<>') {
          confidence = 0.4;
          reason = 'Inequality filter (low selectivity)';
        }

        hints.push({
          variable: filter.variable,
          index: filter.property,
          source: 'automatic',
          confidence,
          reason,
        });
      }

      // Recurse into logical expressions
      if (filter.type === 'logical' && filter.expressions) {
        hints.push(...this.analyzeFilters(filter.expressions));
      }
    }

    return hints;
  }

  /**
   * Infer which variable an index belongs to
   */
  private inferVariableForIndex(
    query: QueryDefinition,
    indexName: string
  ): string | null {
    // Try to match index name to pattern variables
    for (const pattern of query.patterns) {
      // Check if index matches any property in WHERE clause
      if (pattern.where && indexName in pattern.where) {
        return pattern.variable;
      }

      // Check if index matches label
      if (pattern.labels?.some((label) => indexName.toLowerCase().includes(label.toLowerCase()))) {
        return pattern.variable;
      }
    }

    // Default to first pattern variable
    return query.patterns.length > 0 ? query.patterns[0].variable : null;
  }

  /**
   * Merge and deduplicate hints
   */
  private mergeHints(hints: IndexHint[]): IndexHint[] {
    const merged = new Map<string, IndexHint>();

    for (const hint of hints) {
      const key = `${hint.variable}:${hint.index}`;

      if (!merged.has(key)) {
        merged.set(key, hint);
      } else {
        // Merge: take highest confidence
        const existing = merged.get(key)!;
        if ((hint.confidence || 0) > (existing.confidence || 0)) {
          merged.set(key, hint);
        }
      }
    }

    return Array.from(merged.values());
  }
}

/**
 * Singleton instance
 */
let indexSelectorInstance: IndexSelector | null = null;

/**
 * Get shared index selector instance
 */
export function getIndexSelector(): IndexSelector {
  if (!indexSelectorInstance) {
    indexSelectorInstance = new IndexSelector();
  }
  return indexSelectorInstance;
}
