#!/usr/bin/env bun
/**
 * Query/DSL Layer for Actor Fabric
 *
 * Provides declarative query interface with type-safe pattern matching,
 * cost-based optimization, and adaptive learning (inspired by Halo paper).
 *
 * @example
 * ```typescript
 * import { query, pattern, send } from '@simplify/query';
 *
 * // Find ready tasks and auto-start
 * const readyTasks = query()
 *   .match(
 *     pattern<Task>('task')
 *       .label('Task')
 *       .where({ status: 'open' })
 *       .notExists(
 *         pattern('blocker')
 *           .label('Task')
 *           .where({ status: 'open' })
 *           .relatedTo('task', { type: 'requires', direction: 'inbound' })
 *       )
 *   )
 *   .forEach(send('task').tell('start'));
 *
 * await readyTasks.execute(queryExecutor);
 * ```
 */

// Core types
export * from './types.ts';

// Pattern matching
export { pattern, filter, logic, PatternBuilder, FilterBuilder } from './pattern.ts';

// Path-based filtering
export {
  pathFilter,
  pathPrefix,
  pathPattern,
  pathExact,
  PathFilterBuilder,
} from './path-filter.ts';
export type { PathFilterOptions } from './path-filter.ts';
export {
  compilePathFilter,
  runtimePathMatch,
  generatePathQuery,
} from './path-pattern.ts';
export type { PathFilterSQL } from './path-pattern.ts';

// Query building
export { query, send, update, create, deleteEntity, createRelationship, deleteRelationship, upsertRelationship, QueryBuilder, ActionBuilder } from './builder.ts';

// Compilation and execution
export { QueryCompiler } from './compiler.ts';
export { QueryCache } from './cache.ts';

// EXPLAIN functionality
export { QueryExplainer, PlanFormatter, PlanVisualizer } from './explain/index.ts';
export type {
  ExplainResult,
  ExplainOptions,
  OptimizationNote,
  CostBreakdown,
  CacheAnalysis,
} from './explain/index.ts';

// Knowledge integration
export {
  knowledge,
  decisions,
  learnings,
  errors,
  searchKnowledge,
  traverseKnowledge,
  knowledgeTraversal,
  knowledgeAddress,
  parseKnowledgeAddress,
} from './knowledge-integration.ts';
export type {
  KnowledgeSearchOptions,
  KnowledgeSearchResult,
  KnowledgeTraversalOptions,
} from './knowledge-integration.ts';

// Convenience re-exports
export type {
  QueryDefinition,
  QueryPlan,
  QueryResult,
  ExecutionContext,
  PatternSpec,
  ActionSpec,
} from './types.ts';
