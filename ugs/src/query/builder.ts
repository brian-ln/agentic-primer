#!/usr/bin/env bun
/**
 * Query Builder
 *
 * Fluent API for constructing graph queries with type safety.
 * Composes patterns, filters, traversals, and actions.
 */

import type {
  QueryDefinition,
  PatternSpec,
  FilterExpression,
  TraversalSpec,
  AggregationSpec,
  ActionSpec,
  QueryMetadata,
  QueryPlan,
  ExecutionContext,
  Subscription,
  SubscriptionCallbacks,
} from './types.ts';
import { PatternBuilder, FilterBuilder } from './pattern.ts';
import { QueryCompiler } from './compiler.ts';
import { QueryExplainer } from './explain/explainer.ts';
import type { ExplainResult, ExplainOptions } from './explain/types.ts';
import { matchPattern } from '@agentic-primer/actors';

/**
 * Main query builder class
 */
export class QueryBuilder {
  private definition: QueryDefinition;

  constructor() {
    this.definition = {
      patterns: [],
      filters: [],
      traversals: [],
      aggregations: [],
      actions: [],
      returns: [],
      metadata: {},
    };
  }

  /**
   * Add pattern matching clauses
   *
   * @example
   * query()
   *   .match(
   *     pattern('task').label('Task').where({ status: 'open' }),
   *     pattern('user').label('User').where({ id: 'alice' })
   *   )
   */
  match(...patterns: PatternBuilder[]): this {
    this.definition.patterns.push(
      ...patterns.map((p) => p.build())
    );
    return this;
  }

  /**
   * Add WHERE filter clause
   *
   * @example
   * query()
   *   .match(pattern('task').label('Task'))
   *   .where(filter('task', 'priority').gte('medium'))
   */
  where(...filters: FilterBuilder[]): this {
    if (!this.definition.filters) {
      this.definition.filters = [];
    }
    this.definition.filters.push(
      ...filters.map((f) => f.build())
    );
    return this;
  }

  /**
   * Add traversal operation
   *
   * @example
   * query()
   *   .match(pattern('root').label('Task').where({ id: 'build' }))
   *   .traverse({
   *     from: 'root',
   *     relationship: 'requires',
   *     direction: 'outbound',
   *     depth: { max: 5 },
   *     as: 'dependencies'
   *   })
   */
  traverse(spec: TraversalSpec): this {
    if (!this.definition.traversals) {
      this.definition.traversals = [];
    }
    this.definition.traversals.push(spec);
    return this;
  }

  /**
   * Add aggregation operation
   *
   * @example
   * query()
   *   .match(pattern('tasks').label('Task'))
   *   .aggregate({
   *     operation: 'count',
   *     variable: 'tasks',
   *     as: 'taskCount'
   *   })
   */
  aggregate(spec: AggregationSpec): this {
    if (!this.definition.aggregations) {
      this.definition.aggregations = [];
    }
    this.definition.aggregations.push(spec);
    return this;
  }

  /**
   * Specify variables to return
   *
   * @example
   * query()
   *   .match(pattern('task').label('Task'))
   *   .return(['task'])
   */
  return(variables: string[]): this {
    if (!this.definition.returns) {
      this.definition.returns = [];
    }
    this.definition.returns.push(...variables);
    return this;
  }

  /**
   * Add action to perform on each result
   *
   * @example
   * query()
   *   .match(pattern('task').label('Task').where({ status: 'ready' }))
   *   .forEach(send('task').tell('start'))
   */
  forEach(action: ActionBuilder): this {
    if (!this.definition.actions) {
      this.definition.actions = [];
    }
    this.definition.actions.push(action.build());
    return this;
  }

  /**
   * Add action (alias for forEach)
   *
   * Convenience method for adding actions with a more declarative syntax.
   *
   * @example
   * query()
   *   .match(pattern('task'), pattern('user'))
   *   .action(upsertRelationship('task', 'user', 'assignedTo'))
   */
  action(action: ActionBuilder): this {
    return this.forEach(action);
  }

  /**
   * Conditional action (WHEN ... THEN)
   *
   * @example
   * query()
   *   .match(pattern('test').label('Task').where({ id: 'test' }))
   *   .when(pattern('test').where({ lifecycle: 'completed', result: { passed: true } }))
   *   .then(send('deploy').tell('start'))
   */
  when(...patterns: PatternBuilder[]): ConditionalBuilder {
    return new ConditionalBuilder(this, patterns);
  }

  /**
   * Create relationship between two matched nodes
   *
   * @example
   * query()
   *   .match(
   *     pattern('task').label('Task').where({ id: 'task-1' }),
   *     pattern('blocker').label('Task').where({ id: 'task-2' })
   *   )
   *   .createRelationship('task', 'blocker', {
   *     type: 'requires',
   *     properties: { priority: 'high', createdAt: Date.now() }
   *   })
   */
  createRelationship(
    from: string,
    to: string,
    options: {
      type: string;
      properties?: Record<string, any>;
    }
  ): this {
    if (!this.definition.actions) {
      this.definition.actions = [];
    }

    const action = ActionBuilder.createRelationship(from, to, options.type);
    if (options.properties) {
      (action as CreateRelationshipActionBuilder).withProperties(options.properties);
    }

    this.definition.actions.push(action.build());
    return this;
  }

  /**
   * Upsert relationship between two matched nodes (idempotent)
   *
   * @example
   * query()
   *   .match(
   *     pattern('task').label('Task').where({ id: 'task-1' }),
   *     pattern('user').label('User').where({ id: 'alice' })
   *   )
   *   .upsertRelationship('task', 'user', {
   *     type: 'assignedTo',
   *     properties: { priority: 'high', assignedAt: Date.now() }
   *   })
   */
  upsertRelationship(
    from: string,
    to: string,
    options: {
      type: string;
      properties?: Record<string, any>;
    }
  ): this {
    if (!this.definition.actions) {
      this.definition.actions = [];
    }

    if (!options.type) {
      throw new Error('Relationship type is required for upsert');
    }

    const action = ActionBuilder.upsertRelationship(from, to, options.type);
    if (options.properties) {
      (action as UpsertRelationshipActionBuilder).withProperties(options.properties);
    }

    this.definition.actions.push(action.build());
    return this;
  }

  /**
   * Shorthand for creating a relationship (alias for createRelationship)
   *
   * @example
   * query()
   *   .match(pattern('task'), pattern('user'))
   *   .link('task', 'user', 'assignedTo')
   */
  link(from: string, to: string, type: string, properties?: Record<string, any>): this {
    return this.createRelationship(from, to, { type, properties });
  }

  /**
   * Add manual index hint
   *
   * @example
   * query()
   *   .match(pattern('task').label('Task').where({ status: 'open' }))
   *   .useIndex('task', 'status')
   *   .return(['task'])
   */
  useIndex(variable: string, index: string, reason?: string): this {
    if (!this.definition.metadata) {
      this.definition.metadata = {};
    }
    if (!this.definition.metadata.indexHints) {
      this.definition.metadata.indexHints = [];
    }

    this.definition.metadata.indexHints.push({
      variable,
      index,
      source: 'manual',
      confidence: 1.0,
      reason: reason || 'Manual index hint',
    });

    return this;
  }

  /**
   * Set query metadata
   */
  withMetadata(metadata: Partial<QueryMetadata>): this {
    this.definition.metadata = {
      ...this.definition.metadata,
      ...metadata,
    };
    return this;
  }

  /**
   * Build the query definition
   */
  build(): QueryDefinition {
    return this.definition;
  }

  /**
   * Explain query execution plan
   *
   * Compiles the query and generates a human-readable explanation
   * showing execution steps, costs, and optimization opportunities.
   *
   * @param options - Explain options (verbose, costs, cache, optimize)
   * @param context - Optional execution context for cost estimates
   * @returns Explain result with formatted text and visualizations
   *
   * @example
   * const explanation = await query()
   *   .match(pattern('task').label('Task').where({ status: 'open' }))
   *   .return(['task'])
   *   .explain({ verbose: true, costs: true });
   *
   * console.log(explanation.text);
   * console.log(explanation.tree);
   */
  async explain(
    options: ExplainOptions = {},
    context?: ExecutionContext
  ): Promise<ExplainResult> {
    // Compile query to plan
    const compiler = new QueryCompiler();
    const plan = await compiler.compile(this.definition, context);

    // Generate explanation
    const explainer = new QueryExplainer();
    return explainer.explain(plan, options);
  }

  /**
   * Compile query to execution plan
   *
   * Lower-level method that returns the compiled plan without explanation.
   * Useful when you need the plan for execution or caching.
   *
   * @param context - Optional execution context
   * @returns Compiled query plan
   *
   * @example
   * const plan = await query()
   *   .match(pattern('task').label('Task'))
   *   .compile();
   */
  async compile(context?: ExecutionContext): Promise<QueryPlan> {
    const compiler = new QueryCompiler();
    return compiler.compile(this.definition, context);
  }

  /**
   * Execute query and return streaming results
   *
   * Returns an AsyncIterable that yields results from stream actions.
   * Multiplexes multiple actor streams into a single async iterator.
   *
   * @param context - Optional execution context
   * @returns AsyncIterable of streamed results
   *
   * @example
   * const results = query()
   *   .match(pattern('task').where({ type: 'build' }))
   *   .forEach(send('task').stream('logs'))
   *   .stream();
   *
   * for await (const log of results) {
   *   console.log(log);
   * }
   */
  async *stream<T = any>(context?: ExecutionContext): AsyncIterableIterator<T> {
    // Note: This is a placeholder implementation
    // Full implementation would:
    // 1. Compile the query to a plan
    // 2. Execute pattern matching to find target actors
    // 3. Call streamAsync() on each target actor
    // 4. Multiplex the streams into a single AsyncIterable
    // 5. Handle backpressure, cancellation, and cleanup

    // For now, this enables the API to be defined and tested
    // The full implementation will be in QueryExecutor
    throw new Error('stream() not yet implemented - requires QueryExecutor integration');
  }

  /**
   * Subscribe to reactive query updates
   *
   * Creates a live subscription that automatically notifies when results
   * match or unmatch the pattern. The subscription monitors relevant actor
   * state changes and re-evaluates the query.
   *
   * @param callbacks - Subscription callbacks (onMatch, onUnmatch, onError)
   * @returns Subscription handle for cleanup
   *
   * @example
   * const sub = query()
   *   .match(pattern('task').where({ status: 'failed' }))
   *   .subscribe({
   *     onMatch: (tasks) => console.log('Failed tasks:', tasks),
   *     onUnmatch: (tasks) => console.log('Recovered:', tasks),
   *     onError: (error) => console.error('Subscription error:', error)
   *   });
   *
   * // Later: cleanup
   * sub.unsubscribe();
   */
  subscribe<T = any>(callbacks: SubscriptionCallbacks<T>): Subscription {
    // This method requires runtime context (executor, router)
    // For now, throw an error indicating setup is needed
    // The QueryExecutor will provide a setupSubscribe() method
    throw new Error(
      'subscribe() requires SubscriptionManager setup. ' +
      'Use QueryExecutor.setupSubscribe() first, or create subscription ' +
      'through QueryExecutor.subscribe(query, callbacks)'
    );
  }

  /**
   * Register event trigger for declarative workflows
   *
   * Listen for specific event types and execute actions when patterns match.
   * Supports debouncing, throttling, and error handling with retries.
   *
   * @param eventType - Single event type or array of event types to listen for
   * @returns this (for chaining with where/forEach)
   *
   * @example
   * query()
   *   .on('task.lifecycle.completed')
   *   .where(
   *     pattern('task')
   *       .where({ type: 'test', result: { passed: true } })
   *   )
   *   .forEach(send('deploy').tell('start'));
   *
   * @example
   * // Multiple event types
   * query()
   *   .on(['task.created', 'task.updated'])
   *   .where(pattern('task').where({ priority: 'critical' }))
   *   .forEach(send('alerts').tell('notify'));
   */
  on(eventType: string | string[]): this {
    // Store event types (can be accessed via getTriggerEventTypes)
    if (!this.definition.metadata) {
      this.definition.metadata = {};
    }
    this.definition.metadata.triggerEventTypes = Array.isArray(eventType) ? eventType : [eventType];

    return this;
  }

  /**
   * Get trigger event types (for trigger compilation)
   */
  getTriggerEventTypes(): string[] | undefined {
    return this.definition.metadata?.triggerEventTypes as string[] | undefined;
  }

  /**
   * Match path pattern against registered actors/nodes
   *
   * Uses hierarchical path pattern matching with wildcards to find all
   * actors or nodes that match the given pattern. Returns an array of
   * QueryDefinition objects for each matched path.
   *
   * Supported wildcards:
   * - `*` matches exactly one segment (e.g., `domain/*` matches `domain/inference`)
   * - `**` matches zero or more segments (e.g., `domain/**` matches `domain/a/b/c`)
   * - `{a,b}` matches alternatives (e.g., `domain/{a,b}` matches `domain/a` or `domain/b`)
   *
   * @param pattern - Path pattern with wildcards
   * @param router - Message router containing registered actors (optional, for validation)
   * @returns Array of query definitions for matched paths
   *
   * @example
   * // Match all tasks under workflows
   * const queries = query().matchPath('/workflows/star/tasks/*');
   * // Returns queries for: /workflows/build/tasks/test, /workflows/deploy/tasks/verify, etc.
   *
   * @example
   * // Match all actors in domain namespace
   * const queries = query().matchPath('/domain/**');
   * // Returns queries for all domain actors
   *
   * @example
   * // Match specific service types
   * const queries = query().matchPath('/services/{llm,executor}');
   * // Returns queries for: /services/llm, /services/executor
   */
  matchPath(pattern: string, router?: any): QueryDefinition[] {
    // Using matchPattern from @agentic-primer/actors (imported at top)

    // TODO: If router is provided, get registered actor paths from it
    // For now, return a single QueryDefinition with the pattern stored in metadata
    // The actual matching will happen during query execution

    const queryDef: QueryDefinition = {
      patterns: [],
      filters: [],
      traversals: [],
      aggregations: [],
      actions: [],
      returns: [],
      metadata: {
        ...this.definition.metadata,
        pathPattern: pattern,
        isPathQuery: true,
      },
    };

    return [queryDef];
  }

  /**
   * Resolve alias to canonical path
   *
   * Uses the AliasResolver to resolve an alias path to its canonical path.
   * Returns the resolved path, or throws an error if the alias is invalid
   * or creates a cycle.
   *
   * @param alias - Alias path to resolve (e.g., 'services/llm')
   * @param resolver - Alias resolver instance (required)
   * @returns Promise resolving to the canonical path
   * @throws AliasError if alias is invalid or creates a cycle
   *
   * @example
   * const resolver = new AliasResolver(graphStore);
   * await resolver.createAlias('services/llm', 'domain/inference');
   *
   * const canonicalPath = await query().resolveAlias('services/llm', resolver);
   * // Returns: 'domain/inference'
   *
   * @example
   * // With context injection
   * const resolved = await query().resolveAlias('services/llm', resolver);
   * // Can then use resolved path in queries:
   * query().match(pattern('actor').where({ path: resolved }))
   */
  async resolveAlias(alias: string, resolver: any): Promise<string> {
    if (!resolver || typeof resolver.resolve !== 'function') {
      throw new Error('AliasResolver instance required for resolveAlias()');
    }

    const resolved = await resolver.resolve(alias);
    return resolved.path;
  }
}

/**
 * Conditional builder for WHEN ... THEN clauses
 */
class ConditionalBuilder {
  constructor(
    private queryBuilder: QueryBuilder,
    private whenPatterns: PatternBuilder[]
  ) {}

  /**
   * Specify action to execute when condition matches
   */
  then(action: ActionBuilder): QueryBuilder {
    // Add conditional as a pattern constraint with action
    // For now, treat this as a forEach with implicit filter
    // In full implementation, would support proper conditional evaluation
    this.queryBuilder.match(...this.whenPatterns);
    this.queryBuilder.forEach(action);
    return this.queryBuilder;
  }
}

/**
 * Action builder for mutations
 */
export class ActionBuilder {
  private spec: ActionSpec;

  private constructor(type: ActionSpec['type'], target: string) {
    this.spec = {
      type,
      target,
      params: {},
    };
  }

  /**
   * Create send action builder
   *
   * @example
   * send('task').tell('start')
   * send('task').ask('get', { id: '123' })
   */
  static send(target: string): SendActionBuilder {
    return new SendActionBuilder(target);
  }

  /**
   * Create update action builder
   *
   * @example
   * update('task').set({ status: 'in_progress' })
   */
  static update(target: string): UpdateActionBuilder {
    return new UpdateActionBuilder(target);
  }

  /**
   * Create create action builder
   *
   * @example
   * create('task').as({ title: 'New Task', status: 'open' })
   */
  static create(entity: string): CreateActionBuilder {
    return new CreateActionBuilder(entity);
  }

  /**
   * Create delete action builder
   *
   * @example
   * delete('task').confirm()
   * delete('task').cascade()
   */
  static delete(target: string): DeleteActionBuilder {
    return new DeleteActionBuilder(target);
  }

  /**
   * Create relationship action builder
   *
   * @example
   * createRelationship('task', 'blocker', 'requires')
   *   .withProperties({ priority: 'high' })
   */
  static createRelationship(
    from: string,
    to: string,
    type: string
  ): CreateRelationshipActionBuilder {
    return new CreateRelationshipActionBuilder(from, to, type);
  }

  /**
   * Create delete relationship action builder
   *
   * @example
   * deleteRelationship('task', 'dep', { type: 'requires' }).confirm()
   * deleteAllRelationships('task', { type: 'assignedTo' }).confirm()
   */
  static deleteRelationship(
    from: string,
    to?: string,
    options?: {
      type?: string;
      direction?: 'outbound' | 'inbound' | 'both';
    }
  ): DeleteRelationshipActionBuilder {
    return new DeleteRelationshipActionBuilder(from, to, options);
  }

  /**
   * Create upsert relationship action builder
   *
   * @example
   * upsertRelationship('task', 'user', 'assignedTo')
   *   .withProperties({ priority: 'high', assignedAt: Date.now() })
   */
  static upsertRelationship(
    from: string,
    to: string,
    type: string
  ): UpsertRelationshipActionBuilder {
    return new UpsertRelationshipActionBuilder(from, to, type);
  }

  /**
   * Build the action specification
   */
  build(): ActionSpec {
    return this.spec;
  }

  protected setParams(params: any): void {
    this.spec.params = params;
  }

  protected getSpec(): ActionSpec {
    return this.spec;
  }
}

/**
 * Send action builder
 */
class SendActionBuilder extends ActionBuilder {
  constructor(target: string) {
    super('send', target);
  }

  /**
   * Send tell message (fire-and-forget)
   */
  tell(type: string, payload: any = {}): ActionBuilder {
    this.setParams({ pattern: 'tell', type, payload });
    return this;
  }

  /**
   * Send ask message (request-response)
   */
  ask(type: string, payload: any = {}): ActionBuilder {
    this.setParams({ pattern: 'ask', type, payload });
    return this;
  }

  /**
   * Send stream message (continuous data stream)
   */
  stream(type: string, payload: any = {}): ActionBuilder {
    this.setParams({ pattern: 'stream', type, payload });
    return this;
  }

  /**
   * Send message with custom pattern
   */
  message(message: { type: string; payload?: any; pattern?: 'tell' | 'ask' }): ActionBuilder {
    this.setParams({
      pattern: message.pattern || 'tell',
      type: message.type,
      payload: message.payload || {},
    });
    return this;
  }
}

/**
 * Update action builder
 */
class UpdateActionBuilder extends ActionBuilder {
  constructor(target: string) {
    super('update', target);
  }

  /**
   * Set properties to update
   */
  set(properties: Record<string, any>): ActionBuilder {
    this.setParams({ properties });
    return this;
  }
}

/**
 * Create action builder
 */
class CreateActionBuilder extends ActionBuilder {
  constructor(entity: string) {
    super('create', entity);
  }

  /**
   * Specify entity properties
   */
  as(properties: Record<string, any>): ActionBuilder {
    this.setParams({ properties });
    return this;
  }
}

/**
 * Delete action builder
 */
class DeleteActionBuilder extends ActionBuilder {
  constructor(target: string) {
    super('delete', target);
  }

  /**
   * Confirm deletion (required for single entity delete)
   *
   * Safety check to prevent accidental deletions.
   */
  confirm(): ActionBuilder {
    this.setParams({ confirmed: true });
    return this;
  }

  /**
   * Cascade delete to related entities
   *
   * WARNING: This will delete the target entity and all entities
   * related through specified relationships.
   */
  cascade(relationships?: string[]): ActionBuilder {
    this.setParams({
      confirmed: true,
      cascade: true,
      relationships,
    });
    return this;
  }

  /**
   * Soft delete (mark as deleted without removing)
   */
  soft(): ActionBuilder {
    this.setParams({
      confirmed: true,
      soft: true,
    });
    return this;
  }

  /**
   * Require explicit bulk confirmation for multiple entities
   *
   * Only used internally by the executor when multiple entities
   * are matched.
   */
  confirmBulk(count: number): ActionBuilder {
    this.setParams({
      confirmed: true,
      bulk: true,
      count,
    });
    return this;
  }
}

/**
 * Create relationship action builder
 */
class CreateRelationshipActionBuilder extends ActionBuilder {
  constructor(from: string, to: string, type: string) {
    super('create_relationship', from);
    this.setParams({
      from,
      to,
      type,
      properties: {},
    });
  }

  /**
   * Set relationship properties
   *
   * @example
   * createRelationship('task', 'blocker', 'requires')
   *   .withProperties({ priority: 'high', createdAt: Date.now() })
   */
  withProperties(properties: Record<string, any>): ActionBuilder {
    this.setParams({
      ...this.getSpec().params,
      properties: {
        ...this.getSpec().params.properties,
        ...properties,
      },
    });
    return this;
  }

  /**
   * Set relationship strength (0-1)
   *
   * @example
   * createRelationship('task', 'blocker', 'requires')
   *   .strength(0.9)
   */
  strength(value: number): ActionBuilder {
    if (value < 0 || value > 1) {
      throw new Error('Strength must be between 0 and 1');
    }
    this.setParams({
      ...this.getSpec().params,
      properties: {
        ...this.getSpec().params.properties,
        strength: value,
      },
    });
    return this;
  }

  /**
   * Set relationship evidence/reasoning
   *
   * @example
   * createRelationship('task', 'blocker', 'requires')
   *   .evidence('Task cannot proceed until blocker is resolved')
   */
  evidence(text: string): ActionBuilder {
    this.setParams({
      ...this.getSpec().params,
      properties: {
        ...this.getSpec().params.properties,
        evidence: text,
      },
    });
    return this;
  }
}

/**
 * Delete relationship action builder
 */
class DeleteRelationshipActionBuilder extends ActionBuilder {
  constructor(
    from: string,
    to?: string,
    options?: {
      type?: string;
      direction?: 'outbound' | 'inbound' | 'both';
    }
  ) {
    super('delete_relationship', from);

    // Store relationship deletion parameters
    this.setParams({
      from,
      to,
      type: options?.type,
      direction: options?.direction || 'outbound',
      confirmed: false,
    });
  }

  /**
   * Confirm relationship deletion (required for safety)
   *
   * @example
   * deleteRelationship('task', 'dep', { type: 'requires' }).confirm()
   */
  confirm(): ActionBuilder {
    const params = this.getSpec().params;
    this.setParams({ ...params, confirmed: true });
    return this;
  }

  /**
   * Delete all matching relationships
   *
   * WARNING: This will delete all relationships matching the pattern.
   * Use with caution.
   *
   * @example
   * deleteRelationship('task', undefined, { type: 'assignedTo' }).confirmAll()
   */
  confirmAll(): ActionBuilder {
    const params = this.getSpec().params;
    this.setParams({ ...params, confirmed: true, deleteAll: true });
    return this;
  }

  /**
   * Perform cascade deletion of orphaned nodes
   *
   * After deleting relationships, also delete nodes that have
   * no remaining relationships.
   *
   * @example
   * deleteRelationship('task', 'dep', { type: 'requires' }).cascadeOrphans().confirm()
   */
  cascadeOrphans(): ActionBuilder {
    const params = this.getSpec().params;
    this.setParams({ ...params, cascadeOrphans: true });
    return this;
  }
}

/**
 * Upsert relationship action builder (idempotent)
 */
class UpsertRelationshipActionBuilder extends ActionBuilder {
  constructor(from: string, to: string, type: string) {
    super('upsert_relationship', from);
    this.setParams({
      from,
      to,
      type,
      properties: {},
      idempotent: true,
      mergeStrategy: 'shallow',
    });
  }

  /**
   * Set relationship properties
   *
   * @example
   * upsertRelationship('task', 'user', 'assignedTo')
   *   .withProperties({ priority: 'high', assignedAt: Date.now() })
   */
  withProperties(properties: Record<string, any>): ActionBuilder {
    this.setParams({
      ...this.getSpec().params,
      properties: {
        ...this.getSpec().params.properties,
        ...properties,
      },
    });
    return this;
  }

  /**
   * Set relationship strength (0-1)
   *
   * @example
   * upsertRelationship('task', 'dep', 'requires')
   *   .strength(0.85)
   */
  strength(value: number): ActionBuilder {
    if (value < 0 || value > 1) {
      throw new Error('Strength must be between 0 and 1');
    }
    this.setParams({
      ...this.getSpec().params,
      properties: {
        ...this.getSpec().params.properties,
        strength: value,
      },
    });
    return this;
  }

  /**
   * Set relationship evidence/reasoning
   *
   * @example
   * upsertRelationship('task', 'user', 'assignedTo')
   *   .evidence('Updated assignment based on capacity')
   */
  evidence(text: string): ActionBuilder {
    this.setParams({
      ...this.getSpec().params,
      properties: {
        ...this.getSpec().params.properties,
        evidence: text,
      },
    });
    return this;
  }

  /**
   * Set property merge strategy
   *
   * @example
   * upsertRelationship('task', 'user', 'assignedTo')
   *   .mergeStrategy('deep')
   */
  mergeStrategy(strategy: 'shallow' | 'deep' | 'replace'): ActionBuilder {
    const validStrategies = ['shallow', 'deep', 'replace'];
    if (!validStrategies.includes(strategy)) {
      throw new Error('Invalid merge strategy. Must be: shallow, deep, or replace');
    }
    this.setParams({
      ...this.getSpec().params,
      mergeStrategy: strategy,
    });
    return this;
  }
}

/**
 * Create query builder
 *
 * @example
 * const q = query()
 *   .match(pattern('task').label('Task').where({ status: 'open' }))
 *   .return(['task'])
 */
export function query(): QueryBuilder {
  return new QueryBuilder();
}

/**
 * Shorthand for action builders
 */
export const send = ActionBuilder.send;
export const update = ActionBuilder.update;
export const create = ActionBuilder.create;
export const deleteEntity = ActionBuilder.delete;
export const createRelationship = ActionBuilder.createRelationship;
export const deleteRelationship = ActionBuilder.deleteRelationship;
export const upsertRelationship = ActionBuilder.upsertRelationship;
