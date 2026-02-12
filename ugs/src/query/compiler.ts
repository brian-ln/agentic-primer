#!/usr/bin/env bun
/**
 * Query Compiler
 *
 * Translates QueryDefinition → QueryPlan with DAG representation
 * and cost-based optimization (inspired by Halo paper).
 *
 * Key innovations from Halo:
 * - DAG-based query representation
 * - Signature-based operation canonicalization
 * - Cost model with state awareness
 */

import type {
  QueryDefinition,
  QueryPlan,
  PlanStep,
  PlanMetadata,
  PlanCost,
  StepCost,
  ExecutionContext,
  PatternSpec,
  TraversalSpec,
  ActionSpec,
  IndexHint,
} from './types.ts';
import { address, type Address } from '@agentic-primer/actors';
import { createHash } from 'crypto';
import { JoinOptimizer } from './optimizer/join-optimizer.ts';
import { getIndexSelector } from './optimizer/index-selector.ts';
import { PredicatePushdownOptimizer } from './optimizer/predicate-pushdown.ts';

/**
 * Query compiler - translates DSL to executable plans
 */
export class QueryCompiler {
  private joinOptimizer: JoinOptimizer;
  private predicatePushdownOptimizer: PredicatePushdownOptimizer;
  private enableJoinOptimization: boolean;
  private enablePredicatePushdown: boolean;

  constructor(options?: {
    joinOptimizer?: JoinOptimizer;
    enableJoinOptimization?: boolean;
    predicatePushdownOptimizer?: PredicatePushdownOptimizer;
    enablePredicatePushdown?: boolean;
  }) {
    this.joinOptimizer = options?.joinOptimizer || new JoinOptimizer();
    this.predicatePushdownOptimizer = options?.predicatePushdownOptimizer || new PredicatePushdownOptimizer();
    this.enableJoinOptimization = options?.enableJoinOptimization !== false;
    this.enablePredicatePushdown = options?.enablePredicatePushdown !== false;
  }

  /**
   * Compile query definition into execution plan
   */
  async compile(
    query: QueryDefinition,
    context?: ExecutionContext
  ): Promise<QueryPlan> {
    const steps: PlanStep[] = [];
    let stepIdCounter = 0;

    // Generate unique plan ID from query hash
    const planId = this.hashQuery(query);

    // Compile patterns into query steps
    for (const pattern of query.patterns) {
      const step = this.compilePattern(
        pattern,
        `step_${stepIdCounter++}`,
        context
      );
      steps.push(step);
    }

    // Compile traversals
    if (query.traversals) {
      for (const traversal of query.traversals) {
        const step = this.compileTraversal(
          traversal,
          `step_${stepIdCounter++}`,
          steps,
          context
        );
        steps.push(step);
      }
    }

    // Compile actions
    if (query.actions) {
      for (const action of query.actions) {
        const step = this.compileAction(
          action,
          `step_${stepIdCounter++}`,
          steps,
          context
        );
        steps.push(step);
      }
    }

    // Optimize join order for multi-pattern queries BEFORE building dependencies
    if (this.enableJoinOptimization && query.patterns.length > 1) {
      const optimized = this.joinOptimizer.optimizeJoinOrder(steps, context);

      // Replace steps with optimized order, reassign IDs
      steps.length = 0;
      optimized.forEach((step, i) => {
        step.id = `step_${i}`;
        steps.push(step);
      });
    }

    // Build dependency graph and detect parallelism
    this.buildDependencyGraph(steps);

    // Generate index hints (manual + automatic)
    const indexHints = this.generateIndexHints(query, context);

    // Apply index hints to steps
    this.applyIndexHints(steps, indexHints);

    // Estimate costs
    const metadata = this.estimatePlanCost(steps, context, indexHints);

    // Create initial plan
    let plan: QueryPlan = {
      id: planId,
      steps,
      variables: this.extractVariables(query),
      metadata,
      original: query,
    };

    // Apply predicate pushdown optimization
    if (this.enablePredicatePushdown && query.filters && query.filters.length > 0) {
      const optimizationResult = this.predicatePushdownOptimizer.optimize(plan);
      if (optimizationResult.optimized) {
        plan = optimizationResult.plan;
      }
    }

    return plan;
  }

  /**
   * Compile pattern into query step
   */
  private compilePattern(
    pattern: PatternSpec,
    stepId: string,
    context?: ExecutionContext
  ): PlanStep {
    // Determine target actor based on label
    const label = pattern.labels?.[0] || 'unknown';
    const actorAddress = this.getActorAddress(label);

    // Build query message payload
    const payload: any = {
      filter: pattern.where || {},
      limit: 1000, // Default limit
    };

    // Extract path filters from where clause
    const pathFilters = this.extractPathFilters(pattern.where || {});
    if (pathFilters) {
      payload.pathFilter = pathFilters;
      // Store in metadata for SQL compilation
      payload._pathFilterMetadata = {
        hasPathFilter: true,
        filterType: pathFilters.type,
        filterValue: pathFilters.value,
      };
    }

    // Generate operation signature for deduplication
    const signature = this.generateSignature('query', actorAddress, payload);

    // Estimate cost
    const cost = this.estimateStepCost(
      'query',
      actorAddress,
      payload,
      context
    );

    return {
      id: stepId,
      type: 'query',
      actor: actorAddress,
      message: {
        pattern: 'ask',
        type: 'query',
        payload,
        from: address('services/query-executor'),
      },
      bindings: [pattern.variable],
      dependencies: [],
      parallelizable: true,
      signature,
      cost,
      metadata: {
        pathFilter: pathFilters,
      },
    };
  }

  /**
   * Extract path filter properties from where clause
   *
   * Recognizes special path filter keys:
   * - path_prefix
   * - path_pattern
   * - path_exact
   *
   * @internal
   */
  private extractPathFilters(
    where: Record<string, any>
  ): { type: 'exact' | 'prefix' | 'pattern'; value: string } | null {
    if (where.path_exact) {
      return { type: 'exact', value: where.path_exact };
    }

    if (where.path_prefix) {
      return { type: 'prefix', value: where.path_prefix };
    }

    if (where.path_pattern) {
      return { type: 'pattern', value: where.path_pattern };
    }

    return null;
  }

  /**
   * Compile traversal into query step
   */
  private compileTraversal(
    traversal: TraversalSpec,
    stepId: string,
    previousSteps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep {
    // Find dependency on the 'from' variable
    const fromStep = previousSteps.find((s) =>
      s.bindings.includes(traversal.from)
    );

    if (!fromStep) {
      throw new Error(
        `Traversal references unknown variable: ${traversal.from}`
      );
    }

    // Use RelationshipActor for traversal
    const actorAddress = address('domain/relationships');

    const payload = {
      relationship: traversal.relationship,
      direction: traversal.direction,
      depth: traversal.depth || { max: 1 },
    };

    const signature = this.generateSignature(
      'traverse',
      actorAddress,
      payload
    );

    const cost = this.estimateStepCost(
      'traverse',
      actorAddress,
      payload,
      context
    );

    return {
      id: stepId,
      type: 'traverse',
      actor: actorAddress,
      message: {
        pattern: 'ask',
        type: 'traverse',
        payload,
        from: address('services/query-executor'),
      },
      bindings: [traversal.as],
      dependencies: [fromStep.id],
      parallelizable: false, // Depends on previous step
      signature,
      cost,
    };
  }

  /**
   * Compile action into execution step
   */
  private compileAction(
    action: ActionSpec,
    stepId: string,
    previousSteps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep {
    // DELETE actions require special validation
    if (action.type === 'delete') {
      return this.compileDeleteAction(
        action,
        stepId,
        previousSteps,
        context
      );
    }

    // CREATE_RELATIONSHIP actions require special handling
    if (action.type === 'create_relationship') {
      return this.compileCreateRelationshipAction(
        action,
        stepId,
        previousSteps,
        context
      );
    }

    // UPSERT_RELATIONSHIP actions require special handling
    if (action.type === 'upsert_relationship') {
      return this.compileUpsertRelationshipAction(
        action,
        stepId,
        previousSteps,
        context
      );
    }

    // DELETE_RELATIONSHIP actions require special validation
    if (action.type === 'delete_relationship') {
      return this.compileDeleteRelationshipAction(
        action,
        stepId,
        previousSteps,
        context
      );
    }

    // CREATE actions don't require a target entity (they create new ones)
    // SEND/UPDATE actions require the target variable to exist
    const dependencies: string[] = [];
    let targetStep: PlanStep | undefined;

    if (action.type !== 'create') {
      // Find the target variable for non-CREATE actions
      targetStep = previousSteps.find((s) =>
        s.bindings.includes(action.target)
      );

      if (!targetStep) {
        throw new Error(
          `Action references unknown variable: ${action.target}`
        );
      }

      // Action depends on the target being resolved
      dependencies.push(targetStep.id);
    }

    // Build message based on action type
    const messagePayload = this.buildActionPayload(action);
    const messageType = this.getActionMessageType(action);

    // Determine actor address
    const actorAddress = this.getActionActorAddress(action);

    const signature = this.generateSignature(
      messageType,
      actorAddress,
      messagePayload
    );

    const cost = this.estimateStepCost(
      'action',
      actorAddress,
      messagePayload,
      context
    );

    // CREATE actions produce bindings (the created entity)
    const bindings = action.type === 'create' ? [action.target] : [];

    return {
      id: stepId,
      type: 'action',
      actor: actorAddress,
      message: {
        pattern: action.params.pattern || 'tell',
        type: messageType,
        payload: messagePayload,
        from: address('services/query-executor'),
      },
      bindings,
      dependencies,
      parallelizable: true, // Actions on different entities can run in parallel
      signature,
      cost,
      metadata: {
        actionType: action.type,
        targetVariable: action.target,
      },
    };
  }

  /**
   * Compile DELETE action with safety checks
   */
  private compileDeleteAction(
    action: ActionSpec,
    stepId: string,
    previousSteps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep {
    // Safety check: DELETE must be explicitly confirmed
    if (!action.params.confirmed) {
      throw new Error(
        `DELETE action requires explicit confirmation. Use .confirm(), .cascade(), or .soft()`
      );
    }

    // Find the target variable
    const targetStep = previousSteps.find((s) =>
      s.bindings.includes(action.target)
    );

    if (!targetStep) {
      throw new Error(
        `DELETE action references unknown variable: ${action.target}`
      );
    }

    const dependencies = [targetStep.id];

    // Build delete message payload
    const payload: any = {
      soft: action.params.soft || false,
      cascade: action.params.cascade || false,
      requiresBulkConfirmation: !action.params.bulk,
    };

    if (action.params.cascade && action.params.relationships) {
      payload.relationships = action.params.relationships;
    }

    const actorAddress = address(`query/placeholders/${action.target}`);
    const signature = this.generateSignature('delete', actorAddress, payload);
    const cost = this.estimateStepCost('action', actorAddress, payload, context);

    return {
      id: stepId,
      type: 'action',
      actor: actorAddress,
      message: {
        pattern: 'tell',
        type: 'delete',
        payload,
        from: address('services/query-executor'),
      },
      bindings: [],
      dependencies,
      parallelizable: false, // DELETE is not parallelizable for safety
      signature,
      cost,
      metadata: {
        actionType: 'delete',
        targetVariable: action.target,
      },
    };
  }

  /**
   * Compile DELETE_RELATIONSHIP action with safety checks
   */
  /**
   * Compile CREATE_RELATIONSHIP action with validation
   */
  private compileCreateRelationshipAction(
    action: ActionSpec,
    stepId: string,
    previousSteps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep {
    // Validate that both 'from' and 'to' variables exist
    const fromStep = previousSteps.find((s) =>
      s.bindings.includes(action.params.from)
    );

    if (!fromStep) {
      throw new Error(
        `CREATE_RELATIONSHIP action references unknown from variable: ${action.params.from}`
      );
    }

    const toStep = previousSteps.find((s) =>
      s.bindings.includes(action.params.to)
    );

    if (!toStep) {
      throw new Error(
        `CREATE_RELATIONSHIP action references unknown to variable: ${action.params.to}`
      );
    }

    // Both source and target nodes must be resolved first
    const dependencies = [fromStep.id, toStep.id];

    // Build create relationship message payload
    const payload: any = {
      from: action.params.from,
      to: action.params.to,
      type: action.params.type,
      ...action.params.properties,
    };

    // Use RelationshipActor for creation
    const actorAddress = address('domain/relationships');
    const signature = this.generateSignature('create', actorAddress, payload);
    const cost = this.estimateStepCost('action', actorAddress, payload, context);

    return {
      id: stepId,
      type: 'action',
      actor: actorAddress,
      message: {
        pattern: 'ask', // Ask pattern to get relationship ID back
        type: 'create',
        payload,
        from: address('services/query-executor'),
      },
      bindings: [], // Could bind the created relationship if needed
      dependencies,
      parallelizable: false, // Requires both nodes to exist first
      signature,
      cost,
      metadata: {
        actionType: 'create_relationship',
        fromVariable: action.params.from,
        toVariable: action.params.to,
        relationshipType: action.params.type,
      },
    };
  }

  /**
   * Compile UPSERT_RELATIONSHIP action with validation
   */
  private compileUpsertRelationshipAction(
    action: ActionSpec,
    stepId: string,
    previousSteps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep {
    // Validate that both 'from' and 'to' variables exist
    const fromStep = previousSteps.find((s) =>
      s.bindings.includes(action.params.from)
    );

    if (!fromStep) {
      throw new Error(
        `UPSERT_RELATIONSHIP action references unknown from variable: ${action.params.from}`
      );
    }

    const toStep = previousSteps.find((s) =>
      s.bindings.includes(action.params.to)
    );

    if (!toStep) {
      throw new Error(
        `UPSERT_RELATIONSHIP action references unknown to variable: ${action.params.to}`
      );
    }

    // Both source and target nodes must be resolved first
    const dependencies = [fromStep.id, toStep.id];

    // Build upsert relationship message payload
    const payload: any = {
      from: action.params.from,
      to: action.params.to,
      type: action.params.type,
      mergeStrategy: action.params.mergeStrategy || 'shallow',
      ...action.params.properties,
    };

    // Use RelationshipActor for upsert
    const actorAddress = address('domain/relationships');
    const signature = this.generateSignature('upsert', actorAddress, payload);
    const cost = this.estimateStepCost('action', actorAddress, payload, context);

    return {
      id: stepId,
      type: 'action',
      actor: actorAddress,
      message: {
        pattern: 'ask', // Ask pattern to get relationship back
        type: 'upsert',
        payload,
        from: address('services/query-executor'),
      },
      bindings: [], // Could bind the upserted relationship if needed
      dependencies,
      parallelizable: false, // Requires both nodes to exist first, not parallelizable for safety
      signature,
      cost,
      metadata: {
        actionType: 'upsert_relationship',
        fromVariable: action.params.from,
        toVariable: action.params.to,
        relationshipType: action.params.type,
      },
    };
  }

  /**
   * Compile DELETE_RELATIONSHIP action with validation
   */
  private compileDeleteRelationshipAction(
    action: ActionSpec,
    stepId: string,
    previousSteps: PlanStep[],
    context?: ExecutionContext
  ): PlanStep {
    // Safety check: DELETE_RELATIONSHIP must be explicitly confirmed
    if (!action.params.confirmed) {
      throw new Error(
        `DELETE_RELATIONSHIP action requires explicit confirmation. Use .confirm() or .confirmAll()`
      );
    }

    // Find the 'from' variable
    const fromStep = previousSteps.find((s) =>
      s.bindings.includes(action.params.from)
    );

    if (!fromStep) {
      throw new Error(
        `DELETE_RELATIONSHIP action references unknown from variable: ${action.params.from}`
      );
    }

    const dependencies = [fromStep.id];

    // If 'to' is specified, ensure it exists
    if (action.params.to) {
      const toStep = previousSteps.find((s) =>
        s.bindings.includes(action.params.to)
      );

      if (!toStep) {
        throw new Error(
          `DELETE_RELATIONSHIP action references unknown to variable: ${action.params.to}`
        );
      }

      dependencies.push(toStep.id);
    }

    // Build delete relationship message payload
    const payload: any = {
      from: action.params.from,
      to: action.params.to,
      type: action.params.type,
      direction: action.params.direction || 'outbound',
      deleteAll: action.params.deleteAll || false,
      cascadeOrphans: action.params.cascadeOrphans || false,
    };

    // Use RelationshipActor for deletion
    const actorAddress = address('domain/relationships');
    const signature = this.generateSignature('delete_relationship', actorAddress, payload);
    const cost = this.estimateStepCost('action', actorAddress, payload, context);

    return {
      id: stepId,
      type: 'action',
      actor: actorAddress,
      message: {
        pattern: 'tell',
        type: 'delete_relationship',
        payload,
        from: address('services/query-executor'),
      },
      bindings: [],
      dependencies,
      parallelizable: false, // DELETE_RELATIONSHIP is not parallelizable for safety
      signature,
      cost,
      metadata: {
        actionType: 'delete_relationship',
        fromVariable: action.params.from,
        toVariable: action.params.to,
      },
    };
  }

  /**
   * Build message payload for action
   */
  private buildActionPayload(action: ActionSpec): any {
    switch (action.type) {
      case 'create':
        // CREATE: payload is the properties of the new entity
        return action.params.properties || {};

      case 'update':
        // UPDATE: payload is the properties to update
        return action.params.properties || {};

      case 'send':
        // SEND: payload is from params
        return action.params.payload || {};

      case 'delete':
        // DELETE: handled by compileDeleteAction
        return action.params;

      default:
        return action.params;
    }
  }

  /**
   * Get message type for action
   */
  private getActionMessageType(action: ActionSpec): string {
    switch (action.type) {
      case 'create':
        return 'create';

      case 'update':
        return 'update';

      case 'send':
        return action.params.type;

      case 'delete':
        return 'delete';

      default:
        return action.type;
    }
  }

  /**
   * Get actor address for action
   */
  private getActionActorAddress(action: ActionSpec): Address {
    if (action.type === 'create') {
      // For CREATE, target is the entity type (e.g., 'task')
      // Route to the appropriate collection actor (e.g., @tasks)
      const collectionActor = action.target.endsWith('s')
        ? action.target
        : `${action.target}s`;
      return address(collectionActor);
    }

    // For other actions, placeholder will be resolved at runtime
    return address(`query/placeholders/${action.target}`);
  }

  /**
   * Generate index hints for query optimization (manual + automatic)
   */
  private generateIndexHints(
    query: QueryDefinition,
    context?: ExecutionContext
  ): IndexHint[] {
    const hints: IndexHint[] = [];

    // Add manual hints from query metadata
    if (query.metadata?.indexHints) {
      hints.push(...query.metadata.indexHints);
    }

    // Generate automatic hints if no manual hints provided
    // or if manual hints don't cover all patterns
    const manualVariables = new Set(
      query.metadata?.indexHints?.map((h) => h.variable) || []
    );

    const needsAutomatic =
      hints.length === 0 ||
      query.patterns.some((p) => !manualVariables.has(p.variable));

    if (needsAutomatic) {
      const selector = getIndexSelector();
      const autoHints = selector.selectIndexes(query);

      // Only add automatic hints for variables not covered by manual hints
      for (const hint of autoHints) {
        if (!manualVariables.has(hint.variable)) {
          hints.push(hint);
        }
      }
    }

    return hints;
  }

  /**
   * Apply index hints to plan steps
   */
  private applyIndexHints(steps: PlanStep[], hints: IndexHint[]): void {
    // Create lookup map: variable -> hints
    const hintMap = new Map<string, IndexHint[]>();
    for (const hint of hints) {
      if (!hintMap.has(hint.variable)) {
        hintMap.set(hint.variable, []);
      }
      hintMap.get(hint.variable)!.push(hint);
    }

    // Apply hints to query steps
    for (const step of steps) {
      if (step.type !== 'query') continue;

      // Find hints for this step's bindings
      for (const binding of step.bindings) {
        const stepHints = hintMap.get(binding);
        if (!stepHints || stepHints.length === 0) continue;

        // Add hints to step metadata
        if (!step.metadata) {
          step.metadata = {};
        }
        step.metadata.indexHints = stepHints;

        // Add index info to message payload
        const indexes = stepHints.map((h) => h.index);
        step.message.payload.useIndexes = indexes;

        // Adjust cost estimate if using index
        if (stepHints.length > 0) {
          const bestHint = stepHints.reduce((best, current) =>
            (current.confidence || 0) > (best.confidence || 0) ? current : best
          );

          // Reduce latency based on index confidence
          const improvement = (bestHint.confidence || 0) * 0.5; // Up to 50% improvement
          step.cost.latencyMs *= 1 - improvement;
          step.cost.cpuMs *= 1 - improvement;
          step.cost.cacheHitProb = Math.min(
            step.cost.cacheHitProb + improvement,
            0.95
          );
        }
      }
    }
  }

  /**
   * Build dependency graph and mark parallelizable steps
   */
  private buildDependencyGraph(steps: PlanStep[]): void {
    // Already built during compilation
    // Could add more sophisticated analysis here:
    // - Detect potential parallelism based on data flow
    // - Optimize step ordering
    // - Merge compatible steps
  }

  /**
   * Estimate total plan cost
   */
  private estimatePlanCost(
    steps: PlanStep[],
    context?: ExecutionContext,
    indexHints?: IndexHint[]
  ): PlanMetadata {
    // Calculate critical path (makespan)
    const makespan = this.calculateMakespan(steps);

    // Calculate total work (aggregate)
    const totalWork = steps.reduce(
      (sum, step) => sum + step.cost.latencyMs,
      0
    );

    // Estimate resource usage
    const resourceUsage = {
      memoryBytes: steps.length * 1024 * 100, // ~100KB per step
      ioOps: steps.filter((s) => s.type === 'query').length,
      messageCount: steps.length,
    };

    const estimatedCost: PlanCost = {
      makespan,
      totalWork,
      resourceUsage,
    };

    // Find critical path length
    const criticalPathSteps = this.findCriticalPath(steps).length;

    // Legacy string format for backward compatibility
    const legacyIndexes = indexHints?.map((h) => `${h.variable}:${h.index}`) || [];

    return {
      estimatedCost,
      indexes: legacyIndexes,
      indexHints: indexHints || [],
      parallelizable: steps.some((s) => s.parallelizable),
      criticalPathSteps,
      compiledAt: Date.now(),
    };
  }

  /**
   * Calculate makespan (critical path latency)
   */
  private calculateMakespan(steps: PlanStep[]): number {
    // Simple implementation: longest dependency chain
    const stepMap = new Map(steps.map((s) => [s.id, s]));
    const memo = new Map<string, number>();

    const calcPath = (stepId: string): number => {
      if (memo.has(stepId)) {
        return memo.get(stepId)!;
      }

      const step = stepMap.get(stepId)!;
      const depCosts = step.dependencies.map((depId) => calcPath(depId));
      const maxDepCost = depCosts.length > 0 ? Math.max(...depCosts) : 0;
      const totalCost = maxDepCost + step.cost.latencyMs;

      memo.set(stepId, totalCost);
      return totalCost;
    };

    return Math.max(...steps.map((s) => calcPath(s.id)));
  }

  /**
   * Find critical path steps
   */
  private findCriticalPath(steps: PlanStep[]): PlanStep[] {
    // Simplified: return all steps on longest dependency chain
    const stepMap = new Map(steps.map((s) => [s.id, s]));
    let longestPath: PlanStep[] = [];

    const findPath = (stepId: string): PlanStep[] => {
      const step = stepMap.get(stepId)!;
      if (step.dependencies.length === 0) {
        return [step];
      }

      const depPaths = step.dependencies.map((depId) => findPath(depId));
      const longestDepPath = depPaths.reduce((longest, path) =>
        path.length > longest.length ? path : longest
      );

      return [...longestDepPath, step];
    };

    for (const step of steps) {
      const path = findPath(step.id);
      if (path.length > longestPath.length) {
        longestPath = path;
      }
    }

    return longestPath;
  }

  /**
   * Estimate step cost
   */
  private estimateStepCost(
    type: string,
    actor: Address,
    payload: any,
    context?: ExecutionContext
  ): StepCost {
    // Simple cost model (would be enhanced with profiling data)
    const baseCosts = {
      query: 10, // ms
      traverse: 50, // ms
      action: 5, // ms
      filter: 1, // ms
      aggregate: 20, // ms
    };

    const latencyMs = baseCosts[type as keyof typeof baseCosts] || 10;

    // Check if actor is warm (from context)
    const isWarm = context?.warmActors.has(actor) || false;
    const warmBonus = isWarm ? 0.5 : 1.0; // 50% faster if warm

    return {
      latencyMs: latencyMs * warmBonus,
      cpuMs: latencyMs * 0.8,
      resultCount: 10, // Estimate
      cacheHitProb: isWarm ? 0.7 : 0.1,
    };
  }

  /**
   * Generate operation signature for deduplication
   *
   * Implements signature canonicalization from Halo paper.
   */
  private generateSignature(
    operation: string,
    actor: Address,
    payload: any
  ): string {
    // Normalize payload (sort keys, handle semantic equivalence)
    const normalized = this.normalizePayload(payload);

    // Create signature string
    const sigString = `${operation}:${actor}:${JSON.stringify(normalized)}`;

    // Hash for compact representation
    return createHash('sha256').update(sigString).digest('hex').slice(0, 16);
  }

  /**
   * Normalize payload for signature generation
   */
  private normalizePayload(payload: any): any {
    if (typeof payload !== 'object' || payload === null) {
      return payload;
    }

    if (Array.isArray(payload)) {
      return payload.map((item) => this.normalizePayload(item));
    }

    // Sort object keys for consistent hashing
    const sorted: any = {};
    Object.keys(payload)
      .sort()
      .forEach((key) => {
        sorted[key] = this.normalizePayload(payload[key]);
      });

    return sorted;
  }

  /**
   * Hash query definition for plan caching
   */
  private hashQuery(query: QueryDefinition): string {
    // Create stable string representation
    const queryStr = JSON.stringify({
      patterns: query.patterns,
      filters: query.filters,
      traversals: query.traversals,
      aggregations: query.aggregations,
      actions: query.actions,
      returns: query.returns,
    });

    return createHash('sha256').update(queryStr).digest('hex').slice(0, 16);
  }

  /**
   * Extract all variables from query
   */
  private extractVariables(query: QueryDefinition): string[] {
    const variables = new Set<string>();

    // From patterns
    query.patterns.forEach((p) => variables.add(p.variable));

    // From traversals
    query.traversals?.forEach((t) => {
      variables.add(t.from);
      variables.add(t.as);
    });

    // From aggregations
    query.aggregations?.forEach((a) => {
      variables.add(a.variable);
      variables.add(a.as);
    });

    // From CREATE actions (which produce new entities)
    query.actions?.forEach((a) => {
      if (a.type === 'create') {
        variables.add(a.target);
      }
    });

    return Array.from(variables);
  }

  /**
   * Get join optimizer instance (for external access to statistics)
   */
  getJoinOptimizer(): JoinOptimizer {
    return this.joinOptimizer;
  }

  /**
   * Get actor address for entity label
   */
  private getActorAddress(label: string): Address {
    const labelToActor: Record<string, string> = {
      Task: 'tasks',
      Knowledge: 'knowledge',
      Relationship: 'relationships',
      User: 'users',
    };

    // Default to 'store' for unknown/unlabeled patterns
    // This allows queries without labels to work against the graph store
    const actorId = labelToActor[label] || (label === 'unknown' ? 'store' : label.toLowerCase());
    return address(actorId);
  }
}
