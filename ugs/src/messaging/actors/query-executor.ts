#!/usr/bin/env bun
/**
 * QueryExecutor Actor
 *
 * Coordinates query execution using compiled plans.
 * Implements wavefront-style eager execution (Halo paper).
 *
 * Key features:
 * - Executes query plans with dependency tracking
 * - Eager execution of ready steps
 * - Result batching and streaming
 * - Execution profiling and feedback to cache
 */

import { Actor } from '../actor.ts';
import type { MessageRouter } from '../router.ts';
import type {
  Message,
  MessageResponse,
  Address,
} from '@agentic-primer/actors';
import {
  createResponse,
  createErrorResponse,
  address as createAddress,
  generateMessageId,
} from '@agentic-primer/actors';
import { QueryCompiler } from '@src/query/compiler.ts';
import { QueryCache } from '@src/query/cache.ts';
import { SubscriptionManager } from '@src/query/reactive/subscriber.ts';
import { EventTriggerManager, type TriggerSpec, type EventPayload } from '@src/query/reactive/trigger.ts';
import type {
  QueryDefinition,
  QueryPlan,
  QueryResult,
  ExecutionContext,
  ExecutionStats,
  PlanStep,
  StepStats,
  Subscription,
  SubscriptionCallbacks,
} from '@src/query/types.ts';

/**
 * QueryExecutor - Executes compiled query plans
 */
export class QueryExecutor extends Actor {
  private compiler: QueryCompiler;
  private cache: QueryCache;
  private subscriptionManager: SubscriptionManager;
  private triggerManager: EventTriggerManager;
  private executionContext: ExecutionContext;

  constructor(id: string, router: MessageRouter) {
    super(id, router);
    this.compiler = new QueryCompiler();
    this.cache = new QueryCache();
    this.subscriptionManager = new SubscriptionManager(this, router);
    this.triggerManager = new EventTriggerManager(router);

    // Initialize execution context
    this.executionContext = {
      warmActors: new Set(),
      computationCache: new Map(),
      resources: {
        maxConcurrency: 10,
        availableMemory: 1024 * 1024 * 1024, // 1GB
      },
      startTime: Date.now(),
    };
  }

  /**
   * Handle incoming messages
   */
  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      switch (type) {
        case 'execute':
          return await this.handleExecute(message, payload);

        case 'execute-plan':
          return await this.handleExecutePlan(message, payload);

        case 'get-cache-stats':
          return await this.handleGetCacheStats(message);

        case 'clear-cache':
          return await this.handleClearCache(message);

        case 'register-trigger':
          return await this.handleRegisterTrigger(message, payload);

        case 'unregister-trigger':
          return await this.handleUnregisterTrigger(message, payload);

        case 'emit-event':
          return await this.handleEmitEvent(message, payload);

        case 'get-trigger-stats':
          return await this.handleGetTriggerStats(message, payload);

        default:
          return createErrorResponse(
            message,
            `Unknown message type: ${type}`
          );
      }
    } catch (error: any) {
      return createErrorResponse(message, error.message);
    }
  }

  /**
   * Execute query definition
   */
  private async handleExecute(
    message: Message,
    payload: { query: QueryDefinition }
  ): Promise<MessageResponse> {
    const { query } = payload;

    if (!query) {
      return createErrorResponse(message, 'Missing query definition');
    }

    const startTime = Date.now();

    // Try to get cached plan
    let plan = this.cache.get(query, this.executionContext);
    let cacheHit = !!plan;

    if (!plan) {
      // Compile new plan
      plan = await this.compiler.compile(query, this.executionContext);

      // Cache the plan
      this.cache.put(query, plan, this.executionContext);
    }

    // Execute the plan
    const result = await this.executePlan(plan);

    // Record execution statistics
    const stats: ExecutionStats = {
      durationMs: Date.now() - startTime,
      stepsExecuted: result.stats.stepsExecuted,
      messagesSent: result.stats.messagesSent,
      cacheHits: cacheHit ? 1 : 0,
      cacheMisses: cacheHit ? 0 : 1,
      resultsReturned: result.stats.resultsReturned,
      stepStats: result.stats.stepStats,
    };

    this.cache.recordExecution(plan, stats);

    return createResponse(message, {
      result,
      plan: {
        id: plan.id,
        cacheHit,
      },
    });
  }

  /**
   * Execute pre-compiled plan
   */
  private async handleExecutePlan(
    message: Message,
    payload: { plan: QueryPlan }
  ): Promise<MessageResponse> {
    const { plan } = payload;

    if (!plan) {
      return createErrorResponse(message, 'Missing query plan');
    }

    const result = await this.executePlan(plan);

    return createResponse(message, { result });
  }

  /**
   * Get cache statistics
   */
  private async handleGetCacheStats(
    message: Message
  ): Promise<MessageResponse> {
    const cacheStats = this.cache.getCacheStats();
    const queryStats = this.cache.getAllStatistics();

    return createResponse(message, {
      cache: cacheStats,
      queries: queryStats.slice(0, 20), // Top 20
    });
  }

  /**
   * Clear cache
   */
  private async handleClearCache(
    message: Message
  ): Promise<MessageResponse> {
    this.cache.clear();
    this.cache.clearStatistics();

    return createResponse(message, { cleared: true });
  }

  /**
   * Create a reactive subscription for a query
   *
   * The subscription will automatically re-evaluate when relevant actor state changes.
   *
   * @param query - Query definition to subscribe to
   * @param callbacks - Subscription callbacks (onMatch, onUnmatch, onError)
   * @returns Subscription handle for cleanup
   *
   * @example
   * const sub = await executor.subscribe(
   *   query().match(pattern('task').where({ status: 'failed' })).build(),
   *   {
   *     onMatch: (tasks) => console.log('Failed:', tasks),
   *     onUnmatch: (tasks) => console.log('Recovered:', tasks)
   *   }
   * );
   */
  async subscribe<T = any>(
    query: QueryDefinition,
    callbacks: SubscriptionCallbacks<T>
  ): Promise<Subscription> {
    return await this.subscriptionManager.subscribe(query, callbacks, this.executionContext);
  }

  /**
   * Get subscription statistics
   */
  getSubscriptionStats() {
    return this.subscriptionManager.getStats();
  }

  /**
   * Execute query plan
   *
   * Implements wavefront-style eager execution:
   * - Maintain ready queue (dependencies satisfied)
   * - Execute steps from ready queue when resources available
   * - On completion, promote successors to ready queue
   */
  async executePlan(plan: QueryPlan): Promise<QueryResult> {
    const startTime = Date.now();
    const stepResults = new Map<string, any>();
    const stepStats = new Map<string, StepStats>();
    const completed = new Set<string>();
    const inProgress = new Set<string>();

    // Find initial ready steps (no dependencies)
    const readyQueue: PlanStep[] = plan.steps.filter(
      (s) => s.dependencies.length === 0
    );

    let totalMessages = 0;

    // Execute steps in wavefront style
    while (readyQueue.length > 0 || inProgress.size > 0) {
      // Execute ready steps (up to max concurrency)
      const batch: Promise<void>[] = [];

      while (
        readyQueue.length > 0 &&
        batch.length < this.executionContext.resources.maxConcurrency
      ) {
        const step = readyQueue.shift()!;
        inProgress.add(step.id);

        const executePromise = this.executeStep(step, stepResults)
          .then((result) => {
            // Store result
            stepResults.set(step.id, result);
            completed.add(step.id);
            inProgress.delete(step.id);

            // Record step stats
            stepStats.set(step.id, {
              stepId: step.id,
              durationMs: 0, // Would track in production
              resultCount: Array.isArray(result) ? result.length : 1,
              cacheHit: false,
              success: true,
            });

            totalMessages++;

            // Mark actor as warm
            this.executionContext.warmActors.add(step.actor);

            // Find newly ready steps
            for (const nextStep of plan.steps) {
              if (
                !completed.has(nextStep.id) &&
                !inProgress.has(nextStep.id) &&
                !readyQueue.includes(nextStep) &&
                nextStep.dependencies.every((depId) => completed.has(depId))
              ) {
                readyQueue.push(nextStep);
              }
            }
          })
          .catch((error) => {
            inProgress.delete(step.id);
            stepStats.set(step.id, {
              stepId: step.id,
              durationMs: 0,
              resultCount: 0,
              cacheHit: false,
              success: false,
              error: error.message,
            });
          });

        batch.push(executePromise);
      }

      // Wait for batch to complete
      if (batch.length > 0) {
        await Promise.all(batch);
      }

      // If nothing ready and nothing in progress, we're done
      if (readyQueue.length === 0 && inProgress.size === 0) {
        break;
      }
    }

    // Build result bindings
    const bindings = new Map<string, any[]>();

    for (const variable of plan.variables) {
      const values: any[] = [];

      // Collect values from step results
      for (const [stepId, result] of stepResults) {
        const step = plan.steps.find((s) => s.id === stepId);
        if (step?.bindings.includes(variable)) {
          if (Array.isArray(result)) {
            values.push(...result);
          } else {
            values.push(result);
          }
        }
      }

      bindings.set(variable, values);
    }

    // Calculate total results
    const totalResults = Array.from(bindings.values()).reduce(
      (sum, arr) => sum + arr.length,
      0
    );

    return {
      planId: plan.id,
      bindings,
      stats: {
        durationMs: Date.now() - startTime,
        stepsExecuted: completed.size,
        messagesSent: totalMessages,
        cacheHits: 0, // Tracked at plan level
        cacheMisses: 0,
        resultsReturned: totalResults,
        stepStats,
      },
      success: completed.size === plan.steps.length,
    };
  }

  /**
   * Execute single plan step
   */
  private async executeStep(
    step: PlanStep,
    previousResults: Map<string, any>
  ): Promise<any> {
    // Build message with resolved parameters
    const message: Message = {
      id: generateMessageId(),
      pattern: step.message.pattern,
      type: step.message.type,
      payload: step.message.payload,
      from: createAddress('query-executor'),
      to: step.actor,
      timestamp: Date.now(),
    };

    // Handle different action types
    if (step.type === 'action') {
      return await this.executeAction(step, message, previousResults);
    }

    // Send message to actor (for query/traverse steps)
    return await this.sendMessage(step.actor, message);
  }

  /**
   * Execute action step with proper handling for CREATE/UPDATE/SEND/DELETE
   */
  private async executeAction(
    step: PlanStep,
    message: Message,
    previousResults: Map<string, any>
  ): Promise<any> {
    // Determine action type from message
    const actionType = step.message.type;

    if (actionType === 'create') {
      // CREATE: Send to collection actor, returns created entity
      const result = await this.sendMessage(step.actor, message);
      return result;
    }

    // Special handling for CREATE_RELATIONSHIP actions
    if (actionType === 'create_relationship' || (step.metadata?.actionType === 'create_relationship')) {
      return await this.executeCreateRelationshipAction(step, message, previousResults);
    }

    // Special handling for UPSERT_RELATIONSHIP actions
    if (actionType === 'upsert' || (step.metadata?.actionType === 'upsert_relationship')) {
      return await this.executeUpsertRelationshipAction(step, message, previousResults);
    }

    // Special handling for DELETE actions with safety checks
    if (actionType === 'delete') {
      return await this.executeDeleteAction(step, message, previousResults);
    }

    // Special handling for DELETE_RELATIONSHIP actions
    if (actionType === 'delete_relationship') {
      return await this.executeDeleteRelationshipAction(step, message, previousResults);
    }

    // For UPDATE/SEND actions, we need to resolve target entities
    // The target variable is stored in step bindings from compilation
    // We need to find which variable this action operates on
    const targetVariable = this.findActionTargetVariable(step, previousResults);

    if (!targetVariable) {
      throw new Error('Cannot determine target variable for action');
    }

    const targetEntities = previousResults.get(targetVariable);

    if (!targetEntities) {
      throw new Error(`No results found for variable: ${targetVariable}`);
    }

    if (Array.isArray(targetEntities)) {
      // Execute action on each entity in parallel
      const results = await Promise.all(
        targetEntities.map((entity: any) =>
          this.sendMessage(this.resolveEntityAddress(entity), {
            ...message,
            to: this.resolveEntityAddress(entity),
          })
        )
      );
      return results;
    }

    // Single entity
    const entityAddress = this.resolveEntityAddress(targetEntities);
    return await this.sendMessage(entityAddress, {
      ...message,
      to: entityAddress,
    });
  }

  /**
   * Execute DELETE action with safety checks
   */
  private async executeDeleteAction(
    step: PlanStep,
    message: Message,
    previousResults: Map<string, any>
  ): Promise<any> {
    // Find the target variable for this DELETE action
    const targetVariable = this.findActionTargetVariable(step, previousResults);

    if (!targetVariable) {
      throw new Error('Cannot determine target variable for DELETE action');
    }

    const targetEntities = previousResults.get(targetVariable);

    if (!targetEntities) {
      throw new Error(`No entities found for delete target: ${targetVariable}`);
    }

    const entities = Array.isArray(targetEntities) ? targetEntities : [targetEntities];
    const entityCount = entities.length;

    // Safety check: prevent accidental bulk delete
    if (entityCount > 1 && message.payload.requiresBulkConfirmation) {
      throw new Error(
        `DELETE operation would affect ${entityCount} entities. ` +
        `This requires explicit bulk confirmation. ` +
        `Use .confirmBulk(${entityCount}) or limit your query to fewer entities.`
      );
    }

    // Safety check: warn about cascade deletes
    if (message.payload.cascade && entityCount > 0) {
      console.warn(
        `CASCADE DELETE will affect ${entityCount} entities and their related entities. ` +
        `Relationships: ${message.payload.relationships?.join(', ') || 'all'}`
      );
    }

    // Log soft deletes for audit trail
    if (message.payload.soft && entityCount > 0) {
      console.info(
        `SOFT DELETE marking ${entityCount} entities as deleted without removal`
      );
    }

    // Execute delete on each entity
    const results = await Promise.all(
      entities.map((entity: any) => {
        const entityAddress = this.resolveEntityAddress(entity);
        const deleteMessage: Message = {
          ...message,
          to: entityAddress,
        };
        return this.sendMessage(entityAddress, deleteMessage);
      })
    );

    return results;
  }

  /**
   * Execute UPSERT_RELATIONSHIP action (idempotent)
   */
  private async executeUpsertRelationshipAction(
    step: PlanStep,
    message: Message,
    previousResults: Map<string, any>
  ): Promise<any> {
    const payload = message.payload;

    // Get 'from' and 'to' variables from metadata (set during compilation)
    const fromVariable = step.metadata?.fromVariable || payload.from;
    const toVariable = step.metadata?.toVariable || payload.to;

    if (!fromVariable || !toVariable) {
      throw new Error('UPSERT_RELATIONSHIP requires both from and to variables');
    }

    // Get entities from previous results
    const fromEntities = previousResults.get(fromVariable);
    const toEntities = previousResults.get(toVariable);

    if (!fromEntities) {
      throw new Error(`No entities found for from variable: ${fromVariable}`);
    }

    if (!toEntities) {
      throw new Error(`No entities found for to variable: ${toVariable}`);
    }

    // Convert to arrays for consistent handling
    const fromArray = Array.isArray(fromEntities) ? fromEntities : [fromEntities];
    const toArray = Array.isArray(toEntities) ? toEntities : [toEntities];

    const results: any[] = [];

    // Upsert relationships for each from-to pair
    for (const fromEntity of fromArray) {
      const fromAddress = this.resolveEntityAddress(fromEntity);

      for (const toEntity of toArray) {
        const toAddress = this.resolveEntityAddress(toEntity);

        // Build relationship upsert message
        const upsertMessage: Message = {
          ...message,
          type: 'upsert',
          payload: {
            type: payload.type,
            from: fromAddress,
            to: toAddress,
            mergeStrategy: payload.mergeStrategy || 'shallow',
            ...payload, // Include any additional properties (strength, evidence, etc.)
          },
        };

        // Send to RelationshipActor
        const result = await this.sendMessage(step.actor, upsertMessage);
        results.push(result);
      }
    }

    // Return all upserted relationships
    return results.length === 1 ? results[0] : results;
  }

  /**
   * Execute DELETE_RELATIONSHIP action
   */
  /**
   * Execute CREATE_RELATIONSHIP action
   */
  private async executeCreateRelationshipAction(
    step: PlanStep,
    message: Message,
    previousResults: Map<string, any>
  ): Promise<any> {
    const payload = message.payload;

    // Get 'from' and 'to' variables from metadata (set during compilation)
    const fromVariable = step.metadata?.fromVariable || payload.from;
    const toVariable = step.metadata?.toVariable || payload.to;

    if (!fromVariable || !toVariable) {
      throw new Error('CREATE_RELATIONSHIP requires both from and to variables');
    }

    // Get entities from previous results
    const fromEntities = previousResults.get(fromVariable);
    const toEntities = previousResults.get(toVariable);

    if (!fromEntities) {
      throw new Error(`No entities found for from variable: ${fromVariable}`);
    }

    if (!toEntities) {
      throw new Error(`No entities found for to variable: ${toVariable}`);
    }

    // Convert to arrays for consistent handling
    const fromArray = Array.isArray(fromEntities) ? fromEntities : [fromEntities];
    const toArray = Array.isArray(toEntities) ? toEntities : [toEntities];

    const results: any[] = [];

    // Create relationships for each from-to pair
    for (const fromEntity of fromArray) {
      const fromAddress = this.resolveEntityAddress(fromEntity);

      for (const toEntity of toArray) {
        const toAddress = this.resolveEntityAddress(toEntity);

        // Build relationship creation message
        const createMessage: Message = {
          ...message,
          type: 'create',
          payload: {
            type: payload.type,
            from: fromAddress,
            to: toAddress,
            ...payload, // Include any additional properties (strength, evidence, etc.)
          },
        };

        // Send to RelationshipActor
        const result = await this.sendMessage(step.actor, createMessage);
        results.push(result);
      }
    }

    // Return all created relationships
    return results.length === 1 ? results[0] : results;
  }

  /**
   * Execute DELETE_RELATIONSHIP action
   */
  private async executeDeleteRelationshipAction(
    step: PlanStep,
    message: Message,
    previousResults: Map<string, any>
  ): Promise<any> {
    const payload = message.payload;

    // Get 'from' entities
    const fromVariable = payload.from;
    const fromEntities = previousResults.get(fromVariable);

    if (!fromEntities) {
      throw new Error(`No entities found for from variable: ${fromVariable}`);
    }

    const fromArray = Array.isArray(fromEntities) ? fromEntities : [fromEntities];

    // Get 'to' entities if specified
    let toArray: any[] | undefined;
    if (payload.to) {
      const toEntities = previousResults.get(payload.to);
      if (!toEntities) {
        throw new Error(`No entities found for to variable: ${payload.to}`);
      }
      toArray = Array.isArray(toEntities) ? toEntities : [toEntities];
    }

    const results: any[] = [];

    // Delete relationships for each 'from' entity
    for (const fromEntity of fromArray) {
      const fromAddress = this.resolveEntityAddress(fromEntity);

      if (toArray) {
        // Specific from-to relationship deletion
        for (const toEntity of toArray) {
          const toAddress = this.resolveEntityAddress(toEntity);

          // Query relationships matching the pattern
          const queryMessage: Message = {
            ...message,
            type: 'query',
            payload: {
              filter: {
                from: fromAddress,
                to: toAddress,
                ...(payload.type ? { type: payload.type } : {}),
              },
              limit: payload.deleteAll ? 1000 : 1,
            },
          };

          const relationships = await this.sendMessage(step.actor, queryMessage);

          // Delete each matching relationship
          if (relationships && relationships.relationships) {
            for (const rel of relationships.relationships) {
              const deleteMessage: Message = {
                ...message,
                type: 'delete',
                payload: { id: rel.id },
              };
              const deleteResult = await this.sendMessage(step.actor, deleteMessage);
              results.push(deleteResult);
            }
          }
        }
      } else {
        // Delete all relationships from this entity matching the pattern
        const filter: any = {};

        if (payload.direction === 'outbound' || payload.direction === 'both') {
          filter.from = fromAddress;
        }
        if (payload.direction === 'inbound' || payload.direction === 'both') {
          filter.to = fromAddress;
        }
        if (payload.type) {
          filter.type = payload.type;
        }

        const queryMessage: Message = {
          ...message,
          type: 'query',
          payload: {
            filter,
            limit: payload.deleteAll ? 1000 : 1,
          },
        };

        const relationships = await this.sendMessage(step.actor, queryMessage);

        // Delete each matching relationship
        if (relationships && relationships.relationships) {
          const relCount = relationships.relationships.length;

          // Safety check for bulk deletion
          if (relCount > 1 && !payload.deleteAll) {
            throw new Error(
              `DELETE_RELATIONSHIP would affect ${relCount} relationships. ` +
              `Use .confirmAll() to delete multiple relationships or be more specific.`
            );
          }

          for (const rel of relationships.relationships) {
            const deleteMessage: Message = {
              ...message,
              type: 'delete',
              payload: { id: rel.id },
            };
            const deleteResult = await this.sendMessage(step.actor, deleteMessage);
            results.push(deleteResult);
          }

          // Handle orphan cascade if requested
          if (payload.cascadeOrphans && results.length > 0) {
            console.warn(
              `CASCADE_ORPHANS requested but not yet implemented. ` +
              `Would check for orphaned nodes after deleting ${results.length} relationships.`
            );
          }
        }
      }
    }

    return results;
  }

  /**
   * Find the target variable for an action from dependencies
   */
  private findActionTargetVariable(
    step: PlanStep,
    previousResults: Map<string, any>
  ): string | undefined {
    // First, try to get from metadata (most reliable)
    if (step.metadata?.targetVariable) {
      return step.metadata.targetVariable;
    }

    // Fallback: Look through dependencies to find which variables are available
    for (const [variable] of previousResults) {
      if (previousResults.has(variable)) {
        return variable;
      }
    }
    return undefined;
  }

  /**
   * Send message to actor and get result
   * Respects message.pattern to use tell (fire-and-forget) vs ask (request-response) vs stream
   */
  private async sendMessage(
    actor: Address,
    message: Message
  ): Promise<any> {
    // Respect the message pattern: tell vs ask vs stream
    if (message.pattern === 'tell') {
      // Fire-and-forget: no response expected
      await this.router.tell({
        ...message,
        to: actor,
      });
      return null; // TELL returns no data
    }

    if (message.pattern === 'stream') {
      // STREAM pattern: return AsyncIterable
      // Collect all streamed items into an array
      // (In full implementation, this would return the AsyncIterable directly)
      const results: any[] = [];

      for await (const event of this.router.streamAsync(
        actor,
        message.type,
        message.payload,
        {}
      )) {
        if (event.type === 'data') {
          results.push(event.payload);
        } else if (event.type === 'error') {
          throw new Error(event.error || 'Stream error');
        } else if (event.type === 'end') {
          break;
        }
      }

      return results;
    }

    // ASK pattern (default): wait for response
    const response = await this.router.ask({
      ...message,
      to: actor,
      correlationId: message.correlationId || this.generateCorrelationId(),
    });

    if (!response.success) {
      throw new Error(response.error || 'Unknown error');
    }

    return response.payload;
  }
  /**
   * Generate correlation ID for ask messages
   */
  private generateCorrelationId(): string {
    return `corr_${Date.now()}_${Math.random().toString(36).slice(2)}`;
  }

  /**
   * Resolve entity to actor address
   */
  private resolveEntityAddress(entity: any): Address {
    // Simple resolution based on entity type
    if (entity.id) {
      // Assume entity has type prefix in id (e.g., 'task-123')
      const parts = entity.id.split('-');
      if (parts.length >= 2) {
        const type = parts[0] + 's'; // pluralize
        return createAddress(`${type}/${entity.id}`);
      }
    }

    // Fallback: use entity directly as address
    return createAddress(String(entity));
  }

  /**
   * Register an event trigger
   */
  private async handleRegisterTrigger(
    message: Message,
    payload: { trigger: TriggerSpec }
  ): Promise<MessageResponse> {
    const { trigger } = payload;

    if (!trigger) {
      return createErrorResponse(message, 'Missing trigger specification');
    }

    try {
      const triggerId = this.triggerManager.register(trigger);
      return createResponse(message, { triggerId, registered: true });
    } catch (error: any) {
      return createErrorResponse(message, `Failed to register trigger: ${error.message}`);
    }
  }

  /**
   * Unregister an event trigger
   */
  private async handleUnregisterTrigger(
    message: Message,
    payload: { triggerId: string }
  ): Promise<MessageResponse> {
    const { triggerId } = payload;

    if (!triggerId) {
      return createErrorResponse(message, 'Missing trigger ID');
    }

    const success = this.triggerManager.unregister(triggerId);
    return createResponse(message, { unregistered: success });
  }

  /**
   * Emit an event to the trigger system
   */
  private async handleEmitEvent(
    message: Message,
    payload: { event: EventPayload }
  ): Promise<MessageResponse> {
    const { event } = payload;

    if (!event) {
      return createErrorResponse(message, 'Missing event payload');
    }

    try {
      await this.triggerManager.emit(event);
      return createResponse(message, { emitted: true });
    } catch (error: any) {
      return createErrorResponse(message, `Failed to emit event: ${error.message}`);
    }
  }

  /**
   * Get trigger statistics
   */
  private async handleGetTriggerStats(
    message: Message,
    payload: { triggerId?: string }
  ): Promise<MessageResponse> {
    const { triggerId } = payload;

    if (triggerId) {
      const stats = this.triggerManager.getStats(triggerId);
      return createResponse(message, { stats });
    }

    // Return all triggers
    const triggers = this.triggerManager.listTriggers();
    return createResponse(message, { triggers });
  }

  /**
   * Public method to register triggers from query definitions
   */
  registerTrigger(query: QueryDefinition): string {
    const eventTypes = query.metadata?.triggerEventTypes as string[] | undefined;

    if (!eventTypes || eventTypes.length === 0) {
      throw new Error('Query does not have trigger event types. Use .on() method.');
    }

    const triggerSpec: TriggerSpec = {
      id: `trigger_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      eventTypes,
      pattern: query.patterns[0], // Use first pattern as filter
      filters: query.filters,
      actions: query.actions || [],
      debounce: query.metadata?.debounce as number | undefined,
      throttle: query.metadata?.throttle as number | undefined,
      maxRetries: query.metadata?.maxRetries as number | undefined,
    };

    return this.triggerManager.register(triggerSpec);
  }

  /**
   * Public method to emit events
   */
  emitEvent(event: EventPayload): void {
    this.triggerManager.emit(event).catch((error) => {
      console.error('Error emitting event:', error);
    });
  }

  /**
   * Get trigger manager for direct access
   */
  getTriggerManager(): EventTriggerManager {
    return this.triggerManager;
  }
}
