#!/usr/bin/env bun
/**
 * Subscription Manager for Reactive Queries
 *
 * Implements live queries that automatically update when graph patterns match/unmatch.
 * Subscribes to actor ports for state change notifications and re-evaluates patterns.
 */

import type {
  QueryDefinition,
  QueryPlan,
  Subscription,
  SubscriptionCallbacks,
  ExecutionContext,
} from '../types.ts';
import { QueryCompiler } from '../compiler.ts';
import { QueryExecutor } from '@src/messaging/actors/query-executor.ts';
import type { MessageRouter } from '@src/messaging/router.ts';
import type { Channel } from '@src/messaging/channel.ts';
import { address, createMessage } from '@agentic-primer/actors';

/**
 * Internal subscription state
 */
interface SubscriptionState<T = any> {
  id: string;
  query: QueryDefinition;
  plan: QueryPlan;
  callbacks: SubscriptionCallbacks<T>;
  previousResults: Set<string>; // Serialized result keys for diffing
  active: boolean;
  cleanup: () => void;
}

/**
 * SubscriptionManager - Manages reactive query subscriptions
 */
export class SubscriptionManager {
  private subscriptions = new Map<string, SubscriptionState>();
  private nextSubscriptionId = 0;
  private compiler: QueryCompiler;
  private executor: QueryExecutor;
  private router: MessageRouter;
  private loggerAddress: string;
  private hasWarnedNoStore = false; // Only warn once about missing GraphStore

  constructor(
    executor: QueryExecutor,
    router: MessageRouter,
    options: { loggerAddress?: string } = {}
  ) {
    this.executor = executor;
    this.router = router;
    this.compiler = new QueryCompiler();
    this.loggerAddress = options.loggerAddress || 'logger';
  }

  /**
   * Create a new subscription for a query
   */
  async subscribe<T = any>(
    query: QueryDefinition,
    callbacks: SubscriptionCallbacks<T>,
    context?: ExecutionContext
  ): Promise<Subscription> {
    const subscriptionId = `sub-${this.nextSubscriptionId++}`;

    // Compile query plan
    const plan = await this.compiler.compile(query, context);

    // Initialize subscription state
    const state: SubscriptionState<T> = {
      id: subscriptionId,
      query,
      plan,
      callbacks,
      previousResults: new Set(),
      active: true,
      cleanup: () => {}, // Will be set after port subscriptions
    };

    this.subscriptions.set(subscriptionId, state);

    // Subscribe to relevant actor ports for state changes
    const cleanupFns = await this.setupPortSubscriptions(state);

    // Store cleanup function
    state.cleanup = () => {
      cleanupFns.forEach((fn) => fn());
    };

    // Perform initial evaluation to get baseline results
    await this.evaluateSubscription(state);

    // Return subscription handle
    return {
      unsubscribe: () => this.unsubscribe(subscriptionId),
      isActive: () => this.isSubscriptionActive(subscriptionId),
    };
  }

  /**
   * Unsubscribe from a subscription
   */
  private unsubscribe(subscriptionId: string): void {
    const state = this.subscriptions.get(subscriptionId);
    if (!state) return;

    state.active = false;
    state.cleanup();
    this.subscriptions.delete(subscriptionId);
  }

  /**
   * Check if subscription is active
   */
  private isSubscriptionActive(subscriptionId: string): boolean {
    const state = this.subscriptions.get(subscriptionId);
    return state?.active ?? false;
  }

  /**
   * Setup subscriptions to actor ports for state change notifications
   */
  private async setupPortSubscriptions<T>(
    state: SubscriptionState<T>
  ): Promise<(() => void)[]> {
    const cleanupFns: (() => void)[] = [];

    // For graph queries, we need to subscribe to the GraphStore's state changes
    // The router has access to the store, so we look for common store registrations
    const possibleStoreNames = ['store', 'graph', 'graphstore'];

    let storeActor: any = null;
    for (const name of possibleStoreNames) {
      storeActor = this.router.getActor?.(name);
      if (storeActor?.ports?.stateChanges) {
        break;
      }
    }

    if (!storeActor?.ports?.stateChanges) {
      // No store with state-change ports found
      // This means subscriptions will only evaluate once (initial)
      if (!this.hasWarnedNoStore) {
        this.logWarn(
          'No GraphStore with state-change ports found. ' +
            'Subscriptions will not receive updates. Register store with router.registerActor("store", store)',
          { component: 'SubscriptionManager', action: 'setupPortSubscriptions' }
        );
        this.hasWarnedNoStore = true;
      }
      return cleanupFns;
    }

    // Subscribe to store state changes
    const unsubscribe = storeActor.ports.stateChanges.subscribe(
      (event: any) => {
        // Re-evaluate subscription on state changes
        // TODO: Add filtering - only re-evaluate if change is relevant to this subscription
        // TODO: Add debouncing - batch updates within 50ms window
        this.evaluateSubscription(state).catch((error) => {
          if (state.callbacks.onError) {
            state.callbacks.onError(
              new Error(`Re-evaluation error: ${error.message}`)
            );
          }
        });
      }
    );

    cleanupFns.push(unsubscribe);

    return cleanupFns;
  }

  /**
   * Extract actor addresses from query plan that we need to subscribe to
   */
  private extractActorAddresses(plan: QueryPlan): Set<string> {
    const addresses = new Set<string>();

    for (const step of plan.steps) {
      // Extract base actor address (e.g., 'tasks' from 'tasks/task-1')
      const actorPath = step.actor.split('/')[0];
      addresses.add(actorPath);
    }

    return addresses;
  }

  /**
   * Evaluate subscription and emit events on changes
   */
  private async evaluateSubscription<T>(state: SubscriptionState<T>): Promise<void> {
    if (!state.active) return;

    try {
      // Execute query plan to get current results
      const result = await this.executeQueryPlan(state.plan);

      // Convert results to a set of serialized keys for diffing
      const currentResults = new Set<string>();
      const currentResultsData = new Map<string, T>();

      if (result.bindings) {
        for (const [variable, values] of result.bindings) {
          for (const value of values) {
            const key = this.serializeResult(variable, value);
            currentResults.add(key);
            currentResultsData.set(key, value);
          }
        }
      }

      // Compute diff: what's new (matched) and what's gone (unmatched)
      const newMatches = new Set<string>();
      const unmatchedKeys = new Set<string>();

      // Find new matches
      for (const key of currentResults) {
        if (!state.previousResults.has(key)) {
          newMatches.add(key);
        }
      }

      // Find unmatches
      for (const key of state.previousResults) {
        if (!currentResults.has(key)) {
          unmatchedKeys.add(key);
        }
      }

      // Emit events for changes
      // Simple strategy: On ANY evaluation (triggered by state changes),
      // fire onMatch with ALL current matching results.
      // This ensures subscribers get notified of updates to matched nodes.
      if (currentResults.size > 0) {
        const allResults = Array.from(currentResults).map((key) =>
          currentResultsData.get(key)!
        );
        try {
          state.callbacks.onMatch(allResults);
        } catch (error: any) {
          // User callback error - emit to onError if available
          if (state.callbacks.onError) {
            state.callbacks.onError(
              new Error(`onMatch callback error: ${error.message}`)
            );
          }
        }
      }

      if (unmatchedKeys.size > 0 && state.callbacks.onUnmatch) {
        // For unmatches, we need to reconstruct from previous results
        // Since we don't store full previous data, we'll use keys
        const unmatchedResults = Array.from(unmatchedKeys).map((key) => {
          // Parse key back to minimal object
          return this.deserializeResultKey(key) as T;
        });

        try {
          state.callbacks.onUnmatch(unmatchedResults);
        } catch (error: any) {
          // User callback error - emit to onError if available
          if (state.callbacks.onError) {
            state.callbacks.onError(
              new Error(`onUnmatch callback error: ${error.message}`)
            );
          }
        }
      }

      // Update previous results
      state.previousResults = currentResults;
    } catch (error: any) {
      // Error during evaluation
      if (state.callbacks.onError) {
        state.callbacks.onError(
          new Error(`Subscription evaluation error: ${error.message}`)
        );
      }
    }
  }

  /**
   * Execute query plan (delegates to QueryExecutor)
   */
  private async executeQueryPlan(plan: QueryPlan): Promise<any> {
    // Delegate to QueryExecutor's executePlan method
    try {
      return await this.executor.executePlan(plan);
    } catch (error) {
      // Return empty result on error (will be handled by caller)
      return {
        success: false,
        bindings: new Map(),
      };
    }
  }

  /**
   * Serialize a result for diffing
   */
  private serializeResult(variable: string, value: any): string {
    // Create a stable key for this result
    if (value?.id) {
      return `${variable}:${value.id}`;
    }

    // Fallback: JSON serialize
    try {
      return `${variable}:${JSON.stringify(value)}`;
    } catch {
      // Non-serializable, use string representation
      return `${variable}:${String(value)}`;
    }
  }

  /**
   * Deserialize a result key back to minimal object
   */
  private deserializeResultKey(key: string): any {
    const [variable, ...rest] = key.split(':');
    const valueStr = rest.join(':');

    try {
      return JSON.parse(valueStr);
    } catch {
      // Return as string if not JSON
      return { [variable]: valueStr };
    }
  }

  /**
   * Send warning to logger actor (helper method)
   */
  private logWarn(message: string, context?: Record<string, any>): void {
    const logMessage = createMessage(
      address(this.loggerAddress),
      'log.warn',
      { message, context },
      { pattern: 'tell' }
    );
    this.router.tell(logMessage).catch(() => {
      // Logger might not be registered - silently ignore
    });
  }

  /**
   * Get subscription statistics
   */
  getStats() {
    return {
      activeSubscriptions: Array.from(this.subscriptions.values()).filter(
        (s) => s.active
      ).length,
      totalSubscriptions: this.subscriptions.size,
    };
  }

  /**
   * Cleanup all subscriptions
   */
  cleanup(): void {
    for (const [id] of this.subscriptions) {
      this.unsubscribe(id);
    }
  }
}
