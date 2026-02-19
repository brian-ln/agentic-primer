#!/usr/bin/env bun
/**
 * Event Trigger System
 *
 * Declarative event-driven workflows that execute actions when
 * specific events occur and patterns match.
 *
 * Features:
 * - Event bus integration (actor lifecycle events)
 * - Pattern-based filtering
 * - Debouncing/throttling
 * - Memory-efficient cleanup
 * - Error handling with retry
 */

import type { MessageRouter } from '@src/messaging/router.ts';
import type { Address, Message } from '@agentic-primer/actors';
import type { PatternSpec, FilterExpression, ActionSpec } from '../types.ts';
import { address as createAddress } from '@agentic-primer/actors';

/**
 * Event types that triggers can listen for
 */
export type EventType =
  | 'actor.created'
  | 'actor.updated'
  | 'actor.deleted'
  | 'relationship.connected'
  | 'relationship.disconnected'
  | 'task.lifecycle.completed'
  | 'task.lifecycle.failed'
  | 'task.lifecycle.started'
  | 'build.completed'
  | 'build.failed'
  | 'test.passed'
  | 'test.failed'
  | string; // Allow custom event types

/**
 * Event payload structure
 */
export interface EventPayload {
  type: EventType;
  actor?: Address;
  data?: any;
  timestamp: number;
  metadata?: Record<string, any>;
}

/**
 * Trigger specification
 */
export interface TriggerSpec {
  /** Event types to listen for */
  eventTypes: EventType[];

  /** Pattern to filter events (optional) */
  pattern?: PatternSpec;

  /** Additional WHERE filters */
  filters?: FilterExpression[];

  /** Actions to execute on match */
  actions: ActionSpec[];

  /** Debounce delay in ms (optional) */
  debounce?: number;

  /** Throttle interval in ms (optional) */
  throttle?: number;

  /** Max retries on action failure */
  maxRetries?: number;

  /** Trigger ID for tracking */
  id: string;
}

/**
 * Trigger execution context
 */
interface TriggerContext {
  spec: TriggerSpec;
  lastFired: number;
  pendingDebounce?: ReturnType<typeof setTimeout>;
  eventCount: number;
  errorCount: number;
  lastError?: string;
}

/**
 * Event handler callback
 */
type EventHandler = (event: EventPayload) => void | Promise<void>;

/**
 * EventTriggerManager - Manages declarative event triggers
 */
export class EventTriggerManager {
  private triggers = new Map<string, TriggerContext>();
  private eventHandlers = new Map<EventType, Set<EventHandler>>();
  private router: MessageRouter;
  private nextTriggerId = 0;

  constructor(router: MessageRouter) {
    this.router = router;
  }

  /**
   * Register a new trigger
   */
  register(spec: TriggerSpec): string {
    const triggerId = spec.id || `trigger_${this.nextTriggerId++}`;

    const context: TriggerContext = {
      spec: { ...spec, id: triggerId },
      lastFired: 0,
      eventCount: 0,
      errorCount: 0,
    };

    this.triggers.set(triggerId, context);

    // Subscribe to each event type
    for (const eventType of spec.eventTypes) {
      this.subscribeToEvent(eventType, context);
    }

    return triggerId;
  }

  /**
   * Subscribe to an event type
   */
  private subscribeToEvent(eventType: EventType, context: TriggerContext): void {
    if (!this.eventHandlers.has(eventType)) {
      this.eventHandlers.set(eventType, new Set());
    }

    const handler: EventHandler = async (event) => {
      await this.handleEvent(event, context);
    };

    this.eventHandlers.get(eventType)!.add(handler);

    // Store handler for cleanup
    if (!context.spec.metadata) {
      context.spec.metadata = {};
    }
    if (!context.spec.metadata.handlers) {
      context.spec.metadata.handlers = [];
    }
    context.spec.metadata.handlers.push({ eventType, handler });
  }

  /**
   * Handle incoming event
   */
  private async handleEvent(event: EventPayload, context: TriggerContext): Promise<void> {
    context.eventCount++;

    try {
      // Check if event matches pattern
      if (context.spec.pattern && !this.matchesPattern(event, context.spec.pattern)) {
        return;
      }

      // Check additional filters
      if (context.spec.filters && context.spec.filters.length > 0) {
        if (!this.matchesFilters(event, context.spec.filters)) {
          return;
        }
      }

      // Apply debouncing
      if (context.spec.debounce) {
        this.debounce(event, context);
        return;
      }

      // Apply throttling
      if (context.spec.throttle) {
        const now = Date.now();
        if (now - context.lastFired < context.spec.throttle) {
          return; // Throttled
        }
      }

      // Execute actions
      await this.executeActions(event, context);
    } catch (error: any) {
      context.errorCount++;
      context.lastError = error.message;
      console.error(`Trigger ${context.spec.id} error:`, error);
    }
  }

  /**
   * Debounce event handling
   */
  private debounce(event: EventPayload, context: TriggerContext): void {
    // Clear pending debounce
    if (context.pendingDebounce) {
      clearTimeout(context.pendingDebounce);
    }

    // Schedule new execution
    context.pendingDebounce = setTimeout(async () => {
      context.pendingDebounce = undefined;
      await this.executeActions(event, context);
    }, context.spec.debounce);
  }

  /**
   * Execute actions for matched event
   */
  private async executeActions(event: EventPayload, context: TriggerContext): Promise<void> {
    context.lastFired = Date.now();

    const maxRetries = context.spec.maxRetries ?? 0;

    for (const action of context.spec.actions) {
      let retries = 0;
      let success = false;

      while (!success && retries <= maxRetries) {
        try {
          await this.executeAction(action, event);
          success = true;
        } catch (error: any) {
          retries++;
          if (retries > maxRetries) {
            context.errorCount++;
            context.lastError = `Action failed after ${maxRetries} retries: ${error.message}`;
            console.error(context.lastError);
          } else {
            // Wait before retry (exponential backoff)
            await new Promise((resolve) => setTimeout(resolve, Math.pow(2, retries) * 100));
          }
        }
      }
    }
  }

  /**
   * Execute a single action
   */
  private async executeAction(action: ActionSpec, event: EventPayload): Promise<void> {
    // Build message from action
    const message: Message = {
      id: `msg_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      pattern: action.params.pattern || 'tell',
      type: action.params.type || action.type,
      payload: {
        ...action.params.payload,
        triggerEvent: event, // Include trigger event in payload
      },
      to: this.resolveActionTarget(action, event),
      timestamp: Date.now(),
    };

    // Send message via router
    if (message.pattern === 'tell') {
      await this.router.tell(message);
    } else {
      await this.router.ask(message);
    }
  }

  /**
   * Resolve action target address
   */
  private resolveActionTarget(action: ActionSpec, event: EventPayload): Address {
    // If action has explicit actor address
    if (action.params.actor) {
      return action.params.actor;
    }

    // Use event actor if available
    if (event.actor) {
      return event.actor;
    }

    // Fallback: use action target variable
    return createAddress(action.target);
  }

  /**
   * Check if event matches pattern
   */
  private matchesPattern(event: EventPayload, pattern: PatternSpec): boolean {
    // Simple pattern matching on event data
    if (!event.data) {
      return false;
    }

    // Check labels
    if (pattern.labels && pattern.labels.length > 0) {
      if (!event.data.labels || !pattern.labels.some((l) => event.data.labels?.includes(l))) {
        return false;
      }
    }

    // Check WHERE conditions
    if (pattern.where) {
      for (const [key, value] of Object.entries(pattern.where)) {
        if (!this.matchesProperty(event.data, key, value)) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Check if property matches value (supports nested paths)
   */
  private matchesProperty(data: any, path: string, expectedValue: any): boolean {
    const keys = path.split('.');
    let current = data;

    for (const key of keys) {
      if (current == null || typeof current !== 'object') {
        return false;
      }
      current = current[key];
    }

    // Deep equality check for objects
    if (typeof expectedValue === 'object' && expectedValue !== null) {
      return JSON.stringify(current) === JSON.stringify(expectedValue);
    }

    return current === expectedValue;
  }

  /**
   * Check if event matches filters
   */
  private matchesFilters(event: EventPayload, filters: FilterExpression[]): boolean {
    for (const filter of filters) {
      if (!this.evaluateFilter(event, filter)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Evaluate a single filter expression
   */
  private evaluateFilter(event: EventPayload, filter: FilterExpression): boolean {
    if (filter.type === 'logical') {
      if (filter.operator === 'AND') {
        return filter.expressions?.every((f) => this.evaluateFilter(event, f)) ?? false;
      }
      if (filter.operator === 'OR') {
        return filter.expressions?.some((f) => this.evaluateFilter(event, f)) ?? false;
      }
      if (filter.operator === 'NOT') {
        return !this.evaluateFilter(event, filter.expressions![0]);
      }
    }

    if (filter.type === 'comparison') {
      const value = this.getFilterValue(event, filter.property!);
      return this.compareValues(value, filter.operator!, filter.value);
    }

    return true;
  }

  /**
   * Get value from event data using property path
   */
  private getFilterValue(event: EventPayload, property: string): any {
    if (!event.data) return undefined;

    const keys = property.split('.');
    let current = event.data;

    for (const key of keys) {
      if (current == null || typeof current !== 'object') {
        return undefined;
      }
      current = current[key];
    }

    return current;
  }

  /**
   * Compare values using operator
   */
  private compareValues(left: any, operator: string, right: any): boolean {
    switch (operator) {
      case '=':
      case '==':
        return left === right;
      case '!=':
        return left !== right;
      case '>':
        return left > right;
      case '<':
        return left < right;
      case '>=':
        return left >= right;
      case '<=':
        return left <= right;
      case 'in':
        return Array.isArray(right) && right.includes(left);
      case 'contains':
        return Array.isArray(left) && left.includes(right);
      default:
        return false;
    }
  }

  /**
   * Emit an event to all registered handlers
   */
  async emit(event: EventPayload): Promise<void> {
    const handlers = this.eventHandlers.get(event.type);
    if (!handlers || handlers.size === 0) {
      return;
    }

    // Execute handlers in parallel
    await Promise.all(
      Array.from(handlers).map((handler) =>
        handler(event).catch((error) => {
          console.error(`Event handler error for ${event.type}:`, error);
        })
      )
    );
  }

  /**
   * Unregister a trigger
   */
  unregister(triggerId: string): boolean {
    const context = this.triggers.get(triggerId);
    if (!context) {
      return false;
    }

    // Clear pending debounce
    if (context.pendingDebounce) {
      clearTimeout(context.pendingDebounce);
    }

    // Unsubscribe from events
    if (context.spec.metadata?.handlers) {
      for (const { eventType, handler } of context.spec.metadata.handlers) {
        const handlers = this.eventHandlers.get(eventType);
        if (handlers) {
          handlers.delete(handler);
          if (handlers.size === 0) {
            this.eventHandlers.delete(eventType);
          }
        }
      }
    }

    this.triggers.delete(triggerId);
    return true;
  }

  /**
   * Get trigger statistics
   */
  getStats(triggerId: string): {
    eventCount: number;
    errorCount: number;
    lastFired: number;
    lastError?: string;
  } | null {
    const context = this.triggers.get(triggerId);
    if (!context) {
      return null;
    }

    return {
      eventCount: context.eventCount,
      errorCount: context.errorCount,
      lastFired: context.lastFired,
      lastError: context.lastError,
    };
  }

  /**
   * Get all trigger IDs
   */
  listTriggers(): string[] {
    return Array.from(this.triggers.keys());
  }

  /**
   * Clear all triggers
   */
  clear(): void {
    for (const triggerId of this.triggers.keys()) {
      this.unregister(triggerId);
    }
  }

  /**
   * Destroy manager and cleanup
   */
  destroy(): void {
    this.clear();
    this.eventHandlers.clear();
  }
}
