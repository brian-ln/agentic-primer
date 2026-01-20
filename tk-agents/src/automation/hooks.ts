/**
 * Lifecycle Hooks - Event-driven automation system
 *
 * Hooks subscribe to task lifecycle events and execute automated actions.
 * Design principles:
 * - Synchronous execution in priority order
 * - Error isolation (one hook failure doesn't break others)
 * - Composable actions (multiple hooks can contribute)
 * - Enable/disable per hook
 */

import type { Event } from "../persistence/event-log.ts";
import type { Graph } from "../graph.ts";
import type { EventLog } from "../persistence/event-log.ts";

/**
 * Hook execution context - provided to all hook handlers
 */
export interface HookContext {
  /** Task graph for querying task state */
  graph: Graph;

  /** Event log for querying history and emitting new events */
  eventLog: EventLog;

  /** Optional workflow engine (for future use) */
  workflowEngine?: unknown;
}

/**
 * Actions that hooks can return
 */
export interface HookAction {
  /** Action type */
  type: "create_task" | "update_task" | "execute_workflow" | "log";

  /** Action payload (type-specific) */
  payload: unknown;
}

/**
 * Create task action payload
 */
export interface CreateTaskAction extends HookAction {
  type: "create_task";
  payload: {
    goal: string;
    deliverables?: string[];
    labels?: string[];
    priority?: number;
    parentTaskId?: string;
  };
}

/**
 * Update task action payload
 */
export interface UpdateTaskAction extends HookAction {
  type: "update_task";
  payload: {
    taskId: string;
    action: "start" | "complete" | "block" | "transition_to_ready";
    data?: unknown;
  };
}

/**
 * Execute workflow action payload
 */
export interface ExecuteWorkflowAction extends HookAction {
  type: "execute_workflow";
  payload: {
    workflowId: string;
    context: unknown;
  };
}

/**
 * Log event action payload
 */
export interface LogAction extends HookAction {
  type: "log";
  payload: Event;
}

/**
 * Lifecycle hook definition
 */
export interface LifecycleHook {
  /** Unique hook name */
  name: string;

  /** Event types this hook subscribes to */
  eventTypes: string[];

  /**
   * Hook handler function
   * @param event The triggering event
   * @param context Execution context (graph, eventLog, etc.)
   * @returns Array of actions to execute
   */
  handler: (event: Event, context: HookContext) => Promise<HookAction[]>;

  /** Execution priority (lower = runs first, default: 100) */
  priority?: number;

  /** Enable/disable flag (default: true) */
  enabled?: boolean;

  /** Optional metadata */
  metadata?: {
    description?: string;
    author?: string;
    version?: string;
  };
}

/**
 * Base class for implementing lifecycle hooks
 *
 * Provides common utilities for hook implementations.
 */
export abstract class BaseHook implements LifecycleHook {
  abstract name: string;
  abstract eventTypes: string[];
  abstract handler: (event: Event, context: HookContext) => Promise<HookAction[]>;

  priority?: number;
  enabled?: boolean;
  metadata?: {
    description?: string;
    author?: string;
    version?: string;
  };

  constructor(options?: {
    priority?: number;
    enabled?: boolean;
    metadata?: {
      description?: string;
      author?: string;
      version?: string;
    };
  }) {
    this.priority = options?.priority;
    this.enabled = options?.enabled ?? true;
    this.metadata = options?.metadata;
  }

  /**
   * Helper: Check if event's task has a specific label
   */
  protected hasLabel(context: HookContext, nodeId: string, label: string): boolean {
    try {
      const props = context.graph.getNodeProperties(nodeId);
      return props.labels?.includes(label) ?? false;
    } catch {
      return false;
    }
  }

  /**
   * Helper: Get task properties safely
   */
  protected getTaskProps(context: HookContext, nodeId: string): Record<string, unknown> | null {
    try {
      return context.graph.getNodeProperties(nodeId);
    } catch {
      return null;
    }
  }

  /**
   * Helper: Create a log action
   */
  protected createLogAction(eventType: string, nodeId: string, data: unknown): LogAction {
    return {
      type: "log",
      payload: {
        timestamp: new Date().toISOString(),
        type: eventType,
        nodeId,
        data,
      },
    };
  }

  /**
   * Helper: Create a create_task action
   */
  protected createTaskAction(
    goal: string,
    options: {
      deliverables?: string[];
      labels?: string[];
      priority?: number;
      parentTaskId?: string;
    } = {}
  ): CreateTaskAction {
    return {
      type: "create_task",
      payload: {
        goal,
        deliverables: options.deliverables || ["Task completion"],
        labels: options.labels,
        priority: options.priority,
        parentTaskId: options.parentTaskId,
      },
    };
  }

  /**
   * Helper: Create an update_task action
   */
  protected updateTaskAction(
    taskId: string,
    action: "start" | "complete" | "block" | "transition_to_ready",
    data?: unknown
  ): UpdateTaskAction {
    return {
      type: "update_task",
      payload: { taskId, action, data },
    };
  }
}
