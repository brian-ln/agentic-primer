/**
 * Task Event Type Definitions
 *
 * Defines typed events for task lifecycle operations.
 * These events are emitted by the task CLI and can be subscribed to via EventBus.
 */

import type { Event } from "../persistence/event-log.ts";

/**
 * Base event metadata
 */
export interface EventMetadata {
  triggeredBy?: string;      // User ID or agent ID
  parentTaskId?: string;      // Parent task if applicable
  workflowId?: string;        // Workflow that triggered this
  labels?: string[];          // Task labels at event time
  priority?: number;          // Task priority at event time
}

/**
 * Task lifecycle events
 */

export interface TaskCreatedEvent extends Event {
  type: "task_created";
  data: {
    goal: string;
    deliverables: string[];
    labels?: string[];
    priority?: number;
    parentTaskId?: string;
  };
  metadata?: EventMetadata;
}

export interface TaskStartedEvent extends Event {
  type: "task_started";
  data: {
    context?: string;
  };
  metadata?: EventMetadata;
}

export interface TaskCompletedEvent extends Event {
  type: "task_completed";
  data: {
    result?: string;
    artifacts?: string[];
  };
  metadata?: EventMetadata;
}

export interface TaskBlockedEvent extends Event {
  type: "task_blocked";
  data: {
    reason: string;
    requiredKnowledge?: string[];
  };
  metadata?: EventMetadata;
}

export interface TaskDeletedEvent extends Event {
  type: "task_deleted";
  data: {
    edgesRemoved: number;
  };
  metadata?: EventMetadata;
}

export interface TaskUpdatedEvent extends Event {
  type: "task_updated";
  data: {
    updatedFields: string[];
    oldValues: Record<string, unknown>;
    newValues: Record<string, unknown>;
  };
  metadata?: EventMetadata;
}

/**
 * Dependency events
 */

export interface DependencyAddedEvent extends Event {
  type: "dependency_added";
  data: {
    fromId: string;
    toId: string;
    edgeType: string;
    edgeId: string;
  };
  metadata?: EventMetadata;
}

export interface DependencyRemovedEvent extends Event {
  type: "dependency_removed";
  data: {
    edgeId: string;
    fromId: string;
    toId: string;
    edgeType: string;
  };
  metadata?: EventMetadata;
}

export interface DependencySatisfiedEvent extends Event {
  type: "dependency_satisfied";
  data: {
    dependentTaskId: string;
    completedTaskId: string;
  };
  metadata?: EventMetadata;
}

/**
 * Batch events
 */

export interface BatchCreatedEvent extends Event {
  type: "batch_created";
  data: {
    taskIds: string[];
    count: number;
    source: string;
  };
  metadata?: EventMetadata;
}

export interface BatchUpdatedEvent extends Event {
  type: "batch_updated";
  data: {
    taskIds: string[];
    action: string;
  };
  metadata?: EventMetadata;
}

/**
 * Workflow events (for future use)
 */

export interface WorkflowTriggeredEvent extends Event {
  type: "workflow_triggered";
  data: {
    workflowId: string;
    trigger: string;
    context: unknown;
  };
  metadata?: EventMetadata;
}

export interface WorkflowCompletedEvent extends Event {
  type: "workflow_completed";
  data: {
    workflowId: string;
    actions: unknown[];
    results: unknown;
  };
  metadata?: EventMetadata;
}

export interface WorkflowFailedEvent extends Event {
  type: "workflow_failed";
  data: {
    workflowId: string;
    error: string;
    step?: string;
  };
  metadata?: EventMetadata;
}

/**
 * Union type of all task events
 */
export type TaskEvent =
  | TaskCreatedEvent
  | TaskStartedEvent
  | TaskCompletedEvent
  | TaskBlockedEvent
  | TaskDeletedEvent
  | TaskUpdatedEvent
  | DependencyAddedEvent
  | DependencyRemovedEvent
  | DependencySatisfiedEvent
  | BatchCreatedEvent
  | BatchUpdatedEvent
  | WorkflowTriggeredEvent
  | WorkflowCompletedEvent
  | WorkflowFailedEvent;

/**
 * Event type string constants
 */
export const EVENT_TYPES = {
  // Task lifecycle
  TASK_CREATED: "task_created",
  TASK_STARTED: "task_started",
  TASK_COMPLETED: "task_completed",
  TASK_BLOCKED: "task_blocked",
  TASK_DELETED: "task_deleted",
  TASK_UPDATED: "task_updated",

  // Dependencies
  DEPENDENCY_ADDED: "dependency_added",
  DEPENDENCY_REMOVED: "dependency_removed",
  DEPENDENCY_SATISFIED: "dependency_satisfied",

  // Batch operations
  BATCH_CREATED: "batch_created",
  BATCH_UPDATED: "batch_updated",

  // Workflows
  WORKFLOW_TRIGGERED: "workflow_triggered",
  WORKFLOW_COMPLETED: "workflow_completed",
  WORKFLOW_FAILED: "workflow_failed",
} as const;

/**
 * Type guard to check if an event is a specific type
 */
export function isTaskEvent(event: Event, type: keyof typeof EVENT_TYPES): event is TaskEvent {
  return event.type === EVENT_TYPES[type];
}
