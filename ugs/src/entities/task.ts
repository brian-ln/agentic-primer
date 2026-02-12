#!/usr/bin/env bun
/**
 * Task Entity for Universal Graph System
 *
 * Tasks are work specifications with formal success criteria.
 * State machine: pending -> assigned -> in_progress -> completed | failed
 * Any state can go back to pending (unassign)
 */

import GraphStore, { Node } from '../graph.ts';

// Task lifecycle states
export type TaskLifecycle = 'pending' | 'assigned' | 'in_progress' | 'completed' | 'failed';

// Task priority levels
export type TaskPriority = 'P0' | 'P1' | 'P2' | 'P3' | 'P4';

// Success criteria types
export interface SuccessCriterion {
  type: string;  // "file_exists", "word_count", "custom"
  [key: string]: any;
}

// Task specification
export interface TaskSpec {
  inputs?: string[];
  outputs?: string[];
  constraints?: string[];
  successCriteria?: SuccessCriterion[];
}

// Task event types
export type TaskEventType =
  | 'TASK_CREATED'
  | 'TASK_ASSIGNED'
  | 'TASK_STARTED'
  | 'TASK_COMPLETED'
  | 'TASK_FAILED'
  | 'TASK_UPDATED'
  | 'TASK_UNASSIGNED';

// Task config
export interface TaskConfig {
  title: string;
  spec?: TaskSpec;
  assignee?: string;  // "@(agent-id)" or "@(human-id)"
  priority?: TaskPriority;
  description?: string;
  result?: any;
  failureReason?: string;
}

// Task interface
export interface Task {
  id: string;
  type: 'program';
  programType: 'task';
  lifecycle: TaskLifecycle;
  config: TaskConfig;
  version: number;
  created: number;
  modified: number;
}

// Task event structure
export interface TaskEvent {
  id: string;
  timestamp: number;
  type: TaskEventType;
  taskId: string;
  data: any;
}

/**
 * TaskManager - Manages task lifecycle using GraphStore
 */
export class TaskManager {
  private store: GraphStore;
  private taskEvents: TaskEvent[] = [];
  private eventCounter = 0;

  constructor(store: GraphStore) {
    this.store = store;
  }

  /**
   * Get the underlying GraphStore
   */
  getStore(): GraphStore {
    return this.store;
  }

  /**
   * Generate unique event ID
   */
  private generateEventId(): string {
    return `task_evt_${Date.now()}_${++this.eventCounter}`;
  }

  /**
   * Emit a task event
   */
  private async emitEvent(type: TaskEventType, taskId: string, data: any): Promise<void> {
    const event: TaskEvent = {
      id: this.generateEventId(),
      timestamp: Date.now(),
      type,
      taskId,
      data
    };

    this.taskEvents.push(event);

    // Also persist to the graph's event log
    await this.store.addNode(event.id, 'task_event', {
      eventType: type,
      taskId,
      ...data
    });
  }

  /**
   * Create a new task in pending lifecycle
   */
  async createTask(
    id: string,
    title: string,
    options: {
      spec?: TaskSpec;
      assignee?: string;
      priority?: TaskPriority;
      description?: string;
    } = {}
  ): Promise<Task> {
    // Check if task already exists
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Task already exists: ${id}`);
    }

    // Determine initial lifecycle based on whether assignee is provided
    const initialLifecycle: TaskLifecycle = options.assignee ? 'assigned' : 'pending';

    const config: TaskConfig = {
      title,
      spec: options.spec,
      assignee: options.assignee,
      priority: options.priority,
      description: options.description
    };

    const task: Task = {
      id,
      type: 'program',
      programType: 'task',
      lifecycle: initialLifecycle,
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    };

    // Store as a node
    await this.store.addNode(id, 'task', {
      programType: 'task',
      title,
      spec: options.spec ? JSON.stringify(options.spec) : undefined,
      assignee: options.assignee,
      priority: options.priority,
      description: options.description,
      lifecycle: task.lifecycle,
      version: task.version
    });

    await this.emitEvent('TASK_CREATED', id, {
      title,
      lifecycle: task.lifecycle,
      version: task.version,
      assignee: options.assignee
    });

    // If assignee was provided at creation, also emit assigned event
    if (options.assignee) {
      await this.emitEvent('TASK_ASSIGNED', id, {
        assignee: options.assignee,
        previousLifecycle: 'pending',
        newLifecycle: 'assigned'
      });
    }

    return task;
  }

  /**
   * Get a task by ID
   */
  getTask(id: string): Task | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      return null;
    }

    return this.nodeToTask(node);
  }

  /**
   * Convert a Node to a Task
   */
  private nodeToTask(node: Node): Task {
    const specStr = node.properties.get('spec');
    // Handle legacy 'state' property for backwards compatibility
    const lifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;

    return {
      id: node.id,
      type: 'program',
      programType: 'task',
      lifecycle,
      config: {
        title: node.properties.get('title') || '',
        spec: specStr ? JSON.parse(specStr) : undefined,
        assignee: node.properties.get('assignee'),
        priority: node.properties.get('priority') as TaskPriority | undefined,
        description: node.properties.get('description'),
        result: node.properties.get('result') ? JSON.parse(node.properties.get('result')) : undefined,
        failureReason: node.properties.get('failureReason')
      },
      version: node.properties.get('version') || 1,
      created: node.created,
      modified: node.modified
    };
  }

  /**
   * Update a task (only allowed in pending or assigned lifecycle)
   */
  async updateTask(
    id: string,
    updates: {
      title?: string;
      spec?: TaskSpec;
      priority?: TaskPriority;
      description?: string;
    }
  ): Promise<Task> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      throw new Error(`Task not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;
    if (currentLifecycle !== 'pending' && currentLifecycle !== 'assigned') {
      throw new Error(`Cannot update task in ${currentLifecycle} lifecycle. Only pending or assigned tasks can be updated.`);
    }

    // Build properties to update
    const propsToUpdate: Record<string, any> = {};

    if (updates.title !== undefined) {
      propsToUpdate.title = updates.title;
    }
    if (updates.spec !== undefined) {
      propsToUpdate.spec = JSON.stringify(updates.spec);
    }
    if (updates.priority !== undefined) {
      propsToUpdate.priority = updates.priority;
    }
    if (updates.description !== undefined) {
      propsToUpdate.description = updates.description;
    }

    // Increment version
    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    // Persist the update
    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('TASK_UPDATED', id, {
      updates: Object.keys(updates),
      newVersion: currentVersion + 1
    });

    return this.getTask(id)!;
  }

  /**
   * Assign a task to an actor (transition to assigned lifecycle)
   */
  async assignTask(id: string, assignee: string): Promise<Task> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      throw new Error(`Task not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;
    if (currentLifecycle !== 'pending' && currentLifecycle !== 'assigned') {
      throw new Error(`Cannot assign task in ${currentLifecycle} lifecycle. Only pending or assigned tasks can be assigned.`);
    }

    // Persist the lifecycle change and assignee
    await this.store.updateNode(id, { lifecycle: 'assigned', assignee });

    await this.emitEvent('TASK_ASSIGNED', id, {
      assignee,
      previousLifecycle: currentLifecycle,
      newLifecycle: 'assigned'
    });

    return this.getTask(id)!;
  }

  /**
   * Unassign a task (transition back to pending lifecycle)
   */
  async unassignTask(id: string): Promise<Task> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      throw new Error(`Task not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;
    const currentAssignee = node.properties.get('assignee');

    // Allow unassign from any non-terminal lifecycle
    if (currentLifecycle === 'completed' || currentLifecycle === 'failed') {
      throw new Error(`Cannot unassign task in ${currentLifecycle} lifecycle. Terminal tasks cannot be unassigned.`);
    }

    // Persist the lifecycle change and clear assignee
    await this.store.updateNode(id, { lifecycle: 'pending', assignee: '' });

    await this.emitEvent('TASK_UNASSIGNED', id, {
      previousAssignee: currentAssignee,
      previousLifecycle: currentLifecycle,
      newLifecycle: 'pending'
    });

    return this.getTask(id)!;
  }

  /**
   * Start working on a task (transition to in_progress lifecycle)
   */
  async startTask(id: string): Promise<Task> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      throw new Error(`Task not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;
    if (currentLifecycle !== 'assigned') {
      throw new Error(`Cannot start task in ${currentLifecycle} lifecycle. Only assigned tasks can be started.`);
    }

    // Persist the lifecycle change
    await this.store.updateNode(id, { lifecycle: 'in_progress' });

    await this.emitEvent('TASK_STARTED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'in_progress'
    });

    return this.getTask(id)!;
  }

  /**
   * Complete a task (transition to completed lifecycle)
   */
  async completeTask(id: string, result?: any): Promise<Task> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      throw new Error(`Task not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;
    if (currentLifecycle !== 'in_progress') {
      throw new Error(`Cannot complete task in ${currentLifecycle} lifecycle. Only in_progress tasks can be completed.`);
    }

    // Persist the lifecycle change and result
    const updates: Record<string, any> = { lifecycle: 'completed' };
    if (result !== undefined) {
      updates.result = JSON.stringify(result);
    }
    await this.store.updateNode(id, updates);

    await this.emitEvent('TASK_COMPLETED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'completed',
      result
    });

    return this.getTask(id)!;
  }

  /**
   * Fail a task (transition to failed lifecycle)
   */
  async failTask(id: string, reason: string): Promise<Task> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      throw new Error(`Task not found: ${id}`);
    }

    // Handle legacy 'state' property for backwards compatibility
    const currentLifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;
    if (currentLifecycle !== 'in_progress') {
      throw new Error(`Cannot fail task in ${currentLifecycle} lifecycle. Only in_progress tasks can be failed.`);
    }

    // Persist the lifecycle change and reason
    await this.store.updateNode(id, { lifecycle: 'failed', failureReason: reason });

    await this.emitEvent('TASK_FAILED', id, {
      previousLifecycle: currentLifecycle,
      newLifecycle: 'failed',
      reason
    });

    return this.getTask(id)!;
  }

  /**
   * List all tasks, optionally filtered by lifecycle
   */
  listTasks(filter?: { lifecycle?: TaskLifecycle; assignee?: string; priority?: TaskPriority }): Task[] {
    const taskNodes = this.store.getByType('task');
    let tasks = taskNodes.map(node => this.nodeToTask(node));

    if (filter) {
      if (filter.lifecycle) {
        tasks = tasks.filter(t => t.lifecycle === filter.lifecycle);
      }
      if (filter.assignee) {
        tasks = tasks.filter(t => t.config.assignee === filter.assignee);
      }
      if (filter.priority) {
        tasks = tasks.filter(t => t.config.priority === filter.priority);
      }
    }

    return tasks;
  }

  /**
   * Get all task events
   */
  getTaskEvents(limit?: number): TaskEvent[] {
    const events = this.taskEvents.slice();
    if (limit) {
      return events.slice(-limit);
    }
    return events;
  }

  /**
   * Get events for a specific task
   */
  getTaskEventHistory(taskId: string): TaskEvent[] {
    return this.taskEvents.filter(e => e.taskId === taskId);
  }
}

export default TaskManager;
