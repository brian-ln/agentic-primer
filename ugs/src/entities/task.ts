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

// Backward-compat alias: CLI imports TaskState
export type TaskState = TaskLifecycle;

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
  | 'TASK_UNASSIGNED'
  | 'TASK_DEP_ADDED'
  | 'TASK_DEP_REMOVED'
  | 'TASK_UNBLOCKED'
  | 'TASK_NOTE_APPENDED'
  | 'TASK_DECISION_RECORDED'
  | 'TASK_ARTIFACT_LINKED'
  | 'TASK_LOG_APPENDED';

// Task content event entry (append-only event log per task)
export interface TaskEventEntry {
  id: string;
  taskId: string;
  kind: 'note' | 'decision' | 'link' | 'log';
  text: string;
  author?: string;
  context?: string;      // decisions
  rationale?: string;    // decisions
  linkKind?: 'file' | 'commit' | 'url' | 'issue'; // links
  timestamp: string;
  seq: number;
}

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

// Task interface (includes backward-compat aliases)
export interface Task {
  id: string;
  type: 'program';
  programType: 'task';
  lifecycle: TaskLifecycle;
  /** Alias for lifecycle (backward compat) */
  state: TaskLifecycle;
  config: TaskConfig;
  /** Alias for config (backward compat) */
  data: TaskConfig;
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

// ConflictError for patchTask seq mismatch
export class ConflictError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'ConflictError';
  }
}

// Types for applyBatch
export interface ApplyRecord {
  id: string;
  title?: string;
  priority?: TaskPriority;
  lifecycle?: TaskLifecycle;
  assignee?: string;
  description?: string;
  dependsOn?: string[];
  seq?: number;
  failureReason?: string;
}

// ExportRecord: the JSONL shape written by exportTasks (compatible with applyBatch)
export interface ExportRecord {
  id: string;
  title: string;
  lifecycle: TaskLifecycle;
  priority?: TaskPriority;
  description?: string;
  'depends-on'?: string[];
  assignee?: string;
  tags?: string[];
}

// ExportFilter: controls which tasks are exported
export type ExportFilter = 'all' | 'open' | 'completed' | 'failed';

export interface ApplyResultItem {
  id: string;
  action: 'created' | 'updated';
  seq: number;
}

export interface ConflictInfo {
  id: string;
  reason: string;
}

export interface ErrorInfo {
  id: string;
  error: string;
}

export interface ApplyResult {
  applied: ApplyResultItem[];
  conflicts: ConflictInfo[];
  errors: ErrorInfo[];
  summary: {
    applied: number;
    conflicts: number;
    errors: number;
  };
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
   * Build a Task object from its base fields, adding backward-compat aliases
   */
  private makeTask(base: Omit<Task, 'state' | 'data'>): Task {
    return {
      ...base,
      state: base.lifecycle,
      data: base.config,
    };
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

    const task = this.makeTask({
      id,
      type: 'program',
      programType: 'task',
      lifecycle: initialLifecycle,
      config,
      version: 1,
      created: Date.now(),
      modified: Date.now()
    });

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

    return this.makeTask({
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
    });
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

    // Emit TASK_UNBLOCKED for any pending tasks now unblocked
    await this._emitUnblockedEvents(id);

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

    // Emit TASK_UNBLOCKED for any pending tasks now unblocked
    await this._emitUnblockedEvents(id);

    return this.getTask(id)!;
  }

  /**
   * After a task completes or fails, emit TASK_UNBLOCKED for any tasks
   * that were waiting on it and are now fully unblocked.
   */
  private async _emitUnblockedEvents(completedOrFailedId: string): Promise<void> {
    // Find tasks that depend on completedOrFailedId (incoming DEPENDS_ON edges)
    const incoming = this.store.adjacencyIn.get(completedOrFailedId) || [];
    const waitingTaskIds = incoming
      .filter(e => e.type === 'DEPENDS_ON')
      .map(e => e.from);

    for (const waitingId of waitingTaskIds) {
      const waitingTask = this.getTask(waitingId);
      if (!waitingTask || waitingTask.lifecycle !== 'pending') continue;

      // Check if all deps are now complete/failed
      const { blockers } = this.getDependencies(waitingId);
      if (blockers.length === 0) {
        await this.emitEvent('TASK_UNBLOCKED', waitingId, {
          unblockingTaskId: completedOrFailedId
        });
      }
    }
  }

  /**
   * List all tasks, optionally filtered by lifecycle/state
   */
  listTasks(filter?: {
    lifecycle?: TaskLifecycle;
    state?: TaskLifecycle;  // backward-compat alias
    assignee?: string;
    priority?: TaskPriority;
  }): Task[] {
    const taskNodes = this.store.getByType('task');
    let tasks = taskNodes.map(node => this.nodeToTask(node));

    if (filter) {
      // Support both 'lifecycle' and 'state' filter keys
      const lifecycleFilter = filter.lifecycle || filter.state;
      if (lifecycleFilter) {
        tasks = tasks.filter(t => t.lifecycle === lifecycleFilter);
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
   * Add a dependency: id depends on dependsOnId
   */
  async addDependency(id: string, dependsOnId: string): Promise<void> {
    const task = this.getTask(id);
    if (!task) throw new Error(`Task not found: ${id}`);

    const depTask = this.getTask(dependsOnId);
    if (!depTask) throw new Error(`Task not found: ${dependsOnId}`);

    // Reject direct circular dependency: if dependsOnId already depends on id
    const depDeps = this.store.adjacencyOut.get(dependsOnId) || [];
    const isCircular = depDeps.some(e => e.type === 'DEPENDS_ON' && e.to === id);
    if (isCircular) {
      throw new Error(`Circular dependency: ${dependsOnId} already depends on ${id}`);
    }

    const edgeId = `dep:${id}:${dependsOnId}`;
    // Remove existing edge if any (idempotent add)
    if (this.store.edges.has(edgeId)) {
      return; // already exists
    }

    await this.store.addEdge(edgeId, id, dependsOnId, 'DEPENDS_ON', {}, 1);

    await this.emitEvent('TASK_DEP_ADDED', id, {
      dependsOnId
    });
  }

  /**
   * Remove a dependency: id no longer depends on dependsOnId
   */
  async removeDependency(id: string, dependsOnId: string): Promise<void> {
    const edgeId = `dep:${id}:${dependsOnId}`;
    await this.store.deleteEdge(edgeId);

    await this.emitEvent('TASK_DEP_REMOVED', id, {
      dependsOnId
    });
  }

  /**
   * Get dependencies for a task
   * Returns: { dependsOn: all tasks this depends on, blockers: incomplete ones }
   */
  getDependencies(id: string): { dependsOn: Task[]; blockers: Task[] } {
    const outgoing = this.store.adjacencyOut.get(id) || [];
    const depEdges = outgoing.filter(e => e.type === 'DEPENDS_ON');

    const dependsOn: Task[] = [];
    const blockers: Task[] = [];

    for (const edge of depEdges) {
      const depTask = this.getTask(edge.to);
      if (!depTask) continue;
      dependsOn.push(depTask);
      if (depTask.lifecycle !== 'completed' && depTask.lifecycle !== 'failed') {
        blockers.push(depTask);
      }
    }

    return { dependsOn, blockers };
  }

  /**
   * List all pending tasks with no incomplete dependencies (ready to work)
   */
  listReady(): Task[] {
    const pending = this.listTasks({ lifecycle: 'pending' });
    return pending.filter(task => {
      const { blockers } = this.getDependencies(task.id);
      return blockers.length === 0;
    });
  }

  /**
   * List all pending tasks with at least one incomplete dependency (blocked)
   */
  listBlocked(): Task[] {
    const pending = this.listTasks({ lifecycle: 'pending' });
    return pending.filter(task => {
      const { blockers } = this.getDependencies(task.id);
      return blockers.length > 0;
    });
  }

  /**
   * Partially update a task. Only fields explicitly provided are changed.
   * If seq is provided, it must match the current version or a ConflictError is thrown.
   */
  async patchTask(
    id: string,
    changes: Partial<TaskConfig & { lifecycle?: TaskLifecycle }>,
    seq?: number
  ): Promise<Task> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'task') {
      throw new Error(`Task not found: ${id}`);
    }

    const currentVersion = node.properties.get('version') || 1;

    // Optimistic concurrency check
    if (seq !== undefined && seq !== currentVersion) {
      throw new ConflictError(
        `Conflict: expected seq=${seq} but current version is ${currentVersion}`
      );
    }

    const propsToUpdate: Record<string, any> = {};

    if (changes.title !== undefined) {
      propsToUpdate.title = changes.title === null ? '' : changes.title;
    }
    if (changes.spec !== undefined) {
      propsToUpdate.spec = changes.spec === null ? null : JSON.stringify(changes.spec);
    }
    if (changes.priority !== undefined) {
      propsToUpdate.priority = changes.priority;
    }
    if (changes.description !== undefined) {
      propsToUpdate.description = changes.description;
    }
    if (changes.assignee !== undefined) {
      propsToUpdate.assignee = changes.assignee;
    }
    if (changes.lifecycle !== undefined) {
      propsToUpdate.lifecycle = changes.lifecycle;
    }
    if (changes.result !== undefined) {
      propsToUpdate.result = changes.result === null ? null : JSON.stringify(changes.result);
    }
    if (changes.failureReason !== undefined) {
      propsToUpdate.failureReason = changes.failureReason;
    }

    propsToUpdate.version = currentVersion + 1;

    await this.store.updateNode(id, propsToUpdate);

    await this.emitEvent('TASK_UPDATED', id, {
      updates: Object.keys(changes),
      newVersion: currentVersion + 1
    });

    return this.getTask(id)!;
  }

  /**
   * Apply a batch of create-or-patch records.
   * Phase 1: create or patch each task in parallel
   * Phase 2: wire dependsOn edges in parallel
   * Per-record errors don't fail the whole batch.
   */
  async applyBatch(records: ApplyRecord[]): Promise<ApplyResult> {
    const applied: ApplyResultItem[] = [];
    const conflicts: ConflictInfo[] = [];
    const errors: ErrorInfo[] = [];

    // Phase 1: create or patch
    await Promise.all(records.map(async (record) => {
      try {
        const existing = this.getTask(record.id);
        if (!existing) {
          // Create
          const title = record.title || record.id;
          await this.createTask(record.id, title, {
            priority: record.priority,
            assignee: record.assignee,
            description: record.description,
          });
          // Apply lifecycle transition if specified and not 'pending' or 'assigned'
          if (record.lifecycle && record.lifecycle !== 'pending' && record.lifecycle !== 'assigned') {
            if (record.lifecycle === 'in_progress') {
              // pending -> assigned -> in_progress
              await this.assignTask(record.id, record.assignee || '@(system)');
              await this.startTask(record.id);
            } else if (record.lifecycle === 'completed') {
              // pending -> assigned -> in_progress -> completed
              await this.assignTask(record.id, record.assignee || '@(system)');
              await this.startTask(record.id);
              await this.completeTask(record.id);
            } else if (record.lifecycle === 'failed') {
              // pending -> assigned -> in_progress -> failed
              await this.assignTask(record.id, record.assignee || '@(system)');
              await this.startTask(record.id);
              await this.failTask(record.id, record.failureReason || 'unknown');
            }
          }
          const finalTask = this.getTask(record.id)!;
          applied.push({ id: record.id, action: 'created', seq: finalTask.version });
        } else {
          // Patch
          const changes: Partial<TaskConfig & { lifecycle?: TaskLifecycle }> = {};
          if (record.title !== undefined) changes.title = record.title;
          if (record.priority !== undefined) changes.priority = record.priority;
          if (record.assignee !== undefined) changes.assignee = record.assignee;
          if (record.description !== undefined) changes.description = record.description;
          if (record.lifecycle !== undefined) changes.lifecycle = record.lifecycle;

          const patched = await this.patchTask(record.id, changes, record.seq);
          applied.push({ id: record.id, action: 'updated', seq: patched.version });
        }
      } catch (err: any) {
        if (err instanceof ConflictError) {
          conflicts.push({ id: record.id, reason: err.message });
        } else {
          errors.push({ id: record.id, error: err.message });
        }
      }
    }));

    // Phase 2: wire dependsOn edges
    await Promise.all(records.map(async (record) => {
      if (!record.dependsOn || record.dependsOn.length === 0) return;
      // Only if phase 1 succeeded for this record
      if (conflicts.some(c => c.id === record.id) || errors.some(e => e.id === record.id)) return;

      for (const depId of record.dependsOn) {
        try {
          await this.addDependency(record.id, depId);
        } catch (err: any) {
          errors.push({ id: record.id, error: `dep ${depId}: ${err.message}` });
        }
      }
    }));

    return {
      applied,
      conflicts,
      errors,
      summary: {
        applied: applied.length,
        conflicts: conflicts.length,
        errors: errors.length,
      },
    };
  }

  /**
   * Export tasks as a list of ExportRecord objects suitable for JSONL output.
   * Each record is compatible with applyBatch (ugs task apply).
   *
   * @param filter   'all' | 'open' | 'completed' | 'failed'
   * @param prefix   Only include tasks whose ID starts with this prefix
   */
  exportTasks(filter: ExportFilter = 'all', prefix?: string): ExportRecord[] {
    // Determine lifecycle set to include
    const openLifecycles: TaskLifecycle[] = ['pending', 'in_progress', 'assigned'];

    let tasks = this.listTasks();

    // Apply lifecycle filter
    if (filter === 'open') {
      tasks = tasks.filter(t => openLifecycles.includes(t.lifecycle));
    } else if (filter === 'completed') {
      tasks = tasks.filter(t => t.lifecycle === 'completed');
    } else if (filter === 'failed') {
      tasks = tasks.filter(t => t.lifecycle === 'failed');
    }
    // 'all' — no lifecycle filter

    // Apply ID prefix filter
    if (prefix) {
      tasks = tasks.filter(t => t.id.startsWith(prefix));
    }

    return tasks.map(task => {
      // Resolve dependency IDs from outgoing DEPENDS_ON edges
      const outgoing = this.store.adjacencyOut.get(task.id) || [];
      const depIds = outgoing
        .filter(e => e.type === 'DEPENDS_ON')
        .map(e => e.to);

      const record: ExportRecord = {
        id: task.id,
        title: task.config.title,
        lifecycle: task.lifecycle,
      };

      if (task.config.priority) record.priority = task.config.priority;
      if (task.config.description) record.description = task.config.description;
      if (depIds.length > 0) record['depends-on'] = depIds;
      if (task.config.assignee) record.assignee = task.config.assignee;

      return record;
    });
  }

  // === APPEND-ONLY CONTENT OPERATIONS (Phase 2 / wg-2) ===

  /**
   * Get next seq for a task's event log (count existing HAS_EVENT edges from task)
   */
  private _nextEventSeq(taskId: string): number {
    const outgoing = this.store.adjacencyOut.get(taskId) || [];
    return outgoing.filter(e => e.type === 'HAS_EVENT').length;
  }

  /**
   * Build a TaskEventEntry from a graph node
   */
  private _nodeToEventEntry(node: any): TaskEventEntry {
    const props = node.properties;
    return {
      id: node.id,
      taskId: props.get('taskId'),
      kind: props.get('eventKind'),
      text: props.get('text'),
      author: props.get('author'),
      context: props.get('context'),
      rationale: props.get('rationale'),
      linkKind: props.get('linkKind'),
      timestamp: props.get('timestamp'),
      seq: props.get('seq'),
    };
  }

  /**
   * Append a note to a task
   */
  async appendNote(taskId: string, text: string, author?: string): Promise<TaskEventEntry> {
    const task = this.getTask(taskId);
    if (!task) throw new Error(`Task not found: ${taskId}`);

    const seq = this._nextEventSeq(taskId);
    const eventNodeId = `evt:${taskId}:${seq}`;
    const timestamp = new Date().toISOString();

    const props: Record<string, any> = {
      taskId,
      eventKind: 'note',
      text,
      timestamp,
      seq,
    };
    if (author !== undefined) props.author = author;

    await this.store.addNode(eventNodeId, 'task_event', props);
    await this.store.addEdge(`task-evt:${taskId}:${seq}`, taskId, eventNodeId, 'HAS_EVENT', {}, seq);

    await this.emitEvent('TASK_NOTE_APPENDED', taskId, { seq, text, author });

    return {
      id: eventNodeId,
      taskId,
      kind: 'note',
      text,
      author,
      timestamp,
      seq,
    };
  }

  /**
   * Record a decision on a task
   */
  async recordDecision(
    taskId: string,
    decision: string,
    opts?: { context?: string; rationale?: string; author?: string }
  ): Promise<TaskEventEntry> {
    const task = this.getTask(taskId);
    if (!task) throw new Error(`Task not found: ${taskId}`);

    const seq = this._nextEventSeq(taskId);
    const eventNodeId = `evt:${taskId}:${seq}`;
    const timestamp = new Date().toISOString();

    const props: Record<string, any> = {
      taskId,
      eventKind: 'decision',
      text: decision,
      timestamp,
      seq,
    };
    if (opts?.author !== undefined) props.author = opts.author;
    if (opts?.context !== undefined) props.context = opts.context;
    if (opts?.rationale !== undefined) props.rationale = opts.rationale;

    await this.store.addNode(eventNodeId, 'task_event', props);
    await this.store.addEdge(`task-evt:${taskId}:${seq}`, taskId, eventNodeId, 'HAS_EVENT', {}, seq);

    await this.emitEvent('TASK_DECISION_RECORDED', taskId, { seq, decision, ...opts });

    return {
      id: eventNodeId,
      taskId,
      kind: 'decision',
      text: decision,
      author: opts?.author,
      context: opts?.context,
      rationale: opts?.rationale,
      timestamp,
      seq,
    };
  }

  /**
   * Link an artifact to a task
   */
  async linkArtifact(
    taskId: string,
    artifact: string,
    kind: 'file' | 'commit' | 'url' | 'issue',
    author?: string
  ): Promise<TaskEventEntry> {
    const task = this.getTask(taskId);
    if (!task) throw new Error(`Task not found: ${taskId}`);

    const seq = this._nextEventSeq(taskId);
    const eventNodeId = `evt:${taskId}:${seq}`;
    const timestamp = new Date().toISOString();

    const props: Record<string, any> = {
      taskId,
      eventKind: 'link',
      text: artifact,
      linkKind: kind,
      timestamp,
      seq,
    };
    if (author !== undefined) props.author = author;

    await this.store.addNode(eventNodeId, 'task_event', props);
    await this.store.addEdge(`task-evt:${taskId}:${seq}`, taskId, eventNodeId, 'HAS_EVENT', {}, seq);

    await this.emitEvent('TASK_ARTIFACT_LINKED', taskId, { seq, artifact, kind, author });

    return {
      id: eventNodeId,
      taskId,
      kind: 'link',
      text: artifact,
      author,
      linkKind: kind,
      timestamp,
      seq,
    };
  }

  /**
   * Append a log entry to a task
   */
  async appendLog(taskId: string, text: string, author?: string): Promise<TaskEventEntry> {
    const task = this.getTask(taskId);
    if (!task) throw new Error(`Task not found: ${taskId}`);

    const seq = this._nextEventSeq(taskId);
    const eventNodeId = `evt:${taskId}:${seq}`;
    const timestamp = new Date().toISOString();

    const props: Record<string, any> = {
      taskId,
      eventKind: 'log',
      text,
      timestamp,
      seq,
    };
    if (author !== undefined) props.author = author;

    await this.store.addNode(eventNodeId, 'task_event', props);
    await this.store.addEdge(`task-evt:${taskId}:${seq}`, taskId, eventNodeId, 'HAS_EVENT', {}, seq);

    await this.emitEvent('TASK_LOG_APPENDED', taskId, { seq, text, author });

    return {
      id: eventNodeId,
      taskId,
      kind: 'log',
      text,
      author,
      timestamp,
      seq,
    };
  }

  /**
   * Get all events for a task (ordered by seq)
   */
  getEventLog(taskId: string): TaskEventEntry[] {
    const outgoing = this.store.adjacencyOut.get(taskId) || [];
    const eventEdges = outgoing.filter(e => e.type === 'HAS_EVENT');

    const entries: TaskEventEntry[] = [];
    for (const edge of eventEdges) {
      const node = this.store.nodes.get(edge.to);
      if (node) {
        entries.push(this._nodeToEventEntry(node));
      }
    }

    return entries.sort((a, b) => a.seq - b.seq);
  }

  /**
   * Get decisions only (filtered from getEventLog)
   */
  getDecisions(taskId: string): TaskEventEntry[] {
    return this.getEventLog(taskId).filter(e => e.kind === 'decision');
  }

  /**
   * Get full task context for agent consumption
   */
  primeTask(taskId: string): { task: Task; events: TaskEventEntry[]; decisions: TaskEventEntry[] } {
    const task = this.getTask(taskId);
    if (!task) throw new Error(`Task not found: ${taskId}`);

    const events = this.getEventLog(taskId);
    const decisions = events.filter(e => e.kind === 'decision');

    return { task, events, decisions };
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
