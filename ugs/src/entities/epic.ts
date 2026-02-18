#!/usr/bin/env bun
/**
 * Epic Entity for Universal Graph System
 *
 * Epics group related Tasks into a meaningful unit of work.
 * State machine: planning -> active -> completed | cancelled
 * Use CONTAINS edges: epic -> task
 */

import GraphStore, { Node } from '../graph.ts';
import { TaskLifecycle } from './task.ts';

// Epic lifecycle states
export type EpicLifecycle = 'planning' | 'active' | 'completed' | 'cancelled';

// Epic config (the user-visible fields)
export interface EpicConfig {
  title: string;
  description?: string;
  owner?: string;
}

// Epic interface (includes backward-compat aliases)
export interface Epic {
  id: string;
  lifecycle: EpicLifecycle;
  /** Alias for lifecycle (backward compat) */
  state: EpicLifecycle;
  config: EpicConfig;
  /** Alias for config (backward compat) */
  data: EpicConfig;
  seq: number;
  version: number;
  created: string;
  modified: string;
}

/**
 * EpicManager - Manages epic lifecycle using GraphStore
 */
export class EpicManager {
  constructor(private store: GraphStore) {}

  /**
   * Build an Epic object from its base fields, adding backward-compat aliases
   */
  private makeEpic(base: Omit<Epic, 'state' | 'data'>): Epic {
    return {
      ...base,
      state: base.lifecycle,
      data: base.config,
    };
  }

  /**
   * Convert a Node to an Epic
   */
  private nodeToEpic(node: Node): Epic {
    const lifecycle = (node.properties.get('lifecycle') || 'planning') as EpicLifecycle;
    return this.makeEpic({
      id: node.id,
      lifecycle,
      config: {
        title: node.properties.get('title') || '',
        description: node.properties.get('description'),
        owner: node.properties.get('owner'),
      },
      seq: node.properties.get('seq') || 1,
      version: node.properties.get('version') || 1,
      created: new Date(node.created).toISOString(),
      modified: new Date(node.modified).toISOString(),
    });
  }

  /**
   * Create a new epic in planning lifecycle
   */
  async createEpic(id: string, config: EpicConfig): Promise<Epic> {
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Epic already exists: ${id}`);
    }

    await this.store.addNode(id, 'epic', {
      title: config.title,
      description: config.description,
      owner: config.owner,
      lifecycle: 'planning',
      seq: 1,
      version: 1,
    });

    return this.getEpic(id)!;
  }

  /**
   * Get an epic by ID
   */
  getEpic(id: string): Epic | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'epic') {
      return null;
    }
    return this.nodeToEpic(node);
  }

  /**
   * List all epics, optionally filtered by lifecycle
   */
  listEpics(filter?: { lifecycle?: EpicLifecycle }): Epic[] {
    const epicNodes = this.store.getByType('epic');
    let epics = epicNodes.map(node => this.nodeToEpic(node));

    if (filter?.lifecycle) {
      epics = epics.filter(e => e.lifecycle === filter.lifecycle);
    }

    return epics;
  }

  /**
   * Update an epic's config fields
   */
  async updateEpic(id: string, changes: Partial<EpicConfig>): Promise<Epic> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'epic') {
      throw new Error(`Epic not found: ${id}`);
    }

    const propsToUpdate: Record<string, any> = {};
    if (changes.title !== undefined) propsToUpdate.title = changes.title;
    if (changes.description !== undefined) propsToUpdate.description = changes.description;
    if (changes.owner !== undefined) propsToUpdate.owner = changes.owner;

    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    await this.store.updateNode(id, propsToUpdate);
    return this.getEpic(id)!;
  }

  /**
   * Transition epic to active lifecycle
   */
  async activateEpic(id: string): Promise<Epic> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'epic') {
      throw new Error(`Epic not found: ${id}`);
    }

    const currentLifecycle = (node.properties.get('lifecycle') || 'planning') as EpicLifecycle;
    if (currentLifecycle !== 'planning') {
      throw new Error(`Cannot activate epic in ${currentLifecycle} lifecycle. Only planning epics can be activated.`);
    }

    await this.store.updateNode(id, { lifecycle: 'active' });
    return this.getEpic(id)!;
  }

  /**
   * Transition epic to completed lifecycle
   */
  async completeEpic(id: string): Promise<Epic> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'epic') {
      throw new Error(`Epic not found: ${id}`);
    }

    const currentLifecycle = (node.properties.get('lifecycle') || 'planning') as EpicLifecycle;
    if (currentLifecycle !== 'active') {
      throw new Error(`Cannot complete epic in ${currentLifecycle} lifecycle. Only active epics can be completed.`);
    }

    await this.store.updateNode(id, { lifecycle: 'completed' });
    return this.getEpic(id)!;
  }

  /**
   * Transition epic to cancelled lifecycle
   */
  async cancelEpic(id: string): Promise<Epic> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'epic') {
      throw new Error(`Epic not found: ${id}`);
    }

    const currentLifecycle = (node.properties.get('lifecycle') || 'planning') as EpicLifecycle;
    if (currentLifecycle === 'completed') {
      throw new Error(`Cannot cancel epic in ${currentLifecycle} lifecycle. Completed epics cannot be cancelled.`);
    }

    await this.store.updateNode(id, { lifecycle: 'cancelled' });
    return this.getEpic(id)!;
  }

  /**
   * Add a task to an epic (creates a CONTAINS edge)
   */
  async addTaskToEpic(epicId: string, taskId: string): Promise<void> {
    const epic = this.getEpic(epicId);
    if (!epic) throw new Error(`Epic not found: ${epicId}`);

    const taskNode = this.store.get(taskId);
    if (!taskNode || !(taskNode instanceof Node) || taskNode.type !== 'task') {
      throw new Error(`Task not found: ${taskId}`);
    }

    const edgeId = `epic-contains:${epicId}:${taskId}`;
    if (this.store.edges.has(edgeId)) {
      return; // already exists, idempotent
    }

    await this.store.addEdge(edgeId, epicId, taskId, 'CONTAINS', {}, 1);
  }

  /**
   * Remove a task from an epic
   */
  async removeTaskFromEpic(epicId: string, taskId: string): Promise<void> {
    const epic = this.getEpic(epicId);
    if (!epic) throw new Error(`Epic not found: ${epicId}`);

    const edgeId = `epic-contains:${epicId}:${taskId}`;
    await this.store.deleteEdge(edgeId);
  }

  /**
   * Get all task IDs in an epic
   */
  getEpicTaskIds(epicId: string): string[] {
    const outgoing = this.store.adjacencyOut.get(epicId) || [];
    return outgoing
      .filter(e => e.type === 'CONTAINS')
      .map(e => e.to);
  }

  /**
   * Get all tasks in an epic (returns partial Task objects from store nodes)
   * Requires the task nodes to exist in the same GraphStore
   */
  getEpicTasks(epicId: string): Array<{ id: string; lifecycle: TaskLifecycle; title: string }> {
    const taskIds = this.getEpicTaskIds(epicId);
    const tasks: Array<{ id: string; lifecycle: TaskLifecycle; title: string }> = [];

    for (const taskId of taskIds) {
      const node = this.store.get(taskId);
      if (node && node instanceof Node && node.type === 'task') {
        const lifecycle = (node.properties.get('lifecycle') || node.properties.get('state') || 'pending') as TaskLifecycle;
        tasks.push({
          id: node.id,
          lifecycle,
          title: node.properties.get('title') || node.id,
        });
      }
    }

    return tasks;
  }

  /**
   * Get completion stats for an epic
   */
  getEpicStats(epicId: string): {
    total: number;
    completed: number;
    pending: number;
    in_progress: number;
    blocked: number;
  } {
    const tasks = this.getEpicTasks(epicId);
    const stats = {
      total: tasks.length,
      completed: 0,
      pending: 0,
      in_progress: 0,
      blocked: 0,
    };

    for (const task of tasks) {
      if (task.lifecycle === 'completed') {
        stats.completed++;
      } else if (task.lifecycle === 'pending') {
        stats.pending++;
      } else if (task.lifecycle === 'in_progress') {
        stats.in_progress++;
      } else if (task.lifecycle === 'assigned') {
        // count assigned as in_progress for reporting purposes
        stats.in_progress++;
      }
    }

    // blocked = pending tasks with unresolved deps (approximate: no dep info here)
    // We count all pending as potentially blocked; callers can refine if needed
    stats.blocked = stats.pending;

    return stats;
  }
}

export default EpicManager;
