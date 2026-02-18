#!/usr/bin/env bun
/**
 * Project Entity for Universal Graph System
 *
 * Projects group Epics (and optionally loose Tasks) into a top-level container.
 * Status machine: draft -> active -> completed | archived
 * Use CONTAINS edges: project -> epic
 */

import GraphStore, { Node } from '../graph.ts';
import { EpicLifecycle } from './epic.ts';

// Project status values
export type ProjectStatus = 'draft' | 'active' | 'completed' | 'archived';

// Project config (the user-visible fields)
export interface ProjectConfig {
  title: string;
  description?: string;
  owner?: string;
}

// Project interface (includes backward-compat aliases)
export interface Project {
  id: string;
  status: ProjectStatus;
  config: ProjectConfig;
  /** Alias for config (backward compat) */
  data: ProjectConfig;
  seq: number;
  version: number;
  created: string;
  modified: string;
}

/**
 * ProjectManager - Manages project lifecycle using GraphStore
 */
export class ProjectManager {
  constructor(private store: GraphStore) {}

  /**
   * Build a Project object from its base fields, adding backward-compat aliases
   */
  private makeProject(base: Omit<Project, 'data'>): Project {
    return {
      ...base,
      data: base.config,
    };
  }

  /**
   * Convert a Node to a Project
   */
  private nodeToProject(node: Node): Project {
    const status = (node.properties.get('status') || 'draft') as ProjectStatus;
    return this.makeProject({
      id: node.id,
      status,
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
   * Create a new project in draft status
   */
  async createProject(id: string, config: ProjectConfig): Promise<Project> {
    const existing = this.store.get(id);
    if (existing) {
      throw new Error(`Project already exists: ${id}`);
    }

    await this.store.addNode(id, 'project', {
      title: config.title,
      description: config.description,
      owner: config.owner,
      status: 'draft',
      seq: 1,
      version: 1,
    });

    return this.getProject(id)!;
  }

  /**
   * Get a project by ID
   */
  getProject(id: string): Project | null {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'project') {
      return null;
    }
    return this.nodeToProject(node);
  }

  /**
   * List all projects, optionally filtered by status
   */
  listProjects(filter?: { status?: ProjectStatus }): Project[] {
    const projectNodes = this.store.getByType('project');
    let projects = projectNodes.map(node => this.nodeToProject(node));

    if (filter?.status) {
      projects = projects.filter(p => p.status === filter.status);
    }

    return projects;
  }

  /**
   * Update a project's config fields
   */
  async updateProject(id: string, changes: Partial<ProjectConfig>): Promise<Project> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'project') {
      throw new Error(`Project not found: ${id}`);
    }

    const propsToUpdate: Record<string, any> = {};
    if (changes.title !== undefined) propsToUpdate.title = changes.title;
    if (changes.description !== undefined) propsToUpdate.description = changes.description;
    if (changes.owner !== undefined) propsToUpdate.owner = changes.owner;

    const currentVersion = node.properties.get('version') || 1;
    propsToUpdate.version = currentVersion + 1;

    await this.store.updateNode(id, propsToUpdate);
    return this.getProject(id)!;
  }

  /**
   * Transition project to active status
   */
  async activateProject(id: string): Promise<Project> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'project') {
      throw new Error(`Project not found: ${id}`);
    }

    const currentStatus = (node.properties.get('status') || 'draft') as ProjectStatus;
    if (currentStatus !== 'draft') {
      throw new Error(`Cannot activate project in ${currentStatus} status. Only draft projects can be activated.`);
    }

    await this.store.updateNode(id, { status: 'active' });
    return this.getProject(id)!;
  }

  /**
   * Transition project to completed status
   */
  async completeProject(id: string): Promise<Project> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'project') {
      throw new Error(`Project not found: ${id}`);
    }

    const currentStatus = (node.properties.get('status') || 'draft') as ProjectStatus;
    if (currentStatus !== 'active') {
      throw new Error(`Cannot complete project in ${currentStatus} status. Only active projects can be completed.`);
    }

    await this.store.updateNode(id, { status: 'completed' });
    return this.getProject(id)!;
  }

  /**
   * Transition project to archived status
   */
  async archiveProject(id: string): Promise<Project> {
    const node = this.store.get(id);
    if (!node || !(node instanceof Node) || node.type !== 'project') {
      throw new Error(`Project not found: ${id}`);
    }

    const currentStatus = (node.properties.get('status') || 'draft') as ProjectStatus;
    if (currentStatus === 'archived') {
      throw new Error(`Project ${id} is already archived.`);
    }

    await this.store.updateNode(id, { status: 'archived' });
    return this.getProject(id)!;
  }

  /**
   * Add an epic to a project (creates a CONTAINS edge)
   */
  async addEpicToProject(projectId: string, epicId: string): Promise<void> {
    const project = this.getProject(projectId);
    if (!project) throw new Error(`Project not found: ${projectId}`);

    const epicNode = this.store.get(epicId);
    if (!epicNode || !(epicNode instanceof Node) || epicNode.type !== 'epic') {
      throw new Error(`Epic not found: ${epicId}`);
    }

    const edgeId = `project-contains:${projectId}:${epicId}`;
    if (this.store.edges.has(edgeId)) {
      return; // already exists, idempotent
    }

    await this.store.addEdge(edgeId, projectId, epicId, 'CONTAINS', {}, 1);
  }

  /**
   * Remove an epic from a project
   */
  async removeEpicFromProject(projectId: string, epicId: string): Promise<void> {
    const project = this.getProject(projectId);
    if (!project) throw new Error(`Project not found: ${projectId}`);

    const edgeId = `project-contains:${projectId}:${epicId}`;
    await this.store.deleteEdge(edgeId);
  }

  /**
   * Get all epic IDs in a project
   */
  getProjectEpicIds(projectId: string): string[] {
    const outgoing = this.store.adjacencyOut.get(projectId) || [];
    return outgoing
      .filter(e => e.type === 'CONTAINS')
      .map(e => e.to);
  }

  /**
   * Get all epics in a project (returns partial Epic objects from store nodes)
   */
  getProjectEpics(projectId: string): Array<{ id: string; lifecycle: EpicLifecycle; title: string }> {
    const epicIds = this.getProjectEpicIds(projectId);
    const epics: Array<{ id: string; lifecycle: EpicLifecycle; title: string }> = [];

    for (const epicId of epicIds) {
      const node = this.store.get(epicId);
      if (node && node instanceof Node && node.type === 'epic') {
        const lifecycle = (node.properties.get('lifecycle') || 'planning') as EpicLifecycle;
        epics.push({
          id: node.id,
          lifecycle,
          title: node.properties.get('title') || node.id,
        });
      }
    }

    return epics;
  }

  /**
   * Get rolled-up stats for a project (aggregates all epics and their tasks)
   */
  getProjectStats(projectId: string): {
    epics: number;
    tasks: number;
    completed: number;
  } {
    const epicIds = this.getProjectEpicIds(projectId);
    let totalTasks = 0;
    let completedTasks = 0;

    for (const epicId of epicIds) {
      // Find all task nodes reachable from this epic via CONTAINS edges
      const outgoing = this.store.adjacencyOut.get(epicId) || [];
      const taskEdges = outgoing.filter(e => e.type === 'CONTAINS');

      for (const edge of taskEdges) {
        const taskNode = this.store.get(edge.to);
        if (taskNode && taskNode instanceof Node && taskNode.type === 'task') {
          totalTasks++;
          const lifecycle = (taskNode.properties.get('lifecycle') || taskNode.properties.get('state') || 'pending');
          if (lifecycle === 'completed') {
            completedTasks++;
          }
        }
      }
    }

    return {
      epics: epicIds.length,
      tasks: totalTasks,
      completed: completedTasks,
    };
  }
}

export default ProjectManager;
