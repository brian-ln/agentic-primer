#!/usr/bin/env bun
/**
 * Workflow Builder - Declarative workflow construction
 *
 * Provides fluent API for building workflow definitions.
 * Compiles to WorkflowDefinition format for orchestrator.
 *
 * Examples:
 *   const workflow = buildWorkflow('build-pipeline')
 *     .task('compile', { title: 'Compile TypeScript' })
 *     .task('link', { title: 'Link binaries', dependsOn: ['compile'] })
 *     .task('test', { title: 'Run tests', dependsOn: ['link'] })
 *     .build();
 */

import type { WorkflowDefinition } from './workflow-orchestrator.ts';

export interface TaskDefinition {
  id: string;
  title: string;
  description?: string;
  dependsOn?: string[];
  priority?: 'P0' | 'P1' | 'P2' | 'P3' | 'P4';
}

/**
 * Fluent workflow builder
 */
export class WorkflowBuilder {
  private id: string;
  private name: string;
  private description?: string;
  private tasks: TaskDefinition[] = [];

  constructor(id: string, name?: string) {
    this.id = id;
    this.name = name || id;
  }

  /**
   * Set workflow description
   */
  describe(description: string): this {
    this.description = description;
    return this;
  }

  /**
   * Add a task to the workflow
   */
  task(id: string, config: Omit<TaskDefinition, 'id'>): this {
    this.tasks.push({
      id,
      ...config
    });
    return this;
  }

  /**
   * Add multiple tasks at once
   */
  tasks(tasks: TaskDefinition[]): this {
    this.tasks.push(...tasks);
    return this;
  }

  /**
   * Build workflow definition
   */
  build(): WorkflowDefinition {
    // Validate dependencies
    const taskIds = new Set(this.tasks.map(t => t.id));
    for (const task of this.tasks) {
      if (task.dependsOn) {
        for (const dep of task.dependsOn) {
          if (!taskIds.has(dep)) {
            throw new Error(`Task ${task.id} depends on unknown task: ${dep}`);
          }
        }
      }
    }

    // Check for cycles
    this.detectCycles();

    return {
      id: this.id,
      name: this.name,
      description: this.description,
      tasks: this.tasks
    };
  }

  /**
   * Detect dependency cycles using DFS
   */
  private detectCycles(): void {
    const visited = new Set<string>();
    const recStack = new Set<string>();

    const hasCycle = (taskId: string): boolean => {
      if (!visited.has(taskId)) {
        visited.add(taskId);
        recStack.add(taskId);

        const task = this.tasks.find(t => t.id === taskId);
        if (task?.dependsOn) {
          for (const dep of task.dependsOn) {
            if (!visited.has(dep) && hasCycle(dep)) {
              return true;
            } else if (recStack.has(dep)) {
              return true;
            }
          }
        }
      }

      recStack.delete(taskId);
      return false;
    };

    for (const task of this.tasks) {
      if (hasCycle(task.id)) {
        throw new Error(`Cycle detected in workflow dependencies involving task: ${task.id}`);
      }
    }
  }

  /**
   * Visualize workflow as ASCII DAG
   */
  visualize(): string {
    const lines: string[] = [];
    lines.push(`Workflow: ${this.name} (${this.id})`);
    if (this.description) {
      lines.push(`  ${this.description}`);
    }
    lines.push('');

    // Find root tasks
    const rootTasks = this.tasks.filter(t => !t.dependsOn || t.dependsOn.length === 0);
    const visited = new Set<string>();

    const printTask = (taskId: string, indent: number): void => {
      if (visited.has(taskId)) return;
      visited.add(taskId);

      const task = this.tasks.find(t => t.id === taskId);
      if (!task) return;

      const prefix = '  '.repeat(indent);
      lines.push(`${prefix}→ ${task.id}: ${task.title}`);

      // Find dependents
      const dependents = this.tasks.filter(t =>
        t.dependsOn?.includes(taskId)
      );

      for (const dep of dependents) {
        printTask(dep.id, indent + 1);
      }
    };

    for (const root of rootTasks) {
      printTask(root.id, 0);
    }

    return lines.join('\n');
  }
}

/**
 * Create a new workflow builder
 */
export function buildWorkflow(id: string, name?: string): WorkflowBuilder {
  return new WorkflowBuilder(id, name);
}

/**
 * Common workflow patterns
 */

/**
 * Linear pipeline (A → B → C → D)
 */
export function linearPipeline(
  id: string,
  name: string,
  stages: Array<{ id: string; title: string; description?: string }>
): WorkflowDefinition {
  const builder = buildWorkflow(id, name);

  for (let i = 0; i < stages.length; i++) {
    const stage = stages[i];
    const dependsOn = i > 0 ? [stages[i - 1].id] : undefined;

    builder.task(stage.id, {
      title: stage.title,
      description: stage.description,
      dependsOn
    });
  }

  return builder.build();
}

/**
 * Fan-out pattern (A → [B, C, D] → E)
 */
export function fanOutFanIn(
  id: string,
  name: string,
  root: { id: string; title: string },
  parallel: Array<{ id: string; title: string }>,
  join: { id: string; title: string }
): WorkflowDefinition {
  const builder = buildWorkflow(id, name);

  // Root task
  builder.task(root.id, { title: root.title });

  // Parallel tasks (all depend on root)
  for (const task of parallel) {
    builder.task(task.id, {
      title: task.title,
      dependsOn: [root.id]
    });
  }

  // Join task (depends on all parallel tasks)
  builder.task(join.id, {
    title: join.title,
    dependsOn: parallel.map(t => t.id)
  });

  return builder.build();
}

/**
 * Diamond pattern (A → B → D, A → C → D)
 */
export function diamond(
  id: string,
  name: string,
  start: { id: string; title: string },
  left: { id: string; title: string },
  right: { id: string; title: string },
  end: { id: string; title: string }
): WorkflowDefinition {
  return buildWorkflow(id, name)
    .task(start.id, { title: start.title })
    .task(left.id, { title: left.title, dependsOn: [start.id] })
    .task(right.id, { title: right.title, dependsOn: [start.id] })
    .task(end.id, { title: end.title, dependsOn: [left.id, right.id] })
    .build();
}

export default {
  buildWorkflow,
  linearPipeline,
  fanOutFanIn,
  diamond
};
