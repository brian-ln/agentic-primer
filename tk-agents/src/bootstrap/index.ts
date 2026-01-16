// Bootstrap API - Self-managing development system

import type { Actor } from "../actors/base";
import type { Graph } from "../graph";
import type { CreateTaskOptions, TaskNode } from "../task";
import type { ObjectiveCriterion, TaskState } from "../types";
import type { AgentTask } from "./bridge";
import type { TaskInjection } from "./injector";
import { taskInjector, TaskInjector } from "./injector";
import { BootstrapRegistry } from "./registry";

/**
 * Task progress with agent information
 */
export interface TaskProgress {
  taskId: string;
  state: TaskState;
  progress: number;
  agentId?: string;
  blockers: string[];
  children: TaskProgress[];
}

/**
 * Bootstrap - Main API for self-managing development
 */
export class Bootstrap {
  readonly registry: BootstrapRegistry;
  readonly injector: TaskInjector;
  private graph: Graph;

  constructor(graph: Graph) {
    this.graph = graph;
    this.registry = new BootstrapRegistry(graph);
    this.injector = taskInjector;
  }

  /**
   * Create an agent-task (agent + task in one operation)
   */
  create(options: {
    agent: Actor;
    goal: string;
    deliverables: string[];
    criteria: ObjectiveCriterion[];
    parentTaskId?: string;
  }): AgentTask {
    const taskOptions: CreateTaskOptions = {
      goal: options.goal,
      desiredDeliverables: options.deliverables,
      objectiveSuccessCriteria: options.criteria,
      parentTaskId: options.parentTaskId,
    };

    return this.registry.registerAgentTask(options.agent, taskOptions, this.graph);
  }

  /**
   * Inject a task dynamically
   */
  inject(injection: TaskInjection): TaskNode {
    return this.injector.inject(injection, this.graph);
  }

  /**
   * Get task progress (with agent info)
   */
  status(taskId: string): TaskProgress {
    const statusResponse = this.graph.send(taskId, "query_status", {}) as {
      state: TaskState;
      progress: number;
      blockers: string[];
      childrenStatus: Array<{ id: string; state: TaskState; progress: number }>;
    };

    const agentId = this.registry.getAgentId(taskId);

    // Recursively get children
    const children: TaskProgress[] = statusResponse.childrenStatus.map((child) =>
      this.status(child.id)
    );

    return {
      taskId,
      state: statusResponse.state,
      progress: statusResponse.progress,
      agentId,
      blockers: statusResponse.blockers,
      children,
    };
  }

  /**
   * Complete an agent-task
   */
  complete(agentId: string, result: unknown): void {
    this.registry.completeAgent(agentId, result);
  }

  /**
   * Block an agent-task
   */
  block(agentId: string, reason: string, requiredKnowledge?: string[]): void {
    this.registry.blockAgent(agentId, reason, requiredKnowledge);
  }

  /**
   * Get all agent-tasks
   */
  list(): AgentTask[] {
    return this.registry.listAgentTasks();
  }

  /**
   * Get project status (starting from root task)
   */
  projectStatus(rootTaskId: string): TaskProgress {
    return this.status(rootTaskId);
  }
}

/**
 * Create a bootstrap instance
 */
export function createBootstrap(graph: Graph): Bootstrap {
  return new Bootstrap(graph);
}

// Re-export types
export type { AgentTask, TaskInjection, TaskProgress };
export { BootstrapRegistry, TaskInjector, taskInjector };
