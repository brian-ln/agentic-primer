// Bootstrap Registry - Actor management with task integration

import { EventEmitter } from "events";
import type { Actor, Message, Response } from "../actors/base";
import { Registry } from "../actors/registry";
import type { Graph } from "../graph";
import { createTask, type CreateTaskOptions } from "../task";
import type { AgentTask } from "./bridge";
import {
  handleAgentBlocked,
  handleAgentCompletion,
  handleAgentDeath,
  syncAgentToTask,
} from "./bridge";

/**
 * BootstrapRegistry - Manages agents as tasks
 */
export class BootstrapRegistry extends Registry {
  private agentTasks: Map<string, AgentTask> = new Map();
  private graph: Graph | null = null;

  constructor(graph?: Graph) {
    super();
    if (graph) {
      this.graph = graph;
    }

    // Listen for actor deaths
    this.on("actor_died", (event: { actorId: string; error: string }) => {
      this.onActorDied(event.actorId, event.error);
    });
  }

  /**
   * Set the graph for task management
   */
  setGraph(graph: Graph): void {
    this.graph = graph;
  }

  /**
   * Register an agent and create its task node
   */
  registerAgentTask(
    actor: Actor,
    taskOptions: CreateTaskOptions,
    graph?: Graph
  ): AgentTask {
    const targetGraph = graph || this.graph;
    if (!targetGraph) {
      throw new Error("No graph available for agent-task registration");
    }

    // Register actor with base Registry
    this.register(actor);

    // Create task node
    const taskNode = createTask(taskOptions, targetGraph);

    // Link to parent if specified
    if (taskOptions.parentTaskId) {
      targetGraph.addEdge(taskNode.properties.id, taskOptions.parentTaskId, "spawned_by");
    }

    // Create bridge
    const agentTask: AgentTask = {
      taskId: taskNode.properties.id,
      agentId: actor.id,
    };

    this.agentTasks.set(actor.id, agentTask);

    // Initial sync
    syncAgentToTask(agentTask, actor, targetGraph);

    return agentTask;
  }

  /**
   * Sync agent state to task state
   */
  syncAgentToTask(agentId: string): void {
    const agentTask = this.agentTasks.get(agentId);
    if (!agentTask || !this.graph) return;

    const actor = this.get(agentId);
    if (!actor) return;

    syncAgentToTask(agentTask, actor, this.graph);
  }

  /**
   * Handle actor died event
   */
  private onActorDied(agentId: string, reason: string): void {
    const agentTask = this.agentTasks.get(agentId);
    if (agentTask && this.graph) {
      handleAgentDeath(agentTask, reason, this.graph);
    }
    this.agentTasks.delete(agentId);
  }

  /**
   * Override send to intercept agent responses
   */
  override async send(actorId: string, message: Message): Promise<Response> {
    const response = await super.send(actorId, message);

    // Sync task state after message
    if (response.success) {
      this.syncAgentToTask(actorId);
    }

    return response;
  }

  /**
   * Mark agent as completed
   */
  completeAgent(agentId: string, result: unknown): void {
    const agentTask = this.agentTasks.get(agentId);
    if (!agentTask || !this.graph) return;

    handleAgentCompletion(agentTask, result, this.graph);
  }

  /**
   * Mark agent as blocked
   */
  blockAgent(agentId: string, reason: string, requiredKnowledge?: string[]): void {
    const agentTask = this.agentTasks.get(agentId);
    if (!agentTask || !this.graph) return;

    handleAgentBlocked(agentTask, reason, requiredKnowledge, this.graph);
  }

  /**
   * Get agent-task mapping
   */
  getAgentTask(agentId: string): AgentTask | undefined {
    return this.agentTasks.get(agentId);
  }

  /**
   * Get task ID for an agent
   */
  getTaskId(agentId: string): string | undefined {
    return this.agentTasks.get(agentId)?.taskId;
  }

  /**
   * Get agent ID for a task
   */
  getAgentId(taskId: string): string | undefined {
    for (const [agentId, agentTask] of this.agentTasks.entries()) {
      if (agentTask.taskId === taskId) {
        return agentId;
      }
    }
    return undefined;
  }

  /**
   * List all agent-tasks
   */
  listAgentTasks(): AgentTask[] {
    return Array.from(this.agentTasks.values());
  }

  /**
   * Override unregister to clean up task
   */
  override unregister(actorId: string): boolean {
    this.agentTasks.delete(actorId);
    return super.unregister(actorId);
  }

  /**
   * Override clear to clean up all tasks
   */
  override clear(): void {
    this.agentTasks.clear();
    super.clear();
  }
}
