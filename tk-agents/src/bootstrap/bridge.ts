// Agent-Task Bridge - Connects background agents to task graph

import type { Actor } from "../actors/base";
import type { Graph } from "../graph";
import type { TaskNode } from "../task";
import type { TaskState } from "../types";

/**
 * AgentTask - Bridges a background agent (execution) with a TaskNode (state)
 */
export interface AgentTask {
  taskId: string;      // TaskNode ID in graph
  agentId: string;     // Actor ID in registry
  processId?: number;  // OS process ID if background (future use)
}

/**
 * State mapping between Agent activity and Task states
 */
export function mapActorStateToTaskState(actor: Actor): TaskState {
  // For now, simple mapping
  // Future: actors could expose their own state
  return "active"; // Default: if actor exists, it's active
}

/**
 * Sync agent state to task state
 */
export function syncAgentToTask(
  agentTask: AgentTask,
  actor: Actor,
  graph: Graph
): void {
  const taskNode = graph.getNode(agentTask.taskId) as TaskNode | undefined;
  if (!taskNode) {
    throw new Error(`Task not found: ${agentTask.taskId}`);
  }

  // Infer state from actor
  const taskState = mapActorStateToTaskState(actor);

  // Update task state if needed
  if (taskNode.properties.state !== taskState) {
    graph.send(agentTask.taskId, "update", {
      properties: { state: taskState }
    });
  }
}

/**
 * Handle agent death - mark task as failed
 */
export function handleAgentDeath(
  agentTask: AgentTask,
  reason: string,
  graph: Graph
): void {
  const taskNode = graph.getNode(agentTask.taskId);
  if (!taskNode) return;

  // Mark task as failed
  graph.send(agentTask.taskId, "update", {
    properties: {
      state: "failed",
      failureReason: reason,
      failedAt: new Date()
    }
  });
}

/**
 * Handle agent completion - mark task as completed
 */
export function handleAgentCompletion(
  agentTask: AgentTask,
  result: unknown,
  graph: Graph
): void {
  const taskNode = graph.getNode(agentTask.taskId);
  if (!taskNode) return;

  // Attempt to complete task (will eval criteria first)
  graph.send(agentTask.taskId, "complete", {
    result,
    artifacts: [] // Agent can provide artifacts list
  });
}

/**
 * Handle agent blocked - mark task as blocked
 */
export function handleAgentBlocked(
  agentTask: AgentTask,
  reason: string,
  requiredKnowledge?: string[],
  graph?: Graph
): void {
  if (!graph) return;

  const taskNode = graph.getNode(agentTask.taskId);
  if (!taskNode) return;

  // Mark task as blocked
  graph.send(agentTask.taskId, "block", {
    reason,
    requiredKnowledge
  });
}
