/**
 * Agent Actor - Actor model interface for background agents
 *
 * Provides message-passing interface to agents instead of direct CLI tool calls.
 * Inspired by Erlang/Akka actor patterns.
 */

import { readFileSync, existsSync } from "fs";
import { Agent } from "../graph/session-graph";

export type ActorMessage =
  | { type: "status" }
  | { type: "tail"; lines?: number }
  | { type: "stop" }
  | { type: "query"; query: string }
  | { type: "ping" };

export type ActorResponse =
  | { type: "status"; status: string; data: Agent }
  | { type: "tail"; lines: string[] }
  | { type: "stop"; success: boolean }
  | { type: "query"; result: unknown }
  | { type: "pong" }
  | { type: "error"; error: string };

/**
 * AgentActor - Actor wrapper around a background agent
 *
 * Provides message-passing interface following Actor model principles:
 * - Encapsulation: Agent state is private
 * - Message passing: All interaction via send()
 * - Asynchronous: Messages are async
 * - Location transparency: Could be local or remote
 */
export class AgentActor {
  private agent: Agent;

  constructor(agent: Agent) {
    this.agent = agent;
  }

  /**
   * Send a message to this actor and receive a response
   */
  async send(message: ActorMessage): Promise<ActorResponse> {
    try {
      switch (message.type) {
        case "status":
          return this.handleStatus();
        case "tail":
          return this.handleTail(message.lines);
        case "stop":
          return this.handleStop();
        case "query":
          return this.handleQuery(message.query);
        case "ping":
          return { type: "pong" };
        default:
          return {
            type: "error",
            error: `Unknown message type: ${(message as any).type}`,
          };
      }
    } catch (error) {
      return {
        type: "error",
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Get actor ID
   */
  getId(): string {
    return this.agent.id;
  }

  /**
   * Get current agent state (read-only)
   */
  getState(): Readonly<Agent> {
    return Object.freeze({ ...this.agent });
  }

  /**
   * Update internal agent state (private to actor)
   */
  private updateState(updates: Partial<Agent>): void {
    this.agent = { ...this.agent, ...updates };
  }

  // ============================================================================
  // Message Handlers
  // ============================================================================

  private async handleStatus(): Promise<ActorResponse> {
    return {
      type: "status",
      status: this.agent.status,
      data: this.getState(),
    };
  }

  private async handleTail(lines: number = 50): Promise<ActorResponse> {
    if (!this.agent.output_path || !existsSync(this.agent.output_path)) {
      return {
        type: "tail",
        lines: [],
      };
    }

    try {
      const content = readFileSync(this.agent.output_path, "utf-8");
      const allLines = content.split("\n");
      const tailLines = allLines.slice(-lines);

      return {
        type: "tail",
        lines: tailLines,
      };
    } catch (error) {
      return {
        type: "error",
        error: `Failed to tail output: ${error instanceof Error ? error.message : String(error)}`,
      };
    }
  }

  private async handleStop(): Promise<ActorResponse> {
    // In a real implementation, this would signal the background process
    // For POC, we just update state
    this.updateState({ status: "stopped", ended_at: new Date().toISOString() });

    return {
      type: "stop",
      success: true,
    };
  }

  private async handleQuery(query: string): Promise<ActorResponse> {
    // In a real implementation, this would query the agent's state/database
    // For POC, we do simple pattern matching on query
    const lowerQuery = query.toLowerCase();

    if (lowerQuery.includes("progress")) {
      return {
        type: "query",
        result: {
          agent_id: this.agent.id,
          command: this.agent.command,
          status: this.agent.status,
          started_at: this.agent.started_at,
          running_time: this.calculateRunningTime(),
        },
      };
    }

    if (lowerQuery.includes("output")) {
      const tail = await this.handleTail(10);
      return {
        type: "query",
        result: {
          agent_id: this.agent.id,
          recent_output: tail.type === "tail" ? tail.lines : [],
        },
      };
    }

    return {
      type: "query",
      result: {
        message: "Query not understood. Try 'progress' or 'output'",
      },
    };
  }

  // ============================================================================
  // Utilities
  // ============================================================================

  private calculateRunningTime(): string {
    const startTime = new Date(this.agent.started_at).getTime();
    const endTime = this.agent.ended_at
      ? new Date(this.agent.ended_at).getTime()
      : Date.now();

    const durationMs = endTime - startTime;
    const seconds = Math.floor(durationMs / 1000);
    const minutes = Math.floor(seconds / 60);
    const hours = Math.floor(minutes / 60);

    if (hours > 0) {
      return `${hours}h ${minutes % 60}m`;
    } else if (minutes > 0) {
      return `${minutes}m ${seconds % 60}s`;
    } else {
      return `${seconds}s`;
    }
  }
}

/**
 * AgentActorSystem - Manages a collection of agent actors
 *
 * Registry pattern for looking up actors by ID or other criteria.
 */
export class AgentActorSystem {
  private actors: Map<string, AgentActor> = new Map();

  /**
   * Register an agent as an actor
   */
  register(agent: Agent): AgentActor {
    const actor = new AgentActor(agent);
    this.actors.set(agent.id, actor);
    return actor;
  }

  /**
   * Get an actor by ID
   */
  get(agentId: string): AgentActor | undefined {
    return this.actors.get(agentId);
  }

  /**
   * Get all actors
   */
  getAll(): AgentActor[] {
    return Array.from(this.actors.values());
  }

  /**
   * Send a message to an actor by ID
   */
  async send(agentId: string, message: ActorMessage): Promise<ActorResponse> {
    const actor = this.actors.get(agentId);
    if (!actor) {
      return {
        type: "error",
        error: `Actor not found: ${agentId}`,
      };
    }

    return actor.send(message);
  }

  /**
   * Broadcast a message to all actors
   */
  async broadcast(message: ActorMessage): Promise<Map<string, ActorResponse>> {
    const responses = new Map<string, ActorResponse>();

    for (const [id, actor] of this.actors.entries()) {
      const response = await actor.send(message);
      responses.set(id, response);
    }

    return responses;
  }

  /**
   * Remove an actor from the system
   */
  unregister(agentId: string): boolean {
    return this.actors.delete(agentId);
  }

  /**
   * Get count of registered actors
   */
  count(): number {
    return this.actors.size;
  }

  /**
   * Clear all actors
   */
  clear(): void {
    this.actors.clear();
  }
}
