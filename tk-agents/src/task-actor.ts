/**
 * Task Actor - Simple implementation for dual-write coordinator
 *
 * This is a minimal task actor that handles basic state transitions.
 * Full implementation would include evaluation, child task spawning, etc.
 */

import type { Address, Message, Response } from "./actors/base";
import type { Graph } from "./graph";
import type { TaskProperties, TaskState } from "./types";

export interface TaskActorData extends TaskProperties {
  graph: Graph;
}

/**
 * Task Actor Factory
 *
 * Creates an actor that manages task state and responds to messages.
 */
export function TaskActor(data: TaskActorData): Address {
  let state = { ...data };

  const send = async (message: Message): Promise<Response> => {
    try {
      switch (message.type) {
        case "start":
          return handleStart();
        case "complete":
          return handleComplete(message.payload);
        case "block":
          return handleBlock(message.payload);
        case "get":
          return handleGet();
        case "observe":
          return handleObserve();
        case "query_status":
          return handleQueryStatus();
        default:
          return {
            success: false,
            error: `Unknown message type: ${message.type}`,
          };
      }
    } catch (error) {
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  };

  const handleStart = (): Response => {
    if (state.state !== "created" && state.state !== "ready") {
      return {
        success: false,
        error: `Cannot start task in state: ${state.state}`,
      };
    }

    state.state = "active";
    state.startedAt = new Date();

    return {
      success: true,
      data: { state: state.state, startedAt: state.startedAt },
    };
  };

  const handleComplete = (payload: unknown): Response => {
    if (state.state !== "active") {
      return {
        success: false,
        error: `Cannot complete task in state: ${state.state}`,
      };
    }

    state.state = "completed";
    state.completedAt = new Date();
    state.result = (payload as { result?: unknown })?.result;

    return {
      success: true,
      data: { state: state.state, completedAt: state.completedAt, result: state.result },
    };
  };

  const handleBlock = (payload: unknown): Response => {
    const { reason } = payload as { reason?: string };

    state.state = "blocked";

    return {
      success: true,
      data: { state: state.state, reason },
    };
  };

  const handleGet = (): Response => {
    return {
      success: true,
      data: { ...state },
    };
  };

  const handleObserve = (): Response => {
    return {
      success: true,
      data: {
        state: state.state,
        observations: [
          `Task: ${state.goal}`,
          `State: ${state.state}`,
          `Created: ${state.createdAt.toISOString()}`,
        ],
        metadata: {
          id: state.id,
          type: state.type,
        },
      },
    };
  };

  const handleQueryStatus = (): Response => {
    return {
      success: true,
      data: {
        state: state.state,
        progress: calculateProgress(),
        blockers: [],
        childrenStatus: [],
      },
    };
  };

  const calculateProgress = (): number => {
    switch (state.state) {
      case "created":
        return 0;
      case "ready":
        return 0.1;
      case "active":
        return 0.5;
      case "blocked":
        return 0.3;
      case "completed":
        return 1.0;
      case "failed":
        return 0;
      default:
        return 0;
    }
  };

  const address: Address = {
    __id: Symbol("task-actor"),
    send,
  };

  return address;
}
