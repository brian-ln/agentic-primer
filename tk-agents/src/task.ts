// TaskNode - Task actor using ActorFactory pattern

import type { Graph } from "./graph";
import type { Address, ActorFactory, Message as ActorMessage, Response } from "./actors/index.ts";
import type {
  EvalResponse,
  GetResponse,
  ObjectiveCriterion,
  ObserveResponse,
  StatusResponse,
  TaskProperties,
  TaskState,
} from "./types";

let taskCounter = 0;

export interface CreateTaskOptions {
  goal: string;
  desiredDeliverables: string[];
  objectiveSuccessCriteria: ObjectiveCriterion[];
  subjectiveSuccessCriteria?: TaskProperties["subjectiveSuccessCriteria"];
  informationGaps?: string[];
  toolsAvailable?: string[];
  parentTaskId?: string;
}

interface TaskActorData extends CreateTaskOptions {
  graph: Graph;
}

/**
 * TaskActor - ActorFactory that creates task actors
 *
 * Returns an Address with task message handling capabilities
 */
export const TaskActor: ActorFactory<TaskActorData> = (data) => {
  // Initialize task properties
  const id = `task_${++taskCounter}`;
  const properties: TaskProperties = {
    id,
    type: "task",
    state: "created",
    createdAt: new Date(),
    goal: data.goal,
    desiredDeliverables: data.desiredDeliverables,
    objectiveSuccessCriteria: data.objectiveSuccessCriteria,
    subjectiveSuccessCriteria: data.subjectiveSuccessCriteria,
    knownInformation: [],
    informationGaps: data.informationGaps ?? [],
    toolsAvailable: data.toolsAvailable ?? [],
    parentTaskId: data.parentTaskId,
  };

  // Create actor with message handler
  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      try {
        const result = await handleMessage(message, properties, data.graph);
        return { success: true, data: result };
      } catch (error) {
        return {
          success: false,
          error: error instanceof Error ? error.message : String(error),
        };
      }
    },
  };

  // Register with graph's system and return Address
  const address = data.graph.getSystem().register(actor);

  // Register with graph (using string ID for serialization)
  data.graph.registerNode(id, address, properties);

  return address;
};

/**
 * Handle task messages
 */
async function handleMessage(
  message: ActorMessage,
  properties: TaskProperties,
  graph: Graph
): Promise<unknown> {
  const payload = message.payload as Record<string, unknown>;

  switch (message.type) {
    // Standard messages
    case "get":
      return handleGet(properties, graph);
    case "observe":
      return handleObserve(properties, graph);
    case "update":
      return handleUpdate(payload as { properties: Partial<TaskProperties> }, properties);
    case "link":
      return handleLink(
        payload as { toId: string; edgeType: string; properties?: Record<string, unknown> },
        properties,
        graph
      );
    case "unlink":
      return handleUnlink(payload as { edgeId: string }, graph);
    case "delete":
      return handleDelete(properties, graph);

    // Task-specific messages
    case "start":
      return handleStart(payload as { context?: Record<string, unknown> }, properties);
    case "create_task":
      return handleCreateTask(
        payload as {
          goal: string;
          deliverables: string[];
          criteria: ObjectiveCriterion[];
          context?: Record<string, unknown>;
        },
        properties,
        graph
      );
    case "eval":
      return handleEval(properties, graph);
    case "complete":
      return handleComplete(payload as { result: unknown; artifacts?: string[] }, properties, graph);
    case "block":
      return handleBlock(payload as { reason: string; requiredKnowledge?: string[] }, properties);
    case "query_status":
      return handleQueryStatus(properties, graph);

    default:
      throw new Error(`Unknown message type: ${message.type}`);
  }
}

// Message handlers

function handleGet(properties: TaskProperties, graph: Graph): GetResponse {
  return {
    id: properties.id,
    type: properties.type,
    properties: properties,
    edges: graph.getAllEdges(properties.id),
  };
}

function handleObserve(properties: TaskProperties, graph: Graph): ObserveResponse {
  const childIds = graph.getChildTasks(properties.id);
  const observations: string[] = [];

  observations.push(`Task "${properties.goal}" is ${properties.state}`);

  if (childIds.length > 0) {
    const childStates = childIds.map((childId) => {
      const childProps = graph.getNodeProperties(childId) as TaskProperties;
      return childProps?.state || "unknown";
    });
    observations.push(`Has ${childIds.length} child tasks: ${childStates.join(", ")}`);
  }

  if (properties.informationGaps.length > 0) {
    observations.push(`Information gaps: ${properties.informationGaps.join(", ")}`);
  }

  return {
    state: properties.state,
    observations,
    metadata: {
      progress: calculateProgress(properties, graph),
      childCount: childIds.length,
    },
  };
}

function handleUpdate(
  payload: { properties: Partial<TaskProperties> },
  properties: TaskProperties
): { success: boolean; updatedProperties: string[] } {
  const updatedProperties: string[] = [];

  for (const [key, value] of Object.entries(payload.properties)) {
    if (key !== "id" && key !== "type" && key !== "createdAt") {
      (properties as Record<string, unknown>)[key] = value;
      updatedProperties.push(key);
    }
  }

  return { success: true, updatedProperties };
}

function handleLink(
  payload: { toId: string; edgeType: string; properties?: Record<string, unknown> },
  properties: TaskProperties,
  graph: Graph
): { edgeId: string; success: boolean } {
  const edge = graph.addEdge(properties.id, payload.toId, payload.edgeType as any, payload.properties ?? {});
  return { edgeId: edge.id, success: true };
}

function handleUnlink(payload: { edgeId: string }, graph: Graph): { success: boolean } {
  const success = graph.removeEdge(payload.edgeId);
  return { success };
}

function handleDelete(properties: TaskProperties, graph: Graph): { success: boolean } {
  const success = graph.removeNode(properties.id);
  return { success };
}

function handleStart(
  payload: { context?: Record<string, unknown> },
  properties: TaskProperties
): { success: boolean; state: TaskState } {
  if (properties.state !== "created" && properties.state !== "ready") {
    return { success: false, state: properties.state };
  }

  properties.state = "active";
  properties.startedAt = new Date();

  if (payload.context) {
    (properties as Record<string, unknown>).context = payload.context;
  }

  return { success: true, state: properties.state };
}

function handleCreateTask(
  payload: {
    goal: string;
    deliverables: string[];
    criteria: ObjectiveCriterion[];
    context?: Record<string, unknown>;
  },
  properties: TaskProperties,
  graph: Graph
): { childTaskId: string; success: boolean } {
  // Create child task using TaskActor factory
  const childAddress = TaskActor({
    goal: payload.goal,
    desiredDeliverables: payload.deliverables,
    objectiveSuccessCriteria: payload.criteria,
    parentTaskId: properties.id,
    toolsAvailable: properties.toolsAvailable,
    graph,
  });

  // Get the child's ID from graph (it was registered in TaskActor)
  const childId = Array.from(graph.getNodeIds()).find((id) => {
    const addr = graph.getNode(id);
    return addr === childAddress;
  });

  if (!childId) {
    throw new Error("Failed to find child task ID");
  }

  // Link child to parent with spawned_by edge
  graph.addEdge(childId, properties.id, "spawned_by");

  return { childTaskId: childId, success: true };
}

function handleEval(properties: TaskProperties, graph: Graph): EvalResponse {
  const observations: string[] = [];
  let totalScore = 0;
  let passedCount = 0;

  const evaluatedCriteria = properties.objectiveSuccessCriteria.map((criterion) => {
    const actual = criterion.actual;
    let passed = false;

    if (typeof criterion.threshold === "boolean") {
      passed = actual === criterion.threshold;
    } else if (typeof criterion.threshold === "number" && typeof actual === "number") {
      passed = actual >= criterion.threshold;
    }

    if (passed) passedCount++;

    return {
      ...criterion,
      passed,
      value: actual,
    };
  });

  totalScore =
    properties.objectiveSuccessCriteria.length > 0
      ? passedCount / properties.objectiveSuccessCriteria.length
      : 1;

  const allPassed = passedCount === properties.objectiveSuccessCriteria.length;

  // Check children status
  const childIds = graph.getChildTasks(properties.id);
  const incompleteChildren = childIds.filter((childId) => {
    const childProps = graph.getNodeProperties(childId) as TaskProperties;
    return childProps?.state !== "completed";
  });

  if (incompleteChildren.length > 0) {
    observations.push(`${incompleteChildren.length} child tasks not yet completed`);
  }

  if (allPassed && incompleteChildren.length === 0) {
    observations.push("All criteria passed - ready to complete");
  } else {
    observations.push(`${passedCount}/${properties.objectiveSuccessCriteria.length} criteria passed`);
  }

  return {
    score: totalScore,
    passed: allPassed && incompleteChildren.length === 0,
    objectiveCriteria: evaluatedCriteria,
    subjectiveCriteria: properties.subjectiveSuccessCriteria,
    observations,
  };
}

function handleComplete(
  payload: { result: unknown; artifacts?: string[] },
  properties: TaskProperties,
  graph: Graph
): { success: boolean; finalState: TaskState } {
  // Run eval first to check if we can complete
  const evalResult = handleEval(properties, graph);

  if (!evalResult.passed) {
    return { success: false, finalState: properties.state };
  }

  properties.state = "completed";
  properties.completedAt = new Date();
  properties.result = payload.result;

  // Link to any artifacts
  if (payload.artifacts) {
    for (const artifactId of payload.artifacts) {
      graph.addEdge(properties.id, artifactId, "produces");
    }
  }

  return { success: true, finalState: "completed" };
}

function handleBlock(
  payload: { reason: string; requiredKnowledge?: string[] },
  properties: TaskProperties
): { success: boolean; state: TaskState } {
  properties.state = "blocked";
  (properties as Record<string, unknown>).blockReason = payload.reason;

  if (payload.requiredKnowledge) {
    properties.informationGaps.push(...payload.requiredKnowledge);
  }

  return { success: true, state: "blocked" };
}

function handleQueryStatus(properties: TaskProperties, graph: Graph): StatusResponse {
  const childIds = graph.getChildTasks(properties.id);

  const childrenStatus = childIds.map((childId) => {
    const childProps = graph.getNodeProperties(childId) as TaskProperties;
    return {
      id: childId,
      state: childProps?.state || ("unknown" as TaskState),
      progress: childProps ? calculateProgress(childProps, graph) : 0,
    };
  });

  const blockers: string[] = [];
  if (properties.state === "blocked") {
    blockers.push((properties as Record<string, unknown>).blockReason as string);
  }

  for (const edge of graph.getEdgesFrom(properties.id)) {
    if (edge.type === "depends_on") {
      const depProps = graph.getNodeProperties(edge.toId) as TaskProperties;
      if (depProps && depProps.state !== "completed") {
        blockers.push(`Waiting on: ${depProps.goal}`);
      }
    }
  }

  return {
    state: properties.state,
    progress: calculateProgress(properties, graph),
    blockers,
    childrenStatus,
  };
}

// Helper functions

function calculateProgress(properties: TaskProperties, graph: Graph): number {
  if (properties.state === "completed") return 1;
  if (properties.state === "created") return 0;

  const childIds = graph.getChildTasks(properties.id);
  if (childIds.length === 0) {
    // Leaf task - progress based on state
    return properties.state === "active" ? 0.5 : 0.1;
  }

  // Progress based on children
  const childProgress = childIds.reduce((sum, childId) => {
    const childProps = graph.getNodeProperties(childId) as TaskProperties;
    return sum + (childProps ? calculateProgress(childProps, graph) : 0);
  }, 0);

  return childProgress / childIds.length;
}

/**
 * Factory function for convenience
 * Creates a TaskActor and returns its Address
 */
export function createTask(options: CreateTaskOptions, graph: Graph): Address {
  return TaskActor({ ...options, graph });
}

/**
 * Get task properties from graph by ID
 */
export function getTaskProperties(taskId: string, graph: Graph): TaskProperties | undefined {
  return graph.getNodeProperties(taskId) as TaskProperties;
}
