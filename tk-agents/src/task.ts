// TaskNode - Task actor with lifecycle state machine

import type { Graph, NodeActor } from "./graph";
import type {
  Edge,
  EvalResponse,
  GetResponse,
  Message,
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
  knownInformation?: string[];
  informationGaps?: string[];
  toolsAvailable?: string[];
  parentTaskId?: string;
}

export class TaskNode implements NodeActor {
  properties: TaskProperties;

  constructor(options: CreateTaskOptions) {
    this.properties = {
      id: `task_${++taskCounter}`,
      type: "task",
      state: "created",
      createdAt: new Date(),
      goal: options.goal,
      desiredDeliverables: options.desiredDeliverables,
      objectiveSuccessCriteria: options.objectiveSuccessCriteria,
      subjectiveSuccessCriteria: options.subjectiveSuccessCriteria,
      knownInformation: options.knownInformation ?? [],
      informationGaps: options.informationGaps ?? [],
      toolsAvailable: options.toolsAvailable ?? [],
      parentTaskId: options.parentTaskId,
    };
  }

  handleMessage(message: Message, graph: Graph): unknown {
    switch (message.type) {
      // Standard messages
      case "get":
        return this.handleGet(graph);
      case "observe":
        return this.handleObserve(graph);
      case "update":
        return this.handleUpdate(message.payload as { properties: Partial<TaskProperties> });
      case "link":
        return this.handleLink(message.payload as { toId: string; edgeType: Edge["type"]; properties?: Record<string, unknown> }, graph);
      case "unlink":
        return this.handleUnlink(message.payload as { edgeId: string }, graph);
      case "delete":
        return this.handleDelete(graph);

      // Task-specific messages
      case "start":
        return this.handleStart(message.payload as { context?: Record<string, unknown> });
      case "spawn":
        return this.handleSpawn(message.payload as { goal: string; deliverables: string[]; criteria: ObjectiveCriterion[]; context?: Record<string, unknown> }, graph);
      case "eval":
        return this.handleEval(graph);
      case "complete":
        return this.handleComplete(message.payload as { result: unknown; artifacts?: string[] }, graph);
      case "block":
        return this.handleBlock(message.payload as { reason: string; requiredKnowledge?: string[] });
      case "query_status":
        return this.handleQueryStatus(graph);

      default:
        throw new Error(`Unknown message type: ${message.type}`);
    }
  }

  // Standard message handlers

  private handleGet(graph: Graph): GetResponse {
    return {
      id: this.properties.id,
      type: this.properties.type,
      properties: this.properties,
      edges: graph.getAllEdges(this.properties.id),
    };
  }

  private handleObserve(graph: Graph): ObserveResponse {
    const children = graph.getChildTasks(this.properties.id);
    const observations: string[] = [];

    observations.push(`Task "${this.properties.goal}" is ${this.properties.state}`);

    if (children.length > 0) {
      const childStates = children.map((c) => (c.properties as TaskProperties).state);
      observations.push(`Has ${children.length} child tasks: ${childStates.join(", ")}`);
    }

    if (this.properties.informationGaps.length > 0) {
      observations.push(`Information gaps: ${this.properties.informationGaps.join(", ")}`);
    }

    return {
      state: this.properties.state,
      observations,
      metadata: {
        progress: this.calculateProgress(graph),
        childCount: children.length,
      },
    };
  }

  private handleUpdate(payload: { properties: Partial<TaskProperties> }): { success: boolean; updatedProperties: string[] } {
    const updatedProperties: string[] = [];

    for (const [key, value] of Object.entries(payload.properties)) {
      if (key !== "id" && key !== "type" && key !== "createdAt") {
        (this.properties as Record<string, unknown>)[key] = value;
        updatedProperties.push(key);
      }
    }

    return { success: true, updatedProperties };
  }

  private handleLink(payload: { toId: string; edgeType: Edge["type"]; properties?: Record<string, unknown> }, graph: Graph): { edgeId: string; success: boolean } {
    const edge = graph.addEdge(this.properties.id, payload.toId, payload.edgeType, payload.properties ?? {});
    return { edgeId: edge.id, success: true };
  }

  private handleUnlink(payload: { edgeId: string }, graph: Graph): { success: boolean } {
    const success = graph.removeEdge(payload.edgeId);
    return { success };
  }

  private handleDelete(graph: Graph): { success: boolean } {
    const success = graph.removeNode(this.properties.id);
    return { success };
  }

  // Task-specific handlers

  private handleStart(payload: { context?: Record<string, unknown> }): { success: boolean; state: TaskState } {
    if (this.properties.state !== "created" && this.properties.state !== "ready") {
      return { success: false, state: this.properties.state };
    }

    this.properties.state = "active";
    this.properties.startedAt = new Date();

    if (payload.context) {
      (this.properties as Record<string, unknown>).context = payload.context;
    }

    return { success: true, state: this.properties.state };
  }

  private handleSpawn(
    payload: { goal: string; deliverables: string[]; criteria: ObjectiveCriterion[]; context?: Record<string, unknown> },
    graph: Graph
  ): { childTaskId: string; success: boolean } {
    const childTask = new TaskNode({
      goal: payload.goal,
      desiredDeliverables: payload.deliverables,
      objectiveSuccessCriteria: payload.criteria,
      parentTaskId: this.properties.id,
      toolsAvailable: this.properties.toolsAvailable, // Inherit tools
    });

    graph.registerNode(childTask);

    // Link child to parent with spawned_by edge
    graph.addEdge(childTask.properties.id, this.properties.id, "spawned_by");

    return { childTaskId: childTask.properties.id, success: true };
  }

  private handleEval(graph: Graph): EvalResponse {
    const observations: string[] = [];
    let totalScore = 0;
    let passedCount = 0;

    const evaluatedCriteria = this.properties.objectiveSuccessCriteria.map((criterion) => {
      // For MVP, criteria should have 'actual' value set before eval
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

    totalScore = this.properties.objectiveSuccessCriteria.length > 0
      ? passedCount / this.properties.objectiveSuccessCriteria.length
      : 1;

    const allPassed = passedCount === this.properties.objectiveSuccessCriteria.length;

    // Check children status
    const children = graph.getChildTasks(this.properties.id);
    const incompleteChildren = children.filter(
      (c) => (c.properties as TaskProperties).state !== "completed"
    );

    if (incompleteChildren.length > 0) {
      observations.push(`${incompleteChildren.length} child tasks not yet completed`);
    }

    if (allPassed && incompleteChildren.length === 0) {
      observations.push("All criteria passed - ready to complete");
    } else {
      observations.push(`${passedCount}/${this.properties.objectiveSuccessCriteria.length} criteria passed`);
    }

    return {
      score: totalScore,
      passed: allPassed && incompleteChildren.length === 0,
      objectiveCriteria: evaluatedCriteria,
      subjectiveCriteria: this.properties.subjectiveSuccessCriteria,
      observations,
    };
  }

  private handleComplete(
    payload: { result: unknown; artifacts?: string[] },
    graph: Graph
  ): { success: boolean; finalState: TaskState } {
    // Run eval first to check if we can complete
    const evalResult = this.handleEval(graph);

    if (!evalResult.passed) {
      return { success: false, finalState: this.properties.state };
    }

    this.properties.state = "completed";
    this.properties.completedAt = new Date();
    this.properties.result = payload.result;

    // Link to any artifacts
    if (payload.artifacts) {
      for (const artifactId of payload.artifacts) {
        graph.addEdge(this.properties.id, artifactId, "produces");
      }
    }

    return { success: true, finalState: "completed" };
  }

  private handleBlock(payload: { reason: string; requiredKnowledge?: string[] }): { success: boolean; state: TaskState } {
    this.properties.state = "blocked";
    (this.properties as Record<string, unknown>).blockReason = payload.reason;

    if (payload.requiredKnowledge) {
      this.properties.informationGaps.push(...payload.requiredKnowledge);
    }

    return { success: true, state: "blocked" };
  }

  private handleQueryStatus(graph: Graph): StatusResponse {
    const children = graph.getChildTasks(this.properties.id);

    const childrenStatus = children.map((child) => {
      const props = child.properties as TaskProperties;
      return {
        id: props.id,
        state: props.state,
        progress: this.calculateProgressForTask(props, graph),
      };
    });

    const blockers: string[] = [];
    if (this.properties.state === "blocked") {
      blockers.push((this.properties as Record<string, unknown>).blockReason as string);
    }

    for (const edge of graph.getEdgesFrom(this.properties.id)) {
      if (edge.type === "depends_on") {
        const dep = graph.getNode(edge.toId);
        if (dep && (dep.properties as TaskProperties).state !== "completed") {
          blockers.push(`Waiting on: ${(dep.properties as TaskProperties).goal}`);
        }
      }
    }

    return {
      state: this.properties.state,
      progress: this.calculateProgress(graph),
      blockers,
      childrenStatus,
    };
  }

  // Helpers

  private calculateProgress(graph: Graph): number {
    return this.calculateProgressForTask(this.properties, graph);
  }

  private calculateProgressForTask(props: TaskProperties, graph: Graph): number {
    if (props.state === "completed") return 1;
    if (props.state === "created") return 0;

    const children = graph.getChildTasks(props.id);
    if (children.length === 0) {
      // Leaf task - progress based on state
      return props.state === "active" ? 0.5 : 0.1;
    }

    // Progress based on children
    const childProgress = children.reduce((sum, child) => {
      const childProps = child.properties as TaskProperties;
      return sum + this.calculateProgressForTask(childProps, graph);
    }, 0);

    return childProgress / children.length;
  }
}

// Factory function for convenience
export function createTask(options: CreateTaskOptions, graph: Graph): TaskNode {
  const task = new TaskNode(options);
  graph.registerNode(task);
  return task;
}
