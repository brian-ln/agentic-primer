// TaskNode - Task actor with complete lifecycle state machine

import type { Graph, NodeActor } from "./graph";
import type { KnowledgeNode } from "./knowledge";
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
import { TERMINAL_STATES, VALID_TRANSITIONS } from "./types";

let taskCounter = 0;

export interface CreateTaskOptions {
  goal: string;
  desiredDeliverables?: string[];
  objectiveSuccessCriteria?: ObjectiveCriterion[];
  subjectiveSuccessCriteria?: TaskProperties["subjectiveSuccessCriteria"];
  knowledge?: KnowledgeNode[];
  informationGaps?: string[];
  toolsAvailable?: string[];
  parentTaskId?: string;
  priority?: number;
  maxAttempts?: number;
  retryDelayMs?: number;
  deadline?: Date;
}

export class TaskNode implements NodeActor {
  properties: TaskProperties;

  constructor(options: CreateTaskOptions) {
    this.properties = {
      id: `task_${++taskCounter}`,
      type: "task",
      state: "created",
      createdAt: new Date(),

      // Definition
      goal: options.goal,
      desiredDeliverables: options.desiredDeliverables ?? [],
      objectiveSuccessCriteria: options.objectiveSuccessCriteria ?? [],
      subjectiveSuccessCriteria: options.subjectiveSuccessCriteria,

      // Knowledge & Tools
      knownInformation: options.knowledge?.map((k) => k.properties.id) ?? [],
      informationGaps: options.informationGaps ?? [],
      toolsAvailable: options.toolsAvailable ?? [],

      // Hierarchy
      parentTaskId: options.parentTaskId,

      // Scheduling
      deadline: options.deadline,
      priority: options.priority ?? 50,

      // Retry
      attemptCount: 0,
      maxAttempts: options.maxAttempts ?? 1,
      retryDelayMs: options.retryDelayMs ?? 1000,

      // Timing
      totalActiveMs: 0,
    };
  }

  // Validate state transition
  private canTransition(to: TaskState): boolean {
    const validTargets = VALID_TRANSITIONS[this.properties.state];
    return validTargets.includes(to);
  }

  // Transition state with validation
  private transition(to: TaskState): { success: boolean; state: TaskState; error?: string } {
    if (!this.canTransition(to)) {
      return {
        success: false,
        state: this.properties.state,
        error: `Invalid transition: ${this.properties.state} -> ${to}`,
      };
    }
    this.properties.state = to;
    return { success: true, state: to };
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
        return this.handleLink(
          message.payload as { toId: string; edgeType: Edge["type"]; properties?: Record<string, unknown> },
          graph
        );
      case "unlink":
        return this.handleUnlink(message.payload as { edgeId: string }, graph);
      case "delete":
        return this.handleDelete(graph);

      // Lifecycle messages
      case "plan":
        return this.handlePlan(message.payload as { agent?: string });
      case "define":
        return this.handleDefine(message.payload as { criteria?: ObjectiveCriterion[]; deliverables?: string[] });
      case "schedule":
        return this.handleSchedule(message.payload as { time?: Date; condition?: string });
      case "unschedule":
        return this.handleUnschedule();
      case "assign":
        return this.handleAssign(message.payload as { actorId: string });
      case "release":
        return this.handleRelease();
      case "start":
        return this.handleStart(message.payload as { context?: Record<string, unknown> });
      case "pause":
        return this.handlePause(message.payload as { reason?: string; checkpoint?: unknown });
      case "resume":
        return this.handleResume(message.payload as { context?: Record<string, unknown> });
      case "block":
        return this.handleBlock(message.payload as { reason: string; dependencies?: string[] });
      case "unblock":
        return this.handleUnblock(message.payload as { resolution?: string });
      case "complete":
        return this.handleComplete(message.payload as { result: unknown; artifacts?: string[] }, graph);
      case "fail":
        return this.handleFail(message.payload as { error: string; details?: unknown });
      case "cancel":
        return this.handleCancel(message.payload as { reason?: string; cancelChildren?: boolean }, graph);
      case "skip":
        return this.handleSkip(message.payload as { reason: string });
      case "retry":
        return this.handleRetry();
      case "timeout":
        return this.handleTimeout();

      // Work messages
      case "spawn":
        return this.handleSpawn(
          message.payload as {
            goal: string;
            deliverables: string[];
            criteria: ObjectiveCriterion[];
            context?: Record<string, unknown>;
          },
          graph
        );
      case "eval":
        return this.handleEval(graph);
      case "checkpoint":
        return this.handleCheckpoint(message.payload as { data: unknown });
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

    if (this.properties.assignedTo) {
      observations.push(`Assigned to: ${this.properties.assignedTo}`);
    }

    if (children.length > 0) {
      const childStates = children.map((c) => (c.properties as TaskProperties).state);
      observations.push(`Has ${children.length} child tasks: ${childStates.join(", ")}`);
    }

    if (this.properties.informationGaps.length > 0) {
      observations.push(`Information gaps: ${this.properties.informationGaps.join(", ")}`);
    }

    if (this.properties.blockReason) {
      observations.push(`Blocked: ${this.properties.blockReason}`);
    }

    if (this.properties.attemptCount > 0) {
      observations.push(`Attempt ${this.properties.attemptCount}/${this.properties.maxAttempts}`);
    }

    if (this.properties.checkpoint) {
      observations.push(`Has checkpoint data`);
    }

    return {
      state: this.properties.state,
      observations,
      metadata: {
        progress: this.calculateProgress(graph),
        childCount: children.length,
        attemptCount: this.properties.attemptCount,
        totalActiveMs: this.properties.totalActiveMs,
      },
    };
  }

  private handleUpdate(payload: { properties: Partial<TaskProperties> }): {
    success: boolean;
    updatedProperties: string[];
  } {
    const updatedProperties: string[] = [];
    const immutableKeys = ["id", "type", "createdAt", "state"]; // state changes via transitions only

    for (const [key, value] of Object.entries(payload.properties)) {
      if (!immutableKeys.includes(key)) {
        (this.properties as Record<string, unknown>)[key] = value;
        updatedProperties.push(key);
      }
    }

    return { success: true, updatedProperties };
  }

  private handleLink(
    payload: { toId: string; edgeType: Edge["type"]; properties?: Record<string, unknown> },
    graph: Graph
  ): { edgeId: string; success: boolean } {
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

  // Lifecycle handlers

  private handlePlan(payload: { agent?: string }): { success: boolean; state: TaskState; error?: string } {
    const result = this.transition("planning");
    if (result.success && payload.agent) {
      this.properties.assignedTo = payload.agent;
    }
    return result;
  }

  private handleDefine(payload: { criteria?: ObjectiveCriterion[]; deliverables?: string[] }): {
    success: boolean;
    state: TaskState;
    error?: string;
  } {
    const result = this.transition("ready");
    if (result.success) {
      if (payload.criteria) {
        this.properties.objectiveSuccessCriteria = payload.criteria;
      }
      if (payload.deliverables) {
        this.properties.desiredDeliverables = payload.deliverables;
      }
    }
    return result;
  }

  private handleSchedule(payload: { time?: Date; condition?: string }): {
    success: boolean;
    state: TaskState;
    error?: string;
  } {
    const result = this.transition("scheduled");
    if (result.success) {
      this.properties.scheduledFor = payload.time;
      if (payload.condition) {
        (this.properties as Record<string, unknown>).scheduleCondition = payload.condition;
      }
    }
    return result;
  }

  private handleUnschedule(): { success: boolean; state: TaskState; error?: string } {
    const result = this.transition("ready");
    if (result.success) {
      this.properties.scheduledFor = undefined;
      delete (this.properties as Record<string, unknown>).scheduleCondition;
    }
    return result;
  }

  private handleAssign(payload: { actorId: string }): { success: boolean; state: TaskState; error?: string } {
    const result = this.transition("assigned");
    if (result.success) {
      this.properties.assignedTo = payload.actorId;
      this.properties.assignedAt = new Date();
    }
    return result;
  }

  private handleRelease(): { success: boolean; state: TaskState; error?: string } {
    const result = this.transition("ready");
    if (result.success) {
      this.properties.assignedTo = undefined;
      this.properties.assignedAt = undefined;
    }
    return result;
  }

  private handleStart(payload: { context?: Record<string, unknown> }): {
    success: boolean;
    state: TaskState;
    error?: string;
  } {
    const result = this.transition("active");
    if (result.success) {
      this.properties.startedAt = new Date();
      if (payload.context) {
        (this.properties as Record<string, unknown>).context = payload.context;
      }
    }
    return result;
  }

  private handlePause(payload: { reason?: string; checkpoint?: unknown }): {
    success: boolean;
    state: TaskState;
    error?: string;
  } {
    // Track active time before pausing
    if (this.properties.startedAt) {
      const now = new Date();
      const activeTime = this.properties.pausedAt
        ? 0
        : now.getTime() - (this.properties.startedAt?.getTime() ?? now.getTime());
      this.properties.totalActiveMs += activeTime;
    }

    const result = this.transition("paused");
    if (result.success) {
      this.properties.pausedAt = new Date();
      if (payload.checkpoint !== undefined) {
        this.properties.checkpoint = payload.checkpoint;
      }
      if (payload.reason) {
        (this.properties as Record<string, unknown>).pauseReason = payload.reason;
      }
    }
    return result;
  }

  private handleResume(payload: { context?: Record<string, unknown> }): {
    success: boolean;
    state: TaskState;
    checkpoint?: unknown;
    error?: string;
  } {
    const checkpoint = this.properties.checkpoint;
    const result = this.transition("active");
    if (result.success) {
      this.properties.pausedAt = undefined;
      delete (this.properties as Record<string, unknown>).pauseReason;
      if (payload.context) {
        (this.properties as Record<string, unknown>).context = payload.context;
      }
    }
    return { ...result, checkpoint };
  }

  private handleBlock(payload: { reason: string; dependencies?: string[] }): {
    success: boolean;
    state: TaskState;
    error?: string;
  } {
    const result = this.transition("blocked");
    if (result.success) {
      this.properties.blockReason = payload.reason;
      this.properties.blockDependencies = payload.dependencies;
    }
    return result;
  }

  private handleUnblock(payload: { resolution?: string }): { success: boolean; state: TaskState; error?: string } {
    const result = this.transition("active");
    if (result.success) {
      this.properties.blockReason = undefined;
      this.properties.blockDependencies = undefined;
      if (payload.resolution) {
        (this.properties as Record<string, unknown>).lastBlockResolution = payload.resolution;
      }
    }
    return result;
  }

  private handleComplete(
    payload: { result: unknown; artifacts?: string[] },
    graph: Graph
  ): { success: boolean; state: TaskState; error?: string } {
    // Run eval first to check if we can complete
    const evalResult = this.handleEval(graph);
    if (!evalResult.passed) {
      return {
        success: false,
        state: this.properties.state,
        error: `Cannot complete: ${evalResult.observations.join(", ")}`,
      };
    }

    const result = this.transition("completed");
    if (result.success) {
      this.properties.completedAt = new Date();
      this.properties.result = payload.result;

      // Calculate total active time
      if (this.properties.startedAt && !this.properties.pausedAt) {
        this.properties.totalActiveMs += new Date().getTime() - this.properties.startedAt.getTime();
      }

      // Link to any artifacts
      if (payload.artifacts) {
        for (const artifactId of payload.artifacts) {
          graph.addEdge(this.properties.id, artifactId, "produces");
        }
      }
    }
    return result;
  }

  private handleFail(payload: { error: string; details?: unknown }): {
    success: boolean;
    state: TaskState;
    error?: string;
  } {
    const result = this.transition("failed");
    if (result.success) {
      this.properties.error = payload.error;
      if (payload.details) {
        (this.properties as Record<string, unknown>).errorDetails = payload.details;
      }
    }
    return result;
  }

  private handleCancel(
    payload: { reason?: string; cancelChildren?: boolean },
    graph: Graph
  ): { success: boolean; state: TaskState; cancelledChildren?: string[]; error?: string } {
    if (TERMINAL_STATES.includes(this.properties.state)) {
      return {
        success: false,
        state: this.properties.state,
        error: `Cannot cancel: task is already in terminal state ${this.properties.state}`,
      };
    }

    this.properties.state = "cancelled";
    this.properties.cancelReason = payload.reason;
    this.properties.cancelledBy = "system"; // Could be enhanced to track who cancelled

    const cancelledChildren: string[] = [];

    // Propagate cancellation to children
    if (payload.cancelChildren !== false) {
      const children = graph.getChildTasks(this.properties.id);
      for (const child of children) {
        const childProps = child.properties as TaskProperties;
        if (!TERMINAL_STATES.includes(childProps.state)) {
          graph.send(childProps.id, "cancel", { reason: `Parent cancelled: ${payload.reason}` });
          cancelledChildren.push(childProps.id);
        }
      }
    }

    return { success: true, state: "cancelled", cancelledChildren };
  }

  private handleSkip(payload: { reason: string }): { success: boolean; state: TaskState; error?: string } {
    const result = this.transition("skipped");
    if (result.success) {
      (this.properties as Record<string, unknown>).skipReason = payload.reason;
    }
    return result;
  }

  private handleRetry(): { success: boolean; state: TaskState; attempt?: number; error?: string } {
    if (this.properties.state !== "failed") {
      return { success: false, state: this.properties.state, error: "Can only retry from failed state" };
    }

    if (this.properties.attemptCount >= this.properties.maxAttempts - 1) {
      return {
        success: false,
        state: this.properties.state,
        error: `Max attempts (${this.properties.maxAttempts}) reached`,
      };
    }

    const result = this.transition("retrying");
    if (result.success) {
      this.properties.attemptCount++;
      this.properties.error = undefined;
      delete (this.properties as Record<string, unknown>).errorDetails;

      // Immediately transition to active (scheduler could add delay in future)
      this.properties.state = "active";
    }
    return { success: result.success, state: this.properties.state, attempt: this.properties.attemptCount };
  }

  private handleTimeout(): { success: boolean; state: TaskState; error?: string } {
    if (!["active", "blocked"].includes(this.properties.state)) {
      return { success: false, state: this.properties.state, error: "Can only timeout from active or blocked state" };
    }

    this.properties.state = "timed_out";
    this.properties.error = "Deadline exceeded";
    return { success: true, state: "timed_out" };
  }

  // Work message handlers

  private handleSpawn(
    payload: { goal: string; deliverables: string[]; criteria: ObjectiveCriterion[]; context?: Record<string, unknown> },
    graph: Graph
  ): { childTaskId: string; success: boolean } {
    const childTask = new TaskNode({
      goal: payload.goal,
      desiredDeliverables: payload.deliverables,
      objectiveSuccessCriteria: payload.criteria,
      parentTaskId: this.properties.id,
      toolsAvailable: this.properties.toolsAvailable,
      maxAttempts: this.properties.maxAttempts,
    });

    graph.registerNode(childTask);
    graph.addEdge(childTask.properties.id, this.properties.id, "spawned_by");

    return { childTaskId: childTask.properties.id, success: true };
  }

  private handleEval(graph: Graph): EvalResponse {
    const observations: string[] = [];
    let passedCount = 0;

    const evaluatedCriteria = this.properties.objectiveSuccessCriteria.map((criterion) => {
      const actual = criterion.actual;
      let passed = false;

      if (typeof criterion.threshold === "boolean") {
        passed = actual === criterion.threshold;
      } else if (typeof criterion.threshold === "number" && typeof actual === "number") {
        passed = actual >= criterion.threshold;
      }

      if (passed) passedCount++;

      return { ...criterion, passed, value: actual };
    });

    const totalScore =
      this.properties.objectiveSuccessCriteria.length > 0
        ? passedCount / this.properties.objectiveSuccessCriteria.length
        : 1;

    const allPassed = passedCount === this.properties.objectiveSuccessCriteria.length;

    // Check children status
    const children = graph.getChildTasks(this.properties.id);
    const incompleteChildren = children.filter((c) => (c.properties as TaskProperties).state !== "completed");

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

  private handleCheckpoint(payload: { data: unknown }): { success: boolean; version: number } {
    this.properties.checkpoint = payload.data;
    const version = ((this.properties as Record<string, unknown>).checkpointVersion as number) || 0;
    (this.properties as Record<string, unknown>).checkpointVersion = version + 1;
    return { success: true, version: version + 1 };
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
    if (this.properties.blockReason) {
      blockers.push(this.properties.blockReason);
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
      attemptCount: this.properties.attemptCount,
      maxAttempts: this.properties.maxAttempts,
      checkpoint: this.properties.checkpoint,
    };
  }

  // Helpers

  private calculateProgress(graph: Graph): number {
    return this.calculateProgressForTask(this.properties, graph);
  }

  private calculateProgressForTask(props: TaskProperties, graph: Graph): number {
    if (props.state === "completed") return 1;
    if (props.state === "created" || props.state === "planning") return 0;
    if (TERMINAL_STATES.includes(props.state) && props.state !== "completed") return 0;

    const children = graph.getChildTasks(props.id);
    if (children.length === 0) {
      // Leaf task - progress based on state
      const stateProgress: Record<TaskState, number> = {
        created: 0,
        planning: 0.1,
        ready: 0.2,
        scheduled: 0.2,
        assigned: 0.3,
        active: 0.5,
        paused: 0.5,
        blocked: 0.4,
        retrying: 0.5,
        completed: 1,
        failed: 0,
        cancelled: 0,
        skipped: 0,
        timed_out: 0,
      };
      return stateProgress[props.state] ?? 0;
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

  // Auto-link knowledge nodes
  if (options.knowledge) {
    for (const knowledgeNode of options.knowledge) {
      graph.send(task.properties.id, "link", {
        toId: knowledgeNode.properties.id,
        edgeType: "requires_knowledge",
      });
    }
  }

  return task;
}
