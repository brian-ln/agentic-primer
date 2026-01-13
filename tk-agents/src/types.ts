// Core Types for Task/Knowledge Graph Actor Protocol

export type NodeType = "task" | "knowledge" | "artifact" | "pattern";

// Complete Task State Machine
export type TaskState =
  // Lifecycle states
  | "created"     // Exists, minimal definition
  | "planning"    // Being decomposed/researched by agent
  | "ready"       // Fully defined, in backlog, waiting for capacity
  | "scheduled"   // Will run at specific time or when condition met
  | "assigned"    // Claimed by an actor, not yet started
  | "active"      // Being worked on
  | "paused"      // Intentionally stopped, can resume with checkpoint
  | "blocked"     // Waiting on dependency/info/human

  // Retry loop
  | "retrying"    // Failed, attempting again (with backoff)

  // Terminal states
  | "completed"   // Finished successfully
  | "failed"      // Errored (after max retries)
  | "cancelled"   // Explicitly aborted by user/system
  | "skipped"     // Intentionally not executed (conditional)
  | "timed_out";  // Deadline exceeded

// Terminal states helper
export const TERMINAL_STATES: TaskState[] = ["completed", "failed", "cancelled", "skipped", "timed_out"];

export type EdgeType =
  | "depends_on"
  | "requires_knowledge"
  | "produces"
  | "spawned_by"
  | "blocks"
  | "references"
  | "assigned_to";

// Success Criteria
export interface ObjectiveCriterion {
  criterion: string;
  measure: string;
  threshold: number | boolean;
  actual?: number | boolean;
  passed?: boolean;
}

export interface SubjectiveCriterion {
  criterion: string;
  evaluationGuidance: string;
  assessment?: string;
  notes?: string;
}

// Base Node
export interface NodeProperties {
  id: string;
  type: NodeType;
  createdAt: Date;
  [key: string]: unknown;
}

export interface Edge {
  id: string;
  fromId: string;
  toId: string;
  type: EdgeType;
  properties: Record<string, unknown>;
}

// Task Node Properties (Extended)
export interface TaskProperties extends NodeProperties {
  type: "task";
  state: TaskState;

  // Definition
  goal: string;
  desiredDeliverables: string[];
  objectiveSuccessCriteria: ObjectiveCriterion[];
  subjectiveSuccessCriteria?: SubjectiveCriterion[];

  // Knowledge & Tools
  knownInformation: string[];  // node IDs
  informationGaps: string[];
  toolsAvailable: string[];

  // Hierarchy
  parentTaskId?: string;

  // Scheduling
  scheduledFor?: Date;
  deadline?: Date;
  priority: number;            // 0-100, higher = more urgent (default: 50)

  // Assignment
  assignedTo?: string;         // actor ID
  assignedAt?: Date;

  // Retry
  attemptCount: number;        // starts at 0
  maxAttempts: number;         // default 1 (no retry)
  retryDelayMs: number;        // backoff delay (default: 1000)

  // Timing
  startedAt?: Date;
  pausedAt?: Date;
  completedAt?: Date;
  totalActiveMs: number;       // accumulated active time

  // Checkpointing
  checkpoint?: unknown;        // saved progress for resume

  // Cancellation
  cancelledBy?: string;
  cancelReason?: string;

  // Blocking
  blockReason?: string;
  blockDependencies?: string[];

  // Result
  result?: unknown;
  error?: string;
}

// Knowledge Node Properties
export interface KnowledgeProperties extends NodeProperties {
  type: "knowledge";
  title: string;
  content: string;
  sources: string[];
  version: number;
}

// Message Types
export type StandardMessage =
  | { type: "get"; payload: Record<string, never> }
  | { type: "observe"; payload: Record<string, never> }
  | { type: "update"; payload: { properties: Partial<NodeProperties> } }
  | { type: "link"; payload: { toId: string; edgeType: EdgeType; properties?: Record<string, unknown> } }
  | { type: "unlink"; payload: { edgeId: string } }
  | { type: "delete"; payload: Record<string, never> };

export type TaskMessage =
  // Lifecycle messages
  | { type: "plan"; payload: { agent?: string } }
  | { type: "define"; payload: { criteria?: ObjectiveCriterion[]; deliverables?: string[] } }
  | { type: "schedule"; payload: { time?: Date; condition?: string } }
  | { type: "unschedule"; payload: Record<string, never> }
  | { type: "assign"; payload: { actorId: string } }
  | { type: "release"; payload: Record<string, never> }
  | { type: "start"; payload: { context?: Record<string, unknown> } }
  | { type: "pause"; payload: { reason?: string; checkpoint?: unknown } }
  | { type: "resume"; payload: { context?: Record<string, unknown> } }
  | { type: "block"; payload: { reason: string; dependencies?: string[] } }
  | { type: "unblock"; payload: { resolution?: string } }
  | { type: "complete"; payload: { result: unknown; artifacts?: string[] } }
  | { type: "fail"; payload: { error: string; details?: unknown } }
  | { type: "cancel"; payload: { reason?: string; cancelChildren?: boolean } }
  | { type: "skip"; payload: { reason: string } }
  | { type: "retry"; payload: Record<string, never> }
  | { type: "timeout"; payload: Record<string, never> }

  // Work messages
  | { type: "spawn"; payload: { goal: string; deliverables: string[]; criteria: ObjectiveCriterion[]; context?: Record<string, unknown> } }
  | { type: "eval"; payload: Record<string, never> }
  | { type: "checkpoint"; payload: { data: unknown } }
  | { type: "query_status"; payload: Record<string, never> };

export type KnowledgeMessage =
  | { type: "append"; payload: { data: string; source?: string } }
  | { type: "query"; payload: { question: string; context?: Record<string, unknown> } }
  | { type: "synthesize"; payload: { fromNodes: string[] } };

export type Message = StandardMessage | TaskMessage | KnowledgeMessage;

// Response Types
export interface GetResponse {
  id: string;
  type: NodeType;
  properties: NodeProperties;
  edges: Edge[];
}

export interface ObserveResponse {
  state: string;
  observations: string[];
  metadata: Record<string, unknown>;
}

export interface EvalResponse {
  score: number;
  passed: boolean;
  objectiveCriteria: Array<ObjectiveCriterion & { passed: boolean; value: unknown }>;
  subjectiveCriteria?: SubjectiveCriterion[];
  observations: string[];
}

export interface StatusResponse {
  state: TaskState;
  progress: number;
  blockers: string[];
  childrenStatus: Array<{ id: string; state: TaskState; progress: number }>;
  attemptCount: number;
  maxAttempts: number;
  checkpoint?: unknown;
}

// Valid state transitions
export const VALID_TRANSITIONS: Record<TaskState, TaskState[]> = {
  created: ["planning", "ready", "cancelled"],
  planning: ["ready", "failed", "cancelled"],
  ready: ["scheduled", "assigned", "skipped", "cancelled"],
  scheduled: ["ready", "assigned", "cancelled"],
  assigned: ["ready", "active", "cancelled"],
  active: ["paused", "blocked", "completed", "failed", "timed_out", "cancelled"],
  paused: ["active", "cancelled"],
  blocked: ["active", "cancelled", "timed_out"],
  retrying: ["active", "failed"],
  completed: [],
  failed: ["retrying"],
  cancelled: [],
  skipped: [],
  timed_out: [],
};
