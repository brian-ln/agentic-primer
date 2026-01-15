// Core Types for Task/Knowledge Graph Actor Protocol

export type NodeType = "task" | "knowledge" | "artifact" | "pattern";

export type TaskState =
  | "created"
  | "ready"
  | "active"
  | "blocked"
  | "completed"
  | "failed";

export type EdgeType =
  | "depends_on"
  | "requires_knowledge"
  | "produces"
  | "spawned_by"
  | "blocks"
  | "references";

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

// Task Node Properties
export interface TaskProperties extends NodeProperties {
  type: "task";
  state: TaskState;
  goal: string;
  desiredDeliverables: string[];
  objectiveSuccessCriteria: ObjectiveCriterion[];
  subjectiveSuccessCriteria?: SubjectiveCriterion[];
  knownInformation: string[]; // node IDs
  informationGaps: string[];
  toolsAvailable: string[];
  startedAt?: Date;
  completedAt?: Date;
  parentTaskId?: string;
  result?: unknown;
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
  | { type: "start"; payload: { context?: Record<string, unknown> } }
  | { type: "create_task"; payload: { goal: string; deliverables: string[]; criteria: ObjectiveCriterion[]; context?: Record<string, unknown> } }
  | { type: "eval"; payload: Record<string, never> }
  | { type: "complete"; payload: { result: unknown; artifacts?: string[] } }
  | { type: "block"; payload: { reason: string; requiredKnowledge?: string[] } }
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
}
