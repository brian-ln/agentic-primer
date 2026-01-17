# Task Actor System Specification

## Overview

The Task Actor System provides a hierarchical task management framework built on the Actor System. Tasks are actors that encapsulate goal-oriented work units with explicit success criteria, dependency tracking, and progress computation. Tasks integrate with a Graph for persistent identity mapping, edge-based relationships, and parent-child hierarchies.

## Core Concepts

### Task as Actor

A Task is an actor created by the `TaskActor` factory. Each task:
- Has a unique string identifier (`task_N` where N is auto-incremented)
- Is registered in both the Graph (for edges/queries) and the System (for messaging)
- Maintains internal state governed by a finite state machine
- Processes messages via a centralized handler

### Task Properties

```typescript
interface TaskProperties {
  // Identity
  id: string;                              // "task_1", "task_2", etc.
  type: "task";                            // Node type discriminator
  createdAt: Date;                         // Timestamp of creation

  // Goal Definition
  goal: string;                            // What the task aims to accomplish
  desiredDeliverables: string[];           // Expected outputs

  // Success Criteria
  objectiveSuccessCriteria: ObjectiveCriterion[];   // Measurable criteria
  subjectiveSuccessCriteria?: SubjectiveCriterion[]; // Qualitative criteria

  // Knowledge Management
  knownInformation: string[];              // Node IDs of known knowledge
  informationGaps: string[];               // What information is missing
  toolsAvailable: string[];                // Available tools/resources

  // State Management
  state: TaskState;                        // Current FSM state
  startedAt?: Date;                        // When task was started
  completedAt?: Date;                      // When task was completed
  result?: unknown;                        // Final result on completion

  // Hierarchy
  parentTaskId?: string;                   // Parent task ID (if spawned)

  // Metadata
  labels?: string[];                       // Categorization labels
  priority?: 0 | 1 | 2 | 3 | 4;           // Priority level (P0=critical, P4=low)
}
```

### Success Criteria

#### Objective Criteria
Measurable, quantifiable criteria with explicit thresholds:

```typescript
interface ObjectiveCriterion {
  criterion: string;     // What is being measured
  measure: string;       // How it is measured
  threshold: number | boolean;  // Pass threshold
  actual?: number | boolean;    // Actual measured value
  passed?: boolean;             // Evaluation result
}
```

**Examples:**
- `{ criterion: "Test coverage", measure: "percentage", threshold: 80 }`
- `{ criterion: "All tests pass", measure: "boolean", threshold: true }`

#### Subjective Criteria
Qualitative criteria requiring human judgment:

```typescript
interface SubjectiveCriterion {
  criterion: string;          // What is being evaluated
  evaluationGuidance: string; // How to evaluate
  assessment?: string;        // Human assessment result
  notes?: string;             // Additional notes
}
```

## Task State Machine

### States

| State | Description | Entry Conditions |
|-------|-------------|------------------|
| `created` | Task initialized, not yet ready | Factory creation |
| `ready` | Prerequisites satisfied, can be started | All dependencies met |
| `active` | Task is being worked on | Start message received |
| `blocked` | Task cannot proceed | Block message received or dependency failure |
| `completed` | Task finished successfully | All criteria passed + complete message |
| `failed` | Task failed | Explicit failure or unrecoverable error |

### State Transition Diagram

```
             [initialize]
                  |
                  v
              +-------+
              |created|
              +-------+
                  |
     [dependencies met] or [start]
                  |
                  v
              +-------+
              | ready |<-----------------------+
              +-------+                        |
                  |                            |
              [start]                    [unblock]
                  |                            |
                  v                            |
              +-------+      [block]      +-------+
              |active |------------------>|blocked|
              +-------+                   +-------+
                  |                            |
         [eval passes]                    [abort]
          + [complete]                         |
                  |                            |
                  v                            v
           +----------+                  +--------+
           |completed |                  | failed |
           +----------+                  +--------+
```

### Transition Rules

1. **created -> active**: Via `start` message (bypasses `ready`)
2. **created -> ready**: When all `depends_on` edges point to completed tasks
3. **ready -> active**: Via `start` message
4. **active -> blocked**: Via `block` message with reason
5. **active -> completed**: Via `complete` message when `eval` passes
6. **blocked -> ready**: Via `unblock` when blockage resolved
7. **blocked -> failed**: Via `abort` or timeout

## Message Interface

### Standard Node Messages

These messages are shared across all node types (task, knowledge, artifact):

#### get
Returns full node state including properties and edges.

```typescript
// Request
{ type: "get", payload: {} }

// Response
{
  id: string;
  type: "task";
  properties: TaskProperties;
  edges: Edge[];
}
```

#### observe
Returns a human-readable summary of task status.

```typescript
// Request
{ type: "observe", payload: {} }

// Response
{
  state: TaskState;
  observations: string[];  // Human-readable observations
  metadata: {
    progress: number;      // 0-1 progress
    childCount: number;    // Number of child tasks
  }
}
```

#### update
Modifies task properties (except protected fields).

```typescript
// Request
{
  type: "update",
  payload: {
    properties: Partial<TaskProperties>
  }
}

// Response
{
  success: boolean;
  updatedProperties: string[];  // List of updated field names
}
```

**Protected fields** (cannot be updated): `id`, `type`, `createdAt`

#### link
Creates an edge from this task to another node.

```typescript
// Request
{
  type: "link",
  payload: {
    toId: string;          // Target node ID
    edgeType: EdgeType;    // Edge type
    properties?: Record<string, unknown>;
  }
}

// Response
{ edgeId: string; success: boolean }
```

#### unlink
Removes an edge by ID.

```typescript
// Request
{ type: "unlink", payload: { edgeId: string } }

// Response
{ success: boolean }
```

#### delete
Removes the task and all connected edges from the graph.

```typescript
// Request
{ type: "delete", payload: {} }

// Response
{ success: boolean }
```

### Task-Specific Messages

#### start
Transitions task from `created` or `ready` to `active`.

```typescript
// Request
{
  type: "start",
  payload: {
    context?: Record<string, unknown>  // Optional execution context
  }
}

// Response
{ success: boolean; state: TaskState }
```

**Preconditions:**
- Task state must be `created` or `ready`

**Effects:**
- Sets `state` to `active`
- Sets `startedAt` to current timestamp
- Stores `context` if provided

#### create_task
Spawns a child task with inheritance from parent.

```typescript
// Request
{
  type: "create_task",
  payload: {
    goal: string;
    deliverables: string[];
    criteria: ObjectiveCriterion[];
    context?: Record<string, unknown>;
  }
}

// Response
{ childTaskId: string; success: boolean }
```

**Effects:**
- Creates new TaskActor with provided parameters
- Inherits `toolsAvailable` from parent
- Sets `parentTaskId` to this task's ID
- Creates `spawned_by` edge from child to parent

#### eval
Evaluates success criteria and checks child task completion.

```typescript
// Request
{ type: "eval", payload: {} }

// Response
{
  score: number;          // 0-1, ratio of passed criteria
  passed: boolean;        // All criteria passed + children completed
  objectiveCriteria: Array<{
    criterion: string;
    measure: string;
    threshold: number | boolean;
    actual: unknown;
    passed: boolean;
    value: unknown;
  }>;
  subjectiveCriteria?: SubjectiveCriterion[];
  observations: string[];
}
```

**Evaluation Logic:**
1. For each objective criterion:
   - Boolean threshold: `actual === threshold`
   - Numeric threshold: `actual >= threshold`
2. Calculate score: `passedCount / totalCriteria`
3. Check all child tasks are completed
4. `passed` = all criteria passed AND all children completed

#### complete
Marks task as completed with result and optional artifacts.

```typescript
// Request
{
  type: "complete",
  payload: {
    result: unknown;       // Final result data
    artifacts?: string[];  // IDs of produced artifacts
  }
}

// Response
{ success: boolean; finalState: TaskState }
```

**Preconditions:**
- `eval` must return `passed: true`

**Effects:**
- Sets `state` to `completed`
- Sets `completedAt` to current timestamp
- Stores `result` in properties
- Creates `produces` edges to artifact IDs

#### block
Marks task as blocked with reason.

```typescript
// Request
{
  type: "block",
  payload: {
    reason: string;                  // Why the task is blocked
    requiredKnowledge?: string[];    // What information is needed
  }
}

// Response
{ success: boolean; state: TaskState }
```

**Effects:**
- Sets `state` to `blocked`
- Stores `blockReason` in properties
- Appends `requiredKnowledge` to `informationGaps`

#### query_status
Returns comprehensive status including children and blockers.

```typescript
// Request
{ type: "query_status", payload: {} }

// Response
{
  state: TaskState;
  progress: number;          // 0-1, recursive progress
  blockers: string[];        // List of blocking reasons
  childrenStatus: Array<{
    id: string;
    state: TaskState;
    progress: number;
  }>;
}
```

**Blocker Detection:**
- Includes `blockReason` if task is blocked
- Includes incomplete dependencies (`depends_on` edges)

## Progress Calculation

Progress is calculated recursively based on state and children:

```typescript
function calculateProgress(task: TaskProperties, graph: Graph): number {
  // Completed tasks have full progress
  if (task.state === "completed") return 1;

  // Created tasks have no progress
  if (task.state === "created") return 0;

  // Leaf tasks (no children) - progress by state
  const children = graph.getChildTasks(task.id);
  if (children.length === 0) {
    return task.state === "active" ? 0.5 : 0.1;
  }

  // Tasks with children - average of child progress
  const childProgress = children.reduce((sum, childId) => {
    const childProps = graph.getNodeProperties(childId);
    return sum + calculateProgress(childProps, graph);
  }, 0);

  return childProgress / children.length;
}
```

## Edge Types

Tasks use these edge types for relationships:

| Edge Type | From | To | Meaning |
|-----------|------|-----|---------|
| `spawned_by` | Child task | Parent task | Parent spawned this child |
| `depends_on` | Task | Task | This task depends on another |
| `requires_knowledge` | Task | Knowledge | Task needs this knowledge |
| `produces` | Task | Artifact | Task produced this artifact |
| `blocks` | Task | Task | This task blocks another |
| `references` | Any | Any | Generic reference |

## Priority Levels

Priority is expressed as P0-P4:

| Level | Name | Description |
|-------|------|-------------|
| P0 | Critical | Immediate attention required |
| P1 | High | Should be addressed soon |
| P2 | Medium | Normal priority (default) |
| P3 | Low | Can be deferred |
| P4 | Backlog | Nice to have |

## Factory Pattern

### TaskActor Factory

```typescript
const TaskActor: ActorFactory<TaskActorData> = (data) => {
  // 1. Initialize properties
  const id = `task_${++taskCounter}`;
  const properties: TaskProperties = { ... };

  // 2. Create actor with message handler
  const actor = {
    send: async (message: ActorMessage) => {
      const result = await handleMessage(message, properties, data.graph);
      return { success: true, data: result };
    }
  };

  // 3. Register with system
  const address = data.graph.getSystem().register(actor);

  // 4. Register with graph (for edges and queries)
  data.graph.registerNode(id, address, properties);

  // 5. Return Address
  return address;
};
```

### CreateTaskOptions

```typescript
interface CreateTaskOptions {
  goal: string;
  desiredDeliverables: string[];
  objectiveSuccessCriteria: ObjectiveCriterion[];
  subjectiveSuccessCriteria?: SubjectiveCriterion[];
  informationGaps?: string[];
  toolsAvailable?: string[];
  parentTaskId?: string;
  labels?: string[];
  priority?: 0 | 1 | 2 | 3 | 4;
}
```

## Graph Integration

### Dual Registration

Tasks are registered in two places:
1. **System**: For actor messaging (Address-based)
2. **Graph**: For edges, queries, and serialization (string ID-based)

### String ID Mapping

The Graph maintains bidirectional mapping:
- `nodes: Map<string, Address>` - ID to Address
- `nodeProperties: Map<string, NodeProperties>` - ID to properties cache

### Message Routing

External callers use string IDs:
```typescript
await graph.send("task_1", "start", { context: {} });
```

Graph resolves ID to Address and forwards message:
```typescript
const address = this.nodes.get(nodeId);
await address.send(message);
```

## Usage Examples

### Create a Task

```typescript
import { TaskActor, createTask } from "./task";
import { Graph } from "./graph";

const graph = new Graph();

// Using factory directly
const taskAddress = TaskActor({
  goal: "Implement user authentication",
  desiredDeliverables: ["Auth module", "Tests", "Documentation"],
  objectiveSuccessCriteria: [
    { criterion: "All tests pass", measure: "boolean", threshold: true },
    { criterion: "Coverage", measure: "percentage", threshold: 80 }
  ],
  graph
});

// Using convenience function
const task2 = createTask({
  goal: "Write login endpoint",
  desiredDeliverables: ["POST /login endpoint"],
  objectiveSuccessCriteria: [
    { criterion: "Returns JWT on success", measure: "boolean", threshold: true }
  ]
}, graph);
```

### Start and Complete a Task

```typescript
// Start the task
await graph.send("task_1", "start", { context: { assignee: "dev1" } });

// Do work...

// Update criteria actuals
await graph.send("task_1", "update", {
  properties: {
    objectiveSuccessCriteria: [
      { criterion: "All tests pass", measure: "boolean", threshold: true, actual: true }
    ]
  }
});

// Complete
await graph.send("task_1", "complete", {
  result: { endpoint: "/login", method: "POST" }
});
```

### Create Child Task

```typescript
// From within task (or via graph.send)
const response = await graph.send("task_1", "create_task", {
  goal: "Write unit tests",
  deliverables: ["auth.test.ts"],
  criteria: [{ criterion: "Tests pass", measure: "boolean", threshold: true }]
});

console.log(response.childTaskId); // "task_2"
```

### Query Status

```typescript
const status = await graph.send("task_1", "query_status", {});
console.log(status);
// {
//   state: "active",
//   progress: 0.75,
//   blockers: [],
//   childrenStatus: [
//     { id: "task_2", state: "completed", progress: 1 },
//     { id: "task_3", state: "active", progress: 0.5 }
//   ]
// }
```

## Invariants

### Task Identity Invariant
```
forall task: TaskProperties
  => task.id matches pattern "task_N"
  => task.id is unique in graph
  => task.type === "task"
```

### State Machine Invariant
```
forall task: TaskProperties
  => task.state in {created, ready, active, blocked, completed, failed}
  => state transitions follow FSM rules
  => completed/failed are terminal states
```

### Hierarchy Invariant
```
forall child: TaskProperties where child.parentTaskId exists
  => exists edge(child.id, child.parentTaskId, "spawned_by")
  => parent.id === child.parentTaskId
```

### Progress Invariant
```
forall task: TaskProperties
  => 0 <= progress(task) <= 1
  => task.state === "completed" implies progress(task) === 1
  => task.state === "created" implies progress(task) === 0
```

### Completion Invariant
```
forall task: TaskProperties where task.state === "completed"
  => task.completedAt exists
  => task.result exists
  => eval(task).passed === true
```

## Non-Requirements

This specification does NOT require:
- Automatic state transitions (e.g., auto-ready when deps complete)
- Persistence or serialization
- Distributed task execution
- Task scheduling or prioritization enforcement
- Timeout handling
- Retry logic
- Notification system

These may be added as extensions but are not part of the core specification.
