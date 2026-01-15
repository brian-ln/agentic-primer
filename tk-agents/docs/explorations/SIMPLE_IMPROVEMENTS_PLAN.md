# Implementation Plan: 3 Simple Improvements

**Status**: Ready for implementation by AI agents
**Estimated Effort**: 2-3 hours total (agent implementation)
**Impact**: High value, low complexity

---

## Improvement 1: Rename `spawn` → `create_task`

### Rationale
- Current: `spawn` implies "start running now" (Erlang semantics)
- Reality: Creates task in `created` state, requires explicit `start`
- Better: `create_task` is honest about what it does

### Implementation Steps

**Step 1.1**: Update TaskNode message handler
```typescript
// File: src/task.ts
// Find: case "spawn":
// Replace with: case "create_task":

handleMessage(message: Message, graph: Graph): unknown {
  switch (message.type) {
    // ... other cases
    case "create_task":  // Changed from "spawn"
      return this.handleCreateTask(message.payload as {...}, graph);
  }
}
```

**Step 1.2**: Rename the handler method
```typescript
// File: src/task.ts
// Rename: private handleSpawn(...) {...}
// To: private handleCreateTask(...) {...}

private handleCreateTask(
  payload: { goal: string; deliverables: string[]; criteria: ObjectiveCriterion[]; context?: Record<string, unknown> },
  graph: Graph
): { childTaskId: string; success: boolean } {
  // Implementation stays the same
}
```

**Step 1.3**: Update demo.ts
```typescript
// File: demo.ts
// Find all: graph.send(mainTask.properties.id, "spawn", {...})
// Replace with: graph.send(mainTask.properties.id, "create_task", {...})
```

**Step 1.4**: Update tests
```typescript
// File: src/task.test.ts (create if doesn't exist)
test("create_task message creates child task", () => {
  const graph = new Graph();
  const parent = createTask({...}, graph);

  const result = graph.send(parent.properties.id, "create_task", {
    goal: "Child task",
    deliverables: ["test"],
    criteria: []
  });

  expect(result.success).toBe(true);
  expect(result.childTaskId).toBeDefined();
});
```

**Success Criteria**:
- ✅ All references to "spawn" replaced with "create_task"
- ✅ Demo runs successfully
- ✅ Tests pass
- ✅ No breaking changes to API semantics

---

## Improvement 2: Death Detection (Exception + Heartbeat)

### Rationale
- Current: Actors can crash silently or hang indefinitely
- Need: Detect when actors die (exception) or become unresponsive (timeout)
- Solution: Try/catch in Registry.send() + optional heartbeat monitoring

### Implementation Steps

**Step 2.1**: Add death detection to Registry.send()

```typescript
// File: src/actors/registry.ts

import { EventEmitter } from "events";

export class Registry extends EventEmitter {
  private actors: Map<string, ActorInfo> = new Map();

  async send(actorId: string, message: Message): Promise<Response> {
    const info = this.actors.get(actorId);
    if (!info) {
      return { success: false, error: `Actor not found: ${actorId}` };
    }

    info.messageCount++;
    info.lastMessageAt = new Date();

    try {
      const response = await info.actor.send(message);
      info.lastSuccessAt = new Date();  // Track last successful message
      return response;
    } catch (error) {
      // Actor crashed!
      this.emit('actor_died', {
        actorId,
        error: error instanceof Error ? error.message : String(error),
        reason: 'exception',
        lastMessageAt: info.lastMessageAt,
      });

      // Remove dead actor from registry
      this.actors.delete(actorId);

      return {
        success: false,
        error: `Actor ${actorId} died: ${error instanceof Error ? error.message : String(error)}`,
      };
    }
  }
}
```

**Step 2.2**: Add ActorInfo fields for monitoring

```typescript
// File: src/actors/registry.ts

export interface ActorInfo {
  actor: Actor;
  registeredAt: Date;
  messageCount: number;
  lastMessageAt?: Date;
  lastSuccessAt?: Date;  // New: track successful sends
  heartbeatInterval?: NodeJS.Timeout;  // New: for heartbeat monitoring
}
```

**Step 2.3**: Add optional heartbeat monitoring

```typescript
// File: src/actors/registry.ts

export class Registry extends EventEmitter {
  // ... existing code

  /**
   * Start heartbeat monitoring for an actor
   * Pings actor periodically, emits 'actor_died' if ping fails
   */
  startHeartbeat(actorId: string, intervalMs: number = 30000): void {
    const info = this.actors.get(actorId);
    if (!info) {
      throw new Error(`Cannot start heartbeat: Actor not found ${actorId}`);
    }

    // Clear existing heartbeat if any
    if (info.heartbeatInterval) {
      clearInterval(info.heartbeatInterval);
    }

    info.heartbeatInterval = setInterval(async () => {
      try {
        await this.send(actorId, { type: 'ping', id: `heartbeat_${Date.now()}`, payload: {} });
      } catch {
        // Ping failed - actor is dead or unresponsive
        this.emit('actor_died', {
          actorId,
          reason: 'heartbeat_timeout',
          lastSuccessAt: info.lastSuccessAt,
        });

        // Clean up
        if (info.heartbeatInterval) {
          clearInterval(info.heartbeatInterval);
        }
        this.actors.delete(actorId);
      }
    }, intervalMs);
  }

  /**
   * Stop heartbeat monitoring
   */
  stopHeartbeat(actorId: string): void {
    const info = this.actors.get(actorId);
    if (info?.heartbeatInterval) {
      clearInterval(info.heartbeatInterval);
      info.heartbeatInterval = undefined;
    }
  }

  /**
   * Clean up on unregister
   */
  unregister(actorId: string): boolean {
    const info = this.actors.get(actorId);
    if (info) {
      this.stopHeartbeat(actorId);
      if (info.actor.stop) {
        info.actor.stop();
      }
    }
    return this.actors.delete(actorId);
  }
}
```

**Step 2.4**: Add 'ping' handler to actors

```typescript
// File: src/actors/claude.ts (and bash.ts, mock.ts)

async send(message: Message): Promise<Response> {
  // Handle ping for heartbeat
  if (message.type === 'ping') {
    return {
      success: true,
      data: { alive: true, timestamp: Date.now() },
    };
  }

  // ... rest of implementation
}
```

**Step 2.5**: Add tests for death detection

```typescript
// File: src/actors/registry.test.ts

import { describe, test, expect, mock } from "bun:test";
import { Registry } from "./registry";
import { MockActor } from "./mock";

describe("Death Detection", () => {
  test("emits actor_died on exception", async () => {
    const registry = new Registry();
    const actor = new MockActor("test-1");

    // Make actor throw on next send
    actor.setResponseForNext(new Error("Crash!"));
    registry.register(actor);

    let deathEvent: any;
    registry.on('actor_died', (event) => {
      deathEvent = event;
    });

    const response = await registry.send("test-1", { type: "test", id: "1", payload: {} });

    expect(response.success).toBe(false);
    expect(deathEvent).toBeDefined();
    expect(deathEvent.actorId).toBe("test-1");
    expect(deathEvent.reason).toBe("exception");
    expect(registry.has("test-1")).toBe(false); // Actor removed
  });

  test("heartbeat detects unresponsive actor", async () => {
    const registry = new Registry();
    const actor = new MockActor("test-2");
    registry.register(actor);

    let deathEvent: any;
    registry.on('actor_died', (event) => {
      deathEvent = event;
    });

    // Start heartbeat with short interval for testing
    registry.startHeartbeat("test-2", 100);

    // Make actor hang on next send
    actor.setDelayForNext(200); // Longer than heartbeat interval

    // Wait for heartbeat to detect failure
    await new Promise(resolve => setTimeout(resolve, 250));

    expect(deathEvent).toBeDefined();
    expect(deathEvent.actorId).toBe("test-2");
    expect(deathEvent.reason).toBe("heartbeat_timeout");
  });
});
```

**Success Criteria**:
- ✅ Registry.send() catches exceptions and emits 'actor_died'
- ✅ Dead actors removed from registry
- ✅ Optional heartbeat monitoring works
- ✅ Actors respond to 'ping' messages
- ✅ Tests verify both exception and heartbeat detection
- ✅ No performance impact when heartbeat not enabled

---

## Improvement 3: Task-to-Executor Edges

### Rationale
- Current: No connection between tasks and who executes them
- Needed: Model assignment via graph edges (consistent with "everything is a node")
- Benefits: Queryable assignment, historical tracking, worker pool routing

### Implementation Steps

**Step 3.1**: Create ActorNode (wrapper for executors)

```typescript
// File: src/actors/actor-node.ts

import type { Graph, NodeActor } from "../graph";
import type { Message, NodeProperties } from "../types";
import type { Actor } from "./base";

export interface ActorNodeProperties extends NodeProperties {
  type: "actor";
  actorType: "claude" | "bash" | "mock";
  capabilities: string[];  // e.g., ["code", "terminal", "reasoning"]
  status: "idle" | "busy" | "offline";
  executor: Actor;  // Reference to actual ClaudeActor/BashActor
}

export class ActorNode implements NodeActor {
  properties: ActorNodeProperties;

  constructor(executor: Actor, capabilities: string[] = []) {
    this.properties = {
      id: `actor_${executor.id}`,
      type: "actor",
      actorType: executor.type as "claude" | "bash" | "mock",
      capabilities,
      status: "idle",
      createdAt: new Date(),
      executor,
    };
  }

  handleMessage(message: Message, graph: Graph): unknown {
    switch (message.type) {
      // Standard messages
      case "get":
        return this.handleGet(graph);
      case "observe":
        return this.handleObserve();
      case "update":
        return this.handleUpdate(message.payload as { properties: Partial<ActorNodeProperties> });

      // Actor-specific messages
      case "execute":
        return this.handleExecute(message);
      case "ping":
        return this.handlePing();
      case "get_status":
        return this.handleGetStatus();

      default:
        throw new Error(`Unknown message type: ${message.type}`);
    }
  }

  private async handleExecute(message: Message): Promise<unknown> {
    this.properties.status = "busy";
    try {
      const result = await this.properties.executor.send(message);
      this.properties.status = "idle";
      return result;
    } catch (error) {
      this.properties.status = "offline";
      throw error;
    }
  }

  private handlePing(): unknown {
    return { alive: true, status: this.properties.status };
  }

  private handleGetStatus(): unknown {
    return {
      id: this.properties.id,
      type: this.properties.actorType,
      capabilities: this.properties.capabilities,
      status: this.properties.status,
    };
  }

  // ... implement standard message handlers (get, observe, update)
}

export function createActorNode(executor: Actor, capabilities: string[], graph: Graph): ActorNode {
  const node = new ActorNode(executor, capabilities);
  graph.registerNode(node);
  return node;
}
```

**Step 3.2**: Create ExecutionNode

```typescript
// File: src/execution.ts

import type { Graph, NodeActor } from "./graph";
import type { Message, NodeProperties } from "./types";

export interface ExecutionNodeProperties extends NodeProperties {
  type: "execution";
  state: "pending" | "running" | "completed" | "failed";
  startedAt?: Date;
  completedAt?: Date;
  result?: unknown;
  error?: string;
  context: Record<string, unknown>;
}

export class ExecutionNode implements NodeActor {
  properties: ExecutionNodeProperties;

  constructor(options: { context?: Record<string, unknown> } = {}) {
    this.properties = {
      id: `execution_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      type: "execution",
      state: "pending",
      createdAt: new Date(),
      context: options.context || {},
    };
  }

  handleMessage(message: Message, graph: Graph): unknown {
    switch (message.type) {
      case "get":
        return this.handleGet(graph);
      case "start":
        return this.handleStart(graph);
      case "complete":
        return this.handleComplete(message.payload as { result: unknown });
      case "fail":
        return this.handleFail(message.payload as { error: string });
      default:
        throw new Error(`Unknown message type: ${message.type}`);
    }
  }

  private handleStart(graph: Graph): unknown {
    if (this.properties.state !== "pending") {
      return { success: false, error: "Execution already started" };
    }

    this.properties.state = "running";
    this.properties.startedAt = new Date();

    // Find assigned actor and task
    const edges = graph.getEdges();
    const taskEdge = edges.find(e => e.fromId === this.properties.id && e.type === "executes");
    const actorEdge = edges.find(e => e.fromId === this.properties.id && e.type === "assigned_to");

    if (!taskEdge || !actorEdge) {
      return { success: false, error: "Execution not properly assigned" };
    }

    // Delegate to assigned actor
    return graph.send(actorEdge.toId, "execute", {
      taskId: taskEdge.toId,
      context: this.properties.context,
    });
  }

  private handleComplete(payload: { result: unknown }): unknown {
    this.properties.state = "completed";
    this.properties.completedAt = new Date();
    this.properties.result = payload.result;
    return { success: true };
  }

  private handleFail(payload: { error: string }): unknown {
    this.properties.state = "failed";
    this.properties.completedAt = new Date();
    this.properties.error = payload.error;
    return { success: true };
  }

  // ... implement get handler
}

export function createExecution(options: { context?: Record<string, unknown> }, graph: Graph): ExecutionNode {
  const node = new ExecutionNode(options);
  graph.registerNode(node);
  return node;
}
```

**Step 3.3**: Create WorkerPoolNode

```typescript
// File: src/worker-pool.ts

import type { Graph, NodeActor } from "./graph";
import type { Message, NodeProperties } from "./types";

export interface WorkerPoolNodeProperties extends NodeProperties {
  type: "worker_pool";
  name: string;
  capabilities: string[];
  policy: "round_robin" | "least_busy" | "random";
}

export class WorkerPoolNode implements NodeActor {
  properties: WorkerPoolNodeProperties;
  private nextIndex: number = 0;

  constructor(options: { name: string; capabilities: string[]; policy?: "round_robin" | "least_busy" | "random" }) {
    this.properties = {
      id: `pool_${options.name}`,
      type: "worker_pool",
      name: options.name,
      capabilities: options.capabilities,
      policy: options.policy || "round_robin",
      createdAt: new Date(),
    };
  }

  handleMessage(message: Message, graph: Graph): unknown {
    switch (message.type) {
      case "get":
        return this.handleGet(graph);
      case "add_worker":
        return this.handleAddWorker(message.payload as { actorId: string }, graph);
      case "remove_worker":
        return this.handleRemoveWorker(message.payload as { actorId: string }, graph);
      case "select_worker":
        return this.handleSelectWorker(graph);
      case "get_workers":
        return this.handleGetWorkers(graph);
      default:
        throw new Error(`Unknown message type: ${message.type}`);
    }
  }

  private handleAddWorker(payload: { actorId: string }, graph: Graph): unknown {
    graph.addEdge(this.properties.id, payload.actorId, "has_member");
    return { success: true };
  }

  private handleRemoveWorker(payload: { actorId: string }, graph: Graph): unknown {
    const edges = graph.getEdges();
    const edge = edges.find(e =>
      e.fromId === this.properties.id &&
      e.toId === payload.actorId &&
      e.type === "has_member"
    );
    if (edge) {
      graph.removeEdge(edge.id);
    }
    return { success: true };
  }

  private handleSelectWorker(graph: Graph): unknown {
    const workers = this.getWorkers(graph);
    if (workers.length === 0) {
      return { success: false, error: "No workers available" };
    }

    switch (this.properties.policy) {
      case "round_robin":
        const selected = workers[this.nextIndex % workers.length];
        this.nextIndex++;
        return { success: true, actorId: selected };

      case "random":
        const random = workers[Math.floor(Math.random() * workers.length)];
        return { success: true, actorId: random };

      case "least_busy":
        // TODO: Query execution count per actor, select least busy
        return { success: true, actorId: workers[0] }; // Fallback to first

      default:
        return { success: true, actorId: workers[0] };
    }
  }

  private handleGetWorkers(graph: Graph): unknown {
    return { workers: this.getWorkers(graph) };
  }

  private getWorkers(graph: Graph): string[] {
    const edges = graph.getEdges();
    return edges
      .filter(e => e.fromId === this.properties.id && e.type === "has_member")
      .map(e => e.toId);
  }

  // ... implement get handler
}

export function createWorkerPool(
  options: { name: string; capabilities: string[]; policy?: "round_robin" | "least_busy" | "random" },
  graph: Graph
): WorkerPoolNode {
  const node = new WorkerPoolNode(options);
  graph.registerNode(node);
  return node;
}
```

**Step 3.4**: Update demo to use task-to-executor edges

```typescript
// File: demo-with-execution.ts (new example)

import { Graph } from "./src/graph";
import { createTask } from "./src/task";
import { createActorNode } from "./src/actors/actor-node";
import { createExecution } from "./src/execution";
import { createWorkerPool } from "./src/worker-pool";
import { ClaudeActor } from "./src/actors/claude";

const graph = new Graph();

// Create worker pool
const claudePool = createWorkerPool({
  name: "claude-workers",
  capabilities: ["code", "reasoning"],
}, graph);

// Create actors and add to pool
const claude1 = new ClaudeActor({ sessionId: "session-1" });
const actorNode1 = createActorNode(claude1, ["code", "reasoning"], graph);
graph.send(claudePool.properties.id, "add_worker", { actorId: actorNode1.properties.id });

// Create task
const task = createTask({
  goal: "Implement feature X",
  desiredDeliverables: ["code", "tests"],
  objectiveSuccessCriteria: [{ criterion: "Tests pass", measure: "pass_rate", threshold: 1.0 }],
}, graph);

// Create execution and assign
const execution = createExecution({}, graph);
graph.send(execution.properties.id, "link", { toId: task.properties.id, edgeType: "executes" });

// Pool selects worker
const workerSelection = graph.send(claudePool.properties.id, "select_worker", {});
if (workerSelection.success) {
  graph.send(execution.properties.id, "link", {
    toId: workerSelection.actorId,
    edgeType: "assigned_to"
  });
}

// Start execution
graph.send(execution.properties.id, "start", {});
```

**Step 3.5**: Add tests for execution flow

```typescript
// File: src/execution.test.ts

import { describe, test, expect } from "bun:test";
import { Graph } from "./graph";
import { createTask } from "./task";
import { createExecution } from "./execution";
import { createActorNode } from "./actors/actor-node";
import { MockActor } from "./actors/mock";

describe("Task-to-Executor Edges", () => {
  test("execution links to task and actor", () => {
    const graph = new Graph();
    const task = createTask({ goal: "test", desiredDeliverables: [], objectiveSuccessCriteria: [] }, graph);
    const actor = createActorNode(new MockActor("test-1"), ["code"], graph);
    const execution = createExecution({}, graph);

    // Link execution to task and actor
    graph.send(execution.properties.id, "link", { toId: task.properties.id, edgeType: "executes" });
    graph.send(execution.properties.id, "link", { toId: actor.properties.id, edgeType: "assigned_to" });

    // Verify edges exist
    const edges = graph.getEdges();
    expect(edges.some(e => e.fromId === execution.properties.id && e.toId === task.properties.id)).toBe(true);
    expect(edges.some(e => e.fromId === execution.properties.id && e.toId === actor.properties.id)).toBe(true);
  });

  test("worker pool selects actor", () => {
    const graph = new Graph();
    const pool = createWorkerPool({ name: "test-pool", capabilities: ["code"] }, graph);
    const actor = createActorNode(new MockActor("test-1"), ["code"], graph);

    graph.send(pool.properties.id, "add_worker", { actorId: actor.properties.id });

    const result = graph.send(pool.properties.id, "select_worker", {});
    expect(result.success).toBe(true);
    expect(result.actorId).toBe(actor.properties.id);
  });
});
```

**Success Criteria**:
- ✅ ActorNode wraps executors in graph
- ✅ ExecutionNode tracks execution state
- ✅ WorkerPoolNode manages pool of actors
- ✅ Edges express assignment relationships
- ✅ Can query "what actor is executing this task?"
- ✅ Can query "what tasks has this actor executed?"
- ✅ Tests verify full execution flow

---

## Integration & Testing

### Overall Test Strategy

**Unit Tests** (per improvement):
- Improvement 1: Message handler routing
- Improvement 2: Death detection events
- Improvement 3: Node creation and edge management

**Integration Tests**:
```typescript
// File: tests/integration.test.ts

describe("Three Improvements Integration", () => {
  test("full workflow: create task, assign to pool, detect actor death", async () => {
    const graph = new Graph();
    const registry = new Registry();

    // Setup pool and actors
    const pool = createWorkerPool({ name: "workers", capabilities: ["code"] }, graph);
    const actor = new ClaudeActor({ sessionId: "test" });
    const actorNode = createActorNode(actor, ["code"], graph);
    registry.register(actor);
    graph.send(pool.properties.id, "add_worker", { actorId: actorNode.properties.id });

    // Create task (using new create_task message)
    const parent = createTask({ goal: "parent", desiredDeliverables: [], objectiveSuccessCriteria: [] }, graph);
    const childResult = graph.send(parent.properties.id, "create_task", {
      goal: "child task",
      deliverables: ["code"],
      criteria: []
    });

    // Assign to pool
    const execution = createExecution({}, graph);
    graph.send(execution.properties.id, "link", { toId: childResult.childTaskId, edgeType: "executes" });
    const workerSelection = graph.send(pool.properties.id, "select_worker", {});
    graph.send(execution.properties.id, "link", { toId: workerSelection.actorId, edgeType: "assigned_to" });

    // Setup death detection
    let actorDied = false;
    registry.on('actor_died', () => { actorDied = true; });

    // Simulate actor failure
    // ... test death detection works

    expect(actorDied).toBe(true);
  });
});
```

**Demo Update**:
- Update existing demo.ts to use "create_task"
- Create new demo-with-execution.ts showing full execution flow
- Verify both demos run successfully

---

## Agent Implementation Specification

### For Each Improvement:

**Input**: This plan document + current codebase
**Process**:
1. Read specified files
2. Implement changes as described
3. Run tests (bun test)
4. Fix any failures
5. Verify demo still works

**Output**:
- Modified files committed to git
- All tests passing
- Demo runs successfully
- Documentation updated if needed

**Success Validation**:
- Run: `bun test` → all tests pass
- Run: `bun run demo.ts` → completes without errors
- Run: `git diff` → review changes match specification
- Run: `git log` → commit messages describe each improvement

---

## Estimated Effort

| Improvement | Effort | Files Changed | Tests Added |
|-------------|--------|---------------|-------------|
| 1. Rename spawn → create_task | 30 min | 2-3 files | 1-2 tests |
| 2. Death detection | 1 hour | 3-4 files | 3-4 tests |
| 3. Task-to-executor edges | 1-1.5 hours | 5-7 new files | 4-6 tests |
| **Total** | **2.5-3 hours** | **10-14 files** | **8-12 tests** |

**Parallelization**: Improvements 1 and 2 can be done in parallel. Improvement 3 depends on both.

---

## Next Steps

1. ✅ Plan complete - ready for agent implementation
2. Assign to AI agent with specification prompt
3. Agent implements, tests, commits
4. Human reviews git diff and runs demos
5. Merge if successful

**Note**: This is detailed enough for AI agents to implement autonomously with objective success criteria (tests passing, demos working).
