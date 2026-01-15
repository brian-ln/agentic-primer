# Test Protocol Design: Layered Testing Strategy

**Purpose**: Define separate test suites for each protocol layer
**Status**: Design specification for agent implementation
**Test Framework**: Bun test

---

## Testing Philosophy

### Principle: Test What You Build On

```
Layer 3: Actor Lifecycle Tests (death detection, monitoring)
         ↑ depends on
Layer 2: Domain Protocol Tests (task, knowledge, execution)
         ↑ depends on
Layer 1: Graph Protocol Tests (send/receive primitives)
```

Each layer has its own test suite. Lower layers must pass before testing higher layers.

---

## Layer 1: Graph Protocol Tests

**File**: `tests/graph-protocol.test.ts`

**Purpose**: Verify the primitive `send/receive` protocol works correctly for ALL node types.

### Test Coverage

#### 1.1 Core Routing
```typescript
import { describe, test, expect } from "bun:test";
import { Graph } from "../src/graph";
import { MockNode } from "./fixtures/mock-node";

describe("Graph Protocol: Core Routing", () => {
  test("send() routes message to node.handleMessage()", () => {
    const graph = new Graph();
    const node = new MockNode("test-1");
    graph.registerNode(node);

    const result = graph.send("test-1", "test_message", { data: "hello" });

    expect(result.received).toBe(true);
    expect(result.messageType).toBe("test_message");
    expect(result.payload.data).toBe("hello");
  });

  test("send() throws on unknown node", () => {
    const graph = new Graph();
    expect(() => graph.send("unknown", "test", {})).toThrow("Node not found");
  });

  test("send() returns value from handleMessage()", () => {
    const graph = new Graph();
    const node = new MockNode("test-1");
    node.setResponse({ custom: "response" });
    graph.registerNode(node);

    const result = graph.send("test-1", "test", {});
    expect(result.custom).toBe("response");
  });
});
```

#### 1.2 Standard Messages (ALL nodes must support)
```typescript
describe("Graph Protocol: Standard Messages", () => {
  test.each([
    { nodeType: "task", createFn: createTask },
    { nodeType: "knowledge", createFn: createKnowledge },
    { nodeType: "actor", createFn: createActorNode },
    { nodeType: "execution", createFn: createExecution },
  ])("$nodeType node handles 'get' message", ({ createFn }) => {
    const graph = new Graph();
    const node = createFn({...defaultOptions}, graph);

    const result = graph.send(node.properties.id, "get", {});

    expect(result).toBeDefined();
    expect(result.properties).toBeDefined();
    expect(result.edges).toBeDefined();
  });

  test.each([
    { nodeType: "task", createFn: createTask },
    { nodeType: "knowledge", createFn: createKnowledge },
  ])("$nodeType node handles 'observe' message", ({ createFn }) => {
    const graph = new Graph();
    const node = createFn({...defaultOptions}, graph);

    const result = graph.send(node.properties.id, "observe", {});

    expect(result).toBeDefined();
    expect(result.observations).toBeInstanceOf(Array);
  });

  test.each([
    { nodeType: "task", createFn: createTask },
    { nodeType: "knowledge", createFn: createKnowledge },
  ])("$nodeType node handles 'update' message", ({ createFn }) => {
    const graph = new Graph();
    const node = createFn({...defaultOptions}, graph);

    const result = graph.send(node.properties.id, "update", {
      properties: { /* update data */ }
    });

    expect(result.success).toBe(true);
  });

  test.each([
    { nodeType: "task", createFn: createTask },
    { nodeType: "knowledge", createFn: createKnowledge },
  ])("$nodeType node handles 'link' message", ({ createFn }) => {
    const graph = new Graph();
    const node1 = createFn({...defaultOptions}, graph);
    const node2 = createFn({...defaultOptions}, graph);

    const result = graph.send(node1.properties.id, "link", {
      toId: node2.properties.id,
      edgeType: "test_relation"
    });

    expect(result.success).toBe(true);
    expect(graph.getEdges().length).toBeGreaterThan(0);
  });
});
```

#### 1.3 Edge Management
```typescript
describe("Graph Protocol: Edge Management", () => {
  test("link creates edge between nodes", () => {
    const graph = new Graph();
    const node1 = new MockNode("n1");
    const node2 = new MockNode("n2");
    graph.registerNode(node1);
    graph.registerNode(node2);

    graph.send("n1", "link", { toId: "n2", edgeType: "connects_to" });

    const edges = graph.getEdges();
    expect(edges.some(e => e.fromId === "n1" && e.toId === "n2")).toBe(true);
  });

  test("unlink removes edge", () => {
    const graph = new Graph();
    const node1 = new MockNode("n1");
    const node2 = new MockNode("n2");
    graph.registerNode(node1);
    graph.registerNode(node2);

    graph.send("n1", "link", { toId: "n2", edgeType: "connects_to" });
    const edges = graph.getEdges();
    const edgeId = edges.find(e => e.fromId === "n1")?.id;

    graph.send("n1", "unlink", { edgeId });

    expect(graph.getEdges().length).toBe(0);
  });
});
```

**Success Criteria**:
- ✅ All standard messages work on all node types
- ✅ Message routing verified
- ✅ Edge management tested
- ✅ Error cases handled (unknown nodes, invalid messages)

---

## Layer 2: Domain Protocol Tests

**Purpose**: Test domain-specific message handlers for each node type.

### 2.1 Task Protocol Tests

**File**: `tests/task-protocol.test.ts`

```typescript
import { describe, test, expect } from "bun:test";
import { Graph } from "../src/graph";
import { createTask } from "../src/task";

describe("Task Protocol: Lifecycle", () => {
  test("start transitions from created to active", () => {
    const graph = new Graph();
    const task = createTask({
      goal: "test",
      desiredDeliverables: [],
      objectiveSuccessCriteria: []
    }, graph);

    expect(task.properties.state).toBe("created");

    const result = graph.send(task.properties.id, "start", {});

    expect(result.success).toBe(true);
    expect(task.properties.state).toBe("active");
  });

  test("start fails if not in created state", () => {
    const graph = new Graph();
    const task = createTask({...}, graph);

    graph.send(task.properties.id, "start", {});
    const result2 = graph.send(task.properties.id, "start", {});

    expect(result2.success).toBe(false);
  });

  test("create_task creates child with spawned_by edge", () => {
    const graph = new Graph();
    const parent = createTask({...}, graph);

    const result = graph.send(parent.properties.id, "create_task", {
      goal: "child",
      deliverables: ["test"],
      criteria: []
    });

    expect(result.success).toBe(true);
    expect(result.childTaskId).toBeDefined();

    const edges = graph.getEdges();
    expect(edges.some(e =>
      e.fromId === result.childTaskId &&
      e.toId === parent.properties.id &&
      e.type === "spawned_by"
    )).toBe(true);
  });

  test("eval checks objective criteria", () => {
    const graph = new Graph();
    const task = createTask({
      goal: "test",
      desiredDeliverables: [],
      objectiveSuccessCriteria: [
        { criterion: "Tests pass", measure: "pass_rate", threshold: 0.9, actual: 1.0 }
      ]
    }, graph);

    const result = graph.send(task.properties.id, "eval", {});

    expect(result.passed).toBe(true);
    expect(result.score).toBeGreaterThanOrEqual(0.9);
  });

  test("complete transitions to completed state", () => {
    const graph = new Graph();
    const task = createTask({...}, graph);

    graph.send(task.properties.id, "start", {});
    const result = graph.send(task.properties.id, "complete", {
      result: { deliverables: ["done"] }
    });

    expect(result.success).toBe(true);
    expect(task.properties.state).toBe("completed");
  });
});

describe("Task Protocol: State Machine Validation", () => {
  test("only valid transitions allowed", () => {
    const graph = new Graph();
    const task = createTask({...}, graph);

    // created → completed should fail (must go through active)
    const result = graph.send(task.properties.id, "complete", {});
    expect(result.success).toBe(false);
  });

  test("block transitions to blocked state", () => {
    const graph = new Graph();
    const task = createTask({...}, graph);

    graph.send(task.properties.id, "start", {});
    const result = graph.send(task.properties.id, "block", {
      reason: "dependency not ready"
    });

    expect(result.success).toBe(true);
    expect(task.properties.state).toBe("blocked");
  });
});

describe("Task Protocol: Query Status", () => {
  test("query_status returns task and children status", () => {
    const graph = new Graph();
    const parent = createTask({...}, graph);
    graph.send(parent.properties.id, "start", {});

    const child1Result = graph.send(parent.properties.id, "create_task", {...});
    const child2Result = graph.send(parent.properties.id, "create_task", {...});

    const status = graph.send(parent.properties.id, "query_status", {});

    expect(status.state).toBe("active");
    expect(status.childrenStatus).toHaveLength(2);
    expect(status.childrenStatus[0].id).toBe(child1Result.childTaskId);
  });
});
```

### 2.2 Knowledge Protocol Tests

**File**: `tests/knowledge-protocol.test.ts`

```typescript
import { describe, test, expect } from "bun:test";
import { Graph } from "../src/graph";
import { createKnowledge } from "../src/knowledge";

describe("Knowledge Protocol: Content Operations", () => {
  test("append adds content", () => {
    const graph = new Graph();
    const knowledge = createKnowledge({
      title: "Test",
      content: "Initial content"
    }, graph);

    const result = graph.send(knowledge.properties.id, "append", {
      data: " More content",
      source: "test"
    });

    expect(result.success).toBe(true);
    expect(knowledge.properties.content).toContain("More content");
  });

  test("query searches content", () => {
    const graph = new Graph();
    const knowledge = createKnowledge({
      title: "Erlang",
      content: "Erlang uses supervision trees for fault tolerance"
    }, graph);

    const result = graph.send(knowledge.properties.id, "query", {
      question: "How does Erlang handle faults?"
    });

    expect(result.answer).toBeDefined();
    expect(result.confidence).toBeGreaterThan(0);
  });

  test("synthesize combines multiple knowledge nodes", () => {
    const graph = new Graph();
    const k1 = createKnowledge({ title: "K1", content: "Actor model..." }, graph);
    const k2 = createKnowledge({ title: "K2", content: "Message passing..." }, graph);

    const k3 = createKnowledge({ title: "Combined", content: "" }, graph);
    const result = graph.send(k3.properties.id, "synthesize", {
      fromNodes: [k1.properties.id, k2.properties.id]
    });

    expect(result.success).toBe(true);
    expect(k3.properties.content.length).toBeGreaterThan(0);
  });
});
```

### 2.3 Execution Protocol Tests

**File**: `tests/execution-protocol.test.ts`

```typescript
import { describe, test, expect } from "bun:test";
import { Graph } from "../src/graph";
import { createExecution } from "../src/execution";
import { createTask } from "../src/task";
import { createActorNode } from "../src/actors/actor-node";
import { MockActor } from "../src/actors/mock";

describe("Execution Protocol: Lifecycle", () => {
  test("execution starts in pending state", () => {
    const graph = new Graph();
    const execution = createExecution({}, graph);
    expect(execution.properties.state).toBe("pending");
  });

  test("start transitions to running and delegates to actor", async () => {
    const graph = new Graph();
    const task = createTask({...}, graph);
    const actor = createActorNode(new MockActor("test"), [], graph);
    const execution = createExecution({}, graph);

    graph.send(execution.properties.id, "link", { toId: task.properties.id, edgeType: "executes" });
    graph.send(execution.properties.id, "link", { toId: actor.properties.id, edgeType: "assigned_to" });

    const result = await graph.send(execution.properties.id, "start", {});

    expect(execution.properties.state).toBe("running");
  });

  test("complete transitions to completed", () => {
    const graph = new Graph();
    const execution = createExecution({}, graph);

    const result = graph.send(execution.properties.id, "complete", {
      result: { output: "done" }
    });

    expect(result.success).toBe(true);
    expect(execution.properties.state).toBe("completed");
  });

  test("fail transitions to failed", () => {
    const graph = new Graph();
    const execution = createExecution({}, graph);

    const result = graph.send(execution.properties.id, "fail", {
      error: "Something went wrong"
    });

    expect(result.success).toBe(true);
    expect(execution.properties.state).toBe("failed");
    expect(execution.properties.error).toBe("Something went wrong");
  });
});
```

**Success Criteria**:
- ✅ Each domain protocol's messages tested
- ✅ State transitions validated
- ✅ Error cases covered
- ✅ Complex operations (create_task, synthesize) verified

---

## Layer 3: Actor Lifecycle Tests

**Purpose**: Test actor-specific concerns (death detection, monitoring, heartbeat).

**File**: `tests/actor-lifecycle.test.ts`

```typescript
import { describe, test, expect, mock } from "bun:test";
import { Registry } from "../src/actors/registry";
import { MockActor } from "../src/actors/mock";

describe("Actor Lifecycle: Death Detection", () => {
  test("Registry emits actor_died on exception", async () => {
    const registry = new Registry();
    const actor = new MockActor("test-1");

    // Configure actor to throw next time
    actor.setResponseForNext(new Error("Crash!"));
    registry.register(actor);

    let deathEvent: any;
    registry.on('actor_died', (event) => {
      deathEvent = event;
    });

    const response = await registry.send("test-1", {
      type: "test",
      id: "msg-1",
      payload: {}
    });

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

    // Start heartbeat with short interval
    registry.startHeartbeat("test-2", 100);

    // Make actor hang
    actor.setDelayForNext(200);

    // Wait for heartbeat to detect
    await new Promise(resolve => setTimeout(resolve, 250));

    expect(deathEvent).toBeDefined();
    expect(deathEvent.reason).toBe("heartbeat_timeout");
  });

  test("stopHeartbeat prevents false death detection", () => {
    const registry = new Registry();
    const actor = new MockActor("test-3");
    registry.register(actor);

    registry.startHeartbeat("test-3", 100);
    registry.stopHeartbeat("test-3");

    let deathDetected = false;
    registry.on('actor_died', () => { deathDetected = true; });

    // Wait longer than heartbeat interval
    setTimeout(() => {
      expect(deathDetected).toBe(false);
    }, 200);
  });
});

describe("Actor Lifecycle: Registration", () => {
  test("register adds actor to registry", () => {
    const registry = new Registry();
    const actor = new MockActor("test");

    registry.register(actor);

    expect(registry.has("test")).toBe(true);
    expect(registry.get("test")).toBe(actor);
  });

  test("register throws if actor already registered", () => {
    const registry = new Registry();
    const actor = new MockActor("test");

    registry.register(actor);

    expect(() => registry.register(actor)).toThrow("already registered");
  });

  test("unregister removes actor and stops heartbeat", () => {
    const registry = new Registry();
    const actor = new MockActor("test");
    registry.register(actor);
    registry.startHeartbeat("test", 1000);

    const result = registry.unregister("test");

    expect(result).toBe(true);
    expect(registry.has("test")).toBe(false);
  });
});

describe("Actor Lifecycle: ActorNode Integration", () => {
  test("ActorNode wraps executor and responds to messages", () => {
    const graph = new Graph();
    const executor = new MockActor("test");
    const actorNode = createActorNode(executor, ["code"], graph);

    const result = graph.send(actorNode.properties.id, "ping", {});

    expect(result.alive).toBe(true);
    expect(result.status).toBe("idle");
  });

  test("ActorNode tracks busy status during execution", async () => {
    const graph = new Graph();
    const executor = new MockActor("test");
    const actorNode = createActorNode(executor, ["code"], graph);

    expect(actorNode.properties.status).toBe("idle");

    const promise = graph.send(actorNode.properties.id, "execute", {
      type: "work",
      payload: { /* work */ }
    });

    // Status should be busy during execution
    // (in real implementation, check between promise start and completion)

    await promise;
    expect(actorNode.properties.status).toBe("idle");
  });
});
```

**Success Criteria**:
- ✅ Death detection works (exception and timeout)
- ✅ Registration/unregistration verified
- ✅ ActorNode integration tested
- ✅ Heartbeat lifecycle managed correctly

---

## Integration Tests

**Purpose**: Test interactions across layers.

**File**: `tests/integration.test.ts`

```typescript
describe("Integration: Full Task Execution Flow", () => {
  test("create task, assign to pool, execute, complete", async () => {
    // Setup
    const graph = new Graph();
    const registry = new Registry();

    const pool = createWorkerPool({ name: "workers", capabilities: ["code"] }, graph);
    const actor = new MockActor("worker-1");
    const actorNode = createActorNode(actor, ["code"], graph);
    registry.register(actor);

    graph.send(pool.properties.id, "add_worker", { actorId: actorNode.properties.id });

    // Create task
    const task = createTask({
      goal: "Test task",
      desiredDeliverables: ["output"],
      objectiveSuccessCriteria: [
        { criterion: "Completed", measure: "done", threshold: 1, actual: 1 }
      ]
    }, graph);

    // Assign to pool
    const execution = createExecution({}, graph);
    graph.send(execution.properties.id, "link", { toId: task.properties.id, edgeType: "executes" });

    const workerSelection = graph.send(pool.properties.id, "select_worker", {});
    graph.send(execution.properties.id, "link", { toId: workerSelection.actorId, edgeType: "assigned_to" });

    // Execute
    await graph.send(execution.properties.id, "start", {});

    // Verify execution completed
    expect(execution.properties.state).toBe("completed");
  });

  test("actor death during execution fails execution", async () => {
    // Similar setup...
    // Configure actor to crash
    // Verify execution transitions to failed state
    // Verify death event emitted
  });
});
```

---

## Test Organization

### Directory Structure
```
tests/
├── fixtures/
│   ├── mock-node.ts          # Generic mock node for protocol tests
│   └── test-helpers.ts       # Common test utilities
├── graph-protocol.test.ts    # Layer 1: Primitives
├── task-protocol.test.ts     # Layer 2: Task domain
├── knowledge-protocol.test.ts # Layer 2: Knowledge domain
├── execution-protocol.test.ts # Layer 2: Execution domain
├── actor-lifecycle.test.ts   # Layer 3: Actor concerns
└── integration.test.ts       # Cross-layer integration
```

### Test Execution Order
```bash
# Run in order - each layer depends on previous
bun test tests/graph-protocol.test.ts       # Layer 1 must pass first
bun test tests/task-protocol.test.ts        # Then domain tests
bun test tests/knowledge-protocol.test.ts
bun test tests/execution-protocol.test.ts
bun test tests/actor-lifecycle.test.ts      # Then actor tests
bun test tests/integration.test.ts          # Finally integration
```

Or run all:
```bash
bun test                                     # Runs all tests
```

---

## Agent Implementation Specification

**Input**: This design document + codebase

**Process**:
1. Create test files in tests/ directory
2. Implement mock fixtures as needed
3. Write tests following specifications above
4. Run tests (expect failures initially - implementation needed)
5. Update as implementation progresses

**Output**:
- Complete test suite covering all three layers
- Mock fixtures for testing
- Test helpers for common operations
- Documentation of test organization

**Success Criteria**:
- ✅ All test files created
- ✅ Tests cover specified scenarios
- ✅ Tests are runnable (even if implementation incomplete)
- ✅ Clear layer separation maintained
- ✅ Good test coverage (aim for 80%+)

---

## Metrics & Coverage

**Target Coverage**:
- Layer 1 (Graph Protocol): 100% - it's the foundation
- Layer 2 (Domain Protocols): 90% - cover all message types
- Layer 3 (Actor Lifecycle): 85% - focus on critical paths
- Integration: 70% - key workflows

**Run Coverage**:
```bash
bun test --coverage
```

**Quality Gates**:
- All tests must pass before merging
- No decrease in coverage percentage
- Integration tests must cover happy path + 2 failure modes minimum

---

## Next Steps

1. Agent implements test files
2. Run initial test suite (expect failures)
3. Implement features to make tests pass (TDD approach)
4. Iterate until all tests green
5. Human reviews test quality and coverage
