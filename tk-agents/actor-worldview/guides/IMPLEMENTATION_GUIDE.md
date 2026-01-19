# Actor Model Guide for tk-agents

**Date:** 2026-01-17 (as of timestamp check)
**Purpose:** Design philosophy and evaluation checklist for maintaining actor model consistency
**Status:** Reference Guide

---

## Core Philosophy

The tk-agents system follows a **strict actor model** where:

> **"Everything is a node, edge, or actor"**

This means:
- **No utility classes** for domain logic
- **No direct method calls** between domain components
- **All state** lives in nodes (actor properties) or edges (relationships)
- **All behavior** happens via message passing to actors
- **Graph is the source of truth** for all persistent state

---

## Actor Model Principles

### 1. Hewitt Actor Model Fundamentals

Based on Carl Hewitt's 1973 formalism, actors have three fundamental capabilities:

1. **Receive messages** (via `receive()` method, NOT `send()`)
2. **Create new actors** (via factory pattern)
3. **Send messages** (to other actors via addresses)

**Key Properties:**
- **Isolation:** Actors don't share state
- **Location Transparency:** Communicate via addresses, not direct references
- **Asynchronous:** Message passing is non-blocking
- **Uniform Composition:** Systems are actors containing actors

### 2. Virtual Actor Pattern (Orleans-style)

We use the **virtual actor pattern**:

- **On-demand instantiation:** Actors created when first referenced
- **No explicit lifecycle:** Don't manually "new" actors, just message them
- **Single instance per ID:** Each ID maps to exactly one actor
- **Location transparency:** Don't care where actor lives, just its ID

### 3. Everything is a Node/Edge/Actor

**Nodes are Actors:**
```typescript
// TaskActor creates a node with actor behavior
const address = TaskActor({
  goal: "Implement feature X",
  graph,
  // ...
});
// Node stored in graph with Address for message passing
```

**Edges are Relationships:**
```typescript
// Create edge to represent relationship
graph.addEdge("task_1", "task_2", "blocks", { reason: "dependency" });

// Query edges via graph traversal
const blockers = graph.getEdgesFrom("task_1").filter(e => e.type === "blocks");
```

**Actors Handle Messages:**
```typescript
// Send message to actor via graph
const result = await graph.send("task_1", "update", {
  properties: { state: "in_progress" }
});
```

---

## Actor Design Patterns

### Pattern 1: ActorFactory

**Correct Implementation (TaskActor):**
```typescript
export const TaskActor: ActorFactory<TaskActorData> = (data) => {
  // 1. Create local state
  const id = `task_${++taskCounter}`;
  const properties: TaskProperties = { id, type: "task", /* ... */ };

  // 2. Create actor with message handler
  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      try {
        const result = await handleMessage(message, properties, data.graph);
        return { success: true, data: result };
      } catch (error) {
        return { success: false, error: String(error) };
      }
    },
  };

  // 3. Register with system and get Address
  const address = data.graph.getSystem().register(actor);

  // 4. Register with graph (for persistence and lookup)
  data.graph.registerNode(id, address, properties);

  // 5. Return address
  return address;
};
```

**Key Points:**
- ✅ Returns `Address` (opaque reference)
- ✅ State captured in closure (private)
- ✅ Message handler is pure function
- ✅ Registered with both System (routing) and Graph (persistence)

### Pattern 2: Message Handling

**Correct Message Handler:**
```typescript
async function handleMessage(
  message: ActorMessage,
  properties: TaskProperties,
  graph: Graph
): Promise<unknown> {
  const payload = message.payload as Record<string, unknown>;

  switch (message.type) {
    case "get":
      return handleGet(properties, graph);
    case "update":
      return handleUpdate(payload, properties);
    case "link":
      return handleLink(payload, properties, graph);
    default:
      throw new Error(`Unknown message type: ${message.type}`);
  }
}
```

**Key Points:**
- ✅ Pure function (no side effects except via graph)
- ✅ Properties updated in-place (captured in closure)
- ✅ Returns structured response
- ✅ Type-safe message handling

### Pattern 3: Graph as Coordinator

**Correct Graph Usage:**
```typescript
export class Graph {
  private system: SystemType;
  private nodes: Map<string, Address> = new Map();
  private nodeProperties: Map<string, NodeProperties> = new Map();
  private edges: Map<string, Edge> = new Map();

  async send(nodeId: string, messageType: string, payload: {}): Promise<unknown> {
    const address = this.nodes.get(nodeId);
    if (!address) throw new Error(`Node not found: ${nodeId}`);

    const message: ActorMessage = { id: crypto.randomUUID(), type: messageType, payload };
    const response = await address.send(message);

    if (!response.success) throw new Error(response.error || "Failed");
    return response.data;
  }
}
```

**Key Points:**
- ✅ Graph holds no domain logic
- ✅ Graph routes messages to actors
- ✅ Graph stores structure (nodes/edges)
- ✅ Graph is NOT an actor (it's infrastructure)

---

## Evaluation Checklist

Use this checklist when designing new features or reviewing code:

### ✅ Actor Model Compliance

**1. Is all domain state in nodes/edges?**
- [ ] No utility classes holding state
- [ ] All properties stored as NodeProperties
- [ ] All relationships stored as edges

**2. Is all domain behavior in actors?**
- [ ] No utility methods doing domain logic
- [ ] All behavior via message handlers
- [ ] No direct actor-to-actor calls

**3. Are actors properly isolated?**
- [ ] No shared mutable state
- [ ] State captured in closures
- [ ] Communication via messages only

**4. Are messages used correctly?**
- [ ] All operations via `graph.send()`
- [ ] Message types defined in types.ts
- [ ] Responses are structured (success/error)

**5. Are edges used for relationships?**
- [ ] No implicit relationships (like array fields)
- [ ] Explicit edge types defined
- [ ] Bidirectional queries possible

### ❌ Anti-Patterns to Avoid

**1. Utility Classes for Domain Logic**
```typescript
// ❌ WRONG - AttachmentManager is NOT an actor
export class AttachmentManager {
  attach(taskId: string, file: string) {
    // Domain logic in utility class
  }
}

// ✅ CORRECT - AttachmentActor with message handling
export const AttachmentActor: ActorFactory<AttachmentActorData> = (data) => {
  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      switch (message.type) {
        case "attach":
          return handleAttach(message.payload, properties, data.graph);
        // ...
      }
    }
  };
  return data.graph.getSystem().register(actor);
};
```

**2. Direct Method Calls**
```typescript
// ❌ WRONG - Direct method call
taskActor.updateStatus("complete");

// ✅ CORRECT - Message passing
await graph.send("task_1", "update", { properties: { state: "complete" } });
```

**3. Implicit Relationships**
```typescript
// ❌ WRONG - Attachments as array field
interface TaskProperties {
  attachments: Attachment[];  // Implicit relationship
}

// ✅ CORRECT - Attachments as nodes with edges
// task_1 --[attached_to]--> attachment_1
graph.addEdge("attachment_1", "task_1", "attached_to");
```

**4. Stateful Utility Classes**
```typescript
// ❌ WRONG - Utility class with state
export class EventStream {
  private events: StreamEvent[] = [];  // State!

  add(event: StreamEvent): void {
    this.events.push(event);
  }
}

// ✅ CORRECT - Events as nodes in graph
const EventActor: ActorFactory = (data) => {
  const properties: EventProperties = { /* ... */ };
  // Store events in graph, not in-memory array
};
```

**5. Graph Holding Domain Logic**
```typescript
// ❌ WRONG - Graph has domain-specific methods
export class Graph {
  getTasksWithAttachments(): TaskProperties[] {
    // Domain logic in infrastructure
  }
}

// ✅ CORRECT - Query via graph traversal or Datalog
const tasksWithAttachments = graph.getNodeIds()
  .filter(id => graph.getEdgesFrom(id).some(e => e.type === "attached_to"));
```

---

## Planning Guidelines

### When to Create a New Actor Type

**Create a new actor when:**
1. **New domain entity** (e.g., Task, Knowledge, Attachment)
2. **Stateful component** with lifecycle
3. **Independent behavior** (not just data transformation)
4. **Long-lived** (persisted across sessions)

**Don't create an actor for:**
1. Pure functions (helpers, formatters)
2. Data structures (DTOs, interfaces)
3. Infrastructure (Graph, System, EventLog)
4. Temporary computation

### Example: Attachment Feature Design

**❌ WRONG Approach (Utility Class):**
```typescript
// blob-manager.ts
export class AttachmentManager {
  constructor(private graph: Graph) {}

  attach(taskId: string, file: string): Attachment {
    // Violates actor model - domain logic in utility class
  }
}
```

**✅ CORRECT Approach (Actor-based):**
```typescript
// attachment.ts
export const AttachmentActor: ActorFactory<AttachmentActorData> = (data) => {
  const id = `attachment_${++counter}`;
  const properties: AttachmentProperties = {
    id,
    type: "attachment",
    filename: data.filename,
    hash: data.hash,
    // ...
  };

  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      switch (message.type) {
        case "validate":
          return handleValidate(properties);
        case "read":
          return handleRead(properties);
        default:
          throw new Error(`Unknown message: ${message.type}`);
      }
    }
  };

  const address = data.graph.getSystem().register(actor);
  data.graph.registerNode(id, address, properties);

  // Create edge: attachment -> task
  data.graph.addEdge(id, data.taskId, "attached_to");

  return address;
};

// Usage
const attachment = AttachmentActor({
  filename: "report.md",
  hash: "sha256:...",
  taskId: "task_28",
  graph,
});

// Validate via message
await graph.send(attachmentId, "validate", {});
```

---

## Code Examples

### Good: Actor-based Task with Dependencies

```typescript
// Create tasks as actors
const designTask = TaskActor({
  goal: "Design auth system",
  graph,
  // ...
});

const implTask = TaskActor({
  goal: "Implement auth",
  graph,
  // ...
});

// Create edge: impl blocks on design
graph.addEdge("task_impl", "task_design", "blocks");

// Query blockers via graph
const blockers = graph.getEdgesFrom("task_impl")
  .filter(e => e.type === "blocks")
  .map(e => e.toId);

// Update via message
await graph.send("task_design", "update", {
  properties: { state: "complete" }
});
```

### Bad: Utility Class with State

```typescript
// ❌ DON'T DO THIS
export class TaskManager {
  private tasks: Map<string, TaskData> = new Map();

  createTask(goal: string): string {
    const id = `task_${this.tasks.size + 1}`;
    this.tasks.set(id, { goal, state: "created" });
    return id;
  }

  updateTask(id: string, updates: Partial<TaskData>): void {
    const task = this.tasks.get(id);
    if (task) {
      Object.assign(task, updates);
    }
  }
}
```

**Problems:**
- State in utility class (not graph)
- Direct method calls (not messages)
- No persistence (in-memory only)
- Not queryable via graph traversal

---

## Migration Strategy

If you find code violating the actor model:

### 1. Identify Violations
- Utility classes with domain state
- Direct method calls between domain components
- Implicit relationships (array/object fields)

### 2. Design Actor-based Alternative
- Define NodeProperties for state
- Define message types for behavior
- Define edge types for relationships

### 3. Implement ActorFactory
- Create actor factory
- Implement message handlers
- Register with graph

### 4. Update Callers
- Replace direct calls with `graph.send()`
- Replace property access with "get" messages
- Add edges for relationships

### 5. Remove Old Code
- Delete utility class
- Remove implicit relationship fields
- Clean up imports

---

## Testing Actor Implementations

### Unit Test Pattern

```typescript
import { test, expect } from "bun:test";
import { Graph } from "./graph.ts";
import { TaskActor } from "./task.ts";

test("task actor handles update message", async () => {
  const graph = new Graph();

  const address = TaskActor({
    goal: "Test task",
    desiredDeliverables: [],
    objectiveSuccessCriteria: [],
    graph,
  });

  // Get task ID from graph
  const taskId = graph.getNodeIds()[0];

  // Send update message
  const result = await graph.send(taskId, "update", {
    properties: { state: "in_progress" }
  });

  // Verify state changed
  const props = graph.getNodeProperties(taskId) as TaskProperties;
  expect(props.state).toBe("in_progress");
});
```

### Integration Test Pattern

```typescript
test("task dependency blocking", async () => {
  const graph = new Graph();

  // Create two tasks
  const design = TaskActor({ goal: "Design", graph, /* ... */ });
  const impl = TaskActor({ goal: "Implement", graph, /* ... */ });

  const designId = graph.getNodeIds()[0];
  const implId = graph.getNodeIds()[1];

  // Create blocking edge
  graph.addEdge(implId, designId, "blocks");

  // Verify impl is blocked
  const status = await graph.send(implId, "status", {}) as StatusResponse;
  expect(status.blocked).toBe(true);
  expect(status.blockers).toContain(designId);

  // Complete design
  await graph.send(designId, "update", { properties: { state: "complete" } });

  // Verify impl is unblocked
  const newStatus = await graph.send(implId, "status", {}) as StatusResponse;
  expect(newStatus.blocked).toBe(false);
});
```

---

## Quick Reference

### ✅ DO

- Store all state in nodes (NodeProperties)
- Store all relationships as edges
- Communicate via `graph.send()`
- Create actors via ActorFactory
- Use message handlers for behavior
- Query via graph traversal

### ❌ DON'T

- Create utility classes for domain logic
- Store state outside graph
- Call methods directly between actors
- Use implicit relationships (arrays/objects)
- Bypass message passing
- Mix infrastructure with domain logic

### Message Types Convention

```typescript
// Standard messages (all actors)
"get"         // Get full state
"observe"     // Get observable state
"update"      // Update properties
"link"        // Create edge to another node
"unlink"      // Remove edge
"delete"      // Delete node

// Domain-specific messages (per actor type)
"evaluate"    // Task: evaluate completion
"validate"    // Attachment: check integrity
"synthesize"  // Knowledge: synthesize from sources
```

---

## References

- **Hewitt Actor Model ADR:** [docs/decisions/HEWITT_ACTOR_MODEL.md](/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/docs/decisions/HEWITT_ACTOR_MODEL.md)
- **Graph Actor System:** [docs/research/GRAPH_ACTOR_SYSTEM.md](/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/docs/research/GRAPH_ACTOR_SYSTEM.md)
- **Current Implementations:** [src/task.ts](/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/task.ts), [src/knowledge.ts](/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/knowledge.ts)
- **Actor System Spec:** [src/actors/ACTOR_SYSTEM.spec.md](/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/actors/ACTOR_SYSTEM.spec.md)

---

## Conclusion

The actor model provides:

1. **Uniform architecture** - Everything is a node/edge/actor
2. **Clear boundaries** - State in graph, behavior in actors
3. **Location transparency** - Communicate via addresses
4. **Testability** - Mock messages, not implementation
5. **Persistence** - Serialize graph, not in-memory objects
6. **Queryability** - Traverse graph, don't scan objects

**When in doubt, ask:**
> "Is this a node, edge, or actor? Or should it be a pure function?"
