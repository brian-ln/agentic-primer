# Actor Architecture Clarification

## Current System: Graph Nodes ARE Actors

Your graph nodes (tasks, knowledge) **already use the actor pattern** for state management.

### Message Flow

```
CLI Command
    ↓
bun src/cli/task.ts update task_1 complete
    ↓
graph.send("task_1", "complete", { result: "Done" })
    ↓
Graph looks up Address for "task_1"
    ↓
address.send({ type: "complete", payload: { result: "Done" }})
    ↓
TaskActor message handler receives message
    ↓
handleComplete() updates task properties
    ↓
New state: task_1.state = "completed"
```

### Code Evidence

**From src/cli/task.ts:416** (CLI update command):
```typescript
result = await graph.send(id, "complete", {
  result: args[0] || "Task completed"
});
```

**From src/graph.ts:45** (Graph routes to actor):
```typescript
async send(nodeId: string, messageType: string, payload = {}) {
  const address = this.nodes.get(nodeId);
  const message = { type: messageType, payload };
  const response = await address.send(message);
  return response.data;
}
```

**From src/task.ts:60** (Actor message handler):
```typescript
const actor = {
  send: async (message: ActorMessage): Promise<Response> => {
    const result = await handleMessage(message, properties, graph);
    return { success: true, data: result };
  },
};
```

### Supported Messages (Task Nodes)

**Standard actor messages:**
- `get` - Read node properties
- `observe` - Subscribe to changes
- `update` - Update properties
- `link` - Create edge to another node
- `unlink` - Remove edge
- `delete` - Remove node

**Task-specific messages:**
- `start` - Begin task (created → active)
- `complete` - Finish task (active → completed)
- `block` - Block task (any → blocked)
- `create_task` - Spawn child task
- `eval` - Evaluate success criteria
- `query_status` - Get detailed status

## Actor POC: Different Purpose

The **Actor POC's AgentActor** is for a **different use case**: monitoring background agents.

### Two Different Actor Types

| Actor Type | Purpose | Messages |
|------------|---------|----------|
| **TaskActor** (existing) | Manage task state in graph | start, complete, block, update |
| **AgentActor** (POC) | Monitor background agents | status, tail, stop, query, ping |

### AgentActor Use Case

**AgentActor wraps background agent processes:**

```typescript
// Monitor a running background agent
const agent = new AgentActor({
  id: "a31e6f6",
  session_id: "session-123",
  command: "Research CozoDB patterns",
  status: "running",
  output_path: "/tmp/claude/.../a31e6f6.output",
});

// Send messages to monitor agent
const statusResp = await agent.send({ type: "status" });
// Returns: { type: "status", status: "running", data: Agent }

const tailResp = await agent.send({ type: "tail", lines: 50 });
// Returns: { type: "tail", lines: ["...", "..."] }

const stopResp = await agent.send({ type: "stop" });
// Returns: { type: "stop", success: true }
```

**TaskActor manages graph nodes:**

```typescript
// Update a task node via messaging
await graph.send("task_1", "complete", { result: "Done" });

// Query task status via messaging
const status = await graph.send("task_1", "query_status", {});

// Create subtask via messaging
const subtask = await graph.send("task_1", "create_task", {
  goal: "Subtask goal",
  deliverables: ["Output"],
  criteria: [{ criterion: "Done", measure: "Boolean", threshold: true }],
});
```

## Architecture Diagram

```
┌──────────────────────────────────────────────────────────────┐
│ Graph Nodes (State Management via Message-Passing)          │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  TaskActor (task nodes)                                      │
│  ├── send({ type: "start" })      → state: created→active   │
│  ├── send({ type: "complete" })   → state: active→completed │
│  ├── send({ type: "block" })      → state: any→blocked      │
│  └── send({ type: "update" })     → properties updated      │
│                                                               │
│  KnowledgeActor (knowledge nodes)                            │
│  ├── send({ type: "append" })     → content += new          │
│  ├── send({ type: "query" })      → search content          │
│  └── send({ type: "synthesize" }) → combine sources         │
│                                                               │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│ Background Agents (Process Monitoring via Message-Passing)  │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  AgentActor (monitors background agents)                     │
│  ├── send({ type: "status" })     → get agent metadata      │
│  ├── send({ type: "tail" })       → stream output lines     │
│  ├── send({ type: "stop" })       → request termination     │
│  ├── send({ type: "query" })      → query agent state       │
│  └── send({ type: "ping" })       → health check            │
│                                                               │
└──────────────────────────────────────────────────────────────┘

Both use message-passing, different domains
```

## State Management: Message-Passing Only

**✅ YES - All state changes go through message-passing:**

```typescript
// Update task state
await graph.send("task_1", "complete", { result: "Done" });

// NOT this (direct mutation forbidden):
// task.state = "completed";  // ❌ NO DIRECT ACCESS
```

**Why message-passing:**
1. **Encapsulation** - Task state is private to actor
2. **Validation** - Message handler validates transitions
3. **History** - All changes are messages (audit trail)
4. **Concurrency** - Messages can be queued/serialized
5. **Location transparency** - Actor could be local or remote

## Integration Plan

### Current State (Already Works)

**Graph nodes use actors:**
```typescript
// CLI already uses message-passing
await graph.send("task_1", "complete", { result: "Done" });

// Properties are encapsulated in actor
const actor = TaskActor({
  goal: "My task",
  desiredDeliverables: ["Output"],
  objectiveSuccessCriteria: [{ ... }],
  graph,
});

// State updates only via messages
await actor.send({ type: "complete", payload: { result: "Done" }});
```

### Actor POC Addition (New)

**Monitor background agents:**
```typescript
// Create AgentActor for monitoring
const agent = new AgentActor({
  id: "a31e6f6",
  session_id: "session-123",
  command: "Research task",
  status: "running",
  output_path: "/tmp/...",
});

// Monitor via messages
const status = await agent.send({ type: "status" });
const output = await agent.send({ type: "tail", lines: 100 });
```

**AgentActorSystem for coordination:**
```typescript
// Registry of all agent actors
const system = new AgentActorSystem();

// Send to specific agent
const resp = await system.send("a31e6f6", { type: "status" });

// Broadcast to all agents
const results = await system.broadcast({ type: "ping" });
```

## No Architecture Change Required

**Your existing system already uses message-passing for graph nodes.**

**The Actor POC adds:**
- ✅ AgentActor for monitoring background agents
- ✅ AgentActorSystem for coordination
- ✅ Event streaming for file watching

**It does NOT change:**
- ❌ TaskActor (already works perfectly)
- ❌ KnowledgeActor (already works perfectly)
- ❌ Graph message routing (already works)

## Example: Complete Update Flow

### CLI Update (Current)

```bash
bun src/cli/task.ts update task_1 complete "All done"
```

**Execution:**
```typescript
// 1. CLI parses command
const taskId = "task_1";
const action = "complete";
const result = "All done";

// 2. CLI sends message to graph
const response = await graph.send(taskId, action, { result });

// 3. Graph routes to actor
const address = graph.nodes.get(taskId);
const message = { type: action, payload: { result }};
const actorResponse = await address.send(message);

// 4. Actor handles message
async function handleComplete(payload, properties, graph) {
  // Validate transition
  if (properties.state !== "active") {
    throw new Error("Can only complete active tasks");
  }

  // Update state (private to actor)
  properties.state = "completed";
  properties.completedAt = new Date();
  properties.result = payload.result;

  // Persist to file
  saveGraph(graph, TASKS_FILE);

  return { finalState: "completed" };
}

// 5. Response returned to CLI
console.log("Completed task task_1:", response);
```

**Key points:**
- ✅ State updates only via messages
- ✅ Actor validates transitions
- ✅ Properties encapsulated in actor
- ✅ No direct property access
- ✅ Audit trail via messages

## Summary

**Question:** Are changes to graph nodes sent as messages to the node actors?

**Answer:** ✅ **YES, absolutely.**

Your system already uses proper actor-based message-passing for all graph node state changes. The Actor POC adds agent monitoring capabilities (separate concern), not a replacement for existing actors.

**No changes needed to current architecture.** It's already actor-based.

## Verification

Check any CLI command - they all use `graph.send()`:

```bash
# All of these use message-passing internally:
bun src/cli/task.ts update task_1 start
bun src/cli/task.ts update task_1 complete
bun src/cli/task.ts update task_1 block "Reason"

# Implemented as:
graph.send("task_1", "start", {})
graph.send("task_1", "complete", { result: "..." })
graph.send("task_1", "block", { reason: "..." })
```

**Your architecture is solid. Message-passing all the way down.**
