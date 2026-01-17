# Graph System Specification

## Overview

The Graph System is a central store and message router that bridges string-based external interfaces with the actor-based internal messaging system. It manages nodes (actors) and edges, providing bidirectional mapping between human-readable string IDs (for serialization, CLI, persistence) and Address objects (for type-safe internal messaging).

### Core Responsibilities

1. **Node Management** - Register, retrieve, and remove nodes with bidirectional ID mapping
2. **Edge Management** - Create and manage typed relationships between nodes
3. **Message Routing** - Route messages by string ID through underlying Address proxies
4. **Property Caching** - Maintain quick-access cache of node properties
5. **Serialization** - Support dump/restore for persistence

## Core Concepts

### Dual Identity Model

Every node in the graph has two identities:

```
String ID (external)  <-->  Address (internal)
"task-1"             <-->  { __id: Symbol(), send: fn }
```

**Why Two Identities?**
- **String IDs**: Serializable, human-readable, suitable for CLI/REST/persistence
- **Addresses**: Type-safe, unforgeable, carry built-in messaging behavior

### Layered Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    External API                         │
│         (String IDs for CLI, REST, Persistence)         │
├─────────────────────────────────────────────────────────┤
│                    Graph Layer                          │
│    (Bidirectional mapping: String ID <--> Address)      │
├─────────────────────────────────────────────────────────┤
│                   Actor System                          │
│         (Address proxies, message routing)              │
└─────────────────────────────────────────────────────────┘
```

## Core Features

### 1. Node Registration

Register an actor with a string ID for external reference:

```typescript
// Create actor via factory (returns Address)
const taskAddress = TaskActor({
  id: "task-1",
  status: "todo",
  system: graph.getSystem()
});

// Register in graph with string ID
graph.registerNode("task-1", taskAddress, {
  id: "task-1",
  type: "task",
  state: "created",
  createdAt: new Date()
});
```

**Properties:**
- String ID must be unique within the graph
- Properties are cached for quick access without messaging
- Address enables type-safe internal communication

### 2. Message Sending

Send messages to nodes by string ID:

```typescript
// Send message by string ID (external API)
const result = await graph.send("task-1", "start", { context: {} });

// Internally converts to Address-based send:
// 1. Look up Address for "task-1"
// 2. Call address.send(message)
// 3. Return response
```

**Message Structure:**
```typescript
{
  id: string;      // Auto-generated UUID
  type: string;    // Message type
  payload: {};     // Message payload
}
```

### 3. Edge Management

Create typed relationships between nodes:

```typescript
// Add edge: task-2 depends on task-1
const edge = graph.addEdge("task-2", "task-1", "depends_on", {
  reason: "Must complete task-1 first"
});

// Query edges
const dependencies = graph.getEdgesFrom("task-2");
const dependents = graph.getEdgesTo("task-1");
const allConnected = graph.getAllEdges("task-1");

// Remove edge
graph.removeEdge(edge.id);
```

**Edge Types:**
- `depends_on` - Task dependency relationship
- `requires_knowledge` - Knowledge requirement
- `produces` - Task produces artifact
- `spawned_by` - Parent-child task relationship
- `blocks` - Blocking relationship
- `references` - General reference

### 4. Property Caching

Access node properties without message overhead:

```typescript
// Get cached properties (fast, no actor message)
const props = graph.getNodeProperties("task-1");
console.log(props.state); // "created"

// Get all node IDs
const allIds = graph.getNodeIds();
```

**Note:** Properties are a cache. For authoritative state, send a `get` message to the actor.

### 5. Node Removal

Remove a node and all its edges:

```typescript
// Removes node and all connected edges
const removed = graph.removeNode("task-1");
// Returns: true if node existed, false otherwise
```

**Cascade behavior:**
- All edges where `fromId === nodeId` are removed
- All edges where `toId === nodeId` are removed
- Node properties are removed from cache
- Actor remains in underlying System (no automatic cleanup)

### 6. Serialization Support

Dump graph state for persistence:

```typescript
// Export current state
const snapshot = graph.dump();
// Returns: { nodes: NodeProperties[], edges: Edge[] }

// Restore requires factory recreation
// (Addresses are runtime-only, cannot serialize symbols)
```

**Serialization Limitations:**
- Addresses (symbols) cannot be serialized
- Restore requires re-creating actors via factories
- Use `setEdgeCounter(n)` to restore edge ID sequence

## Usage Examples

### Example 1: Task Graph Setup

```typescript
const graph = new Graph();
const system = graph.getSystem();

// Create and register root task
const rootAddr = TaskActor({
  id: "root",
  goal: "Complete project",
  state: "created",
  system
});
graph.registerNode("root", rootAddr, {
  id: "root",
  type: "task",
  goal: "Complete project",
  state: "created",
  createdAt: new Date()
});

// Create subtask
const subtaskAddr = TaskActor({
  id: "subtask-1",
  goal: "Write tests",
  state: "created",
  system
});
graph.registerNode("subtask-1", subtaskAddr, {
  id: "subtask-1",
  type: "task",
  goal: "Write tests",
  state: "created",
  createdAt: new Date()
});

// Create spawned_by relationship
graph.addEdge("subtask-1", "root", "spawned_by");
```

### Example 2: Message Orchestration

```typescript
// Start a task
await graph.send("subtask-1", "start", { context: { priority: "high" } });

// Check status
const status = await graph.send("subtask-1", "query_status", {});

// Complete task with result
await graph.send("subtask-1", "complete", {
  result: { testsWritten: 5, coverage: 85 }
});
```

### Example 3: Graph Traversal

```typescript
// Find all child tasks
function getChildTasks(graph: Graph, taskId: string): string[] {
  return graph.getEdgesTo(taskId)
    .filter(edge => edge.type === "spawned_by")
    .map(edge => edge.fromId);
}

// Built-in helper for common case
const children = graph.getChildTasks("root");

// Find task dependencies
function getDependencies(graph: Graph, taskId: string): string[] {
  return graph.getEdgesFrom(taskId)
    .filter(edge => edge.type === "depends_on")
    .map(edge => edge.toId);
}
```

### Example 4: Persistence Workflow

```typescript
// Save to file
const snapshot = graph.dump();
await Bun.write("graph.json", JSON.stringify(snapshot, null, 2));

// Restore from file (requires factory function)
async function restoreGraph(
  factoryFn: (props: NodeProperties, system: System) => Address
): Promise<Graph> {
  const data = await Bun.file("graph.json").json();
  const graph = new Graph();
  const system = graph.getSystem();

  // Recreate nodes
  for (const props of data.nodes) {
    const address = factoryFn(props, system);
    graph.registerNode(props.id, address, props);
  }

  // Restore edges
  let maxEdgeId = 0;
  for (const edge of data.edges) {
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
    const edgeNum = parseInt(edge.id.replace("edge_", ""));
    maxEdgeId = Math.max(maxEdgeId, edgeNum);
  }
  graph.setEdgeCounter(maxEdgeId);

  return graph;
}
```

## Integration Points

### Actor System Integration

The Graph wraps an Actor System and delegates message routing:

```typescript
class Graph {
  private system: System;

  constructor() {
    this.system = System(); // Create underlying actor system
  }

  getSystem(): System {
    return this.system; // Expose for factory creation
  }
}
```

**Key Integration:**
- Graph creates and owns the System
- Factories need System reference for actor registration
- All messaging goes through System's send implementation

### Node Type Integration

Graph supports multiple node types defined in types.ts:

| Node Type | Purpose | Properties |
|-----------|---------|------------|
| `task` | Executable work units | goal, state, deliverables, criteria |
| `knowledge` | Information store | title, content, sources |
| `artifact` | Produced outputs | TBD |
| `pattern` | Reusable patterns | TBD |

### Edge Type Semantics

| Edge Type | From | To | Meaning |
|-----------|------|-----|---------|
| `depends_on` | task | task | Source needs target complete first |
| `requires_knowledge` | task | knowledge | Task needs knowledge to proceed |
| `produces` | task | artifact | Task produces artifact |
| `spawned_by` | task | task | Child task created by parent |
| `blocks` | task | task | Source is blocking target |
| `references` | any | any | General reference relationship |

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| `registerNode` | O(1) | Map insertion |
| `getNode` | O(1) | Map lookup |
| `getNodeProperties` | O(1) | Map lookup |
| `getNodeIds` | O(n) | Array.from(keys()) |
| `send` | O(1) + actor | Lookup + actor processing |
| `addEdge` | O(1) | Map insertion |
| `removeEdge` | O(1) | Map deletion |
| `getEdgesFrom/To` | O(e) | Scan all edges |
| `removeNode` | O(e) | Scan all edges for cleanup |
| `dump` | O(n + e) | Copy all data |

Where n = number of nodes, e = number of edges.

### Memory Usage

- Node mapping: ~100 bytes per node (ID + Address + properties reference)
- Edge storage: ~50 bytes per edge (fixed fields)
- Property cache: Depends on property object size

### Scalability Considerations

- Edge queries scale linearly with total edges
- Consider indexing for large graphs (not currently implemented)
- Dump/restore is synchronous (may block for large graphs)

## Error Handling

### Node Not Found

```typescript
try {
  await graph.send("nonexistent", "get", {});
} catch (error) {
  // Error: "Node not found: nonexistent"
}
```

### Actor Message Failure

```typescript
const result = await graph.send("task-1", "invalid-type", {});
// Throws if response.success === false
// Error message from response.error
```

### Edge to Missing Node

Edges can reference non-existent nodes (no validation):

```typescript
// This succeeds even if "missing-node" doesn't exist
graph.addEdge("task-1", "missing-node", "depends_on");
// Caller responsible for validation
```

## State Management

### Graph State

The Graph maintains three internal state structures:

```typescript
{
  nodes: Map<string, Address>,       // ID -> Address mapping
  nodeProperties: Map<string, NodeProperties>,  // ID -> Properties cache
  edges: Map<string, Edge>,          // EdgeID -> Edge mapping
  edgeCounter: number                // For generating edge IDs
}
```

### State Invariants

1. **Bidirectional Consistency**: If `nodes.has(id)` then `nodeProperties.has(id)`
2. **Edge Counter Monotonicity**: `edgeCounter` never decreases
3. **Edge ID Uniqueness**: All edge IDs are unique (guaranteed by counter)

### Actor Lifecycle

- Graph does not manage actor lifecycle
- Removing a node from graph does not stop the actor
- Actor continues in System until garbage collected
- No automatic supervision or restart

## Message Interface

### Standard Messages (All Node Types)

| Message Type | Payload | Response | Purpose |
|--------------|---------|----------|---------|
| `get` | `{}` | `GetResponse` | Get node state and edges |
| `observe` | `{}` | `ObserveResponse` | Get observations |
| `update` | `{ properties }` | `{ success }` | Update properties |
| `link` | `{ toId, edgeType }` | `Edge` | Create edge |
| `unlink` | `{ edgeId }` | `{ success }` | Remove edge |
| `delete` | `{}` | `{ success }` | Delete node |

### Task Messages

| Message Type | Payload | Response | Purpose |
|--------------|---------|----------|---------|
| `start` | `{ context? }` | `{ success }` | Start task execution |
| `create_task` | `{ goal, deliverables, criteria }` | `{ taskId }` | Create subtask |
| `eval` | `{}` | `EvalResponse` | Evaluate success criteria |
| `complete` | `{ result, artifacts? }` | `{ success }` | Mark complete |
| `block` | `{ reason, requiredKnowledge? }` | `{ success }` | Block task |
| `query_status` | `{}` | `StatusResponse` | Get status |

### Knowledge Messages

| Message Type | Payload | Response | Purpose |
|--------------|---------|----------|---------|
| `append` | `{ data, source? }` | `{ success }` | Add content |
| `query` | `{ question, context? }` | `{ answer }` | Query knowledge |
| `synthesize` | `{ fromNodes }` | `{ synthesis }` | Combine knowledge |

## Invariants

### 1. Node ID Uniqueness

```
forall n1, n2 in graph.nodes:
  n1.id == n2.id implies n1 === n2
```

### 2. Bidirectional Mapping Consistency

```
forall id in graph.nodes.keys():
  graph.nodeProperties.has(id) == true
```

### 3. Edge ID Uniqueness

```
forall e1, e2 in graph.edges:
  e1.id == e2.id implies e1 === e2
```

### 4. Message Routing Equivalence

```
forall nodeId, message:
  graph.send(nodeId, msg.type, msg.payload) ===
    graph.getNode(nodeId).send(msg)
```

### 5. Cascade Deletion

```
after graph.removeNode(id):
  forall e in graph.edges:
    e.fromId != id AND e.toId != id
```

### 6. Edge Counter Progression

```
forall edges e1, e2:
  e1 created before e2 implies
    parseInt(e1.id.split("_")[1]) < parseInt(e2.id.split("_")[1])
```

## Non-Requirements

What this spec **does NOT** require:

- Transactions or atomic operations
- Edge validation (dangling references allowed)
- Actor lifecycle management
- Distributed operation
- Query language or indexing
- Event sourcing or history
- Concurrent modification safety

## Summary

**Key insights:**

1. **Dual Identity Model** - String IDs for external, Addresses for internal
2. **Actor System Bridge** - Graph bridges serializable world with actor world
3. **Property Caching** - Fast access without message overhead
4. **Edge Types** - Semantic relationships between nodes
5. **Serialization Support** - Dump/restore with factory recreation

The Graph System provides the essential coordination layer between human-readable external interfaces and the type-safe actor system.
