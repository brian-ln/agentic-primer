# Combined System Specification

## Overview

The Combined System integrates four subsystems into a cohesive task and knowledge management platform:

1. **Actor System** - Foundation layer providing message-passing infrastructure
2. **Graph System** - Coordination layer bridging string IDs with actor addresses
3. **Task System** - Domain layer for hierarchical task management
4. **Knowledge System** - Domain layer for information storage and retrieval
5. **Task CLI** - Interface layer for human interaction

Together, these systems form a complete architecture for managing goal-oriented work with knowledge dependencies.

## System Architecture

### Layered Architecture

```
+------------------------------------------------------------------+
|                        Interface Layer                            |
|                         (Task CLI)                                |
|  - Command parsing and validation                                 |
|  - Display formatting                                             |
|  - File persistence (tasks.json)                                  |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                      Coordination Layer                           |
|                       (Graph System)                              |
|  - String ID <-> Address bidirectional mapping                    |
|  - Edge management                                                |
|  - Property caching                                               |
|  - Serialization support                                          |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                        Domain Layer                               |
|              (Task System + Knowledge System)                     |
|  - Task: Hierarchical work units with state machine               |
|  - Knowledge: Information storage with versioning                 |
+------------------------------------------------------------------+
                              |
                              v
+------------------------------------------------------------------+
|                      Foundation Layer                             |
|                       (Actor System)                              |
|  - Address-based messaging                                        |
|  - Actor registration                                             |
|  - Message routing                                                |
+------------------------------------------------------------------+
```

### Component Relationships

```
             +----------+
             | Task CLI |
             +----+-----+
                  |
                  | load/save (tasks.json)
                  | commands -> Graph operations
                  v
             +----------+
             |  Graph   |<---- owns ---> +----------+
             | System   |                |  System  |
             +----+-----+                | (Actors) |
                  |                      +----+-----+
                  | registerNode               |
                  | addEdge                    | register
                  | send                       | send
                  v                            v
         +-------+-------+            +-------+-------+
         |               |            |               |
    +----+----+     +----+----+  +----+----+     +----+----+
    | Task    |     |Knowledge|  |  Task   |     |Knowledge|
    |  Node   |     |  Node   |  |  Actor  |     |  Actor  |
    +----+----+     +----+----+  +---------+     +---------+
         |               |            ^               ^
         +-------+-------+            |               |
                 |                    |               |
        (string IDs for CLI)   (Addresses for messaging)
```

## Integration Points

### 1. Graph Wraps Actor System

The Graph creates and owns the underlying Actor System:

```typescript
class Graph {
  private system: System;

  constructor() {
    this.system = System();  // Create actor system
  }

  getSystem(): System {
    return this.system;      // Expose for factories
  }
}
```

**Integration Contract:**
- Graph creates System at construction
- System reference exposed via `getSystem()`
- All actor registration goes through System
- All messaging routed through System

### 2. Task/Knowledge Nodes Use Graph

Domain actors integrate with Graph for dual registration:

```typescript
function TaskActor(data: { graph: Graph }): Address {
  const system = data.graph.getSystem();

  // Create actor with message handler
  const actor = {
    send: async (message: Message) => { /* ... */ }
  };

  // Register with System (get Address)
  const address = system.register(actor);

  // Register with Graph (string ID mapping)
  data.graph.registerNode(id, address, properties);

  return address;
}
```

**Dual Registration:**
1. System registration: Actor -> Address (for messaging)
2. Graph registration: String ID -> Address (for serialization/CLI)

### 3. CLI Uses Graph for Task Management

The CLI operates through the Graph layer:

```typescript
// CLI command execution
async function executeAdd(goal: string, options: Options) {
  const graph = await loadGraph();  // Load from tasks.json

  // Create task via factory
  const taskAddr = TaskActor({
    goal,
    deliverables: options.deliverables,
    graph
  });

  // Create edges if dependencies specified
  if (options.depends) {
    for (const depId of options.depends) {
      graph.addEdge(newTaskId, depId, "depends_on");
    }
  }

  // Persist changes
  await saveGraph(graph);
}
```

**CLI -> Graph Operations:**
| CLI Command | Graph Operations |
|-------------|------------------|
| `add` | `TaskActor()`, `registerNode()`, `addEdge()` |
| `update` | `send(id, action)` |
| `delete` | `removeNode()` |
| `list` | `getNodeIds()`, `getNodeProperties()` |
| `show` | `send(id, 'get')` |
| `graph` | `getEdgesFrom()`, recursive traversal |

## Data Flow

### CLI Load/Save Cycle

```
+------------------+     load      +------------------+
|   tasks.json     | ------------> |   Graph Instance |
| (nodes + edges)  |               |  (in-memory)     |
+------------------+               +------------------+
        ^                                   |
        |                                   |
        |  save (dump)                      v
        |                          +------------------+
        +------------------------- | Modified Graph   |
                                   +------------------+
```

**Load Process:**
1. Read `tasks.json` from file system
2. Parse JSON into nodes and edges
3. For each node: recreate actor via factory, register in Graph
4. For each edge: add to Graph edge store
5. Set edge counter to max edge ID

**Save Process:**
1. Call `graph.dump()` to get current state
2. Serialize to JSON with ISO 8601 dates
3. Write to `tasks.json`

### Task Spawning Flow

```
Parent Task                       Graph                        Child Task
     |                              |                               |
     | create_task message          |                               |
     |----------------------------->|                               |
     |                              |                               |
     |                              | TaskActor(...)                |
     |                              |------------------------------>|
     |                              |                               |
     |                              | registerNode(childId, addr)   |
     |                              |<------------------------------|
     |                              |                               |
     |                              | addEdge(childId, parentId,    |
     |                              |         "spawned_by")         |
     |                              |------------------------------>|
     |                              |                               |
     |  { childTaskId: "task_N" }   |                               |
     |<-----------------------------|                               |
```

### Knowledge Linking Flow

```
Task                              Graph                      Knowledge
  |                                 |                             |
  | link message                    |                             |
  | {toId: "knowledge_1",           |                             |
  |  edgeType: "requires_knowledge"}|                             |
  |-------------------------------->|                             |
  |                                 |                             |
  |                                 | addEdge(taskId, knowledgeId,|
  |                                 |         "requires_knowledge")|
  |                                 |---------------------------->|
  |                                 |                             |
  | { edgeId: "edge_N" }            |                             |
  |<--------------------------------|                             |
```

## Message Routing

### CLI to Task Actor

```
CLI Command                         Graph                      TaskActor
     |                                |                             |
     | graph.send("task_1",           |                             |
     |            "start", {})        |                             |
     |------------------------------->|                             |
     |                                |                             |
     |                    Look up: nodes.get("task_1") -> Address   |
     |                                |                             |
     |                                | address.send({              |
     |                                |   type: "start",            |
     |                                |   payload: {} })            |
     |                                |---------------------------->|
     |                                |                             |
     |                                |        handleMessage(...)   |
     |                                |                             |
     |                                | { success: true,            |
     |                                |   state: "active" }         |
     |                                |<----------------------------|
     |                                |                             |
     | { success: true, ... }         |                             |
     |<-------------------------------|                             |
```

### Task to Task Communication

```
TaskActor A                       Graph                      TaskActor B
     |                              |                             |
     | // Within message handler:   |                             |
     | graph.send("task_B",         |                             |
     |            "notify", {})     |                             |
     |----------------------------->|                             |
     |                              |                             |
     |                   Resolve "task_B" -> Address B            |
     |                              |                             |
     |                              | addressB.send(...)          |
     |                              |---------------------------->|
     |                              |                             |
     |                              | Response                    |
     |                              |<----------------------------|
     |                              |                             |
     | Response                     |                             |
     |<-----------------------------|                             |
```

## Edge Type Semantics

### Task Hierarchy Edge: `spawned_by`

```
     Child Task ----spawned_by----> Parent Task
```

**Semantics:**
- Child was created by parent via `create_task` message
- Child inherits `toolsAvailable` from parent
- Parent can query children via `graph.getChildTasks(parentId)`
- Child completion contributes to parent progress

**Invariant:**
```
forall child where child.parentTaskId exists:
  exists edge(child.id, child.parentTaskId, "spawned_by")
```

### Task Dependency Edge: `depends_on`

```
     Dependent Task ----depends_on----> Prerequisite Task
```

**Semantics:**
- Dependent cannot start until prerequisite completes
- Used for blocker detection in `task ready` command
- Creates DAG structure (cycles detected in visualization)

**Ready Criteria:**
```
task.isReady =
  task.state not in {completed, failed, blocked} AND
  forall edge(task.id, dep, "depends_on"):
    dep.state == completed
```

### Knowledge Requirement Edge: `requires_knowledge`

```
     Task ----requires_knowledge----> Knowledge Node
```

**Semantics:**
- Task needs information from knowledge node
- Knowledge node must exist for task to proceed
- Can be added via `task link` or during task creation

### Production Edge: `produces`

```
     Task ----produces----> Artifact/Knowledge
```

**Semantics:**
- Task creates this artifact upon completion
- Created when `complete` message includes `artifacts` list
- Links task output to reusable knowledge

### Cross-Reference Edge: `references`

```
     Node A ----references----> Node B
```

**Semantics:**
- General-purpose relationship
- No semantic constraints
- Used for linking related knowledge nodes

## State Consistency

### Graph State Invariants

1. **Bidirectional Mapping Consistency**
```
forall id in graph.nodes.keys():
  graph.nodeProperties.has(id) == true
```

2. **Edge ID Uniqueness**
```
forall e1, e2 in graph.edges:
  e1.id == e2.id implies e1 === e2
```

3. **Cascade Deletion**
```
after graph.removeNode(id):
  not exists edge where edge.fromId == id OR edge.toId == id
```

### Task State Invariants

1. **Valid State Transitions**
```
transitions must follow FSM:
  created -> {ready, active, blocked}
  ready -> {active, blocked}
  active -> {blocked, completed, failed}
  blocked -> {ready, failed}
```

2. **Completion Requirements**
```
task.state == completed implies:
  task.completedAt exists AND
  task.result exists AND
  eval(task).passed == true
```

3. **Hierarchy Consistency**
```
task.parentTaskId exists implies:
  edge(task.id, task.parentTaskId, "spawned_by") exists
```

### Knowledge State Invariants

1. **Version Monotonicity**
```
forall append operations:
  version_after > version_before
```

2. **Source Append-Only**
```
forall time t1 < t2:
  sources_at(t1) subset_of sources_at(t2)
```

### CLI State Invariants

1. **File Integrity**
```
after save_graph(graph):
  load_graph(file) produces equivalent graph
```

2. **Precondition Enforcement**
```
commands only execute when preconditions satisfied
```

## Integration Invariants

### 1. All Nodes Registered in Graph

```
forall node created by factory:
  exists id where graph.nodes.has(id) AND
                  graph.nodeProperties.has(id)
```

### 2. All Edges Reference Valid Nodes

**Note:** Graph does not enforce this invariant by design (edges can reference non-existent nodes). Caller is responsible for validation.

```
// Recommended but not enforced:
forall edge in graph.edges:
  graph.nodes.has(edge.fromId) AND
  graph.nodes.has(edge.toId)
```

### 3. CLI Operations Preserve Graph Invariants

```
forall CLI command execution:
  pre: graph satisfies invariants
  post: graph satisfies invariants
```

### 4. Task State Transitions Respect Dependencies

```
// Cannot start task with incomplete dependencies
forall task, action=start:
  task.state in {created, ready} AND
  forall dep in getEdgesFrom(task, depends_on):
    dep.target.state == completed
```

**Note:** This is a *should* invariant, not currently enforced automatically.

## Lifecycle Coordination

### Task Creation via CLI

```
1. User: task add "Goal" --parent task_1 --depends task_2
2. CLI: Parse arguments, validate parent/dependencies exist
3. CLI: Load graph from tasks.json
4. CLI: TaskActor({ goal, graph }) -> Address
   4a. Factory: system.register(actor) -> Address
   4b. Factory: graph.registerNode(id, address, props)
5. CLI: graph.addEdge(newId, parentId, "spawned_by")
6. CLI: graph.addEdge(newId, depId, "depends_on")
7. CLI: graph.dump() -> JSON
8. CLI: Write tasks.json
9. CLI: Output "Added task: task_N"
```

### Task Deletion Cascade

```
1. User: task delete task_1 --force
2. CLI: Load graph
3. CLI: graph.removeNode("task_1")
   3a. Graph: Remove from nodes map
   3b. Graph: Remove from nodeProperties map
   3c. Graph: Scan edges, remove where fromId=task_1 OR toId=task_1
4. CLI: Save graph
5. CLI: Output "Deleted task task_1, removed N edge(s)"
```

### Graph Serialization/Restoration

```
SAVE (graph.dump()):
1. Collect all nodeProperties values -> nodes array
2. Collect all edges values -> edges array
3. Return { nodes, edges }

RESTORE (loadGraph from file):
1. Read and parse JSON
2. Create fresh Graph instance
3. For each node in data.nodes:
   a. Factory(props, graph) -> Address
   b. graph.registerNode(id, address, props)
4. For each edge in data.edges:
   a. graph.addEdge(fromId, toId, type, props)
5. Set edge counter to max(edge IDs)
```

**Critical Note:** Addresses are runtime-only (symbols). Restoration requires recreating actors via factories.

## Performance Characteristics

### Time Complexity

| Operation | Graph | Task | Knowledge | CLI |
|-----------|-------|------|-----------|-----|
| Node lookup | O(1) | - | - | - |
| Send message | O(1) + actor | O(1) | O(1) | O(1) + graph |
| Add edge | O(1) | - | - | - |
| Get edges from/to | O(e) | - | - | - |
| Remove node | O(e) | - | - | - |
| Child progress | - | O(c) | - | - |
| Query knowledge | - | - | O(k*n) | - |
| Synthesize | - | - | O(m*n) | - |
| Search tasks | - | - | - | O(t*f) |
| Ready filter | - | - | - | O(t*d) |

Where:
- e = total edges
- c = child count
- k = keywords, n = content length
- m = source nodes
- t = total tasks, f = fields, d = dependencies

### Memory Usage

- Graph: ~100 bytes/node + ~50 bytes/edge
- Task: Properties object (~500-1000 bytes typical)
- Knowledge: Content string (variable)
- CLI: Full graph loaded per command

## Error Handling

### Layer-Specific Errors

| Layer | Error | Handling |
|-------|-------|----------|
| Actor | Actor not found | Return `{ success: false, error: "Actor not found" }` |
| Graph | Node not found | Throw Error |
| Graph | Message failed | Propagate actor error |
| Task | Invalid state transition | Return error response |
| Task | Eval fails on complete | Reject completion |
| Knowledge | Unknown message type | Throw Error |
| CLI | File not found | Exit code 1 |
| CLI | Invalid arguments | Exit code 1 |
| CLI | Task not found | Exit code 1 |

### Error Propagation

```
CLI -> Graph -> Actor
  |       |        |
  |       |        v
  |       |    Actor returns { success: false, error }
  |       v
  |    Graph throws if response.success === false
  v
CLI catches, prints "Error: {message}", exits 1
```

## Testing Strategy

### Unit Testing Per Layer

1. **Actor System**: Mock system, verify send/register
2. **Graph System**: Mock actors, verify ID mapping
3. **Task System**: Mock graph, verify state machine
4. **Knowledge System**: Mock graph, verify versioning
5. **CLI**: Mock file system and graph

### Integration Testing

1. **Graph + Actor**: Verify message routing end-to-end
2. **Task + Graph**: Verify dual registration, edge creation
3. **CLI + Graph**: Verify load/save round-trip
4. **Full Stack**: CLI command -> file change verification

### Invariant Verification

Use Datalog specifications (`*.spec.datalog`) to verify:
- All invariants hold for test scenarios
- Edge cases covered
- Integration contracts satisfied

## Summary

The Combined System integrates four subsystems through well-defined contracts:

1. **Actor System** provides message-passing foundation with Address objects
2. **Graph System** bridges string IDs (for CLI/persistence) with Addresses (for messaging)
3. **Task/Knowledge Systems** implement domain logic as actors
4. **Task CLI** provides human interface through Graph operations

**Key Integration Patterns:**
- Graph owns System, exposes via `getSystem()`
- Factories register in both System and Graph
- CLI operates through Graph layer exclusively
- Serialization requires factory recreation
- Edges create semantic relationships between nodes

**Critical Invariants:**
- Bidirectional mapping consistency (nodes <-> properties)
- Task state machine adherence
- Knowledge version monotonicity
- CLI file integrity

This architecture enables:
- Hierarchical task management with dependencies
- Knowledge linking for informed task execution
- Persistent storage for session continuity
- Extensibility through additional node types
