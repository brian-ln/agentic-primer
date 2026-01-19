# Actor Addressing Conventions

**Purpose:** Standard actor hierarchy and addressing patterns
**Audience:** Agents creating new actors in the Primer system
**Foundation:** Actor Worldview (see `actor-worldview/`)

---

## Hierarchical Addressing Format

### Pattern
```
primer.<domain>.<entity_type>_<id>
```

### Examples
```
primer.tasks.task_123
primer.knowledge.know_456
primer.ideas.cluster_7
primer.graph.cozodb
primer.watcher.file_monitor
```

---

## Domain Hierarchy

```
primer (root supervisor)
├─ primer.tasks (task collection supervisor)
│  ├─ primer.tasks.task_123 (individual task actor)
│  └─ primer.tasks.task_124
├─ primer.knowledge (knowledge collection supervisor)
│  ├─ primer.knowledge.know_456
│  └─ primer.knowledge.know_789
├─ primer.ideas (idea cluster supervisor)
│  ├─ primer.ideas.cluster_1
│  ├─ primer.ideas.cluster_2
│  └─ primer.ideas.cluster_3
├─ primer.graph (graph operations supervisor)
│  ├─ primer.graph.cozodb (effect actor - database)
│  └─ primer.graph.query (read-only query actor)
├─ primer.eventlog (audit trail supervisor)
└─ primer.watcher (file watching supervisor)
```

---

## Actor Types

### Collection Supervisors
**Role:** Manage collections of entities (tasks, knowledge, ideas)

**Pattern:** `primer.<domain>`

**Responsibilities:**
- Create new child actors
- Route messages to children
- Maintain collection index
- Supervise child lifecycle

**Example:**
```typescript
// primer.tasks supervisor
async handleMessage(msg: ActorMessage) {
  switch (msg.type) {
    case "create":
      return await this.createTask(msg.payload);
    case "query":
      return await this.queryTasks(msg.payload);
    case "route":
      return await this.send(msg.payload.target, msg.payload.message);
  }
}
```

### Entity Actors
**Role:** Manage individual entities (specific task, knowledge node, idea cluster)

**Pattern:** `primer.<domain>.<type>_<id>`

**Responsibilities:**
- Maintain entity state
- Process entity operations
- Emit state change events

**Example:**
```typescript
// primer.tasks.task_123
async handleMessage(msg: ActorMessage) {
  switch (msg.type) {
    case "update":
      return await this.updateState(msg.payload);
    case "get_state":
      return { success: true, data: this.state };
    case "delete":
      return await this.cleanup();
  }
}
```

### Effect Actors
**Role:** Interface with external systems (database, filesystem, network)

**Pattern:** `primer.<domain>.<effect_name>`

**Responsibilities:**
- Encapsulate I/O operations
- Provide clean actor interface to impure operations
- Handle errors and retries

**Example:**
```typescript
// primer.graph.cozodb
async handleMessage(msg: ActorMessage) {
  switch (msg.type) {
    case "query":
      return await this.db.run(msg.payload.query);
    case "write":
      return await this.db.run(msg.payload.mutation);
  }
}
```

---

## Message Routing

### Direct Addressing
```typescript
// Send to specific actor
await send("primer.tasks.task_123", "update", { state: "active" });
```

### Supervisor Routing
```typescript
// Send to supervisor, it routes to child
await send("primer.tasks", "route", {
  target: "task_123",
  message: { type: "update", payload: { state: "active" } }
});
```

### Broadcast
```typescript
// Send to all children in collection
await send("primer.tasks", "broadcast", {
  filter: { label: "P0" },
  message: { type: "check_status" }
});
```

---

## Graph Integration

### Actors as Graph Nodes
Every actor is a node in the graph:

```typescript
// When actor created, register in graph
graph.addNode(`primer.ideas.cluster_${clusterId}`, {
  type: "idea_cluster",
  address: `primer.ideas.cluster_${clusterId}`,
  properties: {
    topic: "actor-model",
    ideaCount: 12,
    createdAt: new Date().toISOString()
  }
});
```

### Multiple Access Paths
Hierarchical address is ONE path. Graph traversal provides others:

```
// Hierarchical path
primer.ideas.cluster_1

// Graph traversal paths
task_123 --inspired_by--> cluster_1
know_456 --related_to--> cluster_1
```

### Router Actors
Create router actors for alternate indexes:

```typescript
// primer.ideas.by_topic router
const clustersByTopic = new Map([
  ["actor-model", ["cluster_1", "cluster_5", "cluster_9"]],
  ["blob-storage", ["cluster_2", "cluster_8"]],
  ["cozodb", ["cluster_3", "cluster_7"]]
]);

async handleMessage(msg: ActorMessage) {
  if (msg.type === "find_by_topic") {
    const clusters = clustersByTopic.get(msg.payload.topic);
    return { success: true, data: clusters };
  }
}
```

---

## Actor Registration

### Creation Pattern
```typescript
// 1. Create actor instance
const actor = new IdeaClusterActor(clusterId, {
  topic: "actor-model",
  ideas: []
});

// 2. Register in actor system
actorSystem.register(`primer.ideas.cluster_${clusterId}`, actor);

// 3. Register in graph
graph.addNode(`primer.ideas.cluster_${clusterId}`, {
  type: "idea_cluster",
  address: `primer.ideas.cluster_${clusterId}`,
  properties: actor.getProperties()
});

// 4. Link to supervisor
graph.addEdge(
  "primer.ideas",
  `primer.ideas.cluster_${clusterId}`,
  "supervises",
  {}
);
```

### Cleanup Pattern
```typescript
// When actor deleted
async deleteActor(address: string) {
  // 1. Unregister from actor system
  actorSystem.unregister(address);

  // 2. Remove from graph (but keep in history)
  graph.removeNode(address);  // Or mark as deleted

  // 3. Log event
  eventLog.append({
    type: "actor_deleted",
    nodeId: address,
    timestamp: new Date().toISOString()
  });
}
```

---

## Naming Conventions

### DO
✅ Use domain prefixes (`primer.tasks`, not `task-manager`)
✅ Include entity type in ID (`task_123`, not `123`)
✅ Use snake_case for multi-word domains (`idea_cluster`, not `ideaCluster`)
✅ Keep addresses < 100 chars

### DON'T
❌ Use UUIDs in addresses (`primer.tasks.550e8400-e29b...` too long)
❌ Mix separator styles (`primer/tasks.task_123` inconsistent)
❌ Nest more than 3 levels deep (keep hierarchy flat)
❌ Use special characters except `.` and `_`

---

## Virtual Actors (Optional)

Some actors may be virtual (created on-demand):

```typescript
// primer.tasks supervisor with virtual children
async handleMessage(msg: ActorMessage) {
  if (msg.type === "route") {
    const targetAddress = `primer.tasks.${msg.payload.target}`;

    // Check if actor exists
    if (!actorSystem.has(targetAddress)) {
      // Virtual actor - instantiate on-demand
      const taskData = await loadTaskFromGraph(msg.payload.target);
      const actor = new TaskActor(taskData);
      actorSystem.register(targetAddress, actor);
    }

    // Route to actor (now guaranteed to exist)
    return await send(targetAddress, msg.payload.message);
  }
}
```

**When to use:**
- Large collections (100+ entities)
- Infrequent access patterns
- Memory constraints

**When NOT to use:**
- Frequently accessed actors
- Actors with heavy initialization
- Real-time requirements

---

## Message Format

### Standard ActorMessage
```typescript
interface ActorMessage {
  id: string;           // Message ID (UUID)
  type: string;         // Message type
  payload: any;         // Message-specific data
  sender?: string;      // Optional sender address
  timestamp?: string;   // Optional timestamp
}
```

### ActorResponse
```typescript
interface ActorResponse {
  success: boolean;
  data?: any;           // Response data
  error?: string;       // Error message
  code?: string;        // Error code
}
```

---

## Quick Reference

**Creating new actor domain:**
1. Choose domain name (`primer.<domain>`)
2. Create supervisor actor
3. Define entity addressing (`primer.<domain>.<type>_<id>`)
4. Register supervisor in graph
5. Implement collection operations (create, query, route)

**Creating entity actor:**
1. Use pattern `primer.<domain>.<type>_<id>`
2. Register in actor system
3. Register in graph as node
4. Link to supervisor via edge
5. Implement entity operations (update, delete, get_state)

**Example:**
```typescript
// New domain: primer.experiments
// Supervisor: primer.experiments
// Entities: primer.experiments.exp_001, primer.experiments.exp_002
```

---

**Status:** REFERENCE READY
**Use by:** Semantic agent (for `primer.ideas.*`), all future actor implementations
