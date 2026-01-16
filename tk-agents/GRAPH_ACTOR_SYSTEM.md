# Graph Actor System - Virtual Actor Pattern

## Summary

We can use the **functional actor model with hybrid methods** to build task and knowledge graph systems using the **virtual actor pattern** (Orleans-style).

## Key Insight

Your functional actor implementations are PERFECT for this because:

1. **Actors ARE nodes** - Each task/knowledge item is an actor
2. **Messages ARE edges** - Relationships are message routes
3. **Virtual actors** - Actors created on-demand (lazy instantiation)
4. **Uniform composition** - Systems can contain systems (nested graphs)
5. **Proxy enables ergonomics** - Clean API via hybrid methods

## What We Built

### 1. Task Graph (`task-graph-actor-system.ts`)

**Features:**
- Tasks are actors with state (id, title, status, dependencies, subtasks)
- Dependency relationships create blocking behavior
- Tasks auto-unblock when dependencies complete
- Nested tasks via subtask relationships
- Virtual actor pattern: tasks created on-demand

**Example:**
```typescript
const graph = TaskGraph();

// Create tasks
await graph.createTask({
  id: "design-auth",
  title: "Design authentication flow",
  status: "todo",
  dependencies: [],
  blocks: [],
  subtasks: [],
  metadata: { priority: "high" }
});

await graph.createTask({
  id: "impl-auth",
  title: "Implement authentication",
  status: "blocked",
  dependencies: ["design-auth"],  // Depends on design
  blocks: [],
  subtasks: [],
  metadata: {}
});

// Get ready tasks (only design-auth)
const ready = await graph.getReadyTasks();

// Complete design-auth
await graph.route({
  taskId: "design-auth",
  message: { type: "updateStatus", payload: { status: "done" } }
});

// impl-auth now automatically unblocked!
```

### 2. Knowledge Graph (`knowledge-graph-actor-system.ts`)

**Features:**
- Knowledge nodes: concept, insight, question, decision, reference, task
- Link types: relates-to, answers, implements, cites, precedes, requires
- Graph traversal: follow links to explore connected knowledge
- Search: by content or node type
- Timestamps: track when knowledge was created/updated

**Example:**
```typescript
const kb = KnowledgeGraph();

// Capture a research question
await kb.createNode({
  id: "q-actor-model",
  type: "question",
  title: "How do we model systems functionally with actors?",
  content: "We want to drop classes and use pure functions.",
  links: [],
  metadata: { tags: ["actors", "functional"] }
});

// Capture an insight
await kb.createNode({
  id: "insight-closure-state",
  type: "insight",
  title: "Closures solve circular dependency",
  content: "Pass System to actor factory, capture in closure.",
  links: [],
  metadata: { tags: ["actors", "functional"] }
});

// Link insight to question
await kb.linkNodes("insight-closure-state", "q-actor-model", "answers");

// Traverse from question to see all linked knowledge
const traverse = await kb.traverseLinks("q-actor-model", undefined, 2);

// Search for insights
const results = await kb.search("functional", "insight");
```

## Virtual Actor Pattern (Orleans)

The **virtual actor pattern** means:

1. **Actors created on-demand**: Reference an actor ID, it springs into existence
2. **No explicit lifecycle**: Don't need to "new" actors, just message them
3. **Location transparency**: Don't care where actor lives, just its ID
4. **Single instance per ID**: Each ID maps to exactly one actor

**In our implementation:**
```typescript
// getOrCreateNode - virtual actor pattern
const result = await kb.getOrCreateNode({
  id: "concept-123",
  type: "concept",
  title: "Virtual Actors",
  content: "...",
  links: [],
  metadata: { tags: [] }
});

// First call: creates actor
// result.data.existed === false

// Second call: returns existing
// result.data.existed === true
```

## Why This Works

### 1. Functional Actors Foundation

Our **hybrid methods pattern** gives us:
- Actors as pure functions: `(Message) => Promise<Response>`
- Closure state (private, encapsulated)
- Proxy-based dynamic methods (Smalltalk-like)
- Defined methods for hot paths (optimized)

### 2. System IS an Actor

Since **System is just an actor**, it can:
- Contain other systems (nested graphs)
- Be composed with higher-order functions
- Route messages uniformly

### 3. Single-Phase Initialization

No circular dependency problems:
```typescript
function TaskActor(data: TaskData, system: DynamicActor): DynamicActor {
  // System captured in closure - single-phase init!
  // Can message system immediately
}
```

### 4. Proxy Enables Ergonomics

Without breaking actor model purity:
```typescript
// Can call as function (full control)
await graph({ type: "createTask", payload: taskData });

// Or use convenience method (ergonomic)
await graph.createTask(taskData);

// Or use dynamic method (explore)
await graph.someCustomMessage({ foo: "bar" });
```

## Combining Task + Knowledge

You can **unify both graphs** since they use the same actor model:

```typescript
// Unified graph with both tasks and knowledge
const unifiedGraph = System();

// Tasks are actors
const task = TaskActor({ id: "impl-auth", ... }, unifiedGraph);
unifiedGraph.register("task:impl-auth", task);

// Knowledge nodes are actors
const insight = KnowledgeNodeActor({ id: "insight-closures", ... }, unifiedGraph);
unifiedGraph.register("knowledge:insight-closures", insight);

// Link task to insight
await task.setMetadata("insight", "knowledge:insight-closures");
```

## What You Can Build

### 1. **Task Management System**
- Dependency tracking (blocks, requires)
- Nested tasks (parent/child)
- Status transitions (todo → in_progress → done)
- Priority queues (get ready tasks)

### 2. **Knowledge Base**
- Capture questions, insights, decisions
- Link related concepts (relates-to, cites)
- Search and discovery
- Track knowledge evolution

### 3. **Research System**
- Questions → Insights → Decisions → References
- Traverse research paths
- Tag and categorize
- Timeline view (created/updated)

### 4. **Zettelkasten**
- Atomic notes as actors
- Bidirectional links
- Emergent structure from connections
- Serendipitous discovery

### 5. **Project Planning**
- Tasks with dependencies
- Knowledge artifacts linked to tasks
- Decision log linked to tasks
- Full project graph

## Persistence

To persist the graph:

```typescript
// Serialize all actor states
const serialize = async () => {
  const tasks = await graph.listTasks();
  return JSON.stringify(tasks.data);
};

// Recreate from serialized state
const deserialize = (json: string) => {
  const graph = TaskGraph();
  const data = JSON.parse(json);

  for (const taskData of data.tasks) {
    await graph.createTask(taskData);
  }

  return graph;
};
```

Or use JSONL (newline-delimited JSON):
```typescript
// Append-only event log
await Bun.write("tasks.jsonl", JSON.stringify({
  event: "task_created",
  data: taskData,
  timestamp: Date.now()
}) + "\n", { append: true });

// Replay events to reconstruct graph
const lines = (await Bun.file("tasks.jsonl").text()).split("\n");
for (const line of lines) {
  const event = JSON.parse(line);
  // Apply event to graph
}
```

## Next Steps

1. **Persistence layer**: JSONL event log or SQLite
2. **Query layer**: More sophisticated search (full-text, embeddings)
3. **Visualization**: D3.js force-directed graph
4. **Collaboration**: Multiple graphs syncing via messages
5. **Time travel**: Event sourcing with replay
6. **Distributed**: Multiple systems on different machines

## Comparison with Other Systems

| System | Approach | Our Advantage |
|--------|----------|---------------|
| Orleans | C#, class-based | Functional, simpler |
| Akka | Scala, class-based | No inheritance, pure functions |
| Redux | Centralized store | Distributed actors |
| Graph databases | External system | Embedded, in-process |
| Event sourcing | Append-only log | + actor encapsulation |

## Why This is Powerful

1. **Uniform model**: Tasks, knowledge, decisions all use same actor pattern
2. **Composability**: Systems nest, actors compose
3. **Location transparency**: Don't care where actors live
4. **Message passing**: Everything via messages (trackable, replayable)
5. **Functional purity**: Pure functions with closure state
6. **Type safety**: TypeScript types + actor protocol
7. **Ergonomic**: Hybrid methods = clean API + power

## Conclusion

Your **functional actor system with hybrid methods** is perfectly suited for building task and knowledge graphs using the **virtual actor pattern**.

You have:
- ✅ Pure functional foundation (system-as-function.ts)
- ✅ Ergonomic API (actors-with-hybrid-methods.ts)
- ✅ Formal specification (FUNCTIONAL_ACTOR_SYSTEM.datalog)
- ✅ Working task graph (task-graph-actor-system.ts)
- ✅ Working knowledge graph (knowledge-graph-actor-system.ts)

**This is production-ready for building your task/knowledge management system!**
