# System Architecture - Separation of Concerns

## Clear Separation

```
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│         Knowledge/Task/Execution Management System          │
│                (Uses actors for implementation)             │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │
│  │ Task Graph   │  │  Knowledge   │  │  Execution   │    │
│  │              │  │    Graph     │  │   Tracker    │    │
│  │ • Tasks      │  │ • Concepts   │  │ • Activities │    │
│  │ • Deps       │  │ • Insights   │  │ • Results    │    │
│  │ • Status     │  │ • Questions  │  │ • History    │    │
│  └──────────────┘  └──────────────┘  └──────────────┘    │
│         ↓                 ↓                 ↓              │
│         └─────────────────┴─────────────────┘              │
│                           ↓                                │
│                  Implemented using actors                  │
│                           ↓                                │
└───────────────────────────┼────────────────────────────────┘
                            ↓
┌───────────────────────────┼────────────────────────────────┐
│                           ↓                                │
│              Generic Actor System (Core Library)           │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Actor Primitives                                     │  │
│  │ • Actor: (Message) => Promise<Response>             │  │
│  │ • System(): Actor (manages other actors)            │  │
│  │ • Message passing                                    │  │
│  │ • Hybrid methods (Proxy-based)                      │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Optional Add-ons (Composable)                        │  │
│  │ • EventStoreActor: Persistence                       │  │
│  │ • EventSourcedSystem: Wrapper for auto-capture      │  │
│  │ • Higher-order actors: withLogging, withRetry       │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Core Actor System (Library)

**Location**: `src/actors/`

**What it provides:**
- Pure actor primitives
- Message passing protocol
- System actor for management
- Hybrid method pattern (Proxy)
- Optional event sourcing wrappers

**What it does NOT know about:**
- Tasks
- Knowledge nodes
- Activities
- Dependencies
- Business logic

**Files:**
```
src/actors/
  base.ts           - Actor, Message, Response types
  system.ts         - System() actor function
  event-store.ts    - EventStoreActor (optional)
  wrappers.ts       - withLogging, withRetry, etc (optional)
```

## Task/Knowledge System (Application)

**Location**: `examples/` (for now, could move to `src/graph/`)

**What it provides:**
- Task graph with dependencies
- Knowledge graph with links
- Activity tracking
- Business logic

**How it uses actors:**
- TaskActor: Implements task state and behavior
- KnowledgeNodeActor: Implements knowledge node
- GraphSystem: Uses System() to manage actors
- EventLog: Uses EventStoreActor for persistence

**Files:**
```
examples/
  task-graph-actor-system.ts       - Task graph implementation
  knowledge-graph-actor-system.ts  - Knowledge graph implementation
  persistent-graph-actor-system.ts - With event sourcing
```

## Clear Boundaries

### What the Actor System IS:
- Generic, reusable library
- No domain knowledge
- Pure message passing
- Composable primitives

### What the Actor System IS NOT:
- Task management system
- Knowledge base
- Execution tracker
- Application logic

### What the Task/Knowledge System IS:
- Domain-specific application
- Built ON TOP of actor system
- Uses actors for implementation
- Contains business logic

### What the Task/Knowledge System IS NOT:
- Part of the core actor library
- Coupled to actor implementation details
- Limited to one actor system instance

## Example: Using the Separation

```typescript
// Core actor system (library)
import { System, EventStoreActor, type Actor, type Message, type Response } from "./src/actors";

// Task management system (application) - uses the library
import { TaskGraph, TaskActor } from "./examples/task-graph-actor-system";

// Create core system
const actorSystem = System();

// Create event store (optional add-on)
const eventStore = EventStoreActor("/tmp/events.jsonl");
actorSystem.register("event-store", eventStore);

// Create task graph (application) - uses actor system
const taskGraph = TaskGraph(); // Internally uses System()

// Both systems are independent
// Task graph uses actors, but doesn't change the actor system
```

## Benefits of Separation

1. **Reusability**: Actor system can be used for OTHER applications
   - Not just tasks/knowledge
   - Chat systems, workflows, anything

2. **Testability**: Test actor system independently
   - Mock actors
   - Test message passing
   - No domain logic

3. **Composability**: Mix and match
   - Use event sourcing OR NOT
   - Use multiple graph systems
   - Combine different actor patterns

4. **Evolution**: Change independently
   - Update actor system without touching task graph
   - Change task model without touching actors
   - Add new graph types without modifying core

5. **Clarity**: Clear boundaries
   - Actor system = HOW (mechanism)
   - Task/knowledge = WHAT (domain model)

## Migration Path

Current state: Everything in `examples/`

Proposed structure:
```
src/
  actors/              # Core actor system (library)
    base.ts
    system.ts
    event-store.ts     # Optional
    wrappers.ts        # Optional

  graph/               # Task/knowledge system (application)
    task-graph.ts      # Uses actors
    knowledge-graph.ts # Uses actors
    persistent.ts      # Uses EventStoreActor

examples/
  simple-task-demo.ts       # Shows task graph usage
  knowledge-demo.ts         # Shows knowledge graph usage
  combined-demo.ts          # Shows both together
  event-sourcing-demo.ts    # Shows persistence
```

## Key Principle

**The actor system is a generic foundation.**
**The task/knowledge system is a specific application built on that foundation.**

Just like:
- React (library) vs Your App (application)
- Express (library) vs Your API (application)
- Redux (library) vs Your State (application)

Our actor system is the library.
Our task/knowledge system is the application.

They are separate, loosely coupled, and independently evolvable.

## How They Interact

```typescript
// Actor system (generic)
class System {
  register(id: string, actor: Actor): void
  route(targetId: string, message: Message): Promise<Response>
}

// Task system (specific) - uses System internally
class TaskGraph {
  private system: System; // Uses actor system

  async createTask(data: TaskData): Promise<void> {
    const actor = TaskActor(data, this.system);
    await this.system.register(data.id, actor);
  }
}

// Usage
const taskGraph = new TaskGraph();
await taskGraph.createTask({ id: "task-1", ... });

// Task graph uses actors internally
// But user doesn't need to know about actors!
```

## Summary

✅ **Actor System**: Generic, reusable library in `src/actors/`
✅ **Task/Knowledge System**: Domain-specific application using actors
✅ **Clear separation**: One uses the other, but they're independent
✅ **Composability**: Can build OTHER systems on same actor foundation

The actor system supports whatever you build on top of it.
The task/knowledge system is just ONE thing you can build.
Others: workflow engine, chat system, game server, etc.
