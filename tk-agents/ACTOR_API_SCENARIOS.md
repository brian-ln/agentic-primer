# Actor API Scenarios - External vs Internal

## The Key Distinction

**External Code** (Library User):
- TypeScript application using the actor library
- Calls domain-specific methods
- Doesn't think about actors/messages

**Internal Actor Model** (Library Implementation):
- How actors communicate within the system
- Pure message passing
- Addresses, routing, send semantics

## Scenario 1: Task Management System

### External API (What Users See)

```typescript
import { TaskGraph } from "./actor-library";

// User creates task graph system
const taskGraph = TaskGraph();

// User calls domain methods
await taskGraph.createTask({
  id: "task-1",
  title: "Build authentication",
  status: "todo",
  dependencies: []
});

await taskGraph.updateTask("task-1", { status: "in_progress" });

await taskGraph.addDependency("task-2", "task-1");

const tasks = await taskGraph.getTasks();
const ready = await taskGraph.getReadyTasks();

// User doesn't see: actors, send, messages, routing
// User sees: domain methods, business logic
```

### Internal Implementation (What Actors Do)

```typescript
// TaskGraph wraps actor system
class TaskGraph {
  private system: System;
  private tasks: Map<string, TaskActor>;

  async createTask(data: TaskData): Promise<void> {
    // Internally: create actor, register with system
    const actor = TaskActor(data, this.system);
    this.tasks.set(data.id, actor);

    // Actor needs to communicate with other actors
    // How does it send messages?
  }

  async updateTask(id: string, updates: Partial<TaskData>): Promise<void> {
    // Internally: send message to actor
    const actor = this.tasks.get(id);

    // Option A: Direct send to actor
    await actor.send({ type: "update", payload: updates });

    // Option B: Send via system
    await this.system.send(id, { type: "update", payload: updates });
  }
}

// Inside TaskActor: Actor-to-Actor Communication
function TaskActor(data: TaskData, system: System) {
  return {
    send: async (message: Message) => {
      if (message.type === "updateStatus") {
        // Status changed to "done"
        // Need to notify actors that depend on me

        // How do I send to other actors?

        // Option A: System reference + target ID
        for (const dependentId of data.blocks) {
          await system.send(dependentId, {
            type: "unblock",
            payload: { unblockedBy: data.id }
          });
        }

        // Option B: Send with address in message
        for (const dependentId of data.blocks) {
          await this.send({
            to: dependentId,  // Address in message
            type: "unblock",
            payload: { unblockedBy: data.id }
          });
        }

        // Option C: Special system address
        await this.send({
          to: "system",  // Special address
          type: "sendTo",
          payload: {
            target: dependentId,
            message: { type: "unblock", ... }
          }
        });
      }
    }
  };
}
```

## Scenario 2: Knowledge Graph

### External API

```typescript
import { KnowledgeGraph } from "./actor-library";

const kb = KnowledgeGraph();

// Domain methods
await kb.addConcept({
  id: "actor-model",
  title: "Actor Model",
  content: "..."
});

await kb.addInsight({
  id: "closure-pattern",
  title: "Closures solve circular deps",
  content: "..."
});

await kb.linkNodes("closure-pattern", "actor-model", "relates-to");

const related = await kb.traverse("actor-model", "relates-to");
const results = await kb.search("functional");

// No actors, no messages visible
```

### Internal Implementation

```typescript
class KnowledgeGraph {
  private system: System;

  async linkNodes(fromId: string, toId: string, linkType: string): Promise<void> {
    // Question: How does fromNode send to toNode to establish link?

    // Actors need to communicate!
    // What's the protocol?
  }
}

function KnowledgeNodeActor(data: NodeData, system: System) {
  return {
    send: async (message: Message) => {
      if (message.type === "addLink") {
        const { targetId, linkType } = message.payload;

        // Need to notify target node about bidirectional link
        // How do I send to target?

        // ??? What's the addressing mechanism?
      }
    }
  };
}
```

## The Addressing Question

### Option A: System Reference + Target ID

**Actor has system reference, specifies target ID:**

```typescript
function TaskActor(data, system) {
  return {
    send: async (message) => {
      // To send to another actor:
      await system.send("target-actor-id", {
        type: "notify",
        payload: { ... }
      });
    }
  };
}

// System API
system.send(targetId: string, message: Message)
```

**Pros:**
- ✅ Clear separation: system handles routing
- ✅ Actors don't need to know addressing details
- ✅ System can enforce security, logging, etc.

**Cons:**
- ❌ Actor needs system reference
- ❌ Two APIs: actor.send (self) vs system.send (others)

### Option B: Address in Message

**Messages have destination address:**

```typescript
function TaskActor(data, system) {
  return {
    send: async (message) => {
      // Send to self
      if (message.to === data.id) {
        // Handle locally
      }

      // Send to another actor - forward to system
      else {
        await system.send({
          to: message.to,  // Address in message
          type: message.type,
          payload: message.payload
        });
      }
    }
  };
}

// Message format
interface Message {
  to: string;      // Destination address
  from?: string;   // Source address
  type: string;
  payload: unknown;
}
```

**Pros:**
- ✅ Addresses in messages (like Erlang, Hewitt)
- ✅ Can forward messages easily
- ✅ Uniform message format

**Cons:**
- ❌ Actors must route messages themselves
- ❌ Addressing logic in every actor

### Option C: Special System Address

**System has a well-known address:**

```typescript
function TaskActor(data, system) {
  const SYSTEM_ADDRESS = "system";

  return {
    send: async (message) => {
      // Send to self
      if (!message.to || message.to === data.id) {
        // Handle locally
      }

      // Send to another actor via system
      else if (message.to !== SYSTEM_ADDRESS) {
        // Forward to system to route
        await system.send({
          to: SYSTEM_ADDRESS,
          type: "forward",
          payload: {
            target: message.to,
            message: { ...message }
          }
        });
      }
    }
  };
}
```

**Pros:**
- ✅ System is addressable
- ✅ Uniform addressing
- ✅ Can send to system for special operations

**Cons:**
- ❌ More indirection
- ❌ Complex message format

## Recommended Approach

### Two-Level API

**Level 1: External Library API (Domain Methods)**

```typescript
// User-facing API - no actors visible
class TaskGraph {
  async createTask(data: TaskData): Promise<void>;
  async updateTask(id: string, updates: Partial<TaskData>): Promise<void>;
  async getTasks(): Promise<TaskData[]>;
}

// Users call domain methods
const taskGraph = TaskGraph();
await taskGraph.createTask({ ... });
await taskGraph.updateTask("task-1", { status: "done" });
```

**Level 2: Internal Actor API (Message Passing)**

```typescript
// Internal actor implementation
function TaskActor(data: TaskData, system: System) {
  return {
    send: async (message: Message) => {
      // Actor handles messages
      switch (message.type) {
        case "update":
          // Update state
          break;

        case "notifyDependent":
          // Send to another actor
          await system.send("dependent-id", {
            type: "unblock",
            payload: { ... }
          });
          break;
      }
    }
  };
}
```

### Key Insight: System is the Router

```typescript
// Actors send to System, System routes to target
class System {
  private actors: Map<string, Actor>;

  async send(targetId: string, message: Message): Promise<Response> {
    const actor = this.actors.get(targetId);
    if (!actor) {
      throw new Error(`Actor not found: ${targetId}`);
    }

    // Forward to target actor
    return actor.send(message);
  }
}

// Actor-to-actor communication
function ActorA(system: System) {
  return {
    send: async (message) => {
      // To communicate with ActorB:
      await system.send("actor-b", {
        type: "notify",
        payload: { ... }
      });
    }
  };
}
```

## The Pattern

```
External Code (TypeScript App)
    ↓ Calls domain methods
TaskGraph (Domain Layer)
    ↓ Translates to actor messages
System (Actor Routing Layer)
    ↓ Routes to target actor
Actor (Message Handler)
    ↓ Processes message
    ↓ May send to other actors via System
System (Routes again)
    ↓
Another Actor
```

## Summary

1. **External API**: Domain-specific methods (createTask, updateTask, etc.)
   - Users don't see actors
   - Clean, intuitive interface

2. **Internal Actor Model**: Message passing via System
   - Actors send to System with targetId
   - System routes to target actor
   - `system.send(targetId, message)`

3. **No addressing in messages** (Option A wins)
   - Actor has system reference
   - Calls `system.send(targetId, message)` to reach others
   - System handles routing

4. **Two APIs**:
   - `actor.send(message)` - Send to self
   - `system.send(targetId, message)` - Send to others via System

This gives us:
- ✅ Clean external API
- ✅ Pure actor model internally
- ✅ System as router (clear responsibility)
- ✅ No addressing complexity in messages
