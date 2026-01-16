# Actor System - New Implementation

A clean, spec-compliant implementation of the actor model following `ACTOR_SPEC.md`.

## Overview

This implementation provides a minimal, type-safe actor system with three core principles:

1. **Actors are pure functions** - All dependencies explicit, no hidden state
2. **Send is the only primitive** - `send(targetId, message)` for all communication
3. **Two APIs for bridging** - External uses `actor.send()`, internal uses `send()`

## Quick Start

```typescript
import { createSystem } from "./index.ts";
import type { ActorFactory, Message } from "./index.ts";

// 1. Define your actor factory
const CounterActor: ActorFactory<{ count: number }> = (data, send) => ({
  send: async (message: Message) => {
    if (message.type === "increment") {
      data.count++;
      return { success: true, data: data.count };
    }
    if (message.type === "get") {
      return { success: true, data: data.count };
    }
    return { success: false, error: "Unknown message type" };
  },
});

// 2. Create system
const system = createSystem();

// 3. Create and register actor
const counter = CounterActor({ count: 0 }, system.send);
system.register("counter", counter);

// 4. Send messages (external API)
await counter.send({ id: "1", type: "increment", payload: {} });
const result = await counter.send({ id: "2", type: "get", payload: {} });
console.log(result.data); // 1
```

## API Reference

### Types

#### `Message`
```typescript
interface Message {
  id: string;        // Unique message ID
  type: string;      // Message type (e.g., "increment", "query")
  payload: unknown;  // Message data
}
```

#### `Response`
```typescript
interface Response {
  success: boolean;   // Operation success flag
  data?: unknown;     // Optional response data
  error?: string;     // Optional error message
}
```

#### `Actor`
```typescript
interface Actor {
  send: (message: Message) => Promise<Response>;
}
```

#### `SendFunction`
```typescript
type SendFunction = (
  targetId: string,
  message: Message
) => Promise<Response>;
```

#### `ActorFactory`
```typescript
type ActorFactory<TData> = (
  data: TData,
  send: SendFunction
) => Actor;
```

### Functions

#### `createSystem()`

Creates a new actor system.

```typescript
function createSystem(): System;
```

**Returns:** A system with:
- `send` - SendFunction for routing messages
- `register` - Function to register actors by ID

**Example:**
```typescript
const system = createSystem();
```

#### `System.register(id, actor)`

Registers an actor with an ID.

```typescript
register(id: string, actor: Actor): void;
```

**Example:**
```typescript
system.register("my-actor", actor);
```

#### `System.send(targetId, message)`

Sends a message to a registered actor.

```typescript
send(targetId: string, message: Message): Promise<Response>;
```

**Example:**
```typescript
const response = await system.send("my-actor", {
  id: "msg-1",
  type: "test",
  payload: {},
});
```

## Patterns

### Pattern 1: Simple Actor

```typescript
const EchoActor: ActorFactory<{ prefix: string }> = (data, send) => ({
  send: async (message: Message) => {
    return {
      success: true,
      data: `${data.prefix}: ${message.type}`,
    };
  },
});
```

### Pattern 2: Actor-to-Actor Communication

```typescript
const NotifierActor: ActorFactory<{ targetId: string }> = (data, send) => ({
  send: async (message: Message) => {
    if (message.type === "notify") {
      // Use send from scope (internal API)
      await send(data.targetId, {
        id: crypto.randomUUID(),
        type: "notification",
        payload: message.payload,
      });
      return { success: true };
    }
    return { success: false };
  },
});
```

### Pattern 3: Stateful Actor

```typescript
interface TaskData {
  status: "pending" | "running" | "done";
}

const TaskActor: ActorFactory<TaskData> = (data, send) => ({
  send: async (message: Message) => {
    switch (message.type) {
      case "start":
        data.status = "running";
        return { success: true };
      case "complete":
        data.status = "done";
        return { success: true };
      case "getStatus":
        return { success: true, data: data.status };
      default:
        return { success: false, error: "Unknown message type" };
    }
  },
});
```

### Pattern 4: Nested Systems

```typescript
const root = createSystem();
const subsystem = createSystem();

// Register subsystem as actor
root.register("subsystem", subsystem);

// Send to subsystem
await root.send("subsystem", {
  id: "1",
  type: "list",
  payload: {},
});
```

## Examples

See `examples-new/` for complete working examples:

- **task-graph.ts** - Task dependency system with notifications
- **nested-systems.ts** - Hierarchical system composition
- **knowledge-graph.ts** - Distributed graph traversal

## Testing

Run the spec verification tests:

```bash
bun test src/actors-new/spec-verification.test.ts
```

All 22 tests verify implementation against `ACTOR_SPEC.md`.

## Architecture

```
src/actors-new/
├── base.ts     # Core type definitions
├── system.ts   # System implementation
├── index.ts    # Public exports
└── README.md   # This file
```

**Total LOC:** ~150 lines of implementation code

## Key Principles

### Pure Functions
Actors are created by factory functions with explicit dependencies:
```typescript
const actor = MyActor(data, send); // All deps passed as parameters
```

### Send Injection
All actors receive `send` function at creation:
```typescript
const MyActor: ActorFactory<Data> = (data, send) => {
  // send is available throughout actor
};
```

### Two APIs
- **External (bridge):** `actor.send(message)` - 1 argument
- **Internal:** `send(targetId, message)` - 2 arguments

### Uniform Composition
Systems ARE actors - they can be nested:
```typescript
root.register("subsystem", subsystem);
```

## Migration from Old Implementation

If you're migrating from the old implementation:

1. Convert classes to factory functions
2. Remove mailbox usage (handle directly in actor)
3. Use `send` from scope instead of `this.send`
4. External calls: `actor.send(message)` not `system.send(actorId, message)`

## Next Steps

This implementation satisfies all spec requirements. Possible extensions:

- **Mailboxes** - Add message queueing
- **Supervision** - Add error recovery
- **Persistence** - Add event sourcing
- **Distribution** - Add network support

All extensions should maintain spec compliance.

## Documentation

- **Spec:** `ACTOR_SPEC.md` - Complete specification
- **Datalog:** `ACTOR_SPEC.datalog` - Formal verification model
- **Summary:** `SPEC_SUMMARY.md` - Quick reference
- **Implementation:** `IMPLEMENTATION_NOTES.md` - This implementation

## License

See project root for license information.
