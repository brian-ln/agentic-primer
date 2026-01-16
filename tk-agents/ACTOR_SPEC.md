# Actor System Specification - Final

## Core Principles

### 1. Pure Functional Actors

Actors are **pure functions** with no hidden dependencies or implicit context.

```typescript
function Actor(
  data: ActorData,          // Actor's data
  send: SendFunction        // Messaging primitive (injected)
): Actor {
  return {
    send: async (message: Message) => {
      // Actor logic - all dependencies explicit
    }
  };
}
```

**Properties:**
- ✅ All dependencies passed as parameters
- ✅ No hidden state or context
- ✅ Testable (mock `send` function)
- ✅ Composable (functions taking functions)

### 2. The Send Primitive

The `send` function is the **only** messaging primitive in the system.

```typescript
type SendFunction = (targetId: string, message: Message) => Promise<Response>;
```

**From actor's perspective:**
```typescript
await send(targetId, message);  // Send to any address
```

**Implementation is irrelevant to actors:**
- Could be registry lookup
- Could be direct reference
- Could be network call
- Could be anything

Actors just call `send(address, message)`.

### 3. System Provides Send Implementation

The System is the messaging infrastructure that provides the `send` implementation.

```typescript
function System() {
  const actors = new Map<string, Actor>();

  // This IS the send primitive
  const send: SendFunction = async (targetId, message) => {
    const actor = actors.get(targetId);
    if (!actor) throw new Error(`Actor not found: ${targetId}`);
    return actor.send(message);
  };

  return {
    send,
    register: (id: string, actor: Actor) => {
      actors.set(id, actor);
    }
  };
}
```

**System responsibilities:**
- Provide `send` implementation
- Maintain actor registry (address → actor mapping)
- Route messages to target actors

### 4. Two APIs: Bridge and Internal

**External API (Bridge into Actor World):**

External code calls `.send()` on actor objects:

```typescript
const system = System();
const actor = TaskActor(data, system.send);
system.register("task-1", actor);

// Bridge: Call .send() on actor object
await actor.send({ type: "complete" });
```

**Internal API (Inside Actor World):**

Actors use `send` function from scope:

```typescript
function TaskActor(data: TaskData, send: SendFunction) {
  return {
    send: async (message: Message) => {
      // Use send function in scope
      await send("other-task", { type: "notify" });
    }
  };
}
```

**The distinction:**
- `actor.send(message)` - External code → Actor (bridge in)
- `send(targetId, message)` - Actor → Actor (inside actor world)

### 5. Uniform Composition

Systems ARE actors - they have the same interface.

```typescript
const subsystem = System();  // Creates actor-like object
const rootSystem = System();

// Register subsystem as an actor
rootSystem.register("subsystem", subsystem);

// Can send to subsystem like any actor
await rootSystem.send("subsystem", message);
```

## Complete Type Signatures

```typescript
// Message
interface Message {
  id: string;
  type: string;
  payload: unknown;
}

// Response
interface Response {
  success: boolean;
  data?: unknown;
  error?: string;
}

// Actor
interface Actor {
  send: (message: Message) => Promise<Response>;
}

// Send function (the primitive)
type SendFunction = (targetId: string, message: Message) => Promise<Response>;

// Actor factory signature
type ActorFactory<TData> = (
  data: TData,
  send: SendFunction
) => Actor;

// System
interface System extends Actor {
  send: SendFunction & {
    (message: Message): Promise<Response>;      // As actor
    (targetId: string, message: Message): Promise<Response>; // As send primitive
  };
  register: (id: string, actor: Actor) => void;
}
```

## Usage Patterns

### Pattern 1: Create and Register Actor

```typescript
const system = System();

// Create actor with send injected
const actor = TaskActor(
  { id: "task-1", status: "todo" },
  system.send  // Inject send function
);

// Register with system
system.register("task-1", actor);
```

### Pattern 2: External Code Sends to Actor

```typescript
// Bridge into actor world via .send()
await actor.send({
  id: "msg-1",
  type: "updateStatus",
  payload: { status: "done" }
});

// Or via system
await system.send("task-1", {
  id: "msg-1",
  type: "updateStatus",
  payload: { status: "done" }
});
```

### Pattern 3: Actor-to-Actor Communication

```typescript
function TaskActor(data: TaskData, send: SendFunction) {
  return {
    send: async (message: Message) => {
      if (message.type === "complete") {
        // Use send function from scope
        await send("dependent-task", {
          id: crypto.randomUUID(),
          type: "unblock",
          payload: { unblockedBy: data.id }
        });
      }
    }
  };
}
```

### Pattern 4: Nested Systems

```typescript
const root = System();
const sub = System();

// Subsystem is an actor
root.register("subsystem", sub);

// Register actor in subsystem
const actor = TaskActor(data, sub.send);
sub.register("task-1", actor);

// Send through hierarchy
await root.send("subsystem", {
  type: "send_to_actor",
  payload: {
    targetId: "task-1",
    message: { type: "update" }
  }
});
```

## Invariants

### 1. Pure Function Invariant
```
∀ actor: Actor created by factory function
  ⟹ actor has no hidden dependencies
```

### 2. Send Injection Invariant
```
∀ actor: Actor
  ⟹ actor received send function at creation
```

### 3. Bridge Invariant
```
External code → Actor
  ⟹ Uses actor.send(message)

Actor → Actor
  ⟹ Uses send(targetId, message)
```

### 4. System Composition Invariant
```
∀ system: System
  ⟹ system IS an Actor (has .send() method)
  ⟹ system CAN be registered in another system
```

### 5. Addressing Invariant
```
∀ message sent via send(targetId, message)
  ⟹ Implementation of send is opaque to sender
  ⟹ Routing mechanism irrelevant to actor
```

## Non-Requirements

What this spec **does NOT** require:

- ❌ Specific routing mechanism (registry vs direct refs vs other)
- ❌ Synchronous vs asynchronous delivery
- ❌ Mailboxes (can be implemented but not required)
- ❌ Supervision (can be added as separate concern)
- ❌ Distribution (can be added later)
- ❌ Persistence (can be added later)

The spec defines the **interface**, not the implementation.

## Summary

**Three key insights:**

1. **Actors are pure functions** - All dependencies injected, no hidden state
2. **Send is the only primitive** - `send(targetId, message)` is the universal operation
3. **Two APIs for bridging** - External uses `actor.send()`, internal uses `send()`

This is the complete, final specification.
