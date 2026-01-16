# Functional Actor Design (No Classes)

## Question
What if we modeled the actor system functionally instead of with classes? Keep types, but drop classes. What are the implications?

## Current Design (Classes)

```typescript
// Classes with methods
class System implements Actor {
  private actors: Map<string, Actor> = new Map();

  async receive(message: Message): Promise<Response> {
    // ...
  }
}

abstract class BaseActor implements Actor {
  protected system?: System;

  abstract receive(message: Message): Promise<Response>;

  protected async send(targetId: string, message: Message): Promise<Response> {
    return this.system.receive({ /* route message */ });
  }
}
```

## Functional Design (Functions + Data)

### Core Types (Same)

```typescript
// Types stay the same
interface Message {
  id: string;
  type: string;
  payload: unknown;
  sender?: string;
}

interface Response {
  success: boolean;
  data?: unknown;
  error?: string;
}

type ActorType = "deterministic" | "agent";

// Actor is now just a function signature
type Actor = (message: Message) => Promise<Response>;

// Actor State (what was hidden in classes)
interface ActorState {
  id: string;
  type: ActorType;
  receive: Actor;  // The function that handles messages
}
```

### System (Function + State)

```typescript
// System State
interface SystemState {
  id: string;
  actors: Map<string, ActorState>;
}

// System is a function that creates a stateful actor
function createSystem(id: string = "system"): {
  state: SystemState;
  receive: Actor;
} {
  const state: SystemState = {
    id,
    actors: new Map(),
  };

  const receive: Actor = async (message: Message): Promise<Response> => {
    switch (message.type) {
      case 'ping':
        return { success: true, data: { alive: true, timestamp: Date.now() } };

      case 'list':
        return {
          success: true,
          data: {
            actors: Array.from(state.actors.values()).map(a => ({
              id: a.id,
              type: a.type
            }))
          }
        };

      case 'register':
        const { actor: actorState } = message.payload as { actor: ActorState };
        if (state.actors.has(actorState.id)) {
          return { success: false, error: `Actor already registered: ${actorState.id}` };
        }
        state.actors.set(actorState.id, actorState);
        return { success: true, data: { actorId: actorState.id } };

      case 'route':
        const { targetId, message: innerMessage } = message.payload as {
          targetId: string;
          message: Message;
        };
        const actor = state.actors.get(targetId);
        if (!actor) {
          return { success: false, error: `Actor not found: ${targetId}` };
        }
        return actor.receive(innerMessage);

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  };

  return { state, receive };
}
```

### Simple Actor (Function)

```typescript
// Factory function that returns an actor
function createEchoActor(id: string): ActorState {
  const receive: Actor = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    return {
      success: true,
      data: {
        echo: message.payload,
        receivedFrom: message.sender,
        actorId: id
      }
    };
  };

  return {
    id,
    type: "deterministic",
    receive
  };
}
```

### Coordinating Actor (Higher-Order Function)

```typescript
// For actors that need to send to others, pass system's receive function
function createCoordinatorActor(
  id: string,
  workerIds: string[],
  systemReceive: Actor  // Closure over system's receive
): ActorState {
  const receive: Actor = async (message: Message): Promise<Response> => {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    if (message.type !== 'coordinate') {
      return { success: false, error: 'Unknown message type' };
    }

    // Send to all workers
    const results = await Promise.all(
      workerIds.map(async (workerId, index) => {
        const response = await systemReceive({
          id: crypto.randomUUID(),
          type: 'route',
          payload: {
            targetId: workerId,
            message: {
              id: crypto.randomUUID(),
              type: 'work',
              payload: { task: message.payload, workerId: index },
              sender: id
            }
          }
        });
        return { workerId, response: response.data };
      })
    );

    return {
      success: true,
      data: { coordinatedBy: id, workerCount: workerIds.length, results }
    };
  };

  return { id, type: "deterministic", receive };
}
```

## Comparison

### Class-Based Pros
- Familiar OOP patterns
- Encapsulation via private fields
- `this` context is automatic
- Inheritance (BaseActor) shares code
- IDE tooling expectations

### Class-Based Cons
- Mutable state hidden in instances
- `this` binding issues
- Two-phase initialization (construct, then setSystem)
- Harder to test (need instances)
- Inheritance couples implementations

### Functional Pros
- **Explicit state** (everything visible in function signatures)
- **No `this` binding issues**
- **Pure functions easier to test** (just call them)
- **Composition over inheritance** (higher-order functions)
- **Single-phase initialization** (all dependencies in factory args)
- **System reference via closure** (no setSystem needed)
- **Immutable patterns easier** (return new state instead of mutating)

### Functional Cons
- **More verbose** (explicit state passing)
- **Less familiar to OOP developers**
- **No private fields** (convention only)
- **IDE tooling less helpful** (no class introspection)
- **Performance**: function creation on each call vs class method

## Key Insight: Dependency Injection

**Class-based:**
```typescript
const actor = new CoordinatorActor("coord-1", ["w1", "w2"]);
// Two-phase: must call setSystem() later
system.register(actor);
```

**Functional:**
```typescript
const actor = createCoordinatorActor(
  "coord-1",
  ["w1", "w2"],
  system.receive  // Dependency passed at creation
);
// Single-phase: actor is ready to use
```

The functional approach **solves the circular dependency problem** by passing `system.receive` as a closure parameter at actor creation time.

## Actor as Data

Functional design makes actors **just data structures**:

```typescript
interface ActorState {
  id: string;
  type: ActorType;
  receive: (message: Message) => Promise<Response>;
}
```

This makes actors:
- **Serializable** (almost - functions aren't, but state is)
- **Inspectable** (just read the fields)
- **Testable** (mock the receive function)
- **Composable** (pass them around as values)

## Migration Path

Could support both patterns:

```typescript
// Functional
const functionalActor = createEchoActor("echo-1");

// Class-based
class EchoActor implements Actor {
  // ...
}

// System accepts both
system.receive({
  type: 'register',
  payload: { actor: functionalActor }  // Works with ActorState
});

system.receive({
  type: 'register',
  payload: { actor: new EchoActor("echo-2") }  // Works with Actor instances
});
```

## Recommendation

**Hybrid approach:**
1. Keep **System as class** (familiar, encapsulation)
2. Support **actors as pure functions** (simpler)
3. Keep **Actor interface** (polymorphism)
4. Drop **BaseActor** (use factory functions with closures instead)

This gives:
- ✅ Simple actors stay simple (pure functions)
- ✅ Coordinating actors get system via closure (no setSystem)
- ✅ System provides familiar API
- ✅ No inheritance hierarchy
- ✅ Better testability

## Example: Hybrid

```typescript
// System stays as class
class System implements Actor {
  // ...
}

// Simple actors as functions
function createEchoActor(id: string): Actor {
  return {
    id,
    type: "deterministic",
    async receive(message: Message): Promise<Response> {
      // ...
    }
  };
}

// Coordinating actors get system via factory
function createCoordinatorActor(
  id: string,
  workerIds: string[],
  system: System
): Actor {
  return {
    id,
    type: "deterministic",
    async receive(message: Message): Promise<Response> {
      // Uses system via closure
      await system.receive({ /* route to workers */ });
    }
  };
}
```

## Next Steps

1. **Prototype**: Build functional version of one actor
2. **Test**: Compare testability, verbosity, clarity
3. **Decide**: Pure functional, hybrid, or stay class-based
4. **Document**: Capture decision rationale
