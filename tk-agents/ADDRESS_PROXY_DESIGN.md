# Address Proxy Design

## Core Concept

**Addresses are objects with a `.send()` method built in.**

Instead of addresses being opaque symbols, they're objects that:
1. Act as unique identifiers (via internal symbol)
2. Have a convenient `.send()` method attached

## Type Definitions

```typescript
// base.ts
export type Address = {
  readonly __id: symbol;  // Internal unique identifier
  send: (message: Message) => Promise<Response>;
};

export type SendFunction = (
  targetAddress: Address,
  message: Message
) => Promise<Response>;

export interface Actor {
  send: (message: Message) => Promise<Response>;
}

export type ActorFactory<TData> = (data: TData) => Address;
```

## System Implementation

```typescript
// system.ts
export interface System {
  send: SendFunction;
  register: (actor: Actor) => Address;
}

export function System(): System {
  const actors = new Map<symbol, Actor>();

  // The send implementation
  const send: SendFunction = async (addr: Address, message: Message) => {
    const actor = actors.get(addr.__id);
    if (!actor) {
      return { success: false, error: "Actor not found" };
    }
    return actor.send(message);
  };

  // Register an actor and return an Address proxy
  const register = (actor: Actor): Address => {
    const id = Symbol();
    actors.set(id, actor);

    // Create Address with .send() method
    const address: Address = {
      __id: id,
      send: (message: Message) => send(address, message)
    };

    return address;
  };

  return { send, register };
}
```

## Actor Factory Pattern

```typescript
// task-actor.ts
import type { Address, Message, System } from "../actors";

export function TaskActor(data: {
  id: string;
  status: string;
  system: System;
}): Address {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "complete") {
        data.status = "done";
        return { success: true, data: { status: data.status } };
      }

      if (message.type === "getStatus") {
        return { success: true, data: { status: data.status } };
      }

      return { success: false, error: "Unknown message type" };
    }
  };

  // Register and return Address
  return data.system.register(actor);
}
```

## Usage Examples

### Example 1: Direct Send on Address

```typescript
import { System } from "./actors";
import { TaskActor } from "./graph/task-actor";

const system = System();

const task = TaskActor({
  id: "task-1",
  status: "todo",
  system
});

// Ergonomic: call .send() on the address directly
const result = await task.send({
  id: "msg-1",
  type: "complete",
  payload: {}
});

console.log(result);  // { success: true, data: { status: "done" } }
```

### Example 2: System Send with Address

```typescript
const system = System();

const task1 = TaskActor({ id: "task-1", status: "todo", system });
const task2 = TaskActor({ id: "task-2", status: "todo", system });

// Explicit: use system.send() with address
await system.send(task1, {
  id: "msg-1",
  type: "complete",
  payload: {}
});

// Both styles work!
await task2.send({
  id: "msg-2",
  type: "complete",
  payload: {}
});
```

### Example 3: Passing Addresses Between Actors

```typescript
// Actor that notifies another actor
export function NotifierActor(data: {
  targetAddr: Address;  // Address IS the reference
  system: System;
}): Address {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "notify") {
        // Send to target using either style:

        // Style 1: Direct
        await data.targetAddr.send({ type: "notification", payload: {} });

        // Style 2: Via system
        await data.system.send(data.targetAddr, {
          type: "notification",
          payload: {}
        });

        return { success: true };
      }
      return { success: false, error: "Unknown message type" };
    }
  };

  return data.system.register(actor);
}

// Usage
const system = System();
const task = TaskActor({ id: "task-1", status: "todo", system });
const notifier = NotifierActor({ targetAddr: task, system });

await notifier.send({ type: "notify", payload: {} });
```

### Example 4: Actor-to-Actor Communication

```typescript
export function TaskActor(data: {
  id: string;
  status: string;
  dependents: Address[];  // Other actors' addresses
  system: System;
}): Address {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "complete") {
        data.status = "done";

        // Notify all dependents
        for (const dependent of data.dependents) {
          await dependent.send({
            type: "unblocked",
            payload: { by: data.id }
          });
        }

        return { success: true };
      }
      return { success: false };
    }
  };

  return data.system.register(actor);
}

// Usage
const system = System();

const task1 = TaskActor({
  id: "task-1",
  status: "todo",
  dependents: [],
  system
});

const task2 = TaskActor({
  id: "task-2",
  status: "blocked",
  dependents: [task1],  // Pass address directly!
  system
});

await task2.send({ type: "complete" });  // Will notify task1
```

## Benefits

### ✅ Ergonomic API
```typescript
await task.send({ type: "complete" });  // Clean!
```

### ✅ Type-Safe Addresses
```typescript
const task: Address = TaskActor({ ... });  // Address type enforced
```

### ✅ No Magic Strings
```typescript
// Before (BAD):
system.register("task-1", actor);
await system.send("task-1", message);  // Typo risk!

// After (GOOD):
const task = TaskActor({ ... });
await task.send(message);  // No strings!
```

### ✅ Pass Addresses as Data
```typescript
// Addresses ARE the references
const notifier = NotifierActor({ targetAddr: task, system });
```

### ✅ Both Styles Work
```typescript
await task.send(message);              // Ergonomic
await system.send(task, message);      // Explicit
```

## Implementation Notes

### Internal Symbol
The `__id` field is the internal unique identifier. It's a symbol, so:
- Guaranteed unique
- Not serializable (intentionally - addresses are runtime only)
- Fast Map lookup in System

### Send Method Closure
The `.send()` method on Address closes over the system's send function:
```typescript
send: (message: Message) => send(address, message)
```

This creates a bound method that always sends to the correct actor.

### No Registration API Change
Actors still don't know their own address. They receive `system` in data, create the actor object, and `system.register()` returns the Address.

## Migration from Current System

### Before (Current)
```typescript
const system = createSystem();
const actor = TaskActor({ id: "task-1" }, system.send);
system.register("task-1", actor);
await system.send("task-1", message);
```

### After (New)
```typescript
const system = System();
const task = TaskActor({ id: "task-1", system });
await task.send(message);
```

**Much cleaner!**



## Going Further: Alternative API Pattern

### The Question

What if we inverted the creation API so that the System explicitly creates actors?

```typescript
// Current approach - explicit system passing
const task = TaskActor({ id: "task-1", status: "todo", system });

// Alternative approach - system.create()
const task = system.create(TaskActor, { id: "task-1", status: "todo" });
```

### API Comparison

#### Current: Factory-Driven Creation
```typescript
// Actor factory signature
export type ActorFactory<TData> = (data: TData) => Address;

// Usage
const system = System();
const task = TaskActor({
  id: "task-1",
  status: "todo",
  system  // Explicitly passed
});
```

**Key characteristics:**
- System is part of the data parameter
- Factory function is called directly
- Explicit dependency injection
- Factory controls registration timing

#### Alternative: System-Driven Creation
```typescript
// Actor factory signature (no system in data)
export type ActorFactory<TData> = (data: TData & { system: System }) => Address;

// System interface extends with create method
export interface System {
  send: SendFunction;
  register: (actor: Actor) => Address;
  create: <TData>(factory: ActorFactory<TData>, data: TData) => Address;
}

// Usage
const system = System();
const task = system.create(TaskActor, {
  id: "task-1",
  status: "todo"
  // system injected automatically
});
```

**Key characteristics:**
- System is injected by the system
- Factory function is called by system
- Implicit dependency injection
- System controls creation lifecycle

### Implementation Sketch

```typescript
// System implementation with create()
export function System(): System {
  const actors = new Map<symbol, Actor>();

  const send: SendFunction = async (addr: Address, message: Message) => {
    const actor = actors.get(addr.__id);
    if (!actor) {
      return { success: false, error: "Actor not found" };
    }
    return actor.send(message);
  };

  const register = (actor: Actor): Address => {
    const id = Symbol();
    actors.set(id, actor);
    const address: Address = {
      __id: id,
      send: (message: Message) => send(address, message)
    };
    return address;
  };

  // New create method
  const create = <TData>(
    factory: ActorFactory<TData>,
    data: TData
  ): Address => {
    // Inject system into data automatically
    const dataWithSystem = { ...data, system: systemInstance };
    return factory(dataWithSystem);
  };

  const systemInstance: System = { send, register, create };
  return systemInstance;
}

// Actor factories would still accept system in data
export function TaskActor(data: {
  id: string;
  status: string;
  system: System;  // Still here, but injected
}): Address {
  const actor = {
    send: async (message: Message) => {
      // Implementation unchanged
    }
  };
  return data.system.register(actor);
}
```

### Trade-offs Analysis

#### Explicitness vs Implicitness

| Current (Explicit) | Alternative (Implicit) |
|-------------------|----------------------|
| `TaskActor({ id, system })` | `system.create(TaskActor, { id })` |
| System is visible in call | System is hidden/injected |
| Clear data dependencies | Magic parameter injection |
| "Data flows explicitly" | "Framework controls creation" |

**Current wins on:** Functional programming principles (explicit dependencies)
**Alternative wins on:** Reduced boilerplate, DRY principle

#### Type Safety

**Current approach:**
```typescript
// Type errors caught immediately
const task = TaskActor({
  id: "task-1",
  // TypeScript error: missing 'system' property
});

// All parameters explicit in type signature
type TaskData = {
  id: string;
  status: string;
  system: System;
};
```

**Alternative approach:**
```typescript
// Type safety requires more complex generics
const task = system.create(TaskActor, {
  id: "task-1"
  // System is magic - TypeScript can't easily verify
  // the factory actually expects it
});

// Type signature becomes more complex
type ActorFactory<TData> = (
  data: TData & { system: System }
) => Address;

// Or split into two types:
type UserData<TData> = Omit<TData, 'system'>;
type FullData<TData> = TData & { system: System };
```

**Winner:** Current approach has simpler, more straightforward type safety.

#### IDE Support & Developer Experience

**Current approach:**
```typescript
// IDE autocomplete shows:
TaskActor({
  id: string,
  status: string,
  system: System  // ← clearly visible
})

// Jump to definition shows full signature
```

**Alternative approach:**
```typescript
// IDE autocomplete shows:
system.create(
  factory: ActorFactory<TData>,
  data: TData  // ← system is hidden
)

// Less clear what factory expects
// Need to jump into factory definition to see full data shape
```

**Winner:** Current approach provides clearer IDE feedback.

#### Consistency with Patterns

**Functional programming lens:**
- **Current:** Pure function, all dependencies explicit → ✅ Aligns with FP
- **Alternative:** Framework magic, implicit injection → ❌ Less FP, more OOP/DI

**Dependency injection lens:**
- **Current:** Constructor injection at call site → ✅ Explicit DI
- **Alternative:** Framework-managed injection → ✅ Standard DI container pattern

**Actor model lens:**
- **Current:** Actor factories are independent, composable → ✅ Actors don't depend on creation context
- **Alternative:** Actors coupled to System creation → ⚠️ System becomes required framework

#### Framework vs Library Philosophy

**Current approach: Library**
- Actors are just functions that return Addresses
- Can be created anywhere, anytime
- System is a utility, not a framework
- Testable without mocking: `TaskActor({ id: "test", system: testSystem })`

**Alternative approach: Framework**
- System controls creation lifecycle
- Central point of control for all actors
- More "frameworky" - you work within System's rules
- Testing requires system instance: `system.create(TaskActor, { id: "test" })`

**Considerations:**
- Is the project a lightweight toolkit or an opinionated framework?
- Do we want users to feel "in control" or "guided by structure"?

### Code Examples Side-by-Side

#### Creating Multiple Actors

**Current:**
```typescript
const system = System();

const task1 = TaskActor({ id: "task-1", status: "todo", system });
const task2 = TaskActor({ id: "task-2", status: "todo", system });
const task3 = TaskActor({ id: "task-3", status: "todo", system });

// Repetitive: 'system' appears 3 times
```

**Alternative:**
```typescript
const system = System();

const task1 = system.create(TaskActor, { id: "task-1", status: "todo" });
const task2 = system.create(TaskActor, { id: "task-2", status: "todo" });
const task3 = system.create(TaskActor, { id: "task-3", status: "todo" });

// Less repetitive, but less explicit
```

#### Passing Addresses Between Actors

**Current:**
```typescript
const system = System();

const worker = WorkerActor({ id: "worker", system });
const manager = ManagerActor({
  id: "manager",
  workers: [worker],
  system  // Last parameter
});

// Clear: all dependencies visible
```

**Alternative:**
```typescript
const system = System();

const worker = system.create(WorkerActor, { id: "worker" });
const manager = system.create(ManagerActor, {
  id: "manager",
  workers: [worker]
  // system is implicit
});

// Cleaner call site, but magic injection
```

#### Testing Scenarios

**Current:**
```typescript
// Unit test - easy to create isolated actors
test("TaskActor completes", () => {
  const mockSystem = MockSystem();  // Test double
  const task = TaskActor({ id: "test", status: "todo", system: mockSystem });

  // Test directly - no framework needed
});
```

**Alternative:**
```typescript
// Unit test - requires system instance
test("TaskActor completes", () => {
  const mockSystem = MockSystem();  // Test double
  const task = mockSystem.create(TaskActor, { id: "test", status: "todo" });

  // Slightly more coupled to System API
});
```

### Recommendations & Considerations

#### When Current Approach Excels
- **Functional codebases** - Explicit dependencies align with FP principles
- **Lightweight toolkits** - Don't want a framework, want composable utilities
- **Maximum clarity** - New developers see all dependencies immediately
- **Simple type safety** - Straightforward TypeScript signatures

#### When Alternative Approach Might Excel
- **Large codebases** - DRY matters more, reduce repetitive `system` passing
- **Framework mindset** - Users expect central control and lifecycle management
- **Consistent creation** - Want to guarantee all actors go through system.create()
- **Future extensibility** - System.create() could add hooks, logging, metrics

#### Hybrid Option?

Could we support both?

```typescript
// Both styles work
const task1 = TaskActor({ id: "task-1", system });           // Direct
const task2 = system.create(TaskActor, { id: "task-2" });    // Via system

// Implementation: create() just adds system and calls factory
```

**Pros:** Maximum flexibility, gradual adoption
**Cons:** Two ways to do the same thing (violates "one obvious way")

### Conclusion

The current explicit approach (`TaskActor({ system })`) aligns better with:
- Functional programming principles
- Explicit dependency injection
- Library philosophy over framework philosophy
- Simpler type safety
- Clearer developer experience

The alternative approach (`system.create(TaskActor, {})`) would provide:
- Less boilerplate in large codebases
- Central creation control
- More traditional DI container pattern
- Framework-style lifecycle management

**Neutral stance:** Both are valid design choices. The current approach was chosen for its explicitness and functional purity. The alternative would be valuable if the project evolves toward a more framework-oriented architecture or if creation boilerplate becomes a real pain point in practice.

The existence of this choice point is actually a sign of good design flexibility - the Address proxy pattern works with either creation style.
