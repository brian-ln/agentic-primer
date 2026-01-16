# Actor System Specification

## Core Principles

### 1. Addresses Are First-Class Objects

Addresses are objects with both identity and behavior.

```typescript
type Address = {
  readonly __id: symbol;  // Internal unique identifier
  send: (message: Message) => Promise<Response>;
};
```

**Properties:**
- ✅ Unique identity (via internal symbol)
- ✅ Built-in `.send()` method for ergonomic messaging
- ✅ Can be passed as data between actors
- ✅ Type-safe (no magic strings)

### 2. Actor Factories Return Addresses

Actor factories create actors, register them, and return addresses directly.

```typescript
function TaskActor(data: {
  id: string;
  status: string;
  system: System;
}): Address {
  const actor = {
    send: async (message: Message) => {
      // Actor logic - uses data.system.send() to communicate
    }
  };

  return data.system.register(actor);  // Returns Address
}
```

**Properties:**
- ✅ Single call creates and registers
- ✅ System injected via data
- ✅ Returns Address for immediate use
- ✅ No magic strings anywhere

### 3. Two Ways to Send Messages

Both ergonomic and explicit sending work:

```typescript
// Ergonomic: Use address's .send() method
await task.send({ type: "complete", payload: {} });

// Explicit: Use system.send() with address
await system.send(task, { type: "complete", payload: {} });
```

**Both are equivalent** - address.send() just calls system.send() internally.

### 4. System Provides Infrastructure

The System provides the messaging infrastructure.

```typescript
interface System {
  send: (targetAddress: Address, message: Message) => Promise<Response>;
  register: (actor: Actor) => Address;
}

function System(): System {
  const actors = new Map<symbol, Actor>();

  const send = async (addr: Address, message: Message) => {
    const actor = actors.get(addr.__id);
    if (!actor) return { success: false, error: "Actor not found" };
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

  return { send, register };
}
```

**System responsibilities:**
- Maintain actor registry (symbol → actor mapping)
- Provide send implementation
- Generate unique addresses with .send() methods

### 5. Pure Functions with Explicit Dependencies

Actors remain pure functions - all dependencies in data object.

```typescript
function TaskActor(data: {
  id: string;
  status: string;
  system: System;        // Required dependency
  dependents?: Address[];  // Optional: references to other actors
}): Address {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "complete") {
        // Use system from data
        if (data.dependents) {
          for (const dep of data.dependents) {
            await dep.send({ type: "unblocked" });
          }
        }
      }
      return { success: true };
    }
  };

  return data.system.register(actor);
}
```

**Properties:**
- ✅ All dependencies explicit in data parameter
- ✅ No hidden state or context
- ✅ Testable (mock system)
- ✅ Can hold references to other addresses

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

// Address - first-class object with identity and behavior
type Address = {
  readonly __id: symbol;
  send: (message: Message) => Promise<Response>;
};

// Actor interface
interface Actor {
  send: (message: Message) => Promise<Response>;
}

// SendFunction
type SendFunction = (
  targetAddress: Address,
  message: Message
) => Promise<Response>;

// ActorFactory - returns Address, not Actor
type ActorFactory<TData> = (data: TData) => Address;

// System
interface System {
  send: SendFunction;
  register: (actor: Actor) => Address;
}
```

## Usage Patterns

### Pattern 1: Create Actor

```typescript
const system = System();

// Factory creates, registers, returns Address
const task = TaskActor({
  id: "task-1",
  status: "todo",
  system
});

// task IS an Address with .send()
```

### Pattern 2: Send Messages (Ergonomic)

```typescript
// Use address's .send() method
const result = await task.send({
  id: "msg-1",
  type: "complete",
  payload: {}
});
```

### Pattern 3: Send Messages (Explicit)

```typescript
// Use system.send() with address
const result = await system.send(task, {
  id: "msg-1",
  type: "complete",
  payload: {}
});
```

### Pattern 4: Actor-to-Actor References

```typescript
// Create tasks
const task1 = TaskActor({ id: "task-1", status: "todo", system });
const task2 = TaskActor({
  id: "task-2",
  status: "blocked",
  system,
  dependents: [task1]  // Pass address directly!
});

// task2 can send to task1 directly
function TaskActor(data: { dependents?: Address[]; system: System }): Address {
  const actor = {
    send: async (message: Message) => {
      if (message.type === "complete" && data.dependents) {
        for (const dep of data.dependents) {
          await dep.send({ type: "unblocked" });  // Ergonomic!
        }
      }
      return { success: true };
    }
  };
  return data.system.register(actor);
}
```

### Pattern 5: Passing Addresses as Messages

```typescript
// Send address in message payload
await system.send(coordinator, {
  id: "msg-1",
  type: "register_task",
  payload: { taskAddr: task1 }  // Address as data
});

// Coordinator can send to received address
function CoordinatorActor(data: { system: System }): Address {
  const tasks: Address[] = [];

  const actor = {
    send: async (message: Message) => {
      if (message.type === "register_task") {
        const { taskAddr } = message.payload as { taskAddr: Address };
        tasks.push(taskAddr);

        // Can send to it later
        await taskAddr.send({ type: "status" });
      }
      return { success: true };
    }
  };
  return data.system.register(actor);
}
```

## Invariants

### 1. Address Identity Invariant
```
∀ address: Address
  ⟹ address.__id is unique symbol
  ⟹ address.__id never changes
```

### 2. Address Send Invariant
```
∀ address: Address
  ⟹ address.send(msg) ≡ system.send(address, msg)
  ⟹ Both produce identical results
```

### 3. Factory Returns Address Invariant
```
∀ factory: ActorFactory
  ⟹ factory(data) returns Address
  ⟹ Address is already registered in system
```

### 4. System Dependency Invariant
```
∀ factory: ActorFactory
  ⟹ data parameter includes system: System
  ⟹ System used for registration
```

### 5. No Magic Strings Invariant
```
∀ code using actor system
  ⟹ No string literals used as addresses
  ⟹ Only Address objects used for targeting
```

## Non-Requirements

What this spec **does NOT** require:

- ❌ Virtual actors (can be added but not required)
- ❌ Automatic activation/deactivation
- ❌ Serializable addresses (symbols are runtime-only)
- ❌ Mailboxes
- ❌ Supervision
- ❌ Distribution
- ❌ Persistence

The spec defines the **interface**, not the implementation.

## Verification

### Manual Verification Checklist

Use this checklist to verify an implementation against the spec:

- [ ] Address is object with `__id: symbol` and `send: (msg) => Promise<Response>`
- [ ] Actor factories take data with `system: System` as required field
- [ ] Actor factories return `Address` (not `Actor`)
- [ ] `system.register(actor)` returns `Address`
- [ ] `address.send(msg)` works (ergonomic API)
- [ ] `system.send(address, msg)` works (explicit API)
- [ ] Both send styles produce identical results
- [ ] No string literals used as addresses
- [ ] Addresses can be passed in message payloads
- [ ] Addresses can be stored in actor data

### Datalog Verification

Load `ACTOR_SYSTEM.spec.datalog` and run these queries:

```prolog
% Verify all invariants hold
?- all_invariants_hold.

% Check address has identity and send
?- address_well_formed(Addr).

% Check factory returns address
?- factory_returns_address(FactoryID).

% Verify no magic strings used
?- no_magic_strings.
```

## Summary

**Key insights:**

1. **Addresses are first-class objects** - Have identity (__id) and behavior (.send())
2. **Factories return addresses** - Single call creates, registers, and returns reference
3. **Two send styles** - Ergonomic (addr.send()) and explicit (system.send(addr, msg))
4. **No magic strings** - Type-safe addresses eliminate typos
5. **System in data** - Pure functions with explicit dependencies

This is the complete, final specification.
