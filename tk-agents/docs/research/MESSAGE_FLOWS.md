# Message Flows - Modeling Actor Communication

## Erlang Reflection

In Erlang, the actor model is beautifully simple:

```erlang
% Spawn actor (returns PID - address)
Pid = spawn(fun() -> actor_loop(State) end),

% Send directly to PID (no intermediary!)
Pid ! {update_status, done},

% Actor receives
receive
  {update_status, Status} -> handle_update(Status)
end

% Process registry for name-based lookup
register(task_manager, Pid),
task_manager ! {create_task, Data}  % Send by name
```

**Key Erlang insights:**

1. **PIDs are addresses** - Every actor has a unique identifier
2. **Direct send** - `Pid ! Message` sends directly, no routing layer
3. **Actors hold PIDs** - Actors store references to other actors
4. **Process registry** - Optional name→PID lookup
5. **System/Supervisor** - Doesn't route messages, just manages lifecycle

**Erlang message flow:**
```
ActorA has Pid_B
    ↓
Pid_B ! Message  (direct send to PID)
    ↓
ActorB receives message
```

**OR via registry:**
```
ActorA
    ↓
registered_name ! Message  (send to name)
    ↓
Process Registry (lookup name → PID)
    ↓
ActorB receives message
```

## Our Current Model (Routing Heavy)

```typescript
// Current: Everything through System.route
await system.send({
  type: "route",
  payload: {
    targetId: "task-1",
    message: { type: "update" }
  }
});

// System handles ALL message routing
// Actors don't have references to each other
```

**Message flow:**
```
External Code
    ↓
System.send({ type: "route", payload: { targetId, message } })
    ↓
System looks up actor by targetId
    ↓
Actor.send(message)
    ↓
Actor receives message
```

**For actor-to-actor:**
```
ActorA
    ↓
system.send({ type: "route", payload: { targetId: "actor-b", message } })
    ↓
System looks up ActorB
    ↓
ActorB.send(message)
```

## Better Model (Erlang-Inspired)

### Actors Hold References

```typescript
// Actor can hold references to other actors directly
function TaskActor(data: TaskData, dependencies: Map<string, Actor>) {
  return {
    send: async (message: Message) => {
      if (message.type === "updateStatus" && message.payload.status === "done") {
        // Notify dependents DIRECTLY (like Erlang's Pid !)
        for (const [depId, depActor] of dependencies) {
          await depActor.send({
            id: crypto.randomUUID(),
            type: "unblock",
            payload: { unblockedBy: data.id }
          });
        }
      }
    }
  };
}

// Creation: wire up references
const task1 = TaskActor(data1, new Map());
const task2 = TaskActor(data2, new Map([["task-1", task1]]));  // task2 knows about task1

// Send directly
await task1.send({ type: "updateStatus", payload: { status: "done" } });
// → task1 directly sends to task2, no system intermediary!
```

**Message flow:**
```
ActorA (has reference to ActorB)
    ↓
ActorB.send(message)  // Direct send!
    ↓
ActorB receives message
```

### System as Registry + Supervisor

```typescript
// System is a registry, not a router
class System {
  private registry: Map<string, Actor> = new Map();

  // Register actor by name
  register(name: string, actor: Actor): void {
    this.registry.set(name, actor);
  }

  // Lookup actor by name (like Erlang's whereis)
  lookup(name: string): Actor | undefined {
    return this.registry.get(name);
  }

  // Send by name (convenience - does lookup then send)
  async send(name: string, message: Message): Promise<Response> {
    const actor = this.registry.get(name);
    if (!actor) {
      throw new Error(`Actor not found: ${name}`);
    }
    return actor.send(message);
  }

  // NOT a message router - just a registry!
}

// Usage
const system = System();

const task1 = TaskActor(data1);
const task2 = TaskActor(data2);

system.register("task-1", task1);
system.register("task-2", task2);

// Option A: Lookup then send
const actor = system.lookup("task-1");
await actor.send(message);

// Option B: Send by name (convenience)
await system.send("task-1", message);
```

### Actors Can Choose Communication Style

```typescript
// Style 1: Direct references (fast, coupled)
function TaskActorDirect(data: TaskData, dependents: Actor[]) {
  return {
    send: async (message) => {
      // Send to dependents directly
      for (const dep of dependents) {
        await dep.send({ type: "notify" });
      }
    }
  };
}

// Style 2: Via registry (slower, decoupled)
function TaskActorRegistry(data: TaskData, system: System, dependentIds: string[]) {
  return {
    send: async (message) => {
      // Lookup and send
      for (const depId of dependentIds) {
        const dep = system.lookup(depId);
        if (dep) {
          await dep.send({ type: "notify" });
        }
      }
    }
  };
}

// Style 3: Hybrid (fast common paths, registry for discovery)
function TaskActorHybrid(data: TaskData, system: System) {
  // Cache frequently-used actors
  const dependentCache: Map<string, Actor> = new Map();

  return {
    send: async (message) => {
      for (const depId of data.dependencies) {
        // Check cache first
        let dep = dependentCache.get(depId);

        // Lookup if not cached
        if (!dep) {
          dep = system.lookup(depId);
          if (dep) {
            dependentCache.set(depId, dep);
          }
        }

        if (dep) {
          await dep.send({ type: "notify" });
        }
      }
    }
  };
}
```

## Message Flow Diagrams

### Flow 1: External → System → Actor

```
External TypeScript Code
    ↓
    taskGraph.updateTask("task-1", { status: "done" })
    ↓
TaskGraph (domain layer)
    ↓
    system.send("task-1", { type: "update", payload })
    ↓
System (registry/lookup)
    ↓
    actor = registry.get("task-1")
    actor.send(message)
    ↓
TaskActor
    ↓
    receive(message)
    ↓
    return response
```

### Flow 2: Actor → Actor (Direct Reference)

```
ActorA
    ↓
    dependentActor.send({ type: "unblock" })  // Direct!
    ↓
ActorB
    ↓
    receive(message)
```

### Flow 3: Actor → Actor (Via System Registry)

```
ActorA
    ↓
    system.lookup("actor-b")
    ↓
System Registry
    ↓
    return ActorB reference
    ↓
ActorA
    ↓
    actorB.send({ type: "notify" })
    ↓
ActorB
    ↓
    receive(message)
```

### Flow 4: Actor → System → Actor

```
ActorA
    ↓
    system.send("actor-b", { type: "notify" })
    ↓
System
    ↓
    actor = registry.get("actor-b")
    actor.send(message)
    ↓
ActorB
    ↓
    receive(message)
```

## Recommendation: Erlang-Style Direct References

```typescript
// System is a registry, not a router
interface System {
  register(name: string, actor: Actor): void;
  unregister(name: string): void;
  lookup(name: string): Actor | undefined;
  send(name: string, message: Message): Promise<Response>;  // Convenience
  list(): string[];
}

// Actors can choose:
// 1. Hold direct references (fast, like Erlang PIDs)
// 2. Use system.lookup() (dynamic)
// 3. Use system.send() (convenience)

function TaskActor(
  data: TaskData,
  system: System,
  dependentRefs: Actor[] = []  // Direct references
) {
  return {
    send: async (message: Message) => {
      if (message.type === "complete") {
        // Direct send (like Erlang Pid !)
        for (const dep of dependentRefs) {
          await dep.send({ type: "unblock", payload: { ... } });
        }
      }
    }
  };
}

// OR lookup dynamically
function TaskActorDynamic(
  data: TaskData,
  system: System,
  dependentIds: string[] = []  // IDs, not references
) {
  return {
    send: async (message: Message) => {
      if (message.type === "complete") {
        // Lookup then send (like Erlang whereis + !)
        for (const depId of dependentIds) {
          const dep = system.lookup(depId);
          if (dep) {
            await dep.send({ type: "unblock" });
          }
        }
      }
    }
  };
}
```

## Does Our Datalog Reflect This?

**Our current Datalog talks about "routing"** but should talk about:
1. **Direct sends** (actor has reference, sends directly)
2. **Registry lookup** (system.lookup then send)
3. **Named sends** (system.send as convenience)

Let me update the Datalog to reflect the Erlang-inspired design!
