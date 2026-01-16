# Actor System Specification Summary

## The One True Spec

This is the complete, final specification for the actor system.

## Files

### 1. **ACTOR_SPEC.md** - Human-Readable Specification

Complete specification document covering:
- Core principles (pure functions, send primitive, system role)
- Type signatures
- Usage patterns
- Invariants
- Non-requirements

**Read this to understand the system.**

### 2. **ACTOR_SPEC.datalog** - Formal Verification Model

Datalog rules that programmatically verify:
- Pure function invariant (no hidden dependencies)
- Send injection invariant (all actors have send in scope)
- Send primitive invariant (send is the only messaging primitive)
- System send invariant (systems provide send implementation)
- Bridge invariant (external uses actor.send(), internal uses send())
- Composition invariant (systems can contain systems)
- Addressing invariant (implementation is opaque)

**Use this to verify implementations.**

## The Three Key Insights

### 1. Actors Are Pure Functions

```typescript
function Actor(
  data: ActorData,
  send: SendFunction  // ← Injected dependency
): Actor {
  return {
    send: async (message: Message) => {
      // All dependencies explicit
    }
  };
}
```

**No hidden state. No implicit context.**

### 2. Send Is The Only Primitive

```typescript
type SendFunction = (targetId: string, message: Message) => Promise<Response>;

// From actor's perspective
await send(targetId, message);
```

**Implementation irrelevant. Just call send().**

### 3. Two APIs For Bridging

```typescript
// External → Actor (bridge in)
await actor.send(message);

// Actor → Actor (inside actor world)
await send(targetId, message);
```

**actor.send() is the bridge. send() is the primitive.**

## Complete Example

```typescript
// 1. System provides send implementation
function System() {
  const actors = new Map<string, Actor>();

  const send: SendFunction = async (targetId, message) => {
    const actor = actors.get(targetId);
    if (!actor) throw new Error(`Actor not found: ${targetId}`);
    return actor.send(message);
  };

  return {
    send,
    register: (id: string, actor: Actor) => actors.set(id, actor)
  };
}

// 2. Actor receives send via injection
function TaskActor(
  data: TaskData,
  send: SendFunction  // ← Injected
): Actor {
  return {
    send: async (message: Message) => {
      if (message.type === "complete") {
        // Use send from scope
        await send("dependent-task", {
          id: crypto.randomUUID(),
          type: "unblock",
          payload: { unblockedBy: data.id }
        });
      }
      return { success: true };
    }
  };
}

// 3. Usage
const system = System();
const actor = TaskActor(
  { id: "task-1", status: "todo" },
  system.send  // ← Inject send
);
system.register("task-1", actor);

// 4. Bridge into actor world
await actor.send({
  id: "msg-1",
  type: "complete",
  payload: {}
});
```

## Verification

To verify an implementation against the spec:

### Manual Verification Checklist

- [ ] Actors are pure functions (all dependencies passed as parameters)
- [ ] Actors receive `send` function at creation
- [ ] `send` signature is `(targetId: string, message: Message) => Promise<Response>`
- [ ] System provides `send` implementation
- [ ] System maintains actor registry
- [ ] External code uses `actor.send(message)`
- [ ] Internal actor code uses `send(targetId, message)`
- [ ] Systems ARE actors (have `.send()` method)
- [ ] Systems can be registered in other systems

### Datalog Verification

Load `ACTOR_SPEC.datalog` and query:

```prolog
% Verify all invariants
?- all_invariants_hold.

% Check specific actor is well-formed
?- actor_well_formed("task-1").

% Check specific system is well-formed
?- system_well_formed("system").

% Find all actors with send in scope
?- send_in_scope(ActorID, SendFnID).

% Find all bridge calls
?- is_bridge_call(CallerID, ActorID, Message).
```

## What This Spec Does NOT Define

The spec intentionally leaves these as implementation details:

- ❌ Specific routing mechanism (registry vs direct refs vs network)
- ❌ Synchronous vs asynchronous delivery
- ❌ Mailbox implementation
- ❌ Supervision strategy
- ❌ Distribution mechanism
- ❌ Persistence approach

**The spec defines the interface, not the implementation.**

## Status

✅ **This is the complete, final specification.**

All previous explorations (MESSAGE_FLOWS.md, ACTOR_API_SCENARIOS.md, etc.) were design iterations. This spec supersedes them.

## Next Steps

1. **Implement** actors following this spec
2. **Verify** implementation using Datalog
3. **Test** with example systems (task graph, knowledge graph)
4. **Extend** with additional features (supervision, persistence, etc.) as separate concerns

The foundation is solid. Build on it.
