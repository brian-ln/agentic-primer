# ADR: Migration to Hewitt Actor Model Semantics

**Status:** Proposed
**Date:** 2026-01-16
**Decision Makers:** Core Development Team

## Context

The tk-agents system implements an actor-based architecture, but the current implementation has semantic inconsistencies with the Hewitt Actor Model, which is the canonical formalism for actor systems (used successfully in Erlang, Akka, Orleans).

### Current Problems

1. **Semantically Incorrect Method Names**
   - `Actor.send(message)` actually RECEIVES a message (confusing!)
   - "send" implies the actor is sending something OUT, but it's actually the entry point for incoming messages

2. **Registry is Not an Actor**
   - Registry manages actors but cannot itself receive messages as an actor
   - Creates a special privileged entity outside the actor model
   - Breaks uniform composition (can't nest systems as actors)

3. **No Clear Public/Private Interface**
   - Actors call `registry.send()` directly to send to other actors
   - Creates implicit coupling to registry
   - No clear boundary between "receive from outside" and "send to other actors"

4. **Actor-to-Actor Direct Communication**
   - Current design allows actors to directly reference each other
   - Breaks location transparency principle
   - Makes testing and mocking harder

### Hewitt Actor Model Principles

The canonical actor model (Carl Hewitt, 1973) defines actors as:

1. **Concurrent computational entities** that communicate exclusively via asynchronous messages
2. **Three fundamental capabilities:**
   - Receive messages (from address)
   - Create new actors
   - Send messages (to addresses)
3. **Location transparency:** Actors communicate via addresses, not direct references
4. **Everything is an actor:** No privileged entities; systems are actors containing actors

## Decision

We will migrate to proper Hewitt Actor Model semantics through a phased, backward-compatible approach.

### Semantic Changes

| Current | New (Hewitt) | Rationale |
|---------|--------------|-----------|
| `Actor.send(message)` | `Actor.receive(message)` | Semantically correct: "receive incoming message" |
| `Registry` | `System extends Actor` | System IS an actor with `receive()` |
| `registry.send(actorId, message)` | Internal to System | System routes messages to contained actors |
| Actors call `registry.send()` | `Actor.send(address, message)` | Private method that goes through System |

### New Architecture

```typescript
// System is an actor that contains and manages other actors
export class System implements Actor {
  readonly id: string = "system";
  readonly type = "deterministic" as const;

  private actors: Map<string, Actor> = new Map();

  // Public: System receives messages from outside
  async receive(message: Message): Promise<Response> {
    if (message.type === 'route') {
      // Route to contained actor
      const { targetId, payload } = message.payload;
      return this.routeToActor(targetId, payload);
    }

    if (message.type === 'register') {
      // Register new actor
      return this.registerActor(message.payload);
    }

    // ... other system commands
  }

  // Private: internal routing to contained actors
  private async routeToActor(actorId: string, message: Message): Promise<Response> {
    const actor = this.actors.get(actorId);
    if (!actor) {
      return { success: false, error: `Actor not found: ${actorId}` };
    }
    return actor.receive(message);
  }
}

// Actors receive messages and can send through System
export interface Actor {
  readonly id: string;
  readonly type: ActorType;

  // Public: receive incoming messages
  receive(message: Message): Promise<Response>;

  // Optional lifecycle
  start?(): Promise<void>;
  stop?(): Promise<void>;
}

// Actors hold System reference for sending
export abstract class BaseActor implements Actor {
  protected system?: System;

  setSystem(system: System): void {
    this.system = system;
  }

  // Protected: send through System
  protected async send(targetId: string, message: Message): Promise<Response> {
    if (!this.system) {
      throw new Error("Actor not registered with System");
    }

    return this.system.receive({
      id: crypto.randomUUID(),
      type: 'route',
      payload: { targetId, message },
      sender: this.id
    });
  }
}
```

## Consequences

### Positive

1. **Semantic Correctness**
   - `receive()` accurately describes what actors do with incoming messages
   - Clear distinction between public interface (receive) and internal implementation (send)

2. **Uniform Composition**
   - System is an actor, can be nested in other systems
   - Enables hierarchical actor systems (like Erlang supervision trees)

3. **Better Testing**
   - Clear boundary between "incoming messages" and "outgoing sends"
   - Mock System easily to test actors in isolation

4. **Location Transparency**
   - Actors don't hold direct references to each other
   - System can relocate actors, implement remote actors later

5. **Alignment with Literature**
   - Matches Hewitt's formalism, Erlang, Akka patterns
   - Makes codebase easier to understand for experienced actor system developers

### Negative

1. **Breaking Change**
   - All actors must rename `send()` → `receive()`
   - All calls to `actor.send()` → `actor.receive()`
   - Registry singleton → System instance

2. **Migration Effort**
   - ~12 actor implementations to update
   - ~100 tests to update
   - All examples and documentation

3. **Temporary Verbosity**
   - During migration, may have both old and new APIs coexisting
   - Need deprecation warnings and shims

### Neutral

1. **No Performance Impact**
   - Purely semantic change, same underlying operations

2. **Opportunity for Related Improvements**
   - Could add supervision trees
   - Could add location transparency features
   - Could implement distributed actors

## Implementation Plan

See [MIGRATION_PLAN.md](../../MIGRATION_PLAN.md) for detailed phased migration strategy.

### Phase 1: Add New Interfaces (Backward Compatible)
- Add `receive()` method to Actor interface (alongside `send()`)
- Create System class (wrapping Registry)
- No breaking changes yet

### Phase 2: Update Implementations
- Update all actor implementations to use `receive()`
- Deprecate `send()` with warnings
- Update tests one module at a time

### Phase 3: Remove Old Interfaces
- Remove `send()` method from Actor interface
- Remove Registry singleton (keep as internal to System)
- Verify all tests pass

### Phase 4: Documentation and Examples
- Update all documentation
- Create new examples showing Hewitt model
- Add ADR to decision log

## Alternatives Considered

### Alternative 1: Keep Current Design
**Rejected:** Semantic confusion continues, makes onboarding harder, diverges from established patterns.

### Alternative 2: Rename to `handle()` Instead of `receive()`
**Rejected:** `handle()` is generic but less precise. `receive()` is the canonical term in actor literature (Erlang: `receive` block, Akka: `receive` method).

### Alternative 3: Big Bang Migration
**Rejected:** Too risky. Phased approach allows validation at each step and easier rollback if issues found.

### Alternative 4: Keep `send()` and `receive()` as Synonyms
**Rejected:** Maintains confusion. Clear semantics require choosing one term consistently.

## References

- [Hewitt, C., Bishop, P., & Steiger, R. (1973). A Universal Modular Actor Formalism for Artificial Intelligence](https://doi.org/10.1145/1624775.1624804)
- [Erlang Actor Model Documentation](https://www.erlang.org/doc/getting_started/conc_prog.html)
- [Akka Actor Reference](https://doc.akka.io/docs/akka/current/typed/actors.html)
- [Orleans Virtual Actors](https://learn.microsoft.com/en-us/dotnet/orleans/overview)
- Current implementation: [src/actors/base.ts](/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/actors/base.ts)
- Current registry: [src/actors/registry.ts](/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/actors/registry.ts)

## Decision Log

- **2026-01-16:** Proposed this ADR
- **TBD:** Review with team
- **TBD:** Approval decision
- **TBD:** Start implementation
