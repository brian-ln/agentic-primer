# Actor Interface Reference (Hewitt Actor Model)

**Status:** Proposed (Migration to Hewitt semantics in progress)
**See:** [Migration Plan](MIGRATION_PLAN.md), [ADR: Hewitt Actor Model](docs/decisions/HEWITT_ACTOR_MODEL.md)

This document defines the actor interface requirements for tk-agents, aligned with the Hewitt Actor Model—the canonical formalism for actor systems used in Erlang, Akka, and Orleans.

## Overview

The Hewitt Actor Model (Carl Hewitt, 1973) defines actors as concurrent computational entities that:
1. **Receive messages** from addresses (public interface)
2. **Send messages** to other addresses (private, goes through System)
3. **Create new actors** (when needed)

**Key Principle:** Everything is an actor. The System itself is an actor that manages and routes messages to contained actors.

## Core Types

### Message
```typescript
interface Message {
  id: string;              // Unique message identifier
  type: string;            // Message type (e.g., "ping", "execute", "query")
  payload: unknown;        // Message content (type-specific)
  correlationId?: string;  // Optional: for matching responses to requests
  sender?: string;         // Optional: ID of actor or entity that sent this message
}
```

**Sender Field Usage:**
- Identifies who sent the message
- Automatically populated by System when routing messages
- Actors can set sender to their own ID when forwarding messages
- Useful for task coordination, multi-party protocols, and audit trails

### Response
```typescript
interface Response {
  success: boolean;        // Whether operation succeeded
  data?: unknown;          // Result data (if successful)
  error?: string | ActorError;  // Error message or structured error
  metadata?: {
    durationMs?: number;   // Execution time
    costUsd?: number;      // Cost (for agent actors)
    sessionId?: string;    // Session identifier (for stateful actors)
    [key: string]: unknown; // Additional metadata
  };
}
```

### StreamEvent (Optional)
```typescript
interface StreamEvent {
  type: "init" | "message" | "tool_use" | "tool_result" | "result" | "error";
  data: unknown;
  timestamp: Date;
}
```

## Actor Interface (NEW - Hewitt Semantics)

### Required Fields and Methods

| Field/Method | Type | Required | Description |
|--------------|------|----------|-------------|
| `id` | `string` | ✅ Yes | Unique actor identifier |
| `type` | `ActorType` | ✅ Yes | Either `"deterministic"` or `"agent"` |
| `receive(message)` | `(Message) => Promise<Response>` | ✅ Yes | **PUBLIC:** Receive incoming messages |

### Recommended Fields and Methods

| Field/Method | Type | Required | Description |
|--------------|------|----------|-------------|
| `stream(message)` | `(Message) => AsyncGenerator<StreamEvent, Response>` | ⚠️ Optional | Streaming interface for long-running operations |
| `start()` | `() => Promise<void>` | ⚠️ Optional | Initialize resources, called on registration |
| `stop()` | `() => Promise<void>` | ⚠️ Optional | Cleanup resources, called on unregistration |

### Interface Definition

```typescript
export interface Actor {
  readonly id: string;
  readonly type: ActorType;

  // PUBLIC: Receive incoming messages (semantically correct!)
  receive(message: Message): Promise<Response>;

  // Optional: streaming interface for long-running operations
  stream?(message: Message): AsyncGenerator<StreamEvent, Response>;

  // Lifecycle
  start?(): Promise<void>;
  stop?(): Promise<void>;
}
```

## BaseActor (For Actors That Send to Others)

If your actor needs to send messages to other actors, extend `BaseActor` instead of implementing `Actor` directly:

```typescript
export abstract class BaseActor implements Actor {
  readonly id: string;
  readonly type: ActorType;

  protected system?: System;

  constructor(id: string, type: ActorType) {
    this.id = id;
    this.type = type;
  }

  // System injects itself during registration
  setSystem(system: System): void {
    this.system = system;
  }

  // Implement this in your actor
  abstract receive(message: Message): Promise<Response>;

  // PROTECTED: Send to other actors (goes through System)
  protected async send(targetId: string, message: Message): Promise<Response> {
    if (!this.system) {
      throw new Error(`Actor ${this.id} not registered with System`);
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

**Why Protected `send()`?**
- Actors should not expose `send()` as public API (confusing semantics)
- Public interface is `receive()` - what the actor does with incoming messages
- Private/protected `send()` - how actor sends to others (through System)

## System: The Actor That Manages Actors

The System is itself an actor that contains and routes messages to other actors:

```typescript
export class System implements Actor {
  readonly id = "system";
  readonly type = "deterministic" as const;

  private actors: Map<string, Actor> = new Map();

  // PUBLIC: System receives messages from outside
  async receive(message: Message): Promise<Response> {
    switch (message.type) {
      case 'route':
        // Route message to contained actor
        const { targetId, message: innerMessage } = message.payload;
        return this.routeToActor(targetId, innerMessage);

      case 'register':
        // Register new actor
        return this.registerActor(message.payload.actor);

      case 'unregister':
        // Unregister actor
        return this.unregisterActor(message.payload.actorId);

      case 'list':
        // List all actors
        return this.listActors();

      case 'ping':
        // System responds to ping
        return { success: true, data: { alive: true, timestamp: Date.now() } };

      default:
        return { success: false, error: `Unknown message type: ${message.type}` };
    }
  }

  // PRIVATE: Route to contained actor
  private async routeToActor(actorId: string, message: Message): Promise<Response> {
    const actor = this.actors.get(actorId);
    if (!actor) {
      return { success: false, error: `Actor not found: ${actorId}` };
    }

    // Call actor's receive() method
    return actor.receive(message);
  }

  // PRIVATE: Registration management
  private registerActor(actor: Actor): Response {
    if (this.actors.has(actor.id)) {
      return { success: false, error: `Actor already registered: ${actor.id}` };
    }

    this.actors.set(actor.id, actor);

    // If actor extends BaseActor, inject System reference
    if ('setSystem' in actor) {
      (actor as BaseActor).setSystem(this);
    }

    // Call actor's start() if it exists
    if (actor.start) {
      actor.start();
    }

    return { success: true, data: { actorId: actor.id } };
  }
}
```

**System is an Actor:**
- Has `id`, `type`, and `receive()` method
- Can be nested in other Systems (supervision trees!)
- Uniform composition: no special privileged entities

## Actor Type Guidelines

### Deterministic Actors (`type: "deterministic"`)

**Characteristics:**
- Predictable, repeatable behavior
- Same input always produces same output
- Typically stateless or with well-defined state

**Requirements:**
- Must handle `ping` messages (for heartbeat monitoring)
- Should return quickly (< 5 seconds typical)
- Should use metadata for execution timing

**Examples:**
- BashActor: Executes shell commands
- API clients: HTTP requests
- Database queries
- File operations

### Agent Actors (`type: "agent"`)

**Characteristics:**
- Non-deterministic decision-making
- May involve LLMs, human input, or complex reasoning
- Often stateful (sessions, conversations)

**Requirements:**
- Must handle `ping` messages (for heartbeat monitoring)
- Should implement `stream()` for long-running operations
- Should track session IDs in metadata
- May track cost/usage metrics

**Examples:**
- ClaudeActor: Claude CLI with resumable sessions
- HumanActor: Human decision-making
- Multi-step reasoning agents
- Planning and orchestration agents

## Standard Message Types

### Ping (Heartbeat)

All actors must handle `ping` messages:

```typescript
async receive(message: Message): Promise<Response> {
  if (message.type === 'ping') {
    return {
      success: true,
      data: { alive: true, timestamp: Date.now() },
    };
  }

  // ... your actor logic
}
```

### Custom Message Types

Define your own message types based on actor capabilities:

```typescript
// BashActor: "execute" type
{ type: "execute", payload: "ls -la" }
{ type: "execute", payload: { command: "git status", args: [] } }

// ClaudeActor: message payload is the prompt
{ type: "query", payload: "Explain this code" }

// Custom actor types
{ type: "translate", payload: { text: "Hello", targetLang: "es" } }
{ type: "analyze", payload: { data: [...] } }
```

## Minimal Implementation Checklist

To implement a minimal actor:

1. ✅ Implement the `Actor` interface
2. ✅ Define `id: string` and `type: ActorType`
3. ✅ Implement `receive(message: Message): Promise<Response>` (NOT `send()`!)
4. ✅ Handle `ping` messages
5. ✅ Return structured `Response` objects
6. ⚠️ Optional: Extend `BaseActor` if you need to send to other actors
7. ⚠️ Optional: Implement `stream()` for long-running operations
8. ⚠️ Optional: Implement `start()`/`stop()` for resource management

## Example: Simple Deterministic Actor

```typescript
export class EchoActor implements Actor {
  readonly id: string;
  readonly type = "deterministic" as const;

  constructor(id: string) {
    this.id = id;
  }

  async receive(message: Message): Promise<Response> {
    // Required: handle ping
    if (message.type === 'ping') {
      return {
        success: true,
        data: { alive: true, timestamp: Date.now() },
      };
    }

    // Echo the payload back
    return {
      success: true,
      data: { echo: message.payload, receivedFrom: message.sender },
    };
  }
}
```

## Example: Actor That Sends to Others

```typescript
export class ChainActor extends BaseActor {
  constructor(id: string, private nextActorId: string) {
    super(id, "deterministic");
  }

  async receive(message: Message): Promise<Response> {
    if (message.type === 'ping') {
      return { success: true, data: { alive: true, timestamp: Date.now() } };
    }

    // Process message
    const processed = `[${this.id}] processed: ${message.payload}`;

    // Send to next actor in chain
    const response = await this.send(this.nextActorId, {
      id: crypto.randomUUID(),
      type: 'process',
      payload: processed,
      sender: this.id
    });

    return response;
  }
}
```

## Integration with System

### Registration

```typescript
import { System } from "./actors/system";
import { EchoActor } from "./echo-actor";

const system = new System();

// Register actor
const actor = new EchoActor("echo-1");
await system.receive({
  id: "reg-1",
  type: 'register',
  payload: { actor }
});
```

### Message Routing

```typescript
// Send message to actor through System
const response = await system.receive({
  id: "msg-1",
  type: 'route',
  payload: {
    targetId: "echo-1",
    message: {
      id: "msg-2",
      type: "process",
      payload: "Hello, actor!"
    }
  }
});
```

### Heartbeat Monitoring

```typescript
// System can ping actors
const pingResponse = await system.receive({
  id: "ping-1",
  type: 'route',
  payload: {
    targetId: "echo-1",
    message: { id: "ping-2", type: 'ping', payload: {} }
  }
});
```

### Cleanup

```typescript
// Unregister actor (calls stop() if defined)
await system.receive({
  id: "unreg-1",
  type: 'unregister',
  payload: { actorId: "echo-1" }
});
```

## Error Handling

### Legacy String Errors
```typescript
return {
  success: false,
  error: "Something went wrong",
};
```

### Structured Errors (Recommended)
```typescript
import { validationError, transientError, permanentError } from "./actors/errors";

return {
  success: false,
  error: validationError("Invalid input", { field: "command" }),
};

return {
  success: false,
  error: transientError("Network timeout", originalError, { retryAfter: 5000 }),
};
```

## Best Practices

1. **Semantic Correctness**
   - Use `receive()` for incoming messages (public interface)
   - Use protected `send()` for outgoing messages (private implementation)
   - Never expose `send()` as public API

2. **Location Transparency**
   - Actors communicate via IDs/addresses, not direct references
   - System can relocate actors, implement remote actors later
   - Easier testing and mocking

3. **Uniform Composition**
   - System is an actor, can be nested in other Systems
   - Enables supervision trees (like Erlang/OTP)
   - No special privileged entities

4. **Single Responsibility**
   - Each actor should do one thing well
   - Compose actors to build complex behavior

5. **Type Safety**
   - Use TypeScript interfaces for message payloads
   - Define clear message type contracts

6. **Error Handling**
   - Always return structured responses, never throw
   - Use structured errors for better error handling

7. **Metadata**
   - Include timing, cost, and session info in responses
   - Helps with monitoring and debugging

8. **Idempotency**
   - Design actors to handle duplicate messages gracefully
   - Use message IDs and correlationIds for deduplication

9. **Timeouts**
   - Implement timeouts for long-running operations
   - Return timeout errors instead of hanging

10. **Cleanup**
    - Implement `stop()` to release resources
    - System calls `stop()` when unregistering actors

## Migration from Old API

**Old API (WRONG - Semantically Confusing):**
```typescript
interface Actor {
  send(message: Message): Promise<Response>;  // CONFUSING!
}

const response = await actor.send(message);
await registry.send(actorId, message);
```

**New API (CORRECT - Hewitt Semantics):**
```typescript
interface Actor {
  receive(message: Message): Promise<Response>;  // CLEAR!
}

const response = await actor.receive(message);
const response = await system.receive({
  type: 'route',
  payload: { targetId: actorId, message }
});
```

**See [Migration Plan](MIGRATION_PLAN.md) for detailed upgrade guide.**

## See Also

- [Migration Plan](MIGRATION_PLAN.md) - Phased migration strategy
- [ADR: Hewitt Actor Model](docs/decisions/HEWITT_ACTOR_MODEL.md) - Decision rationale
- [base.ts](./src/actors/base.ts) - Core interface definitions
- [system.ts](./src/actors/system.ts) - System implementation (proposed)
- [examples/hewitt-model-example.ts](./examples/hewitt-model-example.ts) - New examples (proposed)
- [Hewitt's Original Paper (1973)](https://doi.org/10.1145/1624775.1624804)
- [Erlang Actor Model](https://www.erlang.org/doc/getting_started/conc_prog.html)
- [Akka Actor Reference](https://doc.akka.io/docs/akka/current/typed/actors.html)
