# Actor/Program Integration Notes

This document explains how the actor/program domain integrates with the existing WIT infrastructure from Phase 1.

## Dependencies

### Phase 1 Infrastructure

The actor/program domain builds on these existing packages:

#### 1. **agentic-primer:message@0.1.0** (Message Protocol)

The actor system uses the message protocol for all communication:

```wit
// From message/message.wit
interface message {
    record message-envelope {
        id: string,
        version: string,
        source: string,
        destination: string,
        message-type: string,
        payload: list<u8>,
        timestamp: string,
        correlation-id: option<string>,
        metadata: list<tuple<string, string>>,
    }
}
```

**Integration Points:**

- **Actor.receive()**: Accepts `message-envelope` from Phase 1
- **Actor.ask()**: Returns responses using correlation-id
- **Actor.tell()**: Sends `message-envelope` without expecting response
- **Router**: Uses existing router interface for message routing

**Mapping:**

```
Actor Message Pattern          Message Protocol
─────────────────────────────  ─────────────────────────────
tell (fire-and-forget)    →   message-envelope (no correlation-id)
ask (request-response)    →   message-envelope (with correlation-id)
stream (continuous)       →   multiple message-envelopes (same correlation-id)
```

#### 2. **convergence:domain-graph@0.1.0** (Graph Primitives)

Actors are addressed using graph addresses:

```wit
// From domain-graph/address.wit
interface address {
    record address {
        id: string,
        namespace: option<string>,
        scope: address-scope,
        version: option<string>,
    }
}
```

**Integration Points:**

- **Actor.get-address()**: Returns graph address
- **Actor.tell()**: Uses address for destination
- **Actor.ask-actor()**: Uses address for destination
- **Program.get-address()**: Programs have graph addresses

**Address Format:**

```
@(id)                    → Simple actor address
@(namespace/id)          → Namespaced actor
@(tool-bash)             → Tool actor
@(api-service)           → API program
@(inference-claude)      → Inference endpoint
```

#### 3. **convergence:domain-entity@0.1.0** (Entity System)

Programs are entities with lifecycle management:

```wit
// From domain-entity/entity.wit
enum entity-kind {
    program,  // ← Programs are entities
    // ...
}

enum entity-lifecycle {
    draft,
    published,
    deprecated,
}
```

**Integration Points:**

- **Program**: Implements entity semantics
- **program-state**: Maps to entity-lifecycle
- **program-metadata**: Extends entity-metadata
- **ProgramManager**: Uses entity operations

**Lifecycle Mapping:**

```
Program State      Entity Lifecycle
─────────────────  ─────────────────
draft          →   draft
published      →   published
deprecated     →   deprecated
```

#### 4. **agentic-primer:types@0.1.0** (Shared Types)

Uses shared error handling and types:

```wit
// From types/types.wit
interface types {
    record error-info {
        code: string,
        message: string,
        severity: error-severity,
        category: error-category,
        // ...
    }
}
```

**Integration Points:**

- **error-info**: All operations return `result<T, error-info>`
- **error-category**: Uses existing categories (routing, timeout, etc.)
- **retry-info**: Inherited from types
- **health-check-result**: Used for actor health

## Implementation Architecture

### Layering

The actor/program domain sits between the infrastructure and application layers:

```
┌─────────────────────────────────────────────────┐
│         Application Layer                       │
│  (Convergence, Simplify, User Code)           │
└─────────────────────────────────────────────────┘
                    ▲
                    │ Uses actors/programs
                    │
┌─────────────────────────────────────────────────┐
│      Actor/Program Domain (This Package)       │
│  • Actor system and messaging                   │
│  • Program execution and lifecycle              │
│  • Tool actors                                  │
│  • Inference actors                             │
└─────────────────────────────────────────────────┘
                    ▲
                    │ Built on
                    │
┌─────────────────────────────────────────────────┐
│         Infrastructure Layer (Phase 1)          │
│  • Message protocol (message.wit)               │
│  • Graph primitives (domain-graph/*.wit)        │
│  • Entity system (domain-entity/*.wit)          │
│  • Shared types (types/types.wit)               │
└─────────────────────────────────────────────────┘
```

### Message Flow Integration

How actor messages flow through the Phase 1 infrastructure:

```
┌────────────────────────────────────────────────────────┐
│  Actor System (domain-actor)                           │
│                                                        │
│  actor.ask(@(target), type, payload)                   │
│         │                                              │
│         ▼                                              │
│  Create message-envelope {                             │
│    source: @(self),                                    │
│    destination: @(target),                             │
│    message-type: type,                                 │
│    payload: serialize(payload),                        │
│    correlation-id: generate-correlation-id()           │
│  }                                                     │
└────────────────────────────────────────────────────────┘
         │
         ▼
┌────────────────────────────────────────────────────────┐
│  Message Router (message/router.wit from Phase 1)      │
│                                                        │
│  1. Parse destination address                          │
│  2. Look up registered node                            │
│  3. Route message to node                              │
│  4. Track correlation for response                     │
└────────────────────────────────────────────────────────┘
         │
         ▼
┌────────────────────────────────────────────────────────┐
│  Target Actor (domain-actor)                           │
│                                                        │
│  actor.receive(message-envelope)                       │
│         │                                              │
│         ▼                                              │
│  Process message                                       │
│         │                                              │
│         ▼                                              │
│  Return response with same correlation-id              │
└────────────────────────────────────────────────────────┘
         │
         ▼
┌────────────────────────────────────────────────────────┐
│  Message Router (Phase 1)                              │
│                                                        │
│  Match correlation-id to pending request               │
│  Deliver response to waiting actor                     │
└────────────────────────────────────────────────────────┘
```

### Graph Integration

How actors integrate with the graph primitives:

```
┌─────────────────────────────────────────────────┐
│           Graph Store (domain-graph)            │
│                                                 │
│  Nodes:                                         │
│  ├─ @(task-123)     type: task                 │
│  ├─ @(session-456)  type: session              │
│  ├─ @(tool-bash)    type: program              │
│  └─ @(api-service)  type: program              │
│                                                 │
│  Edges:                                         │
│  ├─ @(task-123) → @(session-456) [contains]    │
│  └─ @(session-456) → @(api-service) [uses]     │
└─────────────────────────────────────────────────┘
         ▲                           ▲
         │ Stores actors             │ References actors
         │                           │
┌────────┴─────────┐       ┌────────┴──────────┐
│  Actor Registry  │       │  Program Manager  │
│  (domain-actor)  │       │  (domain-actor)   │
└──────────────────┘       └───────────────────┘
```

**Key Points:**

1. Actors are registered as graph nodes
2. Programs have type='program' in the graph
3. Actor addresses are graph addresses
4. Actor relationships are graph edges

### Entity Integration

How programs integrate with the entity system:

```
┌─────────────────────────────────────────────────┐
│      Entity System (domain-entity)              │
│                                                 │
│  entity-kind = program                          │
│  entity-lifecycle = draft/published/deprecated  │
│                                                 │
│  Provides:                                      │
│  • get-entity(addr) → entity                    │
│  • list-by-kind(program) → list<address>        │
│  • list-by-lifecycle(published) → list<addr>    │
└─────────────────────────────────────────────────┘
         ▲
         │ Implements
         │
┌────────┴─────────────────────────────────────────┐
│      Program Resource (domain-actor)             │
│                                                  │
│  program-state = draft/published/deprecated      │
│  program-metadata includes entity-metadata       │
│                                                  │
│  Extends entity with:                            │
│  • invoke(input) → result                        │
│  • get-implementation() → code                   │
│  • publish() → state transition                  │
└──────────────────────────────────────────────────┘
```

## Usage Examples

### Example 1: Tool Actor Registration

Shows integration of all layers:

```javascript
// 1. Create program entity (domain-entity)
const bashTool = await programManager.createProgram(
  'tool-bash',              // id
  'Bash Tool',              // name
  'async function(input) {...}', // implementation
  'javascript',             // runtime
  'subprocess'              // execution-mode
);

// 2. Publish program (entity lifecycle)
await bashTool.publish();

// 3. Register as actor (domain-actor)
actorSystem.registerActor('tool-bash', new ProgramActor('tool-bash', router));

// 4. Actor is now addressable via graph (domain-graph)
// @(tool-bash) resolves to the program actor

// 5. Other actors can message it (message protocol)
const result = await actor.ask(
  address('tool-bash'),     // graph address
  'execute',                // message type
  { command: 'ls -la' }     // payload
);
// Uses message-envelope from Phase 1
```

### Example 2: Program Using Actor Context

Shows how programs use the actor context to call other actors:

```javascript
// Create orchestrator program
const orchestrator = await programManager.createProgram(
  'orchestrator',
  'File Orchestrator',
  `async function(input) {
    // this.ask is from program-context interface
    // internally uses actor system and message protocol

    // 1. Read file via tool actor
    const content = await this.ask(
      '@(tool-read)',      // graph address (domain-graph)
      'read',              // message type
      { path: input.file } // payload
    );
    // → Creates message-envelope (Phase 1)
    // → Routes to @(tool-read) actor
    // → Returns response

    // 2. Process content
    const result = content.toUpperCase();

    // 3. Write result via tool actor
    await this.tell(
      '@(tool-write)',     // graph address
      'write',             // message type
      { path: input.output, content: result }
    );
    // → Fire-and-forget message

    return { success: true };
  }`,
  'javascript',
  'worker'
);

await orchestrator.publish();

// Invoke the orchestrator
const result = await orchestrator.invoke(
  { file: '/tmp/input.txt', output: '/tmp/output.txt' },
  30000
);
```

### Example 3: Streaming Integration

Shows streaming actor integration with message protocol:

```javascript
// Register streaming inference actor
const inferenceActor = new InferenceActor('inference-claude', router);
actorSystem.registerActor('inference-claude', inferenceActor);

// Stream request
const streamHandle = await streamingActor.streamRequest(
  address('inference-claude'),
  'generate',
  { prompt: 'Hello!' },
  100  // buffer size
);

// Consume stream events
while (!streamHandle.isComplete()) {
  const event = await streamHandle.next(1000);

  if (event) {
    // Each event is a stream-event from domain-actor
    // Internally uses message-envelope with same correlation-id
    console.log(event.payload);
  }
}
```

## Type Mappings

### Message Protocol → Actor System

```wit
// Phase 1: message-envelope
message-envelope {
    source: string,           → actor.from: address
    destination: string,      → actor.to: address
    message-type: string,     → message.type: string
    payload: list<u8>,        → message.payload: list<u8>
    correlation-id: option<string>, → for ask/stream patterns
}

// domain-actor: message-response
message-response {
    from: address,            ← message-envelope.source
    to: address,              ← message-envelope.destination
    correlation-id: string,   ← message-envelope.correlation-id
    success: bool,            ← added by actor
    payload: option<list<u8>>, ← message-envelope.payload
}
```

### Entity System → Program

```wit
// Phase 1: entity-lifecycle
entity-lifecycle {
    draft,
    published,
    deprecated,
}

// domain-actor: program-state
program-state {
    draft,       ← maps to entity-lifecycle.draft
    published,   ← maps to entity-lifecycle.published
    deprecated,  ← maps to entity-lifecycle.deprecated
}
```

### Graph Address → Actor Address

```wit
// Phase 1: address
address {
    id: string,              → used as actor ID
    namespace: option<string>, → optional actor namespace
    scope: address-scope,    → typically node scope
}

// domain-actor: actor uses address directly
actor {
    get-address: func() -> address,  ← returns Phase 1 address
}
```

## Error Handling Integration

All operations use Phase 1 error-info:

```wit
// Phase 1: error-info
record error-info {
    code: string,
    message: string,
    severity: error-severity,
    category: error-category,
    details: option<string>,
    timestamp: string,
    correlation-id: option<string>,
    retry-info: option<retry-info>,
}

// domain-actor: Uses error-info for all results
actor {
    receive: func(message) -> result<_, error-info>,
    ask: func(message, timeout) -> result<message-response, error-info>,
    tell: func(to, type, payload) -> result<_, error-info>,
}

program {
    invoke: func(input, timeout) -> result<invocation-result, error-info>,
    publish: func() -> result<_, error-info>,
}
```

**Error Categories Used:**

- **routing**: Actor not found, invalid address
- **timeout**: Message timeout, invocation timeout
- **validation**: Invalid input/output schema
- **protocol**: Invalid message format
- **resources**: Actor mailbox full, memory exceeded
- **unknown**: Unexpected errors

## Configuration Integration

Uses Phase 1 configuration types:

```wit
// Phase 1: config-value
variant config-value {
    string-val(string),
    int-val(s64),
    float-val(f64),
    bool-val(bool),
    json-val(string),
}

// domain-actor: actor-system.create-actor
create-actor: func(
    id: string,
    actor-type: node-type,
    config: list<tuple<string, string>>  ← simplified from config-value
) -> result<actor, error-info>
```

## Health Check Integration

Uses Phase 1 health types:

```wit
// Phase 1: node-health
record node-health {
    state: node-state,
    checked-at: string,
    messages-processed: u64,
    messages-failed: u64,
    details: option<string>,
}

// domain-actor: actor health
actor {
    health: func() -> node-health,  ← returns Phase 1 node-health
}
```

## Future Integration Points

### 1. Query System Integration

When domain-query is added:

```wit
// Future: Query actors via query system
query-executor.execute({
  match: { kind: "actor", type: "producer" },
  return: ["address", "metadata"]
})
→ list of producer actors
```

### 2. Knowledge Graph Integration

When domain-knowledge is added:

```wit
// Future: Actors as knowledge graph entities
knowledge.relate(@(actor-a), "communicates-with", @(actor-b))
knowledge.query("actors communicating with @(actor-a)")
```

### 3. Session Integration

Current sessions can use actors:

```wit
// Session actor (already exists in domain-entity)
session-actor.receive({
  type: "user-message",
  payload: { text: "Hello!" }
})
```

## Migration Path

For existing Simplify/UGS code:

### Before (Direct Program Invocation)

```javascript
const program = programManager.getProgram('tool-bash');
const result = await programManager.invokeProgram('tool-bash', { command: 'ls' });
```

### After (Actor-Based)

```javascript
const actor = actorSystem.getActor(address('tool-bash'));
const result = await actor.ask(
  createMessage(address('tool-bash'), 'execute', { command: 'ls' })
);
```

Both approaches are supported for backwards compatibility.

## Summary

The actor/program domain is fully integrated with Phase 1 infrastructure:

1. **Message Protocol**: All actor communication uses message-envelope
2. **Graph Primitives**: Actors use graph addresses (@(id))
3. **Entity System**: Programs are entities with lifecycle
4. **Shared Types**: Uses error-info, health, config types
5. **Routing**: Leverages existing router infrastructure

The integration is designed to be:
- **Transparent**: Existing code works without changes
- **Composable**: Actors can call other actors naturally
- **Type-Safe**: WIT ensures type safety across boundaries
- **Extensible**: New actor types can be added easily
