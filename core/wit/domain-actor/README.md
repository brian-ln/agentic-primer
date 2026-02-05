# Actor/Program Domain Package

This package provides actor and program primitives for the Convergence framework's Universal Graph System (UGS).

## Overview

The actor/program domain enables message-based communication and executable code on the graph. It provides two primary abstractions:

- **Actors**: Message-based communication primitives with uniform interface
- **Programs**: Executable code units that can be invoked with inputs to produce outputs

Together, these primitives enable tool use, API exposure, inference endpoints, and general computation on the graph using messaging and actor patterns.

## Key Concepts

### Actors

Actors are the fundamental communication primitive in the graph. Every actor:

- Has a unique **address** (@(id)) for addressing
- Communicates only via **messages** (no shared state)
- Encapsulates state and behavior
- Supports multiple communication patterns: tell, ask, stream

#### Communication Patterns

1. **Tell (Fire-and-Forget)**: Send a message without waiting for response
   - Use for notifications, events, side effects
   - No response expected

2. **Ask (Request-Response)**: Send a message and wait for response
   - Use for queries, commands that need confirmation
   - Returns response with correlation to request

3. **Stream**: Send a request and receive continuous updates
   - Use for real-time data (LLM tokens, live updates)
   - Backpressure and cancellation support

#### Actor Types

- **Producer**: Emits messages (e.g., event sources)
- **Consumer**: Processes messages (e.g., workers)
- **Relay**: Forwards messages (e.g., proxies)
- **Processor**: Transforms messages (e.g., adapters)
- **Hybrid**: Multiple roles

### Programs

Programs are executable units that run code in response to invocations. They are specialized actors with:

- **State machine**: draft → published → deprecated
- **Invocation semantics**: Input → Execute → Output
- **Schema validation**: Input/output schema enforcement
- **Actor context**: Can call other actors via ask/tell

#### Program Lifecycle

1. **Draft**: Program is being developed
   - Editable (can update implementation)
   - Not invokable
   - Can be modified or deleted

2. **Published**: Program is ready for production
   - Immutable (no further edits)
   - Invokable by other actors
   - Can only transition to deprecated

3. **Deprecated**: Program is obsolete
   - Still invokable for backwards compatibility
   - Not recommended for new use
   - Cannot transition back

#### Program Runtimes

- **JavaScript**: JS/TS code execution
- **Python**: Python code execution
- **WebAssembly**: WASM modules
- **Native**: Native binaries
- **Container**: Container images
- **Custom**: Custom runtimes

#### Execution Modes

- **Inline**: Execute in same process (fast, unsafe)
- **Worker**: Execute in worker thread (isolated, shared memory)
- **Subprocess**: Execute in subprocess (isolated, separate memory)
- **Container**: Execute in container (highly isolated)

## Use Cases

### Tool Use on the Graph

Tools are exposed as specialized actors with well-defined interfaces:

```wit
// Tool actor at @(tool-bash)
actor.receive({
  type: "execute",
  payload: { command: "ls -la" }
})
→ response: { stdout: "...", stderr: "", exitCode: 0 }
```

Examples of tool actors:
- `@(tool-bash)`: Execute shell commands
- `@(tool-read)`: Read files
- `@(tool-write)`: Write files
- `@(tool-http)`: Make HTTP requests

### API Exposure

Programs can expose APIs that are invoked via the actor system:

```wit
// Create API endpoint program
program = create-program(
  id: "api-user-service",
  implementation: "async function(input) { ... }",
  runtime: javascript,
  execution-mode: worker
)

// Invoke via actor messaging
actor.ask(@(api-user-service), "get-user", { id: 123 })
→ response: { id: 123, name: "Alice", ... }
```

### Inference Endpoints

Inference actors provide AI model endpoints:

```wit
// Register inference endpoint
register-inference(
  model-name: "claude-3-sonnet",
  endpoint: @(inference-anthropic)
)

// Run inference
infer({
  model: "claude-3-sonnet",
  input: { prompt: "Hello!" },
  stream: true
})
→ stream: [token, token, token, ..., done]
```

### Graph-Based Tool Use

Programs can use the actor context to call other actors/tools:

```javascript
// Inside a program implementation
async function(input) {
  // this.ask is injected from program-context
  const fileContent = await this.ask('@(tool-read)', 'read', {
    path: input.filePath
  });

  const result = processContent(fileContent);

  await this.tell('@(tool-write)', 'write', {
    path: input.outputPath,
    content: result
  });

  return { success: true };
}
```

## Architecture

### Actor System

The actor system manages actor lifecycle and message routing:

```
┌─────────────────────────────────────────────────┐
│           Actor System (Router)                 │
│                                                 │
│  ┌─────────────────────────────────────────┐  │
│  │         Actor Registry                  │  │
│  │  @(tool-bash) → BashToolActor          │  │
│  │  @(api-service) → ProgramActor         │  │
│  │  @(session-123) → SessionActor         │  │
│  └─────────────────────────────────────────┘  │
│                                                 │
│  Message Routing:                              │
│  • Tell: Fire-and-forget delivery             │
│  • Ask: Request-response with correlation     │
│  • Stream: Continuous updates                 │
└─────────────────────────────────────────────────┘
         ▲                           ▲
         │ messages                  │ messages
         │                           │
    ┌────┴────┐               ┌─────┴──────┐
    │  Actor  │               │   Actor    │
    │  @(a)   │────────────→  │   @(b)     │
    └─────────┘   message     └────────────┘
```

### Program Execution

Programs execute with an injected actor context:

```
┌─────────────────────────────────────────────────┐
│              Program Execution                  │
│                                                 │
│  1. Invocation Request                         │
│     input: { ... }                             │
│                                                 │
│  2. Validate Input Schema                      │
│     ✓ Input matches schema                     │
│                                                 │
│  3. Execute with Context                       │
│     context = {                                │
│       ask: (to, type, payload) => {...},       │
│       tell: (to, type, payload) => {...},      │
│       selfAddress: () => @(program-id),        │
│       log: (level, msg) => {...}               │
│     }                                           │
│     output = await fn.call(context, input)     │
│                                                 │
│  4. Validate Output Schema                     │
│     ✓ Output matches schema                    │
│                                                 │
│  5. Return Result                              │
│     { success: true, output: {...} }           │
└─────────────────────────────────────────────────┘
```

### Message Flow

Message routing through the actor system:

```
External Request
      │
      ▼
┌──────────────────┐
│   Router         │
│  (address parse) │
└──────────────────┘
      │
      ├─ @(tool-bash) ──→ BashToolActor.receive()
      │                        │
      │                        ▼
      │                   Execute command
      │                        │
      │                        ▼
      │                   Return response
      │
      ├─ @(program-id) ──→ ProgramManager.invoke()
      │                        │
      │                        ▼
      │                   Execute program code
      │                        │
      │                        ├─ this.ask(@(other)) ─┐
      │                        │                       │
      │                        │←──────────────────────┘
      │                        ▼
      │                   Return result
      │
      └─ @(session-id) ──→ SessionActor.receive()
                               │
                               ▼
                          Process message
                               │
                               ▼
                          Return response
```

## Integration with Existing Infrastructure

### Message Protocol (Phase 1)

The actor system builds on the message protocol from Phase 1:

- **message-envelope**: Core message structure
- **node-type**: Actor type classification
- **node-capabilities**: Actor capabilities
- **router**: Message routing infrastructure

### Graph Primitives

Actors use graph addresses for universal interconnectedness:

- **address**: @(id) addressing primitive
- **node**: Graph node abstraction
- **edge**: Relationships between actors

### Entity System

Programs are entities with lifecycle management:

- **entity-kind**: program is an entity kind
- **entity-lifecycle**: draft, published, deprecated states
- **entity-metadata**: Creation, modification tracking

## Examples

### Creating a Tool Actor

```javascript
// Bash tool actor
class BashToolActor extends Actor {
  async receive(message) {
    if (message.type !== 'execute') {
      return { success: false, error: 'Expected execute type' };
    }

    const { command } = message.payload;
    const { stdout, stderr } = await exec(command);

    return {
      success: true,
      payload: { stdout, stderr, exitCode: 0 }
    };
  }
}

// Register with actor system
actorSystem.registerActor('tool-bash', new BashToolActor());
```

### Creating a Program

```javascript
// Create API endpoint program
const program = await programManager.createProgram(
  'api-calculate',
  'Calculate API',
  `async function(input) {
    const { operation, a, b } = input.message;

    switch (operation) {
      case 'add': return a + b;
      case 'subtract': return a - b;
      case 'multiply': return a * b;
      case 'divide': return b !== 0 ? a / b : null;
      default: throw new Error('Unknown operation');
    }
  }`,
  { runtime: 'javascript', executionMode: 'worker' }
);

// Publish it
await program.publish();

// Invoke it
const result = await program.invoke(
  { operation: 'add', a: 5, b: 3 },
  30000 // timeout
);
// result: { success: true, output: 8 }
```

### Using Actor Context in Programs

```javascript
// Program that orchestrates multiple tools
const orchestratorProgram = await programManager.createProgram(
  'orchestrator',
  'File Processor',
  `async function(input) {
    const { filePath } = input.message;

    // Read file using tool actor
    const content = await this.ask('@(tool-read)', 'read', {
      path: filePath
    });

    // Process content
    const processed = content.toUpperCase();

    // Write result using tool actor
    await this.ask('@(tool-write)', 'write', {
      path: filePath + '.processed',
      content: processed
    });

    // Send notification (fire-and-forget)
    await this.tell('@(notification-service)', 'notify', {
      message: 'File processed successfully'
    });

    return { success: true, outputPath: filePath + '.processed' };
  }`,
  { runtime: 'javascript', executionMode: 'worker' }
);
```

### Streaming Inference

```javascript
// Stream tokens from LLM
const streamHandle = await inferenceActor.inferStream({
  model: 'claude-3-sonnet',
  input: { prompt: 'Write a haiku about graphs' },
  stream: true,
  timeoutMs: 60000
});

// Consume stream
while (!streamHandle.isComplete()) {
  const event = await streamHandle.next(1000);
  if (event && event.type === 'data') {
    console.log(event.payload); // Token chunk
  }
}
```

## Design Principles

1. **Universal Interconnectedness**: Everything is addressable via @(id)
2. **Message-Based Communication**: No shared state, only messages
3. **Actor Encapsulation**: State and behavior are private
4. **Composability**: Actors can call other actors
5. **Type Safety**: WIT provides static types for all interfaces
6. **Lifecycle Management**: Programs have explicit state machines
7. **Schema Validation**: Input/output schemas for programs
8. **Supervision**: Actors can be supervised for fault tolerance

## Future Extensions

- **Supervision Trees**: Hierarchical actor supervision
- **Actor Clustering**: Distributed actor systems
- **Hot Code Reload**: Update programs without downtime
- **Actor Persistence**: Durable actors with state recovery
- **Backpressure Control**: Flow control for high-throughput scenarios
- **Circuit Breakers**: Fault tolerance patterns
- **Actor Groups**: Broadcast to actor groups
- **Priority Queues**: Priority-based message processing

## Related Packages

- **agentic-primer:message@0.1.0**: Message protocol and routing
- **convergence:domain-graph@0.1.0**: Graph primitives and addressing
- **convergence:domain-entity@0.1.0**: Entity lifecycle and metadata
- **agentic-primer:types@0.1.0**: Shared types and utilities
