# Pure Actor Model Architecture with Location Transparency

**Version:** 2.0
**Status:** Design Specification
**Date:** 2026-01-18
**Context:** Evolution from hybrid actor/HTTP to pure actor model with location transparency
**Parent Designs:** CLI_ACTOR_ARCHITECTURE.md (agent a18ee7e), ACTOR_MODEL_GUIDE.md, HEWITT_ACTOR_MODEL.md

---

## Executive Summary

### Vision

**User's Insight:**
> "What if you modeled the task creation, update, delete, etc and the knowledge operations, and even the graph operations as an actor... Then where the code ran would be a system definition detail. With the right system they could be in the client (cli) or on the server (daemon)."

This architecture realizes that vision by making **everything** an actor with hierarchical addresses. Location transparency means the same code can run anywhere - CLI, daemon, remote server, or distributed across multiple machines. Deployment topology becomes pure configuration.

### Core Principles

1. **Everything is an Actor** - Tasks, knowledge, graph, CozoDB operations are all actors
2. **Hierarchical Addressing** - `primer.tasks`, `primer.tasks.task_123`, `primer.knowledge.{id}`
3. **Location Transparency** - Actors can run anywhere; address resolution handles routing
4. **System Topology is Configuration** - Same code, different deployment via config
5. **Pure Actor Model** - No HTTP/REST coupling, no special cases

### Key Design Decisions

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| **Addressing** | Hierarchical dotted paths (`primer.tasks.{id}`) | Natural namespace, supports wildcards, intuitive |
| **Resolution** | Registry with actor factories | On-demand instantiation (virtual actor pattern) |
| **Remoting** | Transparent proxy actors | Local and remote actors have identical API |
| **Serialization** | JSON for messages, addresses as strings | Simple, debuggable, language-independent |
| **CozoDB** | Actor interface wrapper | Database becomes just another actor |
| **Supervision** | Hierarchical supervisors (Erlang-style) | Fault isolation, restart policies |

---

## Architecture Overview

### Conceptual Model

```
┌─────────────────────────────────────────────────────────────┐
│                     primer (Root Actor)                      │
│                  Hierarchical Supervision                    │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │
│  │ primer.tasks │  │primer.knowledge│ │ primer.graph │       │
│  │ (Collection) │  │  (Collection)  │  │  (Queries)   │       │
│  └──────────────┘  └──────────────┘  └──────────────┘       │
│         │                  │                  │              │
│         ├─task_123         ├─know_456         └─ nodes.*    │
│         ├─task_124         ├─know_457                        │
│         └─task_125         └─know_458                        │
│                                                               │
│  ┌──────────────┐  ┌──────────────┐                          │
│  │primer.cozodb │  │primer.events │                          │
│  │   (DB Ops)   │  │ (EventLog)   │                          │
│  └──────────────┘  └──────────────┘                          │
│                                                               │
└─────────────────────────────────────────────────────────────┘

Address Examples:
- "primer"                    → Root supervisor
- "primer.tasks"              → Task collection actor
- "primer.tasks.task_123"     → Individual task actor
- "primer.knowledge.know_456" → Individual knowledge node
- "primer.cozodb.query"       → CozoDB query interface
- "primer.cozodb.write"       → CozoDB write interface
```

### Location Transparency in Action

**Same code, different deployment:**

```typescript
// Configuration determines where actors run
// Code never changes!

// ALL-LOCAL (current CLI direct mode)
const config: SystemConfig = {
  "primer.tasks": { location: "local", factory: TaskCollectionActor },
  "primer.graph": { location: "local", factory: GraphActor },
  "primer.cozodb": { location: "local", factory: CozoActor }
};

// ALL-REMOTE (daemon mode)
const config: SystemConfig = {
  "primer.tasks": { location: "remote", url: "ws://localhost:3000" },
  "primer.graph": { location: "remote", url: "ws://localhost:3000" },
  "primer.cozodb": { location: "remote", url: "ws://localhost:3000" }
};

// HYBRID (mixed deployment)
const config: SystemConfig = {
  "primer.tasks": { location: "remote", url: "ws://localhost:3000" },
  "primer.graph": { location: "local", factory: GraphActor },
  "primer.cozodb": { location: "remote", url: "ws://localhost:3000" }
};

// Usage - identical regardless of config!
const system = await System.create(config);
const response = await system.send("primer.tasks", "create", {
  goal: "Implement feature X"
});
```

---

## Hierarchical Actor Addressing

### Address Scheme

**Format:** Dotted path notation (like DNS, Erlang PIDs with names, Akka actor paths)

```
<root>.<collection>.<instance>.<operation>

Examples:
- primer                          (root supervisor)
- primer.tasks                    (task collection)
- primer.tasks.task_123           (specific task)
- primer.knowledge.know_456       (specific knowledge node)
- primer.cozodb.query             (CozoDB query interface)
```

### Address Properties

1. **Hierarchical** - Natural parent/child relationships
2. **Namespace Separation** - `tasks`, `knowledge`, `graph` are isolated
3. **Wildcard Support** - `primer.tasks.*` (all tasks), `primer.*` (all collections)
4. **Human-Readable** - No UUIDs in addresses, semantic names
5. **Location-Independent** - Address doesn't encode location

### Address Resolution

**Resolution Algorithm:**

```typescript
class AddressResolver {
  private config: SystemConfig;
  private localActors: Map<string, Actor>;
  private remoteProxies: Map<string, RemoteActorProxy>;

  async resolve(address: string): Promise<Actor> {
    // 1. Exact match in local cache
    if (this.localActors.has(address)) {
      return this.localActors.get(address)!;
    }

    // 2. Check config for location
    const actorConfig = this.findConfigForAddress(address);

    if (actorConfig.location === "local") {
      // 3. Create local actor on-demand (virtual actor pattern)
      const actor = actorConfig.factory(address, this);
      this.localActors.set(address, actor);
      return actor;
    }

    if (actorConfig.location === "remote") {
      // 4. Create or reuse remote proxy
      if (!this.remoteProxies.has(address)) {
        const proxy = new RemoteActorProxy(address, actorConfig.url);
        this.remoteProxies.set(address, proxy);
      }
      return this.remoteProxies.get(address)!;
    }

    throw new Error(`Cannot resolve address: ${address}`);
  }

  private findConfigForAddress(address: string): ActorConfig {
    // Try exact match first
    if (this.config[address]) {
      return this.config[address];
    }

    // Try prefix matching (e.g., "primer.tasks.task_123" → "primer.tasks")
    const parts = address.split(".");
    for (let i = parts.length - 1; i > 0; i--) {
      const prefix = parts.slice(0, i).join(".");
      if (this.config[prefix]) {
        return this.config[prefix];
      }
    }

    throw new Error(`No config found for address: ${address}`);
  }
}
```

**Key Insight:** Resolution is lazy (virtual actor pattern). Actors are created when first addressed, not pre-allocated.

---

## Location Transparency Mechanism

### Design: Transparent Proxy Pattern

**Choice:** Erlang-style transparent remoting via proxy actors

**Alternatives Considered:**
1. **Akka Remoting** - Requires serializable actor references, complex routing
2. **Orleans Grain Directory** - Requires central directory service, single point of failure
3. **Transparent Proxy (Erlang)** - ✅ Simple, no special cases, works like local actors

### Local vs Remote Actors

```typescript
// Local Actor - Direct in-process invocation
class LocalActor implements Actor {
  async send(message: Message): Promise<Response> {
    // Handle message directly
    return this.handleMessage(message);
  }
}

// Remote Actor Proxy - Transparently forwards to network
class RemoteActorProxy implements Actor {
  private ws: WebSocket;
  private pendingRequests: Map<string, PendingRequest>;

  constructor(address: string, url: string) {
    this.ws = new WebSocket(url);
    // Handle responses...
  }

  async send(message: Message): Promise<Response> {
    // Serialize and send over network
    const id = crypto.randomUUID();
    const envelope = {
      id,
      targetAddress: this.address,
      message
    };

    this.ws.send(JSON.stringify(envelope));

    // Wait for response (Promise-based)
    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });
      setTimeout(() => reject(new Error("Timeout")), 5000);
    });
  }
}
```

**Key Point:** Both implement the same `Actor` interface. Caller cannot tell the difference - that's location transparency!

### Message Serialization

**Format:** JSON for simplicity and debuggability

```typescript
interface MessageEnvelope {
  id: string;              // Request ID (for matching responses)
  targetAddress: string;   // Hierarchical address
  message: Message;        // Actor message (type + payload)
  sender?: string;         // Optional sender address (for replies)
}

interface ResponseEnvelope {
  id: string;              // Matches request ID
  response: Response;      // Actor response (success + data/error)
}
```

**Serialization Rules:**
1. **Addresses** → Serialize as strings (hierarchical paths)
2. **Messages** → JSON.stringify (payload must be JSON-serializable)
3. **Responses** → JSON.stringify (data must be JSON-serializable)
4. **Actor References** → Serialize as address strings, resolve on receiver side

**Non-Serializable Types (Excluded):**
- Functions (use message types instead)
- Symbols (use string IDs)
- WeakMaps/WeakSets (use regular Maps/Sets)
- Native objects (File, Blob - serialize to base64 strings)

---

## Actor Hierarchy Design

### Root Supervisor: `primer`

```typescript
const PrimerActor: ActorFactory = (data: { system: System }) => {
  const children = [
    "primer.tasks",
    "primer.knowledge",
    "primer.graph",
    "primer.cozodb",
    "primer.events"
  ];

  const actor = {
    send: async (message: Message): Promise<Response> => {
      switch (message.type) {
        case "health":
          // Check health of all children
          const health = await Promise.all(
            children.map(addr => data.system.send(addr, { type: "ping" }))
          );
          return { success: true, data: { children: health } };

        case "shutdown":
          // Graceful shutdown of all children
          for (const addr of children) {
            await data.system.send(addr, { type: "stop" });
          }
          return { success: true };

        default:
          return { success: false, error: "Unknown message" };
      }
    }
  };

  return data.system.register(actor);
};
```

**Responsibilities:**
- Supervise child actors (restart on failure)
- Health monitoring
- Graceful shutdown coordination

### Collection Actors: `primer.tasks`, `primer.knowledge`

```typescript
const TaskCollectionActor: ActorFactory = (data: {
  address: string;
  system: System;
  resolver: AddressResolver;
}) => {
  const tasks: Map<string, string> = new Map(); // id → address

  const actor = {
    send: async (message: Message): Promise<Response> => {
      switch (message.type) {
        case "create": {
          const { goal, ...options } = message.payload;
          const taskId = `task_${Date.now()}`;
          const taskAddr = `${data.address}.${taskId}`;

          // Resolve (creates) task actor
          const taskActor = await data.resolver.resolve(taskAddr);

          // Initialize task
          await taskActor.send({
            id: crypto.randomUUID(),
            type: "init",
            payload: { goal, ...options }
          });

          tasks.set(taskId, taskAddr);
          return { success: true, data: { id: taskId, address: taskAddr } };
        }

        case "list": {
          const filters = message.payload;
          const taskList = [];

          for (const [id, addr] of tasks) {
            const taskActor = await data.resolver.resolve(addr);
            const result = await taskActor.send({
              id: crypto.randomUUID(),
              type: "get",
              payload: {}
            });

            if (result.success) {
              taskList.push(result.data);
            }
          }

          return { success: true, data: taskList };
        }

        case "delete": {
          const { id } = message.payload;
          const addr = tasks.get(id);
          if (!addr) {
            return { success: false, error: "Task not found" };
          }

          // Send delete to task actor
          const taskActor = await data.resolver.resolve(addr);
          await taskActor.send({
            id: crypto.randomUUID(),
            type: "delete",
            payload: {}
          });

          tasks.delete(id);
          return { success: true };
        }

        default:
          return { success: false, error: "Unknown message" };
      }
    }
  };

  return data.system.register(actor);
};
```

**Responsibilities:**
- Manage collection of child actors (tasks, knowledge nodes)
- Route messages to specific instances
- Handle collection-wide operations (list, search)

### Instance Actors: `primer.tasks.task_123`

```typescript
const TaskInstanceActor: ActorFactory = (data: {
  address: string;
  system: System;
  resolver: AddressResolver;
}) => {
  // State captured in closure
  let state: TaskState = {
    id: extractIdFromAddress(data.address),
    goal: "",
    state: "pending",
    labels: [],
    priority: 2
  };

  const actor = {
    send: async (message: Message): Promise<Response> => {
      switch (message.type) {
        case "init": {
          Object.assign(state, message.payload);
          return { success: true, data: state };
        }

        case "get": {
          return { success: true, data: state };
        }

        case "update": {
          const { properties } = message.payload;
          Object.assign(state, properties);

          // Persist to CozoDB (if configured)
          await data.system.send("primer.cozodb.write", {
            id: crypto.randomUUID(),
            type: "update_task",
            payload: state
          });

          return { success: true, data: state };
        }

        case "delete": {
          // Persist deletion
          await data.system.send("primer.cozodb.write", {
            id: crypto.randomUUID(),
            type: "delete_task",
            payload: { id: state.id }
          });

          return { success: true };
        }

        default:
          return { success: false, error: "Unknown message" };
      }
    }
  };

  return data.system.register(actor);
};
```

**Responsibilities:**
- Manage individual entity state
- Handle CRUD operations
- Persist changes to CozoDB

---

## CozoDB as Actor Interface

### Design: Dual Actor Interface

**Separation of Concerns:**
- `primer.cozodb.query` - Read operations (idempotent)
- `primer.cozodb.write` - Write operations (with coordination)

```typescript
const CozoQueryActor: ActorFactory = (data: {
  client: CozoWasmClient;
  system: System;
}) => {
  const actor = {
    send: async (message: Message): Promise<Response> => {
      switch (message.type) {
        case "query": {
          const { query, params } = message.payload;
          const result = await data.client.query(query, params);
          return { success: true, data: result };
        }

        case "get_task": {
          const { id } = message.payload;
          const result = await data.client.query(
            `?[id, goal, state] := *tasks{id, goal, state}, id = $id`,
            { id }
          );
          return { success: true, data: result.rows[0] };
        }

        default:
          return { success: false, error: "Unknown query" };
      }
    }
  };

  return data.system.register(actor);
};

const CozoWriteActor: ActorFactory = (data: {
  client: CozoWasmClient;
  eventLog: EventLog;
  system: System;
}) => {
  const actor = {
    send: async (message: Message): Promise<Response> => {
      switch (message.type) {
        case "write": {
          const { query, params } = message.payload;

          // Triple-write coordination
          const event = {
            timestamp: Date.now(),
            type: "write",
            query,
            params
          };

          // 1. EventLog (append-only)
          await data.eventLog.append(event);

          // 2. CozoDB
          const result = await data.client.query(query, params);

          // 3. Broadcast to watchers (WebSocket)
          await data.system.send("primer.events", {
            id: crypto.randomUUID(),
            type: "broadcast",
            payload: event
          });

          return { success: true, data: result };
        }

        case "update_task": {
          const task = message.payload;
          const query = `
            ?[id, goal, state, priority] <- [[$id, $goal, $state, $priority]]
            :put tasks {id, goal, state, priority}
          `;
          return actor.send({
            id: crypto.randomUUID(),
            type: "write",
            payload: { query, params: task }
          });
        }

        default:
          return { success: false, error: "Unknown write" };
      }
    }
  };

  return data.system.register(actor);
};
```

**Key Design:**
- CozoDB client wrapped in actor interface
- Write operations coordinated (triple-write)
- Query operations fast (direct read)
- EventLog and broadcasting integrated

---

## System Topology Configurations

### Configuration Schema

```typescript
interface SystemConfig {
  [address: string]: ActorConfig;
}

interface ActorConfig {
  location: "local" | "remote";
  factory?: ActorFactory;  // Required for local
  url?: string;            // Required for remote
  options?: {
    autoReconnect?: boolean;
    timeout?: number;
    maxRetries?: number;
  };
}
```

### Topology Examples

#### 1. All-Local (Development, Single-User)

```typescript
const ALL_LOCAL: SystemConfig = {
  "primer": {
    location: "local",
    factory: PrimerActor
  },
  "primer.tasks": {
    location: "local",
    factory: TaskCollectionActor
  },
  "primer.knowledge": {
    location: "local",
    factory: KnowledgeCollectionActor
  },
  "primer.graph": {
    location: "local",
    factory: GraphActor
  },
  "primer.cozodb": {
    location: "local",
    factory: CozoActor
  }
};

// Usage
const system = await System.create(ALL_LOCAL);
await system.send("primer.tasks", "create", { goal: "Test task" });
```

**Use Cases:**
- Local CLI without daemon
- Testing/development
- Single-user workflows

**Performance:**
- Latency: <1ms (in-process)
- Memory: ~50MB
- Startup: <100ms

#### 2. All-Remote (Multi-User, Daemon)

```typescript
const ALL_REMOTE: SystemConfig = {
  "primer": {
    location: "remote",
    url: "ws://localhost:3000",
    options: { autoReconnect: true, timeout: 5000 }
  },
  "primer.tasks": {
    location: "remote",
    url: "ws://localhost:3000"
  },
  "primer.knowledge": {
    location: "remote",
    url: "ws://localhost:3000"
  },
  "primer.graph": {
    location: "remote",
    url: "ws://localhost:3000"
  },
  "primer.cozodb": {
    location: "remote",
    url: "ws://localhost:3000"
  }
};

// Usage - identical API!
const system = await System.create(ALL_REMOTE);
await system.send("primer.tasks", "create", { goal: "Test task" });
```

**Use Cases:**
- Multiple CLI instances
- Browser workbench + CLI
- Shared workspace

**Performance:**
- Latency: 5-10ms (local network)
- Memory: <10MB per client
- Startup: <500ms (includes connection)

#### 3. Hybrid (Hot/Cold Split)

```typescript
const HYBRID: SystemConfig = {
  "primer": {
    location: "local",
    factory: PrimerActor
  },
  "primer.tasks": {
    location: "remote",
    url: "ws://localhost:3000"  // Hot data in daemon
  },
  "primer.knowledge": {
    location: "remote",
    url: "ws://localhost:3000"  // Hot data in daemon
  },
  "primer.graph": {
    location: "local",           // Query engine local (fast)
    factory: GraphActor
  },
  "primer.cozodb": {
    location: "remote",
    url: "ws://localhost:3000"  // DB operations in daemon
  }
};
```

**Use Cases:**
- Optimize for query latency
- Balance load across CLI and daemon
- Cache frequently accessed data locally

**Performance:**
- Latency: Mixed (1ms local, 5-10ms remote)
- Memory: ~30MB (partial state)
- Startup: <200ms

#### 4. Distributed (Multi-Region)

```typescript
const DISTRIBUTED: SystemConfig = {
  "primer": {
    location: "remote",
    url: "ws://us-east-1.example.com:3000"
  },
  "primer.tasks": {
    location: "remote",
    url: "ws://us-east-1.example.com:3000"  // Primary region
  },
  "primer.knowledge": {
    location: "remote",
    url: "ws://us-west-1.example.com:3000"  // Secondary region
  },
  "primer.graph": {
    location: "remote",
    url: "ws://eu-west-1.example.com:3000"  // Edge region
  },
  "primer.cozodb": {
    location: "remote",
    url: "ws://us-east-1.example.com:3000"  // Primary DB
  }
};
```

**Use Cases:**
- Geographic distribution
- High availability
- Load balancing

**Performance:**
- Latency: 50-200ms (cross-region)
- Memory: <5MB per client
- Startup: <1s (multiple connections)

---

## Message Routing Across Locations

### Routing Algorithm

```typescript
class System {
  private resolver: AddressResolver;

  async send(address: string, type: string, payload: unknown): Promise<Response> {
    // 1. Resolve address to actor (local or proxy)
    const actor = await this.resolver.resolve(address);

    // 2. Create message
    const message: Message = {
      id: crypto.randomUUID(),
      type,
      payload
    };

    // 3. Send to actor (local or remote, transparently)
    const response = await actor.send(message);

    // 4. Return response
    return response;
  }
}
```

**Key Point:** Routing is entirely handled by address resolution. System.send() doesn't know or care if actor is local or remote!

### Network Protocol: WebSocket

**Choice:** WebSocket over HTTP for persistent connections

**Benefits:**
- Bidirectional communication (server can push to clients)
- Low latency (no connection overhead per request)
- Connection pooling built-in
- Works through firewalls (port 80/443)

**Protocol Messages:**

```typescript
// Client → Server: Request
{
  "type": "request",
  "id": "req-123",
  "targetAddress": "primer.tasks",
  "message": {
    "id": "msg-456",
    "type": "create",
    "payload": { "goal": "Test task" }
  }
}

// Server → Client: Response
{
  "type": "response",
  "id": "req-123",
  "response": {
    "success": true,
    "data": { "id": "task_789", "goal": "Test task" }
  }
}

// Server → Client: Broadcast (unsolicited)
{
  "type": "broadcast",
  "event": {
    "type": "task_created",
    "data": { "id": "task_789", "goal": "Test task" }
  }
}
```

### Error Handling & Retries

```typescript
class RemoteActorProxy implements Actor {
  async send(message: Message): Promise<Response> {
    let attempt = 0;
    const maxRetries = this.options.maxRetries || 3;

    while (attempt < maxRetries) {
      try {
        return await this.sendWithTimeout(message);
      } catch (error) {
        attempt++;
        if (attempt >= maxRetries) {
          return {
            success: false,
            error: `Failed after ${maxRetries} attempts: ${error}`
          };
        }
        // Exponential backoff
        await sleep(Math.pow(2, attempt) * 100);
      }
    }
  }

  private async sendWithTimeout(message: Message): Promise<Response> {
    const timeout = this.options.timeout || 5000;
    return Promise.race([
      this.sendOverWebSocket(message),
      sleep(timeout).then(() => {
        throw new Error("Timeout");
      })
    ]);
  }
}
```

---

## Migration Strategy

### Phase 1: Introduce Location Transparency

**Goal:** Add addressing and resolution without breaking existing code

**Steps:**

1. **Add AddressResolver**
   ```typescript
   // src/actors/resolver.ts
   export class AddressResolver {
     // Implementation...
   }
   ```

2. **Add SystemConfig types**
   ```typescript
   // src/actors/config.ts
   export interface SystemConfig { /* ... */ }
   ```

3. **Update System to use resolver**
   ```typescript
   // src/actors/system.ts
   export class System {
     private resolver: AddressResolver;

     async send(address: string, type: string, payload: unknown) {
       const actor = await this.resolver.resolve(address);
       return actor.send({ id: crypto.randomUUID(), type, payload });
     }
   }
   ```

4. **Test with all-local config**
   - No network, all in-process
   - Should work identically to current system

### Phase 2: Implement Remote Proxies

**Goal:** Support remote actors via WebSocket

**Steps:**

1. **Implement RemoteActorProxy**
   ```typescript
   // src/actors/remote-proxy.ts
   export class RemoteActorProxy implements Actor { /* ... */ }
   ```

2. **Implement WebSocket server in daemon**
   ```typescript
   // daemon/ws-server.ts
   export class ActorWebSocketServer { /* ... */ }
   ```

3. **Test with all-remote config**
   - Start daemon
   - Connect via WebSocket
   - Verify identical behavior

### Phase 3: Refactor CLIs to Use Addressing

**Goal:** Replace daemon.send() with system.send(address)

**Before (Hybrid a18ee7e design):**
```typescript
const response = await daemon.send("create_task", { goal: "..." });
```

**After (Pure actor design):**
```typescript
const response = await system.send("primer.tasks", "create", { goal: "..." });
```

**Steps:**

1. Replace `DaemonClient` with `System` + config
2. Replace message types with hierarchical addresses
3. Update all CLI commands
4. Test with both local and remote configs

### Phase 4: Implement Actor Factories

**Goal:** Convert existing actors to factory pattern

**Steps:**

1. **Task actors**
   ```typescript
   export const TaskCollectionActor: ActorFactory = (data) => { /* ... */ };
   export const TaskInstanceActor: ActorFactory = (data) => { /* ... */ };
   ```

2. **Knowledge actors**
   ```typescript
   export const KnowledgeCollectionActor: ActorFactory = (data) => { /* ... */ };
   ```

3. **Graph actors**
   ```typescript
   export const GraphActor: ActorFactory = (data) => { /* ... */ };
   ```

4. **CozoDB actors**
   ```typescript
   export const CozoQueryActor: ActorFactory = (data) => { /* ... */ };
   export const CozoWriteActor: ActorFactory = (data) => { /* ... */ };
   ```

### Phase 5: Introduce Supervision

**Goal:** Add Erlang-style supervision trees

**Steps:**

1. Implement SupervisorActor
2. Add restart policies (one-for-one, all-for-one)
3. Wrap collections in supervisors
4. Test failure scenarios

### Rollback Plan

**If issues arise, rollback is easy:**

1. Switch config to all-local (no network)
2. System behaves like original design
3. Fix issues, re-test, switch back to remote

**No code changes needed for rollback** - just config!

---

## Comparison with Hybrid Design

### What Changes

| Aspect | Hybrid (a18ee7e) | Pure Actor (This Design) |
|--------|------------------|--------------------------|
| **CLI Communication** | HTTP POST to `/api/actor/message` | WebSocket send to address |
| **Message Routing** | Routes switch on `type` | AddressResolver → Actor.send() |
| **Location Coupling** | Hardcoded: CLI vs daemon | Config-driven: arbitrary topology |
| **Actor Granularity** | Collection-level (tasks, graph) | Instance-level (task_123) |
| **CozoDB Access** | Direct coordinator calls | CozoDB as actor (primer.cozodb) |
| **Testing** | Requires daemon | Local config (no daemon) |

### What Stays the Same

| Aspect | Preserved |
|--------|-----------|
| **Message Protocol** | Still `Message { id, type, payload }` |
| **Response Format** | Still `Response { success, data?, error? }` |
| **Triple-Write** | Still coordinated via actor (write actor) |
| **Event Broadcasting** | Still via WebSocket (events actor) |
| **CLI Thin Shell** | Still parse args → send message → format |

### Benefits of Pure Actor

1. **True Location Transparency**
   - Same code runs anywhere
   - Config determines topology
   - Easy to test (local config)

2. **Fine-Grained Actors**
   - Each task is an actor (not collection)
   - Better isolation
   - Easier to reason about

3. **No HTTP Coupling**
   - Pure actor messaging
   - WebSocket for network (persistent connection)
   - Bidirectional communication

4. **Flexible Deployment**
   - All-local (no daemon)
   - All-remote (daemon)
   - Hybrid (mixed)
   - Distributed (multi-region)

5. **Supervision Trees**
   - Erlang-style fault tolerance
   - Restart policies
   - Hierarchical error handling

### Tradeoffs

**Complexity:**
- More actors (instance-level vs collection-level)
- Address resolution logic
- Remote proxy implementation

**Performance:**
- Network overhead for remote actors (5-10ms vs <1ms local)
- Serialization cost (JSON stringify/parse)

**Debugging:**
- Harder to trace messages across network
- Need better logging/observability

**Mitigation:**
- Start with all-local config (no complexity)
- Add remoting only when needed
- Comprehensive logging and tracing

---

## Implementation Roadmap

### Week 1: Foundation

**Goals:**
- AddressResolver implemented
- SystemConfig types defined
- System.send() uses resolver

**Deliverables:**
- `src/actors/resolver.ts` (~200 LOC)
- `src/actors/config.ts` (~100 LOC)
- Updated `src/actors/system.ts` (~50 LOC changes)
- Tests for all-local config

### Week 2: Remoting

**Goals:**
- RemoteActorProxy implemented
- WebSocket server in daemon
- All-remote config works

**Deliverables:**
- `src/actors/remote-proxy.ts` (~300 LOC)
- `daemon/ws-server.ts` (~400 LOC)
- Tests for all-remote config
- E2E test: CLI → daemon via WebSocket

### Week 3: Actor Factories

**Goals:**
- Convert all actors to factory pattern
- Hierarchical addressing works
- Virtual actor pattern (on-demand instantiation)

**Deliverables:**
- `src/actors/task-actors.ts` (~500 LOC)
- `src/actors/knowledge-actors.ts` (~400 LOC)
- `src/actors/graph-actors.ts` (~300 LOC)
- `src/actors/cozo-actors.ts` (~400 LOC)
- Tests for each actor type

### Week 4: CLI Refactor

**Goals:**
- CLIs use system.send(address) instead of daemon.send()
- All CLI commands work with new addressing
- Backward compatibility via config

**Deliverables:**
- Updated `src/cli/task.ts` (~200 LOC, -1900 LOC)
- Updated `src/cli/graph.ts` (~150 LOC, -550 LOC)
- Updated `src/cli/knowledge.ts` (~150 LOC, -500 LOC)
- Integration tests

### Week 5: Supervision & Polish

**Goals:**
- Supervision trees implemented
- Restart policies work
- Documentation complete

**Deliverables:**
- `src/actors/supervisor.ts` (~300 LOC)
- Updated ACTOR_MODEL_GUIDE.md
- Migration guide
- Performance benchmarks

---

## Success Criteria

### Functional Requirements

- ✅ All CLI commands work with hierarchical addressing
- ✅ Config switch between local/remote/hybrid works seamlessly
- ✅ Remote actors behave identically to local actors
- ✅ Virtual actor pattern (on-demand instantiation) works
- ✅ CozoDB accessible as actor (primer.cozodb.*)
- ✅ Supervision trees restart failed actors

### Performance Requirements

- ✅ Local actors: <1ms latency
- ✅ Remote actors (localhost): <10ms latency
- ✅ Remote actors (LAN): <50ms latency
- ✅ Memory: <100MB for daemon, <10MB per CLI
- ✅ Connection pool: <5 connections per CLI

### Code Quality Requirements

- ✅ No HTTP/REST coupling in actor code
- ✅ Location transparency verified (same code, different config)
- ✅ Test coverage: >80% for actors, resolver, remoting
- ✅ Documentation: Complete for all actor types and configs

---

## Conclusion

This pure actor model architecture achieves the user's vision: **location transparency** where "where the code ran would be a system definition detail."

**Key Achievements:**

1. **Everything is an actor** - Tasks, knowledge, graph, CozoDB operations
2. **Hierarchical addressing** - Natural namespace, semantic names
3. **Location transparency** - Same code, different deployment
4. **System topology is config** - No code changes to relocate actors
5. **Pure actor model** - No special cases, uniform composition

**Next Steps:**

1. Implement AddressResolver and SystemConfig
2. Create RemoteActorProxy and WebSocket server
3. Convert actors to factory pattern
4. Refactor CLIs to use addressing
5. Add supervision trees

**Migration Path:**

- Start with all-local config (no risk, no daemon)
- Add remoting incrementally
- Switch to all-remote when ready
- Rollback is just config change (safe!)

---

**End of Architecture Document**
