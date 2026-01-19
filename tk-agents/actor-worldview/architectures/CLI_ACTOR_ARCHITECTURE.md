# CLI Actor Architecture V2: Pure Actor Model with Location Transparency

**Version:** 2.0
**Status:** Design Specification
**Date:** 2026-01-18
**Context:** Evolution from V1 (hybrid actor/HTTP) to pure actor model with location transparency
**Supersedes:** CLI_ACTOR_ARCHITECTURE.md (V1, agent a18ee7e)
**Related:** PURE_ACTOR_MODEL_ARCHITECTURE.md, ACTOR_MODEL_COMPARISON.md

---

## Executive Summary

### What Changed from V1

**V1 (Hybrid Actor/HTTP):**
- CLIs connect to daemon via HTTP
- Single endpoint: `/api/actor/message`
- Message routing via switch on `message.type`
- Two locations: CLI (thin) or Daemon (fat)

**V2 (Pure Actor with Location Transparency):**
- CLIs use hierarchical actor addressing
- Address resolution determines location (local/remote)
- Message routing via `AddressResolver`
- Unlimited locations: config-driven topology

### Key Improvements

1. **Location Transparency** - Same code runs anywhere (CLI, daemon, distributed)
2. **Hierarchical Addressing** - `primer.tasks.task_123` instead of message type strings
3. **Configuration-Driven** - Deployment topology is pure config
4. **Better Testing** - Local config works without daemon
5. **Fine-Grained Actors** - Instance-level actors (not just collections)

### Migration Strategy

**V1 (Now):** Implement hybrid HTTP design (fast, low-risk)
**V1.5 (Later):** Add addressing to hybrid (improve testing)
**V2 (Future):** Full pure actor with location transparency (distributed systems)

---

## Architecture Overview

### High-Level Design

```
┌───────────────────────────────────────────────────────────────┐
│                          CLI Process                           │
│                                                                │
│  ┌──────────────┐       ┌──────────────┐                      │
│  │ Task Command │       │ Graph Command│                      │
│  │  (task.ts)   │       │  (graph.ts)  │                      │
│  └──────────────┘       └──────────────┘                      │
│         │                       │                              │
│         └───────────┬───────────┘                              │
│                     ↓                                          │
│            ┌─────────────────┐                                 │
│            │  System (Core)  │                                 │
│            └─────────────────┘                                 │
│                     │                                          │
│         ┌───────────┴───────────┐                              │
│         ↓                       ↓                              │
│  ┌──────────────┐        ┌──────────────┐                     │
│  │ AddressRslvr │        │  SystemCfg   │                     │
│  │  (routing)   │        │   (config)   │                     │
│  └──────────────┘        └──────────────┘                     │
│         │                                                      │
│         ├─── Local? ──▶ LocalActor (in-process)               │
│         │                                                      │
│         └─── Remote? ──▶ RemoteActorProxy                     │
│                                   │                            │
└───────────────────────────────────┼────────────────────────────┘
                                    │ WebSocket
                                    ↓
                    ┌───────────────────────────────┐
                    │        Daemon Process         │
                    │                               │
                    │  ┌─────────────────────┐      │
                    │  │ WebSocketServer     │      │
                    │  │ (actor remoting)    │      │
                    │  └─────────────────────┘      │
                    │            ↓                  │
                    │  ┌─────────────────────┐      │
                    │  │   AddressResolver   │      │
                    │  └─────────────────────┘      │
                    │            ↓                  │
                    │  ┌─────────────────────┐      │
                    │  │    Local Actors     │      │
                    │  │ - primer.tasks      │      │
                    │  │ - primer.knowledge  │      │
                    │  │ - primer.cozodb     │      │
                    │  └─────────────────────┘      │
                    │            ↓                  │
                    │   Graph + CozoDB + EventLog   │
                    └───────────────────────────────┘
```

### Actor Hierarchy

```
primer                              (Root supervisor)
├─ primer.tasks                     (Task collection actor)
│  ├─ primer.tasks.task_123         (Task instance actor)
│  ├─ primer.tasks.task_124
│  └─ primer.tasks.task_125
├─ primer.knowledge                 (Knowledge collection actor)
│  ├─ primer.knowledge.know_456
│  └─ primer.knowledge.know_457
├─ primer.graph                     (Graph query actor)
├─ primer.cozodb                    (CozoDB interface)
│  ├─ primer.cozodb.query           (Read operations)
│  └─ primer.cozodb.write           (Write operations)
└─ primer.events                    (Event broadcast actor)
```

---

## System Components

### 1. System (Core)

**Location:** `src/actors/system.ts`

**Purpose:** Central message routing and actor lifecycle management

```typescript
export class System {
  private resolver: AddressResolver;
  private config: SystemConfig;

  static async create(config: SystemConfig): Promise<System> {
    const resolver = new AddressResolver(config);
    return new System(resolver, config);
  }

  /**
   * Send message to actor by hierarchical address
   * Location transparent - works for local and remote actors
   */
  async send(
    address: string,
    type: string,
    payload: unknown
  ): Promise<Response> {
    // 1. Resolve address to actor (local or remote proxy)
    const actor = await this.resolver.resolve(address);

    // 2. Create message
    const message: Message = {
      id: crypto.randomUUID(),
      type,
      payload
    };

    // 3. Send to actor (transparently handles local vs remote)
    return await actor.send(message);
  }
}
```

**Key Methods:**
- `create(config)` - Initialize system with configuration
- `send(address, type, payload)` - Send message to actor
- `shutdown()` - Graceful shutdown of all actors

### 2. AddressResolver

**Location:** `src/actors/resolver.ts`

**Purpose:** Resolve hierarchical addresses to actor instances (local or remote)

```typescript
export class AddressResolver {
  private config: SystemConfig;
  private localActors: Map<string, Actor> = new Map();
  private remoteProxies: Map<string, RemoteActorProxy> = new Map();
  private factories: Map<string, ActorFactory> = new Map();

  /**
   * Resolve address to actor instance
   * Creates actors on-demand (virtual actor pattern)
   */
  async resolve(address: string): Promise<Actor> {
    // 1. Check local cache
    if (this.localActors.has(address)) {
      return this.localActors.get(address)!;
    }

    // 2. Find config for address (supports prefix matching)
    const actorConfig = this.findConfigForAddress(address);

    // 3. Create local actor or remote proxy
    if (actorConfig.location === "local") {
      return this.createLocalActor(address, actorConfig);
    } else {
      return this.createRemoteProxy(address, actorConfig);
    }
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

  private createLocalActor(
    address: string,
    config: ActorConfig
  ): Actor {
    const actor = config.factory!(address, this);
    this.localActors.set(address, actor);
    return actor;
  }

  private createRemoteProxy(
    address: string,
    config: ActorConfig
  ): RemoteActorProxy {
    // Reuse proxy for same URL
    const proxyKey = config.url!;
    if (!this.remoteProxies.has(proxyKey)) {
      const proxy = new RemoteActorProxy(config.url!, config.options);
      this.remoteProxies.set(proxyKey, proxy);
    }
    return this.remoteProxies.get(proxyKey)!;
  }
}
```

**Key Features:**
- **Virtual Actor Pattern** - Actors created on-demand
- **Prefix Matching** - `primer.tasks.task_123` matches `primer.tasks` config
- **Connection Pooling** - Reuse proxies for same URL
- **Lazy Initialization** - Only create actors when first accessed

### 3. RemoteActorProxy

**Location:** `src/actors/remote-proxy.ts`

**Purpose:** Transparent proxy for remote actors via WebSocket

```typescript
export class RemoteActorProxy implements Actor {
  private ws: WebSocket;
  private pendingRequests: Map<string, PendingRequest> = new Map();
  private connected: boolean = false;

  constructor(url: string, options?: RemoteActorOptions) {
    this.ws = new WebSocket(url);

    this.ws.on("open", () => {
      this.connected = true;
    });

    this.ws.on("message", (data: string) => {
      const envelope = JSON.parse(data) as ResponseEnvelope;
      const pending = this.pendingRequests.get(envelope.id);
      if (pending) {
        pending.resolve(envelope.response);
        this.pendingRequests.delete(envelope.id);
      }
    });

    this.ws.on("close", () => {
      this.connected = false;
      // Reject all pending requests
      for (const [id, pending] of this.pendingRequests) {
        pending.reject(new Error("Connection closed"));
      }
      this.pendingRequests.clear();
    });
  }

  async send(message: Message): Promise<Response> {
    if (!this.connected) {
      throw new Error("Not connected to remote actor");
    }

    const id = crypto.randomUUID();
    const envelope: MessageEnvelope = {
      id,
      targetAddress: this.address,
      message
    };

    // Send request
    this.ws.send(JSON.stringify(envelope));

    // Wait for response (with timeout)
    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });

      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error("Request timeout"));
        }
      }, this.options.timeout || 5000);
    });
  }
}
```

**Key Features:**
- **Transparent Remoting** - Same API as local actors
- **Promise-based** - Async request/response
- **Timeout Handling** - Configurable timeout
- **Connection Management** - Auto-reconnect (optional)

### 4. Actor Factories

**Location:** `src/actors/task-actors.ts`, `src/actors/knowledge-actors.ts`, etc.

**Purpose:** Create actor instances with state management

```typescript
/**
 * Task Collection Actor
 * Manages collection of task instance actors
 */
export const TaskCollectionActor: ActorFactory = (data: {
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

          // Virtual actor pattern: resolve creates instance
          const taskActor = await data.resolver.resolve(taskAddr);
          await taskActor.send({
            id: crypto.randomUUID(),
            type: "init",
            payload: { goal, ...options }
          });

          tasks.set(taskId, taskAddr);
          return { success: true, data: { id: taskId, address: taskAddr } };
        }

        case "list": {
          // Query all tasks
          const taskList = [];
          for (const [id, addr] of tasks) {
            const actor = await data.resolver.resolve(addr);
            const result = await actor.send({
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

        default:
          return { success: false, error: "Unknown message" };
      }
    }
  };

  return data.system.register(actor);
};

/**
 * Task Instance Actor
 * Manages individual task state
 */
export const TaskInstanceActor: ActorFactory = (data: {
  address: string;
  system: System;
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

          // Persist to CozoDB
          await data.system.send("primer.cozodb.write", {
            id: crypto.randomUUID(),
            type: "update_task",
            payload: state
          });

          return { success: true, data: state };
        }

        default:
          return { success: false, error: "Unknown message" };
      }
    }
  };

  return data.system.register(actor);
};
```

---

## CLI Transformation Pattern

### CLI Structure (All CLIs)

```typescript
// src/cli/task.ts, graph.ts, knowledge.ts

import { System } from "../actors/system.ts";
import { loadConfig } from "../actors/config.ts";

// 1. Load config (determines topology)
const config = loadConfig();
const system = await System.create(config);

// 2. Parse CLI arguments
const program = new Command();

program
  .command("add <goal>")
  .option("--labels <labels>", "Task labels")
  .option("--priority <p>", "Priority", "2")
  .action(async (goal, options) => {
    await cmdAdd(goal, options);
  });

// 3. Send messages via system (location transparent!)
async function cmdAdd(goal: string, options: any) {
  const response = await system.send("primer.tasks", "create", {
    goal,
    labels: options.labels?.split(",") || [],
    priority: parseInt(options.priority)
  });

  if (!response.success) {
    console.error(`Error: ${response.error}`);
    process.exit(1);
  }

  const { id, address } = response.data;
  console.log(`✓ Created task: ${id}`);
  console.log(`  Address: ${address}`);
}

// 4. Format output
// (minimal logic, just display response.data)

program.parse();
```

**Key Points:**
- **Config-driven** - `loadConfig()` determines local/remote
- **Location transparent** - `system.send()` works anywhere
- **Thin shell** - Parse args → send message → format output
- **No domain logic** - All logic in actors

### Example Commands (All Identical API)

```bash
# Create task
bun src/cli/task.ts add "Implement feature X" --labels backend --priority 1

# Update task
bun src/cli/task.ts update task_123 complete "Done"

# List tasks
bun src/cli/task.ts list --label backend

# Show task
bun src/cli/task.ts show task_123

# Add dependency
bun src/cli/task.ts dep add task_123 task_124
```

**Works identically with:**
- All-local config (no daemon)
- All-remote config (daemon required)
- Hybrid config (mixed local/remote)

---

## Configuration

### SystemConfig Schema

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

### Configuration Examples

#### 1. All-Local (Development, Testing)

```typescript
// config/local.ts
export const LOCAL_CONFIG: SystemConfig = {
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
```

**Use Cases:**
- Local CLI without daemon
- Unit testing (fast, no network)
- Development/debugging

**Performance:**
- Latency: <1ms
- Memory: ~50MB
- Startup: <100ms

#### 2. All-Remote (Production, Multi-User)

```typescript
// config/remote.ts
export const REMOTE_CONFIG: SystemConfig = {
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
```

**Use Cases:**
- Multiple CLIs + browser
- Shared workspace
- Persistent daemon

**Performance:**
- Latency: 5-10ms
- Memory: <10MB per client
- Startup: <500ms

#### 3. Hybrid (Optimized)

```typescript
// config/hybrid.ts
export const HYBRID_CONFIG: SystemConfig = {
  "primer": {
    location: "local",
    factory: PrimerActor
  },
  "primer.tasks": {
    location: "remote",
    url: "ws://localhost:3000"  // Writes to daemon
  },
  "primer.knowledge": {
    location: "remote",
    url: "ws://localhost:3000"
  },
  "primer.graph": {
    location: "local",           // Queries local (fast!)
    factory: GraphActor
  },
  "primer.cozodb": {
    location: "remote",          // DB in daemon
    url: "ws://localhost:3000"
  }
};
```

**Use Cases:**
- Optimize query latency
- Balance load
- Cache hot data locally

**Performance:**
- Latency: Mixed (1ms local, 5-10ms remote)
- Memory: ~30MB

### Loading Configuration

```typescript
// src/actors/config.ts
export function loadConfig(): SystemConfig {
  const env = process.env.PRIMER_CONFIG || "local";

  switch (env) {
    case "local":
      return LOCAL_CONFIG;
    case "remote":
      return REMOTE_CONFIG;
    case "hybrid":
      return HYBRID_CONFIG;
    default:
      throw new Error(`Unknown config: ${env}`);
  }
}

// Usage
const config = loadConfig();
const system = await System.create(config);
```

**Environment Variables:**
```bash
# Use local config (no daemon)
PRIMER_CONFIG=local bun src/cli/task.ts add "Test"

# Use remote config (daemon required)
PRIMER_CONFIG=remote bun src/cli/task.ts add "Test"

# Use hybrid config
PRIMER_CONFIG=hybrid bun src/cli/task.ts add "Test"
```

---

## WebSocket Protocol

### Message Format

```typescript
// Client → Server: Request
interface MessageEnvelope {
  id: string;              // Request ID (for matching response)
  targetAddress: string;   // Hierarchical address (e.g., "primer.tasks")
  message: Message;        // Actor message { id, type, payload }
  sender?: string;         // Optional sender address
}

// Server → Client: Response
interface ResponseEnvelope {
  id: string;              // Matches request ID
  response: Response;      // Actor response { success, data?, error? }
}

// Server → Client: Broadcast (unsolicited)
interface BroadcastEnvelope {
  type: "broadcast";
  event: {
    type: string;
    data: unknown;
  };
}
```

### Example Message Flow

```
CLI                                Daemon
 │                                   │
 ├──WS CONNECT ws://localhost:3000──→│
 │                                   │
 ├──WS SEND─────────────────────────→│
 │  {                                │
 │    id: "req-123",                 │
 │    targetAddress: "primer.tasks", │
 │    message: {                     │
 │      id: "msg-456",               │
 │      type: "create",              │
 │      payload: { goal: "..." }     │
 │    }                              │
 │  }                                │
 │                                   ├─ Resolve address
 │                                   ├─ Send to actor
 │                                   ├─ Get response
 │                                   │
 │←──WS SEND──────────────────────────┤
    {                                │
      id: "req-123",                 │
      response: {                    │
        success: true,               │
        data: { id: "task_789" }     │
      }                              │
    }                                │
 │                                   │
 │←──WS SEND (broadcast)──────────────┤
    {                                │
      type: "broadcast",             │
      event: {                       │
        type: "task_created",        │
        data: { id: "task_789" }     │
      }                              │
    }                                │
 │                                   │
```

---

## Migration from V1

### Phase 1: V1 Implementation (Current)

**Goals:**
- Solve CozoDB connection error
- Establish CLI thin shell pattern
- Learn actor messaging

**Components:**
- `DaemonClient` (HTTP)
- `/api/actor/message` endpoint
- Transformed CLIs

**Duration:** 2-3 weeks

### Phase 2: Add Addressing to V1

**Goals:**
- Introduce hierarchical addresses
- Add AddressResolver (local only)
- Support local config for testing

**Changes:**
```typescript
// Before (V1)
await daemon.send("create_task", { goal: "..." });

// After (V1.5)
const config = { "primer.tasks": { location: "local", factory: TaskActor } };
const system = await System.create(config);
await system.send("primer.tasks", "create", { goal: "..." });
```

**Duration:** 1 week

### Phase 3: Add Remote Proxies

**Goals:**
- Implement RemoteActorProxy
- WebSocket server in daemon
- Support all-remote config

**Changes:**
```typescript
// Config switch (no code change!)
const config = { "primer.tasks": { location: "remote", url: "ws://localhost:3000" } };
const system = await System.create(config);
await system.send("primer.tasks", "create", { goal: "..." }); // Same API!
```

**Duration:** 2 weeks

### Phase 4: Instance-Level Actors

**Goals:**
- Convert to fine-grained actors
- Virtual actor pattern
- Full V2 architecture

**Changes:**
```typescript
// Collection creates instances
await system.send("primer.tasks", "create", { goal: "..." });
// Returns: { id: "task_123", address: "primer.tasks.task_123" }

// Update instance directly
await system.send("primer.tasks.task_123", "update", { state: "completed" });
```

**Duration:** 2 weeks

### Total Migration: ~7-8 weeks from V1 to V2

---

## Testing Strategy

### Unit Tests (Local Config)

```typescript
test("Create task with local config", async () => {
  const config = {
    "primer.tasks": { location: "local", factory: TaskCollectionActor }
  };
  const system = await System.create(config);

  const response = await system.send("primer.tasks", "create", {
    goal: "Test task"
  });

  expect(response.success).toBe(true);
  expect(response.data.id).toMatch(/^task_/);
});
```

**Benefits:**
- No daemon needed
- Fast (in-process)
- Easy to debug

### Integration Tests (Remote Config)

```typescript
test("Create task with remote config", async () => {
  // Start daemon first
  const daemon = await startDaemon();

  const config = {
    "primer.tasks": { location: "remote", url: "ws://localhost:3001" }
  };
  const system = await System.create(config);

  const response = await system.send("primer.tasks", "create", {
    goal: "Test task"
  });

  expect(response.success).toBe(true);

  await daemon.stop();
});
```

**Benefits:**
- Tests real network communication
- Verifies location transparency

### E2E Tests

```bash
#!/bin/bash
# Test CLI commands with different configs

# Test local config
PRIMER_CONFIG=local bun src/cli/task.ts add "Test"
PRIMER_CONFIG=local bun src/cli/task.ts list

# Start daemon
bun daemon/server.ts &
DAEMON_PID=$!
sleep 1

# Test remote config
PRIMER_CONFIG=remote bun src/cli/task.ts add "Test"
PRIMER_CONFIG=remote bun src/cli/task.ts list

# Cleanup
kill $DAEMON_PID
```

---

## Performance Benchmarks

### Latency Comparison

| Operation | V1 (HTTP) | V2 (Local) | V2 (Remote) |
|-----------|-----------|------------|-------------|
| Create task | 5-10ms | <1ms | 5-10ms |
| Update task | 5-10ms | <1ms | 5-10ms |
| List tasks | 10-20ms | 1-2ms | 10-20ms |
| Query graph | 10-20ms | 1-2ms | 10-20ms |

**Key Insight:** V2 local config is 5-10x faster than V1!

### Memory Usage

| Configuration | Memory per CLI | Memory Daemon |
|---------------|----------------|---------------|
| V1 (HTTP) | <5MB | ~80MB |
| V2 (Local) | ~50MB | N/A |
| V2 (Remote) | <10MB | ~80MB |

---

## Success Criteria

### Functional Requirements

- ✅ All CLI commands work with hierarchical addressing
- ✅ Config switch (local/remote/hybrid) requires no code changes
- ✅ Local config works without daemon (for testing)
- ✅ Remote config has identical behavior to local
- ✅ Virtual actor pattern (on-demand instantiation)
- ✅ CozoDB accessible as actor interface

### Performance Requirements

- ✅ Local actors: <1ms latency
- ✅ Remote actors: <10ms latency (localhost)
- ✅ Memory: <100MB daemon, <10MB per CLI (remote)
- ✅ Startup: <500ms (including connection)

### Code Quality Requirements

- ✅ No HTTP coupling in actor code
- ✅ Location transparency verified (same code, different config)
- ✅ Test coverage: >80% for actors, resolver, remoting
- ✅ Documentation complete

---

## Conclusion

**V2 Pure Actor Architecture** achieves true location transparency while maintaining backward compatibility with V1.

**Key Benefits:**
1. **Flexible Deployment** - Config determines topology (local/remote/hybrid/distributed)
2. **Better Testing** - Local config works without daemon
3. **Fine-Grained Actors** - Instance-level actors for better isolation
4. **Future-Proof** - Designed for distributed systems
5. **Same Code, Different Deployment** - Location transparency achieved!

**Migration Path:**
- V1 (Now): Hybrid HTTP design - fast, low-risk
- V1.5 (Soon): Add addressing - improve testing
- V2 (Later): Pure actor - location transparency

**Next Steps:**
1. Complete V1 implementation (2-3 weeks)
2. Add addressing to V1 (1 week)
3. Implement remote proxies (2 weeks)
4. Convert to instance-level actors (2 weeks)
5. Full V2 deployment (1 week testing + docs)

---

**End of V2 Architecture Document**
