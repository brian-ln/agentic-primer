# Actor Model Migration Guide: Hybrid → Pure

**Version:** 1.0
**Date:** 2026-01-18
**Purpose:** Practical migration guide from hybrid actor/HTTP to pure actor with location transparency
**Audience:** Developers implementing the actor model evolution

---

## Overview

### Migration Phases

```
V1 (Hybrid)        V1.5 (Addressing)      V2 (Pure Actor)
    ↓                      ↓                       ↓
HTTP/REST          Addressing Layer        Location Transparency
DaemonClient   →   System + Resolver   →   Remote Proxies + Config
2 weeks            1 week                  2 weeks

Total Migration Time: ~5 weeks
```

### Risk Mitigation

**Low-Risk Approach:**
1. Implement V1 first (solve immediate problem)
2. Add addressing incrementally (improve testing)
3. Introduce remoting (enable location transparency)
4. Full pure actor (distributed systems)

**Rollback Strategy:**
- Each phase is independently functional
- Can rollback to previous phase at any time
- Config changes only (no code changes for rollback)

---

## Phase 1: Implement Hybrid (V1)

**Timeline:** 2-3 weeks
**Risk:** Low
**Goal:** Solve CozoDB connection error, establish CLI thin shell

### Step 1.1: Create DaemonClient

**File:** `src/daemon-client.ts`

```typescript
export class DaemonClient {
  private baseUrl: string;

  constructor(options?: { baseUrl?: string }) {
    this.baseUrl = options?.baseUrl || "http://127.0.0.1:3000";
  }

  async send(type: string, payload: Record<string, unknown>): Promise<Response> {
    const message = {
      id: crypto.randomUUID(),
      type,
      payload
    };

    const response = await fetch(`${this.baseUrl}/api/actor/message`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(message)
    });

    return await response.json();
  }
}

export const daemon = new DaemonClient();
```

**Tests:**
```typescript
test("DaemonClient sends message", async () => {
  const client = new DaemonClient({ baseUrl: "http://localhost:3001" });
  const response = await client.send("create_task", { goal: "Test" });
  expect(response.success).toBe(true);
});
```

### Step 1.2: Add Daemon Routes

**File:** `daemon/api/routes.ts`

```typescript
{
  method: "POST",
  path: "/api/actor/message",
  handler: async (req) => {
    const message = await req.json();
    const { type, payload } = message;

    switch (type) {
      case "create_task":
        return handleCreateTask(payload, coordinator);
      case "update_task":
        return handleUpdateTask(payload, coordinator);
      // ... other handlers
      default:
        return { success: false, error: "Unknown message" };
    }
  }
}

async function handleCreateTask(payload: any, coordinator: Coordinator) {
  const taskId = await coordinator.createTask({
    goal: payload.goal,
    // ...
  });
  return { success: true, data: { id: taskId } };
}
```

### Step 1.3: Transform CLI Commands

**Before:**
```typescript
// src/cli/task.ts
const graph = await loadGraph();
const coordinator = await initializeCoordinator();
const taskId = await coordinator.createTask({ goal });
await saveGraph(graph);
console.log(`Added task: ${taskId}`);
```

**After:**
```typescript
// src/cli/task.ts
import { daemon } from "../daemon-client.ts";

const response = await daemon.send("create_task", { goal });
if (!response.success) throw new Error(response.error);
console.log(`Added task: ${response.data.id}`);
```

**LOC Reduction:** 2155 → 250 per CLI (88% reduction)

### Step 1.4: Testing

```bash
# Start daemon
bun daemon/server.ts &

# Test CLI
bun src/cli/task.ts add "Test task"
bun src/cli/task.ts list

# Verify: No "CozoDB write failed" errors!
```

**Success Criteria:**
- ✅ All CLI commands work
- ✅ Zero CozoDB connection errors
- ✅ Daemon auto-starts if needed
- ✅ 88% LOC reduction achieved

---

## Phase 2: Add Addressing (V1.5)

**Timeline:** 1 week
**Risk:** Low
**Goal:** Introduce hierarchical addressing, improve testing

### Step 2.1: Define Address Types

**File:** `src/actors/types.ts`

```typescript
export type ActorAddress = string; // Hierarchical: "primer.tasks.task_123"

export interface SystemConfig {
  [address: string]: ActorConfig;
}

export interface ActorConfig {
  location: "local" | "remote";
  factory?: ActorFactory;
  url?: string;
}

export type ActorFactory = (
  address: string,
  resolver: AddressResolver
) => Actor;
```

### Step 2.2: Create AddressResolver (Local Only)

**File:** `src/actors/resolver.ts`

```typescript
export class AddressResolver {
  private config: SystemConfig;
  private localActors: Map<string, Actor> = new Map();

  constructor(config: SystemConfig) {
    this.config = config;
  }

  async resolve(address: string): Promise<Actor> {
    // Check cache
    if (this.localActors.has(address)) {
      return this.localActors.get(address)!;
    }

    // Find config (prefix matching)
    const actorConfig = this.findConfigForAddress(address);

    // Create local actor only (no remote yet!)
    if (actorConfig.location === "local") {
      const actor = actorConfig.factory!(address, this);
      this.localActors.set(address, actor);
      return actor;
    }

    throw new Error(`Remote actors not yet supported: ${address}`);
  }

  private findConfigForAddress(address: string): ActorConfig {
    // Exact match
    if (this.config[address]) {
      return this.config[address];
    }

    // Prefix match: "primer.tasks.task_123" → "primer.tasks"
    const parts = address.split(".");
    for (let i = parts.length - 1; i > 0; i--) {
      const prefix = parts.slice(0, i).join(".");
      if (this.config[prefix]) {
        return this.config[prefix];
      }
    }

    throw new Error(`No config for address: ${address}`);
  }
}
```

### Step 2.3: Create System Class

**File:** `src/actors/system.ts`

```typescript
export class System {
  private resolver: AddressResolver;

  static async create(config: SystemConfig): Promise<System> {
    const resolver = new AddressResolver(config);
    return new System(resolver);
  }

  async send(
    address: string,
    type: string,
    payload: unknown
  ): Promise<Response> {
    const actor = await this.resolver.resolve(address);
    const message = {
      id: crypto.randomUUID(),
      type,
      payload
    };
    return await actor.send(message);
  }
}
```

### Step 2.4: Create Local Config

**File:** `config/local.ts`

```typescript
import { TaskCollectionActor } from "../src/actors/task-actors.ts";
import { GraphActor } from "../src/actors/graph-actor.ts";

export const LOCAL_CONFIG: SystemConfig = {
  "primer.tasks": {
    location: "local",
    factory: TaskCollectionActor
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

### Step 2.5: Update CLI to Use System

**File:** `src/cli/task.ts`

```typescript
import { System } from "../actors/system.ts";
import { LOCAL_CONFIG } from "../../config/local.ts";

const system = await System.create(LOCAL_CONFIG);

async function cmdAdd(goal: string, options: any) {
  const response = await system.send("primer.tasks", "create", {
    goal,
    labels: options.labels?.split(",") || []
  });

  if (!response.success) throw new Error(response.error);
  console.log(`Added task: ${response.data.id}`);
}
```

### Step 2.6: Testing

```typescript
// No daemon needed!
test("Create task with local config", async () => {
  const system = await System.create(LOCAL_CONFIG);
  const response = await system.send("primer.tasks", "create", {
    goal: "Test task"
  });
  expect(response.success).toBe(true);
});
```

**Success Criteria:**
- ✅ Hierarchical addresses work
- ✅ Local config works without daemon
- ✅ Tests run without network
- ✅ Prefix matching works

---

## Phase 3: Add Remote Proxies (V2)

**Timeline:** 2 weeks
**Risk:** Medium
**Goal:** Enable location transparency via WebSocket

### Step 3.1: Create RemoteActorProxy

**File:** `src/actors/remote-proxy.ts`

```typescript
export class RemoteActorProxy implements Actor {
  private ws: WebSocket;
  private pendingRequests: Map<string, PendingRequest> = new Map();

  constructor(url: string) {
    this.ws = new WebSocket(url);

    this.ws.on("message", (data: string) => {
      const envelope = JSON.parse(data);
      const pending = this.pendingRequests.get(envelope.id);
      if (pending) {
        pending.resolve(envelope.response);
        this.pendingRequests.delete(envelope.id);
      }
    });
  }

  async send(message: Message): Promise<Response> {
    const id = crypto.randomUUID();
    const envelope = {
      id,
      targetAddress: this.address,
      message
    };

    this.ws.send(JSON.stringify(envelope));

    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });
      setTimeout(() => reject(new Error("Timeout")), 5000);
    });
  }
}
```

### Step 3.2: Update AddressResolver (Add Remote Support)

**File:** `src/actors/resolver.ts`

```typescript
export class AddressResolver {
  private remoteProxies: Map<string, RemoteActorProxy> = new Map();

  async resolve(address: string): Promise<Actor> {
    // ... local logic same as before ...

    // Add remote support
    if (actorConfig.location === "remote") {
      const proxyKey = actorConfig.url!;
      if (!this.remoteProxies.has(proxyKey)) {
        const proxy = new RemoteActorProxy(actorConfig.url!);
        this.remoteProxies.set(proxyKey, proxy);
      }
      return this.remoteProxies.get(proxyKey)!;
    }
  }
}
```

### Step 3.3: Implement WebSocket Server in Daemon

**File:** `daemon/ws-server.ts`

```typescript
export class ActorWebSocketServer {
  private wss: WebSocketServer;
  private resolver: AddressResolver;

  constructor(port: number, resolver: AddressResolver) {
    this.resolver = resolver;
    this.wss = new WebSocketServer({ port });

    this.wss.on("connection", (ws) => {
      ws.on("message", async (data: string) => {
        const envelope = JSON.parse(data);
        const response = await this.handleMessage(envelope);
        ws.send(JSON.stringify(response));
      });
    });
  }

  private async handleMessage(envelope: MessageEnvelope): Promise<ResponseEnvelope> {
    try {
      const actor = await this.resolver.resolve(envelope.targetAddress);
      const response = await actor.send(envelope.message);
      return { id: envelope.id, response };
    } catch (error) {
      return {
        id: envelope.id,
        response: {
          success: false,
          error: String(error)
        }
      };
    }
  }
}
```

### Step 3.4: Update Daemon Startup

**File:** `daemon/server.ts`

```typescript
// Start HTTP server
const httpServer = Bun.serve({ /* ... */ });

// Start WebSocket server
const resolver = new AddressResolver(DAEMON_CONFIG);
const wsServer = new ActorWebSocketServer(3000, resolver);

console.log("Daemon running:");
console.log("  HTTP: http://localhost:3000");
console.log("  WebSocket: ws://localhost:3000");
```

### Step 3.5: Create Remote Config

**File:** `config/remote.ts`

```typescript
export const REMOTE_CONFIG: SystemConfig = {
  "primer.tasks": {
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

### Step 3.6: Update CLI to Support Config Switch

**File:** `src/cli/task.ts`

```typescript
import { loadConfig } from "../actors/config.ts";

// Load config from environment
const config = loadConfig(); // Reads PRIMER_CONFIG env var
const system = await System.create(config);

// Rest of CLI unchanged!
async function cmdAdd(goal: string, options: any) {
  const response = await system.send("primer.tasks", "create", { goal });
  // ...
}
```

**File:** `src/actors/config.ts`

```typescript
export function loadConfig(): SystemConfig {
  const env = process.env.PRIMER_CONFIG || "local";

  switch (env) {
    case "local":
      return LOCAL_CONFIG;
    case "remote":
      return REMOTE_CONFIG;
    default:
      throw new Error(`Unknown config: ${env}`);
  }
}
```

### Step 3.7: Testing

```bash
# Test local config (no daemon)
PRIMER_CONFIG=local bun src/cli/task.ts add "Test"
# ✓ Works without daemon!

# Start daemon
bun daemon/server.ts &

# Test remote config (daemon required)
PRIMER_CONFIG=remote bun src/cli/task.ts add "Test"
# ✓ Works via WebSocket!

# Verify same behavior
PRIMER_CONFIG=local bun src/cli/task.ts list
PRIMER_CONFIG=remote bun src/cli/task.ts list
# ✓ Identical output!
```

**Success Criteria:**
- ✅ Remote proxies work via WebSocket
- ✅ Config switch requires no code changes
- ✅ Local and remote have identical behavior
- ✅ Location transparency achieved!

---

## Phase 4: Instance-Level Actors (V2 Complete)

**Timeline:** 2 weeks
**Risk:** Medium
**Goal:** Fine-grained actors, virtual actor pattern

### Step 4.1: Create Task Instance Actor

**File:** `src/actors/task-actors.ts`

```typescript
export const TaskInstanceActor: ActorFactory = (address, resolver) => {
  // Extract ID from address: "primer.tasks.task_123" → "task_123"
  const id = address.split(".").pop()!;

  let state: TaskState = {
    id,
    goal: "",
    state: "pending",
    labels: []
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
          Object.assign(state, message.payload.properties);
          // Persist to CozoDB
          const system = await System.getCurrent();
          await system.send("primer.cozodb.write", {
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

  return actor;
};
```

### Step 4.2: Update Task Collection Actor

**File:** `src/actors/task-actors.ts`

```typescript
export const TaskCollectionActor: ActorFactory = (address, resolver) => {
  const tasks: Map<string, string> = new Map(); // id → address

  const actor = {
    send: async (message: Message): Promise<Response> => {
      switch (message.type) {
        case "create": {
          const { goal, ...options } = message.payload;
          const taskId = `task_${Date.now()}`;
          const taskAddr = `${address}.${taskId}`;

          // Virtual actor pattern: resolve creates instance
          const taskActor = await resolver.resolve(taskAddr);
          await taskActor.send({
            id: crypto.randomUUID(),
            type: "init",
            payload: { goal, ...options }
          });

          tasks.set(taskId, taskAddr);
          return { success: true, data: { id: taskId, address: taskAddr } };
        }

        case "list": {
          const taskList = [];
          for (const [id, addr] of tasks) {
            const actor = await resolver.resolve(addr);
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

  return actor;
};
```

### Step 4.3: Update Config for Instance Actors

**File:** `config/local.ts`

```typescript
export const LOCAL_CONFIG: SystemConfig = {
  "primer.tasks": {
    location: "local",
    factory: TaskCollectionActor
  },
  // Prefix match: "primer.tasks.*" → TaskInstanceActor
  "primer.tasks.*": {
    location: "local",
    factory: TaskInstanceActor
  }
};
```

**Note:** Resolver's prefix matching handles `primer.tasks.task_123` → `primer.tasks.*` config

### Step 4.4: Update CLI for Instance Addressing

**File:** `src/cli/task.ts`

```typescript
// Create task
async function cmdAdd(goal: string, options: any) {
  const response = await system.send("primer.tasks", "create", { goal });
  const { id, address } = response.data;
  console.log(`✓ Created: ${address}`);
}

// Update task (direct to instance!)
async function cmdUpdate(taskId: string, action: string) {
  const address = `primer.tasks.${taskId}`;
  const response = await system.send(address, "update", {
    properties: { state: action }
  });
  console.log(`✓ Updated: ${taskId}`);
}

// Show task (direct to instance!)
async function cmdShow(taskId: string) {
  const address = `primer.tasks.${taskId}`;
  const response = await system.send(address, "get", {});
  console.log(response.data);
}
```

### Step 4.5: Testing

```bash
# Create task (returns address)
bun src/cli/task.ts add "Test task"
# Output: ✓ Created: primer.tasks.task_123

# Update task (via instance address)
bun src/cli/task.ts update task_123 complete

# Show task (via instance address)
bun src/cli/task.ts show task_123

# Test with remote config (same commands!)
PRIMER_CONFIG=remote bun src/cli/task.ts add "Test"
PRIMER_CONFIG=remote bun src/cli/task.ts update task_123 complete
```

**Success Criteria:**
- ✅ Instance-level actors work
- ✅ Virtual actor pattern (on-demand creation)
- ✅ Direct addressing to instances
- ✅ Works with both local and remote configs

---

## Rollback Procedures

### Rollback from V2 → V1.5

**Steps:**
1. Switch config to use collection actors only
2. Remove instance actor references from CLI
3. No code changes needed (config change only!)

### Rollback from V1.5 → V1

**Steps:**
1. Replace `system.send()` with `daemon.send()`
2. Use message type strings instead of addresses
3. Remove addressing layer

### Rollback from V1 → Original

**Steps:**
1. Restore direct Graph/Coordinator usage
2. Remove DaemonClient
3. Git revert to previous commit

---

## Troubleshooting

### Issue: "Actor not found" Error

**Cause:** Address doesn't match any config

**Solution:**
```typescript
// Check config has matching address or prefix
console.log("Config keys:", Object.keys(config));
console.log("Address:", address);
console.log("Prefix:", address.split(".").slice(0, -1).join("."));
```

### Issue: "Timeout" on Remote Call

**Cause:** Daemon not running or wrong URL

**Solution:**
```bash
# Check daemon is running
curl http://localhost:3000/api/health

# Check WebSocket connection
wscat -c ws://localhost:3000
```

### Issue: "Location Not Supported"

**Cause:** RemoteActorProxy not implemented yet

**Solution:**
- Use local config until Phase 3 complete
- Check `resolver.resolve()` supports both local and remote

---

## Checklist

### Phase 1: Hybrid (V1)
- [ ] Create DaemonClient class
- [ ] Add `/api/actor/message` endpoint
- [ ] Transform CLI commands
- [ ] Test all CLI operations
- [ ] Verify zero CozoDB errors

### Phase 2: Addressing (V1.5)
- [ ] Define address types
- [ ] Create AddressResolver (local only)
- [ ] Create System class
- [ ] Create local config
- [ ] Update CLI to use System
- [ ] Test without daemon

### Phase 3: Remote Proxies (V2)
- [ ] Create RemoteActorProxy
- [ ] Update AddressResolver (add remote)
- [ ] Implement WebSocket server
- [ ] Create remote config
- [ ] Update CLI for config switch
- [ ] Test with both local and remote

### Phase 4: Instance Actors (V2 Complete)
- [ ] Create instance actor factories
- [ ] Update collection actors
- [ ] Update config for instances
- [ ] Update CLI for instance addressing
- [ ] Test virtual actor pattern
- [ ] Verify location transparency

---

## Success Metrics

### Phase 1
- ✅ 88% LOC reduction in CLIs
- ✅ Zero CozoDB connection errors
- ✅ All CLI commands work

### Phase 2
- ✅ Tests run without daemon
- ✅ 10x faster tests (no network)
- ✅ Hierarchical addresses work

### Phase 3
- ✅ Config switch requires zero code changes
- ✅ Local and remote identical behavior
- ✅ <10ms latency for remote calls

### Phase 4
- ✅ Instance-level actor isolation
- ✅ Virtual actor pattern works
- ✅ Location transparency achieved

---

## Next Steps After Migration

### Short-Term (1-3 months)
1. Add supervision trees (Erlang-style)
2. Implement actor monitoring
3. Add health checks
4. Performance optimization

### Long-Term (3-12 months)
1. Multi-region deployment
2. Hot/cold tiering
3. Actor migration
4. Distributed graph

---

**End of Migration Guide**
