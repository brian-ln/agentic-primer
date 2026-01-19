# Actor Model Comparison: Hybrid vs Pure

**Version:** 1.0
**Date:** 2026-01-18
**Purpose:** Compare hybrid actor/HTTP design (agent a18ee7e) with pure actor model (location transparency)
**Context:** Evaluate tradeoffs and provide migration guidance

---

## Executive Summary

### Quick Comparison

| Aspect | Hybrid (a18ee7e) | Pure Actor (This Design) | Winner |
|--------|------------------|--------------------------|---------|
| **Location Flexibility** | CLI vs Daemon (hardcoded) | Arbitrary topology (config) | **Pure** |
| **Implementation Complexity** | Simpler (HTTP REST) | More complex (resolution + proxies) | **Hybrid** |
| **Testing** | Requires daemon | Local config (no daemon) | **Pure** |
| **Actor Granularity** | Collection-level | Instance-level | **Pure** |
| **Network Protocol** | HTTP (request/response) | WebSocket (persistent) | **Pure** |
| **Migration Risk** | Low (incremental) | Medium (more moving parts) | **Hybrid** |
| **Long-Term Scalability** | Limited (2 locations) | Unlimited (distributed) | **Pure** |

### Recommendation

**Phase 1 (Now):** Implement hybrid design (a18ee7e) to solve immediate CozoDB error
**Phase 2 (Future):** Evolve to pure actor model for location transparency and scalability

**Rationale:**
- Hybrid is lower risk, faster to implement
- Pure actor adds significant value but needs more time
- Hybrid → Pure migration is straightforward (mostly config changes)

---

## Architecture Comparison

### Hybrid Actor/HTTP Architecture (a18ee7e)

```
┌─────────────┐
│   CLI       │ (thin shell)
│             │
│ Parse args  │
│     ↓       │
│ DaemonClient│────HTTP POST───▶ ┌──────────────┐
│     ↓       │  /api/actor/msg  │   Daemon     │
│ Format resp │                  │              │
└─────────────┘                  │  ┌────────┐  │
                                 │  │ Router │  │
                                 │  └────────┘  │
                                 │      ↓       │
                                 │ Coordinator  │
                                 │      ↓       │
                                 │  Graph + DB  │
                                 └──────────────┘

Message Example:
POST /api/actor/message
{
  "id": "msg-123",
  "type": "create_task",
  "payload": { "goal": "..." }
}
```

**Key Characteristics:**
- **Two locations:** CLI (client) and Daemon (server)
- **HTTP protocol:** Request/response, stateless
- **Single endpoint:** `/api/actor/message` for all operations
- **Message routing:** Switch statement on `message.type`
- **Collection actors:** Task collection, not individual tasks

### Pure Actor Architecture (Location Transparency)

```
┌─────────────┐
│   CLI       │
│             │
│ Parse args  │
│     ↓       │
│   System    │──────Address Resolution──────┐
│     ↓       │                               │
│  Resolver   │────Local?────▶ LocalActor    │
│     │       │                               │
│     └─Remote?─────WebSocket───▶ ┌──────────────┐
│             │                    │   Daemon     │
│ Format resp │                    │              │
└─────────────┘                    │  ┌────────┐  │
                                   │  │Resolver│  │
                                   │  └────────┘  │
                                   │      ↓       │
                                   │ LocalActors  │
                                   │      ↓       │
                                   │  Graph + DB  │
                                   └──────────────┘

Message Example:
system.send("primer.tasks.task_123", "update", { state: "completed" })
                    ↓
        AddressResolver looks up "primer.tasks.task_123"
                    ↓
        Local? → Direct call | Remote? → WebSocket send
```

**Key Characteristics:**
- **Unlimited locations:** Local, daemon, remote servers, distributed
- **WebSocket protocol:** Persistent connection, bidirectional
- **Hierarchical addresses:** `primer.tasks.task_123` (no magic strings)
- **Address resolution:** Automatic routing (local vs remote)
- **Instance actors:** Every task is an actor

---

## Detailed Comparison

### 1. Location Coupling

#### Hybrid Design

**How it works:**
```typescript
// CLIs always connect to daemon
const daemon = new DaemonClient({ baseUrl: "http://localhost:3000" });

// Daemon has all the logic
const response = await daemon.send("create_task", { goal: "..." });
```

**Location options:**
1. **CLI direct mode** (no daemon) - Loads full graph locally
2. **Daemon mode** - Daemon has graph, CLI is thin

**Switching:** Requires code change (detect daemon, fallback to direct)

**Limitations:**
- Only 2 locations possible
- Cannot split actors across multiple machines
- Cannot run some actors locally, some remotely
- Topology is hardcoded in logic

#### Pure Actor Design

**How it works:**
```typescript
// System reads config, creates resolver
const config = loadConfig(); // Could be local, remote, mixed
const system = await System.create(config);

// Send to address (location transparent)
const response = await system.send("primer.tasks", "create", { goal: "..." });
```

**Location options:**
1. **All-local** - All actors in CLI process
2. **All-remote** - All actors in daemon
3. **Hybrid** - Some local (graph queries), some remote (writes)
4. **Distributed** - Actors across multiple daemons/regions

**Switching:** Change config file (no code change)

**Benefits:**
- Unlimited locations
- Fine-grained control (per-actor placement)
- Easy testing (local config)
- Future-proof (distributed systems)

**Winner: Pure Actor** (significantly more flexible)

---

### 2. Implementation Complexity

#### Hybrid Design

**Components:**
1. `DaemonClient` - HTTP client (~200 LOC)
2. Daemon routes - Switch on message.type (~300 LOC)
3. Message handlers - One per operation (~50 LOC each)

**Total:** ~1000 LOC

**Complexity:**
- Simple HTTP requests
- Familiar REST pattern
- Easy to debug (HTTP logs, curl, Postman)
- No new abstractions

#### Pure Actor Design

**Components:**
1. `AddressResolver` - Resolve addresses to actors (~200 LOC)
2. `RemoteActorProxy` - Network transparency (~300 LOC)
3. `SystemConfig` - Config schema and loading (~100 LOC)
4. `WebSocketServer` - Daemon WS endpoint (~400 LOC)
5. Actor factories - Convert to factory pattern (~500 LOC)

**Total:** ~1500 LOC

**Complexity:**
- New abstractions (resolver, proxy)
- WebSocket protocol (bidirectional)
- Address resolution logic
- Factory pattern refactor
- Config-driven behavior

**Winner: Hybrid** (simpler to implement and understand)

---

### 3. Testing

#### Hybrid Design

**Unit tests:**
```typescript
// Need mock daemon or spawn real daemon
test("CLI sends create_task message", async () => {
  const daemon = new DaemonClient({ baseUrl: "http://localhost:3001" });
  // Requires daemon running on 3001!
  const response = await daemon.send("create_task", { goal: "Test" });
  expect(response.success).toBe(true);
});
```

**Integration tests:**
- Start daemon process
- Wait for ready
- Run CLI commands
- Cleanup daemon

**Challenges:**
- Daemon required for all tests
- Port conflicts (parallel tests)
- Process management (start/stop)
- Slower (network overhead)

#### Pure Actor Design

**Unit tests:**
```typescript
// All-local config (no daemon needed!)
test("CLI sends create_task message", async () => {
  const config = { "primer.tasks": { location: "local", factory: TaskActor } };
  const system = await System.create(config);
  const response = await system.send("primer.tasks", "create", { goal: "Test" });
  expect(response.success).toBe(true);
});
```

**Integration tests:**
- No daemon needed for local config
- Switch to remote config for network tests
- Mock RemoteActorProxy for isolation

**Benefits:**
- Fast (no network, no process spawn)
- Parallel tests (no port conflicts)
- Easy mocking (swap config)
- Test location transparency (same code, different config)

**Winner: Pure Actor** (much easier to test)

---

### 4. Actor Granularity

#### Hybrid Design

**Actor hierarchy:**
```
Daemon
  ├─ TaskCollection (one actor for all tasks)
  ├─ KnowledgeCollection (one actor for all knowledge)
  └─ Graph (one actor for graph queries)
```

**Example:**
```typescript
// Send to collection
await daemon.send("create_task", { goal: "Task 1" });
await daemon.send("create_task", { goal: "Task 2" });

// Collection internally manages tasks
// No individual task actors
```

**Characteristics:**
- Coarse-grained (collection-level)
- Less actors = less overhead
- State managed inside collection actor
- No per-task isolation

#### Pure Actor Design

**Actor hierarchy:**
```
primer
  ├─ primer.tasks (collection actor)
  │    ├─ primer.tasks.task_123 (instance actor)
  │    ├─ primer.tasks.task_124 (instance actor)
  │    └─ primer.tasks.task_125 (instance actor)
  ├─ primer.knowledge (collection actor)
  │    ├─ primer.knowledge.know_456 (instance actor)
  │    └─ primer.knowledge.know_457 (instance actor)
  └─ primer.graph (query actor)
```

**Example:**
```typescript
// Send to collection to create
await system.send("primer.tasks", "create", { goal: "Task 1" });
// Returns: { id: "task_123", address: "primer.tasks.task_123" }

// Send to instance to update
await system.send("primer.tasks.task_123", "update", { state: "completed" });
```

**Characteristics:**
- Fine-grained (instance-level)
- More actors = more overhead (but virtual actor pattern helps)
- State isolated per instance
- Better encapsulation

**Winner: Depends on use case**
- Hybrid: Simpler, less overhead
- Pure: Better isolation, more flexible

---

### 5. Network Protocol

#### Hybrid Design

**Protocol:** HTTP/1.1 (request/response)

**Message flow:**
```
CLI                         Daemon
 │                            │
 ├──HTTP POST /api/actor/msg→│
 │  { type: "create_task" }   │
 │                            ├─process
 │                            │
 │←────HTTP 200 OK───────────┤
    { success: true, data }   │
 │                            │
```

**Characteristics:**
- **Stateless** - Each request independent
- **Unidirectional** - CLI → Daemon only
- **Connection overhead** - New TCP per request (unless HTTP/2)
- **Simple** - Standard HTTP tools work (curl, Postman, proxies)

**Limitations:**
- Daemon cannot push to CLI (no server → client messages)
- Polling needed for real-time updates
- Connection overhead for frequent requests

#### Pure Actor Design

**Protocol:** WebSocket (persistent, bidirectional)

**Message flow:**
```
CLI                         Daemon
 │                            │
 ├──WS CONNECT ws://...─────→│
 │                            │
 ├──WS SEND (request)────────→│
 │  { targetAddress, message }│
 │                            ├─process
 │                            │
 │←──WS SEND (response)───────┤
 │  { id, response }          │
 │                            │
 │←──WS SEND (broadcast)──────┤ (unsolicited!)
    { type: "task_created" }  │
 │                            │
```

**Characteristics:**
- **Stateful** - Persistent connection
- **Bidirectional** - CLI ↔ Daemon (both can send)
- **Low latency** - No connection overhead
- **Real-time** - Push notifications work

**Benefits:**
- Server can push events to CLI
- Lower latency (persistent connection)
- Better for real-time updates
- Matches actor model (async messages)

**Winner: Pure Actor** (better fit for actor model, real-time)

---

### 6. Migration Risk

#### Hybrid Design

**Migration from current state:**
1. Create `DaemonClient` class
2. Add `/api/actor/message` endpoint to daemon
3. Transform CLI commands to use `daemon.send()`
4. Test

**Risk:** Low
- Incremental transformation (one CLI at a time)
- Fallback to direct mode built-in
- HTTP is well-understood
- Easy rollback (keep old code path)

**Effort:** ~2 weeks

#### Pure Actor Design

**Migration from current state:**
1. Create `AddressResolver`, `RemoteActorProxy`, `SystemConfig`
2. Implement WebSocket server in daemon
3. Convert all actors to factory pattern
4. Refactor CLIs to use addressing
5. Add supervision trees
6. Test all topologies (local, remote, hybrid)

**Risk:** Medium
- More moving parts
- New abstractions to learn
- WebSocket protocol less familiar
- Config-driven behavior (harder to debug)

**Effort:** ~5 weeks

**Winner: Hybrid** (lower risk, faster to implement)

---

### 7. Long-Term Scalability

#### Hybrid Design

**Scalability limits:**
- **Two locations only** - CLI or Daemon
- **Single daemon** - All state in one process
- **Horizontal scaling** - Requires load balancer + session affinity
- **Multi-region** - Not supported (would need federation protocol)

**Scaling strategy:**
- Vertical scaling (more RAM/CPU for daemon)
- Read replicas (separate daemon for reads)
- Sharding (multiple daemons, shard by workspace)

**Limitations:**
- Cannot split actors across machines (coarse-grained collections)
- Cannot place actors near data (all in daemon)
- Cannot optimize for latency (fixed topology)

#### Pure Actor Design

**Scalability options:**
- **Unlimited locations** - Local, daemon, remote, edge, multi-region
- **Fine-grained placement** - Per-actor configuration
- **Horizontal scaling** - Actors distributed across machines
- **Multi-region** - Native support (addresses resolved to closest instance)

**Scaling strategy:**
- Hot/cold tiering (RAM → disk → network)
- Geo-distribution (place actors near users)
- Load balancing (distribute actors across servers)
- Edge caching (read-heavy actors on CDN)

**Future possibilities:**
- Distributed graph (CozoDB on multiple machines)
- Actor migration (move hot actors to faster machines)
- Eventual consistency (multi-master writes)

**Winner: Pure Actor** (designed for distributed systems)

---

## Side-by-Side Examples

### Example 1: Create Task

#### Hybrid (a18ee7e)

```typescript
// CLI: src/cli/task.ts
import { daemon } from "../daemon-client.ts";

async function cmdAdd(goal: string, options: any) {
  const response = await daemon.send("create_task", {
    goal,
    deliverables: options.deliverables,
    labels: options.labels,
    priority: options.priority
  });

  if (!response.success) {
    throw new Error(response.error);
  }

  console.log(`Added task: ${response.data.id}`);
}

// Daemon: daemon/api/routes.ts
async function handleCreateTask(payload: any, coordinator: Coordinator) {
  const taskId = await coordinator.createTask({
    goal: payload.goal,
    desiredDeliverables: payload.deliverables || [],
    labels: payload.labels
  });

  return { id: taskId, goal: payload.goal };
}
```

#### Pure Actor

```typescript
// CLI: src/cli/task.ts
import { system } from "../system.ts";

async function cmdAdd(goal: string, options: any) {
  const response = await system.send("primer.tasks", "create", {
    goal,
    deliverables: options.deliverables,
    labels: options.labels,
    priority: options.priority
  });

  if (!response.success) {
    throw new Error(response.error);
  }

  console.log(`Added task: ${response.data.address}`);
}

// Actor: src/actors/task-collection-actor.ts
export const TaskCollectionActor: ActorFactory = (data) => {
  const actor = {
    send: async (message: Message): Promise<Response> => {
      if (message.type === "create") {
        const taskId = `task_${Date.now()}`;
        const taskAddr = `${data.address}.${taskId}`;

        // Create instance actor (virtual actor pattern)
        const taskActor = await data.resolver.resolve(taskAddr);
        await taskActor.send({
          id: crypto.randomUUID(),
          type: "init",
          payload: message.payload
        });

        return { success: true, data: { id: taskId, address: taskAddr } };
      }
      // ...
    }
  };

  return data.system.register(actor);
};
```

**Key Differences:**
- Hybrid: HTTP endpoint, message type string
- Pure: Address resolution, hierarchical naming

### Example 2: Update Task

#### Hybrid (a18ee7e)

```typescript
// CLI
await daemon.send("update_task", {
  id: "task_123",
  action: "complete",
  result: "Done"
});

// Daemon route
case "update_task": {
  const { id, action, result } = payload;
  await coordinator.updateTask(id, action, result);
  return { success: true };
}
```

#### Pure Actor

```typescript
// CLI (location transparent!)
await system.send("primer.tasks.task_123", "complete", {
  result: "Done"
});

// Task instance actor
const TaskInstanceActor: ActorFactory = (data) => {
  let state = { /* ... */ };

  const actor = {
    send: async (message: Message) => {
      if (message.type === "complete") {
        state.status = "completed";
        state.result = message.payload.result;

        // Persist via CozoDB actor
        await data.system.send("primer.cozodb.write", {
          id: crypto.randomUUID(),
          type: "update_task",
          payload: state
        });

        return { success: true, data: state };
      }
      // ...
    }
  };

  return data.system.register(actor);
};
```

**Key Differences:**
- Hybrid: ID passed in payload, coordinator handles update
- Pure: Address directly identifies task, actor handles own state

---

## Migration Path: Hybrid → Pure

### Strategy: Incremental Evolution

**Good news:** Hybrid design is a stepping stone to pure actor!

**Phase 1: Implement Hybrid (Now)**
- Get immediate value (solve CozoDB error)
- Learn actor messaging patterns
- Build CLI thin shell foundation
- **Duration:** 2-3 weeks

**Phase 2: Introduce Addressing (Later)**
- Add `AddressResolver` to hybrid system
- Make addresses first-class (not just strings)
- Support local config (for testing)
- **Duration:** 1 week

**Phase 3: Add Remote Proxies (Later)**
- Implement `RemoteActorProxy` for WebSocket
- Replace HTTP with WebSocket in daemon
- Test all-remote config
- **Duration:** 2 weeks

**Phase 4: Refactor to Instance Actors (Later)**
- Convert collection actors to factories
- Create instance actors (task_123, etc.)
- Update CLIs to use hierarchical addresses
- **Duration:** 2 weeks

**Total Migration Time:** ~7-8 weeks from hybrid to pure

### Backward Compatibility

**During migration:**
- Keep HTTP endpoint for old clients
- Support both `/api/actor/message` (HTTP) and WebSocket
- Gradual cutover (no big bang)

**After migration:**
- Remove HTTP endpoint
- Pure WebSocket + actor addressing
- Config determines all topology

---

## Recommendations

### Short-Term (Now - 3 months)

**Use Hybrid Design (a18ee7e)**

**Why:**
1. Solves immediate problem (CozoDB connection error)
2. Lower implementation risk
3. Faster to implement (2 weeks vs 5 weeks)
4. Easier to understand and debug
5. Good foundation for future evolution

**Deliverables:**
1. `DaemonClient` class
2. `/api/actor/message` endpoint
3. Transformed CLIs (thin shell)
4. Tests and documentation

### Long-Term (3-12 months)

**Evolve to Pure Actor Model**

**Why:**
1. Location transparency enables flexible deployment
2. Better testing (local config, no daemon)
3. Scalability (distributed systems)
4. Future-proof architecture
5. Matches actor model principles

**Triggers for migration:**
1. Need for distributed deployment (multi-region)
2. Performance optimization (hot/cold tiering)
3. Testing complexity (daemon dependencies)
4. Real-time requirements (server push)

### Hybrid Approach (Recommended)

**Phase 1:** Implement hybrid now (solve immediate problem)
**Phase 2:** Add addressing and resolution (improve testing)
**Phase 3:** Introduce remoting (location transparency)
**Phase 4:** Full pure actor (distributed systems)

**Benefits:**
- Incremental value delivery
- Lower risk at each phase
- Learn from production use
- Easy rollback at any phase

---

## Conclusion

### Summary Table

| Criterion | Hybrid | Pure | Recommended |
|-----------|--------|------|-------------|
| **Immediate Value** | High | Medium | **Hybrid** |
| **Implementation Time** | 2 weeks | 5 weeks | **Hybrid** |
| **Complexity** | Low | Medium | **Hybrid** |
| **Testing** | Medium | Excellent | **Pure** |
| **Scalability** | Limited | Unlimited | **Pure** |
| **Flexibility** | Low | High | **Pure** |
| **Future-Proof** | No | Yes | **Pure** |

### Final Recommendation

**Now (Week 1-4):** Implement hybrid actor/HTTP design (a18ee7e)
- Fast, low-risk solution to CozoDB error
- Establishes CLI thin shell pattern
- Foundation for future evolution

**Later (Month 3-6):** Evolve to pure actor with location transparency
- Add addressing and resolution
- Introduce remoting via WebSocket
- Refactor to instance-level actors
- Enable distributed deployment

**Migration is straightforward** - hybrid design is compatible with pure actor model. Most changes are config-driven, not code-driven.

---

**End of Comparison Document**
