# Path-Based Addressing

**Status:** Production Ready
**Date:** 2026-02-07
**Branch:** feature/path-addressing

---

## Overview

Path-based addressing enables hierarchical actor organization using filesystem-like paths instead of flat IDs. Actors are addressed using paths like `/workflows/tasks/task-123` instead of opaque IDs like `@(abc-def-123)`.

**Core Benefits:**
- **Human-readable addresses** - `/workflows/tasks/task-123` vs `@(abc-def-123)`
- **Hierarchical organization** - Natural namespacing and grouping
- **Access control** - Routing determines capabilities (pure actor model)
- **Performance** - Sub-millisecond routing latency with caching

---

## Architecture

### Addressing Scheme

**Path Format:** `/namespace/category/actor-id`

```typescript
// Examples
address('/workflows/tasks/task-123')        // Workflow task
address('/domain/users/user-456')           // Domain entity
address('/workflows/system/scheduler')      // System actor
address('/session-knowledge/system/http')   // Isolated HTTP client
```

**Address Types:**
- **Actor Addresses:** Hierarchical paths to actors
- **ID Addresses:** Legacy flat IDs `@(id)` (backward compatible)

### Routing Model

Routing follows the **supervision tree**. Messages flow down the hierarchy until reaching the target actor.

```
Root Supervisor
├─ /workflows (Supervisor)
│  ├─ /workflows/tasks (Supervisor)
│  │  └─ /workflows/tasks/task-123 (LeafActor)
│  └─ /workflows/system (Supervisor)
│     └─ /workflows/system/scheduler (SchedulerActor)
└─ /domain (Supervisor)
   └─ /domain/users (Supervisor)
      └─ /domain/users/user-456 (UserActor)
```

**Message Flow:**
```
Request to /workflows/tasks/task-123
  → Root delegates to /workflows
  → /workflows delegates to /workflows/tasks
  → /workflows/tasks delegates to task-123
  → LeafActor processes message
```

**Routing Latency:**
- 2-level routing: 0.003ms average
- 3-level routing: 0.011ms average
- P99 latency: <0.022ms

---

## PathResolver Utility

Core path operations are provided by `PathResolver`:

```typescript
import {
  parsePath,
  validatePath,
  normalizePath,
  matchPattern,
  getParentPath,
  joinPath
} from './messaging/path-resolver.ts';

// Parse path into segments
const segments = parsePath('/workflows/tasks/task-123');
// ['workflows', 'tasks', 'task-123']

// Validate path format
const isValid = validatePath('/workflows/tasks/task-123');
// true

// Normalize path
const normalized = normalizePath('/Workflows//Tasks/task-123');
// '/workflows/tasks/task-123'

// Match pattern with wildcards
matchPattern('/workflows/*/task-*', '/workflows/tasks/task-123');
// true

matchPattern('/workflows/**', '/workflows/tasks/task-123');
// true (** matches all descendants)

// Get parent path
getParentPath('/workflows/tasks/task-123');
// '/workflows/tasks'

// Join paths
joinPath('/workflows', 'tasks', 'task-123');
// '/workflows/tasks/task-123'
```

**Performance:**
- `parsePath`: 0.00027ms per operation
- `normalizePath`: 0.00031ms per operation
- `validatePath`: 0.00040ms per operation
- `matchPattern`: 0.00020-0.00048ms per operation

All operations are sub-millisecond for 1000 iterations.

---

## Hierarchical Routing

### Supervisor Pattern

Supervisors delegate messages to children based on path matching:

```typescript
import { PathSupervisor } from './messaging/hierarchical-routing-poc.ts';

class WorkflowsSupervisor extends PathSupervisor {
  constructor(id: string, router: MessageRouter) {
    super(id, router, '/workflows');

    // Register child supervisors
    this.registerChild('/workflows/tasks', tasksSupervisor);
    this.registerChild('/workflows/system', systemSupervisor);
  }
}

// Message to /workflows/tasks/task-123
// → WorkflowsSupervisor receives message
// → Delegates to tasksSupervisor (matches /workflows/tasks)
// → tasksSupervisor delegates to task-123 actor
```

### Leaf Actors

Leaf actors are endpoints that process messages:

```typescript
import { LeafActor } from './messaging/hierarchical-routing-poc.ts';

class TaskActor extends LeafActor {
  constructor(id: string, router: MessageRouter, path: string) {
    super(id, router, path);
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Process message at this endpoint
    return createResponse(message, { processed: true });
  }
}

// Register at specific path
const taskActor = new TaskActor('task-123', router, '/workflows/tasks/task-123');
```

---

## Access Control Through Routing

**Core Principle:** Capabilities are actors. Access control = routing decisions.

```typescript
// Workflows CAN use HTTP (registered at /workflows/system/http)
const workflowsHttp = new HTTPClientActor('workflows-http', router, {
  allowedHosts: ['api.anthropic.com', 'api.github.com'],
  methods: ['GET', 'POST'],
  rateLimit: { requests: 100, window: 60000 },
  timeout: 30000
});
router.registerActor('/workflows/system/http', workflowsHttp);

// Session knowledge CAN use HTTP (different config)
const knowledgeHttp = new HTTPClientActor('knowledge-http', router, {
  allowedHosts: ['cloudflare.com'],
  methods: ['POST'],
  rateLimit: { requests: 10, window: 60000 },
  timeout: 5000
});
router.registerActor('/session-knowledge/system/http', knowledgeHttp);

// Domain actors CANNOT use HTTP (not registered)
// Messages to /domain/system/http fail with "Actor not found"
```

**Benefits:**
- **Explicit access** - Registration = permission
- **Isolation** - Different namespaces = different capabilities
- **No helper classes** - Actors are capabilities
- **Audit trail** - All access through routing

---

## Performance

### Routing Overhead

**Without Caching (POC Phase 4-5):**
```
Flat routing (direct):      0.001ms avg
Hierarchical (2-level):     0.002ms avg
Overhead:                   86%
```

**With Caching (Phase 6-7):**
```
Hierarchical (cached):      0.001ms avg
Overhead:                   24%
Cache hit rate:             99.9%
Throughput:                 578K msg/s (81% of flat routing)
```

**Conclusion:** With caching, hierarchical routing approaches flat routing performance while providing namespacing and access control benefits.

### Memory

**Cache Bounds:**
- Maximum 1000 entries (~100KB memory)
- LRU eviction when full
- No memory leaks detected

---

## Migration from Flat IDs

### Backward Compatibility

Both addressing schemes are supported:

```typescript
// New: Path-based addressing
await actor.ask(address('/workflows/system/scheduler'), 'schedule', { ... });

// Old: ID-based addressing (still works)
await actor.ask(address('@(scheduler-id)'), 'schedule', { ... });
```

### Migration Strategy

1. **Register actors at paths** (in addition to IDs)
2. **Update new code** to use path-based addresses
3. **Legacy code** continues using ID addresses
4. **Gradually migrate** existing code to paths
5. **Eventually deprecate** ID-only registration

```typescript
// Migration example
const scheduler = new SchedulerActor(router);

// Phase 1: Register both
router.registerActor('/workflows/system/scheduler', scheduler);
router.registerById('@(scheduler-id)', scheduler); // legacy

// Phase 2: New code uses path
await actor.ask(address('/workflows/system/scheduler'), 'schedule', { ... });

// Phase 3: Legacy code still works
await actor.ask(address('@(scheduler-id)'), 'schedule', { ... });

// Phase 4: Deprecate ID registration
// router.registerById('@(scheduler-id)', scheduler); // removed
```

---

## Usage Examples

### System Actor Registration

```typescript
import { MessageRouter } from './messaging/router.ts';
import { SchedulerActor, StorageActor, HTTPClientActor } from './system-actors/index.ts';

const router = new MessageRouter(store);

// Register system actors in /workflows namespace
const scheduler = new SchedulerActor(router, { clock: 'real' });
router.registerActor('/workflows/system/scheduler', scheduler);

const storage = new StorageActor('workflows-storage', router, {
  allowedKeys: ['workflows.*', 'tasks.*'],
  maxSize: 10 * 1024 * 1024 // 10MB
});
router.registerActor('/workflows/system/storage', storage);

const http = new HTTPClientActor('workflows-http', router, {
  methods: ['GET', 'POST'],
  allowedHosts: ['api.anthropic.com'],
  rateLimit: { requests: 100, window: 60000 },
  timeout: 30000
});
router.registerActor('/workflows/system/http', http);

// Register in /domain namespace (different config)
const domainStorage = new StorageActor('domain-storage', router, {
  allowedKeys: ['users.*', 'entities.*'],
  maxSize: 50 * 1024 * 1024 // 50MB
});
router.registerActor('/domain/system/storage', domainStorage);
```

### Using Path Addresses

```typescript
class WorkflowActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // Schedule delayed execution
    await this.ask(
      address('/workflows/system/scheduler'),
      'schedule',
      { delay: 5000, messageType: 'delayed-task' }
    );

    // Store workflow state
    await this.ask(
      address('/workflows/system/storage'),
      'storage.set',
      { key: 'workflow.state', value: this.state }
    );

    // Make HTTP request
    const response = await this.ask(
      address('/workflows/system/http'),
      'http.post',
      {
        url: 'https://api.anthropic.com/v1/messages',
        body: { model: 'claude-sonnet-4.5', ... }
      }
    );

    return createResponse(message, { success: true });
  }
}
```

### Pattern Matching

```typescript
// Match all tasks in workflows namespace
matchPattern('/workflows/tasks/*', '/workflows/tasks/task-123');
// true

// Match all descendants of workflows
matchPattern('/workflows/**', '/workflows/tasks/subtasks/task-456');
// true

// Match specific pattern
matchPattern('/workflows/*/task-*', '/workflows/active/task-123');
// true
```

---

## Implementation Files

**Core Implementation:**
- `src/messaging/path-resolver.ts` (307 lines) - Path utilities
- `src/messaging/hierarchical-routing-poc.ts` (247 lines) - Supervisor pattern POC
- `src/messaging/message.ts` - Address creation and validation

**Tests:**
- `src/messaging/__tests__/path-resolver.test.ts` (69 tests)
- `src/messaging/__tests__/hierarchical-routing.test.ts` (17 tests)
- `src/messaging/__tests__/path-performance.test.ts` (68 tests)

**Documentation:**
- `docs/PATH_ADDRESSING_POC_SUMMARY.md` - POC results
- `docs/PATH_ADDRESSING_DESIGN.md` - Detailed design
- `docs/phases/PHASE_5_7_IMPLEMENTATION_PLAN.md` - Implementation phases

**Total:** 898 lines of implementation, 154 tests passing

---

## Design Decisions

### Why Paths Over Flat IDs?

**Problem with Flat IDs:**
```typescript
// What namespace? What capabilities?
await actor.ask(address('@(abc-123)'), 'http.get', { ... });
```

**Solution with Paths:**
```typescript
// Clear namespace and capabilities
await actor.ask(address('/workflows/system/http'), 'http.get', { ... });
```

**Benefits:**
1. **Discoverability** - Path reveals purpose
2. **Namespacing** - Natural isolation
3. **Access control** - Routing = permissions
4. **Human-readable** - Self-documenting

### Why Supervision Tree Routing?

**Alternative:** Central registry with path lookups

**Chosen Approach:** Delegate through supervision tree

**Rationale:**
- **No central bottleneck** - Distributed routing
- **Natural hierarchy** - Follows actor supervision
- **Fault isolation** - Supervisor restarts don't affect siblings
- **Performance** - Sub-millisecond latency with caching

### Why Allow Both Paths and IDs?

**Backward compatibility** during migration. Existing code using IDs continues working while new code adopts paths.

---

## Future Enhancements

### Phase 8+ (Planned)

1. **Dynamic Path Registration**
   - Actors register themselves at construction
   - No manual `router.registerActor()` calls

2. **Path-Based Subscriptions**
   - Subscribe to all messages matching pattern
   - `subscribe('/workflows/tasks/*')` receives all task messages

3. **Path-Based Queries**
   - Query actors by path pattern
   - `findActors('/workflows/**')` returns all workflow actors

4. **Path Permissions**
   - Define allowed paths per namespace
   - Enforce at routing level

---

## References

- **POC Summary:** `docs/PATH_ADDRESSING_POC_SUMMARY.md`
- **Design Document:** `docs/PATH_ADDRESSING_DESIGN.md`
- **Implementation Plan:** `docs/phases/PHASE_5_7_IMPLEMENTATION_PLAN.md`
- **Architecture:** `ARCHITECTURE.md` (Path addressing section)

---

**Document End**
