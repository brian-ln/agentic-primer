# Entangled Actors Pattern - Recommendations for tk-agents

**Date:** January 17, 2026
**Context:** Workbench V2 Actor System Design
**Pattern Source:** Actor System Meta-Model project

## Executive Summary

The entangled actors pattern is **highly recommended** for tk-agents workbench design. It provides transparent browser ↔ daemon communication with zero external dependencies, automatic reconnection, and natural request/response semantics that align perfectly with the workbench requirements.

## Pattern Applicability Assessment

### Current tk-agents Architecture

**Workbench Requirements:**
- Browser-based UI for task management
- Daemon process for persistence (CozoDB)
- Real-time updates (file watching, task changes)
- Query operations (list tasks, dependency graph)
- Mutation operations (create, update, delete tasks)

**Current Gaps:**
- No browser ↔ daemon communication layer
- No real-time update mechanism
- No workbench UI implementation

### How Entangled Actors Solves These

| Requirement | Entangled Actors Solution | Benefit |
|-------------|---------------------------|---------|
| **Browser ↔ Daemon** | Client transport in browser, server transport in daemon | Zero-dependency communication |
| **Real-time Updates** | Server can broadcast to all connected browsers | Task updates appear instantly |
| **Query Operations** | Request/response pattern with async/await | Natural TypeScript API |
| **Mutation Operations** | Same request/response pattern | Consistent interface |
| **Reconnection** | Automatic with exponential backoff | Handles daemon restarts gracefully |

## Recommended Implementation

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Browser (Workbench UI)                    │
│                                                              │
│  ┌────────────────┐  ┌──────────────────────────────────┐  │
│  │ UI Components  │  │  Browser Actor Runtime           │  │
│  │                │  │                                  │  │
│  │ - Task List    │  │  - TaskListActor                 │  │
│  │ - Task Detail  │  │  - TaskEditorActor               │  │
│  │ - Dep Graph    │  │  - GraphViewActor                │  │
│  │ - Status View  │  │  - WebSocketClientTransportActor │  │
│  └────────┬───────┘  └───────────┬──────────────────────┘  │
│           │                      │                          │
└───────────┼──────────────────────┼──────────────────────────┘
            │                      │
            │  Component calls     │  WebSocket
            │  ask actor           │  ws://localhost:9876
            │                      │
┌───────────┼──────────────────────┼──────────────────────────┐
│           │                      │                          │
│  ┌────────▼───────┐  ┌──────────▼──────────────────────┐  │
│  │ Actor System   │  │  Daemon Actor Runtime           │  │
│  │                │  │                                  │  │
│  │ - Routes msgs  │  │  - TaskManagerActor             │  │
│  │ - Broadcasts   │  │  - GraphQueryActor              │  │
│  │   updates      │  │  - FileWatcherActor             │  │
│  │                │  │  - WebSocketServerTransportActor│  │
│  └────────┬───────┘  └───────────┬──────────────────────┘  │
│           │                      │                          │
│           │                      │  CozoDB queries          │
│           │  Actor messages      │                          │
│           │                      ▼                          │
│           │              ┌───────────────┐                  │
│           │              │   CozoDB      │                  │
│           │              │  (tasks.db)   │                  │
│           │              └───────────────┘                  │
│                                                              │
│                    Daemon Process (Bun)                     │
└──────────────────────────────────────────────────────────────┘
```

### Component Breakdown

#### Browser Side

**1. WebSocketClientTransportActor**
```typescript
// Extracted from: runtimes/browser/websocket-client-transport.ts
// Size: ~377 lines (production ready)
// Dependencies: None (uses built-in WebSocket API)

class WebSocketClientTransportActor extends Actor {
  // Connects to daemon at ws://localhost:9876
  // Handles reconnection on daemon restart
  // Routes messages to daemon actors
}
```

**2. UI-Actor Integration**
```typescript
class TaskListActor extends Actor {
  async accept(message: ActorMessage) {
    if (message.type === 'load-tasks') {
      // Transparent call to daemon
      const tasks = await this.runtime.ask('daemon/task-manager', {
        type: 'list-tasks',
        data: { label: message.data.label }
      });

      // Update UI
      this.updateUI(tasks);
      return tasks;
    }
  }
}
```

#### Daemon Side

**3. WebSocketServerTransportActor**
```typescript
// Extracted from: runtimes/server/websocket-server-transport.ts
// Size: ~490 lines (production ready)
// Dependencies: None (uses Bun.serve WebSocket support)

class WebSocketServerTransportActor extends Actor {
  // Listens on port 9876
  // Routes messages to TaskManagerActor, GraphQueryActor, etc.
  // Broadcasts task updates to all connected browsers
}
```

**4. TaskManagerActor**
```typescript
class TaskManagerActor extends Actor {
  constructor(private cozoClient: CozoClient) {
    super('task-manager', 'Manage tasks via CozoDB');
  }

  async accept(message: ActorMessage) {
    switch (message.type) {
      case 'list-tasks':
        return this.listTasks(message.data);

      case 'create-task':
        const task = await this.createTask(message.data);
        // Broadcast update to all connected browsers
        await this.runtime.tell('websocket-server-transport', {
          type: 'broadcast',
          data: {
            type: 'actor-message',
            messageType: 'task-created',
            data: task
          }
        });
        return task;

      case 'update-task':
        const updated = await this.updateTask(message.data);
        // Broadcast update
        await this.runtime.tell('websocket-server-transport', {
          type: 'broadcast',
          data: {
            type: 'actor-message',
            messageType: 'task-updated',
            data: updated
          }
        });
        return updated;
    }
  }

  private async listTasks(filters: any) {
    const result = await this.cozoClient.run(`
      ?[id, goal, status, priority] :=
        *task{id, goal, status, priority},
        ${this.buildFilters(filters)}
    `);
    return result.rows.map(row => this.rowToTask(row));
  }
}
```

## Implementation Steps

### Phase 1: Extract Transport Code

1. Copy `websocket-server-transport.ts` from Actor System Meta-Model
2. Copy `websocket-client-transport.ts` (browser version)
3. Place in `src/actors/` directory
4. Minimal modifications needed (already zero-dependency)

**Estimated Effort:** 2-4 hours

### Phase 2: Create Daemon Actors

1. Create `TaskManagerActor` (wraps CozoDB queries)
2. Create `GraphQueryActor` (dependency graph queries)
3. Create `FileWatcherActor` (already exists, integrate with broadcast)

**Estimated Effort:** 8-12 hours

### Phase 3: Setup Daemon Runtime

1. Create daemon entry point (`src/daemon/workbench-daemon.ts`)
2. Initialize actor runtime
3. Register actors (TaskManager, GraphQuery, FileWatcher)
4. Register WebSocketServerTransport
5. Start listening on port 9876

**Estimated Effort:** 4-6 hours

### Phase 4: Create Browser Workbench

1. Create minimal HTML/CSS workbench UI
2. Create browser actor runtime
3. Create UI actors (TaskListActor, TaskEditorActor, GraphViewActor)
4. Register WebSocketClientTransport
5. Connect to daemon

**Estimated Effort:** 16-24 hours

### Phase 5: Implement Real-time Updates

1. TaskManagerActor broadcasts on mutations
2. FileWatcherActor broadcasts on file changes
3. Browser actors handle broadcast messages
4. Update UI reactively

**Estimated Effort:** 6-8 hours

**Total Estimated Effort:** 36-54 hours (1-2 weeks)

## Code Size Estimates

| Component | Lines of Code | Notes |
|-----------|--------------|-------|
| **Transport (Reused)** | ~900 lines | Already production-ready |
| WebSocketServerTransport | ~490 | Copy from Actor System Meta-Model |
| WebSocketClientTransport | ~377 | Copy from Actor System Meta-Model |
| TransportMessage protocol | ~30 | Copy interface definition |
| **New Daemon Code** | ~600 lines | Custom to tk-agents |
| TaskManagerActor | ~200 | CRUD + CozoDB queries |
| GraphQueryActor | ~150 | Dependency graph queries |
| Daemon runtime setup | ~100 | Initialize actors, start server |
| FileWatcher integration | ~150 | Broadcast file changes |
| **New Browser Code** | ~800 lines | Custom to tk-agents |
| TaskListActor | ~150 | List view, filtering |
| TaskEditorActor | ~200 | Create/edit tasks |
| GraphViewActor | ~150 | Dependency graph visualization |
| Browser runtime setup | ~100 | Initialize actors, connect |
| HTML/CSS workbench UI | ~200 | Basic layout and styles |
| **Total New Code** | ~1,400 lines | Plus ~900 reused transport code |

## Benefits for tk-agents

### 1. Zero External Dependencies

Aligns perfectly with tk-agents philosophy:
- No Express, no Socket.io, no external WebSocket libraries
- Uses Bun's built-in `Bun.serve()` WebSocket support
- Browser uses native `WebSocket` API

### 2. Transparent Distribution

Daemon actors look like local actors to browser:

```typescript
// Browser code - looks local
const tasks = await runtime.ask('daemon/task-manager', {
  type: 'list-tasks',
  data: { label: 'agent' }
});

// But actually goes: Browser → WebSocket → Daemon → CozoDB → Response
```

### 3. Automatic Reconnection

Handles daemon restarts gracefully:
- Browser detects disconnect
- Exponential backoff: 1s, 2s, 4s, 8s, 16s, 30s (max)
- Auto-reconnects when daemon comes back
- UI shows connection status

### 4. Real-time Updates

Daemon can push updates to all browsers:

```typescript
// Daemon broadcasts task update
await runtime.tell('websocket-server-transport', {
  type: 'broadcast',
  data: {
    type: 'actor-message',
    messageType: 'task-updated',
    data: { taskId: 'task_5', status: 'complete' }
  }
});

// All connected browsers receive update immediately
// No polling required
```

### 5. Request/Response Pattern

Natural async/await API:

```typescript
// Promise-based, no callbacks
const result = await runtime.ask('daemon/graph-query', {
  type: 'get-dependencies',
  data: { taskId: 'task_5' }
});
```

### 6. Built-in Error Handling

Network errors appear as actor errors:

```typescript
try {
  const result = await runtime.ask('daemon/task-manager', {
    type: 'create-task',
    data: { goal: 'New task' }
  });
} catch (error) {
  // Could be:
  // - Network error (daemon down)
  // - Actor error (validation failed)
  // - Timeout error (slow query)
  // All handled the same way
}
```

## Performance Characteristics

Based on Actor System Meta-Model validation:

| Metric | Expected Value | Notes |
|--------|---------------|-------|
| **Latency** | < 100ms | Sub-100ms for simple queries |
| **Throughput** | > 10 msg/s | Hundreds of messages/second possible |
| **Reconnection** | 1-30s | Exponential backoff, configurable |
| **Memory** | < 1MB | Per connection, includes pending requests |
| **Message Size** | < 1MB | WebSocket frame limit, chunking possible |

## Testing Strategy

### Unit Tests

```typescript
import { test, expect } from 'bun:test';
import { WebSocketServerTransportActor } from './websocket-server-transport';

test('server transport starts successfully', async () => {
  const transport = new WebSocketServerTransportActor();
  const result = await transport.accept({
    type: 'start-server',
    data: { port: 9876 }
  });

  expect(result.success).toBe(true);
  expect(result.port).toBe(9876);
});
```

### Integration Tests

```typescript
test('browser can query daemon tasks', async () => {
  // Start daemon with test database
  const daemon = await startTestDaemon();

  // Connect browser runtime
  const browserRuntime = createBrowserRuntime();
  await browserRuntime.tell('websocket-client-transport', {
    type: 'connect',
    data: { url: 'ws://localhost:9876' }
  });

  // Query tasks
  const tasks = await browserRuntime.ask('daemon/task-manager', {
    type: 'list-tasks',
    data: {}
  });

  expect(tasks.length).toBeGreaterThan(0);

  // Cleanup
  await daemon.stop();
});
```

### End-to-End Tests

```typescript
test('file change triggers browser update', async () => {
  const daemon = await startTestDaemon();
  const browser = await connectTestBrowser();

  // Setup listener
  const updates: any[] = [];
  browser.onMessage('file-changed', (msg) => {
    updates.push(msg.data);
  });

  // Trigger file change
  await writeFile('test-file.md', 'Updated content');

  // Wait for update
  await new Promise(resolve => setTimeout(resolve, 500));

  expect(updates.length).toBe(1);
  expect(updates[0].path).toContain('test-file.md');
});
```

## Risks and Mitigations

### Risk 1: WebSocket Connection Limits

**Risk:** Daemon limited to 1000s of concurrent connections
**Impact:** Medium (unlikely for local workbench)
**Mitigation:** Not a concern for single-user local workbench

### Risk 2: Message Size Limits

**Risk:** Large dependency graphs may exceed WebSocket frame size
**Impact:** Low (tasks are small)
**Mitigation:** Implement chunking if needed, or use pagination

### Risk 3: State Synchronization

**Risk:** Browser and daemon state may diverge
**Impact:** Medium (stale UI)
**Mitigation:** Implement version numbers, periodic refresh

### Risk 4: Security

**Risk:** WebSocket server accessible to other processes
**Impact:** Low (localhost only)
**Mitigation:** Bind to 127.0.0.1 only, add authentication token if needed

## Alternative Approaches Considered

### 1. HTTP REST API

**Pros:**
- Standard, well-understood
- Easy to test with curl

**Cons:**
- No real-time updates (requires polling)
- More boilerplate code
- Separate transport from actor system

**Verdict:** Entangled actors superior for real-time workbench

### 2. gRPC

**Pros:**
- Efficient binary protocol
- Built-in streaming

**Cons:**
- External dependency (violates zero-dependency goal)
- More complex setup
- Overkill for local communication

**Verdict:** Entangled actors better fit

### 3. IPC (Inter-Process Communication)

**Pros:**
- Fastest for local communication
- Native OS support

**Cons:**
- Not browser-compatible
- Platform-specific
- Can't support remote workbench in future

**Verdict:** Entangled actors more flexible

## Recommendation Summary

### Strong Recommendation: Adopt Entangled Actors Pattern

**Reasons:**
1. ✅ **Zero Dependencies** - Aligns with tk-agents philosophy
2. ✅ **Production Ready** - 490 + 377 lines of validated code to reuse
3. ✅ **Real-time** - Built-in broadcast for instant updates
4. ✅ **Transparent** - Actors don't know about distribution
5. ✅ **Robust** - Automatic reconnection, error handling
6. ✅ **Testable** - Unit, integration, e2e tests all straightforward
7. ✅ **Scalable** - Can extend to remote workbench later

**Implementation Effort:** 1-2 weeks (36-54 hours)
**Code Reuse:** ~900 lines of production-ready transport code
**New Code:** ~1,400 lines custom to tk-agents

**Next Steps:**
1. Extract transport code from Actor System Meta-Model
2. Create TaskManagerActor wrapping CozoDB
3. Setup daemon runtime with WebSocket server
4. Build minimal browser workbench UI
5. Test end-to-end communication
6. Iterate on UI polish and features

---

**Recommendation Status:** APPROVED for tk-agents workbench V2
**Confidence:** High (pattern proven in production-like validation)
**Risk Level:** Low (well-understood, tested pattern)
