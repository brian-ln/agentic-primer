# Namespace Routing for System Actors

**Last Updated:** 2026-02-07
**Status:** Production Pattern

---

## Overview

System actors use **namespace routing** to provide different capabilities to different parts of the application. Instead of all actors having the same access, each namespace (workflows, tasks, domain, etc.) gets its own set of system actors with different configurations.

**Key Principle:** Access control happens through routing registration, not through helper classes or permission checks in business logic.

---

## Architecture

### Namespace Hierarchy

```
/system/                       # Global shared system actors
  /scheduler                   # Shared time/scheduling (singleton)
  /logger                      # Shared logging (singleton)

/workflows/system/             # Workflow namespace system actors
  /storage                     # Workflow database (read/write, all tables)
  /fs                          # Filesystem (read/write to workflows/ dir)
  /http                        # HTTP client (all methods, specific hosts)
  /websocket                   # WebSocket (specific hosts)

/tasks/system/                 # Task namespace system actors
  /storage                     # Task database (read/write, tasks tables only)
  /fs                          # Filesystem (read/write to tasks/ dir)
  /http                        # HTTP client (rate limited)
  /websocket                   # WebSocket (specific hosts)

/domain/system/                # Domain logic namespace (most restricted)
  /storage                     # Domain database (read-only)
  /fs                          # NOT REGISTERED (no filesystem access)
  /http                        # NOT REGISTERED (no network access)
  /websocket                   # NOT REGISTERED (no WebSocket access)

/session-knowledge/system/     # Session knowledge namespace
  /storage                     # Dedicated database (session.db)
  /fs                          # Read-only access to session logs
  /http                        # HTTP allowed (for embedding services)
```

### Access Control Model

**Registration = Permission**

If a system actor is registered at a namespace path, actors in that namespace can use it:

```typescript
// Workflow actors CAN use storage
router.registerActor('/workflows/system/storage', workflowsStorage);

// Domain actors CANNOT use storage (not registered)
// router.registerActor('/domain/system/storage', domainStorage); ← NOT REGISTERED

// Usage
await workflowActor.ask(address('/workflows/system/storage'), 'storage.query', {...});
// ✅ Success - storage registered for workflows

await domainActor.ask(address('/domain/system/storage'), 'storage.query', {...});
// ❌ Error: No actor registered at /domain/system/storage
```

---

## Sharing Strategies

### Strategy 1: Singleton (Shared Instance)

One instance shared by all namespaces. Used for stateless or global services.

**Example: SchedulerActor**

```typescript
// Create one scheduler instance
const scheduler = new SchedulerActor(router, { clock: 'real' });

// Register at global path
router.registerActor('/system/scheduler', scheduler);

// All actors use the same scheduler
await workflowActor.ask(address('/system/scheduler'), 'scheduler.schedule', {...});
await taskActor.ask(address('/system/scheduler'), 'scheduler.schedule', {...});
await domainActor.ask(address('/system/scheduler'), 'scheduler.schedule', {...});
// All use the same SchedulerActor instance
```

**When to use:**
- Stateless actors (no namespace-specific state)
- Global services (logging, metrics, scheduling)
- Performance-critical (avoid instance overhead)

### Strategy 2: Proxy (Delegation)

One namespace delegates to another's system actor. Used for shared resources.

**Example: Task storage proxies to Workflow storage**

```typescript
// Create workflow storage (real instance)
const workflowsStorage = new StorageActor(router, {
  dbPath: 'workflows.db',
  allowedTables: ['tasks', 'workflows', 'relationships']
});
router.registerActor('/workflows/system/storage', workflowsStorage);

// Create task storage (proxy to workflows)
const tasksStorageProxy = new ProxyActor(router, '/workflows/system/storage');
router.registerActor('/tasks/system/storage', tasksStorageProxy);

// Task actors think they have their own storage
await taskActor.ask(address('/tasks/system/storage'), 'storage.query', {...});
// → Routed to /workflows/system/storage (same database)
```

**When to use:**
- Shared databases (tasks and workflows in same DB)
- Resource consolidation (reduce connection pools)
- Transparent delegation (actors don't need to know)

### Strategy 3: Isolated (Dedicated Instance)

Each namespace gets its own isolated instance. Used for complete isolation.

**Example: Session knowledge has its own database**

```typescript
// Workflows storage
const workflowsStorage = new StorageActor(router, {
  dbPath: 'workflows.db',
  allowedTables: ['tasks', 'workflows']
});
router.registerActor('/workflows/system/storage', workflowsStorage);

// Session knowledge storage (different database)
const sessionStorage = new StorageActor(router, {
  dbPath: 'session.db',
  allowedTables: ['decisions', 'learnings', 'errors']
});
router.registerActor('/session-knowledge/system/storage', sessionStorage);

// Completely separate databases
await workflowActor.ask(address('/workflows/system/storage'), 'storage.query', {...});
// → workflows.db

await sessionActor.ask(address('/session-knowledge/system/storage'), 'storage.query', {...});
// → session.db
```

**When to use:**
- Data isolation requirements (separate DBs)
- Different configurations (different paths, hosts, etc.)
- Security boundaries (untrusted code)

---

## Configuration Examples

### StorageActor (Database)

```typescript
// Workflows: Full access
const workflowsStorage = new StorageActor(router, {
  allowedTables: ['tasks', 'workflows', 'relationships', 'users'],
  operations: ['read', 'write', 'delete', 'admin'],
});
router.registerActor('/workflows/system/storage', workflowsStorage);

// Domain: Read-only
const domainStorage = new StorageActor(router, {
  allowedTables: ['tasks', 'workflows'],
  operations: ['read'], // No write, delete, admin
});
router.registerActor('/domain/system/storage', domainStorage);

// Session knowledge: Isolated database
const sessionStorage = new StorageActor(router, {
  dbPath: 'session.db',
  allowedTables: '*', // All tables in this DB
  operations: ['read', 'write', 'delete'],
});
router.registerActor('/session-knowledge/system/storage', sessionStorage);
```

### FileSystemActor (File I/O)

```typescript
// Workflows: Read/write to workflows directory
const workflowsFs = new FileSystemActor(router, {
  allowedPaths: ['/data/workflows', '/tmp/workflows'],
  operations: ['read', 'write', 'delete'],
});
router.registerActor('/workflows/system/fs', workflowsFs);

// Session knowledge: Read-only to session logs
const sessionFs = new FileSystemActor(router, {
  allowedPaths: ['/logs/sessions'],
  operations: ['read'], // No write or delete
});
router.registerActor('/session-knowledge/system/fs', sessionFs);

// Domain: NO filesystem access (not registered)
// router.registerActor('/domain/system/fs', ...) ← NOT REGISTERED
```

### HTTPClientActor (Network Requests)

```typescript
// Workflows: Full HTTP access
const workflowsHttp = new HTTPClientActor(router, {
  allowedMethods: ['GET', 'POST', 'PUT', 'DELETE'],
  allowedHosts: [
    'api.anthropic.com',
    'api.github.com',
    'api.openai.com'
  ],
  rateLimit: { requests: 100, window: 60000 }, // 100 req/min
  timeout: 30000,
});
router.registerActor('/workflows/system/http', workflowsHttp);

// Tasks: Rate-limited HTTP
const tasksHttp = new HTTPClientActor(router, {
  allowedMethods: ['GET', 'POST'],
  allowedHosts: ['api.github.com'], // Limited hosts
  rateLimit: { requests: 10, window: 60000 }, // 10 req/min
  timeout: 10000,
});
router.registerActor('/tasks/system/http', tasksHttp);

// Domain: NO HTTP access (not registered)
// router.registerActor('/domain/system/http', ...) ← NOT REGISTERED
```

### WebSocketActor (Real-time Connections)

```typescript
// Workflows: WebSocket connections allowed
const workflowsWs = new WebSocketActor(router, {
  allowedHosts: ['wss://api.example.com', 'wss://updates.github.com'],
  maxConnections: 10,
  reconnect: true,
  reconnectDelay: 1000,
});
router.registerActor('/workflows/system/websocket', workflowsWs);

// Domain: NO WebSocket access (not registered)
// router.registerActor('/domain/system/websocket', ...) ← NOT REGISTERED
```

---

## Usage Patterns

### From Business Actors

Business actors use system actors through standard ask/tell messaging:

```typescript
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'create') {
      // Use StorageActor (via namespace routing)
      const response = await this.ask(
        address('/workflows/system/storage'),
        'storage.execute',
        {
          sql: 'INSERT INTO tasks (title, status) VALUES (?, ?)',
          params: [message.payload.title, 'open']
        }
      );

      if (!response.success) {
        return createErrorResponse(message, response.error);
      }

      // Use FileSystemActor (via namespace routing)
      await this.ask(
        address('/workflows/system/fs'),
        'fs.write',
        {
          path: `/data/workflows/task-${response.payload.id}.json`,
          content: JSON.stringify(message.payload),
        }
      );

      return createResponse(message, { id: response.payload.id });
    }

    return createErrorResponse(message, 'Unknown message type');
  }
}
```

### Denied Access Example

```typescript
class DomainActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // This will FAIL - domain actors have no HTTP access
    const response = await this.ask(
      address('/domain/system/http'),
      'http.get',
      { url: 'https://api.example.com/data' }
    );

    // response.success === false
    // response.error === "No actor registered at /domain/system/http"

    return createErrorResponse(message, 'Domain actors cannot make HTTP requests');
  }
}
```

---

## Security Implications

### Defense in Depth

Namespace routing provides multiple security layers:

1. **Routing Layer:** No registration = no access (enforced by router)
2. **Actor Layer:** Internal validation (allowedTables, allowedPaths, allowedHosts)
3. **Message Layer:** Type safety and schema validation

**Example Attack Prevention:**

```typescript
// Attacker tries to access restricted table
await attackerActor.ask(
  address('/domain/system/storage'),
  'storage.query',
  { sql: 'SELECT * FROM users' } // Sensitive table
);

// Layer 1: Router denies (no actor registered at /domain/system/storage)
// ❌ Error: No actor registered

// Even if registered, Layer 2 would deny:
// StorageActor checks allowedTables
// ❌ Error: Access denied - table 'users' not allowed

// Even if table allowed, Layer 3 validates SQL:
// Query parser checks for malicious patterns
// ❌ Error: SQL injection detected
```

### Principle of Least Privilege

Each namespace gets only the capabilities it needs:

**Workflows (Most Privileged):**
- ✅ Database (read/write all tables)
- ✅ Filesystem (read/write workflows dir)
- ✅ HTTP (all methods, specific hosts)
- ✅ WebSocket (specific hosts)

**Tasks (Moderate Privilege):**
- ✅ Database (read/write task tables only)
- ✅ Filesystem (read/write tasks dir only)
- ✅ HTTP (rate limited, limited hosts)
- ✅ WebSocket (limited hosts)

**Domain (Least Privileged):**
- ✅ Database (read-only, limited tables)
- ❌ Filesystem (denied)
- ❌ HTTP (denied)
- ❌ WebSocket (denied)

---

## Testing Namespace Isolation

### Integration Tests

```typescript
describe('Namespace Routing - Access Control', () => {
  test('Workflows can access storage, Domain cannot', async () => {
    // Setup routing
    const workflowsStorage = new StorageActor(router, {
      allowedTables: ['tasks'],
      operations: ['read', 'write']
    });
    router.registerActor('/workflows/system/storage', workflowsStorage);
    // Domain storage NOT registered

    // Workflow actor can query
    const workflowActor = new WorkflowActor('workflow-1', router);
    const workflowResponse = await workflowActor.ask(
      address('/workflows/system/storage'),
      'storage.query',
      { sql: 'SELECT * FROM tasks' }
    );
    expect(workflowResponse.success).toBe(true);

    // Domain actor cannot query
    const domainActor = new DomainActor('domain-1', router);
    const domainResponse = await domainActor.ask(
      address('/domain/system/storage'),
      'storage.query',
      { sql: 'SELECT * FROM tasks' }
    );
    expect(domainResponse.success).toBe(false);
    expect(domainResponse.error).toContain('No actor registered');
  });

  test('Different namespaces have isolated filesystem access', async () => {
    // Workflow filesystem
    const workflowsFs = new FileSystemActor(router, {
      allowedPaths: ['/data/workflows']
    });
    router.registerActor('/workflows/system/fs', workflowsFs);

    // Task filesystem (different path)
    const tasksFs = new FileSystemActor(router, {
      allowedPaths: ['/data/tasks']
    });
    router.registerActor('/tasks/system/fs', tasksFs);

    // Workflow can write to /data/workflows
    const workflowActor = new WorkflowActor('workflow-1', router);
    const workflowWrite = await workflowActor.ask(
      address('/workflows/system/fs'),
      'fs.write',
      { path: '/data/workflows/test.txt', content: 'workflow data' }
    );
    expect(workflowWrite.success).toBe(true);

    // Task cannot write to /data/workflows (different allowed path)
    const taskActor = new TaskActor('task-1', router);
    const taskWrite = await taskActor.ask(
      address('/tasks/system/fs'),
      'fs.write',
      { path: '/data/workflows/test.txt', content: 'task data' }
    );
    expect(taskWrite.success).toBe(false);
    expect(taskWrite.error).toContain('Access denied');
  });
});
```

---

## Migration from Direct Calls

### Before: Direct System Calls

```typescript
import Database from 'bun:sqlite';
import { readFile, writeFile } from 'fs/promises';

class TaskService {
  private db: Database;

  async createTask(title: string) {
    // Direct database call
    const result = this.db.query('INSERT INTO tasks (title) VALUES (?)').run(title);

    // Direct filesystem call
    await writeFile(`/data/tasks/${result.lastInsertRowid}.json`, JSON.stringify({ title }));

    return result.lastInsertRowid;
  }
}
```

**Problems:**
- ❌ Hard to test (real DB, real files)
- ❌ No access control (can access anything)
- ❌ Tight coupling (knows about DB structure, file paths)
- ❌ No observability (can't see what's happening)

### After: Namespace Routing

```typescript
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'create') {
      // Use storage via routing
      const dbResponse = await this.ask(
        address('/tasks/system/storage'),
        'storage.execute',
        {
          sql: 'INSERT INTO tasks (title) VALUES (?)',
          params: [message.payload.title]
        }
      );

      if (!dbResponse.success) {
        return createErrorResponse(message, dbResponse.error);
      }

      // Use filesystem via routing
      await this.ask(
        address('/tasks/system/fs'),
        'fs.write',
        {
          path: `/data/tasks/${dbResponse.payload.id}.json`,
          content: JSON.stringify(message.payload)
        }
      );

      return createResponse(message, { id: dbResponse.payload.id });
    }
  }
}
```

**Benefits:**
- ✅ Easy to test (mock routing)
- ✅ Access control (via registration)
- ✅ Loose coupling (message-based)
- ✅ Full observability (all operations are messages)

---

## Best Practices

### 1. Register at Application Startup

```typescript
// app/setup.ts
export function setupSystemActors(router: MessageRouter) {
  // Global singletons
  const scheduler = new SchedulerActor(router);
  router.registerActor('/system/scheduler', scheduler);

  const logger = new LoggerActor(router);
  router.registerActor('/system/logger', logger);

  // Workflow namespace
  setupWorkflowSystemActors(router);

  // Task namespace
  setupTaskSystemActors(router);

  // Domain namespace
  setupDomainSystemActors(router);
}
```

### 2. Use Type-Safe Addresses

```typescript
// Define namespace paths as constants
export const SYSTEM_ACTORS = {
  SCHEDULER: address('/system/scheduler'),
  LOGGER: address('/system/logger'),

  WORKFLOWS: {
    STORAGE: address('/workflows/system/storage'),
    FS: address('/workflows/system/fs'),
    HTTP: address('/workflows/system/http'),
  },

  TASKS: {
    STORAGE: address('/tasks/system/storage'),
    FS: address('/tasks/system/fs'),
    HTTP: address('/tasks/system/http'),
  },
};

// Usage
await actor.ask(SYSTEM_ACTORS.WORKFLOWS.STORAGE, 'storage.query', {...});
```

### 3. Document Namespace Capabilities

```typescript
/**
 * Workflow System Actors
 *
 * Capabilities:
 * - Storage: Read/write all tables
 * - Filesystem: Read/write /data/workflows
 * - HTTP: All methods, specific hosts
 * - WebSocket: Specific hosts
 */
export function setupWorkflowSystemActors(router: MessageRouter) {
  // ...
}
```

### 4. Test Namespace Isolation

Always include integration tests verifying that:
- Actors can access their namespace's system actors
- Actors cannot access other namespaces' system actors
- Configuration differences are enforced

---

## Summary

**Namespace routing provides:**

1. **Access Control** - Registration = permission
2. **Configuration Flexibility** - Different namespaces, different capabilities
3. **Security** - Principle of least privilege enforced
4. **Testability** - Easy to mock by not registering actors
5. **Observability** - All operations are messages
6. **Maintainability** - Clear separation of concerns

**Key Takeaway:** System actors use routing paths to provide different capabilities to different parts of the application, enforcing access control at the infrastructure level without requiring permission checks in business logic.
