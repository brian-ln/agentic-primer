# StorageActor Design (Pure Actor Model)

**Date:** 2026-02-06
**Status:** Design Phase
**Branch:** feature/path-addressing
**Bead:** simplify-0u7.1

---

## Overview

StorageActor is a **system actor** that provides database access with internal table-level scoping. Unlike the helper class approach, StorageActor IS the capability - access control happens through routing, not helper objects.

**Core Principle:** Capabilities are actors. Access control = routing decisions.

---

## Pure Actor Model Architecture

### What Changed

**Old Approach (Helper Classes):**
```typescript
// Helper class wraps router
class StorageCapability {
  private router: MessageRouter;
  private config: { allowedTables: string[] };

  async query(sql: string) {
    this.validateTables(sql); // Validation in helper
    return await this.router.ask(storageActor, 'storage.query', { sql });
  }
}

// Actors compose helpers
class TaskActor extends Actor {
  private storage: StorageCapability; // Helper instance
}
```

**New Approach (Pure Actor):**
```typescript
// StorageActor validates internally
class StorageActor extends Actor {
  private allowedTables: Set<string>;

  async receive(message: Message) {
    if (message.type === 'storage.query') {
      const tables = this.extractTables(message.payload.sql);
      if (!this.allowedTables.has(tables[0])) {
        return createErrorResponse(message, 'Access denied');
      }
      // Execute query
    }
  }
}

// Access control = routing
router.registerActor('/workflows/system/storage', workflowsStorage);
// NOT registered for /domain → access denied by absence
```

---

## Design Principles

### 1. System Actors ARE Capabilities

StorageActor is not a wrapper - it IS the storage capability. Configuration happens at construction.

```typescript
const workflowsStorage = new StorageActor('workflows-storage', router, {
  allowedTables: ['tasks', 'workflows', 'executions'],
  operations: ['read', 'write', 'delete']
});
```

### 2. Routing Determines Access

If an actor path is registered, access is granted. If not registered, access is implicitly denied.

```typescript
// Workflows can access their storage
router.registerActor('/workflows/system/storage', workflowsStorage);

// Domain has its own storage (different tables)
router.registerActor('/domain/system/storage', domainStorage);

// No storage for /untrusted → messages fail with "Actor not found"
```

### 3. Internal Validation

StorageActor validates every message against its configuration before execution.

```typescript
async receive(message: Message): Promise<MessageResponse> {
  if (message.type === 'storage.query') {
    const tables = this.extractTables(message.payload.sql);

    // Internal validation
    for (const table of tables) {
      if (!this.allowedTables.has(table)) {
        return createErrorResponse(message,
          `Access denied: table '${table}' not in allowedTables`
        );
      }
    }

    // Execute query
    const rows = await this.db.execute(message.payload.sql);
    return createSuccessResponse(message, { rows });
  }
}
```

### 4. Message-Based Protocol

All storage operations are messages. No direct method calls.

```typescript
// Query
await actor.ask(address('/workflows/system/storage'), 'storage.query', {
  sql: 'SELECT * FROM tasks WHERE status = ?',
  params: ['open']
});

// Execute (INSERT/UPDATE)
await actor.ask(address('/workflows/system/storage'), 'storage.execute', {
  sql: 'UPDATE tasks SET status = ? WHERE id = ?',
  params: ['done', 'task-123']
});

// Transaction
await actor.ask(address('/workflows/system/storage'), 'storage.transaction', {
  statements: [
    { sql: 'DELETE FROM tasks WHERE id = ?', params: ['task-123'] },
    { sql: 'INSERT INTO tasks_archive ...', params: [...] }
  ]
});
```

---

## StorageActor Interface

### Constructor

```typescript
export class StorageActor extends Actor {
  private db: Database;
  private allowedTables: Set<string>;
  private operations: Set<StorageOperation>;

  constructor(
    id: string,
    router: MessageRouter,
    config: StorageActorConfig
  ) {
    super(id, router);
    this.db = new Database(config.dbPath);
    this.allowedTables = new Set(config.allowedTables);
    this.operations = new Set(config.operations);
  }
}
```

### Configuration

```typescript
export interface StorageActorConfig {
  /** Database path */
  dbPath: string;

  /** Allowed tables (use '*' for all tables) */
  allowedTables: string[];

  /** Allowed operations */
  operations: StorageOperation[];
}

export type StorageOperation = 'read' | 'write' | 'delete' | 'admin';
```

### Message Protocol

**storage.query** (Read operation)
```typescript
{
  type: 'storage.query',
  payload: {
    sql: 'SELECT * FROM tasks WHERE status = ?',
    params: ['open']
  }
}

// Response
{
  success: true,
  payload: {
    rows: [{ id: 'task-1', status: 'open' }, ...]
  }
}
```

**storage.execute** (Write/Delete operation)
```typescript
{
  type: 'storage.execute',
  payload: {
    sql: 'UPDATE tasks SET status = ? WHERE id = ?',
    params: ['done', 'task-123']
  }
}

// Response
{
  success: true,
  payload: {
    changes: 1,
    lastInsertRowid: undefined
  }
}
```

**storage.transaction** (Multi-statement transaction)
```typescript
{
  type: 'storage.transaction',
  payload: {
    statements: [
      { sql: 'DELETE FROM tasks WHERE id = ?', params: ['task-123'] },
      { sql: 'INSERT INTO tasks_archive VALUES (?, ?)', params: ['task-123', '...'] }
    ]
  }
}

// Response
{
  success: true,
  payload: { committed: true }
}
```

---

## Implementation

### Core Logic

```typescript
export class StorageActor extends Actor {
  private db: Database;
  private allowedTables: Set<string>;
  private operations: Set<StorageOperation>;

  constructor(id: string, router: MessageRouter, config: StorageActorConfig) {
    super(id, router);
    this.db = new Database(config.dbPath);
    this.allowedTables = new Set(config.allowedTables);
    this.operations = new Set(config.operations);
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      if (type === 'storage.query') {
        return await this.handleQuery(message, payload);
      }

      if (type === 'storage.execute') {
        return await this.handleExecute(message, payload);
      }

      if (type === 'storage.transaction') {
        return await this.handleTransaction(message, payload);
      }

      return createErrorResponse(message, `Unknown message type: ${type}`);
    } catch (error: any) {
      this.logError('Storage operation failed', {
        type,
        error: error.message
      });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleQuery(
    message: Message,
    payload: { sql: string; params?: any[] }
  ): Promise<MessageResponse> {
    // Check operation permission
    if (!this.operations.has('read')) {
      return createErrorResponse(message, 'Read operation not permitted');
    }

    // Validate tables
    const tables = this.extractTables(payload.sql);
    const violation = this.validateTables(tables);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Execute query
    const stmt = this.db.prepare(payload.sql);
    const rows = stmt.all(payload.params || []);

    return createSuccessResponse(message, { rows });
  }

  private async handleExecute(
    message: Message,
    payload: { sql: string; params?: any[] }
  ): Promise<MessageResponse> {
    // Determine operation type
    const operation = this.inferOperation(payload.sql);

    // Check operation permission
    if (!this.operations.has(operation)) {
      return createErrorResponse(message,
        `Operation '${operation}' not permitted`
      );
    }

    // Validate tables
    const tables = this.extractTables(payload.sql);
    const violation = this.validateTables(tables);
    if (violation) {
      return createErrorResponse(message, violation);
    }

    // Execute statement
    const stmt = this.db.prepare(payload.sql);
    const info = stmt.run(payload.params || []);

    return createSuccessResponse(message, {
      changes: info.changes,
      lastInsertRowid: info.lastInsertRowid
    });
  }

  private async handleTransaction(
    message: Message,
    payload: { statements: Array<{ sql: string; params?: any[] }> }
  ): Promise<MessageResponse> {
    // Validate all statements first
    for (const stmt of payload.statements) {
      const operation = this.inferOperation(stmt.sql);
      if (!this.operations.has(operation)) {
        return createErrorResponse(message,
          `Transaction statement requires '${operation}' permission`
        );
      }

      const tables = this.extractTables(stmt.sql);
      const violation = this.validateTables(tables);
      if (violation) {
        return createErrorResponse(message, violation);
      }
    }

    // Execute transaction
    const transaction = this.db.transaction(() => {
      for (const stmt of payload.statements) {
        const prepared = this.db.prepare(stmt.sql);
        prepared.run(stmt.params || []);
      }
    });

    transaction();

    return createSuccessResponse(message, { committed: true });
  }

  /**
   * Validate tables against allowedTables
   * @returns Error message if validation fails, null if OK
   */
  private validateTables(tables: string[]): string | null {
    if (this.allowedTables.has('*')) {
      return null; // Wildcard = all tables allowed
    }

    for (const table of tables) {
      if (!this.allowedTables.has(table)) {
        return `Access denied: table '${table}' not in allowedTables: [${Array.from(this.allowedTables).join(', ')}]`;
      }
    }

    return null;
  }

  /**
   * Extract table names from SQL
   */
  private extractTables(sql: string): string[] {
    const tables = new Set<string>();

    // Match FROM clause
    const fromMatch = sql.match(/FROM\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (fromMatch) {
      fromMatch.forEach(match => {
        const table = match.replace(/FROM\s+/i, '').trim();
        tables.add(table);
      });
    }

    // Match INTO clause (INSERT)
    const intoMatch = sql.match(/INTO\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (intoMatch) {
      intoMatch.forEach(match => {
        const table = match.replace(/INTO\s+/i, '').trim();
        tables.add(table);
      });
    }

    // Match UPDATE clause
    const updateMatch = sql.match(/UPDATE\s+([a-zA-Z_][a-zA-Z0-9_]*)/gi);
    if (updateMatch) {
      updateMatch.forEach(match => {
        const table = match.replace(/UPDATE\s+/i, '').trim();
        tables.add(table);
      });
    }

    return Array.from(tables);
  }

  /**
   * Infer operation from SQL statement
   */
  private inferOperation(sql: string): StorageOperation {
    const trimmed = sql.trim().toUpperCase();

    if (trimmed.startsWith('SELECT')) return 'read';
    if (trimmed.startsWith('INSERT') || trimmed.startsWith('UPDATE')) return 'write';
    if (trimmed.startsWith('DELETE')) return 'delete';
    if (trimmed.startsWith('ALTER') || trimmed.startsWith('DROP') || trimmed.startsWith('CREATE')) {
      return 'admin';
    }

    throw new Error(`Cannot infer operation from SQL: ${sql.substring(0, 50)}...`);
  }
}
```

---

## Usage Examples

### Setup: Register StorageActors

```typescript
const router = new MessageRouter(store, programManager);

// Workflows namespace storage
const workflowsStorage = new StorageActor('workflows-storage', router, {
  dbPath: './data/workflows.db',
  allowedTables: ['tasks', 'workflows', 'executions'],
  operations: ['read', 'write', 'delete']
});
router.registerActor('/workflows/system/storage', workflowsStorage);

// Domain namespace storage
const domainStorage = new StorageActor('domain-storage', router, {
  dbPath: './data/domain.db',
  allowedTables: ['entities', 'relationships'],
  operations: ['read'] // Read-only
});
router.registerActor('/domain/system/storage', domainStorage);

// Session knowledge storage (admin access)
const knowledgeStorage = new StorageActor('knowledge-storage', router, {
  dbPath: './data/knowledge.db',
  allowedTables: ['*'], // All tables
  operations: ['read', 'write', 'delete', 'admin']
});
router.registerActor('/session-knowledge/system/storage', knowledgeStorage);
```

### Actors Use StorageActor via Messages

```typescript
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'get-tasks') {
      // Send message to StorageActor
      const response = await this.ask(
        address('/workflows/system/storage'),
        'storage.query',
        {
          sql: 'SELECT * FROM tasks WHERE status = ?',
          params: ['open']
        }
      );

      if (!response.success) {
        return createErrorResponse(message, response.error);
      }

      return createSuccessResponse(message, {
        tasks: response.payload.rows
      });
    }
  }
}
```

### Access Control Through Routing

```typescript
// ✅ Workflows actor CAN access workflows storage
const workflowActor = new TaskActor('workflow-task', router);
router.registerActor('/workflows/tasks/processor', workflowActor);

// Actor can send to /workflows/system/storage ✅
await workflowActor.ask(
  address('/workflows/system/storage'),
  'storage.query',
  { sql: 'SELECT * FROM tasks' }
);

// ❌ Workflows actor CANNOT access domain storage (different namespace)
await workflowActor.ask(
  address('/domain/system/storage'), // Different namespace
  'storage.query',
  { sql: 'SELECT * FROM entities' }
);
// Error: Actor not found at /domain/system/storage (from workflow namespace)

// ❌ Untrusted actor CANNOT access any storage (not registered)
const untrustedActor = new Actor('untrusted', router);
// NOT registered with router

await untrustedActor.ask(
  address('/workflows/system/storage'),
  'storage.query',
  { sql: 'SELECT * FROM tasks' }
);
// Error: Cannot send messages (actor not registered)
```

---

## Error Handling

### Access Denied Errors

```typescript
// Table not in allowedTables
{
  success: false,
  error: "Access denied: table 'users' not in allowedTables: [tasks, workflows, executions]"
}

// Operation not permitted
{
  success: false,
  error: "Operation 'write' not permitted"
}

// Actor not found (routing denial)
{
  success: false,
  error: "Actor not found: /workflows/system/storage"
}
```

### Clear Error Messages

All errors include:
- **What was denied:** Table name or operation type
- **Why it was denied:** Not in allowedTables or operations
- **What is allowed:** List of allowedTables or operations

---

## Comparison: Old vs New

### Old Design (Helper Classes)

```typescript
// ❌ Complexity: Two layers of validation
class StorageCapability {
  async query(sql: string) {
    this.validateTables(sql); // Layer 1: Helper validation
    await this.router.ask(storageActor, ...); // Layer 2: Actor receives pre-validated
  }
}

// ❌ Coupling: Actors depend on capability helpers
class TaskActor extends Actor {
  private storage: StorageCapability; // Dependency on helper class
}

// ❌ Configuration: Passed through constructor
const storage = new StorageCapability(router, config);
const actor = new TaskActor('task', router, { storage });
```

### New Design (Pure Actors)

```typescript
// ✅ Simplicity: Single layer of validation (in actor)
class StorageActor extends Actor {
  async receive(message: Message) {
    const tables = this.extractTables(message.payload.sql);
    this.validateTables(tables); // Single validation point
    // Execute
  }
}

// ✅ No coupling: Actors use standard messaging
class TaskActor extends Actor {
  async receive(message: Message) {
    const response = await this.ask(
      address('/workflows/system/storage'),
      'storage.query',
      { sql: '...' }
    ); // Standard actor messaging
  }
}

// ✅ Configuration: At actor construction
const storage = new StorageActor('storage', router, {
  allowedTables: ['tasks'],
  operations: ['read', 'write']
});

// ✅ Access control: Through routing
router.registerActor('/workflows/system/storage', storage);
```

---

## Benefits

### 1. Simpler Architecture
- No helper classes
- Single validation point (in actor)
- Standard actor messaging throughout

### 2. Clearer Access Control
- Access = routing registration
- No access = actor not registered
- Explicit and auditable

### 3. Better Testability
- Mock router, not helper classes
- Test actors in isolation
- No capability configuration needed in tests

### 4. More Flexible
- Easy to add new storage actors
- Per-namespace storage actors
- Different configs per namespace

---

## Next Steps

1. ✅ Design approved (this document)
2. Implement StorageActor (simplify-0u7.3)
3. Create tests for enforcement
4. Document migration from helper classes
5. Update existing code to use StorageActor

---

**Document End**
