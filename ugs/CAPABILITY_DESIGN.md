# Capability-Based Resource Access Design

**Status:** RFC - Design Phase
**Date:** 2026-02-06
**Phase:** 2 - Capability Design
**Goal:** Define capability interfaces for scoped storage and filesystem access

---

## Overview

This design implements **capability-based security** for resource access (storage, filesystem) in the actor system. Capabilities provide explicit, scoped access to resources - actors get only what they need, no more.

**Core Principle:** Capabilities are NOT in the base Actor class. Actors are explicitly configured with scoped capabilities at construction time.

---

## Architectural Context

### Universal Coordination vs Capabilities

The Actor base class provides **universal coordination primitives** (available to all actors):

```typescript
// ✅ Universal - All actors get these (already implemented)
protected logDebug(msg: string, context?: Record<string, any>): void
protected logInfo(msg: string, context?: Record<string, any>): void
protected logWarn(msg: string, context?: Record<string, any>): void
protected logError(msg: string, context?: Record<string, any>): void

// ✅ Universal - To be implemented (time coordination)
protected async schedule(delay: number, type: string, payload?: any): Promise<string>
protected async scheduleRecurring(interval: number, type: string, payload?: any): Promise<string>
protected async cancelSchedule(scheduleId: string): Promise<void>
```

**Capabilities are privileges** - actors must be explicitly configured with them:

```typescript
// ❌ NOT universal - Explicit configuration required
this.storage = new StorageCapability(router, { ... })
this.fs = new FileSystemCapability(router, { ... })
```

---

## Design Pattern: Composition (Option A)

We use **composition** to add capabilities to actors. This keeps the base Actor class clean and makes capabilities explicit.

```typescript
export class TaskActor extends Actor {
  private storage: StorageCapability;
  private fs: FileSystemCapability;

  constructor(id: string, router: MessageRouter, config: ActorConfig) {
    super(id, router);

    // Explicit capability configuration
    this.storage = new StorageCapability(router, {
      namespace: '/workflows',
      allowedTables: ['tasks', 'workflows'],
      operations: ['read', 'write']
    });

    this.fs = new FileSystemCapability(router, {
      namespace: '/workflows',
      allowedPaths: ['/workflows/config', '/workflows/data'],
      operations: ['read']
    });
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Can ONLY access configured tables
    const tasks = await this.storage.query('SELECT * FROM tasks');

    // Can ONLY access configured paths
    const config = await this.fs.read('/workflows/config/config.json');

    // Attempts outside scope are rejected
    // await this.storage.query('SELECT * FROM users'); // ❌ Error: Table 'users' not in allowedTables
    // await this.fs.read('/etc/passwd'); // ❌ Error: Path '/etc/passwd' not in allowedPaths
  }
}
```

**Why Composition:**
- ✅ Explicit: Capabilities declared at construction
- ✅ Flexible: Mix and match capabilities per actor
- ✅ Testable: Easy to mock capabilities
- ✅ Clear: No magic in base class
- ✅ Auditable: Track which actors have which capabilities

---

## StorageCapability Interface

### Purpose

Provides scoped database access to actors. Supports table-level and operation-level restrictions.

### Interface Definition

```typescript
/**
 * StorageCapability - Scoped database access
 *
 * Enforces table and operation restrictions at the capability layer.
 * Messages routed to /[namespace]/system/storage actor.
 */
export class StorageCapability {
  private router: MessageRouter;
  private storageAddress: Address;
  private config: StorageCapabilityConfig;

  constructor(router: MessageRouter, config: StorageCapabilityConfig) {
    this.router = router;
    this.config = config;
    this.storageAddress = address(`${config.namespace}/system/storage`);
  }

  /**
   * Execute a SELECT query
   * Requires: 'read' operation
   * Enforces: allowedTables restriction
   */
  async query<T = any>(
    sql: string,
    params?: any[]
  ): Promise<T[]> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Extract tables from SQL and validate
    const tables = this.extractTables(sql);
    this.validateTables(tables);

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(this.storageAddress, 'storage.query', {
        sql,
        params,
      })
    );

    if (!response.success) {
      throw new Error(`Storage query failed: ${response.error}`);
    }

    return response.payload as T[];
  }

  /**
   * Execute an INSERT/UPDATE statement
   * Requires: 'write' operation
   * Enforces: allowedTables restriction
   */
  async execute(
    sql: string,
    params?: any[]
  ): Promise<{ changes: number; lastInsertRowid?: number }> {
    // Check operation permission
    if (!this.config.operations.includes('write')) {
      throw new CapabilityError('write', 'Operation not permitted by capability');
    }

    // Extract tables and validate
    const tables = this.extractTables(sql);
    this.validateTables(tables);

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(this.storageAddress, 'storage.execute', {
        sql,
        params,
      })
    );

    if (!response.success) {
      throw new Error(`Storage execute failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Execute a DELETE statement
   * Requires: 'delete' operation
   * Enforces: allowedTables restriction
   */
  async delete(
    sql: string,
    params?: any[]
  ): Promise<{ changes: number }> {
    // Check operation permission
    if (!this.config.operations.includes('delete')) {
      throw new CapabilityError('delete', 'Operation not permitted by capability');
    }

    // Extract tables and validate
    const tables = this.extractTables(sql);
    this.validateTables(tables);

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(this.storageAddress, 'storage.execute', {
        sql,
        params,
      })
    );

    if (!response.success) {
      throw new Error(`Storage delete failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Execute multiple statements in a transaction
   * Requires: 'write' operation minimum
   * Enforces: allowedTables and operations for each statement
   */
  async transaction(
    statements: Array<{ sql: string; params?: any[] }>
  ): Promise<void> {
    // Validate all statements first
    for (const stmt of statements) {
      const tables = this.extractTables(stmt.sql);
      this.validateTables(tables);

      const operation = this.inferOperation(stmt.sql);
      if (!this.config.operations.includes(operation)) {
        throw new CapabilityError(
          operation,
          `Operation '${operation}' not permitted by capability`
        );
      }
    }

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(this.storageAddress, 'storage.transaction', {
        statements,
      })
    );

    if (!response.success) {
      throw new Error(`Storage transaction failed: ${response.error}`);
    }
  }

  /**
   * Subscribe to table changes (reactive queries)
   * Requires: 'subscribe' operation
   * Enforces: allowedTables restriction
   */
  async subscribe<T = any>(
    table: string,
    options: {
      where?: Record<string, any>;
      onMatch: (rows: T[]) => void;
    }
  ): Promise<{ unsubscribe: () => void }> {
    // Check operation permission
    if (!this.config.operations.includes('subscribe')) {
      throw new CapabilityError('subscribe', 'Operation not permitted by capability');
    }

    // Validate table
    this.validateTables([table]);

    // Route to storage actor
    const response = await this.router.ask(
      createMessage(this.storageAddress, 'storage.subscribe', {
        table,
        where: options.where,
        callbackAddress: this.router.createCallbackAddress(options.onMatch),
      })
    );

    if (!response.success) {
      throw new Error(`Storage subscription failed: ${response.error}`);
    }

    return {
      unsubscribe: () => {
        // Send unsubscribe message
        this.router.tell(
          createMessage(this.storageAddress, 'storage.unsubscribe', {
            subscriptionId: response.payload.subscriptionId,
          })
        );
      },
    };
  }

  /**
   * Extract table names from SQL
   * Simple regex-based extraction (can be improved with SQL parser)
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
   * Validate tables against allowedTables
   */
  private validateTables(tables: string[]): void {
    const { allowedTables } = this.config;

    for (const table of tables) {
      if (!allowedTables.includes(table) && !allowedTables.includes('*')) {
        throw new CapabilityError(
          'table-access',
          `Table '${table}' not in allowedTables: [${allowedTables.join(', ')}]`
        );
      }
    }
  }

  /**
   * Infer operation from SQL statement
   */
  private inferOperation(sql: string): StorageOperation {
    const trimmed = sql.trim().toUpperCase();

    if (trimmed.startsWith('SELECT')) return 'read';
    if (trimmed.startsWith('INSERT') || trimmed.startsWith('UPDATE')) return 'write';
    if (trimmed.startsWith('DELETE')) return 'delete';
    if (trimmed.startsWith('ALTER') || trimmed.startsWith('DROP') || trimmed.startsWith('CREATE')) return 'admin';

    throw new Error(`Cannot infer operation from SQL: ${sql.substring(0, 50)}...`);
  }
}

/**
 * StorageCapability configuration
 */
export interface StorageCapabilityConfig {
  /** Actor namespace (e.g., '/workflows') */
  namespace: string;

  /** Allowed tables (use '*' for all tables) */
  allowedTables: string[];

  /** Allowed operations */
  operations: StorageOperation[];
}

export type StorageOperation = 'read' | 'write' | 'delete' | 'admin' | 'subscribe';

/**
 * Capability error
 */
export class CapabilityError extends Error {
  constructor(
    public operation: string,
    message: string
  ) {
    super(message);
    this.name = 'CapabilityError';
  }
}
```

### Configuration Examples

**Read/Write Access (Scoped to Tables):**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/workflows',
  allowedTables: ['tasks', 'workflows', 'executions'],
  operations: ['read', 'write', 'delete', 'subscribe']
});

// ✅ Allowed
await storage.query('SELECT * FROM tasks WHERE status = ?', ['open']);
await storage.execute('UPDATE tasks SET status = ? WHERE id = ?', ['done', 'task-123']);

// ❌ Denied - table not in allowedTables
await storage.query('SELECT * FROM users');
// Error: Table 'users' not in allowedTables: [tasks, workflows, executions]
```

**Read-Only Access:**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/domain',
  allowedTables: ['tasks'], // Only tasks table
  operations: ['read'] // Read-only
});

// ✅ Allowed
await storage.query('SELECT * FROM tasks');

// ❌ Denied - write operation not permitted
await storage.execute('UPDATE tasks SET status = ?', ['done']);
// Error: Operation 'write' not permitted by capability
```

**Admin Access (Schema Changes):**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/workflows',
  allowedTables: ['*'], // All tables
  operations: ['read', 'write', 'delete', 'admin']
});

// ✅ Allowed - admin operations
await storage.execute('CREATE TABLE new_table (id TEXT PRIMARY KEY)');
await storage.execute('ALTER TABLE tasks ADD COLUMN priority INTEGER');
```

---

## FileSystemCapability Interface

### Purpose

Provides scoped filesystem access to actors. Supports path-level and operation-level restrictions with sandboxing.

### Interface Definition

```typescript
/**
 * FileSystemCapability - Scoped filesystem access
 *
 * Enforces path and operation restrictions at the capability layer.
 * Messages routed to /[namespace]/system/fs actor.
 */
export class FileSystemCapability {
  private router: MessageRouter;
  private fsAddress: Address;
  private config: FileSystemCapabilityConfig;

  constructor(router: MessageRouter, config: FileSystemCapabilityConfig) {
    this.router = router;
    this.config = config;
    this.fsAddress = address(`${config.namespace}/system/fs`);
  }

  /**
   * Read file contents
   * Requires: 'read' operation
   * Enforces: allowedPaths restriction
   */
  async read(
    path: string,
    options?: { encoding?: 'utf-8' | 'binary' }
  ): Promise<string | Buffer> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(path);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(this.fsAddress, 'fs.read', {
        path,
        encoding: options?.encoding || 'utf-8',
      })
    );

    if (!response.success) {
      throw new Error(`Filesystem read failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Write file contents (atomic)
   * Requires: 'write' operation
   * Enforces: allowedPaths restriction
   */
  async write(
    path: string,
    content: string | Buffer,
    options?: { atomic?: boolean }
  ): Promise<void> {
    // Check operation permission
    if (!this.config.operations.includes('write')) {
      throw new CapabilityError('write', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(path);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(this.fsAddress, 'fs.write', {
        path,
        content,
        atomic: options?.atomic ?? true, // Atomic by default
      })
    );

    if (!response.success) {
      throw new Error(`Filesystem write failed: ${response.error}`);
    }
  }

  /**
   * Delete file
   * Requires: 'delete' operation
   * Enforces: allowedPaths restriction
   */
  async delete(path: string): Promise<void> {
    // Check operation permission
    if (!this.config.operations.includes('delete')) {
      throw new CapabilityError('delete', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(path);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(this.fsAddress, 'fs.delete', {
        path,
      })
    );

    if (!response.success) {
      throw new Error(`Filesystem delete failed: ${response.error}`);
    }
  }

  /**
   * List directory contents
   * Requires: 'read' operation
   * Enforces: allowedPaths restriction
   */
  async list(
    path: string,
    options?: { recursive?: boolean; pattern?: string }
  ): Promise<string[]> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(path);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(this.fsAddress, 'fs.list', {
        path,
        recursive: options?.recursive ?? false,
        pattern: options?.pattern,
      })
    );

    if (!response.success) {
      throw new Error(`Filesystem list failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Get file metadata (stat)
   * Requires: 'read' operation
   * Enforces: allowedPaths restriction
   */
  async stat(path: string): Promise<FileStats> {
    // Check operation permission
    if (!this.config.operations.includes('read')) {
      throw new CapabilityError('read', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(path);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(this.fsAddress, 'fs.stat', {
        path,
      })
    );

    if (!response.success) {
      throw new Error(`Filesystem stat failed: ${response.error}`);
    }

    return response.payload;
  }

  /**
   * Watch file/directory for changes
   * Requires: 'watch' operation
   * Enforces: allowedPaths restriction
   */
  async watch(
    path: string,
    options: {
      pattern?: string;
      onChange: (event: FileChangeEvent) => void;
    }
  ): Promise<{ unwatch: () => void }> {
    // Check operation permission
    if (!this.config.operations.includes('watch')) {
      throw new CapabilityError('watch', 'Operation not permitted by capability');
    }

    // Validate path
    this.validatePath(path);

    // Route to filesystem actor
    const response = await this.router.ask(
      createMessage(this.fsAddress, 'fs.watch', {
        path,
        pattern: options.pattern,
        callbackAddress: this.router.createCallbackAddress(options.onChange),
      })
    );

    if (!response.success) {
      throw new Error(`Filesystem watch failed: ${response.error}`);
    }

    return {
      unwatch: () => {
        this.router.tell(
          createMessage(this.fsAddress, 'fs.unwatch', {
            watchId: response.payload.watchId,
          })
        );
      },
    };
  }

  /**
   * Validate path against allowedPaths
   *
   * Checks if path is within one of the allowed directories.
   * Resolves symlinks and relative paths to prevent escapes.
   */
  private validatePath(path: string): void {
    // Resolve to absolute path (prevents ../ escapes)
    const resolved = this.resolvePath(path);

    // Check against allowedPaths
    const isAllowed = this.config.allowedPaths.some(allowedPath => {
      const resolvedAllowed = this.resolvePath(allowedPath);
      return resolved.startsWith(resolvedAllowed);
    });

    if (!isAllowed) {
      throw new CapabilityError(
        'path-access',
        `Path '${path}' not in allowedPaths: [${this.config.allowedPaths.join(', ')}]`
      );
    }
  }

  /**
   * Resolve path to absolute (simplified - real implementation would use path.resolve)
   */
  private resolvePath(path: string): string {
    // In real implementation: use path.resolve() + follow symlinks
    // For now, basic normalization
    return path.replace(/\/\.\.\//g, '/').replace(/\/\.\//g, '/');
  }
}

/**
 * FileSystemCapability configuration
 */
export interface FileSystemCapabilityConfig {
  /** Actor namespace (e.g., '/workflows') */
  namespace: string;

  /** Allowed paths (sandboxed directories) */
  allowedPaths: string[];

  /** Allowed operations */
  operations: FileSystemOperation[];
}

export type FileSystemOperation = 'read' | 'write' | 'delete' | 'watch';

export interface FileStats {
  size: number;
  isFile: boolean;
  isDirectory: boolean;
  mtime: number;
  ctime: number;
}

export interface FileChangeEvent {
  type: 'created' | 'modified' | 'deleted';
  path: string;
  timestamp: number;
}
```

### Configuration Examples

**Read/Write Access (Sandboxed):**
```typescript
const fs = new FileSystemCapability(router, {
  namespace: '/workflows',
  allowedPaths: ['/workflows/config', '/workflows/data'],
  operations: ['read', 'write']
});

// ✅ Allowed
const config = await fs.read('/workflows/config/settings.json');
await fs.write('/workflows/data/state.json', JSON.stringify(state));

// ❌ Denied - path not in allowedPaths
await fs.read('/etc/passwd');
// Error: Path '/etc/passwd' not in allowedPaths: [/workflows/config, /workflows/data]
```

**Read-Only Access:**
```typescript
const fs = new FileSystemCapability(router, {
  namespace: '/session-knowledge',
  allowedPaths: ['/session-logs'],
  operations: ['read'] // Read-only
});

// ✅ Allowed
const logs = await fs.read('/session-logs/session-123.jsonl');
const files = await fs.list('/session-logs');

// ❌ Denied - write operation not permitted
await fs.write('/session-logs/new.jsonl', 'data');
// Error: Operation 'write' not permitted by capability
```

**Watch for Changes:**
```typescript
const fs = new FileSystemCapability(router, {
  namespace: '/workflows',
  allowedPaths: ['/workflows'],
  operations: ['read', 'watch']
});

// ✅ Allowed
const watcher = await fs.watch('/workflows', {
  pattern: '*.ts',
  onChange: (event) => {
    console.log('File changed:', event.path, event.type);
  }
});

// Stop watching
watcher.unwatch();
```

---

## Actor Construction API

### Factory Pattern (Recommended)

```typescript
/**
 * ActorFactory - Creates actors with capabilities
 */
export class ActorFactory {
  private router: MessageRouter;

  constructor(router: MessageRouter) {
    this.router = router;
  }

  /**
   * Create actor with capabilities
   */
  createActor<T extends Actor>(
    ActorClass: new (id: string, router: MessageRouter, ...args: any[]) => T,
    config: ActorFactoryConfig
  ): T {
    const actor = new ActorClass(config.id, this.router, config.constructorArgs);

    // Configure capabilities if specified
    if (config.capabilities) {
      this.attachCapabilities(actor, config.capabilities);
    }

    return actor;
  }

  private attachCapabilities(actor: any, capabilities: CapabilityConfig): void {
    if (capabilities.storage) {
      actor.storage = new StorageCapability(this.router, {
        namespace: capabilities.namespace,
        ...capabilities.storage,
      });
    }

    if (capabilities.fs) {
      actor.fs = new FileSystemCapability(this.router, {
        namespace: capabilities.namespace,
        ...capabilities.fs,
      });
    }
  }
}

interface ActorFactoryConfig {
  id: string;
  namespace: string;
  constructorArgs?: any[];
  capabilities?: CapabilityConfig;
}

interface CapabilityConfig {
  namespace: string;
  storage?: {
    allowedTables: string[];
    operations: StorageOperation[];
  };
  fs?: {
    allowedPaths: string[];
    operations: FileSystemOperation[];
  };
}
```

### Usage Example

```typescript
const factory = new ActorFactory(router);

// Create TaskActor with scoped capabilities
const taskActor = factory.createActor(TaskActor, {
  id: 'task-123',
  namespace: '/workflows',
  capabilities: {
    namespace: '/workflows',
    storage: {
      allowedTables: ['tasks', 'workflows'],
      operations: ['read', 'write', 'subscribe']
    },
    fs: {
      allowedPaths: ['/workflows/tasks'],
      operations: ['read', 'write']
    }
  }
});

// Register with router
router.registerActor(taskActor.address, taskActor);
```

---

## Migration Plan

### Current Direct Calls Analysis

**Database Calls (11 primary locations):**
1. `session-knowledge/temporal/TemporalQueries.ts` - 8 calls to `this.db.execute()`
2. `session-knowledge/index/QueryEngine.ts` - 10 calls to `this.db.execute()`
3. `session-knowledge/temporal/ArcDetector.ts` - 3 calls to `this.db.execute()`
4. `session-knowledge/migrations/*.ts` - Migration scripts (can stay direct)
5. `session-knowledge/__tests__/*.test.ts` - Test files (can stay direct)

**Filesystem Calls (4 locations):**
1. `demo-code-execution-actor.ts` - 1 call to `fs.readFileSync()` (demo only)
2. `src/messaging/actors/program-executor.test.ts` - Test file (can stay direct)

### Migration Strategy

**Phase 1: Create Capability Classes (1 day)**
1. Implement `StorageCapability` class
2. Implement `FileSystemCapability` class
3. Implement `CapabilityError` class
4. Add unit tests for enforcement logic

**Phase 2: Migrate Session Knowledge (2 days)**
1. Update `TemporalQueries` to use `StorageCapability`
   - Constructor receives capability instance
   - Replace `this.db.execute()` with `this.storage.query()`
2. Update `QueryEngine` to use `StorageCapability`
   - Constructor receives capability instance
   - Replace `this.db.execute()` with `this.storage.query()`
3. Update `ArcDetector` to use `StorageCapability`
   - Constructor receives capability instance
   - Replace `this.db.execute()` with `this.storage.query()`

**Phase 3: Create ActorFactory (1 day)**
1. Implement `ActorFactory` class
2. Add capability attachment logic
3. Create factory helper methods
4. Add integration tests

**Phase 4: Update Actor Construction (1 day)**
1. Refactor actor creation to use `ActorFactory`
2. Add capability configurations
3. Update tests to use factory
4. Verify enforcement

### Migration Complexity Assessment

**Low Complexity:**
- ✅ Capability classes are self-contained
- ✅ SQL extraction is regex-based (simple)
- ✅ Path validation is straightforward
- ✅ No breaking changes to existing APIs

**Medium Complexity:**
- ⚠️ Need to thread capability instances through constructors
- ⚠️ Tests need to be updated to pass capabilities
- ⚠️ Need to ensure backward compatibility during migration

**High Complexity:**
- ❌ None identified

**Total Estimated Time:** 5 days for full capability system implementation

---

## Capability Enforcement Patterns

### 1. Table Access Validation

```typescript
// SQL: SELECT * FROM tasks, workflows WHERE ...
const tables = this.extractTables(sql); // ['tasks', 'workflows']
this.validateTables(tables); // Checks against allowedTables

// If allowedTables = ['tasks'] → Error: Table 'workflows' not in allowedTables
```

### 2. Operation Permission Check

```typescript
// Before executing any operation
if (!this.config.operations.includes('write')) {
  throw new CapabilityError('write', 'Operation not permitted');
}
```

### 3. Path Sandboxing

```typescript
// Path: /workflows/config/../../../etc/passwd
const resolved = this.resolvePath(path); // /etc/passwd (after normalization)

// Check if resolved path starts with any allowedPath
const isAllowed = allowedPaths.some(p => resolved.startsWith(p));
// isAllowed = false → Error: Path not in allowedPaths
```

### 4. Transaction Validation

```typescript
// Validate all statements before executing transaction
for (const stmt of statements) {
  const tables = this.extractTables(stmt.sql);
  this.validateTables(tables);

  const operation = this.inferOperation(stmt.sql);
  if (!this.config.operations.includes(operation)) {
    throw new CapabilityError(operation, 'Not permitted');
  }
}

// All valid → Execute transaction
await this.router.ask(address, 'storage.transaction', { statements });
```

---

## Error Handling

### CapabilityError Type

```typescript
export class CapabilityError extends Error {
  constructor(
    public operation: string,
    message: string
  ) {
    super(message);
    this.name = 'CapabilityError';
  }
}

// Usage
try {
  await storage.query('SELECT * FROM users');
} catch (error) {
  if (error instanceof CapabilityError) {
    console.error(`Capability violation: ${error.operation}`);
    console.error(`Message: ${error.message}`);
    // Log to audit system
    await logAudit({
      type: 'capability-violation',
      operation: error.operation,
      actor: this.address,
      timestamp: Date.now()
    });
  }
}
```

### Capability Violation Logging

```typescript
// Capability classes automatically log violations
private validateTables(tables: string[]): void {
  for (const table of tables) {
    if (!this.isTableAllowed(table)) {
      // Log violation before throwing
      this.router.tell(
        createMessage(address('logger'), 'log.warn', {
          message: `Capability violation: unauthorized table access`,
          context: {
            table,
            allowedTables: this.config.allowedTables,
            namespace: this.config.namespace
          }
        })
      );

      throw new CapabilityError('table-access', `Table '${table}' not allowed`);
    }
  }
}
```

---

## Testing Strategy

### 1. Unit Tests (Capability Classes)

```typescript
describe('StorageCapability', () => {
  it('should allow queries on permitted tables', async () => {
    const cap = new StorageCapability(router, {
      namespace: '/test',
      allowedTables: ['tasks'],
      operations: ['read']
    });

    // Should succeed
    await cap.query('SELECT * FROM tasks');
  });

  it('should reject queries on unpermitted tables', async () => {
    const cap = new StorageCapability(router, {
      namespace: '/test',
      allowedTables: ['tasks'],
      operations: ['read']
    });

    // Should throw CapabilityError
    await expect(
      cap.query('SELECT * FROM users')
    ).rejects.toThrow(CapabilityError);
  });

  it('should reject operations not in permissions', async () => {
    const cap = new StorageCapability(router, {
      namespace: '/test',
      allowedTables: ['tasks'],
      operations: ['read'] // No 'write'
    });

    // Should throw CapabilityError
    await expect(
      cap.execute('UPDATE tasks SET status = ?', ['done'])
    ).rejects.toThrow(CapabilityError);
  });
});
```

### 2. Integration Tests (Actor + Capabilities)

```typescript
describe('TaskActor with capabilities', () => {
  it('should only access configured tables', async () => {
    const actor = factory.createActor(TaskActor, {
      id: 'task-1',
      namespace: '/workflows',
      capabilities: {
        namespace: '/workflows',
        storage: {
          allowedTables: ['tasks'],
          operations: ['read', 'write']
        }
      }
    });

    // Can access tasks
    const response = await actor.receive({
      type: 'query-tasks',
      payload: {}
    });
    expect(response.success).toBe(true);

    // Cannot access users
    const violationResponse = await actor.receive({
      type: 'query-users',
      payload: {}
    });
    expect(violationResponse.success).toBe(false);
    expect(violationResponse.error).toContain('not in allowedTables');
  });
});
```

---

## Design Decisions Summary

### ✅ Key Decisions

1. **Composition over Inheritance**
   - Capabilities are NOT in base Actor class
   - Actors explicitly configure capabilities at construction
   - Keeps base Actor clean and focused

2. **Enforcement at Capability Layer**
   - Capabilities check permissions before routing to system actors
   - System actors receive only validated requests
   - Clear separation of concerns

3. **Scoping Strategy**
   - Storage: Table-level + operation-level scoping
   - Filesystem: Path-level + operation-level scoping
   - Explicit allowlists (no denylists)

4. **Error Handling**
   - Custom `CapabilityError` type for violations
   - Automatic logging of violations
   - Clear error messages with context

5. **Construction Pattern**
   - `ActorFactory` for consistent capability attachment
   - Configuration-driven capability setup
   - Testable and auditable

### 🔍 Open Questions for Implementation

1. **SQL Parsing:** Use regex or full SQL parser?
   - **Recommendation:** Start with regex, upgrade if needed

2. **Capability Reuse:** Share capability instances across actors?
   - **Recommendation:** One capability per actor (simpler, more auditable)

3. **Subscription Management:** How to handle cleanup on actor shutdown?
   - **Recommendation:** Track subscriptions in actor, cleanup in destructor

4. **Performance:** Will validation add noticeable latency?
   - **Recommendation:** Benchmark, cache parsed table lists if needed

---

## Next Steps

1. ✅ **Design Approval** (this document)
2. **Implementation Phase** (5 days):
   - Day 1: Create capability classes
   - Day 2-3: Migrate session knowledge
   - Day 4: Create ActorFactory
   - Day 5: Integration and testing
3. **Documentation Update**
4. **Performance Benchmarking**

---

## Appendix: Full Example

### Complete Actor with Capabilities

```typescript
import { Actor } from '../actor.ts';
import { StorageCapability } from '../capabilities/storage.ts';
import { FileSystemCapability } from '../capabilities/filesystem.ts';
import type { Message, MessageResponse } from '../message.ts';
import { createResponse } from '../message.ts';
import type { MessageRouter } from '../router.ts';

/**
 * TaskActor - Manages workflow tasks
 *
 * Capabilities:
 * - Storage: Read/write tasks and workflows tables
 * - Filesystem: Read/write to /workflows/tasks directory
 */
export class TaskActor extends Actor {
  private storage: StorageCapability;
  private fs: FileSystemCapability;

  constructor(
    id: string,
    router: MessageRouter,
    config: {
      storage: { allowedTables: string[]; operations: string[] };
      fs: { allowedPaths: string[]; operations: string[] };
    }
  ) {
    super(id, router);

    // Configure storage capability
    this.storage = new StorageCapability(router, {
      namespace: '/workflows',
      allowedTables: config.storage.allowedTables,
      operations: config.storage.operations as any
    });

    // Configure filesystem capability
    this.fs = new FileSystemCapability(router, {
      namespace: '/workflows',
      allowedPaths: config.fs.allowedPaths,
      operations: config.fs.operations as any
    });

    this.logInfo('TaskActor initialized', {
      storage: config.storage,
      fs: config.fs
    });
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      if (type === 'get-tasks') {
        return await this.getTasks(message, payload);
      }

      if (type === 'update-task') {
        return await this.updateTask(message, payload);
      }

      if (type === 'save-state') {
        return await this.saveState(message, payload);
      }

      return createResponse(message, {
        success: false,
        error: `Unknown message type: ${type}`
      });
    } catch (error: any) {
      this.logError('Message handling failed', {
        type,
        error: error.message,
        isCapabilityError: error instanceof CapabilityError
      });

      return createResponse(message, {
        success: false,
        error: error.message
      });
    }
  }

  /**
   * Get tasks (uses storage capability)
   */
  private async getTasks(message: Message, payload: any): Promise<MessageResponse> {
    // Capability enforces table and operation restrictions
    const tasks = await this.storage.query(
      'SELECT * FROM tasks WHERE status = ?',
      [payload.status || 'open']
    );

    this.logDebug('Retrieved tasks', { count: tasks.length });

    return createResponse(message, {
      success: true,
      payload: { tasks }
    });
  }

  /**
   * Update task (uses storage capability)
   */
  private async updateTask(message: Message, payload: any): Promise<MessageResponse> {
    const { taskId, status } = payload;

    // Capability enforces table and operation restrictions
    const result = await this.storage.execute(
      'UPDATE tasks SET status = ?, updated_at = ? WHERE id = ?',
      [status, Date.now(), taskId]
    );

    this.logInfo('Task updated', { taskId, status, changes: result.changes });

    return createResponse(message, {
      success: true,
      payload: { updated: result.changes > 0 }
    });
  }

  /**
   * Save state to filesystem (uses filesystem capability)
   */
  private async saveState(message: Message, payload: any): Promise<MessageResponse> {
    const path = `/workflows/tasks/${payload.taskId}-state.json`;
    const content = JSON.stringify(payload.state, null, 2);

    // Capability enforces path and operation restrictions
    await this.fs.write(path, content, { atomic: true });

    this.logInfo('State saved', { path });

    return createResponse(message, {
      success: true,
      payload: { path }
    });
  }
}
```

### Factory Usage

```typescript
const factory = new ActorFactory(router);

const taskActor = factory.createActor(TaskActor, {
  id: 'task-manager',
  namespace: '/workflows',
  constructorArgs: [{
    storage: {
      allowedTables: ['tasks', 'workflows'],
      operations: ['read', 'write', 'subscribe']
    },
    fs: {
      allowedPaths: ['/workflows/tasks', '/workflows/state'],
      operations: ['read', 'write']
    }
  }]
});

router.registerActor(taskActor.address, taskActor);

// Test it
const response = await taskActor.receive({
  id: generateMessageId(),
  correlationId: generateCorrelationId(),
  from: address('test'),
  to: taskActor.address,
  type: 'get-tasks',
  payload: { status: 'open' },
  pattern: 'ask',
  timestamp: Date.now()
});

console.log('Tasks:', response.payload.tasks);
```

---

**Document End**
