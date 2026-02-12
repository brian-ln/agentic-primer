# System Actors Architecture Design

**Status:** RFC - Design Phase
**Date:** 2026-02-06
**Goal:** Replace all direct system library calls with message-based actors

---

## Overview

Replace direct system calls (database, filesystem, process, timers) with actor-based abstractions. Each actor system namespace gets its own set of system actors with configurable capabilities.

**Core Principles:**
1. **No direct system calls** - All I/O through actors
2. **Namespace isolation** - Each system has its own system actors
3. **Capability-based security** - Actors can refuse operations
4. **Pure functions allowed** - crypto, path, url utilities are fine (no I/O)
5. **Testability first** - Mock actors for tests, real for production

---

## Architecture

### 1. System Actor Hierarchy

```
/system/                       # Global shared system actors
  /scheduler                   # Shared time/scheduling (singleton)
  /logger                      # Shared logging (singleton)
  /metrics                     # Shared telemetry (singleton)

/workflows/system/             # Workflow namespace system actors
  /storage                     # Workflow-specific database
  /fs                          # Filesystem (read/write to workflows dir)
  /process                     # Process spawning (restricted)
  /http                        # HTTP client (allowed)

/tasks/system/                 # Task namespace system actors
  /storage -> /workflows/system/storage  # Proxy to shared storage
  /fs                          # Filesystem (read/write to tasks dir)
  /process                     # Process spawning (allowed)
  /http                        # HTTP client (rate limited)

/domain/system/                # Domain logic namespace (most restricted)
  /storage                     # Read-only storage
  /fs                          # Denied (no filesystem access)
  /process                     # Denied (no process spawning)
  /http                        # Denied (no network access)

/session-knowledge/system/     # Session knowledge namespace
  /storage                     # Dedicated database (session.db)
  /fs                          # Read-only access to session logs
  /process                     # Denied
  /http                        # Allowed (for embedding services)
```

### 2. Sharing Strategies

**Strategy 1: Singleton (Shared)**
```typescript
// One instance shared by all namespaces
const scheduler = new SchedulerActor(router);
router.registerActor('/system/scheduler', scheduler);

// All namespaces use the same scheduler
await actor.ask(address('/system/scheduler'), 'schedule', {...});
```

**Strategy 2: Proxy (Delegation)**
```typescript
// Tasks proxy to workflows storage
const tasksStorage = new ProxyActor(router, '/workflows/system/storage');
router.registerActor('/tasks/system/storage', tasksStorage);

// Tasks storage requests forwarded to workflows storage
await actor.ask(address('/tasks/system/storage'), 'query', {...});
// → Routed to /workflows/system/storage
```

**Strategy 3: Isolated (Dedicated Instance)**
```typescript
// Each namespace gets its own isolated instance
const workflowsStorage = new StorageActor(router, { dbPath: 'workflows.db' });
const sessionStorage = new StorageActor(router, { dbPath: 'session.db' });

router.registerActor('/workflows/system/storage', workflowsStorage);
router.registerActor('/session-knowledge/system/storage', sessionStorage);
```

---

## System Actor Interfaces

### StorageActor

**Purpose:** Database operations (queries, transactions, subscriptions)

**Messages:**
- `storage.query` - Execute SELECT query
- `storage.execute` - Execute INSERT/UPDATE/DELETE
- `storage.transaction` - Atomic multi-statement transaction
- `storage.subscribe` - Subscribe to table changes (reactive queries)

**Capabilities:**
```typescript
interface StorageCapabilities {
  read: boolean;      // SELECT queries
  write: boolean;     // INSERT/UPDATE
  delete: boolean;    // DELETE
  admin: boolean;     // Schema changes (ALTER, DROP)
  subscribe: boolean; // Reactive subscriptions
}
```

**Example:**
```typescript
// Read-only query
const result = await actor.ask(address('/workflows/system/storage'), 'storage.query', {
  sql: 'SELECT * FROM tasks WHERE status = ?',
  params: ['open'],
});

// Write (requires write capability)
await actor.ask(address('/workflows/system/storage'), 'storage.execute', {
  sql: 'UPDATE tasks SET status = ? WHERE id = ?',
  params: ['running', 'task-123'],
});

// Reactive subscription
const sub = await actor.ask(address('/workflows/system/storage'), 'storage.subscribe', {
  table: 'tasks',
  where: { status: 'open' },
  onMatch: (rows) => console.log('Open tasks:', rows),
});
```

---

### SchedulerActor

**Purpose:** Time-based operations (delays, intervals, cron)

**Messages:**
- `scheduler.schedule` - Schedule one-time delayed message
- `scheduler.recurring` - Schedule recurring message (interval/cron)
- `scheduler.cancel` - Cancel scheduled message
- `scheduler.list` - List scheduled messages

**Capabilities:**
```typescript
interface SchedulerCapabilities {
  schedule: boolean;  // Schedule messages
  cancel: boolean;    // Cancel schedules
  list: boolean;      // List schedules
}
```

**Example:**
```typescript
// Low-level API (direct message to scheduler)
const scheduleId = await actor.ask(address('/system/scheduler'), 'scheduler.schedule', {
  delay: 5000, // 5 seconds
  message: {
    to: address('/workflows/task-123'),
    type: 'timeout',
    payload: { reason: 'max_duration_exceeded' },
  },
});

// Recurring message (every 10 minutes)
await actor.ask(address('/system/scheduler'), 'scheduler.recurring', {
  interval: 600000, // 10 minutes
  message: {
    to: address('/workflows/health-check'),
    type: 'ping',
  },
});

// Cancel schedule
await actor.ask(address('/system/scheduler'), 'scheduler.cancel', {
  scheduleId,
});
```

**Actor Base Class Helpers:**
```typescript
// Universal coordination primitives - ALL actors get these
export class Actor {
  // Schedule message to self (delayed)
  protected async schedule(delay: number, type: string, payload?: any): Promise<string> {
    return await this.ask(address('/system/scheduler'), 'scheduler.schedule', {
      delay,
      message: { to: this.address, type, payload },
    });
  }

  // Schedule recurring message to self
  protected async scheduleRecurring(interval: number, type: string, payload?: any): Promise<string> {
    return await this.ask(address('/system/scheduler'), 'scheduler.recurring', {
      interval,
      message: { to: this.address, type, payload },
    });
  }

  // Cancel scheduled message
  protected async cancelSchedule(scheduleId: string): Promise<void> {
    await this.ask(address('/system/scheduler'), 'scheduler.cancel', { scheduleId });
  }

  // Logging already implemented
  protected logDebug(msg: string, context?: Record<string, any>): void
  protected logInfo(msg: string, context?: Record<string, any>): void
  protected logWarn(msg: string, context?: Record<string, any>): void
  protected logError(msg: string, context?: Record<string, any>): void
}

// Usage - clean and safe
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      // Universal coordination - all actors can do this
      await this.schedule(5000, 'timeout');
      this.logInfo('Task started, timeout scheduled');

      return createResponse(message, { status: 'started' });
    }

    if (message.type === 'timeout') {
      this.logWarn('Task timed out');
      return createResponse(message, { status: 'timed_out' });
    }
  }
}
```

**Testability:**
```typescript
// Production: Real time
const scheduler = new SchedulerActor(router, { clock: 'real' });

// Tests: Virtual time (instant)
const scheduler = new SchedulerActor(router, { clock: 'virtual' });
await scheduler.advance(5000); // Fast-forward 5 seconds
```

---

### FileSystemActor

**Purpose:** File I/O operations (read, write, watch)

**Messages:**
- `fs.read` - Read file contents
- `fs.write` - Write file contents (atomic)
- `fs.delete` - Delete file
- `fs.list` - List directory contents
- `fs.watch` - Subscribe to file changes
- `fs.stat` - Get file metadata

**Capabilities:**
```typescript
interface FileSystemCapabilities {
  read: boolean;         // Read files
  write: boolean;        // Write/create files
  delete: boolean;       // Delete files
  watch: boolean;        // Watch for changes
  allowedPaths: string[]; // Sandboxed paths (e.g., ['/workflows', '/tmp'])
}
```

**Example:**
```typescript
// Read file (requires read capability + path in allowedPaths)
const content = await actor.ask(address('/workflows/system/fs'), 'fs.read', {
  path: '/workflows/config.json',
});

// Write file (requires write capability)
await actor.ask(address('/workflows/system/fs'), 'fs.write', {
  path: '/workflows/state.json',
  content: JSON.stringify(state),
  atomic: true, // Write to temp, then rename
});

// Watch directory for changes
await actor.ask(address('/workflows/system/fs'), 'fs.watch', {
  path: '/workflows',
  pattern: '*.ts',
  onMatch: (event) => console.log('File changed:', event),
});
```

**Sandboxing:**
```typescript
// Domain actors cannot access filesystem
await domainActor.ask(address('/domain/system/fs'), 'fs.read', { path: '/etc/passwd' });
// → Error: FileSystemActor not registered (denied)

// Workflow actors sandboxed to /workflows directory
await workflowActor.ask(address('/workflows/system/fs'), 'fs.read', { path: '/etc/passwd' });
// → Error: Path '/etc/passwd' not in allowedPaths: ['/workflows', '/tmp']
```

---

### ProcessActor

**Purpose:** Spawn/manage child processes (shell commands, programs)

**Messages:**
- `process.spawn` - Spawn process (async)
- `process.exec` - Execute command (wait for completion)
- `process.kill` - Send signal to process
- `process.list` - List running processes

**Capabilities:**
```typescript
interface ProcessCapabilities {
  spawn: boolean;         // Spawn processes
  kill: boolean;          // Send signals
  allowedCommands: string[]; // Whitelist (e.g., ['echo', 'date', 'curl'])
  maxConcurrent: number;  // Max concurrent processes
}
```

**Example:**
```typescript
// Spawn process (requires spawn capability)
const result = await actor.ask(address('/workflows/system/process'), 'process.spawn', {
  command: 'bun',
  args: ['test', '--filter', 'integration'],
  cwd: '/workflows',
  env: { NODE_ENV: 'test' },
  timeout: 60000,
});

// Domain actors denied process spawning
await domainActor.ask(address('/domain/system/process'), 'process.spawn', {...});
// → Error: ProcessActor not registered (denied)
```

---

### HTTPClientActor

**Purpose:** HTTP requests (external APIs, services)

**Messages:**
- `http.get` - GET request
- `http.post` - POST request
- `http.put` - PUT request
- `http.delete` - DELETE request

**Capabilities:**
```typescript
interface HTTPCapabilities {
  methods: ('GET' | 'POST' | 'PUT' | 'DELETE')[];
  allowedHosts: string[]; // Whitelist (e.g., ['api.anthropic.com', 'api.github.com'])
  rateLimit: { requests: number; window: number }; // Rate limiting
  timeout: number;        // Request timeout
}
```

**Example:**
```typescript
// HTTP request (requires method in capabilities + host in allowedHosts)
const response = await actor.ask(address('/workflows/system/http'), 'http.post', {
  url: 'https://api.anthropic.com/v1/messages',
  headers: { 'x-api-key': apiKey },
  body: { model: 'claude-4.5-sonnet', messages: [...] },
  timeout: 30000,
});

// Domain actors denied HTTP access
await domainActor.ask(address('/domain/system/http'), 'http.get', {...});
// → Error: HTTPClientActor not registered (denied)
```

---

## Capability Configuration

### SystemActorRegistry

```typescript
/**
 * Registry of system actors with capability enforcement
 */
export class SystemActorRegistry {
  private router: MessageRouter;
  private capabilities: Map<string, SystemCapabilities>;

  /**
   * Register system actors for a namespace
   */
  async registerSystemActors(
    namespace: string,
    config: SystemActorConfig
  ): Promise<void> {
    const basePath = `${namespace}/system`;

    // Storage
    if (config.storage) {
      const actor = config.storage.isolated
        ? new StorageActor(this.router, config.storage.options)
        : new ProxyActor(this.router, config.storage.proxyTo);

      this.router.registerActor(`${basePath}/storage`, actor);
      this.capabilities.set(`${basePath}/storage`, config.storage.capabilities);
    }

    // Scheduler (usually shared singleton)
    if (config.scheduler) {
      // Point to shared scheduler
      const proxy = new ProxyActor(this.router, '/system/scheduler');
      this.router.registerActor(`${basePath}/scheduler`, proxy);
    }

    // FileSystem
    if (config.fs) {
      const actor = new FileSystemActor(this.router, config.fs.options);
      this.router.registerActor(`${basePath}/fs`, actor);
      this.capabilities.set(`${basePath}/fs`, config.fs.capabilities);
    }

    // Process
    if (config.process) {
      const actor = new ProcessActor(this.router, config.process.options);
      this.router.registerActor(`${basePath}/process`, actor);
      this.capabilities.set(`${basePath}/process`, config.process.capabilities);
    }

    // HTTP Client
    if (config.http) {
      const actor = new HTTPClientActor(this.router, config.http.options);
      this.router.registerActor(`${basePath}/http`, actor);
      this.capabilities.set(`${basePath}/http`, config.http.capabilities);
    }
  }

  /**
   * Check if operation is allowed by capabilities
   */
  checkCapability(actorPath: string, operation: string): boolean {
    const caps = this.capabilities.get(actorPath);
    if (!caps) return false;

    // Parse operation (e.g., 'storage.query' -> check 'read' capability)
    const [domain, action] = operation.split('.');
    return this.hasCapability(caps, domain, action);
  }

  private hasCapability(caps: any, domain: string, action: string): boolean {
    // Custom logic per domain
    // e.g., storage.query requires read capability
    // storage.execute requires write capability
    // etc.
  }
}
```

### Configuration Examples

**Workflows (Full Access):**
```typescript
registry.registerSystemActors('/workflows', {
  storage: {
    isolated: true,
    options: { dbPath: 'workflows.db' },
    capabilities: {
      read: true,
      write: true,
      delete: true,
      admin: true,
      subscribe: true,
    },
  },
  scheduler: { enabled: true }, // Proxy to /system/scheduler
  fs: {
    options: { allowedPaths: ['/workflows', '/tmp'] },
    capabilities: {
      read: true,
      write: true,
      delete: true,
      watch: true,
    },
  },
  process: {
    options: { allowedCommands: ['bun', 'node', 'git'], maxConcurrent: 5 },
    capabilities: {
      spawn: true,
      kill: true,
    },
  },
  http: {
    options: {
      allowedHosts: ['*'], // All hosts
      rateLimit: { requests: 100, window: 60000 },
      timeout: 30000,
    },
    capabilities: {
      methods: ['GET', 'POST', 'PUT', 'DELETE'],
    },
  },
});
```

**Domain Logic (Restricted):**
```typescript
registry.registerSystemActors('/domain', {
  storage: {
    isolated: false,
    proxyTo: '/workflows/system/storage', // Share workflows storage
    capabilities: {
      read: true,   // Read-only
      write: false,
      delete: false,
      admin: false,
      subscribe: true,
    },
  },
  scheduler: { enabled: true }, // Proxy to /system/scheduler
  // fs: NOT REGISTERED (denied)
  // process: NOT REGISTERED (denied)
  // http: NOT REGISTERED (denied)
});
```

**Session Knowledge (Isolated Storage, HTTP Only):**
```typescript
registry.registerSystemActors('/session-knowledge', {
  storage: {
    isolated: true,
    options: { dbPath: 'session.db' },
    capabilities: {
      read: true,
      write: true,
      delete: true,
      admin: true,
      subscribe: false,
    },
  },
  fs: {
    options: { allowedPaths: ['/session-logs'] },
    capabilities: {
      read: true,  // Read-only
      write: false,
      delete: false,
      watch: false,
    },
  },
  http: {
    options: {
      allowedHosts: ['api.cloudflare.com'], // Embedding service only
      rateLimit: { requests: 1000, window: 60000 },
      timeout: 10000,
    },
    capabilities: {
      methods: ['POST'],
    },
  },
});
```

---

## Actor Base Class Utilities

**Key Architectural Principle:** Distinguish between **universal coordination** (all actors) and **capability-based resources** (explicit configuration).

### Universal Coordination (Base Actor) ✅

These are **coordination primitives** - all actors get them, no security implications:

**Already Implemented:**
```typescript
export class Actor {
  // Logging (observability)
  protected logDebug(msg: string, context?: Record<string, any>): void
  protected logInfo(msg: string, context?: Record<string, any>): void
  protected logWarn(msg: string, context?: Record<string, any>): void
  protected logError(msg: string, context?: Record<string, any>): void
}
```

**To Be Implemented:**
```typescript
export class Actor {
  // Scheduling (time coordination)
  protected async schedule(delay: number, type: string, payload?: any): Promise<string>
  protected async scheduleRecurring(interval: number, type: string, payload?: any): Promise<string>
  protected async cancelSchedule(scheduleId: string): Promise<void>
}
```

**Why universal:** Time and logging are part of the actor model itself. Every actor coordinates through time and needs observability.

### Capability-Based Resources (Explicit Configuration) 🔒

These are **privileges**, NOT universal - actors must be explicitly configured:

**NOT in base Actor class:**
```typescript
// ❌ NO arbitrary database access
this.query('SELECT * FROM users')

// ❌ NO arbitrary file access
this.readFile('/etc/passwd')

// ❌ NO arbitrary process spawning
this.spawnProcess('rm -rf /')
```

**Instead: Capability-based design (Option A - Composition):**
```typescript
// Actor explicitly configured with scoped capabilities
export class TaskActor extends Actor {
  private storage: StorageCapability;
  private fs: FileSystemCapability;

  constructor(id: string, router: MessageRouter, config: {
    storage: { allowedTables: string[], operations: string[] },
    fs: { allowedPaths: string[], operations: string[] }
  }) {
    super(id, router);

    // Storage scoped to specific tables
    this.storage = new StorageCapability(router, {
      namespace: this.namespace,
      allowedTables: config.storage.allowedTables,
      operations: config.storage.operations
    });

    // FS scoped to specific directories
    this.fs = new FileSystemCapability(router, {
      namespace: this.namespace,
      allowedPaths: config.fs.allowedPaths,
      operations: config.fs.operations
    });
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Can ONLY access configured tables
    const tasks = await this.storage.query('SELECT * FROM tasks');

    // Can ONLY access configured paths
    const config = await this.fs.read('/workflows/tasks/config.json');

    // Attempts outside scope are denied
    // await this.storage.query('SELECT * FROM users'); // ❌ Error
    // await this.fs.read('/etc/passwd'); // ❌ Error
  }
}
```

**Option B (Future):** Specialized actor base classes
```typescript
// Pure computation (no I/O)
class ComputeActor extends Actor { }

// Storage-backed
class StorageActor extends Actor {
  protected storage: StorageCapability;
}

// Could be explored later if needed
```

**Recommendation:** Start with Option A (composition). Keep base Actor clean with only universal coordination primitives.

### Usage Comparison

**Before (Direct System Calls - Unsafe):**
```typescript
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // Direct setTimeout - not testable, blocks
    setTimeout(() => this.handleTimeout(), 5000);

    // Direct fs - no sandboxing, can read /etc/passwd
    const data = await readFile('config.json');

    // Direct database - no access control
    const tasks = db.prepare('SELECT * FROM tasks WHERE status = ?').all('open');

    return createResponse(message, { status: 'ok' });
  }
}
```

**After (Actor-based - Safe & Testable):**
```typescript
class TaskActor extends Actor {
  private storage: StorageCapability;
  private fs: FileSystemCapability;

  constructor(id: string, router: MessageRouter) {
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
    // Universal coordination - all actors have this
    await this.schedule(5000, 'timeout');
    this.logInfo('Task started');

    // Scoped file access - sandboxed to /workflows/*
    const data = await this.fs.read('/workflows/config/config.json');

    // Scoped database access - only 'tasks' and 'workflows' tables
    const tasks = await this.storage.query('SELECT * FROM tasks WHERE status = ?', ['open']);

    return createResponse(message, { status: 'ok' });
  }
}
```

**Benefits:**
- ✅ **Security** - Scoped access, cannot read arbitrary files/tables
- ✅ **Testable** - Mock capabilities, virtual time in tests
- ✅ **Explicit** - Capabilities declared at construction
- ✅ **Auditable** - Track which actors access what resources
- ✅ **Clean separation** - Coordination (schedule) vs capabilities (storage/fs)

---

## Pure Functions Allowlist

These are pure functions (no I/O) and are **allowed** to use directly:

**Crypto:**
- `crypto.randomUUID()` - Generate UUIDs
- `crypto.createHash()` - Hash data
- `crypto.createHmac()` - HMAC signatures
- `crypto.subtle.*` - Web Crypto API

**Path:**
- `path.join()`, `path.resolve()`, `path.basename()`, etc.
- All pure path manipulation

**URL:**
- `new URL()`, `URL.parse()`
- URL parsing and manipulation

**Utilities:**
- `JSON.parse()`, `JSON.stringify()`
- `Array`, `Set`, `Map` operations
- String manipulation

---

## Enforcement Tooling

### 1. ESLint Plugin: `eslint-plugin-actor-purity`

**Purpose:** Detect and prevent direct system calls

**Rules:**

```javascript
module.exports = {
  rules: {
    'no-direct-fs': {
      create(context) {
        return {
          ImportDeclaration(node) {
            if (node.source.value === 'fs' || node.source.value === 'node:fs') {
              context.report({
                node,
                message: 'Direct fs import forbidden. Use FileSystemActor instead.',
              });
            }
          },
        };
      },
    },
    'no-direct-database': {
      create(context) {
        return {
          ImportDeclaration(node) {
            const forbidden = ['better-sqlite3', '@libsql/client', 'pg', 'mysql2'];
            if (forbidden.includes(node.source.value)) {
              context.report({
                node,
                message: 'Direct database import forbidden. Use StorageActor instead.',
              });
            }
          },
        };
      },
    },
    'no-direct-timers': {
      create(context) {
        return {
          CallExpression(node) {
            if (
              node.callee.type === 'Identifier' &&
              ['setTimeout', 'setInterval'].includes(node.callee.name)
            ) {
              context.report({
                node,
                message: `Direct ${node.callee.name} forbidden. Use SchedulerActor instead.`,
              });
            }
          },
        };
      },
    },
    'no-direct-process': {
      create(context) {
        return {
          ImportDeclaration(node) {
            if (node.source.value === 'child_process' || node.source.value === 'node:child_process') {
              context.report({
                node,
                message: 'Direct child_process import forbidden. Use ProcessActor instead.',
              });
            }
          },
          CallExpression(node) {
            if (
              node.callee.type === 'MemberExpression' &&
              node.callee.object.name === 'Bun' &&
              ['spawn', 'exec'].includes(node.callee.property.name)
            ) {
              context.report({
                node,
                message: `Direct Bun.${node.callee.property.name} forbidden. Use ProcessActor instead.`,
              });
            }
          },
        };
      },
    },
  },
};
```

**Usage:**
```json
// .eslintrc.json
{
  "plugins": ["actor-purity"],
  "rules": {
    "actor-purity/no-direct-fs": "error",
    "actor-purity/no-direct-database": "error",
    "actor-purity/no-direct-timers": "error",
    "actor-purity/no-direct-process": "error"
  }
}
```

**Allowlist (exceptions):**
```json
{
  "rules": {
    "actor-purity/no-direct-fs": ["error", {
      "allowedPaths": [
        "src/messaging/actors/filesystem.ts",  // Actor implementation
        "src/system-actors/**/*.ts"             // System actor implementations
      ]
    }]
  }
}
```

---

### 2. Pre-commit Hook Scanner

**Purpose:** Scan for violations before commit

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "🔍 Scanning for direct system calls..."

# Find all TypeScript files in src (exclude system-actors and tests)
files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '^src/.*\.ts$' | grep -v 'system-actors' | grep -v '.test.ts')

if [ -z "$files" ]; then
  exit 0
fi

violations=0

# Check for forbidden imports
for file in $files; do
  # Check for direct fs imports
  if grep -q "import.*from ['\"]fs['\"]" "$file" || grep -q "import.*from ['\"]node:fs['\"]" "$file"; then
    echo "❌ $file: Forbidden 'fs' import (use FileSystemActor)"
    violations=$((violations + 1))
  fi

  # Check for direct database imports
  if grep -q "import.*from ['\"]better-sqlite3['\"]" "$file" || grep -q "import.*from ['\"]@libsql/client['\"]" "$file"; then
    echo "❌ $file: Forbidden database import (use StorageActor)"
    violations=$((violations + 1))
  fi

  # Check for setTimeout/setInterval
  if grep -q "setTimeout\|setInterval" "$file"; then
    echo "⚠️  $file: Found timer usage (prefer SchedulerActor)"
    # Warning only, not blocking
  fi

  # Check for Bun.spawn/exec
  if grep -q "Bun\.spawn\|Bun\.exec" "$file"; then
    echo "❌ $file: Forbidden Bun.spawn/exec (use ProcessActor)"
    violations=$((violations + 1))
  fi
done

if [ $violations -gt 0 ]; then
  echo ""
  echo "❌ Commit blocked: $violations violation(s) found"
  echo "Fix violations or implement using system actors"
  exit 1
fi

echo "✅ No violations found"
exit 0
```

---

### 3. Runtime Auditor

**Purpose:** Detect violations at runtime (development mode)

```typescript
/**
 * Runtime auditor - Monkey-patch system APIs to detect violations
 */
export class SystemCallAuditor {
  private violations: Array<{ type: string; stack: string; timestamp: number }> = [];

  /**
   * Enable auditing (development only)
   */
  enable(): void {
    if (process.env.NODE_ENV === 'production') {
      console.warn('SystemCallAuditor should not be enabled in production');
      return;
    }

    // Patch setTimeout
    const originalSetTimeout = global.setTimeout;
    global.setTimeout = ((fn: any, delay: any, ...args: any[]) => {
      this.recordViolation('setTimeout', new Error().stack);
      return originalSetTimeout(fn, delay, ...args);
    }) as any;

    // Patch fs methods
    const fs = require('fs');
    const originalReadFile = fs.readFile;
    fs.readFile = (...args: any[]) => {
      this.recordViolation('fs.readFile', new Error().stack);
      return originalReadFile(...args);
    };

    // Similar for other methods...

    console.log('🔍 SystemCallAuditor enabled');
  }

  private recordViolation(type: string, stack?: string): void {
    this.violations.push({
      type,
      stack: stack || '',
      timestamp: Date.now(),
    });

    console.warn(`⚠️  Direct system call detected: ${type}`);
    console.warn(stack);
  }

  /**
   * Get violation report
   */
  getReport(): { total: number; byType: Record<string, number> } {
    const byType: Record<string, number> = {};
    for (const v of this.violations) {
      byType[v.type] = (byType[v.type] || 0) + 1;
    }

    return {
      total: this.violations.length,
      byType,
    };
  }
}

// Usage in tests
const auditor = new SystemCallAuditor();
auditor.enable();

// ... run code ...

const report = auditor.getReport();
expect(report.total).toBe(0); // Assert no violations
```

---

## Implementation Phases

### Phase 0: Foundation (1 day)
- [ ] Design approval
- [ ] Create `src/system-actors/` directory structure
- [ ] Define capability interfaces
- [ ] Create SystemActorRegistry

### Phase 1: SchedulerActor (1-2 days)
- [ ] Implement SchedulerActor (real + virtual time)
- [ ] Replace all setTimeout/setInterval calls
- [ ] Update tests to use virtual time
- [ ] ~80% faster test suite (instant time)

### Phase 2: StorageActor (2-3 days)
- [ ] Implement StorageActor
- [ ] Replace all direct database calls (11 locations)
- [ ] In-memory storage for tests
- [ ] ~10x faster tests (no disk I/O)

### Phase 3: FileSystemActor (1-2 days)
- [ ] Implement FileSystemActor with sandboxing
- [ ] Replace all fs imports (4 locations)
- [ ] Virtual FS for tests
- [ ] Path-based permissions

### Phase 4: ProcessActor (1 day)
- [ ] Implement ProcessActor
- [ ] Replace ProgramExecutor's direct spawn calls
- [ ] Resource limits and supervision

### Phase 5: HTTPClientActor (1 day)
- [ ] Implement HTTPClientActor
- [ ] Wrap InferenceActor's HTTP calls
- [ ] Rate limiting and circuit breakers

### Phase 6: Enforcement Tooling (1 day)
- [ ] Create ESLint plugin
- [ ] Pre-commit hook
- [ ] Runtime auditor
- [ ] Documentation

### Phase 7: Migration & Cleanup (1-2 days)
- [ ] Migrate all namespaces to system actors
- [ ] Remove direct system call dependencies
- [ ] Update documentation
- [ ] Performance benchmarks

**Total Estimated Time:** 8-12 days

---

## Success Metrics

**Code Quality:**
- ✅ Zero direct system calls (enforced by linter)
- ✅ All I/O through actors
- ✅ Capability-based security enforced

**Testing:**
- ✅ 80% faster test suite (virtual time)
- ✅ 10x faster integration tests (in-memory storage)
- ✅ 100% mockable (all I/O through actors)

**Architecture:**
- ✅ Namespace isolation
- ✅ Actor sharing/proxying working
- ✅ Capability enforcement

**Security:**
- ✅ Sandboxed filesystem access
- ✅ Process spawning restrictions
- ✅ HTTP allowlist enforcement

---

## Open Questions

1. **Shared vs Isolated Storage:** Should all namespaces share one database, or have isolated databases?
   - **Recommendation:** Isolated by default, proxy where sharing needed

2. **Performance Overhead:** Will message passing add significant latency?
   - **Mitigation:** Benchmark critical paths, optimize hot paths

3. **Migration Strategy:** Big-bang or incremental?
   - **Recommendation:** Incremental - start with SchedulerActor (highest impact)

4. **Error Handling:** How do system actors report errors?
   - **Recommendation:** Standard error responses, capability errors distinct from runtime errors

5. **Observability:** How to monitor system actor usage?
   - **Recommendation:** Logger actor receives all system actor messages (audit log)

---

## Next Steps

1. Review and approve design
2. Create implementation plan (beads)
3. Start with Phase 1 (SchedulerActor) - highest ROI
4. Iterate and refine

**Ready to proceed?**
