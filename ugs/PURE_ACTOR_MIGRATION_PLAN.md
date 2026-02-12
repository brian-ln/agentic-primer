# Pure Actor Model Migration Plan

**Date:** 2026-02-06
**Status:** Implementation Roadmap
**Branch:** feature/path-addressing
**Bead:** simplify-0u7.5

---

## Overview

This document describes migration from the capability helper class approach to the pure actor model. The key insight: **capabilities should be actors, not helper objects**.

---

## Architectural Shift

### Old Approach: Helper Classes

```typescript
// Helper class wraps router calls
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
  private storage: StorageCapability;

  constructor(id: string, router: MessageRouter) {
    super(id, router);
    this.storage = new StorageCapability(router, {
      namespace: '/workflows',
      allowedTables: ['tasks']
    });
  }
}
```

**Problems:**
- Two layers of validation (helper + actor)
- Actors depend on helper classes (coupling)
- Configuration spread across helper and actor
- Testing requires mocking helpers AND router

### New Approach: Pure Actor Model

```typescript
// System actor validates internally
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

// Actors use standard messaging
class TaskActor extends Actor {
  async receive(message: Message) {
    if (message.type === 'get-tasks') {
      const response = await this.ask(
        address('/workflows/system/storage'),
        'storage.query',
        { sql: 'SELECT * FROM tasks' }
      );
      return createSuccessResponse(message, { tasks: response.payload.rows });
    }
  }
}

// Access control through routing
router.registerActor('/workflows/system/storage', workflowsStorage);
router.registerActor('/domain/system/storage', domainStorage);
// No registration for /untrusted → implicit denial
```

**Benefits:**
- Single validation point (in actor)
- No coupling (standard messaging)
- Configuration at actor construction
- Access control through routing (explicit)
- Simpler testing (mock router only)

---

## Migration Strategy

### Phase 1: Create System Actors

**Goal:** Implement StorageActor and FileSystemActor

**Tasks:**
1. Implement StorageActor (see STORAGE_ACTOR_DESIGN.md)
   - Internal table validation
   - Message protocol (storage.query, storage.execute, storage.transaction)
   - Clear error messages
2. Implement FileSystemActor (see FILESYSTEM_ACTOR_DESIGN.md)
   - Internal path sandboxing
   - Message protocol (fs.read, fs.write, fs.list, etc.)
   - Atomic writes, path normalization
3. Create unit tests
   - Table/path validation tests
   - Operation permission tests
   - Error message tests

**Deliverables:**
- `src/messaging/actors/system/StorageActor.ts`
- `src/messaging/actors/system/FileSystemActor.ts`
- `src/messaging/actors/system/__tests__/StorageActor.test.ts`
- `src/messaging/actors/system/__tests__/FileSystemActor.test.ts`

**Estimated Time:** 2 days

---

### Phase 2: Update Existing Actors

**Goal:** Replace helper class usage with direct messaging

**Current Usage (Helper Classes):**
```typescript
class TaskActor extends Actor {
  private storage: StorageCapability;

  constructor(id: string, router: MessageRouter) {
    super(id, router);
    this.storage = new StorageCapability(router, {
      namespace: '/workflows',
      allowedTables: ['tasks']
    });
  }

  async getTasks() {
    return await this.storage.query('SELECT * FROM tasks');
  }
}
```

**New Usage (Pure Actor):**
```typescript
class TaskActor extends Actor {
  async getTasks() {
    const response = await this.ask(
      address('/workflows/system/storage'),
      'storage.query',
      { sql: 'SELECT * FROM tasks' }
    );
    return response.payload.rows;
  }
}
```

**Files to Update:**
- `src/session-knowledge/temporal/TemporalQueries.ts` - Remove StorageCapability
- `src/session-knowledge/index/QueryEngine.ts` - Remove StorageCapability
- `src/session-knowledge/temporal/ArcDetector.ts` - Remove StorageCapability
- `src/session-knowledge/index/SessionMetadataExtractor.ts` - Remove StorageCapability
- `src/session-knowledge/watcher/LiveProcessor.ts` - Remove StorageCapability
- Any demo/test actors using FileSystemCapability

**Pattern:**
1. Remove capability instance variables
2. Replace `this.storage.query()` with `this.ask(address('/namespace/system/storage'), 'storage.query', ...)`
3. Replace `this.fs.read()` with `this.ask(address('/namespace/system/fs'), 'fs.read', ...)`
4. Update error handling (check `response.success`)

**Estimated Time:** 1 day

---

### Phase 3: Router Registration

**Goal:** Register system actors for all namespaces

**Registration Pattern:**
```typescript
// Create system actors with namespace-specific configs
const workflowsStorage = new StorageActor('workflows-storage', router, {
  dbPath: './data/workflows.db',
  allowedTables: ['tasks', 'workflows', 'executions'],
  operations: ['read', 'write', 'delete']
});

const domainStorage = new StorageActor('domain-storage', router, {
  dbPath: './data/domain.db',
  allowedTables: ['entities', 'relationships'],
  operations: ['read'] // Read-only
});

const knowledgeStorage = new StorageActor('knowledge-storage', router, {
  dbPath: './data/knowledge.db',
  allowedTables: ['*'], // All tables
  operations: ['read', 'write', 'delete', 'admin']
});

// Register at namespace paths
router.registerActor('/workflows/system/storage', workflowsStorage);
router.registerActor('/domain/system/storage', domainStorage);
router.registerActor('/session-knowledge/system/storage', knowledgeStorage);

// Filesystem actors
const workflowsFs = new FileSystemActor('workflows-fs', router, {
  allowedPaths: ['/workflows/config', '/workflows/data'],
  operations: ['read', 'write', 'delete']
});

router.registerActor('/workflows/system/fs', workflowsFs);
```

**Central Registration File:**
Create `src/messaging/system-actors.ts`:
```typescript
import { MessageRouter } from './router.ts';
import { StorageActor } from './actors/system/StorageActor.ts';
import { FileSystemActor } from './actors/system/FileSystemActor.ts';
import { address } from './message.ts';

export function registerSystemActors(router: MessageRouter): void {
  // Storage actors
  const workflowsStorage = new StorageActor('workflows-storage', router, {
    dbPath: './data/workflows.db',
    allowedTables: ['tasks', 'workflows', 'executions'],
    operations: ['read', 'write', 'delete']
  });
  router.registerActor(address('/workflows/system/storage'), workflowsStorage);

  const domainStorage = new StorageActor('domain-storage', router, {
    dbPath: './data/domain.db',
    allowedTables: ['entities', 'relationships'],
    operations: ['read']
  });
  router.registerActor(address('/domain/system/storage'), domainStorage);

  // Filesystem actors
  const workflowsFs = new FileSystemActor('workflows-fs', router, {
    allowedPaths: ['/workflows/config', '/workflows/data'],
    operations: ['read', 'write', 'delete']
  });
  router.registerActor(address('/workflows/system/fs'), workflowsFs);

  // Add more as needed
}

// In main initialization
const router = new MessageRouter(store, programManager);
registerSystemActors(router);
```

**Estimated Time:** 0.5 days

---

### Phase 4: Remove Helper Classes

**Goal:** Delete StorageCapability and FileSystemCapability files

**Files to Remove:**
- `src/messaging/capabilities/storage.ts`
- `src/messaging/capabilities/filesystem.ts`
- `src/messaging/capabilities/factory.ts` (if not used for other purposes)

**Verification:**
1. Run tests to ensure no imports remain
2. Search codebase for `StorageCapability` and `FileSystemCapability` references
3. Verify all actors use direct messaging

**Estimated Time:** 0.5 days

---

### Phase 5: Integration Testing

**Goal:** Verify routing-based access control works end-to-end

**Test Scenarios:**

**1. Access Within Namespace (✅ Should Work)**
```typescript
// Workflow actor accessing workflow storage
const workflowActor = new TaskActor('workflow-task', router);
router.registerActor(address('/workflows/tasks/processor'), workflowActor);

const response = await workflowActor.ask(
  address('/workflows/system/storage'),
  'storage.query',
  { sql: 'SELECT * FROM tasks' }
);

expect(response.success).toBe(true);
```

**2. Table Restriction (❌ Should Fail)**
```typescript
// Actor tries to access table not in allowedTables
const response = await workflowActor.ask(
  address('/workflows/system/storage'),
  'storage.query',
  { sql: 'SELECT * FROM users' } // 'users' not in allowedTables
);

expect(response.success).toBe(false);
expect(response.error).toContain('not in allowedTables');
```

**3. Operation Restriction (❌ Should Fail)**
```typescript
// Read-only storage actor receiving write request
const response = await domainActor.ask(
  address('/domain/system/storage'),
  'storage.execute',
  { sql: 'UPDATE entities SET ...' }
);

expect(response.success).toBe(false);
expect(response.error).toContain('Operation not permitted');
```

**4. Path Sandboxing (❌ Should Fail)**
```typescript
// Actor tries to access path outside allowedPaths
const response = await workflowActor.ask(
  address('/workflows/system/fs'),
  'fs.read',
  { path: '/etc/passwd' }
);

expect(response.success).toBe(false);
expect(response.error).toContain('Path access denied');
```

**5. Path Escape Prevention (❌ Should Fail)**
```typescript
// Actor tries to escape via ../
const response = await workflowActor.ask(
  address('/workflows/system/fs'),
  'fs.read',
  { path: '/workflows/config/../../../etc/passwd' }
);

expect(response.success).toBe(false);
expect(response.error).toContain('Path access denied');
// (path.resolve() normalizes to /etc/passwd)
```

**Test File:** `src/messaging/actors/system/__tests__/access-control.integration.test.ts`

**Estimated Time:** 1 day

---

## Current System Call Inventory

### Database Calls (Non-Test, Non-Migration)

**1. TemporalQueries.ts** (8 calls)
- Tables: `session_decisions`, `session_learnings`, `session_errors`, `session_workflows`, `knowledge_relationships`, `thinking_arcs`
- Operations: Read, Write
- **Change:** Replace `this.db.execute()` with `this.ask(address('/session-knowledge/system/storage'), 'storage.query', ...)`

**2. QueryEngine.ts** (10 calls)
- Tables: `session_decisions`, `session_learnings`, `session_errors`, `sessions`
- Operations: Read
- **Change:** Replace `this.db.execute()` with `this.ask(address('/session-knowledge/system/storage'), 'storage.query', ...)`

**3. ArcDetector.ts** (3 calls)
- Tables: `session_decisions`, `session_learnings`, `thinking_arcs`
- Operations: Read, Write
- **Change:** Replace `this.db.execute()` with `this.ask()` for both read and write

**4. SessionMetadataExtractor.ts** (15+ calls)
- Tables: `sessions`, `session_files`, `session_tools`, `session_agents`
- Operations: Read, Write, Delete, Admin (schema setup)
- **Change:** Replace `this.db.run()` with `this.ask()`, use `storage.transaction` for multi-statement operations

**5. LiveProcessor.ts** (1 call)
- Tables: `sessions`
- Operations: Write
- **Change:** Replace `this.db.run()` with `this.ask()`

### Filesystem Calls (Non-Test)

**1. demo-code-execution-actor.ts** (1 call)
- Paths: `/etc/passwd` (intentional violation for demo)
- Operations: Read
- **Change:** Keep as demo of access control violation

---

## Migration Checklist

### Phase 1: Create System Actors
- [ ] Implement StorageActor
  - [ ] Message handlers (query, execute, transaction)
  - [ ] Table validation
  - [ ] Operation permissions
  - [ ] Error handling
- [ ] Implement FileSystemActor
  - [ ] Message handlers (read, write, delete, list, stat, watch)
  - [ ] Path validation
  - [ ] Operation permissions
  - [ ] Atomic writes
- [ ] Unit tests
  - [ ] StorageActor tests (25 tests)
  - [ ] FileSystemActor tests (20 tests)

### Phase 2: Update Existing Actors
- [ ] Migrate TemporalQueries
- [ ] Migrate QueryEngine
- [ ] Migrate ArcDetector
- [ ] Migrate SessionMetadataExtractor
- [ ] Migrate LiveProcessor
- [ ] Update tests

### Phase 3: Router Registration
- [ ] Create system-actors.ts
- [ ] Register StorageActors for namespaces
- [ ] Register FileSystemActors for namespaces
- [ ] Update initialization code

### Phase 4: Remove Helper Classes
- [ ] Delete storage.ts
- [ ] Delete filesystem.ts
- [ ] Delete factory.ts (if unused)
- [ ] Verify no imports remain

### Phase 5: Integration Testing
- [ ] Access within namespace test
- [ ] Table restriction test
- [ ] Operation restriction test
- [ ] Path sandboxing test
- [ ] Path escape prevention test
- [ ] Cross-namespace isolation test
- [ ] Error message clarity test

---

## Benefits of Pure Actor Model

### 1. Simpler Architecture
- **Before:** Actor → Helper (validates) → Router → System Actor
- **After:** Actor → Router → System Actor (validates)
- One less layer, clearer data flow

### 2. Explicit Access Control
- **Before:** Access = capability instance passed to actor
- **After:** Access = system actor registered at namespace path
- Routing table IS the access control list

### 3. Better Testability
- **Before:** Mock helper classes + mock router
- **After:** Mock router only
- Fewer mocks = simpler tests

### 4. More Flexible
- **Before:** One helper instance per actor (configuration duplication)
- **After:** One system actor per namespace (shared configuration)
- Easier to update access policies

### 5. Clear Separation of Concerns
- **Before:** Validation split between helper and actor
- **After:** Validation entirely in system actor
- System actors are the single source of truth

---

## Estimated Timeline

- **Phase 1:** 2 days (create system actors + tests)
- **Phase 2:** 1 day (update existing actors)
- **Phase 3:** 0.5 days (router registration)
- **Phase 4:** 0.5 days (remove helper classes)
- **Phase 5:** 1 day (integration testing)

**Total:** 5 days

---

## Success Criteria

1. ✅ All production code uses system actors (no helper classes)
2. ✅ All tests pass (100% pass rate maintained)
3. ✅ Routing-based access control verified
4. ✅ Clear error messages on violations
5. ✅ Documentation complete (design + migration)
6. ✅ Helper classes removed from codebase

---

## Next Steps

1. Get design approval (STORAGE_ACTOR_DESIGN.md, FILESYSTEM_ACTOR_DESIGN.md)
2. Implement Phase 1 (system actors + tests)
3. Implement Phase 2 (migrate existing actors)
4. Implement Phase 3 (router registration)
5. Implement Phase 4 (remove helpers)
6. Implement Phase 5 (integration testing)
7. Documentation & deployment

---

**Document End**
