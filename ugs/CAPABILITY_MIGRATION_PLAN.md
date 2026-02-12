# Capability Migration Plan

**Status:** Implementation Roadmap
**Date:** 2026-02-06
**Goal:** Migrate all direct database and filesystem calls to capability-based access

---

## Overview

This document maps existing direct system calls to the new capability-based system and provides a phased migration strategy.

---

## Current System Call Inventory

### Database Calls (Primary - Non-Test)

**1. TemporalQueries.ts** (8 calls)
- Location: `src/session-knowledge/temporal/TemporalQueries.ts`
- Current: `this.db.execute({ sql, params })`
- Tables: `session_decisions`, `session_learnings`, `session_errors`, `session_workflows`, `knowledge_relationships`, `thinking_arcs`
- Operations: Read, Write

**2. QueryEngine.ts** (10 calls)
- Location: `src/session-knowledge/index/QueryEngine.ts`
- Current: `await this.db.execute({ sql, params })`
- Tables: `session_decisions`, `session_learnings`, `session_errors`, `sessions`
- Operations: Read

**3. ArcDetector.ts** (3 calls)
- Location: `src/session-knowledge/temporal/ArcDetector.ts`
- Current: `await this.db.execute({ sql, params })`
- Tables: `session_decisions`, `session_learnings`, `thinking_arcs`
- Operations: Read, Write

**4. SessionMetadataExtractor.ts** (15+ calls)
- Location: `src/session-knowledge/index/SessionMetadataExtractor.ts`
- Current: `this.db.run()`, `this.db.exec()`
- Tables: `sessions`, `session_files`, `session_tools`, `session_agents`
- Operations: Read, Write, Delete, Admin (schema setup)

**5. LiveProcessor.ts** (1 call)
- Location: `src/session-knowledge/watcher/LiveProcessor.ts`
- Current: `this.db.run()`
- Tables: `sessions`
- Operations: Write

### Filesystem Calls (Primary - Non-Test)

**1. demo-code-execution-actor.ts** (1 call)
- Location: `demo-code-execution-actor.ts`
- Current: `fs.readFileSync('/etc/passwd', 'utf-8')`
- Paths: `/etc/passwd` (demo only - intentional violation)
- Operations: Read

### Migration Classification

**Can Stay Direct (Tests & Migrations):**
- `src/session-knowledge/__tests__/*.test.ts` - Test files (35+ calls)
- `src/session-knowledge/migrations/*.ts` - Migration scripts (30+ calls)
- `src/messaging/actors/program-executor.test.ts` - Test file (7 calls)

**Must Migrate (Production Code):**
- 5 production files with 37+ database calls
- 1 demo file with 1 filesystem call

---

## Migration Strategy

### Phase 1: Capability Infrastructure (Day 1)

**Goal:** Create capability classes and factory

**Tasks:**
1. ✅ Create `StorageCapability` class
   - `src/messaging/capabilities/storage.ts`
2. ✅ Create `FileSystemCapability` class
   - `src/messaging/capabilities/filesystem.ts`
3. ✅ Create `ActorFactory` class
   - `src/messaging/capabilities/factory.ts`
4. Create unit tests for capabilities
   - `src/messaging/capabilities/__tests__/storage.test.ts`
   - `src/messaging/capabilities/__tests__/filesystem.test.ts`

**Acceptance Criteria:**
- StorageCapability enforces table restrictions
- StorageCapability enforces operation restrictions
- FileSystemCapability enforces path restrictions
- CapabilityError thrown on violations
- All tests pass

---

### Phase 2: Migrate Session Knowledge (Days 2-3)

#### 2.1 TemporalQueries Migration

**File:** `src/session-knowledge/temporal/TemporalQueries.ts`

**Before:**
```typescript
export class TemporalQueries {
  private db: LibSQLDatabase;

  constructor(db: LibSQLDatabase) {
    this.db = db;
  }

  async getValidDecisions(timestamp: number): Promise<Decision[]> {
    const result = await this.db.execute({
      sql: 'SELECT * FROM session_decisions WHERE ...',
      args: [timestamp]
    });
    return result.rows;
  }
}
```

**After:**
```typescript
import { StorageCapability } from '../../messaging/capabilities/storage.ts';

export class TemporalQueries {
  private storage: StorageCapability;

  constructor(storage: StorageCapability) {
    this.storage = storage;
  }

  async getValidDecisions(timestamp: number): Promise<Decision[]> {
    return await this.storage.query(
      'SELECT * FROM session_decisions WHERE ...',
      [timestamp]
    );
  }
}
```

**Configuration:**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/session-knowledge',
  allowedTables: [
    'session_decisions',
    'session_learnings',
    'session_errors',
    'session_workflows',
    'knowledge_relationships',
    'thinking_arcs'
  ],
  operations: ['read', 'write']
});

const temporalQueries = new TemporalQueries(storage);
```

**Changes Required:**
- Constructor signature change: `LibSQLDatabase` → `StorageCapability`
- Replace all `this.db.execute()` with `this.storage.query()` or `this.storage.execute()`
- Update all callers to pass `StorageCapability` instead of `LibSQLDatabase`

**Risk Level:** Low
- API is similar (just different object)
- Tables are clearly scoped
- No complex transactions

---

#### 2.2 QueryEngine Migration

**File:** `src/session-knowledge/index/QueryEngine.ts`

**Before:**
```typescript
export class QueryEngine {
  private db: LibSQLDatabase;

  constructor(db: LibSQLDatabase) {
    this.db = db;
  }

  async getDecisions(filter: string): Promise<Decision[]> {
    const result = await this.db.execute({
      sql: 'SELECT * FROM session_decisions WHERE ...',
      args: [...]
    });
    return result.rows;
  }
}
```

**After:**
```typescript
import { StorageCapability } from '../../messaging/capabilities/storage.ts';

export class QueryEngine {
  private storage: StorageCapability;

  constructor(storage: StorageCapability) {
    this.storage = storage;
  }

  async getDecisions(filter: string): Promise<Decision[]> {
    return await this.storage.query(
      'SELECT * FROM session_decisions WHERE ...',
      [...]
    );
  }
}
```

**Configuration:**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/session-knowledge',
  allowedTables: [
    'session_decisions',
    'session_learnings',
    'session_errors',
    'sessions'
  ],
  operations: ['read'] // Read-only
});

const queryEngine = new QueryEngine(storage);
```

**Changes Required:**
- Constructor signature change
- Replace all `this.db.execute()` with `this.storage.query()`
- Update all callers

**Risk Level:** Low
- All queries are read-only
- Clear table scope
- No side effects

---

#### 2.3 ArcDetector Migration

**File:** `src/session-knowledge/temporal/ArcDetector.ts`

**Before:**
```typescript
export class ArcDetector {
  private db: LibSQLDatabase;

  async detectArcs(sessionId: string): Promise<ThinkingArc[]> {
    const decisions = await this.db.execute({
      sql: 'SELECT * FROM session_decisions WHERE session_id = ?',
      args: [sessionId]
    });

    // ... analysis ...

    await this.db.execute({
      sql: 'INSERT INTO thinking_arcs ...',
      args: [...]
    });
  }
}
```

**After:**
```typescript
import { StorageCapability } from '../../messaging/capabilities/storage.ts';

export class ArcDetector {
  private storage: StorageCapability;

  constructor(storage: StorageCapability) {
    this.storage = storage;
  }

  async detectArcs(sessionId: string): Promise<ThinkingArc[]> {
    const decisions = await this.storage.query(
      'SELECT * FROM session_decisions WHERE session_id = ?',
      [sessionId]
    );

    // ... analysis ...

    await this.storage.execute(
      'INSERT INTO thinking_arcs ...',
      [...]
    );
  }
}
```

**Configuration:**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/session-knowledge',
  allowedTables: ['session_decisions', 'session_learnings', 'thinking_arcs'],
  operations: ['read', 'write']
});

const arcDetector = new ArcDetector(storage);
```

**Risk Level:** Medium
- Mix of read and write operations
- Need to ensure capability has both operations

---

#### 2.4 SessionMetadataExtractor Migration

**File:** `src/session-knowledge/index/SessionMetadataExtractor.ts`

**Before:**
```typescript
export class SessionMetadataExtractor {
  private db: Database; // better-sqlite3

  constructor(dbPath: string) {
    this.db = new Database(dbPath);
    this.db.exec('PRAGMA journal_mode = WAL');
    this.db.exec(schema); // Schema setup
  }

  async extractMetadata(sessionId: string): Promise<void> {
    this.db.run('BEGIN TRANSACTION');
    try {
      this.db.run('DELETE FROM session_files WHERE session_id = ?', [sessionId]);
      this.db.run('INSERT INTO session_files ...', [...]);
      this.db.run('COMMIT');
    } catch (error) {
      this.db.run('ROLLBACK');
      throw error;
    }
  }
}
```

**After:**
```typescript
import { StorageCapability } from '../../messaging/capabilities/storage.ts';

export class SessionMetadataExtractor {
  private storage: StorageCapability;

  constructor(storage: StorageCapability) {
    this.storage = storage;
  }

  async extractMetadata(sessionId: string): Promise<void> {
    await this.storage.transaction([
      { sql: 'DELETE FROM session_files WHERE session_id = ?', params: [sessionId] },
      { sql: 'INSERT INTO session_files ...', params: [...] }
    ]);
  }
}
```

**Configuration:**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/session-knowledge',
  allowedTables: ['sessions', 'session_files', 'session_tools', 'session_agents'],
  operations: ['read', 'write', 'delete', 'admin'] // Admin for schema setup
});

const extractor = new SessionMetadataExtractor(storage);
```

**Changes Required:**
- Constructor signature change (no longer creates DB)
- Replace `this.db.run()` with `this.storage.execute()`
- Replace `BEGIN/COMMIT/ROLLBACK` with `this.storage.transaction()`
- Schema setup moved outside (StorageActor responsibility)

**Risk Level:** Medium-High
- Uses transactions
- Direct DB construction removed (needs DB to be pre-created)
- Schema setup needs new pattern

**Recommendation:** Create StorageActor first to handle schema setup

---

#### 2.5 LiveProcessor Migration

**File:** `src/session-knowledge/watcher/LiveProcessor.ts`

**Before:**
```typescript
export class LiveProcessor {
  private db: Database;

  async processSession(sessionId: string): Promise<void> {
    this.db.run(`
      INSERT INTO sessions (id, processed_at)
      VALUES (?, ?)
    `, [sessionId, Date.now()]);
  }
}
```

**After:**
```typescript
import { StorageCapability } from '../../messaging/capabilities/storage.ts';

export class LiveProcessor {
  private storage: StorageCapability;

  constructor(storage: StorageCapability) {
    this.storage = storage;
  }

  async processSession(sessionId: string): Promise<void> {
    await this.storage.execute(
      'INSERT INTO sessions (id, processed_at) VALUES (?, ?)',
      [sessionId, Date.now()]
    );
  }
}
```

**Configuration:**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/session-knowledge',
  allowedTables: ['sessions'],
  operations: ['write']
});

const liveProcessor = new LiveProcessor(storage);
```

**Risk Level:** Low
- Simple insert operations
- Single table

---

### Phase 3: Create StorageActor (Day 4)

**Goal:** Implement system actor that capabilities route to

**Tasks:**
1. Create `StorageActor` implementation
   - `src/messaging/actors/storage.ts`
   - Handles `storage.query`, `storage.execute`, `storage.transaction` messages
2. Create initialization logic
   - Database creation
   - Schema setup
   - Connection pooling
3. Register StorageActor for namespaces
   - `/session-knowledge/system/storage`
   - `/workflows/system/storage`
4. Add integration tests

**Example:**
```typescript
export class StorageActor extends Actor {
  private db: LibSQLDatabase;

  constructor(router: MessageRouter, config: { dbPath: string }) {
    super('storage', router);
    this.db = createClient({ url: `file:${config.dbPath}` });
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    if (type === 'storage.query') {
      const result = await this.db.execute({
        sql: payload.sql,
        args: payload.params || []
      });
      return createResponse(message, { success: true, payload: result.rows });
    }

    if (type === 'storage.execute') {
      const result = await this.db.execute({
        sql: payload.sql,
        args: payload.params || []
      });
      return createResponse(message, {
        success: true,
        payload: {
          changes: result.rowsAffected,
          lastInsertRowid: result.lastInsertRowid
        }
      });
    }

    if (type === 'storage.transaction') {
      await this.db.batch(payload.statements.map((s: any) => ({
        sql: s.sql,
        args: s.params || []
      })));
      return createResponse(message, { success: true });
    }

    return createResponse(message, { success: false, error: 'Unknown message type' });
  }
}
```

---

### Phase 4: Integration & Testing (Day 5)

**Goal:** Verify end-to-end capability system works

**Tasks:**
1. Create integration tests
   - Actor with capabilities accessing storage
   - Capability violations properly rejected
   - Error messages clear and actionable
2. Update existing tests
   - Pass StorageCapability instead of LibSQLDatabase
   - Verify backward compatibility
3. Performance benchmarking
   - Measure overhead of capability layer
   - Optimize if needed
4. Documentation updates
   - Update API docs
   - Add migration guide
   - Update examples

**Integration Test Example:**
```typescript
describe('Capability Integration', () => {
  it('should enforce table restrictions', async () => {
    const router = new MessageRouter();

    // Register StorageActor
    const storageActor = new StorageActor(router, { dbPath: ':memory:' });
    router.registerActor(address('/test/system/storage'), storageActor);

    // Create capability
    const storage = new StorageCapability(router, {
      namespace: '/test',
      allowedTables: ['tasks'],
      operations: ['read']
    });

    // ✅ Should succeed
    const tasks = await storage.query('SELECT * FROM tasks');

    // ❌ Should fail
    await expect(
      storage.query('SELECT * FROM users')
    ).rejects.toThrow(CapabilityError);
  });
});
```

---

## Migration Checklist

### Phase 1: Infrastructure ✅
- [x] Create StorageCapability class
- [x] Create FileSystemCapability class
- [x] Create ActorFactory class
- [ ] Unit tests for StorageCapability
- [ ] Unit tests for FileSystemCapability
- [ ] Unit tests for CapabilityError

### Phase 2: Migrate Session Knowledge
- [ ] Migrate TemporalQueries
  - [ ] Update constructor
  - [ ] Replace db.execute calls
  - [ ] Update callers
  - [ ] Tests pass
- [ ] Migrate QueryEngine
  - [ ] Update constructor
  - [ ] Replace db.execute calls
  - [ ] Update callers
  - [ ] Tests pass
- [ ] Migrate ArcDetector
  - [ ] Update constructor
  - [ ] Replace db.execute calls
  - [ ] Update callers
  - [ ] Tests pass
- [ ] Migrate SessionMetadataExtractor
  - [ ] Update constructor
  - [ ] Replace db.run calls
  - [ ] Replace transactions
  - [ ] Update callers
  - [ ] Tests pass
- [ ] Migrate LiveProcessor
  - [ ] Update constructor
  - [ ] Replace db.run calls
  - [ ] Update callers
  - [ ] Tests pass

### Phase 3: Create StorageActor
- [ ] Implement StorageActor class
- [ ] Add message handlers
  - [ ] storage.query
  - [ ] storage.execute
  - [ ] storage.transaction
  - [ ] storage.subscribe
- [ ] Add database initialization
- [ ] Register actors for namespaces
- [ ] Integration tests

### Phase 4: Integration & Testing
- [ ] End-to-end integration tests
- [ ] Performance benchmarks
- [ ] Update documentation
- [ ] Migration guide
- [ ] Code review
- [ ] Deploy to production

---

## Risk Mitigation

### Risk: Breaking Changes
**Mitigation:**
- Migrate one file at a time
- Keep tests passing at each step
- Create compatibility layer if needed

### Risk: Performance Degradation
**Mitigation:**
- Benchmark before/after
- Optimize hot paths if needed
- Consider caching parsed SQL

### Risk: Capability Violations in Production
**Mitigation:**
- Comprehensive testing of capability configs
- Log all violations (don't just throw)
- Gradual rollout with monitoring

### Risk: Transaction Complexity
**Mitigation:**
- Test transaction handling thoroughly
- Document transaction patterns
- Provide examples

---

## Success Criteria

1. ✅ All production code uses capabilities
2. ✅ Zero direct database/filesystem calls (except tests/migrations)
3. ✅ All tests pass
4. ✅ Performance within 5% of baseline
5. ✅ Clear error messages on capability violations
6. ✅ Documentation complete

---

## Estimated Timeline

- **Phase 1:** 1 day (capability infrastructure + tests)
- **Phase 2:** 2 days (migrate 5 files)
- **Phase 3:** 1 day (create StorageActor)
- **Phase 4:** 1 day (integration & testing)

**Total:** 5 days

---

## Next Steps

1. Get design approval (CAPABILITY_DESIGN.md)
2. Implement Phase 1 (capability classes + tests)
3. Migrate TemporalQueries (easiest first)
4. Continue with remaining files
5. Create StorageActor
6. Integration testing
7. Documentation & deployment

---

**Document End**
