# Phase 2: Capability Design - Completion Summary

**Date:** 2026-02-06
**Status:** ✅ Design Complete - Ready for Implementation
**Branch:** feature/path-addressing

---

## Deliverables Completed

### 1. Design Documentation ✅

**CAPABILITY_DESIGN.md** - Comprehensive capability system design
- Location: `/Users/bln/play/agentic-primer/simplify/CAPABILITY_DESIGN.md`
- 900+ lines of detailed design specifications
- Complete interface definitions for StorageCapability and FileSystemCapability
- Actor construction patterns (ActorFactory)
- Configuration examples
- Error handling strategies
- Testing approach

**Key Design Decisions:**
- ✅ Composition pattern (capabilities NOT in base Actor class)
- ✅ Enforcement at capability layer (before routing to system actors)
- ✅ Table-level + operation-level scoping for storage
- ✅ Path-level + operation-level scoping for filesystem
- ✅ Explicit allowlists (no denylists)
- ✅ CapabilityError type for violations

### 2. Code Implementations ✅

**StorageCapability Class**
- Location: `/Users/bln/play/agentic-primer/simplify/src/messaging/capabilities/storage.ts`
- Methods: query(), execute(), delete(), transaction(), subscribe()
- Features:
  - SQL table extraction (regex-based)
  - Table validation against allowedTables
  - Operation validation (read/write/delete/admin/subscribe)
  - Message routing to /[namespace]/system/storage
  - CapabilityError on violations

**FileSystemCapability Class**
- Location: `/Users/bln/play/agentic-primer/simplify/src/messaging/capabilities/filesystem.ts`
- Methods: read(), write(), delete(), list(), stat(), watch()
- Features:
  - Path resolution and validation
  - Sandboxing to allowedPaths
  - Operation validation (read/write/delete/watch)
  - Message routing to /[namespace]/system/fs
  - Prevention of ../ escapes

**ActorFactory Class**
- Location: `/Users/bln/play/agentic-primer/simplify/src/messaging/capabilities/factory.ts`
- Features:
  - Factory pattern for actor construction with capabilities
  - Capability attachment via duck typing
  - Pre-configured capability profiles (readOnly, fullAccess, etc.)
  - Clean separation of actor creation and capability configuration

### 3. Examples & Patterns ✅

**Capability Configuration Examples**
- Location: `/Users/bln/play/agentic-primer/simplify/examples/capability-configurations.ts`
- 7 example actor configurations:
  - WorkflowExecutorActor - Full access
  - TaskManagerActor - Scoped read/write
  - SessionIndexerActor - Isolated storage + read-only filesystem
  - QueryEngineActor - Read-only storage
  - BusinessLogicActor - Storage only, no filesystem
  - ComputeActor - No capabilities (pure computation)
  - MigrationRunnerActor - Admin access for schema changes
- Shows different scoping patterns for different use cases

### 4. Migration Planning ✅

**CAPABILITY_MIGRATION_PLAN.md** - Detailed migration roadmap
- Location: `/Users/bln/play/agentic-primer/simplify/CAPABILITY_MIGRATION_PLAN.md`
- Complete inventory of current direct calls:
  - 5 production files with 37+ database calls
  - 1 demo file with 1 filesystem call
- File-by-file migration guides with before/after code
- 4-phase implementation plan:
  - Phase 1: Capability infrastructure + tests (1 day)
  - Phase 2: Migrate session knowledge files (2 days)
  - Phase 3: Create StorageActor (1 day)
  - Phase 4: Integration & testing (1 day)
- Risk assessment and mitigation strategies
- Success criteria and acceptance tests

### 5. Beads Created ✅

**Design Phase Beads** (6 beads)
- Design StorageCapability interface
- Design FileSystemCapability interface
- Define capability configuration format
- Document capability enforcement patterns
- Create migration plan for database calls
- Create migration plan for filesystem calls

**Implementation Phase Beads** (7 beads)
- Implement StorageCapability unit tests
- Implement FileSystemCapability unit tests
- Migrate TemporalQueries to StorageCapability
- Migrate QueryEngine to StorageCapability
- Migrate ArcDetector to StorageCapability
- Implement StorageActor for message routing
- Create capability integration tests

---

## Key Design Highlights

### Capability Scoping Examples

**Storage Capability:**
```typescript
const storage = new StorageCapability(router, {
  namespace: '/workflows',
  allowedTables: ['tasks', 'workflows'], // Table-level scoping
  operations: ['read', 'write']           // Operation-level scoping
});

// ✅ Allowed
await storage.query('SELECT * FROM tasks WHERE status = ?', ['open']);

// ❌ Denied - table not in allowedTables
await storage.query('SELECT * FROM users');
// Error: Table 'users' not in allowedTables: [tasks, workflows]
```

**Filesystem Capability:**
```typescript
const fs = new FileSystemCapability(router, {
  namespace: '/workflows',
  allowedPaths: ['/workflows/config', '/workflows/data'], // Path-level scoping
  operations: ['read', 'write']                            // Operation-level scoping
});

// ✅ Allowed
await fs.read('/workflows/config/settings.json');

// ❌ Denied - path not in allowedPaths
await fs.read('/etc/passwd');
// Error: Path '/etc/passwd' not in allowedPaths: [/workflows/config, /workflows/data]
```

### Composition Pattern

```typescript
export class TaskActor extends Actor {
  private storage: StorageCapability;  // Explicitly configured
  private fs: FileSystemCapability;     // Not in base class

  constructor(id: string, router: MessageRouter, config: ActorConfig) {
    super(id, router);

    // Explicit capability configuration at construction
    this.storage = new StorageCapability(router, {
      namespace: '/workflows',
      allowedTables: ['tasks', 'workflows'],
      operations: ['read', 'write']
    });

    this.fs = new FileSystemCapability(router, {
      namespace: '/workflows',
      allowedPaths: ['/workflows/tasks'],
      operations: ['read', 'write']
    });
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Use capabilities with enforced restrictions
    const tasks = await this.storage.query('SELECT * FROM tasks');
    const config = await this.fs.read('/workflows/tasks/config.json');
    // ...
  }
}
```

---

## Migration Complexity Assessment

### Low Complexity ✅
- Capability classes are self-contained
- SQL extraction is regex-based (simple, can upgrade later)
- Path validation is straightforward
- No breaking changes to external APIs

### Medium Complexity ⚠️
- Need to thread capability instances through constructors
- Tests need to be updated to pass capabilities instead of raw DB
- Backward compatibility during migration

### High Complexity ❌
- None identified

**Total Estimated Time:** 5 days for full implementation

---

## Architecture Alignment

### Aligns with System Actors Design ✅

The capability design is fully compatible with SYSTEM_ACTORS_DESIGN.md:

1. **Universal Coordination** (base Actor class):
   - Logging (implemented)
   - Scheduling (to be implemented)

2. **Capability-Based Resources** (explicit configuration):
   - Storage (this design)
   - Filesystem (this design)
   - Process (future)
   - HTTP (future)

3. **Composition Pattern** (Option A):
   - Capabilities attached at construction
   - Not in base Actor class
   - Clean separation of concerns

4. **System Actor Routing**:
   - Capabilities route to `/[namespace]/system/storage`
   - Capabilities route to `/[namespace]/system/fs`
   - System actors handle actual I/O

---

## Examples of Capability Usage

### Read-Only Query Engine
```typescript
const queryEngine = factory.createActor(QueryEngineActor, {
  id: 'query-engine',
  namespace: '/session-knowledge',
  capabilities: CapabilityProfiles.readOnly(
    '/session-knowledge',
    ['sessions', 'messages', 'session_decisions'],
    []
  )
});

// Can read, cannot write
const decisions = await queryEngine.storage.query('SELECT * FROM session_decisions');
// await queryEngine.storage.execute('DELETE FROM sessions'); // ❌ Error
```

### Full Access Workflow Executor
```typescript
const executor = factory.createActor(WorkflowExecutorActor, {
  id: 'workflow-executor',
  namespace: '/workflows',
  capabilities: CapabilityProfiles.fullAccess(
    '/workflows',
    ['tasks', 'workflows', 'executions'],
    ['/workflows/data', '/workflows/state']
  )
});

// Can read/write storage and filesystem
await executor.storage.execute('UPDATE tasks SET status = ?', ['done']);
await executor.fs.write('/workflows/state/task-123.json', JSON.stringify(state));
```

### Pure Computation Actor (No Capabilities)
```typescript
const compute = factory.createActor(ComputeActor, {
  id: 'compute-actor',
  namespace: '/domain',
  capabilities: CapabilityProfiles.noCapabilities('/domain')
});

// No I/O capabilities - pure computation only
// const data = await compute.storage.query('...'); // ❌ Error: storage is undefined
```

---

## Next Steps

### Immediate (Phase 1 - Day 1)
1. Implement unit tests for StorageCapability
   - Table validation tests
   - Operation enforcement tests
   - SQL parsing edge cases
   - CapabilityError handling

2. Implement unit tests for FileSystemCapability
   - Path validation tests
   - Sandboxing tests (../ escapes)
   - Operation enforcement tests

### Short-term (Phase 2 - Days 2-3)
1. Migrate TemporalQueries (easiest first - 8 calls)
2. Migrate QueryEngine (read-only - 10 calls)
3. Migrate ArcDetector (read/write - 3 calls)

### Medium-term (Phase 3 - Day 4)
1. Implement StorageActor
2. Register StorageActor for namespaces
3. Integration testing

### Long-term (Phase 4 - Day 5)
1. End-to-end integration tests
2. Performance benchmarking
3. Documentation updates
4. Code review and deployment

---

## Recommendations

### Implementation Order
1. **Start with tests** - Implement capability unit tests first to validate design
2. **Easiest file first** - Migrate TemporalQueries (clean interface, clear scope)
3. **Read-only next** - Migrate QueryEngine (no side effects, lower risk)
4. **Complex last** - Migrate SessionMetadataExtractor (transactions, schema setup)

### Success Metrics
- ✅ All capability unit tests pass
- ✅ Zero CapabilityErrors in production (all configs correct)
- ✅ Performance within 5% of baseline
- ✅ Clear error messages on violations
- ✅ 100% test coverage for capability enforcement

### Risk Mitigation
- Migrate one file at a time
- Keep tests passing at each step
- Log all capability violations (audit trail)
- Gradual rollout with monitoring

---

## Files Created

1. `/Users/bln/play/agentic-primer/simplify/CAPABILITY_DESIGN.md` (900+ lines)
2. `/Users/bln/play/agentic-primer/simplify/src/messaging/capabilities/storage.ts` (300+ lines)
3. `/Users/bln/play/agentic-primer/simplify/src/messaging/capabilities/filesystem.ts` (250+ lines)
4. `/Users/bln/play/agentic-primer/simplify/src/messaging/capabilities/factory.ts` (150+ lines)
5. `/Users/bln/play/agentic-primer/simplify/examples/capability-configurations.ts` (400+ lines)
6. `/Users/bln/play/agentic-primer/simplify/CAPABILITY_MIGRATION_PLAN.md` (600+ lines)
7. `/Users/bln/play/agentic-primer/simplify/PHASE_2_COMPLETION_SUMMARY.md` (this file)

**Total Lines:** ~2,600 lines of design, implementation sketches, and documentation

---

## Design Questions Answered

### 1. How do capabilities check scope?
✅ **Answer:** Capabilities extract tables/paths from operations and validate against allowedTables/allowedPaths before routing to system actors. Violations throw CapabilityError.

### 2. How do capabilities interact with system actors?
✅ **Answer:** Capabilities create messages and route them to `/[namespace]/system/storage` or `/[namespace]/system/fs` addresses. System actors handle actual I/O.

### 3. Should capabilities be reusable across actors or per-instance?
✅ **Answer:** Per-instance for better auditability. Each actor gets its own capability instances configured at construction time.

### 4. How to handle capability violations?
✅ **Answer:** Throw CapabilityError (custom error type) with clear message. Log violations for audit trail. Don't silently fail.

### 5. What's the actor construction API?
✅ **Answer:** ActorFactory pattern - actors created with explicit capability configuration. Factory attaches capabilities after construction via duck typing.

---

**Phase 2 Design Complete ✅**

Ready to proceed with Phase 1 implementation (unit tests + infrastructure).
