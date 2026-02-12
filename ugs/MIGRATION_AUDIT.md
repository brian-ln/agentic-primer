# Migration Audit: src/messaging/ to @agentic-primer/actors

**Generated:** 2026-02-11
**Purpose:** Identify redundant files in `simplify/src/messaging/` that are superseded by the `@agentic-primer/actors` package

---

## Executive Summary

The `@agentic-primer/actors` package now contains portable, production-ready implementations of core actor system components. Many files in `simplify/src/messaging/` are redundant or partially redundant.

**Key Findings:**
- **35 files** actively import from `simplify/src/messaging/`
- **Core infrastructure** (actor, message, router, channels) exists in both locations
- **Simplify versions** are generally larger and more feature-rich but less portable
- **Package versions** are streamlined and platform-agnostic

---

## File-by-File Analysis

### Core Actor System Files

| File | Package Equivalent | Status | Size Comparison | Decision |
|------|-------------------|--------|-----------------|----------|
| `actor.ts` | `@agentic-primer/actors/actor.ts` | Superseded | 1013 vs 236 lines | **KEEP** (extends package with GraphStore integration) |
| `message.ts` | `@agentic-primer/actors/message.ts` | Superseded | 218 vs 165 lines | **DELETE** (package version is complete) |
| `router.ts` | `@agentic-primer/actors/router.ts` | Superseded | 1041 vs 281 lines | **KEEP** (has GraphStore integration) |
| `supervisor-base.ts` | `@agentic-primer/actors/supervisor.ts` | Superseded | 339 vs 153 lines | **DELETE** (package version is sufficient) |
| `index.ts` | `@agentic-primer/actors/index.ts` | Superseded | N/A | **MIGRATE** (update to re-export from package) |

### Channel Implementations

| File | Package Equivalent | Status | Import Count | Decision |
|------|-------------------|--------|--------------|----------|
| `channel.ts` | `@agentic-primer/actors/channels/channel.ts` | Superseded | 0 | **DELETE** (nearly identical) |
| `channels/stream.ts` | `@agentic-primer/actors/channels/stream.ts` | Superseded | 0 | **DELETE** (minor import path diffs only) |
| `channels/port.ts` | `@agentic-primer/actors/channels/port.ts` | Superseded | 0 | **DELETE** (redundant) |
| `channels/bridge.ts` | `@agentic-primer/actors/channels/bridge.ts` | Superseded | 0 | **DELETE** (redundant) |
| `channels/index.ts` | `@agentic-primer/actors/channels/index.ts` | Superseded | 1 (internal) | **DELETE** (redundant) |
| `channels/ChannelActor.ts` | **NOT IN PACKAGE** | Unique | 2 | **KEEP** (future work, messaging platform adapters) |

### Routing & Path Resolution

| File | Package Equivalent | Status | Import Count | Decision |
|------|-------------------|--------|--------------|----------|
| `path-resolver.ts` | `@agentic-primer/actors/routing/path-resolver.ts` | Superseded | 0 | **DELETE** (package has streamlined version) |
| `path-patterns.ts` | `@agentic-primer/actors/routing/path-patterns.ts` | Superseded | 0 | **DELETE** (redundant) |
| `path-cache.ts` | `@agentic-primer/actors/routing/path-cache.ts` | Superseded | 0 | **DELETE** (redundant) |
| `address-parser.ts` | `@agentic-primer/actors/routing/address-parser.ts` | Superseded | 0 | **DELETE** (redundant) |
| `alias-resolver.ts` | **NOT IN PACKAGE** | Unique | 4 | **KEEP** (GraphStore-specific, commented out in refactor.ts) |

### Supervision

| File | Package Equivalent | Status | Import Count | Decision |
|------|-------------------|--------|--------------|----------|
| `supervision/types.ts` | `@agentic-primer/actors/supervision/types.ts` | Superseded | 1 | **DELETE** (package version is sufficient) |
| `supervision/strategies.ts` | `@agentic-primer/actors/supervision/strategies.ts` | Superseded | 1 | **DELETE** (package has streamlined version) |

### Browser-Specific Code

| File | Package Equivalent | Status | Import Count | Decision |
|------|-------------------|--------|--------------|----------|
| `browser/actor-registry.ts` | **NOT IN PACKAGE** | Unique | 1 test | **KEEP** (browser-specific) |
| `browser/bridges.ts` | **NOT IN PACKAGE** | Unique | 1 test | **KEEP** (browser-specific) |
| `browser/widget-actor.ts` | **NOT IN PACKAGE** | Unique | 1 test | **KEEP** (browser-specific) |

### Capabilities

| File | Package Equivalent | Status | Import Count | Decision |
|------|-------------------|--------|--------------|----------|
| `capabilities/factory.ts` | **NOT IN PACKAGE** | Unique | 0 | **EVALUATE** (may be unused) |
| `capabilities/filesystem.ts` | **NOT IN PACKAGE** | Unique | 0 | **EVALUATE** (may be unused) |
| `capabilities/storage.ts` | **NOT IN PACKAGE** | Unique | 0 | **EVALUATE** (may be unused) |

### Actor Implementations (Domain-Specific)

All files in `actors/` subdirectory are **KEEP** - they are domain-specific implementations:

- `actors/compute/*` - Code execution actors (subprocess, worker)
- `actors/inference.ts` - LLM inference actor
- `actors/knowledge.ts` - Knowledge graph actor
- `actors/query-executor.ts` - Query DSL executor
- `actors/program-executor.ts` - Program execution actor
- `actors/relationship.ts` - Graph relationship actor
- `actors/session.ts` - Session management actor
- `actors/task.ts` - Task management actor
- `actors/tool.ts` - Tool invocation actor
- `actors/workflow-*.ts` - Workflow orchestration actors
- `actors/logger.ts` - Logging actor
- `actors/filesystem.ts` - Filesystem operations actor

### Proof-of-Concept / Experimental

| File | Decision |
|------|----------|
| `hierarchical-routing-poc.ts` | **DELETE** (POC, functionality in package) |

### Tests

All test files in `__tests__/` directory: **EVALUATE PER FILE**

- Tests for deleted files should be deleted
- Tests for package functionality should reference package tests
- Integration tests may need to be updated to import from package

### Benchmarks

All files in `benchmarks/` directory: **KEEP** (useful for performance tracking)

---

## Import Analysis

### Files Actively Importing from `src/messaging/`

**Count:** 35 files import from messaging (79 total import occurrences)

#### System Actors (6 files)
- `system-actors/filesystem.ts` - imports Actor, MessageRouter
- `system-actors/http-client.ts` - imports Actor, MessageRouter
- `system-actors/scheduler.ts` - imports Actor, MessageRouter
- `system-actors/signal-hub-bridge.ts` - imports Actor, MessageRouter
- `system-actors/storage.ts` - imports Actor, MessageRouter
- `system-actors/websocket.ts` - imports Actor, MessageRouter

#### Query System (8 files)
- `query/live-demo-reactive-messaging.ts`
- `query/examples-phase3.ts`
- `query/integration-demo.ts`
- `query/compiler.ts.bak`
- `query/reactive/*.ts` (4 files)

#### Storage (1 file)
- `storage/LibSQLKnowledgeStore.ts` - imports KnowledgeItem, Relationship types

#### Tests (20 files)
- Various test files across system-actors, query, and messaging

---

## Detailed Migration Strategy

### Phase 1: Delete Redundant Core Files (SAFE)

These files are fully superseded by the package and not imported anywhere:

```bash
# Core routing (package has complete implementations)
rm simplify/src/messaging/path-resolver.ts
rm simplify/src/messaging/path-patterns.ts
rm simplify/src/messaging/path-cache.ts
rm simplify/src/messaging/address-parser.ts

# Channels (package has complete implementations)
rm simplify/src/messaging/channel.ts
rm simplify/src/messaging/channels/stream.ts
rm simplify/src/messaging/channels/port.ts
rm simplify/src/messaging/channels/bridge.ts
rm simplify/src/messaging/channels/index.ts

# Supervision (package has complete implementations)
rm simplify/src/messaging/supervision/types.ts
rm simplify/src/messaging/supervision/strategies.ts

# Message primitives (package version is complete)
rm simplify/src/messaging/message.ts

# Supervisor base (package version is sufficient)
rm simplify/src/messaging/supervisor-base.ts

# POC files
rm simplify/src/messaging/hierarchical-routing-poc.ts
```

### Phase 2: Update Imports in Active Files

#### Update system-actors (6 files)

**Before:**
```typescript
import { Actor } from '../messaging/actor.ts';
import type { MessageRouter } from '../messaging/router.ts';
```

**After:**
```typescript
import { Actor } from '../messaging/actor.ts'; // Still extends package Actor
import type { MessageRouter } from '../messaging/router.ts'; // GraphStore-aware
```

**Note:** These files can continue using `simplify/src/messaging/actor.ts` since it extends the package Actor with GraphStore integration. However, consider updating to:

```typescript
import { Actor as BaseActor } from '@agentic-primer/actors';
// Use BaseActor directly or extend with minimal GraphStore adapter
```

#### Update storage/LibSQLKnowledgeStore.ts

**Current:**
```typescript
import type { KnowledgeItem, EpistemicLevel, EvidenceLink } from '../messaging/actors/knowledge.ts';
import type { Relationship, RelationshipType } from '../messaging/actors/relationship.ts';
```

**After:**
```typescript
// Keep as-is (domain types, not in package)
```

### Phase 3: Migrate actor.ts and router.ts

These files have GraphStore integration that should be decoupled:

**Option A:** Minimal wrapper
```typescript
// simplify/src/messaging/actor.ts
export { Actor } from '@agentic-primer/actors';
// Add GraphStore-specific extensions as needed
```

**Option B:** Adapter pattern
```typescript
// simplify/src/messaging/graph-actor.ts
import { Actor as BaseActor } from '@agentic-primer/actors';

export class GraphActor extends BaseActor {
  // GraphStore-specific functionality
}
```

**Recommendation:** Use Option B to make the GraphStore dependency explicit.

### Phase 4: Update index.ts

**Current:** Re-exports from local files
**Target:** Re-export from package + local extensions

```typescript
// simplify/src/messaging/index.ts
export * from '@agentic-primer/actors';

// Local extensions only
export * from './channels/ChannelActor.ts';
export * from './alias-resolver.ts';
export * from './graph-actor.ts'; // GraphStore-specific actor
```

### Phase 5: Clean Up Tests

1. Delete tests for deleted files:
   - `__tests__/path-*.test.ts` (covered by package tests)
   - `__tests__/supervision-strategies.test.ts` (covered by package tests)
   - `__tests__/channels.test.ts` (covered by package tests)
   - `__tests__/message.test.ts` (covered by package tests)

2. Update integration tests to import from package

3. Keep unique tests:
   - `__tests__/channel-actor.test.ts` (ChannelActor not in package)
   - `__tests__/alias-resolution.test.ts` (alias-resolver not in package)
   - Integration tests using GraphStore

---

## Risk Assessment

### Low Risk (Safe to Delete Immediately)

- Path resolution files (path-resolver, path-patterns, path-cache, address-parser)
- Channel implementations (channel, stream, port, bridge, channels/index)
- Supervision files (types, strategies)
- message.ts
- supervisor-base.ts

**Why:** Package versions are complete, no active imports, functionality is equivalent.

### Medium Risk (Requires Import Updates)

- actor.ts (imported by 6 system-actors)
- router.ts (imported by 6+ files)

**Why:** Need to update imports, but package provides equivalent functionality.

### No Risk (Keep)

- channels/ChannelActor.ts (unique, future work)
- alias-resolver.ts (GraphStore-specific)
- browser/* (browser-specific)
- actors/* (domain implementations)
- benchmarks/* (performance tracking)

---

## Files Safe to Delete (Immediate Action)

### Core Files (11 files)
1. `message.ts`
2. `supervisor-base.ts`
3. `hierarchical-routing-poc.ts`
4. `channel.ts`

### Routing (4 files)
5. `path-resolver.ts`
6. `path-patterns.ts`
7. `path-cache.ts`
8. `address-parser.ts`

### Channels (4 files)
9. `channels/stream.ts`
10. `channels/port.ts`
11. `channels/bridge.ts`
12. `channels/index.ts`

### Supervision (2 files)
13. `supervision/types.ts`
14. `supervision/strategies.ts`

### Tests for Deleted Files (8 files)
15. `__tests__/message.test.ts`
16. `__tests__/supervisor-base.test.ts`
17. `__tests__/channels.test.ts`
18. `__tests__/path-resolver.test.ts`
19. `__tests__/path-patterns.test.ts`
20. `__tests__/path-cache.test.ts`
21. `__tests__/path-cache-integration.test.ts`
22. `__tests__/path-cache-benchmark.test.ts`
23. `__tests__/path-performance.test.ts`
24. `__tests__/supervision-strategies.test.ts`

**Total:** 24 files can be safely deleted immediately

---

## Imports to Rewire

### Pattern 1: System Actors (6 files)

**Files:**
- `system-actors/filesystem.ts`
- `system-actors/http-client.ts`
- `system-actors/scheduler.ts`
- `system-actors/signal-hub-bridge.ts`
- `system-actors/storage.ts`
- `system-actors/websocket.ts`

**Current:**
```typescript
import { Actor } from '../messaging/actor.ts';
import type { MessageRouter } from '../messaging/router.ts';
```

**Strategy:** Keep as-is until actor.ts/router.ts are refactored (Phase 3)

### Pattern 2: Query System

**Files:** 8 files in query/

**Current:** Various imports from messaging
**Strategy:** Will automatically work once messaging/index.ts is updated to re-export from package

### Pattern 3: Tests

**Strategy:** Update import paths from `../messaging/` to `@agentic-primer/actors` where applicable

---

## Unique Features to Consider Migrating

### 1. ChannelActor (channels/ChannelActor.ts)

**Status:** Not in package
**Size:** 648 lines
**Purpose:** Standard interface for messaging platform adapters (WhatsApp, Telegram, Discord, Slack)

**Recommendation:** Keep in simplify for now. Consider moving to package if we build a multi-platform messaging system.

**Features:**
- Connection management (connect, disconnect, reconnect)
- Authentication flow with QR codes
- Message sending/receiving
- Typing indicators, read receipts
- Media handling
- Contact/group management
- Supervision integration

### 2. alias-resolver.ts

**Status:** Not in package
**Size:** ~200 lines
**Purpose:** Graph-based alias resolution for path addressing

**Recommendation:** Keep in simplify (GraphStore-specific). Package uses simpler flat addressing.

**Features:**
- Graph-stored aliases
- Priority-based resolution
- Context injection via metadata
- Queryable relationships

### 3. GraphStore Integration (actor.ts, router.ts)

**Status:** Larger implementations with GraphStore coupling
**Purpose:** Integrate actor system with UGS graph database

**Recommendation:** Decouple via adapter pattern (see Phase 3)

---

## Next Steps

1. **Immediate:** Delete 24 redundant files (see "Files Safe to Delete" section)
2. **Short-term:** Update tests to import from package where applicable
3. **Medium-term:** Refactor actor.ts and router.ts to use package + adapter
4. **Long-term:** Evaluate if capabilities/* are used (may be dead code)

---

## Notes

- **Package Version:** `@agentic-primer/actors` is production-ready and platform-agnostic
- **Simplify Version:** Larger, more feature-rich, but tightly coupled to GraphStore
- **Migration Goal:** Use package for core functionality, keep only GraphStore-specific extensions
- **Test Coverage:** Package has comprehensive tests, can safely delete redundant simplify tests

---

## Appendix: Diff Summary

### File Size Comparison

| Component | Simplify (lines) | Package (lines) | Difference |
|-----------|------------------|-----------------|------------|
| actor.ts | 1013 | 236 | +777 (GraphStore integration) |
| message.ts | 218 | 165 | +53 (minor additions) |
| router.ts | 1041 | 281 | +760 (GraphStore integration) |
| supervisor-base.ts | 339 | 153 | +186 (extra docs/features) |
| supervision/types.ts | 635 | 146 | +489 (extensive docs) |
| supervision/strategies.ts | 428 | 217 | +211 (extra utilities) |
| channel.ts | 159 | 162 | -3 (nearly identical) |

**Insight:** Simplify files are generally 2-3x larger due to:
1. GraphStore integration
2. More extensive documentation
3. Additional features not needed for portable package
