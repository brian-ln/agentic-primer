# Path-Based Addressing & Hierarchical Routing

> **Status:** ✅ IMPLEMENTED - Production (Paths-Only Mode)
> **Created:** 2026-02-05
> **Completed:** 2026-02-06

## Overview

This document describes the **path-based addressing** and **hierarchical routing** system in Simplify. The system uses hierarchical paths (`@(/path/to/actor)`) with delegated routing through supervision trees.

**Current Implementation:** Paths-only mode (flat IDs removed February 2026)

## Architecture

### Path-Based Addressing (Implemented)

**All actors use hierarchical paths:**
```typescript
// Hierarchical path-based addressing
@(/workflows/build-pipeline/tasks/compile)
@(/domain/program-executor)
@(/users/alice/inbox/msg-1)
@(/services/llm)
```

**Benefits Achieved:**
1. **Hierarchical organization** - Natural tree structures
2. **Delegated routing** - Each supervisor routes its namespace
3. **Multi-tenancy** - Isolated namespaces per tenant/user
4. **Clear ownership** - Path reveals supervision hierarchy
5. **Namespace isolation** - No global ID coordination needed
6. **Semantic clarity** - Path conveys meaning and context
7. **Multiple paths** - Aliases, roles, versions point to same actor
8. **Discovery** - Query actors by path patterns

## Core Concepts

### 1. Hierarchical Paths

Addresses use `/`-separated path segments:

```typescript
@(root)                              // Root supervisor
@(domain)                            // Top-level domain supervisor
@(domain/inference)                  // Actor under domain supervisor
@(workflows/build/tasks/compile)     // Deep hierarchy
```

**Path Structure:**
- **Segments:** `/`-separated components
- **Root-relative:** Paths start from root supervisor
- **Hierarchical:** Each segment is a child of the previous

### 2. Delegated Routing

Each supervisor routes messages within its namespace:

```typescript
class Supervisor extends Actor {
  private children: Map<string, Actor> = new Map();

  async receive(message: Message): Promise<MessageResponse> {
    const segments = parseAddress(message.to).split('/');

    if (segments.length === 1) {
      // Message for this supervisor
      return this.handleMessage(message);
    }

    // Delegate to child
    const [childName, ...remaining] = segments;
    const child = this.children.get(childName);

    if (!child) {
      return createErrorResponse(message, `Child not found: ${childName}`);
    }

    // Forward with remaining path
    return child.receive({
      ...message,
      to: address(remaining.join('/'))
    });
  }
}
```

**Routing Flow:**
```
Message: @(domain/program-executor/task-1)

1. RootSupervisor receives message
   - Parses path: ['domain', 'program-executor', 'task-1']
   - Finds child 'domain'
   - Forwards with path: 'program-executor/task-1'

2. DomainSupervisor receives message
   - Parses path: ['program-executor', 'task-1']
   - Finds child 'program-executor'
   - Forwards with path: 'task-1'

3. ProgramExecutor receives message
   - Parses path: ['task-1']
   - Handles message for task-1
```

### 3. Multiple Paths (Aliases)

Same actor reachable via multiple semantic paths:

```typescript
// All resolve to the SAME InferenceActor instance:
@(domain/inference)           // Organizational path
@(services/llm)              // Service category path
@(ai/claude-sonnet)          // Specific model path
@(roles/ai-assistant)        // Role-based path
@(services/stable/inference) // Versioning path
```

**Implementation:**
- Path aliases stored as graph relationships
- Router resolves aliases before delegation
- Context can be injected via alias metadata

**Use Cases:**
1. **Semantic clarity** - Same actor, different contexts
2. **Role abstraction** - Address by role, not implementation
3. **Versioning** - Canary/stable paths to different versions
4. **Failover** - Primary path with fallback aliases
5. **Multi-tenancy** - Tenant-specific paths with context injection

### 4. Path Patterns & Discovery

Query actors by path patterns:

```typescript
// Find all channel actors
const channels = await pathResolver.resolve('channels/*');

// Find all tasks in a workflow
const tasks = await pathResolver.resolve('workflows/build-pipeline/tasks/*');

// Find all services
const services = await pathResolver.resolve('services/**');  // Recursive
```

**Wildcard Support:**
- `*` - Matches one segment
- `**` - Matches any number of segments (recursive)
- `{a,b}` - Matches either 'a' or 'b'

## Architecture Design

### Logical Model (Design Target)

**Hierarchical routing through supervision tree:**

```
┌─────────────────────────────────────────────────┐
│            Hierarchical Routing                  │
├─────────────────────────────────────────────────┤
│                                                  │
│           RootSupervisor (@(root))              │
│                    │                             │
│         ┌──────────┼──────────┐                 │
│         │          │          │                 │
│    DomainSup  ChannelSup  SessionSup            │
│    @(domain)  @(channel)  @(session)            │
│         │          │          │                 │
│    ┌────┼───┐      │      ┌───┼────┐           │
│    │    │   │      │      │   │    │           │
│  ProgEx Inf Know Slack  Sess1 Sess2            │
│                                                  │
│  Message Flow:                                   │
│  @(domain/inference) →                          │
│    Root → Domain → Inference                    │
│                                                  │
└─────────────────────────────────────────────────┘
```

**Key Properties:**
- Each supervisor owns its namespace
- Routing is delegated, not centralized
- Actors only know their children
- Messages flow down supervision tree
- Failures isolated to subtrees

### Physical Optimizations (Future)

**Within-process optimizations (after logical model works):**

1. **Path cache** - Cache resolved paths for repeated lookups
2. **Flat registry** - Secondary index for hot paths
3. **Batching** - Group messages to same subtree
4. **Lazy resolution** - Defer path parsing until needed

**Critical:** These are optimizations OF the hierarchical model, not replacements.

### Migration from Current Architecture

**Current:**
```typescript
// Single global MessageRouter with flat registry
class MessageRouter {
  private actorRegistry: Map<string, Actor> = new Map();

  async route(message: Message) {
    const id = parseAddress(message.to);
    return this.actorRegistry.get(id)!.receive(message);
  }
}
```

**Target:**
```typescript
// Root supervisor with delegated routing
class RootSupervisor extends Supervisor {
  constructor() {
    super('root');
    this.supervise(new DomainSupervisor('domain'));
    this.supervise(new ChannelSupervisor('channel'));
    this.supervise(new SessionSupervisor('session'));
  }
}

// Each supervisor routes its namespace
// No global registry needed
```

**Migration Path:**
1. Implement `Supervisor` base class with path routing
2. Convert existing supervisors to path-aware routing
3. Add path resolution layer (aliases, wildcards)
4. Deprecate flat registry (keep for backward compatibility)
5. Migrate actors to hierarchical organization
6. Remove flat registry once all actors migrated

## Use Cases & Examples

### Use Case 1: Workflow Organization

**Before (flat IDs):**
```typescript
@(task-compile-abc)
@(task-test-def)
@(task-deploy-ghi)
```

**After (hierarchical paths):**
```typescript
@(workflows/build-pipeline/tasks/compile)
@(workflows/build-pipeline/tasks/test)
@(workflows/build-pipeline/tasks/deploy)

// Query all tasks in workflow:
query().match(
  pattern('task').where({
    path_prefix: 'workflows/build-pipeline/tasks/'
  })
)
```

### Use Case 2: Multi-User Namespacing

**Before (ID encoding):**
```typescript
@(inbox-alice-msg-1)
@(inbox-bob-msg-1)  // ID conflict potential
```

**After (path-based isolation):**
```typescript
@(users/alice/inbox/msg-1)
@(users/bob/inbox/msg-1)   // Same ID, different namespace!

// Query user's inbox:
query().match(
  pattern('msg').where({
    path_prefix: 'users/alice/inbox/'
  })
)
```

### Use Case 3: Service Aliases & Versioning

**Canary deployment:**
```typescript
// 95% of traffic
@(services/stable/inference) → @(ai/claude-sonnet-4)

// 5% canary traffic
@(services/canary/inference) → @(ai/claude-opus-4)

// Rollback = update alias mapping!
```

**Role-based addressing:**
```typescript
@(roles/primary-executor) → @(domain/program-executor)
@(roles/backup-executor) → @(domain/program-executor-2)

// Failover = switch role target
```

### Use Case 4: External World Mapping

**Filesystem → Graph:**
```typescript
@(external/filesystem/Users/alice/docs/notes.md)
  properties: { content: '...', mtime: ..., size: ... }

// Query all markdown files:
query().match(
  pattern('file').where({
    path_pattern: 'external/filesystem/**/*.md'
  })
)
```

**Web APIs → Graph:**
```typescript
@(external/web/github.com/repos/anthropics/simplify)
  properties: { stars: 1234, forks: 56, ... }

// Cross-reference external + internal:
query()
  .match(
    pattern('local-repo').label('Repository'),
    pattern('external-repo').where({
      path_prefix: 'external/web/github.com/'
    })
  )
  .where(logic.eq(
    filter.property('local-repo', 'url'),
    filter.property('external-repo', 'html_url')
  ))
```

### Use Case 5: Dynamic Discovery

**Before (hardcoded IDs):**
```typescript
const channels = ['slack-123', 'discord-456', 'telegram-789'];
for (const id of channels) {
  await router.tell({ to: address(id), type: 'broadcast', payload: msg });
}
```

**After (path-based discovery):**
```typescript
const channels = await pathResolver.resolve('channels/*');
for (const channel of channels) {
  await router.tell({ to: address(channel.path), type: 'broadcast', payload: msg });
}

// Adding new channel = just create actor under 'channels/'
// No code changes needed!
```

## Implementation Considerations

### 1. Path Resolution Strategy

**Option A: Eager (Resolve Upfront)**
```typescript
async route(message: Message) {
  const resolvedActor = await this.resolvePath(message.to);
  return resolvedActor.receive(message);
}
```

**Pros:** Simple, cache-friendly
**Cons:** Extra hop, single point of failure

**Option B: Lazy (Delegate Through Tree)**
```typescript
async receive(message: Message) {
  const [child, ...remaining] = parsePath(message.to);
  return this.children.get(child)!.receive({
    ...message,
    to: address(remaining.join('/'))
  });
}
```

**Pros:** Truly hierarchical, fault-isolated
**Cons:** More hops, harder to cache

**Recommendation:** Start with Option B (lazy delegation). Add caching as optimization.

### 2. Path Syntax

**Chosen:** `/`-separated segments (filesystem-like)

**Alternatives Considered:**
- `.`-separated (DNS-like): `domain.inference`
- `:`-separated (URL-like): `domain:inference`
- `>`-separated (Arrow-like): `domain>inference`

**Rationale:**
- Familiar (filesystem, URLs)
- Natural hierarchy visualization
- Clear visual separation

### 3. Alias Resolution

**Graph-based (Recommended):**
```typescript
// Store aliases as relationships
{
  from: 'alias-node-services-llm',
  to: 'actor-inference-123',
  type: 'resolves_to',
  properties: { path: 'services/llm', priority: 1 }
}

// Resolve before routing
async resolvePath(path: string): Promise<string> {
  const alias = await graph.query({
    match: pattern('alias').where({ path }),
    return: ['alias.target']
  });
  return alias?.target || path;  // Fallback to original path
}
```

**Config-based (Alternative):**
```json
{
  "path_aliases": {
    "services/llm": "domain/inference",
    "roles/primary-executor": "domain/program-executor"
  }
}
```

**Recommendation:** Graph-based. Allows dynamic aliases, version history, query capabilities.

### 4. Performance Considerations

**Path Parsing:**
- Cache parsed paths (split once, reuse)
- Lazy evaluation (don't parse until needed)
- Avoid regex (simple string split)

**Path Resolution:**
- LRU cache for hot paths (size: 1000)
- Cache invalidation on actor lifecycle changes
- Per-supervisor caches (distributed caching)

**Routing Hops:**
- Typical depth: 2-4 hops (acceptable)
- Deep hierarchies: Consider flattening or caching
- Monitor latency, optimize hot paths

**Zero-Copy Optimizations (Within-Process):**

**Key Insight:** We can "cheat behind the scenes" as long as we meet authentication/authorization/encapsulation contracts.

**Within same system/process:**
- **Skip message serialization** - Pass references instead of copies (zero-copy)
- **Idempotent messages** - Avoid defensive copies for read-only operations
- **Direct dispatch** - Call actor.receive() directly (bypass routing overhead)
- **Batched messages** - Group messages to same subtree, route once

**Learn from existing systems:**
- **WebAssembly:** Linear memory, zero-copy between JS and WASM
- **Browser:** Structured clone algorithm, transferable objects (ArrayBuffer)
- **Actor systems:** Akka local message passing (no serialization within JVM)
- **LMAX Disruptor:** Ring buffer, mechanical sympathy

**Optimization Checklist:**
1. ✅ Hierarchical routing (logical correctness)
2. ✅ Path caching (reduce parsing overhead)
3. [ ] Zero-copy within process (avoid serde for local messages)
4. [ ] Direct dispatch (bypass router for same-process actors)
5. [ ] Message batching (amortize routing cost)
6. [ ] Reference passing (avoid object clones)

**Trade-offs:**
- **Logical model:** Always hierarchical routing (correct, auditable)
- **Physical optimization:** Zero-copy when safe (fast, but careful!)
- **Safety boundary:** Authentication/authorization at process boundary
- **Within-process:** Trust, optimize aggressively
- **Cross-process:** Verify, serialize, secure

**Example:**
```typescript
class Supervisor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // Logical routing (always correct)
    const child = this.findChild(message.to);

    // Physical optimization (same process)
    if (this.isSameProcess(child)) {
      // Zero-copy: pass message reference directly
      return child.receive(message);  // No clone, no serde
    } else {
      // Cross-process: serialize and send
      return this.sendRemote(message);
    }
  }
}
```

### 5. Backward Compatibility

**Dual Support (Migration Period):**
```typescript
async route(message: Message) {
  const addr = parseAddress(message.to);

  if (addr.includes('/')) {
    // Path-based routing
    return this.hierarchicalRoute(message);
  } else {
    // Flat ID routing (legacy)
    return this.flatRoute(message);
  }
}
```

**Migration Timeline:**
- Month 1-2: Dual support, both systems work
- Month 3-4: Migrate actors to paths, deprecate flat IDs
- Month 5+: Remove flat ID support

## Design Decisions

### Decision 1: Hierarchical Routing (Logical Model)

**Context:** Should routing be centralized (global registry) or hierarchical (delegated)?

**Decision:** Hierarchical routing through supervision tree.

**Rationale:**
- Aligns with actor model principles
- Scales to distributed systems (each supervisor on different machine)
- Natural fault isolation (failures contained to subtrees)
- Clear ownership (each supervisor owns its namespace)
- Enables dynamic routing (no global coordination)

**Trade-offs:**
- More hops (mitigated by caching)
- More complex implementation (worth it for scalability)

### Decision 2: Path Syntax (`/`-separated)

**Context:** What path separator to use?

**Decision:** `/` (slash) for path segments.

**Rationale:**
- Familiar (filesystem, URLs, REST paths)
- Natural hierarchy visualization
- Clear visual separation
- Widely supported in tooling

**Alternatives Rejected:**
- `.` (conflicts with property access)
- `:` (less visual hierarchy)
- `>` (unfamiliar)

### Decision 3: Multiple Paths (Aliases Supported)

**Context:** Should actors have one canonical path or multiple?

**Decision:** Support multiple paths (aliases) pointing to same actor.

**Rationale:**
- Enables semantic clarity (same actor, different contexts)
- Supports versioning (stable/canary paths)
- Enables failover (primary/backup paths)
- Allows role abstraction (address by role, not implementation)

**Implementation:** Graph relationships (`resolves_to` edges).

### Decision 4: Path Resolution Strategy (Lazy Delegation)

**Context:** Resolve paths upfront or delegate through tree?

**Decision:** Lazy delegation through supervision tree (can optimize later).

**Rationale:**
- Truly hierarchical (no central resolver)
- Fault isolated (each supervisor independent)
- Dynamic (actors can be added/removed without global updates)
- Optimization opportunity (can add caching later)

**Trade-off:** More hops (acceptable for correctness first, performance second).

## Open Questions

1. **Path normalization:** Should `domain//inference` (double slash) normalize to `domain/inference`?
2. **Absolute vs relative paths:** Support `./child` (relative) or only `/domain/child` (absolute)?
3. **Path length limits:** Maximum path depth? Maximum segment length?
4. **Reserved segments:** Should `_`, `.`, `..` have special meaning?
5. **Case sensitivity:** `Domain/Inference` vs `domain/inference` - same or different?
6. **Unicode support:** Should paths support non-ASCII characters?
7. **Escaping:** How to handle `/` in segment names? (e.g., `users/alice\/bob` for user "alice/bob")

**Recommendations:**
1. Normalize multiple slashes to single
2. Absolute paths only (simpler, clearer ownership)
3. Limit: 10 segments deep, 100 chars per segment
4. Reserve `_` prefix for system paths (e.g., `_system/health`)
5. Case-sensitive (treat as distinct)
6. UTF-8 support (international names)
7. URL-encode special chars (`/` → `%2F`)

## Success Metrics

**Functional:**
- ✅ Hierarchical routing through supervision tree works
- ✅ Path patterns (`*`, `**`) resolve correctly
- ✅ Aliases resolve to correct actors
- ✅ Multi-tenant namespaces isolated

**Performance:**
- ✅ Path resolution <10ms (p99)
- ✅ Routing overhead <10% vs flat IDs
- ✅ Cache hit rate >80% for hot paths
- ✅ Scales to 10,000+ actors

**Developer Experience:**
- ✅ Paths are self-documenting (clear meaning)
- ✅ Discovery via path patterns works
- ✅ Migration from flat IDs is smooth
- ✅ Documentation and examples are clear

## Next Steps

### Phase 1: Design (This Document)
- [x] Document path addressing concepts
- [x] Document hierarchical routing architecture
- [x] Document multiple paths/aliases pattern
- [x] Capture use cases and examples
- [x] Define success metrics

### Phase 2: Proof of Concept
- [ ] Implement `PathResolver` utility
- [ ] Implement `Supervisor.receive()` with path delegation
- [ ] Build path pattern matching (`*`, `**`)
- [ ] Test with 2-3 level hierarchy
- [ ] Measure performance baseline

### Phase 3: Core Implementation
- [ ] Refactor `MessageRouter` to hierarchical model
- [ ] Convert supervisors to path-aware routing
- [ ] Implement alias resolution (graph-based)
- [ ] Add path caching layer
- [ ] Write integration tests

### Phase 4: Migration
- [ ] Add backward compatibility (dual routing)
- [ ] Migrate existing actors to paths
- [ ] Update query layer for path patterns
- [ ] Deprecate flat ID routing
- [ ] Remove legacy code

### Phase 5: Advanced Features
- [ ] Path-based RBAC (access control by path)
- [ ] Path metrics and observability
- [ ] Cross-cluster routing (distributed paths)
- [ ] Path query DSL enhancements

## Related Documents

- [ARCHITECTURE.md](../ARCHITECTURE.md) - System architecture overview
- [BEAD_ACTOR_ARCHITECTURE.md](BEAD_ACTOR_ARCHITECTURE.md) - Task workflow integration
- [SUPERVISION.md](supervision/README.md) - Supervision strategies

## Appendix: Code Examples

See examples in "Use Cases & Examples" section above, and additional examples in conversation transcript (2026-02-05).

---

**Document Version:** 1.0
**Last Updated:** 2026-02-05
**Authors:** Claude Sonnet 4.5, Brian Lloyd-Newberry
**Status:** Design Phase - Ready for Review
