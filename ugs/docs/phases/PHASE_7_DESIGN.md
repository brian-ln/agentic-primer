# Phase 7: Path Patterns & Aliases - Design Document

> **Status:** DESIGN PHASE
> **Created:** 2026-02-06
> **Phase:** 7 (Advanced Features)
> **Dependencies:** Phase 5-6 Complete (paths-only mode, path caching)

## Executive Summary

Phase 7 completes the path-based addressing system with **advanced pattern matching** and **alias resolution**. This enables dynamic actor discovery, flexible addressing, and semantic routing.

**Key Features:**
1. **Pattern Matching** - Wildcards (`*`, `**`), alternatives (`{a,b}`)
2. **Alias Resolution** - Multiple paths point to same actor
3. **Graph Integration** - Aliases stored as relationships
4. **Query DSL** - Path-aware query methods

**Deliverables:**
- Production-ready pattern matching (beyond POC)
- Graph-based alias resolution
- Enhanced query DSL
- Comprehensive tests

---

## Current State Assessment

### What Exists (Phase 5-6 Complete)

**✅ Hierarchical Routing:**
- `src/messaging/path-resolver.ts` - Path parsing, validation
- `src/messaging/router.ts` - Hierarchical routing through supervision tree
- `src/messaging/path-cache.ts` - LRU cache for resolved paths

**✅ Basic Pattern Matching:**
- `matchPattern()` in `path-resolver.ts` - Supports `*` and `**`
- POC-quality implementation
- Needs production hardening

**❌ Missing (Phase 7 Scope):**
- **Production-grade patterns** - Error handling, edge cases, performance
- **Alternatives** - `{a,b}` syntax not implemented
- **Alias resolution** - No graph-based alias storage/lookup
- **Query integration** - No path pattern filters in query DSL

### What Works Well

1. **Performance** - Path caching effective (>80% hit rate measured in POC)
2. **Architecture** - Clean separation: resolver, router, cache
3. **Tests** - Good coverage for basic path routing
4. **Documentation** - Design doc comprehensive, examples clear

### What Needs Work

1. **Pattern matching robustness** - Edge cases, invalid patterns, error messages
2. **Alias storage** - No graph relationships yet
3. **Query DSL** - No path-specific helpers
4. **Integration tests** - Pattern + alias + query end-to-end tests needed

---

## Phase 7 Architecture

### 7.1: Enhanced Pattern Matching

**Goal:** Production-ready pattern matching with comprehensive error handling.

**Features:**
- **Wildcards:** `*` (one segment), `**` (any segments)
- **Alternatives:** `{a,b,c}` matches any of the alternatives
- **Validation:** Reject invalid patterns with clear errors
- **Performance:** <5ms for 1000 node graph

**Implementation Strategy:**

```typescript
// Current (POC):
export function matchPattern(path: string, pattern: string): boolean {
  const pathSegments = parsePath(path);
  const patternSegments = parsePath(pattern);
  return matchSegments(pathSegments, patternSegments);
}

// Enhanced (Phase 7):
export function matchPattern(
  path: string,
  pattern: string,
  options?: { caseSensitive?: boolean }
): boolean {
  // 1. Validate pattern syntax
  validatePattern(pattern);

  // 2. Expand alternatives: {a,b} → [a, b]
  const expandedPatterns = expandAlternatives(pattern);

  // 3. Match against each expanded pattern
  for (const expandedPattern of expandedPatterns) {
    if (matchSegments(parsePath(path), parsePath(expandedPattern), options)) {
      return true;
    }
  }

  return false;
}
```

**Files to Create:**
- `src/messaging/path-patterns.ts` - Pattern validation, expansion, matching
- `src/messaging/__tests__/path-patterns.test.ts` - Comprehensive pattern tests

**Files to Modify:**
- `src/messaging/path-resolver.ts` - Use enhanced pattern matching

**Success Criteria:**
- `*` matches one segment: `domain/*` → `domain/inference` ✓, `domain/a/b` ✗
- `**` matches any depth: `domain/**` → `domain/a`, `domain/a/b`, `domain/a/b/c` ✓
- `{a,b}` alternatives: `domain/{inference,executor}` → `domain/inference` ✓, `domain/executor` ✓
- Invalid patterns rejected: `{a,b` (unclosed brace), `***` (invalid wildcard)
- Performance: <5ms per 1000 nodes

---

### 7.2: Graph-Based Alias Resolution

**Goal:** Store and resolve aliases as graph relationships.

**Design Decision:** Use graph relationships over config files.

**Rationale:**
- **Dynamic:** Add/remove aliases at runtime
- **Queryable:** Find all aliases, reverse lookups
- **Versioned:** Track alias changes over time
- **Integrated:** Leverage existing graph infrastructure

**Graph Schema:**

```typescript
// Alias Node (represents an alias)
{
  id: 'alias-services-llm',
  type: 'alias',
  properties: {
    alias_path: 'services/llm',        // Alias path
    canonical_path: 'domain/inference', // Canonical target
    priority: 1,                        // Resolution priority (higher = preferred)
    description: 'LLM service alias',
    created: 1707230400000,
    context: {                          // Context injection metadata
      role: 'llm-service',
      version: 'stable'
    }
  }
}

// Relationship: Alias → Actor
{
  from: 'alias-services-llm',
  to: 'actor-inference',
  type: 'resolves_to',
  properties: {
    created: 1707230400000
  }
}
```

**Resolution Algorithm:**

```typescript
async function resolveAlias(path: string): Promise<ResolvedPath> {
  // 1. Check if path is an alias (query graph)
  const aliasNode = await graph.query({
    match: pattern('alias').where({ alias_path: path }),
    return: ['alias.canonical_path', 'alias.context', 'alias.priority']
  });

  if (!aliasNode) {
    // Not an alias, return original path
    return { path, context: {} };
  }

  // 2. Return canonical path with injected context
  return {
    path: aliasNode.canonical_path,
    context: aliasNode.context || {},
    priority: aliasNode.priority || 0
  };
}
```

**Alias API:**

```typescript
class AliasResolver {
  /** Create an alias */
  async createAlias(
    aliasPath: string,
    canonicalPath: string,
    options?: {
      priority?: number;
      context?: Record<string, any>;
      description?: string;
    }
  ): Promise<void>;

  /** Resolve alias to canonical path */
  async resolve(path: string): Promise<ResolvedPath>;

  /** List all aliases */
  async listAliases(): Promise<Alias[]>;

  /** Delete an alias */
  async deleteAlias(aliasPath: string): Promise<boolean>;

  /** Find aliases pointing to canonical path (reverse lookup) */
  async findAliasesFor(canonicalPath: string): Promise<Alias[]>;
}
```

**Files to Create:**
- `src/messaging/alias-resolver.ts` - Alias management and resolution
- `src/messaging/__tests__/alias-resolution.test.ts` - Alias tests

**Files to Modify:**
- `src/messaging/router.ts` - Resolve aliases before routing
- `src/graph.ts` - Add `alias` node type, `resolves_to` relationship

**Success Criteria:**
- Aliases resolve to canonical paths correctly
- Multiple aliases can point to same actor
- Alias resolution <5ms per lookup (cached)
- Context injection works (metadata from alias)
- Reverse lookups work (find all aliases for actor)

---

### 7.3: Query DSL Enhancements

**Goal:** Add path-specific helpers to query DSL.

**New Query Methods:**

```typescript
// Path pattern matching
query()
  .matchPath('workflows/*/tasks')      // Pattern in match
  .wherePathPrefix('workflows/build-') // Prefix filter
  .returnPaths()                       // Return path strings

// Alias resolution
query()
  .matchAlias('services/llm')          // Match by alias path
  .resolveCanonicalPath()              // Follow resolves_to
  .returnWithContext()                 // Include alias context

// Hierarchical queries
query()
  .matchPath('domain/**')              // Recursive match
  .groupByParentPath()                 // Group by parent
  .aggregate({ count: 'children' })    // Count children
```

**Implementation:**

```typescript
// Add to QueryBuilder
class QueryBuilder {
  matchPath(pattern: string): this {
    // Convert pattern to SQL LIKE/GLOB
    const sqlPattern = convertPatternToSQL(pattern);
    this.addWhere(`path GLOB '${sqlPattern}'`);
    return this;
  }

  wherePathPrefix(prefix: string): this {
    this.addWhere(`path LIKE '${prefix}%'`);
    return this;
  }

  matchAlias(aliasPath: string): this {
    // Join with alias nodes
    this.addMatch(pattern('alias').where({ alias_path: aliasPath }));
    return this;
  }

  resolveCanonicalPath(): this {
    // Follow resolves_to relationship
    this.addFollow('resolves_to');
    return this;
  }
}
```

**Files to Modify:**
- `src/query/builder.ts` - Add path query methods
- `src/query/types.ts` - Add PathQueryOptions type

**Files to Create:**
- `src/query/__tests__/path-queries.test.ts` - Path query tests

**Success Criteria:**
- Path pattern queries compile to correct SQL
- Alias resolution queries work end-to-end
- Performance matches existing query layer
- Documentation includes examples

---

## Implementation Phases

### Phase 7.1: Pattern Matching (2 days)

**Bead:** `simplify-8e8` (already exists)

**Tasks:**
1. Extract pattern matching to `path-patterns.ts`
2. Implement alternatives (`{a,b}`)
3. Add pattern validation
4. Write comprehensive tests
5. Performance benchmarks

**Success Gate:**
- All tests pass
- Performance: <5ms per 1000 nodes
- Pattern validation catches invalid patterns

---

### Phase 7.2: Alias Resolution (2 days)

**Bead:** `simplify-alias` (already exists)

**Tasks:**
1. Design alias graph schema
2. Implement `AliasResolver` class
3. Integrate with router (resolve before routing)
4. Add graph storage (alias nodes, resolves_to edges)
5. Write tests (create, resolve, delete, reverse lookup)

**Success Gate:**
- Aliases resolve correctly
- Multiple aliases to same actor work
- Performance: <5ms per lookup (cached)
- All tests pass

---

### Phase 7.3: Query DSL (1 day)

**Bead:** `simplify-dsl` (already exists)

**Tasks:**
1. Add `matchPath()` to QueryBuilder
2. Add `matchAlias()` to QueryBuilder
3. Add hierarchical query helpers
4. Write tests
5. Update documentation with examples

**Success Gate:**
- All query methods work
- SQL compilation correct
- Tests pass
- Examples in docs work

---

## Testing Strategy

### Unit Tests

**Pattern Matching:**
- Valid patterns: `*`, `**`, `{a,b}`, nested patterns
- Invalid patterns: unclosed braces, invalid wildcards
- Edge cases: empty patterns, single segment, deep hierarchies

**Alias Resolution:**
- Create, resolve, delete aliases
- Multiple aliases to same actor
- Priority resolution (highest priority wins)
- Context injection
- Reverse lookups

**Query DSL:**
- Pattern compilation to SQL
- Alias resolution queries
- Hierarchical aggregation

### Integration Tests

**End-to-End:**
- Create alias → resolve → route message
- Query by pattern → resolve actors → send messages
- Pattern + alias + cache integration

### Performance Tests

**Benchmarks:**
- Pattern matching: 1000 paths against complex patterns (<5ms)
- Alias resolution: 1000 lookups with cache (>80% hit rate)
- Query performance: Path filters on 10,000 node graph (<10ms)

---

## Risk Assessment

### Risk 1: Pattern Complexity
**Risk:** Complex patterns (nested alternatives, deep recursion) cause performance issues
**Mitigation:**
- Limit pattern complexity (max 5 alternatives, max 10 segments)
- Early validation rejects overly complex patterns
- Performance tests catch regressions

### Risk 2: Alias Cycles
**Risk:** Alias A → B → A creates infinite loops
**Mitigation:**
- Cycle detection in alias resolution (track visited paths)
- Max resolution depth limit (10 hops)
- Graph validation on alias creation (reject if creates cycle)

### Risk 3: Cache Invalidation
**Risk:** Alias changes don't invalidate path cache
**Mitigation:**
- Invalidate cache when alias created/deleted
- Use `invalidatePrefix()` for subtree invalidation
- Test cache + alias integration

---

## Success Metrics

### Functional
- ✅ Pattern matching: `*`, `**`, `{a,b}` work correctly
- ✅ Aliases resolve to canonical paths
- ✅ Query DSL supports path patterns
- ✅ All integration tests pass

### Performance
- ✅ Pattern matching: <5ms per 1000 nodes
- ✅ Alias resolution: <5ms per lookup (cached)
- ✅ Cache hit rate: >80% for hot paths
- ✅ Query latency: <10ms with path filters

### Quality
- ✅ Test coverage: >90% for new code
- ✅ Documentation complete with examples
- ✅ No regression in existing tests
- ✅ Code review approved

---

## Open Questions & Decisions

### Q1: Pattern Syntax Extensions?
**Question:** Support character classes (`[a-z]`) or ranges (`task-[1-10]`)?
**Decision:** DEFER to Phase 8. Current scope sufficient.
**Rationale:** YAGNI - no use cases yet, adds complexity

### Q2: Alias Priority Conflicts?
**Question:** How to handle multiple aliases with same priority?
**Decision:** Use creation timestamp (oldest wins)
**Rationale:** Deterministic, simple, predictable

### Q3: Case Sensitivity?
**Question:** Should path patterns be case-sensitive?
**Decision:** YES (case-sensitive by default)
**Rationale:** Matches filesystem behavior, prevents ambiguity
**Note:** Add `caseSensitive: false` option if needed later

### Q4: Alias Namespacing?
**Question:** Should aliases be scoped (per-tenant, per-domain)?
**Decision:** DEFER to Phase 8 (multi-tenancy)
**Rationale:** Global aliases sufficient for MVP

---

## Dependencies & Blockers

### Dependencies (Must Complete Before Phase 7)
- ✅ Phase 5: Hierarchical routing implemented
- ✅ Phase 6: Path caching working
- ✅ Graph store: Node/relationship storage

### Blockers (None)
- No external dependencies
- No breaking changes to existing APIs
- Can develop in parallel with other work

---

## References

- [PATH_ADDRESSING_DESIGN.md](docs/PATH_ADDRESSING_DESIGN.md) - Overall design
- [PHASE_5_7_IMPLEMENTATION_PLAN.md](docs-archive/plans/PHASE_5_7_IMPLEMENTATION_PLAN.md) - Implementation plan
- `src/messaging/path-resolver.ts` - Current implementation
- `src/messaging/router.ts` - Router integration point
- `src/query/builder.ts` - Query DSL

---

**Document Version:** 1.0
**Last Updated:** 2026-02-06
**Authors:** Claude Sonnet 4.5 (Background Agent)
**Status:** Design Complete - Ready for Implementation
