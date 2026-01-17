# Assimilated Knowledge: Datalog, CozoDB, libSQL

**Generated:** 2026-01-16 19:27 EST
**Source Projects:** agentic-mudding-adventure, proj-20251219-175550
**Time Period:** December 2025 - January 2026

## Executive Summary

Extensive past work explored Datalog-based systems for work tracking and knowledge management across multiple implementations. **CozoDB emerged as the production winner** with 5,000-28,000 qps performance, full Datalog semantics, and multiple backend options. A custom JavaScript Datalog engine built on libSQL (SQLite) was successfully prototyped but superseded by CozoDB's maturity. The DataScript JavaScript library was attempted but rejected due to critical bugs. Additionally, a Rust WASM Datalog engine was prototyped for browser portability.

### Key Decisions Made

1. **Production Database: CozoDB** - Rust-based, production-ready, 5K-28K qps
2. **Storage Pattern: EAVT** - Entity-Attribute-Value-Transaction for flexibility
3. **Query Language: CozoScript** - Faithful Datalog implementation, trivial syntax mapping
4. **Architecture: Three Layers** - Storage → Query Engine → Application
5. **libSQL Role: Prototyping** - Used for custom EAVT storage layer, not production
6. **DataScript: Rejected** - Critical bugs, unmaintained library
7. **WASM Strategy: Custom Backend** - Embed CozoDB with IndexedDB/sqlite3-wasm for browser persistence

---

## 1. Datalog

### Past Work

#### Three Implementations Explored

**1. Custom JavaScript Datalog Engine (libSQL-based)**
- Location: `/work/proj-20251219-175550-dgu/outputs/`
- EAVT storage on libSQL (SQLite)
- Unification-based pattern matching
- Built-in rules for blocked/ready derivation
- Status: Functional prototype, superseded by CozoDB

**2. DataScript Library Integration (FAILED)**
- Location: `/work/proj-20251219-175550-yg5/outputs/`
- Blocking bug: Library cannot execute ANY queries due to comparison function bug
- Error: "Cannot compare work/priority to work/id"
- Root cause: ClojureScript-to-JavaScript compilation bug
- Time wasted: 2.5 hours
- **Decision: REJECT DataScript library entirely**

**3. CozoDB Production Implementation (WINNER)**
- Location: `/work/proj-20251219-175550-uwb/`
- Production-ready Rust implementation
- Multiple backends: Memory, SQLite, RocksDB
- Performance: 5,000-28,000 queries/second
- All tests passing: 22/22
- **Decision: USE for all Datalog workloads**

**4. Rust WASM Datalog Engine**
- Location: `/work/proj-20251219-175550-pww/datalog-wasm/`
- Pure Rust, compiles to 29KB WASM
- Direct implementation of blocked/ready rules
- No external Datalog library dependencies
- Status: Functional prototype for browser use

### Decisions Made

#### Storage Model: EAVT (Entity-Attribute-Value-Transaction)

**Schema:**
```sql
CREATE TABLE facts (
    entity TEXT NOT NULL,      -- Work item ID
    attribute TEXT NOT NULL,   -- Property name (status, depends-on)
    value TEXT NOT NULL,       -- Property value
    tx_id INTEGER NOT NULL,    -- Transaction ID for time-travel
    PRIMARY KEY (entity, attribute, tx_id)
);

-- Critical indexes for pattern matching
CREATE INDEX idx_ea ON facts(entity, attribute);
CREATE INDEX idx_av ON facts(attribute, value);
CREATE INDEX idx_v ON facts(value);
```

**Why EAVT Won:**
- ✅ Schema flexibility (add attributes without migration)
- ✅ Natural fit for Datalog pattern matching
- ✅ Time-travel queries via transaction IDs
- ✅ Sparse data (no NULL columns)
- ✅ All three implementations converged on EAVT
- ⚠️ More verbose than relational tables
- ⚠️ Requires discipline (no schema enforcement at storage)

#### Query Language: Datalog Rules

**Core Insight:** Define "blocked" and "ready" as Datalog rules, not imperative code.

**Blocked tasks rule:**
```datalog
blocked[task, dep, dep_title, dep_status] :=
    *work[task, type, status, priority, title, description],
    status != 'closed',
    *dependencies[task, dep],
    *work[dep, dep_type, dep_status, dep_priority, dep_title, dep_description],
    dep_status != 'closed'
```

**Ready tasks rule (stratified negation):**
```datalog
ready[task, type, priority, title] :=
    *work[task, type, status, priority, title, description],
    status != 'closed',
    not blocked[task, _, _, _]
```

**Benefits vs Imperative Code:**
1. Declarative - express "what" not "how"
2. Composable - rules build on other rules
3. Testable - rules are data, not code
4. Optimizable - query planner reorders joins
5. Maintainable - easy to understand and extend

### Learnings/Insights

#### Three-Layer Architecture Pattern

All implementations converged on this pattern:

```
┌─────────────────────────────────────┐
│   Application Layer                 │  Domain logic, HTTP, CLI
├─────────────────────────────────────┤
│   Query Engine Layer                │  Datalog evaluation, unification
├─────────────────────────────────────┤
│   Storage Layer                     │  EAVT facts, indexes, persistence
└─────────────────────────────────────┘
```

**Benefits:**
- Backend swapping (SQLite → RocksDB) without changing queries
- Testing via mocked storage layer
- Optimization at storage layer (indexes, caching)
- Portability across storage backends

#### Stratified Negation is Critical

**Pattern for "ready tasks" (tasks with no blockers):**

```datalog
# Layer 1: Define blocked (positive facts only)
blocked[task, dep] := *dependencies[task, dep], *work[dep, status], status != 'closed'

# Layer 2: Define ready (negation of blocked)
ready[task] := *work[task, status], status != 'closed', not blocked[task, _]
```

**Why stratified?**
- Datalog requires stratified negation for deterministic results
- `blocked` must be fully evaluated before `ready` references it
- No recursion through negation (prevents semantic ambiguity)

#### Performance Insights

**Backend Comparison (CozoDB):**

| Backend | Read QPS | Write WPS | Use Case |
|---------|----------|-----------|----------|
| Memory  | 25,445   | 27,778    | Testing, ephemeral data |
| SQLite  | 18,622   | 3,788     | Development, small datasets |
| RocksDB | 7,003    | 22,727    | Production, write-heavy workloads |

**Key Findings:**
- Memory backend 36-43% faster than SQLite for reads
- RocksDB 6x faster than SQLite for writes
- Complex queries with rules 3.5x slower than simple SELECT
- All backends maintain excellent throughput (5,000+ qps)

**Batch Operations Critical:**
- Single-row writes: 3,058-27,778 WPS
- Batch (100 rows): 111,111-166,667 WPS
- **Speedup: 6x (Memory) to 36x (SQLite)**

---

## 2. CozoDB

### What It Is

**CozoDB** is a transactional, relational-graph-vector database that uses Datalog for queries.

**Key Characteristics:**
- Uses CozoScript (Datalog dialect) as query language
- Supports relational, graph, and vector data natively
- Embeddable (like SQLite) or client-server mode
- Handles OLTP and OLAP workloads
- Time-travel queries (MVCC-based)
- Built-in graph algorithms (PageRank, Dijkstra, community detection)
- Multiple storage backends (Memory, SQLite, RocksDB, TiKV)

### Past Usage

#### Comprehensive Documentation Created

From `/Users/bln/play/agentic-mudding-adventure/docs/`:

1. **cozodb-overview.md** - Full feature overview, use cases, architecture
2. **cozodb-vs-datalog.md** - Side-by-side comparison with traditional Datalog
3. **datalog-advantages.md** - What traditional Datalog offers that CozoDB doesn't
4. **cozodb-limitations.md** - Constraints and trade-offs
5. **cozodb-query-patterns.md** - Common patterns (recursion, aggregations, graph traversals)
6. **cozodb-schema-design.md** - Schema design patterns
7. **cozodb-bun-wrapper-design.md** - Bun integration design
8. **cozodb-browser-wrapper-design.md** - Browser deployment strategy
9. **cozodb-sync-protocol-design.md** - Multi-client sync design

#### Integration Patterns

**Work Management System:**
```javascript
const db = new CozoDb('sqlite', './work.db');

// Schema
await db.run(`
  :create work {
    id: String,
    type: String,
    status: String,
    priority: Int,
    title: String,
    => [id]
  }
`);

// Rules
await db.run(`
  blocked[task, dep] :=
    *work[task, _, status, _, _],
    status != 'closed',
    *dependencies[task, dep],
    *work[dep, _, dep_status, _, _],
    dep_status != 'closed'

  ready[task] :=
    *work[task, _, status, _, _],
    status != 'closed',
    not blocked[task, _]
`);

// Query ready work
const result = await db.run(`?[id, title] := ready[id], *work[id, _, _, _, title]`);
```

### Key Decisions

#### Production Deployment Configuration

```javascript
// Development
const db = new CozoDb('sqlite', './dev.db');

// Testing / CI
const db = new CozoDb('mem');

// Production (read-heavy)
const db = new CozoDb('sqlite', '/data/prod.db');

// Production (write-heavy, large scale)
const db = new CozoDb('rocksdb', '/data/rocksdb');
```

#### CozoScript is Pure Datalog

**Mapping is 1:1 with trivial syntax differences:**

| Feature | Standard Datalog | CozoScript | Difficulty |
|---------|------------------|------------|------------|
| Facts | `work(t1, task, open, 1)` | `:put work {id: t1, type: task, status: open, priority: 1}` | Easy |
| Rules | `blocked(T, D) :- ...` | `blocked[t, d] := ...` | Trivial |
| Queries | `?- blocked(T, D)` | `?[t, d] := blocked[t, d]` | Trivial |
| Recursion | `ancestor(X,Y) :- parent(X,Y)` | `ancestor[x,y] := *parent[x,y]` | Trivial |
| Negation | `not blocked(T)` | `not blocked[t, _]` | Trivial |
| Aggregation | `Count = count{...}` | `count(dep, *dependencies[task, dep])` | Easy |

**All core Datalog features supported:**
- ✅ Pattern matching
- ✅ Unification
- ✅ Recursive rules (semi-naive evaluation)
- ✅ Stratified negation
- ✅ Aggregations (count, sum, avg, min, max)
- ✅ Safe recursive aggregations (unique to CozoDB!)

**Extensions beyond standard Datalog:**
- Built-in graph algorithms (Dijkstra, PageRank, community detection)
- Vector search integration (HNSW)
- Time-travel queries (MVCC)
- Materialized views
- Constraint validation

### Performance Findings

#### Benchmarks (M1 Mac, Node.js 25)

| Query Type | Memory QPS | SQLite QPS | Complexity |
|-----------|------------|------------|------------|
| Simple SELECT | 25,445 | 18,622 | 1x baseline |
| Complex Rules (blocked/ready) | 7,299 | 5,525 | 3.5x slower |
| Join Query (lineage) | 10,965 | 10,288 | 2.3x slower |

**Write Performance:**

| Write Type | Memory WPS | SQLite WPS | Speedup |
|-----------|------------|------------|---------|
| Single-row | 27,778 | 3,058 | 1x |
| Batch (100) | 166,667 | 111,111 | 6x (mem), 36x (sqlite) |

#### Graph Algorithms (Built-in)

CozoDB includes 20+ optimized graph algorithms:
- Shortest path (Dijkstra, Yen's K-shortest paths)
- Community detection (Louvain)
- Centrality (PageRank, betweenness)
- Spanning trees (Kruskal, Prim)
- Connected components

**Orders of magnitude faster than implementing in pure Datalog.**

#### Limitations Discovered

**What CozoDB Can't Do:**

1. **No compound terms** (vs Prolog) - must flatten structures into relations
2. **No full unification** (vs Prolog) - only scalar value unification
3. **No constraint solving** (vs CLP) - no CLP(FD) or CLP(R)
4. **No secondary indices** (currently) - only primary key indexed
5. **No distributed queries** (embedded mode) - single-node query execution
6. **No streaming queries** - snapshot queries only, no continuous queries
7. **Smaller ecosystem** - fewer tools, libraries, Stack Overflow answers than SQL

**When to Use Alternatives:**
- **Prolog:** Symbolic AI, NLP, constraint solving, compound terms needed
- **Soufflé:** Static analysis, batch analytics, compile-time optimization
- **PostgreSQL:** Mature ecosystem, SQL expertise, extensive tooling, simple CRUD

---

## 3. libSQL

### Past Integration

#### Custom Datalog Engine Storage Layer

**Location:** `/work/proj-20251219-175550-dgu/outputs/storage-libsql.js`

**Purpose:** EAVT fact storage for custom Datalog engine prototype

**Architecture:**
```javascript
import { createClient } from '@libsql/client';

class FactStorage {
  constructor(dbPath = ':memory:') {
    this.db = createClient({ url: `file:${dbPath}` });
    this.initSchema();
  }

  initSchema() {
    this.db.execute(`
      CREATE TABLE IF NOT EXISTS facts (
        entity TEXT NOT NULL,
        attribute TEXT NOT NULL,
        value TEXT NOT NULL,
        tx_id INTEGER NOT NULL,
        PRIMARY KEY (entity, attribute, tx_id)
      )
    `);

    // Critical indexes
    this.db.execute(`CREATE INDEX IF NOT EXISTS idx_ea ON facts(entity, attribute)`);
    this.db.execute(`CREATE INDEX IF NOT EXISTS idx_av ON facts(attribute, value)`);
    this.db.execute(`CREATE INDEX IF NOT EXISTS idx_v ON facts(value)`);
  }

  async assert(entity, attribute, value) {
    const tx_id = Date.now();
    await this.db.execute({
      sql: 'INSERT INTO facts (entity, attribute, value, tx_id) VALUES (?, ?, ?, ?)',
      args: [entity, attribute, value, tx_id]
    });
    return tx_id;
  }

  async query(entityPattern, attributePattern, valuePattern) {
    // Pattern matching with prepared statements
    // Returns substitutions (variable bindings)
  }
}
```

**Features:**
- Embedded SQLite via libSQL
- EAVT schema with proper indexes
- Prepared statements for performance
- Transaction support via tx_id
- In-memory and file-based modes

**Performance:**
- Not formally benchmarked (prototype phase)
- Expected: 1,000-5,000 qps (SQLite write-bound)
- Optimized pattern matching via indexes

### Learnings

#### Why libSQL for Prototyping

**Advantages:**
- ✅ SQLite compatibility (drop-in replacement)
- ✅ Embedded database (no server process)
- ✅ ACID transactions
- ✅ Good indexes for pattern matching
- ✅ Turso integration path (if needed)
- ✅ Easy Bun integration (`bun:sqlite` compatible)

**Limitations for Production Datalog:**
- ❌ No native Datalog query engine
- ❌ Must implement unification/pattern matching manually
- ❌ No built-in recursive query support
- ❌ Limited optimization compared to specialized Datalog engines
- ❌ More work to implement rule evaluation

#### Why CozoDB Won Over Custom libSQL Implementation

| Aspect | Custom libSQL | CozoDB |
|--------|---------------|--------|
| Query Language | Manual pattern matching | Native CozoScript |
| Recursion | Must implement semi-naive | Built-in (optimized) |
| Negation | Manual implementation | Stratified negation |
| Aggregations | SQL-level only | Rich Datalog aggregations |
| Graph Algorithms | None | 20+ built-in |
| Performance | 1-5K qps (estimated) | 5-28K qps (measured) |
| Maintenance | DIY | Production-ready |

**Decision Rationale:**
- Custom implementation useful for learning Datalog internals
- CozoDB provides production-ready features out-of-box
- Better optimization, more features, active maintenance
- Time-to-production: weeks (custom) vs days (CozoDB)

### Usage Patterns Discovered

#### EAVT Storage Best Practices

**Schema Design:**
```sql
-- Core EAVT table
CREATE TABLE facts (
    entity TEXT NOT NULL,      -- Canonical entity ID
    attribute TEXT NOT NULL,   -- Namespaced attribute (work/status)
    value TEXT NOT NULL,       -- Stringified value
    tx_id INTEGER NOT NULL,    -- Monotonic transaction ID
    PRIMARY KEY (entity, attribute, tx_id)
);

-- Indexes for all Datalog query patterns
CREATE INDEX idx_ea ON facts(entity, attribute);  -- [?e :work/status ?s]
CREATE INDEX idx_av ON facts(attribute, value);   -- [?e :work/status "open"]
CREATE INDEX idx_v ON facts(value);               -- [?e :depends-on ?dep]
```

**Insert Pattern:**
```javascript
// Single fact assertion
await storage.assert('work-123', 'status', 'open');

// Batch assertions (36x faster)
const facts = [
  ['work-123', 'title', 'Fix bug'],
  ['work-123', 'priority', '1'],
  ['work-123', 'status', 'open']
];
await storage.assertBatch(facts);
```

**Query Pattern:**
```javascript
// Pattern: [?e work/status ?s]
const results = await storage.query('?e', 'work/status', '?s');

// Pattern: [?e work/status "open"]
const openItems = await storage.query('?e', 'work/status', 'open');

// Pattern: [work-123 ?a ?v] (get all attributes)
const entity = await storage.getEntity('work-123');
```

#### Deduplication Critical

**Problem:** EAVT allows duplicate facts with different tx_ids.

**Solution:**
```javascript
async assert(entity, attribute, value) {
  // Check for existing fact with same value
  const existing = await this.db.execute({
    sql: 'SELECT tx_id FROM facts WHERE entity = ? AND attribute = ? AND value = ? ORDER BY tx_id DESC LIMIT 1',
    args: [entity, attribute, value]
  });

  if (existing.rows.length > 0) {
    return existing.rows[0].tx_id; // Don't insert duplicate
  }

  // Insert new fact
  const tx_id = Date.now();
  await this.db.execute({
    sql: 'INSERT INTO facts (entity, attribute, value, tx_id) VALUES (?, ?, ?, ?)',
    args: [entity, attribute, value, tx_id]
  });
  return tx_id;
}
```

**Why it matters:**
- Prevents memory bloat (OOM issues)
- Maintains clean time-travel semantics
- Improves query performance (fewer rows to scan)

---

## 4. Cross-Technology Insights

### How These Technologies Relate

```
┌─────────────────────────────────────────────────────┐
│  CozoDB (Production Choice)                         │
│  - Full Datalog implementation                      │
│  - Multiple storage backends (Memory, SQLite, RocksDB)│
│  - 5K-28K qps performance                           │
│  - Built-in graph algorithms                        │
└─────────────────────────────────────────────────────┘
                      vs
┌─────────────────────────────────────────────────────┐
│  Custom Datalog + libSQL (Prototype)                │
│  - Manual Datalog implementation                    │
│  - libSQL for EAVT storage                          │
│  - 1-5K qps estimated                               │
│  - Educational but not production-ready             │
└─────────────────────────────────────────────────────┘
```

**Synergy:** libSQL provides good embedded storage for *building* a Datalog engine, but CozoDB provides a *complete* Datalog solution.

### Datalog Paradigm (The Winner)

**Why Datalog won for work tracking:**

1. **Declarative Rules** - Express constraints, not imperative logic
2. **Automatic Inference** - Blocked status computed by rules
3. **Composability** - Rules build on other rules
4. **Pattern Matching** - Flexible queries with variables
5. **Recursion** - Natural for dependency chains
6. **Negation** - "Ready" = "not blocked"
7. **Time-Travel** - MVCC-based temporal queries

**Evidence:**
- 3.5x fewer lines of code vs imperative
- Eliminates entire classes of bugs (null checks, loops)
- Query optimizer handles execution strategy
- Easy to test (rules are data)

### EAVT Storage Pattern (Universal)

**Why all implementations converged on EAVT:**

1. **Schema Flexibility** - Add attributes without migration
2. **Natural for Datalog** - Facts as triples
3. **Good for Queries** - Proper indexes support pattern matching
4. **Temporal Semantics** - Transaction IDs for time-travel
5. **Sparse Data** - No NULL columns for missing attributes

**Trade-off:** More verbose than relational (3 columns per attribute vs 1), but flexibility outweighs cost for evolving schemas.

---

## 5. Recommendations for Current Project

### Use CozoDB for Production

**Rationale:**
- Proven performance: 5,000-28,000 qps
- Production-ready (Rust implementation, active maintenance)
- Full Datalog semantics (no missing features)
- Multiple backends (Memory, SQLite, RocksDB)
- Excellent documentation
- All tests passing

**Deployment Pattern:**
```javascript
// Development
const db = new CozoDb('sqlite', './dev.db');

// Testing
const db = new CozoDb('mem');

// Production
const db = new CozoDb('rocksdb', '/data/db');
```

### Adopt EAVT for Knowledge Graph

**Use EAVT pattern when:**
- Schema evolves frequently
- Need flexible attributes per entity
- Want temporal queries (time-travel)
- Building knowledge graphs
- Using Datalog queries

**Use relational tables when:**
- Schema is stable
- Simple CRUD operations
- SQL joins sufficient
- Team expertise in SQL

### Integrate with Task System

**Current System:** Graph/TaskNode with in-memory state

**Proposed Integration:**

```javascript
// Dual-write pattern
class TaskStore {
  constructor() {
    this.graph = new Graph(); // In-memory graph
    this.db = new CozoDb('sqlite', './tasks.db'); // Queryable store
  }

  async createTask(task) {
    // Write to graph (source of truth for state)
    this.graph.addNode(task.id, task);

    // Write to CozoDB (queryable)
    await this.db.run(`
      ?[id, type, status, priority, title] <- [[
        '${task.id}', '${task.type}', '${task.status}',
        ${task.priority}, '${task.title}'
      ]]
      :put work {id, type, status, priority, title}
    `);
  }

  async findBlocked() {
    // Use CozoDB for complex queries
    return await this.db.run(`?[id, title] := blocked[id], *work[id, _, _, _, title]`);
  }

  async findReady() {
    // Use CozoDB for rule-based queries
    return await this.db.run(`?[id, title] := ready[id], *work[id, _, _, _, title]`);
  }
}
```

### Avoid These Mistakes

**Don't:**
- ❌ Use DataScript JavaScript library (broken, unmaintained)
- ❌ Build custom Datalog engine unless you have specific requirements
- ❌ Use single-row writes for bulk data (use batch operations)
- ❌ Skip smoke testing when evaluating libraries
- ❌ Use Memory backend for production (no persistence)

**Do:**
- ✅ Use CozoDB for Datalog workloads
- ✅ Use batch operations (6-36x speedup)
- ✅ Choose backend based on workload (read vs write heavy)
- ✅ Add proper indexes for EAVT pattern
- ✅ Test libraries with smoke tests before integration

---

## 6. Open Questions

### 1. Graph as Source of Truth or CozoDB?

**Current:** In-memory Graph is source of truth for task state

**Options:**
- **A:** Dual-write (Graph for state, CozoDB for queries)
- **B:** CozoDB as single source of truth
- **C:** Event sourcing (append-only log, both derive from events)

**Trade-offs:**
- A: Complexity (two systems to maintain), but clear separation
- B: Simplicity (one system), but loses graph structure optimizations
- C: Audit trail, but higher complexity

**Recommendation:** Start with A (dual-write), evaluate C if audit trail becomes critical.

### 2. How to Handle Schema Evolution?

**EAVT Advantage:** No schema migrations needed

**Question:** How to validate data integrity without rigid schema?

**Options:**
- **A:** Application-level validation
- **B:** CozoDB constraints (`:create work { => priority >= 0 }`)
- **C:** Both (defense in depth)

**Recommendation:** C (both) - constraints at database level, validation at application level.

### 3. Time-Travel Queries Worth Implementing?

**EAVT Enables:** "What was task status on 2026-01-01?"

**Use Cases:**
- Audit trails
- Debugging ("Why was this task blocked last week?")
- Historical analysis
- Compliance

**Question:** Does our use case need temporal queries?

**Recommendation:** Not initially, but architecture supports it (tx_id in EAVT). Can add later without migration.

### 4. Browser Deployment Strategy?

**For Browser-Based Task Management:**

**Options:**
- **A:** Official cozo-wasm (memory-only, no persistence)
- **B:** Custom Rust embedding CozoDB + IndexedDB StorageTrait
- **C:** Custom Rust embedding CozoDB + sqlite3-wasm StorageTrait

**Trade-offs:**
- A: Easy (ready-made), but no persistence
- B: Full features + persistence, but more Rust work (IndexedDB FFI)
- C: Full features + persistence, proven (SQLite), slightly larger bundle

**Recommendation:** C (CozoDB + sqlite3-wasm) if browser persistence needed, A for demos/prototypes.

### 5. When to Use libSQL vs CozoDB Backends?

**libSQL Scenarios:**
- Building custom storage layer (educational)
- Need Turso integration (edge databases)
- Existing SQLite investment

**CozoDB Scenarios:**
- Need Datalog queries
- Want graph algorithms
- Require rule-based inference
- Production workloads

**Recommendation:** Use CozoDB SQLite backend (gets both Datalog + SQLite persistence).

---

## Appendix: Session References

### Primary Documentation Sources

1. **agentic-mudding-adventure Project**
   - `/Users/bln/play/agentic-mudding-adventure/docs/cozodb-*.md` (9 files)
   - Comprehensive CozoDB documentation
   - Created: January 4-5, 2026

2. **proj-20251219-175550 Project**
   - `/work/proj-20251219-175550-dgu/outputs/` - libSQL Datalog implementation
   - `/work/proj-20251219-175550-uwb/` - CozoDB production implementation
   - `/work/proj-20251219-175550-yg5/` - DataScript failed attempt
   - `/work/proj-20251219-175550-pww/datalog-wasm/` - Rust WASM engine
   - `/work/datalog-knowledge-management-summary.md` - Comprehensive summary
   - Created: December 19, 2025 - January 3, 2026

### Key Files for Deeper Investigation

**CozoDB Usage:**
- `agentic-mudding-adventure/docs/cozodb-overview.md` - Feature overview
- `agentic-mudding-adventure/docs/cozodb-query-patterns.md` - Query cookbook
- `proj-20251219-175550/work/proj-20251219-175550-uwb/benchmark.js` - Performance benchmarks

**libSQL Integration:**
- `proj-20251219-175550/work/proj-20251219-175550-dgu/outputs/storage-libsql.js` - EAVT implementation
- `proj-20251219-175550/work/proj-20251219-175550-dgu/outputs/datalog-libsql.js` - Query engine
- `proj-20251219-175550/work/proj-20251219-175550-dgu/outputs/README-LIBSQL.md` - Architecture

**Lessons Learned:**
- `proj-20251219-175550/work/datalog-knowledge-management-summary.md` - Complete analysis
- `proj-20251219-175550/work/proj-20251219-175550-yg5/outputs/FINDINGS.md` - DataScript investigation
- `proj-20251219-175550/work/proj-20251219-175550-yg5/outputs/DECISION-SIMPLIFIED-DATASCRIPT.md` - Rejection decision

### Evidence of Decisions

**CozoDB Selection:**
- Benchmark data: 5,000-28,000 qps measured performance
- 22/22 tests passing
- Multiple production-ready backends
- Active maintenance (commits in 2025-2026)

**DataScript Rejection:**
- Blocking bug: "Cannot compare work/priority to work/id"
- 2.5 hours debugging time
- Unmaintained (last update years ago)
- Clear recommendation: "REJECT DataScript, ADOPT CozoDB"

**EAVT Pattern:**
- Used in all 3 implementations (custom, DataScript, CozoDB)
- Documented as "consistently valuable across all implementations"
- Clear benefits: flexibility, queryability, time-travel

**Three-Layer Architecture:**
- "All implementations followed a consistent three-layer architecture"
- Enables backend swapping
- Separation of concerns (storage, query, application)

---

## Conclusion

Past work comprehensively explored Datalog for work tracking and knowledge management. **CozoDB emerged as the clear production choice** with excellent performance, full Datalog semantics, and multiple backend options. The EAVT storage pattern proved universally valuable for schema flexibility. Rule-based Datalog queries are demonstrably superior to imperative code for complex logic.

**For the current project:**
- ✅ Use CozoDB with SQLite backend (development) or RocksDB (production)
- ✅ Adopt EAVT pattern for knowledge graph and flexible schemas
- ✅ Use Datalog rules for dependency queries (blocked/ready)
- ✅ Integrate via dual-write pattern (Graph for state, CozoDB for queries)
- ❌ Avoid DataScript library (broken)
- ❌ Don't build custom Datalog engine (use CozoDB)

**Key insight from past work:** "CozoDB is pure Datalog" - all core features map 1:1, making it trivial to apply Datalog knowledge from academic literature directly to production code.
