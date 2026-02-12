# Session Knowledge System - Specification

**Date:** 2026-01-30
**Status:** ✅ Implemented (Phases 1-4 Complete)
**Related Beads:**
- `session-knowledge-core` (architecture)
- `session-knowledge-incremental` (implementation)
- `session-knowledge-embeddings` (semantic search)

---

## Problem Statement

### Current Pain Points

**User context:** Power user running dozens of parallel sessions daily

**Failures:**
1. **Cross-session amnesia** - "What did we decide about auth in Session 2?" → No way to know
2. **Lost knowledge** - Dozens of sessions/day, knowledge evaporates after session ends
3. **Repeated work** - Solving same problems in parallel sessions without reuse
4. **No search** - Can't find where we discussed something across 60+ sessions/week
5. **Context limits** - Long sessions hit context window limits, lose early conversations

**Real usage pattern:**
```
12 sessions/day
5 days/week
= 240 sessions/month
= Gigabytes of conversation data
= Completely unsearchable
```

**Time wasted:**
- 5 cross-session questions/day
- 10 minutes hunting manually each
- = 4 hours/week wasted on knowledge retrieval

---

## Solution Overview

**Build a knowledge system that:**
1. **Indexes** all sessions automatically
2. **Searches** across sessions semantically
3. **Processes incrementally** as sessions run
4. **Enables** cross-session knowledge reuse

**Core capability:**
```bash
# From any session, ask about any other session
/search "authentication approach"
→ Finds discussion from Session 2, 3 days ago
→ Loads context
→ Agent can reference previous work
```

---

## Architecture

### Three-Tier Storage

```
┌─────────────────────────────────────┐
│  TIER 1: Index (Always in memory)  │
│  - Session summaries                │
│  - Metadata (dates, files, tools)  │
│  - Fast filters                     │
│  Size: ~10 MB for 1000 sessions    │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│  TIER 2: Session Detail (On-demand)│
│  - Full conversations               │
│  - Tool executions                  │
│  - Agent results                    │
│  Size: ~190 KB per session         │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│  TIER 3: Raw Logs (Stream rarely)  │
│  - Complete JSONL events            │
│  - File snapshots                   │
│  - Debugging data                   │
│  Size: ~500 KB - 5 MB per session  │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐
│  SEARCH: Embeddings (Disk-based)    │
│  - Vector DB for semantic search    │
│  - SQLite FTS for keyword search    │
│  Size: ~36-72 MB for 1000 sessions │
└─────────────────────────────────────┘
```

### Data Flow

```
Session in progress:
  ↓
File watcher detects new event
  ↓
[Tier 1] Update stats (cost, files, tools)
  ↓
[Tier 2] Queue for embedding (background)
  ↓
[Tier 3] Event logged to JSONL
  ↓
Periodic: Extract decisions, learnings
  ↓
On query: Search index + embeddings
```

---

## Core Components (Beads)

### Bead: session-knowledge-core

**Purpose:** Core indexing and search infrastructure

**Components:**
1. **SessionMetadataExtractor** - Scans session logs, extracts metadata to database
2. **SessionAnalyzer** - Extracts metadata (files, tools, cost)
3. **QueryEngine** - Fast lookups by time, file, cost
4. **SessionCache** - LRU cache for recently accessed sessions

**Storage:**
```sql
-- sessions table
CREATE TABLE sessions (
  id TEXT PRIMARY KEY,
  created INTEGER,
  modified INTEGER,
  summary TEXT,
  message_count INTEGER,
  cost REAL,
  duration INTEGER,
  content_hash TEXT  -- For incremental updates
);

-- session_files table
CREATE TABLE session_files (
  session_id TEXT,
  file_path TEXT
);

-- session_tools table
CREATE TABLE session_tools (
  session_id TEXT,
  tool_name TEXT,
  count INTEGER
);

-- Full-text search
CREATE VIRTUAL TABLE sessions_fts USING fts5(
  session_id,
  summary
);
```

**Performance targets:**
- Index build: <2 min for 1000 sessions
- Query by date: <5ms
- Query by file: <10ms
- Search summary: <20ms

---

### Bead: session-knowledge-incremental

**Purpose:** Real-time processing of active sessions

**Motivation:**
- Without watching: Must embed all messages at end (60s delay)
- With watching: Embed incrementally (instant queries)

**Components:**
1. **FileWatcher** - Monitors active session JSONL files
2. **EventProcessor** - Parses new events as they arrive
3. **EmbeddingQueue** - Batches and rate-limits embedding API calls
4. **MultiSessionWatcher** - Tracks all active sessions in project

**Processing tiers:**

```typescript
// Tier 1: Programmatic (always, <5ms)
- Cost tracking
- File modification tracking
- Tool usage stats
- Error detection

// Tier 2: Periodic (every 10min, ~$0.01)
- Session summaries
- Context compression

// Tier 3: Event-triggered (~$0.005 each)
- Decision extraction (when "decided" appears)
- Learning capture (when "learned" appears)
- Error analysis (when tool fails)

// Tier 4: Daily analysis (~$0.50)
- Cross-session patterns
- Duplicate work detection
- Productivity insights
```

**Cost targets:**
- Per session: ~$0.46 (200 messages, 6 hours)
- Per day (12 sessions): ~$6.00
- ROI: Saves 4 hours/week vs. $6/day cost

---

### Bead: session-knowledge-embeddings

**Purpose:** Semantic search across all sessions

**Motivation:**
- Keyword search: "authentication" misses "login", "user verification"
- Semantic search: Finds all related concepts
- Cross-session: "How did we handle X?" finds similar work

**Components:**
1. **EmbeddingGenerator** - Calls LM Studio / OpenAI API
2. **VectorStoreLibSQL** - libSQL native F32_BLOB vectors with DiskANN
3. **SemanticSearch** - Cosine similarity search
4. **SessionEmbeddingIndexer** - Batch processes sessions for embedding

**Architecture Decision: sqlite-vec vs libsql**
- **Chosen:** sqlite-vec extension
- **Rationale:**
  - Compatible with existing bun:sqlite setup
  - Ultra-portable (30KB, no dependencies, MIT/Apache-2.0)
  - SIMD-accelerated performance (AVX/NEON)
  - Sufficient scale (10K+ vectors full scan, 100K+ with indexing)
  - No vendor lock-in (standard SQLite)
- **Not chosen:** libsql (Turso's fork with native vectors)
  - Pros: Zero setup, native F32_BLOB type, built-in ANN indexing
  - Cons: Incompatible with bun:sqlite, requires migration, vendor-specific
  - Use case: Better for managed/cloud deployments, multi-tenant apps

**What to embed:**
```typescript
✅ Embed:
- User messages (questions, requests)
- Assistant text responses
- Session summaries
- Agent results
- Extracted decisions
- Extracted learnings

❌ Don't embed:
- Tool results (code dumps)
- Progress events
- File snapshots
- Metadata
```

**Performance:**
- Generate embedding: ~150ms (API call)
- Search 1000 sessions: ~5ms (HNSW index)
- Batch 25 messages: ~300ms vs 3750ms individual

**Storage:**
- Per message: 6 KB (1536 floats × 4 bytes)
- Compressed: 1.5 KB (int8 quantization)
- 1000 sessions × 50 messages: ~72 MB

---

## Key Design Decisions

### Decision 1: On-Demand Read vs. File Watching

**Reasoning trail:**
1. Initially proposed: Simple on-demand reading
2. User revealed: Dozens of parallel sessions daily
3. **Insight:** With embeddings, watching is essential
4. **Why:** Embedding at end = 60s delay, watching = instant

**Decision:** Watch active files, embed incrementally

**Rationale:**
- Amortizes embedding cost over session
- Enables instant semantic queries
- Better UX (no waiting)
- Same total cost, better timing

**Trade-off:** Complexity (background processes) vs. UX (instant search)

---

### Decision 2: Three-Tier vs. Single Storage

**Reasoning trail:**
1. Could store everything in one format
2. User needs: Fast filters (by date/file) + Semantic search
3. **Insight:** Different query patterns need different indexes
4. **Why:** Time-based = SQL index, Semantic = vector index

**Decision:** Three-tier architecture (Index, Detail, Raw)

**Rationale:**
- Most queries hit lightweight index (10 MB, <5ms)
- Detailed queries load on-demand (190 KB, ~50ms)
- Debugging uses raw logs (streamed)
- Each tier optimized for its access pattern

**Trade-off:** Complexity vs. Performance

---

### Decision 3: Incremental Processing Tiers

**Reasoning trail:**
1. Could process everything at session end
2. User asks: "What else could we process incrementally?"
3. **Analysis:** Different processing has different cost/value
4. **Insight:** Tier by cost and value

**Decision:** Four-tier incremental processing

**Tiers:**
```
Tier 1: Programmatic (free, always)
Tier 2: Periodic summaries ($0.01/10min)
Tier 3: Event-triggered ($0.005 each)
Tier 4: Daily analysis ($0.50/day)
```

**Rationale:**
- Free operations: Do always (stats, tracking)
- Cheap operations: Do selectively (decisions, learnings)
- Expensive operations: Do rarely (daily analysis)
- User controls cost/value trade-off

**Trade-off:** Cost vs. Insight depth

---

### Decision 4: Content Hash vs. mtime for Changes

**Reasoning trail:**
1. Need incremental index updates (not full rebuild)
2. Could use file modification time (mtime)
3. **Problem discovered:** Files get "touched" without content changing
4. **Evidence:** Session from Jan 26, mtime Jan 29, content unchanged

**Decision:** Use MD5 content hash for change detection

**Rationale:**
- mtime unreliable (file systems, backups touch files)
- Content hash: Only detects real changes
- Cost: Hash 1MB file = ~10ms (acceptable)

**Trade-off:** 10ms hashing cost vs. false-positive reindexing

---

### Decision 5: Bun for File I/O

**Reasoning trail:**
1. Node.js fs.readFile is standard
2. Bun claims 2x faster file I/O
3. **Measured:** 1MB file: Bun 8.7ms vs. Node ~18ms
4. **Impact:** Dozens of files read per query

**Decision:** Use Bun for all file operations

**Performance data:**
```
Read 1MB file:     8.7ms
Parse JSONL:       4.7ms
Search:            2.5ms
Total:            16ms (feels instant)
```

**Rationale:**
- 2x faster file I/O matters at scale
- Simple read is fast enough (no streaming needed)
- 30-second cache makes subsequent queries <1ms

---

## Query Examples

### Basic Queries (Tier 1 Index)

```bash
# What did we do yesterday?
/sessions --yesterday
→ 5ms (index lookup)

# What sessions modified this file?
/sessions --file src/auth.ts
→ 10ms (index join)

# How much did I spend this week?
/cost --week
→ 5ms (index aggregate)

# What tools do we use most?
/stats --tools
→ 5ms (index group-by)
```

### Semantic Queries (Embeddings)

```bash
# Where did we talk about authentication?
/search "authentication security login"
→ 200ms (embed query + vector search)
→ Finds: "user verification", "JWT tokens", "session management"

# What have we learned about caching?
/search "caching strategies optimization" --type learning
→ Returns extracted learnings from all sessions

# How did we fix that bug?
/search "login timeout error fix"
→ Finds similar bug from 3 days ago with solution
```

### Cross-Session Queries

```bash
# In Session B, reference Session A
/recall --session <id> "the authentication approach"
→ Loads context from Session A into current session

# Find sessions similar to current work
/similar
→ Uses current session embedding to find related sessions
→ "Session X from yesterday covered similar ground"
```

### Incremental Processing Results

```bash
# Real-time stats
/stats
→ Cost: $2.34
→ Files: src/auth.ts, src/session.ts
→ Cache hit: 87%

# Decisions made
/decisions
→ "11:23 - Use JWT (vs session cookies)"
→ "11:45 - Redis for storage (vs in-memory)"

# Learnings captured
/learnings
→ "Cache hit rate improved with better prompts"
→ "Parallel agents saved 40% time"

# Daily insights
/daily
→ "12 sessions today, $12.50 total"
→ "Pattern: 3x validation logic - extract to module?"
→ "Most productive: 10am-12pm"
```

---

## Implementation Phases

### Phase 1: Core Index (2 hours)
**Bead:** `session-knowledge-core.1`

- Build SessionMetadataExtractor
- libSQL schema with native vectors
- Basic query CLI
- Test on existing sessions

**Deliverable:** `/sessions`, `/search` (keyword only)

---

### Phase 2: File Watching (2 hours)
**Bead:** `session-knowledge-incremental.1`

- Implement FileWatcher
- EventProcessor
- Tier 1 programmatic processing
- Test with active session

**Deliverable:** Real-time stats, file tracking

---

### Phase 3: Embeddings (2 hours)
**Bead:** `session-knowledge-embeddings.1`

- EmbeddingGenerator
- VectorStore setup
- Batch processing
- Incremental embedding queue

**Deliverable:** `/search` (semantic)

---

### Phase 4: Incremental Processing (2 hours)
**Bead:** `session-knowledge-incremental.2`

- Decision detector
- Learning extractor
- Error analyzer
- Periodic summarizer

**Deliverable:** `/decisions`, `/learnings`, structured knowledge

---

### Phase 5: Multi-Session (1 hour)
**Bead:** `session-knowledge-incremental.3`

- MultiSessionWatcher
- Cross-session search
- Session similarity

**Deliverable:** `/similar`, full cross-session queries

---

## Success Metrics

### Performance
- Index query: <10ms ✓
- Semantic search: <200ms ✓
- Session load: <50ms ✓
- Real-time embedding: Background, no user wait ✓

### Cost
- Per session: <$0.50 ✓
- Per day (12 sessions): <$6 ✓
- ROI: Save 4 hours/week >> $6/day ✓

### User Experience
- Find any past discussion: <5 seconds
- Cross-session reference: Instant
- Current session searchable: Real-time
- No manual log hunting: Eliminated

---

## Future Extensions

### Extension 1: Agent Integration
**Bead:** `session-knowledge-agents`

Agents can query knowledge base:
```typescript
// Agent asks about previous work
const similar = await knowledge.search("how we handled validation");
// Agent proposes reusing approach
```

### Extension 2: UGS Native Storage
**Bead:** `session-knowledge-ugs-migration`

Migrate from SQLite to UGS `information` nodes:
```typescript
// Session becomes UGS node
const session = await ugs.create({
  type: "information",
  infoType: "session-log",
  properties: { summary, cost, files }
});

// Decisions become linked information nodes
const decision = await ugs.create({
  type: "information",
  infoType: "decision",
  links: [session]
});
```

### Extension 3: Live Dashboard
**Bead:** `session-knowledge-dashboard`

Real-time view of all active sessions:
- Current sessions running
- Live cost tracking
- Recent decisions/learnings
- Productivity patterns

---

## Dependencies

**Required:**
- Bun runtime
- SQLite
- OpenAI API key (for embeddings)

**Optional:**
- ChromaDB (better vector search)
- sqlite-vec extension (vector search in SQLite)

**Related beads:**
- `agentic-primer-64k` - Actor runtime (eventual integration)
- `agentic-primer-737` - Capability security (API key management)

---

## Open Questions

1. **Retention policy:** How long to keep session embeddings? (Proposal: 90 days)
2. **Privacy:** What about sensitive info in logs? (Proposal: User-defined redaction patterns)
3. **Multi-project:** How to handle queries across projects? (Proposal: Project scoping)
4. **Sharing:** Can knowledge be shared across users? (Future: Team knowledge base)

---

## Conclusion

**This system transforms session logs from write-only archives into a queryable knowledge base.**

**Key insight:** The user's workflow (dozens of parallel sessions) makes this essential infrastructure, not a nice-to-have feature.

**Next step:** Create beads for each component and start implementation.
