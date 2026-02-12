# Session Query Architecture - Balancing Memory and Speed

## The Problem

**Your questions require:**
- "What did we do yesterday?" → Filter by time, summarize
- "What have we learned?" → Semantic understanding across sessions
- "Where did we talk about X?" → Full-text + semantic search
- "How much did this cost?" → Aggregate token usage
- "How could we have done this better?" → Pattern analysis, comparison

**Memory constraint:** Can't load 1000+ sessions × 190KB = 190MB+ into memory

**Solution:** Tiered architecture with lazy loading

---

## Architecture: 3-Tier + Search

```
┌─────────────────────────────────────────────────────────────┐
│ TIER 1: Index (Always in memory, ~1-10 MB)                 │
│ - Session summaries                                         │
│ - Time ranges, file lists, agent counts                    │
│ - Quick filters: "yesterday", "modified file X"            │
└─────────────────────────────────────────────────────────────┘
                          ↓ (load on demand)
┌─────────────────────────────────────────────────────────────┐
│ TIER 2: Session Detail (Load when needed, ~100-500 KB)     │
│ - Full conversation                                         │
│ - Tool executions                                           │
│ - Agent results                                             │
└─────────────────────────────────────────────────────────────┘
                          ↓ (load rarely)
┌─────────────────────────────────────────────────────────────┐
│ TIER 3: Raw Logs (Stream when debugging, ~500 KB - 5 MB)   │
│ - Full JSONL with all events                               │
│ - File snapshots                                            │
│ - Hook execution details                                    │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ SEARCH LAYER: Embeddings + Full-text (Disk-based)          │
│ - Vector DB for semantic search                            │
│ - SQLite FTS for keyword search                            │
│ - Links back to sessions                                    │
└─────────────────────────────────────────────────────────────┘
```

---

## Tier 1: Lightweight Index (Always Loaded)

```typescript
// In memory: ~2-10 MB for entire project
class ProjectIndex {
  sessions: Map<string, SessionSummary>;  // All sessions

  // Fast lookups
  byDate: Map<string, string[]>;         // "2026-01-29" → [sessionIds]
  byFile: Map<string, string[]>;         // "src/core.ts" → [sessionIds]
  byAgent: Map<string, string[]>;        // "Explore" → [sessionIds]

  // Aggregates (computed once)
  totalCost: number;
  totalSessions: number;
  dateRange: { start: Date, end: Date };
}

type SessionSummary = {
  id: string;
  created: Date;
  modified: Date;

  // Metadata (small!)
  summary: string;              // AI-generated, ~50-200 chars
  messageCount: number;
  agentCount: number;

  // Lists (compact)
  filesModified: string[];      // Just paths
  toolsUsed: string[];          // Just names
  agentTypes: string[];         // Just types

  // Aggregates
  cost: number;
  duration: number;             // ms

  // For loading tier 2
  filePath: string;
};

// Size: ~200 bytes per session
// 1000 sessions = 200 KB (easily fits in memory)
```

### Query: "What did we do yesterday?"

```typescript
async function whatDidWeDoYesterday() {
  const yesterday = new Date();
  yesterday.setDate(yesterday.getDate() - 1);
  const dateKey = yesterday.toISOString().split('T')[0];

  // O(1) lookup in index (in memory)
  const sessionIds = projectIndex.byDate.get(dateKey) ?? [];

  // Return summaries (already in memory, no loading!)
  return sessionIds.map(id => projectIndex.sessions.get(id));
}

// Result: ~instant, no disk access
// Output: [
//   { summary: "Verified UGS spec accuracy", cost: 0.42, ... },
//   { summary: "Fixed authentication bug", cost: 0.31, ... }
// ]
```

---

## Tier 2: Session Detail (Load on Demand)

```typescript
class SessionAnalyzer {
  // Only loaded when accessed
  private loaded: boolean = false;

  // Summary always available (from Tier 1)
  summary: SessionSummary;

  // Detail loaded lazily
  private _messages?: Map<string, Message>;
  private _tools?: Map<string, ToolExecution>;
  private _agents?: Map<string, Agent>;

  async load() {
    if (this.loaded) return;

    // Load and parse JSONL (streaming, not all at once)
    await this.parseLog(this.summary.filePath);
    this.loaded = true;
  }

  // Lazy getters
  get messages(): Map<string, Message> {
    if (!this._messages) {
      throw new Error("Call load() first");
    }
    return this._messages;
  }
}
```

### Query: "Show me details of yesterday's sessions"

```typescript
async function yesterdayDetails() {
  // Step 1: Get session IDs from index (instant)
  const summaries = await whatDidWeDoYesterday();

  // Step 2: Load only the ones user wants to see
  const sessions = summaries.map(s => new SessionAnalyzer(s));

  // Step 3: Load first session's details
  await sessions[0].load();  // ~190 KB loaded

  // Now can query details
  return {
    conversation: Array.from(sessions[0].messages.values()),
    tools: Array.from(sessions[0].tools.values()),
    agents: Array.from(sessions[0].agents.values())
  };
}

// Memory: Only loads what's needed
// - Index: 10 MB (already in memory)
// - Session detail: 190 KB (loaded on demand)
// - Total: ~10.2 MB
```

---

## Tier 3: Raw Logs (Stream When Needed)

```typescript
class SessionAnalyzer {
  // For debugging or detailed analysis
  async *streamRawEvents() {
    const stream = createReadStream(this.summary.filePath);
    for await (const line of stream) {
      yield JSON.parse(line);
    }
  }

  // Example: Find specific event
  async findEvent(predicate: (event: any) => boolean) {
    for await (const event of this.streamRawEvents()) {
      if (predicate(event)) return event;
    }
    return null;
  }
}
```

**Only used for:** Debugging, recovering data, edge cases

---

## Search Layer: Semantic + Full-text

For queries like: "Where did we talk about authentication?"

### Option A: SQLite FTS (Fast, local)

```sql
-- Create once
CREATE VIRTUAL TABLE messages_fts USING fts5(
  session_id,
  message_id,
  timestamp,
  role,
  content
);

-- Index all messages (done during session index build)
INSERT INTO messages_fts VALUES
  ('session-abc', 'msg-123', '2026-01-29', 'user', 'Fix authentication bug'),
  ('session-abc', 'msg-124', '2026-01-29', 'assistant', 'I'll check the auth code...');

-- Query: "Where did we talk about authentication?"
SELECT session_id, timestamp, content
FROM messages_fts
WHERE content MATCH 'authentication'
ORDER BY timestamp DESC
LIMIT 10;
```

**Size:** ~5-10 MB for 1000 sessions (disk-based, not in RAM)
**Speed:** ~1-10ms for keyword search

### Option B: Vector Embeddings (Semantic)

```typescript
// Embed important messages during indexing
class EmbeddingIndex {
  // Stored in vector DB (e.g., SQLite with vector extension)
  async index(sessionId: string, message: Message) {
    const embedding = await embed(message.content);
    await db.execute(
      'INSERT INTO embeddings (session_id, message_id, vector) VALUES (?, ?, ?)',
      [sessionId, message.id, embedding]
    );
  }

  // Semantic search
  async search(query: string, limit = 10) {
    const queryVec = await embed(query);
    return await db.execute(`
      SELECT session_id, message_id,
             vector_distance(vector, ?) as distance
      FROM embeddings
      ORDER BY distance
      LIMIT ?
    `, [queryVec, limit]);
  }
}

// Query: "Where did we learn about caching strategies?"
const results = await embeddingIndex.search(
  "caching strategies optimization",
  10
);
// Returns sessions where we discussed caching (semantically)
```

---

## Answering Your Questions

### Q: "What did we do yesterday?"

```typescript
// Tier 1 only (instant)
const yesterday = getSessionsByDate(yesterday);
console.log(yesterday.map(s => s.summary));
```

**Memory:** ~10 MB (index only)
**Time:** ~1ms

---

### Q: "What have we learned in this session?"

```typescript
// Tier 2: Load current session
const session = await loadSession(currentSessionId);

// Extract learning artifacts
const learnings = {
  insights: extractInsights(session.messages),
  filesModified: session.files.keys(),
  toolsUsed: session.tools.values(),
  agents: session.agents.values().map(a => a.summary)
};
```

**Memory:** ~10 MB index + 190 KB session = 10.2 MB
**Time:** ~50-100ms

---

### Q: "What have we learned in this project?"

```typescript
// Tier 1 + Search
const allSessions = projectIndex.sessions.values();

// Option 1: Aggregate summaries (fast but shallow)
const quickView = allSessions.map(s => s.summary);

// Option 2: Load and analyze key sessions (slower but deeper)
const keySessions = findKeyLearnings(allSessions);  // e.g., sessions with many files modified
const detailed = await Promise.all(
  keySessions.slice(0, 10).map(s => loadSession(s.id))
);

// Option 3: Use embeddings to find "learning moments"
const learnings = await embeddingIndex.search("what we learned", 50);
```

**Memory:** Variable (10 MB + N × 190 KB for N sessions loaded)
**Strategy:** Load top 10-20 sessions, not all

---

### Q: "Where did we talk about authentication?"

```typescript
// Search layer (SQLite FTS)
const results = await db.execute(`
  SELECT session_id, timestamp, content
  FROM messages_fts
  WHERE content MATCH 'authentication'
  ORDER BY timestamp DESC
`);

// Load session summaries (Tier 1, already in memory)
const sessions = results.map(r =>
  projectIndex.sessions.get(r.session_id)
);
```

**Memory:** ~10 MB (index + search index on disk)
**Time:** ~5-10ms

---

### Q: "How much did this cost me?"

```typescript
// Tier 1 only (aggregated)
console.log(projectIndex.totalCost);

// Or filter by time
const lastWeek = getSessionsSince(sevenDaysAgo);
const cost = lastWeek.reduce((sum, s) => sum + s.cost, 0);
```

**Memory:** ~10 MB
**Time:** ~1ms

---

### Q: "How could we have done this better?"

This requires **pattern analysis** across sessions:

```typescript
async function analyzePatternsForImprovement() {
  // 1. Find similar past sessions (Tier 1 + embeddings)
  const currentTask = currentSession.summary;
  const similar = await embeddingIndex.search(currentTask, 10);

  // 2. Load those sessions (Tier 2)
  const pastSessions = await Promise.all(
    similar.map(s => loadSession(s.session_id))
  );

  // 3. Compare metrics
  const comparison = {
    current: {
      cost: currentSession.summary.cost,
      duration: currentSession.summary.duration,
      toolsUsed: currentSession.tools.size,
      cacheHitRate: calculateCacheHitRate(currentSession)
    },
    past: pastSessions.map(s => ({
      id: s.summary.id,
      cost: s.summary.cost,
      duration: s.summary.duration,
      toolsUsed: s.tools.size,
      cacheHitRate: calculateCacheHitRate(s)
    }))
  };

  // 4. Find improvements
  const betterSessions = comparison.past.filter(p =>
    p.cost < comparison.current.cost &&
    p.duration < comparison.current.duration
  );

  return {
    improvements: betterSessions,
    suggestions: generateSuggestions(comparison)
  };
}
```

**Memory:** ~10 MB index + 10 × 190 KB sessions = ~12 MB
**Time:** ~500ms

---

## Memory Budget Examples

### Scenario 1: Lightweight (10 MB)
```
✅ Tier 1 index (all sessions)
❌ No sessions loaded
❌ No search index

Can answer:
- "What sessions happened yesterday?"
- "Total cost?"
- "Which sessions modified file X?"
```

### Scenario 2: Normal (50 MB)
```
✅ Tier 1 index (10 MB)
✅ Current session loaded (190 KB)
✅ Last 10 sessions loaded (1.9 MB)
✅ SQLite FTS (10 MB on disk)

Can answer:
- All Scenario 1 questions
- "Show me yesterday's conversations"
- "Where did we talk about X?"
- "What have we learned recently?"
```

### Scenario 3: Analysis Mode (200 MB)
```
✅ Tier 1 index (10 MB)
✅ 100 sessions loaded (19 MB)
✅ Vector embeddings (50 MB in memory)
✅ Full-text search (20 MB)

Can answer:
- All questions
- Deep pattern analysis
- Cross-project comparisons
- "How have we improved over time?"
```

---

## Loading Strategy

```typescript
class SessionManager {
  private index: ProjectIndex;           // Always loaded
  private cache: LRU<string, SessionAnalyzer>;  // Recently used

  constructor(maxMemoryMB: number = 50) {
    this.index = await ProjectIndex.load();  // ~10 MB

    // LRU cache: Keep recently used sessions in memory
    const sessionBudget = maxMemoryMB - 10;  // Reserve for index
    const maxSessions = Math.floor(sessionBudget / 0.2);  // 0.2 MB per session
    this.cache = new LRU(maxSessions);
  }

  async get(sessionId: string): Promise<SessionAnalyzer> {
    // Check cache first
    if (this.cache.has(sessionId)) {
      return this.cache.get(sessionId)!;
    }

    // Load from disk
    const summary = this.index.sessions.get(sessionId);
    const session = new SessionAnalyzer(summary);
    await session.load();

    // Add to cache (evicts LRU if full)
    this.cache.set(sessionId, session);

    return session;
  }
}
```

---

## Implementation Strategy

### Phase 1: Build Index (One-time)
```bash
# Scan all session logs, build index
./scripts/build-index.sh

# Creates:
# - ~/.claude/projects/.index/sessions-index.json (10 MB)
# - ~/.claude/projects/.index/messages.fts (SQLite, 20 MB)
# - ~/.claude/projects/.index/embeddings.db (optional, 50 MB)
```

### Phase 2: Query Interface
```typescript
const sessions = new SessionManager(maxMemoryMB: 50);

// Fast queries (index only)
await sessions.find({ date: 'yesterday' });
await sessions.find({ file: 'src/core.ts' });
await sessions.totalCost();

// Detail queries (loads on demand)
const session = await sessions.get(sessionId);
console.log(session.conversation);

// Search queries
await sessions.search("authentication bug");
await sessions.semanticSearch("caching strategies");
```

---

## The Balance

| Strategy | Memory | Speed | Capabilities |
|----------|--------|-------|--------------|
| Index only | 10 MB | Instant | Summaries, filters, aggregates |
| + LRU cache (10 sessions) | 12 MB | Fast | Recent session details |
| + LRU cache (50 sessions) | 20 MB | Fast | More history |
| + SQLite FTS | 10 MB + 20 MB disk | Fast | Keyword search |
| + Embeddings | 10 MB + 50 MB disk | Medium | Semantic search |
| Load all sessions | 190 MB+ | Fastest | Everything, no lazy loading |

**Recommended:** 20-50 MB with index + LRU cache + SQLite FTS

**Key insight:** Most queries hit the index (10 MB). Detail loading is lazy and cached. Search is disk-based.

---

## Answer: You Don't Load Everything

**You load in tiers:**
1. **Always:** Index (10 MB) - enables 80% of queries
2. **On demand:** Session details (190 KB each) - cached LRU
3. **Rarely:** Raw logs (500 KB each) - streamed when needed
4. **Disk-based:** Search indexes - queried, not loaded

**This handles 10,000+ sessions in <50 MB RAM while still being fast.**
