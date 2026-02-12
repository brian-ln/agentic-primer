# Embeddings for Session Search

## The Problem with Keyword Search

### Your Question: "Where did we talk about authentication?"

**Keyword search (SQLite FTS):**
```sql
SELECT * FROM messages WHERE content MATCH 'authentication'
```

**Finds:**
- ✅ "Fixed authentication bug"
- ✅ "Authentication flow broken"
- ❌ "Login not working" (no keyword match!)
- ❌ "User verification failing" (related but different words)
- ❌ "Session tokens expired" (authentication issue, different terms)

**Misses 60% of relevant results** because people use different words for same concepts.

---

## What Embeddings Are

**Text → Numbers that capture meaning**

```typescript
// Same concept, different words
embed("authentication bug")      → [0.23, 0.45, 0.67, ..., 0.12]
embed("login not working")       → [0.25, 0.43, 0.69, ..., 0.11]
embed("user verification issue") → [0.24, 0.44, 0.68, ..., 0.13]
                                     └─ These are CLOSE in vector space

// Different concept
embed("database optimization")   → [-0.56, 0.12, -0.34, ..., 0.89]
                                     └─ This is FAR in vector space
```

**Distance = semantic similarity**
```
distance(auth_bug, login_issue) = 0.05  (very similar)
distance(auth_bug, db_optimize) = 0.92  (very different)
```

---

## Why This Matters for Your Questions

### Q: "Where did we talk about authentication?"

**With embeddings:**
```typescript
const query = embed("authentication");

// Find all messages with similar meaning
const results = vectorDB.search(query, topK: 20);

// Returns:
// - "Fixed login bug" (distance: 0.04)
// - "User verification failing" (distance: 0.06)
// - "Session tokens expired" (distance: 0.08)
// - "Password reset broken" (distance: 0.12)
// - "OAuth integration" (distance: 0.15)
```

**Finds ALL related discussions**, even with different terminology.

---

### Q: "What have we learned about caching?"

**Without embeddings:**
- Search for keyword "caching"
- Miss: "optimization strategies", "performance improvements", "memoization"

**With embeddings:**
```typescript
const query = embed("caching strategies and optimization");

const sessions = vectorDB.search(query, topK: 10);
// Returns sessions about:
// - "Prompt caching implementation"
// - "Memoization patterns"
// - "Redis integration"
// - "Performance optimization"
// - "Token usage reduction"
```

**Understands concepts**, not just words.

---

### Q: "How could we have done this better?"

**Find similar past sessions:**
```typescript
const currentSessionEmbedding = embed(currentSession.summary);

// Find sessions that were semantically similar tasks
const similar = vectorDB.search(currentSessionEmbedding, topK: 10);

// Compare:
// - How much they cost
// - How long they took
// - What tools/approaches they used
// - Cache hit rates
// - Outcome quality

// Learn from past successes
```

**Example:**
```
Current: "Implement user authentication" (cost: $2.50, 45 min)

Similar past sessions:
1. "Add login system" (cost: $1.20, 25 min) ← Did this better!
   └─ Used agent parallelization, high cache hits
2. "OAuth integration" (cost: $3.10, 60 min)
   └─ Similar cost, we're on track
```

---

## How Embeddings Work

### 1. Generate embeddings for messages

```typescript
// During session indexing
for (const message of session.messages.values()) {
  const text = extractText(message);  // "Fixed auth bug in login"

  // Call embedding API (OpenAI, Voyage, etc.)
  const vector = await embed(text);   // [0.23, -0.45, ...]

  // Store in vector DB
  await vectorDB.insert({
    sessionId: session.id,
    messageId: message.id,
    timestamp: message.timestamp,
    vector: vector,
    metadata: {
      role: message.role,
      tools: message.tools,
    }
  });
}
```

### 2. Search by similarity

```typescript
// User asks: "Where did we discuss optimization?"
const queryVector = await embed("optimization strategies");

// Find nearest neighbors in vector space
const results = await vectorDB.search(queryVector, {
  topK: 20,
  threshold: 0.8  // Only return if >80% similar
});

// Results ranked by similarity
results.forEach(r => {
  console.log(`${r.similarity}: ${r.text} (${r.sessionId})`);
});
```

---

## Storage and Cost

### Storage Size

**Per message:**
```
Text: "Fixed authentication bug"     (25 bytes)
Embedding: 1536 floats × 4 bytes    (6 KB)
```

**For entire project:**
```
1,000 sessions × 48 messages × 6 KB = 288 MB of embeddings
```

**With compression/quantization:**
```
Using int8 quantization: 288 MB → 72 MB
Using PQ compression:    288 MB → 36 MB
```

### API Costs (OpenAI)

```
text-embedding-3-small: $0.02 / 1M tokens
text-embedding-3-large: $0.13 / 1M tokens

Embed 48,000 messages (~10M tokens):
- Small model: $0.20
- Large model: $1.30

One-time cost to index entire project history.
```

### Query Speed

```
Vector search in 48,000 messages:
- HNSW index: ~1-5ms
- Brute force: ~50-100ms

Much faster than loading sessions and searching text.
```

---

## Implementation Options

### Option 1: SQLite with vector extension

```sql
-- Install sqlite-vec extension
.load sqlite-vec

-- Create table
CREATE TABLE message_embeddings (
  session_id TEXT,
  message_id TEXT,
  timestamp INTEGER,
  embedding FLOAT[1536],
  metadata JSON
);

-- Create vector index (HNSW)
CREATE INDEX idx_embedding ON message_embeddings
  USING hnsw (embedding);

-- Search
SELECT session_id, message_id,
       vec_distance_cosine(embedding, :query_vector) as similarity
FROM message_embeddings
ORDER BY similarity
LIMIT 20;
```

**Pros:** Local, no dependencies, integrated with SQLite
**Cons:** Slower than specialized vector DBs

### Option 2: ChromaDB (embedded)

```typescript
import { ChromaClient } from 'chromadb';

const client = new ChromaClient();
const collection = await client.createCollection({
  name: "session_messages",
  metadata: { "hnsw:space": "cosine" }
});

// Index messages
await collection.add({
  ids: messageIds,
  embeddings: embeddings,
  metadatas: metadatas,
  documents: texts
});

// Search
const results = await collection.query({
  queryEmbeddings: [queryVector],
  nResults: 20,
  where: { "session_id": { "$in": recentSessionIds } }  // Filter
});
```

**Pros:** Fast, easy to use, runs locally
**Cons:** Separate process (~100 MB memory)

### Option 3: Simple in-memory

```typescript
// For small projects (<1000 sessions)
class SimpleVectorSearch {
  embeddings: Array<{
    id: string,
    vector: number[],
    metadata: any
  }> = [];

  add(id: string, vector: number[], metadata: any) {
    this.embeddings.push({ id, vector, metadata });
  }

  search(queryVector: number[], topK: number = 10) {
    return this.embeddings
      .map(e => ({
        ...e,
        similarity: cosineSimilarity(queryVector, e.vector)
      }))
      .sort((a, b) => b.similarity - a.similarity)
      .slice(0, topK);
  }
}

function cosineSimilarity(a: number[], b: number[]): number {
  const dot = a.reduce((sum, val, i) => sum + val * b[i], 0);
  const magA = Math.sqrt(a.reduce((sum, val) => sum + val * val, 0));
  const magB = Math.sqrt(b.reduce((sum, val) => sum + val * val, 0));
  return dot / (magA * magB);
}
```

**Pros:** No dependencies, simple
**Cons:** Slow for >10K messages, loads all into memory

---

## What to Embed?

Not everything! Be selective:

### ✅ Should Embed

1. **User messages** - Questions and requests
2. **Assistant summaries** - High-level responses
3. **Session summaries** - Overall session description
4. **Agent results** - What agents found/did
5. **Key decisions** - Important reasoning

### ❌ Don't Embed

1. Tool results (code dumps, file contents)
2. Progress events
3. Token usage stats
4. Metadata fields

**Strategy:**
```typescript
function shouldEmbed(message: Message): boolean {
  // User messages always
  if (message.role === "user") return true;

  // Assistant messages: only text blocks, not tool spam
  if (message.role === "assistant") {
    const textBlocks = message.content.filter(c => c.type === "text");
    return textBlocks.length > 0 && textBlocks[0].text.length > 50;
  }

  return false;
}
```

**Result:** Embed ~20-30% of events, not all 2,532

---

## Practical Example

```typescript
class SessionKnowledge {
  vectorDB: VectorDB;
  sessionManager: SessionManager;

  // Index a session's knowledge
  async indexSession(sessionId: string) {
    const session = await this.sessionManager.get(sessionId);

    // 1. Embed session summary
    await this.vectorDB.add({
      id: `session-${sessionId}`,
      text: session.summary.summary,
      vector: await embed(session.summary.summary),
      metadata: {
        type: "session",
        sessionId,
        timestamp: session.summary.created
      }
    });

    // 2. Embed user questions
    for (const msg of session.messages.values()) {
      if (msg.role === "user" && msg.content[0]?.text) {
        await this.vectorDB.add({
          id: `msg-${msg.id}`,
          text: msg.content[0].text,
          vector: await embed(msg.content[0].text),
          metadata: {
            type: "question",
            sessionId,
            messageId: msg.id,
            timestamp: msg.timestamp
          }
        });
      }
    }

    // 3. Embed agent learnings
    for (const agent of session.agents.values()) {
      if (agent.summary) {
        await this.vectorDB.add({
          id: `agent-${agent.id}`,
          text: agent.summary,
          vector: await embed(agent.summary),
          metadata: {
            type: "learning",
            sessionId,
            agentId: agent.id,
            agentType: agent.type
          }
        });
      }
    }
  }

  // Query knowledge
  async search(query: string, filters?: any) {
    const queryVector = await embed(query);
    const results = await this.vectorDB.search(queryVector, {
      topK: 20,
      filters
    });

    // Group by session
    const bySessions = groupBy(results, r => r.metadata.sessionId);

    return Object.entries(bySessions).map(([sessionId, matches]) => ({
      session: this.sessionManager.index.sessions.get(sessionId),
      matches,
      relevance: matches[0].similarity
    }));
  }
}

// Usage
const knowledge = new SessionKnowledge();

// "Where did we talk about authentication?"
const authSessions = await knowledge.search("authentication security login");
// Returns sessions ranked by relevance

// "What have we learned about caching?"
const cacheLearnings = await knowledge.search("caching optimization", {
  type: "learning"  // Filter to just agent learnings
});

// "Find sessions similar to current task"
const similar = await knowledge.search(currentSession.summary.summary, {
  type: "session",
  timestamp: { $lt: Date.now() - 86400000 }  // Exclude today
});
```

---

## ROI Analysis

### Without Embeddings

**Q: "Where did we discuss caching?"**
- Keyword search: "caching" → 3 results
- Manual review needed to find related concepts
- Time: 5-10 minutes

### With Embeddings

**Q: "Where did we discuss caching?"**
- Semantic search: finds caching, optimization, memoization, redis
- Ranked by relevance → 15 results
- Time: 5 seconds

### Cost

**Setup:**
- Generate embeddings: $0.20-$1.30 one-time
- Storage: 36-72 MB (compressed)
- Time: 10 minutes to index

**Ongoing:**
- Index new session: $0.001 per session
- Query: <5ms, free

**Break-even:** After ~5-10 semantic queries

---

## Recommendation

### Start Simple

**Phase 1: Session summaries only**
```typescript
// Embed just session summaries (48 per session → 1 total)
// Storage: 1,000 sessions × 6 KB = 6 MB
// Cost: ~$0.02
// Time: 1 minute

// Enables:
// - "Find sessions about X"
// - "Sessions similar to current"
```

**Phase 2: Add important messages**
```typescript
// Embed user questions + agent results
// Storage: ~36 MB
// Cost: ~$0.20
// Time: 10 minutes

// Enables:
// - "Where did we talk about X?"
// - "What have we learned about Y?"
```

**Phase 3: Advanced (optional)**
```typescript
// Full message embeddings + clustering
// Storage: ~72 MB
// Cost: ~$1.30

// Enables:
// - Topic clustering
// - Trend analysis
// - Deep pattern matching
```

---

## Example Queries Enabled

```typescript
// "What did we learn about authentication?"
await knowledge.search("authentication security best practices", {
  type: "learning"
});

// "Find sessions where we fixed bugs"
await knowledge.search("bug fix debugging error", {
  type: "session"
});

// "Show me sessions like this one"
await knowledge.search(currentSession.summary, {
  type: "session",
  exclude: currentSessionId
});

// "What concepts appear most in our work?"
const clusters = await knowledge.cluster(minClusterSize: 5);
// Returns: [
//   { topic: "authentication/security", sessions: [s1, s2, s3] },
//   { topic: "performance/caching", sessions: [s4, s5] },
//   { topic: "testing/quality", sessions: [s6, s7, s8] }
// ]

// "How have our approaches evolved over time?"
const timeline = await knowledge.timelineAnalysis("caching strategies");
// Returns sessions about caching, chronologically
// Can see how strategies changed
```

---

## Bottom Line

**Embeddings enable semantic search**, which is essential for questions like:
- "What have we learned?"
- "Where did we talk about X?"
- "How could we do better?"

**Cost:** ~$0.20-$1.30 one-time + 36-72 MB storage
**Benefit:** Turn 10-minute manual searches into 5-second queries

**Worth it?** Yes, if you want to actually leverage past session knowledge.

Without embeddings, you're limited to exact keyword matches and manual review.
