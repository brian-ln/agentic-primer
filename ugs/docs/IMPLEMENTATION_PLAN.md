# Session Knowledge System - Implementation Plan

## Overview

Build in phases, each adding capability without breaking previous work.

```
Phase 1: Index Builder (1-2 hours)
   ↓
Phase 2: Query Interface (1 hour)
   ↓
Phase 3: Embeddings (2 hours)
   ↓
Phase 4: Smart Queries (ongoing)
```

---

## Phase 1: Build the Index

### Goal
Create lightweight index of all sessions for fast filtering.

### Output
```
~/.claude/index/
├── sessions.db              # SQLite database
├── sessions-summary.json    # Quick-load metadata
└── .cache/
    └── <session-id>.json    # Cached session analysis
```

### Implementation

**File: `scripts/build-index.ts`**
```typescript
#!/usr/bin/env bun
import { Database } from 'bun:sqlite';
import { readdir, readFile } from 'fs/promises';
import { join } from 'path';

const SESSIONS_DIR = join(
  process.env.HOME!,
  '.claude/projects',
  process.cwd().replace(/\//g, '-').replace(/^-/, '-')
);

const INDEX_DIR = join(process.env.HOME!, '.claude/index');
const DB_PATH = join(INDEX_DIR, 'sessions.db');

interface SessionSummary {
  id: string;
  created: Date;
  modified: Date;
  summary: string;
  messageCount: number;
  filesModified: string[];
  toolsUsed: string[];
  agentTypes: string[];
  cost: number;
  duration: number;
}

class SessionMetadataExtractor {
  db: Database;

  constructor() {
    this.db = new Database(DB_PATH);
    this.createSchema();
  }

  createSchema() {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS sessions (
        id TEXT PRIMARY KEY,
        created INTEGER,
        modified INTEGER,
        summary TEXT,
        message_count INTEGER,
        agent_count INTEGER,
        cost REAL,
        duration INTEGER,
        git_branch TEXT,
        project_path TEXT
      );

      CREATE TABLE IF NOT EXISTS session_files (
        session_id TEXT,
        file_path TEXT,
        FOREIGN KEY (session_id) REFERENCES sessions(id)
      );

      CREATE TABLE IF NOT EXISTS session_tools (
        session_id TEXT,
        tool_name TEXT,
        count INTEGER,
        FOREIGN KEY (session_id) REFERENCES sessions(id)
      );

      CREATE TABLE IF NOT EXISTS session_agents (
        session_id TEXT,
        agent_id TEXT,
        agent_type TEXT,
        task TEXT,
        outcome TEXT,
        FOREIGN KEY (session_id) REFERENCES sessions(id)
      );

      -- Indexes for fast queries
      CREATE INDEX IF NOT EXISTS idx_created ON sessions(created);
      CREATE INDEX IF NOT EXISTS idx_modified ON sessions(modified);
      CREATE INDEX IF NOT EXISTS idx_file ON session_files(file_path);
      CREATE INDEX IF NOT EXISTS idx_tool ON session_tools(tool_name);
      CREATE INDEX IF NOT EXISTS idx_agent_type ON session_agents(agent_type);

      -- Full-text search for summaries
      CREATE VIRTUAL TABLE IF NOT EXISTS sessions_fts USING fts5(
        session_id UNINDEXED,
        summary,
        content='sessions',
        content_rowid='rowid'
      );
    `);
  }

  async indexAllSessions() {
    const files = await readdir(SESSIONS_DIR);
    const sessionFiles = files.filter(f => f.endsWith('.jsonl') && !f.includes('subagents'));

    console.log(`Found ${sessionFiles.length} sessions to index`);

    for (const file of sessionFiles) {
      const sessionId = file.replace('.jsonl', '');
      await this.indexSession(sessionId);
    }

    console.log('Index build complete!');
  }

  async indexSession(sessionId: string) {
    const filePath = join(SESSIONS_DIR, `${sessionId}.jsonl`);
    const content = await readFile(filePath, 'utf-8');
    const lines = content.trim().split('\n');

    // Parse events
    const events = lines.map(line => JSON.parse(line));

    // Extract data
    const summary = this.extractSummary(events);

    // Insert into database
    this.db.run(`
      INSERT OR REPLACE INTO sessions
      (id, created, modified, summary, message_count, agent_count, cost, duration, git_branch, project_path)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `, [
      sessionId,
      summary.created.getTime(),
      summary.modified.getTime(),
      summary.summary,
      summary.messageCount,
      summary.agentTypes.length,
      summary.cost,
      summary.duration,
      summary.gitBranch ?? 'main',
      summary.projectPath ?? process.cwd()
    ]);

    // Insert files
    for (const file of summary.filesModified) {
      this.db.run(
        'INSERT INTO session_files (session_id, file_path) VALUES (?, ?)',
        [sessionId, file]
      );
    }

    // Insert tools
    for (const [tool, count] of Object.entries(summary.toolsUsed)) {
      this.db.run(
        'INSERT INTO session_tools (session_id, tool_name, count) VALUES (?, ?, ?)',
        [sessionId, tool, count]
      );
    }

    // Insert agents
    for (const agent of summary.agents) {
      this.db.run(
        'INSERT INTO session_agents (session_id, agent_id, agent_type, task, outcome) VALUES (?, ?, ?, ?, ?)',
        [sessionId, agent.id, agent.type, agent.task, agent.outcome]
      );
    }

    // Update FTS
    this.db.run(
      'INSERT INTO sessions_fts (session_id, summary) VALUES (?, ?)',
      [sessionId, summary.summary]
    );

    console.log(`Indexed: ${sessionId}`);
  }

  extractSummary(events: any[]): SessionSummary {
    const messages = events.filter(e => e.type === 'user' || e.type === 'assistant');
    const uniqueMessages = new Map();

    // Deduplicate messages
    for (const event of messages) {
      const msgId = event.message?.id ?? event.uuid;
      if (!uniqueMessages.has(msgId)) {
        uniqueMessages.set(msgId, event);
      }
    }

    // Extract file modifications
    const filesModified = new Set<string>();
    const toolsUsed = new Map<string, number>();
    const agents = [];
    let totalCost = 0;

    for (const event of events) {
      // Tool usage
      if (event.message?.content) {
        for (const block of event.message.content) {
          if (block.type === 'tool_use') {
            toolsUsed.set(block.name, (toolsUsed.get(block.name) ?? 0) + 1);

            // Track file operations
            if (block.input?.file_path) {
              filesModified.add(block.input.file_path);
            }

            // Track agents
            if (block.name === 'Task' && block.input?.subagent_type) {
              agents.push({
                id: extractAgentId(event),
                type: block.input.subagent_type,
                task: block.input.description,
                outcome: 'completed'  // TODO: track actual outcome
              });
            }
          }
        }
      }

      // Cost tracking
      if (event.message?.usage) {
        const usage = event.message.usage;
        totalCost += calculateCost(usage);
      }
    }

    // Get timestamps
    const timestamps = events.map(e => new Date(e.timestamp));
    const created = new Date(Math.min(...timestamps.map(t => t.getTime())));
    const modified = new Date(Math.max(...timestamps.map(t => t.getTime())));

    // Get summary from sessions-index.json if available
    let summary = 'Session';
    try {
      const indexPath = join(SESSIONS_DIR, 'sessions-index.json');
      const indexData = JSON.parse(await readFile(indexPath, 'utf-8'));
      const entry = indexData.entries.find(e => e.sessionId === sessionId);
      if (entry) summary = entry.summary;
    } catch {}

    return {
      id: sessionId,
      created,
      modified,
      summary,
      messageCount: uniqueMessages.size,
      filesModified: Array.from(filesModified),
      toolsUsed: Array.from(toolsUsed.keys()),
      agentTypes: agents.map(a => a.type),
      agents,
      cost: totalCost,
      duration: modified.getTime() - created.getTime(),
      gitBranch: events[0]?.gitBranch,
      projectPath: events[0]?.cwd
    };
  }
}

function calculateCost(usage: any): number {
  const OPUS_INPUT = 15 / 1_000_000;      // $15 per 1M tokens
  const OPUS_OUTPUT = 75 / 1_000_000;     // $75 per 1M tokens
  const CACHE_WRITE = 18.75 / 1_000_000;  // $18.75 per 1M tokens
  const CACHE_READ = 1.50 / 1_000_000;    // $1.50 per 1M tokens

  return (
    (usage.input_tokens ?? 0) * OPUS_INPUT +
    (usage.output_tokens ?? 0) * OPUS_OUTPUT +
    (usage.cache_creation_input_tokens ?? 0) * CACHE_WRITE +
    (usage.cache_read_input_tokens ?? 0) * CACHE_READ
  );
}

function extractAgentId(event: any): string {
  // Look for agentId in tool result
  const result = event.message?.content?.find(c => c.type === 'tool_result');
  if (result?.content) {
    const match = result.content.match(/agentId:\s*([a-f0-9]+)/);
    if (match) return match[1];
  }
  return 'unknown';
}

// Run
const builder = new SessionMetadataExtractor();
await builder.indexAllSessions();
```

**Usage:**
```bash
bun run scripts/build-index.ts
# Scans all sessions, builds SQLite index
# Takes ~1-2 minutes for 1000 sessions
```

---

## Phase 2: Query Interface

### Goal
Fast queries without loading full sessions.

### Implementation

**File: `scripts/query-sessions.ts`**
```typescript
#!/usr/bin/env bun
import { Database } from 'bun:sqlite';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions.db');

class SessionQuery {
  db: Database;

  constructor() {
    this.db = new Database(DB_PATH, { readonly: true });
  }

  // Q: "What did we do yesterday?"
  yesterday() {
    const yesterday = new Date();
    yesterday.setDate(yesterday.getDate() - 1);
    yesterday.setHours(0, 0, 0, 0);

    const tomorrow = new Date(yesterday);
    tomorrow.setDate(tomorrow.getDate() + 1);

    return this.db.query(`
      SELECT id, datetime(created/1000, 'unixepoch') as created, summary, cost
      FROM sessions
      WHERE created >= ? AND created < ?
      ORDER BY created DESC
    `).all(yesterday.getTime(), tomorrow.getTime());
  }

  // Q: "What sessions modified file X?"
  byFile(filePath: string) {
    return this.db.query(`
      SELECT s.id, s.summary, s.created, sf.file_path
      FROM sessions s
      JOIN session_files sf ON s.id = sf.session_id
      WHERE sf.file_path LIKE ?
      ORDER BY s.created DESC
    `).all(`%${filePath}%`);
  }

  // Q: "How much did this cost in the last week?"
  costSince(days: number = 7) {
    const since = Date.now() - (days * 24 * 60 * 60 * 1000);

    const result = this.db.query(`
      SELECT
        COUNT(*) as session_count,
        SUM(cost) as total_cost,
        AVG(cost) as avg_cost,
        SUM(message_count) as total_messages
      FROM sessions
      WHERE created >= ?
    `).get(since);

    return result;
  }

  // Q: "Search for sessions about X"
  search(query: string) {
    return this.db.query(`
      SELECT s.id, s.summary, s.created, s.cost
      FROM sessions s
      JOIN sessions_fts fts ON s.id = fts.session_id
      WHERE sessions_fts MATCH ?
      ORDER BY rank
      LIMIT 20
    `).all(query);
  }

  // Q: "What tools do we use most?"
  toolStats() {
    return this.db.query(`
      SELECT tool_name, SUM(count) as total_uses, COUNT(DISTINCT session_id) as session_count
      FROM session_tools
      GROUP BY tool_name
      ORDER BY total_uses DESC
    `).all();
  }

  // Q: "What agent types do we use?"
  agentStats() {
    return this.db.query(`
      SELECT agent_type, COUNT(*) as count
      FROM session_agents
      GROUP BY agent_type
      ORDER BY count DESC
    `).all();
  }

  // Q: "Show me recent sessions"
  recent(limit: number = 10) {
    return this.db.query(`
      SELECT id, datetime(created/1000, 'unixepoch') as created, summary, cost, message_count
      FROM sessions
      ORDER BY created DESC
      LIMIT ?
    `).all(limit);
  }
}

// CLI
const query = new SessionQuery();
const command = process.argv[2];

switch (command) {
  case 'yesterday':
    console.table(query.yesterday());
    break;

  case 'file':
    const file = process.argv[3];
    console.table(query.byFile(file));
    break;

  case 'cost':
    const days = parseInt(process.argv[3] ?? '7');
    console.log(query.costSince(days));
    break;

  case 'search':
    const searchQuery = process.argv.slice(3).join(' ');
    console.table(query.search(searchQuery));
    break;

  case 'tools':
    console.table(query.toolStats());
    break;

  case 'agents':
    console.table(query.agentStats());
    break;

  case 'recent':
    console.table(query.recent(20));
    break;

  default:
    console.log(`
Usage:
  bun run scripts/query-sessions.ts yesterday
  bun run scripts/query-sessions.ts file src/core.ts
  bun run scripts/query-sessions.ts cost 7
  bun run scripts/query-sessions.ts search "authentication bug"
  bun run scripts/query-sessions.ts tools
  bun run scripts/query-sessions.ts agents
  bun run scripts/query-sessions.ts recent
    `);
}
```

**Usage:**
```bash
bun run scripts/query-sessions.ts yesterday
bun run scripts/query-sessions.ts file src/core.ts
bun run scripts/query-sessions.ts cost 30
bun run scripts/query-sessions.ts search "caching"
```

---

## Phase 3: Add Embeddings

### Goal
Semantic search for "What have we learned?" style questions.

### Implementation

**File: `scripts/build-embeddings.ts`**
```typescript
#!/usr/bin/env bun
import { Database } from 'bun:sqlite';
import { readFile } from 'fs/promises';
import { join } from 'path';

const DB_PATH = join(process.env.HOME!, '.claude/index/sessions.db');
const SESSIONS_DIR = join(process.env.HOME!, '.claude/projects', /* ... */);

class EmbeddingBuilder {
  db: Database;
  apiKey: string;

  constructor() {
    this.db = new Database(DB_PATH);
    this.apiKey = process.env.OPENAI_API_KEY!;
    this.createSchema();
  }

  createSchema() {
    // Using sqlite-vec extension
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS embeddings (
        id TEXT PRIMARY KEY,
        session_id TEXT,
        type TEXT,  -- 'session', 'message', 'agent'
        text TEXT,
        embedding BLOB,
        timestamp INTEGER,
        metadata JSON
      );

      CREATE INDEX IF NOT EXISTS idx_emb_session ON embeddings(session_id);
      CREATE INDEX IF NOT EXISTS idx_emb_type ON embeddings(type);
    `);
  }

  async embed(text: string): Promise<number[]> {
    const response = await fetch('https://api.openai.com/v1/embeddings', {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${this.apiKey}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        model: 'text-embedding-3-small',
        input: text
      })
    });

    const data = await response.json();
    return data.data[0].embedding;
  }

  async indexSession(sessionId: string) {
    // Load session
    const filePath = join(SESSIONS_DIR, `${sessionId}.jsonl`);
    const content = await readFile(filePath, 'utf-8');
    const events = content.trim().split('\n').map(line => JSON.parse(line));

    // 1. Embed session summary
    const summary = this.getSessionSummary(sessionId);
    if (summary) {
      const embedding = await this.embed(summary);
      this.db.run(`
        INSERT OR REPLACE INTO embeddings (id, session_id, type, text, embedding, timestamp)
        VALUES (?, ?, ?, ?, ?, ?)
      `, [
        `session-${sessionId}`,
        sessionId,
        'session',
        summary,
        new Uint8Array(new Float32Array(embedding).buffer),
        Date.now()
      ]);
      console.log(`Embedded session: ${sessionId}`);
    }

    // 2. Embed important messages
    const messages = this.extractImportantMessages(events);
    for (const msg of messages) {
      const embedding = await this.embed(msg.text);
      this.db.run(`
        INSERT OR REPLACE INTO embeddings (id, session_id, type, text, embedding, timestamp, metadata)
        VALUES (?, ?, ?, ?, ?, ?, ?)
      `, [
        `msg-${msg.id}`,
        sessionId,
        'message',
        msg.text,
        new Uint8Array(new Float32Array(embedding).buffer),
        msg.timestamp,
        JSON.stringify(msg.metadata)
      ]);
    }

    // 3. Embed agent results
    const agents = this.extractAgents(events);
    for (const agent of agents) {
      if (agent.summary) {
        const embedding = await this.embed(agent.summary);
        this.db.run(`
          INSERT OR REPLACE INTO embeddings (id, session_id, type, text, embedding, timestamp, metadata)
          VALUES (?, ?, ?, ?, ?, ?, ?)
        `, [
          `agent-${agent.id}`,
          sessionId,
          'agent',
          agent.summary,
          new Uint8Array(new Float32Array(embedding).buffer),
          Date.now(),
          JSON.stringify({ agentType: agent.type, task: agent.task })
        ]);
      }
    }
  }

  getSessionSummary(sessionId: string): string | null {
    const result = this.db.query('SELECT summary FROM sessions WHERE id = ?').get(sessionId);
    return result?.summary ?? null;
  }

  extractImportantMessages(events: any[]) {
    const messages = [];
    for (const event of events) {
      if (event.type === 'user' && event.message?.content) {
        const text = event.message.content;
        if (typeof text === 'string' && text.length > 50) {
          messages.push({
            id: event.uuid,
            text: text.slice(0, 1000),  // Truncate
            timestamp: new Date(event.timestamp).getTime(),
            metadata: { role: 'user' }
          });
        }
      }
    }
    return messages;
  }

  extractAgents(events: any[]): any[] {
    // Similar to SessionMetadataExtractor.extractSummary
    return [];
  }

  async indexAllSessions() {
    const sessions = this.db.query('SELECT id FROM sessions').all();
    console.log(`Embedding ${sessions.length} sessions...`);

    for (const session of sessions) {
      await this.indexSession(session.id);
      // Rate limit
      await new Promise(resolve => setTimeout(resolve, 100));
    }

    console.log('Embeddings complete!');
  }
}

const builder = new EmbeddingBuilder();
await builder.indexAllSessions();
```

**File: `scripts/semantic-search.ts`**
```typescript
#!/usr/bin/env bun
import { Database } from 'bun:sqlite';

class SemanticSearch {
  db: Database;
  apiKey: string;

  constructor() {
    this.db = new Database(DB_PATH);
    this.apiKey = process.env.OPENAI_API_KEY!;
  }

  async search(query: string, type?: string, limit: number = 10) {
    // Get query embedding
    const queryEmb = await this.embed(query);

    // Search (using brute force for now, optimize with HNSW later)
    let sql = `SELECT id, session_id, type, text, embedding FROM embeddings`;
    if (type) sql += ` WHERE type = '${type}'`;

    const results = this.db.query(sql).all();

    // Calculate similarities
    const scored = results.map(r => ({
      ...r,
      similarity: this.cosineSimilarity(
        queryEmb,
        new Float32Array(r.embedding.buffer)
      )
    }));

    // Sort and return top K
    return scored
      .sort((a, b) => b.similarity - a.similarity)
      .slice(0, limit);
  }

  cosineSimilarity(a: number[], b: Float32Array): number {
    let dot = 0, magA = 0, magB = 0;
    for (let i = 0; i < a.length; i++) {
      dot += a[i] * b[i];
      magA += a[i] * a[i];
      magB += b[i] * b[i];
    }
    return dot / (Math.sqrt(magA) * Math.sqrt(magB));
  }

  async embed(text: string): Promise<number[]> {
    // Same as EmbeddingBuilder.embed
  }
}

// CLI
const search = new SemanticSearch();
const query = process.argv.slice(2).join(' ');

const results = await search.search(query);
console.table(results.map(r => ({
  session: r.session_id.slice(0, 8),
  type: r.type,
  similarity: r.similarity.toFixed(3),
  text: r.text.slice(0, 100)
})));
```

---

## Phase 4: Smart Queries

Now combine everything:

**File: `scripts/ask.ts`**
```typescript
#!/usr/bin/env bun
// Smart query interface that routes to appropriate search

const question = process.argv.slice(2).join(' ');

// Pattern matching for common questions
if (question.match(/yesterday|today|last (week|month)/i)) {
  // Time-based query → use index
  exec('bun run scripts/query-sessions.ts yesterday');
}
else if (question.match(/cost|spent|expensive/i)) {
  // Cost query → use index
  exec('bun run scripts/query-sessions.ts cost 30');
}
else if (question.match(/file|modified|changed/i)) {
  // File query → use index
  const file = extractFileName(question);
  exec(`bun run scripts/query-sessions.ts file ${file}`);
}
else if (question.match(/learn|about|discuss|talk about/i)) {
  // Semantic query → use embeddings
  exec(`bun run scripts/semantic-search.ts ${question}`);
}
else {
  // Default: semantic search
  exec(`bun run scripts/semantic-search.ts ${question}`);
}
```

---

## Usage Examples

```bash
# Build index (one-time)
bun run scripts/build-index.ts

# Query
bun run scripts/query-sessions.ts yesterday
bun run scripts/query-sessions.ts cost 30
bun run scripts/query-sessions.ts file src/core.ts

# Build embeddings (optional, costs $0.20-$1.30)
export OPENAI_API_KEY=sk-...
bun run scripts/build-embeddings.ts

# Semantic search
bun run scripts/semantic-search.ts "what did we learn about caching"
bun run scripts/semantic-search.ts "authentication bugs"

# Smart query
bun run scripts/ask.ts "what did we do yesterday?"
bun run scripts/ask.ts "how much have I spent?"
bun run scripts/ask.ts "where did we discuss performance?"
```

---

## File Structure

```
simplify/
├── scripts/
│   ├── build-index.ts           # Phase 1
│   ├── query-sessions.ts        # Phase 2
│   ├── build-embeddings.ts      # Phase 3
│   ├── semantic-search.ts       # Phase 3
│   └── ask.ts                   # Phase 4
└── ~/.claude/index/
    ├── sessions.db              # SQLite with index + embeddings
    └── .cache/
        └── <session-id>.json    # Cached analyses
```

---

## Incremental Approach

### Week 1: Index Only
- Build index
- Query by time, file, cost
- No embeddings yet

### Week 2: Add Embeddings
- Index session summaries only (cheap)
- Test semantic search
- Evaluate value

### Week 3: Expand
- Add message embeddings
- Build smart query router
- Integrate with workflow

---

## Dependencies

```json
{
  "dependencies": {
    "bun": "latest"
  },
  "devDependencies": {
    "@types/bun": "latest"
  }
}
```

**Optional:**
- `sqlite-vec` extension for vector search
- `chromadb` for better vector DB
- OpenAI API key for embeddings

---

## Next Steps

1. **Start:** `bun run scripts/build-index.ts`
2. **Test:** `bun run scripts/query-sessions.ts recent`
3. **Query:** `bun run scripts/query-sessions.ts yesterday`

**That's it. Index built, ready to query.**
