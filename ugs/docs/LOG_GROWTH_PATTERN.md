# Log Growth Patterns and Indexing Strategy

## Do Logs Grow?

**Yes, in two ways:**

### 1. During Active Session (Append-Only)

```
Session starts:
  file.jsonl created (0 bytes)

User sends message:
  → new line appended (event 1)

Assistant responds:
  → multiple lines appended (event 2, 3, 4... streaming chunks)

Tools execute:
  → more lines appended (tool use, tool result)

Session continues:
  → file grows continuously
  → append-only (never modifies earlier lines)

Session ends:
  → file closed
  → final size: 34K - 5MB depending on activity
```

**Pattern:** Append-only, chronological, grows during session

---

### 2. After Session Ends (File Touched, Content Stable)

**Observation:**
```
Session from Jan 26:
  Events: 2026-01-26T22:02:30 - 22:02:37
  File mtime: Jan 29 21:00  ← File "touched" 3 days later!
  Content: Unchanged (MD5 hash stable)
```

**Why are old files touched?**

Likely causes:
1. **sessions-index.json update** - CLI updates index, may touch files
2. **Backup/sync** - iCloud, Time Machine, etc. updating metadata
3. **File system maintenance** - Cleanup operations
4. **Summary generation** - Post-processing adding metadata

**Important:** Content doesn't change, just file metadata (mtime)

---

## Growth Characteristics

### Active Session
```
Time    Lines   Size    Rate
0:00    1       500B    -
0:01    5       3KB     3KB/min
0:05    25      15KB    3KB/min
0:30    150     90KB    3KB/min
1:00    300     180KB   3KB/min
```

**Average:** ~3KB/min for active coding session
**Burst:** Can spike to 50KB/min during heavy tool use

### Completed Session
```
Content: Stable ✓
Size: Frozen ✓
mtime: May change (metadata updates)
Events: Never modified
```

---

## Indexing Strategy

### Option 1: Full Reindex (Simple)

```bash
# Rebuild entire index from scratch
bun run scripts/build-index.ts
```

**Pros:**
- Simple, no state management
- Always correct
- Handles any changes

**Cons:**
- Slow for 1000+ sessions (~1-2 minutes)
- Wasteful (reprocesses unchanged files)

**When to use:** Occasional full rebuild (weekly?)

---

### Option 2: Incremental Update (Smart)

```typescript
class IncrementalIndexer {
  async update() {
    // 1. Check index version
    const lastIndexed = this.getLastIndexTime();

    // 2. Find new/modified sessions
    const sessions = await this.findSessionsSince(lastIndexed);

    // 3. Index only those
    for (const session of sessions) {
      await this.indexSession(session);
    }

    // 4. Update index timestamp
    this.setLastIndexTime(Date.now());
  }

  async findSessionsSince(timestamp: number): Promise<string[]> {
    const files = await readdir(SESSIONS_DIR);
    const modified = [];

    for (const file of files) {
      if (!file.endsWith('.jsonl')) continue;

      const stats = await stat(join(SESSIONS_DIR, file));

      // File modified after last index OR new file
      if (stats.mtimeMs > timestamp) {
        modified.push(file.replace('.jsonl', ''));
      }
    }

    return modified;
  }
}
```

**Pros:**
- Fast (only processes new/changed sessions)
- Efficient for regular updates

**Cons:**
- More complex
- Needs to track last index time
- Can miss changes if mtime unreliable

**When to use:** Regular updates (hourly, on session end)

---

### Option 3: Hybrid (Recommended)

```typescript
class SmartIndexer {
  async update(force: boolean = false) {
    if (force) {
      // Full reindex
      return await this.fullReindex();
    }

    // Incremental update
    const lastIndexed = this.getLastIndexTime();
    const now = Date.now();

    // If index is old (>7 days), do full reindex
    if (now - lastIndexed > 7 * 24 * 60 * 60 * 1000) {
      console.log('Index is old, doing full reindex...');
      return await this.fullReindex();
    }

    // Otherwise, incremental
    const newSessions = await this.findNewSessions(lastIndexed);

    if (newSessions.length === 0) {
      console.log('Index is up to date');
      return;
    }

    console.log(`Indexing ${newSessions.length} new/modified sessions...`);
    for (const sessionId of newSessions) {
      await this.indexSession(sessionId);
    }

    this.setLastIndexTime(now);
  }

  // Track indexed sessions and their content hashes
  async findNewSessions(since: number): Promise<string[]> {
    const indexed = this.getIndexedSessions();  // From DB
    const files = await readdir(SESSIONS_DIR);

    const toIndex = [];

    for (const file of files) {
      if (!file.endsWith('.jsonl')) continue;

      const sessionId = file.replace('.jsonl', '');
      const filePath = join(SESSIONS_DIR, file);
      const stats = await stat(filePath);

      // New session?
      if (!indexed.has(sessionId)) {
        toIndex.push(sessionId);
        continue;
      }

      // Modified session?
      // Use content hash, not mtime (mtime can be touched)
      const hash = await this.hashFile(filePath);
      if (hash !== indexed.get(sessionId).hash) {
        toIndex.push(sessionId);
      }
    }

    return toIndex;
  }

  async hashFile(path: string): Promise<string> {
    const content = await readFile(path);
    return createHash('md5').update(content).digest('hex');
  }

  getIndexedSessions(): Map<string, {hash: string, indexed: number}> {
    // Query from database
    const rows = this.db.query('SELECT id, content_hash, indexed_at FROM sessions').all();
    return new Map(rows.map(r => [r.id, {hash: r.content_hash, indexed: r.indexed_at}]));
  }
}
```

**Schema addition:**
```sql
ALTER TABLE sessions ADD COLUMN content_hash TEXT;
ALTER TABLE sessions ADD COLUMN indexed_at INTEGER;
```

**Pros:**
- Fast incremental updates
- Reliable (uses content hash, not mtime)
- Periodic full reindex for safety

**Cons:**
- Slightly more complex
- Needs to hash files

**When to use:** Production system

---

## Growth During Current Session

**Problem:** How to show data from current (incomplete) session?

### Option A: Ignore Until Complete
```typescript
async findCompletedSessions() {
  // Only index sessions not modified in last 5 minutes
  const cutoff = Date.now() - (5 * 60 * 1000);

  return files.filter(f => {
    const stats = statSync(f);
    return stats.mtimeMs < cutoff;
  });
}
```

**Pros:** Simple, no partial data
**Cons:** Current session not searchable

---

### Option B: Index Incrementally During Session
```typescript
// Re-index current session every N minutes
setInterval(async () => {
  const currentSession = getCurrentSessionId();
  await indexer.indexSession(currentSession);
  console.log('Current session re-indexed');
}, 5 * 60 * 1000);  // Every 5 minutes
```

**Pros:** Current session searchable
**Cons:** Re-indexes same data multiple times

---

### Option C: Stream Processing (Advanced)
```typescript
// Watch session file for changes
const watcher = watch(sessionFile);

for await (const event of watcher) {
  if (event.type === 'change') {
    // Only process new lines
    const newLines = await readNewLines(sessionFile);
    await indexer.indexLines(sessionId, newLines);
  }
}
```

**Pros:** Real-time updates, efficient
**Cons:** Complex, needs file watching

---

## Recommended Strategy

### For Your Use Case

```typescript
class SessionIndexer {
  async update() {
    // 1. Index new/modified sessions (content hash based)
    const toIndex = await this.findModifiedSessions();

    for (const sessionId of toIndex) {
      await this.indexSession(sessionId);
    }

    // 2. Re-index current session if active
    const current = this.getCurrentSession();
    if (current && !toIndex.includes(current)) {
      await this.indexSession(current);
    }
  }

  async findModifiedSessions(): Promise<string[]> {
    // Use content hash to detect real changes
    // Ignore mtime (gets touched without content changes)
  }

  getCurrentSession(): string | null {
    // Check which session was modified in last 5 minutes
    const files = readdirSync(SESSIONS_DIR);
    const recent = files.find(f => {
      const stats = statSync(join(SESSIONS_DIR, f));
      return Date.now() - stats.mtimeMs < 5 * 60 * 1000;
    });

    return recent ? recent.replace('.jsonl', '') : null;
  }
}
```

### Update Schedule

```bash
# On demand (when you want fresh data)
bun run scripts/update-index.ts

# Automatic (cron job, every 30 minutes)
*/30 * * * * cd /path/to/project && bun run scripts/update-index.ts

# Or: Hook into session lifecycle
# ~/.claude/hooks/SessionEnd.sh
bun run scripts/update-index.ts &  # Background update
```

---

## Size Growth Projection

```
Current: 20 sessions
  → Index: 200 KB
  → Embeddings: 5 MB

After 100 sessions:
  → Index: 1 MB
  → Embeddings: 25 MB
  → Query time: <10ms (still fast)

After 1,000 sessions:
  → Index: 10 MB
  → Embeddings: 250 MB (or 60 MB compressed)
  → Query time: ~10-20ms (still acceptable)

After 10,000 sessions:
  → Index: 100 MB
  → Embeddings: 2.5 GB (or 600 MB compressed)
  → Query time: ~50-100ms (need optimization)
```

**Recommendation:** Rebuild index fully once per week, incremental updates daily

---

## Summary

**Q: Do logs grow?**

**A: Yes, two ways:**
1. **During session:** Append-only, grows continuously
2. **After session:** Content stable, but mtime may change (metadata only)

**Indexing strategy:**
- **Simple:** Full reindex (~1-2 min for 1000 sessions)
- **Smart:** Incremental using content hash
- **Current session:** Re-index periodically or on-demand

**File watching not needed** - poll/check on demand is sufficient.

**mtime unreliable** - use content hash for change detection.
