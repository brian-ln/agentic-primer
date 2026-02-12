# Session Log Waste Analysis

## Executive Summary

**Current: 474KB → Optimized: 28KB = 94% waste**

But "waste" needs context. Here's what the data actually shows:

---

## The Numbers

### Total Breakdown (aeb742bc session)
```
Total file size:      474 KB
├─ User messages:     422 KB (89%) ← THE BULK
├─ Assistant msgs:     39 KB (8%)
├─ Progress events:   0.5 KB (0.1%)
└─ Queue ops:         0.1 KB (0.02%)
```

### Message Distribution
```
Total events:         2,532
Unique messages:         48
Duplication factor:    52.7x
```

### Event Sizes
```
User message:     ~15 KB avg  (range: 7-55 KB)
Assistant msg:     ~1 KB avg
Progress event:  ~500 bytes
```

### Content Breakdown (per assistant event ~1KB)
```
Message content:   525 bytes (77%)  ← Actual conversation
Token usage:        42 bytes (6%)   ← Cost tracking
Metadata:           32 bytes (5%)   ← cwd, branch, version
IDs/timestamps:     28 bytes (4%)   ← Event identity
```

---

## What Can Be Rolled Up?

### ✅ Can Deduplicate (store once per session)

**1. Session metadata (32 bytes × 2,532 = 81KB → 100 bytes)**
```json
{
  "sessionId": "aeb742bc...",
  "cwd": "/Users/bln/play/agentic-primer/simplify",
  "gitBranch": "main",
  "version": "2.1.19",
  "userType": "external",
  "isSidechain": false
}
```
**Savings: ~81KB (17%)**

**2. Duplicate messages (52.7x repetition)**
- Same message.id appears 2-5 times
- Each time with full content
- **Savings: ~450KB (95%)**

**Why duplicated?** Streaming - each content block logged separately

---

## What Cannot Be Rolled Up?

### ❌必须 Keep Unique Per Event

**1. Event identity (28 bytes each)**
```json
{
  "uuid": "unique-per-event",
  "timestamp": "2026-01-27T19:28:22.477Z",
  "parentUuid": "links-to-parent"
}
```
**Why?** Needed for event ordering and threading

**2. Message content (varies greatly)**
```
Text blocks:     100-500 bytes
Tool use:        150-200 bytes
Tool results:    varies (can be KB)
User prompts:    7-55 KB (includes context)
```
**Why?** This is the actual data

**3. Token usage (42 bytes per assistant message)**
```json
{
  "input_tokens": 2,
  "cache_read_input_tokens": 13818,
  "cache_creation_input_tokens": 11912,
  "output_tokens": 3
}
```
**Why?** Varies per API call, needed for cost tracking

---

## Are Tiny Fields "Bad"?

### Storage Math

**Scenario: One assistant message**
```
Total size:           1,025 bytes
├─ Message content:     525 bytes (51%)
├─ Token usage:          42 bytes (4%)
├─ Metadata:             32 bytes (3%)  ← "tiny field"
└─ IDs/timestamps:       28 bytes (3%)  ← "tiny field"
```

**32-byte metadata when content is 525 bytes = 6% overhead**

**But wait...**

**With 52.7x duplication:**
```
Same message appears 52 times
Metadata waste:    32 × 52 = 1,664 bytes
Content waste:    525 × 52 = 27,300 bytes
```

**Tiny fields become noise in the presence of massive duplication.**

---

## What IS "Waste"?

### 1. **Storage Waste** (Disk space)
```
Current:     474 KB
Needed:       28 KB
Waste:       446 KB (94%)
```

**But:** Disk is cheap. 474KB is nothing.

**Verdict: Not a real problem**

---

### 2. **Processing Waste** (CPU/Memory)

**Loading the file:**
```bash
cat session.jsonl | jq ...
```
- Parses 2,532 events
- Only 48 unique messages
- **52x more work than necessary**

**Example queries:**
```bash
# Get unique messages
cat session.jsonl | jq 'select(.message.id)' |
  jq -s 'unique_by(.message.id)'
# Must parse 474KB, dedupe 2,532 events → 48 results
```

**Verdict: Real problem for querying**

---

### 3. **Cognitive Waste** (Hard to understand)

**Raw log:**
```
Line 100: {message: "Verify spec...", content: ["text"]}
Line 150: {message: "Verify spec...", content: ["tool_use"]}  ← same message!
Line 200: {message: "Verify spec...", content: ["tool_use"]}  ← again!
```

**Human reading this:** "Wait, did it say that 3 times or is this streaming?"

**Verdict: Real problem for debugging**

---

### 4. **Query Waste** (Slow insights)

**Without deduplication:**
```bash
# How many messages in this session?
cat session.jsonl | jq 'select(.type == "user" or .type == "assistant")' | wc -l
# → 2,532 events

# No wait, unique messages:
cat session.jsonl | jq -r '.message.id // .uuid' | sort -u | wc -l
# → 48 messages
```

**With index:**
```bash
jq '.messageCount' sessions-index.json
# → 13 (turns out this is the real count!)
```

**Verdict: Real problem, but solvable with indexing**

---

## Optimization Strategy

### What to Optimize

| Field | Current | Optimized | Savings | Priority |
|-------|---------|-----------|---------|----------|
| Duplicate messages | 474KB | 28KB | 94% | 🔥 HIGH |
| Repeated metadata | 81KB | 0.1KB | 99.9% | 💡 MEDIUM |
| Token usage | 9.7KB | 9.7KB | 0% | ✅ KEEP |
| IDs/timestamps | 11KB | 11KB | 0% | ✅ KEEP |
| User prompts | 422KB | 422KB | 0% | ✅ KEEP |

### Recommended Approach

**Don't create new storage format. Instead:**

**1. Deduplication on read (current approach ✅)**
```bash
./scripts/analyze-session.sh transcript <id>
# Dedupes by message.id, shows 48 messages not 2,532
```

**2. Add cached index (for speed)**
```json
// .cache/session-<id>.json
{
  "uniqueMessages": 48,
  "totalEvents": 2532,
  "agents": [...],
  "filesModified": [...],
  "toolsUsed": {...}
}
```
Generated on first read, reused later

**3. Keep raw logs as-is (for debugging)**
- Full fidelity
- Can always go back to source
- No data loss

---

## Answer to "Are tiny fields bad?"

**No.**

**Breakdown of 474KB:**
```
User prompts (large):           422 KB (89%)  ← Cannot optimize
Message duplication:             39 KB (8%)   ← CAN optimize (dedup)
Everything else (including
  "tiny" metadata fields):       13 KB (3%)   ← Not worth optimizing
```

**The problem isn't tiny fields. The problem is:**
1. **52x message duplication** (streaming artifacts)
2. **Large user prompts** (necessary - they contain context)

**The tiny metadata fields (32 bytes) are 0.007% of the file.**

---

## Recommendations

### ✅ Do This
1. **Deduplicate on read** - Already implemented ✅
2. **Cache analysis results** - Generate once, reuse
3. **Index sessions** - Already have sessions-index.json ✅
4. **Query tools** - Already have analyze-session.sh ✅

### ❌ Don't Do This
1. ~~Create new storage format~~ - Raw logs are fine
2. ~~Remove metadata fields~~ - They're 3% overhead
3. ~~Compress logs~~ - 474KB is tiny, gzip would add complexity
4. ~~Normalize database~~ - Overkill for this use case

### 🤔 Consider This
1. **Stream processing** - Don't load entire file into memory
2. **Incremental indexing** - Update cache as session runs
3. **Query API** - Structured access instead of jq scripts

---

## Conclusion

**"Waste" is relative:**

- **Storage**: 94% waste, but 474KB is negligible
- **Processing**: 52x redundant parsing is real cost
- **Cognitive**: Hard to read raw logs
- **Tiny fields**: Not the problem (< 3% of data)

**Real data:**
- 89% is user prompts (necessary)
- 8% is duplicate assistant messages (fixable with dedup)
- 3% is everything else (acceptable overhead)

**Solution: Don't change storage, improve querying.**

The logs are fine. We just need better tools to read them.
