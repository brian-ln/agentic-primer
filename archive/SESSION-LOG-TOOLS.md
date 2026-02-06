# Session Log Compaction Tools

This directory contains tools for compacting Claude Code session logs into a minimal, replay-capable format.

## The Problem

Claude Code session logs can become extremely large:
- **Full session log**: ~1.8MB for a single session
- **With agent logs**: 4+ MB total
- **Bloat sources**:
  - Repetitive metadata on every line (cwd, branch, sessionId, version, etc.)
  - Full file contents in tool results (Read operations, web searches)
  - Large thinking blocks
  - Empty/null system messages
  - File version hashes

## The Solution

**`compact-session-log.py`** - Compacts session and agent logs into a minimal format:
- **94.5% size reduction** (4.2MB → 230KB)
- **Chronologically interleaved** - Merges main session + all agent logs by timestamp
- **Tool summarization** - Records tool use without full content
- **Context tracking** - Only records changes to branch/cwd, not on every line
- **Replay capable** - Contains everything needed to reconstruct the conversation

**`view-session.py`** - Human-readable viewer for compact logs:
- Color-coded roles (user/assistant/system)
- Agent identification
- Tool use visualization
- Optional filtering by agent
- Optional thinking block display

## Files

### Analysis
- **`ANALYSIS.md`** - Detailed analysis of log structure and compaction strategy

### Tools
- **`compact-session-log.py`** - Log compactor (session + agents → compact JSONL)
- **`view-session.py`** - Human-readable log viewer

### Output
- **`session-compact.jsonl`** - Compacted current session log

## Usage

### Compact a Session

```bash
./compact-session-log.py \
  /Users/bln/.claude/projects/-Users-bln-play-agentic-primer/SESSION_ID.jsonl \
  session-compact.jsonl
```

This will:
1. Find all related agent logs in the same directory
2. Merge them chronologically by timestamp
3. Compact to minimal format
4. Report size reduction stats

**Output:**
```
Original size: 4,195,092 bytes
Compact size: 230,379 bytes
Reduction: 94.5%
Entries processed: 1069
```

### View a Compact Log

```bash
# View full log
./view-session.py session-compact.jsonl

# View with thinking blocks
./view-session.py --thinking session-compact.jsonl

# Filter by specific agent
./view-session.py --agent a96518c session-compact.jsonl
```

**Example output:**
```
================================================================================
Session: a6214a01...
Started: 2026-01-06T00:42:01.897Z
CWD: /Users/bln/play/agentic-primer
Branch: main
CC Version: 2.0.76
================================================================================

[00:42:01] 👤 USER
  I need to create some content. Iterate on it with me.

[00:42:11] 🤖 ASSISTANT
  [thinking...]

[00:42:15] 🤖 ASSISTANT
  I like this concept - a git-native workflow automation system...

[00:46:06] 🤖 ASSISTANT
  🔧 Write → /Users/bln/play/agentic-primer/BOOTLOADER.md (8304 bytes)

[00:46:06] 👤 USER
  ✅ Result: 74 bytes
```

## Compact Log Format

### Header Line
```json
{
  "v": 1,
  "format": "compact-session-log",
  "session": "a6214a01-0396-4893-b5ef-eac084cb9ff6",
  "cwd": "/Users/bln/play/agentic-primer",
  "branch": "main",
  "started": "2026-01-06T00:42:01.897Z",
  "cc_version": "2.0.76"
}
```

### Entry Types

**User message:**
```json
{
  "t": "2026-01-06T00:42:01.897Z",
  "r": "user",
  "m": "I need to create some content..."
}
```

**Assistant message:**
```json
{
  "t": "2026-01-06T00:42:15.254Z",
  "r": "assistant",
  "m": "I like this concept..."
}
```

**Thinking block:**
```json
{
  "t": "2026-01-06T00:42:11.378Z",
  "r": "assistant",
  "thinking": true
}
```

**Tool use:**
```json
{
  "t": "2026-01-06T00:46:06.226Z",
  "r": "assistant",
  "tool": "Write",
  "file": "/Users/bln/play/agentic-primer/BOOTLOADER.md",
  "size": 8304
}
```

**Tool result:**
```json
{
  "t": "2026-01-06T00:46:06.665Z",
  "r": "user",
  "status": "success",
  "size": 74,
  "tool_result": true
}
```

**Agent work:**
```json
{
  "t": "2026-01-06T01:12:21.500Z",
  "r": "assistant",
  "agent": "a96518c",
  "tool": "Task",
  "task": "ok, let's capture..."
}
```

**Context change:**
```json
{
  "ctx": "branch",
  "v": "genesis",
  "t": "2026-01-06T02:04:10.499Z"
}
```

## Tool Summarization

Instead of storing full file contents, tool results are summarized:

| Tool | What's Stored | What's Omitted |
|------|---------------|----------------|
| Read | File path | Full file content |
| Write/Edit | File path, size | Full file content |
| Grep/Glob | Pattern | Full match results |
| Bash | Command (truncated to 100 chars), exit code | Full stdout/stderr |
| WebSearch | Query | Full search results |
| WebFetch | URL | Full HTML content |
| Task (agents) | Task prompt (100 chars), agent ID | Full agent output |

## Replay Capability

The compact log contains everything needed to replay/reconstruct the conversation:

1. **Who said what, when** - All user/assistant messages with timestamps
2. **Tool interactions** - What tools were called, with what params, what they returned
3. **Agent work** - When agents were launched, what they did, results
4. **Context changes** - Branch switches, directory changes
5. **Chronological ordering** - Main session + all agents interleaved by timestamp

**What's NOT needed for replay:**
- UUIDs (can use sequential IDs)
- File version hashes
- Repetitive session metadata
- Thinking signatures
- Full tool result content
- Empty system messages
- Queue operation internals

## Statistics

For the current session (a6214a01):

| Metric | Original | Compact | Reduction |
|--------|----------|---------|-----------|
| **Size** | 4.2 MB | 230 KB | **94.5%** |
| **Entries** | 1069 | 991 | 7.3% |
| **Session log** | 1.8 MB | - | - |
| **Agent logs** | 2.4 MB | - | - |

**Largest entry in original**: 329 KB (tool_result with agent output)
**Largest entry in compact**: <2 KB (truncated message)

## Implementation Notes

### Timestamp Parsing
- All timestamps are ISO 8601 with 'Z' suffix
- Converted to timezone-aware datetime objects for sorting
- Entries without timestamps are skipped

### Agent Integration
- Script automatically finds all `agent-*.jsonl` files in same directory
- Agent entries tagged with `"agent": "ID"`
- Chronologically merged with main session

### Truncation Rules
- User/assistant messages: Truncated at 1000 chars (preserves preview)
- Bash commands: Truncated at 100 chars
- Tool prompts: Truncated at 100 chars
- Thinking blocks: Flagged only (content omitted unless needed)

### Empty Entry Handling
- System messages with null content: Skipped
- Summary markers with null timestamp: Skipped
- Queue operations: Skipped (internal state, not user-visible)

## Future Enhancements

Possible improvements:
1. **Delta compression** - Only store changed fields
2. **Message deduplication** - Detect repeated warmup messages
3. **Semantic summarization** - AI-generated summaries of long messages
4. **Binary format** - Further compression with binary encoding
5. **Indexing** - Add seek indices for fast random access
6. **Reconstruction tool** - Replay compact log back to full format
7. **Diff viewer** - Compare two sessions side-by-side

## License

These tools are part of the agentic-primer project.
