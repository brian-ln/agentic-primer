# Session Log Analysis & Replay Format Plan

## Current State Analysis

### Session Log: `a6214a01-0396-4893-b5ef-eac084cb9ff6.jsonl`
- **Total entries:** 526 lines
- **Largest entry:** 329KB (line 293 - tool_result with 101KB content)
- **19 entries** over 10KB in size

### Agent Logs: Multiple `agent-*.jsonl` files
- Current agent: `agent-afa7308.jsonl` (21 lines)
- Previous agents have hundreds of lines

### Entry Types Found
1. **user** - User messages (external or tool results)
2. **assistant** - Assistant responses (thinking, text, tool_use)
3. **system** - System messages (usually empty content)
4. **summary** - Summary markers (null timestamp)
5. **queue-operation** - Queue state changes

### Bloat Sources
1. **Repetitive fields** on every entry:
   - `cwd`, `gitBranch`, `sessionId`, `version`, `userType`, `isSidechain`
   - These rarely change - could be session metadata

2. **Tool results** with massive content:
   - File contents from Read operations
   - Web search results
   - Agent output logs
   - Often 10KB-329KB per entry

3. **Thinking blocks** - Large but may be useful for replay

4. **Empty/null entries**:
   - System messages with null content
   - Summary markers with null timestamp

5. **File version tracking** in tool_use:
   - Not essential for conversation replay

## Replay Requirements

To completely replay a conversation, we need:

### Minimal Essential Data
1. **Who said what, when**
   - Role (user/assistant/system)
   - Timestamp
   - Content (text only for display)

2. **Tool interactions** (for understanding flow):
   - Tool name
   - Tool input (summary, not full params)
   - Tool result (summary, not full content)

3. **Agent spawning** (to track parallel work):
   - When agents launched
   - What they were asked to do
   - Their results

4. **Context switches**:
   - Branch changes
   - Directory changes
   - Agent transitions

### Data We DON'T Need for Replay
1. File version hashes
2. Full file contents in tool results (can note "Read file X" instead)
3. Repetitive session metadata on every line
4. Thinking signatures/hashes
5. Queue operation internals
6. Empty system messages
7. UUIDs (can use sequential IDs)

## Proposed Minimal Format

### Structure
```jsonl
{"v": 1, "session": "a6214a01", "cwd": "/Users/bln/play/agentic-primer", "branch": "main", "started": "2026-01-06T00:42:01.897Z"}
{"t": "2026-01-06T00:42:01.897Z", "r": "user", "m": "I need to create some content..."}
{"t": "2026-01-06T00:42:11.378Z", "r": "assistant", "thinking": true}
{"t": "2026-01-06T00:42:15.254Z", "r": "assistant", "m": "I like this concept..."}
{"t": "2026-01-06T00:46:06.226Z", "r": "assistant", "tool": "Write", "args": {"file": "BOOTLOADER.md", "size": 1234}}
{"t": "2026-01-06T00:46:06.665Z", "r": "user", "tool_result": "Write", "status": "success", "msg": "File created"}
{"t": "2026-01-06T01:12:21.500Z", "r": "agent", "agent": "a96518c", "task": "ok, let's capture..."}
{"t": "2026-01-06T01:33:40.382Z", "r": "agent_result", "agent": "a96518c", "status": "timeout", "size": 101934}
{"ctx": "branch", "v": "genesis", "t": "2026-01-06T02:04:10.499Z"}
```

### Field Abbreviations
- `v` = version
- `t` = timestamp
- `r` = role
- `m` = message
- `ctx` = context change
- `tool` = tool name
- `args` = tool arguments (summarized)

### Tool Result Summarization
- `Read` → Note file path, show size not content
- `Grep/Glob` → Show pattern, count matches not full results
- `WebSearch` → Show query, count results not full HTML
- `WebFetch` → Show URL, response size
- `Bash` → Show command, exit code, output size

### Agent Handling
- Launch: Record task prompt
- Results: Record status, output size, key findings (not full output)
- Interleave agent events with main session chronologically

## Implementation Plan

1. **Parse both session and agent logs**
   - Read main session JSONL
   - Find all related agent JONSLs
   - Merge by timestamp

2. **Extract session metadata** (one-time header)
   - sessionId, cwd, gitBranch, version
   - Start timestamp

3. **Process each entry:**
   - User messages: Keep text, truncate if > 1KB
   - Assistant text: Keep full text
   - Assistant thinking: Flag only (don't include content unless replay needs it)
   - Tool use: Summarize args (file paths, commands, queries)
   - Tool results: Summarize (success/fail, sizes, not content)
   - Agent launches: Record task
   - Agent results: Record status, size, key findings

4. **Track context changes:**
   - Branch switches
   - Directory changes
   - Agent transitions

5. **Write minimal JSONL:**
   - Header line with session metadata
   - Chronologically merged entries
   - Abbreviated fields
   - Summarized tool interactions

## Size Reduction Estimate

Current: ~1.8MB session log + agent logs
Expected: < 100KB (95%+ reduction)

Reason: Most bloat is file contents, web results, repeated metadata
