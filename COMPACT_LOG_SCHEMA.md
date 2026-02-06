# Compact Agent Log Schema

Minimal JSONL schema for agent execution analysis. Keeps only what's needed, strips redundant metadata.

## Schema (v1.0)

One JSON object per line. Four entry types:

### 1. Prompt (User Input)
```json
{
  "t": "prompt",
  "ts": "2026-01-06T02:25:09",
  "text": "Full prompt text preserved..."
}
```

### 2. Tool Call (Agent Action)
```json
{
  "t": "tool",
  "ts": "2026-01-06T02:25:14",
  "name": "WebSearch",
  "params": {"query": "GitHub Copilot..."},
  "id": "toolu_01ABC...",
  "tok": {
    "in": 3,
    "out": 2,
    "c_r": 0,
    "c_w": 21311
  }
}
```

### 3. Tool Result
```json
{
  "t": "result",
  "ts": "2026-01-06T02:25:16",
  "tool_id": "toolu_01ABC...",
  "status": "ok",
  "size": 12345
}
```

### 4. Response (Agent Output)
```json
{
  "t": "response",
  "ts": "2026-01-06T02:25:30",
  "text": "Here's what I would create...",
  "tok": {
    "in": 500,
    "out": 1200,
    "c_r": 0,
    "c_w": 0
  }
}
```

## Field Glossary

| Field | Type | Description |
|-------|------|-------------|
| `t` | string | Entry type: `prompt` \| `tool` \| `result` \| `response` |
| `ts` | string | ISO 8601 timestamp, truncated to seconds |
| `text` | string | Full prompt or response text |
| `name` | string | Tool name (WebSearch, Read, Write, Bash, etc.) |
| `params` | object | Minimal tool parameters (see below) |
| `id` | string | Tool use ID (for matching results) |
| `tool_id` | string | Tool use ID that this result belongs to |
| `status` | string | `ok` \| `error` |
| `size` | number | Size of result in characters (not content itself) |
| `tok` | object | Token usage breakdown |

### Token Object (`tok`)

| Field | Description |
|-------|-------------|
| `in` | Input tokens |
| `out` | Output tokens |
| `c_r` | Cache read tokens |
| `c_w` | Cache write (creation) tokens |

### Tool Parameters by Type

**WebSearch:**
```json
{"query": "search terms"}
```

**Read:**
```json
{"file": "/path/to/file"}
```

**Write:**
```json
{"file": "/path/to/file"}
```

**Bash:**
```json
{"cmd": "ls -la"}  // truncated at 100 chars if longer
```

## What Gets Removed

- ❌ Full tool results (e.g., HTML from WebSearch, file contents from Read)
- ❌ UUIDs, session IDs, request IDs
- ❌ Version numbers, branch names, metadata
- ❌ Redundant context in every entry
- ❌ Parent/child relationships

## What's Preserved

- ✅ Full prompt text
- ✅ Tool names and parameters
- ✅ Token usage (all 4 types)
- ✅ Timestamps (for duration calculation)
- ✅ Tool IDs (for matching calls to results)
- ✅ Response text (truncated at 500 chars)

## Compression Ratio

Typical: **90-95% smaller**
- Full log: 100-300 KB
- Compact log: 5-15 KB

## Usage

```bash
# Compact single agent log
./scripts/compact-agent-log.py ~/.claude/projects/.../agent-a7c3dfb.jsonl

# Creates: agent-a7c3dfb.compact.jsonl

# Specify output location
./scripts/compact-agent-log.py input.jsonl output.jsonl
```

## Analysis Compatible

The compact schema preserves all metrics needed for:
- Token usage analysis (input/output/cached)
- Tool usage patterns (Read, Write, WebSearch, Bash counts)
- Execution duration (first/last timestamp)
- Efficiency metrics (tokens/tool, tokens/sec)
- Behavioral signatures (what tools, what order)
- Web search queries (full query text preserved)
- Files created (file paths preserved)

## Example: Full vs Compact

**Full log entry (995 chars):**
```json
{
  "parentUuid": "ac9d2d4c-610b-4ff3-8bac-710546c84520",
  "isSidechain": true,
  "userType": "external",
  "cwd": "/Users/bln/play/agentic-primer",
  "sessionId": "a6214a01-0396-4893-b5ef-eac084cb9ff6",
  "version": "2.0.76",
  "gitBranch": "genesis",
  "agentId": "a7c3dfb",
  "slug": "jaunty-bubbling-truffle",
  "message": {
    "model": "claude-sonnet-4-5-20250929",
    "id": "msg_013uNzxyW51yHRfP2MGfzFQz",
    "type": "message",
    "role": "assistant",
    "content": [
      {
        "type": "tool_use",
        "id": "toolu_01UARbm9LFjzoMxm8aYSvaot",
        "name": "WebSearch",
        "input": {"query": "GitHub Copilot issue automation auto-review 2026"}
      }
    ],
    "usage": {
      "input_tokens": 3,
      "cache_creation_input_tokens": 21311,
      "cache_read_input_tokens": 0,
      "output_tokens": 2
    }
  },
  "requestId": "req_011CWqLAm5QUMDq6ikj3rDUj",
  "type": "assistant",
  "uuid": "ac9d2d4c-610b-4ff3-8bac-710546c84520",
  "timestamp": "2026-01-06T02:25:14.635Z"
}
```

**Compact log entry (176 chars, 82% smaller):**
```json
{"t":"tool","ts":"2026-01-06T02:25:14","name":"WebSearch","params":{"query":"GitHub Copilot issue automation auto-review 2026"},"id":"toolu_01UARbm9LFjzoMxm8aYSvaot","tok":{"in":3,"out":2,"c_r":0,"c_w":21311}}
```

## Version History

**v1.0** (2026-01-06)
- Initial schema
- Four entry types: prompt, tool, result, response
- Token breakdown with cache metrics
- Tool parameter extraction by type
- Response text truncation at 500 chars
