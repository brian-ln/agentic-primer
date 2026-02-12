# Session Log Schema - Complete Field Catalog

## Event Types

### 1. `type: "user"` - User messages
```typescript
{
  type: "user",
  uuid: string,              // Unique event ID
  timestamp: string,         // ISO 8601 timestamp
  sessionId: string,         // Session this belongs to
  parentUuid: string | null, // Links to previous event in conversation thread

  // Message content
  message: {
    role: "user",
    content: string | Array<{type: "text", text: string} | {type: "image", source: ...}>
  },

  // Context (repeated in every event)
  cwd: string,              // Working directory
  gitBranch: string,        // Git branch
  version: string,          // Claude CLI version
  userType: string,         // "external" typically
  isSidechain: boolean,     // If this is a sidechain session
  permissionMode: string    // "default" or permission level
}
```

**Valuable fields**: `uuid`, `timestamp`, `message`, `parentUuid`
**Redundant**: `cwd`, `gitBranch`, `version` (same for entire session)
**Noise**: `userType`, `isSidechain`, `permissionMode` (rarely changes)

---

### 2. `type: "assistant"` - Assistant responses
```typescript
{
  type: "assistant",
  uuid: string,
  timestamp: string,
  sessionId: string,
  parentUuid: string | null,
  requestId: string,        // API request ID

  // Claude API response
  message: {
    id: string,             // Message ID (msg_xxx) - KEY FOR DEDUPLICATION
    model: string,          // "claude-opus-4-5-20251101"
    role: "assistant",
    type: "message",
    stop_reason: string | null,    // "end_turn", "max_tokens", "stop_sequence"
    stop_sequence: string | null,

    // Content blocks (streaming = multiple events with same message.id)
    content: Array<
      | {type: "text", text: string}
      | {type: "tool_use", id: string, name: string, input: object}
      | {type: "tool_result", tool_use_id: string, content: string | object}
    >,

    // Token usage and cost data
    usage: {
      input_tokens: number,
      output_tokens: number,
      cache_creation_input_tokens: number,    // Prompt cache writes
      cache_read_input_tokens: number,        // Prompt cache reads
      cache_creation: {
        ephemeral_5m_input_tokens: number,
        ephemeral_1h_input_tokens: number
      },
      service_tier: "standard"
    }
  },

  // Same context fields as user events
  cwd: string,
  gitBranch: string,
  version: string,
  userType: string,
  isSidechain: boolean
}
```

**CRITICAL**: Same `message.id` appears in 2-5 events (streaming chunks)
**Valuable fields**: `message.id`, `message.content`, `message.usage`, `timestamp`, `uuid`
**Deduplication key**: `message.id` (same message logged multiple times)
**Cost tracking**: `message.usage` (token counts for billing)

---

### 3. `type: "progress"` - Tool execution and hooks
```typescript
{
  type: "progress",
  uuid: string,
  timestamp: string,
  sessionId: string,
  parentUuid: string | null,
  parentToolUseID: string,      // Links to tool being executed
  toolUseID: string,            // Tool use ID

  data: {
    type: "hook_progress",
    hookEvent: string,          // "SessionStart", "ToolUse", etc.
    hookName: string,           // Name of hook being run
    command: string             // Shell command executed by hook
  },

  // Context
  cwd: string,
  gitBranch: string,
  version: string,
  userType: string,
  isSidechain: boolean
}
```

**Valuable fields**: `data` (hook execution details), `parentToolUseID` (links to tool)
**Use case**: Debugging hooks, understanding tool execution timeline

---

### 4. `type: "file-history-snapshot"` - File state tracking
```typescript
{
  type: "file-history-snapshot",
  messageId: string,
  timestamp: string,
  sessionId: string,
  isSnapshotUpdate: boolean,

  snapshot: {
    messageId: string,
    timestamp: string,
    trackedFileBackups: {
      [filePath: string]: {
        content: string,
        lastModified: number
      }
    }
  }
}
```

**Valuable fields**: `snapshot.trackedFileBackups` (file versions)
**Use case**: File history, rollback capability, understanding what changed

---

### 5. `type: "queue-operation"` - Session queue management
```typescript
{
  type: "queue-operation",
  operation: "enqueue" | "dequeue",
  timestamp: string,
  sessionId: string
}
```

**Valuable fields**: `operation`, `timestamp`
**Use case**: Understanding session lifecycle, concurrent session handling

---

## Agent Log Fields (Subagents)

Agent logs have **same structure** as session logs, plus:

```typescript
{
  agentId: string,                    // Short agent ID (e.g., "a3584f4")
  sessionId: string,                  // Parent session
  parentToolUseID: string | null,     // Tool use that spawned this agent
  sourceToolAssistantUUID: string | null,

  // ... rest same as session logs
}
```

**Key relationship**: `agentId` links agent log to session's tool result

---

## Session Index Fields

```typescript
{
  sessionId: string,
  fullPath: string,             // Path to .jsonl file
  fileMtime: number,            // File modification time
  created: string,              // ISO timestamp
  modified: string,             // ISO timestamp
  messageCount: number,         // Total messages in session
  gitBranch: string,
  projectPath: string,
  isSidechain: boolean,

  // AI-generated metadata
  summary: string,              // Human-readable session summary
  firstPrompt: string           // First user message (truncated)
}
```

**All fields valuable** - this is already optimized metadata

---

## Tool Use/Result Structure

### Tool Use (in assistant messages)
```typescript
{
  type: "tool_use",
  id: string,                  // toolu_xxx
  name: string,                // "Task", "Read", "Grep", "Edit", etc.
  input: {
    // Tool-specific parameters
    // Task example:
    subagent_type: string,
    prompt: string,
    description: string,

    // Read example:
    file_path: string,

    // Edit example:
    file_path: string,
    old_string: string,
    new_string: string
  }
}
```

### Tool Result (in assistant messages)
```typescript
{
  type: "tool_result",
  tool_use_id: string,         // Links to tool_use.id
  content: string | Array<{
    type: "text",
    text: string               // May include "agentId: xxx" for Task tool
  }>
}
```

**Valuable**: All of it - shows what was done and results

---

## Summary: Information Tiers

### 🔥 Critical (Always extract)
- `message.id` - Deduplication key
- `message.content` - Actual conversation
- `timestamp` - Timeline
- `uuid` - Event identity
- `agentId` - Agent relationship
- `tool_use` / `tool_result` - What was done
- `message.usage` - Cost tracking

### 💡 Valuable (Extract for analysis)
- `parentUuid` - Conversation threading
- `parentToolUseID` - Tool execution tree
- `message.model` - Which model used
- `message.stop_reason` - Why response ended
- `snapshot.trackedFileBackups` - File history
- `data.hookEvent` - Hook execution

### 📊 Metadata (Extract once per session)
- `cwd`, `gitBranch`, `version` - Session context
- `sessionId`, `projectPath` - Identity
- `summary`, `firstPrompt` - Session index

### 🗑️ Noise (Can ignore for most use cases)
- `userType`, `isSidechain`, `permissionMode` - Rarely useful
- `stop_sequence` - Usually null
- `requestId` - Only for API debugging
- `fileMtime` - Redundant with timestamp

---

## Derived Insights (Not stored, but computable)

```typescript
// From session logs
sessionCost: number           // Sum of all usage.input_tokens * pricing
sessionDuration: number       // last.timestamp - first.timestamp
toolsUsed: Map<string, number>    // Tool name -> count
filesModified: string[]       // From Edit/Write tool uses
agentsSpawned: AgentInfo[]    // From Task tool uses + results

// From agent relationships
agentTree: TreeNode[]         // Parent-child agent relationships
parallelAgents: string[][]    // Groups of agents run in parallel

// From file snapshots
fileHistory: Map<string, Version[]>  // File -> timeline of changes

// From token usage
cacheEfficiency: number       // cache_read / (cache_read + input)
costByTool: Map<string, number>      // Tool -> accumulated cost
```

---

## What's Actually Needed?

**For understanding "what we did":**
- Session summary + first prompt
- Unique messages (deduplicated by message.id)
- Tool uses and results
- Agent spawns and results
- Files modified

**For cost analysis:**
- Token usage per message
- Model used
- Cache hit rates

**For debugging:**
- Full event stream with timestamps
- Hook execution (progress events)
- Tool execution tree (parentToolUseID)

**For file recovery:**
- File history snapshots

**Question: What use cases do you have in mind? That will help prioritize which fields matter most.**
