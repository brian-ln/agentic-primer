# In-Memory Session Analysis Model

## Design Goals

1. **Deduplicate** - Store each message once
2. **Index** - Fast lookups by common queries
3. **Link** - Preserve relationships (parent-child, agent-session)
4. **Separate hot/cold** - Frequently accessed vs rarely accessed data

---

## Core Data Structure

```typescript
class SessionAnalyzer {
  // === IDENTITY ===
  sessionId: string;
  created: Date;
  modified: Date;

  // === METADATA (stored once) ===
  meta: {
    cwd: string;
    gitBranch: string;
    version: string;
    projectPath: string;
  };

  // === MESSAGES (deduplicated by ID) ===
  messages: Map<string, Message>;  // messageId → Message

  // === EVENTS (timeline with references) ===
  events: Event[];  // Chronological, points to messages

  // === RELATIONSHIPS ===
  agents: Map<string, Agent>;           // agentId → Agent
  tools: Map<string, ToolExecution>;    // toolUseId → ToolExecution
  files: Map<string, FileHistory>;      // filePath → changes

  // === INDEXES (for fast queries) ===
  index: {
    byTimestamp: Map<number, Event[]>;        // timestamp → events
    byTool: Map<string, ToolExecution[]>;     // toolName → executions
    byAgent: Map<string, Event[]>;            // agentId → events
    byFile: Map<string, ToolExecution[]>;     // filePath → modifications
    conversationTree: TreeNode;               // parentUuid hierarchy
  };

  // === DERIVED (computed once, cached) ===
  cache: {
    totalCost?: number;
    duration?: number;
    summary?: string;
  };
}
```

---

## Message Storage (Deduplicated)

```typescript
// Instead of 52 copies of same message, store ONCE
type Message = {
  id: string;                    // msg_xxx or uuid
  role: "user" | "assistant";
  timestamp: Date;

  // Content blocks (all blocks for this message)
  content: ContentBlock[];

  // API metadata (assistant only)
  model?: string;
  usage?: TokenUsage;
  stopReason?: string;
};

type ContentBlock =
  | { type: "text", text: string }
  | { type: "tool_use", id: string, name: string, input: any }
  | { type: "tool_result", toolUseId: string, content: any };

messages: Map<string, Message> = new Map([
  ["msg_01ABC", { id: "msg_01ABC", role: "assistant", content: [...] }],
  ["uuid-123", { id: "uuid-123", role: "user", content: [...] }]
]);
```

**Benefit**: 48 messages instead of 2,532 events

---

## Event Timeline (References to Messages)

```typescript
// Timeline of events (NOT duplicating message content)
type Event = {
  uuid: string;
  timestamp: Date;
  type: "user" | "assistant" | "progress" | "agent_result";

  // Reference to deduplicated message (not full copy!)
  messageRef?: string;  // Points to messages.get(id)

  // Event-specific data
  parentUuid?: string;
  agentId?: string;
  data?: any;
};

events: Event[] = [
  { uuid: "e1", timestamp: t1, type: "user", messageRef: "uuid-123" },
  { uuid: "e2", timestamp: t2, type: "assistant", messageRef: "msg_01ABC" },
  { uuid: "e3", timestamp: t3, type: "agent_result", agentId: "a3584f4", data: {...} }
];
```

**Benefit**: Timeline preserved, but message content not duplicated

---

## Agent Relationships

```typescript
type Agent = {
  id: string;              // "a3584f4"
  type: string;            // "Explore"
  task: string;            // Human-readable task description

  // Links
  sessionId: string;       // Parent session
  parentToolUseId?: string;

  // Timeline
  spawned: Date;
  completed?: Date;

  // Results
  outcome: "success" | "failure" | "running";
  summary: string;

  // Nested conversation (if we loaded agent logs)
  messages?: Map<string, Message>;
  events?: Event[];
};

agents: Map<string, Agent> = new Map([
  ["a3584f4", {
    id: "a3584f4",
    type: "Explore",
    task: "Verify spec accuracy",
    spawned: new Date("2026-01-27T19:28:24Z"),
    outcome: "success",
    summary: "Found 388 tests. Spec claims 388. ✓"
  }]
]);
```

---

## Tool Execution Tracking

```typescript
type ToolExecution = {
  id: string;              // tool_use_id
  name: string;            // "Read", "Edit", "Task", etc.
  timestamp: Date;

  // Input/Output
  input: any;
  result?: any;

  // Relationships
  messageId: string;       // Which message requested this
  agentId?: string;        // Which agent spawned (if Task tool)

  // File operations
  filePath?: string;       // For Read, Edit, Write

  // Derived
  duration?: number;
  success: boolean;
};

tools: Map<string, ToolExecution> = new Map([
  ["toolu_01XYZ", {
    id: "toolu_01XYZ",
    name: "Task",
    timestamp: t1,
    input: { subagent_type: "Explore", description: "Verify spec" },
    agentId: "a3584f4",
    success: true
  }],
  ["toolu_02ABC", {
    id: "toolu_02ABC",
    name: "Edit",
    timestamp: t2,
    input: { file_path: "src/core.ts", ... },
    filePath: "src/core.ts",
    success: true
  }]
]);
```

---

## File History

```typescript
type FileHistory = {
  path: string;

  // All modifications in this session
  modifications: Array<{
    timestamp: Date;
    toolUseId: string;
    operation: "read" | "write" | "edit";
    snapshot?: string;      // Content before change (from file-history-snapshot)
  }>;

  // Current state
  currentContent?: string;
};

files: Map<string, FileHistory> = new Map([
  ["src/core.ts", {
    path: "src/core.ts",
    modifications: [
      { timestamp: t1, toolUseId: "toolu_02ABC", operation: "edit" }
    ]
  }]
]);
```

---

## Indexes for Fast Queries

```typescript
index: {
  // "What happened at time X?"
  byTimestamp: Map<number, Event[]> = new Map([
    [1706392102000, [event1, event2]],
    [1706392103000, [event3]]
  ]);

  // "How many times did we use the Read tool?"
  byTool: Map<string, ToolExecution[]> = new Map([
    ["Read", [tool1, tool2, tool3]],
    ["Edit", [tool4]],
    ["Task", [tool5, tool6]]
  ]);

  // "What did agent a3584f4 do?"
  byAgent: Map<string, Event[]> = new Map([
    ["a3584f4", [event5, event6, event7]]
  ]);

  // "What files were modified?"
  byFile: Map<string, ToolExecution[]> = new Map([
    ["src/core.ts", [tool4, tool8]],
    ["docs/spec.md", [tool9]]
  ]);

  // "What's the conversation tree?"
  conversationTree: TreeNode = {
    uuid: "root",
    children: [
      { uuid: "e1", children: [
        { uuid: "e2", children: [...] }
      ]}
    ]
  };
}
```

---

## Memory Layout

```
SessionAnalyzer (one instance)
├─ meta: 100 bytes                    (stored once)
├─ messages: 48 entries × 600 bytes   (deduplicated)
│  └─ Total: ~29 KB
├─ events: 2,532 refs × 50 bytes      (small, just refs)
│  └─ Total: ~127 KB
├─ agents: 3 entries × 300 bytes
│  └─ Total: ~1 KB
├─ tools: 50 entries × 200 bytes
│  └─ Total: ~10 KB
├─ files: 5 entries × 500 bytes
│  └─ Total: ~2.5 KB
└─ indexes: ~20 KB                    (pointers, not data)

TOTAL: ~190 KB (vs 474 KB raw file)
```

**Savings**: 60% reduction, but more importantly:
- **Instant queries** via indexes
- **No duplication** in messages
- **Rich relationships** preserved

---

## Query Examples

```typescript
// Q: "What files were modified in this session?"
analyzer.files.keys()
// → ["src/core.ts", "docs/spec.md"]

// Q: "How much did this session cost?"
analyzer.cache.totalCost ??=
  Array.from(analyzer.messages.values())
    .filter(m => m.usage)
    .reduce((sum, m) => sum + calculateCost(m.usage), 0);
// → $0.42

// Q: "What did agent a3584f4 do?"
analyzer.agents.get("a3584f4")?.summary
// → "Found 388 tests. Spec claims 388. ✓"

// Q: "Show me all Edit tool uses"
analyzer.index.byTool.get("Edit")
// → [ToolExecution, ToolExecution, ...]

// Q: "What was the conversation flow?"
function walkTree(node: TreeNode, depth = 0) {
  const event = analyzer.events.find(e => e.uuid === node.uuid);
  const message = event?.messageRef ?
    analyzer.messages.get(event.messageRef) : null;
  console.log("  ".repeat(depth) + message?.content[0]?.text);
  node.children.forEach(child => walkTree(child, depth + 1));
}
walkTree(analyzer.index.conversationTree);

// Q: "How many times did we read files vs write?"
{
  reads: analyzer.index.byTool.get("Read")?.length ?? 0,
  writes: analyzer.index.byTool.get("Write")?.length ?? 0,
  edits: analyzer.index.byTool.get("Edit")?.length ?? 0
}

// Q: "What's the cache hit rate?"
const usage = Array.from(analyzer.messages.values())
  .map(m => m.usage)
  .filter(Boolean);
const totalInput = usage.reduce((s, u) => s + u.input_tokens, 0);
const cacheHits = usage.reduce((s, u) => s + u.cache_read_input_tokens, 0);
const hitRate = cacheHits / (cacheHits + totalInput);
// → 0.87 (87% cache hit rate)
```

---

## Loading Strategy

```typescript
class SessionAnalyzer {
  static async load(sessionId: string): Promise<SessionAnalyzer> {
    const analyzer = new SessionAnalyzer(sessionId);

    // 1. Load session metadata from index (fast)
    const indexEntry = await loadSessionIndex(sessionId);
    analyzer.meta = indexEntry.meta;

    // 2. Stream parse JSONL file (memory efficient)
    const stream = createReadStream(indexEntry.fullPath);
    for await (const line of stream) {
      const event = JSON.parse(line);
      analyzer.addEvent(event);
    }

    // 3. Build indexes (one pass after loading)
    analyzer.buildIndexes();

    // 4. Load agent logs if needed (lazy)
    if (analyzer.agents.size > 0) {
      await analyzer.loadAgents();  // optional
    }

    return analyzer;
  }

  private addEvent(event: RawEvent) {
    // Deduplicate messages
    if (event.message) {
      const msgId = event.message.id ?? event.uuid;
      if (!this.messages.has(msgId)) {
        this.messages.set(msgId, normalizeMessage(event.message));
      }
    }

    // Add event with reference
    this.events.push({
      uuid: event.uuid,
      timestamp: new Date(event.timestamp),
      type: event.type,
      messageRef: event.message?.id ?? event.uuid,
      parentUuid: event.parentUuid
    });

    // Extract tool uses
    if (event.message?.content) {
      for (const block of event.message.content) {
        if (block.type === "tool_use") {
          this.tools.set(block.id, {
            id: block.id,
            name: block.name,
            input: block.input,
            timestamp: new Date(event.timestamp),
            messageId: event.message.id,
            success: true  // updated when we see tool_result
          });

          // Track file operations
          if (block.input.file_path) {
            this.trackFileOp(block.input.file_path, block.id, block.name);
          }

          // Track agent spawns
          if (block.name === "Task") {
            this.trackAgent(block);
          }
        }
      }
    }
  }

  private buildIndexes() {
    // Build all indexes in one pass
    for (const event of this.events) {
      // By timestamp
      const ts = event.timestamp.getTime();
      if (!this.index.byTimestamp.has(ts)) {
        this.index.byTimestamp.set(ts, []);
      }
      this.index.byTimestamp.get(ts)!.push(event);

      // By agent
      if (event.agentId) {
        if (!this.index.byAgent.has(event.agentId)) {
          this.index.byAgent.set(event.agentId, []);
        }
        this.index.byAgent.get(event.agentId)!.push(event);
      }
    }

    // By tool
    for (const [id, tool] of this.tools) {
      if (!this.index.byTool.has(tool.name)) {
        this.index.byTool.set(tool.name, []);
      }
      this.index.byTool.get(tool.name)!.push(tool);
    }

    // Conversation tree
    this.index.conversationTree = this.buildTree();
  }
}
```

---

## Multi-Session Analysis

```typescript
class ProjectAnalyzer {
  sessions: Map<string, SessionAnalyzer>;

  // Aggregate queries across sessions
  async totalCost(): Promise<number> {
    let total = 0;
    for (const session of this.sessions.values()) {
      total += session.cache.totalCost ?? 0;
    }
    return total;
  }

  // Find sessions that modified a file
  sessionsModifying(filePath: string): SessionAnalyzer[] {
    return Array.from(this.sessions.values())
      .filter(s => s.files.has(filePath));
  }

  // Agent usage patterns
  agentStats(): Map<string, number> {
    const stats = new Map<string, number>();
    for (const session of this.sessions.values()) {
      for (const agent of session.agents.values()) {
        stats.set(agent.type, (stats.get(agent.type) ?? 0) + 1);
      }
    }
    return stats;
  }
}
```

---

## Key Optimizations

1. **Deduplication**: Messages stored once, referenced by events
2. **Indexes**: Fast lookups without scanning
3. **Lazy loading**: Load agent logs only when needed
4. **Streaming**: Don't load entire file into memory
5. **Caching**: Compute expensive queries once
6. **References**: Events point to messages, not duplicate them

**Result**: ~190KB in memory, instant queries, rich relationships

---

## What This Enables

**Fast queries I can answer:**
- "What files were modified?"
- "How much did this cost?"
- "What agents were used?"
- "Show me the conversation"
- "Which tools were used most?"
- "What's the cache hit rate?"
- "Find sessions that edited X"
- "Show agent execution tree"

**All O(1) or O(log n) lookups, not O(n) scans.**
