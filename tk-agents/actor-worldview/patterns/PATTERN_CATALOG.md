# Actor Pattern Audit Report

**Date:** 2026-01-17
**Auditor:** Subagent (Actor Model Compliance Review)
**Scope:** Complete src/ directory scan for actor model violations

---

## Executive Summary

**Total Violations Found:** 12
- **Critical (P0):** 3 violations - Breaks actor model fundamentals
- **High (P1):** 5 violations - Significant architectural issues
- **Medium (P2):** 3 violations - Minor inconsistencies
- **Low (P3):** 1 violation - Style/documentation

**Compliance Status:** 🟡 **Partial Compliance**

Most core actors (TaskActor, KnowledgeActor) follow the pattern correctly, but several utility classes and infrastructure components violate the model.

---

## Audit Methodology

1. **Pattern Search:** Scanned for `export class` declarations
2. **Domain Logic Analysis:** Identified classes with domain state/behavior
3. **Relationship Analysis:** Checked for implicit relationships (arrays/objects)
4. **Message Passing Analysis:** Verified communication patterns
5. **Infrastructure vs Domain:** Distinguished infrastructure (Graph, System) from domain (Task, Knowledge)

---

## Critical Violations (P0)

### P0-1: EventLog Class with Stateful API

**File:** `src/persistence/event-log.ts:43`

**Code:**
```typescript
export class EventLog {
  private filePath: string;

  constructor(filePath: string) {
    this.filePath = filePath;
  }

  append(event: Event): void {
    const line = JSON.stringify(event) + "\n";
    appendFileSync(this.filePath, line, "utf-8");
  }

  replay(handler: (event: Event) => void): void {
    // Read and parse events
  }
}
```

**Violation:**
- EventLog is domain infrastructure but uses class pattern
- Direct method calls (`log.append()`) instead of message passing
- Events should be nodes in graph, not separate system

**Impact:**
- Events not queryable via graph
- No actor lifecycle for events
- Cannot traverse event→node relationships

**Recommended Fix:**

**Option A: EventActor (Full Actor Model)**
```typescript
export const EventActor: ActorFactory<EventActorData> = (data) => {
  const id = `event_${++eventCounter}`;
  const properties: EventProperties = {
    id,
    type: "event",
    eventType: data.eventType,
    nodeId: data.nodeId,
    timestamp: new Date(),
    payload: data.payload,
  };

  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      // Handle event messages (get, query, etc.)
    }
  };

  const address = data.graph.getSystem().register(actor);
  data.graph.registerNode(id, address, properties);

  // Create edge: event -> target node
  data.graph.addEdge(id, data.nodeId, "relates_to");

  return address;
};

// Usage
EventActor({
  eventType: "task_created",
  nodeId: "task_28",
  payload: { /* ... */ },
  graph,
});
```

**Option B: Pure Append-Only Log (Infrastructure)**

If EventLog is truly infrastructure (like filesystem), keep it as a utility but:
1. Don't store domain events (use graph instead)
2. Only for audit/replay, not domain logic
3. Document as infrastructure, not domain

**Priority:** P0 - Events are domain entities and should be in graph

**Migration Effort:** Medium (2-4 hours)

---

### P0-2: EventStream Class with In-Memory State

**File:** `src/events/stream-watcher.ts:32`

**Code:**
```typescript
export class EventStream {
  private events: StreamEvent[] = [];  // VIOLATION - state!
  private maxEvents: number;

  add(event: StreamEvent): void {
    this.events.push(event);
  }

  search(options: SearchOptions): StreamEvent[] {
    // Query in-memory array
  }
}
```

**Violation:**
- Domain state (events) stored in utility class
- Not persisted in graph
- Cannot query via graph traversal
- Lost on restart (in-memory only)

**Impact:**
- Events not persistent
- Cannot query "which events relate to task_28"
- Cannot traverse event relationships
- Violates "everything in graph" principle

**Recommended Fix:**

Store events as nodes in graph with edges to related entities:

```typescript
// Create event as actor
const eventAddr = EventActor({
  eventType: "file_changed",
  sourceFile: "/path/to/file",
  payload: { /* ... */ },
  graph,
});

// Link event to related entities
graph.addEdge(eventId, "task_28", "relates_to");

// Query events
const taskEvents = graph.getEdgesFrom("task_28")
  .filter(e => e.type === "relates_to")
  .map(e => e.toId)
  .map(id => graph.getNodeProperties(id))
  .filter(props => props.type === "event");
```

**Priority:** P0 - Events are domain data and must be in graph

**Migration Effort:** Medium (3-5 hours)

---

### P0-3: StreamWatcher Class Managing Domain State

**File:** `src/events/stream-watcher.ts:229`

**Code:**
```typescript
export class StreamWatcher {
  private stream: EventStream;
  private watchers: Map<string, FileWatcher> = new Map();

  constructor() {
    this.stream = new EventStream();
  }

  watch(path: string): void {
    const watcher = new FileWatcher(path, this.stream);
    this.watchers.set(path, watcher);
  }
}
```

**Violation:**
- Manages domain state (file watches) outside graph
- No actor lifecycle
- Cannot query "which files are being watched"
- Cannot persist watch state

**Impact:**
- Watch state lost on restart
- Cannot query watch relationships
- No message-based control (start/stop watch)

**Recommended Fix:**

Use FileWatcherActor (which already exists!):

```typescript
// src/actors/file-watcher-actor.ts already implements this correctly!

// Create file watcher actor
const watcherAddr = FileWatcherActor.create({
  path: "/path/to/file",
  graph,
});

// Control via messages
await graph.send(watcherId, "start", {});
await graph.send(watcherId, "stop", {});

// Query active watchers
const watchers = graph.getNodeIds()
  .map(id => graph.getNodeProperties(id))
  .filter(props => props.type === "file_watcher" && props.state === "active");
```

**Priority:** P0 - FileWatcherActor exists but StreamWatcher duplicates logic

**Migration Effort:** Low (1-2 hours) - Just use existing actor

---

## High Priority Violations (P1)

### P1-1: FileWatcher Class (Internal to StreamWatcher)

**File:** `src/events/stream-watcher.ts:148`

**Code:**
```typescript
class FileWatcher {
  private watcher: FSWatcher | null = null;
  private lastPosition: number = 0;

  constructor(private path: string, private stream: EventStream) {}

  start(): void {
    this.watcher = watch(this.path, () => {
      this.readNewContent();
    });
  }
}
```

**Violation:**
- Stateful class managing file watching
- FileWatcherActor already exists and does this correctly!
- Duplicates actor functionality

**Impact:**
- Code duplication
- Inconsistent patterns (some use actors, some use classes)
- Harder to maintain

**Recommended Fix:**

Delete this class and use FileWatcherActor instead:

```typescript
// DELETE FileWatcher class entirely

// USE existing FileWatcherActor
import { FileWatcherActor } from "../actors/file-watcher-actor.ts";

const watcherAddr = FileWatcherActor.create({
  path: "/path/to/file",
  graph,
});
```

**Priority:** P1 - Duplicate logic, actor already exists

**Migration Effort:** Low (1-2 hours)

---

### P1-2: AgentActor Class (Not Using ActorFactory Pattern)

**File:** `src/actors/agent-actor.ts:35`

**Code:**
```typescript
export class AgentActor {
  private state: AgentState = "idle";
  private currentTask: string | null = null;

  constructor(
    private id: string,
    private graph: Graph
  ) {}

  async send(message: Message): Promise<Response> {
    // Message handling
  }
}
```

**Violation:**
- Uses class pattern instead of ActorFactory
- Inconsistent with TaskActor, KnowledgeActor patterns
- Manual instantiation (`new AgentActor()`) instead of factory

**Impact:**
- Inconsistent patterns in codebase
- Harder to understand (why is this different?)
- May not register with System correctly

**Recommended Fix:**

Refactor to ActorFactory pattern:

```typescript
export interface CreateAgentOptions {
  agentType: string;
  graph: Graph;
}

export const AgentActor: ActorFactory<CreateAgentOptions> = (data) => {
  const id = `agent_${++agentCounter}`;
  const properties: AgentProperties = {
    id,
    type: "agent",
    agentType: data.agentType,
    state: "idle",
    currentTask: null,
  };

  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      try {
        const result = await handleMessage(message, properties, data.graph);
        return { success: true, data: result };
      } catch (error) {
        return { success: false, error: String(error) };
      }
    }
  };

  const address = data.graph.getSystem().register(actor);
  data.graph.registerNode(id, address, properties);

  return address;
};
```

**Priority:** P1 - Inconsistent pattern with core actors

**Migration Effort:** Medium (2-3 hours)

---

### P1-3: AgentActorSystem Class

**File:** `src/actors/agent-actor.ts:208`

**Code:**
```typescript
export class AgentActorSystem {
  private agents: Map<string, AgentActor> = new Map();

  createAgent(id: string): AgentActor {
    const agent = new AgentActor(id, this.graph);
    this.agents.set(id, agent);
    return agent;
  }
}
```

**Violation:**
- Manages agents outside of System actor
- Duplicates System functionality
- Direct agent references (not Addresses)

**Impact:**
- Violates uniform composition (System should manage all actors)
- Cannot route messages through System
- Hard to test

**Recommended Fix:**

Delete this class. Use Graph + System instead:

```typescript
// DELETE AgentActorSystem

// USE Graph.getSystem() instead
const graph = new Graph();

// Create agent via factory
const agentAddr = AgentActor({
  agentType: "research",
  graph,
});

// Message via graph
await graph.send(agentId, "start_task", { taskId: "task_28" });
```

**Priority:** P1 - Duplicates core infrastructure

**Migration Effort:** Medium (2-3 hours)

---

### P1-4: FileWatcherActor Using Class Pattern

**File:** `src/actors/file-watcher-actor.ts:244`

**Code:**
```typescript
export class FileWatcherActor {
  private state: "idle" | "watching" | "stopped" = "idle";
  private watcher: FSWatcher | null = null;

  static create(options: CreateOptions): FileWatcherActor {
    return new FileWatcherActor(options);
  }

  async send(message: Message): Promise<Response> {
    // Message handling
  }
}
```

**Violation:**
- Uses class pattern instead of ActorFactory
- Inconsistent with TaskActor, KnowledgeActor
- Static factory method instead of pure function

**Impact:**
- Inconsistent patterns
- Harder to refactor
- May not integrate well with System

**Recommended Fix:**

Refactor to ActorFactory pattern (like TaskActor):

```typescript
export const FileWatcherActor: ActorFactory<CreateFileWatcherOptions> = (data) => {
  const id = `file_watcher_${++watcherCounter}`;
  const properties: FileWatcherProperties = {
    id,
    type: "file_watcher",
    path: data.path,
    state: "idle",
  };

  let watcher: FSWatcher | null = null;

  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      // Handle start/stop/status messages
      // Use 'watcher' captured in closure
    }
  };

  const address = data.graph.getSystem().register(actor);
  data.graph.registerNode(id, address, properties);

  return address;
};
```

**Priority:** P1 - Inconsistent with core pattern

**Migration Effort:** Medium (2-3 hours)

---

### P1-5: FileWatcherSupervisor Using Class Pattern

**File:** `src/actors/file-watcher-supervisor.ts:93`

**Code:**
```typescript
export class FileWatcherSupervisor {
  private watchers: Map<string, FileWatcherActor> = new Map();

  supervise(path: string): void {
    const watcher = FileWatcherActor.create({ path, graph: this.graph });
    this.watchers.set(path, watcher);
  }
}
```

**Violation:**
- Uses class pattern instead of ActorFactory
- Manages child actors directly (should use System)
- Inconsistent with supervisor tree pattern

**Impact:**
- Cannot implement proper supervision (restart on failure)
- Hard to persist supervision state
- Not queryable via graph

**Recommended Fix:**

Refactor to SupervisorActor:

```typescript
export const SupervisorActor: ActorFactory<SupervisorActorData> = (data) => {
  const id = `supervisor_${++supervisorCounter}`;
  const properties: SupervisorProperties = {
    id,
    type: "supervisor",
    supervisedActors: [], // IDs of child actors
  };

  const actor = {
    send: async (message: ActorMessage): Promise<Response> => {
      switch (message.type) {
        case "supervise":
          // Create child actor and track ID
          const childAddr = FileWatcherActor({ /* ... */ });
          const childId = /* get ID */;
          properties.supervisedActors.push(childId);
          data.graph.addEdge(id, childId, "supervises");
          return { success: true, data: { childId } };

        case "restart_child":
          // Restart failed child
          break;
      }
    }
  };

  const address = data.graph.getSystem().register(actor);
  data.graph.registerNode(id, address, properties);

  return address;
};
```

**Priority:** P1 - Supervision is important pattern

**Migration Effort:** High (4-6 hours)

---

## Medium Priority Violations (P2)

### P2-1: MarkdownGraph Class (Domain Logic)

**File:** `src/markdown-graph/MarkdownGraph.ts:31`

**Code:**
```typescript
export class MarkdownGraph {
  private nodes: Map<string, MarkdownNode> = new Map();
  private edges: Map<string, MarkdownEdge> = new Map();

  addNode(node: MarkdownNode): void {
    this.nodes.set(node.id, node);
  }

  query(selector: string): MarkdownNode[] {
    // Query logic
  }
}
```

**Violation:**
- Separate graph for markdown (should use main Graph)
- Domain state outside main graph
- Cannot query markdown→task relationships

**Impact:**
- Fragmented data (multiple graphs)
- Cannot traverse markdown→task edges
- Duplicate graph implementation

**Recommended Fix:**

Store markdown nodes in main Graph:

```typescript
// Create markdown section as node
const sectionAddr = MarkdownSectionActor({
  heading: "## Summary",
  content: "...",
  level: 2,
  graph,
});

// Link to document
graph.addEdge(sectionId, documentId, "section_of");

// Link to task
graph.addEdge(documentId, "task_28", "documents");

// Query via graph traversal
const taskDocs = graph.getEdgesFrom("task_28")
  .filter(e => e.type === "documents")
  .map(e => e.toId);
```

**Priority:** P2 - Works but fragmented

**Migration Effort:** High (6-8 hours) - Large refactor

---

### P2-2: SessionGraph Class

**File:** `src/graph/session-graph.ts:43`

**Code:**
```typescript
export class SessionGraph {
  private sessions: Map<string, SessionData> = new Map();

  addSession(id: string, data: SessionData): void {
    this.sessions.set(id, data);
  }
}
```

**Violation:**
- Separate graph for sessions (should use main Graph)
- Session data not in main graph
- Cannot query session→task relationships

**Impact:**
- Cannot traverse "which tasks were worked on in session X"
- Fragmented data

**Recommended Fix:**

Store sessions as nodes:

```typescript
const sessionAddr = SessionActor({
  startTime: new Date(),
  graph,
});

// Link tasks worked on
graph.addEdge("task_28", sessionId, "worked_in_session");

// Query
const sessionTasks = graph.getEdgesTo(sessionId)
  .filter(e => e.type === "worked_in_session")
  .map(e => e.fromId);
```

**Priority:** P2 - Sessions should be in graph

**Migration Effort:** Medium (3-4 hours)

---

### P2-3: CozoDB Client Classes

**Files:**
- `src/cozo-client.ts:25` - CozoClient
- `src/cozo-wasm-client.ts:36` - CozoWasmClient
- `src/cozo-ffi-client.ts:86` - CozoFfiClient
- `src/cozo-wasm-native.ts:43` - CozoWasmNativeClient
- `src/cozo-repl-client.ts:20` - CozoReplClient

**Code:**
```typescript
export class CozoClient {
  async query(script: string): Promise<QueryResult> {
    // Query CozoDB
  }
}
```

**Violation:**
- Infrastructure classes (database clients)
- NOT domain logic - these are fine!

**Impact:**
- None - these are infrastructure

**Recommended Action:**

**No changes needed.** These are infrastructure classes (like Graph, System).

Document as infrastructure:

```typescript
/**
 * CozoClient - Infrastructure for CozoDB queries
 *
 * NOTE: This is infrastructure, NOT a domain actor.
 * Domain logic should use Graph and actors, not direct DB access.
 */
export class CozoClient {
  // ...
}
```

**Priority:** P2 - Documentation only

**Migration Effort:** Low (30 min) - Add comments

---

## Low Priority Violations (P3)

### P3-1: Inconsistent Naming (send vs receive)

**Files:** Multiple actor files

**Code:**
```typescript
// Current (semantically incorrect)
const actor = {
  send: async (message: Message): Promise<Response> => {
    // RECEIVES message, but method called "send"
  }
};
```

**Violation:**
- Method should be `receive()` per Hewitt Actor Model
- See HEWITT_ACTOR_MODEL.md ADR

**Impact:**
- Semantic confusion
- Inconsistent with literature (Erlang, Akka use "receive")

**Recommended Fix:**

See HEWITT_ACTOR_MODEL.md migration plan. This is tracked separately.

**Priority:** P3 - Semantic issue, tracked in ADR

**Migration Effort:** High (full codebase) - Phased migration planned

---

## Infrastructure vs Domain Classification

### ✅ Legitimate Infrastructure Classes (NOT Violations)

These classes are **infrastructure** and correctly use class pattern:

1. **Graph** (`src/graph.ts:16`) - Graph infrastructure
2. **System** (`src/actors/system.ts`) - Actor system runtime
3. **CozoClient** (multiple files) - Database clients
4. **EventLog** - IF used for audit only (see P0-1)

**Rationale:**
- Not domain entities
- Provide infrastructure services
- Don't hold domain state
- Not queryable via graph (they ARE the query mechanism)

### ❌ Domain Classes Violating Actor Model

These should be actors:

1. EventStream - Events are domain data
2. StreamWatcher - Watch state is domain
3. AgentActor - Agents are domain entities
4. FileWatcherActor - File watches are domain
5. MarkdownGraph - Markdown nodes are domain

---

## Violation Summary Table

| ID | File:Line | Violation | Priority | Effort | Status |
|----|-----------|-----------|----------|--------|--------|
| P0-1 | event-log.ts:43 | EventLog class | P0 | Medium | 🔴 Open |
| P0-2 | stream-watcher.ts:32 | EventStream class | P0 | Medium | 🔴 Open |
| P0-3 | stream-watcher.ts:229 | StreamWatcher class | P0 | Low | 🔴 Open |
| P1-1 | stream-watcher.ts:148 | FileWatcher class | P1 | Low | 🔴 Open |
| P1-2 | agent-actor.ts:35 | AgentActor class | P1 | Medium | 🔴 Open |
| P1-3 | agent-actor.ts:208 | AgentActorSystem | P1 | Medium | 🔴 Open |
| P1-4 | file-watcher-actor.ts:244 | FileWatcherActor class | P1 | Medium | 🔴 Open |
| P1-5 | file-watcher-supervisor.ts:93 | FileWatcherSupervisor | P1 | High | 🔴 Open |
| P2-1 | MarkdownGraph.ts:31 | MarkdownGraph class | P2 | High | 🔴 Open |
| P2-2 | session-graph.ts:43 | SessionGraph class | P2 | Medium | 🔴 Open |
| P2-3 | cozo-*.ts | CozoDB clients | P2 | Low | 🟢 OK (infra) |
| P3-1 | Multiple | send vs receive | P3 | High | 🟡 Tracked in ADR |

---

## Recommended Migration Priority

### Phase 1: Critical Violations (P0)

**Goal:** Fix violations breaking fundamental actor model

1. **P0-3:** Replace StreamWatcher with FileWatcherActor (1-2 hours)
   - **Quick win:** Actor already exists, just use it

2. **P0-2:** Convert EventStream to EventActor (3-5 hours)
   - Store events as graph nodes
   - Create edges for event relationships

3. **P0-1:** Clarify EventLog role (2-4 hours)
   - **Option A:** Convert to EventActor
   - **Option B:** Document as infrastructure only

**Total:** 6-11 hours

### Phase 2: High Priority Violations (P1)

**Goal:** Achieve pattern consistency

4. **P1-1:** Delete FileWatcher class (1-2 hours)
   - Use FileWatcherActor instead

5. **P1-2:** Refactor AgentActor to ActorFactory (2-3 hours)
   - Match TaskActor pattern

6. **P1-3:** Delete AgentActorSystem (2-3 hours)
   - Use Graph + System

7. **P1-4:** Refactor FileWatcherActor to factory (2-3 hours)
   - Match TaskActor pattern

8. **P1-5:** Refactor FileWatcherSupervisor (4-6 hours)
   - Implement proper supervision pattern

**Total:** 11-17 hours

### Phase 3: Medium Priority Violations (P2)

**Goal:** Consolidate into single graph

9. **P2-2:** Merge SessionGraph into Graph (3-4 hours)
   - Sessions as nodes

10. **P2-1:** Merge MarkdownGraph into Graph (6-8 hours)
    - Markdown sections as nodes
    - Large refactor

11. **P2-3:** Document CozoDB clients as infrastructure (30 min)
    - Add comments only

**Total:** 10-13 hours

### Phase 4: Low Priority (P3)

12. **P3-1:** Rename send→receive (tracked in ADR)
    - Phased migration planned separately

---

## Testing Strategy

### Test Each Migration

```typescript
// Before migration
test("OLD: manager.attach()", async () => {
  const manager = new AttachmentManager(graph);
  const result = await manager.attach("task_28", { /* ... */ });
  expect(result.id).toBeDefined();
});

// After migration
test("NEW: AttachmentActor via messages", async () => {
  const addr = AttachmentActor({ /* ... */ });
  const result = await graph.send(attachmentId, "validate", {});
  expect(result.valid).toBe(true);
});
```

### Regression Tests

Ensure existing functionality works after migration:

```typescript
test("task dependency blocking still works", async () => {
  // Same test as before, should pass after migration
});
```

---

## Documentation Updates Needed

1. **ACTOR_MODEL_GUIDE.md** - ✅ Created
2. **BLOB_STORAGE_ACTOR_REDESIGN.md** - ✅ Created
3. Update README.md with actor model principles
4. Add architecture diagram showing actor relationships
5. Document infrastructure vs domain distinction
6. Add testing guide for actors

---

## Conclusion

**Current Status:**
- ✅ Core actors (Task, Knowledge) follow pattern correctly
- 🟡 Several utility classes violate pattern
- 🟡 Fragmented graphs (Markdown, Session)
- ✅ Infrastructure properly separated

**Recommended Action:**

1. **Start with P0-3** (StreamWatcher) - Quick win, actor exists
2. **Complete Phase 1** (P0) - Fix critical violations
3. **Evaluate** - Measure improvements in consistency
4. **Continue** to Phase 2 (P1) - Pattern consistency
5. **Defer** Phase 3 (P2) - Larger refactors, lower risk

**Estimated Total Effort:**
- Phase 1 (P0): 6-11 hours
- Phase 2 (P1): 11-17 hours
- Phase 3 (P2): 10-13 hours
- **Total:** 27-41 hours (3-5 days)

**Benefits:**
- Uniform architecture (everything in graph)
- Better queryability (graph traversal)
- Consistent patterns (easier maintenance)
- Actor model compliance (matches literature)

---

**Next Steps:**

1. Review this audit with team
2. Prioritize violations to fix
3. Start with P0-3 (quick win)
4. Create migration tasks for each violation
5. Track progress in tasks.json
