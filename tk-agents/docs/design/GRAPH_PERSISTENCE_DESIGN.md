# Graph Persistence and Event Log Design

## Executive Summary

This document analyzes the current persistence architecture for the tk-agents graph system and proposes a unified event-sourcing approach. The system currently uses two persistence strategies: CLI-managed JSON snapshots (tasks.json) and a low-level EventLog JSONL implementation. This design unifies these approaches while maintaining the actor model's concurrency guarantees.

## Current State Analysis

### What Exists Today

#### 1. Graph Core (src/graph.ts)
**Current Behavior:** Pure in-memory graph with no built-in persistence
- `nodes: Map<string, Address>` - String ID to Actor Address mapping
- `nodeProperties: Map<string, NodeProperties>` - Cached node properties
- `edges: Map<string, Edge>` - Edge storage
- `edgeCounter` - Sequential edge ID generation

**Serialization Support:**
```typescript
dump(): { nodes: NodeProperties[]; edges: Edge[] }
setEdgeCounter(counter: number): void
```

The `dump()` method provides a snapshot of graph state, but **Graph itself has no load/save methods**.

#### 2. Task CLI Persistence (src/cli/task.ts)
**Current Implementation:** Manual JSON snapshot management

```typescript
// File format
interface TaskFile {
  nodes: NodeProperties[];  // Full node property dumps
  edges: Edge[];            // All edges
}

// Loading pattern
async function loadGraph(filePath: string): Promise<Graph> {
  const taskFile = JSON.parse(readFileSync(filePath));
  const graph = new Graph();

  // Recreate actors using factory pattern
  for (const nodeProps of taskFile.nodes) {
    if (nodeProps.type === "task") {
      TaskActor({ ...taskProps, graph });

      // Restore state by mutating properties
      const restoredProps = graph.getNodeProperties(taskProps.id);
      Object.assign(restoredProps, taskProps);
    }
  }

  // Recreate edges
  for (const edge of taskFile.edges) {
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
  }

  return graph;
}

// Saving pattern
async function saveGraph(graph: Graph, filePath: string): Promise<void> {
  const dump = graph.dump();
  writeFileSync(filePath, JSON.stringify(dump, dateSerializer, 2));
}
```

**Usage Pattern:**
1. Load full graph at CLI startup
2. Execute mutations via actor messages
3. Save full graph after every mutation
4. No incremental saves, no event history

**Strengths:**
- Simple and working
- Human-readable JSON
- Full state in single file

**Weaknesses:**
- Full graph rewrite on every change (inefficient)
- No change history or audit trail
- No crash recovery between saves
- No replay capability
- Race conditions if multiple CLIs run concurrently

#### 3. EventLog Implementation (src/persistence/event-log.ts)
**Current Implementation:** JSONL append-only event log (UNUSED by Graph)

```typescript
interface Event {
  timestamp: string;
  type: string;
  nodeId: string;
  data: unknown;
  metadata?: Record<string, unknown>;
}

class EventLog {
  append(event: Event): void
  replay(handler: (event: Event) => void): void
  getAllEvents(): Event[]
  getEventsByType(type: string): Event[]
  getEventsByNode(nodeId: string): Event[]
}
```

**Strengths:**
- Append-only (durable, fast writes)
- JSONL format (line-oriented, recoverable)
- Replay capability built-in
- Filtering by type/node
- Working tests (13 passing)

**Weaknesses:**
- **NOT integrated with Graph** (exists in isolation)
- No event schema defined for graph mutations
- No compaction/snapshotting (log grows unbounded)
- No Graph-level API (low-level tool only)

### Current Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│ CLI Layer (task.ts, knowledge.ts, graph.ts)                │
│                                                             │
│  loadGraph() ──> new Graph() ──> recreate actors           │
│  mutations() ──> graph.send(msg) ──> actor state change    │
│  saveGraph() ──> graph.dump() ──> writeFile(tasks.json)    │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ Graph (src/graph.ts) - IN-MEMORY ONLY                      │
│                                                             │
│  nodes: Map<string, Address>                               │
│  nodeProperties: Map<string, NodeProperties>               │
│  edges: Map<string, Edge>                                  │
│                                                             │
│  NO PERSISTENCE LAYER                                      │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ EventLog (src/persistence/event-log.ts) - ISOLATED         │
│                                                             │
│  append-only JSONL                                         │
│  replay capability                                         │
│                                                             │
│  NOT USED BY GRAPH OR CLI                                  │
└─────────────────────────────────────────────────────────────┘
```

### Problems Identified

#### 1. Durability Gaps
- **Process crash:** All in-memory state lost
- **Power failure:** Mutations since last save lost
- **Between-save window:** No durability guarantee

#### 2. Performance Issues
- **Write amplification:** Full graph serialization on every mutation
- **Inefficient for large graphs:** O(nodes + edges) write per mutation
- **No incremental updates:** Can't save just changed parts

#### 3. Audit and Debug Limitations
- **No history:** Can't answer "when did task X change state?"
- **No causality tracking:** Can't trace mutation chain
- **No rollback:** Can't undo changes or replay from checkpoint

#### 4. Concurrency Risks
- **No locking:** Multiple CLI instances could corrupt tasks.json
- **Race conditions:** Last-write-wins without coordination

#### 5. Architecture Mismatch
- **EventLog exists but unused:** Dead code or missed integration?
- **CLI owns persistence:** Should be Graph's responsibility
- **Actor model ignored:** Actors are stateful but persistence is snapshot-based

## Event Log Architecture

### Core Design Principles

1. **Append-Only Mutations:** All graph changes logged as immutable events
2. **Replay for Recovery:** Reconstruct graph state by replaying event log
3. **Snapshot + Replay Hybrid:** Periodic snapshots + recent events for fast startup
4. **Actor Boundaries:** Events at actor message boundaries (not internal state)
5. **CLI Transparency:** CLI code should not manage persistence directly

### Event Schema

#### Event Types

```typescript
// Node lifecycle events
type NodeEvent =
  | { type: "node_created"; nodeId: string; nodeType: NodeType; properties: NodeProperties }
  | { type: "node_updated"; nodeId: string; updates: Partial<NodeProperties> }
  | { type: "node_deleted"; nodeId: string }

// Edge lifecycle events
type EdgeEvent =
  | { type: "edge_created"; edgeId: string; fromId: string; toId: string; edgeType: EdgeType; properties: Record<string, unknown> }
  | { type: "edge_deleted"; edgeId: string }

// Message events (optional, for audit trail)
type MessageEvent =
  | { type: "message_sent"; messageId: string; nodeId: string; messageType: string; payload: unknown }
  | { type: "message_completed"; messageId: string; nodeId: string; success: boolean; result: unknown }

// Snapshot events
type SnapshotEvent =
  | { type: "snapshot_created"; snapshotId: string; nodeCount: number; edgeCount: number; timestamp: string }
```

#### Event Format (extends EventLog.Event)

```typescript
interface GraphEvent extends Event {
  timestamp: string;
  type: string;        // One of the event types above
  nodeId: string;      // Primary node involved
  data: unknown;       // Event-specific payload
  metadata?: {
    sequenceNumber?: number;     // For ordering
    correlationId?: string;      // Link related events
    causedBy?: string;           // Message ID that caused this event
  }
}
```

### Persistence Strategy Options

#### Option A: Pure Event Log (Simplest)
**Approach:** Single JSONL file with all events, replay from beginning

```
graph-events.jsonl:
{"timestamp":"...","type":"node_created","nodeId":"task_1","data":{...}}
{"timestamp":"...","type":"edge_created","edgeId":"edge_1","data":{...}}
{"timestamp":"...","type":"node_updated","nodeId":"task_1","data":{...}}
...
```

**Replay Algorithm:**
```typescript
function replayFromLog(log: EventLog): Graph {
  const graph = new Graph();

  log.replay((event) => {
    switch (event.type) {
      case "node_created":
        // Recreate actor using factory
        const factory = getActorFactory(event.data.nodeType);
        factory({ ...event.data.properties, graph });
        break;

      case "node_updated":
        // Update node properties in-place
        const props = graph.getNodeProperties(event.nodeId);
        Object.assign(props, event.data.updates);
        break;

      case "edge_created":
        graph.addEdge(event.data.fromId, event.data.toId, event.data.edgeType, event.data.properties);
        break;

      case "node_deleted":
        graph.removeNode(event.nodeId);
        break;

      case "edge_deleted":
        graph.removeEdge(event.data.edgeId);
        break;
    }
  });

  return graph;
}
```

**Pros:**
- Dead simple implementation
- Complete audit trail
- Can replay to any point in history
- Append-only (fast, durable)

**Cons:**
- **Slow startup for large graphs** (replay all events)
- **Unbounded log size** (grows forever)
- **No compaction** (old events never removed)

**When to Use:** Small graphs (<1000 nodes), audit-heavy use cases

#### Option B: Snapshot + Incremental Events (Recommended)
**Approach:** Periodic snapshots + recent events since snapshot

```
graph.snapshot.json:   # Most recent snapshot
{
  "snapshotId": "snapshot_001",
  "timestamp": "2026-01-16T12:00:00.000Z",
  "nodes": [...],
  "edges": [...],
  "eventLogPosition": 1234  # Last event included in snapshot
}

graph-events.jsonl:    # All events (append-only)
{"seq":1,"type":"node_created",...}
{"seq":2,"type":"edge_created",...}
...
{"seq":1234,"type":"snapshot_created","snapshotId":"snapshot_001"}
{"seq":1235,"type":"node_updated",...}  # Events after snapshot
```

**Replay Algorithm:**
```typescript
function replayFromSnapshotAndLog(
  snapshotPath: string,
  logPath: string
): Graph {
  const log = new EventLog(logPath);

  // 1. Load snapshot if exists
  let graph: Graph;
  let lastSeq = 0;

  if (existsSync(snapshotPath)) {
    const snapshot = JSON.parse(readFileSync(snapshotPath));
    graph = loadGraphFromSnapshot(snapshot);
    lastSeq = snapshot.eventLogPosition;
  } else {
    graph = new Graph();
  }

  // 2. Replay events since snapshot
  log.replay((event) => {
    const seq = event.metadata?.sequenceNumber || 0;
    if (seq > lastSeq) {
      applyEvent(graph, event);
    }
  });

  return graph;
}
```

**Snapshot Strategy:**
```typescript
class Graph {
  private eventsSinceSnapshot = 0;
  private readonly SNAPSHOT_THRESHOLD = 100; // Snapshot every N events

  private logEvent(event: GraphEvent): void {
    this.eventLog.append(event);
    this.eventsSinceSnapshot++;

    if (this.eventsSinceSnapshot >= this.SNAPSHOT_THRESHOLD) {
      this.createSnapshot();
      this.eventsSinceSnapshot = 0;
    }
  }

  private createSnapshot(): void {
    const snapshot = {
      snapshotId: `snapshot_${Date.now()}`,
      timestamp: new Date().toISOString(),
      nodes: Array.from(this.nodeProperties.values()),
      edges: Array.from(this.edges.values()),
      eventLogPosition: this.lastEventSeq,
    };

    writeFileSync(this.snapshotPath, JSON.stringify(snapshot, null, 2));

    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type: "snapshot_created",
      nodeId: "system",
      data: { snapshotId: snapshot.snapshotId },
      metadata: { sequenceNumber: ++this.lastEventSeq },
    });
  }
}
```

**Pros:**
- **Fast startup:** Load snapshot + replay few recent events
- **Bounded memory:** Old events compacted into snapshots
- **Complete history:** Event log preserved for audit
- **Tunable performance:** Adjust snapshot frequency

**Cons:**
- More complex than pure event log
- Two files to manage (snapshot + log)
- Snapshot creation adds latency

**When to Use:** Production systems, larger graphs (>1000 nodes)

#### Option C: In-Memory + Periodic Flush (Fastest)
**Approach:** Keep graph in memory, flush event log periodically, snapshot on shutdown

```typescript
class Graph {
  private eventBuffer: GraphEvent[] = [];
  private flushTimer: Timer;

  constructor(private eventLog: EventLog) {
    this.flushTimer = setInterval(() => this.flushEvents(), 5000); // Flush every 5s
  }

  private logEvent(event: GraphEvent): void {
    this.eventBuffer.push(event);

    if (this.eventBuffer.length >= 100) {
      this.flushEvents(); // Flush on buffer size
    }
  }

  private flushEvents(): void {
    for (const event of this.eventBuffer) {
      this.eventLog.append(event);
    }
    this.eventBuffer = [];
  }

  async shutdown(): Promise<void> {
    this.flushEvents();
    this.createSnapshot();
    clearInterval(this.flushTimer);
  }
}
```

**Pros:**
- **Fastest writes:** Batch events in memory
- **Reduced I/O:** Periodic flushes instead of per-event
- **Good for short sessions:** CLI commands complete quickly

**Cons:**
- **Durability window:** Events lost if crash before flush
- **Complex shutdown:** Must explicitly call shutdown()
- **Not suitable for long-running processes**

**When to Use:** CLI tools with short-lived processes, batch operations

### Recommended Approach: Option B (Snapshot + Events)

**Rationale:**
1. **Balance:** Fast startup + complete history
2. **Scalability:** Handles graphs of any size
3. **Durability:** Every mutation logged immediately
4. **Audit:** Full event history for debugging
5. **Actor-friendly:** Events at message boundaries

## API Design

### Graph Class Integration

```typescript
export class Graph {
  private system: SystemType;
  private nodes: Map<string, Address> = new Map();
  private nodeProperties: Map<string, NodeProperties> = new Map();
  private edges: Map<string, Edge> = new Map();
  private edgeCounter = 0;

  // NEW: Persistence layer
  private eventLog: EventLog;
  private snapshotPath: string;
  private lastEventSeq = 0;
  private eventsSinceSnapshot = 0;
  private readonly SNAPSHOT_THRESHOLD = 100;

  constructor(options?: {
    eventLogPath?: string;
    snapshotPath?: string;
    snapshotThreshold?: number;
  }) {
    this.system = System();

    // Initialize persistence
    this.eventLog = new EventLog(options?.eventLogPath || "./graph-events.jsonl");
    this.snapshotPath = options?.snapshotPath || "./graph.snapshot.json";
    this.SNAPSHOT_THRESHOLD = options?.snapshotThreshold || 100;
  }

  /**
   * Load graph from snapshot + event log
   * Replaces current state with persisted state
   */
  async load(): Promise<void> {
    // Load snapshot if exists
    if (existsSync(this.snapshotPath)) {
      const snapshot = JSON.parse(readFileSync(this.snapshotPath, "utf-8"));
      await this.loadFromSnapshot(snapshot);
      this.lastEventSeq = snapshot.eventLogPosition || 0;
    }

    // Replay events since snapshot
    this.eventLog.replay((event) => {
      const seq = event.metadata?.sequenceNumber || 0;
      if (seq > this.lastEventSeq) {
        this.applyEvent(event);
        this.lastEventSeq = seq;
      }
    });
  }

  /**
   * Save current state (flush events + create snapshot)
   * Typically called on shutdown or after major mutations
   */
  async save(): Promise<void> {
    this.createSnapshot();
  }

  /**
   * Create a snapshot of current state
   * Called automatically after N events or manually
   */
  private createSnapshot(): void {
    const snapshot = {
      snapshotId: `snapshot_${Date.now()}`,
      timestamp: new Date().toISOString(),
      nodes: Array.from(this.nodeProperties.values()),
      edges: Array.from(this.edges.values()),
      eventLogPosition: this.lastEventSeq,
      edgeCounter: this.edgeCounter,
    };

    writeFileSync(this.snapshotPath, JSON.stringify(snapshot, null, 2), "utf-8");

    this.logEvent({
      timestamp: new Date().toISOString(),
      type: "snapshot_created",
      nodeId: "system",
      data: { snapshotId: snapshot.snapshotId },
      metadata: { sequenceNumber: ++this.lastEventSeq },
    });
  }

  /**
   * Log an event to the event log
   * Called internally on every mutation
   */
  private logEvent(event: Omit<GraphEvent, "metadata">): void {
    const fullEvent: GraphEvent = {
      ...event,
      metadata: {
        sequenceNumber: ++this.lastEventSeq,
      },
    };

    this.eventLog.append(fullEvent as Event);
    this.eventsSinceSnapshot++;

    if (this.eventsSinceSnapshot >= this.SNAPSHOT_THRESHOLD) {
      this.createSnapshot();
      this.eventsSinceSnapshot = 0;
    }
  }

  /**
   * Apply an event to the graph (used during replay)
   */
  private applyEvent(event: Event): void {
    switch (event.type) {
      case "node_created":
        this.replayNodeCreated(event);
        break;
      case "node_updated":
        this.replayNodeUpdated(event);
        break;
      case "node_deleted":
        this.replayNodeDeleted(event);
        break;
      case "edge_created":
        this.replayEdgeCreated(event);
        break;
      case "edge_deleted":
        this.replayEdgeDeleted(event);
        break;
    }
  }

  // Mutation methods (MODIFIED to log events)

  registerNode(id: string, address: Address, properties: NodeProperties): void {
    this.nodes.set(id, address);
    this.nodeProperties.set(id, properties);

    // Log event
    this.logEvent({
      timestamp: new Date().toISOString(),
      type: "node_created",
      nodeId: id,
      data: { nodeType: properties.type, properties },
    });
  }

  removeNode(nodeId: string): boolean {
    // Remove edges first
    for (const [edgeId, edge] of this.edges) {
      if (edge.fromId === nodeId || edge.toId === nodeId) {
        this.removeEdge(edgeId);
      }
    }

    this.nodeProperties.delete(nodeId);
    const removed = this.nodes.delete(nodeId);

    if (removed) {
      this.logEvent({
        timestamp: new Date().toISOString(),
        type: "node_deleted",
        nodeId,
        data: {},
      });
    }

    return removed;
  }

  addEdge(fromId: string, toId: string, type: EdgeType, properties: Record<string, unknown> = {}): Edge {
    const id = `edge_${++this.edgeCounter}`;
    const edge: Edge = { id, fromId, toId, type, properties };
    this.edges.set(id, edge);

    // Log event
    this.logEvent({
      timestamp: new Date().toISOString(),
      type: "edge_created",
      nodeId: fromId,
      data: { edgeId: id, fromId, toId, edgeType: type, properties },
    });

    return edge;
  }

  removeEdge(edgeId: string): boolean {
    const edge = this.edges.get(edgeId);
    const removed = this.edges.delete(edgeId);

    if (removed && edge) {
      this.logEvent({
        timestamp: new Date().toISOString(),
        type: "edge_deleted",
        nodeId: edge.fromId,
        data: { edgeId },
      });
    }

    return removed;
  }

  // Replay methods (for loading from events)

  private replayNodeCreated(event: Event): void {
    const data = event.data as { nodeType: NodeType; properties: NodeProperties };
    const factory = getActorFactory(data.nodeType);
    factory({ ...data.properties, graph: this });
  }

  private replayNodeUpdated(event: Event): void {
    const data = event.data as { updates: Partial<NodeProperties> };
    const props = this.nodeProperties.get(event.nodeId);
    if (props) {
      Object.assign(props, data.updates);
    }
  }

  private replayNodeDeleted(event: Event): void {
    this.nodes.delete(event.nodeId);
    this.nodeProperties.delete(event.nodeId);
  }

  private replayEdgeCreated(event: Event): void {
    const data = event.data as { edgeId: string; fromId: string; toId: string; edgeType: EdgeType; properties: Record<string, unknown> };
    const edge: Edge = {
      id: data.edgeId,
      fromId: data.fromId,
      toId: data.toId,
      type: data.edgeType,
      properties: data.properties,
    };
    this.edges.set(edge.id, edge);

    // Update edge counter
    const edgeNum = parseInt(edge.id.replace("edge_", ""));
    if (!isNaN(edgeNum)) {
      this.edgeCounter = Math.max(this.edgeCounter, edgeNum);
    }
  }

  private replayEdgeDeleted(event: Event): void {
    const data = event.data as { edgeId: string };
    this.edges.delete(data.edgeId);
  }

  private async loadFromSnapshot(snapshot: any): Promise<void> {
    // Recreate nodes from snapshot
    for (const nodeProps of snapshot.nodes) {
      const factory = getActorFactory(nodeProps.type);
      factory({ ...nodeProps, graph: this });

      // Restore full properties
      const restored = this.nodeProperties.get(nodeProps.id);
      if (restored) {
        Object.assign(restored, nodeProps);
      }
    }

    // Recreate edges
    for (const edge of snapshot.edges) {
      this.edges.set(edge.id, edge);
      const edgeNum = parseInt(edge.id.replace("edge_", ""));
      if (!isNaN(edgeNum)) {
        this.edgeCounter = Math.max(this.edgeCounter, edgeNum);
      }
    }

    // Restore edge counter
    if (snapshot.edgeCounter !== undefined) {
      this.edgeCounter = snapshot.edgeCounter;
    }
  }
}

// Factory registry for replay
const actorFactories: Record<NodeType, ActorFactory<any>> = {
  task: TaskActor,
  knowledge: KnowledgeActor,
  // Add other types as needed
};

function getActorFactory(nodeType: NodeType): ActorFactory<any> {
  const factory = actorFactories[nodeType];
  if (!factory) {
    throw new Error(`No actor factory registered for type: ${nodeType}`);
  }
  return factory;
}
```

### CLI Integration (Simplified)

```typescript
// OLD: task.ts (manual load/save)
async function loadGraph(filePath: string): Promise<Graph> {
  const content = readFileSync(filePath, "utf-8");
  const taskFile: TaskFile = JSON.parse(content);
  const graph = new Graph();

  for (const nodeProps of taskFile.nodes) {
    TaskActor({ ...nodeProps, graph });
    // ... restore state
  }

  return graph;
}

async function saveGraph(graph: Graph, filePath: string): Promise<void> {
  const dump = graph.dump();
  writeFileSync(filePath, JSON.stringify(dump, null, 2));
}

// NEW: task.ts (persistence handled by Graph)
async function getGraph(): Promise<Graph> {
  const graph = new Graph({
    eventLogPath: "./tasks.events.jsonl",
    snapshotPath: "./tasks.snapshot.json",
  });

  await graph.load(); // Load snapshot + replay events
  return graph;
}

async function cmdList() {
  const graph = await getGraph();
  // Use graph normally - persistence is automatic
  const tasks = graph.getNodeIds();
  // ...
}

async function cmdAdd(goal: string, options: any) {
  const graph = await getGraph();

  // Create task - automatically logged
  TaskActor({ goal, ...options, graph });

  // Optional: explicit save for immediate flush
  await graph.save();
}
```

### Usage Examples

#### Example 1: Simple CLI Session
```typescript
// Session start
const graph = new Graph({
  eventLogPath: "./tasks.events.jsonl",
  snapshotPath: "./tasks.snapshot.json",
});
await graph.load();

// Mutations (automatically logged)
TaskActor({ goal: "Task 1", graph });
TaskActor({ goal: "Task 2", graph });
graph.addEdge("task_1", "task_2", "depends_on");

// Session end (optional explicit save)
await graph.save();
```

#### Example 2: Crash Recovery
```typescript
// Before crash
const graph1 = new Graph();
await graph1.load();
TaskActor({ goal: "Task 1", graph: graph1 }); // Logged
TaskActor({ goal: "Task 2", graph: graph1 }); // Logged
// CRASH (before save)

// After restart
const graph2 = new Graph();
await graph2.load(); // Replays events, recovers Task 1 and Task 2
```

#### Example 3: Audit Trail
```typescript
const eventLog = new EventLog("./tasks.events.jsonl");

// Query history
const taskCreations = eventLog.getEventsByType("node_created");
console.log(`Total tasks created: ${taskCreations.length}`);

// Investigate specific node
const task1Events = eventLog.getEventsByNode("task_1");
for (const event of task1Events) {
  console.log(`${event.timestamp}: ${event.type}`);
}
```

## Implementation Considerations

### 1. Concurrency (Actor Model Implications)

**Challenge:** Actors may process messages concurrently, but event log requires serial order.

**Solution:** Serialize event logging at Graph level (not actor level)
```typescript
class Graph {
  private eventLogMutex = new Mutex(); // Serialize event writes

  private async logEvent(event: GraphEvent): Promise<void> {
    await this.eventLogMutex.lock();
    try {
      this.eventLog.append(event);
      this.eventsSinceSnapshot++;
    } finally {
      this.eventLogMutex.unlock();
    }
  }
}
```

**Alternative:** Use actor system's message queue as serialization point
- Events logged in order of message processing
- No additional locking needed
- Ordering matches causality

### 2. Error Handling (Corrupt Log Recovery)

**Scenarios:**
- Partial write (process killed mid-event)
- Invalid JSON (disk corruption)
- Missing snapshot file

**Recovery Strategy:**
```typescript
class Graph {
  async load(): Promise<void> {
    try {
      await this.loadSnapshot();
    } catch (error) {
      console.warn("Snapshot load failed, starting from empty graph:", error);
    }

    try {
      this.replayEvents();
    } catch (error) {
      console.error("Event replay failed, using snapshot only:", error);
      // Continue with snapshot state
    }
  }

  private replayEvents(): void {
    let successCount = 0;
    let errorCount = 0;

    this.eventLog.replay((event) => {
      try {
        this.applyEvent(event);
        successCount++;
      } catch (error) {
        console.error(`Failed to apply event ${event.type}:`, error);
        errorCount++;
        // Continue with next event (skip corrupted event)
      }
    });

    console.log(`Replay complete: ${successCount} events applied, ${errorCount} errors`);
  }
}
```

### 3. Performance (Replay Time vs Log Size)

**Benchmarks (Estimated):**
- Pure event log: 1000 events/second replay
- Snapshot + 100 events: <1ms startup
- Snapshot + 10,000 events: ~10s startup

**Tuning Parameters:**
```typescript
const SMALL_GRAPH = {
  snapshotThreshold: 500,  // Snapshot every 500 events
  snapshotRetention: 3,     // Keep last 3 snapshots
};

const LARGE_GRAPH = {
  snapshotThreshold: 100,   // Snapshot more frequently
  snapshotRetention: 5,     // Keep more snapshots
};

const AUDIT_HEAVY = {
  snapshotThreshold: 1000,  // Snapshot less often (keep event detail)
  snapshotRetention: 10,    // Keep many snapshots for history
};
```

### 4. Testing (Replay Correctness)

**Test Strategy:**
```typescript
describe("Graph Persistence", () => {
  test("replay produces identical graph state", () => {
    const graph1 = new Graph({ eventLogPath: "./test1.jsonl" });

    // Apply mutations
    TaskActor({ goal: "Task 1", graph: graph1 });
    TaskActor({ goal: "Task 2", graph: graph1 });
    graph1.addEdge("task_1", "task_2", "depends_on");

    const state1 = graph1.dump();

    // Replay from events
    const graph2 = new Graph({ eventLogPath: "./test1.jsonl" });
    graph2.load();

    const state2 = graph2.dump();

    expect(state2).toEqual(state1);
  });

  test("snapshot + replay produces identical state", () => {
    const graph1 = new Graph({ snapshotThreshold: 2 });

    TaskActor({ goal: "Task 1", graph: graph1 });
    TaskActor({ goal: "Task 2", graph: graph1 }); // Triggers snapshot
    TaskActor({ goal: "Task 3", graph: graph1 }); // After snapshot

    const state1 = graph1.dump();

    // Load from snapshot + events
    const graph2 = new Graph();
    graph2.load();

    const state2 = graph2.dump();

    expect(state2).toEqual(state1);
  });
});
```

## Migration Path

### Phase 1: Add Persistence to Graph (Non-Breaking)
**Goal:** Integrate EventLog into Graph without breaking existing CLI

1. Add `load()` and `save()` methods to Graph
2. Add event logging to mutation methods (registerNode, addEdge, etc.)
3. Keep `dump()` method for backward compatibility
4. Default to empty event log path (no persistence unless opt-in)

**Result:** Graph has persistence capability, but CLI still uses old approach

### Phase 2: Migrate Task CLI (Breaking Change)
**Goal:** Replace manual JSON load/save with Graph persistence

1. Update `cmdAdd`, `cmdUpdate`, etc. to use `await graph.load()`
2. Remove manual `loadGraph()` and `saveGraph()` functions
3. Call `await graph.save()` only on shutdown (not after every mutation)
4. Keep tasks.json format for one release (migration tool)

**Result:** Task CLI uses new persistence, old tasks.json files work via migration

### Phase 3: Cleanup and Optimize
**Goal:** Remove old code, add advanced features

1. Remove `dump()` method (replaced by internal snapshot logic)
2. Add compaction (delete events older than last snapshot)
3. Add event log rotation (archive old logs)
4. Add migration tool for tasks.json → events.jsonl

**Result:** Clean, production-ready persistence layer

### Migration Tool Example

```typescript
// migrate-tasks.ts
async function migrateTasksJsonToEvents(
  oldPath: string,
  newEventLogPath: string,
  newSnapshotPath: string
): Promise<void> {
  // Load old format
  const oldContent = JSON.parse(readFileSync(oldPath, "utf-8"));

  // Create new graph with persistence
  const graph = new Graph({
    eventLogPath: newEventLogPath,
    snapshotPath: newSnapshotPath,
  });

  // Recreate nodes (automatically logged)
  for (const nodeProps of oldContent.nodes) {
    const factory = getActorFactory(nodeProps.type);
    factory({ ...nodeProps, graph });
  }

  // Recreate edges (automatically logged)
  for (const edge of oldContent.edges) {
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
  }

  // Save (creates snapshot)
  await graph.save();

  console.log(`Migration complete: ${oldContent.nodes.length} nodes, ${oldContent.edges.length} edges`);
}

// Usage
await migrateTasksJsonToEvents(
  "./tasks.json",
  "./tasks.events.jsonl",
  "./tasks.snapshot.json"
);
```

## Alternatives Considered

### Alternative 1: SQLite Database
**Approach:** Store nodes and edges in SQLite tables

**Pros:**
- ACID transactions
- SQL queries for complex filtering
- Indexed lookups (fast)

**Cons:**
- Heavier dependency (bun:sqlite)
- Less human-readable
- Overkill for small graphs
- Doesn't match actor model's message-passing semantics

**Verdict:** Good for large-scale systems, too complex for MVP

### Alternative 2: Git as Event Log
**Approach:** Commit each mutation to Git repository

**Pros:**
- Built-in versioning
- Branching for "what-if" scenarios
- Diffing for change visualization

**Cons:**
- Extremely slow (Git not designed for high-frequency commits)
- Complex setup
- Requires Git knowledge

**Verdict:** Interesting for future exploration, not practical now

### Alternative 3: Redis/BullMQ for Event Streaming
**Approach:** Use Redis streams for event log

**Pros:**
- Distributed (multiple processes)
- Pub/sub for real-time updates
- High throughput

**Cons:**
- External dependency (Redis server)
- Over-engineered for single-process use
- Network latency

**Verdict:** Good for distributed systems, unnecessary here

### Alternative 4: Keep Current Approach (Snapshot-Only)
**Approach:** No event log, just improve JSON snapshot performance

**Pros:**
- Simple (no new concepts)
- Human-readable

**Cons:**
- No audit trail
- No crash recovery
- No incremental updates
- Doesn't scale

**Verdict:** Not sustainable for production use

## Recommendations

### Priority 1: Core Persistence (Must-Have)
1. **Integrate EventLog into Graph**
   - Add `load()` and `save()` methods
   - Log events on all mutations
   - Implement replay logic

2. **Snapshot Strategy**
   - Snapshot every 100 events (tunable)
   - Load from snapshot + replay recent events
   - Keep last 3 snapshots for safety

3. **Migrate Task CLI**
   - Replace manual load/save with Graph persistence
   - Test with existing tasks.json files
   - Document migration process

### Priority 2: Reliability (Should-Have)
1. **Error Handling**
   - Graceful recovery from corrupt events
   - Fallback to snapshot-only mode
   - Logging and diagnostics

2. **Testing**
   - Replay correctness tests
   - Crash recovery tests
   - Performance benchmarks

3. **Compaction**
   - Delete events older than last snapshot
   - Archive old event logs

### Priority 3: Advanced Features (Nice-to-Have)
1. **Audit Trail API**
   - Query events by type, node, time range
   - Visualize change history
   - Export to CSV/JSON

2. **Time-Travel Debugging**
   - Replay to specific timestamp
   - Diff graph state at two points in time

3. **Distributed Coordination**
   - File locking for concurrent access
   - Conflict detection and resolution

## Conclusion

**Current State:** Graph is in-memory only, CLI manages persistence via manual JSON snapshots, EventLog implementation exists but is unused.

**Proposed State:** Graph owns persistence via event log + snapshot strategy, CLI transparently benefits from durability and audit trail.

**Key Insight:** The actor model's message boundaries are natural event log boundaries. Every actor message that mutates state should be logged as an immutable event.

**Next Step:** Implement Phase 1 (Add Persistence to Graph) as a non-breaking change, then migrate Task CLI in Phase 2.

**Success Metrics:**
- Graph state survives process crashes
- Startup time remains <1s for graphs up to 10,000 nodes
- Full audit trail available for debugging
- CLI code becomes simpler (no manual load/save logic)
