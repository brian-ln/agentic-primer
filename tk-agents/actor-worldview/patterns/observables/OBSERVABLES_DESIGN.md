# Actor Observables/Signals Pattern

**Version:** 1.0
**Date:** 2026-01-18
**Status:** Design
**Purpose:** Pub/sub notification system for actor worldview

---

## Executive Summary

**Observables** provide a pub/sub notification system within the actor worldview. Producers send messages to named observables; subscribers receive those messages. The system handles discovery, routing, and delivery transparently.

**Core Insight:**
> Observables are **virtual router actors** with subscription semantics. They manage subscriber lists and broadcast messages to all subscribers.

**Key Properties:**
- **Simple producer API**: Send to observable address, done
- **Discoverable**: List available observables via graph query
- **Metadata filtering**: Subscribers can filter messages
- **Actor flexibility**: Handle via sub-actors or normal receive
- **System-managed delivery**: No manual routing logic

---

## The Observable Pattern

### What is an Observable?

An **observable** is a virtual router actor that:
1. Manages a list of subscriber actors
2. Receives messages from producers
3. Broadcasts messages to all subscribers
4. Supports metadata-based filtering

**Observables are NOT:**
- Event streams (no replay/history by default)
- RPC endpoints (no response expected)
- Queues (all subscribers get all messages)
- Channels (no point-to-point semantics)

### Observable Addressing

Observables use graph-based addressing within the actor hierarchy:

```
primer.observables.<observable-name>
```

**Examples:**
```
primer.observables.task_updates       # Task lifecycle events
primer.observables.graph_changes      # Graph mutation events
primer.observables.file_events        # File watcher events
primer.observables.system_health      # Health check events
```

**Router actor:** `primer.observables` manages the collection of observables

---

## Core Capabilities

### 1. Observable Discovery

Actors can discover available observables via graph queries:

```typescript
// List all observables
const observables = await system.send("primer.observables", "list");
// Result: ["task_updates", "graph_changes", "file_events", ...]

// Get observable metadata
const metadata = await system.send(
  "primer.observables.task_updates",
  "describe"
);
// Result: { name: "task_updates", schema: {...}, subscribers: 42 }
```

**Discovery via graph:**
```datalog
# Find all observables
?[name] := *observables[name, schema, created_at]

# Find observables matching topic
?[name, schema] := *observables[name, schema, _],
                    name ~ "task.*"
```

### 2. Subscription Management

Actors subscribe to observables to receive broadcasts:

```typescript
// Basic subscription
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "primer.agents.task_monitor",
  // No filter = receive all messages
});

// Subscription with metadata filter
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "primer.agents.priority_monitor",
  filter: {
    priority: ["P0", "P1"],  // Only P0/P1 tasks
    labels: ["critical"]      // Only critical label
  }
});

// Unsubscribe
await system.send("primer.observables.task_updates", "unsubscribe", {
  subscriber: "primer.agents.task_monitor"
});
```

**Subscription lifecycle:**
1. Send "subscribe" message to observable
2. Observable adds subscriber to routing table
3. Future broadcasts route to subscriber
4. Send "unsubscribe" to stop receiving

### 3. Message Broadcasting

Producers send messages to observable; all subscribers receive them:

```typescript
// Producer: Send to observable
await system.send("primer.observables.task_updates", "broadcast", {
  event: "task_completed",
  task_id: "task_42",
  priority: "P0",
  labels: ["critical", "agent"]
});

// System routes to ALL subscribers
// Subscribers with matching filters receive the message
```

**Broadcast semantics:**
- **Fire and forget**: Producer doesn't wait for subscriber responses
- **Best effort delivery**: System attempts delivery to all subscribers
- **No ordering guarantees**: Subscribers may receive out of order
- **Failure isolation**: One subscriber failure doesn't block others

### 4. Metadata-Based Filtering

Subscribers can filter messages using metadata:

```typescript
// Subscribe with filter
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "primer.agents.p0_monitor",
  filter: {
    priority: ["P0"],           // Match priority
    event: ["task_created", "task_blocked"],  // Match event types
    labels_any: ["critical"],   // Any of these labels
    labels_all: ["agent", "p0"] // All of these labels
  }
});
```

**Filter matching:**
- **Exact match**: `priority: ["P0"]` → Only messages with `priority: "P0"`
- **Any match**: `labels_any: ["a", "b"]` → Messages with label "a" OR "b"
- **All match**: `labels_all: ["a", "b"]` → Messages with label "a" AND "b"
- **No filter**: Subscriber receives all messages

**Filter evaluation:**
- System evaluates filter before delivery
- Non-matching messages are dropped (not delivered)
- Filter logic is system-managed (no manual checks)

### 5. Actor Flexibility

Subscribers can handle broadcasts via:

**A. Normal receive (direct handling):**
```typescript
actor TaskMonitor {
  receive(message) {
    if (message.type === "observable_broadcast") {
      const { event, task_id } = message.payload;
      // Handle broadcast directly
      this.handleTaskUpdate(event, task_id);
    }
  }
}
```

**B. Sub-actor delegation (virtual sub-actor):**
```typescript
actor TaskMonitor {
  async init() {
    // Spawn virtual sub-actor for broadcasts
    const listener = await this.spawnChild("listener", ListenerActor);

    // Subscribe sub-actor to observable
    await system.send("primer.observables.task_updates", "subscribe", {
      subscriber: listener.address
    });
  }
}

actor ListenerActor {
  receive(message) {
    // All broadcasts go to this sub-actor
    this.handleBroadcast(message);
  }
}
```

**C. Actual sub-actor (persistent listener):**
```typescript
// Supervisor spawns dedicated listener
actor TaskUpdateSupervisor {
  async init() {
    const listener = await this.spawnChild("task_listener", TaskUpdateListener);
    // Supervisor restarts listener if it crashes
  }
}
```

**Pattern choice:**
- **Normal receive**: Simple cases, low message volume
- **Virtual sub-actor**: Isolate broadcast handling, testability
- **Actual sub-actor**: High volume, fault tolerance, restart policy

---

## Design Patterns

### Pattern 1: Simple Broadcast/Subscribe

**Use case:** Notify all interested actors of events

```typescript
// Producer
await system.send("primer.observables.task_updates", "broadcast", {
  event: "task_completed",
  task_id: "task_42"
});

// Subscriber
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "primer.agents.monitor"
});
```

**When to use:**
- Event notifications (task lifecycle, file changes)
- System health broadcasts
- Low-volume updates

### Pattern 2: Filtered Subscriptions

**Use case:** Subscribe only to relevant subset of messages

```typescript
// High-priority monitor: Only P0/P1 tasks
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "primer.agents.priority_monitor",
  filter: { priority: ["P0", "P1"] }
});

// Agent monitor: Only agent tasks
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "primer.agents.agent_monitor",
  filter: { labels_any: ["agent"] }
});
```

**When to use:**
- High message volume, selective interest
- Role-based filtering (priority, labels, type)
- Performance optimization (reduce irrelevant deliveries)

### Pattern 3: Sub-Actor Listener

**Use case:** Isolate broadcast handling from main actor logic

```typescript
actor TaskManager {
  async init() {
    // Spawn listener sub-actor
    const listener = await this.spawnChild("updates", TaskUpdateListener);

    await system.send("primer.observables.task_updates", "subscribe", {
      subscriber: listener.address
    });
  }

  receive(message) {
    // Main actor handles normal messages
    // Broadcasts routed to listener sub-actor
  }
}

actor TaskUpdateListener {
  receive(message) {
    // Only handles broadcasts
    // Isolated from parent's main logic
  }
}
```

**When to use:**
- Separation of concerns (main logic vs broadcast handling)
- Testing (mock listener independently)
- High broadcast volume (isolate processing)

### Pattern 4: Fan-out Notification

**Use case:** One event triggers multiple independent reactions

```typescript
// Single broadcast
await system.send("primer.observables.task_completed", "broadcast", {
  task_id: "task_42",
  labels: ["critical", "agent"]
});

// Multiple subscribers react independently
// - Dashboard updates UI
// - Metrics collector records completion
// - Agent scheduler picks next work
// - Email notifier sends notification

// Each subscriber acts independently, failures isolated
```

**When to use:**
- Decoupled reactions to events
- Multiple independent side effects
- System-wide notifications

### Pattern 5: Observables as Graph Edges

**Use case:** Observables represent graph relationships

```typescript
// Observable represents "depends_on" relationship
// When task_A completes, notify all dependent tasks

// Subscribe dependent tasks
await system.send("primer.observables.task_42_completed", "subscribe", {
  subscriber: "primer.tasks.task_50"  // task_50 depends on task_42
});

await system.send("primer.observables.task_42_completed", "subscribe", {
  subscriber: "primer.tasks.task_51"  // task_51 depends on task_42
});

// When task_42 completes, broadcast to dependents
await system.send("primer.observables.task_42_completed", "broadcast", {
  event: "completed"
});
```

**When to use:**
- Dependency graphs (task dependencies, data flows)
- Reactive updates (graph mutations trigger cascades)
- Event-driven workflows

---

## Observable Router Actor Design

### Router Actor: `primer.observables`

**Responsibilities:**
1. Manage collection of observable instances
2. Route subscription/unsubscription messages
3. Provide discovery API (list observables)
4. Spawn observable instances on-demand (virtual actor pattern)

**Messages:**

```typescript
// List all observables
{ type: "list" }
→ ["task_updates", "graph_changes", "file_events", ...]

// Get specific observable metadata
{ type: "describe", observable: "task_updates" }
→ { name: "task_updates", subscribers: 42, schema: {...} }

// Create new observable (lazy creation via virtual actor)
// Observables auto-created on first subscribe or broadcast
```

### Observable Instance: `primer.observables.<name>`

**State:**
```typescript
{
  name: string;                // Observable name
  subscribers: Map<ActorAddress, Filter>; // Subscriber routing table
  schema?: MessageSchema;      // Optional message schema
  created_at: Timestamp;       // Creation time
}
```

**Messages:**

```typescript
// Subscribe
{
  type: "subscribe",
  subscriber: ActorAddress,
  filter?: MessageFilter
}

// Unsubscribe
{
  type: "unsubscribe",
  subscriber: ActorAddress
}

// Broadcast
{
  type: "broadcast",
  payload: Message  // Any serializable message
}

// Describe (metadata)
{ type: "describe" }
→ { name: string, subscribers: number, schema?: MessageSchema }
```

**Broadcast algorithm:**
```typescript
async broadcast(message: Message) {
  for (const [subscriber, filter] of this.subscribers) {
    if (this.matchesFilter(message, filter)) {
      // Fire-and-forget delivery
      await system.send(subscriber, "observable_broadcast", {
        observable: this.name,
        payload: message
      });
    }
  }
}
```

---

## Integration with Existing Actor Worldview

### Graph Addressing Integration

Observables fit naturally into the hierarchical addressing:

```
primer (root)
├─ primer.tasks (router)
│   ├─ primer.tasks.task_28 (virtual actor)
│   └─ primer.tasks.task_29 (virtual actor)
├─ primer.observables (router, NEW)
│   ├─ primer.observables.task_updates (virtual observable)
│   ├─ primer.observables.graph_changes (virtual observable)
│   └─ primer.observables.file_events (virtual observable)
├─ primer.graph (effect actor)
└─ primer.eventlog (effect actor)
```

**Router pattern consistency:**
- `primer.tasks` manages task instances
- `primer.observables` manages observable instances
- Both use virtual actor pattern (lazy creation)

### Virtual Actor Lifecycle

Observables use Orleans-style virtual actor pattern:

1. **Lazy creation**: Observable created on first subscribe/broadcast
2. **Single instance per name**: `primer.observables.task_updates` is unique
3. **Passivation**: Observable may be passivated if no subscribers
4. **System-managed**: Actor system handles lifecycle

**State persistence:**
- Subscriber lists persisted in graph
- Observables can be restored on passivation/reactivation
- Subscriptions survive daemon restarts

### Effect Actor Boundaries

Observables are **pure actors** (no external side effects):
- No file I/O
- No database writes
- No network calls
- Pure message routing and filtering

**Integration with effect actors:**
```typescript
// Effect actor subscribes to observable
await system.send("primer.observables.graph_changes", "subscribe", {
  subscriber: "primer.graph"  // Effect actor
});

// Observable broadcasts trigger effect actor side effects
actor CozoDBAdapter {
  receive(message) {
    if (message.type === "observable_broadcast") {
      // Side effect: Update CozoDB based on broadcast
      await this.client.query("...");
    }
  }
}
```

### System-Managed Placement

Observable delivery is system-managed:
- Subscribers may be local or remote
- System handles message routing (WebSocket, IPC, in-memory)
- Producers don't know subscriber locations
- Failures are isolated (one subscriber failure doesn't block others)

**Local optimization:**
If producer and subscribers are co-located, system may optimize:
- In-memory message passing (no serialization)
- Inline delivery (no actor queue overhead)
- Justified by fitness function (latency requirement)

---

## Example Use Cases

### Use Case 1: Task Lifecycle Notifications

**Scenario:** Dashboard needs real-time task updates

```typescript
// Dashboard subscribes to task updates
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "primer.dashboard.task_widget"
});

// Task actor broadcasts lifecycle events
await system.send("primer.observables.task_updates", "broadcast", {
  event: "task_completed",
  task_id: "task_42",
  priority: "P0"
});

// Dashboard receives broadcast, updates UI
actor DashboardTaskWidget {
  receive(message) {
    if (message.type === "observable_broadcast") {
      this.updateUI(message.payload);
    }
  }
}
```

### Use Case 2: Agent Coordination

**Scenario:** Multiple agents monitor task queue for work

```typescript
// Agents subscribe to task creation events
await system.send("primer.observables.task_created", "subscribe", {
  subscriber: "primer.agents.agent_1",
  filter: { priority: ["P0", "P1"] }  // Only high-priority
});

await system.send("primer.observables.task_created", "subscribe", {
  subscriber: "primer.agents.agent_2",
  filter: { labels_any: ["research"] }  // Only research tasks
});

// Task router broadcasts new tasks
await system.send("primer.observables.task_created", "broadcast", {
  task_id: "task_50",
  priority: "P0",
  labels: ["implementation"]
});

// agent_1 receives (matches P0)
// agent_2 does NOT receive (no "research" label)
```

### Use Case 3: File Watcher Events

**Scenario:** Multiple components react to file changes

```typescript
// Components subscribe to file events
await system.send("primer.observables.file_events", "subscribe", {
  subscriber: "primer.graph.updater",  // Update graph on changes
  filter: { path_pattern: "tasks.json" }
});

await system.send("primer.observables.file_events", "subscribe", {
  subscriber: "primer.dashboard.file_widget",  // Update UI
  filter: { path_pattern: "*.md" }
});

// File watcher broadcasts file changes
await system.send("primer.observables.file_events", "broadcast", {
  event: "file_modified",
  path: "tasks.json",
  timestamp: "2026-01-18T12:30:00Z"
});

// Both graph updater and dashboard receive (both match)
```

### Use Case 4: Dependency Cascade

**Scenario:** Task completion triggers dependent task starts

```typescript
// Dependent tasks subscribe to dependency completion
await system.send("primer.observables.task_42_completed", "subscribe", {
  subscriber: "primer.tasks.task_50"  // Blocked by task_42
});

await system.send("primer.observables.task_42_completed", "subscribe", {
  subscriber: "primer.tasks.task_51"  // Also blocked by task_42
});

// When task_42 completes
await system.send("primer.observables.task_42_completed", "broadcast", {
  event: "completed"
});

// task_50 and task_51 receive broadcast, check if ready to start
```

---

## Comparison with Alternatives

### Observables vs Direct Messaging

**Direct messaging:**
```typescript
// Producer must know all consumers
await system.send("consumer1", "task_completed", {...});
await system.send("consumer2", "task_completed", {...});
await system.send("consumer3", "task_completed", {...});
// Tight coupling, brittle
```

**Observables:**
```typescript
// Producer knows only observable
await system.send("primer.observables.task_updates", "broadcast", {...});
// System routes to subscribers
// Decoupled, extensible
```

**When to use observables:**
- Multiple consumers of same event
- Dynamic consumer set (add/remove subscribers)
- Loose coupling (producer doesn't know consumers)

**When to use direct messaging:**
- Point-to-point communication
- Request/response pattern
- Fixed sender/receiver pairs

### Observables vs Event Log

**Event log (append-only, persistent):**
- All events stored permanently
- Consumers can replay history
- Strong ordering guarantees
- Higher overhead (disk I/O)

**Observables (ephemeral, in-memory):**
- No persistence (broadcasts are fire-and-forget)
- No replay (new subscribers don't see past messages)
- No ordering guarantees
- Lower overhead (pure message routing)

**When to use observables:**
- Real-time notifications
- Ephemeral events (no replay needed)
- High throughput (avoid I/O overhead)

**When to use event log:**
- Audit trail (compliance)
- Event sourcing (rebuild state from history)
- Replay (new consumers need past events)

### Observables vs Polling

**Polling:**
```typescript
// Consumer polls for updates
setInterval(async () => {
  const updates = await system.send("primer.tasks", "list_updated");
  // Process updates
}, 1000);
// Wasteful, high latency
```

**Observables:**
```typescript
// Consumer subscribes, receives push
await system.send("primer.observables.task_updates", "subscribe", {
  subscriber: "consumer"
});
// Efficient, low latency
```

**When to use observables:**
- Real-time updates (low latency)
- Efficient (no wasted polls)
- Push model (event-driven)

**When to use polling:**
- External systems (no push support)
- Batch updates (periodic sync)
- Simplicity (no subscription management)

---

## Design Fitness Function

### Deliverables

**MUST:**
- [x] Observable addressing scheme (`primer.observables.<name>`)
- [x] Discovery mechanism (list, describe)
- [x] Subscription API (subscribe, unsubscribe)
- [x] Broadcast API (fire-and-forget delivery)
- [x] Metadata filtering (priority, labels, event types)
- [x] Integration with graph addressing
- [x] Virtual actor lifecycle (lazy creation, passivation)

**SHOULD:**
- [x] Example use cases (task updates, agent coordination, file events)
- [x] Pattern catalog (simple broadcast, filtered subscriptions, sub-actors)
- [x] Comparison with alternatives (direct messaging, event log, polling)
- [x] Observable router actor design

**MAY:**
- [ ] Performance characteristics (discussed as system-managed)
- [ ] Migration path (deferred to implementation phase)
- [ ] Advanced filtering (complex predicates, deferred to implementation)

### Performance Goals

**Observables are system-managed:**
- System optimizes delivery (local vs remote)
- Co-located actors may use in-memory delivery
- Remote actors use WebSocket/IPC
- Filter evaluation before serialization (avoid waste)

**Expected characteristics:**
- **Latency:** <10ms for local delivery, <100ms for remote
- **Throughput:** 1000+ broadcasts/sec for typical observables
- **Scalability:** 100+ subscribers per observable
- **Failure isolation:** One subscriber failure doesn't block others

**Optimization opportunities:**
- Inline delivery for co-located actors
- Batch broadcasts for high-volume observables
- Subscriber de-duplication (multiple filters on same subscriber)

### Success Criteria

**Design completeness:**
- [x] Observable pattern documented
- [x] Discovery mechanism defined
- [x] Subscription model specified
- [x] Producer API (simple broadcast)
- [x] Integration with actor worldview

**Consistency:**
- [x] Aligns with graph addressing
- [x] Uses virtual actor pattern
- [x] System-managed delivery
- [x] Format-agnostic (serializable messages)

**Actionability:**
- [x] Clear examples for each pattern
- [x] Use cases demonstrate real-world application
- [x] Design ready for implementation

---

## Next Steps

### Phase 1: Core Implementation

**Tasks:**
1. Implement `ObservableRouter` actor (`primer.observables`)
2. Implement `Observable` virtual actor instances
3. Add subscription/unsubscription messages
4. Implement broadcast with filtering
5. Integrate with actor system

**Deliverables:**
- `src/actors/observable-router.ts`
- `src/actors/observable.ts`
- Integration tests

### Phase 2: Graph Integration

**Tasks:**
1. Persist subscriber lists in CozoDB
2. Add observable schema to graph
3. Implement discovery queries
4. Support observable metadata

**Deliverables:**
- CozoDB schema for observables
- Graph queries for discovery
- Persistence integration

### Phase 3: Advanced Features

**Tasks:**
1. Complex filtering (predicates, expressions)
2. Subscription policies (rate limiting, backpressure)
3. Observable history (optional replay)
4. Metrics and monitoring

**Deliverables:**
- Advanced filter API
- Subscription policy configuration
- Metrics dashboard

---

## References

### Actor Model Foundations

- **Hewitt Actor Model (1973):** Message passing, addresses
- **Erlang/OTP:** Supervision trees, pub/sub via `pg` module
- **Orleans:** Virtual actors, grain directory, streams

### Pub/Sub Patterns

- **Observer Pattern (GoF):** Classical pub/sub
- **Reactive Extensions (Rx):** Observable streams
- **NATS:** Lightweight pub/sub messaging
- **Redis Pub/Sub:** Simple broadcast semantics

### Integration with Primer

- **WORLDVIEW.md:** Seven principles, design framework
- **ADDRESSING_CONVENTIONS.md:** Graph addressing, router actors
- **PURE_ACTOR_MODEL.md:** Virtual actor lifecycle
- **PATTERN_CATALOG.md:** Router pattern, effect actors

---

## Appendix: Observable Message Schema

### Subscribe Message

```typescript
{
  type: "subscribe",
  subscriber: ActorAddress,  // Subscriber actor address
  filter?: {
    // Exact match (value must equal one of these)
    priority?: string[],
    event?: string[],

    // Label matching
    labels_any?: string[],   // Match any of these labels
    labels_all?: string[],   // Match all of these labels

    // Path pattern (glob matching)
    path_pattern?: string,   // e.g., "*.md", "tasks.json"

    // Custom predicates (future)
    predicate?: FilterExpression
  }
}
```

### Unsubscribe Message

```typescript
{
  type: "unsubscribe",
  subscriber: ActorAddress
}
```

### Broadcast Message

```typescript
{
  type: "broadcast",
  payload: {
    // Arbitrary serializable message
    // Must include fields used in filters
    event?: string,
    priority?: string,
    labels?: string[],
    [key: string]: any
  }
}
```

### Observable Broadcast Envelope

```typescript
// Message delivered to subscribers
{
  type: "observable_broadcast",
  observable: string,      // Observable name
  payload: Message,        // Original broadcast payload
  timestamp: Timestamp     // Broadcast time
}
```

### Filter Matching Semantics

```typescript
function matchesFilter(message: Message, filter: Filter): boolean {
  // No filter = match all
  if (!filter) return true;

  // Exact match: value must be in allowed list
  if (filter.priority && !filter.priority.includes(message.priority)) {
    return false;
  }

  if (filter.event && !filter.event.includes(message.event)) {
    return false;
  }

  // Label matching: any
  if (filter.labels_any) {
    const hasAny = filter.labels_any.some(label =>
      message.labels?.includes(label)
    );
    if (!hasAny) return false;
  }

  // Label matching: all
  if (filter.labels_all) {
    const hasAll = filter.labels_all.every(label =>
      message.labels?.includes(label)
    );
    if (!hasAll) return false;
  }

  // Path pattern: glob matching
  if (filter.path_pattern) {
    const matches = micromatch(message.path, filter.path_pattern);
    if (!matches) return false;
  }

  return true;  // All filters passed
}
```

---

**End of Observable Design Document**

**Status:** Ready for review and implementation
**Next:** Create implementation tasks, delegate review
