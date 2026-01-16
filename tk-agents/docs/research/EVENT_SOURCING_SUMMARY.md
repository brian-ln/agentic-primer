# Event Sourcing in the Actor System

## Summary

**YES!** Event capture and replay can be built directly into the actor system. Here are three approaches:

## Approach 1: Event Store as Separate Service

```typescript
const eventLog = new EventLog("/tmp/events.jsonl");
const graph = createPersistentGraph(eventLog);

// Operations append to event log
await graph.createTask({ id: "task-1", ... });
await graph.updateTaskStatus("task-1", "done");

// Replay from log
const events = await eventLog.loadAll();
const rebuiltGraph = await replayEvents(events, eventLog);
```

**Pros:**
- Simple, explicit
- Clear separation of concerns
- Easy to understand

**Cons:**
- Manual event logging in each operation
- Event log is separate from actor system

## Approach 2: EventStoreActor (Event Sourcing IS an Actor)

```typescript
// Event store is just another actor!
const eventStore = EventStoreActor("/tmp/events.jsonl");

// Capture event by sending message
await eventStore({
  type: "recordEvent",
  payload: { actorId: "task-1", message, response }
});

// Load events by sending message
const { data } = await eventStore({
  type: "loadEvents",
  payload: {}
});
```

**Pros:**
- Event store is an actor (uniform model!)
- Can be registered in system like any actor
- Can have multiple event stores

**Cons:**
- Still need to manually call event store
- Requires explicit integration

## Approach 3: Event-Sourced System (Automatic Capture)

The most elegant approach - **event capture is transparent**:

```typescript
// Create event-sourced system
const eventStore = EventStoreActor("/tmp/events.jsonl");
const system = EventSourcedSystem(eventStore);

// Register actors - they're automatically wrapped with event capture
await system.register("task-1", taskActor);

// Send messages - automatically captured as events!
await system.route({
  targetId: "task-1",
  message: { type: "updateStatus", payload: { status: "done" } }
});

// Replay all events
await system.replay();
```

**How it works:**

1. **EventStoreActor**: Persistence actor that stores events
2. **EventSourcedSystem**: Wraps the regular System
3. **wrapWithEventCapture**: Transparent wrapper around each actor
   - Captures message + response
   - Sends to EventStoreActor
   - Actor doesn't know it's being captured!

**Pros:**
- ✅ **Transparent**: Actors don't know about events
- ✅ **Automatic**: Every message becomes an event
- ✅ **Uniform**: Event store is just an actor
- ✅ **Composable**: Can wrap any system
- ✅ **Replay built-in**: System has `.replay()` method

## Key Insight: Messages = Events

In the actor model, **messages ARE events**:

```typescript
// A message
{
  id: "msg-123",
  type: "updateStatus",
  payload: { status: "done" },
  sender: "external"
}

// Is already an event!
// Just add timestamp and actorId:
{
  id: "event-456",
  timestamp: Date.now(),
  actorId: "task-1",
  message: { /* the message above */ },
  response: { success: true, data: { ... } }
}
```

**Replay = resend the same messages!**

## Activities vs State Changes

Important distinction:

### State Changes
- Captured as events
- Replayed to rebuild state
- Example: task status update, dependency added

### Activities (Real-world actions)
- Captured as events WITH RESULTS
- NOT re-executed on replay
- Example: test run, deployment, API call

```typescript
// Activity event includes the result
{
  type: "activity_completed",
  data: {
    activityId: "deploy-123",
    activityType: "deploy",
    result: {
      success: true,
      output: { deploymentId: "dep-abc", url: "https://..." }
    }
  }
}

// On replay: Use the result, don't re-execute!
```

## Combining with Virtual Actors

Perfect combination:

```typescript
const eventStore = EventStoreActor("/tmp/events.jsonl");
const system = EventSourcedSystem(eventStore);

// Virtual actor pattern + event sourcing
await system.getOrCreateActor("task-123", () => {
  return TaskActor({ id: "task-123", ... });
});

// Actor created on-demand, all messages captured automatically
await system.route({
  targetId: "task-123",
  message: { type: "updateStatus", payload: { status: "done" } }
});

// Crash, restart, replay - actor state restored
await system.replay();
```

## Why This Is Powerful

1. **Event store is an actor** - uniform model
2. **Every message is an event** - natural fit
3. **Transparent capture** - actors unaware
4. **Replay = message resend** - simple semantics
5. **Activities = events with results** - no re-execution
6. **Audit trail** - complete history
7. **Time travel** - replay to any point

## Next Steps

1. **Implement in real system**: Use EventSourcedSystem wrapper
2. **Add snapshots**: Periodic state dumps to speed replay
3. **Event compaction**: Merge redundant events
4. **Distributed event log**: Multiple event stores syncing
5. **CQRS**: Separate read/write models

## Implementation

See working examples:
- `examples/simple-persistent-graph.ts` - Basic event log
- `examples/event-sourced-actor-system.ts` - Built-in event sourcing

The functional actor model with hybrid methods makes event sourcing natural because:
- Actors are pure functions (deterministic replay)
- Messages are explicit (easy to serialize)
- System is composable (easy to wrap with EventSourcedSystem)
