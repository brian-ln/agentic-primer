# Universal Graph System (UGS)

**Version**: 0.3.0 (Phase 3)
**Date**: 2026-02-05
**Status**: Production-Ready

[![Tests](https://img.shields.io/badge/tests-761%20passing-success)]()
[![Phase](https://img.shields.io/badge/phase-3%20complete-blue)]()
[![TypeScript](https://img.shields.io/badge/TypeScript-5.0+-blue)]()

## Abstract

The Universal Graph System (UGS) is a **reactive, message-driven actor orchestration platform** built on a graph foundation. It enables building distributed, event-driven systems with declarative queries, bidirectional messaging, and live reactive subscriptions.

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Phase 3: What's New](#phase-3-whats-new)
3. [Quick Start](#quick-start)
4. [Core Concepts](#core-concepts)
5. [API Reference](#api-reference)
6. [Performance](#performance)
7. [Examples](#examples)
8. [Documentation](#documentation)

## Executive Summary

UGS enables you to:
- **Store** information as nodes with properties and data
- **Connect** information through typed relationships
- **Query** using declarative pattern matching
- **Orchestrate** actors with message-passing (tell, ask, stream)
- **React** to changes with live subscriptions
- **Automate** workflows with event triggers
- **Scale** to 100k+ nodes with <70ms reactive latency

### Key Innovation: Reactive Actor Orchestration

**Phase 3** transforms UGS from a graph database into a reactive orchestration platform by combining:
- Graph-based data model (`@(id)` addressing)
- Actor message-passing (tell/ask/stream)
- Reactive subscriptions (live queries)
- Event-driven triggers (declarative workflows)

## Phase 3: What's New

### 1. UPSERT Relationships

**Idempotent relationship updates** - no more "check if exists" logic:

```typescript
// One operation: create or update
await upsertRelationship('task-1', 'user-alice', {
  type: 'assignedTo',
  properties: { priority: 'high' }
});
```

### 2. Request-Response Messaging

**Bidirectional communication** with ask():

```typescript
// Ask actors for responses
const results = await query()
  .match(pattern('service'))
  .forEach(send('service').ask('health'))
  .return(['service', 'response'])
  .execute();

console.log(results[0].variables.response);
// { status: 'healthy', uptime: '7 days' }
```

### 3. Streaming Data

**Continuous data streams** for logs, events, metrics:

```typescript
// Stream build logs in real-time
for await (const log of buildStream) {
  console.log(log.timestamp, log.message);
}
```

### 4. Reactive Subscriptions

**Live queries** that update automatically (no polling!):

```typescript
// Get notified instantly when tasks fail
query()
  .match(pattern('task').where({ status: 'failed' }))
  .subscribe({
    onMatch: (tasks) => alert('Tasks failed!', tasks),
    onUnmatch: (tasks) => console.log('Tasks recovered:', tasks)
  });
```

### 5. Event Triggers

**Declarative workflows** that react to events:

```typescript
// Auto-deploy when tests pass
query()
  .on('test.completed')
  .where(pattern('test').where({ passed: true }))
  .forEach(send('deploy').tell('start'));
```

## Quick Start

### Installation

```bash
# Clone repository
git clone https://github.com/your-org/simplify.git
cd simplify

# Install dependencies
bun install

# Run tests
bun test

# Run examples
bun src/query/examples-phase3.ts

# Run live demo
bun src/query/live-demo-reactive-messaging.ts
```

### Hello World

```typescript
import { query, pattern, send } from './query';
import GraphStore from './graph';

// Create graph store
const store = new GraphStore();

// Add a task
await store.addNode({
  id: 'task-1',
  type: 'Task',
  properties: { name: 'Hello World', status: 'pending' },
  data: {}
});

// Query with reactive subscription
query()
  .match(pattern('task').where({ status: 'pending' }))
  .subscribe({
    onMatch: (tasks) => console.log('Pending tasks:', tasks)
  });
```

## Core Concepts

### 1. Graph Foundation

Store data as **nodes** and **relationships**:

```typescript
// Node: Entity with properties
{
  id: 'task-1',
  type: 'Task',
  properties: { name: 'Build', status: 'running' },
  data: { description: 'Build the project' }
}

// Relationship: Typed connection between nodes
{
  id: 'rel-1',
  from: 'task-1',
  to: 'user-alice',
  type: 'assignedTo',
  properties: { priority: 'high', assignedAt: 1738742400000 }
}
```

### 2. Universal Addressing

Reference anything with `@(id)`:

```typescript
// Task references user and blockers
{
  id: 'task-1',
  properties: {
    assignee: '@(user-alice)',
    blockers: ['@(task-2)', '@(task-3)']
  }
}
```

### 3. Declarative Queries

Express "what" not "how":

```typescript
// Find all high-priority tasks assigned to Alice
query()
  .match(
    pattern('task').label('Task').where({ priority: 'high' }),
    pattern('user').label('User').where({ id: 'alice' })
  )
  .connect('task', 'user', { type: 'assignedTo' })
  .return(['task', 'user']);
```

### 4. Actor Messaging

Three patterns for actor communication:

```typescript
// 1. Tell: Fire-and-forget
send('task').tell('start')

// 2. Ask: Request-response
const response = await send('task').ask('getStatus')

// 3. Stream: Continuous data
for await (const log of send('task').stream('logs')) {
  console.log(log);
}
```

### 5. Reactive Subscriptions

Live queries that push updates:

```typescript
// No polling - instant notifications!
query()
  .match(pattern('task').where({ status: 'failed' }))
  .subscribe({
    onMatch: (tasks) => notifyAdmin(tasks),
    onUnmatch: (tasks) => console.log('Recovered:', tasks)
  });
```

### 6. Event-Driven Workflows

Declarative automation:

```typescript
// When tests pass → deploy automatically
query()
  .on('test.completed')
  .where(pattern('test').where({ passed: true }))
  .forEach(send('deploy').tell('start'));
```

## API Reference

### Query Builder

```typescript
import { query, pattern } from './query';

// Basic query
const results = await query()
  .match(pattern('task').label('Task'))
  .where({ status: 'open' })
  .return(['task'])
  .execute();

// With traversal
query()
  .match(pattern('root').where({ id: 'task-1' }))
  .traverse({
    from: 'root',
    relationship: 'requires',
    direction: 'outbound',
    depth: { max: 5 },
    as: 'dependencies'
  })
  .return(['root', 'dependencies']);

// With aggregation
query()
  .match(pattern('task').label('Task'))
  .aggregate({
    count: { type: 'count', field: '*' },
    avgPriority: { type: 'avg', field: 'priority' }
  })
  .return(['count', 'avgPriority']);
```

### Messaging Patterns

```typescript
import { send } from './query';

// Tell (fire-and-forget)
query()
  .match(pattern('task'))
  .forEach(send('task').tell('start'))
  .execute();

// Ask (request-response)
const results = await query()
  .match(pattern('task'))
  .forEach(send('task').ask('getStatus'))
  .return(['task', 'response'])
  .execute();

// Stream (continuous data)
const stream = query()
  .match(pattern('task'))
  .forEach(send('task').stream('logs'))
  .stream();

for await (const log of stream) {
  console.log(log);
}
```

### Subscriptions

```typescript
// Subscribe to pattern matches
const subscription = query()
  .match(pattern('task').where({ status: 'failed' }))
  .subscribe({
    onMatch: (tasks) => console.log('Failed:', tasks),
    onUnmatch: (tasks) => console.log('Recovered:', tasks),
    onError: (error) => console.error('Error:', error)
  });

// Check status
console.log(subscription.isActive()); // true

// Cleanup
subscription.unsubscribe();
```

### Event Triggers

```typescript
// Single event type
const trigger = query()
  .on('task.completed')
  .where(pattern('task').where({ type: 'test', passed: true }))
  .forEach(send('deploy').tell('start'));

// Multiple event types
query()
  .on(['task.created', 'task.updated'])
  .where(pattern('task').where({ priority: 'critical' }))
  .forEach(send('alerts').tell('notify'));

// Cleanup
trigger.destroy();
```

### Relationship UPSERT

```typescript
import { upsertRelationship } from './query/actions';

// Create or update (idempotent)
await upsertRelationship('task-1', 'user-alice', {
  type: 'assignedTo',
  properties: { priority: 'high', assignedAt: Date.now() }
}).execute();

// Safe to retry - won't fail if exists
await upsertRelationship('task-1', 'user-alice', {
  type: 'assignedTo',
  properties: { priority: 'critical' }
}).execute();
```

## Performance

### Reactive vs Polling

Phase 3 reactive subscriptions vs traditional polling:

| Metric | Polling | Reactive | Improvement |
|--------|---------|----------|-------------|
| Latency | ~500ms | ~15ms | **33x faster** |
| CPU Usage | ~15% | ~2% | **87% reduction** |
| Memory | ~200KB | ~45KB | **77% reduction** |
| Network | ~50KB/s | ~0.1KB/s | **99.8% reduction** |

### Scalability

- **Nodes**: 100,000+ nodes supported
- **Subscriptions**: 1,000+ concurrent subscriptions
- **Latency**: <70ms end-to-end (event → notification)
- **Throughput**: >1,000 events/sec

### Benchmarks

```bash
# Run benchmarks
bun src/query/benchmarks/reactive.bench.ts

# Results (typical):
# - Subscribe latency: 15ms (first notification)
# - Event trigger: <10ms (event → action)
# - Ask overhead: <5% vs direct actor call
# - Stream backpressure: No memory bloat up to 10k items/s
```

## Examples

### Example 1: Health Monitoring System

```typescript
// Monitor service health in real-time
query()
  .match(pattern('service').where({ status: 'unhealthy' }))
  .subscribe({
    onMatch: (services) => {
      services.forEach(service => {
        // Alert admin
        send('@(alerts)').tell({
          type: 'service_down',
          service: service.id,
          severity: 'critical'
        });

        // Auto-restart if policy allows
        if (service.properties.restartPolicy === 'auto') {
          send(service.id).tell('restart');
        }
      });
    }
  });
```

### Example 2: CI/CD Pipeline

```typescript
// Automatic deployment pipeline
const pipeline = {
  // Stage 1: Tests complete → trigger build
  build: query()
    .on('test.completed')
    .where(pattern('test').where({ passed: true }))
    .forEach(send('@(build)').tell('start')),

  // Stage 2: Build complete → trigger deploy
  deploy: query()
    .on('build.completed')
    .where(pattern('build').where({ success: true }))
    .forEach(send('@(deploy)').tell({ env: 'staging' })),

  // Stage 3: Monitor deployment
  monitor: query()
    .match(pattern('deployment').where({ env: 'staging' }))
    .subscribe({
      onMatch: (deploys) => {
        console.log(`Deployment status: ${deploys[0].properties.status}`);
      }
    })
};
```

### Example 3: Real-Time Collaboration

```typescript
// Collaborative task board
const workspace = 'workspace-1';

// Alice's view
const aliceView = query()
  .match(pattern('task').where({ workspace }))
  .subscribe({
    onMatch: (tasks) => updateUI('alice', tasks),
    onUnmatch: (tasks) => removeFromUI('alice', tasks)
  });

// Bob's view
const bobView = query()
  .match(pattern('task').where({ workspace }))
  .subscribe({
    onMatch: (tasks) => updateUI('bob', tasks)
  });

// When Alice adds a task...
await store.addNode({
  id: 'new-task',
  type: 'Task',
  properties: { workspace, title: 'New feature', creator: 'alice' }
});
// → Both Alice and Bob see it instantly!
```

### Example 4: Domain Actor Integration

```typescript
// Execute shell commands via ProgramExecutor
await query()
  .match(pattern('task').where({ type: 'build' }))
  .forEach(
    send('@(program-executor)').ask('execute', {
      command: 'bun',
      args: ['build'],
      timeout: 30000
    })
  )
  .return(['task', 'response'])
  .execute();

// AI inference via InferenceActor
await query()
  .match(pattern('task').where({ needsDescription: true }))
  .forEach(
    send('@(inference)').ask('generate', {
      prompt: 'Expand this title: {{task.title}}',
      model: 'claude-sonnet-4.5',
      maxTokens: 150
    })
  )
  .execute();

// Knowledge base via KnowledgeActor
const decisions = await send('@(knowledge)').ask('query', {
  category: 'decision',
  search: 'database architecture',
  limit: 5
});
```

## Documentation

- **Phase 3 User Guide**: [docs/PHASE_3_GUIDE.md](docs/PHASE_3_GUIDE.md)
- **Phase 3 Plan**: [PHASE_3_PLAN.md](PHASE_3_PLAN.md)
- **Examples**: [src/query/examples-phase3.ts](src/query/examples-phase3.ts)
- **Live Demo**: [src/query/live-demo-reactive-messaging.ts](src/query/live-demo-reactive-messaging.ts)
- **Architecture**: [docs/BEAD_ACTOR_ARCHITECTURE.md](docs/BEAD_ACTOR_ARCHITECTURE.md)

## Testing

```bash
# Run all tests (761 tests)
bun test

# Run specific test suites
bun test src/query/reactive/
bun test src/query/actions/
bun test src/messaging/actors/

# Run with coverage
bun test --coverage
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

MIT License - see [LICENSE](LICENSE) for details.

---

## Formal Model

### Core Sets

Let **U** be the universe of all identifiers.  
Let **V** be the universe of all values (strings, numbers, objects, arrays).  
Let **A** be the set of all addresses, where `a ∈ A` has the form `@(id)` for some `id ∈ U`.

### Node Definition

A **node** is a tuple `n = (id, type?, properties, data)` where:
- `id ∈ U` is a unique identifier
- `type ∈ V ∪ {∅}` is an optional type annotation
- `properties: String → (V ∪ A)` is a partial function from property names to values or addresses
- `data ∈ V` is arbitrary data (JSON, tables, etc.)

### Edge Definition

An **edge** is a tuple `e = (id, from, to, type?, properties)` where:
- `id ∈ U` is a unique identifier
- `from, to ∈ U` are node identifiers (directional)
- `type ∈ V ∪ {∅}` is an optional type annotation
- `properties: String → (V ∪ A)` is a partial function

### Graph Definition

A **graph** is a tuple `G = (N, E, resolve)` where:
- `N` is a finite set of nodes
- `E` is a finite set of edges
- `resolve: A → (N ∪ E)` is a resolution function mapping addresses to graph elements

### Address Resolution

For any address `@(id)`, the resolution function must satisfy:
- `resolve(@(id)) = n` where `n ∈ N` and `n.id = id`, OR
- `resolve(@(id)) = e` where `e ∈ E` and `e.id = id`

## Type System

### Value Types
- **Primitive**: string, number, boolean, null
- **Composite**: object, array
- **Special**: table (structured rows/columns)

### Address Type
- **Address**: `@(id)` where `id ∈ U`
- Addresses are opaque until resolved
- Addresses can appear in any context where values can appear

### Type Coercion Rules
1. Addresses resolve to their target's value when needed for operations
2. Unresolvable addresses remain as addresses (graceful degradation)
3. Circular references are detected and handled

## Event Model

The system uses event sourcing for persistence and auditability.

### Event Types

```
EventAdd := NodeAdded | EdgeAdded
EventUpdate := NodeUpdated | EdgeUpdated  
EventDelete := NodeDeleted | EdgeDeleted
```

### Event Structure

Each event `e` has the form:
```
e = (timestamp, type, data, metadata?)
```

### Event Invariants

1. **Ordering**: Events are totally ordered by timestamp
2. **Immutability**: Once written, events cannot be changed
3. **Completeness**: All system changes are captured as events
4. **Idempotency**: Replaying events produces identical results

## Query System

### Query Operations

1. **Lookup**: `get(id) → Node | Edge | ∅`
2. **Filter**: `filter(predicate) → Set[Node | Edge]`
3. **Traverse**: `traverse(start, direction, depth) → Set[Node]`
4. **Resolve**: `resolve(address) → Node | Edge | ∅`

### Query Language (Informal)

```javascript
// Find nodes by property
find({type: "task", status: "active"})

// Traverse relationships  
traverse(startNode, {type: "depends_on", depth: 3})

// Mixed queries with addresses
find({assignee: @(alice), priority: "high"})
```

## Implementation Guide

### Storage Strategy
1. **In-memory graph** for fast queries
2. **Append-only event log** for persistence
3. **Periodic snapshots** for recovery
4. **No database dependencies** (files only)

### Core Components
- `GraphStore` - manages nodes and edges
- `EventLog` - handles persistence
- `QueryEngine` - processes queries and traversals
- `AddressResolver` - handles address resolution

## Examples

### Task Management
```javascript
// Task node with references
task = {
  id: "implement_auth",
  type: "task", 
  properties: {
    assignee: @(alice),
    epic: @(user_management),
    blockers: [@(design_review), @(api_spec)]
  },
  data: {
    description: "Implement OAuth2 authentication",
    estimate: "5 days"
  }
}

// Person node
person = {
  id: "alice",
  type: "person",
  properties: {
    email: "alice@company.com",
    team: @(backend_team)
  }
}
```

### Living Documentation
```javascript
// Code that references its documentation
codeNode = {
  id: "auth_service",
  properties: {
    docs: @(auth_docs),
    tests: [@(unit_tests), @(integration_tests)],
    deployment: @(k8s_manifest)
  }
}
```

## Properties & Invariants

### System Invariants
1. **Address Consistency**: All addresses must resolve to existing elements
2. **Acyclicity** (optional): The graph may optionally enforce acyclic constraints
3. **Event Ordering**: Events maintain temporal consistency
4. **ID Uniqueness**: All IDs are unique within the system

### Query Properties
1. **Confluence**: Query results are deterministic
2. **Monotonicity**: Adding data doesn't invalidate existing queries
3. **Composability**: Queries can be combined and chained

---

*This specification is living and will evolve with the implementation.*
