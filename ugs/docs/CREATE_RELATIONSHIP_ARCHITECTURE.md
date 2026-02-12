# CREATE_RELATIONSHIP Architecture

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                      USER QUERY (DSL)                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  query()                                                    │
│    .match(                                                  │
│      pattern('task').where({ id: 'task-1' }),              │
│      pattern('blocker').where({ id: 'task-2' })            │
│    )                                                        │
│    .link('task', 'blocker', 'requires')                     │
│                                                             │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ build()
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   QUERY DEFINITION                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  patterns: [                                                │
│    { variable: 'task', labels: ['Task'], ... },            │
│    { variable: 'blocker', labels: ['Task'], ... }          │
│  ],                                                         │
│  actions: [                                                 │
│    {                                                        │
│      type: 'create_relationship',                          │
│      target: 'task',                                        │
│      params: {                                              │
│        from: 'task',                                        │
│        to: 'blocker',                                       │
│        type: 'requires',                                    │
│        properties: {}                                       │
│      }                                                      │
│    }                                                        │
│  ]                                                          │
│                                                             │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ compile()
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   QUERY COMPILER                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. compilePattern('task') → step_0                         │
│  2. compilePattern('blocker') → step_1                      │
│  3. compileCreateRelationshipAction()                       │
│     ├─ Validate 'task' exists (step_0)                      │
│     ├─ Validate 'blocker' exists (step_1)                   │
│     ├─ Create dependencies: [step_0, step_1]                │
│     ├─ Route to @relationships actor                        │
│     └─ Generate step_2                                      │
│                                                             │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ QueryPlan
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                      QUERY PLAN                             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  steps: [                                                   │
│    {                                                        │
│      id: 'step_0',                                          │
│      type: 'query',                                         │
│      actor: '@tasks',                                       │
│      bindings: ['task'],                                    │
│      dependencies: []                                       │
│    },                                                       │
│    {                                                        │
│      id: 'step_1',                                          │
│      type: 'query',                                         │
│      actor: '@tasks',                                       │
│      bindings: ['blocker'],                                 │
│      dependencies: []                                       │
│    },                                                       │
│    {                                                        │
│      id: 'step_2',                                          │
│      type: 'action',                                        │
│      actor: '@relationships',                               │
│      message: {                                             │
│        pattern: 'ask',                                      │
│        type: 'create',                                      │
│        payload: {                                           │
│          from: 'task',                                      │
│          to: 'blocker',                                     │
│          type: 'requires'                                   │
│        }                                                    │
│      },                                                     │
│      bindings: [],                                          │
│      dependencies: ['step_0', 'step_1'],                    │
│      metadata: {                                            │
│        actionType: 'create_relationship',                   │
│        fromVariable: 'task',                                │
│        toVariable: 'blocker',                               │
│        relationshipType: 'requires'                         │
│      }                                                      │
│    }                                                        │
│  ]                                                          │
│                                                             │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ execute()
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   QUERY EXECUTOR                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Execution Flow:                                            │
│                                                             │
│  1. Execute step_0 (query for 'task')                       │
│     └─ Result: { id: 'task-1', ... }                        │
│                                                             │
│  2. Execute step_1 (query for 'blocker')                    │
│     └─ Result: { id: 'task-2', ... }                        │
│                                                             │
│  3. Execute step_2 (create relationship)                    │
│     ├─ Check dependencies satisfied                         │
│     ├─ Resolve 'task' → { id: 'task-1', ... }               │
│     ├─ Resolve 'blocker' → { id: 'task-2', ... }            │
│     ├─ Build addresses:                                     │
│     │  ├─ from: @(tasks/task-1)                             │
│     │  └─ to: @(tasks/task-2)                               │
│     └─ Send message to @relationships                       │
│                                                             │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ ask(create, payload)
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                 RELATIONSHIP ACTOR                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  handleCreate(message):                                     │
│    1. Validate payload (type, from, to)                     │
│    2. Generate relationship ID                              │
│    3. Create Relationship object:                           │
│       {                                                     │
│         id: 'rel_xyz',                                      │
│         type: 'requires',                                   │
│         from: '@(tasks/task-1)',                            │
│         to: '@(tasks/task-2)',                              │
│         created: 1738742400000                              │
│       }                                                     │
│    4. Store in database                                     │
│    5. Return response:                                      │
│       {                                                     │
│         address: '@(relationships/requires/rel_xyz)',       │
│         relationship: { ... }                               │
│       }                                                     │
│                                                             │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ response
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                    QUERY RESULT                             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  {                                                          │
│    planId: 'abc123...',                                     │
│    bindings: {                                              │
│      'task': [{ id: 'task-1', ... }],                       │
│      'blocker': [{ id: 'task-2', ... }]                     │
│    },                                                       │
│    stats: {                                                 │
│      durationMs: 15,                                        │
│      stepsExecuted: 3,                                      │
│      messagesSent: 3,                                       │
│      resultsReturned: 2                                     │
│    },                                                       │
│    success: true                                            │
│  }                                                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Component Responsibilities

### 1. QueryBuilder (builder.ts)
**Responsibilities:**
- Provide fluent DSL for relationship creation
- Validate builder method parameters (e.g., strength bounds)
- Support two syntaxes: `createRelationship()` and `link()`
- Enable method chaining (strength, evidence, withProperties)

**Key Classes:**
- `CreateRelationshipActionBuilder`
- `QueryBuilder` (with createRelationship/link methods)

### 2. QueryCompiler (compiler.ts)
**Responsibilities:**
- Validate relationship action structure
- Ensure from/to variables exist in previous steps
- Create dependency graph (relationship depends on both nodes)
- Route to RelationshipActor
- Generate operation signatures for caching
- Estimate costs

**Key Method:**
- `compileCreateRelationshipAction()`

### 3. QueryExecutor (query-executor.ts)
**Responsibilities:**
- Resolve variable bindings from previous results
- Handle single and multiple entities (cartesian product)
- Convert entities to addresses
- Build and send messages to RelationshipActor
- Return created relationship(s)

**Key Method:**
- `executeCreateRelationshipAction()`

### 4. RelationshipActor (relationship.ts)
**Responsibilities:**
- Validate relationship creation requests
- Generate unique relationship IDs
- Store relationships in database
- Return relationship addresses
- (No changes needed - existing functionality)

## Dependency Graph Example

```
Compilation Phase:
┌─────────┐
│ step_0  │  Query for 'task'
│ (query) │
└────┬────┘
     │
     │ depends on
     ▼
┌─────────┐
│ step_2  │  Create relationship
│ (action)│
└─────────┘
     ▲
     │ depends on
     │
┌────┴────┐
│ step_1  │  Query for 'blocker'
│ (query) │
└─────────┘

Execution Phase:
1. Execute step_0 and step_1 in parallel
2. Wait for both to complete
3. Execute step_2 (create relationship)
```

## Cartesian Product Example

When multiple entities match:

```
Input:
  from: 'tasks' → [task-1, task-2, task-3]
  to: 'users' → [alice, bob]
  type: 'assignedTo'

Execution:
  Create relationships:
  1. task-1 → alice
  2. task-1 → bob
  3. task-2 → alice
  4. task-2 → bob
  5. task-3 → alice
  6. task-3 → bob

Result: 6 relationships created (3 × 2)
```

## Message Flow

```
QueryExecutor                RelationshipActor
     │                              │
     │  ask(create, payload)        │
     ├─────────────────────────────>│
     │                              │
     │                         [validate]
     │                         [generate ID]
     │                         [store in DB]
     │                              │
     │  response({ address, rel })  │
     │<─────────────────────────────┤
     │                              │
     │  return result to user       │
     ▼                              ▼
```

## Type Flow

```typescript
// User Input (DSL)
createRelationship('task', 'blocker', 'requires')
  .strength(0.9)
  .evidence('Critical blocker')

// ActionSpec (after build)
{
  type: 'create_relationship',
  target: 'task',
  params: {
    from: 'task',
    to: 'blocker',
    type: 'requires',
    properties: {
      strength: 0.9,
      evidence: 'Critical blocker'
    }
  }
}

// PlanStep (after compile)
{
  id: 'step_2',
  type: 'action',
  actor: '@relationships',
  message: {
    pattern: 'ask',
    type: 'create',
    payload: {
      from: '@(tasks/task-1)',
      to: '@(tasks/task-2)',
      type: 'requires',
      strength: 0.9,
      evidence: 'Critical blocker'
    }
  },
  dependencies: ['step_0', 'step_1'],
  metadata: {
    actionType: 'create_relationship',
    fromVariable: 'task',
    toVariable: 'blocker',
    relationshipType: 'requires'
  }
}

// Relationship (after execute)
{
  id: 'rel_abc123',
  type: 'requires',
  from: '@(tasks/task-1)',
  to: '@(tasks/task-2)',
  strength: 0.9,
  evidence: 'Critical blocker',
  created: 1738742400000
}
```

## Error Handling Flow

```
Compilation Errors:
┌─────────────────────────────────┐
│ Missing 'from' variable         │
│ → Throw: "unknown from variable"│
└─────────────────────────────────┘

┌─────────────────────────────────┐
│ Missing 'to' variable           │
│ → Throw: "unknown to variable"  │
└─────────────────────────────────┘

Execution Errors:
┌─────────────────────────────────┐
│ Entity not found in results     │
│ → Throw: "No entities found"    │
└─────────────────────────────────┘

┌─────────────────────────────────┐
│ Invalid relationship type       │
│ → RelationshipActor rejects     │
└─────────────────────────────────┘
```

## Optimization Opportunities

### Current Implementation
- Each relationship is a separate message
- Sequential execution (wait for dependencies)
- No batching

### Future Optimizations
1. **Batch Creation**
   - Group multiple relationship creations
   - Single message to RelationshipActor
   - Reduce message overhead

2. **Transaction Support**
   - Atomic multi-relationship creation
   - Rollback on failure
   - Consistency guarantees

3. **Parallel Creation**
   - Independent relationships can be created in parallel
   - Dependency analysis for parallelization

## Integration Points

### Existing Systems
- ✅ RelationshipActor (no changes needed)
- ✅ QueryCache (works with relationship queries)
- ✅ Plan visualization (EXPLAIN shows relationships)
- ✅ Cost estimation (relationship creation costs included)

### Future Systems
- Pattern matching on relationship properties
- Relationship updates (UPDATE relationships)
- Relationship deletion (DELETE relationships)
- Transactional queries
- Graph algorithms (shortest path, etc.)
