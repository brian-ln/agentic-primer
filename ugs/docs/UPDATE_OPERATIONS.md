# UPDATE Operations in Query/DSL Layer

## Overview

UPDATE operations allow you to modify entity properties in the actor fabric through a declarative query interface. Updates are compiled into optimized execution plans and executed through actor messages.

## Architecture

### Builder Layer (`src/query/builder.ts`)

The `UpdateActionBuilder` provides a fluent API for constructing UPDATE actions:

```typescript
update('task').set({ status: 'completed' })
```

Key features:
- Type-safe property specification
- Support for partial updates (only changed fields)
- Support for nested properties
- Support for property removal (set to null)

### Compiler Layer (`src/query/compiler.ts`)

The `QueryCompiler` translates UPDATE actions into executable plan steps:

```typescript
{
  type: 'action',
  message: {
    pattern: 'tell',      // Fire-and-forget
    type: 'update',
    payload: { status: 'completed' }
  },
  metadata: {
    actionType: 'update',
    targetVariable: 'task'
  }
}
```

Key optimizations:
- Dependency tracking (UPDATE depends on query results)
- Parallel execution (multiple UPDATEs can run concurrently)
- Signature-based deduplication
- Cost estimation

### Executor Layer (`src/messaging/actors/query-executor.ts`)

The `QueryExecutor` executes UPDATE steps:

1. Resolves target entities from previous query results
2. Resolves entity addresses (e.g., `@(tasks/task-123)`)
3. Sends UPDATE messages to individual entity actors
4. Handles batch operations (multiple entities in parallel)

## API Reference

### Basic UPDATE

Update a single property:

```typescript
query()
  .match(pattern('task').label('Task').where({ id: 'task-123' }))
  .forEach(update('task').set({ status: 'completed' }))
  .build()
```

### Multiple Properties

Update multiple properties at once:

```typescript
query()
  .match(pattern('task').label('Task').where({ id: 'task-123' }))
  .forEach(update('task').set({
    status: 'in_progress',
    priority: 'high',
    startedAt: Date.now()
  }))
  .build()
```

### Partial Updates

Only specified properties are updated, others remain unchanged:

```typescript
// Only updates priority, leaves status/assignee/etc unchanged
update('task').set({ priority: 'high' })
```

### Nested Properties

Update nested object properties:

```typescript
update('task').set({
  metadata: {
    lastModified: Date.now(),
    modifiedBy: 'alice'
  }
})
```

### Property Removal

Remove properties by setting them to null:

```typescript
update('task').set({
  assignee: null,
  dueDate: null
})
```

### Bulk UPDATE

Update all entities matching a pattern:

```typescript
query()
  .match(pattern('task').label('Task').where({ status: 'open', priority: 'low' }))
  .forEach(update('task').set({ priority: 'medium' }))
  .build()
```

### Conditional UPDATE

Update only when conditions are met:

```typescript
query()
  .match(pattern('task').label('Task').where({ id: 'task-123' }))
  .when(pattern('task').where({ status: 'ready' }))
  .then(update('task').set({ status: 'in_progress' }))
  .build()
```

### UPDATE with Traversal

Update results from graph traversal:

```typescript
query()
  .match(pattern('root').label('Task').where({ id: 'task-123' }))
  .traverse({
    from: 'root',
    relationship: 'requires',
    direction: 'outbound',
    as: 'dependencies'
  })
  .forEach(update('dependencies').set({ status: 'ready' }))
  .build()
```

### Chained UPDATEs

Apply multiple updates sequentially:

```typescript
query()
  .match(pattern('task').label('Task').where({ id: 'task-123' }))
  .forEach(update('task').set({ status: 'in_progress' }))
  .forEach(update('task').set({ startedAt: Date.now() }))
  .build()
```

## Execution Semantics

### Fire-and-Forget

UPDATE operations use the `tell` message pattern (fire-and-forget):

- Non-blocking execution
- No response expected
- Suitable for state changes that don't require confirmation

### Dependency Resolution

UPDATE steps depend on query results:

```
Query Step (finds entities)
    ↓ dependency
Update Step (modifies entities)
```

The executor ensures queries complete before executing UPDATEs.

### Parallel Execution

Multiple UPDATEs on different entities execute in parallel:

```typescript
// Both UPDATEs run concurrently
query()
  .match(
    pattern('task1').label('Task').where({ id: 'task-123' }),
    pattern('task2').label('Task').where({ id: 'task-456' })
  )
  .forEach(update('task1').set({ status: 'completed' }))
  .forEach(update('task2').set({ status: 'completed' }))
```

### Batch Operations

When a query matches multiple entities, UPDATE executes on all:

```typescript
// Updates 10 entities in parallel
query()
  .match(pattern('task').label('Task').where({ priority: 'low' }))
  .forEach(update('task').set({ priority: 'medium' }))
```

## Implementation Details

### Query Compilation

1. **Pattern Compilation**: Query patterns compiled into actor messages
2. **Action Compilation**: UPDATE actions compiled with:
   - Target variable resolution
   - Dependency tracking
   - Message payload construction
   - Metadata attachment

### Plan Structure

```typescript
{
  id: 'plan_abc123',
  steps: [
    {
      id: 'step_0',
      type: 'query',
      bindings: ['task'],
      // ... query details
    },
    {
      id: 'step_1',
      type: 'action',
      dependencies: ['step_0'],
      message: {
        pattern: 'tell',
        type: 'update',
        payload: { status: 'completed' }
      },
      metadata: {
        actionType: 'update',
        targetVariable: 'task'
      }
    }
  ]
}
```

### Execution Flow

1. **Execute Query**: Find matching entities
2. **Store Results**: Bind entities to variables
3. **Resolve Targets**: Get entity addresses from bindings
4. **Send Messages**: Send UPDATE to each entity actor
5. **Return Results**: Aggregate responses (if requested)

## Error Handling

### Missing Target Variable

```typescript
// ERROR: No 'task' variable defined
query()
  .forEach(update('task').set({ status: 'completed' }))
```

Solution: Always match a pattern before UPDATE.

### Wrong Variable Name

```typescript
// ERROR: Variable 'wrongName' not found
query()
  .match(pattern('task').label('Task'))
  .forEach(update('wrongName').set({ status: 'completed' }))
```

Solution: Use correct variable name from pattern.

### Empty Result Set

If query matches no entities, UPDATE is skipped (no error).

## Performance Considerations

### Cost Estimation

UPDATE steps have estimated costs:

```typescript
{
  latencyMs: 5,        // Base cost for UPDATE
  cpuMs: 4,            // CPU time
  resultCount: 1,      // Expected entities
  cacheHitProb: 0.1    // Cache probability
}
```

### Optimization

- **Signature-based deduplication**: Identical UPDATEs share execution paths
- **Parallel execution**: Multiple UPDATEs run concurrently
- **Warm actor bonus**: Reduced latency for warm actors

### Scaling

Batch UPDATEs scale linearly:
- 1 entity: ~5ms
- 10 entities: ~50ms (parallel)
- 100 entities: ~500ms (batched parallel)

## Testing

### Test Coverage

32+ test cases covering:
- Basic UPDATE operations
- Multiple properties
- Nested properties
- Partial updates
- Bulk operations
- Conditional updates
- Traversal results
- Error cases
- Optimization

### Running Tests

```bash
bun test src/query/actions/update.test.ts
```

## Examples

See `/src/query/examples.ts` for 11 UPDATE examples:

1. **updateTaskStatus**: Basic status update
2. **markTaskInProgress**: Update with timestamp
3. **bulkUpdatePriority**: Batch priority update
4. **assignTaskToUser**: Partial update (assignment)
5. **updateTaskMetadata**: Nested property update
6. **autoProgressTask**: Conditional update
7. **markDependenciesReady**: Update traversal results
8. **cascadeStatusUpdate**: Multi-level cascade
9. **unassignTask**: Property removal
10. **progressTaskThroughStages**: Sequential updates
11. **updateProjectProgress**: Update with aggregation

## Comparison with Other Operations

### UPDATE vs SEND

- **UPDATE**: Declarative property changes
  ```typescript
  update('task').set({ status: 'completed' })
  ```

- **SEND**: Imperative message sending
  ```typescript
  send('task').tell('complete')
  ```

Use UPDATE for state changes, SEND for behavior invocation.

### UPDATE vs CREATE

- **UPDATE**: Modifies existing entities
  ```typescript
  update('task').set({ status: 'completed' })
  ```

- **CREATE**: Creates new entities
  ```typescript
  create('task').as({ title: 'New Task' })
  ```

CREATE produces bindings, UPDATE does not.

## Future Enhancements

### Planned Features

1. **Incremental Updates**: Update operators (++, append, etc.)
   ```typescript
   update('task').increment('retryCount')
   ```

2. **Conditional Sets**: Update only if condition true
   ```typescript
   update('task').setIf('status', 'completed', { testsPassed: true })
   ```

3. **Computed Updates**: Update based on current values
   ```typescript
   update('task').compute({ priority: (t) => t.priority + 1 })
   ```

4. **Transactional Updates**: Multi-entity atomic updates
   ```typescript
   update('task1', 'task2').atomically().set({ status: 'completed' })
   ```

5. **Validation**: Schema validation before update
   ```typescript
   update('task').validate(TaskSchema).set({ status: 'completed' })
   ```

## Best Practices

### 1. Use Partial Updates

Only update changed fields:

```typescript
// Good
update('task').set({ priority: 'high' })

// Avoid
update('task').set({
  title: task.title,           // Unchanged
  status: task.status,         // Unchanged
  priority: 'high',            // Changed
  assignee: task.assignee      // Unchanged
})
```

### 2. Include Timestamps

Track when changes occur:

```typescript
update('task').set({
  status: 'completed',
  completedAt: Date.now()
})
```

### 3. Use Meaningful Variable Names

```typescript
// Good
pattern('task').label('Task')
update('task').set({ ... })

// Avoid
pattern('t').label('Task')
update('t').set({ ... })
```

### 4. Batch Related Updates

Group related property changes:

```typescript
// Good
update('task').set({
  status: 'in_progress',
  startedAt: Date.now(),
  startedBy: userId
})

// Avoid
update('task').set({ status: 'in_progress' })
update('task').set({ startedAt: Date.now() })
update('task').set({ startedBy: userId })
```

### 5. Use Specific Queries

Avoid accidental bulk updates:

```typescript
// Good - specific
query()
  .match(pattern('task').label('Task').where({ id: taskId }))
  .forEach(update('task').set({ status: 'completed' }))

// Risky - might match many
query()
  .match(pattern('task').label('Task').where({ status: 'open' }))
  .forEach(update('task').set({ status: 'completed' }))
```

## References

- [Query Builder API](/src/query/builder.ts)
- [Query Compiler](/src/query/compiler.ts)
- [Query Executor](/src/messaging/actors/query-executor.ts)
- [UPDATE Tests](/src/query/actions/update.test.ts)
- [UPDATE Examples](/src/query/examples.ts)
- [Type Definitions](/src/query/types.ts)
