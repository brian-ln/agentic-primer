# Query/DSL Layer API Reference

**Status:** ✅ Complete
**Version:** 1.0
**Date:** 2026-02-05

## Overview

The Query/DSL layer provides a declarative, type-safe interface for querying and manipulating the actor fabric. Inspired by Cypher and the Halo paper, it enables:

- Pattern matching with NOT EXISTS constraints
- Cost-based query optimization
- Adaptive learning from execution history
- Query caching with semantic similarity
- Conditional actions (WHEN ... THEN)

## Quick Start

```typescript
import { query, pattern, filter, send } from '@simplify/query';

// Find ready tasks and auto-start them
const readyTasks = query()
  .match(
    pattern('task')
      .label('Task')
      .where({ status: 'open' })
      .notExists(
        pattern('blocker')
          .label('Task')
          .where({ status: 'open' })
          .relatedTo('task', { type: 'requires', direction: 'inbound' })
      )
  )
  .forEach(send('task').tell('start'));

await readyTasks.execute(queryExecutor);
```

## Pattern Matching

### PatternBuilder

Type-safe pattern construction with fluent API.

#### `pattern<T>(variable: string): PatternBuilder<T>`

Create a new pattern builder.

```typescript
const taskPattern = pattern<Task>('task')
  .label('Task')
  .where({ status: 'open' });
```

#### `.label(label: string): this`

Add label constraint.

```typescript
pattern('task')
  .label('Task')
  .label('Urgent');  // Multiple labels
```

#### `.where(properties: Partial<T>): this`

Add property constraints.

```typescript
pattern('task').where({
  status: 'open',
  priority: 'high',
  assignee: 'alice'
});
```

#### `.relatedTo(target: string, options): this`

Add relationship constraint.

**Options:**
- `type?: string` - Relationship type (e.g., 'requires', 'assignedTo')
- `direction: 'outbound' | 'inbound' | 'both'` - Relationship direction
- `properties?: Record<string, any>` - Properties on the relationship

```typescript
pattern('task').relatedTo('user', {
  type: 'assignedTo',
  direction: 'outbound',
  properties: { since: '2024-01-01' }
});
```

#### `.notExists(...patterns: PatternBuilder[]): this`

Add NOT EXISTS constraint (subquery).

```typescript
pattern('task')
  .label('Task')
  .where({ status: 'open' })
  .notExists(
    pattern('blocker')
      .label('Task')
      .where({ status: 'open' })
      .relatedTo('task', { type: 'requires', direction: 'inbound' })
  );
```

#### `.build(): PatternSpec`

Build the pattern specification.

#### `.getVariable(): string`

Get the variable name.

## Filter Expressions

### FilterBuilder

Type-safe filter construction for WHERE clauses.

#### `filter(variable: string, property: string): ComparisonBuilder`

Create a comparison filter builder.

```typescript
filter('task', 'priority').gte('medium')
```

### Comparison Operators

- `.eq(value)` - Equality (=)
- `.neq(value)` - Inequality (!=)
- `.gt(value)` - Greater than (>)
- `.lt(value)` - Less than (<)
- `.gte(value)` - Greater than or equal (>=)
- `.lte(value)` - Less than or equal (<=)
- `.contains(value)` - Contains (for strings/arrays)
- `.startsWith(value)` - Starts with (for strings)

```typescript
filter('task', 'priority').eq('high')
filter('task', 'score').gte(50)
filter('task', 'tags').contains('urgent')
```

### Logical Operators

Combine filters with logical operators.

#### `logic.and(...filters): FilterBuilder`

Logical AND.

```typescript
logic.and(
  filter('task', 'status').eq('open'),
  filter('task', 'priority').eq('high')
)
```

#### `logic.or(...filters): FilterBuilder`

Logical OR.

```typescript
logic.or(
  filter('task', 'status').eq('open'),
  filter('task', 'status').eq('ready')
)
```

#### `logic.not(filter): FilterBuilder`

Logical NOT.

```typescript
logic.not(filter('task', 'status').eq('closed'))
```

### Complex Filters

```typescript
logic.and(
  filter('task', 'status').eq('open'),
  logic.or(
    filter('task', 'priority').eq('high'),
    logic.and(
      filter('task', 'priority').eq('medium'),
      filter('task', 'tags').contains('urgent')
    )
  )
)
```

## Query Building

### QueryBuilder

Main query builder class.

#### `query(): QueryBuilder`

Create a new query builder.

```typescript
const q = query()
  .match(pattern('task').label('Task'))
  .return(['task']);
```

#### `.match(...patterns: PatternBuilder[]): this`

Add pattern matching clauses (MATCH).

```typescript
query().match(
  pattern('task').label('Task'),
  pattern('user').label('User')
)
```

#### `.where(...filters: FilterBuilder[]): this`

Add WHERE filter clauses.

```typescript
query()
  .match(pattern('task').label('Task'))
  .where(filter('task', 'priority').gte('medium'))
```

#### `.traverse(spec: TraversalSpec): this`

Add traversal operation.

**TraversalSpec:**
- `from: string` - Start variable
- `relationship?: string` - Relationship type
- `direction: 'outbound' | 'inbound' | 'both'` - Traversal direction
- `depth?: { min?: number; max?: number }` - Depth constraints
- `as: string` - Result variable name

```typescript
query()
  .match(pattern('root').label('Task'))
  .traverse({
    from: 'root',
    relationship: 'requires',
    direction: 'outbound',
    depth: { max: 5 },
    as: 'dependencies'
  })
```

#### `.aggregate(spec: AggregationSpec): this`

Add aggregation operation.

**AggregationSpec:**
- `operation: 'count' | 'collect' | 'group' | 'sum' | 'avg'` - Operation type
- `variable: string` - Variable to aggregate
- `by?: string` - Group by property (for group operation)
- `as: string` - Result variable name

```typescript
query()
  .match(pattern('tasks').label('Task'))
  .aggregate({
    operation: 'count',
    variable: 'tasks',
    as: 'taskCount'
  })
```

#### `.return(variables: string[]): this`

Specify variables to return.

```typescript
query()
  .match(pattern('task').label('Task'))
  .return(['task'])
```

#### `.forEach(action: ActionBuilder): this`

Add action to perform on each result.

```typescript
query()
  .match(pattern('task').label('Task'))
  .forEach(send('task').tell('start'))
```

#### `.when(...patterns: PatternBuilder[]): ConditionalBuilder`

Create conditional action (WHEN ... THEN).

```typescript
query()
  .match(pattern('test').label('Task'))
  .when(
    pattern('test').where({
      lifecycle: 'completed',
      result: { passed: true }
    })
  )
  .then(send('deploy').tell('start'))
```

#### `.withMetadata(metadata: Partial<QueryMetadata>): this`

Set query metadata for optimization.

```typescript
query()
  .match(pattern('task').label('Task'))
  .withMetadata({
    priority: 'high',
    expectedResults: 100
  })
```

#### `.build(): QueryDefinition`

Build the query definition.

#### `.matchPath(pattern: string, router?: MessageRouter): QueryDefinition[]`

Match hierarchical path patterns against registered actors/nodes.

Uses advanced pattern matching with wildcards to find all actors or nodes that match the given pattern. Returns an array of QueryDefinition objects with path pattern metadata.

**Supported Wildcards:**
- `*` - Matches exactly one segment (e.g., `domain/*` matches `domain/inference`)
- `**` - Matches zero or more segments (e.g., `domain/**` matches `domain/a/b/c`)
- `{a,b}` - Matches alternatives (e.g., `domain/{a,b}` matches `domain/a` or `domain/b`)

**Parameters:**
- `pattern: string` - Path pattern with wildcards
- `router?: MessageRouter` - Optional router for validation (future use)

**Returns:** Array of QueryDefinition with path pattern stored in metadata

```typescript
// Match all tasks under workflows
const queries = query().matchPath('/workflows/*/tasks');

// Match all actors in domain namespace
const queries = query().matchPath('/domain/**');

// Match specific service types
const queries = query().matchPath('/services/{llm,executor}');

// Use with router integration
const queries = query().matchPath('/workflows/**/tasks/*', router);
```

**Performance:** <5ms per operation

#### `.resolveAlias(alias: string, resolver: AliasResolver): Promise<string>`

Resolve alias path to canonical path using AliasResolver.

Uses the graph-based alias resolution system to translate alias paths to their canonical paths. Handles chained aliases and cycle detection.

**Parameters:**
- `alias: string` - Alias path to resolve (e.g., 'services/llm')
- `resolver: AliasResolver` - AliasResolver instance (required)

**Returns:** Promise resolving to the canonical path string

**Throws:** `AliasError` if alias is invalid or creates a cycle

```typescript
import { AliasResolver } from '@simplify/messaging';

const resolver = new AliasResolver(graphStore);
await resolver.createAlias('services/llm', 'domain/inference');

// Resolve alias
const canonicalPath = await query().resolveAlias('services/llm', resolver);
// => 'domain/inference'

// Use in queries
const path = await query().resolveAlias('services/llm', resolver);
query().match(pattern('actor').where({ path }))
  .return(['actor'])
  .execute(executor);
```

**Chained Aliases:**
```typescript
await resolver.createAlias('llm', 'services/llm');
await resolver.createAlias('services/llm', 'domain/inference');

const path = await query().resolveAlias('llm', resolver);
// => 'domain/inference' (follows chain)
```

**Performance:** <5ms per operation

## Actions

### ActionBuilder

Type-safe action construction for mutations.

#### `send(target: string): SendActionBuilder`

Create send action (message sending).

```typescript
// Fire-and-forget
send('task').tell('start')

// Request-response
send('task').ask('get', { id: '123' })

// Custom message
send('task').message({
  type: 'custom',
  payload: { data: 'test' },
  pattern: 'ask'
})
```

#### `update(target: string): UpdateActionBuilder`

Create update action (property modification).

```typescript
update('task').set({
  status: 'in_progress',
  startedAt: Date.now()
})
```

#### `create(entity: string): CreateActionBuilder`

Create create action (entity creation).

```typescript
create('task').as({
  title: 'New Task',
  status: 'open',
  priority: 'high'
})
```

## Query Compilation

### QueryCompiler

Translates QueryDefinition → QueryPlan with DAG representation.

```typescript
const compiler = new QueryCompiler();
const plan = await compiler.compile(queryDef, executionContext);
```

**Key features:**
- DAG-based query representation
- Signature-based operation canonicalization
- Cost model with state awareness
- Parallelism detection

**Query Plan Structure:**
```typescript
interface QueryPlan {
  id: string;                    // Plan hash
  steps: PlanStep[];             // Executable steps (DAG nodes)
  variables: string[];           // Variable bindings
  metadata: PlanMetadata;        // Cost estimates
  original: QueryDefinition;     // Original query
}
```

## Query Caching

### QueryCache

Plan caching with exact and semantic similarity matching.

```typescript
const cache = new QueryCache({
  maxCacheSize: 1000,
  maxStatsSize: 10000
});

// Exact match
const plan = cache.get(query, context);

// Semantic similarity
const similar = cache.findSimilar(query, 0.85);

// Record execution
cache.recordExecution(plan, stats);

// Get statistics
const stats = cache.getStatistics(plan);
```

**Features:**
- Exact match caching (query hash → plan)
- Semantic similarity search (find "close enough" queries)
- Query statistics tracking (latency, hit rate, success rate)
- LRU eviction policy
- Adaptive optimization (improve plans based on history)

## Query Execution

### QueryExecutor

Coordinates query execution using compiled plans.

```typescript
const executor = new QueryExecutor('query-executor', router);

// Execute query
const response = await executor.receive({
  type: 'execute',
  payload: { query: queryDef }
});

// Execute pre-compiled plan
const response = await executor.receive({
  type: 'execute-plan',
  payload: { plan }
});

// Get cache stats
const statsResponse = await executor.receive({
  type: 'get-cache-stats',
  payload: {}
});

// Clear cache
await executor.receive({
  type: 'clear-cache',
  payload: {}
});
```

**Execution Model:**
- Wavefront-style eager execution (Halo paper)
- Ready queue (dependencies satisfied)
- Parallel step execution (up to maxConcurrency)
- Warm actor tracking
- Result batching and streaming

## Examples

### Example 1: Ready Task Detection

Find tasks with no open blockers and auto-start them.

```typescript
const readyTasks = query()
  .match(
    pattern('task')
      .label('Task')
      .where({ status: 'open' })
      .notExists(
        pattern('blocker')
          .label('Task')
          .where({ status: 'open' })
          .relatedTo('task', { type: 'requires', direction: 'inbound' })
      )
  )
  .forEach(send('task').tell('start'))
  .build();
```

### Example 2: Dependency Chain Traversal

Find all dependencies of a task.

```typescript
const dependencies = query()
  .match(pattern('root').label('Task').where({ id: 'build' }))
  .traverse({
    from: 'root',
    relationship: 'requires',
    direction: 'outbound',
    depth: { max: 10 },
    as: 'deps'
  })
  .return(['root', 'deps'])
  .build();
```

### Example 3: Conditional Execution

Start deploy task if test passed.

```typescript
const conditionalDeploy = query()
  .match(pattern('test').label('Task').where({ id: 'test' }))
  .when(
    pattern('test').where({
      lifecycle: 'completed',
      result: { passed: true }
    })
  )
  .then(send('deploy').tell('start'))
  .build();
```

### Example 4: High-Priority Ready Tasks

Find high-priority tasks with no blockers.

```typescript
const highPriorityReady = query()
  .match(
    pattern('task')
      .label('Task')
      .where({ status: 'open' })
      .notExists(
        pattern('blocker')
          .label('Task')
          .where({ status: 'open' })
          .relatedTo('task', { type: 'requires', direction: 'inbound' })
      )
  )
  .where(filter('task', 'priority').eq('high'))
  .forEach(send('task').tell('start'))
  .build();
```

### Example 5: Task Aggregation

Count tasks by status.

```typescript
const taskCounts = query()
  .match(pattern('tasks').label('Task'))
  .aggregate({
    operation: 'group',
    variable: 'tasks',
    by: 'status',
    as: 'byStatus'
  })
  .return(['byStatus'])
  .build();
```

## Integration with WorkflowOrchestrator

The query layer integrates with WorkflowOrchestrator for automatic workflow execution.

### Integration Pattern

```typescript
// WorkflowOrchestrator receives task completion
onTaskComplete(taskId: string) {
  // Query for ready tasks
  const query = buildReadyTaskQuery();

  // Execute via QueryExecutor
  const response = await queryExecutor.ask('execute', { query });

  // Auto-start ready tasks
  for (const task of response.payload.result.bindings.get('task')) {
    await taskActor.tell('start', { id: task.id });
  }
}
```

### Benefits

- **Declarative workflow definition** - No manual polling
- **Query caching** - Reduces overhead on repeated executions
- **Complex dependency patterns** - NOT EXISTS, nested conditions
- **Conditional execution** - WHEN ... THEN patterns
- **Adaptive optimization** - Learns from execution history

## Performance Characteristics

### Query Compilation

- **Cost:** O(patterns + traversals + actions) - Linear in query size
- **Caching:** Plan cache hit rate typically >80% for repeated queries
- **Optimization:** Cost-based with warm actor awareness

### Query Execution

- **Parallelism:** Up to `maxConcurrency` steps in parallel
- **Makespan:** Critical path latency (longest dependency chain)
- **Total Work:** Sum of all step latencies
- **Cache benefit:** 50% faster for warm actors

### Query Cache

- **Exact match:** O(1) hash lookup
- **Semantic similarity:** O(N) linear scan (N = cache size)
- **LRU eviction:** O(N) to find LRU entry
- **Memory:** ~100KB per cached plan

## Type Definitions

### Core Types

```typescript
// Pattern specification
interface PatternSpec {
  variable: string;
  labels?: string[];
  where?: Record<string, any>;
  relationships?: RelationshipConstraint[];
  notExists?: PatternSpec[];
}

// Query definition
interface QueryDefinition {
  patterns: PatternSpec[];
  filters?: FilterExpression[];
  traversals?: TraversalSpec[];
  aggregations?: AggregationSpec[];
  actions?: ActionSpec[];
  returns?: string[];
  metadata?: QueryMetadata;
}

// Query plan
interface QueryPlan {
  id: string;
  steps: PlanStep[];
  variables: string[];
  metadata: PlanMetadata;
  original: QueryDefinition;
}

// Query result
interface QueryResult {
  planId: string;
  bindings: Map<string, any[]>;
  stats: ExecutionStats;
  success: boolean;
  error?: string;
}
```

## Error Handling

### Compilation Errors

```typescript
// Unknown variable reference
query()
  .match(pattern('task').label('Task'))
  .traverse({ from: 'unknownVar', ... })
// Throws: "Traversal references unknown variable: unknownVar"

// Action referencing unknown variable
query()
  .match(pattern('task').label('Task'))
  .forEach(send('unknownVar').tell('start'))
// Throws: "Action references unknown variable: unknownVar"
```

### Execution Errors

```typescript
// Actor not found
// Returns: { success: false, error: "Actor not found: @(unknown)" }

// Message timeout
// Returns: { success: false, error: "Message timeout" }
```

## Best Practices

### 1. Use Type Parameters

```typescript
interface Task {
  id: string;
  status: string;
  priority: string;
}

const tasks = pattern<Task>('task')
  .where({ status: 'open' }); // Type-safe!
```

### 2. Cache Reusable Queries

```typescript
// Define once, execute many times
const readyTaskQuery = query()
  .match(/* ... */)
  .build();

// Executes use cached plan
await executor.ask('execute', { query: readyTaskQuery });
```

### 3. Use Metadata for Optimization

```typescript
query()
  .match(/* ... */)
  .withMetadata({
    priority: 'high',      // Prioritize this query
    expectedResults: 10    // Hint for optimization
  })
```

### 4. Prefer Exact Patterns

```typescript
// Good: Specific labels and properties
pattern('task')
  .label('Task')
  .where({ status: 'open', priority: 'high' })

// Less efficient: Generic patterns
pattern('node')
  .where({ type: 'task' })
```

### 5. Leverage Parallelism

```typescript
// Independent patterns execute in parallel
query()
  .match(
    pattern('tasks').label('Task'),      // Parallel
    pattern('users').label('User'),      // Parallel
    pattern('docs').label('Document')    // Parallel
  )
```

## Roadmap (Phase 2)

Phase 2 will add:

- **Write operations:** CREATE, UPDATE, DELETE nodes/relationships
- **Transactions:** ACID guarantees for multi-step operations
- **Streaming results:** AsyncIterator-based result streaming
- **Query optimization:** Join reordering, predicate pushdown
- **Index hints:** Manual index selection for performance
- **Explain plans:** Query execution plan visualization

## References

- [Halo Paper](https://arxiv.org/abs/2312.13559) - Wavefront-style eager execution
- [Cypher Query Language](https://neo4j.com/docs/cypher-manual/) - Pattern matching syntax
- [BEAD Actor Architecture](/docs/BEAD_ACTOR_ARCHITECTURE.md) - Actor system architecture
- [Workflow Orchestration](/docs/WORKFLOW_ORCHESTRATION.md) - Integration patterns
