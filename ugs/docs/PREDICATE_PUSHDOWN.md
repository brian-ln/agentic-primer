# Predicate Pushdown Optimization

## Overview

Predicate pushdown is a query optimization technique that pushes filter predicates down to source actors for early filtering, reducing data transfer and intermediate result sizes.

## How It Works

### Basic Concept

Instead of fetching all data and filtering later:

```
Actor Query → Large Result Set → Filter → Small Result Set
```

Predicate pushdown filters at the source:

```
Actor Query + Filter → Small Result Set (filtered at source)
```

### Architecture

```
┌─────────────────┐
│  QueryCompiler  │
└────────┬────────┘
         │
         ├──> Pattern Compilation
         │
         ├──> Join Optimization
         │
         ├──> Predicate Pushdown ◄── NEW
         │    • Identify pushable filters
         │    • Rewrite query steps
         │    • Update cost estimates
         │
         └──> QueryPlan
```

## Usage

### Automatic (Default)

Predicate pushdown is enabled by default in the QueryCompiler:

```typescript
import { QueryCompiler } from './query/compiler.ts';
import { query, pattern, filter } from './query/index.ts';

const compiler = new QueryCompiler();

const q = query()
  .match(pattern('task').label('Task'))
  .where(filter('task', 'status').eq('open'))
  .build();

const plan = await compiler.compile(q);
// Filters are automatically pushed to the Task actor
```

### Manual Control

You can explicitly control the optimization:

```typescript
// Disable predicate pushdown
const compiler = new QueryCompiler({
  enablePredicatePushdown: false
});

// Or use the optimizer directly
import { PredicatePushdownOptimizer } from './query/optimizer/predicate-pushdown.ts';

const optimizer = new PredicatePushdownOptimizer();
const result = optimizer.optimize(plan);

if (result.optimized) {
  console.log(`Pushed ${result.stats.filtersPushedDown} filters`);
  console.log(`Estimated reduction: ${result.stats.estimatedReduction}%`);
}
```

## Supported Filters

### ✅ Can Push Down

1. **Equality filters** on single variables:
   ```typescript
   filter('task', 'status').eq('open')
   ```

2. **Comparison operators** (>, <, >=, <=):
   ```typescript
   filter('task', 'priority').gte(5)
   filter('task', 'createdAt').gt(Date.now() - 7*24*60*60*1000)
   ```

3. **AND combinations** of pushable filters:
   ```typescript
   logic.and(
     filter('task', 'status').eq('open'),
     filter('task', 'priority').eq('high')
   )
   ```

4. **Multiple independent filters** to different sources:
   ```typescript
   query()
     .match(
       pattern('task').label('Task'),
       pattern('user').label('User')
     )
     .where(
       filter('task', 'status').eq('open'),
       filter('user', 'active').eq(true)
     )
   // Each filter pushed to its respective actor
   ```

### ❌ Cannot Push Down

1. **Filters on multiple variables** (joins):
   ```typescript
   // Cannot push - requires data from both actors
   filter('task', 'assignee').eq(filter('user', 'id'))
   ```

2. **Filters after aggregations**:
   ```typescript
   query()
     .match(pattern('task').label('Task'))
     .aggregate({ operation: 'count', variable: 'task', as: 'count' })
     .where(filter('count', 'value').gt(10))
   // Cannot push - depends on aggregation result
   ```

3. **Complex OR conditions**:
   ```typescript
   logic.or(
     filter('task', 'priority').eq('high'),
     filter('task', 'status').eq('urgent')
   )
   // May not push - depends on actor support
   ```

4. **Filters to non-query steps** (traversals, actions):
   ```typescript
   query()
     .match(pattern('root').label('Task'))
     .traverse({ from: 'root', relationship: 'requires', as: 'deps' })
     .where(filter('deps', 'status').eq('open'))
   // Cannot push to traversal step
   ```

## Performance Impact

### Benchmarks

From `predicate-pushdown.bench.ts`:

| Scenario | Data Transfer Reduction | Result Size Reduction |
|----------|------------------------|----------------------|
| Single filter | 60% | 65% |
| Multiple filters | 90% | 87% |
| Range query | 60% | 65% |
| Multi-pattern | 80% | 75% |

### When to Use

**Best for:**
- Highly selective filters (filter out >50% of data)
- Large result sets from actors
- Multi-step queries with early filtering opportunities
- Network-bound workloads

**Less beneficial for:**
- Already filtered data (e.g., pattern.where)
- Small result sets (<100 rows)
- CPU-bound queries
- Complex filters requiring full scans anyway

## Implementation Details

### Filter Conversion

The optimizer converts DSL filters to actor-compatible format:

```typescript
// DSL filter
filter('task', 'priority').gt(5)

// Actor format
{
  priority: { $gt: 5 }
}

// DSL AND filter
logic.and(
  filter('task', 'status').eq('open'),
  filter('task', 'priority').eq('high')
)

// Actor format (merged)
{
  status: 'open',
  priority: 'high'
}
```

### Safety Checks

Before pushing, the optimizer verifies:

1. **Single variable** - Filter references only one variable
2. **Query step** - Target is a query step (not traverse/action)
3. **No aggregation dependency** - Filter doesn't depend on aggregate results
4. **No action before** - No mutation steps before the query

### Cost Estimation

After optimization, costs are updated:

- **Result count**: Reduced by estimated selectivity
- **Latency**: Slightly increased (~1ms) for filter overhead
- **Memory**: Reduced proportional to result reduction
- **Data transfer**: Significantly reduced

## Examples

### Example 1: Simple Filter

```typescript
// Before optimization
query()
  .match(pattern('task').label('Task'))
  .where(filter('task', 'status').eq('open'))

// Plan: Query all tasks → Filter by status
// Result: 1000 tasks fetched, 350 match

// After optimization
// Plan: Query tasks WHERE status='open'
// Result: 350 tasks fetched, 350 match
// Improvement: 65% less data transfer
```

### Example 2: Multiple Filters

```typescript
query()
  .match(pattern('task').label('Task'))
  .where(
    filter('task', 'status').eq('open'),
    filter('task', 'priority').eq('high'),
    filter('task', 'assignee').eq('alice')
  )

// Before: 1000 tasks → 35 match
// After: 35 tasks fetched directly
// Improvement: 96.5% less data transfer
```

### Example 3: Multi-Pattern with Selective Filters

```typescript
query()
  .match(
    pattern('task').label('Task'),
    pattern('user').label('User')
  )
  .where(
    filter('task', 'status').eq('open'),
    filter('user', 'active').eq(true)
  )

// Before:
//   - Fetch 1000 tasks
//   - Fetch 500 users
//   - Filter both
//   Total: 1500 entities transferred

// After:
//   - Fetch 350 open tasks
//   - Fetch 400 active users
//   Total: 750 entities transferred
// Improvement: 50% less data transfer
```

## Testing

### Unit Tests

Run optimizer tests:
```bash
bun test src/query/optimizer/predicate-pushdown.test.ts
```

30+ test cases covering:
- Basic pushdown
- Comparison operators
- Safety checks
- AND filter optimization
- Cost updates
- Signature regeneration
- Edge cases

### Integration Tests

Run integration tests:
```bash
bun test src/query/optimizer/predicate-pushdown-integration.test.ts
```

Tests validate:
- Compiler integration
- Semantics preservation
- Safety constraints
- Cost estimation
- Complex query handling

### Benchmarks

Run performance benchmarks:
```bash
bun src/query/optimizer/predicate-pushdown.bench.ts
```

Measures:
- Query execution time
- Data transfer volume
- Results processed
- Improvement percentages

## Configuration

### Compiler Options

```typescript
interface CompilerOptions {
  // Enable/disable predicate pushdown (default: true)
  enablePredicatePushdown?: boolean;

  // Custom optimizer instance
  predicatePushdownOptimizer?: PredicatePushdownOptimizer;

  // Other optimizations
  enableJoinOptimization?: boolean;
  joinOptimizer?: JoinOptimizer;
}

const compiler = new QueryCompiler({
  enablePredicatePushdown: true,
  // ... other options
});
```

### Optimization Result

```typescript
interface OptimizationResult {
  /** Optimized query plan */
  plan: QueryPlan;

  /** Whether any optimizations were applied */
  optimized: boolean;

  /** Transformation statistics */
  stats: OptimizationStats;
}

interface OptimizationStats {
  /** Number of filters pushed down */
  filtersPushedDown: number;

  /** Number of redundant filters eliminated */
  redundantFiltersRemoved: number;

  /** Number of steps modified */
  stepsModified: number;

  /** Estimated result size reduction (%) */
  estimatedReduction: number;
}
```

## Future Enhancements

1. **Partial pushdown** - Push part of OR conditions
2. **Dynamic optimization** - Use runtime statistics to decide
3. **Index-aware pushdown** - Consider available indexes
4. **Join condition pushdown** - Push join predicates
5. **Subquery pushdown** - Push nested query filters

## Related Documentation

- [Query Compiler](./QUERY_COMPILER.md)
- [Join Optimization](./JOIN_OPTIMIZATION.md)
- [Index Selection](./INDEX_SELECTION.md)
- [Query DSL](./QUERY_DSL.md)

## References

- Halo Paper: DAG-based query optimization
- PostgreSQL Query Planner: Predicate pushdown implementation
- Apache Calcite: Rule-based optimization
