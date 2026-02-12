# Query Index Hints

**Status**: Phase 1 Complete (Stream C3)
**Version**: 1.0.0
**Last Updated**: 2026-02-05

## Overview

The index hints system enables manual and automatic optimization of graph queries through strategic index selection. It provides both declarative API for explicit hints and intelligent automatic selection based on query patterns and historical performance.

## Features

### 1. Manual Index Hints

Explicitly specify indexes for query optimization:

```typescript
import { query, pattern } from './query/builder.ts';

const q = query()
  .match(pattern('task').label('Task').where({ status: 'open' }))
  .useIndex('task', 'status', 'Optimize frequent status queries')
  .return(['task']);
```

**Key Characteristics:**
- Confidence: 1.0 (highest priority)
- Source: `manual`
- Chainable API for fluent query construction

### 2. Automatic Index Selection

The system automatically recommends indexes using multiple strategies:

#### Pattern-Based Strategy
Analyzes query structure to identify optimal indexes:
- **Equality filters**: High confidence (0.8)
- **Range queries**: Medium-high confidence (0.75)
- **Array membership**: Medium confidence (0.7)
- **Relationship traversals**: Medium confidence (0.7)

#### Cardinality Strategy
Prioritizes high-cardinality properties:
- **High cardinality** (id, uuid, email, hash): Confidence 0.85
- **Medium cardinality** (name, type, category): Confidence 0.6
- **Low cardinality** (boolean flags): Avoided

#### Composite Strategy
Suggests composite indexes for multi-property queries:
- Triggers for 2-4 properties
- Consistent ordering (alphabetical)
- Confidence: 0.65

#### Historical Strategy
Learns from past execution data:
- Requires ≥3 previous uses
- Minimum 10% improvement
- Success rate >80%
- Confidence based on historical performance

### 3. Index Effectiveness Tracking

The query cache tracks index performance over time:

```typescript
import { QueryCache } from './query/cache.ts';

const cache = new QueryCache();

// After execution
cache.recordExecution(plan, executionStats);

// Retrieve statistics
const stats = cache.getStatistics(plan);
const indexEff = stats?.indexEffectiveness?.get('status');

console.log(`Index "status" used ${indexEff?.useCount} times`);
console.log(`Average improvement: ${(indexEff?.avgImprovement * 100).toFixed(1)}%`);
console.log(`Success rate: ${(indexEff?.successRate * 100).toFixed(1)}%`);
```

**Tracked Metrics:**
- `useCount`: Number of times index was used
- `avgImprovement`: Performance gain (0-1, where 1 = 100% faster)
- `successRate`: Percentage of successful queries
- `avgResultCount`: Average results returned
- `lastUsedAt`: Last usage timestamp

## API Reference

### QueryBuilder.useIndex()

```typescript
useIndex(variable: string, index: string, reason?: string): QueryBuilder
```

**Parameters:**
- `variable`: Variable name from pattern matching (e.g., 'task', 'user')
- `index`: Index name/property to use
- `reason`: Optional description for the hint

**Returns:** QueryBuilder instance (chainable)

**Example:**
```typescript
query()
  .match(pattern('user').label('User').where({ email: 'alice@example.com' }))
  .useIndex('user', 'email', 'Email lookups are critical')
  .return(['user']);
```

### IndexSelector.selectIndexes()

```typescript
selectIndexes(
  query: QueryDefinition,
  statistics?: QueryStatistics[]
): IndexHint[]
```

**Parameters:**
- `query`: Query definition to analyze
- `statistics`: Optional historical statistics

**Returns:** Array of IndexHint objects sorted by confidence (descending)

### IndexHint Type

```typescript
interface IndexHint {
  variable: string;        // Variable to apply hint to
  index: string;           // Index name/property
  source: 'manual' | 'automatic';
  confidence?: number;     // 0-1, higher = more confident
  reason?: string;         // Explanation
}
```

### IndexEffectiveness Type

```typescript
interface IndexEffectiveness {
  indexName: string;
  useCount: number;
  avgImprovement: number;  // 0-1
  successRate: number;     // 0-1
  avgResultCount: number;
  lastUsedAt: number;
}
```

## Usage Patterns

### Basic Manual Hint

```typescript
const findUser = query()
  .match(pattern('user').label('User').where({ id: 'user-123' }))
  .useIndex('user', 'id')
  .return(['user']);
```

### Multiple Hints

```typescript
const complexQuery = query()
  .match(
    pattern('task').label('Task').where({ status: 'open', priority: 'high' }),
    pattern('user').label('User').where({ active: true })
  )
  .useIndex('task', 'status')
  .useIndex('task', 'priority')
  .useIndex('user', 'active')
  .return(['task', 'user']);
```

### Automatic with Selective Manual Override

```typescript
// Manual hint for 'task', automatic for 'user'
const query = query()
  .match(
    pattern('task').label('Task').where({ status: 'open' }),
    pattern('user').label('User').where({ email: 'alice@example.com' })
  )
  .useIndex('task', 'status', 'Critical path optimization')
  .return(['task', 'user']);

// Automatic hints will be generated for 'user' variable
```

### Historical Performance Analysis

```typescript
import { QueryCache } from './query/cache.ts';

const cache = new QueryCache();

// Execute query multiple times
for (let i = 0; i < 10; i++) {
  const result = await executor.execute(plan);
  cache.recordExecution(plan, result.stats);
}

// Analyze index effectiveness
const stats = cache.getStatistics(plan);
if (stats?.indexEffectiveness) {
  for (const [indexName, eff] of stats.indexEffectiveness.entries()) {
    console.log(`Index "${indexName}": ${eff.useCount} uses, ${(eff.avgImprovement * 100).toFixed(1)}% faster`);
  }
}
```

## Performance Considerations

### Cost Estimation

Index hints affect query cost estimation:

```typescript
// Cost reduction based on index confidence
const improvement = indexHint.confidence * 0.5;  // Up to 50% improvement
step.cost.latencyMs *= (1 - improvement);
step.cost.cpuMs *= (1 - improvement);
step.cost.cacheHitProb = Math.min(step.cost.cacheHitProb + improvement, 0.95);
```

### Strategy Priority

Strategies are applied in order of priority:
1. Pattern-based (priority: 100)
2. Historical (priority: 90)
3. Cardinality (priority: 80)
4. Composite (priority: 70)

### Hint Merging

- Duplicates are automatically deduplicated
- Highest confidence hint wins for each variable:index pair
- Manual hints always take precedence over automatic

## Best Practices

### 1. Use Manual Hints for Critical Paths

```typescript
// Critical query that needs consistent performance
query()
  .match(pattern('order').label('Order').where({ id: orderId }))
  .useIndex('order', 'id', 'Primary key lookup')
  .return(['order']);
```

### 2. Let Automatic Selection Handle Exploratory Queries

```typescript
// Automatic selection will optimize based on patterns
query()
  .match(pattern('task').label('Task').where({ status: 'open', assignee: userId }))
  .return(['task']);
```

### 3. Monitor Index Effectiveness

```typescript
// Periodically review index effectiveness
const allStats = cache.getAllStatistics();
for (const stats of allStats) {
  if (stats.indexEffectiveness) {
    for (const [name, eff] of stats.indexEffectiveness.entries()) {
      if (eff.avgImprovement < 0.1 && eff.useCount > 10) {
        console.warn(`Index "${name}" provides minimal benefit`);
      }
    }
  }
}
```

### 4. Prefer Composite Indexes for Multi-Property Filters

```typescript
// For queries that always filter on both properties
query()
  .match(pattern('task').label('Task').where({ status: 'open', priority: 'high' }))
  .useIndex('task', 'composite_priority_status')
  .return(['task']);
```

## Implementation Details

### Architecture

```
QueryBuilder.useIndex()
    ↓
QueryDefinition.metadata.indexHints
    ↓
QueryCompiler.compile()
    ↓ generateIndexHints()
    ├─ Manual hints (from metadata)
    └─ Automatic hints (IndexSelector)
    ↓ applyIndexHints()
    ├─ Add to step metadata
    ├─ Include in message payload
    └─ Adjust cost estimates
    ↓
QueryPlan.metadata.indexHints
    ↓
QueryExecutor.execute()
    ↓
QueryCache.recordExecution()
    ↓ updateIndexEffectiveness()
    └─ Track per-index metrics
```

### Files

- `src/query/builder.ts` - `.useIndex()` API
- `src/query/optimizer/index-selector.ts` - Automatic selection logic
- `src/query/compiler.ts` - Integration and cost adjustment
- `src/query/cache.ts` - Effectiveness tracking
- `src/query/types.ts` - Type definitions

### Testing

- **26 tests** in `index-selector.test.ts` (unit tests)
- **7 tests** in `builder.test.ts` (API tests)
- **13 tests** in `index-hints-integration.test.ts` (integration tests)
- **Total: 46 tests, all passing**

## Future Enhancements

### Potential Improvements

1. **Machine Learning Integration**
   - Train models on historical query patterns
   - Predict optimal indexes for new query shapes
   - Adaptive confidence scoring

2. **Index Creation Suggestions**
   - Analyze missing indexes
   - Generate DDL statements
   - Estimate storage impact

3. **Multi-Tenant Optimization**
   - Per-tenant index effectiveness
   - Workload-specific recommendations
   - Dynamic hint adjustment

4. **Cost-Based Join Optimization**
   - Consider index availability in join ordering
   - Factor index selectivity into cost models
   - Optimize multi-step traversals

## Examples

See comprehensive examples in:
- `src/query/optimizer/index-selector.test.ts`
- `src/query/optimizer/index-hints-integration.test.ts`
- `src/query/builder.test.ts`

## Changelog

### v1.0.0 (2026-02-05)
- Initial implementation
- Manual hint API (`.useIndex()`)
- Automatic selection (4 strategies)
- Cache effectiveness tracking
- 46 test cases, all passing
- Full integration with query compiler
