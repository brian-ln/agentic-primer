# Query EXPLAIN Documentation

## Overview

The EXPLAIN functionality provides detailed analysis of query execution plans, including cost estimates, cache analysis, and optimization recommendations. It helps developers understand how queries will execute and identify performance bottlenecks.

## Quick Start

```typescript
import { query, pattern } from './src/query/index.ts';

// Basic EXPLAIN
const result = await query()
  .match(pattern('task').label('Task').where({ status: 'open' }))
  .return(['task'])
  .explain();

console.log(result.text);  // Human-readable explanation
console.log(result.tree);  // ASCII tree visualization
```

## API

### QueryBuilder.explain()

```typescript
async explain(
  options?: ExplainOptions,
  context?: ExecutionContext
): Promise<ExplainResult>
```

Compiles the query and generates a comprehensive execution plan analysis.

**Parameters:**
- `options` - Explain options (see below)
- `context` - Optional execution context (warm actors, cached data, resources)

**Returns:** `ExplainResult` containing:
- `plan` - Compiled query plan
- `text` - Human-readable explanation
- `tree` - ASCII tree visualization
- `optimizations` - Optimization recommendations
- `costBreakdown` - Detailed cost analysis
- `cacheAnalysis` - Cache hit predictions

### ExplainOptions

```typescript
interface ExplainOptions {
  verbose?: boolean;      // Show detailed step information
  costs?: boolean;        // Show cost breakdown (default: true)
  cache?: boolean;        // Show cache analysis (default: true)
  optimize?: boolean;     // Show optimization tips (default: true)
  format?: 'text' | 'json' | 'both';  // Output format
}
```

### QueryBuilder.compile()

```typescript
async compile(context?: ExecutionContext): Promise<QueryPlan>
```

Lower-level method that returns the compiled plan without explanation. Useful for execution or caching.

## Output Sections

### 1. Overview

High-level summary of the query plan:
- Number of steps
- Variables produced
- Critical path length and latency
- Total work (aggregate latency)
- Parallelization potential

```
OVERVIEW
  Steps: 2
  Variables: root, dependencies
  Critical Path: 2 steps (60.00ms)
  Total Work: 60.00ms
  Parallelizable: Yes
  Parallelism Benefit: 0.00ms saved (0.0%)
```

### 2. Execution Steps

Detailed breakdown of each step:
- Step ID and type (QUERY, TRAVERSE, ACTION, etc.)
- Target actor
- Dependencies (which steps must complete first)
- Variables produced
- Message details (type, payload)
- Cost estimates (latency, CPU, result count)
- Cache hit probability
- Parallelization flag

```
EXECUTION STEPS

  1. [step_0] QUERY → @(tasks)
     Dependencies: None (can start immediately)
     Produces: task
     Cost: 10.00ms (CPU: 8.00ms)
     Expected Results: 10 (cache hit prob: 10.0%)
     Parallel: Yes
```

### 3. Cost Breakdown

Visual breakdown of costs by step:
- Per-step latency with bar charts
- Percentage of total work
- Resource usage (memory, I/O, messages)

```
COST BREAKDOWN

  By Step (sorted by cost):
    step_1       █████████████████████████░░░░░ 50.00ms (83.3%)
    step_0       █████░░░░░░░░░░░░░░░░░░░░░░░░░ 10.00ms (16.7%)

  Resource Usage:
    Memory: 200.0 KB
    I/O Operations: 1
    Messages: 2
```

### 4. Cache Analysis

Cache hit predictions and recommendations:
- Overall cache hit probability
- Expected hits vs. misses
- Per-step cache probabilities with impact assessment
- Recommendations for warming actors

```
CACHE ANALYSIS

  Overall Cache Hit Probability: 10.0%
  Expected Hits: 0.2 / Misses: 1.8

  Per-Step Cache Probabilities:
    step_0       ██░░░░░░░░░░░░░░░░░░ 10.0% (impact: medium)
    step_1       ██░░░░░░░░░░░░░░░░░░ 10.0% (impact: high)

  Recommendations:
    - Warm up @(relationships) before query (expected 50.00ms latency)
```

### 5. Optimization Notes

Actionable recommendations for improving performance:
- Info: General information about execution
- Warnings: Performance concerns
- Tips: Specific optimization suggestions

```
OPTIMIZATION NOTES

  TIP:
    💡 Low parallelism utilization (0.0%). Most steps run sequentially.
    💡 Cold actor (10.0% cache hit). Consider warming up.
      (affects step_0)
```

### 6. Execution Tree

ASCII tree visualization showing dependencies:
- Tree structure with connectors
- Step types with icons [Q], [T], [A]
- Cost per step
- Cache indicators (⚡ hot, ❄ cold)
- Message patterns

```
EXECUTION TREE

└─ [Q] step_0 → tasks (10.0ms)
   → produces: task
   ← depends:
   ✉ ask query
   ❄ cache: cold (10%)

LEGEND
  ├─ Sequential dependency (must wait)
  ╠═ Parallel branch (can run concurrently)
  [Q] Query step
  [T] Traverse step
  [A] Action step
```

## Use Cases

### 1. Performance Analysis

Identify bottlenecks before execution:

```typescript
const result = await query()
  .match(pattern('root').label('Task').where({ id: 'build' }))
  .traverse({
    from: 'root',
    relationship: 'requires',
    direction: 'outbound',
    depth: { max: 10 },
    as: 'all_deps'
  })
  .explain({ costs: true });

// Check if traversal is too expensive
if (result.costBreakdown.totalLatency > 100) {
  console.warn('Query may be slow, consider limiting depth');
}
```

### 2. Cache Warming

Determine which actors to warm up:

```typescript
const result = await query()
  .match(pattern('task').label('Task'))
  .explain({ cache: true });

// Warm up actors with low cache hit rates
for (const step of result.cacheAnalysis.stepCacheProbs) {
  if (step.hitProb < 0.3 && step.impact === 'high') {
    console.log(`Should warm up actor for ${step.stepId}`);
  }
}
```

### 3. Query Optimization

Compare different query strategies:

```typescript
// Strategy 1: Filter after fetch
const result1 = await query()
  .match(pattern('task').label('Task'))
  .where(filter('task', 'status').eq('open'))
  .explain();

// Strategy 2: Filter in pattern
const result2 = await query()
  .match(pattern('task').label('Task').where({ status: 'open' }))
  .explain();

// Compare costs
console.log('Strategy 1:', result1.costBreakdown.totalLatency);
console.log('Strategy 2:', result2.costBreakdown.totalLatency);
```

### 4. Parallelism Analysis

Understand parallel execution opportunities:

```typescript
const result = await query()
  .match(
    pattern('task1').label('Task').where({ id: '1' }),
    pattern('task2').label('Task').where({ id: '2' })
  )
  .explain({ optimize: true });

if (result.plan.metadata.parallelizable) {
  console.log(`Can save ${result.costBreakdown.parallelismBenefit}ms with parallelism`);
}
```

### 5. Development & Debugging

Visualize complex queries during development:

```typescript
const result = await query()
  .match(pattern('test').label('Task').where({ id: 'test' }))
  .when(pattern('test').where({ lifecycle: 'completed' }))
  .then(send('deploy').tell('start'))
  .explain({ verbose: true });

// Use tree visualization to understand execution flow
console.log(result.tree);
```

## Execution Context

Provide execution context for more accurate cost estimates:

```typescript
const context: ExecutionContext = {
  warmActors: new Set([
    address('tasks'),
    address('relationships')
  ]),
  computationCache: new Map(),
  resources: {
    maxConcurrency: 4,
    availableMemory: 1024 * 1024 * 100
  },
  startTime: Date.now()
};

const result = await query()
  .match(pattern('task').label('Task'))
  .explain({}, context);

// With warm actors, cache hit probability will be higher
```

## Cost Model

The cost model estimates:

### Base Costs (ms)
- Query: 10ms
- Traverse: 50ms
- Action: 5ms
- Filter: 1ms
- Aggregate: 20ms

### Adjustments
- **Warm Actor Bonus**: 50% faster if actor is warm
- **Cache Hit Probability**: Based on actor warmth (cold: 10%, warm: 70%)
- **Result Count**: Estimated at 10 per step (simplified model)

### Parallelism
- **Makespan**: Critical path latency (longest dependency chain)
- **Total Work**: Sum of all step latencies
- **Parallelism Benefit**: Total Work - Makespan

## Optimization Tips

EXPLAIN generates recommendations for:

1. **Long Critical Path**: Reduce dependencies or restructure query
2. **Low Parallelism**: Find opportunities to run steps concurrently
3. **Expensive Steps**: Steps >50ms that dominate execution time
4. **Cold Actors**: Actors with <30% cache hit probability
5. **Many Small Steps**: Consider batching to reduce overhead
6. **High Memory Usage**: >100MB may need streaming or pagination
7. **High Message Count**: >50 messages may have significant overhead

## Examples

See `/src/query/explain/examples.ts` for comprehensive examples including:
- Basic EXPLAIN usage
- Cost and cache analysis
- Warm context comparison
- Optimization recommendations
- Query strategy comparison
- Execution flow visualization
- Complex workflow analysis

Run examples:
```bash
bun src/query/explain/examples.ts
```

## Testing

Run EXPLAIN tests:
```bash
bun test src/query/explain/explain.test.ts
```

Current test coverage: 35 test cases covering:
- API surface (explain(), compile())
- Text formatting
- Tree visualization
- Cost breakdown
- Cache analysis
- Optimization notes
- Complex queries and workflows

## Architecture

### Components

1. **QueryExplainer** (`explainer.ts`)
   - Main orchestrator
   - Coordinates formatting and analysis
   - Generates optimization notes

2. **PlanFormatter** (`plan-formatter.ts`)
   - Formats plans as human-readable text
   - Generates cost breakdowns
   - Produces cache analysis sections

3. **PlanVisualizer** (`plan-visualizer.ts`)
   - Creates ASCII tree diagrams
   - Generates execution flow visualizations
   - Groups steps by execution stage

4. **QueryCompiler** (`../compiler.ts`)
   - Compiles query definitions to plans
   - Estimates costs
   - Builds dependency graphs

### Data Flow

```
QueryBuilder
    ↓
.explain()
    ↓
QueryCompiler.compile()
    ↓
QueryPlan (with costs, dependencies)
    ↓
QueryExplainer.explain()
    ↓
┌─────────────┬─────────────┬─────────────┐
│ Formatter   │ Visualizer  │ Analysis    │
└─────────────┴─────────────┴─────────────┘
    ↓
ExplainResult (text, tree, optimizations, costs, cache)
```

## Future Enhancements

Potential improvements:
- Historical cost data from actual executions
- Machine learning-based cost predictions
- Index recommendation engine
- Query rewrite suggestions
- Interactive HTML visualization
- Cost-based query optimization
- Real-time execution monitoring
- Comparative analysis across queries

## Related Documentation

- [Query Builder](./QUERY_BUILDER.md) - Query construction API
- [Compiler](./COMPILER.md) - Query compilation
- [Cache](./CACHE.md) - Plan caching and statistics
- [Examples](../src/query/examples.ts) - Query examples
