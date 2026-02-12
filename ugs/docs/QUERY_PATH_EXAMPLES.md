# Path-Based Query Examples

> **Status:** Phase 6 - Query Layer Integration Complete
> **Created:** 2026-02-06

## Overview

This document provides comprehensive examples of path-based actor queries in Simplify. Path-based filtering enables hierarchical actor discovery using patterns and wildcards.

## Table of Contents

1. [Basic Path Queries](#basic-path-queries)
2. [Wildcard Patterns](#wildcard-patterns)
3. [Recursive Discovery](#recursive-discovery)
4. [Combined Filters](#combined-filters)
5. [Performance Considerations](#performance-considerations)

---

## Basic Path Queries

### Exact Path Match

Find a specific actor by its exact path:

```typescript
import { query, pattern } from './query/index.ts';

// Find the inference actor at domain/inference
const q = query()
  .match(
    pattern('inference').where({
      path_exact: 'domain/inference'
    })
  )
  .return(['inference']);

const plan = await q.compile();
```

**Use Cases:**
- Direct actor lookup when you know the full path
- Most efficient query (uses index for equality)

---

### Path Prefix Match

Find all actors under a specific path prefix:

```typescript
// Find all tasks under the build pipeline
const q = query()
  .match(
    pattern('task').where({
      path_prefix: 'workflows/build-pipeline/tasks/'
    })
  )
  .return(['task']);

// Matches:
// - workflows/build-pipeline/tasks/compile
// - workflows/build-pipeline/tasks/test
// - workflows/build-pipeline/tasks/deploy/staging
```

**Use Cases:**
- Finding all children of a supervisor
- Scoped queries within a namespace
- Efficient (uses index for range scan)

---

## Wildcard Patterns

### Single-Level Wildcard (`*`)

Match exactly one path segment:

```typescript
// Find all top-level channels
const q = query()
  .match(
    pattern('channel').where({
      path_pattern: 'channels/*'
    })
  )
  .return(['channel']);

// Matches:
// - channels/logs
// - channels/metrics
// - channels/events
//
// Does NOT match:
// - channels (no segment after 'channels')
// - channels/logs/errors (too deep - 2 segments after 'channels')
```

**Use Cases:**
- Direct children only (no grandchildren)
- Single-level namespace isolation
- Controlled depth queries

---

### Wildcard in Middle

Use wildcards to match flexible hierarchies:

```typescript
// Find compile tasks in any workflow
const q = query()
  .match(
    pattern('task').where({
      path_pattern: 'workflows/*/tasks/compile'
    })
  )
  .return(['task']);

// Matches:
// - workflows/build/tasks/compile
// - workflows/deploy/tasks/compile
// - workflows/test/tasks/compile
//
// Does NOT match:
// - workflows/tasks/compile (missing middle segment)
// - workflows/build/prod/tasks/compile (too many segments)
```

**Use Cases:**
- Role-based discovery (find all "compile" tasks)
- Cross-workflow queries
- Structural pattern matching

---

### Multiple Wildcards

Combine multiple wildcards for complex patterns:

```typescript
// Find all executors in any domain
const q = query()
  .match(
    pattern('executor').where({
      path_pattern: '*/*/executor'
    })
  )
  .return(['executor']);

// Matches:
// - domain/inference/executor
// - workflows/build/executor
// - services/llm/executor
```

---

## Recursive Discovery

### Recursive Wildcard (`**`)

Match zero or more path segments:

```typescript
// Find ALL actors under services (any depth)
const q = query()
  .match(
    pattern('service').where({
      path_pattern: 'services/**'
    })
  )
  .return(['service']);

// Matches:
// - services/llm
// - services/stable/inference
// - services/a/b/c/d/e
```

**Use Cases:**
- Deep hierarchical discovery
- "Find everything under X"
- Namespace-wide queries

**Performance Note:** Recursive queries may require full table scans for complex patterns.

---

### Recursive in Middle

Use `**` in the middle of a pattern:

```typescript
// Find all 'task' actors anywhere under workflows
const q = query()
  .match(
    pattern('task').where({
      path_pattern: 'workflows/**/task'
    })
  )
  .return(['task']);

// Matches:
// - workflows/build/task
// - workflows/build/stage/prod/task
// - workflows/a/b/c/d/task
```

---

### Complex Recursive Patterns

Combine `*` and `**` for precise queries:

```typescript
// Find tasks at any depth in specific workflows
const q = query()
  .match(
    pattern('task').where({
      path_pattern: 'workflows/*/tasks/**'
    })
  )
  .return(['task']);

// Matches:
// - workflows/build/tasks/compile
// - workflows/build/tasks/test/unit
// - workflows/deploy/tasks/staging/push
//
// Does NOT match:
// - workflows/tasks/compile (missing middle segment)
// - services/build/tasks/compile (wrong root)
```

---

## Combined Filters

### Path + Property Filters

Combine path filters with regular property filters:

```typescript
// Find active tasks in the build pipeline
const q = query()
  .match(
    pattern('task').where({
      path_pattern: 'workflows/build/**',
      status: 'active',
      priority: 'high'
    })
  )
  .return(['task']);
```

**Performance:** Path filter narrows results first, then applies property filters.

---

### Multiple Patterns with Different Path Filters

Query multiple actor types with different path constraints:

```typescript
// Find build tasks AND deployment channels
const q = query()
  .match(
    pattern('task').where({
      path_prefix: 'workflows/build/tasks/'
    }),
    pattern('channel').where({
      path_pattern: 'channels/deploy/*'
    })
  )
  .return(['task', 'channel']);
```

---

### Path Discovery with Actions

Find actors and perform actions:

```typescript
import { send } from './query/builder.ts';

// Find all loggers and send restart command
const q = query()
  .match(
    pattern('logger').where({
      path_pattern: 'services/**/logger'
    })
  )
  .forEach(send('logger').tell('restart'));
```

---

### Conditional Actions Based on Path

Use path patterns for targeted operations:

```typescript
// Archive all tasks in completed workflows
const q = query()
  .match(
    pattern('workflow').where({
      path_prefix: 'workflows/',
      status: 'completed'
    }),
    pattern('task').where({
      path_pattern: 'workflows/*/tasks/**'
    })
  )
  .forEach(send('task').tell('archive'));
```

---

## Performance Considerations

### Query Optimization by Filter Type

| Filter Type | Index Usage | Performance | Use Case |
|-------------|-------------|-------------|----------|
| `path_exact` | Full index (equality) | ⚡️ Fastest | Known exact path |
| `path_prefix` | Index range scan | ⚡️ Fast | Hierarchical queries |
| `path_pattern: 'prefix/*'` | Partial index use | ⚡️ Fast | Direct children |
| `path_pattern: 'prefix/**'` | Full scan + filter | ⚠️ Slower | Deep recursion |
| `path_pattern: '**/suffix'` | Full scan + filter | ⚠️ Slowest | Leading wildcards |

---

### Best Practices

#### ✅ DO: Use Literal Prefixes

```typescript
// GOOD: Index can optimize prefix
pattern('task').where({
  path_pattern: 'workflows/build/**'
})
```

#### ❌ DON'T: Use Leading Wildcards

```typescript
// BAD: Forces full table scan
pattern('task').where({
  path_pattern: '**/tasks'
})

// BETTER: Add literal prefix if possible
pattern('task').where({
  path_pattern: 'workflows/**/tasks'
})
```

---

#### ✅ DO: Combine with Property Filters

```typescript
// GOOD: Path narrows scope, property filters refine
pattern('task').where({
  path_prefix: 'workflows/build/',
  status: 'active'
})
```

---

#### ✅ DO: Use path_prefix for Direct Children

```typescript
// GOOD: Efficient prefix match
pattern('task').where({
  path_prefix: 'workflows/build/tasks/'
})

// LESS EFFICIENT: Single wildcard with runtime check
pattern('task').where({
  path_pattern: 'workflows/build/tasks/*'
})
```

---

### Index Recommendations

For optimal performance, ensure paths are indexed:

```sql
-- Primary path index (exact + prefix queries)
CREATE INDEX idx_actors_path ON actors(path);

-- Covering index for common queries
CREATE INDEX idx_actors_path_status ON actors(path, status);
```

---

## API Reference

### Path Filter Properties

Use in `pattern().where()` clauses:

| Property | Type | Description | Example |
|----------|------|-------------|---------|
| `path_exact` | `string` | Exact path match | `'domain/inference'` |
| `path_prefix` | `string` | Path prefix match | `'workflows/build/'` |
| `path_pattern` | `string` | Wildcard pattern | `'channels/*'`, `'services/**'` |

### Wildcard Syntax

| Pattern | Meaning | Example | Matches |
|---------|---------|---------|---------|
| `*` | Exactly one segment | `channels/*` | `channels/logs` |
| `**` | Zero or more segments | `services/**` | `services/a/b/c` |
| Literal | Exact segment match | `domain/inference` | `domain/inference` |

---

## Real-World Examples

### Multi-Tenant Actor Discovery

```typescript
// Find all actors in Alice's workspace
const q = query()
  .match(
    pattern('actor').where({
      path_pattern: 'users/alice/**'
    })
  )
  .return(['actor']);
```

---

### Service Discovery

```typescript
// Find all stable inference services
const q = query()
  .match(
    pattern('service').where({
      path_pattern: 'services/stable/**',
      type: 'inference'
    })
  )
  .return(['service']);
```

---

### Workflow Task Monitoring

```typescript
// Find all failed tasks across all workflows
const q = query()
  .match(
    pattern('task').where({
      path_pattern: 'workflows/**/tasks/**',
      status: 'failed'
    })
  )
  .return(['task']);
```

---

### Channel Subscription by Path

```typescript
// Subscribe to all deployment channels
const q = query()
  .match(
    pattern('channel').where({
      path_pattern: 'channels/deploy/*'
    })
  )
  .subscribe({
    onMatch: (channels) => {
      console.log('New deployment channels:', channels);
    }
  });
```

---

## Summary

Path-based queries enable powerful hierarchical actor discovery:

- **Exact match** for direct lookups (fastest)
- **Prefix match** for namespace scoping (fast)
- **Wildcard patterns** for flexible structural queries
- **Recursive discovery** for deep hierarchies
- **Combined filters** for precise targeting

Choose the right filter type based on your performance requirements and query patterns.

---

## See Also

- [Path Addressing Design](/Users/bln/play/agentic-primer/simplify/docs/PATH_ADDRESSING_DESIGN.md)
- [Path Resolver Tests](/Users/bln/play/agentic-primer/simplify/src/messaging/__tests__/path-resolver.test.ts)
- [Query Builder API](/Users/bln/play/agentic-primer/simplify/src/query/builder.ts)
