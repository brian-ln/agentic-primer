# Workflow Orchestration on Simplify Actor Fabric

**Status:** ✅ Complete
**Version:** 1.0
**Date:** 2026-02-05

## Overview

Workflow orchestration enables **event-driven execution of algorithmic workflows** on simplify's actor fabric. Think Airflow DAGs, Temporal workflows, or Apache Beam pipelines, but using simplify's existing actor infrastructure.

This is not about building new actors - **everything you need already exists**. This implementation just wires together:

- **TaskActor** - Task lifecycle management (pending → assigned → in_progress → completed)
- **RelationshipActor** - Dependency graphs (requires, supports, contradicts edges)
- **WorkflowOrchestrator** - Event-driven auto-execution engine

## Architecture

### Core Insight

The gap was **automatic dependency resolution** - when a task completes, automatically start dependent tasks if all their dependencies are met.

```typescript
// Before: Manual task management
await taskActor.ask('start', { id: 'compile' });
// ... wait for completion ...
await taskActor.ask('start', { id: 'link' });  // Manual!

// After: Automatic propagation
await orchestrator.ask('execute-workflow', { workflowId: 'build-pipeline' });
// Orchestrator auto-starts dependent tasks when ready
```

### Components

#### 1. WorkflowOrchestrator Actor

**Location:** `src/messaging/actors/workflow-orchestrator.ts`

**Responsibilities:**
- Define workflows (task graphs with dependencies)
- Execute workflows (create task instances + dependency edges)
- Monitor task completions
- Auto-start ready tasks (all dependencies satisfied)

**Key Methods:**

```typescript
class WorkflowOrchestrator extends Actor {
  // Define workflow structure
  async handleDefineWorkflow(payload: WorkflowDefinition): Promise<void>

  // Execute workflow instance
  async handleExecuteWorkflow(workflowId: string): Promise<ExecutionId>

  // Core: Propagate completion to dependents
  async propagateCompletion(taskId: string): Promise<void>

  // Check if task ready (all dependencies completed)
  async isTaskReady(taskAddr: Address): Promise<boolean>
}
```

**Event Flow:**

```
TaskActor.complete(taskId)
    ↓
WorkflowOrchestrator.receive('task-completed', { taskId })
    ↓
propagateCompletion(taskId)
    ↓
RelationshipActor.query({ to: taskAddr, type: 'requires' })  // Find dependents
    ↓
For each dependent:
    isTaskReady(dependent) ?
        ↓
    TaskActor.ask('start', { id: dependent })  // Auto-execute!
```

#### 2. WorkflowBuilder (Fluent API)

**Location:** `src/messaging/actors/workflow-builder.ts`

**Purpose:** Declarative workflow construction with validation

```typescript
const workflow = buildWorkflow('build-pipeline', 'CI/CD Build Pipeline')
  .describe('Automated build, test, and deployment')
  .task('compile', {
    title: 'Compile TypeScript',
    priority: 'P0'
  })
  .task('link', {
    title: 'Link Dependencies',
    dependsOn: ['compile'],  // Dependency declaration
    priority: 'P0'
  })
  .task('test', {
    title: 'Run Tests',
    dependsOn: ['link'],
    priority: 'P0'
  })
  .build();  // Validates cycles, missing dependencies
```

**Features:**
- Cycle detection (DFS-based)
- Missing dependency validation
- ASCII DAG visualization
- Common patterns (linear, fan-out/fan-in, diamond)

#### 3. Existing Infrastructure (Used, Not Modified)

**TaskActor** (`src/messaging/actors/task.ts`):
- Already has full lifecycle: `create`, `start`, `complete`, `fail`
- Already persists to GraphStore
- Already emits events (for future port-based monitoring)

**RelationshipActor** (`src/messaging/actors/relationship.ts`):
- Already supports graph edges: `requires`, `supports`, `contradicts`, etc.
- Already has `traverse()` for BFS graph traversal
- Already persists relationships to LibSQLKnowledgeStore

**Actor Base Class** (`src/messaging/actor.ts`):
- Already has `tell`/`ask` patterns
- Already has `port()` for reactive broadcast
- Already has `stream`/`streamAsync` for long-running work

## Usage

### 1. Define Workflow

```typescript
import { buildWorkflow } from './workflow-builder.ts';

const workflow = buildWorkflow('etl-pipeline', 'ETL Data Pipeline')
  .task('extract-users', { title: 'Extract User Data' })
  .task('extract-events', { title: 'Extract Event Data' })
  .task('validate', {
    title: 'Validate Data',
    dependsOn: ['extract-users', 'extract-events']  // Fan-in
  })
  .task('transform', {
    title: 'Transform Data',
    dependsOn: ['validate']
  })
  .task('load', {
    title: 'Load to Warehouse',
    dependsOn: ['transform']
  })
  .build();
```

### 2. Execute Workflow

```typescript
// Setup actors
const store = new GraphStore('./data');
await store.initialize();

const router = new MessageRouter(store, new ProgramManager(store));
const taskActor = new TaskActor('tasks', router, store);
const relationshipActor = new RelationshipActor('relationships', router);
const orchestrator = new WorkflowOrchestrator('orchestrator', router, store);

router.registerActor('tasks', taskActor);
router.registerActor('relationships', relationshipActor);
router.registerActor('orchestrator', orchestrator);

// Define workflow
await orchestrator.receive({
  type: 'define-workflow',
  payload: workflow,
  ...messageProps
});

// Execute workflow
const execResp = await orchestrator.receive({
  type: 'execute-workflow',
  payload: { workflowId: 'etl-pipeline' },
  ...messageProps
});

const { executionId, taskIds } = execResp.payload;
// Root tasks (no dependencies) start immediately!
```

### 3. Complete Tasks (Triggers Auto-Execution)

```typescript
// Complete a task
await taskActor.receive({
  type: 'complete',
  payload: {
    id: taskIds['extract-users'],
    result: { rowCount: 1000 }
  },
  ...messageProps
});

// Notify orchestrator
await orchestrator.receive({
  type: 'task-completed',
  payload: { taskId: taskIds['extract-users'] },
  ...messageProps
});

// Orchestrator automatically:
// 1. Finds tasks depending on 'extract-users'
// 2. Checks if they're ready (all deps completed)
// 3. Auto-starts ready tasks
```

### 4. Monitor Execution

```typescript
// Get execution state
const execState = await orchestrator.receive({
  type: 'get-execution',
  payload: { id: executionId },
  ...messageProps
});

console.log(execState.payload.execution);
// {
//   id: "etl-exec-123",
//   workflowId: "etl-pipeline",
//   status: "running",
//   startedAt: 1234567890,
//   taskStates: {
//     "etl-exec-123-extract-users": "completed",
//     "etl-exec-123-extract-events": "in_progress",
//     "etl-exec-123-validate": "pending"
//   }
// }
```

## Workflow Patterns

### Linear Pipeline

```typescript
import { linearPipeline } from './workflow-builder.ts';

const workflow = linearPipeline('build', 'Build Pipeline', [
  { id: 'compile', title: 'Compile' },
  { id: 'link', title: 'Link' },
  { id: 'test', title: 'Test' },
  { id: 'deploy', title: 'Deploy' }
]);
// compile → link → test → deploy
```

### Fan-Out + Fan-In

```typescript
import { fanOutFanIn } from './workflow-builder.ts';

const workflow = fanOutFanIn(
  'ml-training',
  'ML Training Pipeline',
  { id: 'preprocess', title: 'Preprocess Data' },
  [
    { id: 'train-xgboost', title: 'Train XGBoost' },
    { id: 'train-nn', title: 'Train Neural Net' },
    { id: 'train-rf', title: 'Train Random Forest' }
  ],
  { id: 'evaluate', title: 'Evaluate Models' }
);
//           ┌→ train-xgboost ┐
// preprocess ┼→ train-nn ——→ evaluate
//           └→ train-rf ——┘
```

### Diamond Pattern

```typescript
import { diamond } from './workflow-builder.ts';

const workflow = diamond(
  'ab-test',
  'A/B Test Pipeline',
  { id: 'preprocess', title: 'Preprocess' },
  { id: 'train-baseline', title: 'Train Baseline' },
  { id: 'train-experimental', title: 'Train Experimental' },
  { id: 'compare', title: 'Compare Models' }
);
//           ┌→ train-baseline ┐
// preprocess                    → compare
//           └→ train-experimental ┘
```

## Examples

**Complete examples in `examples/workflows/`:**

1. **`build-pipeline.ts`** - CI/CD build pipeline (compile → link → test → deploy)
2. **`etl-workflow.ts`** - ETL data pipeline with parallel extraction
3. **`ml-pipeline.ts`** - ML training with parallel model training

Run examples:

```bash
bun examples/workflows/build-pipeline.ts
bun examples/workflows/etl-workflow.ts  # Visualization only
bun examples/workflows/ml-pipeline.ts   # Visualization only
```

## Implementation Details

### Dependency Resolution Algorithm

```typescript
async isTaskReady(taskAddr: Address): Promise<boolean> {
  // Find all tasks this one requires (outbound edges)
  const blockersResp = await relationshipActor.ask('traverse', {
    start: taskAddr,
    direction: 'outbound',
    relationshipType: 'requires',
    depth: 1
  });

  const blockers = blockersResp.payload.paths;

  // Ready if ALL blockers are completed
  for (const blocker of blockers) {
    const task = await getTask(blocker.node);
    if (!task || task.lifecycle !== 'completed') {
      return false;
    }
  }

  return true;
}
```

### Workflow Execution Lifecycle

```
1. Define Workflow
   ↓
2. Execute Workflow
   - Create task instances (TaskActor.create)
   - Create dependency edges (RelationshipActor.create)
   - Start root tasks (no dependencies)
   ↓
3. Task Completion
   - TaskActor.complete(taskId)
   - Notify orchestrator
   ↓
4. Propagate Completion
   - Query dependents (RelationshipActor)
   - Check if ready (all deps completed)
   - Auto-start ready tasks
   ↓
5. Repeat until all tasks complete
```

### Graph Representation

```
Tasks (Nodes):
  @(tasks/build-exec-123-compile)
  @(tasks/build-exec-123-link)
  @(tasks/build-exec-123-test)

Dependencies (Edges):
  @(relationships/requires/rel_456)
    from: @(tasks/build-exec-123-link)
    to: @(tasks/build-exec-123-compile)
    type: 'requires'

  @(relationships/requires/rel_789)
    from: @(tasks/build-exec-123-test)
    to: @(tasks/build-exec-123-link)
    type: 'requires'
```

## Testing

**Test Suite:** `tests/workflow-orchestrator.test.ts`

**Tests:**
- ✅ Define workflow
- ✅ Execute linear pipeline
- ✅ Auto-start dependent tasks
- ✅ Diamond pattern (fan-out + fan-in)
- ✅ Cycle detection
- ✅ List workflows and executions
- ✅ Get workflow by ID

Run tests:

```bash
bun test tests/workflow-orchestrator.test.ts
```

All tests pass ✓

## Future Enhancements

### 1. Port-Based Monitoring (Already Supported!)

```typescript
// WorkflowOrchestrator already extends Actor
// Can add port() for reactive monitoring

class WorkflowOrchestrator extends Actor {
  port(name: 'executions' | 'completions'): Channel<Event> {
    return this.createPort(name);
  }

  async propagateCompletion(taskId: string) {
    // ... existing logic ...

    // Broadcast completion event
    await this.getPort('completions').send({
      type: 'task-completed',
      taskId,
      timestamp: Date.now()
    });
  }
}

// Consumer
using completions = orchestrator.port('completions');
for await (const event of completions.subscribe()) {
  console.log('Task completed:', event.taskId);
}
```

### 2. Conditional Execution

```typescript
.task('deploy', {
  title: 'Deploy to Production',
  dependsOn: ['test'],
  condition: (results) => results.test.passed
})
```

### 3. Parallel Execution Limits

```typescript
.task('train-model', {
  title: 'Train Model',
  maxParallel: 3  // Limit concurrent instances
})
```

### 4. Retry Logic

```typescript
.task('api-call', {
  title: 'Call External API',
  retry: {
    maxAttempts: 3,
    backoff: 'exponential'
  }
})
```

### 5. Dynamic Workflow Generation

```typescript
// Generate tasks programmatically
const datasets = ['users', 'orders', 'products'];
const builder = buildWorkflow('etl', 'Dynamic ETL');

for (const dataset of datasets) {
  builder.task(`extract-${dataset}`, {
    title: `Extract ${dataset}`,
  });
}
```

## Comparison with Existing Systems

| Feature | Airflow | Temporal | Simplify Workflow |
|---------|---------|----------|-------------------|
| **Execution Model** | DAG scheduler | Event-driven | Event-driven |
| **State Management** | Database | Event sourcing | GraphStore + LibSQL |
| **Dependency Resolution** | Manual | Automatic | Automatic |
| **Actor Model** | No | No | Yes (native) |
| **Type Safety** | Python typing | TypeScript | TypeScript |
| **Persistence** | PostgreSQL | PostgreSQL/MySQL | GraphStore + LibSQL |
| **Graph Queries** | No | No | Yes (RelationshipActor) |

## Migration from bd CLI

The existing `bd` command system can be reimplemented as workflows:

```bash
# Before: bd ready (manual query + filter)
bd list tasks --filter lifecycle=pending --query dependencies

# After: Auto-executed workflow
orchestrator.ask('execute-workflow', { workflowId: 'research-pipeline' })
# Tasks automatically execute when dependencies satisfied
```

**Implementation:**

```typescript
// bd ready → workflow query
const readyTasks = await relationshipActor.ask('query', {
  filter: {
    // Tasks with all dependencies completed
  }
});
```

## Files

**Core Implementation:**
- `src/messaging/actors/workflow-orchestrator.ts` - Orchestrator actor
- `src/messaging/actors/workflow-builder.ts` - Fluent API + patterns

**Examples:**
- `examples/workflows/build-pipeline.ts` - CI/CD pipeline
- `examples/workflows/etl-workflow.ts` - ETL pipeline
- `examples/workflows/ml-pipeline.ts` - ML training pipeline

**Tests:**
- `tests/workflow-orchestrator.test.ts` - Comprehensive test suite

**Documentation:**
- `docs/WORKFLOW_ORCHESTRATION.md` - This file

## Summary

Workflow orchestration on simplify's actor fabric provides:

✅ **Event-driven execution** - Tasks auto-start when dependencies satisfied
✅ **Graph-based dependencies** - Uses existing RelationshipActor
✅ **Type-safe workflows** - TypeScript end-to-end
✅ **Persistent state** - GraphStore + LibSQL
✅ **Extensible patterns** - Linear, fan-out/fan-in, diamond, custom
✅ **Production-ready** - Full test coverage, validated examples

**Most importantly:** Built entirely on existing infrastructure. No new primitives needed.
