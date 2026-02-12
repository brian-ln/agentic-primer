# Workflow Examples

Examples demonstrating workflow orchestration on simplify's actor fabric.

## Examples

### 1. Build Pipeline (`build-pipeline.ts`)

**Pattern:** Linear pipeline
**Description:** CI/CD workflow with sequential stages

```typescript
compile → link → test → deploy
```

**Run:**
```bash
bun examples/workflows/build-pipeline.ts
```

**Output:**
- Visualizes workflow DAG
- Executes workflow with auto-propagation
- Shows task lifecycle transitions
- Reports final execution state

---

### 2. ETL Pipeline (`etl-workflow.ts`)

**Pattern:** Fan-out + Fan-in
**Description:** Extract from multiple sources, validate, transform, load

```typescript
       ┌→ extract-users ┐
start                     → validate → transform-users → load → notify
       └→ extract-events ┘           → transform-events ┘
```

**Run:**
```bash
bun examples/workflows/etl-workflow.ts
```

**Output:**
- Visualizes parallel extraction pattern

---

### 3. ML Training Pipeline (`ml-pipeline.ts`)

**Pattern:** Complex fan-out + fan-in
**Description:** Train multiple models in parallel, evaluate, deploy best

```typescript
                  ┌→ train-xgboost ┐
preprocess → feature-engineering ┼→ train-nn ——→ evaluate → select-best → deploy
                  └→ train-rf ——┘
```

**Run:**
```bash
bun examples/workflows/ml-pipeline.ts
```

**Output:**
- Visualizes parallel training pattern
- Shows diamond pattern for A/B testing

---

## Key Features Demonstrated

### 1. Auto-Execution
Tasks automatically start when all dependencies are satisfied.

### 2. Dependency Resolution
Uses `RelationshipActor` for graph-based dependency tracking.

### 3. Parallel Execution
Multiple independent tasks execute concurrently.

### 4. Event-Driven Orchestration
Completion events trigger dependent task execution.

### 5. Type Safety
Full TypeScript type checking end-to-end.

---

## Architecture

All examples use the same infrastructure:

```typescript
// Setup actors
const store = new GraphStore(dataDir);
await store.initialize();

const router = new MessageRouter(store, new ProgramManager(store));
const taskActor = new TaskActor('tasks', router, store);
const relationshipActor = new RelationshipActor('relationships', router);
const orchestrator = new WorkflowOrchestrator('orchestrator', router, store);

// Register
router.registerActor('tasks', taskActor);
router.registerActor('relationships', relationshipActor);
router.registerActor('orchestrator', orchestrator);

// Define + Execute
await orchestrator.receive({ type: 'define-workflow', payload: workflow });
await orchestrator.receive({ type: 'execute-workflow', payload: { workflowId } });
```

---

## See Also

- **Documentation:** `docs/WORKFLOW_ORCHESTRATION.md`
- **Tests:** `tests/workflow-orchestrator.test.ts`
- **Source:** `src/messaging/actors/workflow-orchestrator.ts`
