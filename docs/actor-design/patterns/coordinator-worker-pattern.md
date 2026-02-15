# Coordinator-Worker Pattern

## Overview

The Coordinator-Worker Pattern implements a task queue with worker pool orchestration using Cloudflare Durable Objects. The CoordinatorActor manages task distribution and worker registration, while WorkerActors process tasks and report completion.

## When to Use

**Use Coordinator-Worker Pattern when:**
- You need **distributed task processing** across multiple workers
- Implementing **work queue semantics** (FIFO task assignment)
- Building **auto-scaling worker pools** (register on demand)
- Coordinating **stateful async work** with automatic task reassignment
- Need **visibility** into active tasks and queue depth

**Don't use when:**
- Tasks are **independent** and don't need coordination (use direct actor messaging)
- **Real-time streaming** required (this pattern queues, not streams)
- Workers are **ephemeral** (pattern assumes persistent DO workers)
- Need **priority queues** or complex scheduling (pattern is FIFO only)

## Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                      CoordinatorActor                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │
│  │   Workers    │  │  Task Queue  │  │ Active Tasks │         │
│  │   Set<id>    │  │  string[]    │  │ Map<id, wId> │         │
│  └──────────────┘  └──────────────┘  └──────────────┘         │
└────────┬───────────────────┬───────────────────┬───────────────┘
         │ assign_task       │ task_complete     │
         ▼                   ▼                   ▼
┌────────────────┐  ┌────────────────┐  ┌────────────────┐
│  WorkerActor   │  │  WorkerActor   │  │  WorkerActor   │
│  id: worker-1  │  │  id: worker-2  │  │  id: worker-3  │
│  tasks: 5      │  │  tasks: 3      │  │  tasks: 0      │
└────────────────┘  └────────────────┘  └────────────────┘
```

### Message Flow

```
Client                 Coordinator              Worker
  │                         │                     │
  ├─register_worker─────────►                     │
  │◄────registered──────────┤                     │
  │                         │                     │
  ├─assign_task("task-1")───►                     │
  │                         ├─process_task────────►
  │◄────assigned────────────┤                     │
  │                         │                     │
  │                         │◄─task_complete──────┤
  │                         │                     │
  ├─get_status──────────────►                     │
  │◄────{workers, tasks}────┤                     │
```

## Implementation

### CoordinatorActor

```typescript
export class CoordinatorActor extends DurableObject {
  private workers: Set<string> = new Set();
  private taskQueue: string[] = [];
  private activeTasks: Map<string, string> = new Map();

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);

    // Restore state from storage
    this.ctx.blockConcurrencyWhile(async () => {
      const state = await this.ctx.storage.get<{
        workers: string[];
        taskQueue: string[];
        activeTasks: [string, string][];
      }>("state");

      if (state) {
        this.workers = new Set(state.workers);
        this.taskQueue = state.taskQueue;
        this.activeTasks = new Map(state.activeTasks);
      }
    });
  }

  async handleMessage(message: CoordinatorMessage): Promise<unknown> {
    switch (message.type) {
      case "register_worker": {
        const workerId = message.from!;
        this.workers.add(workerId);
        await this.persist();

        // Auto-assign queued task if available
        if (this.taskQueue.length > 0) {
          const taskId = this.taskQueue.shift()!;
          await this.assignTask(taskId, workerId);
        }

        return { status: "registered", workerId };
      }

      case "assign_task": {
        const taskId = message.payload as string;

        const availableWorker = this.findAvailableWorker();
        if (availableWorker) {
          // Assign immediately
          await this.assignTask(taskId, availableWorker);
          return { status: "assigned", taskId, workerId: availableWorker };
        } else {
          // Queue for later
          this.taskQueue.push(taskId);
          await this.persist();
          return { status: "queued", taskId, position: this.taskQueue.length };
        }
      }

      case "task_complete": {
        const { taskId, workerId } = message.payload as {
          taskId: string;
          workerId: string;
        };
        this.activeTasks.delete(taskId);
        await this.persist();

        // Auto-assign next queued task to freed worker
        if (this.taskQueue.length > 0) {
          const nextTask = this.taskQueue.shift()!;
          await this.assignTask(nextTask, workerId);
        }

        return { status: "completed", taskId };
      }

      case "get_status": {
        return {
          workers: Array.from(this.workers),
          queuedTasks: this.taskQueue.length,
          activeTasks: Array.from(this.activeTasks.entries()),
        };
      }

      default:
        return { error: "Unknown message type" };
    }
  }

  private findAvailableWorker(): string | null {
    const busyWorkers = new Set(this.activeTasks.values());
    for (const workerId of this.workers) {
      if (!busyWorkers.has(workerId)) {
        return workerId;
      }
    }
    return null;
  }

  private async assignTask(taskId: string, workerId: string) {
    this.activeTasks.set(taskId, workerId);
    await this.persist();

    const workerStub = this.env.WORKER_ACTOR.get(
      this.env.WORKER_ACTOR.idFromName(workerId)
    );

    // Fire and forget - worker will call task_complete when done
    const promise = workerStub.handleMessage({
      id: crypto.randomUUID(),
      type: "process_task",
      from: "coordinator",
      to: workerId,
      taskId,
      timestamp: Date.now(),
    });

    // Ensure promise completes even if assignTask() returns first
    this.ctx.waitUntil(promise);
  }

  private async persist() {
    await this.ctx.storage.put("state", {
      workers: Array.from(this.workers),
      taskQueue: this.taskQueue,
      activeTasks: Array.from(this.activeTasks.entries()),
    });
  }
}
```

### WorkerActor

```typescript
export class WorkerActor extends DurableObject {
  private workerId: string;
  private currentTask: string | null = null;
  private processedCount: number = 0;

  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    this.workerId = ctx.id.toString();

    this.ctx.blockConcurrencyWhile(async () => {
      const state = await this.ctx.storage.get<{
        currentTask: string | null;
        processedCount: number;
      }>("state");

      if (state) {
        this.currentTask = state.currentTask;
        this.processedCount = state.processedCount;
      }
    });
  }

  async handleMessage(message: WorkerMessage): Promise<unknown> {
    switch (message.type) {
      case "process_task": {
        const taskId = message.taskId!;
        this.currentTask = taskId;
        await this.persist();

        // Do the work
        await this.processTask(taskId);

        // Update state
        this.currentTask = null;
        this.processedCount++;
        await this.persist();

        // Notify coordinator
        const coordinatorStub = this.env.COORDINATOR_ACTOR.get(
          this.env.COORDINATOR_ACTOR.idFromName("main")
        );

        await coordinatorStub.handleMessage({
          id: crypto.randomUUID(),
          type: "task_complete",
          from: this.workerId,
          payload: { taskId, workerId: this.workerId },
          timestamp: Date.now(),
        });

        return { status: "completed", taskId };
      }

      case "get_state": {
        return {
          workerId: this.workerId,
          currentTask: this.currentTask,
          processedCount: this.processedCount,
        };
      }

      default:
        return { error: "Unknown message type" };
    }
  }

  private async processTask(taskId: string): Promise<void> {
    // Simulate work (replace with actual processing logic)
    await new Promise((resolve) => setTimeout(resolve, 1000));
    console.log(`Worker ${this.workerId} processed task ${taskId}`);
  }

  private async persist() {
    await this.ctx.storage.put("state", {
      currentTask: this.currentTask,
      processedCount: this.processedCount,
    });
  }
}
```

## Key Design Decisions

### 1. Fire-and-Forget Task Assignment

```typescript
// Don't await - send message and continue
const promise = workerStub.handleMessage({...});
this.ctx.waitUntil(promise); // Ensure it completes
```

**Why?** Prevents coordinator from blocking on worker processing. Worker notifies coordinator when done via `task_complete`.

### 2. Auto-Assignment on Registration

```typescript
if (this.taskQueue.length > 0) {
  const taskId = this.taskQueue.shift()!;
  await this.assignTask(taskId, workerId);
}
```

**Why?** Newly registered workers immediately pull from queue, maximizing throughput.

### 3. Auto-Assignment on Completion

```typescript
if (this.taskQueue.length > 0) {
  const nextTask = this.taskQueue.shift()!;
  await this.assignTask(nextTask, workerId);
}
```

**Why?** Workers automatically pull next task, implementing work-stealing semantics.

### 4. Availability Detection

```typescript
private findAvailableWorker(): string | null {
  const busyWorkers = new Set(this.activeTasks.values());
  for (const workerId of this.workers) {
    if (!busyWorkers.has(workerId)) {
      return workerId;
    }
  }
  return null;
}
```

**Why?** O(workers) scan to find idle worker. Simple and correct for small worker pools (< 100 workers).

## State Management

### CoordinatorActor State

```typescript
{
  workers: string[];           // Registered worker IDs
  taskQueue: string[];         // Pending task IDs (FIFO)
  activeTasks: [string, string][]; // [taskId, workerId] pairs
}
```

**Persistence:** After every state change (register, assign, complete)

### WorkerActor State

```typescript
{
  currentTask: string | null;  // Currently processing task
  processedCount: number;      // Total tasks completed
}
```

**Persistence:** Before and after task processing

## Testing Strategy

The implementation has **128 passing tests** covering:

### 1. Message Routing
- Register worker → coordinator
- Assign task → coordinator
- Get status → coordinator
- Get state → worker
- Process task → worker (via coordinator)

### 2. Task Queue Behavior
- Immediate assignment (worker available)
- Queuing (no worker available)
- Auto-assignment on registration
- Auto-assignment on completion

### 3. State Persistence
- Coordinator state survives restarts
- Worker state survives restarts
- Active tasks persist across DO instances

### 4. Complete Workflows
```typescript
describe("CoordinatorActor Complete Flow", () => {
  it("should handle: register → assign → complete → status", async () => {
    // 1. Register worker
    // 2. Assign task (should be immediate)
    // 3. Wait for auto-completion (1s processing)
    // 4. Get status (verify task cleared)
  });
});
```

### 5. Error Conditions
- Invalid message types
- Unknown actor types
- Wire protocol errors

See `testing-actor-systems.md` for full testing methodology.

## Performance Characteristics

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| register_worker | O(1) + O(N) queue check | N = queue length |
| assign_task | O(W) + O(1) | W = worker count |
| task_complete | O(1) + O(N) queue check | N = queue length |
| get_status | O(W + T) | W = workers, T = active tasks |
| findAvailableWorker | O(W) | Linear scan of workers |

**Scalability limits:**
- Workers: ~100 (O(N) availability scan)
- Queue depth: ~10,000 (array operations)
- Active tasks: ~100 (Map operations)

For larger scales, consider:
- Worker availability bitmap (O(1) lookups)
- Priority queue (heap-based)
- Sharded coordinators (partition by task type)

## Pattern Variations

### 1. Priority Queue

```typescript
private taskQueue: Array<{ taskId: string; priority: number }> = [];

// Use heap or sorted insertion
this.taskQueue.push({ taskId, priority });
this.taskQueue.sort((a, b) => b.priority - a.priority);
```

### 2. Worker Specialization

```typescript
private workers: Map<string, { id: string; type: string }> = new Map();

private findAvailableWorker(taskType: string): string | null {
  // Filter by worker type
  for (const [id, worker] of this.workers) {
    if (worker.type === taskType && !this.isBusy(id)) {
      return id;
    }
  }
  return null;
}
```

### 3. Task Timeout

```typescript
private async assignTask(taskId: string, workerId: string) {
  this.activeTasks.set(taskId, {
    workerId,
    startedAt: Date.now(),
  });

  // Schedule timeout check
  setTimeout(() => this.checkTaskTimeout(taskId), 60000);
}

private async checkTaskTimeout(taskId: string) {
  const task = this.activeTasks.get(taskId);
  if (task && Date.now() - task.startedAt > 60000) {
    // Task timed out - requeue
    this.taskQueue.push(taskId);
    this.activeTasks.delete(taskId);
  }
}
```

## Integration with SessionActor

The Coordinator-Worker pattern works seamlessly with the Session Gateway Pattern:

```typescript
// SessionActor routes to coordinator
private async routeToCoordinator(wireMsg: WireMessage, actorId: string, ws: WebSocket) {
  const stub = this.env.COORDINATOR_ACTOR.get(
    this.env.COORDINATOR_ACTOR.idFromName(actorId)
  );

  const result = await stub.handleMessage({
    type: wireMsg.type,
    payload: wireMsg.payload,
    from: `session:${this.sessionId}`,
    id: wireMsg.id,
    timestamp: wireMsg.timestamp,
  });

  this.sendToClient({
    type: `${wireMsg.type}Response`,
    payload: result,
    replyTo: wireMsg.id,
  }, ws);
}
```

Client can:
1. Register workers via SessionActor
2. Assign tasks via SessionActor
3. Poll status via SessionActor
4. Query worker state via SessionActor

## Best Practices

### 1. Use `ctx.waitUntil` for Fire-and-Forget

```typescript
const promise = workerStub.handleMessage({...});
this.ctx.waitUntil(promise); // ✅ Ensures completion
// DON'T: await promise // ❌ Blocks coordinator
```

### 2. Persist Before External Calls

```typescript
this.activeTasks.set(taskId, workerId);
await this.persist(); // ✅ State saved first
await workerStub.handleMessage({...}); // Then call worker
```

### 3. Auto-Assignment Logic

```typescript
// On registration
if (this.taskQueue.length > 0) {
  await this.assignTask(this.taskQueue.shift()!, workerId);
}

// On completion
if (this.taskQueue.length > 0) {
  await this.assignTask(this.taskQueue.shift()!, workerId);
}
```

### 4. State Validation

```typescript
// Validate state on restore
if (state.activeTasks.length > state.workers.length) {
  console.warn("More active tasks than workers - possible corruption");
}
```

## When Not to Use

Avoid Coordinator-Worker pattern if:
- **No coordination needed** (use direct actor messaging)
- **Stateless workers** (use Cloudflare Queues instead)
- **Huge scale** (> 1000 workers, use sharded coordinators)
- **Complex scheduling** (use dedicated job scheduler)
- **Real-time processing** (use streaming actors instead)

## References

- **Implementation**: `/Users/bln/play/projects/proj-20260211-140744/src/actor-system-session.ts` (lines 47-170, 176-250)
- **Tests**: `/Users/bln/play/projects/proj-20260211-140744/src/session-actor.test.ts` (128 tests)
- **Related Patterns**: `session-gateway-pattern.md`, `testing-actor-systems.md`
