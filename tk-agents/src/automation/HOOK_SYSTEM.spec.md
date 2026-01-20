# Lifecycle Hook System Specification

**Created:** 2026-01-17
**Version:** 1.0
**Status:** Specification

---

## Overview

The Lifecycle Hook System enables automated task orchestration by subscribing to task lifecycle events and executing predefined actions. Hooks are synchronous, composable, and prioritized.

---

## Core Concepts

### Hook Definition

A **Lifecycle Hook** is a function that:
1. Subscribes to one or more event types
2. Receives event and context when triggered
3. Returns array of actions to execute
4. Runs synchronously in priority order

### Event Types

Hooks can subscribe to these event types:

| Event Type | When Emitted | Payload |
|------------|--------------|---------|
| `task_created` | Task created via CLI | `{ goal, deliverables, labels, priority }` |
| `task_started` | Task started | `{ context }` |
| `task_completed` | Task marked complete | `{ result, artifacts }` |
| `task_blocked` | Task blocked | `{ reason, requiredKnowledge }` |
| `task_deleted` | Task deleted | `{ edgesRemoved }` |
| `dependency_added` | Dependency edge created | `{ fromId, toId, edgeType }` |
| `dependency_removed` | Dependency edge removed | `{ edgeId, fromId, toId }` |
| `dependency_satisfied` | All dependencies completed | `{ dependentTaskId, completedTaskId }` |

### Hook Actions

Hooks return array of actions to execute:

| Action Type | Purpose | Payload |
|-------------|---------|---------|
| `create_task` | Create new task | `CreateTaskOptions` |
| `update_task` | Update existing task | `{ taskId, action, payload }` |
| `execute_workflow` | Run workflow | `{ workflowId, context }` |
| `log` | Emit event to EventLog | `Event` |

---

## TypeScript Interface

```typescript
/**
 * Lifecycle hook definition
 */
interface LifecycleHook {
  /** Unique hook name */
  name: string;

  /** Event types to subscribe to */
  eventTypes: string[];

  /**
   * Hook handler function
   * @param event The triggering event
   * @param context Execution context (graph, eventLog, etc.)
   * @returns Array of actions to execute
   */
  handler: (event: Event, context: HookContext) => Promise<HookAction[]>;

  /** Execution priority (lower = runs first) */
  priority?: number;

  /** Enable/disable flag */
  enabled?: boolean;

  /** Optional metadata */
  metadata?: {
    description?: string;
    author?: string;
    version?: string;
  };
}

/**
 * Hook execution context
 */
interface HookContext {
  graph: Graph;
  eventLog: EventLog;
  workflowEngine: WorkflowEngine;
}

/**
 * Hook action result
 */
interface HookAction {
  type: "create_task" | "update_task" | "execute_workflow" | "log";
  payload: unknown;
}
```

---

## Hook Execution Lifecycle

### 1. Event Emission

Events are emitted after successful CLI operations:

```typescript
// In src/cli/task.ts after cmdAdd
eventLog.append({
  timestamp: new Date().toISOString(),
  type: "task_created",
  nodeId: taskId,
  data: {
    goal: options.goal,
    deliverables: options.deliverables,
    labels: options.labels,
    priority: options.priority,
  },
  metadata: {
    triggeredBy: process.env.USER || "unknown",
  },
});
```

### 2. Hook Retrieval

Hook registry finds all hooks subscribed to event type:

```typescript
const hooks = hookRegistry.getHooksForEvent("task_created");
// Returns hooks in priority order (low to high)
```

### 3. Hook Execution

Hooks execute sequentially in priority order:

```typescript
const allActions: HookAction[] = [];

for (const hook of hooks) {
  try {
    const actions = await hook.handler(event, context);
    allActions.push(...actions);
  } catch (error) {
    console.error(`Hook ${hook.name} failed:`, error);
    // Continue executing other hooks
  }
}
```

### 4. Action Processing

Actions are executed after all hooks complete:

```typescript
for (const action of allActions) {
  switch (action.type) {
    case "create_task":
      await createTask(action.payload);
      break;
    case "update_task":
      await updateTask(action.payload.taskId, action.payload.action, action.payload.payload);
      break;
    case "execute_workflow":
      await workflowEngine.executeWorkflow(action.payload.workflowId, action.payload.context);
      break;
    case "log":
      eventLog.append(action.payload);
      break;
  }
}
```

---

## Built-in Hooks

### 1. Auto-Create Review Task

**Name:** `auto-create-review-task`
**Events:** `task_completed`
**Priority:** 10

**Purpose:** Automatically create review task when agent task completes.

**Logic:**
```typescript
const autoCreateReviewTask: LifecycleHook = {
  name: "auto-create-review-task",
  eventTypes: ["task_completed"],
  priority: 10,
  enabled: true,

  handler: async (event, context) => {
    const taskProps = context.graph.getNodeProperties(event.nodeId);

    // Only for agent tasks
    if (!taskProps.labels?.includes("agent")) {
      return [];
    }

    // Don't create review for review tasks
    if (taskProps.labels?.includes("review")) {
      return [];
    }

    // Create review task
    return [{
      type: "create_task",
      payload: {
        goal: `Review: ${taskProps.goal}`,
        deliverables: ["Review completed", "Feedback provided"],
        labels: ["review", ...(taskProps.labels || [])],
        priority: taskProps.priority || 2,
        parentTaskId: event.nodeId,
      },
    }];
  },
};
```

**Example:**
```
Input Event:
{
  type: "task_completed",
  nodeId: "task_42",
  data: { result: "Research complete" }
}

Task Properties (task_42):
{
  goal: "Research graph patterns",
  labels: ["agent", "research"],
  priority: 1
}

Output Actions:
[
  {
    type: "create_task",
    payload: {
      goal: "Review: Research graph patterns",
      deliverables: ["Review completed", "Feedback provided"],
      labels: ["review", "agent", "research"],
      priority: 1,
      parentTaskId: "task_42"
    }
  }
]

Result:
- New task created: task_43
- spawned_by edge: task_43 → task_42
```

### 2. Check Dependency Satisfaction

**Name:** `check-dependency-satisfaction`
**Events:** `task_completed`
**Priority:** 20

**Purpose:** Transition dependent tasks to `ready` when dependencies complete.

**Logic:**
```typescript
const checkDependencySatisfaction: LifecycleHook = {
  name: "check-dependency-satisfaction",
  eventTypes: ["task_completed"],
  priority: 20,

  handler: async (event, context) => {
    const actions: HookAction[] = [];

    // Find tasks that depend on this completed task
    const dependentTasks = context.graph.getEdgesTo(event.nodeId)
      .filter(e => e.type === "depends_on")
      .map(e => e.fromId);

    for (const taskId of dependentTasks) {
      const taskProps = context.graph.getNodeProperties(taskId);

      // Check if all dependencies are satisfied
      const dependencies = context.graph.getEdgesFrom(taskId)
        .filter(e => e.type === "depends_on");

      const allSatisfied = dependencies.every(dep => {
        const depProps = context.graph.getNodeProperties(dep.toId);
        return depProps.state === "completed";
      });

      // If all dependencies satisfied and task is created → transition to ready
      if (allSatisfied && taskProps.state === "created") {
        actions.push({
          type: "update_task",
          payload: {
            taskId,
            action: "transition_to_ready",
          },
        });

        // Emit dependency_satisfied event
        actions.push({
          type: "log",
          payload: {
            timestamp: new Date().toISOString(),
            type: "dependency_satisfied",
            nodeId: taskId,
            data: { completedTaskId: event.nodeId },
          },
        });
      }
    }

    return actions;
  },
};
```

**Example:**
```
Graph State:
- task_10: state=completed
- task_20: state=created, depends_on=[task_10, task_15]
- task_15: state=active

Event:
{ type: "task_completed", nodeId: "task_10" }

Result:
- No action (task_15 still active)

---

Later Event:
{ type: "task_completed", nodeId: "task_15" }

Result:
- task_20 state → ready
- Event logged: dependency_satisfied (nodeId: task_20)
```

### 3. Track Agent Metrics

**Name:** `track-agent-task-lifecycle`
**Events:** `task_created`, `task_started`, `task_completed`
**Priority:** 50

**Purpose:** Log agent task metrics (duration, deliverables, labels).

**Logic:**
```typescript
const trackAgentTaskLifecycle: LifecycleHook = {
  name: "track-agent-task-lifecycle",
  eventTypes: ["task_created", "task_started", "task_completed"],
  priority: 50,

  handler: async (event, context) => {
    const taskProps = context.graph.getNodeProperties(event.nodeId);

    // Only for agent tasks
    if (!taskProps.labels?.includes("agent")) {
      return [];
    }

    const actions: HookAction[] = [];

    if (event.type === "task_completed") {
      // Calculate duration
      const duration = taskProps.startedAt && taskProps.completedAt
        ? new Date(taskProps.completedAt).getTime() - new Date(taskProps.startedAt).getTime()
        : null;

      // Log agent metrics
      actions.push({
        type: "log",
        payload: {
          timestamp: new Date().toISOString(),
          type: "agent_metrics",
          nodeId: event.nodeId,
          data: {
            goal: taskProps.goal,
            labels: taskProps.labels,
            priority: taskProps.priority,
            durationMs: duration,
            deliverables: taskProps.desiredDeliverables,
          },
        },
      });
    }

    return actions;
  },
};
```

**Example:**
```
Event:
{ type: "task_completed", nodeId: "task_agent_5" }

Task Properties:
{
  goal: "Research graph patterns",
  labels: ["agent", "research"],
  priority: 1,
  startedAt: "2026-01-17T10:00:00Z",
  completedAt: "2026-01-17T12:30:00Z"
}

Output Actions:
[
  {
    type: "log",
    payload: {
      type: "agent_metrics",
      nodeId: "task_agent_5",
      data: {
        goal: "Research graph patterns",
        labels: ["agent", "research"],
        priority: 1,
        durationMs: 9000000,  // 2.5 hours
        deliverables: ["Research doc", "Pattern catalog"]
      }
    }
  }
]
```

---

## Hook Registry

### Registry Interface

```typescript
class HookRegistry {
  private hooks: Map<string, LifecycleHook> = new Map();

  /**
   * Register a lifecycle hook
   */
  register(hook: LifecycleHook): void {
    this.hooks.set(hook.name, hook);
  }

  /**
   * Unregister a hook by name
   */
  unregister(name: string): void {
    this.hooks.delete(name);
  }

  /**
   * Get all hooks subscribed to an event type
   * @returns Hooks in priority order (low to high)
   */
  getHooksForEvent(eventType: string): LifecycleHook[] {
    return Array.from(this.hooks.values())
      .filter(h => h.enabled !== false && h.eventTypes.includes(eventType))
      .sort((a, b) => (a.priority || 100) - (b.priority || 100));
  }

  /**
   * Execute all hooks for an event
   * @returns Aggregated actions from all hooks
   */
  async executeHooks(event: Event, context: HookContext): Promise<HookAction[]> {
    const hooks = this.getHooksForEvent(event.type);
    const allActions: HookAction[] = [];

    for (const hook of hooks) {
      try {
        const actions = await hook.handler(event, context);
        allActions.push(...actions);
      } catch (error) {
        console.error(`Hook ${hook.name} failed:`, error);
        // Continue executing other hooks
      }
    }

    return allActions;
  }

  /**
   * List all registered hooks
   */
  listHooks(): LifecycleHook[] {
    return Array.from(this.hooks.values());
  }

  /**
   * Get hook by name
   */
  getHook(name: string): LifecycleHook | undefined {
    return this.hooks.get(name);
  }
}
```

### Global Registry Instance

```typescript
// src/automation/hook-registry.ts
export const hookRegistry = new HookRegistry();

// Register built-in hooks
hookRegistry.register(autoCreateReviewTask);
hookRegistry.register(checkDependencySatisfaction);
hookRegistry.register(trackAgentTaskLifecycle);
```

---

## Hook Configuration

### JSON Configuration

**File:** `data/automation-hooks.json`

```json
{
  "version": "1.0",
  "hooks": [
    {
      "name": "auto-create-review-task",
      "enabled": true,
      "priority": 10,
      "eventTypes": ["task_completed"]
    },
    {
      "name": "check-dependency-satisfaction",
      "enabled": true,
      "priority": 20,
      "eventTypes": ["task_completed"]
    },
    {
      "name": "track-agent-task-lifecycle",
      "enabled": true,
      "priority": 50,
      "eventTypes": ["task_created", "task_started", "task_completed"]
    }
  ]
}
```

### Loading Configuration

```typescript
function loadHookConfig(filePath: string): void {
  const config = JSON.parse(readFileSync(filePath, "utf-8"));

  for (const hookConfig of config.hooks) {
    const hook = hookRegistry.getHook(hookConfig.name);

    if (hook) {
      // Update hook properties from config
      hook.enabled = hookConfig.enabled;
      hook.priority = hookConfig.priority;
    }
  }
}
```

---

## Error Handling

### Hook Failure Isolation

Hooks are isolated - failure in one hook doesn't affect others:

```typescript
for (const hook of hooks) {
  try {
    const actions = await hook.handler(event, context);
    allActions.push(...actions);
  } catch (error) {
    // Log error but continue
    console.error(`Hook ${hook.name} failed:`, error);

    // Optionally emit error event
    eventLog.append({
      timestamp: new Date().toISOString(),
      type: "hook_error",
      nodeId: event.nodeId,
      data: {
        hookName: hook.name,
        error: error.message,
        originalEvent: event,
      },
    });
  }
}
```

### Action Execution Errors

Actions are executed with error handling:

```typescript
for (const action of allActions) {
  try {
    await executeAction(action, context);
  } catch (error) {
    console.error(`Action ${action.type} failed:`, error);

    // Optionally emit error event
    eventLog.append({
      timestamp: new Date().toISOString(),
      type: "action_error",
      nodeId: "system",
      data: {
        actionType: action.type,
        error: error.message,
        payload: action.payload,
      },
    });
  }
}
```

---

## Testing Hooks

### Unit Test Example

```typescript
import { test, expect } from "bun:test";
import { hookRegistry } from "./hook-registry.ts";

test("auto-create-review-task creates review for agent completion", async () => {
  const event = {
    timestamp: "2026-01-17T12:00:00Z",
    type: "task_completed",
    nodeId: "task_42",
    data: { result: "Research complete" },
  };

  const context = {
    graph: mockGraph({
      "task_42": {
        goal: "Research patterns",
        labels: ["agent", "research"],
        priority: 1,
      },
    }),
    eventLog: mockEventLog(),
    workflowEngine: mockWorkflowEngine(),
  };

  const actions = await hookRegistry.executeHooks(event, context);

  expect(actions).toHaveLength(1);
  expect(actions[0].type).toBe("create_task");
  expect(actions[0].payload.goal).toContain("Review:");
  expect(actions[0].payload.labels).toContain("review");
});

test("hook failure doesn't break execution", async () => {
  // Register failing hook
  hookRegistry.register({
    name: "failing-hook",
    eventTypes: ["test_event"],
    priority: 10,
    handler: async () => {
      throw new Error("Test error");
    },
  });

  // Register successful hook
  hookRegistry.register({
    name: "success-hook",
    eventTypes: ["test_event"],
    priority: 20,
    handler: async () => [{ type: "log", payload: {} }],
  });

  const event = { type: "test_event", nodeId: "x", data: {} };
  const actions = await hookRegistry.executeHooks(event, context);

  // Successful hook still executed
  expect(actions).toHaveLength(1);
  expect(actions[0].type).toBe("log");
});
```

---

## Performance Considerations

### Hook Execution Time

Hooks execute synchronously in CLI operations - keep handlers fast:

- **Target:** <10ms per hook
- **Maximum:** <50ms total hook execution time

### Optimization Strategies

1. **Defer Heavy Work:**
   ```typescript
   // BAD: Heavy computation in hook
   handler: async (event, context) => {
     const result = await expensiveCalculation();
     return [{ type: "create_task", payload: result }];
   }

   // GOOD: Queue workflow for heavy work
   handler: async (event, context) => {
     return [{
       type: "execute_workflow",
       payload: { workflowId: "heavy-computation", context: event }
     }];
   }
   ```

2. **Batch Actions:**
   ```typescript
   // Batch multiple similar actions
   const actions: HookAction[] = dependentTasks.map(taskId => ({
     type: "update_task",
     payload: { taskId, action: "transition_to_ready" }
   }));
   ```

3. **Filter Early:**
   ```typescript
   // Exit early if hook doesn't apply
   if (!taskProps.labels?.includes("agent")) {
     return [];  // No work needed
   }
   ```

---

## Invariants

### Hook Execution Invariants

```
forall event: Event
  => hooks execute in priority order (low to high)
  => hook failure doesn't prevent other hooks from executing
  => actions aggregate across all hooks
```

### Hook Registration Invariants

```
forall hook: LifecycleHook
  => hook.name is unique in registry
  => hook.eventTypes is non-empty array
  => hook.handler is async function
```

### Action Execution Invariants

```
forall action: HookAction
  => action.type in {create_task, update_task, execute_workflow, log}
  => action execution errors don't propagate to CLI operation
```

---

## Document History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-17 | 1.0 | Initial specification |

---

**END OF SPECIFICATION**
