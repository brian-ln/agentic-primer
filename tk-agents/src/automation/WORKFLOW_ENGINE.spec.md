# Workflow Engine Specification

**Created:** 2026-01-17
**Version:** 1.0
**Status:** Specification

---

## Overview

The Workflow Engine enables declarative multi-step automation using JSON-defined workflows. Workflows execute as sequences of steps with conditional branching, parallel execution, and error handling.

---

## Core Concepts

### Workflow Definition

A **Workflow** is a JSON document that defines:
- Unique identifier and metadata
- Sequence of steps to execute
- Step dependencies and branching logic
- Error handling strategies

### Step Types

| Step Type | Purpose | Example Use Case |
|-----------|---------|------------------|
| `create_task` | Create new task | Auto-create review task |
| `update_task` | Update existing task | Transition task to ready |
| `execute_command` | Run shell command | Generate report, query CLI |
| `condition` | Conditional branching | Check if task has label |
| `parallel` | Execute steps in parallel | Create multiple tasks simultaneously |
| `noop` | No operation (end marker) | Workflow termination |

### Workflow Context

Workflows execute with context containing:
- **Triggering event data**
- **Graph instance**
- **EventLog instance**
- **Intermediate step results**

---

## TypeScript Interface

```typescript
/**
 * Workflow definition
 */
interface Workflow {
  /** Unique workflow ID */
  id: string;

  /** Human-readable name */
  name: string;

  /** Workflow description */
  description: string;

  /** Sequence of workflow steps */
  steps: WorkflowStep[];

  /** Optional metadata */
  metadata?: {
    author?: string;
    version?: string;
    tags?: string[];
  };
}

/**
 * Workflow step definition
 */
interface WorkflowStep {
  /** Unique step ID within workflow */
  id: string;

  /** Step type */
  type: "create_task" | "update_task" | "execute_command" | "condition" | "parallel" | "noop";

  /** Step-specific configuration */
  config: StepConfig;

  /** Next step on success */
  onSuccess?: string;

  /** Next step on failure (error handler) */
  onFailure?: string;
}

/**
 * Workflow execution context
 */
interface WorkflowContext {
  /** Triggering event */
  event: Event;

  /** Graph instance */
  graph: Graph;

  /** EventLog instance */
  eventLog: EventLog;

  /** Workflow-specific data */
  data: Record<string, unknown>;

  /** Results from previous steps */
  stepResults: Map<string, StepResult>;
}

/**
 * Step execution result
 */
interface StepResult {
  /** Step ID */
  stepId: string;

  /** Success flag */
  success: boolean;

  /** Result data */
  data?: unknown;

  /** Error if failed */
  error?: Error;

  /** Execution time (ms) */
  durationMs: number;
}

/**
 * Workflow execution result
 */
interface WorkflowResult {
  /** Workflow ID */
  workflowId: string;

  /** Overall success */
  success: boolean;

  /** Step results */
  steps: StepResult[];

  /** Error if failed */
  error?: Error;

  /** Total execution time (ms) */
  durationMs: number;
}
```

---

## Step Type Configurations

### 1. Create Task Step

```typescript
interface CreateTaskStepConfig {
  /** Task goal (supports variable interpolation) */
  goal: string;

  /** Deliverables (supports variable interpolation) */
  deliverables: string[];

  /** Labels */
  labels: string[];

  /** Priority (0-4) */
  priority: number;

  /** Parent task ID (literal or from context) */
  parentTaskId?: string | { fromContext: string };

  /** Dependencies (task IDs) */
  depends?: string[];
}
```

**Example:**
```json
{
  "id": "create-review",
  "type": "create_task",
  "config": {
    "goal": "Review: ${context.task.goal}",
    "deliverables": ["Review completed", "Feedback provided"],
    "labels": ["review", "${context.task.labels}"],
    "priority": "${context.task.priority || 2}",
    "parentTaskId": { "fromContext": "task.id" }
  },
  "onSuccess": "log-creation"
}
```

### 2. Update Task Step

```typescript
interface UpdateTaskStepConfig {
  /** Task ID to update (literal or from context) */
  taskId: string | { fromContext: string };

  /** Action to perform */
  action: "start" | "complete" | "block" | "transition_to_ready";

  /** Action-specific payload */
  payload?: unknown;
}
```

**Example:**
```json
{
  "id": "transition-ready",
  "type": "update_task",
  "config": {
    "taskId": { "fromContext": "dependentTask.id" },
    "action": "transition_to_ready"
  },
  "onSuccess": "emit-event"
}
```

### 3. Execute Command Step

```typescript
interface ExecuteCommandStepConfig {
  /** Command to execute */
  command: string;

  /** Command arguments (supports variable interpolation) */
  args: string[];

  /** Capture stdout/stderr */
  captureOutput?: boolean;

  /** Store output in context key */
  storeAs?: string;
}
```

**Example:**
```json
{
  "id": "query-stale",
  "type": "execute_command",
  "config": {
    "command": "bun",
    "args": ["src/cli/task.ts", "list", "--status", "active", "--json"],
    "captureOutput": true,
    "storeAs": "activeTasks"
  },
  "onSuccess": "filter-stale"
}
```

### 4. Condition Step

```typescript
interface ConditionStepConfig {
  /** JavaScript expression to evaluate */
  predicate: string;

  /** Step ID if true */
  onTrue: string;

  /** Step ID if false */
  onFalse: string;
}
```

**Example:**
```json
{
  "id": "check-labels",
  "type": "condition",
  "config": {
    "predicate": "context.task.labels.includes('agent') && !context.task.labels.includes('review')",
    "onTrue": "create-review",
    "onFalse": "end"
  }
}
```

### 5. Parallel Step

```typescript
interface ParallelStepConfig {
  /** Steps to execute in parallel */
  steps: WorkflowStep[];

  /** Wait for all to complete before continuing */
  waitForAll: boolean;
}
```

**Example:**
```json
{
  "id": "create-subtasks",
  "type": "parallel",
  "config": {
    "steps": [
      {
        "id": "create-task-1",
        "type": "create_task",
        "config": { "goal": "Subtask 1", ... }
      },
      {
        "id": "create-task-2",
        "type": "create_task",
        "config": { "goal": "Subtask 2", ... }
      }
    ],
    "waitForAll": true
  },
  "onSuccess": "link-tasks"
}
```

---

## Variable Interpolation

Workflows support variable interpolation using `${...}` syntax:

### Context Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `context.event.nodeId` | Triggering event's node ID | `task_42` |
| `context.event.type` | Triggering event type | `task_completed` |
| `context.event.data.*` | Event payload data | `context.event.data.result` |
| `context.task.*` | Task properties | `context.task.goal` |
| `context.data.*` | Workflow-specific data | `context.data.userName` |
| `context.stepResults.get('step-id')` | Previous step result | `context.stepResults.get('query-tasks').data` |

### Interpolation Examples

```json
{
  "goal": "Review: ${context.task.goal}",
  "labels": ["review", "${context.task.labels}"],
  "priority": "${context.task.priority || 2}",
  "parentTaskId": "${context.event.nodeId}"
}
```

Evaluates to:
```json
{
  "goal": "Review: Research graph patterns",
  "labels": ["review", "agent", "research"],
  "priority": 1,
  "parentTaskId": "task_42"
}
```

---

## Workflow Execution

### Execution Flow

```
1. Load workflow definition from JSON
2. Initialize workflow context
3. Start at first step
4. Execute step
5. Check success/failure
6. Follow onSuccess or onFailure path
7. Repeat until terminal step (noop or no next step)
8. Return workflow result
```

### Step Execution

```typescript
async function executeStep(
  step: WorkflowStep,
  context: WorkflowContext
): Promise<StepResult> {
  const startTime = Date.now();

  try {
    let result: unknown;

    switch (step.type) {
      case "create_task":
        result = await executeCreateTask(step.config, context);
        break;
      case "update_task":
        result = await executeUpdateTask(step.config, context);
        break;
      case "execute_command":
        result = await executeCommand(step.config, context);
        break;
      case "condition":
        result = await executeCondition(step.config, context);
        break;
      case "parallel":
        result = await executeParallel(step.config, context);
        break;
      case "noop":
        result = { noop: true };
        break;
    }

    return {
      stepId: step.id,
      success: true,
      data: result,
      durationMs: Date.now() - startTime,
    };
  } catch (error) {
    return {
      stepId: step.id,
      success: false,
      error: error as Error,
      durationMs: Date.now() - startTime,
    };
  }
}
```

### Next Step Resolution

```typescript
function getNextStep(
  currentStep: WorkflowStep,
  stepResult: StepResult,
  workflow: Workflow
): WorkflowStep | null {
  // Get next step ID based on success/failure
  const nextStepId = stepResult.success
    ? currentStep.onSuccess
    : currentStep.onFailure;

  if (!nextStepId) {
    return null;  // Terminal step
  }

  // Find step by ID
  return workflow.steps.find(s => s.id === nextStepId) || null;
}
```

---

## Built-in Workflows

### 1. Create Review Task

**File:** `data/workflows/create-review-task.json`

```json
{
  "id": "create-review-task",
  "name": "Create Review Task",
  "description": "Creates a review task when agent task completes",
  "steps": [
    {
      "id": "check-labels",
      "type": "condition",
      "config": {
        "predicate": "context.task.labels.includes('agent') && !context.task.labels.includes('review')",
        "onTrue": "create-review",
        "onFalse": "end"
      }
    },
    {
      "id": "create-review",
      "type": "create_task",
      "config": {
        "goal": "Review: ${context.task.goal}",
        "deliverables": ["Review completed", "Feedback provided"],
        "labels": ["review"],
        "priority": "${context.task.priority || 2}",
        "parentTaskId": "${context.event.nodeId}"
      },
      "onSuccess": "log-creation",
      "onFailure": "log-error"
    },
    {
      "id": "log-creation",
      "type": "execute_command",
      "config": {
        "command": "echo",
        "args": ["Created review task for ${context.event.nodeId}"]
      },
      "onSuccess": "end"
    },
    {
      "id": "log-error",
      "type": "execute_command",
      "config": {
        "command": "echo",
        "args": ["Failed to create review task: ${context.stepResults.get('create-review').error}"]
      },
      "onSuccess": "end"
    },
    {
      "id": "end",
      "type": "noop"
    }
  ],
  "metadata": {
    "author": "system",
    "version": "1.0"
  }
}
```

### 2. Stale Task Report

**File:** `data/workflows/stale-task-report.json`

```json
{
  "id": "stale-task-report",
  "name": "Stale Task Report",
  "description": "Generates report of tasks active >7 days",
  "steps": [
    {
      "id": "query-active",
      "type": "execute_command",
      "config": {
        "command": "bun",
        "args": ["src/cli/task.ts", "list", "--status", "active", "--json"],
        "captureOutput": true,
        "storeAs": "activeTasks"
      },
      "onSuccess": "filter-stale",
      "onFailure": "end"
    },
    {
      "id": "filter-stale",
      "type": "condition",
      "config": {
        "predicate": "context.data.activeTasks.data.filter(t => (Date.now() - new Date(t.startedAt).getTime()) > 7*24*60*60*1000).length > 0",
        "onTrue": "create-report",
        "onFalse": "end"
      }
    },
    {
      "id": "create-report",
      "type": "create_task",
      "config": {
        "goal": "Review ${context.data.staleTasks.length} stale tasks",
        "deliverables": ["Stale task report", "Action plan"],
        "labels": ["report", "stale-tasks"],
        "priority": 1
      },
      "onSuccess": "end"
    },
    {
      "id": "end",
      "type": "noop"
    }
  ]
}
```

### 3. Notify Critical Completion

**File:** `data/workflows/notify-critical-completion.json`

```json
{
  "id": "notify-critical-completion",
  "name": "Notify Critical Completion",
  "description": "Send notification when P0 task completes",
  "steps": [
    {
      "id": "check-priority",
      "type": "condition",
      "config": {
        "predicate": "context.task.priority === 0",
        "onTrue": "send-notification",
        "onFalse": "end"
      }
    },
    {
      "id": "send-notification",
      "type": "execute_command",
      "config": {
        "command": "echo",
        "args": ["CRITICAL: Task ${context.event.nodeId} completed - ${context.task.goal}"]
      },
      "onSuccess": "log-event",
      "onFailure": "end"
    },
    {
      "id": "log-event",
      "type": "execute_command",
      "config": {
        "command": "echo",
        "args": ["Logged critical completion notification"]
      },
      "onSuccess": "end"
    },
    {
      "id": "end",
      "type": "noop"
    }
  ]
}
```

---

## Workflow Engine Implementation

### WorkflowEngine Class

```typescript
class WorkflowEngine {
  private workflows: Map<string, Workflow> = new Map();

  /**
   * Load workflow from JSON definition
   */
  loadWorkflow(workflow: Workflow): void {
    this.workflows.set(workflow.id, workflow);
  }

  /**
   * Load workflow from file
   */
  loadWorkflowFromFile(filePath: string): void {
    const content = readFileSync(filePath, "utf-8");
    const workflow = JSON.parse(content) as Workflow;
    this.loadWorkflow(workflow);
  }

  /**
   * Execute a workflow
   */
  async executeWorkflow(
    workflowId: string,
    context: WorkflowContext
  ): Promise<WorkflowResult> {
    const workflow = this.workflows.get(workflowId);
    if (!workflow) {
      throw new Error(`Workflow not found: ${workflowId}`);
    }

    const startTime = Date.now();
    const result: WorkflowResult = {
      workflowId,
      success: true,
      steps: [],
      durationMs: 0,
    };

    // Start at first step
    let currentStep = workflow.steps[0];

    while (currentStep) {
      // Execute step
      const stepResult = await this.executeStep(currentStep, context);
      result.steps.push(stepResult);

      // Store result in context
      context.stepResults.set(currentStep.id, stepResult);

      // Get next step
      currentStep = this.getNextStep(currentStep, stepResult, workflow);

      // Break if step failed and no error handler
      if (!stepResult.success && !currentStep) {
        result.success = false;
        break;
      }
    }

    result.durationMs = Date.now() - startTime;
    return result;
  }

  private async executeStep(
    step: WorkflowStep,
    context: WorkflowContext
  ): Promise<StepResult> {
    // Implementation in previous section
  }

  private getNextStep(
    currentStep: WorkflowStep,
    stepResult: StepResult,
    workflow: Workflow
  ): WorkflowStep | null {
    // Implementation in previous section
  }

  /**
   * List all loaded workflows
   */
  listWorkflows(): Workflow[] {
    return Array.from(this.workflows.values());
  }

  /**
   * Get workflow by ID
   */
  getWorkflow(workflowId: string): Workflow | undefined {
    return this.workflows.get(workflowId);
  }
}
```

### Global Instance

```typescript
// src/automation/workflow-engine.ts
export const workflowEngine = new WorkflowEngine();

// Load built-in workflows
workflowEngine.loadWorkflowFromFile("./data/workflows/create-review-task.json");
workflowEngine.loadWorkflowFromFile("./data/workflows/stale-task-report.json");
workflowEngine.loadWorkflowFromFile("./data/workflows/notify-critical-completion.json");
```

---

## Integration with Hooks

Hooks can trigger workflows via `execute_workflow` action:

```typescript
const autoCreateReviewTask: LifecycleHook = {
  name: "auto-create-review-task",
  eventTypes: ["task_completed"],

  handler: async (event, context) => {
    return [{
      type: "execute_workflow",
      payload: {
        workflowId: "create-review-task",
        context: {
          event,
          task: context.graph.getNodeProperties(event.nodeId),
        },
      },
    }];
  },
};
```

---

## Error Handling

### Step-Level Error Handling

Steps can specify error handler via `onFailure`:

```json
{
  "id": "risky-operation",
  "type": "execute_command",
  "config": { "command": "risky-cmd" },
  "onSuccess": "next-step",
  "onFailure": "error-handler"
}
```

### Workflow-Level Error Handling

Workflows fail gracefully:
- Step errors captured in `StepResult`
- Workflow continues if error handler specified
- Workflow fails if no error handler
- All executed steps included in result

### Example Error Handling

```json
{
  "steps": [
    {
      "id": "try-operation",
      "type": "create_task",
      "config": { ... },
      "onSuccess": "success-path",
      "onFailure": "handle-error"
    },
    {
      "id": "handle-error",
      "type": "execute_command",
      "config": {
        "command": "echo",
        "args": ["Operation failed: ${context.stepResults.get('try-operation').error}"]
      },
      "onSuccess": "end"
    },
    {
      "id": "success-path",
      "type": "execute_command",
      "config": {
        "command": "echo",
        "args": ["Operation succeeded"]
      },
      "onSuccess": "end"
    },
    {
      "id": "end",
      "type": "noop"
    }
  ]
}
```

---

## Testing Workflows

### Unit Tests

```typescript
import { test, expect } from "bun:test";
import { workflowEngine } from "./workflow-engine.ts";

test("create-review-task workflow creates review", async () => {
  const context: WorkflowContext = {
    event: {
      type: "task_completed",
      nodeId: "task_42",
      data: { result: "Research complete" },
    },
    task: {
      goal: "Research patterns",
      labels: ["agent", "research"],
      priority: 1,
    },
    graph: mockGraph(),
    eventLog: mockEventLog(),
    data: {},
    stepResults: new Map(),
  };

  const result = await workflowEngine.executeWorkflow("create-review-task", context);

  expect(result.success).toBe(true);
  expect(result.steps).toHaveLength(3);  // check-labels, create-review, log-creation
  expect(result.steps[1].stepId).toBe("create-review");
  expect(result.steps[1].success).toBe(true);
});

test("workflow handles step failure", async () => {
  const context = {
    event: { type: "test", nodeId: "x", data: {} },
    task: { goal: "Test", labels: [] },
    graph: mockGraphWithError(),  // Will fail on create_task
    eventLog: mockEventLog(),
    data: {},
    stepResults: new Map(),
  };

  const result = await workflowEngine.executeWorkflow("create-review-task", context);

  expect(result.success).toBe(false);
  expect(result.steps.some(s => !s.success)).toBe(true);
});
```

### Integration Tests

```typescript
test("workflow triggered by hook creates task", async () => {
  // Create agent task
  const taskId = await createTask({
    goal: "Test agent work",
    labels: ["agent"],
  });

  // Complete task (triggers hook → workflow)
  await updateTask(taskId, "complete");

  // Verify review task created by workflow
  const reviewTasks = await listTasks({ label: "review" });
  expect(reviewTasks.some(t => t.parentTaskId === taskId)).toBe(true);
});
```

---

## Performance Considerations

### Workflow Execution Time

Target execution times:
- **Simple workflows** (<5 steps): <100ms
- **Complex workflows** (5-10 steps): <500ms
- **Heavy workflows** (>10 steps): <2000ms

### Optimization Strategies

1. **Parallel Execution:**
   ```json
   {
     "id": "parallel-tasks",
     "type": "parallel",
     "config": {
       "steps": [...],  // Execute simultaneously
       "waitForAll": true
     }
   }
   ```

2. **Early Termination:**
   ```json
   {
     "id": "check-needed",
     "type": "condition",
     "config": {
       "predicate": "!context.task.labels.includes('agent')",
       "onTrue": "end",  // Skip remaining steps
       "onFalse": "continue"
     }
   }
   ```

3. **Minimize Command Execution:**
   - Prefer in-process operations over shell commands
   - Batch multiple operations

---

## Invariants

### Workflow Execution Invariants

```
forall workflow: Workflow
  => execution starts at steps[0]
  => steps execute in defined order
  => onSuccess/onFailure determine next step
  => execution terminates at noop or no next step
```

### Step Execution Invariants

```
forall step: WorkflowStep
  => step.id is unique within workflow
  => step.type determines execution behavior
  => step failures don't propagate unless no error handler
```

### Context Invariants

```
forall context: WorkflowContext
  => context.event is immutable
  => context.stepResults accumulates during execution
  => context.data is mutable workflow state
```

---

## Document History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-17 | 1.0 | Initial specification |

---

**END OF SPECIFICATION**
