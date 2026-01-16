# Self-Management Guide

## Overview

The Bootstrap system enables **self-managing development**: the system uses itself to manage its own development. Background agents become tasks. Progress is visible. Coordination is automatic. No manual polling needed.

## Core Concept

**Traditional Approach:**
```
Agent runs → Parent polls → Manual tracking → No visibility
```

**Bootstrap Approach:**
```
Agent + Task = Agent-Task → Automatic tracking → Full visibility
```

## Quick Start

### 1. Create a Bootstrap Instance

```typescript
import { createBootstrap } from "./src/bootstrap";
import { Graph } from "./src/graph";

const graph = new Graph();
const bootstrap = createBootstrap(graph);
```

### 2. Create Agent-Tasks

Instead of just registering actors, create **agent-tasks** that combine execution (agent) with state (task):

```typescript
import { MyActor } from "./my-actor";

const actor = new MyActor("agent-123");

// Old way: just register
registry.register(actor);

// Bootstrap way: create agent-task
const agentTask = bootstrap.create({
  agent: actor,
  goal: "Build feature X",
  deliverables: ["feature.ts", "feature.test.ts"],
  criteria: [
    { criterion: "implementation complete", measure: "file exists", threshold: true },
    { criterion: "tests pass", measure: "bun test", threshold: true }
  ]
});

console.log(`Agent ${agentTask.agentId} working on task ${agentTask.taskId}`);
```

### 3. Track Progress Automatically

The task system automatically tracks agent progress:

```typescript
// Get current status
const status = bootstrap.status(agentTask.taskId);

console.log(`State: ${status.state}`);           // active, completed, blocked, etc.
console.log(`Progress: ${status.progress * 100}%`); // 0-100%
console.log(`Agent: ${status.agentId}`);         // Which agent is working on it
console.log(`Blockers: ${status.blockers}`);     // What's blocking it
```

### 4. Create Task Hierarchies

Agent-tasks can have parent-child relationships:

```typescript
// Parent task
const parentTask = bootstrap.create({
  agent: parentActor,
  goal: "Implement authentication system",
  deliverables: ["auth module", "tests", "docs"],
  criteria: [{ criterion: "complete", measure: "all parts done", threshold: true }]
});

// Child task 1
const childTask1 = bootstrap.create({
  agent: authActor,
  goal: "Implement JWT authentication",
  deliverables: ["jwt.ts", "jwt.test.ts"],
  criteria: [{ criterion: "tests pass", measure: "bun test", threshold: true }],
  parentTaskId: parentTask.taskId  // Links to parent
});

// Child task 2
const childTask2 = bootstrap.create({
  agent: docsActor,
  goal: "Document authentication API",
  deliverables: ["AUTH.md"],
  criteria: [{ criterion: "docs complete", measure: "file exists", threshold: true }],
  parentTaskId: parentTask.taskId
});
```

### 5. View Project Status

Get hierarchical view of all agent-tasks:

```typescript
const projectStatus = bootstrap.projectStatus(parentTask.taskId);

// projectStatus contains:
// {
//   taskId: "task_1",
//   state: "active",
//   progress: 0.5,
//   agentId: "parent-agent",
//   children: [
//     { taskId: "task_2", state: "completed", progress: 1.0, agentId: "auth-agent" },
//     { taskId: "task_3", state: "active", progress: 0.7, agentId: "docs-agent" }
//   ]
// }
```

## Dynamic Task Injection

The killer feature: inject tasks **at runtime** based on changing requirements.

### Basic Injection

```typescript
// While agent is working, inject a new requirement
const injectedTask = bootstrap.inject({
  parentTaskId: agentTask.taskId,
  goal: "Add comprehensive tests",
  deliverables: ["tests.ts"],
  criteria: [{ criterion: "coverage > 80%", measure: "test coverage", threshold: 80 }],
  makeDependency: true  // Parent now depends on this task
});

// The parent task now sees this as a blocker
const status = bootstrap.status(agentTask.taskId);
// status.blockers = ["Waiting on: Add comprehensive tests"]
```

### Injection Rules

Pre-register rules that trigger automatically:

```typescript
// Register a rule
bootstrap.injector.registerRule({
  name: "tests-before-completion",
  condition: "before_complete",
  factory: () => ({
    goal: "Write comprehensive tests",
    deliverables: ["tests.ts"],
    criteria: [{ criterion: "tests pass", measure: "bun test", threshold: true }]
  })
});

// Apply manually
const testTask = bootstrap.injector.applyRule(
  "tests-before-completion",
  agentTask.taskId,
  graph
);

// Or check triggers automatically (future enhancement)
const triggered = bootstrap.injector.checkTriggers(
  agentTask.taskId,
  "before_complete",
  graph
);
```

## Real-World Example

Here's how to use Bootstrap for actual development:

```typescript
import { createBootstrap } from "./src/bootstrap";
import { Graph } from "./src/graph";
import { DocumentationAgent, TestingAgent, CodeGenAgent } from "./agents";

async function manageDevelopment() {
  const graph = new Graph();
  const bootstrap = createBootstrap(graph);

  // Create root project task
  const projectTask = bootstrap.create({
    agent: new ProjectManagerAgent("pm"),
    goal: "Implement new feature",
    deliverables: ["code", "tests", "docs"],
    criteria: [{ criterion: "all done", measure: "complete", threshold: true }]
  });

  // Code generation agent
  const codeAgent = bootstrap.create({
    agent: new CodeGenAgent("codegen"),
    goal: "Generate feature code",
    deliverables: ["feature.ts"],
    criteria: [{ criterion: "code complete", measure: "file exists", threshold: true }],
    parentTaskId: projectTask.taskId
  });

  // Start work
  await bootstrap.registry.sendTo(codeAgent.agentId, "start", {});

  // User adds requirement mid-execution
  console.log("User: 'Make sure you add tests!'");

  bootstrap.inject({
    parentTaskId: codeAgent.taskId,
    goal: "Add tests for feature",
    deliverables: ["feature.test.ts"],
    criteria: [{ criterion: "tests pass", measure: "bun test", threshold: true }],
    makeDependency: true
  });

  // Code agent now blocked until tests are added
  const status = bootstrap.status(codeAgent.taskId);
  console.log(`Code agent state: ${status.state}`);
  console.log(`Blockers: ${status.blockers}`);

  // Create test agent to handle the new requirement
  const testAgent = bootstrap.create({
    agent: new TestingAgent("tester"),
    goal: "Add tests for feature",
    deliverables: ["feature.test.ts"],
    criteria: [{ criterion: "tests pass", measure: "bun test", threshold: true }],
    parentTaskId: codeAgent.taskId
  });

  await bootstrap.registry.sendTo(testAgent.agentId, "start", {});

  // Wait for completion
  while (true) {
    const projStatus = bootstrap.projectStatus(projectTask.taskId);
    console.log(`Project progress: ${projStatus.progress * 100}%`);

    if (projStatus.state === "completed") break;
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  console.log("✅ Project complete!");
}
```

## API Reference

### Bootstrap

#### `createBootstrap(graph: Graph): Bootstrap`

Creates a bootstrap instance.

#### `bootstrap.create(options): AgentTask`

Create an agent-task.

**Options:**
- `agent: Actor` - The actor to register
- `goal: string` - Task goal
- `deliverables: string[]` - Expected deliverables
- `criteria: ObjectiveCriterion[]` - Success criteria
- `parentTaskId?: string` - Parent task ID (optional)

**Returns:** `AgentTask` with `taskId` and `agentId`

#### `bootstrap.inject(injection): TaskNode`

Dynamically inject a new task.

**Injection:**
- `parentTaskId: string` - Parent task ID
- `goal: string` - Task goal
- `deliverables: string[]` - Expected deliverables
- `criteria: ObjectiveCriterion[]` - Success criteria
- `triggerCondition?: "immediate" | "before_complete" | "on_block"` - When to trigger
- `makeDependency?: boolean` - Should parent depend on this? (default: true)

**Returns:** `TaskNode` for the injected task

#### `bootstrap.status(taskId): TaskProgress`

Get task status with agent information.

**Returns:** `TaskProgress` with:
- `taskId: string`
- `state: TaskState` - Current state
- `progress: number` - Progress 0.0-1.0
- `agentId?: string` - Agent working on this task
- `blockers: string[]` - What's blocking this task
- `children: TaskProgress[]` - Child task statuses

#### `bootstrap.projectStatus(rootTaskId): TaskProgress`

Get hierarchical status starting from root task.

#### `bootstrap.list(): AgentTask[]`

List all agent-tasks.

#### `bootstrap.complete(agentId, result): void`

Mark agent-task as completed.

#### `bootstrap.block(agentId, reason, requiredKnowledge?): void`

Mark agent-task as blocked.

### BootstrapRegistry

Extends `Registry` with agent-task management.

#### `registry.getAgentTask(agentId): AgentTask | undefined`

Get agent-task mapping.

#### `registry.getTaskId(agentId): string | undefined`

Get task ID for an agent.

#### `registry.getAgentId(taskId): string | undefined`

Get agent ID for a task.

### TaskInjector

Manages dynamic task injection.

#### `injector.registerRule(rule): void`

Register an injection rule.

**Rule:**
- `name: string` - Rule identifier
- `condition: string` - When to trigger
- `factory: () => TaskInjection` - Creates injection spec

#### `injector.applyRule(ruleName, parentTaskId, graph): TaskNode`

Apply a pre-registered rule.

#### `injector.listRules(): InjectionRule[]`

List all registered rules.

#### `injector.removeRule(ruleName): boolean`

Remove a rule.

## Patterns and Best Practices

### Pattern 1: Root Task for Projects

Always create a root task for the overall project:

```typescript
const projectRoot = bootstrap.create({
  agent: coordinatorActor,
  goal: "Overall project goal",
  deliverables: ["all project outputs"],
  criteria: [{ criterion: "project complete", measure: "all done", threshold: true }]
});

// All other tasks are children of root
const feature1 = bootstrap.create({
  agent: actor1,
  goal: "Feature 1",
  deliverables: ["f1.ts"],
  criteria: [...],
  parentTaskId: projectRoot.taskId
});
```

### Pattern 2: Test Injection Rule

Set up a rule to always require tests:

```typescript
bootstrap.injector.registerRule({
  name: "require-tests",
  condition: "before_complete",
  factory: () => ({
    goal: "Add comprehensive tests",
    deliverables: ["tests.ts"],
    criteria: [
      { criterion: "tests exist", measure: "file exists", threshold: true },
      { criterion: "tests pass", measure: "bun test", threshold: true }
    ]
  })
});
```

### Pattern 3: Progress Monitoring

Create a monitoring loop:

```typescript
async function monitorProgress(bootstrap: Bootstrap, rootTaskId: string) {
  while (true) {
    const status = bootstrap.projectStatus(rootTaskId);

    console.clear();
    printTaskTree(status, 0);

    if (status.state === "completed" || status.state === "failed") {
      break;
    }

    await new Promise(resolve => setTimeout(resolve, 1000));
  }
}

function printTaskTree(task: TaskProgress, indent: number) {
  const prefix = "  ".repeat(indent);
  console.log(`${prefix}${task.state} ${task.taskId} - ${Math.floor(task.progress * 100)}%`);

  for (const child of task.children) {
    printTaskTree(child, indent + 1);
  }
}
```

### Pattern 4: Dependency Management

Use task dependencies for coordination:

```typescript
// Code must complete before tests
const codeTask = bootstrap.create({ ... });
const testTask = bootstrap.create({ ..., parentTaskId: codeTask.taskId });

// Manually add dependency
graph.addEdge(testTask.taskId, codeTask.taskId, "depends_on");

// testTask will block until codeTask completes
```

### Pattern 5: Error Handling

Handle agent failures gracefully:

```typescript
bootstrap.registry.on("actor_died", (event) => {
  console.log(`Agent ${event.actorId} died: ${event.error}`);

  // Task automatically marked as failed
  const taskId = bootstrap.registry.getTaskId(event.actorId);
  const status = bootstrap.status(taskId);
  console.log(`Task ${taskId} state: ${status.state}`); // "failed"

  // Option 1: Restart agent
  const newAgent = createReplacementAgent();
  bootstrap.create({
    agent: newAgent,
    goal: "Resume failed work",
    deliverables: [...],
    criteria: [...],
    parentTaskId: status.taskId
  });

  // Option 2: Escalate to parent
  // Parent task will see blocker
});
```

## Troubleshooting

### Task tree not showing children

Make sure parent tasks have `spawned_by` edges:

```typescript
// This happens automatically when using bootstrap.create() with parentTaskId
const child = bootstrap.create({
  agent: actor,
  goal: "Child goal",
  deliverables: ["output.txt"],
  criteria: [...],
  parentTaskId: parent.taskId  // ← Critical!
});
```

### Agent not syncing with task state

Ensure you're using the bootstrap registry:

```typescript
// ✅ Correct
await bootstrap.registry.sendTo(agentId, "message", {});

// ❌ Wrong
await someOtherRegistry.sendTo(agentId, "message", {});
```

### Progress not updating

Progress is calculated from children. Leaf tasks show:
- `created`: 0%
- `active`: 50%
- `completed`: 100%

Parent tasks average their children's progress.

### Injection not creating dependency

Make sure `makeDependency: true`:

```typescript
bootstrap.inject({
  parentTaskId: parent.taskId,
  goal: "...",
  deliverables: [...],
  criteria: [...],
  makeDependency: true  // ← Important!
});
```

## Running the Example

Try the self-managing development example:

```bash
bun examples/self-managing-dev.ts
```

This demonstrates:
- Creating agent-tasks
- Task hierarchies
- Dynamic injection
- Progress tracking
- The "aha!" moment of self-management

## Next Steps

1. **Integrate with `/bg` command**: Make background agents automatically create agent-tasks
2. **Add visualization**: Build a web UI showing the task tree in real-time
3. **Enhance injection triggers**: Auto-trigger rules based on task state changes
4. **Add recovery strategies**: Automatic retry/restart for failed agent-tasks
5. **Enable agent swarms**: Multiple agents collaborating on one task

## Conclusion

The Bootstrap system turns agent execution into visible, manageable task trees. The system can now manage its own development, with automatic tracking, dynamic requirements injection, and zero manual polling.

**The ultimate dog-fooding moment!** 🎉
