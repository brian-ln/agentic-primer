# Bootstrap Design: Self-Managing Development

## Vision

The system uses **itself** to manage its own development. Background agents become tasks in a task graph. Progress is visible. Coordination is automatic. The ultimate dog-fooding moment.

## Current State

**What We Have:**
- TaskNode with state machine (created → ready → active → blocked/completed/failed)
- Graph for message routing and node/edge storage
- Registry for actor lifecycle and message routing
- Actor interface with Message/Response protocol

**The Gap:**
- Background agents run independently (no task tree visibility)
- Manual coordination (parent polls child agents)
- No dynamic task injection (runtime requirement changes)
- Progress tracking is external to the system

## Core Concept: The Agent-Task Bridge

**Key Insight:** An agent doing work IS a task. We need to bridge these two worlds.

```
Background Agent (Process) ←→ TaskNode (State) ←→ Graph (Coordination)
```

### The Bridge Pattern

```typescript
interface AgentTask {
  taskId: string;        // TaskNode ID in graph
  agentId: string;       // Actor ID in registry
  processId?: number;    // OS process ID if background
}
```

**Bidirectional Sync:**
- Agent state changes → Update TaskNode state
- TaskNode messages → Route to Agent
- Agent completion → Mark TaskNode completed
- Agent blocked → Mark TaskNode blocked

## Architecture

### 1. Bootstrap Registry (extends Registry)

Manages agents as tasks with automatic state synchronization.

```typescript
class BootstrapRegistry extends Registry {
  private agentTasks: Map<string, AgentTask> = new Map();

  // Create agent AND its task node in one operation
  registerAgentTask(
    actor: Actor,
    taskOptions: CreateTaskOptions,
    graph: Graph
  ): AgentTask;

  // Sync agent state to task state
  syncAgentToTask(agentId: string): void;

  // Handle agent death → mark task failed
  onActorDied(agentId: string, reason: string): void;
}
```

### 2. Task Injection System

Allow runtime injection of new tasks (e.g., "tests are required").

```typescript
interface TaskInjection {
  parentTaskId: string;
  goal: string;
  deliverables: string[];
  criteria: ObjectiveCriterion[];
  triggerCondition?: "before_complete" | "on_block" | "immediate";
}

class TaskInjector {
  // Inject task dynamically
  inject(injection: TaskInjection, graph: Graph): TaskNode;

  // Pre-register injection rules
  registerRule(
    condition: string,
    factory: () => TaskInjection
  ): void;
}
```

**Example:** "Tests required" rule
```typescript
injector.registerRule("before_documentation_complete", () => ({
  parentTaskId: "task_docs",
  goal: "Write comprehensive tests",
  deliverables: ["test file with 80%+ coverage"],
  criteria: [{ criterion: "tests pass", measure: "bun test", threshold: true }],
  triggerCondition: "before_complete"
}));
```

### 3. State Synchronization

**Agent → Task Sync:**
- Actor sends message → Task state updates
- Actor completes → Task marked completed
- Actor dies → Task marked failed
- Actor blocks → Task marked blocked

**Task → Agent Sync:**
- Task blocked → Notify agent
- Task dependency ready → Resume agent
- Task eval fails → Send feedback to agent

### 4. Progress Visibility

Task tree becomes the single source of truth:

```typescript
interface TaskProgress {
  taskId: string;
  state: TaskState;
  progress: number;      // 0.0 - 1.0
  agentId?: string;      // Which agent is working on it
  blockers: string[];
  children: TaskProgress[];
}

function getProjectStatus(rootTaskId: string, graph: Graph): TaskProgress;
```

## Integration with /bg Command

Current `/bg` pattern:
```bash
# User runs background agent
/bg "create documentation"
# Agent a448d35 created
# [parent polls for status]
```

**Bootstrap pattern:**
```bash
# User runs background agent (creates task automatically)
/bg "create documentation"
# Agent a448d35 created
# Task task_123 created (linked to agent)
# Progress: task_123 [active] 0% → 50% → 100%
```

**Behind the scenes:**
```typescript
// Old way: just register actor
registry.register(actor);

// New way: register as agent-task
const agentTask = bootstrapRegistry.registerAgentTask(
  actor,
  {
    goal: userGoal,
    desiredDeliverables: ["output.md"],
    objectiveSuccessCriteria: [{ criterion: "file created", measure: "exists", threshold: true }]
  },
  graph
);

// Now task system manages it
const status = graph.send(agentTask.taskId, "query_status", {});
```

## Dynamic Task Injection Example

**Scenario:** User says "don't forget tests!" mid-execution

```typescript
// Agent is working on documentation task
const docTask = graph.getNode("task_docs");

// Inject test task dynamically
const testTask = injector.inject({
  parentTaskId: "task_docs",
  goal: "Write tests for minimal-actors",
  deliverables: ["minimal-actors.test.ts"],
  criteria: [
    { criterion: "tests pass", measure: "bun test", threshold: true },
    { criterion: "coverage", measure: "line coverage %", threshold: 80 }
  ],
  triggerCondition: "before_complete"
}, graph);

// doc task now depends on test task
graph.addEdge(docTask.properties.id, testTask.properties.id, "depends_on");

// Agent a448d35 sees new dependency, spawns test agent
// OR: mark doc task blocked, let parent handle
```

## Minimal Bootstrap Protocol

**3 Core Operations:**

1. **Create Agent-Task**
```typescript
const agentTask = bootstrap.create({
  agent: myActor,
  goal: "Build feature X",
  deliverables: ["feature.ts"],
  criteria: [{ criterion: "works", measure: "test", threshold: true }]
});
```

2. **Inject Task**
```typescript
bootstrap.inject({
  parent: agentTask.taskId,
  goal: "Add tests",
  deliverables: ["feature.test.ts"],
  triggerCondition: "before_complete"
});
```

3. **Query Status**
```typescript
const status = bootstrap.status(agentTask.taskId);
// { state: "active", progress: 0.75, children: [...] }
```

## Self-Management Example

**Current Development Scenario:**

```typescript
// Agent a448d35: Creating actor interface docs
// Agent a35bcf5: Fixing Playwright tests
// Manual: Parent polls both agents

// Bootstrap version:
const rootTask = createTask({
  goal: "Improve tk-agents system",
  deliverables: ["working code", "documentation", "tests passing"],
  objectiveSuccessCriteria: [
    { criterion: "tests pass", measure: "bun test", threshold: true }
  ]
}, graph);

// Agent a448d35 becomes child task
const docsAgentTask = bootstrap.create({
  agent: agent_a448d35,
  goal: "Create actor interface docs",
  deliverables: ["ACTOR_INTERFACE.md", "examples/minimal-actors.ts"],
  criteria: [{ criterion: "docs complete", measure: "file exists", threshold: true }],
  parentTaskId: rootTask.properties.id
});

// User says "tests required!" → inject dynamically
bootstrap.inject({
  parent: docsAgentTask.taskId,
  goal: "Add tests for minimal actors",
  deliverables: ["examples/minimal-actors.test.ts"],
  triggerCondition: "before_complete"
});

// Now task tree shows everything:
const status = bootstrap.status(rootTask.properties.id);
/*
{
  state: "active",
  progress: 0.5,
  children: [
    {
      id: "task_docs",
      state: "active",
      progress: 0.75,
      agentId: "a448d35",
      children: [
        {
          id: "task_tests",
          state: "active",
          progress: 0.5,
          agentId: "a448d35_test_subtask"
        }
      ]
    },
    {
      id: "task_playwright",
      state: "active",
      progress: 0.3,
      agentId: "a35bcf5"
    }
  ]
}
*/
```

## Implementation Plan

### Phase 1: Core Bridge (30 mins)
- Create `src/bootstrap/bridge.ts`
- Implement `AgentTask` type
- Basic sync agent → task state

### Phase 2: Task Injection (20 mins)
- Create `src/bootstrap/injector.ts`
- Implement dynamic task injection
- Add dependency updates

### Phase 3: Bootstrap Registry (30 mins)
- Create `src/bootstrap/registry.ts`
- Extend Registry with agent-task management
- Auto-sync on state changes

### Phase 4: Example & Tests (40 mins)
- Create `examples/self-managing-dev.ts`
- Show current agents as tasks
- Demonstrate dynamic injection
- Write tests proving it works

### Phase 5: Documentation (20 mins)
- Create `SELF_MANAGEMENT.md` user guide
- Document patterns and best practices
- Show "aha!" moment examples

## Success Metrics

**The "Aha!" Moment:**
1. ✅ Run `bun examples/self-managing-dev.ts`
2. ✅ See task tree automatically tracking agent progress
3. ✅ Inject task dynamically ("add tests!")
4. ✅ Watch dependency propagate automatically
5. ✅ System manages itself with zero manual polling

**Technical Success:**
- [ ] All tests pass (baseline: 86 pass, 2 fail Playwright)
- [ ] Bootstrap adds ≤200 lines of code (stay SIMPLE)
- [ ] Works with existing code (no major refactoring)
- [ ] Clear example showing the pattern

## Design Decisions

### Why Extend Registry vs Modify Graph?

**Decision:** Create BootstrapRegistry that extends Registry.

**Rationale:**
- Registry already manages actor lifecycle
- Graph should stay simple (message routing only)
- Separation of concerns: Registry = execution, Graph = data
- Easier to add/remove bootstrap feature

### Why Not Make TaskNode Execute Agents?

**Decision:** Keep TaskNode as state, agents as execution.

**Rationale:**
- Single Responsibility: TaskNode = state machine, Actor = execution
- TaskNode already works well for coordination
- Agents already work well for execution
- Bridge connects them without mixing concerns

### Why Promise-Based Task Injection?

**Decision:** Task injection is synchronous (immediate effect).

**Rationale:**
- Injection must update task tree immediately
- Async would complicate dependency tracking
- Agents can handle async execution
- Keep state updates synchronous

## Edge Cases

1. **Agent dies mid-execution**
   - Bootstrap marks task "failed"
   - Parent task sees blocker
   - Can restart or escalate

2. **Circular dependencies**
   - Graph.addEdge should detect cycles
   - Reject injection that creates cycle
   - Return error to injector

3. **Orphaned tasks**
   - Task exists but agent died
   - Bootstrap cleanup: mark failed if agent missing
   - Periodic health check

4. **Multiple agents per task**
   - Future: allow agent swarm
   - MVP: 1 agent = 1 task
   - Document for v2

## Next Steps

1. Implement core bridge functionality
2. Create working example
3. Write tests
4. Document usage patterns
5. Celebrate the bootstrap moment! 🎉
