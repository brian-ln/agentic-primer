# /bg Workflow Graph Integration Design

## User Insight

**From user:**
> "You could store the resume instructions on the task/agent in the graph."

This aligns perfectly with treating agents as actors in the graph!

---

## Current Checkpoint Approach (File-Based)

**Agent writes checkpoint to file:**
```json
// /tmp/claude/checkpoints/task_bg_a7e4d2.checkpoint.json
{
  "agentId": "task_bg_a7e4d2",
  "workState": {...},
  "resumeInstructions": {...}
}
```

**Problems:**
- Ephemeral (lost on system restart)
- Not queryable
- No graph relationships
- Manual cleanup required

---

## Graph-Based Approach (BETTER)

### Agent as Graph Node

**Model agent state in the graph:**

```typescript
// Agent node in graph
{
  id: "agent_task_bg_a7e4d2",
  type: "agent",
  status: "checkpointed", // or: running, completed, failed
  phase: "waiting_clarification",

  // Resume instructions stored in graph
  resumeInstructions: {
    readClarificationFrom: "/tmp/claude/clarifications/...",
    applyResponsesTo: "workState.blocked",
    nextPhase: "implementation",
    estimatedRemainingWork: "15-20 minutes"
  },

  // Work state
  workState: {
    completed: [...],
    inProgress: [...],
    blocked: [...],
    parallelWorkAvailable: [...]
  },

  // Timestamps
  launchedAt: "2026-01-18T10:00:00Z",
  checkpointedAt: "2026-01-18T10:01:10Z",
  resumedAt: null,
  completedAt: null
}
```

### Graph Relationships

**Agent ← spawned_by → Parent Task**
```cypher
// Agent is spawned by task
(agent_task_bg_a7e4d2) -[spawned_by]-> (task_170)

// Query: Which agents are working on task_170?
?[agent_id, status, phase] :=
  *agent{ agent_id, status, phase },
  *spawned_by{ agent_id, task_id: "task_170" }
```

**Agent → worked_on → Files**
```cypher
// Agent created/modified files
(agent_task_bg_a7e4d2) -[created]-> (file_watcher_spec_md)
(agent_task_bg_a7e4d2) -[modified]-> (file_watcher_actor_ts)

// Query: What files did this agent touch?
?[file_path, action] :=
  *created{ agent_id: "agent_task_bg_a7e4d2", file_path },
  action := "created"
OR
  *modified{ agent_id: "agent_task_bg_a7e4d2", file_path },
  action := "modified"
```

**Agent → blocked_by → Clarification**
```cypher
// Agent blocked by clarification need
(agent_task_bg_a7e4d2) -[blocked_by]-> (clarification_xyz789)

// Clarification links to questions
(clarification_xyz789) -[has_question]-> (question_Q1)
(clarification_xyz789) -[has_question]-> (question_Q2)

// Query: What is agent waiting for?
?[question_id, question_text, status] :=
  *agent{ agent_id: "agent_task_bg_a7e4d2", status: "checkpointed" },
  *blocked_by{ agent_id, clarification_id },
  *has_question{ clarification_id, question_id },
  *question{ question_id, text: question_text, status }
```

---

## Implementation Design

### Graph Schema

```typescript
// Datalog schema for agents
interface AgentNode {
  agent_id: string;           // Primary key
  type: "agent";
  status: AgentStatus;
  phase: string;

  // Resume state
  resume_instructions: ResumeInstructions;
  work_state: WorkState;

  // Metadata
  launched_at: string;
  checkpointed_at?: string;
  resumed_at?: string;
  completed_at?: string;

  // Context preservation
  session_log_path: string;
  extracted_context: SessionContext;
}

enum AgentStatus {
  RUNNING = "running",
  CHECKPOINTED = "checkpointed",
  COMPLETED = "completed",
  FAILED = "failed"
}

interface ResumeInstructions {
  read_clarification_from: string;
  apply_responses_to: string;
  next_phase: string;
  estimated_remaining_work: string;
}
```

### Graph Operations

**1. Agent launches:**
```typescript
// Create agent node
await graph.createNode({
  id: `agent_${agentId}`,
  type: "agent",
  status: "running",
  phase: "session_analysis",
  launched_at: new Date().toISOString(),
  session_log_path: sessionLogPath,
  // ...
});

// Link to parent task
await graph.createEdge({
  from: `agent_${agentId}`,
  to: `task_${taskId}`,
  type: "spawned_by"
});
```

**2. Agent checkpoints:**
```typescript
// Update agent node
await graph.updateNode(`agent_${agentId}`, {
  status: "checkpointed",
  phase: "waiting_clarification",
  checkpointed_at: new Date().toISOString(),
  work_state: checkpointData.workState,
  resume_instructions: checkpointData.resumeInstructions
});

// Link to clarification
await graph.createNode({
  id: `clarification_${clarificationId}`,
  type: "clarification",
  questions: questions,
  status: "pending"
});

await graph.createEdge({
  from: `agent_${agentId}`,
  to: `clarification_${clarificationId}`,
  type: "blocked_by"
});
```

**3. User responds:**
```typescript
// Update clarification
await graph.updateNode(`clarification_${clarificationId}`, {
  status: "answered",
  responses: userResponses,
  answered_at: new Date().toISOString()
});
```

**4. Parent resumes agent:**
```typescript
// Load agent state from graph
const agentNode = await graph.getNode(`agent_${agentId}`);
const resumeInstructions = agentNode.resume_instructions;
const workState = agentNode.work_state;

// Resume agent
await Task({
  resume: agentId,
  prompt: buildResumePrompt(agentNode),
  run_in_background: true
});

// Update graph
await graph.updateNode(`agent_${agentId}`, {
  status: "running",
  resumed_at: new Date().toISOString()
});
```

**5. Agent completes:**
```typescript
// Update agent node
await graph.updateNode(`agent_${agentId}`, {
  status: "completed",
  completed_at: new Date().toISOString(),
  deliverables: deliverablesList
});

// Link deliverables
for (const file of deliverables) {
  await graph.createEdge({
    from: `agent_${agentId}`,
    to: `file_${file}`,
    type: "created"
  });
}
```

---

## Query Capabilities

### Parent Queries

**"What agents are currently checkpointed?"**
```datalog
?[agent_id, phase, checkpointed_at, blocked_by] :=
  *agent{ agent_id, status: "checkpointed", phase, checkpointed_at },
  *blocked_by{ agent_id, clarification_id: blocked_by }
```

**"Which agent worked on this file?"**
```datalog
?[agent_id, action, timestamp] :=
  *created{ agent_id, file_path: $file, timestamp },
  action := "created"
OR
  *modified{ agent_id, file_path: $file, timestamp },
  action := "modified"
```

**"What tasks is agent X working on?"**
```datalog
?[task_id, task_goal] :=
  *spawned_by{ agent_id: $agent_id, task_id },
  *task{ task_id, goal: task_goal }
```

### User Queries

**"Show me all pending clarifications"**
```datalog
?[agent_id, question_text, asked_at] :=
  *agent{ agent_id, status: "checkpointed" },
  *blocked_by{ agent_id, clarification_id },
  *has_question{ clarification_id, question_id },
  *question{ question_id, text: question_text, status: "pending" },
  *clarification{ clarification_id, created_at: asked_at }
```

**"What work is agent X blocked on?"**
```datalog
?[blocked_item, reason] :=
  *agent{ agent_id: $agent_id },
  work_state = agent.work_state,
  blocked_item <- work_state["blocked"]
```

---

## Benefits

### 1. Persistence
- ✅ Agent state survives system restart
- ✅ No manual checkpoint file cleanup
- ✅ Historical record of all agents

### 2. Queryability
- ✅ "Show all checkpointed agents"
- ✅ "What files did agent X create?"
- ✅ "Which agents are blocked on clarifications?"
- ✅ "What's the average time agents wait for clarification?"

### 3. Graph Relationships
- ✅ Agent → Task linkage
- ✅ Agent → Files created
- ✅ Agent → Clarifications needed
- ✅ Agent → Parent agent (if sub-agents)

### 4. Analytics
- ✅ Agent performance metrics
- ✅ Clarification patterns
- ✅ Checkpoint/resume efficiency
- ✅ Work completion rates

### 5. Actor Model Alignment
- ✅ Agents are first-class actors in the graph
- ✅ `primer.agents.agent_${id}` addressing
- ✅ Message-based resume (graph → agent)
- ✅ System-managed state

---

## Integration with Actor Worldview

### Agent as Actor

```
primer.agents (supervisor)
├─ primer.agents.agent_a7e4d2 (background agent)
│   ├─ state: checkpointed
│   ├─ phase: waiting_clarification
│   └─ resume_instructions: {...}
└─ primer.agents.agent_b8f3c1 (another agent)
```

**Messages:**

```typescript
// Query agent state
send('primer.agents.agent_a7e4d2', {
  type: 'get_state'
});
// Response: { status: "checkpointed", work_state: {...}, resume_instructions: {...} }

// Resume agent
send('primer.agents.agent_a7e4d2', {
  type: 'resume',
  clarification_responses: responses
});
// Agent resumes execution

// Get agent deliverables
send('primer.agents.agent_a7e4d2', {
  type: 'get_deliverables'
});
// Response: { deliverables: [...], status: "completed" }
```

### Design → Fitness → Optimize

**Design**: Model agents as graph nodes with resume instructions
**Fitness**: Fast resume (<5s), queryable state, no data loss
**Optimize**: May cache hot agent states in memory, lazy-load cold states from graph
**Validate**: All agent state retrievable from graph, no file system dependencies

---

## Migration Path

### Phase 1: Hybrid (File + Graph)
- Write checkpoints to BOTH file and graph
- Resume uses graph if available, falls back to file
- Validate graph data matches file data

### Phase 2: Graph Primary
- Write only to graph
- Files are deprecated
- Resume always uses graph

### Phase 3: Pure Actor Model
- Agents are actors: `primer.agents.agent_${id}`
- Resume via message passing
- No files involved

---

## Implementation Tasks

**Created tasks in graph:**

1. `task_1768748812332_1obx6ckbf`: Update /bg workflow docs with checkpoint-resume pattern
2. `task_1768748817898_xm511wng2`: Add defined formulas for all numeric success criteria
3. `task_1768748824182_9h8tbdsww`: Design: Store agent resume instructions in task graph

**Next steps:**

1. Define Datalog schema for agent nodes
2. Implement graph operations (create, checkpoint, resume, complete)
3. Add query capabilities for agent state
4. Integrate with existing /bg workflow
5. Test checkpoint-resume with graph storage
6. Migrate from file-based to graph-based

---

## Example: Complete Agent Lifecycle in Graph

```typescript
// 1. Launch
await graph.createNode({
  id: "agent_task_bg_a7e4d2",
  type: "agent",
  status: "running",
  launched_at: "2026-01-18T10:00:00Z"
});

// 2. Checkpoint
await graph.updateNode("agent_task_bg_a7e4d2", {
  status: "checkpointed",
  checkpointed_at: "2026-01-18T10:01:10Z",
  resume_instructions: { /* ... */ },
  work_state: { /* ... */ }
});

// 3. User answers (30 minutes later)
await graph.updateNode("clarification_xyz789", {
  status: "answered",
  responses: userResponses
});

// 4. Resume
const agentState = await graph.getNode("agent_task_bg_a7e4d2");
await Task({ resume: agentState.agent_id, /* ... */ });
await graph.updateNode("agent_task_bg_a7e4d2", {
  status: "running",
  resumed_at: "2026-01-18T10:31:50Z"
});

// 5. Complete
await graph.updateNode("agent_task_bg_a7e4d2", {
  status: "completed",
  completed_at: "2026-01-18T10:45:00Z",
  deliverables: ["file1.md", "file2.ts"]
});
```

**All state in graph, fully queryable, persistent, relational!**

---

## Summary

**User insight:** Store resume instructions in the graph
**Why it's brilliant:**
- ✅ Persistent across restarts
- ✅ Queryable (analytics, monitoring)
- ✅ Graph relationships (agent → task → files)
- ✅ Actor model alignment (agents as graph actors)
- ✅ No file cleanup required

**Implementation:** Three-phase migration (Hybrid → Graph Primary → Pure Actor)

**Result:** Agents are first-class citizens in the graph, fully integrated with actor worldview!
