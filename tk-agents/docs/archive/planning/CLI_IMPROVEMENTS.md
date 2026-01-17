# CLI Improvements for Agent Efficiency

## Executive Summary

The Task CLI is currently optimized for human interaction with emojis, verbose output, and interactive prompts. For agent/sub-agent efficiency, the following improvements would reduce tool call overhead, enable batch operations, and provide machine-readable outputs. The primary opportunities are:

1. **JSON output mode** for all commands (eliminates parsing overhead)
2. **Batch operations** to reduce tool calls (create multiple tasks, update multiple states)
3. **Structured error responses** with exit codes and machine-readable formats
4. **Query optimization** with projection/filtering at the CLI level
5. **Non-interactive mode** for automated workflows (disable prompts)

---

## Critical Improvements (P0)

### 1. Add JSON Output Mode (`--json` flag)

**Current Limitation:**
All commands output human-formatted text with emojis, separators, and varying formats. Agents must parse inconsistent text output to extract data.

```bash
# Current output (human-friendly)
task list
⭕ P1 task_1          created    Build authentication module [backend, auth]
🔄 P0 task_2          active     Write unit tests [testing]
```

**Proposed Enhancement:**
Add global `--json` flag to return structured JSON output for all commands.

```bash
# Proposed
task list --json
{
  "success": true,
  "data": [
    {
      "id": "task_1",
      "state": "created",
      "priority": 1,
      "goal": "Build authentication module",
      "labels": ["backend", "auth"]
    },
    {
      "id": "task_2",
      "state": "active",
      "priority": 0,
      "goal": "Write unit tests",
      "labels": ["testing"]
    }
  ]
}
```

**Implementation:**
- Add `--json` option to all commands
- Create `formatAsJson()` helper
- Return structured responses with `success`, `data`, `error` fields
- Apply to: `list`, `ready`, `search`, `show`, `graph`, `status`, `eval`, `add`, `update`, `delete`

**Benefit:**
- Agents parse JSON once instead of regex/splitting text
- Reduces parsing errors and brittleness
- Enables type-safe consumption in agent code

**Effort:** Small (1-2 hours)
- Add option parsing for `--json`
- Wrap outputs in `formatAsJson()` conditional

---

### 2. Batch Task Creation (`task batch-add`)

**Current Limitation:**
Creating multiple tasks requires N CLI invocations, each loading/saving the graph from disk.

```bash
# Current (N file I/O operations)
task add "Task 1" --priority P0
task add "Task 2" --priority P1
task add "Task 3" --priority P2
```

**Proposed Enhancement:**
Add `batch-add` command accepting JSON/YAML array of task specifications.

```bash
# Proposed
task batch-add --file tasks.json

# tasks.json
[
  {
    "goal": "Task 1",
    "priority": 0,
    "deliverables": ["Deliverable 1"],
    "labels": ["backend"]
  },
  {
    "goal": "Task 2",
    "priority": 1,
    "depends": ["task_1"]
  }
]
```

**Output (with --json):**
```json
{
  "success": true,
  "created": ["task_1", "task_2", "task_3"],
  "errors": []
}
```

**Implementation:**
- Add `cmdBatchAdd(filePath: string)` function
- Parse JSON/YAML task specs
- Load graph once, create all tasks, save once
- Return created IDs or errors

**Benefit:**
- Reduces tool calls from N to 1
- Single disk I/O operation vs N
- Atomic operation (all succeed or all fail)

**Effort:** Medium (3-4 hours)
- JSON schema validation for input
- Loop through specs calling TaskActor
- Aggregate results

---

### 3. Batch State Updates (`task batch-update`)

**Current Limitation:**
Updating multiple task states requires N CLI invocations.

```bash
# Current
task update task_1 start
task update task_2 start
task update task_3 complete
```

**Proposed Enhancement:**
```bash
task batch-update --file updates.json

# updates.json
[
  { "id": "task_1", "action": "start" },
  { "id": "task_2", "action": "start" },
  { "id": "task_3", "action": "complete", "result": "Done" }
]
```

**Output:**
```json
{
  "success": true,
  "updated": ["task_1", "task_2", "task_3"],
  "errors": []
}
```

**Benefit:**
- Single graph load/save cycle
- Atomic multi-task state transitions
- Reduces agent tool calls

**Effort:** Small (2-3 hours)

---

### 4. Query with Projection (`task list --fields`)

**Current Limitation:**
`task list` returns all properties, forcing agents to receive and filter large payloads.

```bash
# Current (all fields)
task list --json
{
  "data": [
    {
      "id": "task_1",
      "goal": "...",
      "state": "created",
      "priority": 1,
      "labels": ["backend", "auth"],
      "createdAt": "...",
      "desiredDeliverables": [...],
      "objectiveSuccessCriteria": [...],
      "informationGaps": [...]
      // ... many more fields
    }
  ]
}
```

**Proposed Enhancement:**
Add `--fields` option to project only required fields.

```bash
task list --json --fields id,state,priority

{
  "success": true,
  "data": [
    { "id": "task_1", "state": "created", "priority": 1 },
    { "id": "task_2", "state": "active", "priority": 0 }
  ]
}
```

**Benefit:**
- Reduces payload size (critical for LLM context)
- Agent specifies exactly what it needs
- Faster serialization/deserialization

**Effort:** Small (1-2 hours)
- Parse `--fields` as comma-separated list
- Use `pick()` helper to project properties

---

### 5. Non-Interactive Mode (`--yes` flag)

**Current Limitation:**
`task delete` prompts for confirmation, blocking automated workflows.

```typescript
// src/cli/task.ts:470
const answer = prompt("Delete this task? (yes/no): ");
if (answer?.toLowerCase() !== "yes") {
  console.log("Deletion cancelled.");
  process.exit(0);
}
```

**Proposed Enhancement:**
Add global `--yes` flag to skip all confirmation prompts.

```bash
# Current (blocks)
task delete task_1

# Proposed
task delete task_1 --yes
```

**Implementation:**
- Add `--yes` flag to global options
- Check `options.yes` before any `prompt()` call
- Auto-accept confirmations if flag present

**Benefit:**
- Enables fully automated agent workflows
- No hanging processes waiting for input
- Aligns with `--force` but more intuitive

**Effort:** Small (1 hour)

---

## High-Value Improvements (P1)

### 6. Filter Composition with OR Logic (`task list --or`)

**Current Limitation:**
Filters use AND logic only. Cannot express "P0 OR P1 tasks".

```typescript
// src/cli/task.ts:146-169
// All filters must match (AND logic)
if (filters) {
  tasks = tasks.filter((id) => {
    const props = graph.getNodeProperties(id) as TaskProperties;
    // All conditions must be true
    if (filters.status && props.state !== filters.status) return false;
    if (filters.label && ...) return false;
    if (filters.priority && props.priority !== filters.priority) return false;
    return true;
  });
}
```

**Proposed Enhancement:**
Add `--or` flag to use OR semantics.

```bash
# AND (current)
task list --priority P0 --label backend --json
# Returns: P0 AND backend

# OR (proposed)
task list --priority P0,P1 --or --json
# Returns: P0 OR P1

task list --status active --status blocked --or --json
# Returns: active OR blocked
```

**Implementation:**
- Allow multiple values for options (comma-separated)
- Add `--or` flag to switch semantics
- Refactor filter logic

**Benefit:**
- More expressive queries in single command
- Reduces multiple CLI calls for unions

**Effort:** Medium (2-3 hours)

---

### 7. Task Dependency Chain Query (`task deps <id>`)

**Current Limitation:**
`task graph` shows tree but not flattened dependency list. Agents need to traverse manually.

**Proposed Enhancement:**
Add `task deps <id>` to return flat list of all dependencies (transitive closure).

```bash
task deps task_1 --json
{
  "success": true,
  "id": "task_1",
  "dependencies": [
    { "id": "task_0", "state": "completed", "type": "depends_on", "depth": 1 },
    { "id": "task_3", "state": "completed", "type": "depends_on", "depth": 2 },
    { "id": "task_5", "state": "active", "type": "depends_on", "depth": 2 }
  ],
  "blockedBy": ["task_5"]
}
```

**Benefit:**
- Single command to check all blockers
- No need to recursively call `task show` for dependencies
- Immediate visibility into what's blocking progress

**Effort:** Medium (3-4 hours)
- Implement DFS/BFS traversal
- Flatten dependency tree
- Identify incomplete dependencies

---

### 8. Task Progress Summary (`task summary`)

**Current Limitation:**
No single command to get overall project status. Agents must aggregate manually.

**Proposed Enhancement:**
```bash
task summary --json
{
  "success": true,
  "total": 25,
  "byState": {
    "created": 5,
    "active": 3,
    "blocked": 2,
    "completed": 15,
    "failed": 0
  },
  "byPriority": {
    "P0": 2,
    "P1": 8,
    "P2": 10,
    "P3": 5
  },
  "readyCount": 4,
  "blockedTasks": ["task_7", "task_12"]
}
```

**Benefit:**
- Single command for status overview
- Reduces multiple list/ready/status calls
- Dashboard-style data for agents

**Effort:** Small (2 hours)

---

### 9. Bulk Edge Creation (`task link-many`)

**Current Limitation:**
Creating edges requires individual updates or manual edge creation after task creation.

**Proposed Enhancement:**
```bash
task link-many --file links.json

# links.json
[
  { "from": "task_1", "to": "task_0", "type": "depends_on" },
  { "from": "task_2", "to": "knowledge_1", "type": "requires_knowledge" },
  { "from": "task_3", "to": "task_1", "type": "spawned_by" }
]
```

**Benefit:**
- Batch relationship creation
- Single I/O operation

**Effort:** Small (2 hours)

---

### 10. Search with Regular Expressions (`task search --regex`)

**Current Limitation:**
Search uses case-insensitive substring match. No pattern matching.

```typescript
// src/cli/task.ts:386
if (props.goal.toLowerCase().includes(normalizedQuery)) {
  matchedIn.push("goal");
}
```

**Proposed Enhancement:**
```bash
task search --regex "^Build.*module$" --json
```

**Benefit:**
- More powerful query capabilities
- Match structured patterns in goals/deliverables

**Effort:** Small (1-2 hours)
- Add `--regex` flag
- Compile pattern, use `test()` instead of `includes()`

---

### 11. Export Subgraph (`task export <id>`)

**Current Limitation:**
No way to extract a task and its dependencies as standalone file.

**Proposed Enhancement:**
```bash
task export task_1 --output subtask.json
```

Exports task_1 and all its dependencies (transitive closure) to new file.

**Benefit:**
- Agent can work on subgraphs independently
- Share task clusters across projects
- Checkpoint intermediate state

**Effort:** Medium (3-4 hours)
- DFS to collect all reachable nodes
- Filter edges to subgraph
- Write new tasks.json with subset

---

## Nice-to-Have Improvements (P2)

### 12. Watch Mode (`task watch`)

**Proposed Enhancement:**
```bash
task watch --json
# Continuously outputs JSON on task changes
```

**Benefit:**
- Real-time monitoring for agents
- Event-driven workflows

**Effort:** Large (6-8 hours)
- File system watcher on tasks.json
- Incremental reload on change
- Event stream output

---

### 13. Task Templates (`task add --template`)

**Proposed Enhancement:**
```bash
task add --template test-task --goal "Test login feature"
# Uses predefined deliverables, criteria, labels
```

**Benefit:**
- Reduces agent specification overhead
- Standardize common task types

**Effort:** Medium (4-5 hours)
- Template definition format
- Template storage and lookup

---

### 14. Inline Criteria Evaluation (`task list --eval`)

**Proposed Enhancement:**
```bash
task list --status active --eval --json
# Includes live eval() result in each task
```

**Benefit:**
- Single query for state + evaluation
- No need to call `task eval` per task

**Effort:** Small (2 hours)

---

### 15. Task Reordering (`task reorder`)

**Proposed Enhancement:**
```bash
task reorder task_1 task_2 task_3 --by priority
# Batch update priorities
```

**Benefit:**
- Adjust execution order efficiently

**Effort:** Small (2 hours)

---

### 16. Conditional Execution (`task if-ready <id> then <command>`)

**Proposed Enhancement:**
```bash
task if-ready task_1 then update task_1 start
# Only executes update if task is ready
```

**Benefit:**
- Single command for check-and-act pattern
- Reduces two-step agent logic

**Effort:** Medium (3-4 hours)

---

### 17. Task Cloning (`task clone <id>`)

**Proposed Enhancement:**
```bash
task clone task_1 --goal "New goal"
# Creates new task with same structure
```

**Benefit:**
- Reuse task patterns

**Effort:** Small (2 hours)

---

### 18. Validation Mode (`task validate`)

**Proposed Enhancement:**
```bash
task validate --json
# Checks invariants: cycles, orphaned edges, invalid states
```

**Benefit:**
- Agent verifies data integrity
- Catch corruption early

**Effort:** Medium (3-4 hours)

---

### 19. Task Timeline (`task timeline <id>`)

**Proposed Enhancement:**
```bash
task timeline task_1 --json
# Returns all state transitions with timestamps
```

**Benefit:**
- Historical audit trail
- Duration analysis

**Effort:** Medium (requires state change logging)

---

### 20. Machine-Readable Error Output

**Current Limitation:**
Errors print to stderr as plain text with no structure.

```typescript
// src/cli/task.ts:107
console.error(`Error: ${TASKS_FILE} already exists`);
process.exit(1);
```

**Proposed Enhancement:**
With `--json`, errors should be structured.

```bash
task init --json
# Output (if error):
{
  "success": false,
  "error": {
    "code": "FILE_EXISTS",
    "message": "tasks.json already exists",
    "path": "/path/to/tasks.json"
  }
}
# Exit code: 1
```

**Benefit:**
- Agents parse errors programmatically
- Conditional error handling based on error codes

**Effort:** Medium (2-3 hours)
- Define error code enum
- Wrap all error paths
- Format as JSON when `--json` flag present

---

## Implementation Roadmap

### Phase 1: JSON Output Foundation (Week 1)
**Goal:** Enable machine-readable output for all commands

1. Implement global `--json` flag parsing
2. Add `formatAsJson()` helper with error handling
3. Convert `list`, `ready`, `search`, `show` to JSON output
4. Add machine-readable error format
5. **Deliverable:** All query commands support `--json`

**Priority:** P0 (blocks most other improvements)

---

### Phase 2: Batch Operations (Week 2)
**Goal:** Reduce tool call overhead for multi-task operations

1. Implement `batch-add` command
2. Implement `batch-update` command
3. Implement `link-many` command
4. Add `--yes` non-interactive mode
5. **Deliverable:** Agents can operate on multiple tasks in single call

**Priority:** P0

---

### Phase 3: Query Optimization (Week 3)
**Goal:** Reduce payload size and improve filtering

1. Add `--fields` projection to `list`, `show`, `ready`
2. Implement OR filter semantics (`--or` flag)
3. Add `task deps` transitive dependency query
4. Add `task summary` aggregate statistics
5. **Deliverable:** Fine-grained, efficient queries

**Priority:** P1

---

### Phase 4: Advanced Queries (Week 4)
**Goal:** Power-user features for complex workflows

1. Add regex search (`--regex`)
2. Implement `export` subgraph extraction
3. Add inline eval (`--eval` flag for list)
4. **Deliverable:** Advanced agent query capabilities

**Priority:** P1-P2

---

### Phase 5: Workflow Enhancements (Optional)
**Goal:** Agent-friendly workflow patterns

1. Task templates
2. Conditional execution
3. Watch mode
4. Validation mode
5. **Deliverable:** Reactive and declarative workflows

**Priority:** P2

---

## Measurement Criteria

### Success Metrics

1. **Tool Call Reduction**
   - Before: 10 tool calls to create/update 10 tasks
   - After: 1 tool call with batch operations
   - **Target:** 90% reduction in tool calls for multi-task ops

2. **Payload Size**
   - Before: 5KB JSON for `task list` with all fields
   - After: 500 bytes with `--fields id,state,priority`
   - **Target:** 80% reduction in data transfer

3. **Parsing Overhead**
   - Before: Custom text parsing with regex
   - After: Single `JSON.parse()` call
   - **Target:** Zero parsing errors

4. **Latency**
   - Before: N disk I/O operations for N tasks
   - After: 1 disk I/O operation for batch
   - **Target:** 50% latency reduction for batch ops

5. **Agent Code Complexity**
   - Before: 20 lines to parse text output
   - After: 3 lines to parse JSON
   - **Target:** 70% code reduction in agent parsing logic

---

## Backward Compatibility

### Strategy

All improvements are **additive** and **opt-in**:

1. **Default behavior unchanged**
   - Existing commands work as-is
   - Human-friendly output remains default
   - No breaking changes

2. **Opt-in with flags**
   - `--json` enables machine-readable output
   - `--yes` enables non-interactive mode
   - `--fields` enables projection
   - New commands (`batch-add`, `deps`, `summary`) don't conflict

3. **Version detection**
   - Add `task version --json` to report CLI capabilities
   - Agents can detect feature availability

---

## Risks and Mitigations

### Risk 1: JSON Output Breaks Human Workflows
**Mitigation:** JSON output is opt-in via `--json` flag. Default remains human-friendly.

### Risk 2: Batch Operations Increase Complexity
**Mitigation:** Batch commands validate inputs before execution. Provide clear error messages with line numbers for failed items.

### Risk 3: OR Filters Confuse Users
**Mitigation:** Default remains AND semantics. `--or` must be explicitly specified. Document with examples.

### Risk 4: Projection Increases Maintenance
**Mitigation:** Use TypeScript type safety to ensure projected fields match schema. Add tests for all field combinations.

---

## Conclusion

The Task CLI improvements prioritize **reducing tool calls**, **enabling batch operations**, and **providing machine-readable output** to optimize agent efficiency. The P0 improvements (JSON output, batch operations, non-interactive mode) provide immediate 10x efficiency gains for agents while maintaining backward compatibility for human users.

**Recommended Starting Point:**
1. Implement `--json` flag (2 days)
2. Implement `batch-add` and `batch-update` (3 days)
3. Add `--yes` non-interactive mode (1 day)
4. Add `--fields` projection (2 days)

**Total effort for P0 improvements:** ~2 weeks for single developer

**Expected ROI:**
- 90% reduction in tool calls for multi-task operations
- 80% reduction in payload size with projection
- Zero text parsing errors with JSON output
- Enables fully automated agent workflows with non-interactive mode
