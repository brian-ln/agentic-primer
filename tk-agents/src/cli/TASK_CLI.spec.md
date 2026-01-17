# Task CLI Specification

## Overview

The Task CLI is a command-line interface for managing task graphs using the Graph/TaskNode actor system. It provides a persistent file-based storage layer (`tasks.json`) on top of the in-memory Graph system, enabling human-driven task management workflows.

## Core Mission

Enable users to manage hierarchical task graphs with dependencies, priorities, labels, and success criteria through a simple command-line interface while maintaining data persistence between sessions.

## Architecture

```
+----------------+     +-----------+     +-------------+
|   Task CLI     | --> |   Graph   | --> | TaskActor   |
| (Command Layer)|     | (Router)  |     | (Message    |
|                |     |           |     |  Handler)   |
+----------------+     +-----------+     +-------------+
        |                    ^
        v                    |
+----------------+           |
|  tasks.json    |-----------+
| (Persistence)  |  load/save
+----------------+
```

### Component Responsibilities

- **Task CLI**: Command parsing, display formatting, user interaction
- **Graph**: Node/edge management, message routing, serialization
- **TaskActor**: Task state machine, message handling, business logic
- **tasks.json**: Persistent storage of graph state

## Commands Reference

### Initialization Command

#### `task init`

Creates a new `tasks.json` file with an example task.

**Preconditions:**
- `tasks.json` must NOT exist in current directory

**Postconditions:**
- Creates `tasks.json` with valid JSON structure
- Contains one example task demonstrating the system

**Error Conditions:**
- File already exists: exits with code 1

**Output:**
```
Created tasks.json with example task
Task ID: task_1
```

---

### Task Creation

#### `task add <goal> [options]`

Creates a new task in the graph.

**Arguments:**
- `<goal>`: Required. The task goal/description.

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--deliverables` | `d1,d2,...` | Comma-separated deliverables |
| `--criteria` | `name:measure:threshold` | Success criterion definition |
| `--depends` | `id1,id2,...` | Dependency task IDs |
| `--parent` | `id` | Parent task ID |
| `--labels` | `tag1,tag2,...` | Comma-separated labels |
| `--priority` | `P0\|P1\|P2\|P3\|P4` or `0-4` | Priority (0 is highest) |

**Preconditions:**
- `tasks.json` must exist
- Referenced dependency and parent IDs must exist

**Postconditions:**
- New task created with unique ID (`task_N`)
- Dependency edges created if `--depends` specified
- Spawned_by edge created if `--parent` specified

**Output:**
```
Added task: task_5
Goal: Implement user authentication
Labels: backend, security
Priority: P1
```

---

### Task Modification

#### `task update <id> <action> [args...]`

Updates a task's state.

**Actions:**

| Action | Arguments | Description |
|--------|-----------|-------------|
| `start` | none | Transitions task to `active` state |
| `complete` | none | Transitions task to `completed` state |
| `block` | `[reason]` | Transitions task to `blocked` state |

**State Transition Rules:**
```
start:    created|ready -> active
complete: active -> completed (requires all criteria passed)
block:    * -> blocked
```

**Output:**
```
Started task task_5: {
  success: true,
  state: "active",
}
```

---

#### `task delete <id> [--force]`

Removes a task and all connected edges.

**Flags:**
- `--force`: Skip confirmation prompt

**Safety Mechanism:**
- Without `--force`, displays task details and prompts for confirmation
- User must type "yes" to confirm

**Output:**
```
Task to delete: task_5
--------------------------------------------------------------------------------
Goal: Implement user authentication
State: created

Connected edges (2):
  depends_on: task_5 -> task_3
  spawned_by: task_5 -> task_1

Delete this task? (yes/no): yes

Deleted task task_5
Removed 2 connected edge(s)
```

---

### Task Listing and Filtering

#### `task list [options]`

Lists all tasks with optional filters.

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--status` | `<state>` | Filter by state (created, ready, active, blocked, completed, failed) |
| `--label` | `<label>` | Filter by label (case-insensitive) |
| `--priority` | `P0-P4` or `0-4` | Filter by priority |

**Filter Logic:** AND semantics - all specified filters must match.

**Display Format:**
```
Tasks:
--------------------------------------------------------------------------------
[emoji] [priority] [id]             [state]     [goal preview] [labels]
```

**Status Emojis:**
| State | Emoji |
|-------|-------|
| created | :o: |
| ready | :yellow_circle: |
| active | :arrows_counterclockwise: |
| blocked | :no_entry_sign: |
| completed | :white_check_mark: |
| failed | :x: |
| unknown | :question: |

**Output Example:**
```
Tasks:
--------------------------------------------------------------------------------
:arrows_counterclockwise: P0 task_1          active     Build authentication module [backend, auth]
:o: P1 task_2          created    Write unit tests [testing]
:white_check_mark:    task_3          completed  Setup project structure
```

---

#### `task ready`

Shows tasks with no blockers (ready to work on).

**Ready Criteria:**
1. State is NOT `completed`, `failed`, or `blocked`
2. All `depends_on` edges point to `completed` tasks

**Sort Order:**
1. Priority (P0 first, P4 last)
2. Creation date (oldest first)

**Output:**
```
Ready Tasks (no blockers):
--------------------------------------------------------------------------------
:o: task_2          [P0]  Implement login form
:yellow_circle: task_5          [P1]  Add password validation
:arrows_counterclockwise: task_3                Document API endpoints
```

---

#### `task search <query>`

Searches tasks by keyword.

**Search Fields:**
- Goal text
- Deliverables
- Objective success criteria

**Match:** Case-insensitive substring match.

**Output:**
```
Found 2 task(s) matching "authentication":
--------------------------------------------------------------------------------
:arrows_counterclockwise: task_1          active     Build authentication module
   [goal, deliverables]
:o: task_4          created    Write authentication tests
   [goal]
```

---

### Task Inspection

#### `task show <id>`

Shows detailed task information.

**Display Sections:**
- Basic info: Goal, State, Priority, Labels, Timestamps
- Deliverables list
- Success Criteria with pass/fail status
- Connected edges
- Information gaps

**Output:**
```
Task: task_1
--------------------------------------------------------------------------------
Goal:           Build authentication module
State:          active
Priority:       P1
Labels:         backend, auth
Created:        2024-01-15T10:30:00.000Z
Started:        2024-01-15T11:00:00.000Z

Deliverables:
  - Login endpoint
  - Session management

Success Criteria:
  :hourglass: All tests passing (test count >= 10)
  :hourglass: Code coverage (percentage >= 80)

Edges:
  depends_on: task_1 -> task_0
  spawned_by: task_1 -> task_0

Information Gaps:
  - OAuth provider requirements
```

---

#### `task status <id>`

Shows task status with blockers.

**Output:** JSON format with state, progress, blockers, and children status.

```json
{
  "state": "blocked",
  "progress": 0.5,
  "blockers": [
    "Waiting on: Setup database schema"
  ],
  "childrenStatus": [
    { "id": "task_5", "state": "completed", "progress": 1 },
    { "id": "task_6", "state": "active", "progress": 0.5 }
  ]
}
```

---

#### `task eval <id>`

Evaluates task completion criteria.

**Output:** JSON format with score, pass/fail, and criterion details.

```json
{
  "score": 0.5,
  "passed": false,
  "objectiveCriteria": [
    { "criterion": "Tests passing", "threshold": true, "actual": true, "passed": true },
    { "criterion": "Coverage", "threshold": 80, "actual": 65, "passed": false }
  ],
  "observations": [
    "1/2 criteria passed"
  ]
}
```

---

### Graph Visualization

#### `task graph <id>`

Shows dependency tree for a task.

**Rendering:**
- ASCII tree with box-drawing characters
- Status emojis for each node
- Cycle detection with warning

**Output:**
```
Dependency graph for task_1:
--------------------------------------------------------------------------------
:arrows_counterclockwise: task_1: Build authentication module
|-- depends_on:
    :white_check_mark: task_0: Setup project structure
|-- depends_on:
    :o: task_2: Configure database
    |-- depends_on:
        :white_check_mark: task_3: Install dependencies
```

**Circular Dependency Detection:**
```
:arrows_counterclockwise: task_1: Build feature A
|-- depends_on:
    :warning:  (circular dependency detected)
```

---

## File Format

### tasks.json Structure

```typescript
interface TaskFile {
  nodes: NodeProperties[];  // Array of task property objects
  edges: Edge[];            // Array of edge objects
}
```

### Node Properties Schema

```typescript
interface TaskProperties {
  id: string;                           // "task_N" format
  type: "task";
  state: TaskState;                     // created|ready|active|blocked|completed|failed
  goal: string;
  desiredDeliverables: string[];
  objectiveSuccessCriteria: ObjectiveCriterion[];
  subjectiveSuccessCriteria?: SubjectiveCriterion[];
  knownInformation: string[];
  informationGaps: string[];
  toolsAvailable: string[];
  createdAt: string;                    // ISO 8601 timestamp
  startedAt?: string;
  completedAt?: string;
  parentTaskId?: string;
  result?: unknown;
  labels?: string[];
  priority?: 0 | 1 | 2 | 3 | 4;
}
```

### Edge Schema

```typescript
interface Edge {
  id: string;           // "edge_N" format
  fromId: string;       // Source node ID
  toId: string;         // Target node ID
  type: EdgeType;       // depends_on|requires_knowledge|produces|spawned_by|blocks|references
  properties: Record<string, unknown>;
}
```

---

## State Machine

### Task States

```
                    +----------+
                    | created  |
                    +----+-----+
                         |
            +------------+------------+
            |                         |
            v                         v
       +---------+              +---------+
       | ready   |              | blocked |<--+
       +----+----+              +----+----+   |
            |                        |         |
            +--------+  +------------+         |
                     |  |                      |
                     v  v                      |
                  +--------+                   |
                  | active |-------------------+
                  +---+----+
                      |
          +-----------+-----------+
          |                       |
          v                       v
    +-----------+            +--------+
    | completed |            | failed |
    +-----------+            +--------+
```

### Transition Rules

| From | To | Trigger | Conditions |
|------|----|---------|------------|
| created | active | start | none |
| created | blocked | block | none |
| ready | active | start | none |
| ready | blocked | block | none |
| active | completed | complete | all criteria passed |
| active | blocked | block | none |
| active | failed | fail | explicit failure |
| blocked | active | unblock | blocker resolved |

---

## Priority System

| Level | Display | Meaning |
|-------|---------|---------|
| 0 | P0 | Critical - must do immediately |
| 1 | P1 | High - do soon |
| 2 | P2 | Medium - normal priority |
| 3 | P3 | Low - do when time permits |
| 4 | P4 | Lowest - nice to have |

Priority affects sort order in `task ready` command.

---

## Error Handling

### Common Error Conditions

| Error | Cause | Exit Code |
|-------|-------|-----------|
| `Task file not found` | Running commands before `task init` | 1 |
| `tasks.json already exists` | Running `task init` twice | 1 |
| `Task not found: <id>` | Invalid task ID | 1 |
| `Unknown action: <action>` | Invalid update action | 1 |
| `Invalid priority` | Priority not P0-P4 or 0-4 | 1 |

### Error Output Format

```
Error: <message>
```

---

## Performance Characteristics

- **File I/O**: Synchronous read/write operations
- **Graph operations**: O(n) node lookup, O(e) edge filtering
- **Search**: O(n) full scan with O(m) substring match per field
- **Memory**: Full graph loaded into memory during command execution

---

## Integration Points

### With Graph System

- CLI creates Graph instance per command
- Uses `graph.send()` for actor messaging
- Uses `graph.dump()` for serialization
- Uses `graph.addEdge()`, `graph.removeNode()` for structure changes

### With TaskActor

- Created via `TaskActor()` factory function
- Messages: `get`, `start`, `complete`, `block`, `eval`, `query_status`
- Properties accessed via `graph.getNodeProperties()`

### With File System

- Uses Node.js `fs` module (readFileSync, writeFileSync, existsSync)
- JSON serialization with Date-to-ISO transform

---

## Command Composition Rules

### Prerequisite Chain

```
init -> add -> update/show/eval/status/graph/delete
            -> list/ready/search (after at least one add)
```

### Safe Command Sequences

```
task init
task add "Goal 1" --priority P0
task add "Goal 2" --depends task_1
task list --status created
task update task_1 start
task ready
task update task_1 complete
task graph task_2
task delete task_2 --force
```

---

## Verification Checklist

- [ ] `task init` creates valid tasks.json
- [ ] `task add` creates task with correct properties
- [ ] `task add --depends` creates dependency edges
- [ ] `task add --parent` creates spawned_by edge
- [ ] `task add --labels` stores labels array
- [ ] `task add --priority` stores priority number
- [ ] `task update start` transitions state correctly
- [ ] `task update complete` validates criteria
- [ ] `task update block` stores reason
- [ ] `task delete` prompts for confirmation
- [ ] `task delete --force` skips confirmation
- [ ] `task list` displays all tasks
- [ ] `task list --status` filters correctly
- [ ] `task list --label` filters case-insensitively
- [ ] `task list --priority` filters by exact match
- [ ] `task ready` excludes blocked/completed/failed
- [ ] `task ready` checks dependency completion
- [ ] `task ready` sorts by priority then date
- [ ] `task search` matches goal, deliverables, criteria
- [ ] `task show` displays all task details
- [ ] `task status` shows blockers and children
- [ ] `task eval` evaluates criteria correctly
- [ ] `task graph` renders dependency tree
- [ ] `task graph` detects cycles

---

## Summary

The Task CLI provides a complete task management interface built on the Graph/TaskActor system. Key capabilities:

1. **Persistent storage** via tasks.json
2. **Hierarchical tasks** with parent/child relationships
3. **Dependency tracking** with automatic blocker detection
4. **Filtering and search** for task discovery
5. **Graph visualization** for dependency analysis
6. **Safety mechanisms** for destructive operations

The CLI serves as the human interface to the underlying actor system, enabling both interactive use and scripting for automated workflows.
