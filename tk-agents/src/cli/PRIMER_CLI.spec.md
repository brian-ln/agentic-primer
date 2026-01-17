# Primer CLI Specification

## Overview

The Primer CLI (`primer` or `ap`) is a unified command-line interface that routes commands to the three specialized sub-CLIs: Task, Graph, and Knowledge. It provides a single entry point with consistent namespacing and cross-subsystem functionality.

## Core Mission

Provide a unified interface to all tk-agents subsystems while maintaining clean separation of concerns. Enable users to work across tasks, graphs, and knowledge through a single command namespace with intelligent routing and integrated help systems.

## Architecture

```
+------------------+
|   Primer CLI     |  (Unified entry point)
|   primer/ap      |
+--------+---------+
         |
         | Routes to:
         |
    +----+----+----+
    |         |    |
    v         v    v
+-------+ +-------+ +-----------+
| Task  | | Graph | | Knowledge |
|  CLI  | |  CLI  | |    CLI    |
+-------+ +-------+ +-----------+
    |         |          |
    v         v          v
+-------+ +-------+ +-----------+
|tasks  | |graph  | |knowledge  |
|.json  | |.json  | |.json      |
+-------+ +-------+ +-----------+
```

### Component Responsibilities

- **Primer CLI**: Command routing, help integration, global flags
- **Task CLI**: Task management operations
- **Graph CLI**: Low-level graph manipulation
- **Knowledge CLI**: Knowledge management operations

## Command Format

### Basic Syntax

```bash
primer <subsystem> <command> [args...] [options]
```

**Subsystems:**
- `task` - Routes to Task CLI
- `graph` - Routes to Graph CLI
- `knowledge` (or `k`) - Routes to Knowledge CLI

**Aliases:**
- `primer` - Full command name
- `ap` - Short alias (agentic primer)

### Examples

```bash
# Task operations
primer task add "Implement feature X"
primer task list --status active
primer task update task_1 start

# Graph operations
primer graph create-node node_1 --type task
primer graph show-graph --format mermaid

# Knowledge operations
primer knowledge add "API Guidelines" --content "REST best practices"
primer k search "error handling"  # Using short alias

# Short form
ap task ready
ap k query knowledge_1 "What about auth?"
```

---

## Commands Reference

### Meta Commands

#### `primer help [subsystem]`

Shows help for Primer or specific subsystem.

**Arguments:**
- `[subsystem]`: Optional. Show help for specific subsystem (task, graph, knowledge)

**Output (no subsystem):**
```
Primer CLI - Unified interface for tk-agents

Usage: primer <subsystem> <command> [args...] [options]

Subsystems:
  task        Manage tasks with dependencies and priorities
  graph       Low-level graph manipulation
  knowledge   Manage versioned knowledge with querying

Global Options:
  --json      Output in JSON format (where supported)
  --yes       Auto-confirm all prompts

Examples:
  primer task add "My task" --priority P0
  primer graph list-nodes --type task
  primer knowledge search "API design"

Use 'primer help <subsystem>' for subsystem-specific commands.
Aliases: ap (short form)
```

**Output (with subsystem):**
```
Primer - Task Subsystem

Usage: primer task <command> [args...] [options]

Commands:
  init                      Create tasks.json
  add <goal> [options]      Add new task
  update <id> <action>      Update task state
  delete <id> [--force]     Delete task
  list [options]            List tasks with filters
  ready                     Show tasks with no blockers
  show <id>                 Show task details
  status <id>               Show task status
  eval <id>                 Evaluate task criteria
  graph <id>                Show dependency tree
  search <query>            Search tasks

Global Options:
  --json      Output in JSON format
  --yes       Auto-confirm all prompts

See full documentation: TASK_CLI.spec.md
```

---

#### `primer version`

Shows version information for Primer and all subsystems.

**Output:**
```
Primer CLI v1.0.0
├─ Task CLI v1.0.0
├─ Graph CLI v1.0.0
└─ Knowledge CLI v1.0.0

System: tk-agents
Node: v20.10.0
Bun: 1.0.20
```

---

### Routing Commands

All subsystem commands are routed transparently:

#### Task Commands (via `primer task`)

```bash
primer task init
primer task add "Goal" [options]
primer task update <id> <action>
primer task delete <id> [--force]
primer task list [options]
primer task ready
primer task show <id>
primer task status <id>
primer task eval <id>
primer task graph <id>
primer task search <query>
```

See TASK_CLI.spec.md for full details.

---

#### Graph Commands (via `primer graph`)

```bash
primer graph init
primer graph create-node <id> --type TYPE [--data JSON]
primer graph delete-node <id> [--force]
primer graph create-edge <from> <to> --type TYPE [--data JSON]
primer graph delete-edge <edgeId>
primer graph list-nodes [--type TYPE]
primer graph list-edges [--from ID] [--to ID] [--type TYPE]
primer graph show <id>
primer graph show-graph [--format FORMAT] [--root ID]
primer graph export [--output FILE]
primer graph import <file>
```

See GRAPH_CLI.spec.md for full details.

---

#### Knowledge Commands (via `primer knowledge` or `primer k`)

```bash
primer knowledge init
primer knowledge add <title> [--content TEXT] [--sources S1,S2]
primer knowledge get <id>
primer knowledge list [--limit N]
primer knowledge append <id> <content> [--source SRC]
primer knowledge update <id> [--title TEXT] [--content TEXT]
primer knowledge delete <id> [--force]
primer knowledge query <id> <question>
primer knowledge search <query>
primer knowledge link <kid> <nid> --type TYPE
primer knowledge unlink <kid> <nid>
primer knowledge synthesize <id> --from ID1,ID2
```

Short form with `k`:
```bash
primer k add "Title"
primer k search "query"
primer k query knowledge_1 "question"
```

See KNOWLEDGE_CLI.spec.md for full details.

---

## Global Flags

### `--json`

Outputs results in JSON format (where supported by subsystem).

**Applies to:**
- Task CLI: list, show, status, eval, search
- Graph CLI: list-nodes, list-edges, show, export
- Knowledge CLI: get, list, query, search

**Example:**
```bash
primer task list --json
primer knowledge search "API" --json
```

**Output:**
```json
{
  "command": "task list",
  "results": [
    {
      "id": "task_1",
      "goal": "Implement feature",
      "state": "active",
      "priority": 0
    }
  ],
  "count": 1
}
```

---

### `--yes`

Auto-confirms all prompts (useful for scripting).

**Applies to:**
- Task CLI: delete command
- Graph CLI: delete-node command
- Knowledge CLI: delete command

**Example:**
```bash
primer task delete task_1 --yes
primer graph delete-node node_1 --yes
primer knowledge delete knowledge_1 --yes
```

---

## Subsystem Aliases

| Full | Alias | Notes |
|------|-------|-------|
| `task` | - | No alias (short enough) |
| `graph` | - | No alias (short enough) |
| `knowledge` | `k` | Shortens long subsystem name |

**Example:**
```bash
# These are equivalent
primer knowledge add "Title"
primer k add "Title"
```

---

## Cross-Subsystem Workflows

### Task-Knowledge Integration

Create task with knowledge dependency:
```bash
# Create knowledge
primer k add "Authentication Guide" --content "Use OAuth 2.0"

# Create task that requires knowledge
primer task add "Implement auth" --depends task_0

# Link task to knowledge (via graph or knowledge CLI)
primer knowledge link knowledge_1 task_1 --type requires_knowledge

# Query knowledge from task context
primer k query knowledge_1 "What auth method?"
```

### Graph-Level Inspection

Inspect cross-subsystem graph structure:
```bash
# Show all nodes across subsystems
primer graph list-nodes

# Show specific task node
primer graph show task_1

# Visualize entire graph
primer graph show-graph --format mermaid

# Export combined graph
primer graph export --output full_graph.json
```

### Knowledge Synthesis Across Subsystems

```bash
# Create multiple knowledge nodes
primer k add "REST Guidelines"
primer k add "GraphQL Patterns"
primer k add "API Security"

# Synthesize into summary
primer k synthesize knowledge_summary --from knowledge_1,knowledge_2,knowledge_3
```

---

## Error Handling

### Command Routing Errors

**Unknown subsystem:**
```bash
primer unknown add "Test"
```
Output:
```
Error: Unknown subsystem "unknown"
Valid subsystems: task, graph, knowledge (k)

Use 'primer help' for usage information.
```

**Missing subsystem:**
```bash
primer
```
Output:
```
Error: No subsystem specified

Usage: primer <subsystem> <command> [args...]

Use 'primer help' for more information.
```

**Missing command:**
```bash
primer task
```
Output:
```
Error: No command specified for task subsystem

Usage: primer task <command> [args...]

Use 'primer help task' for available commands.
```

### Subsystem Errors

Errors from subsystem CLIs are passed through transparently:
```bash
primer task add
```
Output:
```
Error: Missing required argument: <goal>
Usage: primer task add <goal> [options]
```

---

## Environment Integration

### Executable Locations

```
src/cli/
├── primer.ts       # Main implementation
├── task.ts         # Task CLI (imported)
├── graph.ts        # Graph CLI (imported)
└── knowledge.ts    # Knowledge CLI (imported)
```

### Symbolic Links

```bash
# Create aliases
ln -s primer.ts ap.ts
```

### PATH Configuration

Add to PATH for global access:
```bash
export PATH="$PATH:/path/to/tk-agents/src/cli"
```

Usage:
```bash
primer task list
ap task ready
```

---

## Implementation Strategy

### Routing Logic

```typescript
function route(args: string[]) {
  const subsystem = args[0];  // task, graph, knowledge
  const command = args[1];    // add, list, create-node, etc.
  const subArgs = args.slice(2);

  // Extract global flags
  const globalFlags = extractGlobalFlags(subArgs);

  switch (subsystem) {
    case 'task':
      return executeTaskCLI(command, subArgs, globalFlags);
    case 'graph':
      return executeGraphCLI(command, subArgs, globalFlags);
    case 'knowledge':
    case 'k':
      return executeKnowledgeCLI(command, subArgs, globalFlags);
    default:
      throw new Error(`Unknown subsystem: ${subsystem}`);
  }
}
```

### Import Strategy

Primer imports the CLI modules directly:
```typescript
import * as TaskCLI from './task.ts';
import * as GraphCLI from './graph.ts';
import * as KnowledgeCLI from './knowledge.ts';
```

**Challenge:** Existing CLIs use `main()` with `process.argv` parsing.

**Solution Options:**
1. **Refactor CLIs**: Extract command handlers into exportable functions
2. **Process delegation**: Spawn child processes with modified argv
3. **Hybrid**: Direct function calls for common operations, delegation for complex

**Recommended:** Option 1 (refactor for clean integration)

---

## Help System Integration

### Hierarchical Help

```
primer help
├── primer help task
│   ├── Subsystem overview
│   ├── Command list
│   └── Link to TASK_CLI.spec.md
├── primer help graph
│   └── ...
└── primer help knowledge
    └── ...
```

### Dynamic Help Generation

- Parse subsystem CLI implementations
- Extract command signatures
- Generate formatted help text
- Include examples and links to specs

---

## Testing Strategy

### Unit Tests

```typescript
test('routes task commands correctly', () => {
  const result = route(['task', 'list', '--status', 'active']);
  expect(result.subsystem).toBe('task');
  expect(result.command).toBe('list');
});

test('handles knowledge alias', () => {
  const result = route(['k', 'search', 'query']);
  expect(result.subsystem).toBe('knowledge');
});

test('extracts global flags', () => {
  const result = route(['task', 'list', '--json', '--yes']);
  expect(result.globalFlags.json).toBe(true);
  expect(result.globalFlags.yes).toBe(true);
});
```

### Integration Tests

```bash
# Test basic routing
primer task init
primer task add "Test task"
primer task list

# Test cross-subsystem
primer k add "Test knowledge"
primer graph list-nodes

# Test global flags
primer task list --json
primer task delete task_1 --yes
```

---

## Performance Considerations

- **Startup time**: Minimal overhead for routing (< 10ms)
- **Process model**: Single process, direct function calls
- **Memory**: Load only required subsystem module
- **File I/O**: Same as direct subsystem CLI usage

---

## Future Extensions

### Command Aliases

```bash
# Add top-level aliases for common commands
primer add "Task"        # Defaults to task subsystem
primer search "query"    # Smart search across all subsystems
```

### Interactive Mode

```bash
primer shell
> task add "Goal"
> task list
> knowledge search "query"
> exit
```

### Configuration

```bash
# .primerrc or primer.config.json
{
  "defaultSubsystem": "task",
  "aliases": {
    "t": "task",
    "k": "knowledge",
    "g": "graph"
  },
  "globalFlags": {
    "json": false,
    "yes": false
  }
}
```

### Pipeline Support

```bash
# Pipe between subsystems
primer task list --status ready | primer task update --action start
primer knowledge search "API" | primer task add --from-search
```

---

## Verification Checklist

- [ ] `primer help` shows overview
- [ ] `primer help task` shows task commands
- [ ] `primer help graph` shows graph commands
- [ ] `primer help knowledge` shows knowledge commands
- [ ] `primer version` shows version info
- [ ] `primer task <cmd>` routes to task CLI
- [ ] `primer graph <cmd>` routes to graph CLI
- [ ] `primer knowledge <cmd>` routes to knowledge CLI
- [ ] `primer k <cmd>` aliases to knowledge CLI
- [ ] `ap` command works as alias
- [ ] `--json` flag passes through correctly
- [ ] `--yes` flag passes through correctly
- [ ] Unknown subsystem shows error
- [ ] Missing subsystem shows error
- [ ] Missing command shows error
- [ ] Subsystem errors pass through transparently

---

## Summary

The Primer CLI provides a unified interface to all tk-agents subsystems. Key features:

1. **Single entry point** - One command namespace for all operations
2. **Clean routing** - Transparent delegation to specialized CLIs
3. **Consistent interface** - Global flags work across all subsystems
4. **Extensible design** - Easy to add new subsystems or features
5. **Cross-subsystem support** - Facilitates integrated workflows

The design maintains separation of concerns while providing convenience and consistency for users working across the task, graph, and knowledge domains.
