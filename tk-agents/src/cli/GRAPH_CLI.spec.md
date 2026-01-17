# Graph CLI Specification

## Overview

The Graph CLI is a low-level command-line interface for direct graph manipulation. It provides raw access to the node-edge structure without high-level abstractions, enabling manual graph construction, inspection, and maintenance.

## Core Mission

Enable users to create, inspect, and manipulate graph structures at the primitive level - nodes and edges - with full control over node types, edge types, and associated data. This serves as the foundation layer for higher-level CLIs (Task, Knowledge) and for debugging graph structures.

## Architecture

```
+----------------+     +-----------+     +-------------+
|   Graph CLI    | --> |   Graph   | --> |  Dummy      |
| (Command Layer)|     | (Router)  |     |  Actors     |
|                |     |           |     | (Minimal)   |
+----------------+     +-----------+     +-------------+
        |                    ^
        v                    |
+----------------+           |
|  graph.json    |-----------+
| (Persistence)  |  load/save
+----------------+
```

### Component Responsibilities

- **Graph CLI**: Command parsing, validation, display formatting
- **Graph**: Node/edge storage, ID management, serialization
- **Dummy Actors**: Minimal actor stubs (no business logic)
- **graph.json**: Raw graph structure storage

### Design Philosophy

The Graph CLI operates at the **data structure level**, not the actor behavior level. It creates nodes with properties but no active behavior. This is intentional - it's for:
- Building graph structures manually
- Debugging graph state
- Importing/exporting graph data
- Testing graph topology

For behavioral operations, use Task CLI or Knowledge CLI.

## Commands Reference

### Initialization Command

#### `graph init`

Creates a new empty `graph.json` file.

**Preconditions:**
- `graph.json` must NOT exist in current directory

**Postconditions:**
- Creates `graph.json` with empty nodes and edges arrays

**Error Conditions:**
- File already exists: exits with code 1

**Output:**
```
Created graph.json
```

---

### Node Creation

#### `graph create-node <id> --type TYPE [--data JSON]`

Creates a new node in the graph.

**Arguments:**
- `<id>`: Required. Unique node identifier (free-form string).

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--type` | `task\|knowledge\|artifact\|pattern` | Node type (required) |
| `--data` | `JSON` | Additional properties as JSON object |

**Preconditions:**
- `graph.json` must exist
- Node ID must not already exist
- Type must be valid (task, knowledge, artifact, pattern)

**Postconditions:**
- New node created with specified ID and type
- Node properties include: `id`, `type`, `createdAt`, plus any additional data

**Output:**
```
Created node: my_node_1
Type: task
```

**Examples:**
```bash
graph create-node task_1 --type task
graph create-node doc_1 --type artifact --data '{"filename":"README.md"}'
```

---

#### `graph delete-node <id> [--force]`

Removes a node and all connected edges.

**Arguments:**
- `<id>`: Node ID to delete

**Options:**
- `--force`: Skip confirmation prompt

**Preconditions:**
- `graph.json` must exist
- Node must exist

**Safety Mechanism:**
- Without `--force`, displays node details and connected edges
- User must type "yes" to confirm

**Output:**
```
Node to delete: my_node
────────────────────────────────────────────────────────────────────────────────
Type: task
Created: 2026-01-16T19:00:00.000Z

Connected edges (2):
  edge_1: my_node -> other_node (depends_on)
  edge_2: parent_node -> my_node (spawned_by)

Delete this node? (yes/no): yes

Deleted node my_node
Removed 2 connected edge(s)
```

---

### Edge Creation

#### `graph create-edge <from> <to> --type TYPE [--data JSON]`

Creates an edge between two nodes.

**Arguments:**
- `<from>`: Source node ID
- `<to>`: Target node ID

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--type` | `EdgeType` | Edge type (required) |
| `--data` | `JSON` | Edge properties as JSON object |

**Valid Edge Types:**
- `depends_on`: Dependency relationship
- `requires_knowledge`: Knowledge prerequisite
- `produces`: Output/artifact production
- `spawned_by`: Parent-child relationship
- `blocks`: Blocking relationship
- `references`: General reference

**Preconditions:**
- `graph.json` must exist
- Both source and target nodes must exist
- Type must be valid

**Postconditions:**
- New edge created with auto-generated ID (`edge_N`)

**Output:**
```
Created edge: edge_5
From: task_1
To: task_2
Type: depends_on
```

**Examples:**
```bash
graph create-edge task_1 task_2 --type depends_on
graph create-edge task_3 knowledge_1 --type requires_knowledge
graph create-edge task_4 artifact_1 --type produces --data '{"format":"json"}'
```

---

#### `graph delete-edge <edgeId>`

Removes an edge from the graph.

**Arguments:**
- `<edgeId>`: Edge ID to delete (e.g., `edge_5`)

**Preconditions:**
- `graph.json` must exist
- Edge must exist

**Output:**
```
Deleted edge: edge_5 (task_1 -> task_2, depends_on)
```

---

### Listing and Inspection

#### `graph list-nodes [--type TYPE]`

Lists all nodes with optional type filter.

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--type` | `NodeType` | Filter by node type |

**Display Format:**
```
Nodes:
────────────────────────────────────────────────────────────────────────────────
ID                  Type           Created
────────────────────────────────────────────────────────────────────────────────
task_1              task           2026-01-16T19:00:00.000Z
knowledge_1         knowledge      2026-01-16T19:05:00.000Z
artifact_1          artifact       2026-01-16T19:10:00.000Z
────────────────────────────────────────────────────────────────────────────────
Total: 3 node(s)
```

**Examples:**
```bash
graph list-nodes              # All nodes
graph list-nodes --type task  # Only task nodes
```

---

#### `graph list-edges [--from ID] [--to ID] [--type TYPE]`

Lists edges with optional filters.

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--from` | `ID` | Filter by source node |
| `--to` | `ID` | Filter by target node |
| `--type` | `EdgeType` | Filter by edge type |

**Filter Logic:** AND semantics - all specified filters must match.

**Display Format:**
```
Edges:
────────────────────────────────────────────────────────────────────────────────
ID          From                To                  Type
────────────────────────────────────────────────────────────────────────────────
edge_1      task_1              task_2              depends_on
edge_2      task_1              knowledge_1         requires_knowledge
────────────────────────────────────────────────────────────────────────────────
Total: 2 edge(s)
```

**Examples:**
```bash
graph list-edges                        # All edges
graph list-edges --from task_1          # Outgoing from task_1
graph list-edges --type depends_on      # Only dependency edges
graph list-edges --from task_1 --type depends_on  # Specific combination
```

---

#### `graph show <id>`

Shows detailed information for a specific node.

**Arguments:**
- `<id>`: Node ID to inspect

**Display Sections:**
- Basic info: ID, Type, Created timestamp
- Additional properties (if any)
- Outgoing edges
- Incoming edges

**Output:**
```
Node: task_1
────────────────────────────────────────────────────────────────────────────────
ID:             task_1
Type:           task
Created:        2026-01-16T19:00:00.000Z

Properties:
  state: created
  goal: Implement authentication
  priority: 1

Outgoing Edges (2):
  edge_1 -> task_2 (depends_on)
  edge_3 -> knowledge_1 (requires_knowledge)

Incoming Edges (1):
  edge_5 <- task_0 (spawned_by)
```

---

### Graph Visualization

#### `graph show-graph [--format ascii|mermaid] [--root ID]`

Visualizes the graph structure.

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--format` | `ascii\|mermaid` | Output format (default: ascii) |
| `--root` | `ID` | Start from specific node |

**ASCII Format:**
Displays tree structure with box-drawing characters. Detects cycles.

**Output (ASCII):**
```
Graph:
────────────────────────────────────────────────────────────────────────────────
task_1 [task]
├── depends_on: task_2
│   └── depends_on: task_3
└── requires_knowledge: knowledge_1

task_0 [task]
└── spawned_by: task_1
    (cycle: task_1)
```

**Mermaid Format:**
Generates Mermaid diagram code for visualization tools.

**Output (Mermaid):**
```
Mermaid Diagram:
────────────────────────────────────────────────────────────────────────────────
graph TD
  task_1["task_1<br/>task"]
  task_2["task_2<br/>task"]
  knowledge_1["knowledge_1<br/>knowledge"]
  task_1 -->|depends_on| task_2
  task_1 -->|requires_knowledge| knowledge_1
────────────────────────────────────────────────────────────────────────────────
Copy the above to mermaid.live or GitHub markdown
```

---

### Import/Export

#### `graph export [--output FILE]`

Exports graph to JSON.

**Options:**
- `--output FILE`: Output file path (if omitted, prints to stdout)

**Output (with file):**
```
Exported graph to backup.json
Nodes: 10
Edges: 15
```

**Output (stdout):**
```json
{
  "nodes": [
    {
      "id": "task_1",
      "type": "task",
      "createdAt": "2026-01-16T19:00:00.000Z"
    }
  ],
  "edges": [
    {
      "id": "edge_1",
      "fromId": "task_1",
      "toId": "task_2",
      "type": "depends_on",
      "properties": {}
    }
  ]
}
```

---

#### `graph import <file>`

Imports nodes and edges from a JSON file.

**Arguments:**
- `<file>`: Path to JSON file

**Behavior:**
- Merges with existing graph if `graph.json` exists
- Creates new graph if `graph.json` doesn't exist
- Skips nodes with duplicate IDs
- Creates new edge IDs to avoid conflicts

**Preconditions:**
- Import file must exist
- Import file must be valid JSON
- Import file must have `nodes` and `edges` arrays

**Output:**
```
Imported graph from backup.json
Added 8 node(s)
Added 12 edge(s)
```

---

## File Format

### graph.json Structure

```typescript
interface GraphFile {
  nodes: NodeProperties[];  // Array of node property objects
  edges: Edge[];            // Array of edge objects
}
```

### Node Properties Schema

```typescript
interface NodeProperties {
  id: string;                    // Unique node identifier
  type: NodeType;                // task | knowledge | artifact | pattern
  createdAt: Date | string;      // ISO 8601 timestamp
  [key: string]: unknown;        // Additional properties
}
```

### Edge Schema

```typescript
interface Edge {
  id: string;                    // "edge_N" format
  fromId: string;                // Source node ID
  toId: string;                  // Target node ID
  type: EdgeType;                // Edge type enum
  properties: Record<string, unknown>;
}

type EdgeType =
  | "depends_on"
  | "requires_knowledge"
  | "produces"
  | "spawned_by"
  | "blocks"
  | "references";
```

---

## Node Types

| Type | Purpose | Typical Use |
|------|---------|-------------|
| `task` | Work items | Task tracking, workflow |
| `knowledge` | Information nodes | Documentation, facts |
| `artifact` | Output products | Files, deliverables |
| `pattern` | Reusable patterns | Templates, conventions |

---

## Edge Types

| Type | Meaning | Direction |
|------|---------|-----------|
| `depends_on` | B depends on A | A → B (B requires A first) |
| `requires_knowledge` | Needs knowledge | Task → Knowledge |
| `produces` | Creates artifact | Task → Artifact |
| `spawned_by` | Child of parent | Child → Parent |
| `blocks` | Blocks progress | Blocker → Blocked |
| `references` | General reference | Any → Any |

---

## Error Handling

### Common Error Conditions

| Error | Cause | Exit Code |
|-------|-------|-----------|
| `Graph file not found` | Running commands before `graph init` | 1 |
| `graph.json already exists` | Running `graph init` twice | 1 |
| `Node not found: <id>` | Invalid node ID | 1 |
| `Node "<id>" already exists` | Duplicate node ID on create | 1 |
| `Invalid type "<type>"` | Unknown node/edge type | 1 |
| `Invalid JSON in --data` | Malformed JSON in --data flag | 1 |

### Error Output Format

```
Error: <message>
```

---

## Performance Characteristics

- **File I/O**: Synchronous read/write operations
- **Node lookup**: O(n) scan through node array
- **Edge filtering**: O(e) scan through edge array
- **Memory**: Full graph loaded into memory during command

---

## Integration Points

### With Graph System

- CLI creates Graph instance per command
- Uses dummy actors (no message handling)
- Uses `graph.registerNode()` for node storage
- Uses `graph.addEdge()` / `graph.removeEdge()` for edge management
- Uses `graph.dump()` for serialization

### With File System

- Uses Node.js `fs` module (readFileSync, writeFileSync, existsSync)
- JSON serialization with Date-to-ISO transform

### With Higher-Level CLIs

- Task CLI and Knowledge CLI build on this structure
- They create nodes with active actors (behavioral)
- Graph CLI can inspect their output files

---

## Command Composition Rules

### Prerequisite Chain

```
init -> create-node -> create-edge
            ↓             ↓
        show, delete  delete-edge
            ↓
    list-nodes, list-edges, show-graph, export, import
```

### Safe Command Sequences

```bash
# Basic workflow
graph init
graph create-node task_1 --type task
graph create-node task_2 --type task
graph create-edge task_1 task_2 --type depends_on
graph list-nodes
graph show-graph
graph export --output backup.json

# Inspection workflow
graph list-edges --from task_1
graph show task_1
graph show-graph --format mermaid --root task_1

# Cleanup
graph delete-edge edge_1
graph delete-node task_2 --force
```

---

## Verification Checklist

- [ ] `graph init` creates valid graph.json
- [ ] `graph create-node` requires --type
- [ ] `graph create-node` validates node types
- [ ] `graph create-node` prevents duplicate IDs
- [ ] `graph create-node --data` parses JSON correctly
- [ ] `graph delete-node` prompts for confirmation
- [ ] `graph delete-node --force` skips confirmation
- [ ] `graph delete-node` removes connected edges
- [ ] `graph create-edge` requires --type
- [ ] `graph create-edge` validates both nodes exist
- [ ] `graph create-edge` validates edge types
- [ ] `graph delete-edge` removes edge successfully
- [ ] `graph list-nodes` displays all nodes
- [ ] `graph list-nodes --type` filters correctly
- [ ] `graph list-edges` displays all edges
- [ ] `graph list-edges` filters by from/to/type
- [ ] `graph show` displays node details and edges
- [ ] `graph show-graph` renders ASCII tree
- [ ] `graph show-graph` detects cycles
- [ ] `graph show-graph --format mermaid` generates diagram
- [ ] `graph export` outputs valid JSON
- [ ] `graph export --output` writes to file
- [ ] `graph import` merges with existing graph
- [ ] `graph import` skips duplicate nodes
- [ ] `graph import` validates JSON structure

---

## Summary

The Graph CLI provides low-level graph manipulation capabilities. Key characteristics:

1. **Raw data access** - operates at structure level, not behavior level
2. **Manual control** - full control over node/edge creation
3. **Debugging tool** - inspect graph topology and state
4. **Import/export** - move graphs between systems
5. **Foundation layer** - basis for Task CLI and Knowledge CLI

The CLI is intentionally minimal - it's for graph construction and inspection, not for executing workflows or processing data. For behavioral operations, use the higher-level CLIs.
