# CLI Implementation Plan

**Date**: 2026-01-16
**Status**: Planning Phase
**Target Completion**: 2026-01-16 (same day)

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture Decision](#architecture-decision)
3. [Graph CLI Design](#graph-cli-design)
4. [Knowledge CLI Design](#knowledge-cli-design)
5. [Implementation Phases](#implementation-phases)
6. [Testing Strategy](#testing-strategy)
7. [Success Criteria](#success-criteria)

---

## Overview

### Goals
1. Implement Graph CLI for low-level graph operations
2. Implement Knowledge CLI for knowledge management
3. Maintain consistency with existing Task CLI pattern
4. Ensure CLIs integrate properly with actor/graph systems

### Constraints
- Must follow task.ts architectural pattern
- Use Bun runtime (#!/usr/bin/env bun)
- Manual argument parsing (no external CLI libraries)
- JSON file persistence per CLI
- Must integrate with existing Graph and Actor systems

### Non-Goals
- Replacing Task CLI
- Implementing artifact or pattern CLIs
- Building a unified graph.json (separate files for now)
- Complex query languages
- Full-text search or semantic search

---

## Architecture Decision

### Issue: Knowledge System Architecture Mismatch

**Problem**: knowledge.ts uses old class-based NodeActor pattern, incompatible with current Graph.registerNode() API that expects Address objects.

**Evidence**:
```typescript
// Current knowledge.ts (OLD pattern)
class KnowledgeNode implements NodeActor {
  handleMessage(message: Message, graph: Graph): unknown { ... }
}
graph.registerNode(knowledge); // Wrong signature

// Current Graph.ts (NEW pattern)
registerNode(id: string, address: Address, properties: NodeProperties): void

// Working task.ts (NEW pattern)
const TaskActor: ActorFactory<TaskActorData> = (data) => {
  const actor = { send: async (message) => {...} };
  const address = data.graph.getSystem().register(actor);
  data.graph.registerNode(id, address, properties);
  return address;
};
```

### Decision: Option A - Refactor Knowledge System

**Rationale**:
- Ensures long-term consistency across all node types
- Makes future node types (artifact, pattern) follow same pattern
- Simplifies CLI implementation (uniform interface)
- Better aligns with specifications

**Implementation**:
1. Create KnowledgeActor factory matching TaskActor pattern
2. Convert class methods to closure-based message handler
3. Return Address from factory
4. Update createKnowledge() helper to use new pattern
5. Preserve all existing functionality

**Impact**: Medium (requires touching knowledge.ts, but preserves API surface)

---

## Graph CLI Design

### File: `src/cli/graph.ts`

### Purpose
Low-level graph manipulation tool for:
- Creating and deleting nodes of any type
- Managing edges between nodes
- Querying graph structure
- Visualizing graph topology
- Importing/exporting graph data

### Data Storage
- **File**: `graph.json`
- **Format**: Same as Graph.dump() output
  ```json
  {
    "nodes": [
      { "id": "node_1", "type": "task", "createdAt": "...", ... }
    ],
    "edges": [
      { "id": "edge_1", "fromId": "node_1", "toId": "node_2", "type": "depends_on", "properties": {} }
    ]
  }
  ```

### Command Structure

```
graph <command> [args] [flags]
```

### Commands Specification

#### 1. `graph init`
**Purpose**: Initialize empty graph.json

**Usage**: `graph init`

**Behavior**:
- Create graph.json with empty nodes/edges arrays
- Fail if graph.json already exists
- Success message with file location

**Output**:
```
Created graph.json
```

---

#### 2. `graph create-node <id> --type <type> [--data <json>]`
**Purpose**: Create a generic node

**Usage**:
```
graph create-node node_1 --type task --data '{"goal":"test"}'
graph create-node kb_1 --type knowledge
```

**Arguments**:
- `<id>` - Required. Node identifier (string)
- `--type <type>` - Required. Node type (task|knowledge|artifact|pattern)
- `--data <json>` - Optional. JSON string of additional properties

**Behavior**:
- Create minimal node with id, type, createdAt
- Merge --data JSON into properties
- Register in graph (no actor needed for generic nodes)
- Save to graph.json

**Validation**:
- ID must be unique
- Type must be valid NodeType
- JSON must parse successfully

**Output**:
```
Created node: node_1
Type: task
```

**Note**: This creates minimal nodes, not full semantic nodes (e.g., won't have TaskActor behavior). For full task/knowledge nodes, use respective CLIs.

---

#### 3. `graph delete-node <id> [--force]`
**Purpose**: Remove node and all connected edges

**Usage**:
```
graph delete-node node_1
graph delete-node node_1 --force
```

**Arguments**:
- `<id>` - Required. Node identifier
- `--force` - Optional. Skip confirmation

**Behavior**:
- Show node details and connected edges
- Prompt for confirmation (unless --force)
- Remove node and all edges (cascade)
- Save to graph.json

**Output**:
```
Node to delete: node_1
Type: task
Connected edges: 2

Delete this node? (yes/no): yes

Deleted node node_1
Removed 2 connected edges
```

---

#### 4. `graph create-edge <from> <to> --type <type> [--data <json>]`
**Purpose**: Create edge between nodes

**Usage**:
```
graph create-edge task_1 task_2 --type depends_on
graph create-edge task_1 kb_1 --type requires_knowledge --data '{"reason":"needs info"}'
```

**Arguments**:
- `<from>` - Required. Source node ID
- `<to>` - Required. Target node ID
- `--type <type>` - Required. Edge type (depends_on|requires_knowledge|produces|spawned_by|blocks|references)
- `--data <json>` - Optional. Edge properties as JSON

**Behavior**:
- Validate both nodes exist
- Create edge with auto-generated ID (edge_N)
- Add optional properties from --data
- Save to graph.json

**Validation**:
- Both nodes must exist in graph
- Type must be valid EdgeType

**Output**:
```
Created edge: edge_5
From: task_1
To: task_2
Type: depends_on
```

---

#### 5. `graph delete-edge <edgeId>`
**Purpose**: Remove specific edge

**Usage**: `graph delete-edge edge_5`

**Arguments**:
- `<edgeId>` - Required. Edge identifier (e.g., edge_1)

**Behavior**:
- Find edge by ID
- Remove from graph
- Save to graph.json

**Output**:
```
Deleted edge: edge_5 (task_1 -> task_2, depends_on)
```

---

#### 6. `graph list-nodes [--type <type>]`
**Purpose**: List all nodes with optional filter

**Usage**:
```
graph list-nodes
graph list-nodes --type task
graph list-nodes --type knowledge
```

**Arguments**:
- `--type <type>` - Optional. Filter by node type

**Behavior**:
- Load graph
- Filter nodes by type (if specified)
- Display in table format

**Output**:
```
Nodes:
────────────────────────────────────────
ID              Type         Created
────────────────────────────────────────
task_1          task         2026-01-16T18:00:00Z
task_2          task         2026-01-16T18:05:00Z
knowledge_1     knowledge    2026-01-16T18:10:00Z
────────────────────────────────────────
Total: 3 nodes
```

---

#### 7. `graph list-edges [--from <id>] [--to <id>] [--type <type>]`
**Purpose**: List edges with optional filters

**Usage**:
```
graph list-edges
graph list-edges --from task_1
graph list-edges --to task_2
graph list-edges --type depends_on
```

**Arguments**:
- `--from <id>` - Optional. Filter by source node
- `--to <id>` - Optional. Filter by target node
- `--type <type>` - Optional. Filter by edge type

**Behavior**:
- Load graph
- Apply filters (AND logic)
- Display edges in table format

**Output**:
```
Edges:
────────────────────────────────────────────────────────
ID       From       To         Type
────────────────────────────────────────────────────────
edge_1   task_1     task_2     depends_on
edge_2   task_1     kb_1       requires_knowledge
edge_3   task_2     task_3     spawned_by
────────────────────────────────────────────────────────
Total: 3 edges
```

---

#### 8. `graph show <id>`
**Purpose**: Show detailed node information with edges

**Usage**: `graph show task_1`

**Arguments**:
- `<id>` - Required. Node identifier

**Behavior**:
- Load graph
- Get node properties
- Get all connected edges (from + to)
- Display detailed view

**Output**:
```
Node: task_1
────────────────────────────────────────
ID:             task_1
Type:           task
Created:        2026-01-16T18:00:00Z

Properties:
  goal: Complete feature X
  state: active
  priority: 0

Outgoing Edges (2):
  edge_1 -> task_2 (depends_on)
  edge_2 -> kb_1 (requires_knowledge)

Incoming Edges (1):
  edge_3 <- task_0 (spawned_by)
```

---

#### 9. `graph show-graph [--format ascii] [--root <id>]`
**Purpose**: Visualize graph structure

**Usage**:
```
graph show-graph
graph show-graph --format ascii
graph show-graph --root task_1
```

**Arguments**:
- `--format <format>` - Optional. Output format (ascii|mermaid) [default: ascii]
- `--root <id>` - Optional. Start from specific node

**Behavior**:
- Load graph
- Build ASCII tree representation (or Mermaid syntax)
- Show relationships visually

**Output (ASCII)**:
```
Graph:
────────────────────────────────────────
task_0 [task]
├── spawned_by: task_1 [task]
│   ├── depends_on: task_2 [task]
│   └── requires_knowledge: kb_1 [knowledge]
└── spawned_by: task_3 [task]
    └── produces: artifact_1 [artifact]
```

**Output (Mermaid)**:
```
graph TD
  task_0[task_0<br/>task]
  task_1[task_1<br/>task]
  task_2[task_2<br/>task]
  kb_1[kb_1<br/>knowledge]
  task_0 -->|spawned_by| task_1
  task_1 -->|depends_on| task_2
  task_1 -->|requires_knowledge| kb_1
```

---

#### 10. `graph export [--output <file>]`
**Purpose**: Export graph to JSON

**Usage**:
```
graph export
graph export --output backup.json
```

**Arguments**:
- `--output <file>` - Optional. Output file [default: stdout]

**Behavior**:
- Load graph.json
- Pretty-print JSON
- Output to file or stdout

**Output**:
```
Exported graph to backup.json
Nodes: 5
Edges: 8
```

---

#### 11. `graph import <file>`
**Purpose**: Import graph from JSON

**Usage**: `graph import backup.json`

**Arguments**:
- `<file>` - Required. JSON file to import

**Behavior**:
- Load JSON file
- Validate format (nodes/edges arrays)
- Merge with existing graph (or replace)
- Save to graph.json

**Validation**:
- Must have nodes and edges arrays
- Node IDs should be unique (warn on collision)
- Edge IDs recalculated to avoid conflicts

**Output**:
```
Imported graph from backup.json
Added 5 nodes
Added 8 edges
```

---

### Help Output

```
Graph CLI - Low-level graph manipulation

Commands:
  graph init                                    Create empty graph.json
  graph create-node <id> --type TYPE [--data JSON]
                                                Create a node
  graph delete-node <id> [--force]              Delete node and edges
  graph create-edge <from> <to> --type TYPE [--data JSON]
                                                Create edge
  graph delete-edge <edgeId>                    Delete edge
  graph list-nodes [--type TYPE]                List nodes
  graph list-edges [--from ID] [--to ID] [--type TYPE]
                                                List edges
  graph show <id>                               Show node details
  graph show-graph [--format ascii|mermaid] [--root ID]
                                                Visualize graph
  graph export [--output FILE]                  Export to JSON
  graph import <file>                           Import from JSON
```

---

## Knowledge CLI Design

### File: `src/cli/knowledge.ts`

### Purpose
High-level knowledge management tool for:
- Creating and managing knowledge nodes
- Querying knowledge base
- Linking knowledge to other nodes
- Versioning and source tracking
- Synthesizing knowledge from multiple sources

### Data Storage
- **File**: `knowledge.json`
- **Format**: Graph dump format (nodes + edges)
- **Node Type**: All nodes are type "knowledge"

### Command Structure

```
knowledge <command> [args] [flags]
```

### Commands Specification

#### 1. `knowledge init`
**Purpose**: Initialize knowledge.json

**Usage**: `knowledge init`

**Behavior**:
- Create knowledge.json with empty graph
- Create example knowledge node
- Success message

**Output**:
```
Created knowledge.json with example knowledge
Knowledge ID: knowledge_1
```

---

#### 2. `knowledge add <title> [--content <text>] [--sources <s1,s2>]`
**Purpose**: Create new knowledge node

**Usage**:
```
knowledge add "Actor Model Basics"
knowledge add "Message Passing" --content "Actors communicate via messages"
knowledge add "Erlang Processes" --content "..." --sources "Armstrong 2003,Agha 1986"
```

**Arguments**:
- `<title>` - Required. Knowledge title
- `--content <text>` - Optional. Initial content
- `--sources <list>` - Optional. Comma-separated sources

**Behavior**:
- Create KnowledgeNode via KnowledgeActor factory
- Set title, content, sources
- Version starts at 1
- Save to knowledge.json

**Output**:
```
Added knowledge: knowledge_5
Title: Actor Model Basics
Version: 1
```

---

#### 3. `knowledge get <id>`
**Purpose**: Show knowledge details

**Usage**: `knowledge get knowledge_1`

**Arguments**:
- `<id>` - Required. Knowledge node ID

**Behavior**:
- Load graph
- Send "get" message to knowledge node
- Display full details

**Output**:
```
Knowledge: knowledge_1
────────────────────────────────────────
Title:          Actor Model Basics
Version:        3
Created:        2026-01-16T18:00:00Z
Content Length: 450 characters
Sources:        2

Content:
Actors are the fundamental units of concurrent computation.
Each actor has a mailbox for receiving messages.
[... full content ...]

Sources:
  - Hewitt 1973
  - Agha 1986

Connected Nodes (2):
  task_1 (requires_knowledge)
  knowledge_2 (references)
```

---

#### 4. `knowledge list [--limit <n>]`
**Purpose**: List all knowledge nodes

**Usage**:
```
knowledge list
knowledge list --limit 10
```

**Arguments**:
- `--limit <n>` - Optional. Maximum results [default: unlimited]

**Behavior**:
- Load graph
- Filter for knowledge nodes
- Sort by creation date (newest first)
- Display table

**Output**:
```
Knowledge Nodes:
────────────────────────────────────────────────────────────────────
ID            Title                      Version  Created
────────────────────────────────────────────────────────────────────
knowledge_3   Actor Model Basics         3        2026-01-16T18:00:00Z
knowledge_2   Message Passing            1        2026-01-16T17:55:00Z
knowledge_1   Erlang Processes           2        2026-01-16T17:50:00Z
────────────────────────────────────────────────────────────────────
Total: 3 knowledge nodes
```

---

#### 5. `knowledge append <id> <content> [--source <src>]`
**Purpose**: Append content to existing knowledge

**Usage**:
```
knowledge append knowledge_1 "Additional information..."
knowledge append knowledge_1 "New findings..." --source "Smith 2024"
```

**Arguments**:
- `<id>` - Required. Knowledge node ID
- `<content>` - Required. Content to append
- `--source <src>` - Optional. Source attribution

**Behavior**:
- Load graph
- Send "append" message with data and source
- Version increments automatically
- Save graph

**Output**:
```
Appended to knowledge_1
New version: 4
Source added: Smith 2024
```

---

#### 6. `knowledge update <id> [--title <text>] [--content <text>]`
**Purpose**: Update knowledge properties

**Usage**:
```
knowledge update knowledge_1 --title "New Title"
knowledge update knowledge_1 --content "Completely new content"
```

**Arguments**:
- `<id>` - Required. Knowledge node ID
- `--title <text>` - Optional. New title
- `--content <text>` - Optional. Replace content (resets version)

**Behavior**:
- Load graph
- Send "update" message with properties
- Content replacement doesn't increment version (direct update)
- Save graph

**Warning**: `--content` replaces, not appends. Use `append` for versioned additions.

**Output**:
```
Updated knowledge_1
Title: New Title
```

---

#### 7. `knowledge delete <id> [--force]`
**Purpose**: Remove knowledge node

**Usage**:
```
knowledge delete knowledge_1
knowledge delete knowledge_1 --force
```

**Arguments**:
- `<id>` - Required. Knowledge node ID
- `--force` - Optional. Skip confirmation

**Behavior**:
- Show knowledge details and connections
- Prompt for confirmation (unless --force)
- Remove node and edges
- Save graph

**Output**:
```
Knowledge to delete: knowledge_1
Title: Actor Model Basics
Connected to 2 nodes

Delete this knowledge? (yes/no): yes

Deleted knowledge_1
```

---

#### 8. `knowledge query <id> <question>`
**Purpose**: Query knowledge node

**Usage**: `knowledge query knowledge_1 "What is an actor?"`

**Arguments**:
- `<id>` - Required. Knowledge node ID
- `<question>` - Required. Query string

**Behavior**:
- Load graph
- Send "query" message with question
- Display answer with confidence and sources

**Output**:
```
Query: "What is an actor?"
Knowledge: knowledge_1

Answer:
Actors are the fundamental units of concurrent computation.

Confidence: 0.85
Sources:
  - Hewitt 1973
  - Agha 1986
```

---

#### 9. `knowledge search <query>`
**Purpose**: Search all knowledge nodes

**Usage**: `knowledge search "message passing"`

**Arguments**:
- `<query>` - Required. Search query

**Behavior**:
- Load graph
- Query each knowledge node
- Collect results with confidence scores
- Sort by confidence (descending)
- Display top results

**Output**:
```
Search: "message passing"

Results:
────────────────────────────────────────────────────────────────────
ID            Title                      Confidence  Snippet
────────────────────────────────────────────────────────────────────
knowledge_2   Message Passing            0.95        Actors communicate via...
knowledge_1   Actor Model Basics         0.60        ...each actor has a mailbox...
────────────────────────────────────────────────────────────────────
Found 2 matching knowledge nodes
```

---

#### 10. `knowledge link <knowledge-id> <node-id> --type <type>`
**Purpose**: Link knowledge to another node

**Usage**:
```
knowledge link knowledge_1 task_1 --type requires_knowledge
knowledge link knowledge_1 knowledge_2 --type references
```

**Arguments**:
- `<knowledge-id>` - Required. Source knowledge node
- `<node-id>` - Required. Target node (any type)
- `--type <type>` - Required. Edge type

**Behavior**:
- Load graph
- Validate both nodes exist
- Create edge
- Save graph

**Output**:
```
Linked knowledge_1 to task_1
Edge type: requires_knowledge
Edge ID: edge_10
```

---

#### 11. `knowledge unlink <knowledge-id> <node-id>`
**Purpose**: Remove link between nodes

**Usage**: `knowledge unlink knowledge_1 task_1`

**Arguments**:
- `<knowledge-id>` - Required. Source knowledge node
- `<node-id>` - Required. Target node

**Behavior**:
- Load graph
- Find edges between nodes
- Remove all matching edges
- Save graph

**Output**:
```
Unlinked knowledge_1 from task_1
Removed 1 edge
```

---

#### 12. `knowledge synthesize <id> --from <id1,id2,...>`
**Purpose**: Synthesize knowledge from multiple sources

**Usage**: `knowledge synthesize knowledge_1 --from knowledge_2,knowledge_3`

**Arguments**:
- `<id>` - Required. Target knowledge node (will receive synthesis)
- `--from <ids>` - Required. Comma-separated source node IDs

**Behavior**:
- Load graph
- Send "synthesize" message to target node
- Combine content from source nodes
- Display synthesis result

**Output**:
```
Synthesized from 2 sources
Target: knowledge_1
Sources: knowledge_2, knowledge_3

Synthesis (500 characters):
[Source 1]
Content from knowledge_2...

---

[Source 2]
Content from knowledge_3...

Combined sources:
  - Hewitt 1973
  - Agha 1986
  - Armstrong 2003
```

---

### Help Output

```
Knowledge CLI - Manage knowledge nodes

Commands:
  knowledge init                            Create knowledge.json
  knowledge add <title> [--content TEXT] [--sources S1,S2]
                                            Create knowledge node
  knowledge get <id>                        Show knowledge details
  knowledge list [--limit N]                List knowledge nodes
  knowledge append <id> <content> [--source SRC]
                                            Append to knowledge
  knowledge update <id> [--title TEXT] [--content TEXT]
                                            Update knowledge properties
  knowledge delete <id> [--force]           Delete knowledge node
  knowledge query <id> <question>           Query knowledge
  knowledge search <query>                  Search all knowledge
  knowledge link <kid> <nid> --type TYPE    Link to node
  knowledge unlink <kid> <nid>              Unlink from node
  knowledge synthesize <id> --from ID1,ID2  Synthesize from sources
```

---

## Implementation Phases

### Phase 0: Fix Knowledge System Architecture (30-45 min)

**Goal**: Refactor knowledge.ts to use Address-based pattern like task.ts

**Tasks**:
1. Create KnowledgeActor factory function
2. Convert KnowledgeNode class to closure-based pattern
3. Update handleMessage to async function
4. Register with System and return Address
5. Update createKnowledge() helper
6. Verify integration with Graph

**Files Modified**:
- `src/knowledge.ts` (major refactor)

**Testing**:
- Create knowledge node
- Send messages (get, append, query)
- Verify registration in graph
- Check serialization/deserialization

---

### Phase 1: Implement Graph CLI (90-120 min)

**Goal**: Create working graph.ts CLI with all commands

**Tasks**:
1. Create file structure and shebang
2. Implement loadGraph() and saveGraph() helpers
3. Implement init command
4. Implement node commands (create-node, delete-node, list-nodes, show)
5. Implement edge commands (create-edge, delete-edge, list-edges)
6. Implement visualization (show-graph)
7. Implement import/export
8. Add help documentation
9. Make executable (chmod +x)

**Files Created**:
- `src/cli/graph.ts`

**Testing**:
- `graph init`
- Create nodes and edges
- List and show operations
- Visualization output
- Import/export cycle

---

### Phase 2: Implement Knowledge CLI (90-120 min)

**Goal**: Create working knowledge.ts CLI with all commands

**Tasks**:
1. Create file structure and shebang
2. Implement loadGraph() and saveGraph() helpers (reuse pattern)
3. Implement init command
4. Implement CRUD commands (add, get, list, update, delete)
5. Implement knowledge operations (append, query, search)
6. Implement linking (link, unlink)
7. Implement synthesize
8. Add help documentation
9. Make executable (chmod +x)

**Files Created**:
- `src/cli/knowledge.ts`

**Testing**:
- `knowledge init`
- Create and manage knowledge
- Append with versioning
- Query and search
- Link to other nodes
- Synthesize from multiple sources

---

### Phase 3: Integration Testing (30 min)

**Goal**: Verify CLIs work together and with existing Task CLI

**Test Scenarios**:
1. Create task in Task CLI, link knowledge from Knowledge CLI
2. Create nodes in Graph CLI, import into Task/Knowledge CLI
3. Export from Task CLI, manipulate in Graph CLI, reimport
4. Cross-CLI edge creation and querying
5. Unified graph operations

**Files Created**:
- `tests/cli-integration.test.ts` (optional)

---

## Testing Strategy

### Unit Testing Approach

**Per-CLI Tests**:

**Graph CLI Tests**:
```typescript
// tests/graph-cli.test.ts
test("graph init creates graph.json", async () => {
  // Run: graph init
  // Assert: graph.json exists with correct structure
});

test("graph create-node adds node", async () => {
  // Run: graph create-node test_1 --type task
  // Assert: node exists in graph.json
});

test("graph create-edge links nodes", async () => {
  // Run: graph create-edge n1 n2 --type depends_on
  // Assert: edge exists in graph.json
});

test("graph show-graph displays ASCII", async () => {
  // Setup: Create graph with nodes/edges
  // Run: graph show-graph
  // Assert: Output contains ASCII tree
});
```

**Knowledge CLI Tests**:
```typescript
// tests/knowledge-cli.test.ts
test("knowledge add creates node", async () => {
  // Run: knowledge add "Test" --content "Content"
  // Assert: knowledge node in file
});

test("knowledge append increments version", async () => {
  // Setup: Create knowledge
  // Run: knowledge append <id> "More content"
  // Assert: Version = 2
});

test("knowledge query returns results", async () => {
  // Setup: Create knowledge with content
  // Run: knowledge query <id> "question"
  // Assert: Output contains answer and confidence
});

test("knowledge search finds matches", async () => {
  // Setup: Create multiple knowledge nodes
  // Run: knowledge search "keyword"
  // Assert: Results ranked by confidence
});
```

### Integration Testing

**Cross-CLI Tests**:
```typescript
test("Task CLI can link to Knowledge CLI nodes", async () => {
  // Setup: knowledge add, task add
  // Run: Task CLI link command (if exists) or Graph CLI
  // Assert: Edge exists, both nodes intact
});

test("Graph CLI can manipulate Task/Knowledge nodes", async () => {
  // Setup: Create task and knowledge via CLIs
  // Run: graph show, graph create-edge
  // Assert: Graph operations work on all node types
});
```

### Manual Testing Checklist

- [ ] Graph init creates valid file
- [ ] Graph create-node with all types
- [ ] Graph edge operations work correctly
- [ ] Graph show-graph produces readable output
- [ ] Graph export/import preserves data
- [ ] Knowledge init creates valid file
- [ ] Knowledge add/append/update work correctly
- [ ] Knowledge query returns sensible results
- [ ] Knowledge search ranks by relevance
- [ ] Knowledge link creates edges correctly
- [ ] Knowledge synthesize combines sources
- [ ] Help messages are clear and complete
- [ ] Error messages are informative
- [ ] Files are executable (chmod +x)

---

## Success Criteria

### Functional Requirements

**Graph CLI**:
- ✅ Can create and delete nodes of any type
- ✅ Can create and delete edges with types
- ✅ Can list nodes and edges with filters
- ✅ Can show detailed node information
- ✅ Can visualize graph structure (ASCII)
- ✅ Can import and export graph data
- ✅ All commands have --help documentation
- ✅ Executable with #!/usr/bin/env bun

**Knowledge CLI**:
- ✅ Can create and manage knowledge nodes
- ✅ Can append content with version tracking
- ✅ Can query individual knowledge nodes
- ✅ Can search across all knowledge
- ✅ Can link knowledge to other nodes
- ✅ Can synthesize from multiple sources
- ✅ All commands have --help documentation
- ✅ Executable with #!/usr/bin/env bun

### Non-Functional Requirements

**Code Quality**:
- ✅ Follows task.ts architectural pattern
- ✅ TypeScript with proper types
- ✅ Clear error messages
- ✅ Consistent command structure
- ✅ DRY principles (shared helpers)

**Integration**:
- ✅ Works with existing Graph system
- ✅ Works with existing Actor system
- ✅ Compatible with Task CLI
- ✅ JSON serialization compatible

**Documentation**:
- ✅ Help messages for all commands
- ✅ Examples in documentation
- ✅ Clear usage patterns

### Acceptance Criteria

**Scenario 1: Create Graph and Knowledge**
```bash
# Setup
graph init
knowledge init

# Create nodes
graph create-node n1 --type task --data '{"goal":"test"}'
knowledge add "Actor Basics" --content "Actors are..."

# Link them
graph create-edge n1 knowledge_1 --type requires_knowledge

# Verify
graph show n1
knowledge get knowledge_1
```

**Scenario 2: Knowledge Management Workflow**
```bash
# Create knowledge
knowledge add "Message Passing" --content "Initial content"

# Build it up
knowledge append knowledge_1 "More details..." --source "Smith 2024"
knowledge append knowledge_1 "Additional findings..." --source "Jones 2025"

# Query it
knowledge query knowledge_1 "What is message passing?"

# Link to task
knowledge link knowledge_1 task_1 --type requires_knowledge
```

**Scenario 3: Graph Visualization**
```bash
# Setup complex graph
graph init
graph create-node root --type task
graph create-node t1 --type task
graph create-node t2 --type task
graph create-node kb1 --type knowledge

# Create relationships
graph create-edge t1 root --type spawned_by
graph create-edge t2 root --type spawned_by
graph create-edge t1 kb1 --type requires_knowledge

# Visualize
graph show-graph --root root
```

---

## Implementation Order

1. **Phase 0**: Fix knowledge.ts architecture (30-45 min)
2. **Phase 1**: Implement Graph CLI (90-120 min)
3. **Phase 2**: Implement Knowledge CLI (90-120 min)
4. **Phase 3**: Testing and validation (30 min)

**Total Estimated Time**: 4-5 hours

---

## Risk Mitigation

### Risk 1: Knowledge System Refactor Breaks Existing Code
**Mitigation**:
- Preserve existing class structure initially
- Create parallel factory function
- Test thoroughly before removing old code
- Keep API surface identical

### Risk 2: Graph CLI Too Complex for Time Budget
**Mitigation**:
- Implement MVP commands first (init, create, list, show)
- Defer visualization to Phase 2
- Start with simple ASCII output, add Mermaid later
- Use simple filtering, defer complex queries

### Risk 3: CLI Integration Issues
**Mitigation**:
- Follow task.ts pattern exactly
- Use same Graph.dump() format
- Test save/load cycles early
- Verify Address registration pattern

### Risk 4: Time Overrun
**Mitigation**:
- Implement core commands first
- Mark advanced features as optional
- Use simpler implementations (e.g., basic search vs. semantic search)
- Defer testing to separate phase if needed

---

## Appendix: Code Patterns

### Pattern 1: Load/Save Graph
```typescript
async function loadGraph(filePath: string): Promise<Graph> {
  if (!existsSync(filePath)) {
    throw new Error(`File not found: ${filePath}`);
  }
  const content = readFileSync(filePath, "utf-8");
  const data = JSON.parse(content);
  const graph = new Graph();

  // Recreate nodes using factories
  for (const nodeProps of data.nodes) {
    if (nodeProps.type === "knowledge") {
      const address = KnowledgeActor({
        title: nodeProps.title,
        content: nodeProps.content,
        sources: nodeProps.sources,
        graph
      });
      // Already registered by factory
    }
  }

  // Recreate edges
  for (const edge of data.edges) {
    graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
  }

  return graph;
}

async function saveGraph(graph: Graph, filePath: string): Promise<void> {
  const dump = graph.dump();
  const serialized = JSON.stringify(dump, (key, value) => {
    if (value instanceof Date) return value.toISOString();
    return value;
  }, 2);
  writeFileSync(filePath, serialized, "utf-8");
}
```

### Pattern 2: Command Function
```typescript
async function cmdCreate(arg1: string, options: Record<string, any>) {
  const filePath = resolve(FILE);

  // Validation
  if (!existsSync(filePath)) {
    console.error(`Error: ${FILE} not found. Run 'cli init' first.`);
    process.exit(1);
  }

  // Load
  const graph = await loadGraph(filePath);

  // Operation
  const result = performOperation(graph, arg1, options);

  // Save
  await saveGraph(graph, filePath);

  // Output
  console.log(`Success: ${result}`);
}
```

### Pattern 3: Main Router
```typescript
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  try {
    switch (command) {
      case "init":
        await cmdInit();
        break;
      case "create":
        await cmdCreate(args[1], parseOptions(args.slice(2)));
        break;
      // ... more cases
      default:
        showHelp();
        process.exit(command ? 1 : 0);
    }
  } catch (error) {
    console.error("Error:", error.message);
    process.exit(1);
  }
}

main();
```

---

## Conclusion

This plan provides a comprehensive roadmap for implementing Graph and Knowledge CLIs. The design follows established patterns, integrates with existing systems, and provides clear specifications for each command.

**Next Steps**:
1. Begin Phase 0 (knowledge.ts refactor)
2. Proceed to Phase 1 (Graph CLI)
3. Continue to Phase 2 (Knowledge CLI)
4. Validate with Phase 3 (testing)

**Expected Outcome**: Two working CLIs that integrate seamlessly with the existing Task CLI and actor/graph systems.
