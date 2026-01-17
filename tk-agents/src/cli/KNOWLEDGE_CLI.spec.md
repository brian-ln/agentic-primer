# Knowledge CLI Specification

## Overview

The Knowledge CLI is a command-line interface for managing knowledge nodes using the Graph/KnowledgeActor system. It provides versioned, append-only knowledge storage with querying, searching, and synthesis capabilities.

## Core Mission

Enable users to capture, organize, query, and synthesize knowledge through a persistent, version-controlled system. Knowledge nodes maintain content history, track sources, and support intelligent querying and cross-node synthesis.

## Architecture

```
+--------------------+     +-----------+     +------------------+
|   Knowledge CLI    | --> |   Graph   | --> | KnowledgeActor   |
| (Command Layer)    |     | (Router)  |     | (Message Handler)|
|                    |     |           |     |                  |
+--------------------+     +-----------+     +------------------+
        |                      ^
        v                      |
+--------------------+         |
|  knowledge.json    |---------+
|  (Persistence)     |  load/save
+--------------------+
```

### Component Responsibilities

- **Knowledge CLI**: Command parsing, display formatting, user interaction
- **Graph**: Node/edge management, message routing, serialization
- **KnowledgeActor**: Knowledge state, versioning, query handling, synthesis
- **knowledge.json**: Persistent storage of knowledge graph

### Design Philosophy

Knowledge nodes are:
- **Versioned**: Each append creates a new version
- **Append-only**: Content grows over time (with full replacement option)
- **Source-tracked**: Every piece of knowledge tracks its origins
- **Queryable**: Natural language queries return relevant excerpts
- **Synthesizable**: Multiple knowledge nodes can be merged

## Commands Reference

### Initialization Command

#### `knowledge init`

Creates a new `knowledge.json` file with an example knowledge node.

**Preconditions:**
- `knowledge.json` must NOT exist in current directory

**Postconditions:**
- Creates `knowledge.json` with valid JSON structure
- Contains one example knowledge node

**Output:**
```
Created knowledge.json with example knowledge
Knowledge ID: knowledge_1
```

---

### Knowledge Creation

#### `knowledge add <title> [--content TEXT] [--sources S1,S2]`

Creates a new knowledge node.

**Arguments:**
- `<title>`: Required. Knowledge title/heading.

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--content` | `TEXT` | Initial content (default: empty string) |
| `--sources` | `S1,S2,...` | Comma-separated source attributions |

**Preconditions:**
- `knowledge.json` must exist

**Postconditions:**
- New knowledge node created with unique ID (`knowledge_N`)
- Version set to 1
- Created timestamp set

**Output:**
```
Added knowledge: knowledge_5
Title: API Design Patterns
Version: 1
```

**Examples:**
```bash
knowledge add "REST API Guidelines"
knowledge add "GraphQL Best Practices" --content "Use schema-first design" --sources "Apollo Docs,Personal Experience"
```

---

### Knowledge Inspection

#### `knowledge get <id>`

Shows complete details for a knowledge node.

**Arguments:**
- `<id>`: Knowledge node ID

**Preconditions:**
- `knowledge.json` must exist
- Knowledge node must exist

**Display Sections:**
- Title, Version, Created timestamp
- Content length
- Source count
- Full content
- Source list
- Connected nodes

**Output:**
```
Knowledge: knowledge_5
────────────────────────────────────────────────────────────────────────────────
Title:          API Design Patterns
Version:        3
Created:        2026-01-16T19:00:00.000Z
Content Length: 1250 characters
Sources:        2

Content:
REST APIs should follow standard HTTP semantics. Use proper status codes...
[full content displayed]

Sources:
  - MDN Web Docs
  - Personal Experience

Connected Nodes (1):
  task_10 -> (requires_knowledge)
```

---

#### `knowledge list [--limit N]`

Lists all knowledge nodes, sorted by creation date (newest first).

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--limit` | `N` | Limit results to N nodes |

**Display Format:**
```
Knowledge Nodes:
────────────────────────────────────────────────────────────────────────────────
ID                  Title                              Ver  Created
────────────────────────────────────────────────────────────────────────────────
knowledge_5         API Design Patterns                3    2026-01-16
knowledge_3         GraphQL Best Practices             1    2026-01-15
knowledge_1         Example Knowledge                  1    2026-01-14
────────────────────────────────────────────────────────────────────────────────
Total: 3 knowledge node(s)
```

**Examples:**
```bash
knowledge list            # All knowledge
knowledge list --limit 10 # First 10 nodes
```

---

### Knowledge Modification

#### `knowledge append <id> <content> [--source SRC]`

Appends content to existing knowledge node (version increment).

**Arguments:**
- `<id>`: Knowledge node ID
- `<content>`: Text to append

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--source` | `SRC` | Source attribution for appended content |

**Preconditions:**
- `knowledge.json` must exist
- Knowledge node must exist

**Postconditions:**
- Content appended to existing content
- Version incremented
- Source added to sources list (if provided)

**Output:**
```
Appended to knowledge_5
New version: 4
Source added: Stack Overflow
```

**Examples:**
```bash
knowledge append knowledge_5 "Additional guideline: Use pagination for collections"
knowledge append knowledge_5 "Error handling: Return 4xx for client errors" --source "RFC 7231"
```

---

#### `knowledge update <id> [--title TEXT] [--content TEXT]`

Updates knowledge properties (title or full content replacement).

**Arguments:**
- `<id>`: Knowledge node ID

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--title` | `TEXT` | New title |
| `--content` | `TEXT` | Replace entire content |

**Preconditions:**
- `knowledge.json` must exist
- Knowledge node must exist
- At least one of `--title` or `--content` must be provided

**Postconditions:**
- Specified properties updated
- Version unchanged (structural update, not content append)

**Output:**
```
Updated knowledge_5
Title: API Design Patterns (Updated)
```

**Examples:**
```bash
knowledge update knowledge_5 --title "REST API Design Patterns"
knowledge update knowledge_5 --content "Complete rewrite of content..."
```

---

#### `knowledge delete <id> [--force]`

Removes a knowledge node and all connected edges.

**Arguments:**
- `<id>`: Knowledge node ID

**Options:**
- `--force`: Skip confirmation prompt

**Preconditions:**
- `knowledge.json` must exist
- Knowledge node must exist

**Safety Mechanism:**
- Without `--force`, displays knowledge details and connected nodes
- User must type "yes" to confirm

**Output:**
```
Knowledge to delete: knowledge_5
────────────────────────────────────────────────────────────────────────────────
Title: API Design Patterns
Version: 3
Content length: 1250 characters

Connected to 2 node(s):
  task_10 (requires_knowledge)
  artifact_3 (produces)

Delete this knowledge? (yes/no): yes

Deleted knowledge_5
Removed 2 connected edge(s)
```

---

### Knowledge Querying

#### `knowledge query <id> <question>`

Queries a specific knowledge node with a natural language question.

**Arguments:**
- `<id>`: Knowledge node ID
- `<question>`: Natural language question

**Preconditions:**
- `knowledge.json` must exist
- Knowledge node must exist

**Response Format:**
- Answer: Relevant excerpt or response
- Confidence: 0.0 to 1.0 (how well content matches question)
- Sources: Source attributions for the answer

**Output:**
```
Query: "What status codes should I use?"
Knowledge: knowledge_5
────────────────────────────────────────────────────────────────────────────────

Answer:
Use proper HTTP status codes: 200 for success, 400 for client errors, 500 for server errors. Return 404 for not found resources.

Confidence: 85%

Sources:
  - RFC 7231
  - MDN Web Docs
```

**Examples:**
```bash
knowledge query knowledge_5 "What about pagination?"
knowledge query knowledge_3 "How do I handle authentication?"
```

---

#### `knowledge search <query>`

Searches all knowledge nodes for relevant information.

**Arguments:**
- `<query>`: Search query string

**Preconditions:**
- `knowledge.json` must exist

**Behavior:**
- Queries every knowledge node
- Filters results by confidence > 0
- Sorts by confidence (descending)

**Output:**
```
Search: "error handling patterns"
────────────────────────────────────────────────────────────────────────────────

Results:

[85%] knowledge_5: API Design Patterns
  Use proper HTTP status codes: 200 for success, 400 for client...

[72%] knowledge_8: JavaScript Error Handling
  Try-catch blocks should wrap async operations. Always log err...

[45%] knowledge_2: System Design Principles
  Fail fast and provide meaningful error messages to users...

────────────────────────────────────────────────────────────────────────────────
Found 3 matching knowledge node(s)
```

**Examples:**
```bash
knowledge search "authentication"
knowledge search "how to handle database connections"
```

---

### Knowledge Linking

#### `knowledge link <kid> <nid> --type TYPE`

Creates an edge from knowledge node to another node.

**Arguments:**
- `<kid>`: Knowledge node ID (source)
- `<nid>`: Target node ID (any type)

**Options:**
| Flag | Format | Description |
|------|--------|-------------|
| `--type` | `EdgeType` | Edge type (required) |

**Valid Edge Types:**
- `requires_knowledge`: Target requires this knowledge
- `produces`: Knowledge produces target
- `references`: General reference
- `depends_on`, `spawned_by`, `blocks`: Other standard types

**Preconditions:**
- `knowledge.json` must exist
- Knowledge node must exist
- Edge type must be specified

**Note:** Target node may be in a different file (e.g., `tasks.json`). The link is created in `knowledge.json` but may reference external nodes.

**Output:**
```
Linked knowledge_5 to task_10
Edge type: requires_knowledge
Edge ID: edge_12
```

**Examples:**
```bash
knowledge link knowledge_5 task_10 --type requires_knowledge
knowledge link knowledge_3 artifact_2 --type produces
```

---

#### `knowledge unlink <kid> <nid>`

Removes all edges between knowledge node and target node.

**Arguments:**
- `<kid>`: Knowledge node ID
- `<nid>`: Target node ID

**Preconditions:**
- `knowledge.json` must exist
- At least one edge must exist between the nodes

**Behavior:**
- Removes edges in both directions
- Removes all matching edges (if multiple exist)

**Output:**
```
Unlinked knowledge_5 from task_10
Removed 1 edge(s)
```

---

### Knowledge Synthesis

#### `knowledge synthesize <id> --from ID1,ID2,...`

Synthesizes knowledge from multiple source nodes into target node.

**Arguments:**
- `<id>`: Target knowledge node ID
- `--from`: Comma-separated list of source node IDs

**Preconditions:**
- `knowledge.json` must exist
- Target knowledge node must exist
- Source nodes should exist (in knowledge.json or other files)

**Behavior:**
- Sends `synthesize` message to target knowledge actor
- Actor combines information from source nodes
- Sources are merged into target's source list

**Output:**
```
Synthesized from 2 source(s)
Target: knowledge_10
Sources: knowledge_5, knowledge_8
────────────────────────────────────────────────────────────────────────────────

Synthesis (2100 characters):
Combined knowledge from API Design Patterns and JavaScript Error Handling:

REST APIs should follow standard HTTP semantics...
[synthesis content]

────────────────────────────────────────────────────────────────────────────────
Combined sources:
  - RFC 7231
  - MDN Web Docs
  - Personal Experience
  - Stack Overflow
```

**Examples:**
```bash
knowledge synthesize knowledge_10 --from knowledge_5,knowledge_8
knowledge synthesize summary_node --from k1,k2,k3,k4
```

---

## File Format

### knowledge.json Structure

```typescript
interface KnowledgeFile {
  nodes: NodeProperties[];  // Array of knowledge property objects
  edges: Edge[];            // Array of edge objects
}
```

### Knowledge Properties Schema

```typescript
interface KnowledgeProperties extends NodeProperties {
  id: string;                    // "knowledge_N" format
  type: "knowledge";             // Always "knowledge"
  title: string;                 // Knowledge title/heading
  content: string;               // Full text content
  sources: string[];             // Source attributions
  version: number;               // Version number (increments with append)
  createdAt: string;             // ISO 8601 timestamp
}
```

### Edge Schema

```typescript
interface Edge {
  id: string;           // "edge_N" format
  fromId: string;       // Source node ID
  toId: string;         // Target node ID
  type: EdgeType;       // Edge type
  properties: Record<string, unknown>;
}
```

---

## Knowledge Versioning

### Version Semantics

- **Version 1**: Initial knowledge creation
- **Version N**: After N-1 append operations
- **Updates**: Title and content replacement do NOT increment version

### Append vs Update

| Operation | Effect | Version | Use Case |
|-----------|--------|---------|----------|
| `append` | Add to content | +1 | Incremental knowledge growth |
| `update --content` | Replace content | No change | Complete rewrite |
| `update --title` | Change title | No change | Rename/clarify |

### Example Evolution

```bash
# v1 - Initial creation
knowledge add "API Guide" --content "Introduction to REST"

# v2 - Append new information
knowledge append knowledge_1 "Authentication: Use OAuth 2.0" --source "RFC 6749"

# v3 - Append more
knowledge append knowledge_1 "Rate limiting: Implement token bucket"

# v3 - Title update (version unchanged)
knowledge update knowledge_1 --title "Complete REST API Guide"
```

---

## Query Mechanism

### Query Processing

1. **Input**: Natural language question
2. **Processing**: Knowledge actor analyzes content relevance
3. **Output**: Answer excerpt with confidence score

### Confidence Scores

| Range | Interpretation |
|-------|----------------|
| 0.8 - 1.0 | Highly relevant, strong match |
| 0.5 - 0.79 | Moderately relevant |
| 0.2 - 0.49 | Weakly relevant |
| 0.0 - 0.19 | Minimal relevance |

### Search Filtering

- `knowledge search` only returns results with confidence > 0
- Results sorted by confidence (descending)
- Each result shows: [confidence%] id: title + snippet

---

## Error Handling

### Common Error Conditions

| Error | Cause | Exit Code |
|-------|-------|-----------|
| `Knowledge file not found` | Running commands before `knowledge init` | 1 |
| `knowledge.json already exists` | Running `knowledge init` twice | 1 |
| `Knowledge not found: <id>` | Invalid knowledge ID | 1 |
| `Provide at least --title or --content` | Empty update command | 1 |
| `--type is required` | Missing edge type in link command | 1 |
| `--from is required` | Missing sources in synthesize command | 1 |
| `No edges found between <kid> and <nid>` | Unlinking non-existent connection | 1 |

### Error Output Format

```
Error: <message>
```

---

## Performance Characteristics

- **File I/O**: Synchronous read/write operations
- **Knowledge loading**: Creates active KnowledgeActor instances
- **Search**: O(n) full scan with query processing per node
- **Query**: O(1) single node query
- **Memory**: Full knowledge graph loaded during command execution

---

## Integration Points

### With Graph System

- CLI creates Graph instance per command
- Uses `KnowledgeActor()` factory for node creation
- Uses `graph.send()` for actor messaging
- Uses `graph.dump()` for serialization
- Uses `graph.addEdge()`, `graph.removeNode()` for structure changes

### With KnowledgeActor

- Created via `KnowledgeActor()` factory function
- Messages: `get`, `append`, `update`, `query`, `synthesize`
- Properties accessed via `graph.getNodeProperties()`
- State restored during graph loading

### With File System

- Uses Node.js `fs` module (readFileSync, writeFileSync, existsSync)
- JSON serialization with Date-to-ISO transform

### Cross-File References

- Knowledge nodes can link to tasks in `tasks.json`
- Tasks can reference knowledge via `requires_knowledge` edges
- Edge stored in knowledge.json, target may be external

---

## Command Composition Rules

### Prerequisite Chain

```
init -> add -> append/update/query/search/get/link/synthesize
          ↓
        list (after at least one add)
          ↓
        delete
```

### Safe Command Sequences

```bash
# Basic workflow
knowledge init
knowledge add "API Basics" --content "REST principles" --sources "RFC"
knowledge list
knowledge get knowledge_1

# Knowledge growth
knowledge append knowledge_1 "More details..." --source "MDN"
knowledge query knowledge_1 "What about authentication?"

# Search and synthesis
knowledge search "error handling"
knowledge synthesize knowledge_summary --from knowledge_1,knowledge_2

# Linking
knowledge link knowledge_1 task_5 --type requires_knowledge
knowledge unlink knowledge_1 task_5

# Cleanup
knowledge delete knowledge_1 --force
```

---

## Verification Checklist

- [ ] `knowledge init` creates valid knowledge.json
- [ ] `knowledge add` creates knowledge with version 1
- [ ] `knowledge add --content` stores content
- [ ] `knowledge add --sources` parses comma-separated sources
- [ ] `knowledge get` displays all knowledge details
- [ ] `knowledge list` sorts by creation date (newest first)
- [ ] `knowledge list --limit` restricts result count
- [ ] `knowledge append` increments version
- [ ] `knowledge append --source` adds to sources list
- [ ] `knowledge update --title` changes title without version change
- [ ] `knowledge update --content` replaces content without version change
- [ ] `knowledge update` requires at least one flag
- [ ] `knowledge delete` prompts for confirmation
- [ ] `knowledge delete --force` skips confirmation
- [ ] `knowledge query` returns answer with confidence
- [ ] `knowledge search` queries all nodes
- [ ] `knowledge search` filters by confidence > 0
- [ ] `knowledge search` sorts by confidence descending
- [ ] `knowledge link` requires --type flag
- [ ] `knowledge link` creates edge successfully
- [ ] `knowledge unlink` removes edges bidirectionally
- [ ] `knowledge synthesize` requires --from flag
- [ ] `knowledge synthesize` combines sources from multiple nodes

---

## Summary

The Knowledge CLI provides a comprehensive knowledge management system built on the Graph/KnowledgeActor architecture. Key capabilities:

1. **Versioned content** via append-only operations
2. **Source tracking** for attribution and credibility
3. **Natural language querying** with confidence scores
4. **Cross-graph search** to find relevant knowledge
5. **Knowledge synthesis** to combine multiple sources
6. **Linking to other nodes** for knowledge-task integration
7. **Safe deletion** with confirmation prompts

The CLI serves as the knowledge layer for the tk-agents system, enabling both human knowledge capture and agent knowledge retrieval.
