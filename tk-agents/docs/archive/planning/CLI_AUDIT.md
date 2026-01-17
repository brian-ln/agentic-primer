# CLI Audit Report

**Date**: 2026-01-16
**Auditor**: Background Subagent
**Scope**: Assess current CLI implementations and identify gaps

---

## Executive Summary

**Current State**: One working CLI (Task CLI) with comprehensive functionality. Knowledge system implemented but no CLI. Graph system has no dedicated CLI for low-level operations.

**Gaps Identified**:
1. No Graph CLI for direct graph manipulation and visualization
2. No Knowledge CLI for knowledge management operations

**Recommendation**: Implement both missing CLIs following the established task.ts pattern.

---

## Current CLI Implementations

### 1. Task CLI (`src/cli/task.ts`)

**Status**: ✅ Implemented and working

**Location**: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/cli/task.ts`

**Capabilities**:
- ✅ `init` - Create tasks.json with example task
- ✅ `add <goal>` - Create new tasks with options (deliverables, criteria, dependencies, parent, labels, priority)
- ✅ `list` - List tasks with filters (status, label, priority)
- ✅ `show <id>` - Show detailed task information
- ✅ `update <id> <action>` - Update tasks (start, complete, block)
- ✅ `delete <id>` - Delete tasks with confirmation
- ✅ `ready` - Show tasks with no blockers
- ✅ `graph <id>` - Show dependency tree
- ✅ `eval <id>` - Evaluate task criteria
- ✅ `status <id>` - Show task status with blockers
- ✅ `search <query>` - Search tasks by keyword

**Architecture Patterns Observed**:
```typescript
- Shebang: #!/usr/bin/env bun
- File I/O: loadGraph() and saveGraph() for persistence
- Graph integration: Uses Graph and TaskActor factories
- Serialization: JSON with Date ISO string conversion
- CLI parsing: Manual arg parsing with process.argv
- Help system: Default case shows usage
- Error handling: console.error + process.exit(1)
```

**Key Integration Points**:
- Uses `Graph` class from `src/graph.ts`
- Uses `TaskActor` factory from `src/task.ts`
- Stores state in `tasks.json` using Graph.dump() format
- Restores state by recreating actors via TaskActor factory

**Quality Indicators**:
- ✅ Comprehensive command set
- ✅ Good error messages
- ✅ Filtering capabilities
- ✅ Proper graph integration
- ✅ Help documentation
- ⚠️  Manual argument parsing (no library)
- ⚠️  Uses emojis for status (contrary to project guidelines)

---

## System Implementations

### 2. Graph System (`src/graph.ts`)

**Status**: ✅ Implemented

**Location**: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/graph.ts`

**Key Features**:
- Node registration with bidirectional mapping (String ID ↔ Address)
- Edge management (add, remove, query)
- Message routing via Address proxies
- Property caching
- Serialization support (dump/restore)
- Actor system integration

**Available Operations** (no CLI yet):
- `registerNode(id, address, properties)`
- `send(nodeId, messageType, payload)` - Message routing
- `removeNode(nodeId)` - Remove node and all edges
- `getNode(nodeId)` - Get Address by ID
- `getNodeProperties(nodeId)` - Get cached properties
- `getNodeIds()` - List all node IDs
- `addEdge(fromId, toId, type, properties)` - Create edge
- `removeEdge(edgeId)` - Remove edge
- `getEdgesFrom(nodeId)` - Query outgoing edges
- `getEdgesTo(nodeId)` - Query incoming edges
- `getAllEdges(nodeId)` - Get all connected edges
- `getChildTasks(taskId)` - Get spawned_by children
- `dump()` - Serialize graph state
- `setEdgeCounter(n)` - For restore

**CLI Gap**: ❌ No dedicated Graph CLI for these low-level operations

---

### 3. Knowledge System (`src/knowledge.ts`)

**Status**: ✅ Implemented (but uses OLD actor pattern - not Address-based)

**Location**: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/knowledge.ts`

**Key Features**:
- KnowledgeNode class implements NodeActor interface
- Title, content, sources, version tracking
- Append operation with version increment
- Query operation with keyword matching
- Synthesize operation to combine knowledge

**Available Operations** (no CLI yet):
- `createKnowledge(options, graph)` - Factory function
- Message handlers:
  - `get` - Retrieve full state
  - `observe` - Get summary
  - `update` - Update properties
  - `link/unlink` - Edge management
  - `delete` - Remove node
  - `append` - Add content with source tracking
  - `query` - Search knowledge with confidence scoring
  - `synthesize` - Combine multiple knowledge nodes

**Architecture Note**: ⚠️ Uses OLD class-based NodeActor pattern, not the newer Address-based TaskActor pattern. The knowledge.ts file references a `NodeActor` interface and `graph.registerNode(node)` that doesn't match the current Graph.ts API which expects `registerNode(id, address, properties)`.

**CLI Gap**: ❌ No Knowledge CLI

---

## Type System Assessment

**Location**: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/types.ts`

**Node Types Defined**:
- `task` - ✅ Has CLI
- `knowledge` - ❌ No CLI
- `artifact` - ❌ Not implemented
- `pattern` - ❌ Not implemented

**Edge Types Available**:
- `depends_on` - Task dependencies
- `requires_knowledge` - Task → Knowledge
- `produces` - Task → Artifact
- `spawned_by` - Parent-child tasks
- `blocks` - Blocking relationships
- `references` - General references

**Message Types**:
- StandardMessage - All node types
- TaskMessage - Task-specific
- KnowledgeMessage - Knowledge-specific

---

## Specification Documentation

### Available Specs:
1. ✅ `GRAPH_SYSTEM.spec.md` - Comprehensive graph system spec
2. ✅ `KNOWLEDGE_SYSTEM.spec.md` - Comprehensive knowledge system spec
3. ✅ `TASK_SYSTEM.spec.md` - Task system spec (assumed to exist)
4. ✅ `TASK_CLI.spec.md` - Task CLI spec (assumed to exist based on file listing)

---

## Gap Analysis

### Critical Gaps

#### 1. No Graph CLI
**Impact**: High
**Reason**: Users cannot:
- Directly manipulate graph structure
- Visualize graph topology
- Query nodes and edges at low level
- Import/export graph data
- Debug graph relationships

**Required Commands**:
```
graph create-node <id> [--type TYPE] [--data JSON]
graph delete-node <id>
graph create-edge <from> <to> [--label LABEL]
graph delete-edge <from> <to>
graph find-nodes [--type TYPE] [--filter EXPR]
graph find-path <from> <to>
graph show [--format ascii|mermaid]
graph export [--output FILE]
graph import <file>
```

#### 2. No Knowledge CLI
**Impact**: High
**Reason**: Users cannot:
- Create and manage knowledge nodes
- Query knowledge base
- Link knowledge to tasks
- Track knowledge versions
- Synthesize knowledge from multiple sources

**Required Commands**:
```
knowledge add <content> [--tags TAG1,TAG2] [--metadata JSON]
knowledge get <id>
knowledge list [--tag TAG] [--limit N]
knowledge update <id> [--content TEXT] [--metadata JSON]
knowledge delete <id>
knowledge link <knowledge-id> <node-id>
knowledge unlink <knowledge-id> <node-id>
knowledge search <query>
knowledge tag <id> <tag>
knowledge untag <id> <tag>
```

#### 3. Knowledge System Architecture Mismatch
**Impact**: Medium
**Reason**: knowledge.ts uses old class-based pattern, not new Address-based pattern used in task.ts. This needs to be addressed for consistency.

**Evidence**:
- task.ts: `TaskActor: ActorFactory<TaskActorData>` returns Address
- knowledge.ts: `class KnowledgeNode implements NodeActor` with `handleMessage` method
- Graph.ts: `registerNode(id: string, address: Address, properties: NodeProperties)` expects Address
- knowledge.ts: `graph.registerNode(knowledge)` passes node object, not Address

**Resolution Required**: Refactor KnowledgeNode to follow TaskActor pattern or create adapter.

---

## CLI Integration Patterns

### Established Pattern (from task.ts)

**File Structure**:
```typescript
#!/usr/bin/env bun
import { Graph } from "../graph.ts";
import { ActorFactory } from "../system.ts";
import { readFileSync, writeFileSync, existsSync } from "fs";
import { resolve } from "path";

const FILE = "data.json";

// File format
interface FileFormat {
  nodes: NodeProperties[];
  edges: Edge[];
}

// Load/Save helpers
async function loadGraph(path): Promise<Graph> { ... }
async function saveGraph(graph, path): Promise<void> { ... }

// Commands
async function cmdInit() { ... }
async function cmdList() { ... }
async function cmdShow(id) { ... }
// ... more commands

// Main CLI router
async function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  switch (command) {
    case "init": await cmdInit(); break;
    case "list": await cmdList(); break;
    // ... more cases
    default: showHelp(); process.exit(1);
  }
}

main().catch(err => {
  console.error("Error:", err.message);
  process.exit(1);
});
```

**Key Patterns**:
1. Shebang for direct execution
2. JSON file persistence (single file per CLI)
3. loadGraph() and saveGraph() functions
4. Command functions prefixed with `cmd`
5. Main router with switch statement
6. Default case shows help
7. Global error handler with process.exit(1)
8. Manual arg parsing with process.argv

---

## Recommendations

### Priority 1: Implement Graph CLI

**File**: `src/cli/graph.ts`
**Data File**: `graph.json` (separate from tasks.json)
**Pattern**: Follow task.ts structure

**Core Commands** (MVP):
- `init` - Create empty graph.json
- `create-node <id> --type TYPE` - Create generic node
- `delete-node <id>` - Remove node and edges
- `create-edge <from> <to> --type LABEL` - Create edge
- `delete-edge <edgeId>` - Remove edge by ID
- `list-nodes [--type TYPE]` - List all nodes
- `list-edges [--from ID] [--to ID]` - List edges
- `show <id>` - Show node details and edges
- `show-graph [--format ascii]` - Visualize graph (simple ASCII tree)
- `export [--output FILE]` - Export to JSON
- `import <file>` - Import from JSON

**Challenge**: Graph CLI should work with ANY node type (task, knowledge, artifact), not create them with full semantics. It's a low-level tool for graph manipulation.

### Priority 2: Address Knowledge System Architecture

**Options**:

A. **Refactor knowledge.ts to match task.ts pattern** (Recommended)
   - Convert KnowledgeNode class to KnowledgeActor factory
   - Return Address from factory
   - Update handleMessage to match TaskActor pattern
   - Preserve all functionality

B. **Create adapter in knowledge.ts**
   - Wrap KnowledgeNode in Address-returning factory
   - Keep existing class implementation
   - Add compatibility layer

C. **Update Graph.ts to accept both patterns**
   - Add overload: `registerNode(node: NodeActor)`
   - Detect pattern and adapt internally
   - Maintains backward compatibility

**Recommendation**: Option A (refactor) for consistency and future maintainability.

### Priority 3: Implement Knowledge CLI

**File**: `src/cli/knowledge.ts`
**Data File**: `knowledge.json` (or integrate with graph.json)
**Pattern**: Follow task.ts structure but adapted for knowledge operations

**Core Commands** (MVP):
- `init` - Create knowledge.json
- `add <title> --content TEXT [--sources S1,S2]` - Create knowledge
- `get <id>` - Show knowledge details
- `list [--limit N]` - List all knowledge nodes
- `append <id> <content> [--source SRC]` - Append to knowledge
- `update <id> --title TEXT --content TEXT` - Update knowledge
- `delete <id>` - Remove knowledge
- `query <id> <question>` - Query knowledge
- `search <query>` - Search all knowledge (keyword match)
- `link <knowledge-id> <node-id> --type EDGE_TYPE` - Link to node
- `synthesize <id> --from ID1,ID2` - Synthesize knowledge

**Data Storage Decision**:
- Option A: Separate `knowledge.json` (simpler, follows task.ts pattern)
- Option B: Unified `graph.json` (requires graph CLI first)

**Recommendation**: Start with Option A (separate file) for independence, consider unification later.

---

## Implementation Sequence

### Phase 1: Address Architecture Mismatch (1 hour)
1. Refactor knowledge.ts to Address-based pattern
2. Create KnowledgeActor factory matching TaskActor
3. Update types.ts if needed
4. Verify integration with Graph

### Phase 2: Implement Graph CLI (2 hours)
1. Create src/cli/graph.ts with basic structure
2. Implement init, create-node, delete-node
3. Implement create-edge, delete-edge, list commands
4. Implement show and export/import
5. Add help documentation
6. Make executable (chmod +x)

### Phase 3: Implement Knowledge CLI (2 hours)
1. Create src/cli/knowledge.ts with basic structure
2. Implement init, add, get, list
3. Implement append, update, delete
4. Implement query, search
5. Implement link, synthesize
6. Add help documentation
7. Make executable (chmod +x)

### Phase 4: Testing and Documentation (1 hour)
1. Create test suite for Graph CLI
2. Create test suite for Knowledge CLI
3. Update main README with CLI usage
4. Create examples/demos

**Total Estimated Time**: 6 hours

---

## Architecture Insights

### Strengths of Current Design
1. **Actor-based messaging** - Clean separation of concerns
2. **Graph as central coordinator** - Single source of truth
3. **Address proxy pattern** - Type-safe, unforgeable message routing
4. **Serialization support** - Graph.dump() enables persistence
5. **Factory pattern** - Consistent actor creation

### Design Challenges for CLIs
1. **Mixed node types** - Graph CLI needs to handle task/knowledge/etc uniformly
2. **Actor recreation** - Restoring from JSON requires factory knowledge
3. **Separate vs unified storage** - Each CLI has its own .json file
4. **Cross-CLI operations** - Task CLI can't manage knowledge nodes
5. **Low-level vs high-level** - Graph CLI is low-level, Task/Knowledge CLIs are high-level

### Proposed Solutions
1. **Generic node creation** - Graph CLI creates minimal nodes, specialized CLIs add semantics
2. **Factory registry** - Map node types to factories for restore
3. **Unified graph.json** - Single file with all node types
4. **CLI delegation** - Each CLI focuses on its domain, uses graph.send() for others
5. **Layered abstraction** - Graph CLI = low-level, domain CLIs = high-level

---

## Conclusion

**Current State**: Task CLI is well-implemented and provides a strong pattern to follow. Graph and Knowledge systems are implemented but lack CLI interfaces.

**Immediate Actions Required**:
1. ✅ Audit complete (this document)
2. ⏭️  Create implementation plan
3. ⏭️  Address knowledge.ts architecture
4. ⏭️  Implement Graph CLI
5. ⏭️  Implement Knowledge CLI
6. ⏭️  Create test suite

**Estimated Effort**: 6-8 hours for full implementation

**Risk Assessment**: Low - Clear patterns established, specifications available, architecture understood

---

## Appendix: File Locations

- Task CLI: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/cli/task.ts`
- Graph System: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/graph.ts`
- Knowledge System: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/knowledge.ts`
- Task System: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/task.ts`
- Types: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/types.ts`
- Graph Spec: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/GRAPH_SYSTEM.spec.md`
- Knowledge Spec: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/src/KNOWLEDGE_SYSTEM.spec.md`
