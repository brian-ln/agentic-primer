# Convergence Domain Protocols

Domain model protocols extracted from Simplify/UGS (Universal Graph System) for the WIT Platform Migration.

## Overview

This package defines the core domain model for the convergence framework including:

1. **Graph Primitives** - Universal graph system with addresses, nodes, and edges
2. **Domain Entities** - Typed entities (agents, tasks, sessions, humans)
3. **Query System** - Pattern matching, traversal, and criteria evaluation
4. **Knowledge Management** - Embeddings, convergence detection, and session state

## Package Structure

```
domain/
├── graph/
│   ├── address.wit          # @(id) address primitive with namespacing
│   ├── node.wit             # Node operations and properties
│   ├── edge.wit             # Edge operations and relationships
│   └── graph.wit            # Graph traversal and analytics
├── entity/
│   ├── entity.wit           # Base entity resource and operations
│   ├── agent.wit            # Agent entity (autonomous executors)
│   ├── task.wit             # Task entity (work specifications)
│   ├── session.wit          # Session entity (conversation management)
│   └── human.wit            # Human entity (user representation)
├── query/
│   ├── criteria.wit         # Success criteria evaluation
│   └── query.wit            # Query DSL and execution
└── knowledge/
    ├── embedding.wit        # Vector embeddings and similarity
    ├── convergence.wit      # Convergence detection
    └── session-state.wit    # Session knowledge management
```

## Design Principles

### 1. Address as Primitive

The `@(id)` syntax is a first-class primitive supporting:
- Universal interconnectedness across the graph
- Namespacing for scoping (`@(namespace/id)`)
- Versioning for temporal queries (`@(id:version)`)
- Scope modifiers (node, edge, computed)

**Design Choice**: Structured record (not string) for extensibility.

### 2. Resource-Based Entities

Domain entities use WIT `resource` types (not records) for:
- Lifecycle management
- State encapsulation
- Polymorphic behavior

**Entity Kinds**:
- `agent` - Autonomous task executors with system prompts
- `task` - Work specifications with success criteria
- `session` - Conversation containers with JSONL logging
- `human` - User actors with approvals and notifications

### 3. Query Compilation (Halo-inspired)

Query system separates:
- **Query Definition** - High-level DSL (data)
- **Query Plan** - Compiled execution plan (behavior)
- **Query Engine** - Distributed execution with caching

Optimizations include predicate pushdown, index selection, and parallelization.

### 4. Knowledge Integration

Session-level knowledge management:
- **Embeddings** - Semantic similarity via vector search
- **Convergence** - Detect when paths converge to similar conclusions
- **Session State** - Capture context, artifacts, and learned patterns

## Cross-Package Dependencies

```wit
domain → signal-hub:core (types, events)
domain/entity → domain/graph
domain/query → domain/graph + domain/entity
```

## Implementation Notes

### Graph Primitives

The graph model supports:
- **Properties**: Indexed key-value pairs (can be addresses)
- **Data**: Arbitrary JSON payloads
- **Event Sourcing**: All mutations create events
- **Indices**: Type, property, and full-text search

### Entity Lifecycles

Entities use state machines:
- **Agent**: `idle → thinking → executing → waiting → completed | error`
- **Task**: `pending → assigned → in-progress → completed | failed`
- **Session**: `created → active → paused | completed`
- **Human**: `available ↔ busy`, `available ↔ away`, `any → offline`

### Query Execution

Query compilation produces optimized plans:
1. Pattern matching with predicate pushdown
2. Index selection for efficient lookup
3. Parallelization where dependencies allow
4. Caching for repeated queries

### Knowledge Management

Embeddings enable:
- Semantic search across graph content
- Convergence detection via clustering
- Pattern extraction from session history

## Usage Examples

### Create and Query Nodes

```wit
// Create node with properties
let addr = node-ops.create-node(
    "task-123",
    some("task"),
    [("title", string-val("Implement auth")),
     ("assignee", address-ref(@(alice)))],
    none
);

// Find by property
let tasks = node-ops.find-by-property("assignee", address-ref(@(alice)));
```

### Agent Execution

```wit
// Create agent
let agent = agent-ops.create-agent(
    "agent-1",
    "Code Assistant",
    "You are a helpful coding assistant...",
    @(claude-balanced),
    some({max-turns: 50, reflect-on-failure: true, checkpoint-every: 5})
);

// Assign task and run
agent-ops.assign-task(agent, @(task-123));
let result = agent-ops.run(agent, none);
```

### Query with DSL

```wit
// Find all in-progress tasks assigned to alice
let query = {
    patterns: [{
        variable: "t",
        labels: ["task"],
        where-clause: [
            ("lifecycle", string-val("in-progress")),
            ("assignee", address-ref(@(alice)))
        ]
    }],
    traversals: [],
    aggregations: [],
    return-variables: ["t"]
};

let result = query.query-execute(query);
```

### Semantic Search

```wit
// Embed nodes
embedding.embed-nodes([@(task-1), @(task-2), @(task-3)]);

// Find similar to text
let similar = embedding.find-similar-to-text(
    "authentication implementation",
    {node-type: some("task"), limit: 10, min-similarity: 0.7}
);
```

## Validation

All WIT files validate with:

```bash
wasm-tools component wit core/wit/domain/
```

## Future Extensions

Planned enhancements:
- Temporal queries with version support
- Distributed graph partitioning
- Real-time convergence streaming
- Advanced pattern mining

## References

- [Universal Graph System Spec](../../../simplify/README.md)
- [Halo Query Optimization Paper](https://arxiv.org/abs/2104.05158)
- [WIT Specification](https://component-model.bytecodealliance.org/design/wit.html)
