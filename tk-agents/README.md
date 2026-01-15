# tk-agents

Task/Knowledge Graph Actor Protocol implementation in TypeScript/Bun.

A unified graph structure where nodes represent tasks, knowledge, artifacts, and patterns. All interactions happen through message passing to node actors.

## Quick Start

```bash
# Install dependencies
bun install

# Run the demo
bun run demo.ts
```

## Core Concept

**Everything is a node. All interactions are messages.**

```typescript
// The only external interface
SEND(node_id, message_type, payload) -> response
```

## Architecture

```
tk-agents/
├── src/
│   ├── types.ts      # Type definitions
│   ├── graph.ts      # Graph store + SEND routing
│   ├── task.ts       # TaskNode with lifecycle
│   ├── knowledge.ts  # KnowledgeNode
│   └── index.ts      # Re-exports
├── demo.ts           # Interactive demo
└── index.ts          # Main entry
```

## Usage

```typescript
import { Graph, createTask, createKnowledge } from "./index";

const graph = new Graph();

// Create knowledge context
const knowledge = createKnowledge({
  title: "Requirements",
  content: "Build a REST API...",
}, graph);

// Create a task
const task = createTask({
  goal: "Implement API",
  desiredDeliverables: ["Endpoints", "Tests"],
  objectiveSuccessCriteria: [
    { criterion: "Tests pass", measure: "pass_rate", threshold: 1.0 }
  ],
}, graph);

// All interactions via SEND
graph.send(task.properties.id, "start", {});
graph.send(task.properties.id, "spawn", { goal: "Subtask...", ... });
graph.send(task.properties.id, "eval", {});
graph.send(task.properties.id, "complete", { result: {...} });
```

## Message Types

### Standard (All Nodes)
- `get` - Get node properties and edges
- `observe` - Get observations about current state
- `update` - Update properties
- `link` / `unlink` - Manage edges
- `delete` - Remove node

### Task-Specific
- `start` - Begin task execution
- `spawn` - Create child task
- `eval` - Evaluate against success criteria
- `complete` - Mark task complete
- `block` - Mark task blocked
- `query_status` - Get status with children

### Knowledge-Specific
- `append` - Add content
- `query` - Ask a question
- `synthesize` - Combine multiple knowledge nodes

## Task Lifecycle

```
created -> [start] -> active -> [eval] -> [complete] -> completed
                         |
                         v
                      blocked -> [unblock] -> active
```
