# V1 vs V2 Architectural Analysis

## Executive Summary

This document analyzes the V1 (Nodes as Data) and V2 (Nodes as Executables) perspectives to identify synergies and design a hybrid architecture that leverages the strengths of both approaches.

**Key Finding**: V1 and V2 are not competing alternatives but complementary layers that can coexist in a unified architecture.

---

## V1: Nodes as Data Entities

### Core Philosophy
Nodes represent **persistent domain entities** (tasks, knowledge, artifacts) that are managed by external clients (agents, humans, programs).

### Strengths
1. **Clear Domain Model**: Tasks and knowledge are first-class data
2. **Persistence-Friendly**: Easy serialization to JSON/JSONL
3. **Query-Optimized**: Can cache properties for fast lookups
4. **Audit Trail**: Changes tracked through git-backed storage
5. **Separation of Concerns**: Data vs behavior cleanly separated
6. **Type Safety**: Domain types are explicit (task, knowledge, artifact)

### Limitations
1. **Static Location**: All nodes assumed local
2. **External Execution**: Agents are outside the graph
3. **Limited Composition**: Can't easily chain execution
4. **No Location Transparency**: Can't scale to remote execution
5. **Tool Integration**: Tools referenced as strings, not addressable

### Use Cases
- Task management and tracking
- Knowledge base construction
- Work item persistence
- Multi-session collaboration (git-backed)
- CLI-driven workflow

---

## V2: Nodes as Executable Work Units

### Core Philosophy
Nodes represent **executable capabilities** (agents, tools, APIs, scripts, humans) accessed through location-transparent addressing.

### Strengths
1. **Location Transparency**: Local/remote/distributed execution unified
2. **Uniform Interface**: Everything addressable via send/receive
3. **Dynamic Composition**: Chain execution through message passing
4. **First-Class Agents**: Agents as nodes, not external clients
5. **Tool Addressability**: Tools are graph entities
6. **Scalability**: Distribute execution across infrastructure

### Limitations
1. **Persistence Challenge**: Executable code harder to serialize
2. **Complexity**: Location routing adds infrastructure burden
3. **Type Proliferation**: Many execution types (script, API, agent, tool, human)
4. **Security Surface**: Remote execution introduces risks
5. **Debugging**: Distributed execution harder to trace

### Use Cases
- Distributed agent coordination
- Location-transparent tool invocation
- Remote API integration
- Human-in-the-loop workflows (chatbots)
- Multi-agent collaboration across hosts

---

## Synergy Analysis

### Complementary Strengths

| Capability | V1 Approach | V2 Approach | Hybrid Benefit |
|------------|-------------|-------------|----------------|
| **Data Persistence** | ✅ Excellent (JSON/JSONL) | ⚠️ Limited (executables don't serialize) | V1 persists state, V2 executes |
| **Execution** | ⚠️ External only | ✅ First-class, location-transparent | V2 executes work defined in V1 |
| **Scalability** | ❌ Local only | ✅ Distributed | V2 scales V1's work |
| **Domain Model** | ✅ Clear types | ⚠️ Execution-focused | V1 defines work, V2 performs it |
| **Tool Integration** | ⚠️ String references | ✅ Addressable nodes | V2 makes V1's tools executable |
| **Composability** | ⚠️ Static dependencies | ✅ Dynamic chaining | V2 enables V1 workflows |

### Identified Synergies

**1. Work Definition + Execution Separation**
- **V1 defines WHAT**: TaskNode describes work to be done
- **V2 defines WHO**: AgentNode/ToolNode performs work
- **Synergy**: Task decomposition (V1) → Agent execution (V2)

**2. Knowledge as Data + Skills**
- **V1**: Knowledge as document nodes (data)
- **V2**: Knowledge as skill nodes (executable patterns)
- **Synergy**: Documentation (V1) + Procedures (V2)

**3. Local Persistence + Remote Execution**
- **V1**: Git-backed storage for multi-session work
- **V2**: Distribute execution across infrastructure
- **Synergy**: Store state locally, execute remotely

**4. Static Structure + Dynamic Dispatch**
- **V1**: Static dependency graphs (task hierarchies)
- **V2**: Dynamic routing (address-based dispatch)
- **Synergy**: Plan structure (V1) → Adaptive execution (V2)

**5. Type Safety + Interface Uniformity**
- **V1**: Domain types (task, knowledge, artifact)
- **V2**: Execution types (agent, tool, API, script)
- **Synergy**: Type system spans both layers

---

## Hybrid Architecture Proposal

### Layered Model

```
┌─────────────────────────────────────────────┐
│  V1 DATA LAYER (Domain Entities)           │
│  - TaskNode: work definition                │
│  - KnowledgeNode: information/skills        │
│  - ArtifactNode: outputs                    │
│  - Persistence: JSON/JSONL + git            │
└─────────────────────────────────────────────┘
                    ↕ (references)
┌─────────────────────────────────────────────┐
│  V2 EXECUTION LAYER (Work Units)            │
│  - AgentNode: autonomous executors          │
│  - ToolNode: callable programs              │
│  - APINode: external service wrappers       │
│  - ScriptNode: script executors             │
│  - Location: local/remote/distributed       │
└─────────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────────┐
│  ACTOR FOUNDATION (Unified)                 │
│  - Address-based messaging                  │
│  - Send/receive protocol                    │
│  - System and AddressProxy                  │
└─────────────────────────────────────────────┘
```

### Type Taxonomy

```typescript
type Node = DataNode | ExecutableNode;

type DataNode = {
  category: "data";
  type: "task" | "knowledge" | "artifact" | "pattern";
  // ... V1 properties
};

type ExecutableNode = {
  category: "executable";
  type: "agent" | "tool" | "api" | "script" | "human";
  location: "local" | "remote" | "distributed";
  // ... V2 properties
};
```

### Integration Patterns

**Pattern 1: Task → Agent Assignment**
```typescript
// V1 TaskNode references V2 AgentNode
const task = {
  type: "task",
  goal: "Research topic X",
  assignedTo: "agent_researcher_001" // Address of AgentNode
};

// V2 AgentNode executes V1 TaskNode
await system.send(agentAddress, {
  type: "execute_task",
  payload: { taskId: "task_123" }
});
```

**Pattern 2: Knowledge as Data + Skills**
```typescript
// V1 KnowledgeNode (data)
const docNode = {
  type: "knowledge",
  category: "data",
  content: "Documentation about API X"
};

// V2 KnowledgeNode (executable skill)
const skillNode = {
  type: "knowledge",
  category: "executable",
  executionType: "skill",
  skillDefinition: "patterns/api_usage.md"
};
```

**Pattern 3: Tool Reference → Tool Invocation**
```typescript
// V1 TaskNode references tool by string
const task = {
  type: "task",
  toolsRequired: ["web_search", "code_analyzer"]
};

// V2 ToolNodes are addressable
const searchTool = await graph.getNode("tool_web_search");
const result = await system.send(searchTool.address, {
  type: "search",
  payload: { query: "..." }
});
```

---

## Coexistence Strategy

### 1. Type-Based Routing

```typescript
function dispatchMessage(nodeId: string, message: Message) {
  const node = graph.getNode(nodeId);

  if (node.category === "data") {
    // V1 path: route to node's actor
    return system.send(node.address, message);
  } else if (node.category === "executable") {
    // V2 path: route with location awareness
    return routingLayer.send(node.address, node.location, message);
  }
}
```

### 2. Dual Registration

All nodes (V1 and V2) are:
- Registered in Graph (string ID, edges, properties)
- Registered in Actor System (Address)
- Type-tagged (category: data | executable)

### 3. Capability Discovery

```typescript
// Query for data nodes
const tasks = graph.query({ category: "data", type: "task" });

// Query for executable nodes
const agents = graph.query({ category: "executable", type: "agent" });

// Query for local executables
const localTools = graph.query({
  category: "executable",
  location: "local"
});
```

### 4. Migration Path

**Phase 1**: V1 Only (Current State)
- All nodes are data entities
- Agents external

**Phase 2**: V1 + V2 Coexistence
- V1 nodes remain (tasks, knowledge)
- V2 nodes added (agents, tools)
- Type system distinguishes

**Phase 3**: V2 Dominant
- Most execution via V2 nodes
- V1 nodes for persistence only
- Fully distributed

---

## Design Principles

### Principle 1: Explicit Categorization
Every node declares its category: `data` or `executable`.

### Principle 2: Layer Independence
V1 and V2 layers operate independently. Neither depends on the other.

### Principle 3: Unified Foundation
Both layers use the same actor foundation (addresses, messaging).

### Principle 4: Opt-In Complexity
Systems can use V1 only (simple), V2 only (distributed), or hybrid (powerful).

### Principle 5: Location Transparency Constraint
Only `executable` nodes can be remote. Data nodes always local (for persistence).

---

## Recommendations

### For Task Management Use Cases
**Use V1 primarily:**
- TaskNodes for work tracking
- KnowledgeNodes for documentation
- Git-backed persistence
- Optional: V2 AgentNodes for execution

### For Distributed Agent Systems
**Use V2 primarily:**
- AgentNodes for autonomous work
- ToolNodes for capabilities
- Location transparency
- Optional: V1 TaskNodes for persistence

### For Hybrid Systems (Recommended)
**Use both layers:**
- V1: Define work structure, track state
- V2: Execute work, scale across infrastructure
- Integration: Task → Agent, Tool references → Tool invocations

---

## Next Steps

1. **task_28**: Design type system (category taxonomy)
2. **task_29**: Design routing layer (location dispatch)
3. **task_30**: Design integration patterns (V1↔V2 interactions)
4. **task_31**: Write formal hybrid specification
5. **task_32**: Create implementation roadmap

---

## Conclusion

V1 and V2 are not alternatives but **complementary perspectives** on the same actor foundation:
- **V1 = Data Layer**: Persistent domain entities
- **V2 = Execution Layer**: Location-transparent work units
- **Hybrid = Best of Both**: Persistent structure + distributed execution

The hybrid architecture enables:
- ✅ Clear domain modeling (V1)
- ✅ Scalable execution (V2)
- ✅ Location transparency (V2)
- ✅ Persistence and collaboration (V1)
- ✅ Unified messaging (shared foundation)
