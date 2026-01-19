# Concept: Self-Evolving Actor Graph (SEAG)

## Vision
A unified system where every node and edge in a knowledge graph is a first-class **Actor**. This system treats information, computation, and external interfaces as a homogeneous graph of message-passing entities.

## Worldview Mapping

| Worldview Principle | SEAG Application |
|---------------------|------------------|
| **1. Graph Addressing** | Hierarchical paths (e.g., `graph.nodes.node_1`) are secondary to edge-based traversal and graph queries. |
| **3. Format-Agnostic** | Nodes can be JSON, binary blobs, or API responses; the actor interface masks the underlying serialization. |
| **5. Virtual Actors** | Nodes are instantiated on-demand when referenced by the graph or a query. |
| **6. External Boundaries** | Web APIs, files, and databases are wrapped as **Effect Actors** that appear as standard nodes. |
| **7. Design vs Implementation** | We design the graph as pure actors; we optimize by "fusing" chatty subgraphs or bypassing for high-speed reads. |

## Core Components

### 1. The Event Stream (The "Truth")
The graph is not a static database; it is a **projection**. Every change (NodeAdded, EdgeLinked, StateUpdated) is an event. The graph "replays" or "folds" this stream to maintain its current state.

### 2. Information Nodes/Edges
Actors representing state.
- **Messages:** `get_state`, `update_state`, `archive`, `link_to(target, edge_type)`.
- **Persistence:** Can be backed by CozoDB, S3, or local files.

### 3. Algorithmic Nodes (Automata)
Actors representing computation.
- **Deterministic:** Pure functions, state machines (Input A always leads to Output B).
- **Non-Deterministic:** LLM inference, Agentic loops, Real-time sensor data.
- **Messages:** `execute(params)`, `subscribe_to_output`.

### 4. Signaling & Observation
A pub/sub mechanism where any actor can `watch` another.
- **Pattern:** `subscribe(signal_type)` and `unsubscribe`.
- **Flow:** When an Information Node updates, it emits a signal to all observing Algorithmic Nodes, triggering reactive computation.
