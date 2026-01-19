# Fitness Function: SEAG Success Criteria

To validate our design, we must define what "good" looks like for this Actor Graph.

## 1. Correctness & Repeatability
- **Deterministic Replay:** Given the same event stream, the algorithmic nodes must be able to reconstruct the graph state identically (for deterministic nodes).
- **Auditability:** Every change to a node or edge must be traceable back to a specific message and event.

## 2. Understandability
- **Self-Description:** Any node in the graph can be queried for its "spec" (what messages it accepts and what its current role is).
- **Visualizable:** The graph topology must be exportable to a format (like D2 or Mermaid) that humans can reason about.

## 3. Efficiency
- **Latency:** Interactive nodes (e.g., Chatbot UI nodes) must respond within <100ms.
- **Resource Usage:** Inactive nodes should be passivated (Virtual Actor pattern) to minimize memory footprint.
- **Selective Bypassing:** High-frequency data nodes (e.g., embeddings) should allow direct read access if the actor overhead exceeds 10% of total processing time.

## 4. Resilience (The "World" Boundary)
- **Degraded Gracefully:** If an external API node (Effect Actor) is unreachable, the graph remains functional for other paths, and the supervisor attempts exponential backoff.
- **Location Transparency:** Moving a node from one runtime to another must not break existing edges/subscriptions.

## 5. Agency
- **Proactivity:** Algorithmic nodes must be capable of initiating messages (autonomy) rather than just responding to external triggers.
