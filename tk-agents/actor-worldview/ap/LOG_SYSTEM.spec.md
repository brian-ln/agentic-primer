# Specification: The Log System

## 1. Concept: The Immutable Stream
A **Log Node** is a specialized Information Node that enforces **Immutability** and **Temporal Ordering**. It represents a "Write-Ahead Log" or "Event Store" for a subgraph.

## 2. Log Types
- **Command Log:** Captures "Intents" (e.g., `request_patch`, `execute_command`).
- **Event Log:** Captures "Facts" (e.g., `node_updated`, `computation_completed`).
- **Interaction Log:** Captures human-agent dialogue (e.g., this Chat Log).
- **Metric Log:** Captures temporal measurements (latency, resource usage).

## 3. Operations

| Operation | Action |
|-----------|--------|
| **Append** | Add a new entry. Generates a monotonically increasing `SequenceID`. |
| **Stream** | Subscribe to new entries starting from a `SequenceID`. |
| **Replay** | Read entries within a range `[start, end]`. |
| **Fold** | Project the log into a "Snapshot" or "State" node. |
| **Compact** | Archive old entries and replace them with a "Snapshot" node to save space. |

## 4. Derived Logs (Synthesis)
A Log can be the **source** for another Log.
- **Example:** A `RawInteractionLog` is processed by an `AnalyticAgent` which emits entries into a `SummaryLog`.
- This creates a **Lineage Graph** of information.
