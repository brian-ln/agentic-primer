# Specification: Stability & Observability

## 1. Error Handling: The Autonomous Troubleshooting Loop

Actors must not just retry; they must **Self-Diagnose**.

### The Escalation Rubric
When an actor fails, the `RootSupervisor` follows this tiered rubric:

1. **Tier 1: Transient Retry (Auto)**
   - **Condition:** Network flicker, file lock, timeout.
   - **Action:** Exponential backoff (max 3 retries).
   - **Outcome:** Success or escalate to Tier 2.

2. **Tier 2: Autonomous Troubleshooting (Agentic)**
   - **Condition:** Retries failed, or error is "Systemic" (Config error, Dependency missing, Disk full).
   - **Action:** Spawn a `DiagnosisAgent`.
     - **Scan:** Read the `ErrorLog` and `SystemMetrics`.
     - **Compare:** Check `KnowledgeGraph` for similar historical failures.
     - **Repair:** Attempt resolution (e.g., clear cache, restart dependency, re-mount volume).
   - **Outcome:** Resolution confirmed via `Harness` or escalate to Tier 3.

3. **Tier 3: Degraded State (Containment)**
   - **Condition:** Repair failed or requires irreversible destructive action.
   - **Action:** Trip the circuit breaker. Mark the node as `degraded`. Route around the failure if possible.
   - **Outcome:** System remains operational in limited capacity.

4. **Tier 4: Human Intervention (Escalation)**
   - **Condition:** Tier 1-3 exhausted.
   - **Action:** Notify the human with a **Detailed Briefing**:
     - "What happened" (Trace ID + Error).
     - "What I tried" (Diagnosis Agent log).
     - "Why I stopped" (Rubric threshold reached).
     - "Proposed Next Steps" (Interactive options).

## 2. Infinite Loop Avoidance (The "TTL" for Messages)
To prevent circular message paths from consuming all CPU:
- **Message TTL:** Every `Envelope` has a `hop_count` (max 100). If it exceeds the limit, it is dropped as a "Cyclic Dead Letter."
- **Rate Limiting:** Every `System` tracks the message throughput per actor. If an actor exceeds 10k msgs/sec, it is throttled.

## 3. Observability: The "Glass Box" Principle
The system must be transparent without overhead.
- **Trace IDs:** Every message carries a `trace_id` originating from the user input. This allows us to visualize the "Causal Graph" of a request.
- **Health Signals:** Every actor periodically emits a `heartbeat` signal to its supervisor.
- **The Inspect Protocol:** Any actor can be `inspected` for its mailbox size and average processing time.

### 3.1 On-Demand Tracing (The "Colored Token")
To debug complex interactions without enabling global logging, we support **Per-Request Tracing**.

1.  **The Token:** The `Message` protocol supports a `meta.trace` boolean flag.
2.  **Propagation:**
    -   If an actor receives a message with `meta.trace = true`, any message it sends *as a result* of processing that input MUST also carry `meta.trace = true`.
    -   This creates a "Colored Path" through the graph.
3.  **Emission:**
    -   When the Kernel dispatches a traced message, it emits a `TRACE_SPAN` event to the `InteractionLog`.
    -   The span includes: `sender`, `target`, `message_type`, `timestamp`.
4.  **Visualization:**
    -   The `UserProxy` or `Gateway` subscribes to these spans to render a waterfall view for the user.

## 4. Resource Quotas
Actors represent "Work." Work requires "Budget."
- **Memory Quota:** Actors that exceed their allocated state size are passivated (virtualized to disk).
- **Compute Quota:** Algorithmic nodes (Inference/Agents) must have a `max_execution_time`.
