# Specification: Topology & Distribution

## 1. The System Hierarchy
We distinguish between the **Logical Graph** and the **Physical Infrastructure**.

- **Logical System:** A namespace and security boundary (e.g., `production`, `user_workspace`, `research_sandbox`).
- **Physical Runtime:** A Bun process, a Docker container, a remote server, or a browser tab.

## 2. Location Transparency & Addressing
Addresses are URI-based: `seag://<logical_system>/<actor_id>`.

- **Resolution:** The `Router Actor` maintains a **DHT (Distributed Hash Table)** or a central registry (in small systems) mapping `actor_id` to its current `Physical Runtime Address`.
- **Relocation:** Actors can migrate between runtimes. The `Router` handles the "Forwarding Pointer" during migration to prevent dropped messages.

## 3. Bonded & Entangled Actors
When actors must act as a single unit across a boundary:

- **Bonded Actors:** Two actors in different runtimes that share a "Tether". If one dies, the other is notified or terminated (Co-fate).
- **Entangled Actors:** State-synchronized actors. A `patch` on Actor A is automatically projected to Actor B's state via a high-speed sync protocol (Principle 7).

## 4. Addressing the Fallacies
We treat distributed fallacies as **Design Constraints** (Fitness Functions):

| Fallacy | Actor Mitigation |
|---------|------------------|
| **Unreliable Network** | **At-least-once delivery** with Idempotency keys. Supervision restarts the "Sender" if no `Ack` is received. |
| **Latency is not Zero** | **Actor Fusion:** If latency between System A and System B exceeds a threshold, the system moves (migrates) the actors to the same Physical Runtime. |
| **Bandwidth is Infinite** | **Selective Projection:** Don't sync the whole state; sync only the "Signals" (deltas) needed by the observer. |
| **Topology Changes** | **Dynamic Routing:** Addresses are logical; the "Spoon Manager" (Router) updates the physical mapping in real-time. |
