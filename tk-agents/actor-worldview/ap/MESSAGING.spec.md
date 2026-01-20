# Specification: Messaging Patterns (Queues & Pub/Sub)

## 1. The Pub/Sub Pattern (Topic Nodes)
A **Topic Node** acts as a message relay. It decouples the "Producer" from the "Consumers."

- **Behavior:** When it receives a `publish` message, it broadcasts the payload to all currently `subscribed` actors.
- **Persistence:** Can be "Volatile" (memory-only) or "Durable" (backed by a Log Node).
- **Filtering:** Supports "Server-side" filtering so consumers only receive relevant signals.

## 2. The Queue Pattern (Work Queues)
A **Queue Node** manages a backlog of work to be distributed among "Workers."

- **Behavior:** Implements the "Competing Consumers" pattern. Unlike Pub/Sub, a message in a Queue is intended to be processed by **exactly one** worker.
- **Acknowledgement:** Supports `ack`/`nack` to ensure reliable processing.
- **Leasing:** When a worker receives a task, it is "leased" (made invisible). If no `ack` is received within the `lease_timeout`, the task is automatically returned to the backlog.
- **Reliability:** Guaranteed delivery. If a message fails `max_retries`, it can be moved to a Dead Letter Queue (DLQ).

## 3. Comparison in the Graph

| Feature | Topic Node | Queue Node |
|---------|------------|------------|
| **Delivery** | One-to-Many (Broadcast) | One-to-One (Distribution) |
| **State** | Stateless (usually) | Stateful (Backlog) |
| **Typical Use**| Notifications, Signals | Task Processing, Load Leveling |
| **Reliability** | Best Effort | Guaranteed (via Acks) |

## 4. Logical Addressing
- `seag://topics/<topic_name>`
- `seag://queues/<queue_name>`
