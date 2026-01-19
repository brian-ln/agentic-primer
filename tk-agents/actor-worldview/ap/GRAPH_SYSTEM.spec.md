# Specification: SEAG Protocol

## Actor Addressing
Nodes and edges follow the hierarchical-graph hybrid addressing:
- `seag.nodes.<id>`
- `seag.edges.<id>`
- `seag.subsystems.<name>.<id>`

## Protocol: Graph Operations

### Node Management
All nodes (Information or Algorithmic) must implement the **Base Node Protocol**:

| Message | Payload | Response | Description |
|---------|---------|----------|-------------|
| `inspect` | None | `NodeSpec` | Returns type, schema, and capabilities. |
| `get` | `keys[]` | `Data` | Retrieves state/properties. |
| `patch` | `updates{}` | `Ack` | Mutates properties (emits Event). |
| `watch` | `signal_filter` | `Stream` | Subscribes to changes/signals. |
| `unwatch`| `sub_id` | `Ack` | Cancels subscription. |

### Edge Management
Edges are actors that link two node addresses.

| Message | Payload | Response | Description |
|---------|---------|----------|-------------|
| `resolve` | None | `[From, To]` | Returns the linked nodes. |
| `traverse`| `direction` | `NodeAddress`| Returns the actor at the other end. |

## Protocol: Algorithmic Execution

| Message | Payload | Response | Description |
|---------|---------|----------|-------------|
| `call` | `input{}` | `Output` | Executes the function/program. |
| `status` | None | `Enum` | `idle`, `running`, `blocked`, `failed`. |

## Event Projection Engine
The "Graph Supervisor" is responsible for maintaining the projection.

1. **Capture:** Every `patch` or `link` message is sent to the `EventLog` actor.
2. **Commit:** The `EventLog` appends the event to a persistent store (e.g., `events.jsonl`).
3. **Notify:** The `EventBus` broadcasts the event to all subscribers.
4. **Project:** Specialized "Projector Actors" listen to the `EventBus` and update CozoDB or in-memory indexes to ensure queryability.

## Different Flavors of Algorithmic Nodes

1. **In-Process Function:** A simple JS/TS function wrapped in an actor.
2. **Process Wrapper:** An actor that spawns a shell command (e.g., `bun run ...`) and pipes stdin/stdout.
3. **Remote API:** An actor that wraps a `fetch` call to an external endpoint.
4. **Agentic Loop:** A complex actor that manages its own internal graph of sub-tasks.
