# Specification: The Agentic REPL

## 1. The Multi-Channel Strategy
To support Web, Terminal, and SMS simultaneously, we use the **Transport Adapter** pattern.
- **Frontend:** A native "Shadow Actor" (Custom Element / Web Component) that renders the current conversation graph.
- **Backend:** A "REPL Coordinator" actor that manages the dialogue state.

## 2. The User-as-Actor Protocol
The Human is represented by a `UserActor`. All interactions are messages:

| Message | Origin | Destination | Payload |
|---------|--------|-------------|---------|
| `INPUT` | UI | UserProxy | `{ text: "...", metadata: {...} }` |
| `OUTPUT`| Agent | UI | `{ content: "...", type: "text|image|graph" }` |
| `SIGNAL`| System | UI | `{ status: "thinking|typing|error", detail: "..." }` |

## 3. The REPL Command Set
The BrainAgent implements an intent-parsing layer for the following commands:

- **`mount <path>`**: Instructs the `FileEffectActor` to read a file and the `DocumentParser` to shred it into the graph.
- **`watch <path>`**: Establishes two-way sync, allowing the graph to reconcile external disk changes automatically.
- **`explore <address>`**: Performs a graph query (via `GraphProjector`) to discover reachable nodes.
- **`get <address>`**: Retrieves the current state of a specific actor.
- **`set <address> <value>`**: Sends a `PATCH` message to a specific fragment actor, triggering back-propagation to disk.
- **`help [query]`**: Lists available commands. If a query is provided, asks the Inference Agent for assistance.

## 4. The Short Path Implementation (Bun + TS)
1. **The Hub:** A `Bun.serve()` WebSocket server that acts as the `Gateway Actor`.
2. **The UI:** Standard HTML5/TS using WebSockets and ES Modules. No external frameworks.
3. **The Loop:**
   - User types → WebSocket → `Gateway` → `UserActor`.
   - `UserActor` sends `THINK` to `BrainAgent`.
   - `BrainAgent` queries the `Graph` and `Logs`.
   - `BrainAgent` sends `OUTPUT` back to `UserActor`.
   - `UserActor` pushes `OUTPUT` to the `Gateway` → WebSocket → Client.
