# Specification: The Agentic REPL

## 1. The Multi-Channel Strategy
To support Web, Terminal, and SMS simultaneously, we use the **Transport Adapter** pattern.
- **Frontend:** A "UI Actor" (React component or TTY state) that renders the current conversation graph.
- **Backend:** A "REPL Coordinator" actor that manages the dialogue state.

## 2. The User-as-Actor Protocol
The Human is represented by a `UserActor`. All interactions are messages:

| Message | Origin | Destination | Payload |
|---------|--------|-------------|---------|
| `input` | UI | UserProxy | `{ text: "...", metadata: {...} }` |
| `output`| Agent | UI | `{ content: "...", type: "text|image|graph" }` |
| `signal`| System | UI | `{ status: "thinking|typing|error" }` |

## 3. The Short Path Implementation (Bun + TS)
1. **The Hub:** A `Bun.serve()` WebSocket server that acts as the `Gateway Actor`.
2. **The Session:** Every connection spawns a `SessionActor` and a `UserActor`.
3. **The Loop:**
   - User types → WebSocket → `Gateway` → `UserActor`.
   - `UserActor` sends `process` to `AgentActor`.
   - `AgentActor` queries the `Graph` and `Logs`.
   - `AgentActor` sends `reply` back to `UserActor`.
   - `UserActor` pushes `output` to the `Gateway` → WebSocket → Client.

## 4. UI as an Actor
In the browser, the UI components are actors:
- **MessageList Actor:** Subscribes to the `InteractionLog` node and re-renders when a new entry is appended.
- **Input Actor:** Sends `input` messages to the `UserActor`.
- **SystemStatus Actor:** Watches the `AlgorithmicNode` for `status` signals.
