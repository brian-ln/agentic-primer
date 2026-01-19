# Specification: External Interface Boundaries

## 1. The Principle of Encapsulation
External systems are wrapped in **Effect Actors**. To the graph, a "Shell" or a "Browser" is just a node that accepts specific protocol messages and emits signals.

## 2. The Process Node (CLI / Shell / Scripts)
Wraps a native OS process (e.g., `bash`, `python`, `gemini-cli`).
- **Input:** `stdin` stream, environment variables, arguments.
- **Output:** `stdout` and `stderr` streams, exit code.
- **Log Integration:** Every line of output is automatically appended to a linked **Log Node** for historical analysis.

## 3. The Terminal Node (TTY / Interactive)
A stateful wrapper for interactive sessions.
- **State:** Terminal buffer, cursor position, ANSI state.
- **Interaction:** Handles the duplex stream of characters and control sequences.
- **Virtualization:** Can be "headed" (attached to a real TTY) or "headless" (virtualized buffer).

## 4. The Browser Node (Headless / Agentic)
Wraps a web engine (e.g., Playwright/Puppeteer).
- **DOM Projection:** The active page's DOM is projected as a sub-graph of nodes.
- **Actions:** `click(selector)`, `type(text)`, `navigate(url)`.
- **Observation:** Emits signals for `console_log`, `network_request`, and `navigation_completed`.
- **Agentic Layer:** Can be controlled by an **Algorithmic Node** that "reads" the projected DOM graph to make decisions.

## 5. Security & Capabilities
Because these actors can affect the real world, they must have **Capability Tokens**.
- A node cannot send a `call` message to a `ShellNode` unless it possesses the `os.execute` capability.
