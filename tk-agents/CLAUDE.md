
Default to using Bun instead of Node.js.

- Use `bun <file>` instead of `node <file>` or `ts-node <file>`
- Use `bun test` instead of `jest` or `vitest`
- Use `bun build <file.html|file.ts|file.css>` instead of `webpack` or `esbuild`
- Use `bun install` instead of `npm install` or `yarn install` or `pnpm install`
- Use `bun run <script>` instead of `npm run <script>` or `yarn run <script>` or `pnpm run <script>`
- Use `bunx <package> <command>` instead of `npx <package> <command>`
- Bun automatically loads .env, so don't use dotenv.

## APIs

- `Bun.serve()` supports WebSockets, HTTPS, and routes. Don't use `express`.
- `bun:sqlite` for SQLite. Don't use `better-sqlite3`.
- `Bun.redis` for Redis. Don't use `ioredis`.
- `Bun.sql` for Postgres. Don't use `pg` or `postgres.js`.
- `WebSocket` is built-in. Don't use `ws`.
- Prefer `Bun.file` over `node:fs`'s readFile/writeFile
- Bun.$`ls` instead of execa.

## Testing

Use `bun test` to run tests.

```ts#index.test.ts
import { test, expect } from "bun:test";

test("hello world", () => {
  expect(1).toBe(1);
});
```

## Frontend

Use HTML imports with `Bun.serve()`. Don't use `vite`. HTML imports fully support React, CSS, Tailwind.

Server:

```ts#index.ts
import index from "./index.html"

Bun.serve({
  routes: {
    "/": index,
    "/api/users/:id": {
      GET: (req) => {
        return new Response(JSON.stringify({ id: req.params.id }));
      },
    },
  },
  // optional websocket support
  websocket: {
    open: (ws) => {
      ws.send("Hello, world!");
    },
    message: (ws, message) => {
      ws.send(message);
    },
    close: (ws) => {
      // handle close
    }
  },
  development: {
    hmr: true,
    console: true,
  }
})
```

HTML files can import .tsx, .jsx or .js files directly and Bun's bundler will transpile & bundle automatically. `<link>` tags can point to stylesheets and Bun's CSS bundler will bundle.

```html#index.html
<html>
  <body>
    <h1>Hello, world!</h1>
    <script type="module" src="./frontend.tsx"></script>
  </body>
</html>
```

With the following `frontend.tsx`:

```tsx#frontend.tsx
import React from "react";
import { createRoot } from "react-dom/client";

// import .css files directly and it works
import './index.css';

const root = createRoot(document.body);

export default function Frontend() {
  return <h1>Hello, world!</h1>;
}

root.render(<Frontend />);
```

Then, run index.ts

```sh
bun --hot ./index.ts
```

For more information, read the Bun API docs in `node_modules/bun-types/docs/**.mdx`.

---

## Task Tracking for Agent Work

### CRITICAL REQUIREMENT

**All background agent work MUST be tracked in tasks.json before launching.**

This ensures:
- Work persists across conversation compactions
- Agent history is queryable
- Dependencies and blockers are visible
- Audit trail exists for all agent work

### Workflow Pattern

#### 1. BEFORE Launching Agent

Create a task that describes the agent's goal:

```bash
bun src/cli/task.ts add "Agent: <description>" \
  --labels agent,<type> \
  --priority <P0-P4>
```

**Labels:**
- Always include: `agent`
- Add type: `research`, `implementation`, `analysis`, `testing`, etc.

**Priority:**
- P0: Critical/blocking other work
- P1: High priority
- P2: Normal (default for most agent work)
- P3-P4: Lower priority/exploration

#### 2. Launch Agent

```bash
# Use /bg command or run_in_background
/bg <your agent prompt>
```

**Optional:** Reference task ID in commit message or notes

#### 3. AFTER Agent Completes

Update task status:

```bash
bun src/cli/task.ts update <task_id> complete
```

**If agent fails or gets blocked:**

```bash
bun src/cli/task.ts update <task_id> block "Reason for blocking"
```

### Sub-Agent Pattern

When a background agent spawns another agent:

```bash
# Parent agent creates child task
bun src/cli/task.ts add "Sub-agent: <description>" \
  --labels agent,child,<type> \
  --parent <parent_task_id> \
  --priority <inherit-from-parent>
```

This creates a `spawned_by` edge linking child → parent.

### Complete Example

```bash
# Research task
bun src/cli/task.ts add "Agent: Research Datalog query optimization patterns" \
  --labels agent,research \
  --priority P1

# Output: Added task: task_5

# Launch agent
/bg Research Datalog query optimization patterns for graph systems

# (Agent runs in background...)

# On completion
bun src/cli/task.ts update task_5 complete

# Output: Completed task task_5
```

### Query Agent Work

```bash
# List all agent tasks
bun src/cli/task.ts list --label agent

# Filter by type
bun src/cli/task.ts list --label research

# Show active agent work
bun src/cli/task.ts list --label agent --status active

# See agent dependency graph
bun src/cli/task.ts graph task_5
```

### Verification Checklist

Before launching ANY background agent, ask:
- [ ] Did I create a task first?
- [ ] Did I add the `agent` label?
- [ ] Did I set appropriate priority?
- [ ] For sub-agents: Did I link to parent task?

**If NO to any question: STOP and create the task first.**
