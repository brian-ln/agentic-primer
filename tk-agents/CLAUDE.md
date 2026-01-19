
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

### Post-Compaction Agent Scheduling

**CRITICAL: After conversation compaction, immediately resume agent scheduling.**

When a conversation is compacted (context summarized), you MUST proactively maintain agent capacity:

**Target Capacity:** Up to 10 concurrent background agents

**Post-Compaction Workflow:**
1. Check current agent count (how many running?)
2. Query for ready work: P0-P1 tasks in `created` state
3. Launch agents to fill capacity (up to 10 total)
4. Don't wait for user prompting - be proactive!

**Finding Ready Work:**
```bash
# Find P0-P1 tasks ready to work
bun src/cli/task.ts list --all | grep "^⭕ P[01]"

# Exclude test tasks and reviews needing human input
# Focus on: implementation, design, research, analysis
```

**Launch Pattern:**
- Launch agents in parallel to fill open slots
- Maintain 10 concurrent as the target operating mode
- Monitor for completions and backfill immediately
- Prioritize P0 over P1 when choosing work

**Why This Matters:**
- Compaction can disrupt agent scheduling momentum
- User expects continuous high-throughput work
- 10-agent concurrency is optimal for this project
- Idle capacity is wasted opportunity

**Example Post-Compaction:**
```
[Compaction occurs]
→ Check: 3 agents running, 7 slots available
→ Query: Found 12 P0-P1 tasks ready
→ Launch: 7 new agents to reach 10/10 capacity
→ Result: Pipeline full, work continues
```

**Remember:** The 10-agent limit is a target to maintain, not a ceiling to avoid.

### Controlling Auto-Mode

**Toggle automatic agent launches:**

```bash
# Disable auto-mode (manual control)
bun src/cli/auto.ts off

# Enable auto-mode (automatic scheduling)
bun src/cli/auto.ts on

# Check current status
bun src/cli/auto.ts status

# Toggle current state
bun src/cli/auto.ts toggle
```

**When to disable auto-mode:**
- You want to manually select which tasks to work on
- You're reorganizing or triaging the task queue
- You're in exploratory/planning mode
- You need to review task priorities before launching agents

**When to enable auto-mode:**
- You want continuous high-throughput work
- You trust the task queue prioritization
- You're comfortable with parallel agent execution
- You want to maximize agent capacity utilization

**Default:** Auto-mode is ENABLED by default (maintains current behavior)

**State persistence:** Auto-mode setting is stored in `~/.primer-config.json` and survives conversation compaction.

### Post-Compaction Hook Integration

**Hook Status:** Phase 2 - Informational Only

The post-compaction hook (`hooks/post-compaction.sh`) runs automatically after conversation compaction when enabled. It:

1. Checks auto-mode status from config
2. Queries for P0-P1 tasks in `created` state with no blockers
3. Calculates available agent capacity (target: 10)
4. Displays tasks that would be launched

**Current Behavior:**
- Hook displays what WOULD be launched
- Does NOT automatically launch agents (requires manual `/bg` commands)
- Respects auto-mode ON/OFF setting

**Manual Testing:**
```bash
# Test hook manually
bun src/hooks/post-compaction.ts

# Output shows:
# - Auto-mode status
# - Current agent count
# - Available capacity
# - Ready tasks list
```

**Hook Installation (Optional):**
```bash
# Copy to global hooks directory
mkdir -p ~/.claude/hooks
cp hooks/post-compaction.sh ~/.claude/hooks/
chmod +x ~/.claude/hooks/post-compaction.sh
```

**Future Enhancement:**
When full agent launch integration is implemented, the hook will automatically launch agents to fill capacity when auto-mode is enabled.

### Batch Operations for Multiple Tasks

When creating 3+ related tasks, use batch operations instead of individual commands:

```bash
# Create JSON file with task definitions
bun src/cli/task.ts batch-add --file tasks.json
```

**Benefits:**
- Atomic operation (all tasks created together)
- Version controllable
- Easy dependency specification
- Reduces command verbosity

**See:** `TASK_CLI_BATCH_OPERATIONS.md` for complete documentation, JSON schema, and examples.

**Quick example:**
```json
[
  {
    "goal": "Foundation task",
    "labels": ["project-x", "foundation"],
    "priority": 0
  },
  {
    "goal": "Dependent task",
    "labels": ["project-x", "feature"],
    "priority": 1,
    "depends": ["task_100"]
  }
]
```

---

## Troubleshooting

When debugging errors or issues, use systematic depth-first troubleshooting.

**See:** `TROUBLESHOOTING_PROTOCOL.md` for complete protocol

**Quick reference:**
- Try 3+ variations before pivoting to alternatives
- Document each attempt with exact commands and results
- Check official documentation/examples
- Reproduce in isolation before declaring failure
- Only pivot after exhausting current approach

**Example:**
```markdown
Attempt 1: Add init() call → Result: [exact outcome]
Attempt 2: Verify WASM file → Result: [exact outcome]
Attempt 3: Check documentation → Result: [found solution!]
```

**Anti-pattern:** "This doesn't work" → immediately try different approach
**Good pattern:** "Attempt 1 failed because X, trying variation 2..."

---

## Agent Completion Protocol

### CRITICAL: Create Review Task Before Announcing Completion

When you complete agent work:

1. **Create review task FIRST:**

```bash
bun src/cli/task.ts add "Review: <concise-description>" \
  --parent <your-agent-task-id> \
  --labels review,<work-type> \
  --priority <P0-P3> \
  --assignee bln
```

2. **Priority guide:**
   - P0: Blocks other work, critical path
   - P1: Feature work, significant changes
   - P2: Research, documentation (default)
   - P3: Experiments, drafts

3. **Announce completion WITH review task ID:**

```
COMPLETION: <work-description>

Deliverables:
- <file1> (<size>, <description>)
- <file2> (<size>, <description>)

Review Task: task_review_XX (<priority>, <labels>)

Status: COMPLETE - Ready for review
```

### Example

```bash
# Agent completed graph research
bun src/cli/task.ts add "Review: Graph query optimization research" \
  --parent task_agent_a8f57b5 \
  --labels review,research,graph \
  --priority P1 \
  --assignee bln

# Announce
COMPLETION: Graph query optimization research

Deliverables:
- GRAPH_QUERY_RESEARCH.md (57KB, 10 patterns)
- examples/ (5 query examples)

Review Task: task_review_21 (P1, research)

Status: COMPLETE - Ready for review
```

---

## Session Orientation

### Automatic Orientation on Session Start

**AUTOMATIC:** Orientation displays automatically when you start a new session or use `/clear`.

The session-start hook (`hooks/session-start`) runs automatically and shows you:
- Blocked tasks (need immediate attention)
- Ready work (P0-P1 unblocked tasks)
- Active tasks (currently in progress)
- Recently completed tasks (needs review)
- Project health snapshot
- Suggested next action

This happens **before your first message** - you see project context immediately.

### Manual Orientation Command

You can also get project context and actionable next steps anytime:

```bash
bun run orient
```

**What You See:**

```
📍 SESSION ORIENTATION
────────────────────────────────────────────────────────────

🚨 BLOCKERS (0)
   None - all clear!

⏭️  READY WORK (P0-P1: 5 tasks)
   P0  task_55  Implement blob storage actor
   P1  task_60  Design session management
   P1  task_62  Research query optimization
   ...

🔄 IN PROGRESS (2 tasks)
   task_50  Agent completion protocol (3h ago)
   task_58  Browser workbench testing (1h ago)

✅ COMPLETED RECENTLY (3 tasks)
   Review tasks pending:
   → task_review_23  Review: Graph query research (P1)
   → task_review_25  Review: Daemon architecture (P0)
   ...

📊 PROJECT HEALTH
   65 tasks total | 42 done (65%) | 8 active | 15 ready | 0 blocked

💡 SUGGESTED ACTION:
   Start task_55 (P0, Implement blob storage actor)

────────────────────────────────────────────────────────────
Type 'bun run orient' to refresh this view
```

### How to Use

**When to run `/orient`:**
- After session start or `/clear` to understand project state
- After completing work to see updated priorities
- When returning to project after break
- To get quick overview before starting work

**Priority Guidance:**
1. **Blockers first** - Unblock P0 tasks immediately
2. **P0 reviews next** - Approve/reject critical work
3. **P0-P1 ready work** - Start highest priority unblocked tasks
4. **Active tasks** - Continue in-progress work if context retained
5. **P2+ work** - Only if P0-P1 queue is empty

**Sections Explained:**
- 🚨 **BLOCKERS**: Tasks explicitly blocked, need unblocking first
- ⏭️ **READY WORK**: P0-P1 tasks with no blockers, ready to start
- 🔄 **IN PROGRESS**: Currently active tasks (shows duration)
- ✅ **COMPLETED RECENTLY**: Tasks done in last 24h, review tasks pending
- 📊 **PROJECT HEALTH**: High-level statistics and completion percentage
- 💡 **SUGGESTED ACTION**: Context-aware recommendation for what to do next

**Verbosity Levels (Phase 3):**

Control output detail level with flags:

```bash
# Quiet mode (minimal output, no footer)
bun run orient --quiet

# Normal mode (default, balanced output)
bun run orient

# Verbose mode (shows cache info, auto-mode status)
bun run orient --verbose

# Force refresh (ignore 5-minute cache)
bun run orient --no-cache
```

**Auto-Mode Integration (Phase 3):**

When auto-mode is enabled (via ~/.primer-config.json), orientation shows:

```
🤖 AUTO-MODE: Active
   Capacity: 3/10 agents running
   💡 7 agent slots available - consider launching ready work
```

This helps you see at-a-glance:
- Whether auto-mode is enabled
- Current vs target agent capacity
- Available slots for launching new agent work

**Smart Caching (Phase 3):**

Orientation caches results for 5 minutes to reduce file reads:
- First call loads fresh data from tasks.json
- Subsequent calls within 5 minutes use cached data
- Use `--no-cache` to force fresh data load
- Cache age shown in `--verbose` mode

**Integration with Task CLI:**

Orientation uses existing task commands - you can always drill down for details:

```bash
# See all ready tasks (not just P0-P1)
bun src/cli/task.ts ready

# See all active tasks
bun src/cli/task.ts list --status active

# See all review tasks
bun src/cli/task.ts list --label review

# See specific task details
bun src/cli/task.ts show <task_id>
```

**Configuration:**

Auto-mode settings are read from `~/.primer-config.json`:

```json
{
  "agentScheduling": {
    "autoMode": true,           // Enable/disable auto-mode
    "targetCapacity": 10,       // Target concurrent agents
    "minPriority": 1            // Minimum priority for auto-launch
  }
}
```

**See also:** `examples/orient-output-example.md` for detailed output explanation
