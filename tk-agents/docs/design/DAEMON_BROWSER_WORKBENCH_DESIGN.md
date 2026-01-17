# Daemon + Browser Workbench Design

**Created:** 2026-01-16 19:38 EST (as of `date`)
**Version:** 1.0
**Status:** Design Specification
**Author:** Background Subagent (Design)

---

## Executive Summary

### Vision

Transform tk-agents from a CLI-only tool into a **hybrid CLI + browser workbench** inspired by Claude Cowork's interface, providing:

- **Real-time visibility** into active agents, pending reviews, and completed work
- **Browser-based review workflow** with inline file preview and approve/reject actions
- **macOS notification integration** to alert user when reviews need attention
- **Hot graph daemon** keeping state in-memory for instant CLI + browser access
- **Event streaming** for live updates as agents complete tasks

### User Experience Goals

**Before (Current CLI-only):**
```bash
# User has no visibility
bln: "What agents are running?"
bln: "What needs my review?"
bln: "Where were we?"
# Must grep through conversation history
```

**After (Daemon + Browser):**
```bash
# Terminal: Daemon running in background
primer daemon start
# Daemon started on http://localhost:3000
# Browser opens automatically

# Browser shows:
┌─────────────────────────────────────────────┐
│  tk-agents Workbench           [bln@local]  │
├─────────────────────────────────────────────┤
│ Dashboard  Active Agents  Reviews (7)  ...  │
├─────────────────────────────────────────────┤
│                                              │
│  NEEDS ATTENTION (3)                        │
│  ────────────────────────────────────────   │
│  🔴 P0 Review: Graph Persistence Design     │
│     → GRAPH_PERSISTENCE_DESIGN.md           │
│     → Blocking: task_18, task_19            │
│     [Approve] [Request Changes] [View]      │
│                                              │
│  🔴 P0 Review: Task Tracking Automation     │
│     → TASK_TRACKING_AUTOMATION.md           │
│     → Blocking: task_12                     │
│     [Approve] [Request Changes] [View]      │
│                                              │
│  ACTIVE AGENTS (2)                          │
│  ────────────────────────────────────────   │
│  🔄 Agent: CozoDB Knowledge Research        │
│     → Started 45 min ago                    │
│     → Output streaming: [View Live Log]     │
│                                              │
│  COMPLETED TODAY (5)                        │
│  ────────────────────────────────────────   │
│  ✅ Agent: Graph Query Research (2h ago)    │
│  ✅ Agent: CLI Specifications (4h ago)      │
│  ...                                         │
└─────────────────────────────────────────────┘

# macOS notification pops up:
┌──────────────────────────────┐
│ tk-agents Workbench          │
│ ──────────────────────────── │
│ New P0 Review Ready          │
│ Graph Persistence Design     │
│ Blocking 2 tasks             │
│                              │
│ [View] [Dismiss]             │
└──────────────────────────────┘
# Clicking "View" opens browser to review page
```

### Key Decisions

1. **Daemon-First Architecture**: Keep graph hot in-memory, expose via HTTP + WebSocket
2. **Browser as Primary UI**: Rich interface for reviews, dashboards, history
3. **CLI as Control Plane**: Start/stop daemon, quick queries, CI/CD integration
4. **Event Sourcing**: Use existing EventLog for audit trail and replay
5. **Bun Stack**: Leverage Bun.serve() for HTTP + WebSocket (no Express needed)
6. **Notification Modes**: Auto-open browser (default) or notification-only (configurable)

---

## Architecture Overview

### System Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│  User                                                       │
│    │                                                        │
│    ├─► CLI (primer daemon start/stop/status)               │
│    ├─► CLI (primer task/graph/knowledge) ──┐               │
│    └─► Browser (http://localhost:3000)     │               │
│                                             │               │
└─────────────────────────────────────────────┼───────────────┘
                                              │
                    ┌─────────────────────────┴────────────┐
                    │                                      │
                    │  DAEMON (Bun Process)                │
                    │  ────────────────────────────────    │
                    │  • HTTP Server (REST API)            │
                    │  • WebSocket Server (events)         │
                    │  • Hot Graph (in-memory)             │
                    │  • EventLog (append-only JSONL)      │
                    │  • Notification Manager (macOS)      │
                    │  • File Watcher (tasks.json)         │
                    │                                      │
                    └──────────────┬───────────────────────┘
                                   │
         ┌─────────────────────────┼──────────────────────┐
         │                         │                      │
         ▼                         ▼                      ▼
   tasks.json              events.jsonl           Browser Clients
   (persist)               (event log)            (WebSocket)
```

### Data Flow

**CLI Command Flow:**
```
CLI: task add "Goal"
  ↓
Daemon API: POST /api/tasks
  ↓
Graph: TaskActor.create()
  ↓
EventLog: append({type: "task_created", ...})
  ↓
Broadcast: WebSocket.send(all_clients, event)
  ↓
Browser: Update UI (optimistic + confirmed)
  ↓
Persist: Save tasks.json
```

**Agent Completion Flow:**
```
Agent: Completes task_5
  ↓
CLI: task update task_5 complete
  ↓
Daemon API: PUT /api/tasks/task_5
  ↓
Graph: Update task state
  ↓
EventLog: append({type: "task_completed", ...})
  ↓
Daemon: Create review task (if deliverables exist)
  ↓
EventLog: append({type: "task_created", ...})
  ↓
Notification: Send macOS notification
  ↓
Browser: Auto-open to /reviews/task_18 (if configured)
  ↓
Broadcast: WebSocket.send(all_clients, events)
  ↓
Browser: Show new review in dashboard
```

### Component Responsibilities

| Component | Responsibility | Technology |
|-----------|----------------|------------|
| **Daemon** | Keep graph hot, serve API, manage state | Bun runtime |
| **HTTP API** | REST endpoints for CRUD operations | Bun.serve() |
| **WebSocket** | Real-time event streaming | Bun WebSocket |
| **EventLog** | Audit trail, replay, event sourcing | JSONL (existing) |
| **Graph** | In-memory actor system, message routing | Existing Graph class |
| **Browser Client** | Rich UI for dashboards, reviews, history | HTML + React + Tailwind |
| **Notification Manager** | macOS notifications, browser auto-open | terminal-notifier |
| **CLI** | Control plane, quick queries, CI/CD | Existing task.ts/graph.ts |

---

## Daemon Design

### Process Management

**Lifecycle:**
```bash
# Start daemon
primer daemon start
# → Spawns Bun process in background
# → Loads tasks.json into Graph
# → Replays events.jsonl for audit trail
# → Starts HTTP server on port 3000
# → Opens browser to http://localhost:3000

# Check status
primer daemon status
# → Daemon running (PID 12345)
# → Uptime: 2h 15m
# → Graph loaded: 17 nodes, 15 edges
# → Connected clients: 2

# Stop daemon
primer daemon stop
# → Sending SIGTERM to PID 12345
# → Flushing EventLog
# → Saving tasks.json
# → Daemon stopped

# Restart (reload graph)
primer daemon restart
```

**PID File Management:**
```typescript
// .primer-daemon.pid stores PID
const PID_FILE = ".primer-daemon.pid";

function start() {
  if (existsSync(PID_FILE)) {
    throw new Error("Daemon already running");
  }

  const proc = spawn("bun", ["src/daemon/server.ts"], {
    detached: true,
    stdio: "ignore",
  });

  writeFileSync(PID_FILE, String(proc.pid));
  proc.unref(); // Detach from parent
}

function stop() {
  const pid = parseInt(readFileSync(PID_FILE, "utf-8"));
  process.kill(pid, "SIGTERM");
  unlinkSync(PID_FILE);
}
```

**Graceful Shutdown:**
```typescript
// daemon/server.ts
process.on("SIGTERM", async () => {
  console.log("Shutting down gracefully...");

  // 1. Close WebSocket connections
  for (const client of wsClients) {
    client.close();
  }

  // 2. Flush EventLog
  await eventLog.flush();

  // 3. Save graph
  await saveGraph(graph, TASKS_FILE);

  // 4. Exit
  process.exit(0);
});
```

### API Layer

#### REST Endpoints

**Tasks:**
```typescript
// GET /api/tasks
// List tasks with optional filters
interface TaskListQuery {
  status?: "created" | "active" | "blocked" | "completed";
  label?: string;
  priority?: 0 | 1 | 2 | 3 | 4;
  limit?: number;
  offset?: number;
}

// Response
interface TaskListResponse {
  tasks: TaskProperties[];
  total: number;
  page: { limit: number; offset: number };
}

// POST /api/tasks
// Create a new task
interface CreateTaskRequest {
  goal: string;
  deliverables?: string[];
  criteria?: ObjectiveCriterion[];
  labels?: string[];
  priority?: 0 | 1 | 2 | 3 | 4;
  parent?: string;
  depends?: string[];
}

// Response: { id: string; created: TaskProperties }

// GET /api/tasks/:id
// Get task details with edges
interface TaskDetailResponse {
  id: string;
  properties: TaskProperties;
  edges: Edge[];
  children: string[]; // Child tasks (spawned_by)
  dependencies: string[]; // Tasks this depends on
}

// PUT /api/tasks/:id
// Update task (start, complete, block)
interface UpdateTaskRequest {
  action: "start" | "complete" | "block";
  result?: string; // For complete
  reason?: string; // For block
}

// DELETE /api/tasks/:id
// Delete task and all edges
```

**Reviews:**
```typescript
// GET /api/reviews
// List pending reviews (tasks with "review" label)
interface ReviewListResponse {
  reviews: Array<{
    id: string;
    goal: string;
    priority: number;
    deliverables: string[];
    createdAt: string;
    blocking: string[]; // Task IDs blocked by this review
  }>;
}

// POST /api/reviews/:id/approve
// Approve a review (mark task complete)
interface ApproveReviewRequest {
  feedback?: string;
}

// POST /api/reviews/:id/reject
// Reject a review (block task, add feedback)
interface RejectReviewRequest {
  reason: string;
  requestedChanges: string[];
}
```

**Agents:**
```typescript
// GET /api/agents
// List active agents (tasks with "agent" label and state "active")
interface AgentListResponse {
  agents: Array<{
    id: string;
    goal: string;
    startedAt: string;
    duration: number; // seconds
    labels: string[];
  }>;
}

// GET /api/agents/:id/output
// Stream agent output (if available)
// Returns Server-Sent Events (SSE) stream
```

**Graph:**
```typescript
// GET /api/graph
// Get full graph dump
interface GraphDumpResponse {
  nodes: NodeProperties[];
  edges: Edge[];
  stats: {
    nodeCount: number;
    edgeCount: number;
    tasksByState: Record<TaskState, number>;
  };
}

// POST /api/graph/query
// Execute Datalog query (future: CozoDB integration)
interface GraphQueryRequest {
  query: string; // Datalog query
  params?: Record<string, unknown>;
}

// Response: { results: unknown[]; }
```

**Activity:**
```typescript
// GET /api/activity
// Get recent activity feed
interface ActivityFeedQuery {
  limit?: number; // default 20
  types?: string[]; // filter by event type
  since?: string; // ISO timestamp
}

interface ActivityFeedResponse {
  events: Array<{
    timestamp: string;
    type: string; // "task_created", "task_completed", etc.
    nodeId: string;
    summary: string; // Human-readable summary
    data: unknown; // Full event data
  }>;
}

// GET /api/history
// Get full history with filtering
interface HistoryQuery {
  startDate?: string;
  endDate?: string;
  types?: string[];
  nodeId?: string;
  limit?: number;
  offset?: number;
}
```

**System:**
```typescript
// GET /api/stats
// Dashboard statistics
interface StatsResponse {
  tasks: {
    total: number;
    byState: Record<TaskState, number>;
    byPriority: Record<string, number>;
  };
  agents: {
    active: number;
    completedToday: number;
    totalRunTime: number; // seconds
  };
  reviews: {
    pending: number;
    byPriority: Record<string, number>;
  };
  activity: {
    eventsToday: number;
    tasksCompletedToday: number;
  };
}

// GET /api/health
// Daemon health check
interface HealthResponse {
  status: "ok" | "degraded";
  uptime: number; // seconds
  graph: { nodes: number; edges: number };
  eventLog: { events: number; size: number };
}
```

#### WebSocket Events

**Server → Client:**

```typescript
// Task events
{
  type: "task.created",
  timestamp: "2026-01-16T19:30:00.000Z",
  data: {
    taskId: "task_18",
    goal: "Review: Graph Persistence Design",
    priority: 0,
    labels: ["review", "design"]
  }
}

{
  type: "task.updated",
  timestamp: "2026-01-16T19:31:00.000Z",
  data: {
    taskId: "task_5",
    state: "completed",
    result: "Completed successfully"
  }
}

{
  type: "task.deleted",
  timestamp: "2026-01-16T19:32:00.000Z",
  data: { taskId: "task_3" }
}

// Agent events
{
  type: "agent.started",
  timestamp: "2026-01-16T19:30:00.000Z",
  data: {
    taskId: "task_10",
    goal: "Agent: CozoDB Knowledge Research"
  }
}

{
  type: "agent.completed",
  timestamp: "2026-01-16T20:15:00.000Z",
  data: {
    taskId: "task_10",
    deliverables: ["DATALOG_COSODB_LIBSQL_KNOWLEDGE.md"],
    duration: 2700 // seconds
  }
}

// Review events
{
  type: "review.created",
  timestamp: "2026-01-16T20:15:30.000Z",
  data: {
    taskId: "task_18",
    priority: 0,
    deliverables: ["GRAPH_PERSISTENCE_DESIGN.md"],
    blocking: ["task_19", "task_20"]
  }
}

{
  type: "review.approved",
  timestamp: "2026-01-16T20:30:00.000Z",
  data: {
    taskId: "task_18",
    feedback: "Looks good!"
  }
}

// System events
{
  type: "daemon.started",
  timestamp: "2026-01-16T19:00:00.000Z",
  data: { version: "1.0" }
}
```

**Client → Server:**

```typescript
// Subscribe to event channels
{
  type: "subscribe",
  channels: ["tasks", "agents", "reviews", "system"]
}

// Unsubscribe from channels
{
  type: "unsubscribe",
  channels: ["agents"]
}

// Ping/pong for keepalive
{ type: "ping" }
// Response: { type: "pong" }
```

### State Management

**Hot Graph:**

```typescript
// daemon/state.ts
export class DaemonState {
  private graph: Graph;
  private eventLog: EventLog;
  private wsClients: Set<WebSocket> = new Set();

  constructor() {
    this.graph = new Graph();
    this.eventLog = new EventLog("events.jsonl");
  }

  // Load graph from tasks.json
  async load() {
    const dump = JSON.parse(readFileSync("tasks.json", "utf-8"));

    // Recreate nodes
    for (const nodeProps of dump.nodes) {
      if (nodeProps.type === "task") {
        TaskActor({ ...nodeProps, graph: this.graph });
      }
    }

    // Recreate edges
    for (const edge of dump.edges) {
      this.graph.addEdge(edge.fromId, edge.toId, edge.type, edge.properties);
    }

    // Replay event log for audit trail
    this.eventLog.replay((event) => {
      // Events already applied, just for audit
    });
  }

  // Save graph to tasks.json
  async save() {
    const dump = this.graph.dump();
    writeFileSync("tasks.json", JSON.stringify(dump, null, 2));
  }

  // Append event and broadcast
  async emit(type: string, nodeId: string, data: unknown) {
    // Append to event log
    this.eventLog.append({
      timestamp: new Date().toISOString(),
      type,
      nodeId,
      data,
    });

    // Broadcast to all WebSocket clients
    const message = JSON.stringify({
      type,
      timestamp: new Date().toISOString(),
      data,
    });

    for (const client of this.wsClients) {
      try {
        client.send(message);
      } catch (err) {
        // Remove dead clients
        this.wsClients.delete(client);
      }
    }

    // Auto-save after mutations
    await this.save();
  }

  // Register WebSocket client
  registerClient(ws: WebSocket) {
    this.wsClients.add(ws);

    ws.on("close", () => {
      this.wsClients.delete(ws);
    });
  }

  getGraph() {
    return this.graph;
  }
}
```

**Event Sourcing Integration:**

```typescript
// Use existing EventLog for audit trail
// All mutations → append to event log → broadcast to clients

// Example: Task creation
async function createTask(goal: string, options: CreateTaskOptions) {
  // 1. Create task actor
  TaskActor({ goal, ...options, graph: state.getGraph() });

  // 2. Get task ID
  const taskId = graph.getNodeIds()[graph.getNodeIds().length - 1];

  // 3. Emit event (appends to log + broadcasts)
  await state.emit("task.created", taskId, {
    taskId,
    goal,
    labels: options.labels,
    priority: options.priority,
  });

  return taskId;
}
```

### Multi-client Support

**CLI Coordination:**

```typescript
// CLI commands send HTTP requests to daemon
// daemon/client.ts - used by CLI

export class DaemonClient {
  private baseUrl = "http://localhost:3000";

  async createTask(goal: string, options: CreateTaskOptions) {
    const response = await fetch(`${this.baseUrl}/api/tasks`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ goal, ...options }),
    });

    return response.json();
  }

  async listTasks(filters?: TaskListQuery) {
    const params = new URLSearchParams(filters as any);
    const response = await fetch(`${this.baseUrl}/api/tasks?${params}`);
    return response.json();
  }
}

// CLI uses client if daemon is running
// cli/task.ts
const client = new DaemonClient();

if (await isDaemonRunning()) {
  // Use daemon API
  await client.createTask(goal, options);
} else {
  // Fallback: direct file manipulation (existing behavior)
  const graph = await loadGraph(TASKS_FILE);
  TaskActor({ goal, ...options, graph });
  await saveGraph(graph, TASKS_FILE);
}
```

**Browser Coordination:**

```typescript
// Browser maintains WebSocket connection
// Receives live updates from daemon
// Sends mutations via REST API

class WorkbenchClient {
  private ws: WebSocket;
  private baseUrl = "http://localhost:3000";

  constructor() {
    this.connectWebSocket();
  }

  connectWebSocket() {
    this.ws = new WebSocket("ws://localhost:3000/ws");

    this.ws.onmessage = (event) => {
      const message = JSON.parse(event.data);
      this.handleEvent(message);
    };

    this.ws.onopen = () => {
      // Subscribe to all channels
      this.ws.send(JSON.stringify({
        type: "subscribe",
        channels: ["tasks", "agents", "reviews", "system"],
      }));
    };
  }

  handleEvent(event: any) {
    // Update UI based on event type
    switch (event.type) {
      case "task.created":
        taskStore.addTask(event.data);
        break;
      case "task.updated":
        taskStore.updateTask(event.data.taskId, event.data);
        break;
      case "review.created":
        reviewStore.addReview(event.data);
        showNotification("New Review", event.data);
        break;
    }
  }

  async createTask(goal: string, options: CreateTaskOptions) {
    const response = await fetch(`${this.baseUrl}/api/tasks`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ goal, ...options }),
    });

    return response.json();
  }
}
```

---

## Browser Interface Design

### Layout & Navigation

**Responsive Layout:**

```
┌─────────────────────────────────────────────────────────────┐
│  Header: tk-agents Workbench        [Stats] [@bln] [Help]   │
├─────────────────────────────────────────────────────────────┤
│ Sidebar       │  Main Content Area                          │
│               │                                              │
│ 📊 Dashboard  │  ┌────────────────────────────────────────┐ │
│ 🔄 Active (2) │  │  Component renders here based on route │ │
│ 📋 Reviews(7) │  │                                        │ │
│ 📝 Tasks      │  │  (Dashboard, Active Agents, Reviews,   │ │
│ 📅 History    │  │   Tasks, History, etc.)                │ │
│ ⚙️  Settings   │  │                                        │ │
│               │  └────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

**Component Tree:**

```typescript
<App>
  <Header>
    <Logo />
    <QuickStats />
    <UserMenu />
  </Header>

  <Layout>
    <Sidebar>
      <NavLink to="/dashboard">Dashboard</NavLink>
      <NavLink to="/agents">Active Agents (2)</NavLink>
      <NavLink to="/reviews" badge={7}>Reviews</NavLink>
      <NavLink to="/tasks">Tasks</NavLink>
      <NavLink to="/history">History</NavLink>
      <NavLink to="/settings">Settings</NavLink>
    </Sidebar>

    <Main>
      <Routes>
        <Route path="/dashboard" element={<Dashboard />} />
        <Route path="/agents" element={<ActiveAgents />} />
        <Route path="/reviews" element={<Reviews />} />
        <Route path="/reviews/:id" element={<ReviewDetail />} />
        <Route path="/tasks" element={<Tasks />} />
        <Route path="/tasks/:id" element={<TaskDetail />} />
        <Route path="/history" element={<History />} />
        <Route path="/settings" element={<Settings />} />
      </Routes>
    </Main>
  </Layout>
</App>
```

### Dashboard View

**Purpose:** Landing page showing prioritized summary of work.

**Wireframe:**

```
╔═══════════════════════════════════════════════════════════╗
║  Dashboard                                    Updated 2s  ║
╚═══════════════════════════════════════════════════════════╝

┌───────────────────────────────────────────────────────────┐
│ NEEDS YOUR ATTENTION (3)                      [View All →] │
├───────────────────────────────────────────────────────────┤
│                                                            │
│  🔴 P0 Review: Graph Persistence Design                   │
│      → Deliverables: GRAPH_PERSISTENCE_DESIGN.md          │
│      → Blocking: task_18, task_19                         │
│      → Created: 45 minutes ago                            │
│      [Approve] [Request Changes] [View Details]           │
│                                                            │
│  🔴 P0 Review: Task Tracking Automation                   │
│      → Deliverables: TASK_TRACKING_AUTOMATION.md, hooks/  │
│      → Blocking: task_12                                  │
│      → Created: 1 hour ago                                │
│      [Approve] [Request Changes] [View Details]           │
│                                                            │
│  🟡 P1 Review: Graph Query Research                       │
│      → Deliverables: GRAPH_QUERY_RESEARCH.md              │
│      → Not blocking anything                              │
│      → Created: 2 hours ago                               │
│      [Approve] [Request Changes] [View Details]           │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ ACTIVE AGENTS (2)                             [View All →] │
├───────────────────────────────────────────────────────────┤
│                                                            │
│  🔄 Agent: CozoDB Knowledge Research                      │
│      → Started: 45 minutes ago                            │
│      → Labels: agent, research                            │
│      [View Live Output]                                   │
│                                                            │
│  🔄 Agent: Daemon Browser Workbench Design                │
│      → Started: 2 minutes ago                             │
│      → Labels: agent, design                              │
│      [View Live Output]                                   │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ RECENT COMPLETIONS (5)                        [View All →] │
├───────────────────────────────────────────────────────────┤
│                                                            │
│  ✅ Agent: Task Tracking Automation (2 hours ago)         │
│  ✅ Agent: Graph Query Research (4 hours ago)             │
│  ✅ Agent: CLI Specifications (5 hours ago)               │
│  ✅ Task: Test batch operations (6 hours ago)             │
│  ✅ Task: Add priority field (7 hours ago)                │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌─────────────────────┬─────────────────────┬───────────────┐
│ QUICK STATS         │                     │               │
├─────────────────────┼─────────────────────┼───────────────┤
│ Tasks               │ Agents Today        │ Activity      │
│  Active: 12         │  Completed: 5       │  Events: 42   │
│  Blocked: 3         │  Running: 2         │  Since: 7h    │
│  Completed: 45      │  Failed: 0          │               │
└─────────────────────┴─────────────────────┴───────────────┘
```

**Component Implementation:**

```typescript
// components/Dashboard.tsx
export function Dashboard() {
  const stats = useStats(); // Fetches /api/stats
  const needsAttention = useReviews({ limit: 3, sort: "priority" });
  const activeAgents = useAgents({ state: "active", limit: 2 });
  const recentCompletions = useActivity({ types: ["task.completed"], limit: 5 });

  return (
    <div className="dashboard">
      <DashboardHeader />

      <Section title="NEEDS YOUR ATTENTION" count={needsAttention.total}>
        {needsAttention.data.map(review => (
          <ReviewCard key={review.id} review={review} compact />
        ))}
        <Link to="/reviews">View All →</Link>
      </Section>

      <Section title="ACTIVE AGENTS" count={activeAgents.total}>
        {activeAgents.data.map(agent => (
          <AgentCard key={agent.id} agent={agent} />
        ))}
        <Link to="/agents">View All →</Link>
      </Section>

      <Section title="RECENT COMPLETIONS" count={recentCompletions.total}>
        <Timeline events={recentCompletions.data} />
        <Link to="/history">View All →</Link>
      </Section>

      <QuickStats stats={stats} />
    </div>
  );
}
```

### Reviews View (Critical!)

**Purpose:** Show all pending reviews with priority sorting, inline preview, approve/reject actions.

**Wireframe:**

```
╔═══════════════════════════════════════════════════════════╗
║  Reviews (7 pending)                                      ║
╚═══════════════════════════════════════════════════════════╝

Filters: [All] [P0] [P1] [P2] [P3]   Sort: [Priority ▼]

┌───────────────────────────────────────────────────────────┐
│ 🔴 P0 Review: Graph Persistence Design            task_18 │
├───────────────────────────────────────────────────────────┤
│ Goal: Review Graph Persistence Design deliverables        │
│ Created: 45 minutes ago                                   │
│ Blocking: 2 tasks (task_19, task_20)                      │
│                                                            │
│ Deliverables:                                             │
│  📄 GRAPH_PERSISTENCE_DESIGN.md (34.8 KB)                 │
│     [Preview] [Download] [View in Editor]                 │
│                                                            │
│ Actions:                                                  │
│  [✅ Approve]  [📝 Request Changes]  [🗑️ Reject]          │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ 🔴 P0 Review: Task Tracking Automation            task_17 │
├───────────────────────────────────────────────────────────┤
│ Goal: Review Task Tracking Automation design and hooks    │
│ Created: 1 hour ago                                       │
│ Blocking: 1 task (task_12)                                │
│                                                            │
│ Deliverables:                                             │
│  📄 TASK_TRACKING_AUTOMATION.md (19.4 KB)                 │
│     [Preview] [Download] [View in Editor]                 │
│  📂 hooks/ (5 files)                                      │
│     [View Files]                                          │
│                                                            │
│ Actions:                                                  │
│  [✅ Approve]  [📝 Request Changes]  [🗑️ Reject]          │
│                                                            │
└───────────────────────────────────────────────────────────┘

(More reviews...)
```

**Review Detail Page (Click "Preview" or "View Details"):**

```
╔═══════════════════════════════════════════════════════════╗
║  Review: Graph Persistence Design                 task_18 ║
╚═══════════════════════════════════════════════════════════╝

[← Back to Reviews]

┌───────────────────────────────────────────────────────────┐
│ REVIEW INFORMATION                                        │
├───────────────────────────────────────────────────────────┤
│ Priority: P0                                              │
│ Status: Pending Review                                    │
│ Created: 2026-01-16 19:15 EST (45 minutes ago)            │
│ Blocking: task_19, task_20                                │
│ Parent Task: task_8 (Agent: CozoDB Research)              │
│ Labels: review, design                                    │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ DELIVERABLES                                              │
├───────────────────────────────────────────────────────────┤
│                                                            │
│ 📄 GRAPH_PERSISTENCE_DESIGN.md (34.8 KB)                  │
│    [Preview] [Download] [View in Editor]                  │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ FILE PREVIEW: GRAPH_PERSISTENCE_DESIGN.md                 │
├───────────────────────────────────────────────────────────┤
│                                                            │
│ # Graph Persistence Design                                │
│                                                            │
│ **Created:** 2026-01-16 19:15 EST                         │
│ **Version:** 1.0                                          │
│ **Status:** Design Specification                          │
│                                                            │
│ ## Executive Summary                                      │
│                                                            │
│ This document proposes adding persistent storage...       │
│                                                            │
│ (Markdown rendered with syntax highlighting)              │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ REVIEW ACTIONS                                            │
├───────────────────────────────────────────────────────────┤
│                                                            │
│ ✅ Approve                                                │
│    Mark this review as approved and complete              │
│    [Add Feedback (optional)]                              │
│    ┌────────────────────────────────────────────────────┐ │
│    │ Looks good! Proceed with implementation.           │ │
│    └────────────────────────────────────────────────────┘ │
│    [Submit Approval]                                      │
│                                                            │
│ ─────────────────────────────────────────────────────────  │
│                                                            │
│ 📝 Request Changes                                        │
│    Block this review and request specific changes         │
│    [Requested Changes (required)]                         │
│    ┌────────────────────────────────────────────────────┐ │
│    │ 1. Add CozoDB benchmarks                           │ │
│    │ 2. Include migration path from JSON                │ │
│    │ 3. Add rollback strategy                           │ │
│    └────────────────────────────────────────────────────┘ │
│    [Submit Change Request]                                │
│                                                            │
│ ─────────────────────────────────────────────────────────  │
│                                                            │
│ 🗑️ Reject                                                 │
│    Reject this work entirely (requires explanation)       │
│    [Reason for Rejection (required)]                      │
│    ┌────────────────────────────────────────────────────┐ │
│    │                                                    │ │
│    └────────────────────────────────────────────────────┘ │
│    [Submit Rejection]                                     │
│                                                            │
└───────────────────────────────────────────────────────────┘
```

**Component Implementation:**

```typescript
// components/Reviews.tsx
export function Reviews() {
  const [filter, setFilter] = useState<{ priority?: number }>({});
  const reviews = useReviews(filter);

  return (
    <div className="reviews">
      <ReviewsHeader count={reviews.total} />

      <Filters>
        <Button onClick={() => setFilter({})}>All</Button>
        <Button onClick={() => setFilter({ priority: 0 })}>P0</Button>
        <Button onClick={() => setFilter({ priority: 1 })}>P1</Button>
        <Button onClick={() => setFilter({ priority: 2 })}>P2</Button>
        <Button onClick={() => setFilter({ priority: 3 })}>P3</Button>
      </Filters>

      <ReviewList reviews={reviews.data} />
    </div>
  );
}

// components/ReviewCard.tsx
export function ReviewCard({ review }: { review: Review }) {
  return (
    <Card>
      <CardHeader>
        <PriorityBadge priority={review.priority} />
        <h3>{review.goal}</h3>
        <span className="task-id">{review.id}</span>
      </CardHeader>

      <CardBody>
        <MetaInfo>
          <span>Created: {formatRelative(review.createdAt)}</span>
          {review.blocking.length > 0 && (
            <span className="blocking">
              Blocking: {review.blocking.length} tasks
            </span>
          )}
        </MetaInfo>

        <Deliverables>
          {review.deliverables.map(file => (
            <FileItem key={file}>
              <FileIcon type={getFileType(file)} />
              <span>{file}</span>
              <FileActions>
                <Button size="sm" onClick={() => previewFile(file)}>Preview</Button>
                <Button size="sm" onClick={() => downloadFile(file)}>Download</Button>
              </FileActions>
            </FileItem>
          ))}
        </Deliverables>
      </CardBody>

      <CardActions>
        <Button variant="success" onClick={() => approveReview(review.id)}>
          ✅ Approve
        </Button>
        <Button variant="warning" onClick={() => requestChanges(review.id)}>
          📝 Request Changes
        </Button>
        <Link to={`/reviews/${review.id}`}>View Details</Link>
      </CardActions>
    </Card>
  );
}

// components/ReviewDetail.tsx
export function ReviewDetail({ id }: { id: string }) {
  const review = useReview(id);
  const [mode, setMode] = useState<"approve" | "request" | "reject" | null>(null);
  const [feedback, setFeedback] = useState("");

  async function submitApproval() {
    await fetch(`/api/reviews/${id}/approve`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ feedback }),
    });

    navigate("/reviews");
  }

  async function submitChangeRequest() {
    await fetch(`/api/reviews/${id}/reject`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        reason: feedback,
        requestedChanges: feedback.split("\n").filter(l => l.trim()),
      }),
    });

    navigate("/reviews");
  }

  return (
    <div className="review-detail">
      <BackButton to="/reviews" />

      <ReviewInfo review={review} />

      <DeliverablesList deliverables={review.deliverables}>
        {review.deliverables.map(file => (
          <FilePreview key={file} file={file} />
        ))}
      </DeliverablesList>

      <ReviewActions>
        {mode === "approve" ? (
          <ApprovalForm
            feedback={feedback}
            onFeedbackChange={setFeedback}
            onSubmit={submitApproval}
            onCancel={() => setMode(null)}
          />
        ) : mode === "request" ? (
          <ChangeRequestForm
            feedback={feedback}
            onFeedbackChange={setFeedback}
            onSubmit={submitChangeRequest}
            onCancel={() => setMode(null)}
          />
        ) : (
          <>
            <Button variant="success" onClick={() => setMode("approve")}>
              ✅ Approve
            </Button>
            <Button variant="warning" onClick={() => setMode("request")}>
              📝 Request Changes
            </Button>
            <Button variant="danger" onClick={() => setMode("reject")}>
              🗑️ Reject
            </Button>
          </>
        )}
      </ReviewActions>
    </div>
  );
}
```

### Active Agents View

**Purpose:** Monitor running agents with live output streaming.

**Wireframe:**

```
╔═══════════════════════════════════════════════════════════╗
║  Active Agents (2 running)                                ║
╚═══════════════════════════════════════════════════════════╝

┌───────────────────────────────────────────────────────────┐
│ 🔄 Agent: CozoDB Knowledge Research               task_10 │
├───────────────────────────────────────────────────────────┤
│ Started: 45 minutes ago                                   │
│ Duration: 45m 23s                                         │
│ Labels: agent, research                                   │
│ Parent: task_8 (Daemon Browser Workbench Design)          │
│                                                            │
│ [📊 View Live Output]  [⏸️ Pause]  [🛑 Stop]              │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ 🔄 Agent: Daemon Browser Workbench Design        task_18  │
├───────────────────────────────────────────────────────────┤
│ Started: 2 minutes ago                                    │
│ Duration: 2m 15s                                          │
│ Labels: agent, design, background-subagent                │
│                                                            │
│ [📊 View Live Output]  [⏸️ Pause]  [🛑 Stop]              │
│                                                            │
└───────────────────────────────────────────────────────────┘
```

**Live Output Modal (Click "View Live Output"):**

```
╔═══════════════════════════════════════════════════════════╗
║  Live Output: Agent CozoDB Knowledge Research     task_10 ║
╚═══════════════════════════════════════════════════════════╝

[← Back to Active Agents]

Duration: 45m 23s   Status: Running   [Auto-scroll: ON]

┌───────────────────────────────────────────────────────────┐
│ CONSOLE OUTPUT                                            │
├───────────────────────────────────────────────────────────┤
│                                                            │
│ [19:15:23] Starting research agent...                     │
│ [19:15:24] Reading DATALOG_COSODB_LIBSQL_KNOWLEDGE.md     │
│ [19:15:25] Searching for CozoDB documentation...          │
│ [19:16:12] Found 3 relevant sources                       │
│ [19:16:13] Analyzing CozoDB vs libSQL trade-offs...       │
│ [19:17:45] Generating comparison table...                 │
│ [19:18:30] Writing recommendations...                     │
│ [19:20:05] Research complete. Writing report...           │
│ [19:20:15] ● (cursor blinking - agent still working)      │
│                                                            │
│ (Scrollable console output)                               │
│                                                            │
└───────────────────────────────────────────────────────────┘

[Close]
```

**Implementation:**

```typescript
// components/ActiveAgents.tsx
export function ActiveAgents() {
  const agents = useAgents({ state: "active" });

  return (
    <div className="active-agents">
      <AgentsHeader count={agents.total} />

      <AgentList>
        {agents.data.map(agent => (
          <AgentCard key={agent.id} agent={agent} />
        ))}
      </AgentList>

      {agents.total === 0 && (
        <EmptyState>
          <p>No agents currently running</p>
        </EmptyState>
      )}
    </div>
  );
}

// components/AgentCard.tsx
export function AgentCard({ agent }: { agent: Agent }) {
  const [showOutput, setShowOutput] = useState(false);

  return (
    <>
      <Card>
        <CardHeader>
          <StatusBadge status="running" />
          <h3>{agent.goal}</h3>
          <span className="task-id">{agent.id}</span>
        </CardHeader>

        <CardBody>
          <MetaInfo>
            <span>Started: {formatRelative(agent.startedAt)}</span>
            <span>Duration: {formatDuration(agent.duration)}</span>
            <span>Labels: {agent.labels.join(", ")}</span>
          </MetaInfo>
        </CardBody>

        <CardActions>
          <Button onClick={() => setShowOutput(true)}>
            📊 View Live Output
          </Button>
          <Button variant="warning">⏸️ Pause</Button>
          <Button variant="danger">🛑 Stop</Button>
        </CardActions>
      </Card>

      {showOutput && (
        <AgentOutputModal
          agentId={agent.id}
          onClose={() => setShowOutput(false)}
        />
      )}
    </>
  );
}

// components/AgentOutputModal.tsx
export function AgentOutputModal({ agentId, onClose }: Props) {
  const [output, setOutput] = useState<string[]>([]);
  const [autoScroll, setAutoScroll] = useState(true);
  const outputRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    // Subscribe to agent output via SSE
    const eventSource = new EventSource(`/api/agents/${agentId}/output`);

    eventSource.onmessage = (event) => {
      const line = event.data;
      setOutput(prev => [...prev, line]);

      if (autoScroll && outputRef.current) {
        outputRef.current.scrollTop = outputRef.current.scrollHeight;
      }
    };

    return () => eventSource.close();
  }, [agentId, autoScroll]);

  return (
    <Modal onClose={onClose}>
      <ModalHeader>
        <h2>Live Output: Agent {agentId}</h2>
        <Toggle
          label="Auto-scroll"
          checked={autoScroll}
          onChange={setAutoScroll}
        />
      </ModalHeader>

      <ModalBody>
        <ConsoleOutput ref={outputRef}>
          {output.map((line, i) => (
            <ConsoleLine key={i}>{line}</ConsoleLine>
          ))}
        </ConsoleOutput>
      </ModalBody>

      <ModalActions>
        <Button onClick={onClose}>Close</Button>
      </ModalActions>
    </Modal>
  );
}
```

### Tasks View

**Purpose:** Browse all tasks with filtering, sorting, Kanban view.

**Wireframe:**

```
╔═══════════════════════════════════════════════════════════╗
║  Tasks (17 total)                          [+ New Task]   ║
╚═══════════════════════════════════════════════════════════╝

View: [List] [Kanban] [Graph]

Filters: [All] [Active] [Blocked] [Completed]
Labels: [agent] [review] [design] [research]
Priority: [All] [P0] [P1] [P2] [P3]

Sort: [Priority ▼]

┌───────────────────────────────────────────────────────────┐
│ ⭕ P0  task_7   Created  Create CLI specs and wrapper     │
│               Labels: agent, implementation                │
│               [Start] [View] [Delete]                      │
├───────────────────────────────────────────────────────────┤
│ 🔄 P0  task_8   Active   Design task tracking automation  │
│               Labels: agent, design                        │
│               Started: 2 hours ago                         │
│               [Complete] [Block] [View]                    │
├───────────────────────────────────────────────────────────┤
│ ✅ P0  task_4   Completed  Test add with JSON             │
│               Labels: test, cli                            │
│               Completed: 7 hours ago                       │
│               [View] [Reopen]                              │
└───────────────────────────────────────────────────────────┘

(More tasks...)
```

**Kanban View:**

```
╔═══════════════════════════════════════════════════════════╗
║  Tasks - Kanban View                       [+ New Task]   ║
╚═══════════════════════════════════════════════════════════╝

┌─────────────┬─────────────┬─────────────┬─────────────────┐
│ Created (3) │ Active (5)  │ Blocked (2) │ Completed (7)   │
├─────────────┼─────────────┼─────────────┼─────────────────┤
│             │             │             │                 │
│ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │ ┌─────────────┐ │
│ │ P0      │ │ │ P0      │ │ │ P1      │ │ │ P0          │ │
│ │ task_7  │ │ │ task_8  │ │ │ task_3  │ │ │ task_4      │ │
│ │ CLI     │ │ │ Task    │ │ │ Test    │ │ │ Test JSON   │ │
│ │ specs   │ │ │ tracking│ │ │ task 2  │ │ │ add         │ │
│ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────────┘ │
│             │             │             │                 │
│ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │ ┌─────────────┐ │
│ │ P1      │ │ │ P1      │ │ │ P2      │ │ │ P1          │ │
│ │ task_5  │ │ │ task_6  │ │ │ task_11 │ │ │ task_10     │ │
│ │ Project │ │ │ Graph   │ │ │ Review: │ │ │ Agent:      │ │
│ │ reflect │ │ │ query   │ │ │ Graph   │ │ │ Task track  │ │
│ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────────┘ │
│             │             │             │                 │
│ (Drag/drop between columns)             │ (More...)       │
│                                          │                 │
└─────────────┴─────────────┴─────────────┴─────────────────┘
```

**Implementation:**

```typescript
// components/Tasks.tsx
export function Tasks() {
  const [view, setView] = useState<"list" | "kanban" | "graph">("list");
  const [filters, setFilters] = useState<TaskListQuery>({});
  const tasks = useTasks(filters);

  return (
    <div className="tasks">
      <TasksHeader count={tasks.total}>
        <Button onClick={() => navigate("/tasks/new")}>+ New Task</Button>
      </TasksHeader>

      <ViewToggle value={view} onChange={setView}>
        <option value="list">List</option>
        <option value="kanban">Kanban</option>
        <option value="graph">Graph</option>
      </ViewToggle>

      <Filters filters={filters} onChange={setFilters} />

      {view === "list" && <TaskList tasks={tasks.data} />}
      {view === "kanban" && <TaskKanban tasks={tasks.data} />}
      {view === "graph" && <TaskGraph tasks={tasks.data} />}
    </div>
  );
}

// components/TaskKanban.tsx
export function TaskKanban({ tasks }: { tasks: TaskProperties[] }) {
  const [columns, setColumns] = useState({
    created: tasks.filter(t => t.state === "created"),
    active: tasks.filter(t => t.state === "active"),
    blocked: tasks.filter(t => t.state === "blocked"),
    completed: tasks.filter(t => t.state === "completed"),
  });

  async function handleDrop(taskId: string, newState: TaskState) {
    // Optimistic update
    setColumns(/* move task between columns */);

    // API call
    await fetch(`/api/tasks/${taskId}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ action: newState }),
    });
  }

  return (
    <KanbanBoard>
      <KanbanColumn title="Created" count={columns.created.length}>
        {columns.created.map(task => (
          <TaskCard
            key={task.id}
            task={task}
            onDrop={state => handleDrop(task.id, state)}
          />
        ))}
      </KanbanColumn>

      <KanbanColumn title="Active" count={columns.active.length}>
        {columns.active.map(task => (
          <TaskCard key={task.id} task={task} onDrop={state => handleDrop(task.id, state)} />
        ))}
      </KanbanColumn>

      <KanbanColumn title="Blocked" count={columns.blocked.length}>
        {columns.blocked.map(task => (
          <TaskCard key={task.id} task={task} onDrop={state => handleDrop(task.id, state)} />
        ))}
      </KanbanColumn>

      <KanbanColumn title="Completed" count={columns.completed.length}>
        {columns.completed.map(task => (
          <TaskCard key={task.id} task={task} onDrop={state => handleDrop(task.id, state)} />
        ))}
      </KanbanColumn>
    </KanbanBoard>
  );
}
```

### History View

**Purpose:** Timeline of all activity with filtering and search.

**Wireframe:**

```
╔═══════════════════════════════════════════════════════════╗
║  History                                                  ║
╚═══════════════════════════════════════════════════════════╝

Date Range: [Today] [This Week] [This Month] [Custom]
Event Types: [All] [Tasks] [Agents] [Reviews] [System]

Search: [_________________________________] [🔍]

┌───────────────────────────────────────────────────────────┐
│ TIMELINE                                                  │
├───────────────────────────────────────────────────────────┤
│                                                            │
│ 🕐 19:38 EST                                              │
│    🔄 Agent started: Daemon Browser Workbench Design      │
│       task_18 | Labels: agent, design                     │
│                                                            │
│ 🕐 19:15 EST                                              │
│    📋 Review created: Graph Persistence Design            │
│       task_17 | Priority: P0 | Blocking: 2 tasks          │
│                                                            │
│ 🕐 18:30 EST                                              │
│    ✅ Agent completed: Task Tracking Automation           │
│       task_10 | Duration: 42m 15s                         │
│       Deliverables: TASK_TRACKING_AUTOMATION.md           │
│                                                            │
│ 🕐 17:45 EST                                              │
│    🔄 Agent started: Task Tracking Automation             │
│       task_10 | Labels: agent, design                     │
│                                                            │
│ 🕐 17:30 EST                                              │
│    ✅ Task completed: Test batch operations               │
│       task_4 | Priority: P0                               │
│                                                            │
│ 🕐 17:15 EST                                              │
│    📝 Task created: Test add with JSON                    │
│       task_4 | Priority: P0 | Labels: test, cli           │
│                                                            │
│ (More events...)                                          │
│                                                            │
└───────────────────────────────────────────────────────────┘

[Load More]
```

**Implementation:**

```typescript
// components/History.tsx
export function History() {
  const [filters, setFilters] = useState<HistoryQuery>({
    startDate: new Date().toISOString(), // Today
    limit: 20,
  });
  const history = useHistory(filters);

  return (
    <div className="history">
      <HistoryHeader />

      <Filters>
        <DateRangePicker
          value={[filters.startDate, filters.endDate]}
          onChange={([start, end]) => setFilters({ ...filters, startDate: start, endDate: end })}
        />
        <EventTypeFilter
          value={filters.types}
          onChange={types => setFilters({ ...filters, types })}
        />
        <SearchInput
          placeholder="Search events..."
          onChange={query => setFilters({ ...filters, query })}
        />
      </Filters>

      <Timeline events={history.data} />

      {history.hasMore && (
        <Button onClick={history.loadMore}>Load More</Button>
      )}
    </div>
  );
}

// components/Timeline.tsx
export function Timeline({ events }: { events: Event[] }) {
  return (
    <div className="timeline">
      {events.map(event => (
        <TimelineEvent key={event.id} event={event} />
      ))}
    </div>
  );
}

// components/TimelineEvent.tsx
export function TimelineEvent({ event }: { event: Event }) {
  const icon = getEventIcon(event.type);
  const color = getEventColor(event.type);

  return (
    <div className="timeline-event">
      <div className="timeline-time">
        {formatTime(event.timestamp)}
      </div>
      <div className="timeline-marker" style={{ backgroundColor: color }}>
        {icon}
      </div>
      <div className="timeline-content">
        <h4>{event.summary}</h4>
        <EventDetails event={event} />
      </div>
    </div>
  );
}
```

### Real-time Updates

**WebSocket Integration:**

```typescript
// hooks/useWebSocket.ts
export function useWebSocket() {
  const [ws, setWs] = useState<WebSocket | null>(null);

  useEffect(() => {
    const socket = new WebSocket("ws://localhost:3000/ws");

    socket.onopen = () => {
      console.log("WebSocket connected");

      // Subscribe to all channels
      socket.send(JSON.stringify({
        type: "subscribe",
        channels: ["tasks", "agents", "reviews", "system"],
      }));

      setWs(socket);
    };

    socket.onclose = () => {
      console.log("WebSocket disconnected");
      setWs(null);

      // Reconnect after 5 seconds
      setTimeout(() => {
        useWebSocket();
      }, 5000);
    };

    return () => socket.close();
  }, []);

  return ws;
}

// hooks/useRealtimeUpdates.ts
export function useRealtimeUpdates() {
  const ws = useWebSocket();
  const queryClient = useQueryClient();

  useEffect(() => {
    if (!ws) return;

    ws.onmessage = (event) => {
      const message = JSON.parse(event.data);

      // Invalidate queries based on event type
      switch (message.type) {
        case "task.created":
        case "task.updated":
        case "task.deleted":
          queryClient.invalidateQueries(["tasks"]);
          queryClient.invalidateQueries(["stats"]);
          break;

        case "agent.started":
        case "agent.completed":
          queryClient.invalidateQueries(["agents"]);
          queryClient.invalidateQueries(["stats"]);
          break;

        case "review.created":
        case "review.approved":
        case "review.rejected":
          queryClient.invalidateQueries(["reviews"]);
          queryClient.invalidateQueries(["stats"]);
          break;
      }

      // Show toast notification for important events
      if (message.type === "review.created") {
        toast.info(`New review: ${message.data.goal}`);
      }
    };
  }, [ws, queryClient]);
}

// App.tsx
export function App() {
  useRealtimeUpdates(); // Global hook for all real-time updates

  return (
    <Router>
      {/* routes */}
    </Router>
  );
}
```

---

## macOS Notification Integration

### Implementation Options

**Option A: terminal-notifier (Daemon-side)**

```bash
# Install terminal-notifier
bun add terminal-notifier

# Send notification from daemon
terminal-notifier \
  -title "tk-agents Workbench" \
  -subtitle "New P0 Review" \
  -message "Graph Persistence Design\nBlocking 2 tasks" \
  -open "http://localhost:3000/reviews/task_18"
```

**Advantages:**
- Native macOS notification
- Works even if browser is closed
- Can auto-open browser URL
- Supports custom icons and actions

**Disadvantages:**
- macOS-only
- Requires external binary
- May need user permission

**Option B: Browser Notification API (Client-side)**

```typescript
// Request permission on app load
if (Notification.permission === "default") {
  await Notification.requestPermission();
}

// Send notification from browser
if (Notification.permission === "granted") {
  new Notification("New P0 Review", {
    body: "Graph Persistence Design\nBlocking 2 tasks",
    icon: "/favicon.ico",
    tag: "review-task_18", // Prevent duplicates
    data: { url: "/reviews/task_18" },
  });
}

// Handle click
notification.onclick = () => {
  window.focus();
  navigate(notification.data.url);
};
```

**Advantages:**
- Cross-platform
- Native browser API
- No external dependencies

**Disadvantages:**
- Requires browser to be open
- User must grant permission
- Less control over appearance

**Option C: Hybrid (Recommended)**

```typescript
// daemon/notification-manager.ts
export class NotificationManager {
  private mode: "auto-open" | "notify-only" | "silent";

  constructor(mode: "auto-open" | "notify-only" | "silent" = "auto-open") {
    this.mode = mode;
  }

  async sendReviewNotification(review: Review) {
    const url = `http://localhost:3000/reviews/${review.id}`;

    if (this.mode === "silent") {
      return; // No notification
    }

    // Always send terminal notification (if available)
    if (isMacOS() && hasTerminalNotifier()) {
      await this.sendTerminalNotification({
        title: "tk-agents Workbench",
        subtitle: `New P${review.priority} Review`,
        message: review.goal,
        url: this.mode === "notify-only" ? url : undefined,
      });

      if (this.mode === "auto-open") {
        // Auto-open browser
        await exec(`open ${url}`);
      }
    }

    // Also broadcast to browser clients for in-app notification
    this.broadcastToClients({
      type: "notification.review",
      data: { review, url },
    });
  }

  private async sendTerminalNotification(options: NotificationOptions) {
    const cmd = [
      "terminal-notifier",
      `-title "${options.title}"`,
      options.subtitle ? `-subtitle "${options.subtitle}"` : "",
      `-message "${options.message}"`,
      options.url ? `-open "${options.url}"` : "",
    ].filter(Boolean).join(" ");

    await exec(cmd);
  }
}
```

### Configuration Modes

**Config File (`.primer-config.json`):**

```json
{
  "daemon": {
    "port": 3000,
    "host": "localhost"
  },
  "notifications": {
    "mode": "auto-open",
    "onReviewCreated": true,
    "onAgentCompleted": false,
    "priorities": ["P0", "P1"]
  },
  "browser": {
    "autoOpen": true,
    "defaultRoute": "/dashboard"
  }
}
```

**CLI Configuration:**

```bash
# Set notification mode
primer config set notifications.mode auto-open
primer config set notifications.mode notify-only
primer config set notifications.mode silent

# Enable/disable specific events
primer config set notifications.onReviewCreated true
primer config set notifications.onAgentCompleted false

# Filter by priority
primer config set notifications.priorities "P0,P1"
```

**UI Settings Page:**

```
╔═══════════════════════════════════════════════════════════╗
║  Settings > Notifications                                 ║
╚═══════════════════════════════════════════════════════════╝

┌───────────────────────────────────────────────────────────┐
│ NOTIFICATION MODE                                         │
├───────────────────────────────────────────────────────────┤
│                                                            │
│ ( ) Auto-open browser                                     │
│     Automatically open browser when review is assigned    │
│                                                            │
│ (•) Notification only                                     │
│     Show notification, click to open browser              │
│                                                            │
│ ( ) Silent                                                │
│     No notifications (check dashboard manually)           │
│                                                            │
└───────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────┐
│ NOTIFICATION TRIGGERS                                     │
├───────────────────────────────────────────────────────────┤
│                                                            │
│ [✓] Review created (only priority P0, P1)                 │
│ [ ] Agent completed                                       │
│ [ ] Task blocked                                          │
│ [✓] System alerts                                         │
│                                                            │
└───────────────────────────────────────────────────────────┘

[Save Changes]
```

---

## Technology Stack

### Backend (Daemon)

**Runtime:**
- **Bun 1.x**: Native TypeScript, fast startup, built-in WebSocket

**HTTP Server:**
- **Bun.serve()**: Built-in HTTP + WebSocket support (no Express needed)

**Database:**
- **In-memory Graph**: Existing Graph class with actor system
- **CozoDB (future)**: SQLite backend for Datalog queries
- **EventLog (existing)**: JSONL append-only log for event sourcing

**Event Streaming:**
- **WebSocket**: Bun native WebSocket support
- **Server-Sent Events (SSE)**: For agent output streaming

**Process Management:**
- **PID file**: `.primer-daemon.pid` for daemon tracking
- **Bun spawn()**: Background process spawning

**Notification:**
- **terminal-notifier**: macOS notifications (npm package)

### Frontend (Browser)

**Framework:**
- **React 18**: Component-based UI (or vanilla HTML/CSS/JS for simplicity)
- **React Router**: Client-side routing

**State Management:**
- **React Query (TanStack Query)**: Server state, caching, real-time updates
- **Zustand**: Client state (lightweight alternative to Redux)

**Styling:**
- **Tailwind CSS**: Utility-first CSS framework

**Build:**
- **Bun bundler**: HTML imports pattern (per CLAUDE.md)
- **HMR**: Hot module replacement during development

**Transport:**
- **Fetch API**: REST API calls
- **WebSocket**: Real-time event streaming

**Notification:**
- **Notification API**: Browser native notifications

### Development Tools

**Testing:**
- **Bun test**: Unit tests for daemon and graph logic
- **Playwright**: E2E browser tests

**Linting:**
- **TypeScript**: Type checking
- **ESLint**: Code linting (optional)

**Deployment:**
- **Daemon**: Single Bun process
- **Frontend**: Served by Bun.serve() (HTML imports)

---

## Implementation Phases

### Phase 1: Daemon Foundation (3-5 days)

**Deliverables:**
- Daemon process management (start/stop/status)
- HTTP server with basic REST API
- Hot graph loading from tasks.json
- EventLog integration
- PID file management

**Tasks:**
1. Create `daemon/server.ts` with Bun.serve()
2. Implement REST endpoints: GET /api/tasks, GET /api/stats, GET /api/health
3. Add PID file management (`daemon/process.ts`)
4. CLI commands: `primer daemon start/stop/status`
5. Load graph from tasks.json on startup
6. Save graph on shutdown
7. EventLog integration (append events on mutations)

**Success Criteria:**
- Daemon starts/stops cleanly
- CLI can query daemon via HTTP
- Graph state persists across daemon restarts
- EventLog records all mutations

**Example Code:**

```typescript
// daemon/server.ts
import { Graph } from "../graph.ts";
import { EventLog } from "../persistence/event-log.ts";
import { loadGraph, saveGraph } from "./state.ts";

const graph = new Graph();
const eventLog = new EventLog("events.jsonl");

// Load graph on startup
await loadGraph(graph, "tasks.json");
eventLog.replay(() => {}); // Replay for audit

// HTTP server
Bun.serve({
  port: 3000,

  routes: {
    "/api/tasks": {
      GET: (req) => {
        const tasks = graph.getNodeIds()
          .filter(id => graph.getNodeProperties(id)?.type === "task")
          .map(id => graph.getNodeProperties(id));

        return new Response(JSON.stringify({ tasks }), {
          headers: { "Content-Type": "application/json" },
        });
      },
    },

    "/api/health": {
      GET: (req) => {
        return new Response(JSON.stringify({
          status: "ok",
          uptime: process.uptime(),
          graph: {
            nodes: graph.getNodeIds().length,
            edges: graph.dump().edges.length,
          },
        }), {
          headers: { "Content-Type": "application/json" },
        });
      },
    },
  },
});

// Graceful shutdown
process.on("SIGTERM", async () => {
  await eventLog.flush();
  await saveGraph(graph, "tasks.json");
  process.exit(0);
});
```

### Phase 2: Browser MVP (5-7 days)

**Deliverables:**
- Dashboard view with stats
- Reviews view with list and detail
- WebSocket real-time updates
- Basic task CRUD

**Tasks:**
1. Create HTML entry point (`public/index.html`)
2. React app setup with routing
3. Dashboard component (stats, reviews, agents, recent completions)
4. Reviews list component (filter, sort, approve/reject)
5. Review detail component (file preview, actions)
6. WebSocket client integration
7. React Query setup for data fetching
8. Basic styling with Tailwind

**Success Criteria:**
- Dashboard shows live stats
- Reviews list updates in real-time
- User can approve/reject reviews
- WebSocket events update UI immediately

**Example Code:**

```typescript
// public/index.html
<html>
  <head>
    <title>tk-agents Workbench</title>
    <script type="module" src="/src/browser/main.tsx"></script>
    <link rel="stylesheet" href="/src/browser/styles.css" />
  </head>
  <body>
    <div id="root"></div>
  </body>
</html>

// src/browser/main.tsx
import { createRoot } from "react-dom/client";
import { App } from "./App.tsx";

const root = createRoot(document.getElementById("root")!);
root.render(<App />);

// src/browser/App.tsx
export function App() {
  useRealtimeUpdates(); // WebSocket integration

  return (
    <Router>
      <Layout>
        <Routes>
          <Route path="/" element={<Dashboard />} />
          <Route path="/reviews" element={<Reviews />} />
          <Route path="/reviews/:id" element={<ReviewDetail />} />
          <Route path="/agents" element={<ActiveAgents />} />
          <Route path="/tasks" element={<Tasks />} />
          <Route path="/history" element={<History />} />
        </Routes>
      </Layout>
    </Router>
  );
}
```

### Phase 3: Notification Integration (2-3 days)

**Deliverables:**
- macOS notifications via terminal-notifier
- Auto-open browser pattern
- Configuration modes (auto-open, notify-only, silent)
- Settings UI

**Tasks:**
1. Add terminal-notifier support
2. Implement NotificationManager class
3. Hook notifications into daemon event flow
4. Add config file support (`.primer-config.json`)
5. CLI commands: `primer config get/set`
6. Settings page in browser UI
7. Browser Notification API integration (fallback)

**Success Criteria:**
- Review creation triggers macOS notification
- Auto-open mode launches browser to review page
- Notify-only mode shows clickable notification
- User can configure notification preferences

**Example Code:**

```typescript
// daemon/notification-manager.ts
export class NotificationManager {
  async sendReviewNotification(review: Review) {
    const config = loadConfig();

    if (config.notifications.mode === "silent") {
      return;
    }

    // Send macOS notification
    await exec([
      "terminal-notifier",
      `-title "tk-agents Workbench"`,
      `-subtitle "New P${review.priority} Review"`,
      `-message "${review.goal}"`,
      config.notifications.mode === "notify-only"
        ? `-open "http://localhost:3000/reviews/${review.id}"`
        : "",
    ].join(" "));

    // Auto-open browser (if configured)
    if (config.notifications.mode === "auto-open") {
      await exec(`open http://localhost:3000/reviews/${review.id}`);
    }
  }
}
```

### Phase 4: Advanced Features (5-7 days)

**Deliverables:**
- Active agents with output streaming
- History timeline with search/filter
- Dependency graph visualization
- Kanban board view
- Task detail page

**Tasks:**
1. Agent output streaming via SSE
2. AgentOutputModal component
3. History timeline component
4. Search and filter implementation
5. Graph visualization (D3.js or Cytoscape.js)
6. Kanban board with drag/drop
7. Task detail page with dependency tree

**Success Criteria:**
- Agent output streams in real-time
- History search returns relevant events
- Dependency graph shows task relationships
- Kanban board allows drag/drop task updates

**Example Code:**

```typescript
// daemon/routes/agents.ts
export const agentOutputRoute = {
  "/api/agents/:id/output": {
    GET: (req) => {
      const agentId = req.params.id;

      // SSE stream
      const stream = new ReadableStream({
        start(controller) {
          // Subscribe to agent output events
          const subscription = agentOutputEmitter.on(agentId, (line) => {
            controller.enqueue(`data: ${line}\n\n`);
          });

          // Clean up on close
          req.signal.addEventListener("abort", () => {
            subscription.unsubscribe();
            controller.close();
          });
        },
      });

      return new Response(stream, {
        headers: {
          "Content-Type": "text/event-stream",
          "Cache-Control": "no-cache",
          "Connection": "keep-alive",
        },
      });
    },
  },
};
```

---

## API Specification

### Complete Endpoint Documentation

**(See API Layer section above for full details)**

Summary of endpoints:

**Tasks:**
- `GET /api/tasks` - List tasks with filters
- `POST /api/tasks` - Create task
- `GET /api/tasks/:id` - Get task details
- `PUT /api/tasks/:id` - Update task
- `DELETE /api/tasks/:id` - Delete task

**Reviews:**
- `GET /api/reviews` - List pending reviews
- `POST /api/reviews/:id/approve` - Approve review
- `POST /api/reviews/:id/reject` - Reject review

**Agents:**
- `GET /api/agents` - List active agents
- `GET /api/agents/:id/output` - Stream agent output (SSE)

**Graph:**
- `GET /api/graph` - Get graph dump
- `POST /api/graph/query` - Execute Datalog query

**Activity:**
- `GET /api/activity` - Recent activity feed
- `GET /api/history` - Full history with filters

**System:**
- `GET /api/stats` - Dashboard statistics
- `GET /api/health` - Daemon health check

---

## WebSocket Protocol

**(See WebSocket Events section above for full details)**

**Event Types:**
- `task.created`, `task.updated`, `task.deleted`
- `agent.started`, `agent.completed`
- `review.created`, `review.approved`, `review.rejected`
- `daemon.started`

**Client Commands:**
- `subscribe`, `unsubscribe`
- `ping`, `pong`

---

## Migration Path

### Adopting Without Breaking CLI Workflow

**Goals:**
1. Daemon is optional (CLI still works standalone)
2. CLI auto-detects daemon and uses it if available
3. No breaking changes to existing CLI commands
4. Gradual adoption path

**Implementation:**

```typescript
// cli/task.ts - Updated to detect daemon
async function executeCommand(command: string, args: any[]) {
  // Check if daemon is running
  if (await isDaemonRunning()) {
    // Use daemon API
    const client = new DaemonClient();
    return await client.executeCommand(command, args);
  } else {
    // Fallback: direct file manipulation (existing behavior)
    const graph = await loadGraph(TASKS_FILE);
    // ... existing CLI logic
    await saveGraph(graph, TASKS_FILE);
  }
}

// Helper to check daemon status
async function isDaemonRunning(): Promise<boolean> {
  try {
    const response = await fetch("http://localhost:3000/api/health", {
      signal: AbortSignal.timeout(1000), // 1s timeout
    });
    return response.ok;
  } catch {
    return false;
  }
}
```

**User Experience:**

```bash
# User workflow (no daemon)
bun src/cli/task.ts add "Goal"
# → Direct file manipulation (existing behavior)

# User starts daemon
primer daemon start
# → Daemon loads graph, starts server

# Same CLI command (daemon running)
bun src/cli/task.ts add "Goal"
# → Detects daemon, uses API
# → Graph updated in-memory
# → Event logged
# → Broadcast to browser clients

# User stops daemon
primer daemon stop
# → Daemon saves graph to tasks.json

# CLI still works (no daemon)
bun src/cli/task.ts list
# → Reads from tasks.json directly
```

**Migration Steps:**

1. **Phase 1: CLI still works standalone**
   - No changes to existing CLI behavior
   - Daemon is optional add-on

2. **Phase 2: CLI auto-detects daemon**
   - CLI checks for daemon on startup
   - Uses API if available, falls back to file I/O

3. **Phase 3: Recommend daemon for multi-client scenarios**
   - Documentation encourages daemon for browser + CLI usage
   - CLI warns if daemon not running when needed

4. **Phase 4 (optional): Make daemon default**
   - Auto-start daemon on first CLI command
   - Still allow standalone mode via flag

---

## Security Considerations

### Localhost Only

**Requirement:** Daemon should only listen on localhost by default.

```typescript
// daemon/server.ts
Bun.serve({
  hostname: "localhost", // NOT "0.0.0.0"
  port: 3000,
  // ...
});
```

**Why:** Prevents external access to daemon API.

### API Tokens (Future)

**Future Enhancement:** Add optional API token authentication.

```typescript
// .primer-config.json
{
  "daemon": {
    "apiToken": "randomly-generated-token"
  }
}

// daemon/middleware.ts
function authMiddleware(req: Request) {
  const token = req.headers.get("Authorization")?.replace("Bearer ", "");
  const expectedToken = loadConfig().daemon.apiToken;

  if (expectedToken && token !== expectedToken) {
    return new Response("Unauthorized", { status: 401 });
  }
}
```

### CORS

**Not needed** if browser and daemon run on same origin (`localhost:3000`).

If needed:

```typescript
// daemon/server.ts
const corsHeaders = {
  "Access-Control-Allow-Origin": "http://localhost:3000",
  "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE",
  "Access-Control-Allow-Headers": "Content-Type",
};
```

### File Access

**Sandboxing:** Daemon should only access project directory.

```typescript
// daemon/file-access.ts
import { resolve, relative } from "path";

const PROJECT_ROOT = process.cwd();

function safeReadFile(filePath: string) {
  const absPath = resolve(PROJECT_ROOT, filePath);
  const relPath = relative(PROJECT_ROOT, absPath);

  // Prevent directory traversal
  if (relPath.startsWith("..")) {
    throw new Error("Access denied: path outside project");
  }

  return readFileSync(absPath, "utf-8");
}
```

---

## Performance Targets

### Response Times

| Endpoint | Target | Notes |
|----------|--------|-------|
| GET /api/tasks | < 50ms | In-memory graph lookup |
| POST /api/tasks | < 100ms | Create + persist + broadcast |
| GET /api/stats | < 20ms | Cached aggregations |
| WebSocket message | < 10ms | Broadcast to all clients |

### Concurrent Clients

- **Target:** Support 10+ concurrent browser clients
- **WebSocket connections:** 1 per client
- **Memory usage:** < 100 MB for typical workload (100 tasks)

### Event Log

- **Write performance:** < 5ms per event (append-only)
- **Replay performance:** < 500ms for 10,000 events

### Browser UI

- **Initial load:** < 1s
- **Dashboard render:** < 200ms
- **WebSocket reconnect:** < 2s

---

## Testing Strategy

### Unit Tests (Bun test)

**Daemon Logic:**
```bash
bun test daemon/server.test.ts
bun test daemon/state.test.ts
bun test daemon/notification-manager.test.ts
```

**Graph Operations:**
```bash
bun test graph.test.ts
bun test task.test.ts
```

**API Endpoints:**
```bash
bun test daemon/routes/tasks.test.ts
bun test daemon/routes/reviews.test.ts
```

### Integration Tests

**Daemon + CLI:**
```bash
# Start daemon
primer daemon start

# Run CLI commands
primer task add "Test task"
primer task list

# Verify state
curl http://localhost:3000/api/tasks
```

**WebSocket:**
```bash
# Connect WebSocket client
# Create task via CLI
# Verify client receives event
```

### E2E Browser Tests (Playwright)

**Test Scenarios:**
- Dashboard loads and displays stats
- Reviews list shows pending reviews
- Approve review workflow
- Real-time updates when task created
- Agent output streaming

**Example:**
```typescript
// tests/e2e/dashboard.spec.ts
import { test, expect } from "@playwright/test";

test("dashboard shows stats", async ({ page }) => {
  await page.goto("http://localhost:3000");

  // Check dashboard loaded
  await expect(page.locator("h1")).toContainText("Dashboard");

  // Check stats
  await expect(page.locator(".stats .tasks-active")).toContainText("12");
  await expect(page.locator(".stats .agents-running")).toContainText("2");
});

test("review approval workflow", async ({ page }) => {
  await page.goto("http://localhost:3000/reviews");

  // Find first review
  const firstReview = page.locator(".review-card").first();
  await firstReview.locator("button:text('Approve')").click();

  // Fill feedback
  await page.locator("textarea").fill("Looks good!");
  await page.locator("button:text('Submit Approval')").click();

  // Verify redirect
  await expect(page).toHaveURL("/reviews");

  // Verify review removed from list
  await expect(firstReview).not.toBeVisible();
});
```

---

## Deployment

### How to Run

**Development:**
```bash
# Terminal 1: Start daemon
bun src/daemon/server.ts

# Terminal 2: Run CLI commands
bun src/cli/task.ts add "Test task"

# Browser: Open http://localhost:3000
```

**Production:**
```bash
# Start daemon as background service
primer daemon start

# Daemon runs in background
# CLI commands use daemon API
# Browser connects to http://localhost:3000
```

### Configuration

**Default Config (`.primer-config.json`):**
```json
{
  "daemon": {
    "port": 3000,
    "host": "localhost",
    "autoStart": false
  },
  "notifications": {
    "mode": "auto-open",
    "onReviewCreated": true,
    "onAgentCompleted": false,
    "priorities": ["P0", "P1"]
  },
  "browser": {
    "autoOpen": true,
    "defaultRoute": "/dashboard"
  }
}
```

### Monitoring

**Health Check:**
```bash
curl http://localhost:3000/api/health
```

**Logs:**
```bash
# Daemon logs to stdout
primer daemon logs

# Or check event log
cat events.jsonl | tail -n 20
```

**Stats:**
```bash
curl http://localhost:3000/api/stats | jq
```

---

## Alternatives Considered

### Alternative 1: Electron App

**Pros:**
- Native desktop app
- No browser required
- Can run in system tray

**Cons:**
- Heavy (200+ MB)
- Complex build process
- Overkill for local-only tool

**Decision:** Rejected. Browser-based is lighter and easier to develop.

### Alternative 2: TUI (Terminal User Interface)

**Pros:**
- No browser required
- Fast and lightweight
- Accessible via SSH

**Cons:**
- Limited UI capabilities
- No rich text preview
- Hard to implement drag/drop, graphs

**Decision:** Rejected. TUI is great for quick queries (keep CLI for this), but browser is better for rich UI.

### Alternative 3: VSCode Extension

**Pros:**
- Integrates with user's editor
- Can preview files in-place

**Cons:**
- VSCode-only
- Complex extension API
- Still need daemon for state

**Decision:** Rejected for MVP. Could be future enhancement.

### Alternative 4: CLI-only with Rich Output

**Pros:**
- Simple
- No daemon required

**Cons:**
- No real-time updates
- No interactive review workflow
- Limited by terminal capabilities

**Decision:** Rejected. User already has CLI, needs richer UI.

---

## Open Questions

### 1. CozoDB Integration Timeline?

**Question:** When should we integrate CozoDB for graph queries?

**Options:**
- Phase 1: Use in-memory Graph only
- Phase 2: Add CozoDB as optional backend
- Phase 3: Make CozoDB default

**Recommendation:** Phase 2. Start with in-memory Graph, add CozoDB when Datalog queries become necessary.

### 2. Multi-project Support?

**Question:** Should daemon support multiple projects simultaneously?

**Scenario:**
- User has `project-a/` with `tasks.json`
- User has `project-b/` with `tasks.json`
- Should daemon manage both?

**Options:**
- Single-project daemon (one daemon per project)
- Multi-project daemon (one daemon for all projects)

**Recommendation:** Single-project for MVP. Add multi-project in Phase 4.

### 3. Authentication for Remote Access?

**Question:** Should we support remote browser access (e.g., from another machine)?

**Scenario:**
- User runs daemon on server
- Wants to access workbench from laptop

**Recommendation:** Not for MVP. Add API token auth in Phase 4 if needed.

### 4. Agent Process Integration?

**Question:** Should daemon spawn/manage agent processes?

**Current:** Agents run as separate Bun processes (launched via Claude Code `/bg`)

**Options:**
- Daemon tracks agents (passive)
- Daemon spawns agents (active)

**Recommendation:** Passive tracking for MVP. Daemon monitors tasks with `agent` label in `active` state.

### 5. Persistence Strategy Long-term?

**Question:** Should we move beyond `tasks.json` + `events.jsonl`?

**Options:**
- Keep JSON files (simple, git-friendly)
- Migrate to SQLite (more scalable)
- Use CozoDB (Datalog queries)

**Recommendation:** Keep JSON for MVP. Evaluate CozoDB in Phase 3 based on performance needs.

---

## Recommendations

### Priorities

**Phase 1 (Must Have):**
- Daemon foundation
- Basic REST API
- Hot graph in-memory
- CLI integration

**Phase 2 (High Priority):**
- Browser MVP
- Reviews workflow
- Real-time WebSocket updates

**Phase 3 (Important):**
- macOS notifications
- Auto-open browser pattern

**Phase 4 (Nice to Have):**
- Agent output streaming
- Dependency graph visualization
- Advanced filtering

### Next Steps

1. **User Approval**: Review this design document
2. **Prototype Phase 1**: Build daemon foundation (3-5 days)
3. **Demo**: Show CLI + daemon integration
4. **Iterate**: Adjust based on feedback
5. **Prototype Phase 2**: Build browser MVP (5-7 days)
6. **Demo**: Show reviews workflow in browser
7. **Polish**: Add notifications, advanced features

### Success Metrics

**For MVP (Phase 1 + 2):**
- ✅ Daemon starts/stops reliably
- ✅ CLI commands work with daemon
- ✅ Browser dashboard shows live stats
- ✅ User can approve/reject reviews in browser
- ✅ Real-time updates when tasks change

**For V1 (Phase 3 + 4):**
- ✅ macOS notifications trigger on review creation
- ✅ Auto-open browser to review page
- ✅ Agent output streams in browser
- ✅ Dependency graph visualizes task relationships

---

## Conclusion

This design provides a **comprehensive roadmap** for transforming tk-agents from a CLI-only tool into a **daemon + browser workbench** inspired by Claude Cowork.

**Key Innovations:**
- **Hot graph daemon** for instant CLI + browser access
- **Browser-based review workflow** with inline preview and approve/reject
- **macOS notification integration** with auto-open browser
- **Event sourcing** via existing EventLog for audit trail
- **Real-time updates** via WebSocket for live dashboard

**Implementation is phased** to deliver value incrementally:
1. Daemon foundation (3-5 days)
2. Browser MVP (5-7 days)
3. Notifications (2-3 days)
4. Advanced features (5-7 days)

**Total estimated time: 15-22 days** for full V1 implementation.

**Migration path is smooth**: CLI still works standalone, daemon is optional, gradual adoption without breaking changes.

---

**Ready for implementation. Awaiting user approval.**
