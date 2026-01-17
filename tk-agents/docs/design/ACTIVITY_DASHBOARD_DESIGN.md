# Activity Dashboard Design

**Created:** 2026-01-16 19:16 EST
**Version:** 1.0
**Status:** Design Specification

---

## Problem Statement

### User Needs Visibility

The user cannot answer basic questions about ongoing work:

**Questions user SHOULD be able to answer:**
- "What needs my attention RIGHT NOW?"
- "What agents are currently running?"
- "What work was completed today?"
- "What's blocked waiting for me?"
- "Which reviews are highest priority?"
- "How many agents have run this week?"
- "What deliverables were produced?"

**Current Reality:**
- NO dashboard or overview
- Must search through conversation history
- Agent announcements lost in chat scroll
- Cannot see active work streams
- No summary of completed work
- No way to prioritize attention

### User Experience Goal

```bash
# User opens terminal and types:
primer dashboard

# Sees clear, prioritized summary:
╔══════════════════════════════════════════╗
║ NEEDS YOUR ATTENTION (3)                 ║
╚══════════════════════════════════════════╝
  🔴 P0 Review: Task CLI optimizations
     → Deliverables: src/cli/task.ts, P0_FEATURES_IMPLEMENTED.md
     → Blocking: 2 tasks

  🟡 P1 Review: Graph query research
     → Deliverables: GRAPH_QUERY_RESEARCH.md
     → Not blocking anything

  ... (more)

# User immediately knows: "I need to review Task CLI first"
```

---

## Requirements

### Must Have

1. **Attention Summary** - What needs action NOW (prioritized)
2. **Active Agents** - What's running in background
3. **Recent Completions** - What finished today/this week
4. **Blocking Status** - What's waiting on user
5. **Quick Stats** - Task counts, agent activity

### Should Have

6. **Filtering** - By priority, label, date range
7. **Detail View** - Drill down into specific items
8. **Trend Data** - Activity over time

### Nice to Have

9. **Cost Tracking** - API spend per session/agent
10. **Deliverable List** - Files created by agents
11. **Dependency Visualization** - Graph of blocked tasks

---

## Dashboard Components

### Component 1: Needs Attention

**Purpose:** Show work requiring immediate user action, prioritized.

**Data Source:** `task list --label review --status created` + priority sort

**Display:**
```
╔══════════════════════════════════════════╗
║ NEEDS YOUR ATTENTION (3)                 ║
╚══════════════════════════════════════════╝
  🔴 P0 Review: Task CLI optimizations
     → Deliverables: src/cli/task.ts, docs
     → Blocking: 2 tasks
     → Created: 2 hours ago

  🔴 P0 Review: Graph system integration
     → Deliverables: src/graph.ts, tests
     → Blocking: 1 task
     → Created: 30 minutes ago

  🟡 P1 Review: Markdown-as-graph prototype
     → Deliverables: src/markdown-graph/*, analysis doc
     → Not blocking anything
     → Created: 4 hours ago
```

**Priority Icons:**
- 🔴 P0 (Blocking - immediate action)
- 🟡 P1 (High - review today)
- 🟢 P2 (Normal - review this week)
- ⚪ P3 (Low - whenever)

**Sort Order:**
1. Priority (P0 first)
2. Blocking count (most blockers first)
3. Age (oldest first)

### Component 2: Active Agents

**Purpose:** Show background agents currently running.

**Data Source:**
- `task list --label agent --status active`
- Agent process list (if available)
- Hook state tracking

**Display:**
```
╔══════════════════════════════════════════╗
║ ACTIVE AGENTS (3)                        ║
╚══════════════════════════════════════════╝
  🔄 task_agent_a9e9c7b: Project reflection
     → Running: 90 minutes
     → Estimated output: 50KB
     → Labels: analysis, reflection

  🔄 task_agent_a8f57b5: Graph query research
     → Running: 45 minutes
     → Estimated output: 44KB
     → Labels: research, graph

  🔄 task_agent_a9d4ef7: CLI specs + wrapper
     → Running: 60 minutes
     → Estimated output: 68KB
     → Labels: implementation, cli
```

**Fields:**
- Task ID + description
- Running duration
- Estimated output size (if available)
- Labels for filtering

### Component 3: Completed Today

**Purpose:** Show work finished in current session/today.

**Data Source:**
- `task list --status completed` + date filter (completedAt > today)
- Filter by agent label for agent work

**Display:**
```
╔══════════════════════════════════════════╗
║ COMPLETED TODAY (5)                      ║
╚══════════════════════════════════════════╝
  ✅ Markdown-as-graph prototype (71 min, SUCCESS)
     → Deliverables: src/markdown-graph/*, MD_AS_GRAPH_ANALYSIS.md

  ✅ Graph & Knowledge CLIs (3.5 hrs, SUCCESS)
     → Deliverables: src/cli/graph.ts, src/cli/knowledge.ts, specs

  ✅ Task CLI P0 features (6 hrs, SUCCESS)
     → Deliverables: src/cli/task.ts, P0_FEATURES_IMPLEMENTED.md

  ✅ Project reflection analysis (90 min, SUCCESS)
     → Deliverables: PROJECT_REFLECTION.md

  ✅ Address proxy implementation (2 hrs, SUCCESS)
     → Deliverables: src/address.ts, ADDRESS_PROXY_DESIGN.md
```

**Fields:**
- Task description
- Duration (if available)
- Status (SUCCESS/FAILED)
- Deliverables

### Component 4: Blocked on You

**Purpose:** Show tasks waiting for user action/review.

**Data Source:**
- Tasks with `depends_on` edges pointing to review tasks assigned to user
- Tasks explicitly blocked with reason mentioning user

**Display:**
```
╔══════════════════════════════════════════╗
║ BLOCKED ON YOU (2)                       ║
╚══════════════════════════════════════════╝
  🚫 Integrate graph optimizations
     → Blocked by: task_review_21 (P1 review)
     → Waiting: 4 hours

  🚫 Build unified primer CLI
     → Blocked by: task_review_20 (P0 review)
     → Waiting: 2 hours
```

**Sort Order:**
1. Blocker priority (P0 reviews first)
2. Wait time (longest first)

### Component 5: Statistics

**Purpose:** High-level metrics and trends.

**Data Source:** Aggregate queries on tasks.json

**Display:**
```
╔══════════════════════════════════════════╗
║ STATISTICS                               ║
╚══════════════════════════════════════════╝
  Tasks:     15 total (3 active, 5 completed, 3 pending review, 4 ready)
  Agents:    3 active, 8 completed today, 65 total (all-time)
  Reviews:   3 pending (1 P0, 1 P1, 1 P2)

  Today:     5 tasks completed, 8 agents launched
  This Week: 15 tasks completed, 65 agents launched

  Tokens:    ~450K today (~$3.60 estimated)
  Files:     22 markdown docs, 8 TypeScript files modified
```

**Metrics:**
- Task state breakdown
- Agent activity (active, completed, total)
- Review queue status
- Time-based aggregates (today, this week)
- Cost estimates (if tracking available)
- Deliverable counts

---

## CLI Commands

### Option A: Unified `primer` CLI (RECOMMENDED)

Create new `primer` wrapper CLI that delegates to task/graph/knowledge CLIs and adds dashboard commands.

**Commands:**

```bash
# Main dashboard (all components)
primer dashboard

# Individual components
primer reviews              # Show pending reviews
primer agents               # Show active agents
primer completed [period]   # Show completed work (today, week, all)
primer blocked              # Show what's blocked on user
primer stats [period]       # Show statistics

# Shortcuts
primer attention            # Alias for reviews --priority P0
primer next                 # Show next recommended action
```

**Usage Examples:**

```bash
# Morning standup
primer dashboard

# Check P0 reviews
primer attention
# or
primer reviews --priority P0

# See what finished today
primer completed today

# Check active background work
primer agents

# See all pending reviews
primer reviews

# Get next recommended action
primer next
# Output: "Review Task CLI optimizations (P0, blocking 2 tasks)"
```

### Option B: Extend `task` CLI

Add dashboard commands to existing task CLI.

**Commands:**

```bash
# Dashboard
task dashboard

# Reviews
task reviews [--priority P0]
task review-show <id>

# Active work
task active [--label agent]

# Blocked
task blocked-on-me
```

**Pros:**
- No new CLI to build
- Unified with existing task commands

**Cons:**
- Mixes task management with dashboard concerns
- Less intuitive for new users ("dashboard is a task command?")

### Option C: Separate `dashboard` CLI

New standalone dashboard CLI.

**Commands:**

```bash
dashboard status
dashboard reviews
dashboard agents
dashboard activity [period]
```

**Pros:**
- Clean separation of concerns
- Can aggregate from multiple sources (tasks, agents, git, etc.)

**Cons:**
- Another CLI to maintain
- Less integrated with task workflow

### Recommendation: Option A (Unified `primer` CLI)

**Rationale:**
1. Single entry point for all primer operations
2. Familiar pattern (git, cargo, kubectl use sub-program delegation)
3. Easy to add new commands without cluttering task CLI
4. Natural place for cross-cutting concerns (dashboard, stats)
5. Already planned in PRIMER_CLI architecture

---

## Implementation Design

### Primer CLI Architecture

**File Structure:**

```
src/cli/
  primer.ts              # Main wrapper (delegates to subcommands)
  task.ts                # Task management (existing)
  graph.ts               # Graph queries (existing)
  knowledge.ts           # Knowledge queries (existing)
  dashboard.ts           # NEW: Dashboard commands

  lib/
    dashboard-renderer.ts  # NEW: Format dashboard output
    task-queries.ts        # NEW: Dashboard data queries
    stats-calculator.ts    # NEW: Aggregate statistics
```

**Primer.ts (Wrapper):**

```typescript
#!/usr/bin/env bun

const subsystem = process.argv[2];
const args = process.argv.slice(3);

// Built-in commands
const builtIn: Record<string, string> = {
  'task': './task.ts',
  'graph': './graph.ts',
  'knowledge': './knowledge.ts',
  'dashboard': './dashboard.ts',
  'reviews': './dashboard.ts',       // Alias
  'agents': './dashboard.ts',        // Alias
  'completed': './dashboard.ts',     // Alias
  'blocked': './dashboard.ts',       // Alias
  'stats': './dashboard.ts',         // Alias
  'attention': './dashboard.ts',     // Alias
  'next': './dashboard.ts',          // Alias
};

if (builtIn[subsystem]) {
  // Delegate to subcommand
  const subcommandPath = new URL(builtIn[subsystem], import.meta.url).pathname;
  Bun.spawn(['bun', subcommandPath, subsystem, ...args], {
    stdio: ['inherit', 'inherit', 'inherit'],
  });
} else {
  // Try external primer-* command
  Bun.spawn([`primer-${subsystem}`, ...args], {
    stdio: ['inherit', 'inherit', 'inherit'],
  });
}
```

**Dashboard.ts (Implementation):**

```typescript
#!/usr/bin/env bun

import { renderDashboard, renderReviews, renderAgents, renderCompleted, renderBlocked, renderStats } from './lib/dashboard-renderer.ts';
import { loadGraph } from './lib/task-queries.ts';

const command = process.argv[2]; // 'dashboard', 'reviews', 'agents', etc.
const args = process.argv.slice(3);

async function main() {
  const graph = await loadGraph('./tasks.json');

  switch (command) {
    case 'dashboard':
      await renderDashboard(graph);
      break;

    case 'reviews':
    case 'attention':
      const priority = args.includes('--priority') ? args[args.indexOf('--priority') + 1] : undefined;
      await renderReviews(graph, { priority });
      break;

    case 'agents':
      await renderAgents(graph);
      break;

    case 'completed':
      const period = args[0] || 'today'; // today, week, all
      await renderCompleted(graph, period);
      break;

    case 'blocked':
      await renderBlocked(graph);
      break;

    case 'stats':
      const statsPeriod = args[0] || 'all';
      await renderStats(graph, statsPeriod);
      break;

    case 'next':
      await renderNextAction(graph);
      break;

    default:
      console.error(`Unknown dashboard command: ${command}`);
      process.exit(1);
  }
}

main();
```

**Dashboard Renderer (dashboard-renderer.ts):**

```typescript
import type { Graph } from '../../graph.ts';
import type { TaskProperties } from '../../types.ts';
import { calculateStats } from './stats-calculator.ts';

export async function renderDashboard(graph: Graph) {
  // Render all components
  await renderAttentionSection(graph);
  console.log();
  await renderAgentsSection(graph);
  console.log();
  await renderCompletedSection(graph, 'today');
  console.log();
  await renderBlockedSection(graph);
  console.log();
  await renderStatsSection(graph);
}

export async function renderReviews(graph: Graph, options?: { priority?: string }) {
  console.log('╔══════════════════════════════════════════╗');
  console.log('║ PENDING REVIEWS                          ║');
  console.log('╚══════════════════════════════════════════╝');

  // Query: tasks with 'review' label and 'created' status
  const reviews = getReviewTasks(graph, options);

  if (reviews.length === 0) {
    console.log('  No pending reviews');
    return;
  }

  for (const review of reviews) {
    const icon = getPriorityIcon(review.priority);
    const blocking = getBlockingCount(graph, review.id);
    const age = getAge(review.createdAt);

    console.log(`  ${icon} P${review.priority} ${review.goal}`);
    console.log(`     → Deliverables: ${review.desiredDeliverables.join(', ')}`);
    if (blocking > 0) {
      console.log(`     → Blocking: ${blocking} task${blocking > 1 ? 's' : ''}`);
    }
    console.log(`     → Created: ${age}`);
    console.log();
  }
}

function getPriorityIcon(priority: number): string {
  if (priority === 0) return '🔴';
  if (priority === 1) return '🟡';
  if (priority === 2) return '🟢';
  return '⚪';
}

function getBlockingCount(graph: Graph, reviewTaskId: string): number {
  // Count tasks with depends_on edge pointing to this review
  const blockedTasks = graph.getEdgesTo(reviewTaskId)
    .filter(edge => edge.type === 'depends_on');
  return blockedTasks.length;
}

function getAge(createdAt: string): string {
  const created = new Date(createdAt);
  const now = new Date();
  const diffMs = now.getTime() - created.getTime();
  const diffMins = Math.floor(diffMs / 60000);

  if (diffMins < 60) return `${diffMins} min ago`;
  const diffHours = Math.floor(diffMins / 60);
  if (diffHours < 24) return `${diffHours} hour${diffHours > 1 ? 's' : ''} ago`;
  const diffDays = Math.floor(diffHours / 24);
  return `${diffDays} day${diffDays > 1 ? 's' : ''} ago`;
}

// ... more renderer functions
```

**Task Queries (task-queries.ts):**

```typescript
import type { Graph } from '../../graph.ts';
import type { TaskProperties } from '../../types.ts';

export function getReviewTasks(graph: Graph, options?: { priority?: string }): TaskProperties[] {
  const allTasks = getAllTasks(graph);

  let reviews = allTasks.filter(task =>
    task.labels?.includes('review') &&
    task.state === 'created'
  );

  // Filter by priority
  if (options?.priority) {
    const priorityNum = parseInt(options.priority.replace(/^P/, ''));
    reviews = reviews.filter(task => task.priority === priorityNum);
  }

  // Sort by priority, blocking count, age
  return reviews.sort((a, b) => {
    if (a.priority !== b.priority) return a.priority - b.priority;
    const blockingA = getBlockingCount(graph, a.id);
    const blockingB = getBlockingCount(graph, b.id);
    if (blockingA !== blockingB) return blockingB - blockingA;
    return new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime();
  });
}

export function getActiveTasks(graph: Graph, labels?: string[]): TaskProperties[] {
  let tasks = getAllTasks(graph).filter(task => task.state === 'active');

  if (labels) {
    tasks = tasks.filter(task =>
      labels.some(label => task.labels?.includes(label))
    );
  }

  return tasks;
}

export function getCompletedTasks(graph: Graph, period: 'today' | 'week' | 'all'): TaskProperties[] {
  const tasks = getAllTasks(graph).filter(task =>
    task.state === 'completed' && task.completedAt
  );

  if (period === 'all') return tasks;

  const now = new Date();
  const cutoff = period === 'today'
    ? new Date(now.getFullYear(), now.getMonth(), now.getDate())
    : new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

  return tasks.filter(task => new Date(task.completedAt!) >= cutoff);
}

// ... more query functions
```

---

## Dashboard Output Specifications

### Full Dashboard Output

```bash
$ primer dashboard

Primer Dashboard - 2026-01-16 19:30 EST

╔══════════════════════════════════════════╗
║ NEEDS YOUR ATTENTION (3)                 ║
╚══════════════════════════════════════════╝
  🔴 P0 Review: Task CLI optimizations
     → Deliverables: src/cli/task.ts, P0_FEATURES_IMPLEMENTED.md
     → Blocking: 2 tasks
     → Created: 2 hours ago

  🔴 P0 Review: Graph & Knowledge CLIs
     → Deliverables: src/cli/graph.ts, src/cli/knowledge.ts, specs
     → Blocking: 1 task
     → Created: 30 minutes ago

  🟡 P1 Review: Markdown-as-graph prototype
     → Deliverables: src/markdown-graph/*, MD_AS_GRAPH_ANALYSIS.md
     → Not blocking anything
     → Created: 4 hours ago

╔══════════════════════════════════════════╗
║ ACTIVE AGENTS (3)                        ║
╚══════════════════════════════════════════╝
  🔄 task_agent_a9e9c7b: Project reflection analysis
     → Running: 90 minutes
     → Labels: analysis, reflection

  🔄 task_agent_a8f57b5: Graph query research
     → Running: 45 minutes
     → Labels: research, graph

  🔄 task_agent_a9d4ef7: CLI specs + wrapper implementation
     → Running: 60 minutes
     → Labels: implementation, cli

╔══════════════════════════════════════════╗
║ COMPLETED TODAY (5)                      ║
╚══════════════════════════════════════════╝
  ✅ Markdown-as-graph prototype (71 min, SUCCESS)
     → Deliverables: src/markdown-graph/*, MD_AS_GRAPH_ANALYSIS.md

  ✅ Graph & Knowledge CLIs (3.5 hrs, SUCCESS)
     → Deliverables: src/cli/graph.ts, src/cli/knowledge.ts, specs

  ✅ Task CLI P0 features (6 hrs, SUCCESS)
     → Deliverables: src/cli/task.ts, P0_FEATURES_IMPLEMENTED.md

  ✅ Project reflection analysis (90 min, SUCCESS)
     → Deliverables: PROJECT_REFLECTION.md

  ✅ Address proxy implementation (2 hrs, SUCCESS)
     → Deliverables: src/address.ts, ADDRESS_PROXY_DESIGN.md

╔══════════════════════════════════════════╗
║ BLOCKED ON YOU (2)                       ║
╚══════════════════════════════════════════╝
  🚫 Integrate graph query optimizations
     → Blocked by: task_review_21 (P1 review)
     → Waiting: 4 hours

  🚫 Build unified primer CLI
     → Blocked by: task_review_20 (P0 review)
     → Waiting: 2 hours

╔══════════════════════════════════════════╗
║ STATISTICS                               ║
╚══════════════════════════════════════════╝
  Tasks:     15 total (3 active, 5 completed, 3 pending review, 4 ready)
  Agents:    3 active, 8 completed today, 65 total (all-time)
  Reviews:   3 pending (1 P0, 1 P1, 1 P2)

  Today:     5 tasks completed, 8 agents launched
  This Week: 15 tasks completed, 65 agents launched

  Files:     22 markdown docs created today

Next Action: Review Task CLI optimizations (P0, blocking 2 tasks)
```

### Focused Views

**Reviews Only:**
```bash
$ primer reviews

PENDING REVIEWS (3)

🔴 P0 Review: Task CLI optimizations
   → Deliverables: src/cli/task.ts, docs
   → Blocking: 2 tasks
   → Created: 2 hours ago

🔴 P0 Review: Graph & Knowledge CLIs
   → Deliverables: src/cli/graph.ts, src/cli/knowledge.ts
   → Blocking: 1 task
   → Created: 30 minutes ago

🟡 P1 Review: Markdown-as-graph
   → Deliverables: src/markdown-graph/*
   → Not blocking
   → Created: 4 hours ago
```

**Attention (P0 only):**
```bash
$ primer attention

HIGH PRIORITY REVIEWS (2)

🔴 P0 Review: Task CLI optimizations
   → Blocking: 2 tasks
   → Created: 2 hours ago
   → Command: primer reviews show task_review_20

🔴 P0 Review: Graph & Knowledge CLIs
   → Blocking: 1 task
   → Created: 30 minutes ago
   → Command: primer reviews show task_review_21
```

**Next Action:**
```bash
$ primer next

NEXT RECOMMENDED ACTION:

Review: Task CLI optimizations (P0)

Priority:     IMMEDIATE (blocking 2 tasks)
Deliverables: - src/cli/task.ts
              - P0_FEATURES_IMPLEMENTED.md
              - examples/task-usage/
Created:      2 hours ago

Commands:
  Start review:    task update task_review_20 start
  View details:    task show task_review_20
  Approve:         task update task_review_20 complete "message"
  Request changes: task update task_review_20 block "feedback"
```

---

## Color and Formatting

### Console Colors (Optional Enhancement)

Use ANSI colors for better visual hierarchy:

```typescript
// Colors
const RED = '\x1b[31m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const BLUE = '\x1b[34m';
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';

// Priority colors
function colorize(priority: number, text: string): string {
  const color = priority === 0 ? RED : priority === 1 ? YELLOW : GREEN;
  return `${color}${text}${RESET}`;
}
```

### Box Drawing

Use Unicode box drawing characters for clean borders:

```
╔═══╗
║   ║
╚═══╝
```

---

## Data Refresh

### Dashboard Update Frequency

**Static Snapshot (v1):**
- Dashboard shows data as of command execution
- User runs `primer dashboard` to refresh

**Auto-refresh (v2, future):**
```bash
# Watch mode (refresh every 30 seconds)
primer dashboard --watch

# Refresh interval
primer dashboard --watch --interval 10
```

---

## Testing Strategy

### Manual Testing

```bash
# Test 1: Empty dashboard (no tasks)
rm tasks.json
task init
primer dashboard
# Expected: All sections empty or minimal

# Test 2: Create review tasks
task add "Review: Test 1" --labels review --priority P0
task add "Review: Test 2" --labels review --priority P1
primer dashboard
# Expected: 2 reviews in attention section

# Test 3: Complete tasks
task update task_1 complete
primer dashboard
# Expected: 1 review in attention, 1 in completed

# Test 4: Blocking dependencies
task add "Blocked task" --depends task_2
primer dashboard
# Expected: "Blocked on You" section shows 1 task
```

### Integration Testing

```bash
# Full workflow test
bun test src/cli/dashboard.test.ts
```

---

## Migration from Current State

### Step 1: Identify Current In-Flight Work

As of 2026-01-16 19:16 EST, create review tasks for recent agent completions:

```bash
# Based on PROJECT_REFLECTION.md, these agents likely completed:
task add "Review: Project reflection analysis" \
  --labels review,analysis --priority P2 \
  --deliverables "PROJECT_REFLECTION.md"

task add "Review: Graph query research" \
  --labels review,research --priority P1 \
  --deliverables "GRAPH_QUERY_RESEARCH.md"

task add "Review: Task CLI optimizations" \
  --labels review,implementation --priority P0 \
  --deliverables "src/cli/task.ts,P0_FEATURES_IMPLEMENTED.md"
```

### Step 2: Test Dashboard

```bash
# Build primer CLI (if needed)
bun build src/cli/primer.ts --target bun --outfile primer

# Run dashboard
./primer dashboard
```

### Step 3: User Feedback

Show user the dashboard and gather feedback:
- Is priority ordering correct?
- Are sections useful?
- Missing information?
- Too much/too little detail?

---

## Future Enhancements

### Phase 2: Advanced Features

**Live Updates:**
```bash
primer dashboard --live
# Dashboard updates in real-time as tasks change
```

**Filtering:**
```bash
primer dashboard --labels research
primer dashboard --since yesterday
primer dashboard --priority P0,P1
```

**Export:**
```bash
primer dashboard --format json > dashboard.json
primer dashboard --format markdown > DASHBOARD.md
```

**Notifications:**
```bash
# Desktop notification on P0 review
primer watch --notify-on P0
```

### Phase 3: Web Dashboard

**HTML/Web UI:**
- Real-time web dashboard
- Interactive task management
- Drag-and-drop priority changes
- Visual dependency graphs

---

## Success Criteria

### Must Achieve

- [ ] User can see all pending reviews in one command
- [ ] Dashboard shows active agents
- [ ] User knows "what needs attention NOW"
- [ ] Blocked tasks visible
- [ ] Statistics provide context

### Should Achieve

- [ ] Dashboard loads in < 1 second
- [ ] Output is readable and clear
- [ ] Priority ordering is correct
- [ ] User can drill down into details

### Nice to Have

- [ ] Auto-refresh capability
- [ ] Color-coded output
- [ ] Export to JSON/Markdown
- [ ] Desktop notifications

---

## Related Documents

- [REVIEW_WORKFLOW_DESIGN.md](./REVIEW_WORKFLOW_DESIGN.md) - Review task pattern
- [IMPLEMENTATION_PLAN.md](./IMPLEMENTATION_PLAN.md) - Rollout plan
- [AGENT_NOTE_PRIMER_ARCHITECTURE.md](./AGENT_NOTE_PRIMER_ARCHITECTURE.md) - Primer CLI architecture

---

## Document History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-16 19:16 EST | 1.0 | Initial design specification |

---

**END OF DOCUMENT**
