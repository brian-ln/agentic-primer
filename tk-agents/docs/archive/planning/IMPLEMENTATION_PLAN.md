# Implementation Plan: Review Workflow and Activity Dashboard

**Created:** 2026-01-16 19:16 EST
**Version:** 1.0
**Status:** Implementation Roadmap

---

## Overview

This plan details the phased implementation of:

1. **Review Workflow** - Systematic tracking of deliverable reviews
2. **Activity Dashboard** - Visibility into all work streams
3. **Primer CLI** - Unified interface for task/graph/knowledge/dashboard

**Goal:** Reduce user cognitive overload from parallel agent execution.

---

## Phased Rollout

### Phase 1: Foundation (Week 1) - IMMEDIATE

**Timeline:** 3-5 days
**Effort:** 10-15 hours
**Deliverables:**
- Review workflow documentation
- CLAUDE.md instructions for agents
- Manual review task creation
- Migration of current in-flight work

**No code changes required - just documentation and process.**

### Phase 2: Dashboard CLI (Week 2-3)

**Timeline:** 7-10 days
**Effort:** 20-30 hours
**Deliverables:**
- Primer CLI wrapper
- Dashboard commands
- Review queries
- Stats calculator

**Code Implementation - builds on existing task CLI.**

### Phase 3: Refinement (Week 4+)

**Timeline:** Ongoing
**Effort:** 5-10 hours/week
**Deliverables:**
- Hook automation (optional)
- Advanced features
- Analytics
- Optimizations

**Iterative improvements based on usage.**

---

## Phase 1: Foundation (IMMEDIATE)

### Objective

Establish review workflow pattern using existing Task CLI, no new code required.

### Tasks

#### Task 1.1: Update CLAUDE.md (1 hour)

**File:** `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/CLAUDE.md`

**Add section:**

```markdown
## Review Workflow for Agent Deliverables

### CRITICAL: Create Review Task on Completion

When you complete agent work with deliverables:

1. **Create review task BEFORE announcing:**

```bash
task add "Review: <concise-description>" \
  --parent <your-agent-task-id> \
  --labels review,<work-type> \
  --priority <P0-P3> \
  --deliverables "<file1>,<file2>" \
  --assignee bln
```

2. **Priority Guide:**
   - **P0:** Blocks other work, critical path
   - **P1:** Feature work, significant changes
   - **P2:** Research, documentation (default)
   - **P3:** Experiments, drafts

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
task add "Review: Graph query optimization research" \
  --parent task_agent_a8f57b5 \
  --labels review,research,graph \
  --priority P1 \
  --deliverables "GRAPH_QUERY_RESEARCH.md,examples/" \
  --assignee bln

# Announce
COMPLETION: Graph query optimization research

Deliverables:
- GRAPH_QUERY_RESEARCH.md (57KB, 10 optimization patterns)
- examples/ (5 query examples)

Review Task: task_review_21 (P1, research)

Status: COMPLETE - Ready for review
```

### Review Commands

```bash
# List pending reviews
task list --label review --status created

# Show review details
task show task_review_XX

# Approve review
task update task_review_XX complete "Approved - looks good"

# Request changes
task update task_review_XX block "Need clarification on X"
```
```

**Testing:**
```bash
# Verify CLAUDE.md updated
cat CLAUDE.md | grep "Review Workflow"
```

#### Task 1.2: Migrate Current In-Flight Work (2 hours)

**Identify recent agent completions** (as of 2026-01-16):

Based on PROJECT_REFLECTION.md and root directory MD files:

```bash
cd /Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents

# Initialize tasks.json if needed
if [ ! -f tasks.json ]; then
  bun src/cli/task.ts init
fi

# Create review tasks for recent deliverables
# (These are likely unreviewed based on user's "firestorm" comment)

# 1. Project reflection
bun src/cli/task.ts add "Review: Project reflection and architecture analysis" \
  --labels review,analysis,reflection \
  --priority P2 \
  --deliverables "PROJECT_REFLECTION.md"

# 2. Graph query research
bun src/cli/task.ts add "Review: Graph query optimization research" \
  --labels review,research,graph \
  --priority P1 \
  --deliverables "GRAPH_QUERY_RESEARCH.md"

# 3. Task CLI P0 features
bun src/cli/task.ts add "Review: Task CLI P0 features implementation" \
  --labels review,implementation,cli \
  --priority P0 \
  --deliverables "src/cli/task.ts,P0_FEATURES_IMPLEMENTED.md,examples/task-usage/"

# 4. Markdown-as-graph
bun src/cli/task.ts add "Review: Markdown-as-graph prototype and analysis" \
  --labels review,research,graph \
  --priority P2 \
  --deliverables "src/markdown-graph/,MD_AS_GRAPH_ANALYSIS.md,MARKDOWN_GRAPH_INTEGRATION.md"

# 5. CLI improvements
bun src/cli/task.ts add "Review: CLI audit and improvement plan" \
  --labels review,planning,cli \
  --priority P1 \
  --deliverables "CLI_AUDIT.md,CLI_IMPROVEMENTS.md,CLI_IMPLEMENTATION_PLAN.md"

# 6. Actor model evolution
bun src/cli/task.ts add "Review: Actor model design iterations" \
  --labels review,architecture \
  --priority P2 \
  --deliverables "ACTOR_NODE_MODEL.md,ACTOR_NODE_MODEL_V2.md"

# 7. Spec extensions
bun src/cli/task.ts add "Review: Specification language extensions" \
  --labels review,specifications \
  --priority P3 \
  --deliverables "SPEC_EXTENSIONS.md,SPEC_EXTENSIONS_V2.md"

# List all review tasks
bun src/cli/task.ts list --label review

# Check priority distribution
bun src/cli/task.ts list --label review --priority P0
bun src/cli/task.ts list --label review --priority P1
```

**Document mapping:**

Create `docs/review-migration-2026-01-16.md`:

```markdown
# Review Task Migration

**Date:** 2026-01-16
**Migrated by:** Background Agent (Review Workflow Design)

## Migrated Reviews

| Review Task | Priority | Deliverables | Notes |
|-------------|----------|--------------|-------|
| task_1 | P2 | PROJECT_REFLECTION.md | 52KB analysis doc |
| task_2 | P1 | GRAPH_QUERY_RESEARCH.md | 57KB research |
| task_3 | P0 | src/cli/task.ts, docs | Blocks other work |
| task_4 | P2 | Markdown-graph prototype | Exploratory |
| task_5 | P1 | CLI audit + plans | Planning docs |
| task_6 | P2 | Actor model iterations | Design docs |
| task_7 | P3 | Spec extensions | Future work |

## Next Steps

1. User reviews P0 task first (Task CLI implementation)
2. Then P1 tasks (graph research, CLI plans)
3. P2/P3 as time permits

## Commands

```bash
# See all pending reviews
task list --label review --status created

# Start with P0
task show task_3
task update task_3 start
# (review files)
task update task_3 complete "Approved"
```
```

**Testing:**
```bash
# Verify tasks created
bun src/cli/task.ts list --label review
# Should show 7 review tasks

# Verify priority distribution
bun src/cli/task.ts list --label review --priority P0 | grep -c task
# Should show 1

bun src/cli/task.ts list --label review --priority P1 | grep -c task
# Should show 2
```

#### Task 1.3: Test Review Workflow (1 hour)

**End-to-end test:**

```bash
# Create a test agent task
bun src/cli/task.ts add "Agent: Test workflow" \
  --labels agent,test \
  --priority P2

# Note task ID (e.g., task_8)

# Simulate agent completion: create review task
bun src/cli/task.ts add "Review: Test workflow results" \
  --parent task_8 \
  --labels review,test \
  --priority P2 \
  --deliverables "test-output.txt"

# Simulate user review
echo "Test deliverable content" > test-output.txt

# Start review
bun src/cli/task.ts update task_9 start

# Check status
bun src/cli/task.ts show task_9

# Complete review
bun src/cli/task.ts update task_9 complete "Test approved"

# Verify task state
bun src/cli/task.ts show task_9 | grep "State:"
# Should show: completed
```

**Cleanup test:**
```bash
rm test-output.txt
bun src/cli/task.ts delete task_8 --yes
bun src/cli/task.ts delete task_9 --yes
```

#### Task 1.4: Document Review Workflow (30 min)

**Create:** `docs/REVIEW_WORKFLOW_GUIDE.md` (user-facing guide)

```markdown
# Review Workflow Guide

Quick reference for managing review tasks.

## Creating Review Tasks

When agent work completes:

```bash
task add "Review: <description>" \
  --parent <agent-task-id> \
  --labels review,<type> \
  --priority P0-P3 \
  --deliverables "<files>"
```

## Reviewing Deliverables

1. **List pending reviews:**
   ```bash
   task list --label review --status created
   ```

2. **Start review:**
   ```bash
   task update task_review_XX start
   ```

3. **Review files** (read code, test, verify)

4. **Approve or request changes:**
   ```bash
   # Approve
   task update task_review_XX complete "Looks good"

   # Request changes
   task update task_review_XX block "Issue: <specific feedback>"
   ```

## Priority Guide

- **P0:** Immediate (< 1 hour) - blocks other work
- **P1:** Same day (< 8 hours) - high priority
- **P2:** Within 2 days - normal
- **P3:** When convenient - low priority

## Examples

See [REVIEW_WORKFLOW_DESIGN.md](../REVIEW_WORKFLOW_DESIGN.md) for detailed examples.
```

### Phase 1 Success Criteria

- [x] CLAUDE.md updated with review workflow
- [ ] 7+ review tasks created for recent work
- [ ] User can list pending reviews
- [ ] User can complete review workflow end-to-end
- [ ] Documentation guide created

### Phase 1 Testing

```bash
# Acceptance test
cd /Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents

# Test 1: CLAUDE.md includes review workflow
grep "Review Workflow" CLAUDE.md
# Expected: Match found

# Test 2: Review tasks exist
bun src/cli/task.ts list --label review
# Expected: 7+ tasks listed

# Test 3: Priority filtering works
bun src/cli/task.ts list --label review --priority P0
# Expected: 1 task (Task CLI implementation)

# Test 4: User can review
bun src/cli/task.ts show task_3
# Expected: Task details with deliverables

# PASS if all 4 tests pass
```

---

## Phase 2: Dashboard CLI (Week 2-3)

### Objective

Build unified primer CLI with dashboard commands for visibility.

### Prerequisites

- Phase 1 complete and tested
- Review workflow in active use for 3-5 days
- User feedback collected

### Tasks

#### Task 2.1: Primer CLI Wrapper (4 hours)

**File:** `src/cli/primer.ts`

```typescript
#!/usr/bin/env bun

/**
 * Primer CLI - Unified interface for task/graph/knowledge/dashboard
 *
 * Sub-program delegation pattern:
 * - primer task <args>    → delegates to task.ts
 * - primer graph <args>   → delegates to graph.ts
 * - primer knowledge <args> → delegates to knowledge.ts
 * - primer dashboard      → delegates to dashboard.ts
 * - primer reviews        → delegates to dashboard.ts
 *
 * Usage:
 *   primer <subsystem> <command> [args]
 *
 * Subsystems:
 *   task       - Task management
 *   graph      - Graph queries
 *   knowledge  - Knowledge queries
 *   dashboard  - Activity dashboard
 *   reviews    - Pending reviews (alias for dashboard reviews)
 *   agents     - Active agents (alias for dashboard agents)
 *   completed  - Completed work (alias for dashboard completed)
 *   stats      - Statistics (alias for dashboard stats)
 */

const subsystem = process.argv[2];
const args = process.argv.slice(3);

// Built-in subsystems
const builtIn: Record<string, string> = {
  'task': './task.ts',
  'graph': './graph.ts',
  'knowledge': './knowledge.ts',
  'dashboard': './dashboard.ts',
  // Aliases
  'reviews': './dashboard.ts',
  'agents': './dashboard.ts',
  'completed': './dashboard.ts',
  'blocked': './dashboard.ts',
  'stats': './dashboard.ts',
  'attention': './dashboard.ts',
  'next': './dashboard.ts',
};

if (!subsystem || subsystem === '--help' || subsystem === '-h') {
  console.log('Primer CLI - Unified interface for task/graph/knowledge/dashboard');
  console.log('');
  console.log('Usage:');
  console.log('  primer <subsystem> <command> [args]');
  console.log('');
  console.log('Subsystems:');
  console.log('  task              Task management (add, list, update, etc.)');
  console.log('  graph             Graph queries');
  console.log('  knowledge         Knowledge graph queries');
  console.log('  dashboard         Activity dashboard (all views)');
  console.log('');
  console.log('Dashboard Commands:');
  console.log('  reviews           List pending reviews');
  console.log('  agents            List active agents');
  console.log('  completed [period] Show completed work (today, week, all)');
  console.log('  blocked           Show tasks blocked on you');
  console.log('  stats [period]    Show statistics');
  console.log('  attention         Show high-priority reviews (P0)');
  console.log('  next              Show next recommended action');
  console.log('');
  console.log('Examples:');
  console.log('  primer task list --label review');
  console.log('  primer dashboard');
  console.log('  primer reviews --priority P0');
  console.log('  primer agents');
  console.log('  primer next');
  process.exit(0);
}

if (builtIn[subsystem]) {
  // Resolve path relative to this file
  const subcommandPath = new URL(builtIn[subsystem], import.meta.url).pathname;

  // Spawn subcommand
  const proc = Bun.spawn(['bun', subcommandPath, subsystem, ...args], {
    stdio: ['inherit', 'inherit', 'inherit'],
  });

  // Wait for completion
  await proc.exited;
  process.exit(proc.exitCode || 0);
} else {
  // Try external primer-* command (plugin support)
  try {
    const proc = Bun.spawn([`primer-${subsystem}`, ...args], {
      stdio: ['inherit', 'inherit', 'inherit'],
    });
    await proc.exited;
    process.exit(proc.exitCode || 0);
  } catch (err) {
    console.error(`Error: Unknown subsystem '${subsystem}'`);
    console.error(`Try 'primer --help' for usage information.`);
    process.exit(1);
  }
}
```

**Testing:**
```bash
# Build and test
bun build src/cli/primer.ts --target bun --outfile primer
chmod +x primer

# Test help
./primer --help

# Test delegation
./primer task list
./primer reviews  # Should delegate to dashboard.ts
```

#### Task 2.2: Dashboard Module (8 hours)

**File:** `src/cli/dashboard.ts`

```typescript
#!/usr/bin/env bun

/**
 * Dashboard CLI - Activity and review visibility
 *
 * Commands:
 *   dashboard                    Full dashboard (all components)
 *   reviews [--priority PX]      Pending reviews
 *   agents                       Active agents
 *   completed [period]           Completed work (today, week, all)
 *   blocked                      Tasks blocked on user
 *   stats [period]               Statistics
 *   attention                    High-priority reviews (P0)
 *   next                         Next recommended action
 */

import { Graph } from '../graph.ts';
import type { TaskProperties } from '../types.ts';
import { readFileSync, existsSync } from 'fs';
import { resolve } from 'path';

const TASKS_FILE = 'tasks.json';

// Load graph from tasks.json
async function loadGraph(filePath: string): Promise<Graph> {
  if (!existsSync(filePath)) {
    throw new Error(`Tasks file not found: ${filePath}`);
  }

  const content = readFileSync(filePath, 'utf-8');
  const taskFile = JSON.parse(content);

  // Recreate graph from dump
  const graph = new Graph();
  // ... (use same logic as task.ts loadGraph)
  return graph;
}

// Get all tasks
function getAllTasks(graph: Graph): TaskProperties[] {
  return graph.getNodeIds()
    .filter(id => {
      const props = graph.getNodeProperties(id);
      return props?.type === 'task';
    })
    .map(id => graph.getNodeProperties(id) as TaskProperties);
}

// Get review tasks
function getReviewTasks(graph: Graph, options?: { priority?: number }): TaskProperties[] {
  let reviews = getAllTasks(graph).filter(task =>
    task.labels?.includes('review') &&
    task.state === 'created'
  );

  if (options?.priority !== undefined) {
    reviews = reviews.filter(task => task.priority === options.priority);
  }

  // Sort by priority, blocking count, age
  return reviews.sort((a, b) => {
    if (a.priority !== b.priority) return (a.priority || 99) - (b.priority || 99);
    const blockingA = getBlockingCount(graph, a.id);
    const blockingB = getBlockingCount(graph, b.id);
    if (blockingA !== blockingB) return blockingB - blockingA;
    return new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime();
  });
}

// Get blocking count
function getBlockingCount(graph: Graph, reviewTaskId: string): number {
  const blockedTasks = graph.getEdgesTo(reviewTaskId)
    .filter(edge => edge.type === 'depends_on');
  return blockedTasks.length;
}

// Get age string
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

// Priority icon
function getPriorityIcon(priority?: number): string {
  if (priority === 0) return '🔴';
  if (priority === 1) return '🟡';
  if (priority === 2) return '🟢';
  return '⚪';
}

// Render components
async function renderReviews(graph: Graph, options?: { priority?: number }) {
  console.log('╔══════════════════════════════════════════╗');
  console.log('║ PENDING REVIEWS                          ║');
  console.log('╚══════════════════════════════════════════╝');

  const reviews = getReviewTasks(graph, options);

  if (reviews.length === 0) {
    console.log('  No pending reviews');
    console.log();
    return;
  }

  for (const review of reviews) {
    const icon = getPriorityIcon(review.priority);
    const blocking = getBlockingCount(graph, review.id);
    const age = getAge(review.createdAt);

    console.log(`  ${icon} P${review.priority || '?'} ${review.goal}`);
    console.log(`     → Deliverables: ${review.desiredDeliverables.join(', ')}`);
    if (blocking > 0) {
      console.log(`     → Blocking: ${blocking} task${blocking > 1 ? 's' : ''}`);
    }
    console.log(`     → Created: ${age}`);
    console.log();
  }
}

async function renderAgents(graph: Graph) {
  console.log('╔══════════════════════════════════════════╗');
  console.log('║ ACTIVE AGENTS                            ║');
  console.log('╚══════════════════════════════════════════╝');

  const agents = getAllTasks(graph).filter(task =>
    task.labels?.includes('agent') &&
    task.state === 'active'
  );

  if (agents.length === 0) {
    console.log('  No active agents');
    console.log();
    return;
  }

  for (const agent of agents) {
    const duration = agent.startedAt
      ? getAge(agent.startedAt).replace(' ago', '')
      : 'unknown';

    console.log(`  🔄 ${agent.id}: ${agent.goal}`);
    console.log(`     → Running: ${duration}`);
    if (agent.labels && agent.labels.length > 1) {
      const otherLabels = agent.labels.filter(l => l !== 'agent');
      console.log(`     → Labels: ${otherLabels.join(', ')}`);
    }
    console.log();
  }
}

async function renderCompleted(graph: Graph, period: string = 'today') {
  console.log('╔══════════════════════════════════════════╗');
  console.log(`║ COMPLETED ${period.toUpperCase().padEnd(32)}║`);
  console.log('╚══════════════════════════════════════════╝');

  const now = new Date();
  let cutoff: Date;

  if (period === 'today') {
    cutoff = new Date(now.getFullYear(), now.getMonth(), now.getDate());
  } else if (period === 'week') {
    cutoff = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
  } else {
    cutoff = new Date(0); // All time
  }

  const completed = getAllTasks(graph).filter(task =>
    task.state === 'completed' &&
    task.completedAt &&
    new Date(task.completedAt) >= cutoff
  );

  if (completed.length === 0) {
    console.log(`  No tasks completed ${period === 'all' ? 'yet' : period}`);
    console.log();
    return;
  }

  for (const task of completed) {
    const duration = task.startedAt && task.completedAt
      ? Math.floor((new Date(task.completedAt).getTime() - new Date(task.startedAt).getTime()) / 60000)
      : null;

    console.log(`  ✅ ${task.goal} ${duration ? `(${duration} min)` : ''}`);
    if (task.desiredDeliverables.length > 0) {
      console.log(`     → Deliverables: ${task.desiredDeliverables.join(', ')}`);
    }
    console.log();
  }
}

async function renderBlocked(graph: Graph) {
  console.log('╔══════════════════════════════════════════╗');
  console.log('║ BLOCKED ON YOU                           ║');
  console.log('╚══════════════════════════════════════════╝');

  // Find tasks that depend on review tasks
  const reviews = getAllTasks(graph).filter(task =>
    task.labels?.includes('review') &&
    task.state !== 'completed'
  );

  let blockedTasks: Array<{ task: TaskProperties; blocker: TaskProperties; waitTime: string }> = [];

  for (const review of reviews) {
    const dependents = graph.getEdgesTo(review.id)
      .filter(edge => edge.type === 'depends_on')
      .map(edge => edge.fromId);

    for (const depId of dependents) {
      const depTask = graph.getNodeProperties(depId) as TaskProperties;
      if (depTask && depTask.state !== 'completed') {
        const waitTime = getAge(review.createdAt);
        blockedTasks.push({ task: depTask, blocker: review, waitTime });
      }
    }
  }

  if (blockedTasks.length === 0) {
    console.log('  No tasks blocked on you');
    console.log();
    return;
  }

  for (const { task, blocker, waitTime } of blockedTasks) {
    console.log(`  🚫 ${task.goal}`);
    console.log(`     → Blocked by: ${blocker.id} (P${blocker.priority || '?'} review)`);
    console.log(`     → Waiting: ${waitTime}`);
    console.log();
  }
}

async function renderStats(graph: Graph) {
  console.log('╔══════════════════════════════════════════╗');
  console.log('║ STATISTICS                               ║');
  console.log('╚══════════════════════════════════════════╝');

  const allTasks = getAllTasks(graph);
  const active = allTasks.filter(t => t.state === 'active').length;
  const completed = allTasks.filter(t => t.state === 'completed').length;
  const reviews = allTasks.filter(t => t.labels?.includes('review') && t.state === 'created').length;
  const ready = allTasks.filter(t => t.state === 'ready' || t.state === 'created').length;

  const agents = allTasks.filter(t => t.labels?.includes('agent'));
  const activeAgents = agents.filter(t => t.state === 'active').length;

  // Today's completed
  const now = new Date();
  const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
  const completedToday = allTasks.filter(t =>
    t.state === 'completed' &&
    t.completedAt &&
    new Date(t.completedAt) >= today
  ).length;

  console.log(`  Tasks:     ${allTasks.length} total (${active} active, ${completed} completed, ${reviews} pending review, ${ready} ready)`);
  console.log(`  Agents:    ${activeAgents} active, ${completedToday} completed today, ${agents.length} total`);
  console.log(`  Reviews:   ${reviews} pending`);
  console.log();
}

async function renderDashboard(graph: Graph) {
  const now = new Date();
  console.log(`\nPrimer Dashboard - ${now.toLocaleString('en-US', { timeZone: 'America/New_York', dateStyle: 'short', timeStyle: 'short' })} EST\n`);

  await renderReviews(graph);
  await renderAgents(graph);
  await renderCompleted(graph, 'today');
  await renderBlocked(graph);
  await renderStats(graph);

  // Next action
  const reviews = getReviewTasks(graph);
  if (reviews.length > 0) {
    const next = reviews[0];
    console.log(`Next Action: ${next.goal} (P${next.priority || '?'}${getBlockingCount(graph, next.id) > 0 ? `, blocking ${getBlockingCount(graph, next.id)} task(s)` : ''})`);
  }
  console.log();
}

async function renderNext(graph: Graph) {
  const reviews = getReviewTasks(graph);

  if (reviews.length === 0) {
    console.log('\nNo pending reviews. All caught up!\n');
    return;
  }

  const next = reviews[0];
  const blocking = getBlockingCount(graph, next.id);

  console.log('\nNEXT RECOMMENDED ACTION:\n');
  console.log(`Review: ${next.goal}`);
  console.log();
  console.log(`Priority:     ${blocking > 0 ? 'IMMEDIATE' : 'HIGH'} ${blocking > 0 ? `(blocking ${blocking} task(s))` : ''}`);
  console.log(`Deliverables: ${next.desiredDeliverables.map(d => `\n              - ${d}`).join('')}`);
  console.log(`Created:      ${getAge(next.createdAt)}`);
  console.log();
  console.log('Commands:');
  console.log(`  Start review:    task update ${next.id} start`);
  console.log(`  View details:    task show ${next.id}`);
  console.log(`  Approve:         task update ${next.id} complete "message"`);
  console.log(`  Request changes: task update ${next.id} block "feedback"`);
  console.log();
}

// Main
async function main() {
  const command = process.argv[2];
  const args = process.argv.slice(3);

  const filePath = resolve(TASKS_FILE);
  const graph = await loadGraph(filePath);

  switch (command) {
    case 'dashboard':
      await renderDashboard(graph);
      break;

    case 'reviews':
    case 'attention': {
      const priority = args.includes('--priority')
        ? parseInt(args[args.indexOf('--priority') + 1].replace(/^P/, ''))
        : command === 'attention' ? 0 : undefined;
      await renderReviews(graph, { priority });
      break;
    }

    case 'agents':
      await renderAgents(graph);
      break;

    case 'completed': {
      const period = args[0] || 'today';
      await renderCompleted(graph, period);
      break;
    }

    case 'blocked':
      await renderBlocked(graph);
      break;

    case 'stats': {
      await renderStats(graph);
      break;
    }

    case 'next':
      await renderNext(graph);
      break;

    default:
      console.error(`Unknown dashboard command: ${command}`);
      process.exit(1);
  }
}

main().catch(err => {
  console.error('Error:', err.message);
  process.exit(1);
});
```

**Testing:**
```bash
# Build
bun build src/cli/dashboard.ts --target bun

# Test commands
./primer dashboard
./primer reviews
./primer agents
./primer completed today
./primer next
```

#### Task 2.3: Integration Testing (2 hours)

**Test Script:** `tests/integration/dashboard.test.ts`

```typescript
import { test, expect, describe, beforeEach } from 'bun:test';
import { execSync } from 'child_process';
import { writeFileSync, unlinkSync, existsSync } from 'fs';

const TASKS_FILE = 'test-tasks.json';

describe('Dashboard Integration', () => {
  beforeEach(() => {
    // Clean up
    if (existsSync(TASKS_FILE)) unlinkSync(TASKS_FILE);
  });

  test('primer dashboard shows empty state', () => {
    // Initialize
    execSync('bun src/cli/task.ts init');

    // Run dashboard
    const output = execSync('./primer dashboard').toString();

    expect(output).toContain('No pending reviews');
    expect(output).toContain('No active agents');
  });

  test('primer reviews shows pending reviews', () => {
    // Setup
    execSync('bun src/cli/task.ts init');
    execSync('bun src/cli/task.ts add "Review: Test" --labels review --priority P0');

    // Run
    const output = execSync('./primer reviews').toString();

    expect(output).toContain('Review: Test');
    expect(output).toContain('P0');
  });

  test('primer next shows highest priority review', () => {
    // Setup
    execSync('bun src/cli/task.ts init');
    execSync('bun src/cli/task.ts add "Review: Low" --labels review --priority P2');
    execSync('bun src/cli/task.ts add "Review: High" --labels review --priority P0');

    // Run
    const output = execSync('./primer next').toString();

    expect(output).toContain('Review: High');
    expect(output).toContain('P0');
  });
});
```

**Run tests:**
```bash
bun test tests/integration/dashboard.test.ts
```

#### Task 2.4: Documentation (2 hours)

**Create:** `docs/PRIMER_CLI_GUIDE.md`

```markdown
# Primer CLI Guide

The unified interface for task/graph/knowledge/dashboard operations.

## Installation

```bash
# Build primer wrapper
bun build src/cli/primer.ts --target bun --outfile primer
chmod +x primer

# Optional: Add to PATH
sudo ln -s $(pwd)/primer /usr/local/bin/primer
```

## Commands

### Task Management

```bash
primer task add "Task description"
primer task list
primer task update <id> complete
```

### Dashboard

```bash
# Full dashboard
primer dashboard

# Pending reviews
primer reviews

# High-priority reviews only
primer reviews --priority P0
# or
primer attention

# Active agents
primer agents

# Completed work
primer completed today
primer completed week

# What's blocked on you
primer blocked

# Statistics
primer stats

# Next recommended action
primer next
```

## Examples

See [examples/](../examples/) for full examples.
```

### Phase 2 Success Criteria

- [ ] Primer CLI wrapper implemented
- [ ] Dashboard commands functional
- [ ] Integration tests pass
- [ ] User can see full dashboard
- [ ] User can drill down into sections
- [ ] Documentation complete

### Phase 2 Testing

```bash
# Acceptance tests
./primer --help
# Expected: Help text shown

./primer dashboard
# Expected: Full dashboard rendered

./primer reviews --priority P0
# Expected: P0 reviews only

./primer next
# Expected: Next recommended action

# Integration tests
bun test tests/integration/
# Expected: All tests pass
```

---

## Phase 3: Refinement (Week 4+)

### Objective

Iterative improvements based on real usage.

### Potential Enhancements

#### Enhancement 3.1: Hook Automation (Optional)

**If:** Manual review task creation becomes burdensome
**Then:** Implement PostToolUse hook to auto-create review tasks

**File:** `.claude/hooks/post-tool-use.sh`

```bash
#!/bin/bash
# Auto-create review task when agent completes

# Check if agent completion
if [[ "$TASK_OUTPUT" == *"COMPLETION:"* ]]; then
  # Extract description (simple heuristic)
  DESCRIPTION=$(echo "$TASK_OUTPUT" | grep "COMPLETION:" | cut -d: -f2 | xargs)

  # Create review task
  cd "$CLAUDE_PROJECT_DIR"
  task add "Review: $DESCRIPTION" \
    --labels review \
    --priority P2 \
    --assignee bln \
    --json > /dev/null

  echo "Created review task for: $DESCRIPTION"
fi

# Return continue
echo '{"continue": true}'
```

**Testing:**
```bash
# Enable hook
mkdir -p .claude/hooks
cp hooks/post-tool-use.sh .claude/hooks/
chmod +x .claude/hooks/post-tool-use.sh

# Test with agent completion
# (Launch agent, complete, verify review task created)
```

#### Enhancement 3.2: Advanced Filtering

```bash
# Filter dashboard by labels
primer dashboard --labels research,implementation

# Filter by date range
primer completed --since "2026-01-15"

# Filter by assignee
primer reviews --assignee bln
```

#### Enhancement 3.3: JSON Output

```bash
# Export dashboard data
primer dashboard --format json > dashboard.json

# Use in scripts
REVIEW_COUNT=$(primer reviews --json | jq '.data | length')
echo "Pending reviews: $REVIEW_COUNT"
```

#### Enhancement 3.4: Analytics

```bash
# Review metrics
primer analytics reviews
# Output:
# Total reviews: 42
# Avg time-to-review: 3.2 hours
# Approval rate: 85%

# Agent metrics
primer analytics agents
# Output:
# Total agents: 65
# Avg duration: 45 minutes
# Success rate: 92%
```

---

## Rollout Schedule

### Week 1: Foundation

**Days 1-2:**
- Update CLAUDE.md
- Migrate current work
- Test review workflow

**Days 3-5:**
- Use manual workflow
- Collect feedback
- Iterate on process

**End of Week 1:**
- Review workflow fully adopted
- All agents creating review tasks
- User comfortable with task CLI

### Week 2-3: Dashboard

**Days 8-10:**
- Build primer CLI wrapper
- Implement dashboard.ts
- Write integration tests

**Days 11-14:**
- Test dashboard with real data
- Refine UI/output
- Write documentation

**Days 15-17:**
- User acceptance testing
- Fix bugs
- Polish output

**End of Week 3:**
- Primer CLI deployed
- Dashboard in daily use
- User has full visibility

### Week 4+: Refinement

**Ongoing:**
- Monitor usage patterns
- Add requested features
- Optimize performance
- Consider hook automation

---

## Risk Mitigation

### Risk 1: User Doesn't Adopt Manual Workflow

**Mitigation:**
- Make CLAUDE.md instructions very explicit
- Provide examples in every agent context
- Review adoption in Week 1
- If <50% adoption, fast-track hook automation

### Risk 2: Dashboard Too Slow

**Mitigation:**
- Optimize graph queries
- Cache computed values
- Add --fast mode (fewer details)
- Profile and optimize hot paths

### Risk 3: Too Much/Too Little Information

**Mitigation:**
- Start with default views
- Add filtering options
- Allow customization (config file)
- Gather user feedback weekly

---

## Success Metrics

### Week 1 (Foundation)

- [ ] 100% of new agent work creates review tasks
- [ ] User reviews 5+ deliverables using workflow
- [ ] Zero lost context from missing reviews

### Week 3 (Dashboard)

- [ ] User checks dashboard daily
- [ ] Dashboard loads in <1 second
- [ ] User finds "next action" helpful
- [ ] Cognitive overload reduced (subjective)

### Week 4+ (Refinement)

- [ ] Review SLAs met (P0 <1hr, P1 <8hr)
- [ ] User satisfaction with visibility
- [ ] No missing reviews or lost work

---

## Maintenance Plan

### Weekly

- Review adoption metrics
- Check for issues/bugs
- Gather user feedback

### Monthly

- Review analytics
- Plan new features
- Optimize performance

### Quarterly

- Major version updates
- Architecture improvements
- Integration with new systems

---

## Appendix: Commands Reference

### Phase 1 Commands

```bash
# Task CLI (existing)
task add "..." --labels review --priority P0
task list --label review
task show <id>
task update <id> complete

# Review workflow
task list --label review --status created
task update <review-id> start
task update <review-id> complete "message"
```

### Phase 2 Commands

```bash
# Primer wrapper
primer task <args>
primer dashboard
primer reviews [--priority P0]
primer agents
primer completed [today|week|all]
primer blocked
primer stats
primer attention  # P0 reviews only
primer next       # Next recommended action
```

### Phase 3 Commands (Future)

```bash
# Advanced
primer dashboard --labels research
primer reviews --since yesterday
primer analytics reviews
primer watch --notify-on P0
```

---

## Related Documents

- [REVIEW_WORKFLOW_DESIGN.md](./REVIEW_WORKFLOW_DESIGN.md) - Review pattern details
- [ACTIVITY_DASHBOARD_DESIGN.md](./ACTIVITY_DASHBOARD_DESIGN.md) - Dashboard specs
- [AGENT_NOTE_PRIMER_ARCHITECTURE.md](./AGENT_NOTE_PRIMER_ARCHITECTURE.md) - Primer CLI architecture

---

## Document History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-16 19:16 EST | 1.0 | Initial implementation plan |

---

**END OF DOCUMENT**
