# Signals Migration Plan for Primer Workbench

**Date**: January 17, 2026
**Library**: @preact/signals-core
**Strategy**: Incremental adoption (low risk, reversible)
**Task ID**: task_35

---

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Phase 0: Pre-Flight Checks](#phase-0-pre-flight-checks)
4. [Phase 1: Infrastructure Setup](#phase-1-infrastructure-setup)
5. [Phase 2: Connection Status Component](#phase-2-connection-status-component)
6. [Phase 3: Stats Cards](#phase-3-stats-cards)
7. [Phase 4: Reviews List](#phase-4-reviews-list)
8. [Phase 5: Agents List](#phase-5-agents-list)
9. [Phase 6: Cleanup and Optimization](#phase-6-cleanup-and-optimization)
10. [Testing Strategy](#testing-strategy)
11. [Rollback Procedures](#rollback-procedures)
12. [Success Metrics](#success-metrics)

---

## Overview

### Migration Goals

1. **Eliminate manual render() calls**—replace with automatic reactivity
2. **Remove polling**—rely on WebSocket-driven updates
3. **Improve performance**—fine-grained DOM updates instead of full re-renders
4. **Maintain functionality**—zero regressions, all features work as before
5. **Keep bundle small**—≤ 2 KB increase

### Migration Strategy

**Incremental approach:** Migrate one component at a time, validate, then proceed.

**Benefits:**
- ✅ Low risk—can rollback individual phases
- ✅ Easy to test—validate each component independently
- ✅ Reversible—signals coexist with current code
- ✅ Gradual learning—team learns signals incrementally

**Timeline:** ~16-22 hours total (spread over 3-5 days)

---

## Prerequisites

### Required Knowledge

- ES6 modules (import/export)
- Basic reactive programming concepts
- Chrome DevTools (for performance profiling)

### Tools

- Bun runtime (already used in project)
- Chrome DevTools Performance tab
- Git (for version control and rollback)

### Recommended Reading

- [Preact Signals Guide](https://preactjs.com/guide/v10/signals/)
- `SIGNALS_COMPARISON.md` (in this repo)
- `SIGNALS_INTEGRATION_PROPOSAL.md` (in this repo)

---

## Phase 0: Pre-Flight Checks

**Duration**: 30 minutes
**Goal**: Establish baseline metrics and verify prerequisites

### Tasks

1. ✅ **Run baseline performance tests**

```bash
# Open workbench in Chrome
cd /Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents
bun src/cli/daemon.ts
```

Open `http://localhost:3000` in Chrome.

2. ✅ **Record baseline metrics**

Open Chrome DevTools → Performance tab:
- Record 10 seconds of interaction
- Note FPS (frames per second)
- Note memory usage
- Note number of re-renders

Document in `BASELINE_METRICS.md`:

```markdown
# Baseline Metrics (Before Signals)

- **Bundle size**: X KB (check Network tab)
- **Average FPS**: X fps (Performance tab)
- **Memory usage**: X MB (Memory tab)
- **API requests (60s)**: X requests (12 from polling + manual refreshes)
- **Re-render time**: X ms (Performance → User Timing)
```

3. ✅ **Verify test suite passes**

```bash
bun test  # If tests exist
```

4. ✅ **Create migration branch**

```bash
git checkout -b feature/signals-migration
git push -u origin feature/signals-migration
```

5. ✅ **Commit baseline**

```bash
git add BASELINE_METRICS.md
git commit -m "chore: record baseline metrics before signals migration"
```

### Validation

- [ ] Baseline metrics documented
- [ ] Current workbench functional
- [ ] Migration branch created
- [ ] Team aware of migration plan

---

## Phase 1: Infrastructure Setup

**Duration**: 1-2 hours
**Goal**: Install @preact/signals-core and create state module

### Tasks

#### 1.1 Install @preact/signals-core

```bash
cd /Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents
bun add @preact/signals-core
```

**Expected output:**
```
bun add v1.x.x
installed @preact/signals-core@1.6.0
```

#### 1.2 Verify bundle size increase

Check `node_modules/@preact/signals-core/package.json`:

```bash
cat node_modules/@preact/signals-core/package.json | grep '"version"'
```

Check size on Bundlephobia or locally:

```bash
du -sh node_modules/@preact/signals-core
```

**Expected:** ~20-30 KB uncompressed (1.6 KB gzipped)

#### 1.3 Create state module

```bash
touch browser/state.js
```

Copy content from `browser/signals-prototype/state.js` (created in this research):

```javascript
// browser/state.js
import { signal, computed } from '@preact/signals-core';

// Base signals
export const stats = signal(null);
export const reviews = signal([]);
export const agents = signal([]);
export const connectionStatus = signal('disconnected');
export const isLoading = signal(true);
export const error = signal(null);

// Computed signals
export const totalTasks = computed(() => stats.value?.totalTasks || 0);
export const tasksByState = computed(() => stats.value?.tasksByState || {});
export const activeAgentsCount = computed(() =>
  agents.value.filter(a => a.state === 'active').length
);
export const pendingReviewsCount = computed(() => reviews.value.length);
export const completedTasksCount = computed(() =>
  tasksByState.value.completed || 0
);
export const isConnected = computed(() =>
  connectionStatus.value === 'connected'
);

// ... (see full content in signals-prototype/state.js)
```

#### 1.4 Create effects directory

```bash
mkdir -p browser/effects
```

#### 1.5 Test signals in browser console

Add temporary test to `browser/app.js`:

```javascript
// browser/app.js (top of file)
import { signal, effect } from '@preact/signals-core';

// Test signal
const testSignal = signal(0);

effect(() => {
  console.log('Test signal value:', testSignal.value);
});

testSignal.value = 42; // Should log "Test signal value: 42"

console.log('Signals test complete—effects working!');
```

Reload workbench, check console for log messages.

**Expected:**
```
Test signal value: 0
Test signal value: 42
Signals test complete—effects working!
```

#### 1.6 Remove test code

```javascript
// browser/app.js
// Remove test signal code
```

#### 1.7 Commit changes

```bash
git add browser/state.js browser/effects/ package.json bun.lockb
git commit -m "feat: add @preact/signals-core infrastructure

- Install @preact/signals-core (1.6 KB gzipped)
- Create centralized state module
- Create effects directory structure
- Verify signals work in browser"
```

### Validation

- [ ] @preact/signals-core installed
- [ ] Bundle size increase ≤ 2 KB (check in browser Network tab)
- [ ] `browser/state.js` created
- [ ] `browser/effects/` directory exists
- [ ] Test signal logs correctly in console
- [ ] Changes committed to Git

### Estimated Effort

- Installation: 5 min
- State module creation: 30 min
- Testing: 15 min
- Documentation: 10 min
- **Total:** ~1 hour

---

## Phase 2: Connection Status Component

**Duration**: 2-3 hours
**Goal**: Replace first component with signals to prove concept

### Tasks

#### 2.1 Create connection status effect

```bash
touch browser/effects/connection-effects.js
```

Copy content from `browser/signals-prototype/connection-status-signals.js`:

```javascript
// browser/effects/connection-effects.js
import { effect } from '@preact/signals-core';
import { connectionStatusClass } from '../state.js';

export function setupConnectionEffects() {
  const statusEl = document.getElementById('connection-status');
  if (!statusEl) return;

  const dot = statusEl.querySelector('.inline-block');
  const text = statusEl.querySelector('span:last-child');

  if (!dot || !text) return;

  effect(() => {
    const { dot: dotClass, text: textClass, label } = connectionStatusClass.value;

    dot.className = `inline-block w-2 h-2 rounded-full ${dotClass}`;
    text.className = textClass;
    text.textContent = label;
  });
}
```

#### 2.2 Update app.js

**Before** (`browser/app.js` lines 77-96):

```javascript
function updateConnectionStatus(status) {
  const statusEl = document.getElementById("connection-status");
  if (!statusEl) return;

  const dot = statusEl.querySelector(".inline-block");
  const text = statusEl.querySelector("span:last-child");

  if (status === "connected") {
    dot.classList.remove("bg-gray-400", "bg-red-500");
    dot.classList.add("bg-green-500");
    text.textContent = "Connected";
    text.classList.remove("text-gray-500", "text-red-500");
    text.classList.add("text-green-600");
  } else {
    dot.classList.remove("bg-green-500", "bg-gray-400");
    dot.classList.add("bg-red-500");
    text.textContent = "Disconnected";
    text.classList.remove("text-gray-500", "text-green-600");
    text.classList.add("text-red-500");
  }
}

// Usage
ws.on("connection", (data) => {
  updateConnectionStatus(data.status);
});
```

**After**:

```javascript
import { setConnectionStatus } from './state.js';
import { setupConnectionEffects } from './effects/connection-effects.js';

// In init() function
setupConnectionEffects();

// Replace updateConnectionStatus() usage
ws.on('connection', (data) => {
  setConnectionStatus(data.status); // Signal update—DOM updates automatically
});

// Remove old updateConnectionStatus() function (delete lines 77-96)
```

#### 2.3 Test connection status changes

```bash
bun src/cli/daemon.ts
```

Open `http://localhost:3000`:

1. Open DevTools → Console
2. Manually trigger WebSocket events:
   ```javascript
   // In browser console
   import { setConnectionStatus } from './state.js';
   setConnectionStatus('connected');   // Should show green dot
   setConnectionStatus('disconnected'); // Should show red dot
   ```
3. Verify DOM updates automatically

#### 2.4 Validate no regressions

- [ ] Connection status indicator updates on WebSocket connect/disconnect
- [ ] Green dot when connected, red when disconnected
- [ ] Text updates correctly ("Connected" / "Disconnected")
- [ ] No console errors

#### 2.5 Commit changes

```bash
git add browser/app.js browser/effects/connection-effects.js browser/state.js
git commit -m "feat: migrate connection status to signals

- Replace updateConnectionStatus() with reactive effect
- Automatic DOM updates when connectionStatus signal changes
- Removes 20 lines of manual DOM manipulation code
- Zero functional regressions"
```

### Validation

- [ ] Connection status updates automatically
- [ ] No manual `updateConnectionStatus()` calls needed
- [ ] Code is cleaner (declarative vs. imperative)
- [ ] No console errors
- [ ] Changes committed

### Estimated Effort

- Effect creation: 30 min
- Integration: 45 min
- Testing: 30 min
- Documentation: 15 min
- **Total:** ~2 hours

---

## Phase 3: Stats Cards

**Duration**: 3-4 hours
**Goal**: Migrate stats rendering and remove polling

### Tasks

#### 3.1 Create stats effects

```bash
touch browser/effects/stats-effects.js
```

```javascript
// browser/effects/stats-effects.js
import { effect } from '@preact/signals-core';
import { statsCards } from '../state.js';

export function setupStatsEffects() {
  effect(() => {
    const cards = statsCards.value;

    cards.forEach((card) => {
      const valueEl = document.getElementById(`stat-value-${card.id}`);
      if (valueEl) {
        valueEl.textContent = card.value;
        valueEl.className = `mt-1 text-3xl font-semibold ${card.color}`;
      }
    });
  });
}
```

#### 3.2 Update HTML template

**Before** (`browser/app.js` or `browser/index.html`):

```html
<dd class="mt-1 text-3xl font-semibold text-gray-900">0</dd>
```

**After** (add IDs to each stat card):

```html
<dd id="stat-value-total-tasks" class="mt-1 text-3xl font-semibold text-gray-900">0</dd>
<dd id="stat-value-active-agents" class="mt-1 text-3xl font-semibold text-blue-600">0</dd>
<dd id="stat-value-pending-reviews" class="mt-1 text-3xl font-semibold text-orange-600">0</dd>
<dd id="stat-value-completed" class="mt-1 text-3xl font-semibold text-green-600">0</dd>
```

#### 3.3 Update API client

**Before** (`browser/api-client.js`):

```javascript
async getStats() {
  const response = await fetch(`${API_BASE}/api/stats`);
  return response.json();
}
```

**After** (hybrid approach—returns data AND updates signal):

```javascript
import { setStats, setLoading, setError } from './state.js';

async getStats() {
  try {
    setLoading(true);
    const response = await fetch(`${API_BASE}/api/stats`);
    const data = await response.json();
    setStats(data); // Update signal—triggers effects
    return data;    // Also return for legacy code
  } catch (err) {
    setError(err.message);
    throw err;
  } finally {
    setLoading(false);
  }
}
```

#### 3.4 Update dashboard.js

**Before** (`browser/dashboard.js`):

```javascript
async loadData() {
  try {
    const [stats, reviews, agents] = await Promise.all([
      this.api.getStats(),
      this.api.getReviews(),
      this.api.getAgents(),
    ]);

    this.stats = stats;
    this.reviews = reviews;
    this.agents = agents;

    this.render(); // Manual render
  } catch (error) {
    console.error("Failed to load data:", error);
    this.renderError(error.message);
  }
}

render() {
  if (!this.element || !this.stats) return;

  this.element.innerHTML = `
    <div class="space-y-6">
      ${this.renderStats()}
      ${this.renderReviews()}
      ${this.renderAgents()}
    </div>
  `;
}
```

**After** (remove render(), rely on effects):

```javascript
import { setStats, setReviews, setAgents } from './state.js';

async loadData() {
  try {
    const [stats, reviews, agents] = await Promise.all([
      this.api.getStats(),
      this.api.getReviews(),
      this.api.getAgents(),
    ]);

    // Update signals—effects handle DOM updates
    setStats(stats);
    setReviews(reviews);
    setAgents(agents);
  } catch (error) {
    console.error("Failed to load data:", error);
    setError(error.message);
  }
}

// render() method removed—no longer needed!
```

#### 3.5 Remove polling interval

**Before** (`browser/dashboard.js`):

```javascript
async mount(element) {
  this.element = element;
  this.renderLoading();
  await this.loadData();

  // 5-second polling (remove this!)
  this.refreshInterval = setInterval(() => this.loadData(), 5000);

  this.element.addEventListener("click", (e) => this.handleClick(e));
}
```

**After** (rely on WebSocket only):

```javascript
async mount(element) {
  this.element = element;

  // Set up effects
  setupStatsEffects();

  // Load initial data
  await this.loadData();

  // No polling—WebSocket handles real-time updates

  this.element.addEventListener("click", (e) => this.handleClick(e));
}
```

#### 3.6 Update WebSocket handlers

**Before** (`browser/dashboard.js`):

```javascript
handleTaskUpdate(data) {
  console.log("Task updated:", data);
  this.loadData(); // Full reload
}
```

**After**:

```javascript
import { updateReview } from './state.js';

handleTaskUpdate(data) {
  console.log("Task updated:", data);
  updateReview(data.id, data); // Granular update—only affects that review
}
```

#### 3.7 Initialize effects in app.js

```javascript
// browser/app.js
import { setupConnectionEffects } from './effects/connection-effects.js';
import { setupStatsEffects } from './effects/stats-effects.js';

async function init() {
  // ... existing code ...

  // Set up effects
  setupConnectionEffects();
  setupStatsEffects();

  // Mount dashboard
  await dashboard.mount(dashboardElement);

  // ... existing code ...
}
```

#### 3.8 Test stats updates

1. Open workbench in browser
2. Verify stats display correctly
3. Manually update stats signal in console:
   ```javascript
   import { setStats } from './state.js';
   setStats({ totalTasks: 999, tasksByState: { completed: 500 } });
   ```
4. Verify stats cards update automatically
5. Check Network tab—no more 5-second polling requests!

#### 3.9 Commit changes

```bash
git add browser/app.js browser/dashboard.js browser/api-client.js browser/effects/stats-effects.js browser/state.js
git commit -m "feat: migrate stats cards to signals and remove polling

- Replace renderStats() with reactive effects
- Automatic DOM updates on stats signal changes
- Remove 5-second polling interval (12 requests/min → 0)
- WebSocket-only updates (real-time reactivity)
- 30% reduction in stats rendering code"
```

### Validation

- [ ] Stats cards display correctly on initial load
- [ ] Stats update automatically on API fetch
- [ ] Stats update automatically on WebSocket events
- [ ] No 5-second polling (check Network tab)
- [ ] Performance improvement measurable (DevTools Performance tab)
- [ ] Changes committed

### Estimated Effort

- Effects creation: 1 hour
- API client updates: 30 min
- Dashboard refactoring: 1 hour
- Testing: 45 min
- Documentation: 15 min
- **Total:** ~3.5 hours

---

## Phase 4: Reviews List

**Duration**: 4-6 hours
**Goal**: Migrate reviews rendering with granular updates

### Tasks

#### 4.1 Create reviews effects

```bash
touch browser/effects/reviews-effects.js
```

Copy advanced keyed diffing version from `browser/signals-prototype/reviews-list-signals.js`.

Or start with simple version:

```javascript
// browser/effects/reviews-effects.js
import { effect } from '@preact/signals-core';
import { reviews } from '../state.js';

export function setupReviewsEffects() {
  const container = document.getElementById('reviews-list');
  if (!container) return;

  effect(() => {
    const reviewsList = reviews.value;

    if (reviewsList.length === 0) {
      container.innerHTML = '<p class="text-gray-500 text-sm">No pending reviews</p>';
      return;
    }

    // Simple approach: replace innerHTML
    container.innerHTML = reviewsList
      .slice(0, 5)
      .map((review) => createReviewHTML(review))
      .join('');
  });
}

function createReviewHTML(review) {
  // ... (copy from signals-prototype/reviews-list-signals.js)
}
```

#### 4.2 Update dashboard.js

**Before**:

```javascript
renderReviews() {
  if (!this.reviews || this.reviews.length === 0) {
    return "";
  }

  const reviewItems = this.reviews.slice(0, 5).map((review) => `
    <!-- Review HTML -->
  `).join("");

  return `
    <div class="bg-white shadow rounded-lg">
      <!-- Container -->
      ${reviewItems}
    </div>
  `;
}
```

**After** (remove renderReviews(), use effect):

```javascript
// Remove renderReviews() method entirely
// Effect in reviews-effects.js handles rendering
```

#### 4.3 Update approve/reject handlers

**Before**:

```javascript
async handleApprove(reviewId) {
  try {
    await this.api.approveReview(reviewId);
    await this.loadData(); // Full reload
  } catch (error) {
    alert(`Failed to approve: ${error.message}`);
  }
}
```

**After**:

```javascript
import { removeReview } from './state.js';

async handleApprove(reviewId) {
  try {
    await this.api.approveReview(reviewId);
    removeReview(reviewId); // Granular update—just remove from list
  } catch (error) {
    alert(`Failed to approve: ${error.message}`);
  }
}
```

#### 4.4 Initialize reviews effects

```javascript
// browser/app.js
import { setupReviewsEffects } from './effects/reviews-effects.js';

async function init() {
  // ... existing effects ...
  setupReviewsEffects();
  // ...
}
```

#### 4.5 Test reviews workflows

1. Load workbench
2. Verify reviews display correctly
3. Click "Approve" button—verify review removed
4. Click "Reject" button—verify review removed with comment
5. Simulate WebSocket review update:
   ```javascript
   import { updateReview } from './state.js';
   updateReview('task_1', { goal: 'UPDATED GOAL' });
   ```
6. Verify review updates without full page reload

#### 4.6 Commit changes

```bash
git add browser/dashboard.js browser/effects/reviews-effects.js
git commit -m "feat: migrate reviews list to signals

- Replace renderReviews() with reactive effect
- Granular updates—only changed reviews re-render
- Approve/reject actions update specific review (no full reload)
- WebSocket updates trigger granular DOM changes"
```

### Validation

- [ ] Reviews list renders correctly
- [ ] Approve/reject buttons work
- [ ] Reviews update granularly (no full list re-render)
- [ ] WebSocket updates reflected in UI
- [ ] No console errors
- [ ] Changes committed

### Estimated Effort

- Effects creation: 2 hours
- Approve/reject refactoring: 1 hour
- Testing: 1 hour
- Documentation: 15 min
- **Total:** ~4.5 hours

---

## Phase 5: Agents List

**Duration**: 2-3 hours
**Goal**: Migrate agents rendering

### Tasks

#### 5.1 Create agents effects

```bash
touch browser/effects/agents-effects.js
```

```javascript
// browser/effects/agents-effects.js
import { effect } from '@preact/signals-core';
import { agents } from '../state.js';

export function setupAgentsEffects() {
  const container = document.getElementById('agents-list');
  if (!container) return;

  effect(() => {
    const agentsList = agents.value;

    if (agentsList.length === 0) {
      container.innerHTML = '<p class="text-gray-500 text-sm">No active agents</p>';
      return;
    }

    container.innerHTML = agentsList
      .map((agent) => createAgentHTML(agent))
      .join('');
  });
}

function createAgentHTML(agent) {
  return `
    <div class="border border-gray-200 rounded-lg p-4">
      <div class="flex items-center gap-2">
        <span class="status-emoji animate-spin">🔄</span>
        <span class="text-sm font-mono text-gray-500">${escapeHtml(agent.id)}</span>
      </div>
      <p class="mt-1 text-sm text-gray-900">${escapeHtml(agent.goal)}</p>
      ${agent.startedAt ? `
        <p class="mt-1 text-xs text-gray-500">
          Started: ${new Date(agent.startedAt).toLocaleString()}
        </p>
      ` : ''}
    </div>
  `;
}

function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}
```

#### 5.2 Update dashboard.js

Remove `renderAgents()` method.

#### 5.3 Add WebSocket handlers for agents

```javascript
// browser/websocket-handlers.js (new file)
import { updateAgent, removeAgent } from './state.js';

export function setupWebSocketHandlers(ws) {
  ws.on('agent_updated', (data) => {
    updateAgent(data.id, data);
  });

  ws.on('agent_completed', (data) => {
    removeAgent(data.id);
  });

  // ... other handlers ...
}
```

#### 5.4 Initialize agents effects

```javascript
// browser/app.js
import { setupAgentsEffects } from './effects/agents-effects.js';
import { setupWebSocketHandlers } from './websocket-handlers.js';

async function init() {
  // ... existing effects ...
  setupAgentsEffects();

  // Set up WebSocket handlers
  setupWebSocketHandlers(ws);

  // ...
}
```

#### 5.5 Test agents

1. Simulate active agent:
   ```javascript
   import { setAgents } from './state.js';
   setAgents([
     { id: 'agent_1', goal: 'Test agent', state: 'active', startedAt: new Date().toISOString() }
   ]);
   ```
2. Verify agent displays correctly
3. Simulate agent completion:
   ```javascript
   import { removeAgent } from './state.js';
   removeAgent('agent_1');
   ```
4. Verify agent removed from list

#### 5.6 Commit changes

```bash
git add browser/dashboard.js browser/effects/agents-effects.js browser/websocket-handlers.js
git commit -m "feat: migrate agents list to signals

- Replace renderAgents() with reactive effect
- WebSocket-driven updates (agent_updated, agent_completed)
- Automatic UI updates on agent lifecycle events"
```

### Validation

- [ ] Agents list renders correctly
- [ ] Agents update on WebSocket events
- [ ] Agents removed when completed
- [ ] No console errors
- [ ] Changes committed

### Estimated Effort

- Effects creation: 1 hour
- WebSocket handlers: 30 min
- Testing: 30 min
- Documentation: 15 min
- **Total:** ~2.5 hours

---

## Phase 6: Cleanup and Optimization

**Duration**: 2-3 hours
**Goal**: Remove old code, optimize, and finalize migration

### Tasks

#### 6.1 Remove old render() method

**Before** (`browser/dashboard.js`):

```javascript
render() {
  if (!this.element || !this.stats) return;

  this.element.innerHTML = `
    <div class="space-y-6">
      ${this.renderStats()}
      ${this.renderReviews()}
      ${this.renderAgents()}
    </div>
  `;
}

renderStats() { /* ... */ }
renderReviews() { /* ... */ }
renderAgents() { /* ... */ }
```

**After**:

```javascript
// All render methods removed—effects handle rendering
```

#### 6.2 Remove unused class properties

**Before**:

```javascript
constructor(apiClient, wsClient) {
  this.api = apiClient;
  this.ws = wsClient;
  this.stats = null;       // Remove—now in state.js
  this.reviews = [];       // Remove
  this.agents = [];        // Remove
  this.element = null;
  this.refreshInterval = null; // Remove—no more polling
}
```

**After**:

```javascript
constructor(apiClient, wsClient) {
  this.api = apiClient;
  this.ws = wsClient;
  this.element = null;
  // Other state moved to state.js
}
```

#### 6.3 Optimize effects (if needed)

Check for performance issues:

1. Open DevTools → Performance tab
2. Record 10 seconds of interaction
3. Look for excessive effect re-runs

If needed, optimize using `batch()`:

```javascript
import { batch } from '@preact/signals-core';

// Update multiple signals in one commit
batch(() => {
  setStats(statsData);
  setReviews(reviewsData);
  setAgents(agentsData);
});
// Effects run once after batch completes
```

#### 6.4 Add error boundaries (optional)

Wrap effects in try/catch to prevent one failing effect from breaking others:

```javascript
effect(() => {
  try {
    // Effect logic
  } catch (error) {
    console.error('Effect error:', error);
    setError(error.message);
  }
});
```

#### 6.5 Measure performance improvements

Repeat baseline tests from Phase 0:

1. Record performance profile (DevTools)
2. Note FPS, memory usage, re-render time
3. Compare to baseline metrics

Document in `MIGRATION_RESULTS.md`:

```markdown
# Migration Results

## Before (Baseline)
- Bundle size: X KB
- Average FPS: X fps
- Memory usage: X MB
- API requests (60s): 12 (from polling)
- Re-render time: X ms

## After (Signals)
- Bundle size: X KB (+1.6 KB)
- Average FPS: X fps (+Y% improvement)
- Memory usage: X MB (-Y% improvement)
- API requests (60s): 0 (no polling!)
- Re-render time: X ms (-Y% improvement)

## Summary
- ✅ Bundle size increase: 1.6 KB (acceptable)
- ✅ FPS improvement: +Y%
- ✅ Memory improvement: -Y%
- ✅ Eliminated polling: -12 requests/min
- ✅ Code reduction: -X lines
```

#### 6.6 Update documentation

Update `README.md` or create `ARCHITECTURE.md`:

```markdown
## Reactive State Management

The workbench uses [@preact/signals-core](https://www.npmjs.com/package/@preact/signals-core) for reactive state management.

**Key files:**
- `browser/state.js` - Centralized signals
- `browser/effects/` - DOM update effects
- `browser/websocket-handlers.js` - WebSocket → signal mapping

**Benefits:**
- Automatic reactivity—no manual render() calls
- Fine-grained DOM updates—only changed values re-render
- WebSocket-driven updates—no polling overhead
- Small bundle size—only +1.6 KB
```

#### 6.7 Final commit

```bash
git add browser/dashboard.js MIGRATION_RESULTS.md README.md
git commit -m "feat: complete signals migration and cleanup

- Remove all old render() methods
- Remove polling infrastructure
- Optimize effects with batching
- Document performance improvements
- Update architecture documentation

Results:
- Bundle size: +1.6 KB
- FPS: +X% improvement
- Memory: -X% improvement
- Eliminated 12 API requests/min from polling
- Code reduction: -X lines

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

### Validation

- [ ] All old render() methods removed
- [ ] No unused code remaining
- [ ] Performance improvements documented
- [ ] Architecture documentation updated
- [ ] Changes committed

### Estimated Effort

- Code cleanup: 1 hour
- Performance testing: 45 min
- Documentation: 30 min
- Final review: 15 min
- **Total:** ~2.5 hours

---

## Testing Strategy

### Manual Testing

For each phase:

1. **Visual testing**—verify UI looks correct
2. **Interaction testing**—click buttons, verify workflows
3. **WebSocket testing**—simulate events, verify reactivity
4. **Performance testing**—DevTools profiler, check FPS

### Automated Testing (if applicable)

If test suite exists:

```bash
bun test  # Should pass after each phase
```

### Regression Testing

Before merging:

1. Test all user workflows:
   - [ ] Load workbench
   - [ ] View stats
   - [ ] Approve/reject reviews
   - [ ] View active agents
   - [ ] WebSocket connect/disconnect
2. Test edge cases:
   - [ ] Empty states (no reviews, no agents)
   - [ ] Error states (API failures)
   - [ ] High-frequency updates (rapid WebSocket events)
3. Cross-browser testing (if needed):
   - [ ] Chrome
   - [ ] Firefox
   - [ ] Safari

---

## Rollback Procedures

### Phase-Level Rollback

If a phase fails:

```bash
git log --oneline  # Find commit hash for failed phase
git revert <commit-hash>
git push
```

### Full Rollback

If migration is unsuccessful:

```bash
git checkout main
git branch -D feature/signals-migration
bun remove @preact/signals-core
```

Restore old code:

```bash
git checkout main -- browser/
```

### Rollback Triggers

Rollback if:

- ❌ Bundle size increase > 5 KB
- ❌ Performance regression > 20%
- ❌ Critical bugs unresolved after 2 days
- ❌ Memory leaks detected
- ❌ Team consensus to abort

---

## Success Metrics

### Must-Have (Phase Completion Criteria)

Each phase must meet:

- ✅ **Functionality**: All features work as before
- ✅ **Performance**: No regressions (FPS, memory)
- ✅ **Bundle size**: Increase ≤ 2 KB total
- ✅ **Code quality**: No linter errors
- ✅ **Tests**: All tests pass (if applicable)

### Nice-to-Have (Overall Success)

Migration is successful if:

1. ✅ **All manual render() calls removed**
2. ✅ **Polling eliminated** (0 API requests from polling)
3. ✅ **Bundle size ≤ 2 KB increase**
4. ✅ **Performance improved by ≥ 30%** (FPS or re-render time)
5. ✅ **No memory leaks** (24-hour soak test)
6. ✅ **Code is more maintainable** (fewer lines, clearer logic)

### Performance Targets

- **FPS**: ≥ 60 fps during updates
- **Memory**: No increase (ideally decrease)
- **Bundle size**: ≤ 2 KB increase
- **API requests**: 0 from polling
- **Re-render time**: ≥ 30% reduction

---

## Timeline Summary

| Phase | Duration | Dependencies |
|-------|----------|--------------|
| Phase 0: Pre-Flight | 30 min | None |
| Phase 1: Infrastructure | 1-2 hours | Phase 0 |
| Phase 2: Connection Status | 2-3 hours | Phase 1 |
| Phase 3: Stats Cards | 3-4 hours | Phase 2 |
| Phase 4: Reviews List | 4-6 hours | Phase 3 |
| Phase 5: Agents List | 2-3 hours | Phase 4 |
| Phase 6: Cleanup | 2-3 hours | Phase 5 |
| **Total** | **16-22 hours** | - |

**Recommended schedule:** 3-5 days, 4-6 hours per day

**Decision gates:**
- ✅ After Phase 2: Decide to continue or rollback
- ✅ After Phase 4: Validate approach is working
- ✅ After Phase 6: Final review before merge

---

## Next Steps

1. ✅ **Review this plan** with team/stakeholders
2. ⬜ **Schedule migration work** (3-5 days)
3. ⬜ **Execute Phase 0** (pre-flight checks)
4. ⬜ **Execute Phases 1-2** (infrastructure + POC)
5. ⬜ **Decision gate**: Continue or rollback?
6. ⬜ **Execute Phases 3-6** (full migration)
7. ⬜ **Merge to main** and deploy

---

## Conclusion

This migration plan provides a **safe, incremental approach** to adopting @preact/signals-core in the Primer Workbench. Each phase is:

- ✅ **Self-contained**—can be validated independently
- ✅ **Reversible**—can rollback without affecting other phases
- ✅ **Low-risk**—signals coexist with current code
- ✅ **High-value**—immediate benefits (cleaner code, better performance)

**Recommendation**: ✅ **Proceed with execution** starting with Phase 0-2, then evaluate before continuing.

See `SIGNALS_COMPARISON.md` and `SIGNALS_INTEGRATION_PROPOSAL.md` for detailed rationale and design.
