# Signals Integration Proposal for Primer Workbench

**Date**: January 17, 2026
**Library Choice**: @preact/signals-core
**Target**: Browser-based Primer Workbench UI
**Task ID**: task_35

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture Design](#architecture-design)
3. [State Management Design](#state-management-design)
4. [WebSocket Integration](#websocket-integration)
5. [DOM Rendering Strategy](#dom-rendering-strategy)
6. [Migration Strategy](#migration-strategy)
7. [File Structure Changes](#file-structure-changes)
8. [API Changes](#api-changes)
9. [Performance Expectations](#performance-expectations)
10. [Risk Assessment](#risk-assessment)

---

## Overview

### Goals

Replace manual DOM manipulation and imperative state management with **@preact/signals-core** reactive primitives to achieve:

1. **Automatic reactivity**—eliminate manual `render()` calls
2. **Granular DOM updates**—update only changed nodes, not full `innerHTML` replacement
3. **Simplified WebSocket integration**—state updates trigger DOM changes automatically
4. **Better performance**—remove polling, reduce re-renders
5. **Cleaner code**—declarative effects replace imperative DOM manipulation

### Non-Goals

- ❌ **Full framework adoption**—not migrating to Preact/React/Vue
- ❌ **Rewrite from scratch**—incremental adoption only
- ❌ **Breaking API changes**—external API remains unchanged
- ❌ **Remove vanilla JS**—signals complement, don't replace vanilla patterns

---

## Architecture Design

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Primer Workbench                        │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────┐      ┌──────────────┐     ┌─────────────┐ │
│  │   Signals   │◄─────┤ API Client   │     │ WS Client   │ │
│  │   (State)   │      │ (HTTP Fetch) │     │ (WebSocket) │ │
│  └──────┬──────┘      └──────────────┘     └──────┬──────┘ │
│         │                                          │         │
│         │ Updates                        Events    │         │
│         ▼                                          ▼         │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              Computed Signals                        │   │
│  │  (totalTasks, activeAgents, pendingReviews, etc.)   │   │
│  └──────────────────┬──────────────────────────────────┘   │
│                     │                                        │
│                     │ Reactive Dependencies                  │
│                     ▼                                        │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                  Effects                             │   │
│  │  (DOM updates, side effects, logging, etc.)         │   │
│  └──────────────────┬──────────────────────────────────┘   │
│                     │                                        │
│                     │ Direct DOM Manipulation                │
│                     ▼                                        │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                   DOM                                │   │
│  │  (No Virtual DOM, direct element updates)           │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Data Flow

**Current Flow** (Imperative):
```
API → setState() → render() → innerHTML = "..." → Browser Parse
```

**New Flow** (Reactive):
```
API → signal.value = data → computed.value updates → effect() runs → Direct DOM update
```

**Key Difference**: State changes automatically propagate to computed values and effects, which update specific DOM nodes without re-parsing HTML strings.

---

## State Management Design

### Core State Signals

Create a centralized `state.js` module:

```javascript
// browser/state.js
import { signal, computed } from '@preact/signals-core';

// ============================================================================
// Base Signals (Writable State)
// ============================================================================

export const stats = signal(null);
export const reviews = signal([]);
export const agents = signal([]);
export const connectionStatus = signal('disconnected');
export const isLoading = signal(true);
export const error = signal(null);

// ============================================================================
// Computed Signals (Derived State)
// ============================================================================

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

export const hasError = computed(() => error.value !== null);

// ============================================================================
// Computed UI State
// ============================================================================

export const connectionStatusClass = computed(() => {
  const status = connectionStatus.value;
  return {
    dot: status === 'connected' ? 'bg-green-500' : 'bg-red-500',
    text: status === 'connected' ? 'text-green-600' : 'text-red-500',
    label: status === 'connected' ? 'Connected' : 'Disconnected',
  };
});

export const statsCards = computed(() => [
  { label: 'Total Tasks', value: totalTasks.value, color: 'text-gray-900' },
  { label: 'Active Agents', value: activeAgentsCount.value, color: 'text-blue-600' },
  { label: 'Pending Reviews', value: pendingReviewsCount.value, color: 'text-orange-600' },
  { label: 'Completed', value: completedTasksCount.value, color: 'text-green-600' },
]);

// ============================================================================
// State Actions (Business Logic)
// ============================================================================

export function setStats(newStats) {
  stats.value = newStats;
}

export function setReviews(newReviews) {
  reviews.value = newReviews;
}

export function setAgents(newAgents) {
  agents.value = newAgents;
}

export function updateReview(reviewId, updates) {
  reviews.value = reviews.value.map(review =>
    review.id === reviewId ? { ...review, ...updates } : review
  );
}

export function removeReview(reviewId) {
  reviews.value = reviews.value.filter(r => r.id !== reviewId);
}

export function updateAgent(agentId, updates) {
  agents.value = agents.value.map(agent =>
    agent.id === agentId ? { ...agent, ...updates } : agent
  );
}

export function setConnectionStatus(status) {
  connectionStatus.value = status;
}

export function setLoading(loading) {
  isLoading.value = loading;
}

export function setError(err) {
  error.value = err;
}

export function clearError() {
  error.value = null;
}
```

### Benefits of This Design

1. **Centralized state**—all signals in one module
2. **Type-safe actions**—functions encapsulate state mutations
3. **Computed caching**—derived values memoized automatically
4. **Single source of truth**—no duplicate state
5. **Testable**—state module can be unit tested independently

---

## WebSocket Integration

### Current Approach (Manual Reload)

```javascript
// dashboard.js lines 263-277
handleTaskUpdate(data) {
  console.log("Task updated:", data);
  this.loadData(); // Full reload!
}

handleTaskCreated(data) {
  console.log("Task created:", data);
  this.loadData(); // Full reload!
}
```

**Problems:**
- Full API refetch on every WebSocket event
- No granular updates—entire dashboard re-renders
- Race conditions between polling and WebSocket

---

### Signals Approach (Granular Updates)

```javascript
// browser/websocket-handlers.js
import { updateReview, removeReview, updateAgent, setConnectionStatus } from './state.js';

export function setupWebSocketHandlers(ws) {
  // Connection status
  ws.on('connection', (data) => {
    setConnectionStatus(data.status);
  });

  // Task updates (affects reviews)
  ws.on('task_updated', (data) => {
    updateReview(data.id, data); // Only update specific review
  });

  // Task created
  ws.on('task_created', (data) => {
    // Add to reviews if it's a review-type task
    if (data.state === 'review') {
      reviews.value = [...reviews.value, data]; // Immutable update
    }
  });

  // Review approved/rejected
  ws.on('review_completed', (data) => {
    removeReview(data.id); // Remove from pending list
  });

  // Agent state changes
  ws.on('agent_updated', (data) => {
    updateAgent(data.id, data);
  });

  // Agent completed
  ws.on('agent_completed', (data) => {
    agents.value = agents.value.filter(a => a.id !== data.id);
  });
}
```

**Benefits:**
- ✅ **No full reloads**—only affected signals update
- ✅ **Granular DOM updates**—only changed elements re-render
- ✅ **No race conditions**—signals batch updates automatically
- ✅ **Declarative**—handlers just update state, effects handle DOM

---

### Removing Polling

Current polling (unnecessary with WebSocket):

```javascript
// dashboard.js line 36
this.refreshInterval = setInterval(() => this.loadData(), 5000);
```

**With signals:** Remove polling entirely. WebSocket updates trigger reactive DOM changes.

**Fallback strategy** (if WebSocket disconnects):

```javascript
import { isConnected } from './state.js';
import { effect } from '@preact/signals-core';

let fallbackInterval = null;

effect(() => {
  if (!isConnected.value) {
    // WebSocket down—start polling
    if (!fallbackInterval) {
      console.warn('WebSocket disconnected, falling back to polling');
      fallbackInterval = setInterval(loadData, 10000); // 10s polling
    }
  } else {
    // WebSocket up—stop polling
    if (fallbackInterval) {
      console.log('WebSocket reconnected, stopping polling');
      clearInterval(fallbackInterval);
      fallbackInterval = null;
    }
  }
});
```

---

## DOM Rendering Strategy

### Current Approach (innerHTML Replacement)

```javascript
// dashboard.js lines 91-101
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

**Problems:**
- Full HTML string re-parsing on every update
- Entire dashboard replaced—expensive
- Loses focus, scroll position, animations
- Event listeners must be re-attached (event delegation helps, but still suboptimal)

---

### Signals Approach (Fine-Grained Effects)

**Strategy:** Use `effect()` to update specific DOM nodes when signals change.

#### Example: Stats Cards

```javascript
// browser/effects/stats-effects.js
import { effect } from '@preact/signals-core';
import { statsCards } from './state.js';

export function setupStatsEffects() {
  effect(() => {
    const cards = statsCards.value;

    // Update each stat card
    cards.forEach((card, index) => {
      const valueEl = document.getElementById(`stat-value-${index}`);
      if (valueEl) {
        valueEl.textContent = card.value;
        valueEl.className = `mt-1 text-3xl font-semibold ${card.color}`;
      }
    });
  });
}
```

**HTML Template** (static, rendered once):

```html
<div class="grid grid-cols-1 md:grid-cols-4 gap-4">
  <div class="bg-white overflow-hidden shadow rounded-lg">
    <div class="px-4 py-5 sm:p-6">
      <dt class="text-sm font-medium text-gray-500 truncate">Total Tasks</dt>
      <dd id="stat-value-0" class="mt-1 text-3xl font-semibold text-gray-900">0</dd>
    </div>
  </div>
  <!-- Repeat for other stats... -->
</div>
```

**Key Insight:** HTML structure is static. Effects only update `textContent` and `className` of specific nodes.

---

#### Example: Reviews List

**Challenge:** Dynamic lists require more complex DOM manipulation.

**Solution:** Use keyed DOM updates (similar to React's reconciliation, but manual).

```javascript
// browser/effects/reviews-effects.js
import { effect } from '@preact/signals-core';
import { reviews } from './state.js';
import { escapeHtml, getStatusEmoji, getPriorityClass } from './utils.js';

export function setupReviewsEffects() {
  const container = document.getElementById('reviews-list');
  if (!container) return;

  effect(() => {
    const reviewsList = reviews.value;

    // Build a map of existing DOM elements by review ID
    const existingElements = new Map();
    Array.from(container.children).forEach(el => {
      const id = el.dataset.reviewId;
      if (id) existingElements.set(id, el);
    });

    // Update or create elements for current reviews
    const fragment = document.createDocumentFragment();

    reviewsList.slice(0, 5).forEach(review => {
      let reviewEl = existingElements.get(review.id);

      if (!reviewEl) {
        // Create new element
        reviewEl = createReviewElement(review);
      } else {
        // Update existing element
        updateReviewElement(reviewEl, review);
        existingElements.delete(review.id); // Mark as still in use
      }

      fragment.appendChild(reviewEl);
    });

    // Remove elements for reviews no longer in list
    existingElements.forEach(el => el.remove());

    // Replace container contents (efficient DOM diffing alternative)
    container.innerHTML = '';
    container.appendChild(fragment);
  });
}

function createReviewElement(review) {
  const div = document.createElement('div');
  div.className = 'border border-gray-200 rounded-lg p-4 hover:bg-gray-50 task-card';
  div.dataset.reviewId = review.id;

  updateReviewElement(div, review); // Populate content

  return div;
}

function updateReviewElement(element, review) {
  element.innerHTML = `
    <div class="flex items-start justify-between">
      <div class="flex-1">
        <div class="flex items-center gap-2">
          <span class="status-emoji">${getStatusEmoji(review.state)}</span>
          <span class="text-sm font-mono text-gray-500">${escapeHtml(review.id)}</span>
          ${review.priority !== undefined ? `
            <span class="priority-badge ${getPriorityClass(review.priority)}">
              P${review.priority}
            </span>
          ` : ''}
        </div>
        <p class="mt-1 text-sm text-gray-900">${escapeHtml(review.goal)}</p>
      </div>
      <div class="flex gap-2 ml-4">
        <button
          data-action="approve"
          data-review-id="${review.id}"
          class="px-3 py-1 bg-green-600 text-white text-sm rounded hover:bg-green-700"
        >
          Approve
        </button>
        <button
          data-action="reject"
          data-review-id="${review.id}"
          class="px-3 py-1 bg-red-600 text-white text-sm rounded hover:bg-red-700"
        >
          Reject
        </button>
      </div>
    </div>
  `;
}
```

**Trade-off Analysis:**

| Approach | Pros | Cons |
|----------|------|------|
| **Full innerHTML replacement** | Simple code | Expensive parsing, loses state |
| **Manual keyed diffing** (above) | Efficient updates, preserves state | More complex code |
| **Template cloning** | Fast DOM creation | Still needs diffing logic |
| **Virtual DOM library** | Declarative, battle-tested | Adds dependency (defeats purpose) |

**Recommendation:** Use **manual keyed diffing** for dynamic lists. Complexity is justified by performance gains and state preservation.

---

#### Alternative: Simpler Approach for Workbench

For the workbench (small data sets, <100 items), a simpler approach may suffice:

```javascript
effect(() => {
  const reviewsList = reviews.value;

  // Simple: just replace innerHTML (good enough for <10 reviews)
  container.innerHTML = reviewsList.slice(0, 5).map(review => `
    <div class="border border-gray-200 rounded-lg p-4 hover:bg-gray-50" data-review-id="${review.id}">
      <!-- Review content -->
    </div>
  `).join('');
});
```

**Trade-off:** Simpler code, but loses focus/scroll state on updates. Acceptable if reviews list updates are infrequent.

**Decision point:** Start with simple approach, optimize if performance issues arise.

---

## Migration Strategy

### Incremental vs. Big Bang

**Chosen Strategy:** ✅ **Incremental migration**

**Rationale:**
- Lower risk—one component at a time
- Easier to validate—test each migration step
- Reversible—can rollback individual components
- Signals coexist with current code—no conflicts

**Anti-pattern:** ❌ Big bang rewrite (high risk, hard to test, all-or-nothing)

---

### Migration Phases

#### Phase 1: Infrastructure Setup (1-2 hours)

**Goals:**
- Install @preact/signals-core
- Create state module
- Set up build/bundle verification

**Tasks:**
1. `bun add @preact/signals-core`
2. Create `browser/state.js` (centralized signals)
3. Create `browser/effects/` directory
4. Verify bundle size increase (<2 KB)
5. Test basic signal/effect in console

**Validation:**
- Signals import successfully in browser
- Bundle size increase ≤ 2 KB
- No runtime errors

---

#### Phase 2: Connection Status Component (2-3 hours)

**Goals:**
- Replace first component with signals
- Prove concept works
- Establish patterns

**Tasks:**
1. Create `connectionStatus` signal in `state.js`
2. Create `browser/effects/connection-effects.js`
3. Replace `updateConnectionStatus()` in `app.js` with signal update
4. Test WebSocket connection events

**Code:**

```javascript
// browser/effects/connection-effects.js
import { effect } from '@preact/signals-core';
import { connectionStatusClass } from '../state.js';

export function setupConnectionEffects() {
  const statusEl = document.getElementById('connection-status');
  if (!statusEl) return;

  effect(() => {
    const { dot, text, label } = connectionStatusClass.value;

    const dotEl = statusEl.querySelector('.inline-block');
    const textEl = statusEl.querySelector('span:last-child');

    if (dotEl && textEl) {
      dotEl.className = `inline-block w-2 h-2 rounded-full ${dot}`;
      textEl.className = text;
      textEl.textContent = label;
    }
  });
}
```

**Validation:**
- Connection status updates on WebSocket events
- No manual `updateConnectionStatus()` calls needed
- DOM updates correctly (green/red indicator)

---

#### Phase 3: Stats Cards (3-4 hours)

**Goals:**
- Migrate stats rendering
- Remove polling (rely on WebSocket)

**Tasks:**
1. Create stats signals (`stats`, `totalTasks`, etc.)
2. Create `browser/effects/stats-effects.js`
3. Replace `renderStats()` with effects
4. Update API client to set signals
5. Remove polling interval

**Validation:**
- Stats update on API fetch
- Stats update on WebSocket events
- No 5-second polling (verify in Network tab)

---

#### Phase 4: Reviews List (4-6 hours)

**Goals:**
- Migrate reviews rendering
- Handle approve/reject actions
- Granular WebSocket updates

**Tasks:**
1. Create `reviews` signal
2. Create `browser/effects/reviews-effects.js`
3. Implement keyed diffing or simple innerHTML approach
4. Test approve/reject workflows
5. Add WebSocket handlers for `review_completed`

**Validation:**
- Reviews list updates on WebSocket events
- Approve/reject actions work correctly
- No full page reloads

---

#### Phase 5: Agents List (2-3 hours)

**Goals:**
- Migrate agents rendering
- Handle agent lifecycle events

**Tasks:**
1. Create `agents` signal
2. Create `browser/effects/agents-effects.js`
3. Add WebSocket handlers for `agent_updated`, `agent_completed`

**Validation:**
- Active agents display correctly
- Agents removed when completed
- Real-time updates via WebSocket

---

#### Phase 6: Cleanup and Optimization (2-3 hours)

**Goals:**
- Remove old code
- Optimize effects
- Add error handling

**Tasks:**
1. Remove `Dashboard.render()` method
2. Remove `loadData()` polling
3. Add error boundaries for effects
4. Performance profiling (DevTools)
5. Bundle size verification

**Validation:**
- No unused code remaining
- Bundle size ≤ 2 KB increase
- Performance improvements measurable

---

## File Structure Changes

### Before

```
browser/
├── index.html
├── app.js           # Main app initialization
├── dashboard.js     # Dashboard component (imperative)
├── api-client.js    # HTTP API client
└── ws-client.js     # WebSocket client
```

### After

```
browser/
├── index.html
├── app.js                  # Main app initialization
├── state.js                # ✨ NEW: Centralized signals
├── dashboard.js            # ⚠️ REFACTORED: Uses signals (or removed)
├── api-client.js           # ⚠️ UPDATED: Sets signals instead of returning data
├── ws-client.js            # WebSocket client (unchanged)
├── websocket-handlers.js   # ✨ NEW: WebSocket → signal mapping
├── effects/                # ✨ NEW: DOM update effects
│   ├── connection-effects.js
│   ├── stats-effects.js
│   ├── reviews-effects.js
│   └── agents-effects.js
└── utils.js                # ✨ NEW: Shared utilities (escapeHtml, etc.)
```

---

## API Changes

### ApiClient Changes

**Before** (returns data):

```javascript
async getStats() {
  const response = await fetch(`${API_BASE}/api/stats`);
  return response.json();
}
```

**After** (updates signals):

```javascript
import { setStats, setLoading, setError } from './state.js';

async fetchStats() {
  try {
    setLoading(true);
    const response = await fetch(`${API_BASE}/api/stats`);
    const data = await response.json();
    setStats(data);
  } catch (err) {
    setError(err.message);
  } finally {
    setLoading(false);
  }
}
```

**Alternative** (hybrid approach—returns data AND updates signals):

Keep current API, but also update signals:

```javascript
async getStats() {
  const response = await fetch(`${API_BASE}/api/stats`);
  const data = await response.json();
  setStats(data); // Update signal
  return data;    // Also return for legacy code
}
```

**Recommendation:** Hybrid approach during migration, pure signals approach post-migration.

---

## Performance Expectations

### Metrics to Track

1. **Bundle size increase**: ≤ 2 KB
2. **FPS during updates**: 60 fps target (measure with DevTools Performance tab)
3. **Memory usage**: Should decrease (fewer re-renders, less garbage)
4. **Network requests**: Eliminate 5-second polling (~12 requests/min → 0)
5. **Time to interactive**: No regression (signals add minimal overhead)

### Benchmarking Plan

**Before migration:**
1. Record baseline metrics (bundle size, FPS, memory)
2. Simulate high-frequency WebSocket updates (e.g., 10 events/sec)
3. Measure re-render time in DevTools

**After migration:**
1. Repeat same tests
2. Compare metrics
3. Validate ≥ 30% performance improvement target

**Tools:**
- Chrome DevTools Performance tab
- Lighthouse performance audit
- `performance.measure()` API for custom timings

---

## Risk Assessment

### High Risks

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| **Memory leaks from effects** | Medium | High | Use effect cleanup, test with long-running sessions |
| **Bundle size creep** | Low | Medium | Monitor with Bundlephobia, enforce <2 KB limit |
| **Breaking existing workflows** | Low | High | Incremental migration, thorough testing per phase |

### Medium Risks

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| **Performance regression** | Low | Medium | Benchmark before/after, profile with DevTools |
| **Complexity increase** | Medium | Low | Document patterns, create reusable effect utilities |
| **Team learning curve** | Medium | Low | Provide examples, code reviews, pair programming |

### Low Risks

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| **Library abandonment** | Low | Medium | @preact/signals-core is actively maintained |
| **Browser compatibility** | Very Low | Low | ES6 Proxy required (supported in all modern browsers) |

---

## Rollback Plan

### If Migration Fails

**Phase-level rollback:**
1. Revert commits for that phase
2. Remove signals imports
3. Restore old component code
4. Verify functionality

**Example:**
```bash
git revert <commit-hash>  # Revert Phase 3 changes
bun test                  # Verify tests pass
```

**Full rollback:**
1. Remove @preact/signals-core dependency
2. Delete `state.js` and `effects/` directory
3. Restore `dashboard.js` to original version
4. Restore polling mechanism

**Rollback triggers:**
- Bundle size increase > 5 KB
- Performance regression > 20%
- Unresolved bugs after 2 days
- Memory leaks in production

---

## Success Criteria

### Phase Completion Criteria

Each phase must meet these criteria before proceeding:

✅ **Functionality**: All features work as before
✅ **Performance**: No regressions (FPS, memory, network)
✅ **Bundle size**: Increase ≤ 2 KB total
✅ **Code quality**: No linter errors, passes code review
✅ **Tests**: All existing tests pass (if any)

### Overall Success Criteria

Migration is successful if:

1. ✅ **All manual `render()` calls removed**
2. ✅ **Polling eliminated** (WebSocket-only updates)
3. ✅ **Bundle size ≤ 2 KB increase**
4. ✅ **Performance improved by ≥ 30%** (measured by FPS/re-render time)
5. ✅ **No memory leaks** (24-hour soak test)
6. ✅ **Code is more maintainable** (subjective, but aim for fewer lines)

---

## Next Steps

1. ✅ **Review this proposal** with team/stakeholders
2. ⬜ **Create proof-of-concept** (Phase 2: connection status)
3. ⬜ **Evaluate POC results** (performance, DX, complexity)
4. ⬜ **Decide:** Proceed with full migration or rollback
5. ⬜ **If proceed:** Execute phases 3-6 per migration plan
6. ⬜ **Document learnings** in retrospective

---

## Conclusion

Integrating @preact/signals-core into the Primer Workbench will:

- ✅ **Simplify code**—declarative effects replace imperative DOM updates
- ✅ **Improve performance**—fine-grained reactivity, no polling, faster updates
- ✅ **Enhance maintainability**—centralized state, clear data flow
- ✅ **Enable incremental adoption**—low risk, reversible phases

**Recommendation**: ✅ **Proceed with Phase 1 and Phase 2** to validate the approach. Decision gate at end of Phase 2 to continue or rollback.

See `SIGNALS_MIGRATION_PLAN.md` for detailed step-by-step implementation guide.

---

## Appendix: Alternative Approaches Considered

### Alternative 1: Keep Current Approach

**Pros:**
- No migration effort
- No new dependencies
- Familiar patterns

**Cons:**
- Manual DOM updates remain tedious
- Polling overhead continues
- No granular reactivity

**Verdict:** ❌ Rejected—current pain points justify migration

---

### Alternative 2: Use Full Framework (Preact/React)

**Pros:**
- Battle-tested ecosystem
- Component model
- Rich tooling

**Cons:**
- Large bundle size increase (40+ KB)
- Requires build step (JSX)
- Overkill for simple workbench
- Virtual DOM overhead

**Verdict:** ❌ Rejected—too heavyweight for workbench needs

---

### Alternative 3: Build Custom Signals Implementation

**Pros:**
- Full control
- Zero dependencies
- Tailored to workbench needs

**Cons:**
- Engineering effort (2-3 days)
- Bug surface area
- Reinventing the wheel
- No community support

**Verdict:** ❌ Rejected—@preact/signals-core is battle-tested and tiny (1.6 KB)

---

**Final Recommendation**: ✅ **Proceed with @preact/signals-core integration per this proposal**
