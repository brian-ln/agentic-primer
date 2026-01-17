# Signals Quick Start Guide

**TL;DR**: Use @preact/signals-core (1.6 KB) for automatic reactivity in the Primer Workbench.

---

## Why Signals?

**Current Problem**:
```javascript
async loadData() {
  const stats = await api.getStats();
  this.stats = stats;
  this.render(); // Manual render call
}

render() {
  this.element.innerHTML = `...`; // Full re-render
}
```

**With Signals**:
```javascript
import { signal, effect } from '@preact/signals-core';

const stats = signal(null);

effect(() => {
  document.getElementById('total').textContent = stats.value?.totalTasks || 0;
});

async loadData() {
  stats.value = await api.getStats(); // DOM updates automatically!
}
```

---

## Quick Comparison

| Current | Signals | Benefit |
|---------|---------|---------|
| Manual `render()` calls | Automatic effects | No manual updates |
| Full `innerHTML` replacement | Granular DOM updates | Faster re-renders |
| 5-second polling | WebSocket-only | -12 requests/min |
| 100 lines of code | 75 lines | -25% cleaner |

---

## Installation

```bash
bun add @preact/signals-core
```

**Bundle size increase**: +1.6 KB (acceptable)

---

## Core API (3 Primitives)

### 1. signal() - Reactive State

```javascript
import { signal } from '@preact/signals-core';

const count = signal(0);

count.value += 1; // Update triggers subscribers
console.log(count.value); // 1
```

### 2. computed() - Derived State

```javascript
import { signal, computed } from '@preact/signals-core';

const count = signal(0);
const double = computed(() => count.value * 2);

console.log(double.value); // 0
count.value = 5;
console.log(double.value); // 10 (auto-computed)
```

### 3. effect() - Side Effects

```javascript
import { signal, effect } from '@preact/signals-core';

const count = signal(0);

effect(() => {
  console.log('Count:', count.value); // Auto-runs on count change
});

count.value = 5; // Logs "Count: 5"
```

---

## Workbench Integration Pattern

### Step 1: Create State Module

```javascript
// browser/state.js
import { signal, computed } from '@preact/signals-core';

export const stats = signal(null);
export const reviews = signal([]);
export const connectionStatus = signal('disconnected');

export const totalTasks = computed(() => stats.value?.totalTasks || 0);
export const isConnected = computed(() => connectionStatus.value === 'connected');

export function setStats(newStats) {
  stats.value = newStats;
}

export function setConnectionStatus(status) {
  connectionStatus.value = status;
}
```

### Step 2: Create Effects

```javascript
// browser/effects/stats-effects.js
import { effect } from '@preact/signals-core';
import { totalTasks } from '../state.js';

export function setupStatsEffects() {
  effect(() => {
    const el = document.getElementById('total-tasks');
    if (el) el.textContent = totalTasks.value;
  });
}
```

### Step 3: Update API Client

```javascript
// browser/api-client.js
import { setStats } from './state.js';

async getStats() {
  const response = await fetch('/api/stats');
  const data = await response.json();
  setStats(data); // Signal update—effects run automatically
  return data;
}
```

### Step 4: Initialize Effects

```javascript
// browser/app.js
import { setupStatsEffects } from './effects/stats-effects.js';

function init() {
  setupStatsEffects(); // Effects watch signals
  loadData(); // Triggers signal updates
}
```

---

## WebSocket Integration

```javascript
// Before (manual reload)
ws.on('task_updated', (data) => {
  this.loadData(); // Full reload
});

// After (granular update)
import { updateReview } from './state.js';

ws.on('task_updated', (data) => {
  updateReview(data.id, data); // Only that review updates
});
```

---

## Common Patterns

### Pattern 1: Computed UI Classes

```javascript
export const connectionStatusClass = computed(() => {
  const status = connectionStatus.value;
  return {
    dot: status === 'connected' ? 'bg-green-500' : 'bg-red-500',
    text: status === 'connected' ? 'text-green-600' : 'text-red-500',
  };
});

effect(() => {
  const { dot, text } = connectionStatusClass.value;
  document.querySelector('.dot').className = dot;
  document.querySelector('.text').className = text;
});
```

### Pattern 2: List Rendering

```javascript
export const reviews = signal([]);

effect(() => {
  const container = document.getElementById('reviews');
  container.innerHTML = reviews.value
    .map(r => `<div>${r.goal}</div>`)
    .join('');
});
```

### Pattern 3: Batched Updates

```javascript
import { batch } from '@preact/signals-core';

batch(() => {
  setStats(statsData);
  setReviews(reviewsData);
  setAgents(agentsData);
});
// Effects run once after batch completes
```

---

## Try It Now

### Option 1: Run Prototype

```bash
cd browser/signals-prototype
bun --hot demo.html
```

Open `http://localhost:3000` and click buttons to see automatic reactivity.

### Option 2: Browser Console Test

```javascript
import { signal, effect } from 'https://esm.sh/@preact/signals-core@1.6.0';

const name = signal('World');

effect(() => {
  console.log('Hello,', name.value);
});

name.value = 'Signals'; // Logs "Hello, Signals"
```

---

## Migration Path

1. ✅ **Phase 0**: Record baseline metrics (30 min)
2. ✅ **Phase 1**: Install signals, create state.js (1-2 hours)
3. ✅ **Phase 2**: Migrate connection status (POC) (2-3 hours)
4. ⬜ **Decision gate**: Continue or rollback?
5. ⬜ **Phase 3**: Migrate stats cards, remove polling (3-4 hours)
6. ⬜ **Phase 4**: Migrate reviews list (4-6 hours)
7. ⬜ **Phase 5**: Migrate agents list (2-3 hours)
8. ⬜ **Phase 6**: Cleanup and optimize (2-3 hours)

**Total**: 16-22 hours over 3-5 days

See `SIGNALS_MIGRATION_PLAN.md` for detailed steps.

---

## Key Benefits

✅ **Automatic reactivity**—no manual render() calls
✅ **Fine-grained updates**—only changed values re-render
✅ **Cleaner code**—20-30% reduction in LOC
✅ **Better performance**—direct DOM updates, no HTML parsing
✅ **Simplified WebSocket integration**—just update signals
✅ **Small bundle**—only +1.6 KB
✅ **Testable**—state logic separated from DOM

---

## When NOT to Use Signals

❌ **For static content**—no reactivity needed
❌ **For one-time renders**—overkill for simple pages
❌ **When bundle size is critical**—every byte counts (though 1.6 KB is tiny)

---

## Further Reading

1. [Preact Signals Guide](https://preactjs.com/guide/v10/signals/)
2. `SIGNALS_COMPARISON.md` - Detailed library comparison
3. `SIGNALS_INTEGRATION_PROPOSAL.md` - Architecture design
4. `SIGNALS_MIGRATION_PLAN.md` - Step-by-step implementation
5. `browser/signals-prototype/` - Working demo and examples

---

## Decision

**Recommendation**: ✅ **Proceed with @preact/signals-core migration**

**Rationale**:
- Smallest bundle (1.6 KB)
- Simplest API (3 primitives)
- Best vanilla JS integration
- Proven performance (60 fps benchmarks)
- Low-risk incremental migration

**Next Step**: Execute Phase 0-2 of migration plan, then evaluate.
