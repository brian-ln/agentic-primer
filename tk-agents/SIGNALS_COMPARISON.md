# Signals-Based Reactive Programming: Comparative Analysis

**Research Date**: January 17, 2026
**Context**: Evaluating signals libraries for Primer Workbench (tk-agents browser UI)
**Task ID**: task_35

---

## Executive Summary

Signals are reactive primitives that enable fine-grained reactivity without Virtual DOM overhead. For the Primer Workbench, signals could replace manual DOM manipulation and simplify WebSocket-driven state updates. After comparing four major signals libraries, **@preact/signals-core** emerges as the recommended choice due to its small bundle size (1.6 KB), framework-agnostic design, and proven performance characteristics.

---

## Current Workbench Reactive Patterns

### Pain Points Identified

From analyzing the current codebase:

1. **Manual DOM Updates** (`dashboard.js:65, 91-101`)
   - Full `innerHTML` replacement on every data change
   - No granular updates—entire dashboard re-renders
   - Performance overhead from parsing HTML strings

2. **Imperative State Management** (`dashboard.js:14-18`)
   ```javascript
   this.stats = null;
   this.reviews = [];
   this.agents = [];
   ```
   - State stored as class properties
   - No automatic reactivity—manual `render()` calls required
   - WebSocket handlers trigger full reloads (`loadData()` on line 266)

3. **Event Listener Complexity** (`dashboard.js:39`)
   - Manual event delegation setup
   - Cleanup responsibilities (`unmount()` on lines 42-50)
   - No declarative event binding

4. **Polling Overhead** (`dashboard.js:36`)
   ```javascript
   this.refreshInterval = setInterval(() => this.loadData(), 5000);
   ```
   - 5-second polling even when WebSocket is active
   - Redundant network requests
   - Manual interval cleanup required

### Current Reactive Flow

```
API Fetch → setState → render() → innerHTML = "..." → Browser Parse/Paint
WebSocket Event → loadData() → Full Re-render
```

**Problems:**
- No granular reactivity
- High re-render cost (template string parsing + DOM reconciliation)
- State and view are decoupled—synchronization is manual

---

## Signals Libraries Comparison

### 1. @preact/signals-core

**Overview**: Framework-agnostic reactive primitives from the Preact team, designed to work with vanilla JavaScript.

**Bundle Size**:
- **1.6 KB** minified + gzipped (standalone core package)
- Zero dependencies

**API Surface**:
```javascript
import { signal, computed, effect, batch } from '@preact/signals-core';

// Basic signal
const count = signal(0);
count.value += 1; // Updates trigger subscribers

// Computed value (derived state)
const double = computed(() => count.value * 2);

// Effect (side effect subscriber)
effect(() => {
  console.log('Count:', count.value); // Auto-runs on count change
});

// Batch updates (single commit)
batch(() => {
  count.value = 10;
  // Other updates...
});
```

**Integration with DOM**:
```javascript
const name = signal('World');
const greeting = computed(() => `Hello, ${name.value}!`);

effect(() => {
  document.getElementById('output').textContent = greeting.value;
});
```

**Pros**:
- ✅ **Smallest bundle size** (1.6 KB vs. competitors)
- ✅ **Framework-agnostic**—no Preact dependency required
- ✅ **Simple API**—three core primitives (`signal`, `computed`, `effect`)
- ✅ **High performance**—[benchmarks show 60 fps with 20,000 text node updates](https://electricui.com/blog/benchmarking-preact-signals)
- ✅ **Battle-tested**—used in production Preact apps
- ✅ **Automatic dependency tracking**—no manual subscriptions

**Cons**:
- ❌ **No built-in DOM rendering**—requires manual `effect()` for DOM updates
- ❌ **Less mature ecosystem** compared to MobX
- ❌ **Manual cleanup** of effects (though rarely needed)

**Workbench Use Case**:
- Perfect fit for replacing manual `render()` calls
- Small enough to include without bundle concerns
- Can integrate incrementally (start with one component)

**Performance**: Signals updates are within **100 nanoseconds** of vanilla `element.nodeValue = value` assignment ([source](https://preactjs.com/blog/introducing-signals/)).

---

### 2. @vue/reactivity

**Overview**: Standalone reactivity system extracted from Vue 3, usable without Vue.

**Bundle Size**:
- **3.7-6 KB** minified + gzipped
- **~9 KB** with Brotli compression

**API Surface**:
```javascript
import { reactive, ref, computed, effect } from '@vue/reactivity';

// Reactive object (proxy-based)
const state = reactive({ count: 0 });
state.count += 1; // Triggers subscribers

// Ref (single value wrapper, like signal)
const count = ref(0);
count.value += 1;

// Computed
const double = computed(() => count.value * 2);

// Effect (called "watchEffect" in Vue docs)
effect(() => {
  console.log('Count:', count.value);
});
```

**Pros**:
- ✅ **Mature and battle-tested**—powers Vue 3
- ✅ **Proxy-based reactivity**—deep object tracking without explicit nesting
- ✅ **Rich API**—includes `reactive()`, `ref()`, `computed()`, `watch()`, etc.
- ✅ **Good documentation**—[official Vue docs cover reactivity in depth](https://vuejs.org/guide/extras/reactivity-in-depth.html)

**Cons**:
- ❌ **Larger bundle** (3.7-6 KB vs. 1.6 KB for Preact)
- ❌ **More complex API**—`ref()` vs. `reactive()` confusion
- ❌ **Not officially designed for standalone use**—[no end-user docs for standalone usage](https://github.com/orgs/vuejs/discussions/10449)
- ❌ **Proxy incompatibility**—doesn't work in legacy environments (though not a concern for workbench)

**Workbench Use Case**:
- Overkill for simple signal-based reactivity
- Proxy-based deep tracking is unnecessary (flat state structure)
- Larger bundle size for minimal gain

---

### 3. SolidJS Signals (solid-js)

**Overview**: SolidJS's core reactive primitives, designed for fine-grained reactivity without Virtual DOM.

**Bundle Size**:
- **~7-10 KB** minified + gzipped (solid-js core)
- Requires `solid-js` package (not standalone signals)

**API Surface**:
```javascript
import { createSignal, createEffect, createMemo } from 'solid-js';

// Signal (returns [getter, setter] tuple)
const [count, setCount] = createSignal(0);
setCount(count() + 1); // Getter is a function

// Computed (called "memo")
const double = createMemo(() => count() * 2);

// Effect
createEffect(() => {
  console.log('Count:', count());
});
```

**Pros**:
- ✅ **Extremely performant**—[top of JS framework benchmarks](https://thenewstack.io/solidjs-creator-on-fine-grained-reactivity-as-next-frontier/)
- ✅ **Read/write segregation**—separate getter/setter prevents accidental mutations
- ✅ **Fine-grained reactivity**—designed for component-level granularity
- ✅ **No Virtual DOM**—direct DOM updates like Preact Signals

**Cons**:
- ❌ **No standalone signals package**—must use full `solid-js` (larger bundle)
- ❌ **Function-based getters**—`count()` vs. `count.value` (less intuitive)
- ❌ **Tied to SolidJS ecosystem**—harder to use without Solid's JSX compiler
- ❌ **Less vanilla JS friendly**—designed for Solid components

**Workbench Use Case**:
- Not recommended—requires Solid ecosystem
- Bundle size overhead for just signals
- API is less ergonomic for vanilla JS

---

### 4. MobX

**Overview**: Mature reactive state management library using observables and reactions.

**Bundle Size**:
- **~16 KB** minified + gzipped (mobx package)
- Largest of the four options

**API Surface**:
```javascript
import { observable, computed, autorun } from 'mobx';

// Observable (proxy-based)
const state = observable({ count: 0 });
state.count += 1; // Triggers reactions

// Computed
const double = computed(() => state.count * 2);

// Reaction (like effect)
autorun(() => {
  console.log('Count:', state.count);
});
```

**Pros**:
- ✅ **Mature ecosystem**—10+ years of development
- ✅ **Rich feature set**—actions, transactions, middleware, etc.
- ✅ **Mutable state**—feels more natural than immutable patterns
- ✅ **Deep object tracking**—automatic reactivity for nested objects

**Cons**:
- ❌ **Large bundle size** (16 KB vs. 1.6 KB for Preact)
- ❌ **Complex API**—decorators, actions, observables, computed, reactions, etc.
- ❌ **Legacy baggage**—[supports old decorators and non-Proxy fallbacks](https://betterprogramming.pub/zustand-vs-signals-e664bff2ce4a)
- ❌ **Overkill for simple use cases**—designed for complex state management
- ❌ **Performance footgun**—[subtle issues when used outside frameworks](https://medium.com/@gaikwadaditya/why-everyone-is-talking-about-signals-the-future-beyond-state-management-c67f9b327e73)

**Workbench Use Case**:
- Too heavyweight for workbench needs
- Bundle size is 10x larger than Preact Signals
- API complexity outweighs benefits

---

## Feature Comparison Matrix

| Feature                     | @preact/signals-core | @vue/reactivity | solid-js      | MobX          |
|-----------------------------|----------------------|-----------------|---------------|---------------|
| **Bundle Size (gzipped)**   | 1.6 KB               | 3.7-6 KB        | ~7-10 KB      | ~16 KB        |
| **Dependencies**            | 0                    | 0               | 0             | 0             |
| **Framework-Agnostic**      | ✅ Yes               | ⚠️ Intended for Vue | ⚠️ Solid ecosystem | ✅ Yes        |
| **API Simplicity**          | ⭐⭐⭐⭐⭐ (3 primitives) | ⭐⭐⭐ (5+ APIs)  | ⭐⭐⭐⭐ (3 APIs) | ⭐⭐ (10+ APIs) |
| **Vanilla JS Integration**  | ✅ Excellent         | ⚠️ Undocumented | ❌ Poor        | ✅ Good       |
| **Performance**             | ⭐⭐⭐⭐⭐ (60 fps)     | ⭐⭐⭐⭐ (fast)   | ⭐⭐⭐⭐⭐ (fastest) | ⭐⭐⭐ (good)  |
| **Proxy-Based Reactivity**  | ❌ No                | ✅ Yes          | ❌ No         | ✅ Yes        |
| **Automatic Dependency Tracking** | ✅ Yes         | ✅ Yes          | ✅ Yes        | ✅ Yes        |
| **Batched Updates**         | ✅ `batch()`         | ✅ Built-in     | ✅ Built-in   | ✅ `transaction()` |
| **Memory Cleanup**          | ⚠️ Manual            | ⚠️ Manual       | ⚠️ Manual     | ⚠️ Manual     |
| **Documentation Quality**   | ⭐⭐⭐⭐ (good)        | ⭐⭐⭐⭐⭐ (excellent) | ⭐⭐⭐⭐⭐ (excellent) | ⭐⭐⭐⭐ (good) |
| **Ecosystem Maturity**      | ⭐⭐⭐ (growing)      | ⭐⭐⭐⭐⭐ (mature) | ⭐⭐⭐⭐ (mature) | ⭐⭐⭐⭐⭐ (mature) |

---

## Code Example: Current vs. Signals Approach

### Current Approach (Manual DOM Updates)

```javascript
// dashboard.js lines 53-70
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

    this.render(); // Manual re-render
  } catch (error) {
    console.error("Failed to load data:", error);
    this.renderError(error.message);
  }
}

// Lines 91-101
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
- Manual `render()` call on every state change
- Full `innerHTML` replacement (expensive parsing/reconciliation)
- No granular reactivity—entire tree re-renders

---

### Signals Approach (Automatic Reactivity)

```javascript
import { signal, computed, effect } from '@preact/signals-core';

// Reactive state
const stats = signal(null);
const reviews = signal([]);
const agents = signal([]);

// Derived state
const totalTasks = computed(() => stats.value?.totalTasks || 0);
const activeAgents = computed(() => agents.value.length);
const pendingReviews = computed(() => reviews.value.length);

// Auto-update DOM on state changes
effect(() => {
  document.getElementById('total-tasks').textContent = totalTasks.value;
});

effect(() => {
  document.getElementById('active-agents').textContent = activeAgents.value;
});

// Load data (no manual render call)
async function loadData() {
  const [statsData, reviewsData, agentsData] = await Promise.all([
    api.getStats(),
    api.getReviews(),
    api.getAgents(),
  ]);

  stats.value = statsData;       // Triggers effects automatically
  reviews.value = reviewsData;   // Updates only affected DOM nodes
  agents.value = agentsData;     // No full re-render
}

// WebSocket updates (granular reactivity)
ws.on('task_updated', (data) => {
  // Update only the affected task in reviews
  reviews.value = reviews.value.map(r =>
    r.id === data.id ? { ...r, ...data } : r
  );
  // DOM updates automatically via effects
});
```

**Benefits:**
- ✅ **No manual `render()` calls**—effects handle DOM updates
- ✅ **Granular updates**—only changed values trigger DOM updates
- ✅ **Automatic reactivity**—state changes propagate to computed values and effects
- ✅ **WebSocket integration**—just update signal values, DOM reacts

---

## Performance Comparison

### Benchmark Data

From [Electric UI's Preact Signals benchmarks](https://electricui.com/blog/benchmarking-preact-signals):

| Scenario                     | React Hooks | Preact Signals | Improvement |
|------------------------------|-------------|----------------|-------------|
| 20,000 text node updates/frame | ~30 fps     | ~60 fps        | **2x faster** |
| Memory usage                 | Higher      | Lower          | Reduced |
| Update overhead              | VDOM diff   | Direct DOM     | **~100ns** vs. vanilla |

### Why Signals Are Faster

1. **No Virtual DOM diffing**—Direct DOM updates via effects
2. **Fine-grained subscriptions**—Only affected effects re-run
3. **Automatic batching**—Multiple signal updates in one tick are batched
4. **Minimal overhead**—Signal reads/writes are highly optimized

### Workbench-Specific Benefits

Current workbench issues:
- **5-second polling** (`dashboard.js:36`)—wasteful network requests
- **Full innerHTML replacement** (`dashboard.js:94`)—expensive parsing
- **WebSocket triggers full reload** (`dashboard.js:266`)—no granular updates

With signals:
- **Remove polling**—WebSocket updates trigger granular reactivity
- **No innerHTML replacement**—`effect()` updates specific DOM nodes
- **Granular WebSocket updates**—update single task/agent, not entire list

---

## Recommendation: @preact/signals-core

### Why @preact/signals-core Wins

1. **Smallest bundle size** (1.6 KB)—negligible overhead
2. **Framework-agnostic**—designed for vanilla JS
3. **Simple API**—3 primitives (`signal`, `computed`, `effect`)
4. **High performance**—proven in benchmarks
5. **Incremental adoption**—can replace one component at a time
6. **No ecosystem lock-in**—works standalone

### When to Consider Alternatives

- **@vue/reactivity**: If you need deep proxy-based reactivity for nested objects (not needed for workbench)
- **solid-js**: If you're building a full SolidJS app (not applicable)
- **MobX**: If you need a full-featured state management solution with middleware/transactions (overkill for workbench)

---

## Integration Strategy

### Phase 1: Proof of Concept (1-2 hours)

Replace **one component** (e.g., connection status indicator) with signals:

```javascript
// app.js lines 77-96 (current)
function updateConnectionStatus(status) {
  const statusEl = document.getElementById("connection-status");
  if (!statusEl) return;

  const dot = statusEl.querySelector(".inline-block");
  const text = statusEl.querySelector("span:last-child");

  if (status === "connected") {
    dot.classList.remove("bg-gray-400", "bg-red-500");
    dot.classList.add("bg-green-500");
    text.textContent = "Connected";
    // ...
  } else {
    // ...
  }
}

// Signals version
import { signal, effect } from '@preact/signals-core';

const connectionStatus = signal('disconnected');

effect(() => {
  const statusEl = document.getElementById("connection-status");
  if (!statusEl) return;

  const dot = statusEl.querySelector(".inline-block");
  const text = statusEl.querySelector("span:last-child");
  const isConnected = connectionStatus.value === 'connected';

  dot.className = `inline-block w-2 h-2 rounded-full ${
    isConnected ? 'bg-green-500' : 'bg-red-500'
  }`;

  text.textContent = isConnected ? 'Connected' : 'Disconnected';
  text.className = `${isConnected ? 'text-green-600' : 'text-red-500'}`;
});

ws.on('connection', (data) => {
  connectionStatus.value = data.status; // Auto-updates DOM
});
```

**Validation:**
- Verify DOM updates automatically on WebSocket events
- Check for performance improvements (DevTools profiler)
- Ensure no memory leaks (watch effects cleanup)

---

### Phase 2: Dashboard State (2-4 hours)

Convert dashboard state to signals:

```javascript
import { signal, computed } from '@preact/signals-core';

const stats = signal(null);
const reviews = signal([]);
const agents = signal([]);

const totalTasks = computed(() => stats.value?.totalTasks || 0);
const pendingReviewsCount = computed(() => reviews.value.length);
const activeAgentsCount = computed(() => agents.value.length);

// Replace renderStats() with effects
effect(() => {
  const el = document.getElementById('total-tasks');
  if (el) el.textContent = totalTasks.value;
});

effect(() => {
  const el = document.getElementById('pending-reviews');
  if (el) el.textContent = pendingReviewsCount.value;
});
```

**Validation:**
- Remove polling interval (WebSocket-only updates)
- Measure bundle size increase (~1.6 KB)
- Verify WebSocket updates trigger granular DOM changes

---

### Phase 3: Full Migration (4-8 hours)

Replace all `render()` methods with effects. See `SIGNALS_MIGRATION_PLAN.md` for detailed steps.

---

## Bundle Size Impact

### Current Workbench

Minimal external dependencies—vanilla JS + Tailwind CSS.

### With @preact/signals-core

**Additional cost**: +1.6 KB minified + gzipped

**Comparison**:
- **Preact Signals**: 1.6 KB
- **Vue Reactivity**: 3.7-6 KB (2-4x larger)
- **MobX**: 16 KB (10x larger)
- **Zustand** (alternative state lib): ~1 KB

**Verdict**: 1.6 KB is negligible for the workbench. Acceptable trade-off for cleaner code and better performance.

---

## Conclusion

**@preact/signals-core** is the best fit for Primer Workbench:

✅ **Smallest bundle** (1.6 KB)
✅ **Simplest API** (3 primitives)
✅ **Best vanilla JS integration**
✅ **High performance** (60 fps in benchmarks)
✅ **Incremental adoption** (no rewrite required)

**Next Steps:**
1. Create proof-of-concept prototype (Phase 1)
2. Evaluate performance and DX improvements
3. Proceed with full migration if POC succeeds

See `SIGNALS_INTEGRATION_PROPOSAL.md` for detailed design and `SIGNALS_MIGRATION_PLAN.md` for implementation roadmap.

---

## Sources

- [Preact Signals Guide](https://preactjs.com/guide/v10/signals/)
- [Introducing Signals – Preact](https://preactjs.com/blog/introducing-signals/)
- [@preact/signals-core - npm](https://www.npmjs.com/package/@preact/signals-core)
- [Signals in Vanilla JS Tutorial](https://www.trpkovski.com/2023/04/25/signals-in-vanilla-js/)
- [Benchmarking Preact Signals Performance](https://electricui.com/blog/benchmarking-preact-signals)
- [Vue Reactivity in Depth](https://vuejs.org/guide/extras/reactivity-in-depth.html)
- [@vue/reactivity - npm](https://www.npmjs.com/package/@vue/reactivity)
- [SolidJS Signals Documentation](https://docs.solidjs.com/concepts/signals)
- [MobX Adoption Guide](https://blog.logrocket.com/mobx-adoption-guide/)
- [Next-Gen Reactivity: Preact, SolidJS, Svelte Comparison](https://leapcell.io/blog/next-gen-reactivity-rethink-preact-solidjs-signals-vs-svelte-5-runes)
- [Zustand vs. Signals Comparison](https://betterprogramming.pub/zustand-vs-signals-e664bff2ce4a)
