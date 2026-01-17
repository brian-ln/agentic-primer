# Simple DOM Update Approach

**Date**: January 17, 2026
**Context**: Refocusing from complex framework patterns to simplest viable solution
**Parent Research**: Agent aeeea84 Web Components research

---

## What's the Actual Problem?

Looking at `/browser/dashboard.js`, the current issues are:

1. **Full `innerHTML` replacement on every update** (line 94)
   - Re-parses entire dashboard HTML every 5 seconds
   - Loses focus/scroll state
   - Inefficient

2. **WebSocket events trigger full reloads** (lines 263-271)
   - `loadData()` fetches ALL data again
   - Not just the changed item

3. **Polling every 5 seconds** (line 36)
   - Could rely on WebSocket instead

**That's it.** The rest works fine.

---

## What Do We Actually Need to Update?

Inspecting the DOM operations needed:

### 1. Stats Cards (4 numbers)
- Total Tasks: `.textContent = number`
- Active Agents: `.textContent = number`
- Pending Reviews: `.textContent = number`
- Completed: `.textContent = number`

### 2. Connection Status (1 indicator)
- Dot color: toggle class `bg-green-500` / `bg-red-500`
- Text: `.textContent = "Connected"` / `"Disconnected"`
- Already has working code in `app.js` lines 77-97

### 3. Reviews List (~5 items)
- Add new review: insert DOM node
- Remove review: delete DOM node
- Update review: change text/attributes

### 4. Agents List (~1-3 items)
- Add agent: insert DOM node
- Remove agent: delete DOM node
- Update agent: change text

---

## Simplest Possible Solution

**Don't overthink it.** Just update the specific DOM nodes that changed.

### Approach 1: Direct DOM Updates (No Library)

#### For Stats (Just Update Numbers)

```javascript
// dashboard.js - Add this method
updateStats(stats) {
  this.stats = stats;

  // Update just the numbers, not the whole HTML
  const cards = this.element.querySelectorAll('.stat-value');
  if (cards[0]) cards[0].textContent = stats.totalTasks || 0;
  if (cards[1]) cards[1].textContent = stats.activeAgents || 0;
  if (cards[2]) cards[2].textContent = stats.pendingReviews || 0;
  if (cards[3]) cards[3].textContent = stats.tasksByState?.completed || 0;
}
```

**Change HTML to have IDs:**

```javascript
renderStats() {
  // ... existing code but add class="stat-value" to the <dd> elements
  return `
    <div class="grid grid-cols-1 md:grid-cols-4 gap-4">
      <div class="bg-white overflow-hidden shadow rounded-lg">
        <div class="px-4 py-5 sm:p-6">
          <dt class="text-sm font-medium text-gray-500 truncate">Total Tasks</dt>
          <dd class="stat-value mt-1 text-3xl font-semibold text-gray-900">${totalTasks}</dd>
        </div>
      </div>
      <!-- ... repeat with class="stat-value" for others -->
    </div>
  `;
}
```

#### For Reviews List (Keyed Updates)

```javascript
// dashboard.js - Replace renderReviews() with updateReviews()
updateReviews(reviews) {
  this.reviews = reviews;

  const container = this.element.querySelector('#reviews-list');
  if (!container) return;

  // Build map of existing reviews
  const existingReviews = new Map();
  container.querySelectorAll('[data-review-id]').forEach(el => {
    existingReviews.set(el.dataset.reviewId, el);
  });

  // Update or create reviews
  const fragment = document.createDocumentFragment();

  reviews.slice(0, 5).forEach(review => {
    let reviewEl = existingReviews.get(review.id);

    if (reviewEl) {
      // Already exists - just update text if needed
      const goalEl = reviewEl.querySelector('.review-goal');
      if (goalEl) goalEl.textContent = review.goal;
      existingReviews.delete(review.id); // Mark as still in use
      fragment.appendChild(reviewEl);
    } else {
      // New review - create element
      reviewEl = this.createReviewElement(review);
      fragment.appendChild(reviewEl);
    }
  });

  // Remove reviews that are gone
  existingReviews.forEach(el => el.remove());

  // Replace container contents
  container.innerHTML = '';
  container.appendChild(fragment);
}

createReviewElement(review) {
  const div = document.createElement('div');
  div.className = 'border border-gray-200 rounded-lg p-4 hover:bg-gray-50 task-card';
  div.dataset.reviewId = review.id;
  div.innerHTML = `
    <div class="flex items-start justify-between">
      <div class="flex-1">
        <div class="flex items-center gap-2">
          <span class="status-emoji">${this.getStatusEmoji(review.state)}</span>
          <span class="text-sm font-mono text-gray-500">${this.escapeHtml(review.id)}</span>
          ${review.priority !== undefined ? `
            <span class="priority-badge ${this.getPriorityClass(review.priority)}">
              P${review.priority}
            </span>
          ` : ''}
        </div>
        <p class="review-goal mt-1 text-sm text-gray-900">${this.escapeHtml(review.goal)}</p>
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
  return div;
}
```

#### For WebSocket Events (Granular Updates)

```javascript
// dashboard.js - Make WebSocket handlers update specific items
handleTaskUpdate(data) {
  console.log("Task updated:", data);

  // Is it a review?
  const reviewIndex = this.reviews.findIndex(r => r.id === data.id);
  if (reviewIndex !== -1) {
    this.reviews[reviewIndex] = { ...this.reviews[reviewIndex], ...data };
    this.updateReviews(this.reviews); // Only update reviews section
    return;
  }

  // Otherwise, might need stats refresh
  this.api.getStats().then(stats => this.updateStats(stats));
}

handleTaskCreated(data) {
  console.log("Task created:", data);

  // If it's a review, add to list
  if (data.labels?.includes('review')) {
    this.reviews.unshift(data);
    this.updateReviews(this.reviews);
  }

  // Update stats count
  this.api.getStats().then(stats => this.updateStats(stats));
}
```

---

## Alternative: Micro-Library Approach

If the keyed diffing feels too manual, we could use a tiny utility:

### Option 1: `morphdom` (5.8 KB)
- Diffs HTML and patches only changes
- Zero learning curve - drop-in replacement

```javascript
import morphdom from 'morphdom';

updateReviews(reviews) {
  this.reviews = reviews;
  const container = this.element.querySelector('#reviews-list');
  const newHTML = this.renderReviewsList(reviews);
  morphdom(container, newHTML);
}
```

**Pros:**
- Simplest code
- Handles all edge cases
- Battle-tested

**Cons:**
- Extra 5.8 KB
- Another dependency

### Option 2: Write Our Own 20-Line Differ

```javascript
// utils/dom-diff.js
export function updateList(container, items, keyFn, createFn, updateFn) {
  const existing = new Map();
  container.querySelectorAll('[data-key]').forEach(el => {
    existing.set(el.dataset.key, el);
  });

  const fragment = document.createDocumentFragment();

  items.forEach(item => {
    const key = keyFn(item);
    let el = existing.get(key);

    if (el) {
      updateFn(el, item);
      existing.delete(key);
    } else {
      el = createFn(item);
      el.dataset.key = key;
    }

    fragment.appendChild(el);
  });

  existing.forEach(el => el.remove());
  container.innerHTML = '';
  container.appendChild(fragment);
}
```

**Usage:**

```javascript
updateReviews(reviews) {
  this.reviews = reviews;
  const container = this.element.querySelector('#reviews-list');

  updateList(
    container,
    reviews.slice(0, 5),
    review => review.id,
    review => this.createReviewElement(review),
    (el, review) => {
      const goalEl = el.querySelector('.review-goal');
      if (goalEl) goalEl.textContent = review.goal;
    }
  );
}
```

**Pros:**
- Zero dependencies
- Exactly what we need
- 20 lines, easy to understand

**Cons:**
- We wrote it (but it's simple enough)

---

## Recommendation

**Start with Approach 1 (Direct DOM Updates), No Library**

Why?
1. **Simple**: 50 lines of code vs 800 lines of signals proposal
2. **Direct**: Update exactly what changed
3. **No dependencies**: Zero bundle size increase
4. **Easy to debug**: Vanilla JS, no magic
5. **Sufficient**: Workbench has <100 items, not Facebook-scale

**If** we find the keyed diffing code repetitive across reviews/agents, then:
- Extract to `updateList()` utility (20 lines)
- Still no external dependencies

**Only if** we have clear performance problems or code complexity:
- Consider `morphdom` (5.8 KB)
- Still way simpler than full signals migration

---

## Implementation Plan

### Phase 1: Stats Only (1 hour)
1. Add `.stat-value` classes to stats HTML
2. Add `updateStats()` method
3. Change `loadData()` to call `updateStats()` instead of `render()`
4. Test: Stats update without full re-render

### Phase 2: Reviews Granular Updates (2 hours)
1. Add `#reviews-list` container ID
2. Add `updateReviews()` method with keyed diffing
3. Add `createReviewElement()` helper
4. Update WebSocket handlers to use `updateReviews()`
5. Test: Review updates don't lose focus

### Phase 3: Agents (1 hour)
1. Same pattern as reviews
2. Add `updateAgents()` method

### Phase 4: Remove Polling (30 min)
1. Remove `setInterval` from line 36
2. Rely on WebSocket for updates
3. Add reconnection fallback if needed

**Total: ~4.5 hours vs 20+ hours for signals migration**

---

## Questions for Discussion

1. **Is the keyed diffing too manual?**
   - If yes: Extract to `updateList()` utility
   - If still yes: Consider `morphdom`

2. **Do we need reactive computed values?**
   - Currently: No. Stats come from API already computed.
   - Future: If we add client-side filtering/sorting, reconsider.

3. **Do we want a "state store" pattern?**
   - Could add a simple store without signals:

   ```javascript
   // state.js
   class State {
     constructor() {
       this.listeners = new Map();
       this.data = {};
     }

     set(key, value) {
       this.data[key] = value;
       this.listeners.get(key)?.forEach(fn => fn(value));
     }

     get(key) {
       return this.data[key];
     }

     subscribe(key, fn) {
       if (!this.listeners.has(key)) this.listeners.set(key, []);
       this.listeners.get(key).push(fn);
     }
   }
   ```

   But honestly, for this workbench, probably overkill.

---

## What We're NOT Doing

❌ Virtual DOM
❌ Framework adoption
❌ Complex reactive graph
❌ Web Components (unless we find a specific need)
❌ Over-engineering

✅ Direct DOM updates
✅ Simple keyed diffing for lists
✅ Optional 20-line utility if code gets repetitive
✅ Keep it boring and maintainable

---

## Follow-Up

If this approach is approved:
1. Create proof-of-concept for Phase 1 (stats update)
2. Measure: lines of code, bundle size, performance
3. Validate: Does it solve the actual problem?
4. Decide: Continue or pivot

**Key metric**: Can we eliminate the 5-second polling and full `innerHTML` replacement in <5 hours of work with <50 lines of new code?

I believe: **Yes.**
