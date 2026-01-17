# Workbench V2 Routing Strategy Refinement

**Date:** 2026-01-17
**Agent:** workbench-routing-refinement
**Parent Task:** task_37
**Context:** Q2 refinement based on user insights

---

## Executive Summary

Refined the Workbench V2 routing strategy (Q2 in WORKBENCH_V2_QUESTIONS.md) based on three critical user insights:

1. **Browser History API**: Can replace routing library (zero dependencies)
2. **Hybrid Criteria**: Need clear decision matrix for routes vs modals
3. **State Management Model**: "Route = default state, exploration = ephemeral, navigation = clean slate"

**Result:** Updated Q2 with comprehensive hybrid approach using native Browser History API.

---

## Part 1: Browser History API Evaluation

### Research Summary

The Browser History API provides three key primitives:

| Method | Purpose | Use Case |
|--------|---------|----------|
| `history.pushState(state, title, url)` | Add new history entry | Route changes (e.g., `/tasks` → `/tasks/task_1`) |
| `history.replaceState(state, title, url)` | Update current entry | Ephemeral state (e.g., filters, sort order) |
| `window.addEventListener('popstate')` | Handle back/forward | Browser navigation buttons |

### Benefits vs Routing Library

| Aspect | Routing Library | Browser History API |
|--------|-----------------|---------------------|
| Dependencies | Yes (React Router, etc.) | Zero (native) |
| Bundle Size | ~10-50KB | 0KB |
| Learning Curve | Framework-specific | Standard web API |
| Flexibility | Opinionated patterns | Full control |
| Maintenance | External dependency | Browser-native (stable) |

**Verdict:** Browser History API is superior for this use case (zero dependencies, full control, no framework lock-in).

### Technical Details

```javascript
// Route change (pushState) - adds to history stack
history.pushState(
  { view: 'task', taskId: 'task_1' },  // state object (serializable)
  '',                                   // title (unused by browsers)
  '/tasks/task_1'                      // URL (must be same-origin)
);

// Ephemeral state (replaceState) - updates current entry
history.replaceState(
  history.state,                       // preserve state object
  '',
  '/tasks?status=active&priority=0'   // updated URL
);

// Back/forward handling (popstate)
window.addEventListener('popstate', (event) => {
  console.log(event.state);  // { view: 'task', taskId: 'task_1' }
  renderView(event.state);
});
```

**Key Constraint:** URL must be same-origin (cannot navigate to different domain).

**Reference:** [MDN: Working with the History API](https://developer.mozilla.org/en-US/docs/Web/API/History_API/Working_with_the_History_API)

---

## Part 2: Hybrid Approach Criteria

### Decision Matrix: Routes vs Modals vs Ephemeral State

#### When to use **Routes** (pushState):

**Criteria:**
- State is shareable/bookmarkable (user may want to send link)
- State represents a "default" view (clean starting point)
- Full-page context switch (navigating to fundamentally different view)
- State should be preserved in browser history (back button should return here)

**Examples:**
- `/tasks` - Default task list view
- `/tasks/task_1` - Task detail (shareable)
- `/graph` - Graph visualization (different context)
- `/agents` - Agent management view
- `/knowledge/knowledge_5` - Knowledge base entry

**Non-Examples:**
- Quick-preview modal (not shareable, transient)
- "Create Task" dialog (action, not view)
- Settings panel (secondary UI)

#### When to use **Modals** (no route change):

**Criteria:**
- Preserve list context (user wants to see list behind modal)
- Transient action (create, edit, delete forms)
- Secondary information (help, tooltips, confirmations)
- NOT shareable (session-specific or temporary)

**Examples:**
- Quick-preview modal (hover or click to see snippet)
- "Create Task" dialog (form submission)
- Confirm deletion dialog
- Settings panel (overlays main view)
- Help tooltips

**Non-Examples:**
- Task detail (shareable, should be route)
- Graph viz (full-page context switch)

#### When to use **Ephemeral State** (replaceState):

**Criteria:**
- User preferences that don't change the "default" view
- Filters/sort that narrow existing view (not new view)
- UI state (collapsed/expanded, view mode)
- Should be preserved when clicking back (query params restored)

**Examples:**
- Filters: `?status=active&priority=0&labels=research`
- Sort: `?sort=date&order=desc`
- Search: `?search=graph`
- View mode: `?view=compact&collapsed=true`

**Non-Examples:**
- Task ID (that's a route: `/tasks/task_1`)
- View type (that's a route: `/tasks` vs `/graph`)

### Decision Flowchart

```
Is the state shareable/bookmarkable?
├─ YES: Is it a full-page context switch?
│  ├─ YES: Use Route (pushState)
│  └─ NO: Is it transient/temporary?
│     ├─ YES: Use Modal (no route change)
│     └─ NO: Use Ephemeral State (replaceState)
└─ NO: Is it a user preference/filter?
   ├─ YES: Use Ephemeral State (replaceState)
   └─ NO: Use Modal (no route change)
```

---

## Part 3: State Management Model

### User's Mental Model

> "Route brings you to default state, exploration from there is ephemeral, clicking away returns to default"

This perfectly describes a **Route = Default + Ephemeral Exploration** pattern.

### Route State (URL Path)

**What it represents:**
- The "default" view user lands on when navigating
- Shareable/bookmarkable entry point
- Clean slate (no filters, no UI preferences)

**Examples:**
- `/tasks` → Default task list (no filters)
- `/tasks/task_1` → Task detail (default view of task_1)
- `/graph` → Graph visualization (default layout, all nodes)

**Reset behavior:**
- Route change → ephemeral state resets to defaults
- Example: `/tasks?status=active` → navigate to `/graph` → all filters cleared

### Ephemeral State (URL Query Params)

**What it represents:**
- User's exploration of the current route
- Filters, sort order, UI preferences
- Session-specific (may not apply to other routes)

**Examples:**
- `?status=active&priority=0` → Filtered task list
- `?search=graph&sort=date` → Search + sort
- `?view=compact&collapsed=true` → UI preferences

**Preservation behavior:**
- Back button → restores ephemeral state (via popstate event)
- Example: Browser back from `/tasks/task_1` to `/tasks?status=active` → filters restored

### State Lifecycle

```
1. User lands on route: /tasks
   - Ephemeral state: empty (defaults)
   - UI: Shows all tasks

2. User applies filter: status=active
   - Route: /tasks (unchanged)
   - Ephemeral state: ?status=active (replaceState)
   - UI: Shows active tasks only

3. User clicks task: task_1
   - Route: /tasks/task_1 (pushState)
   - Ephemeral state: empty (RESET)
   - UI: Shows task detail (clean slate)

4. User clicks back button
   - Route: /tasks (popstate)
   - Ephemeral state: ?status=active (RESTORED)
   - UI: Shows active tasks (ephemeral state preserved)
```

**Key Insight:** Route change = reset to defaults (feature, not bug).

This aligns with user expectation: "Clicking task link should show task as it is, not with my current filters applied."

---

## Part 4: Updated Q2 in WORKBENCH_V2_QUESTIONS.md

### Changes Made

1. **Added Browser History API as Option B**
   - Updated to highlight zero dependencies
   - Listed native API benefits (pushState, replaceState, popstate)

2. **Updated Option C (Hybrid) to incorporate Browser History API**
   - Renamed to "Route-Based Default States + Ephemeral Exploration"
   - Explicitly uses Browser History API (not routing library)

3. **Added "Hybrid Routing Criteria" section**
   - Decision matrix for routes vs modals vs ephemeral state
   - Concrete examples for each pattern
   - Clear criteria (shareable, transient, full-page)

4. **Added "State Management Model" section**
   - Route state (URL path) = default view
   - Ephemeral state (query params) = exploration
   - Behavior: route change resets ephemeral state
   - Back button restores ephemeral state

5. **Added "Technical Implementation" code examples**
   - `pushState()` for routes
   - `replaceState()` for ephemeral state
   - `popstate` event handling

6. **Added MDN references**
   - Links to official documentation for pushState, replaceState, popstate

### File Size

- **Before:** ~19KB (original Q2 section)
- **After:** ~24KB (refined Q2 section)
- **Added:** ~5KB of routing strategy details

---

## Validation Against User Insights

### Insight 1: "Couldn't you just use the browser history API for routing?"

**Addressed:** ✅
- Option B now explicitly uses Browser History API
- Option C (recommended) uses Browser History API (not routing library)
- Zero dependencies, native browser support
- Technical implementation examples provided

### Insight 2: "Hybrid seems reasonable if you can come up with criteria for when to use each"

**Addressed:** ✅
- "Hybrid Routing Criteria" section added
- Decision matrix: routes vs modals vs ephemeral state
- Concrete examples for each pattern
- Flowchart for decision-making

### Insight 3: "Route-based brings you to default state, exploration from there is ephemeral, clicking away returns to default"

**Addressed:** ✅
- "State Management Model" section matches this mental model
- Route = default state (clean slate)
- Ephemeral = exploration (filters, UI state)
- Route change = reset ephemeral state (feature)
- Back button = restore ephemeral state (via popstate)

---

## Quality Assessment

### Browser History API Research

**Sources:**
- [MDN: History.pushState()](https://developer.mozilla.org/en-US/docs/Web/API/History/pushState) - Last updated June 2025
- [MDN: History.replaceState()](https://developer.mozilla.org/en-US/docs/Web/API/History/replaceState) - Recent update
- [MDN: popstate event](https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event) - Last updated May 2025

**Quality:** High - Recent MDN documentation, stable API (widely supported).

### Criteria Specificity

**Routes (pushState):**
- ✅ Specific: "shareable/bookmarkable states"
- ✅ Actionable: Examples provided (`/tasks`, `/tasks/task_1`, `/graph`)
- ✅ Not vague: Clear distinction from modals

**Modals (no route change):**
- ✅ Specific: "preserve list context", "transient actions"
- ✅ Actionable: Examples provided (quick-preview, create dialog)
- ✅ Not vague: Clear criteria (not shareable, temporary)

**Ephemeral State (replaceState):**
- ✅ Specific: "filters that don't change default view"
- ✅ Actionable: Examples provided (query params)
- ✅ Not vague: Clear reset behavior on route change

### State Management Model Alignment

**User's Mental Model:**
> "Route brings you to default state, exploration from there is ephemeral, clicking away returns to default"

**Documented Model:**
- Route = default state ✅
- Exploration = ephemeral state ✅
- Clicking away (route change) = reset to defaults ✅
- Back button = restore ephemeral state ✅

**Alignment:** Perfect match.

---

## Recommendations

### Immediate Next Steps

1. **Review updated Q2** with UI/UX lead
2. **Validate criteria** with team (routes vs modals decision matrix)
3. **Prototype Browser History API integration** (quick POC)
4. **Document edge cases**:
   - What happens when user manually edits URL?
   - How to handle invalid routes (404 view)?
   - Should ephemeral state persist across sessions (localStorage)?

### Future Considerations

**URL State Persistence:**
- Should filters persist across browser sessions?
- Option A: No (ephemeral = session-only)
- Option B: Yes (save to localStorage, restore on load)
- Recommendation: Start with A, add B if users request it

**404 Handling:**
- Invalid routes (e.g., `/tasks/nonexistent_task`)
- Should show 404 page or redirect to `/tasks`?
- Recommendation: Show "Task not found" message, link to `/tasks`

**Deep Linking:**
- Should ephemeral state be shareable? (e.g., `/tasks?status=active`)
- Yes - query params are in URL, shareable by design
- Browser History API supports this (replaceState preserves query params)

---

## Deliverables

1. **WORKBENCH_V2_QUESTIONS.md (updated)**
   - File: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/WORKBENCH_V2_QUESTIONS.md`
   - Size: 24KB (was 19KB, added 5KB)
   - Section: Q2 (Routing)
   - Changes:
     - Added Browser History API option
     - Added Hybrid Routing Criteria
     - Added State Management Model
     - Added Technical Implementation examples
     - Added MDN references

2. **WORKBENCH_V2_ROUTING_REFINEMENT.md (this document)**
   - File: `/Users/bln/play/projects/proj-20260113-150839/agentic-primer/tk-agents/WORKBENCH_V2_ROUTING_REFINEMENT.md`
   - Size: ~12KB
   - Purpose: Detailed explanation of routing strategy refinement
   - Sections:
     - Browser History API evaluation
     - Hybrid criteria decision matrix
     - State management model
     - Validation against user insights

---

## Success Metrics Achieved

### Part 1: Browser History API Evaluation

- ✅ Researched browser History API (pushState, replaceState, popstate)
- ✅ Evaluated benefits: zero dependencies, native, simple
- ✅ Documented in Q2 as Option B (updated existing option)

### Part 2: Hybrid Approach Criteria

- ✅ Defined when to use routes (shareable, default states)
- ✅ Defined when to use modals (preserve context, transient)
- ✅ Defined when to use ephemeral state (filters, UI preferences)
- ✅ Created decision matrix with examples
- ✅ Provided flowchart for decision-making

### Part 3: State Management Model

- ✅ Documented "default state" (route = clean slate)
- ✅ Documented "ephemeral exploration" (filters, UI state)
- ✅ Clarified behavior: route change resets ephemeral state
- ✅ Documented as feature (clean slate on navigation)
- ✅ Back button restores ephemeral state (via popstate)

### Part 4: Update Q2

- ✅ Added Browser History API option
- ✅ Added hybrid criteria section
- ✅ Added state management explanation
- ✅ Updated recommendation (Option C with Browser History API)
- ✅ Preserved existing document structure

---

## Issues Encountered

None. All objectives completed successfully.

---

## Total Duration

- Start: 2026-01-17T13:48:36-05:00
- End: 2026-01-17T13:49:56-05:00
- Duration: ~1.5 minutes (agent execution time)

---

## Sources

- [MDN: History.pushState()](https://developer.mozilla.org/en-US/docs/Web/API/History/pushState)
- [MDN: History.replaceState()](https://developer.mozilla.org/en-US/docs/Web/API/History/replaceState)
- [MDN: popstate event](https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event)
- [MDN: Working with the History API](https://developer.mozilla.org/en-US/docs/Web/API/History_API/Working_with_the_History_API)
