# Workbench V2 Key Decision Questions

**Planning Date:** 2026-01-17
**Master Task:** task_37
**Purpose:** Document critical decision points before implementation

## Architecture Questions

### Q1: State Management - Signals or Vanilla Event-Driven?

**Context:**
- task_35 completed signals research with recommendations
- Current workbench uses ad-hoc component state
- Signals provide reactive updates without manual DOM manipulation

**Options:**

**Option A: Adopt Signals (from task_35 research)**
- Pros:
  - Automatic re-rendering on state changes
  - Cleaner code (no manual event listeners)
  - Better performance (fine-grained reactivity)
  - Leverage research already completed
- Cons:
  - New dependency (library to add)
  - Learning curve for team
  - Migration effort from current approach

**Option B: Stick with Vanilla Event-Driven**
- Pros:
  - Zero dependencies (pure vanilla JS)
  - Team already familiar
  - Simple to understand
- Cons:
  - Manual DOM updates error-prone
  - More boilerplate code
  - Harder to maintain as complexity grows

**Recommendation:** Option A (Signals)
- Rationale: Already invested in task_35 research, worth the migration effort for long-term maintainability
- Follow SIGNALS_INTEGRATION_PROPOSAL.md from task_35

**Decision Maker:** Team lead / architect

**Blocking:** task_37_7 (State management architecture)

---

### Q2: Routing - Single-Page App or Modal-Based?

**Context:**
- Current workbench has no routing (single page)
- Need to support detail views, entity browsing, graph viz
- User insight: Browser History API can replace routing library (no dependencies)
- User mental model: "Route brings you to default state, exploration from there is ephemeral, clicking away returns to default"

**Options:**

**Option A: Modal-Based Detail Views**
- Pros:
  - No routing library needed
  - Faster (no page transitions)
  - Keep list view context visible
  - Simpler to implement
- Cons:
  - Deep linking harder (need URL state)
  - Multiple modals can feel cluttered
  - Mobile UX less intuitive

**Option B: Single-Page App with Browser History API**
- Pros:
  - Natural deep linking (e.g., `/tasks/task_1`)
  - Zero dependencies (native browser API)
  - Better mobile UX (full-page views)
  - Cleaner separation of concerns
  - pushState/replaceState for navigation without page reload
  - popstate event for back/forward button handling
- Cons:
  - More complex state management
  - Losing list context on navigation
  - Need to handle popstate events manually

**Option C: Hybrid (Route-Based Default States + Ephemeral Exploration)**
- Pros:
  - Best of both worlds
  - Routes represent shareable/bookmarkable "default states"
  - Ephemeral exploration (filters, UI state) preserved during session
  - Route change resets to clean slate (feature, not bug)
  - Uses Browser History API (no routing library)
  - Modal overlays for quick views that preserve context
- Cons:
  - Most complex to implement
  - Need clear criteria for when to use routes vs modals
  - Need careful state management (route state vs ephemeral state)

**Recommendation:** Option C (Hybrid with Browser History API)
- Rationale: Matches user's mental model, zero dependencies, best UX
- Implementation:
  - Use `history.pushState()` for route changes (e.g., `/tasks`, `/tasks/task_1`, `/graph`)
  - Use `history.replaceState()` for ephemeral state updates (filters, selections)
  - Listen to `popstate` event for back/forward button handling
  - Modal overlays for quick actions that don't change route

**Hybrid Routing Criteria:**

When to use **Routes** (pushState):
- Shareable/bookmarkable states (task detail, graph viz, settings)
- "Default" views that represent a clean starting point
- Full-page context switches (list → detail → graph)
- Examples: `/tasks`, `/tasks/task_1`, `/graph`, `/agents`, `/knowledge`

When to use **Modals** (no route change):
- Quick views that preserve list context (preview without navigating away)
- Transient actions (create task form, confirm dialog)
- Secondary information (tooltips, help panels)
- Examples: Quick-preview modal, "Create Task" dialog, Settings panel

When to use **replaceState** (ephemeral state):
- Filters that don't change the "default" view (status, priority, labels)
- UI state (sort order, collapsed/expanded sections)
- Search query in current view
- Examples: `?status=active&priority=0`, `?search=graph&sort=date`

**State Management Model:**

**Route State (persisted in URL path):**
- Current view: `/tasks`, `/graph`, `/agents`
- Entity ID: `/tasks/task_1`, `/knowledge/knowledge_5`
- This is the "default state" user lands on when navigating

**Ephemeral State (URL query params, not route):**
- Filters: `?status=active&priority=0&labels=research`
- Sort order: `?sort=date&order=desc`
- Search: `?search=graph`
- UI preferences: `?view=compact&collapsed=filters`

**Behavior:**
- **Route change** (pushState): Resets ephemeral state to defaults (clean slate)
  - Example: `/tasks?status=active` → `/graph` → all filters cleared
- **Ephemeral change** (replaceState): Updates query params without route change
  - Example: `/tasks?status=active` → `/tasks?status=blocked` → same route, different filter
- **Back button**: Restores previous route + ephemeral state (via popstate event)
  - Example: Browser back from `/tasks/task_1` → `/tasks?status=active` → restores both route and filters

**Technical Implementation (Browser History API):**

```javascript
// Route change (pushState) - navigating to new view
function navigateToTask(taskId) {
  history.pushState(
    { view: 'task', taskId },  // state object
    '',                         // title (unused)
    `/tasks/${taskId}`         // URL
  );
  renderTaskDetail(taskId);
}

// Ephemeral state (replaceState) - updating filters
function updateFilters(filters) {
  const params = new URLSearchParams(filters);
  history.replaceState(
    history.state,              // preserve existing state
    '',
    `${location.pathname}?${params}`
  );
  renderFilteredList(filters);
}

// Back/forward handling (popstate)
window.addEventListener('popstate', (event) => {
  if (event.state) {
    // Restore route state
    renderView(event.state);
  } else {
    // Fallback: parse URL
    parseAndRenderCurrentURL();
  }
});
```

**Benefits:**
- Zero dependencies (native browser API)
- Shareable/bookmarkable URLs for all important states
- Clean mental model: routes = defaults, exploration = ephemeral
- Back button "just works" (browser handles history stack)
- Route changes reset ephemeral state (feature: always start from clean slate)

**Decision Maker:** UI/UX lead

**Blocking:** task_37_1 (Task detail modal design)

**References:**
- [MDN: History.pushState()](https://developer.mozilla.org/en-US/docs/Web/API/History/pushState)
- [MDN: History.replaceState()](https://developer.mozilla.org/en-US/docs/Web/API/History/replaceState)
- [MDN: popstate event](https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event)

---

### Q3: API Strategy - Client-Side Filtering or Server-Side Queries?

**Context:**
- Current API returns full lists (all reviews, all agents)
- Client-side filtering works for small datasets (< 1000 tasks)
- May not scale as task count grows

**Options:**

**Option A: Client-Side Filtering (Current)**
- Pros:
  - Instant filtering (no network delay)
  - Simple API (just GET /api/tasks)
  - Works offline once loaded
- Cons:
  - Large payloads (slow initial load for 1000+ tasks)
  - Memory intensive in browser
  - Doesn't scale beyond ~5000 tasks

**Option B: Server-Side Query API**
- Pros:
  - Fast for large datasets
  - Lower memory usage in browser
  - Supports complex queries (Datalog)
- Cons:
  - Network latency on filter changes
  - More complex API surface
  - Requires backend query implementation

**Option C: Hybrid (Load Recent, Query for Old)**
- Pros:
  - Fast for common case (recent tasks)
  - Scales for large datasets
  - Progressive enhancement
- Cons:
  - Most complex
  - Cache invalidation challenges

**Recommendation:** Start with Option A, migrate to Option C if needed
- Rationale: Current task counts are low (< 100), prioritize speed of implementation
- Add server-side pagination when task count > 1000
- Use CozoDB Datalog for server-side queries when implemented

**Decision Maker:** Backend lead

**Blocking:** task_37_3 (Filtering implementation)

**Metrics to Watch:**
- If initial page load > 2 seconds → implement server-side
- If browser memory > 100MB → implement server-side
- If task count > 1000 → implement server-side

---

## Design Questions

### Q4: Layout - Master-Detail or Full-Page Transitions?

**Context:**
- Need to balance list view with detail view
- Desktop vs mobile considerations

**Options:**

**Option A: Master-Detail (Split Screen)**
- Pros:
  - See list and detail simultaneously
  - Common pattern (email clients, file browsers)
  - No context switching
- Cons:
  - Less space for detail content
  - Requires wider screens (mobile harder)
  - More complex responsive design

**Option B: Modal Overlays**
- Pros:
  - Full screen space for details
  - Mobile-friendly
  - Faster to implement
- Cons:
  - Lose list context
  - Multiple modals can stack awkwardly

**Option C: Full-Page Transitions**
- Pros:
  - Maximum space for content
  - Clean mobile UX
  - Natural navigation flow
- Cons:
  - Lose list context completely
  - Back button required
  - Slower (page transitions)

**Recommendation:** Option B (Modal Overlays) for MVP, migrate to Option A later
- Rationale: Fastest to implement, works on mobile, can enhance to master-detail later
- Use modal for detail view initially
- Add split-screen option in Phase 2 for desktop users

**Decision Maker:** UI/UX lead

**Blocking:** task_37_1 (Task detail modal)

---

### Q5: Graph Visualization - How to Show Relationships?

**Context:**
- Tasks have multiple relationship types (depends_on, blocks, spawned_by, produces, requires_knowledge)
- Graph can be complex (500+ nodes, 1000+ edges)

**Options:**

**Option A: Force-Directed Layout (Default)**
- Pros:
  - Natural clustering
  - Well-understood algorithm
  - Works for general graphs
- Cons:
  - Can be messy for large graphs
  - Non-deterministic (layout changes each render)
  - Hard to find specific nodes

**Option B: Hierarchical Layout (Sugiyama)**
- Pros:
  - Clear parent-child relationships
  - Deterministic (same layout each time)
  - Good for DAGs (dependency trees)
- Cons:
  - Requires DAG (breaks on cycles)
  - Less flexible for complex relationships
  - Harder to implement

**Option C: Circular Layout**
- Pros:
  - Compact
  - Easy to implement
  - Works for all graph types
- Cons:
  - Hard to see structure
  - Edge crossings common
  - Limited insight

**Option D: Hybrid (Multiple Layout Modes)**
- Pros:
  - User can choose best layout for their need
  - Force-directed for exploration, hierarchical for dependencies
- Cons:
  - More implementation work
  - More UI controls needed

**Recommendation:** Option D (Hybrid)
- Start with force-directed (Phase 3 initial)
- Add hierarchical layout for dependency view (Phase 3 enhancement)
- Defer circular layout (low value)

**Decision Maker:** Graph viz expert / designer

**Blocking:** task_37_8 (Graph visualization)

---

### Q6: Mobile/Responsive Design - Support Mobile Browsers?

**Context:**
- Current workbench is desktop-optimized
- Tailwind CSS supports responsive design
- Mobile use cases unclear

**Options:**

**Option A: Desktop-Only (Current)**
- Pros:
  - Simpler design
  - Faster implementation
  - Task management likely desktop activity
- Cons:
  - Cannot use on mobile
  - Miss mobile browsing use case

**Option B: Fully Responsive**
- Pros:
  - Works everywhere
  - Future-proof
  - Better accessibility
- Cons:
  - More design work
  - More testing required
  - Complex interactions (graph viz) hard on mobile

**Option C: Responsive Layout, Desktop-Only Features**
- Pros:
  - Basic browsing works on mobile
  - Advanced features (graph viz) desktop-only
  - Progressive enhancement
- Cons:
  - Feature disparity confusing
  - More conditional logic

**Recommendation:** Option C (Responsive layout, desktop-optimized features)
- Rationale: Task lists should work on mobile, graph viz desktop-only
- Use Tailwind responsive classes for basic layout
- Defer complex mobile interactions

**Decision Maker:** Product owner

**Blocking:** None (can decide later, implement responsive CSS from start)

---

## Data Questions

### Q7: Blob Storage - File System, Graph-Embedded, or External?

**Context:**
- task_36 researching blob storage strategy
- Need to store task deliverables (markdown reports, code files, images)
- Current approach: no blob storage (deliverables referenced but not stored)

**Options:**

**Option A: File System Storage**
- Pros:
  - Simple to implement
  - Works immediately
  - Easy to backup (rsync)
  - Can use existing file serving (static endpoint)
- Cons:
  - File paths can break
  - No versioning
  - Not atomic with graph updates
  - Hard to compact

**Option B: Graph-Embedded Blobs**
- Pros:
  - Atomic with task updates
  - Survives compaction
  - Versioned with graph
  - Queryable via Datalog
- Cons:
  - Graph size grows quickly
  - Large blobs (> 1MB) inefficient
  - Harder to implement
  - May need CozoDB blob support

**Option C: External Storage (S3-like)**
- Pros:
  - Scalable
  - Can use CDN
  - Decoupled from graph
- Cons:
  - Most complex
  - Requires cloud service or MinIO
  - Overkill for local-first tool

**Option D: Hybrid (Small in Graph, Large in File System)**
- Pros:
  - Best of both worlds
  - Small files (< 100KB) inline
  - Large files referenced by path
- Cons:
  - Most complex
  - Need size threshold logic

**Recommendation:** Wait for task_36 research completion
- Interim: Use Option A (File system) to unblock development
  - Store deliverables in `data/deliverables/task_N/`
  - Serve via `GET /api/files/:path`
- Migrate to recommended approach when task_36 completes

**Decision Maker:** task_36 research lead

**Blocking:** task_37_2 (Asset viewer)

**Deadline:** Need decision by end of Week 1 (2026-01-24)

---

### Q8: Large Datasets - Pagination, Virtual Scrolling, or Load All?

**Context:**
- Task list may grow to 1000+ items
- Graph may have 500+ nodes
- Need to keep UI responsive

**Options:**

**Option A: Load All (Current)**
- Pros:
  - Instant client-side filtering
  - Simple implementation
  - No pagination UI needed
- Cons:
  - Slow initial load for large datasets
  - High memory usage
  - Doesn't scale beyond ~5000 items

**Option B: Server-Side Pagination**
- Pros:
  - Fast initial load
  - Low memory usage
  - Scales indefinitely
- Cons:
  - Cannot filter/search across all items
  - Slower interaction (network roundtrip)
  - More complex state management

**Option C: Virtual Scrolling (Windowing)**
- Pros:
  - Load all, render only visible
  - Fast scrolling
  - Low DOM overhead
- Cons:
  - More complex implementation
  - Accessibility challenges
  - Fixed row heights needed

**Option D: Hybrid (Load Recent, Lazy Load Old)**
- Pros:
  - Fast for common case (recent tasks)
  - Scales for large datasets
  - Progressive enhancement
- Cons:
  - Most complex
  - Inconsistent UX (sometimes instant, sometimes delayed)

**Recommendation:** Start with Option A, add Option C if needed
- Rationale: Current task count low, prioritize simplicity
- Monitor: If task list > 500 items, implement virtual scrolling
- Monitor: If load time > 2 seconds, implement pagination or virtual scrolling

**Decision Maker:** Frontend lead

**Blocking:** task_37_3 (Filtering), task_37_8 (Graph viz)

**Metrics to Watch:**
- Task count > 500 → implement virtual scrolling
- Initial render > 1 second → optimize or virtualize
- Memory usage > 100MB → implement pagination

---

### Q9: Real-Time Updates - WebSocket for All Entities or Tasks Only?

**Context:**
- Current WebSocket supports task events (created, updated)
- May need real-time updates for knowledge, agents, feedback

**Options:**

**Option A: Tasks Only (Current)**
- Pros:
  - Simple event structure
  - Low bandwidth
  - Easy to implement
- Cons:
  - Knowledge/agents/feedback not real-time
  - Inconsistent UX (some entities update, others don't)

**Option B: All Entities**
- Pros:
  - Consistent real-time experience
  - Comprehensive event stream
  - Better for multi-user (future)
- Cons:
  - More events (higher bandwidth)
  - More complex event handling
  - Potential performance issues

**Option C: Configurable (Subscribe to Entity Types)**
- Pros:
  - Flexible
  - Can optimize bandwidth
  - User controls what updates in real-time
- Cons:
  - Most complex
  - Need subscription protocol

**Recommendation:** Option B (All Entities)
- Rationale: Consistent UX worth the complexity, bandwidth not a concern for local daemon
- Implement entity-agnostic event structure: `{ type: "entity_created" | "entity_updated", entityType: "task" | "knowledge" | "agent", entityId: string, data: any }`

**Decision Maker:** Backend lead

**Blocking:** task_37_5 (Multi-entity support)

---

## Feature Questions

### Q10: Filtering Attributes - What's Most Important?

**Context:**
- Can filter by many attributes (status, priority, labels, dates, assignee, etc.)
- Need to prioritize for Phase 1

**Options for Phase 1:**

**Option A: Minimal (Status Only)**
- Filters: status
- Rationale: Simplest, covers most common use case

**Option B: Essential (Status + Priority + Labels)**
- Filters: status, priority, labels
- Rationale: Covers 80% of filtering needs

**Option C: Comprehensive (All Attributes)**
- Filters: status, priority, labels, dates, assignee, project
- Rationale: Complete filtering from start

**Recommendation:** Option B (Essential)
- Phase 1: status, priority, labels
- Phase 2: Add date range, assignee (when multi-user), project
- Future: Add custom fields, saved filters

**Decision Maker:** Product owner

**Blocking:** task_37_3 (Filtering system)

---

### Q11: Asset Preview - How Deep Should It Go?

**Context:**
- Tasks reference deliverables (markdown, code, images, etc.)
- Need to decide how much to show in UI

**Options:**

**Option A: List Only (No Preview)**
- Show: Filename, size, download button
- Pros: Simple, fast
- Cons: Cannot see content without downloading

**Option B: Text Preview**
- Show: Markdown rendered as HTML, code with syntax highlighting, images inline
- Pros: Rich preview, no download needed
- Cons: More complex, large files slow

**Option C: Selective Preview (Smart Defaults)**
- Show: Small files (< 100KB) previewed, large files list-only
- Pros: Balance of speed and richness
- Cons: Inconsistent (some files preview, others don't)

**Recommendation:** Option B (Text Preview) for Phase 1
- Markdown: Render as HTML (use marked.js or similar)
- Code: Syntax highlighting (use highlight.js or Prism)
- Images: Inline display with max width
- Large files (> 1MB): Show truncated preview with "Load full content" button

**Decision Maker:** Frontend lead

**Blocking:** task_37_2 (Asset viewer)

**Dependencies:** Blob storage decision (Q7)

---

### Q12: Saved Filters/Views - Support Bookmarks?

**Context:**
- Users may want to save common filter combinations
- Example: "My Active P0 Tasks", "Blocked Reviews", "All Agent Work"

**Options:**

**Option A: No Saved Views (Phase 1)**
- Pros: Simpler implementation
- Cons: Users re-apply filters frequently

**Option B: URL Bookmarks**
- Pros: No backend needed (just bookmark the URL)
- Cons: Requires URL state management

**Option C: Named Saved Views**
- Pros: Better UX (dropdown to select view)
- Cons: Need backend storage for views

**Recommendation:** Option B (URL Bookmarks) for Phase 1, Option C for Phase 2
- Phase 1: Persist filters in URL query params (e.g., `?status=active&priority=0`)
- Users can bookmark URLs for common views
- Phase 2: Add "Save View" UI that stores named filters

**Decision Maker:** Product owner

**Blocking:** task_37_6 (Advanced filtering)

---

### Q13: Graph Filtering - Show All Nodes or Filter by Type?

**Context:**
- Full graph has tasks, knowledge, agents, feedback, artifacts (5 node types)
- Graph visualization may be overwhelming with all nodes

**Options:**

**Option A: Show All Nodes (Default)**
- Pros: Complete picture
- Cons: Overwhelming (too much information)

**Option B: Filter by Node Type**
- Pros: Focus on relevant entities
- Cons: Miss cross-entity relationships

**Option C: Progressive Disclosure (Start with Tasks, Expand on Demand)**
- Pros: Simple initial view, can explore deeper
- Cons: More interaction required

**Recommendation:** Option C (Progressive Disclosure)
- Default: Show tasks only
- Checkbox filters: "Show knowledge", "Show feedback", "Show artifacts"
- Default edge types: depends_on, blocks, spawned_by (core task relationships)
- Advanced: Toggle edge types (requires_knowledge, produces, has_feedback)

**Decision Maker:** Graph viz expert

**Blocking:** task_37_8 (Graph visualization)

---

### Q14: Feedback Types - What Categories?

**Context:**
- Need to categorize feedback for filtering and display
- Current spec defines: approval, rejection, note, rating

**Options:**

**Option A: Simple (Note Only)**
- Types: note
- Pros: Simplest, covers all cases
- Cons: Cannot filter by feedback type

**Option B: Workflow-Aligned (Approval, Rejection, Note)**
- Types: approval, rejection, note
- Pros: Aligns with review workflow
- Cons: Rating separate or omitted

**Option C: Comprehensive (Approval, Rejection, Note, Rating, Question)**
- Types: approval, rejection, note, rating, question
- Pros: Rich categorization
- Cons: More complex UI

**Recommendation:** Option B (Workflow-Aligned) for Phase 1
- Types: approval (with optional comment), rejection (with required reason), note (general comment)
- Phase 2: Add rating (1-5 stars) and question (requires response) types

**Decision Maker:** Product owner

**Blocking:** task_37_9 (Feedback system)

---

## Summary Table

| # | Question | Priority | Blocking Task | Deadline | Status |
|---|----------|----------|---------------|----------|--------|
| Q1 | Signals vs Vanilla | P1 | task_37_7 | Week 2 | Open |
| Q2 | Modal vs Routing | P0 | task_37_1 | Week 1 | Open |
| Q3 | Client vs Server Filtering | P0 | task_37_3 | Week 1 | Open |
| Q4 | Master-Detail vs Modal | P0 | task_37_1 | Week 1 | Open |
| Q5 | Graph Layout Options | P2 | task_37_8 | Week 5 | Open |
| Q6 | Mobile Support | P2 | None | Week 3 | Open |
| Q7 | Blob Storage Strategy | P0 | task_37_2 | Week 1 | Blocked on task_36 |
| Q8 | Pagination vs Virtual Scrolling | P1 | task_37_3 | Week 2 | Open |
| Q9 | Real-Time All Entities | P1 | task_37_5 | Week 3 | Open |
| Q10 | Essential Filters | P0 | task_37_3 | Week 1 | Open |
| Q11 | Asset Preview Depth | P0 | task_37_2 | Week 1 | Open |
| Q12 | Saved Views | P2 | task_37_6 | Week 4 | Open |
| Q13 | Graph Filtering | P2 | task_37_8 | Week 5 | Open |
| Q14 | Feedback Types | P2 | task_37_9 | Week 6 | Open |

## Decision Process

**For P0 Questions (Week 1):**
1. Review this document
2. Discuss with team (async or sync)
3. Document decision in WORKBENCH_V2_PLANNING.md
4. Proceed with implementation

**For P1 Questions (Week 2-3):**
1. Prototype approaches if uncertain
2. Gather user feedback on prototypes
3. Decide based on data

**For P2 Questions (Week 4+):**
1. Defer until Phase 3 planning
2. May change based on learnings from Phase 1-2

---

**Next Steps:**
1. Review questions with team
2. Make P0 decisions (Q2, Q3, Q4, Q7, Q10, Q11)
3. Document decisions in planning doc
4. Begin Phase 1 implementation
