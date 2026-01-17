# Workbench V2 Planning

**Planning Date:** 2026-01-17
**Master Task:** task_37 (Note: This task became one of the subtasks during creation. The actual master planning task tracking was handled separately.)
**Subtasks Created:** task_32 through task_41 (all children of task_37)
**Status:** Planning Complete

## Executive Summary

### Current Limitations

The Primer Workbench (http://localhost:3000/) currently provides:
- Basic task review list with approve/reject buttons
- Stats dashboard (total tasks, active agents, pending reviews, completed count)
- Real-time updates via WebSocket
- Active agents display

**Missing Critical Features:**
- Cannot view task details (goal, deliverables, success criteria)
- Cannot see task assets/deliverables/attachments
- No filtering or search capabilities
- Cannot view task dependencies or relationships
- Only supports review tasks (no knowledge entities, agents, general graph exploration)
- No way to drill down into task context

### Proposed Solution

Transform the workbench from a basic monitoring tool into a comprehensive graph explorer with:

1. **Task Detail View** - Click any task to see full details, deliverables, relationships, history
2. **Asset/Deliverable Viewer** - View attached markdown documents, completion reports, artifacts
3. **Filtering & Search** - Filter by status, priority, labels; search task content
4. **Multi-Entity Support** - Browse tasks, knowledge nodes, agents, feedback, artifacts
5. **Graph Visualization** - See relationships visually with force-directed layout
6. **Actions** - Approve/reject reviews, start tasks, complete tasks, add feedback

### Effort Estimate

**Phase 1 (P0 - Must Have):** 5-7 days
- Task detail modal: 2-3 days
- Asset viewer: 1-2 days
- Basic filtering: 2 days

**Phase 2 (P1 - Should Have):** 5-7 days
- Multi-entity support: 3-4 days
- Advanced filtering/search: 2-3 days

**Phase 3 (P2 - Could Have):** 4-6 days
- Graph visualization: 3-5 days
- Polish and testing: 1-2 days

**Total:** 14-20 days (approximately 3-4 weeks)

## Current State Analysis

### What Exists

**Working Features:**
- Vanilla JavaScript dashboard (`browser/dashboard.js`)
- API client wrapper (`browser/api-client.js`)
- WebSocket client with event handling (`browser/ws-client.js`)
- Stats display (total tasks, active agents, pending reviews, completed)
- Review task list with approve/reject actions
- Active agents list with status
- Real-time updates for task creation/updates
- Connection status indicator
- Responsive Tailwind CSS styling

**Architecture:**
```
Browser (Vanilla JS)
├── app.js - Main entry point
├── dashboard.js - Stats + review list
├── api-client.js - HTTP API wrapper
└── ws-client.js - WebSocket event handling
     ↓
Daemon API (src/cli/daemon.ts)
├── GET /api/stats
├── GET /api/reviews
├── GET /api/agents
├── POST /api/reviews/:id/approve
├── POST /api/reviews/:id/reject
└── WebSocket /ws - real-time events
     ↓
Graph Layer (CozoDB + JSON)
└── tasks.json - Task persistence
```

**Technology Stack:**
- No frameworks (vanilla JS, ES modules)
- No build step (native browser imports)
- Tailwind CSS (CDN)
- Fetch API for HTTP
- WebSocket API for real-time
- Template literals for HTML rendering

### What's Missing

**Critical Gaps:**

1. **Task Detail View**
   - Cannot click tasks to see full details
   - No display of deliverables, success criteria, knowledge, tools
   - No relationship/dependency visualization
   - No history timeline

2. **Asset Management**
   - Tasks reference deliverables but cannot view them
   - No way to see completion reports, markdown documents
   - No attachment viewing capability
   - Blob storage research (task_36) needed for implementation

3. **Filtering & Search**
   - Cannot filter by status, priority, label, project
   - No search across task goals/descriptions
   - Large task lists become overwhelming
   - No saved views or bookmarks

4. **Multi-Entity Support**
   - Only shows review tasks
   - Cannot browse all tasks (active, blocked, completed)
   - No knowledge entity browser
   - No agent detail view
   - No feedback node display
   - No artifact viewer

5. **Graph Exploration**
   - No visualization of task dependencies
   - Cannot see parent-child relationships (spawned_by edges)
   - No way to understand blockers
   - Cannot explore knowledge graph

6. **Actions**
   - Only approve/reject for reviews
   - No general task completion
   - Cannot start tasks
   - Cannot add feedback/comments
   - No task creation from UI

### Technical Debt

1. **State Management**
   - Currently ad-hoc with component-level state
   - No centralized state store
   - Signals research (task_35) completed - could use reactive state

2. **API Coverage**
   - Missing endpoints for task details, full graph, filtering
   - No blob/attachment serving
   - No feedback endpoints

3. **Data Model**
   - Feedback nodes designed (workbench-enhancements.spec.md) but not implemented
   - Edge types defined but not fully utilized
   - Knowledge/artifact nodes exist in graph but no UI support

## Requirements Breakdown

### Must Have (P0) - Blocking Approval Workflow

#### 1. Task Detail Modal
**Priority:** P0
**Effort:** 2-3 days
**Blocking:** Yes - Cannot make informed approval decisions without details

**Requirements:**
- Click any task card → opens modal overlay
- Display all task properties:
  - Identity (ID, type, creation timestamp)
  - Goal (full description, may be multi-line)
  - Deliverables (list of expected outputs)
  - Success criteria (objective and subjective)
  - State with visual indicator
  - Priority badge (P0-P4)
  - Labels (tags)
  - Knowledge (known info and gaps)
  - Tools available
  - Parent task (hierarchy)
- Display relationships:
  - Dependencies (depends_on edges)
  - Blockers (blocks edges)
  - Children (spawned_by edges, reverse)
  - Artifacts (produces edges)
  - Required knowledge (requires_knowledge edges)
- Actions:
  - Approve (complete with "Approved" result for reviews)
  - Reject (block with reason via feedback form)
  - Start (transition to active state)
  - Close modal without action
- History timeline:
  - Events from EventLog filtered by nodeId
  - created, started, blocked, completed, feedback_added
  - Chronological display with timestamps
- Keyboard accessibility:
  - ESC to close
  - Tab trap (focus cycles within modal)
  - Prevent body scrolling when open

**API Needs:**
- `GET /api/tasks/:id/full` - Comprehensive task data with edges and history

**Dependencies:**
- None (can implement immediately)

#### 2. Asset/Deliverable Viewer
**Priority:** P0
**Effort:** 1-2 days
**Blocking:** Yes - Need to see completion reports and artifacts

**Requirements:**
- Display list of deliverables/attachments for task
- View markdown documents inline (rendered HTML)
- View text files with syntax highlighting
- Download capability for any file
- Link from task detail modal to asset viewer

**API Needs:**
- `GET /api/tasks/:id/assets` - List assets for task
- `GET /api/assets/:id` or `GET /api/blobs/:id` - Fetch asset content
- Blob storage implementation (depends on task_36 research)

**Dependencies:**
- Blob storage research (task_36)
- May need basic implementation if blob storage not ready

#### 3. Basic Filtering
**Priority:** P0
**Effort:** 2 days
**Blocking:** No - But essential for managing large task lists

**Requirements:**
- Filter by status (all, created, ready, active, blocked, completed, failed)
- Filter by label (any label in system: agent, review, test, etc.)
- Filter by priority (P0, P1, P2, P3, P4)
- Multiple filters combine with AND logic
- Display count per filter option
- Clear individual filters or clear all
- Persist filters in URL query params (shareable views)
- Visual indication of active filters

**API Needs:**
- `GET /api/tasks/counts` - Counts by filter dimension (optional, can compute client-side)

**Dependencies:**
- None (client-side filtering of existing task data)

#### 4. Task Actions
**Priority:** P0
**Effort:** 1 day (integrated with detail modal)
**Blocking:** Yes - Core workflow requirement

**Requirements:**
- Approve review (complete task with "Approved" result)
- Reject review (block task with reason)
- Start task (transition created/ready → active)
- Complete task (transition active → completed)
- Add feedback/comment (create feedback node)

**API Needs:**
- Existing: `POST /api/reviews/:id/approve`, `POST /api/reviews/:id/reject`
- New: `POST /api/tasks/:id/start`, `POST /api/tasks/:id/complete`
- New: `POST /api/tasks/:id/feedback` - Add feedback node

**Dependencies:**
- Feedback node implementation (graph layer)

### Should Have (P1) - High Value Features

#### 5. Multi-Entity Browser
**Priority:** P1
**Effort:** 3-4 days
**Blocking:** No - But needed for full graph exploration

**Requirements:**
- Switch between entity types (tasks, knowledge, agents, feedback, artifacts)
- Unified list view for each entity type
- Search across entity type
- Click entity → detail view (modal or dedicated page)
- Display entity-specific properties
- Show relationships from any entity

**API Needs:**
- `GET /api/knowledge` - List knowledge nodes
- `GET /api/knowledge/:id` - Knowledge detail
- `GET /api/agents/:id` - Agent detail (currently only list)
- `GET /api/feedback` - List feedback (with filters)
- `GET /api/artifacts` - List artifacts

**Dependencies:**
- Task detail modal (establishes pattern for entity views)

#### 6. Advanced Filtering & Search
**Priority:** P1
**Effort:** 2-3 days
**Blocking:** No - Basic filtering sufficient initially

**Requirements:**
- Full-text search across task goals, deliverables, descriptions
- Search knowledge content
- Filter by date range (created, completed)
- Filter by assignee/agent (future, when multi-user)
- Combine filters with OR logic option
- Saved filter views (bookmarks)
- Export filtered results

**API Needs:**
- `GET /api/search?q=query&types=task,knowledge` - Full-text search
- May need search index (future optimization)

**Dependencies:**
- Basic filtering (P0)

#### 7. Enhanced Stats Dashboard
**Priority:** P1
**Effort:** 1 day
**Blocking:** No - Current stats sufficient

**Requirements:**
- Task state distribution chart (created, active, blocked, completed, failed)
- Priority distribution (P0-P4 counts)
- Label distribution (top 10 labels)
- Time-based trends (tasks created/completed per day, week)
- Agent activity timeline

**API Needs:**
- `GET /api/stats/distribution` - Task distributions
- `GET /api/stats/trends` - Time-based aggregations

**Dependencies:**
- None (extends existing stats)

### Could Have (P2) - Future Enhancements

#### 8. Graph Visualization
**Priority:** P2
**Effort:** 3-5 days
**Blocking:** No - High insight value but not blocking workflow

**Requirements:**
- Force-directed layout (nodes repel, edges attract)
- Nodes: color by state, size by priority, label with ID
- Edges: directional arrows, style by edge type
- Interactions:
  - Click node → detail modal
  - Hover → tooltip with summary
  - Drag node → reposition
  - Zoom (mouse wheel or pinch)
  - Pan (drag background)
- Layout controls:
  - Algorithm: force-directed (default), hierarchical, circular
  - Filter by node type, edge type
  - Cluster by label or state
  - Reset view
- Performance:
  - Render up to 500 nodes smoothly
  - Use Canvas API for large graphs
  - Virtual viewport (only render visible)

**API Needs:**
- `GET /api/graph` - Full graph structure (nodes + edges)
- Query params: `?nodeTypes=task,feedback&edgeTypes=depends_on,blocks`

**Dependencies:**
- Task detail modal (for click interaction)

#### 9. Feedback System
**Priority:** P2
**Effort:** 2 days
**Blocking:** No - Basic comment capability sufficient initially

**Requirements:**
- Feedback nodes as first-class graph entities
- Feedback types: approval comment, rejection comment, general note, rating (1-5 stars)
- Feedback UI in task detail modal
- Feedback history timeline
- Search feedback across all tasks
- Feedback persistence in graph (survives compaction)

**API Needs:**
- `POST /api/tasks/:id/feedback` - Create feedback node
- `GET /api/tasks/:id/feedback` - Get task feedback
- `GET /api/feedback/search?q=query` - Search feedback

**Dependencies:**
- Feedback graph schema (defined in spec, needs implementation)
- Task detail modal

#### 10. Bulk Operations
**Priority:** P2
**Effort:** 2 days
**Blocking:** No - Nice to have for efficiency

**Requirements:**
- Select multiple tasks (checkbox selection)
- Bulk complete
- Bulk label (add/remove labels)
- Bulk priority change
- Bulk delete (with confirmation)

**API Needs:**
- `POST /api/tasks/bulk` - Bulk operations endpoint

**Dependencies:**
- Multi-selection UI pattern

## Key Questions

See `WORKBENCH_V2_QUESTIONS.md` for full decision questions.

**Critical Questions:**
1. **State Management:** Use signals (from task_35 research) or stick with vanilla event-driven?
2. **Blob Storage:** What's the decision from task_36 research? File system? Graph blobs? S3-like?
3. **Routing:** Single-page app with routing or modal-based detail views?
4. **API Strategy:** Client-side filtering or server-side query support?
5. **Real-time:** WebSocket updates for all entities or just tasks?

## Proposed Direction

### Architecture Approach

**Reactive State with Signals**
- Leverage signals research from task_35
- Centralized reactive state store
- Components subscribe to state changes
- No manual DOM updates needed

**Master-Detail Layout**
- Keep list view as primary interface
- Modal overlays for detail views
- Avoid full-page navigation (keep it snappy)
- Support deep linking via URL params

**Client-Side First, Server-Optimized Later**
- Start with client-side filtering (load all, filter in browser)
- Add server-side pagination/search when dataset grows
- Use WebSocket for real-time updates across all entity types

**Blob Storage Decision**
- **Option A:** File system relative paths (simple, works immediately)
  - Store deliverables in `data/deliverables/task_N/`
  - Reference by path in task properties
  - Serve via static file endpoint `GET /api/files/:path`
- **Option B:** Graph-embedded blobs (from task_36)
  - Store small files (< 1MB) inline in graph
  - Large files reference external storage
  - More complex but better for compaction
- **Recommendation:** Start with Option A (file system), migrate to Option B if needed

### Technology Choices

**Continue Vanilla JavaScript**
- No frameworks (maintain zero build step)
- Use signals for reactivity (from task_35)
- Keep ES modules and native browser features
- Template literals for HTML rendering

**Tailwind CSS**
- Continue using Tailwind (already working well)
- Add custom components for modal, graph canvas

**Canvas for Graph Viz**
- Use Canvas API (better performance than SVG for 500+ nodes)
- Consider D3-force algorithm (can implement in vanilla JS)

### Phased Roadmap

**Phase 1: Enhanced Tasks View (P0)**
*Estimated: 5-7 days*

Subtasks created:
- task_37_1: Task detail modal
- task_37_2: Asset/deliverable viewer
- task_37_3: Basic filtering (status, priority, labels)
- task_37_4: Task actions API endpoints

**Deliverables:**
- Click task → see full details
- View deliverables/attachments
- Filter task list
- Complete/approve/reject from detail view

**Phase 2: Advanced Interaction (P1)**
*Estimated: 5-7 days*

Subtasks created:
- task_37_5: Multi-entity support (knowledge, agents, feedback, artifacts)
- task_37_6: Advanced filtering and search
- task_37_7: Enhanced stats dashboard

**Deliverables:**
- Browse all entity types
- Search across entities
- Rich stats and trends

**Phase 3: Graph Features (P2)**
*Estimated: 4-6 days*

Subtasks created:
- task_37_8: Graph visualization and exploration
- task_37_9: Feedback system
- task_37_10: Bulk operations

**Deliverables:**
- Visual graph explorer
- Comprehensive feedback mechanism
- Efficient bulk editing

## Subtasks Created

All subtasks created as children of task_37 (via spawned_by edges):

### Phase 1 (P0)
1. **task_32:** Workbench: Task detail modal/drawer
   - Priority: P0, Labels: workbench, ui, feature
   - Modal for viewing full task details, relationships, history, actions

2. **task_33:** Workbench: Asset/deliverable viewer
   - Priority: P0, Labels: workbench, ui, feature
   - Display and render task deliverables (markdown, text, downloads)

3. **task_34:** Workbench: Filtering and search
   - Priority: P0, Labels: workbench, ui, feature
   - Filter by status, priority, labels; basic search

4. **task_35:** Workbench: Task action API endpoints
   - Priority: P0, Labels: workbench, api, backend
   - APIs for start, complete, feedback, task details

### Phase 2 (P1)
5. **task_36:** Workbench: Multi-entity support (tasks/knowledge/agents)
   - Priority: P1, Labels: workbench, ui, architecture
   - Browse different entity types with unified interface

6. **task_37:** Workbench: Advanced filtering, search, saved views
   - Priority: P1, Labels: workbench, ui, feature
   - Full-text search, complex filters, bookmarks
   - Note: This is also the parent task for all workbench subtasks

7. **task_38:** Workbench: State management architecture (signals)
   - Priority: P1, Labels: workbench, architecture
   - Implement signals-based reactive state from signals research

### Phase 3 (P2)
8. **task_39:** Workbench: Graph visualization and exploration
   - Priority: P2, Labels: workbench, ui, graph, viz
   - Force-directed graph with zoom, pan, click interactions

9. **task_40:** Workbench: Feedback system implementation
   - Priority: P2, Labels: workbench, graph, feature
   - Feedback nodes, feedback UI, search feedback

10. **task_41:** Workbench: Bulk operations
    - Priority: P2, Labels: workbench, ui, feature
    - Multi-select and bulk complete/label/priority/delete

## Integration with Existing Work

### Signals Research (task_35 - Completed)

**Key Findings:**
- task_35 completed 2026-01-17
- Deliverables: SIGNALS_COMPARISON.md, SIGNALS_INTEGRATION_PROPOSAL.md, SIGNALS_MIGRATION_PLAN.md
- Recommended signals library for reactive UI

**Integration Points:**
- Use signals for AppState (centralized reactive state)
- Components subscribe to signals for automatic re-rendering
- Eliminates manual DOM updates and event listener management

**Action Items:**
- Review SIGNALS_INTEGRATION_PROPOSAL.md before implementing state management
- Follow migration plan for gradual adoption

### Review Workflow Design (task_16 - To Be Closed)

**Status:** Created, not started
**Deliverables:** Review workflow and activity dashboard design

**Work Completed:**
- `browser/workbench-enhancements.spec.md` - Comprehensive spec for enhancements
- Covers task detail view, filtering, graph viz, feedback mechanism
- Defines API endpoints, component structure, testing strategy

**Integration:**
- Workbench V2 implements features designed in task_16
- Spec provides detailed requirements for subtasks
- Close task_16 as "incorporated into task_37"

**What's Being Incorporated:**
- Task detail modal design
- Filtering system architecture
- Graph visualization patterns
- Feedback node schema
- API endpoint specifications

**What's Being Deferred:**
- Graph editing (read-only viz initially)
- Advanced search (basic search first)
- Hierarchical/circular layouts (force-directed first)
- Export/import features

### Blob Storage Research (task_36 - Active)

**Status:** Created, priority P1
**Deliverables:** Blob storage strategy for task attachments

**Dependencies:**
- task_37_2 (Asset viewer) blocks on blob storage decision
- Need answer: file system, graph-embedded, or external storage?

**Proposed Interim Solution:**
- Use file system storage (`data/deliverables/task_N/`) for immediate needs
- Serve via `GET /api/files/:path` endpoint
- Migrate to better solution when task_36 completes

**Coordination:**
- task_36 should deliver recommendation within 1 week
- task_37_2 implementation can start with file system approach
- Refactor to blob storage later if needed

## Next Steps

### Immediate Actions (This Week)

1. **Answer Key Questions**
   - Review WORKBENCH_V2_QUESTIONS.md
   - Make architectural decisions (signals, blob storage, routing)
   - Document decisions in this plan

2. **Close task_16**
   - Mark as completed (work incorporated into task_37)
   - Link task_16 → task_37 via dependency edge
   - Note in task_16 completion: "Design specs incorporated into Workbench V2 planning"

3. **Review Signals Research**
   - Read SIGNALS_INTEGRATION_PROPOSAL.md from task_35
   - Decide on signals library and integration approach
   - Create signals integration guide for workbench

4. **Coordinate with Blob Storage Research**
   - Check status of task_36
   - If not complete: proceed with file system interim solution
   - Document interim approach in task_37_2 description

### Week 1 Plan (Phase 1 Kickoff)

**Day 1-2: Foundation**
- Implement API endpoints (task_37_4)
  - `GET /api/tasks/:id/full` - Task details with edges and history
  - `POST /api/tasks/:id/start` - Start task
  - `POST /api/tasks/:id/complete` - Complete task
  - `POST /api/tasks/:id/feedback` - Add feedback
- Add feedback node type to graph schema
- Test endpoints with existing tasks

**Day 3-5: Task Detail Modal**
- Implement TaskDetailModal component (task_37_1)
- Modal open/close lifecycle
- Display task properties, relationships, history
- Wire up approve/reject/start actions
- Keyboard accessibility (ESC, Tab trap)

**Day 5: Asset Viewer Start**
- Begin task_37_2 (Asset viewer)
- Implement file system storage for deliverables
- Create `GET /api/files/:path` endpoint
- Display deliverable list in task detail modal

**Checkpoint:** End of Week 1
- Can click tasks and see full details
- Can approve/reject/start tasks from detail view
- Can view basic deliverable list
- Ready for Week 2 (filtering and asset rendering)

### Week 2 Plan (Complete Phase 1)

**Day 1-2: Complete Asset Viewer**
- Markdown rendering in browser
- Syntax highlighting for code files
- Download capability
- Thumbnail preview for images (future)

**Day 3-4: Filtering System**
- Implement FilterPanel component (task_37_3)
- Status, priority, label filters
- URL query param persistence
- Count display per filter option
- Clear filters functionality

**Day 5: Integration & Testing**
- Integrate all Phase 1 features
- End-to-end testing
- Bug fixes and polish
- Update documentation

**Checkpoint:** End of Week 2
- Phase 1 (P0) complete and deployed
- Usable task detail, asset viewing, filtering
- Ready for Phase 2 planning

### Future Weeks

**Week 3-4: Phase 2 (P1)**
- Multi-entity support
- Advanced filtering and search
- Enhanced stats
- Signals-based state management

**Week 5-6: Phase 3 (P2)**
- Graph visualization
- Feedback system
- Bulk operations
- Polish and testing

## Success Metrics

**Phase 1 Success:**
- 100% of review tasks can be inspected in detail view
- Task approval decisions take < 2 minutes (down from needing to check files manually)
- Filter reduces task list to relevant subset (< 20 items visible)
- Zero critical bugs in task detail or approve/reject workflow

**Phase 2 Success:**
- All entity types browsable (tasks, knowledge, agents, feedback, artifacts)
- Search finds relevant entities in < 500ms
- Multi-entity support used by team for knowledge lookup

**Phase 3 Success:**
- Graph visualization renders 500+ nodes at 30fps
- Users discover task dependencies via visual graph
- Feedback mechanism captures at least 1 comment per 5 reviews

**Overall Success:**
- Workbench becomes primary interface for task management
- Replaces need to manually inspect tasks.json or use CLI for most operations
- Human-agent collaboration improves (more context, faster decisions)

## Risk Assessment

### Technical Risks

**Risk: Signals integration complexity**
- Probability: Medium
- Impact: Medium (could delay state management)
- Mitigation: Start without signals, add reactivity incrementally

**Risk: Blob storage decision delay**
- Probability: High (task_36 not complete)
- Impact: Medium (blocks asset viewer)
- Mitigation: Use file system interim solution

**Risk: Graph visualization performance**
- Probability: Medium
- Impact: Low (P2 feature, can defer)
- Mitigation: Start with Canvas API, limit to 500 nodes, virtual viewport

**Risk: API endpoint additions break existing functionality**
- Probability: Low
- Impact: High (breaks current workbench)
- Mitigation: Test existing endpoints before changes, version API if needed

### Schedule Risks

**Risk: Phase 1 takes longer than estimated**
- Probability: Medium
- Impact: Medium (delays later phases)
- Mitigation: Prioritize ruthlessly, defer P1/P2 features if needed

**Risk: Scope creep (adding features mid-phase)**
- Probability: High
- Impact: High (never ship)
- Mitigation: Stick to P0/P1/P2 boundaries, defer new ideas to backlog

### Dependencies

**External Dependencies:**
- task_36 (Blob storage research) - May delay task_37_2
- Daemon API stability - Needs to support new endpoints
- Graph schema changes - Requires careful migration

**Internal Dependencies:**
- Phase 2 blocks on Phase 1 completion (task detail modal pattern)
- Phase 3 blocks on Phase 1 (graph viz needs detail modal for clicks)

## Document History

| Date | Author | Changes |
|------|--------|---------|
| 2026-01-17 | Agent (task_37) | Initial planning document created |
| 2026-01-17 | Agent (task_37) | Subtasks created, key questions identified |
| 2026-01-17 | Agent (task_37) | Integration plan with task_16, task_35, task_36 |

---

**Next Document:** See `WORKBENCH_V2_QUESTIONS.md` for decision questions.
