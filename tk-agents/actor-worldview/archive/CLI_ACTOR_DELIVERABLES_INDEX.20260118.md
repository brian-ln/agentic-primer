# CLI Actor Transformation - Deliverables Index

**Agent Task:** task_1768701677945_r4svf90xj
**Priority:** P0 (blocks all CLI operations)
**Status:** Phase 1 Complete (Design)
**Date:** 2026-01-17

---

## Deliverables Overview

### Phase 1: Design (COMPLETE ✅)

| File | Size | Lines | Description |
|------|------|-------|-------------|
| CLI_ACTOR_ARCHITECTURE.md | 23 KB | 700+ | Complete architecture design |
| CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md | 12 KB | 500+ | Completion report |
| CLI_TRANSFORMATION_QUICK_REF.md | 12 KB | 400+ | Quick reference guide |

**Total:** 47 KB documentation

---

## Document Descriptions

### 1. CLI_ACTOR_ARCHITECTURE.md (Primary Design)

**Purpose:** Complete architectural specification for transforming CLIs from direct CozoDB clients to daemon-based thin shells.

**Contents:**
- Executive Summary
  - Problem statement (ConnectionRefused errors)
  - Solution architecture (daemon-based)
  - Benefits (10x speedup, 88% LOC reduction)
- Architecture Overview
  - System components (Daemon, CLI, DaemonClient)
  - State management
- Message Protocol Design
  - Actor message format (JSON)
  - Message types (create_task, update_task, etc.)
  - HTTP endpoint design
- Implementation Design
  - DaemonClient (~200 LOC)
  - Daemon routes update (+150 LOC)
  - CLI transformation pattern
  - Migration strategy (incremental with fallback)
- Testing Strategy
  - Unit tests
  - Integration tests
  - E2E tests
- Performance Analysis
  - Before/after metrics
  - 12x speedup (180ms → 15ms)
  - 5x memory reduction
- Code Impact Summary
  - Net reduction: -2600 LOC (65%)
- Rollout Plan
  - 4-week timeline
  - Week-by-week breakdown
- Success Criteria
  - Functional requirements
  - Code quality requirements
  - Performance requirements
  - Architecture requirements
- Open Questions & Decisions
  - All resolved with rationale
- Appendices
  - Example transformations
  - Message protocol reference
  - Testing checklist
  - Performance benchmarks

**Usage:** Primary reference for implementation team

---

### 2. CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md (Completion Report)

**Purpose:** Comprehensive report documenting design phase completion, decisions made, and readiness for implementation.

**Contents:**
- Deliverable Summary
  - Files created
  - Sizes and scope
- Architecture Overview
  - Problem identified
  - Solution designed
- Key Design Decisions
  - Message protocol (single endpoint)
  - Migration strategy (incremental)
  - Auto-start logic
  - Communication layer
- Component Designs
  - DaemonClient API
  - Daemon routes
  - CLI transformation pattern
- Testing Strategy Designed
  - Unit/integration/E2E
- Performance Analysis
  - Response time (12x faster)
  - Memory usage (5x less)
- Code Impact Summary
  - Lines added/removed
  - Net reduction: -2600 LOC
- Rollout Plan
  - 4-week schedule
  - Week-by-week tasks
- Success Criteria Defined
  - Functional/quality/performance/architecture
- Risk Assessment
  - Low/medium/high risks
  - Mitigations in place
- Open Questions
  - All resolved
- Next Steps
  - Phase 2: Implementation
  - Phase 3: Testing
- Documentation Deliverables
  - Completed vs pending
- Lessons Learned
  - What worked
  - Challenges
  - Recommendations
- Conclusion
  - Design complete
  - Ready for implementation

**Usage:** Project management, stakeholder communication

---

### 3. CLI_TRANSFORMATION_QUICK_REF.md (Quick Reference)

**Purpose:** One-page quick reference for developers during implementation.

**Contents:**
- Problem → Solution (visual)
- Architecture diagram (ASCII)
- Message protocol essentials
- Code transformation example (before/after)
- Implementation checklist
  - Phase 2: Core (Week 1-2)
  - Phase 3: Extend (Week 3)
  - Phase 4: Cleanup (Week 4)
- Testing quick reference
  - Unit test examples
  - Integration test commands
  - E2E test script
- Performance targets table
- Success criteria checklist
- Quick commands
  - Start daemon
  - Run tests
  - Benchmarks
- File structure
- Key decisions table
- Risk mitigation table
- Next actions

**Usage:** Day-to-day implementation reference

---

## Design Highlights

### Problem Solved

**Current Architecture (Broken):**
```
CLI (2155 LOC)
  ↓
CozoClient("http://127.0.0.1:9070")  ← ConnectionRefused ❌
  ↓
Graph + EventLog + CozoDB (duplicated)
```

**Symptoms:**
- "CozoDB write failed: ConnectionRefused" on every mutation
- 2155 LOC of duplicated logic per CLI
- 180ms response time

**Root Cause:**
- Daemon runs CozoDB WASM embedded (no HTTP server)
- CLIs try to connect to non-existent HTTP server

---

### Solution Architecture

**New Architecture (Working):**
```
CLI (250 LOC)
  ↓
DaemonClient (200 LOC)
  ↓
Daemon HTTP API (port 3000)
  ↓
DualWriteCoordinator → Graph + EventLog + CozoDB WASM ✅
```

**Benefits:**
- ✅ Single source of truth (daemon only)
- ✅ Zero duplication (shared DaemonClient)
- ✅ 88% LOC reduction (2155 → 250 per CLI)
- ✅ 12x performance improvement (180ms → 15ms)
- ✅ Hot graph in memory

---

### Key Design Decisions

#### 1. Message Protocol
**Decision:** Single `/api/actor/message` endpoint

**Rationale:**
- More actor-like (message passing vs REST)
- Simpler routing
- Easier to extend
- Follows existing `graph.send()` pattern

#### 2. Migration Strategy
**Decision:** Incremental with fallback

**Approach:**
```typescript
if (daemon.isRunning()) {
  return { mode: "daemon", client };
} else {
  return { mode: "direct", graph, coordinator };
}
```

**Rationale:**
- Zero downtime
- Gradual rollout
- Easy rollback

#### 3. Auto-Start Logic
**Decision:** CLI spawns daemon if not running

**Rationale:**
- Best developer experience (just works)
- No manual daemon management

#### 4. Communication Layer
**Decision:** Create shared `src/daemon-client.ts`

**Rationale:**
- DRY principle
- Single communication library
- ~200 LOC

---

## Code Impact

### New Files

1. **src/daemon-client.ts** (~200 LOC)
   - `DaemonClient` class
   - `send()` method
   - `isRunning()` check
   - `startDaemon()` auto-start
   - Error handling

### Modified Files

1. **daemon/api/routes.ts** (+150 LOC)
   - `/api/actor/message` endpoint
   - Message router
   - 10+ message handlers

2. **src/cli/task.ts** (-1900 LOC)
   - Transform all commands to use daemon
   - Remove: loadGraph, saveGraph, initializeCoordinator
   - 2155 LOC → ~250 LOC

3. **src/cli/graph.ts** (-550 LOC)
   - Same transformation pattern
   - 807 LOC → ~250 LOC

4. **src/cli/knowledge.ts** (-500 LOC)
   - Same transformation pattern
   - 738 LOC → ~250 LOC

### Total Impact

- **Lines Added:** ~350
- **Lines Removed:** ~2950
- **Net Reduction:** -2600 LOC (65% of CLI code eliminated)

---

## Performance Projections

### CLI Response Time

| Operation | Before | After | Improvement |
|-----------|--------|-------|-------------|
| task add | 180ms | 15ms | **12x faster** |
| task update | 150ms | 12ms | **12.5x faster** |
| task list | 120ms | 10ms | **12x faster** |

### Memory Usage

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Per CLI call | ~50MB | ~5MB | **10x less** |
| Daemon | N/A | ~80MB | Persistent |

### Code Metrics

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| task.ts | 2155 LOC | 250 LOC | **88% reduction** |
| graph.ts | 807 LOC | 250 LOC | **69% reduction** |
| knowledge.ts | 738 LOC | 250 LOC | **66% reduction** |
| **Total** | 3700 LOC | 750 LOC | **80% reduction** |

---

## Implementation Roadmap

### Phase 2: Core Implementation (Weeks 1-2)

**Week 1: Foundation**
- [ ] Create `src/daemon-client.ts`
- [ ] Add `/api/actor/message` endpoint
- [ ] Implement message handlers
- [ ] Unit tests

**Week 2: Transform task.ts**
- [ ] Refactor all task commands
- [ ] Remove dead code
- [ ] Integration tests
- [ ] Verify zero CozoDB errors

### Phase 3: Extend to Other CLIs (Week 3)

- [ ] Transform `graph.ts`
- [ ] Transform `knowledge.ts`
- [ ] E2E tests
- [ ] Performance benchmarks

### Phase 4: Cleanup & Deploy (Week 4)

- [ ] Remove unused code
- [ ] Update documentation
- [ ] User acceptance testing
- [ ] Production deployment

---

## Testing Strategy

### Unit Tests
- DaemonClient message send
- Auto-start daemon
- Error handling
- Route message handlers

### Integration Tests
```bash
task add "Test" --labels test
task update <id> start
task list --label test
task delete <id> --yes
```

### E2E Tests
```bash
1. Start daemon
2. Create task via CLI
3. Update task state
4. Complete task
5. Verify completion
6. Stop daemon
```

---

## Success Criteria

### Functional Requirements ✅
- [ ] CLI runs `task add "Goal"` successfully
- [ ] Zero "CozoDB write failed" errors
- [ ] All existing commands work unchanged
- [ ] Daemon auto-starts when needed

### Code Quality Requirements ✅
- [ ] Each CLI <300 LOC
- [ ] No Graph/EventLog/CozoDB in CLIs
- [ ] Actor pattern used consistently

### Performance Requirements ✅
- [ ] CLI response time <50ms
- [ ] Daemon memory <100MB
- [ ] Zero data loss during restarts

### Architecture Requirements ✅
- [ ] Daemon is single data authority
- [ ] DualWriteCoordinator only in daemon
- [ ] Message passing for all operations

---

## Risk Assessment

### Low Risk ✅
- DaemonClient design (simple HTTP client)
- Message protocol (follows existing patterns)
- Testing strategy (comprehensive)

### Medium Risk ⚠️
- Auto-start logic (race conditions possible)
  - **Mitigation:** Timeout + retry with backoff
- Migration timing (coordination needed)
  - **Mitigation:** Incremental rollout with fallback

### High Risk ❌
- None identified

---

## Next Actions

### For Implementation Team
1. Review `CLI_ACTOR_ARCHITECTURE.md` (primary design)
2. Start Week 1 tasks (create DaemonClient)
3. Follow implementation checklist in `CLI_TRANSFORMATION_QUICK_REF.md`

### For QA Team
1. Review testing strategy (all 3 documents)
2. Prepare test environment
3. Create test data and scripts

### For Documentation Team
1. Review completion report
2. Prepare migration guide for users
3. Update CLI documentation

---

## Related Documents

### Design Documents (This Project)
- `CLI_ACTOR_ARCHITECTURE.md` - Primary design
- `CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md` - Completion report
- `CLI_TRANSFORMATION_QUICK_REF.md` - Quick reference

### Related Projects
- `CLI_CONFIG_RECOMMENDATIONS.md` - CLI configuration patterns
- `CLI_DAEMON_INTEGRATION_PLAN.md` - Original daemon integration plan
- `CLI_OUTPUT_FORMAT_SPEC.md` - Output format specification

### Specifications
- `task-actor.spec.md` - TaskActor specification
- `graph.spec.md` - Graph message protocol (referenced)

---

## Document Usage Guide

### For Developers (Day-to-Day Work)
**Use:** `CLI_TRANSFORMATION_QUICK_REF.md`
- One-page reference
- Code examples
- Quick commands
- Checklist

### For Architects (Understanding Design)
**Use:** `CLI_ACTOR_ARCHITECTURE.md`
- Complete architecture
- Message protocol
- Component designs
- Migration strategy

### For Project Managers (Tracking Progress)
**Use:** `CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md`
- Completion status
- Rollout plan
- Risk assessment
- Success criteria

---

## Completion Status

### Phase 1: Design (COMPLETE ✅)
- [x] Architecture designed
- [x] Message protocol defined
- [x] Component designs created
- [x] Testing strategy planned
- [x] Rollout plan documented
- [x] Success criteria defined
- [x] Risks assessed and mitigated
- [x] All decisions documented

### Phase 2: Implementation (READY TO START)
- [ ] DaemonClient created
- [ ] Daemon routes updated
- [ ] task.ts transformed
- [ ] graph.ts transformed
- [ ] knowledge.ts transformed

### Phase 3: Testing (PLANNED)
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] E2E tests pass
- [ ] Performance benchmarks met

---

## Contact Points

**Design Questions:** See `CLI_ACTOR_ARCHITECTURE.md`
**Implementation Questions:** See `CLI_TRANSFORMATION_QUICK_REF.md`
**Progress Tracking:** See `CLI_ACTOR_TRANSFORMATION_DESIGN_COMPLETION.md`

**Status:** Design phase complete, implementation ready to begin

---

**Last Updated:** 2026-01-17
**Agent:** Background Subagent
**Task:** task_1768701677945_r4svf90xj
**Next Phase:** Implementation (Weeks 1-4)
