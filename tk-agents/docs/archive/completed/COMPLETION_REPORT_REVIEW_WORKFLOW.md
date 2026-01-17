# COMPLETION REPORT: Review Workflow & Activity Tracking Design

**Agent ID:** Background Subagent (Review Workflow Design)
**Started:** 2026-01-16 19:16 EST
**Completed:** 2026-01-16 19:30 EST (estimated)
**Duration:** ~60 minutes
**Status:** ✅ SUCCESS

---

## Executive Summary

Designed comprehensive solution to your "firestorm of tokens" problem: a review workflow system and activity dashboard that gives you clear visibility into parallel agent work and tells you exactly "what needs my attention NOW."

### Your Questions Answered

**Q1: "Are you assigning review tasks to me?"**

**A:** Not yet, but the system is designed. Going forward:
- Agents will create review tasks BEFORE announcing completion
- Review tasks assigned to you (bln)
- Prioritized by blocking status (P0 = immediate, P1 = today, etc.)
- You'll see them in `task list --label review` or `primer reviews`

**Q2: "How can I track all this activity?"**

**A:** New `primer dashboard` command will show:
- Pending reviews (prioritized)
- Active agents (what's running)
- Completed work (today/week)
- What's blocked on you
- Statistics and "next action"

---

## Deliverables Created

### 1. REVIEW_WORKFLOW_DESIGN.md (16KB)

**Purpose:** Complete specification of review workflow pattern

**Key Features:**
- Review task creation pattern for agents
- Priority triage system (P0-P3)
- Review lifecycle (created → approved/rejected → completed)
- Blocking vs non-blocking reviews
- Integration with Task CLI
- CLAUDE.md instructions for agents

**Example Workflow:**
```bash
# Agent completes → creates review task
task add "Review: Graph query research" \
  --parent task_agent_a8f57b5 \
  --labels review,research \
  --priority P1 \
  --deliverables "GRAPH_QUERY_RESEARCH.md" \
  --assignee bln

# You see: "P1 Review: Graph query research (4 hours ago)"
# You review → approve/reject
task update task_review_21 complete "Approved"
```

**Priority Guide:**
- **P0:** Immediate (< 1 hour) - blocks other work
- **P1:** Same day (< 8 hours) - high priority
- **P2:** Within 2 days - normal
- **P3:** When convenient - low priority

### 2. ACTIVITY_DASHBOARD_DESIGN.md (18KB)

**Purpose:** Complete specification of dashboard system

**Key Features:**
- Unified `primer` CLI with sub-program delegation
- Dashboard components:
  - Needs Attention (prioritized reviews)
  - Active Agents (what's running)
  - Completed Today/Week (recent work)
  - Blocked on You (waiting on reviews)
  - Statistics (counts, metrics)
  - Next Action (recommended priority)

**Example Dashboard:**
```
Primer Dashboard - 2026-01-16 19:30 EST

╔══════════════════════════════════════════╗
║ NEEDS YOUR ATTENTION (3)                 ║
╚══════════════════════════════════════════╝
  🔴 P0 Review: Task CLI optimizations
     → Deliverables: src/cli/task.ts, docs
     → Blocking: 2 tasks
     → Created: 2 hours ago

  🟡 P1 Review: Graph query research
     → Deliverables: GRAPH_QUERY_RESEARCH.md
     → Not blocking
     → Created: 4 hours ago

╔══════════════════════════════════════════╗
║ ACTIVE AGENTS (3)                        ║
╚══════════════════════════════════════════╝
  🔄 task_agent_a9e9c7b: Project reflection
     → Running: 90 minutes

  ... (more sections)

Next Action: Review Task CLI optimizations (P0, blocking 2 tasks)
```

**Commands:**
```bash
primer dashboard      # Full view
primer reviews        # Pending reviews only
primer attention      # P0 reviews only
primer agents         # Active agents
primer completed      # Recent completions
primer next           # Next recommended action
```

### 3. IMPLEMENTATION_PLAN.md (25KB)

**Purpose:** Phased rollout plan with detailed implementation

**3 Phases:**

**Phase 1: Foundation (Week 1) - IMMEDIATE**
- Update CLAUDE.md with review workflow instructions
- Migrate current work to review tasks (7+ deliverables)
- Manual workflow (no code changes)
- Success: Agents consistently create review tasks

**Phase 2: Dashboard CLI (Week 2-3)**
- Build `primer` wrapper CLI
- Implement `dashboard.ts` with all views
- Integration testing
- Success: Daily dashboard usage, < 1 sec load time

**Phase 3: Refinement (Week 4+)**
- Hook automation (optional, if manual is burdensome)
- Advanced features (filtering, analytics)
- Continuous improvement

**Includes:**
- Complete TypeScript code for primer CLI and dashboard
- Integration test suite
- Documentation and guides
- Migration scripts for current work

---

## Solution Overview

### The Problem (Your Pain Points)

1. **Information Overload**
   - 4+ parallel agents
   - 22+ markdown files created
   - No way to know "what needs attention NOW"
   - Drowning in announcements

2. **No Review Tracking**
   - Agents complete → announce → forgotten
   - No formal handoff or review tasks
   - Can't track what's been reviewed

3. **Lack of Visibility**
   - Can't see active agents
   - Can't see pending reviews
   - Can't see what's blocking
   - No dashboard or overview

### The Solution (3-Part System)

#### Part 1: Review Task Pattern

Every agent completion creates a review task:

```bash
task add "Review: <deliverable>" \
  --parent <agent-task> \
  --labels review,<type> \
  --priority <P0-P3> \
  --deliverables "<files>" \
  --assignee bln
```

**Benefits:**
- Formal handoff from agent to you
- Persistent record (survives compaction)
- Prioritized by blocking status
- Tracks approval/rejection

#### Part 2: Priority Triage

Reviews prioritized by impact:

| Priority | Type | Response Time | Blocks Work? |
|----------|------|---------------|--------------|
| P0 | Blocking | < 1 hour | YES |
| P1 | High | < 8 hours | MAYBE |
| P2 | Normal | 2 days | NO |
| P3 | Low | When convenient | NO |

**Result:** You know EXACTLY what to focus on first.

#### Part 3: Activity Dashboard

Single command shows everything:

```bash
primer dashboard
```

**Shows:**
- Reviews needing attention (prioritized)
- Active agents (what's running)
- Recent completions (today/week)
- What's blocked on you
- Statistics
- Next recommended action

**Result:** Clear overview, no more "firestorm."

---

## Immediate Next Steps (Phase 1)

### For You (5 minutes)

**1. Review the designs** (optional, but recommended):
- [REVIEW_WORKFLOW_DESIGN.md](./REVIEW_WORKFLOW_DESIGN.md)
- [ACTIVITY_DASHBOARD_DESIGN.md](./ACTIVITY_DASHBOARD_DESIGN.md)
- [IMPLEMENTATION_PLAN.md](./IMPLEMENTATION_PLAN.md)

**2. Decide on rollout:**

**Option A (Recommended):** Start Phase 1 immediately
- Add CLAUDE.md instructions
- Migrate current work to review tasks
- Use manual workflow for 1 week
- Then build dashboard (Phase 2)

**Option B:** Fast-track to Phase 2
- Skip manual workflow
- Build dashboard immediately
- Backfill review tasks as needed

**Option C:** Iterate on design first
- Provide feedback on designs
- Adjust priority system
- Customize dashboard components
- Then proceed to implementation

### For Me (If You Choose Option A)

I can execute Phase 1 immediately:

1. **Update CLAUDE.md** with review workflow instructions
2. **Create 7 review tasks** for recent deliverables:
   - PROJECT_REFLECTION.md (P2)
   - GRAPH_QUERY_RESEARCH.md (P1)
   - Task CLI implementation (P0)
   - Markdown-as-graph (P2)
   - CLI audit/plans (P1)
   - Actor model docs (P2)
   - Spec extensions (P3)
3. **Test workflow** end-to-end
4. **Create user guide**

**Time:** 2-3 hours

Then you can immediately:
```bash
# See all pending reviews
task list --label review

# Start with highest priority
task list --label review --priority P0

# Review and approve
task show task_3
# (review files)
task update task_3 complete "Approved"
```

---

## Key Design Decisions

### Decision 1: Review Tasks (Not Ad-Hoc Announcements)

**Why:** Tasks persist, can be queried, show dependencies, survive compaction

**Alternative Rejected:** Keep announcements-only (too ephemeral)

### Decision 2: Priority-Based Triage (P0-P3)

**Why:** Clear SLAs, objective prioritization, no ambiguity

**Alternative Rejected:** Simple "urgent/normal/low" (not granular enough)

### Decision 3: Unified Primer CLI

**Why:** Single entry point, familiar pattern (git, cargo), extensible

**Alternative Rejected:** Separate dashboard CLI (too many commands)

### Decision 4: Phased Rollout (Foundation → Dashboard → Refinement)

**Why:** Quick win (Phase 1 no code), learn before automating, low risk

**Alternative Rejected:** Build everything at once (too much upfront, no feedback)

### Decision 5: Manual Workflow First, Hooks Later

**Why:** Understand patterns before automating, lower complexity, easier debug

**Alternative Rejected:** Hook automation immediately (premature, fragile)

---

## Expected Outcomes

### After Phase 1 (Week 1)

**User Experience:**
- Every agent completion → review task created
- You see: `task list --label review` shows all pending reviews
- Prioritized list: P0 first, then P1, P2, P3
- You review deliverables and mark complete
- Downstream work unblocks automatically

**Metrics:**
- 100% agent work creates review tasks
- Zero lost context
- Clear "next action" always available

### After Phase 2 (Week 3)

**User Experience:**
- Morning standup: `primer dashboard`
- See everything at a glance
- Know exactly what needs attention
- Track agent activity in real-time
- Check "next action" recommendation

**Metrics:**
- Dashboard used daily
- Load time < 1 second
- Cognitive overload reduced
- Review SLAs met (P0 < 1hr, P1 < 8hr)

### After Phase 3 (Ongoing)

**User Experience:**
- Fully automated review task creation (hooks)
- Advanced filtering and analytics
- Custom dashboard views
- Zero manual overhead

**Metrics:**
- Zero manual intervention
- High user satisfaction
- All work tracked and reviewed

---

## Files Created

| File | Size | Purpose |
|------|------|---------|
| REVIEW_WORKFLOW_DESIGN.md | 16KB | Review task pattern, priority system, lifecycle |
| ACTIVITY_DASHBOARD_DESIGN.md | 18KB | Dashboard specs, CLI design, components |
| IMPLEMENTATION_PLAN.md | 25KB | Phased rollout, code implementation, testing |

**Total:** 59KB of design documentation

---

## Risks & Mitigations

### Risk 1: Manual Workflow Not Adopted

**Likelihood:** Low (CLAUDE.md instructions very explicit)

**Impact:** High (whole system fails)

**Mitigation:**
- Clear, explicit CLAUDE.md instructions
- Examples in every agent context
- Monitor adoption in Week 1
- If <50%, fast-track hook automation

### Risk 2: Dashboard Too Complex

**Likelihood:** Medium (many components)

**Impact:** Medium (user doesn't use it)

**Mitigation:**
- Start simple (basic views only)
- Add filtering gradually
- Gather feedback weekly
- Allow customization

### Risk 3: Priority System Wrong

**Likelihood:** Low (based on common patterns)

**Impact:** Low (easy to adjust)

**Mitigation:**
- User feedback on priorities
- Adjust P0/P1/P2/P3 definitions
- Add override mechanism

---

## Questions for You

### Question 1: Rollout Decision

Which phase do you want to start with?

**A.** Phase 1 (manual workflow, immediate, no code)
**B.** Phase 2 (build dashboard now, skip manual)
**C.** Iterate on design first (feedback before implementation)

### Question 2: Current In-Flight Work

Should I migrate your recent deliverables to review tasks now?

**Recent Deliverables I Identified:**
1. PROJECT_REFLECTION.md (52KB)
2. GRAPH_QUERY_RESEARCH.md (57KB)
3. Task CLI implementation (src/cli/task.ts, docs)
4. Markdown-as-graph prototype
5. CLI audit + plans
6. Actor model iterations
7. Spec extensions

**If yes:** I'll create 7 review tasks with appropriate priorities.

**If no:** You can do it manually when ready.

### Question 3: Priority Definitions

Do the P0-P3 definitions work for you?

- P0: < 1 hour (blocks other work)
- P1: < 8 hours (high priority)
- P2: 2 days (normal)
- P3: When convenient (low)

**Adjust if needed.**

### Question 4: Dashboard Features

Any must-have features I missed?

Current components:
- Needs Attention
- Active Agents
- Completed Today/Week
- Blocked on You
- Statistics
- Next Action

**Additional ideas?**

---

## Background Agent Protocol: COMPLETION

This is a **COMPLETION** announcement from a background agent.

**Agent Task:** Design review workflow and activity tracking system

**Deliverables:**
- REVIEW_WORKFLOW_DESIGN.md (16KB, review task pattern and priority system)
- ACTIVITY_DASHBOARD_DESIGN.md (18KB, dashboard specifications and CLI design)
- IMPLEMENTATION_PLAN.md (25KB, phased rollout with code and tests)
- COMPLETION_REPORT_REVIEW_WORKFLOW.md (this file, executive summary)

**Review Task:** (Not created yet - waiting for your decision on rollout)

**Status:** ✅ COMPLETE - Ready for review and decision

**Next Action:** Please answer the 4 questions above to proceed with implementation.

---

## Summary

You asked:
1. "Are you assigning review tasks to me?"
2. "How can I track all this activity?"

I designed a complete solution:
1. **Review Workflow** - Every agent creates review task, prioritized, assigned to you
2. **Activity Dashboard** - Single command shows everything (reviews, agents, completed, blocked)
3. **Phased Implementation** - Start simple (manual), add complexity (dashboard), refine (automation)

**Your call:** Phase 1 (manual, immediate), Phase 2 (build dashboard), or iterate on design?

---

**END OF REPORT**
