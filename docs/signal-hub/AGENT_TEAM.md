# Signal Hub Agent Team Structure

**Generated:** 2026-02-17
**Project:** Signal Hub Production Readiness
**Duration:** 2-3 weeks
**Team Size:** 3 agents + 1 coordinator

---

## Table of Contents

1. [Team Overview](#team-overview)
2. [Agent Roles & Responsibilities](#agent-roles--responsibilities)
3. [Workstream Assignments](#workstream-assignments)
4. [Communication Protocol](#communication-protocol)
5. [Handoff Procedures](#handoff-procedures)
6. [Escalation Paths](#escalation-paths)
7. [Success Metrics](#success-metrics)

---

## Team Overview

### Team Structure

```
┌────────────────────────────────────────────────────────┐
│                   Team Lead (Human)                    │
│             Coordination & Final Review                │
└────────┬───────────────────────────────────┬───────────┘
         │                                   │
         ▼                                   ▼
┌─────────────────┐                 ┌─────────────────┐
│ Backend Agent   │◄───────────────►│  Test Agent     │
│ (Primary)       │                 │  (Primary)      │
│                 │                 │                 │
│ - P0/P1 Fixes   │                 │ - Test Fixes    │
│ - Server Code   │                 │ - Test Coverage │
└────────┬────────┘                 └─────────────────┘
         │
         ▼
┌─────────────────┐
│ Client Agent    │
│ (Secondary)     │
│                 │
│ - P2 DX Items   │
│ - Client APIs   │
└─────────────────┘
```

### Team Composition

| Role | Count | Primary Focus | Phase Involvement |
|------|-------|--------------|-------------------|
| Backend Agent | 1 | Server-side fixes, handlers, DO | Phase 1, 2, 4 |
| Test Agent | 1 | Test infrastructure, coverage | Phase 3 (parallel) |
| Client Agent | 1 | Client libraries, DX | Phase 4 (optional) |
| Team Lead | 1 | Coordination, reviews, decisions | All phases |

---

## Agent Roles & Responsibilities

### Backend Agent (Primary Executor)

**Agent Type:** Backend/Systems Agent
**Expertise:** TypeScript, Cloudflare Workers, Durable Objects, WebSocket
**Primary Workload:** 44 hours (57% of project)

**Responsibilities:**
- Implement all P0 critical fixes (Phase 1)
- Implement all P1 quality fixes (Phase 2)
- Fix server-side edge cases identified in Opus review
- Maintain handler separation and type safety
- Ensure Cloudflare runtime constraints respected
- Optimize performance (Phase 4)

**Key Deliverables:**
- Duplicate connection detection (Task 1.1.1)
- TTL alarm scheduling (Task 1.2.1)
- Message size validation (Task 1.2.2)
- Broadcast limit enforcement (Task 1.2.3)
- Rate limiting implementation (Task 2.1.1)
- Unsubscribe fix (Task 2.1.2)
- Structured logging (Task 2.2.2)
- Performance optimizations (Phase 4)

**Working Hours Pattern:**
- Week 1: Full-time on Phase 1 (P0)
- Week 2: Full-time on Phase 2 (P1)
- Week 3: Part-time on Phase 4 (P2, optional)

**Communication Style:**
- Daily progress updates to Team Lead
- Real-time coordination with Test Agent for test fixes
- Code review requests after each task
- Escalate blocking issues immediately

---

### Test Agent (Parallel Executor)

**Agent Type:** QA/Testing Agent
**Expertise:** Integration testing, Miniflare, test frameworks, debugging
**Primary Workload:** 12 hours (16% of project)

**Responsibilities:**
- Fix all 13 failing tests (Phase 3.1)
- Investigate root causes of broadcast/pub/sub timeouts
- Add unit tests for handlers (Phase 3.2)
- Create reconnection scenario tests
- Maintain test infrastructure
- Verify P0/P1 fixes don't break existing tests

**Key Deliverables:**
- Module import fix (Task 3.1.1)
- Broadcast timeout resolution (Task 3.1.2)
- Pub/sub timeout resolution (Task 3.1.3)
- Ordering test fix (Task 3.1.4)
- Handler unit tests (Task 3.2.1)
- Reconnection tests (Task 3.2.2)

**Working Hours Pattern:**
- Week 1: Full-time on Phase 3.1 (test framework fixes)
- Week 2: Part-time on Phase 3.2 (test coverage expansion)
- Week 2-3: On-call for verifying Backend Agent fixes

**Communication Style:**
- Daily test run reports to Team Lead
- Immediate notification of test failures
- Root cause analysis for each failing test
- Coordinate with Backend Agent on fix validation

---

### Client Agent (Secondary, Phase 4 Only)

**Agent Type:** Frontend/Client Agent
**Expertise:** Client libraries, API design, developer experience
**Primary Workload:** 5 hours (6% of project)

**Responsibilities:**
- Implement client convenience APIs (sendMessage)
- Improve error message clarity
- Update client documentation
- Enhance developer experience (Phase 4 only)

**Key Deliverables:**
- Enhanced error messages (Task 4.2.1)
- Client sendMessage() API (Task 4.2.2)
- Client library documentation updates

**Working Hours Pattern:**
- Week 1-2: Idle (Phase 1-2 backend-focused)
- Week 3: Part-time on Phase 4 DX items (if approved)

**Communication Style:**
- Weekly check-ins with Team Lead
- Final week coordination with Backend Agent
- API design review requests

---

### Team Lead (Human Coordinator)

**Role:** Project Manager + Technical Lead
**Responsibilities:**
- Daily standup coordination
- Phase gate approvals
- Code review and merge
- Unblock agents when stuck
- Make architecture decisions
- Final quality assessment

**Key Decisions:**
- Phase 1 → Phase 2 transition approval
- Broadcast async queue: simple rejection vs Cloudflare Queues
- Phase 4 scope approval (optional work)
- Production deployment go/no-go

**Communication Style:**
- Daily standup (15 min)
- Review WIP at phase boundaries
- Available for urgent escalations

---

## Workstream Assignments

### Phase 1: P0 Critical Fixes (Week 1)

| Workstream | Agent | Duration | Parallel? |
|-----------|-------|----------|-----------|
| WS1.1: Connection Lifecycle | Backend | 8h | Yes |
| WS1.2: Resource Protection | Backend | 10h | Yes |
| WS1.3: Performance & Observability | Backend | 3h | Yes |

**Note:** All Phase 1 workstreams can run in parallel (no dependencies between them).

---

### Phase 2: P1 Quality Fixes (Week 1-2)

| Workstream | Agent | Duration | Dependencies |
|-----------|-------|----------|--------------|
| WS2.1: Security & Correctness | Backend | 9h | Phase 1 complete |
| WS2.2: Reliability & Operations | Backend | 7h | Phase 1 complete |

**Note:** WS2.1 and WS2.2 can run in parallel after Phase 1 completes.

---

### Phase 3: Test Infrastructure (Week 1, Parallel)

| Workstream | Agent | Duration | Dependencies |
|-----------|-------|----------|--------------|
| WS3.1: Test Framework Fixes | Test | 8h | None |
| WS3.2: Test Coverage Expansion | Test | 4h | WS3.1 complete |

**Note:** Phase 3 runs concurrently with Phase 1. Test Agent starts immediately.

---

### Phase 4: P2 DX Improvements (Week 2-3, Optional)

| Workstream | Agent | Duration | Dependencies |
|-----------|-------|----------|--------------|
| WS4.1: Performance Optimizations | Backend | 7h | Phase 1 & 2 complete |
| WS4.2: Developer Experience | Client | 5h | Phase 1 & 2 complete |
| WS4.3: Advanced Features | Backend | 4h | Phase 1 & 2 complete |

**Note:** Phase 4 requires Team Lead approval before starting.

---

## Communication Protocol

### Daily Standup (15 minutes, async-first)

**Time:** Start of each workday (or async via shared doc)
**Participants:** All active agents + Team Lead
**Format:**

```
Agent: [Name]
Yesterday: [Completed tasks]
Today: [Planned tasks]
Blockers: [Issues needing help]
```

**Example:**
```
Agent: Backend Agent
Yesterday: Completed Task 1.1.1 (duplicate connection detection), tests passing
Today: Starting Task 1.2.1 (TTL alarm scheduling)
Blockers: None

Agent: Test Agent
Yesterday: Fixed module import (Task 3.1.1), all files load
Today: Investigating broadcast timeout root cause (Task 3.1.2)
Blockers: Need access to production-like test environment for latency testing
```

---

### Code Review Protocol

**Trigger:** Task completion
**Reviewer:** Team Lead (or delegated to another agent for peer review)
**Turnaround:** <4 hours during working hours

**Review Checklist:**
- [ ] Code follows TypeScript best practices
- [ ] Tests added for new functionality
- [ ] No breaking changes to existing API
- [ ] Performance impact acceptable
- [ ] Documentation updated
- [ ] Opus review item addressed fully

**Review Response:**
- **Approved:** Merge and move to next task
- **Changes Requested:** Agent revises, re-requests review
- **Blocked:** Escalate to Team Lead for decision

---

### Progress Tracking

**Tool:** Beads system (`bd` CLI)
**Update Frequency:** After each task completion

**Bead States:**
- 🔵 **Pending** - Not started
- 🟡 **In Progress** - Agent actively working
- 🟢 **Complete** - Tested and reviewed
- 🔴 **Blocked** - Needs escalation

**Agent Responsibilities:**
- Update bead status when starting work: `bd start <bead-id>`
- Mark complete when done: `bd done <bead-id>`
- Add notes for blockers: `bd note <bead-id> "Blocked on X"`

---

### Cross-Agent Coordination

**Scenario 1: Test Agent finds bug in Backend Agent's fix**
1. Test Agent documents bug in test results
2. Test Agent messages Backend Agent directly (don't wait for standup)
3. Backend Agent investigates, provides timeline for fix
4. If urgent, escalate to Team Lead

**Scenario 2: Backend Agent needs test validation**
1. Backend Agent completes task, marks bead as "ready for test"
2. Test Agent runs integration tests
3. Test Agent reports: PASS or FAIL with details
4. If FAIL, loop back to Backend Agent

**Scenario 3: Overlapping work (e.g., structured logging affects tests)**
1. Agent with upcoming dependency monitors other's progress
2. Early coordination: "I see you're working on X, which affects my Y. Can we sync?"
3. Quick sync call (10 min) to align on interface/expectations
4. Document agreed interface in shared doc

---

## Handoff Procedures

### Backend Agent → Test Agent (Phase 1 → Phase 3.2)

**Trigger:** P0 fixes complete, ready for validation
**Handoff Artifact:** Pull request with:
- [ ] All P0 code changes
- [ ] Updated type definitions
- [ ] Migration notes (if any)
- [ ] Known issues list

**Test Agent Acceptance Criteria:**
- [ ] Code compiles without errors
- [ ] Existing tests still pass (no regressions)
- [ ] New functionality testable
- [ ] Documentation sufficient to write tests

**Process:**
1. Backend Agent creates PR with tag `ready-for-test`
2. Test Agent reviews PR, runs test suite
3. Test Agent reports results within 4 hours
4. If issues found, loop back to Backend Agent

---

### Test Agent → Backend Agent (Test failure root cause)

**Trigger:** Test Agent identifies code bug (not test bug)
**Handoff Artifact:** Bug report with:
- [ ] Failing test name
- [ ] Expected vs actual behavior
- [ ] Hypothesis on root cause
- [ ] Relevant code pointers

**Backend Agent Acceptance Criteria:**
- [ ] Bug reproducible
- [ ] Root cause clear or hypothesis testable
- [ ] Priority assigned (P0/P1/P2)

**Process:**
1. Test Agent creates issue in shared tracker
2. Test Agent pings Backend Agent
3. Backend Agent acknowledges, provides ETA for fix
4. Backend Agent fixes, Test Agent re-validates

---

### Phase 1 → Phase 2 Handoff (Gate Approval)

**Trigger:** All Phase 1 tasks complete
**Gate Keeper:** Team Lead
**Criteria:**
- [ ] All P0 items implemented
- [ ] All P0 tests passing
- [ ] No known regressions
- [ ] Code reviewed and merged

**Process:**
1. Backend Agent reports: "Phase 1 complete, ready for Phase 2"
2. Test Agent confirms: "All tests passing"
3. Team Lead reviews deliverables
4. Team Lead decision: APPROVE (proceed) or REJECT (fix issues)
5. If approved, Backend Agent starts Phase 2 work

---

### Phase 2 → Phase 4 Handoff (Optional Work Decision)

**Trigger:** Phase 1 & 2 complete, all tests passing
**Decision Maker:** Team Lead
**Criteria for Approval:**
- [ ] Production readiness achieved (P0 + P1 done)
- [ ] Budget/time available for P2 work
- [ ] P2 items prioritized (which to do first)

**Process:**
1. Team Lead reviews overall project status
2. Team Lead decides: GO (Phase 4 approved) or NO-GO (ship as-is)
3. If GO, Client Agent joins project
4. Team Lead assigns specific P2 tasks to agents

---

## Escalation Paths

### Level 1: Agent-to-Agent (Self-Service)

**When:** Minor questions, coordination needs
**Response Time:** Within 2 hours during working hours
**Examples:**
- "What format should this log message use?"
- "Can you review this 10-line change?"
- "When will Task X be ready for testing?"

**Process:**
- Direct message between agents
- Document outcome in shared notes
- No formal escalation

---

### Level 2: Agent-to-Team Lead (Blocking Issue)

**When:** Cannot proceed without decision or unblock
**Response Time:** Within 4 hours
**Examples:**
- "Test requires production API key, don't have access"
- "Two design approaches, need architecture decision"
- "Task estimate was 2h, actually needs 8h"

**Process:**
1. Agent documents blocker clearly
2. Agent messages Team Lead with: BLOCKER, CONTEXT, OPTIONS
3. Team Lead provides decision or unblocks
4. Document decision for future reference

---

### Level 3: Team Lead Escalation (Critical Issue)

**When:** Project timeline or scope at risk
**Response Time:** Immediate
**Examples:**
- "Opus review missed critical issue, need scope change"
- "Phase 1 will take 2x estimated time"
- "Test failures indicate fundamental design flaw"

**Process:**
1. Team Lead assesses impact
2. Team Lead may: re-prioritize, descope, extend timeline
3. Team Lead communicates decision to all agents
4. Project plan updated

---

## Success Metrics

### Agent Performance Metrics

**Backend Agent:**
- Tasks completed on time: >80%
- Code review cycles: <2 per task (avg)
- Test pass rate on first submission: >70%
- Regressions introduced: 0

**Test Agent:**
- Test failures resolved: 13/13 (100%)
- Test coverage increase: >20% (from baseline)
- False positive rate: <5%
- Test flakiness: 0 (3 consecutive clean runs)

**Client Agent:**
- API design approval: first round
- Documentation completeness: 100%
- User-facing error clarity: >90% (subjective, reviewed by Team Lead)

---

### Team Coordination Metrics

- Daily standup participation: 100%
- Handoff turnaround: <4 hours (avg)
- Escalation resolution time: <1 day
- Cross-agent communication: proactive (no surprises at standup)

---

### Phase Gate Success Criteria

**Phase 1 Gate (P0 Complete):**
- [ ] All 6 P0 items implemented
- [ ] Zero test failures in core test suite
- [ ] Code review approved for all tasks
- [ ] No known critical bugs

**Phase 2 Gate (P1 Complete):**
- [ ] All 5 P1 items implemented
- [ ] Rate limiting functional
- [ ] Structured logging operational
- [ ] Pub/sub correctness verified

**Phase 3 Gate (Tests Fixed):**
- [ ] 0 failing tests (was 13)
- [ ] Test suite runtime <30s
- [ ] No flaky tests (3 clean runs)
- [ ] Code coverage >80% for handlers

**Project Complete (Production Ready):**
- [ ] Phase 1, 2, 3 complete
- [ ] Documentation updated
- [ ] Deployment runbook verified
- [ ] Team Lead approval for production deployment

---

## Coordination Anti-Patterns (Avoid These)

### ❌ Silent Blockers
**Problem:** Agent stuck for days, doesn't escalate
**Solution:** Escalate after 4 hours of being stuck

### ❌ Surprise Scope Changes
**Problem:** Agent changes task scope without informing team
**Solution:** Discuss scope changes at standup, get approval

### ❌ Merge Without Review
**Problem:** Agent merges code without Team Lead approval
**Solution:** All merges require code review

### ❌ Test-After Development
**Problem:** Backend Agent completes all work, THEN tests
**Solution:** Test each task immediately after completion

### ❌ Overlapping Work
**Problem:** Two agents edit same file simultaneously
**Solution:** Declare intent at standup, coordinate

---

## Agent Autonomy Guidelines

### ✅ Agents CAN Do Autonomously:
- Implement tasks as specified in WBS
- Write tests for their own code
- Refactor code within scope of task
- Add logging/debugging statements
- Fix obvious bugs found during work
- Ask clarifying questions to other agents

### ⚠️ Agents SHOULD COORDINATE:
- API changes affecting other agents
- Performance optimizations with tradeoffs
- Test approach for complex scenarios
- Logging format/structure (Team Lead decides)

### ❌ Agents MUST ESCALATE:
- Scope changes (task needs more work than estimated)
- Architecture decisions (multiple valid approaches)
- Breaking changes to existing API
- Timeline impacts (task will be late)
- Disagreements with other agents

---

## Daily Workflow Example

### Backend Agent Day 1 (Phase 1)

**Morning:**
- 09:00 - Daily standup (report yesterday's Task 1.1.1 complete)
- 09:15 - Start Task 1.2.1 (TTL alarm scheduling)
- 09:30 - Read Cloudflare Durable Objects alarm docs
- 10:00 - Implement alarm scheduling in constructor
- 11:00 - Write tests for alarm behavior

**Afternoon:**
- 13:00 - Task 1.2.1 complete, mark bead done
- 13:15 - Create PR, request Team Lead review
- 14:00 - Review complete, merge approved
- 14:15 - Start Task 1.2.2 (message size check)
- 16:00 - Task 1.2.2 50% complete, update bead notes
- 17:00 - End of day, prepare standup update

**Communication:**
- Standup: Updated status
- PR review: Responded to comments
- Test Agent: Answered question about new types

---

### Test Agent Day 1 (Phase 3)

**Morning:**
- 09:00 - Daily standup (report Task 3.1.1 complete)
- 09:15 - Start Task 3.1.2 (broadcast timeout investigation)
- 09:30 - Add debug logging to broadcast handler
- 10:30 - Run tests with debug enabled
- 11:30 - Hypothesis: actor address mismatch (@(local/...) vs @(seag/...))

**Afternoon:**
- 13:00 - Confirm hypothesis with address comparison test
- 14:00 - Fix address normalization in test helpers
- 15:00 - All 5 broadcast tests now passing!
- 15:30 - Document root cause in test file comments
- 16:00 - Update bead to complete, notify Backend Agent
- 16:15 - Start Task 3.1.3 (pub/sub timeouts)

**Communication:**
- Standup: Reported progress
- Backend Agent: Shared finding about address normalization
- Team Lead: Confirmed 5/13 tests now passing

---

## Appendix: Agent Personality Profiles

### Backend Agent Traits
- **Strengths:** Systematic, thorough, performance-conscious
- **Working Style:** Deep focus, long blocks of coding time
- **Communication:** Detailed, includes code examples
- **Best Practices:** Writes tests first, refactors often

### Test Agent Traits
- **Strengths:** Debugging, root cause analysis, edge case thinking
- **Working Style:** Iterative, hypothesis-driven
- **Communication:** Concise, data-driven (logs/metrics)
- **Best Practices:** Documents failures, reproducible test cases

### Client Agent Traits
- **Strengths:** User empathy, API design, documentation
- **Working Style:** Prototype-driven, iterate on feedback
- **Communication:** User-centric, focuses on DX
- **Best Practices:** Example-driven docs, consistency checks

---

**Document Owner:** Team Lead
**Review Frequency:** Weekly during project
**Last Updated:** 2026-02-17
