# Signal Hub Production Readiness Fix Plan

**Generated:** 2026-02-17
**Verified Current Date:** 2026-02-17 (via `date` command)
**Based On:** Opus Review (OPUS_REVIEW.md - 612 lines)
**Status:** Active - Ready for Execution

---

## Executive Summary

### Current State Assessment

Signal Hub is a well-designed real-time messaging system with comprehensive protocol documentation and solid architectural foundations. However, the Opus review identified critical gaps between specification and implementation that must be addressed before production deployment.

**Key Statistics:**
- **Test Failures:** 13 tests failing (broadcast: 5, pub/sub: 5, ordering: 1, lifecycle: 1, module: 1)
- **Missing P0 Features:** 6 critical implementation gaps
- **Missing P1 Features:** 5 important but non-blocking issues
- **Protocol Grade:** A- (excellent design)
- **Implementation Grade:** B- (functional but incomplete)

### Primary Risk Factors

1. **Specification-Implementation Divergence** - Documented MVP features not implemented
2. **Cloudflare Runtime Hazards** - Platform-specific failure modes not fully mitigated
3. **State Management Fragility** - All state is volatile, DO restart loses everything
4. **Test Infrastructure Gaps** - 13 failing tests indicate test harness issues
5. **Production Safety Missing** - No rate limiting, oversized message protection incomplete

### Overall Strategy

**Three-Phase Approach:**
1. **Phase 1: P0 Blockers (Critical Path)** - Must-fix before ANY production use
2. **Phase 2: P1 Should-Fix (Quality)** - Important for reliable operation
3. **Phase 3: P2 Nice-to-Have (DX)** - Developer experience improvements

**Success Criteria:**
- ✅ 100% test pass rate (all 13 failures resolved)
- ✅ All P0 production readiness items implemented
- ✅ Zero known data loss scenarios in happy path
- ✅ Rate limiting and DoS protection active
- ✅ Observable behavior through structured logging

---

## Priority Classification

### P0: Must-Fix Before Production (6 Items)

These are **blocking issues** that create data loss, security risks, or runtime failures:

| # | Issue | Impact | Est. Hours |
|---|-------|--------|------------|
| P0-1 | Duplicate connection detection missing | Stale sessions accumulate, messages to dead connections | 4h |
| P0-2 | Alarm-based TTL cleanup never scheduled | Registry fills with expired actors, hits 50K limit | 2h |
| P0-3 | Raw message size check happens after parse | Large messages exhaust memory before rejection | 2h |
| P0-4 | Disconnect response sent after WebSocket close | Clients never receive disconnect acknowledgment | 2h |
| P0-5 | Verbose debug logging in hot paths | O(N) registry iteration on every message, log spam | 3h |
| P0-6 | Broadcast limit warning-only (no enforcement) | CPU exhaustion with >100 recipients | 4h |

**Total P0 Effort:** ~17 hours

### P1: Should-Fix Before Production (5 Items)

These are **important quality issues** that affect reliability but don't block deployment:

| # | Issue | Impact | Est. Hours |
|---|-------|--------|------------|
| P1-1 | Rate limiting code exists but never called | No DoS protection, single client can exhaust CPU | 4h |
| P1-2 | Unsubscribe removes from ALL topics | Violates principle of least surprise, breaks multi-topic use | 3h |
| P1-3 | Heartbeat interval too tight (25s vs 30s) | 5s margin too small for network jitter | 1h |
| P1-4 | Target address ambiguity (msg.to vs payload.to) | Confusing failure modes when target omitted | 2h |
| P1-5 | Unstructured logging | Not queryable, creates excessive noise | 6h |

**Total P1 Effort:** ~16 hours

### P2: Nice-to-Have (DX & Optimization) (5 Items)

These are **developer experience improvements** that don't affect correctness:

| # | Issue | Impact | Est. Hours |
|---|-------|--------|------------|
| P2-1 | Subscription cleanup is O(topics) | Slow disconnect for actors with many topics | 3h |
| P2-2 | No hub:refresh_token implementation | Long-lived sessions expire, require reconnect | 4h |
| P2-3 | Error messages lack resolution steps | Users don't know how to fix issues | 2h |
| P2-4 | Client sendMessage() convenience API | Reduces flat payload confusion | 3h |
| P2-5 | Broadcast JSON.stringify called per recipient | CPU waste, 100 actors = 100 stringify calls | 4h |

**Total P2 Effort:** ~16 hours

---

## Phase Breakdown with Dependencies

### Phase 1: Critical Fixes (P0) - Week 1

**Duration:** 3-4 days
**Effort:** ~17 hours
**Parallel Tracks:** 3 workstreams

#### Workstream 1.1: Connection Lifecycle Fixes
**Owner:** Backend Agent
**Dependencies:** None
**Deliverables:**
- P0-1: Implement duplicate connection detection
- P0-4: Fix disconnect response ordering
- Test: Reconnection scenarios pass

**Success Criteria:**
- `should disconnect gracefully` test passes
- New test: duplicate connection triggers disconnect with `reason: 'duplicate_connection'`
- Disconnect acknowledgment received by client before WebSocket closes

#### Workstream 1.2: Resource Protection
**Owner:** Backend Agent
**Dependencies:** None
**Deliverables:**
- P0-2: Schedule alarm for TTL cleanup
- P0-3: Add raw message size check before parse
- P0-6: Enforce broadcast limit (reject >100 or implement async queue)

**Success Criteria:**
- Alarm fires every 5 minutes, removes expired actors
- 10MB message rejected before JSON.parse
- Broadcast to 101 actors returns error with actionable message

#### Workstream 1.3: Performance & Observability
**Owner:** Backend Agent
**Dependencies:** None
**Deliverables:**
- P0-5: Remove verbose debug logging from hot paths
- Gate remaining debug logs behind `DEBUG=signal-hub` flag

**Success Criteria:**
- No `Array.from(registry.keys())` calls in production
- Log output reduced by >90% at INFO level
- Debug logs still available when needed

---

### Phase 2: Quality & Reliability (P1) - Week 1-2

**Duration:** 4-5 days
**Effort:** ~16 hours
**Parallel Tracks:** 2 workstreams
**Dependencies:** Phase 1 complete (especially P0-1 for P1-1)

#### Workstream 2.1: Security & Correctness
**Owner:** Backend Agent
**Dependencies:** Phase 1 complete
**Deliverables:**
- P1-1: Implement per-session rate limiting
- P1-2: Fix unsubscribe to be topic-specific
- P1-4: Resolve target address ambiguity

**Success Criteria:**
- Rate limiting active (10 msg/sec default per session)
- `hub:rate_limited` returned with `retryAfter` when exceeded
- Unsubscribe from topic A doesn't affect topic B subscription
- Either `msg.to` OR `payload.to` used consistently, not both

#### Workstream 2.2: Reliability & Operations
**Owner:** Backend Agent
**Dependencies:** Phase 1 complete
**Deliverables:**
- P1-3: Reduce heartbeat interval to 20s
- P1-5: Implement structured logging (JSON format)

**Success Criteria:**
- Client sends heartbeat every 20s (10s margin vs hibernation)
- All logs JSON-formatted with `event`, `sessionId`, `traceId`
- Logs queryable in Cloudflare analytics

---

### Phase 3: Test Infrastructure Repair - Week 1 (Parallel)

**Duration:** Concurrent with Phase 1
**Effort:** ~12 hours
**Parallel Tracks:** 2 workstreams
**Dependencies:** None (can start immediately)

#### Workstream 3.1: Test Framework Fixes
**Owner:** Test Agent
**Dependencies:** None
**Deliverables:**
- Fix module path issue (errors.test.ts)
- Investigate and fix broadcast timeout root cause
- Investigate and fix pub/sub timeout root cause
- Fix message ordering test race condition

**Success Criteria:**
- All 13 failing tests pass
- Test suite runs in <30s
- No flaky tests (3 consecutive clean runs)

#### Workstream 3.2: Test Coverage Expansion
**Owner:** Test Agent
**Dependencies:** Workstream 3.1 (framework must work first)
**Deliverables:**
- Unit tests for handlers (registration, messaging, pubsub)
- Reconnection scenario tests
- Edge case tests (expired actors, missing WebSocket, etc.)

**Success Criteria:**
- Handler code coverage >80%
- All P0 fixes have corresponding tests
- Edge cases from Opus review tested

---

### Phase 4: DX Improvements (P2) - Week 2-3 (Optional)

**Duration:** 3-4 days
**Effort:** ~16 hours
**Dependencies:** Phase 1 & 2 complete
**Priority:** Lower - can defer to post-launch

#### Workstream 4.1: Performance Optimizations
**Owner:** Backend Agent
**Deliverables:**
- P2-1: Add inverse index for subscription cleanup
- P2-5: Optimize broadcast JSON.stringify

**Success Criteria:**
- Disconnect with 100 topic subscriptions <10ms
- Broadcast to 100 actors <50ms

#### Workstream 4.2: Developer Experience
**Owner:** Client Agent
**Deliverables:**
- P2-3: Enhance error messages with resolution steps
- P2-4: Add client `sendMessage()` convenience API

**Success Criteria:**
- All error messages include "How to fix" section
- Client API hides flat payload construction

#### Workstream 4.3: Advanced Features
**Owner:** Backend Agent
**Deliverables:**
- P2-2: Implement hub:refresh_token

**Success Criteria:**
- Sessions lasting >1 hour can refresh tokens
- Token refresh documented in PROTOCOL.md

---

## Risk Assessment

### High Risk Areas

1. **Test Failures Root Cause Unknown**
   - **Risk:** Fixes may not address underlying issues
   - **Mitigation:** Thorough investigation before code changes (Workstream 3.1)
   - **Owner:** Test Agent

2. **Broadcast Async Queue Design**
   - **Risk:** Cloudflare Queues integration complex, may delay Phase 1
   - **Mitigation:** Option to reject >100 as interim solution
   - **Owner:** Backend Agent

3. **Rate Limiting Impact on Legitimate Traffic**
   - **Risk:** Too aggressive = false positives, too loose = ineffective
   - **Mitigation:** Start with 10 msg/sec, make configurable
   - **Owner:** Backend Agent

### Medium Risk Areas

1. **Structured Logging Performance Overhead**
   - **Risk:** JSON.stringify on every log may add CPU cost
   - **Mitigation:** Measure before/after, keep log volume low
   - **Owner:** Backend Agent

2. **Heartbeat Interval Change Client Impact**
   - **Risk:** Older clients still using 25s won't benefit
   - **Mitigation:** Document in breaking changes, backward compatible
   - **Owner:** Client Agent

### Low Risk Areas

1. **TTL Alarm Scheduling** - Straightforward, well-documented pattern
2. **Message Size Check** - Simple guard clause addition
3. **Duplicate Connection Detection** - Clear algorithm, testable

---

## Success Criteria Per Phase

### Phase 1 Success (P0 Complete)
- [ ] All 6 P0 items implemented and tested
- [ ] No known data loss scenarios
- [ ] Resource exhaustion attacks mitigated
- [ ] Production deployment possible (with caveats)

### Phase 2 Success (P1 Complete)
- [ ] All 5 P1 items implemented
- [ ] Rate limiting active and tested
- [ ] Pub/sub correctness verified
- [ ] Structured logging operational

### Phase 3 Success (Tests Fixed)
- [ ] 0 failing tests (was 13)
- [ ] Test suite stable (no flakes)
- [ ] Edge cases covered
- [ ] CI/CD green

### Overall Success (Production Ready)
- [ ] Phase 1, 2, 3 complete
- [ ] Documentation updated (PROTOCOL.md, ARCHITECTURE.md)
- [ ] Deployment runbook verified
- [ ] Monitoring dashboards operational
- [ ] On-call playbook documented

---

## Timeline Estimate

### Optimistic (Single Full-Time Engineer)
- **Week 1:** Phase 1 (P0) + Phase 3.1 (test framework) - 5 days
- **Week 2:** Phase 2 (P1) + Phase 3.2 (test coverage) - 5 days
- **Total:** 10 working days

### Realistic (With Review Cycles)
- **Week 1:** Phase 1 (P0) - 5 days
- **Week 2:** Phase 2 (P1) + Test fixes - 5 days
- **Week 3:** P2 DX items (optional) - 3 days
- **Total:** 13 working days

### Parallel Team (3 Agents)
- **Backend Agent:** P0 + P1 items (7 days)
- **Test Agent:** Phase 3 all items (5 days)
- **Client Agent:** P2 DX items (3 days, deferred)
- **Total:** 7 working days (with coordination overhead)

---

## Out of Scope (Phase 2 Future Work)

The following items are documented in PROTOCOL.md as Phase 2 but are **NOT** in this fix plan:

- Durable Object storage persistence (registry survives restarts)
- Async broadcast via Cloudflare Queues (>100 actors)
- Message deduplication (seen-set for idempotency)
- Sharding implementation (consistent hash routing)
- Offline message queue (30s grace period)
- HMAC message signatures
- Actor-to-actor permissions
- Topic ACLs

**Rationale:** These are architectural enhancements, not bugs. Current implementation is correct for MVP scope. Address after production validation.

---

## Next Steps

### Immediate Actions (Today)

1. **Create WBS.md** - Detailed task breakdown with effort estimates
2. **Create AGENT_TEAM.md** - Team structure and coordination protocol
3. **Create beads** - One per workstream with dependencies
4. **Assign ownership** - Map beads to agent types

### Week 1 Day 1 (Start of Execution)

1. **Backend Agent:** Start P0-1 (duplicate connection detection)
2. **Test Agent:** Start Phase 3.1 (test framework investigation)
3. **Daily standup:** Track progress, unblock dependencies

### Week 1 Day 5 (Phase 1 Checkpoint)

1. **Review:** All P0 items complete?
2. **Test:** Run full suite, verify 0 failures
3. **Decision:** Proceed to Phase 2 or address blockers

---

## Appendix: Opus Review Key Findings Summary

### Top 5 Edge Cases (from Opus Review Section 2)

1. **WebSocket.send() is fire-and-forget** - `hub:delivery_ack` overpromises delivery
2. **Duplicate connection without detection** - Stale sessions persist
3. **Registry cleanup lazy-only** - Alarm handler never scheduled
4. **Connection cleanup edge cases** - Registration before connect creates undeliverable actors
5. **Unsubscribe sledgehammer** - Removes from ALL topics, not just one

### Cloudflare-Specific Gotchas (from Section 3)

1. **Hibernation disabled but heartbeat assumes it's active** - Inconsistent design
2. **DO eviction loses everything** - Total amnesia on restart
3. **JSON.stringify cost scales with broadcast** - 100 actors = 50MB allocations
4. **Message size check after parse** - Already consumed memory
5. **No rate limiting** - Single client can exhaust CPU

### Spec-Implementation Divergence (from Section 6)

| Feature | Spec Status | Implementation Status | Priority |
|---------|-------------|----------------------|----------|
| Duplicate detection | MVP | Missing | P0 |
| Rate limiting | MVP | Code exists, not called | P1 |
| Token refresh | MVP | Missing | P2 |
| Pause/resume | MVP | Functions exist, never invoked | P2 |
| Async broadcast | MVP | console.warn only | P0 |
| Alarm cleanup | Documented | Handler exists, not scheduled | P0 |

---

**Document Owner:** Team Lead
**Review Cycle:** Daily during Phase 1, Weekly after
**Last Updated:** 2026-02-17
