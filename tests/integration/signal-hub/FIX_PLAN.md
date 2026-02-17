# Signal Hub Integration Test Fix Plan

**Generated:** 2026-02-17T10:47:59Z
**Project:** Signal Hub Integration Tests
**Goal:** Fix 30 failing tests → 100% pass rate (43/43)
**Current State:** 13/43 passing (30.2%)

---

## Work Breakdown Structure (WBS)

### Phase 1: API Signature Fixes (P0 - Quick Win)
**Complexity:** Low
**Estimated Time:** 30 minutes
**Impact:** +3 tests fixed (16/43 passing = 37%)
**Blocking:** None

#### Deliverables
1. Replace `.onMessage()` calls with `.on('message')` in test files
   - messaging.test.ts:286
   - pubsub.test.ts:80
   - pubsub.test.ts:98

#### Success Criteria
- ✓ All 3 files compile without TypeScript errors
- ✓ No "is not a function" errors when running tests
- ✓ Tests can register message handlers

#### Verification Steps
```bash
# Run affected tests
npm test -- messaging.test.ts
npm test -- pubsub.test.ts

# Verify no "onMessage is not a function" errors
```

---

### Phase 2: Message Delivery Investigation & Fix (P0 - Critical)
**Complexity:** High
**Estimated Time:** 4-6 hours
**Impact:** +22 tests fixed (38/43 passing = 88%)
**Blocking:** Blocks all messaging, pub/sub, broadcast tests
**Dependencies:** Phase 1 must complete first

#### Deliverables
1. **Investigation Report** - Identify exact failure point in message flow
2. **Debug Logging** - Add tracing to client and hub message paths
3. **Root Cause Fix** - Implement solution based on findings
4. **Test Verification** - Ensure messages delivered within timeout

#### Investigation Tasks

**Task 2.1: Add Client-Side Debug Logging**
- SignalHubClient.ts: Log WebSocket onopen/onmessage events
- Log when messages sent via `send()`, `sendWithAck()`
- Log when hub responses received
- Estimated: 30 min

**Task 2.2: Add Hub-Side Debug Logging**
- Signal Hub service: Log incoming WebSocket messages
- Log message routing decisions
- Log outgoing messages to destinations
- Estimated: 45 min

**Task 2.3: Run Diagnostic Test**
- Run single failing test with full logging enabled
- Trace message flow from source to destination
- Identify breakpoint in flow
- Estimated: 30 min

**Task 2.4: Implement Fix**
Based on investigation results, fix one of:
- **Scenario A:** Hub routing broken
  - Fix: Correct message routing logic in hub handlers
  - Files: services/signal-hub/src/handlers/*.ts
  - Estimated: 2 hours

- **Scenario B:** Client not receiving messages
  - Fix: Wire up WebSocket message handlers correctly
  - Files: packages/signal-hub-client/src/SignalHubClient.ts
  - Estimated: 1.5 hours

- **Scenario C:** Acknowledgment system broken
  - Fix: Ensure ack messages generated and sent
  - Files: Both client and hub
  - Estimated: 2 hours

**Task 2.5: Verify Message Delivery**
- Test point-to-point (SEAG ↔ Browser)
- Test pub/sub subscription delivery
- Test broadcast to multiple recipients
- Estimated: 30 min

#### Success Criteria
- ✓ Messages delivered in < 1000ms (well under 5000ms timeout)
- ✓ All 22 messaging tests pass
- ✓ No "Timeout waiting for message" errors
- ✓ Acknowledgments received for `sendWithAck()`

#### Verification Steps
```bash
# Test point-to-point messaging
npm test -- messaging.test.ts

# Test pub/sub
npm test -- pubsub.test.ts

# Test broadcast
npm test -- broadcast.test.ts

# Verify no timeouts in any test
```

---

### Phase 3: Actor Discovery Fix (P1)
**Complexity:** Medium
**Estimated Time:** 2-3 hours
**Impact:** +5 tests fixed (42/43 passing = 98%)
**Blocking:** None (can parallelize with Phase 2)
**Dependencies:** None

#### Deliverables
1. **Registry Investigation** - Verify actor storage in Durable Object
2. **Discovery Handler Fix** - Ensure hub:discover returns registered actors
3. **Test Verification** - Confirm discovery returns expected results

#### Investigation Tasks

**Task 3.1: Verify Actor Registration Storage**
- Check if `hub:register` handler stores actors
- Verify Durable Object state persistence
- Log registry contents after registration
- Estimated: 30 min

**Task 3.2: Debug Discovery Handler**
- Check `hub:discover` handler implementation
- Verify it queries the registry correctly
- Test with and without capability filter
- Estimated: 30 min

**Task 3.3: Implement Fix**
Likely fixes:
- **Scenario A:** Registry not storing actors
  - Fix: Correct storage logic in registration handler
  - Files: services/signal-hub/src/handlers/registration.ts
  - Estimated: 1 hour

- **Scenario B:** Discovery query broken
  - Fix: Correct discovery handler logic
  - Files: services/signal-hub/src/handlers/discovery.ts
  - Estimated: 1 hour

- **Scenario C:** Registration expiry too fast
  - Fix: Adjust TTL in registration logic
  - Files: services/signal-hub/src/registry.ts
  - Estimated: 30 min

**Task 3.4: Verify Discovery**
- Test discover all actors
- Test discover by capability
- Test actor removal after disconnect
- Estimated: 30 min

#### Success Criteria
- ✓ Registered actors appear in discovery results
- ✓ Capability filtering works correctly
- ✓ Disconnected actors removed from discovery
- ✓ All 5 discovery tests pass

#### Verification Steps
```bash
# Run discovery tests
npm test -- discovery.test.ts

# Verify actors discoverable
# Verify filtering works
# Verify cleanup on disconnect
```

---

### Phase 4: Connection Lifecycle Cleanup (P2)
**Complexity:** Low
**Estimated Time:** 15 minutes
**Impact:** +1 test fixed (43/43 passing = 100%)
**Blocking:** None
**Dependencies:** None (can do anytime)

#### Deliverables
1. Add `sessionId` cleanup to disconnect logic
2. Verify state fully reset after disconnect

#### Tasks

**Task 4.1: Fix Session Cleanup**
- File: packages/signal-hub-client/src/SignalHubClient.ts
- Add: `this.sessionId = null` in disconnect() method
- Estimated: 5 min

**Task 4.2: Verify Cleanup**
- Run connection lifecycle tests
- Verify sessionId is null after disconnect
- Estimated: 10 min

#### Success Criteria
- ✓ `sessionId` is `null` after disconnect
- ✓ Connection test "should disconnect gracefully" passes

#### Verification Steps
```bash
# Run connection tests
npm test -- connection.test.ts

# Verify graceful disconnect test passes
```

---

### Phase 5: End-to-End Integration Verification (P1)
**Complexity:** Low
**Estimated Time:** 30 minutes
**Impact:** Confidence in full system
**Dependencies:** Phases 1-4 complete

#### Deliverables
1. Full test suite run
2. Performance metrics report
3. Regression check

#### Tasks

**Task 5.1: Full Test Suite**
- Run all 43 tests
- Verify 100% pass rate
- Estimated: 10 min

**Task 5.2: Performance Check**
- Verify message latency < 1000ms
- Check for memory leaks during test runs
- Verify cleanup happens correctly
- Estimated: 15 min

**Task 5.3: Documentation**
- Update test README with current status
- Document any discovered issues
- Add troubleshooting guide
- Estimated: 15 min

#### Success Criteria
- ✓ 43/43 tests passing (100%)
- ✓ No flaky tests (run 3 times successfully)
- ✓ All tests complete in < 2 minutes total
- ✓ No memory leaks detected

#### Verification Steps
```bash
# Run full suite 3 times
npm test
npm test
npm test

# All should pass consistently
```

---

## Task Dependency Graph

```
[Phase 1: API Fixes]
        ↓
        ├─→ [Phase 2: Message Delivery] ──┐
        │                                  │
        └─→ [Phase 3: Actor Discovery] ────┼─→ [Phase 5: Integration Verification]
             ↑                             │
             │                             │
        [Phase 4: Lifecycle Cleanup] ──────┘

Legend:
─→  Sequential dependency (must complete before)
├─→ Optional parallel work (can run simultaneously)
```

### Critical Path
```
Phase 1 (30 min) → Phase 2 (4-6 hrs) → Phase 5 (30 min)
Total Critical Path: 5-7 hours
```

### Parallel Opportunities
- Phase 3 (Discovery) can run in parallel with Phase 2 investigation
- Phase 4 (Cleanup) can be done anytime
- If 2 developers: Dev 1 on Phase 2, Dev 2 on Phase 3+4

---

## Execution Timeline

### Day 1: Quick Wins + Investigation
**Hours 0-1:** Phase 1 - API signature fixes (+3 tests)
**Hours 1-3:** Phase 2.1-2.3 - Add logging and investigate
**Hours 3-5:** Phase 3.1-3.2 - Investigate actor discovery (parallel)

**End of Day 1:**
- API fixes complete (16/43 passing)
- Root cause of message delivery identified
- Root cause of discovery issues identified

### Day 2: Implementation
**Hours 0-3:** Phase 2.4 - Implement message delivery fix
**Hours 3-4:** Phase 2.5 - Verify messaging works (+22 tests)
**Hours 4-6:** Phase 3.3-3.4 - Implement discovery fix (+5 tests)
**Hours 6:** Phase 4 - Session cleanup (+1 test)

**End of Day 2:**
- All fixes implemented
- 42-43 tests passing

### Day 3: Verification & Polish
**Hours 0-1:** Phase 5 - Full integration verification
**Hours 1-2:** Fix any remaining issues
**Hours 2-3:** Documentation and handoff

**End of Day 3:**
- 43/43 tests passing (100%)
- Full documentation complete
- Ready for production

---

## Resource Allocation

### Option A: Single Developer (Sequential)
- **Total Time:** 2.5-3 days
- **Advantages:** Simpler coordination
- **Disadvantages:** Slower completion

### Option B: Two Developers (Parallel)
- **Developer 1:** Phase 1 → Phase 2 (critical path)
- **Developer 2:** Phase 3 → Phase 4 (parallel work)
- **Both:** Phase 5 (integration verification)
- **Total Time:** 1.5-2 days
- **Advantages:** Faster completion
- **Disadvantages:** Requires coordination

### Option C: Agent Team (Recommended)
- **API-fix agent:** Phase 1 (30 min)
- **Message-delivery agent:** Phase 2 (4-6 hrs)
- **Discovery agent:** Phase 3 (2-3 hrs) - parallel
- **Cleanup agent:** Phase 4 (15 min) - parallel
- **Integration agent:** Phase 5 (30 min) - after all complete
- **Total Time:** 1-1.5 days with perfect parallelization
- **Advantages:** Maximum parallelization, specialized focus
- **Disadvantages:** Requires agent orchestration

---

## Risk Mitigation

### High Risk: Message Delivery Investigation
**Risk:** Investigation takes longer than expected
**Mitigation:**
- Set 3-hour time box for investigation
- If not resolved, escalate for help
- Consider adding temporary workarounds to unblock tests

### Medium Risk: Discovery Fix Complexity
**Risk:** Registry issue deeper than expected (Durable Object state)
**Mitigation:**
- Test with simple in-memory registry first
- If DO state issue, may need Cloudflare-specific debugging
- Have backup plan to mock registry for tests

### Low Risk: API Signature Fixes
**Risk:** Minimal, straightforward find/replace
**Mitigation:** None needed

---

## Rollback Plan

If fixes introduce regressions:

1. **Git safety:** Each phase commits separately
2. **Rollback command:** `git revert <commit-hash>`
3. **Test validation:** Run full suite after each phase
4. **Incremental deployment:** Merge phases individually

---

## Success Metrics & KPIs

| Metric | Baseline | Target | Stretch Goal |
|--------|----------|--------|--------------|
| Tests Passing | 13/43 (30%) | 43/43 (100%) | 43/43 (100%) |
| Message Latency | Timeout (5000ms+) | < 1000ms | < 500ms |
| Test Suite Duration | 138s | < 120s | < 60s |
| Zero Flaky Tests | N/A | 3 consecutive runs pass | 10 consecutive runs |

---

## Next Steps

1. **Review this plan** with team/stakeholders
2. **Create beads** for each phase (see bead creation section)
3. **Assign agents** or developers to phases
4. **Execute Phase 1** immediately (quick win)
5. **Begin Phase 2 investigation** while discovery investigation runs parallel

---

## Bead Creation Commands

See separate section in this document or AGENT_TEAM.md for bead creation and dependency setup.

---

**Plan Status:** Ready for execution
**Last Updated:** 2026-02-17T10:47:59Z
**Next Review:** After Phase 2 completion (milestone check)
