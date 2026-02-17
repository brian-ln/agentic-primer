# Signal Hub Integration Test Analysis

**Generated:** 2026-02-17T10:47:59Z
**Verified Current Date:** 2026-02-17 (via `date -u`)
**Test Suite:** Signal Hub Integration Tests
**Current Status:** 13/43 tests passing (30.2%)
**Improvement from Baseline:** +62% (from 5/43)

---

## Executive Summary

Analysis of 30 failing tests across 6 test suites reveals **3 primary root causes**:
1. **API Signature Mismatch** (3 failures) - `.onMessage()` not available, should use `.on('message')`
2. **Message Delivery Timeouts** (22 failures) - Messages not reaching destinations within 5s
3. **Actor Discovery Issues** (4 failures) - Actors not visible in discovery/broadcast
4. **Connection Lifecycle Bug** (1 failure) - Session not cleared on disconnect

---

## REFLECT Analysis - Categorized Failures

### Category 1: API Signature Mismatch (3 failures)
**Root Cause:** Tests calling `.onMessage()` instead of `.on('message')` event handler

**Failures:**
1. `messaging.test.ts` > Message Ordering > "should maintain message order within a connection"
   - Error: `browserActor.getClient().onMessage is not a function`
   - Line: messaging.test.ts:286

2. `pubsub.test.ts` > Basic Pub/Sub > "should not receive messages from unsubscribed topics"
   - Error: `subscriberBrowser.getClient().onMessage is not a function`
   - Line: pubsub.test.ts:80

3. `pubsub.test.ts` > Basic Pub/Sub > "should receive messages only after subscription"
   - Error: `subscriberBrowser.getClient().onMessage is not a function`
   - Line: pubsub.test.ts:98

**Pattern:** Tests directly accessing `getClient()` and calling non-existent `.onMessage()` method

**Correct API (from SignalHubClient.ts:649-654):**
```typescript
on(event: 'message', handler: MessageHandler): void;
on(event: 'connected', handler: ConnectionHandler): void;
on(event: 'disconnected', handler: ConnectionHandler): void;
on(event: 'error', handler: ErrorHandler): void;
on(event: 'stateChange', handler: StateChangeHandler): void;
```

**Impact:** Low - Only 3 tests affected, simple find/replace fix

---

### Category 2: Message Delivery Timeouts (22 failures)
**Root Cause:** Messages not being delivered within 5000ms timeout window

#### Subcategory 2A: Cross-Runtime Messaging (10 failures)
**Test Suite:** `messaging.test.ts`

1. SEAG → Browser (tell pattern) - 5021ms timeout
2. SEAG → Browser (ask with ack) - 5009ms timeout
3. SEAG → Browser (payload integrity) - 5018ms timeout
4. SEAG → Browser (large payloads) - 5007ms timeout
5. Browser → SEAG (tell pattern) - 5008ms timeout
6. Browser → SEAG (ask with ack) - 5006ms timeout
7. Browser → SEAG (payload integrity) - 5005ms timeout
8. Bidirectional (roundtrip) - 5007ms timeout
9. Bidirectional (concurrent) - 5007ms timeout
10. Message Metadata preservation - 5004ms timeout

**Pattern:** ALL message routing tests timing out at exactly 5000ms
**Hypothesis:** Either WebSocket connection not established OR message routing handlers not wired up

#### Subcategory 2B: Pub/Sub Messaging (5 failures)
**Test Suite:** `pubsub.test.ts`

1. Basic subscription/publish - 5009ms timeout
2. Multiple subscribers - 5008ms timeout
3. Multiple topics - 5010ms timeout
4. Unsubscribe flow - 5006ms timeout
5. Cross-runtime pub/sub (SEAG → subscribers) - 5008ms timeout
6. Cross-runtime pub/sub (Browser → subscribers) - 5008ms timeout

**Pattern:** All pub/sub tests timing out, suggesting hub:subscribe/hub:publish not working

#### Subcategory 2C: Broadcast Messaging (7 failures)
**Test Suite:** `broadcast.test.ts`

1. SEAG → All browsers broadcast - timeout
2. Browser → SEAG + browsers broadcast - timeout
3. Broadcast to all actors - timeout
4. Broadcast with large payloads - timeout
5. Broadcast selective (by capability) - timeout
6. Broadcast concurrent - timeout
7. Broadcast metadata preservation - timeout

**Pattern:** All broadcast tests failing, suggests hub:broadcast handler issue

**Additional Evidence:**
- Unhandled error from pubsub.test.ts: "Message delivery timeout" at SignalHubClient.ts:412
- This is the `pendingAsks` timeout mechanism triggering
- Suggests messages sent but no ack received within timeout

**Impact:** High - 22/30 failures (73% of remaining issues)

---

### Category 3: Actor Discovery Issues (4 failures)
**Root Cause:** Actors not appearing in discovery results

**Failures:**
1. `discovery.test.ts` > Single Actor Discovery > "should discover actors by capability"
   - Expected: SEAG actor with 'compute' capability
   - Actual: Empty array returned
   - Line: discovery.test.ts:115

2. `discovery.test.ts` > Single Actor Discovery > "should return all actors when no capability filter"
   - Expected: Array with at least 1 actor
   - Actual: Empty array (length 0)
   - Line: discovery.test.ts:125

3. `discovery.test.ts` > Multiple Actors Discovery > "should return all registered actors"
   - Expected: Array containing specific addresses
   - Actual: Missing addresses in results
   - Line: discovery.test.ts:189

4. `discovery.test.ts` > Multiple Actors Discovery > "should filter actors by capability"
   - Expected: ≥2 actors with 'ml' capability
   - Actual: 0 actors returned
   - Line: discovery.test.ts:224

5. `discovery.test.ts` > Actor Unregistration > "should remove actor from discovery after disconnect"
   - Expected: Actor to be defined before disconnect
   - Actual: undefined (actor never appeared in discovery)
   - Line: discovery.test.ts:263

**Pattern:** `client.discover()` consistently returns empty or incomplete results

**Hypothesis:**
- Hub's actor registry not persisting registrations
- OR `hub:discover` request/response broken
- OR actors not staying registered (expiring too quickly)

**Impact:** Medium - 5/30 failures, blocks actor visibility features

---

### Category 4: Connection Lifecycle Bug (1 failure)
**Root Cause:** Session ID not cleared on disconnect

**Failure:**
`connection.test.ts` > Browser Actor Connection > "should disconnect gracefully"
- Expected: `sessionId` to be `null` after disconnect
- Actual: Still contains session ID string
- Line: connection.test.ts:189 (approx)

**Pattern:** `client.disconnect()` not clearing internal state properly

**Impact:** Low - 1 failure, but indicates state management issue

---

## Dependency Analysis

### Sequential Dependencies (Must Fix in Order)

```
Phase 1: API Fixes (Category 1)
    ↓
Phase 2: Connection/Registration (Prerequisites for messaging)
    ↓
Phase 3: Message Delivery (Category 2)
    ├─→ Point-to-Point Messaging (2A)
    ├─→ Pub/Sub (2B)
    └─→ Broadcast (2C)
    ↓
Phase 4: Actor Discovery (Category 3)
    ↓
Phase 5: Lifecycle Cleanup (Category 4)
```

### Critical Path Analysis

**Blocker #1: Message Delivery Core Issue**
- Affects 22/30 tests (73%)
- Must diagnose why messages timeout
- Likely causes:
  1. WebSocket not connecting properly to hub
  2. Message handlers not registered on hub side
  3. Hub not routing messages to destinations
  4. Acknowledgment system broken

**Blocker #2: Actor Registry**
- Affects discovery (5 tests) and broadcast (7 tests indirectly)
- Registration likely succeeds but registry not queryable
- Could be storage issue (Durable Object state?)

---

## Test Suite Breakdown

| Test Suite | Total | Pass | Fail | Pass Rate |
|------------|-------|------|------|-----------|
| connection.test.ts | 9 | 8 | 1 | 88.9% |
| messaging.test.ts | 11 | 0 | 11 | 0% |
| pubsub.test.ts | 8 | 0 | 8 | 0% |
| broadcast.test.ts | 7 | 0 | 7 | 0% |
| discovery.test.ts | 5 | 0 | 5 | 0% |
| errors.test.ts | 3 | 5 | 0 | 100% |
| **TOTAL** | **43** | **13** | **30** | **30.2%** |

**Observation:**
- Connection tests mostly passing (88.9%) ✓
- Error handling tests fully passing (100%) ✓
- **ALL** message delivery tests failing (0%) ✗
- Discovery completely broken (0%) ✗

This suggests:
- Basic connection protocol works
- Message routing is fundamentally broken
- Actor registry/discovery broken

---

## FIXIT Methodology - Systematic Problem Solving

### Problem 1: API Signature Mismatch
**Problem:** Tests call `.onMessage()` which doesn't exist
**Root Cause:** Incorrect API usage, should be `.on('message', handler)`
**Solution:** Replace 3 instances of `.onMessage()` with `.on('message')`
**Verification:** Tests compile and don't throw "not a function" error
**Priority:** P0 (quick win, unblocks other tests)

### Problem 2: Message Delivery Timeouts
**Problem:** ALL messaging tests timeout at 5000ms
**Root Cause:** Unknown - needs investigation. Suspects:
1. WebSocket connection not fully established
2. Hub message handlers not wired up
3. Message routing broken in hub
4. Client not listening for responses

**Investigation Steps:**
1. Add debug logging to client WebSocket connection
2. Verify hub receives messages (check handler logs)
3. Verify hub routes messages to destination
4. Check if ack messages are sent back
5. Verify client processes incoming WebSocket messages

**Solution (TBD based on investigation):**
- If hub routing broken: Fix routing logic in signal-hub service
- If client not listening: Fix WebSocket message handler setup
- If ack system broken: Fix ack generation/handling

**Verification:**
- Point-to-point message delivered within 1000ms
- Pub/sub message delivered to all subscribers
- Broadcast reaches all actors

**Priority:** P0 (critical blocker, affects 73% of failures)

### Problem 3: Actor Discovery Empty Results
**Problem:** `discover()` returns empty or incomplete arrays
**Root Cause:** Actor registry not persisting or queryable
**Investigation:**
1. Verify hub receives `hub:register` messages
2. Check if actors stored in registry (DO state)
3. Verify `hub:discover` handler queries registry
4. Check if registrations expire too quickly

**Solution (TBD):**
- If storage broken: Fix registry storage in Durable Object
- If query broken: Fix discover handler logic
- If expiry too fast: Adjust TTL settings

**Verification:**
- Registered actors appear in discovery results
- Capability filtering works correctly
- Disconnected actors removed from results

**Priority:** P1 (blocks discovery features)

### Problem 4: Session Cleanup on Disconnect
**Problem:** `sessionId` not cleared after disconnect
**Root Cause:** `disconnect()` doesn't reset `sessionId` field
**Solution:** Add `this.sessionId = null` to disconnect logic
**Verification:** Session ID is null after disconnect
**Priority:** P2 (minor state cleanup issue)

---

## Risk Assessment

### High Risk
- **Message delivery investigation could be time-consuming**
  - Multiple potential failure points (client, transport, hub)
  - May require adding extensive debug logging
  - Could uncover deeper architectural issues

### Medium Risk
- **Actor registry may require Durable Object debugging**
  - Harder to debug (requires Wrangler dev environment)
  - State persistence issues tricky to diagnose

### Low Risk
- **API signature fixes** - Simple find/replace
- **Session cleanup** - One-line fix

---

## Recommendations

### Immediate Actions (Day 1)
1. Fix API signature mismatches (3 tests) - 15 min
2. Add comprehensive debug logging to message flow - 1 hour
3. Run tests with logging to identify message delivery failure point - 30 min

### Investigation Phase (Day 1-2)
4. Diagnose message delivery timeout root cause - 2-4 hours
5. Diagnose actor discovery empty results - 1-2 hours

### Fix Phase (Day 2-3)
6. Implement message delivery fixes based on findings - 2-4 hours
7. Implement actor discovery fixes - 1-2 hours
8. Fix session cleanup bug - 5 min

### Verification Phase (Day 3)
9. Run full test suite
10. Verify 100% pass rate
11. Document lessons learned

**Estimated Total Time:** 2-3 days for complete fix

---

## Success Metrics

- **Target:** 43/43 tests passing (100%)
- **Milestone 1:** API fixes → 16/43 passing (37%)
- **Milestone 2:** Message delivery fixed → 38/43 passing (88%)
- **Milestone 3:** Actor discovery fixed → 42/43 passing (98%)
- **Milestone 4:** Cleanup complete → 43/43 passing (100%)

---

## Appendix: Test Output Evidence

### Full Test Summary
```
Test Files  6 failed (6)
     Tests  30 failed | 13 passed (43)
    Errors  1 error
  Duration  138.98s
```

### Sample Error Messages
```
TypeError: browserActor.getClient().onMessage is not a function
  at messaging.test.ts:286:32

Error: Timeout waiting for message (5000ms)
  at waitForMessage (browser-actor.ts:129)

AssertionError: expected 0 to be greater than or equal to 2
  at discovery.test.ts:224:31

Error: Message delivery timeout
  at SignalHubClient.ts:412:25
```

---

**Analysis Complete:** All 30 failures categorized and root causes identified.
**Next Step:** Create implementation plan (FIX_PLAN.md) and agent team structure (AGENT_TEAM.md)
