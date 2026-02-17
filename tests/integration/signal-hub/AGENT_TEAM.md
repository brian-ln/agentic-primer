# Agent Team Structure - Signal Hub Test Fixes

**Generated:** 2026-02-17T10:47:59Z
**Project:** Signal Hub Integration Tests
**Execution Model:** Multi-agent parallel orchestration
**Target:** 43/43 tests passing (100%)

---

## Team Composition

### Team 1: API-Fix Agent
**Bead:** `agentic-primer-4ei`
**Priority:** P0 (Critical blocker for messaging work)
**Estimated Duration:** 30 minutes
**Complexity:** Low
**Parallelization:** None (must run first)

#### Scope
Fix 3 API signature mismatches where tests call `.onMessage()` instead of `.on('message')`

#### Ownership Boundaries
**Files to Modify:**
- `/Users/bln/play/agentic-primer/tests/integration/signal-hub/messaging.test.ts`
  - Line 286: `browserActor.getClient().onMessage(handler);`
- `/Users/bln/play/agentic-primer/tests/integration/signal-hub/pubsub.test.ts`
  - Line 80: `subscriberBrowser.getClient().onMessage((msg: any) => {`
  - Line 98: `subscriberBrowser.getClient().onMessage((msg: any) => {`

**Pattern to Apply:**
```typescript
// BEFORE (incorrect)
client.onMessage(handler);

// AFTER (correct)
client.on('message', handler);
```

#### Expected Deliverables
1. Three `.onMessage()` calls replaced with `.on('message')`
2. All test files compile without TypeScript errors
3. Git commit with message: "fix: Replace .onMessage() with .on('message') in tests"
4. Verification: Run affected tests to ensure no "is not a function" errors

#### Success Criteria
- ✓ No TypeScript compilation errors
- ✓ No "onMessage is not a function" runtime errors
- ✓ Tests can register message handlers successfully
- ✓ 16/43 tests passing (up from 13/43)

#### Dependencies
- **Blocks:** Team 2 (messaging agent) - must complete first
- **Blocked by:** None

#### Agent Instructions
```bash
# 1. Navigate to test directory
cd /Users/bln/play/agentic-primer/tests/integration/signal-hub

# 2. Replace API calls (3 instances)
# messaging.test.ts:286
# pubsub.test.ts:80
# pubsub.test.ts:98

# 3. Run tests to verify
npm test -- messaging.test.ts
npm test -- pubsub.test.ts

# 4. Commit changes
git add messaging.test.ts pubsub.test.ts
git commit -m "fix: Replace .onMessage() with .on('message') in tests"

# 5. Mark bead complete
bd done agentic-primer-4ei
```

---

### Team 2: Message-Delivery Agent
**Bead:** `agentic-primer-lr1`
**Priority:** P0 (Critical - affects 73% of failures)
**Estimated Duration:** 4-6 hours
**Complexity:** High
**Parallelization:** None (critical path)

#### Scope
Investigate and fix all message delivery timeouts (22 failing tests)

#### Ownership Boundaries
**Investigation Files:**
- `/Users/bln/play/agentic-primer/packages/signal-hub-client/src/SignalHubClient.ts`
- `/Users/bln/play/agentic-primer/services/signal-hub/src/handlers/*.ts`
- `/Users/bln/play/agentic-primer/services/signal-hub/src/websocket.ts`

**Test Files Affected:**
- `messaging.test.ts` (11 failures)
- `pubsub.test.ts` (8 failures)
- `broadcast.test.ts` (7 failures)

#### Expected Deliverables
1. **Investigation Report** (diagnostic.md)
   - Message flow trace from sender to receiver
   - Exact failure point identified
   - Root cause diagnosis

2. **Debug Logging** (temporary commits)
   - Client WebSocket event logging
   - Hub message handler logging
   - Can be removed after fix

3. **Fix Implementation**
   - Based on investigation findings
   - Could be client-side, hub-side, or both

4. **Verification**
   - All 22 messaging tests pass
   - Message latency < 1000ms
   - No timeout errors

5. **Git Commits**
   - "debug: Add message flow tracing to client and hub"
   - "fix: [Root cause fix description]"
   - "chore: Remove debug logging"

#### Investigation Protocol

**Step 1: Add Client Logging (30 min)**
```typescript
// In SignalHubClient.ts

// Log WebSocket connection
this.ws.onopen = () => {
  console.log('[CLIENT] WebSocket OPEN');
  // ... existing code
};

// Log outgoing messages
send(from, to, type, payload) {
  console.log('[CLIENT] SEND:', { from, to, type, msgId });
  // ... existing code
}

// Log incoming messages
this.ws.onmessage = (event) => {
  console.log('[CLIENT] RECV:', event.data);
  // ... existing code
};
```

**Step 2: Add Hub Logging (45 min)**
```typescript
// In signal-hub handlers

handleWebSocketMessage(ws, data) {
  console.log('[HUB] RECV from client:', data);
  // ... existing code
}

routeMessage(msg) {
  console.log('[HUB] ROUTE:', msg.to, 'type:', msg.type);
  // ... existing code
}
```

**Step 3: Run Diagnostic Test (30 min)**
```bash
# Run single failing test with full output
npm test -- messaging.test.ts -t "should send message from SEAG to browser"

# Analyze logs to find where message flow stops:
# 1. Client sends message? ✓/✗
# 2. Hub receives message? ✓/✗
# 3. Hub routes message? ✓/✗
# 4. Destination receives message? ✓/✗
# 5. Ack sent back? ✓/✗
```

**Step 4: Fix Based on Findings**

**Scenario A: Hub routing broken**
- Fix: Correct message routing logic
- Files: `services/signal-hub/src/handlers/*.ts`
- Commit: "fix: Correct message routing in hub handlers"

**Scenario B: Client not listening**
- Fix: Wire up WebSocket onmessage handler
- Files: `packages/signal-hub-client/src/SignalHubClient.ts`
- Commit: "fix: Ensure WebSocket message handler registered"

**Scenario C: Ack system broken**
- Fix: Generate and send ack messages
- Files: Both client and hub
- Commit: "fix: Implement message acknowledgment system"

#### Success Criteria
- ✓ All 22 messaging tests pass
- ✓ Message delivery < 1000ms
- ✓ No timeout errors
- ✓ 38/43 tests passing (up from 16/43)

#### Dependencies
- **Blocks:** Team 5 (integration verification)
- **Blocked by:** Team 1 (API fixes) - must complete first

#### Coordination Points
- Shares test files with all teams
- Notify Teams 3, 4, 5 when complete (unblocks final verification)

---

### Team 3: Discovery Agent
**Bead:** `agentic-primer-3m1`
**Priority:** P1 (Medium impact)
**Estimated Duration:** 2-3 hours
**Complexity:** Medium
**Parallelization:** Can run parallel to Team 2

#### Scope
Fix actor discovery returning empty or incomplete results (5 failing tests)

#### Ownership Boundaries
**Investigation Files:**
- `/Users/bln/play/agentic-primer/services/signal-hub/src/handlers/registration.ts`
- `/Users/bln/play/agentic-primer/services/signal-hub/src/handlers/discovery.ts`
- `/Users/bln/play/agentic-primer/services/signal-hub/src/registry/*`

**Test Files:**
- `discovery.test.ts` (5 failures)

#### Expected Deliverables
1. **Investigation Report**
   - Verify actors stored in registry
   - Check discovery query logic
   - Identify why results empty

2. **Fix Implementation**
   - Registry storage fix OR
   - Discovery handler fix OR
   - TTL adjustment

3. **Verification**
   - All 5 discovery tests pass
   - Actors appear in results
   - Capability filtering works

4. **Git Commits**
   - "debug: Add registry inspection logging"
   - "fix: [Discovery fix description]"
   - "test: Verify discovery works correctly"

#### Investigation Protocol

**Step 1: Verify Registration Storage (30 min)**
```typescript
// In registration.ts handleRegister

async handleRegister(actor: ActorRegistration) {
  console.log('[REGISTRY] REGISTER:', actor.address);

  await this.registry.set(actor.address, actor);

  // Verify it was stored
  const stored = await this.registry.get(actor.address);
  console.log('[REGISTRY] STORED:', stored ? 'YES' : 'NO');
}
```

**Step 2: Debug Discovery Handler (30 min)**
```typescript
// In discovery.ts handleDiscover

async handleDiscover(capability?: string) {
  console.log('[REGISTRY] DISCOVER capability:', capability);

  const all = await this.registry.getAll();
  console.log('[REGISTRY] Total actors:', all.length);

  if (capability) {
    const filtered = all.filter(a => a.capabilities.includes(capability));
    console.log('[REGISTRY] Filtered:', filtered.length);
    return filtered;
  }

  return all;
}
```

**Step 3: Run Diagnostic Test (30 min)**
```bash
# Run discovery tests
npm test -- discovery.test.ts -t "should discover actors by capability"

# Check logs:
# 1. Actor registered? ✓/✗
# 2. Stored in registry? ✓/✗
# 3. Discovery query runs? ✓/✗
# 4. Results returned? ✓/✗
```

**Step 4: Fix Based on Findings**

**Scenario A: Storage broken**
- Fix: Correct registry storage in Durable Object
- Commit: "fix: Ensure actor registry persists to DO state"

**Scenario B: Query broken**
- Fix: Correct discovery filtering logic
- Commit: "fix: Return all actors in discovery results"

**Scenario C: TTL too short**
- Fix: Increase registration TTL
- Commit: "fix: Extend actor registration TTL to prevent expiry"

#### Success Criteria
- ✓ Registered actors appear in discovery
- ✓ Capability filtering works
- ✓ Unregistered actors removed
- ✓ All 5 discovery tests pass
- ✓ 42/43 tests passing (up from 38/43)

#### Dependencies
- **Blocks:** Team 5 (integration verification)
- **Blocked by:** None (can run parallel to Team 2)

#### Coordination Points
- Independent work, minimal coordination needed
- Notify Team 5 when complete

---

### Team 4: Cleanup Agent
**Bead:** `agentic-primer-zyx`
**Priority:** P2 (Low impact)
**Estimated Duration:** 15 minutes
**Complexity:** Low
**Parallelization:** Can run anytime in parallel

#### Scope
Fix session ID not cleared on disconnect (1 failing test)

#### Ownership Boundaries
**Files to Modify:**
- `/Users/bln/play/agentic-primer/packages/signal-hub-client/src/SignalHubClient.ts`
  - Method: `disconnect()`

**Test Files:**
- `connection.test.ts` (1 failure)

#### Expected Deliverables
1. Add `this.sessionId = null` to disconnect method
2. Git commit: "fix: Clear sessionId on disconnect"
3. Verification: Connection lifecycle test passes

#### Implementation

```typescript
// In SignalHubClient.ts disconnect() method

async disconnect(): Promise<void> {
  // ... existing disconnect logic ...

  // ADD THIS LINE:
  this.sessionId = null;

  // ... rest of cleanup ...
}
```

#### Success Criteria
- ✓ `sessionId` is `null` after disconnect
- ✓ Connection test "should disconnect gracefully" passes
- ✓ 43/43 tests passing (100%)

#### Dependencies
- **Blocks:** Team 5 (integration verification)
- **Blocked by:** None (can run anytime)

#### Agent Instructions
```bash
# 1. Edit SignalHubClient.ts
# Add: this.sessionId = null; to disconnect() method

# 2. Run test
npm test -- connection.test.ts -t "should disconnect gracefully"

# 3. Commit
git add packages/signal-hub-client/src/SignalHubClient.ts
git commit -m "fix: Clear sessionId on disconnect"

# 4. Mark bead complete
bd done agentic-primer-zyx
```

---

### Team 5: Integration-Verification Agent
**Bead:** `agentic-primer-j1s`
**Priority:** P1 (Final validation)
**Estimated Duration:** 30 minutes
**Complexity:** Low
**Parallelization:** Runs after all others complete

#### Scope
End-to-end verification of all fixes

#### Ownership Boundaries
**Activities:**
- Run full test suite 3 times
- Check performance metrics
- Document final results
- Create summary report

#### Expected Deliverables
1. **Verification Report** (VERIFICATION_RESULTS.md)
   - Test pass rate: 43/43 (100%)
   - Performance metrics
   - Flakiness check (3 runs)

2. **Performance Metrics**
   - Message delivery latency
   - Test suite duration
   - Memory usage

3. **Final Documentation**
   - Update README with success
   - Document any discovered issues
   - Add troubleshooting guide

#### Verification Protocol

**Step 1: Full Suite (3 runs)**
```bash
# Run 1
npm test 2>&1 | tee run1.log

# Run 2
npm test 2>&1 | tee run2.log

# Run 3
npm test 2>&1 | tee run3.log

# Verify all 3 show: 43 pass, 0 fail
```

**Step 2: Performance Check**
```bash
# Check message latency (should be < 1000ms)
grep -i "timeout" run*.log  # Should be empty

# Check test duration (should be < 120s)
grep "Duration" run*.log

# Check for memory leaks
# Run tests with --expose-gc flag if available
```

**Step 3: Create Report**
```markdown
# Verification Results

**Date:** 2026-02-17
**Status:** ✓ SUCCESS

## Test Results
- Run 1: 43/43 passing (100%)
- Run 2: 43/43 passing (100%)
- Run 3: 43/43 passing (100%)
- Flakiness: None detected

## Performance
- Message latency: < 500ms (avg)
- Test suite duration: 95s
- Memory usage: Stable

## Fixed Issues
1. API signature mismatches (3 tests)
2. Message delivery timeouts (22 tests)
3. Actor discovery empty results (5 tests)
4. Session cleanup on disconnect (1 test)

**Total fixes:** 30 tests
**Final pass rate:** 100%
```

#### Success Criteria
- ✓ 43/43 tests pass on all 3 runs
- ✓ No flaky tests detected
- ✓ Message latency < 1000ms
- ✓ Test suite completes in < 120s
- ✓ Documentation updated

#### Dependencies
- **Blocks:** None (final step)
- **Blocked by:** Teams 1, 2, 3, 4 (all must complete)

#### Coordination Points
- Wait for all teams to complete
- Collect reports from Teams 2 and 3
- Synthesize final summary

---

## Team Coordination

### Communication Protocol

**Status Updates:**
Each agent posts to shared channel when:
- Work started
- Milestone reached (e.g., investigation complete)
- Blocked on issue
- Work complete

**Format:**
```
[TEAM-X] STATUS: <started|milestone|blocked|complete>
- Current: <what you're doing>
- Next: <what's next>
- Blocking: <what's blocking you, if any>
- ETA: <estimated completion time>
```

**Example:**
```
[TEAM-2] STATUS: milestone
- Current: Investigation complete, root cause identified
- Next: Implementing fix for hub message routing
- Blocking: None
- ETA: 2 hours
```

### Handoff Points

**Handoff 1: Team 1 → Team 2**
- When: Team 1 completes API fixes
- Action: Team 1 notifies Team 2 to start
- Verification: Team 2 confirms tests compile

**Handoff 2: All → Team 5**
- When: Teams 2, 3, 4 all complete
- Action: Each team notifies Team 5
- Verification: Team 5 confirms all beads marked done

### Conflict Resolution

**File Conflicts:**
- All teams work on different files (no direct conflicts)
- If unexpected conflict: Coordinate via git branch strategy
- Team 2 has priority on client code (highest impact)

**Test Conflicts:**
- Multiple teams may want to run tests simultaneously
- Solution: Use separate terminal windows or coordinate test runs
- Team 2 gets priority during investigation phase

---

## Execution Modes

### Mode A: Sequential (Single Agent)
**Timeline:** 2.5-3 days
```
Day 1: Team 1 (30m) → Team 2 investigation (4h)
Day 2: Team 2 fix (2h) → Team 3 (2h) → Team 4 (15m)
Day 3: Team 5 (30m)
```

### Mode B: Parallel (Multi-Agent) - RECOMMENDED
**Timeline:** 1-1.5 days
```
Hour 0-0.5:  Team 1 (API fixes)
Hour 0.5-6:  Team 2 (messaging) || Team 3 (discovery) || Team 4 (cleanup)
Hour 6-7:    Team 5 (integration verification)
```

**Parallelization Strategy:**
- Team 1 runs first (30 min)
- Teams 2, 3, 4 run in parallel after Team 1 completes
- Team 5 runs after all others complete

---

## Bead Status Tracking

```bash
# Check overall status
bd list --status=open | grep agentic-primer

# Check specific bead
bd show agentic-primer-4ei

# Mark work started
bd comment agentic-primer-4ei "Started API signature fixes"

# Mark complete
bd done agentic-primer-4ei
```

### Current Bead State
```
○ agentic-primer-4ei [P2] - API fixes (READY TO START)
  ↓ blocks
○ agentic-primer-lr1 [P2] - Message delivery (BLOCKED by 4ei)
  ↓ blocks
○ agentic-primer-3m1 [P2] - Discovery (READY TO START - parallel)
○ agentic-primer-zyx [P3] - Cleanup (READY TO START - parallel)
  ↓ all block
○ agentic-primer-j1s [P2] - Integration (BLOCKED by all above)
```

---

## Success Dashboard

Track progress with these commands:

```bash
# Test pass rate
npm test 2>&1 | grep "Tests"

# Bead completion
bd list --status=open | grep agentic-primer | wc -l

# Files changed
git status --short

# Commits made
git log --oneline --since="1 day ago"
```

**Target Metrics:**
- [ ] 5 beads created ✓
- [ ] Dependencies configured ✓
- [ ] Team 1 complete → 16/43 tests (37%)
- [ ] Team 2 complete → 38/43 tests (88%)
- [ ] Team 3 complete → 42/43 tests (98%)
- [ ] Team 4 complete → 43/43 tests (100%)
- [ ] Team 5 complete → Verified + documented

---

## Agent Spawn Commands

### Spawn Team 1 (API-Fix Agent)
```bash
# Use Task tool or manual execution
# Estimated: 30 minutes
# Files: messaging.test.ts, pubsub.test.ts
```

### Spawn Team 2 (Message-Delivery Agent)
```bash
# Use Task tool with extended timeout
# Estimated: 4-6 hours
# Files: SignalHubClient.ts, signal-hub handlers
```

### Spawn Team 3 (Discovery Agent)
```bash
# Can spawn in parallel with Team 2
# Estimated: 2-3 hours
# Files: registration.ts, discovery.ts, registry
```

### Spawn Team 4 (Cleanup Agent)
```bash
# Can spawn anytime
# Estimated: 15 minutes
# Files: SignalHubClient.ts disconnect method
```

### Spawn Team 5 (Integration Agent)
```bash
# Spawn after all others complete
# Estimated: 30 minutes
# Files: None (verification only)
```

---

## Fallback & Contingency

### If Team 2 Investigation Takes Too Long
**Threshold:** > 4 hours investigation
**Action:**
1. Create intermediate milestone bead
2. Add temporary workaround to unblock tests
3. Continue investigation in parallel
4. Replace workaround with proper fix later

### If Discovery Fix Requires Durable Object Changes
**Complexity:** Cloudflare-specific debugging
**Action:**
1. Test with in-memory registry first
2. If that works, isolate DO state issue
3. May need to recreate DO instance
4. Escalate to Cloudflare expert if needed

### If Tests Still Fail After All Fixes
**Scenario:** Unexpected issues discovered
**Action:**
1. Team 5 creates new beads for remaining issues
2. Re-categorize failures
3. Run another iteration of fix cycle
4. Update timeline estimate

---

**Team Structure Status:** Ready for execution
**Last Updated:** 2026-02-17T10:47:59Z
**Next Action:** Spawn Team 1 (API-Fix Agent) to begin work
