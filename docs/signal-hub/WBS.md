# Signal Hub - Work Breakdown Structure (WBS)

**Generated:** 2026-02-17
**Project:** Signal Hub Production Readiness
**Based On:** Opus Review Findings (OPUS_REVIEW.md)
**Total Estimated Effort:** 61 hours (optimistic) to 75 hours (realistic)

---

## WBS Overview

```
Signal Hub Production Readiness (61-75h)
├── Phase 1: P0 Critical Fixes (17h)
│   ├── WS1.1: Connection Lifecycle (8h)
│   ├── WS1.2: Resource Protection (10h)
│   └── WS1.3: Performance & Observability (3h)
├── Phase 2: P1 Quality Fixes (16h)
│   ├── WS2.1: Security & Correctness (9h)
│   └── WS2.2: Reliability & Operations (7h)
├── Phase 3: Test Infrastructure (12h)
│   ├── WS3.1: Test Framework Fixes (8h)
│   └── WS3.2: Test Coverage Expansion (4h)
└── Phase 4: P2 DX Improvements (16h) [OPTIONAL]
    ├── WS4.1: Performance Optimizations (7h)
    ├── WS4.2: Developer Experience (5h)
    └── WS4.3: Advanced Features (4h)
```

---

## Phase 1: P0 Critical Fixes (Must-Fix)

**Total Effort:** 17 hours
**Dependencies:** None - can start immediately
**Success Criteria:** All resource exhaustion and data loss scenarios mitigated

### Workstream 1.1: Connection Lifecycle Fixes

**Owner:** Backend Agent
**Duration:** 8 hours
**Dependencies:** None

#### Task 1.1.1: Implement Duplicate Connection Detection
**Effort:** 4 hours
**Priority:** P0
**File:** `services/signal-hub/src/handlers/connection.ts`

**Subtasks:**
1. Add duplicate detection logic to `handleConnect()` (1.5h)
   - Iterate `sessions` map to find existing session with same `actorIdentity`
   - Send `hub:disconnect` with `reason: 'duplicate_connection'` to old WebSocket
   - Close old WebSocket with code 1000
   - Clean up old session's registrations

2. Update session cleanup to handle duplicates (1h)
   - Remove old session from `sessions` map
   - Remove old session from `connections` map
   - Call `cleanupConnection()` for old session

3. Add logging for duplicate detection (0.5h)
   - Structured log: `{ event: 'duplicate_connection', oldSessionId, newSessionId, actorIdentity }`

4. Write tests for duplicate connection (1h)
   - Test: Connect actor A, reconnect actor A, verify old session closed
   - Test: Old session receives `hub:disconnect` before close
   - Test: Messages to old session fail, messages to new session succeed

**Acceptance Criteria:**
- [ ] Reconnecting actor closes previous session
- [ ] Old session receives disconnect notification
- [ ] Registry points to new session only
- [ ] Test: `should handle duplicate connections` passes

---

#### Task 1.1.2: Fix Disconnect Response Ordering
**Effort:** 2 hours
**Priority:** P0
**File:** `services/signal-hub/src/durable-objects/SignalHub.ts`

**Subtasks:**
1. Move disconnect response send before WebSocket close (0.5h)
   - In `routeMessage()` handler for `hub:disconnect`
   - Send `hub:disconnect` acknowledgment BEFORE calling `ws.close()`

2. Handle special case in message routing (1h)
   - Add flag `skipSend` for disconnect handling
   - Or refactor to send response inline

3. Write test for disconnect acknowledgment (0.5h)
   - Test: Client receives `hub:disconnect` ack before WebSocket close event

**Acceptance Criteria:**
- [ ] Disconnect acknowledgment sent before close
- [ ] Client receives ack message
- [ ] Test: `should disconnect gracefully` passes

---

#### Task 1.1.3: Add Connection State Tracking
**Effort:** 2 hours
**Priority:** P0 (nice-to-have, improves observability)
**File:** `services/signal-hub/src/types.ts`

**Subtasks:**
1. Add `connectionState` enum to Session type (0.5h)
   ```typescript
   connectionState: 'connecting' | 'connected' | 'disconnecting' | 'disconnected'
   ```

2. Update handlers to set connection state (1h)
   - `handleWebSocketConnection`: set to 'connecting'
   - `handleConnect`: set to 'connected'
   - `handleDisconnect`: set to 'disconnecting' then 'disconnected'

3. Add state validation (0.5h)
   - Reject messages (except `hub:connect`) in 'connecting' state
   - Log state transitions

**Acceptance Criteria:**
- [ ] Session has explicit connection state
- [ ] State transitions logged
- [ ] Invalid state transitions rejected

---

### Workstream 1.2: Resource Protection

**Owner:** Backend Agent
**Duration:** 10 hours
**Dependencies:** None

#### Task 1.2.1: Schedule Alarm for TTL Cleanup
**Effort:** 2 hours
**Priority:** P0
**File:** `services/signal-hub/src/durable-objects/SignalHub.ts`

**Subtasks:**
1. Schedule initial alarm in constructor (0.5h)
   ```typescript
   await this.ctx.storage.setAlarm(Date.now() + 60000); // 1 minute after start
   ```

2. Update alarm handler to reschedule (0.5h)
   ```typescript
   async alarm(): Promise<void> {
       // ... existing cleanup logic ...
       await this.ctx.storage.setAlarm(Date.now() + 300000); // 5 minutes
   }
   ```

3. Add logging for cleanup (0.5h)
   - Log: actors removed, next alarm time

4. Write test for alarm scheduling (0.5h)
   - Mock storage.setAlarm
   - Verify alarm scheduled in constructor
   - Verify alarm rescheduled after firing

**Acceptance Criteria:**
- [ ] Alarm scheduled on DO initialization
- [ ] Alarm fires every 5 minutes
- [ ] Expired actors removed from registry
- [ ] Alarm reschedules itself

---

#### Task 1.2.2: Add Raw Message Size Check Before Parse
**Effort:** 2 hours
**Priority:** P0
**File:** `services/signal-hub/src/durable-objects/SignalHub.ts`

**Subtasks:**
1. Add size check at top of `webSocketMessage()` (0.5h)
   ```typescript
   const rawSize = typeof message === 'string' ? message.length : message.byteLength;
   if (rawSize > this.env.MESSAGE_SIZE_LIMIT) {
       throw new HubError('message_too_large', 'Message exceeds size limit before parsing');
   }
   ```

2. Move size check before JSON.parse (0.5h)
   - Remove existing check from handlers
   - Consolidate at entry point

3. Add structured error response (0.5h)
   - Send `hub:error` with `code: 'message_too_large'`
   - Include `maxSize` and `actualSize` in error

4. Write tests (0.5h)
   - Test: 10MB message rejected before parse
   - Test: Error response includes size info

**Acceptance Criteria:**
- [ ] Message size checked before JSON.parse
- [ ] Large messages rejected with clear error
- [ ] Memory exhaustion attack mitigated

---

#### Task 1.2.3: Enforce Broadcast Limit or Implement Async Queue
**Effort:** 4 hours
**Priority:** P0
**File:** `services/signal-hub/src/handlers/messaging.ts`

**Option A: Simple Rejection (2h - Recommended for Phase 1)**

**Subtasks:**
1. Add broadcast limit check (0.5h)
   ```typescript
   if (targets.length > 100) {
       throw new HubError('broadcast_too_large',
           `Broadcast to ${targets.length} actors exceeds limit of 100. Use Cloudflare Queues for async broadcast.`);
   }
   ```

2. Return actionable error (0.5h)
   - Error includes: `recipientCount`, `limit`, `suggestion`

3. Document async broadcast pattern (0.5h)
   - Update PROTOCOL.md Section 8.2
   - Add example using Cloudflare Queues

4. Write tests (0.5h)
   - Test: Broadcast to 101 actors returns error
   - Test: Error message includes limit and suggestion

**Option B: Cloudflare Queues Integration (6h - Defer to Phase 2)**

**Subtasks:**
1. Add Queue binding to wrangler.toml (0.5h)
2. Implement async broadcast producer (2h)
3. Implement consumer worker (2h)
4. Add monitoring and dead letter queue (1h)
5. Update tests (0.5h)

**Decision:** Choose Option A for Phase 1, implement Option B in Phase 2 if needed.

**Acceptance Criteria (Option A):**
- [ ] Broadcast to >100 actors rejected with error
- [ ] Error message explains workaround
- [ ] CPU exhaustion attack prevented

---

#### Task 1.2.4: Add Defense-in-Depth for Message Validation
**Effort:** 2 hours
**Priority:** P0
**File:** `services/signal-hub/src/durable-objects/SignalHub.ts`

**Subtasks:**
1. Add JSON parse error handling (1h)
   ```typescript
   try {
       msg = JSON.parse(rawMessage);
   } catch (e) {
       // Send error, close connection for malformed JSON
   }
   ```

2. Add message structure validation (0.5h)
   - Verify `type`, `from`, `to` fields exist
   - Reject messages missing required fields

3. Add tests (0.5h)
   - Test: Malformed JSON closes connection
   - Test: Missing `type` field rejected

**Acceptance Criteria:**
- [ ] Malformed JSON handled gracefully
- [ ] Connection closed on repeated invalid messages
- [ ] Invalid message structure rejected

---

### Workstream 1.3: Performance & Observability

**Owner:** Backend Agent
**Duration:** 3 hours
**Dependencies:** None

#### Task 1.3.1: Remove Verbose Debug Logging
**Effort:** 2 hours
**Priority:** P0
**Files:** Multiple handlers

**Subtasks:**
1. Audit all `console.log` statements (0.5h)
   - Find all `Array.from(registry.keys())` calls
   - Find all `Array.from(connections.keys())` calls
   - Identify O(N) logging operations

2. Remove or gate behind DEBUG flag (1h)
   - Remove: `console.log('[handleSend] Registry has', registry.size, 'actors:', Array.from(registry.keys()))`
   - Keep size metrics: `console.log('[handleSend] Registry size:', registry.size)`
   - Gate verbose logs: `if (DEBUG) { ... }`

3. Add DEBUG environment variable support (0.5h)
   ```typescript
   const DEBUG = this.env.DEBUG === 'true' || this.env.DEBUG === 'signal-hub';
   ```

**Acceptance Criteria:**
- [ ] No O(N) logging in production
- [ ] Log volume reduced by >90%
- [ ] DEBUG flag enables verbose logs when needed

---

#### Task 1.3.2: Add Basic Metrics Collection
**Effort:** 1 hour
**Priority:** P0 (nice-to-have)
**File:** `services/signal-hub/src/durable-objects/SignalHub.ts`

**Subtasks:**
1. Add per-minute counters (0.5h)
   ```typescript
   messagesPerMinute: number
   connectionsPerMinute: number
   currentMinute: number // Math.floor(Date.now() / 60000)
   ```

2. Reset counters each minute (0.5h)
   - Check if minute changed on each message
   - Reset counters, log previous minute's stats

**Acceptance Criteria:**
- [ ] Per-minute message rate tracked
- [ ] Per-minute connection rate tracked
- [ ] Metrics logged for monitoring

---

## Phase 2: P1 Quality Fixes (Should-Fix)

**Total Effort:** 16 hours
**Dependencies:** Phase 1 complete
**Success Criteria:** Rate limiting active, pub/sub correct, structured logging operational

### Workstream 2.1: Security & Correctness

**Owner:** Backend Agent
**Duration:** 9 hours
**Dependencies:** Phase 1 complete (especially Task 1.1.1 for session tracking)

#### Task 2.1.1: Implement Per-Session Rate Limiting
**Effort:** 4 hours
**Priority:** P1
**Files:** `services/signal-hub/src/types.ts`, `SignalHub.ts`

**Subtasks:**
1. Add TokenBucket to Session type (0.5h)
   ```typescript
   rateLimitBucket: { tokens: number; lastRefill: number; capacity: number; refillRate: number }
   ```

2. Initialize bucket on session creation (0.5h)
   - Default: 10 tokens, 1 token/sec refill

3. Add rate check to `routeMessage()` (1h)
   ```typescript
   if (!consumeToken(session.rateLimitBucket)) {
       return createRateLimitedMessage(session.rateLimitBucket);
   }
   ```

4. Implement `hub:rate_limited` response (1h)
   - Include `retryAfter` (seconds until token available)

5. Write tests (1h)
   - Test: 11th message in 1 second returns rate limited
   - Test: After waiting, tokens refilled
   - Test: Different sessions have independent buckets

**Acceptance Criteria:**
- [ ] Rate limiting active per session
- [ ] `hub:rate_limited` returned when exceeded
- [ ] DoS protection verified in tests
- [ ] Rate configurable via env var

---

#### Task 2.1.2: Fix Unsubscribe to Be Topic-Specific
**Effort:** 3 hours
**Priority:** P1
**File:** `services/signal-hub/src/handlers/pubsub.ts`

**Subtasks:**
1. Add subscription tracking map (1h)
   ```typescript
   // In SignalHub class
   private subscriptionIndex: Map<string, { topic: string; actorAddress: CanonicalAddress }>
   ```

2. Update `handleSubscribe` to record subscription ID (0.5h)
   - Store subscriptionId -> { topic, actorAddress } mapping

3. Update `handleUnsubscribe` to use subscription ID (1h)
   - Look up subscription by ID
   - Remove only that subscription
   - Remove from topic's subscriber set

4. Write tests (0.5h)
   - Test: Subscribe to A, B, C; unsubscribe A; still receive B, C
   - Test: Invalid subscription ID returns error

**Acceptance Criteria:**
- [ ] Unsubscribe removes only specified subscription
- [ ] Multi-topic subscriptions work correctly
- [ ] Test: `should stop receiving messages after unsubscribe` passes

---

#### Task 2.1.3: Resolve Target Address Ambiguity
**Effort:** 2 hours
**Priority:** P1
**File:** `services/signal-hub/src/handlers/messaging.ts`

**Subtasks:**
1. Choose single source of truth for target (0.5h)
   - **Decision:** Use `msg.to` as target (consistent with SharedMessage)
   - Remove `payload.to` fallback

2. Update `handleSend` logic (0.5h)
   ```typescript
   const targetAddress = msg.to; // No fallback
   if (!targetAddress || targetAddress === SIGNAL_HUB_ADDRESS) {
       throw new HubError('invalid_target', 'Target actor address required in msg.to field');
   }
   ```

3. Update PROTOCOL.md documentation (0.5h)
   - Clarify: target is always `msg.to`
   - Remove references to `payload.to`

4. Update tests (0.5h)
   - Test: Missing `msg.to` returns error
   - Test: `payload.to` ignored (if present)

**Acceptance Criteria:**
- [ ] Single source of truth for target address
- [ ] Clear error when target missing
- [ ] Documentation consistent with implementation

---

### Workstream 2.2: Reliability & Operations

**Owner:** Backend Agent
**Duration:** 7 hours
**Dependencies:** Phase 1 complete

#### Task 2.2.1: Reduce Heartbeat Interval to 20s
**Effort:** 1 hour
**Priority:** P1
**Files:** Client implementations (SEAG, browser)

**Subtasks:**
1. Update SEAG client heartbeat interval (0.25h)
   - Change from 25000ms to 20000ms

2. Update browser client heartbeat interval (0.25h)
   - Change from 25000ms to 20000ms

3. Update PROTOCOL.md documentation (0.25h)
   - Document: heartbeat every 20s (10s margin vs 30s hibernation)

4. Write test (0.25h)
   - Test: Heartbeat sent every 20s

**Acceptance Criteria:**
- [ ] Both clients send heartbeat every 20s
- [ ] 10s safety margin vs hibernation
- [ ] Documentation updated

---

#### Task 2.2.2: Implement Structured Logging
**Effort:** 6 hours
**Priority:** P1
**Files:** All handlers, SignalHub.ts

**Subtasks:**
1. Create logging utility (1h)
   ```typescript
   function logEvent(event: string, data: Record<string, any>, level: 'info' | 'warn' | 'error' = 'info') {
       console.log(JSON.stringify({
           timestamp: new Date().toISOString(),
           level,
           event,
           ...data,
       }));
   }
   ```

2. Replace all console.log statements (3h)
   - Connection lifecycle events
   - Message routing events
   - Error events
   - Registry operations

3. Add standard fields to all logs (1h)
   - `sessionId`
   - `actorIdentity` (if available)
   - `traceId` (from message metadata)
   - `correlationId`

4. Write log parsing tests (1h)
   - Test: All logs valid JSON
   - Test: Required fields present

**Acceptance Criteria:**
- [ ] All logs JSON-formatted
- [ ] Logs queryable in Cloudflare analytics
- [ ] Standard fields on all log entries
- [ ] Log levels appropriate (info/warn/error)

---

## Phase 3: Test Infrastructure Repair

**Total Effort:** 12 hours
**Dependencies:** None - can run parallel with Phase 1
**Success Criteria:** 0 failing tests, stable CI/CD

### Workstream 3.1: Test Framework Fixes

**Owner:** Test Agent
**Duration:** 8 hours
**Dependencies:** None

#### Task 3.1.1: Fix Module Import Error
**Effort:** 1 hour
**Priority:** P0
**File:** `tests/integration/signal-hub/errors.test.ts.disabled`

**Subtasks:**
1. Investigate import path error (0.5h)
   - Error: Cannot find module '../../../../ugs/src/messaging/signal-hub/client'
   - Find correct path or update package structure

2. Fix import statement (0.25h)
   - Update path or create barrel export

3. Re-enable test file (0.25h)
   - Rename from `.disabled` to `.test.ts`
   - Verify test runs

**Acceptance Criteria:**
- [ ] Test file loads without module errors
- [ ] Tests run (pass or fail is separate issue)

---

#### Task 3.1.2: Investigate and Fix Broadcast Timeouts
**Effort:** 3 hours
**Priority:** P0
**File:** `tests/integration/signal-hub/broadcast.test.ts`

**Subtasks:**
1. Add debug logging to broadcast handler (0.5h)
   - Log: actor addresses being compared for `excludeSelf`
   - Log: target actors found
   - Log: messages sent to each target

2. Investigate address format mismatch (1h)
   - Hypothesis: `msg.from` canonical address doesn't match registered address
   - Compare `@(local/seag-1)` vs `@(seag/seag-1)` formatting
   - Verify `toCanonicalAddress()` consistency

3. Fix root cause (1h)
   - Option 1: Normalize addresses before comparison
   - Option 2: Use consistent addressing in tests

4. Verify all 5 broadcast tests pass (0.5h)

**Acceptance Criteria:**
- [ ] All 5 broadcast timeout tests pass
- [ ] Root cause documented
- [ ] Fix does not break other tests

---

#### Task 3.1.3: Investigate and Fix Pub/Sub Timeouts
**Effort:** 3 hours
**Priority:** P0
**File:** `tests/integration/signal-hub/pubsub.test.ts`

**Subtasks:**
1. Add debug logging to pub/sub handlers (0.5h)
   - Log: subscription map state
   - Log: actor address lookups
   - Log: messages sent to subscribers

2. Investigate subscription lookup mismatch (1h)
   - Hypothesis: Actor address in subscriptions doesn't match registry key
   - Compare addresses in both maps

3. Fix root cause (1h)
   - Ensure `handleSubscribe` uses same address format as `handleRegister`

4. Verify all 7 pub/sub tests pass (0.5h)

**Acceptance Criteria:**
- [ ] All 7 pub/sub timeout tests pass
- [ ] Subscription lookup reliable
- [ ] Multiple subscribers work correctly

---

#### Task 3.1.4: Fix Message Ordering Test Race Condition
**Effort:** 1 hour
**Priority:** P0
**File:** `tests/integration/signal-hub/messaging.test.ts`

**Subtasks:**
1. Investigate message listener timing (0.5h)
   - Hypothesis: Messages arrive before listener registered
   - Add message collector pattern

2. Fix test setup (0.5h)
   - Register listener before sending messages
   - Use promise/await for message collection

**Acceptance Criteria:**
- [ ] Ordering test passes consistently
- [ ] All 10 messages received
- [ ] Order verified

---

### Workstream 3.2: Test Coverage Expansion

**Owner:** Test Agent
**Duration:** 4 hours
**Dependencies:** Workstream 3.1 complete (framework must work)

#### Task 3.2.1: Add Handler Unit Tests
**Effort:** 2 hours
**Priority:** P1
**Files:** Create `services/signal-hub/src/handlers/__tests__/`

**Subtasks:**
1. Create test files (0.5h)
   - `registration.test.ts`
   - `messaging.test.ts`
   - `pubsub.test.ts`

2. Write registration tests (0.5h)
   - Register at capacity
   - Register with expired TTL
   - Duplicate registration increments version

3. Write messaging tests (0.5h)
   - Send to expired actor
   - Send to missing WebSocket
   - Broadcast with excludeSelf

4. Write pub/sub tests (0.5h)
   - Publish to zero subscribers
   - Publish with disconnected subscriber

**Acceptance Criteria:**
- [ ] Handler code coverage >80%
- [ ] Edge cases from Opus review tested

---

#### Task 3.2.2: Add Reconnection Scenario Tests
**Effort:** 2 hours
**Priority:** P1
**File:** `tests/integration/signal-hub/reconnection.test.ts`

**Subtasks:**
1. Create reconnection test file (0.5h)

2. Write reconnection tests (1.5h)
   - Test: Client reconnects after server closes WebSocket
   - Test: Actors re-register on reconnect
   - Test: Subscriptions re-established on reconnect
   - Test: Concurrent reconnection (10 clients simultaneously)

**Acceptance Criteria:**
- [ ] Reconnection happy path verified
- [ ] Exponential backoff tested
- [ ] State restored after reconnect

---

## Phase 4: P2 DX Improvements (Optional)

**Total Effort:** 16 hours
**Dependencies:** Phase 1 & 2 complete
**Success Criteria:** Improved performance and developer experience

### Workstream 4.1: Performance Optimizations

**Owner:** Backend Agent
**Duration:** 7 hours
**Dependencies:** Phase 1 & 2 complete

#### Task 4.1.1: Add Inverse Index for Subscription Cleanup
**Effort:** 3 hours
**Priority:** P2
**File:** `services/signal-hub/src/durable-objects/SignalHub.ts`

**Subtasks:**
1. Add inverse index to SignalHub (0.5h)
   ```typescript
   private actorSubscriptions: Map<CanonicalAddress, Set<string>> // actor -> topics
   ```

2. Update handleSubscribe to maintain index (0.5h)
   - Add topic to actor's subscription set

3. Update cleanupSubscriptions to use index (1h)
   - O(S) where S = subscriptions per actor (typically small)
   - Instead of O(T) where T = total topics

4. Write performance tests (1h)
   - Test: Disconnect with 100 topics completes <10ms
   - Test: Index stays consistent with subscriptions map

**Acceptance Criteria:**
- [ ] Cleanup is O(subscriptions) not O(topics)
- [ ] Disconnect latency improved
- [ ] Index consistency verified

---

#### Task 4.1.2: Optimize Broadcast JSON.stringify
**Effort:** 4 hours
**Priority:** P2
**File:** `services/signal-hub/src/handlers/messaging.ts`

**Subtasks:**
1. Stringify payload once (1h)
   ```typescript
   const basePayload = JSON.stringify(broadcastMessage.payload);
   for (const target of targets) {
       const msg = `{"id":"${uuid()}","type":"hub:broadcast","to":"${target}","payload":${basePayload}}`;
       ws.send(msg);
   }
   ```

2. Benchmark before/after (1h)
   - Measure: 100 actors, 10KB payload
   - Measure: CPU time, memory allocations

3. Handle edge cases (1h)
   - Special characters in addresses
   - Very large payloads (>100KB)

4. Write tests (1h)
   - Test: Broadcast to 100 actors completes <50ms
   - Test: All recipients receive identical payload

**Acceptance Criteria:**
- [ ] Broadcast CPU usage reduced by >50%
- [ ] Memory allocations reduced
- [ ] Functionally identical to original

---

### Workstream 4.2: Developer Experience

**Owner:** Client Agent
**Duration:** 5 hours
**Dependencies:** Phase 1 & 2 complete

#### Task 4.2.1: Enhance Error Messages with Resolution Steps
**Effort:** 2 hours
**Priority:** P2
**Files:** `services/signal-hub/src/types.ts`, handlers

**Subtasks:**
1. Create error message templates (0.5h)
   ```typescript
   const ERROR_MESSAGES = {
       unknown_actor: (address: string) => ({
           message: `Actor ${address} not registered`,
           resolution: 'The actor may not have called hub:register, or its registration may have expired. Call hub:discover to check if the actor exists.',
           actions: ['hub:discover', 'hub:list_actors'],
       }),
   };
   ```

2. Update all error creation sites (1h)
   - Add resolution steps to each error

3. Write tests (0.5h)
   - Test: Error messages include resolution

**Acceptance Criteria:**
- [ ] All errors include resolution steps
- [ ] Users know how to fix issues

---

#### Task 4.2.2: Add Client sendMessage() Convenience API
**Effort:** 3 hours
**Priority:** P2
**Files:** Client libraries (SEAG, browser)

**Subtasks:**
1. Add sendMessage() to SEAG client (1h)
   ```typescript
   async sendMessage(to: string, type: string, data: any): Promise<void> {
       return this.send({
           to,
           type: 'hub:send',
           payload: { type, data },
       });
   }
   ```

2. Add sendMessage() to browser client (1h)
   - Same signature

3. Update documentation and examples (0.5h)
   - Show new API in README
   - Update tests to use new API

4. Write tests (0.5h)
   - Test: sendMessage() constructs correct wire format

**Acceptance Criteria:**
- [ ] Convenience API available in both clients
- [ ] Flat payload construction hidden
- [ ] Tests use new API

---

### Workstream 4.3: Advanced Features

**Owner:** Backend Agent
**Duration:** 4 hours
**Dependencies:** Phase 1 & 2 complete

#### Task 4.3.1: Implement hub:refresh_token
**Effort:** 4 hours
**Priority:** P2
**Files:** `services/signal-hub/src/handlers/auth.ts` (new file)

**Subtasks:**
1. Create auth handler file (0.5h)

2. Implement handleRefreshToken (1.5h)
   - Accept current JWT
   - Validate current token
   - Issue new token with extended expiration
   - Return `hub:token_refreshed` with new token

3. Update PROTOCOL.md (0.5h)
   - Document refresh flow
   - Specify when to refresh (e.g., at 50% TTL)

4. Write tests (1.5h)
   - Test: Valid token refreshed successfully
   - Test: Expired token rejected
   - Test: New token has extended expiration

**Acceptance Criteria:**
- [ ] Token refresh implemented
- [ ] Long-lived sessions supported
- [ ] Documented in protocol

---

## Task Dependencies Graph

```
┌─────────────────────────────────────────────────────────┐
│ START                                                   │
└───────┬─────────────────────────────────────────────────┘
        │
        ├──────────────────────────────────────┐
        │                                      │
        ▼                                      ▼
┌───────────────┐                    ┌──────────────────┐
│ Phase 1: P0   │                    │ Phase 3: Tests   │
│ (Parallel)    │                    │ (Parallel)       │
└───┬───┬───┬───┘                    └────┬──────┬──────┘
    │   │   │                             │      │
    │   │   │                             │      │
    │   │   └──────────┐                  │      │
    │   │              │                  │      │
    ▼   ▼              ▼                  ▼      ▼
  WS1.1 WS1.2       WS1.3              WS3.1  WS3.2
    │   │              │                  │      │
    │   │              │                  │      │
    └───┴──────────────┴──────────────────┴──────┤
                                                  │
                                                  ▼
                                         ┌────────────────┐
                                         │ Phase 1 & 3    │
                                         │ Complete       │
                                         └────────┬───────┘
                                                  │
                                                  ▼
                                         ┌────────────────┐
                                         │ Phase 2: P1    │
                                         └────┬──────┬────┘
                                              │      │
                                              ▼      ▼
                                           WS2.1  WS2.2
                                              │      │
                                              └──────┤
                                                     │
                                                     ▼
                                            ┌────────────────┐
                                            │ Phase 2        │
                                            │ Complete       │
                                            └────────┬───────┘
                                                     │
                                                     ▼
                                            ┌────────────────┐
                                            │ Phase 4: P2    │
                                            │ (Optional)     │
                                            └────┬──────┬────┘
                                                 │      │
                                                 ▼      ▼
                                              WS4.1  WS4.2  WS4.3
                                                 │      │      │
                                                 └──────┴──────┤
                                                                │
                                                                ▼
                                                       ┌────────────────┐
                                                       │ COMPLETE       │
                                                       └────────────────┘
```

---

## Resource Allocation

### Backend Agent Tasks
- Phase 1: WS1.1, WS1.2, WS1.3 (17h)
- Phase 2: WS2.1, WS2.2 (16h)
- Phase 4: WS4.1, WS4.3 (11h)
- **Total:** 44 hours

### Test Agent Tasks
- Phase 3: WS3.1, WS3.2 (12h)
- **Total:** 12 hours

### Client Agent Tasks
- Phase 4: WS4.2 (5h)
- **Total:** 5 hours

### Coordination Overhead
- Daily standups (30min × 10 days = 5h)
- Code reviews (2h per phase × 4 phases = 8h)
- Integration testing (3h)
- **Total:** 16 hours

---

## Effort Summary

| Phase | Workstream | Effort | Owner |
|-------|-----------|--------|-------|
| **Phase 1: P0** | | **17h** | |
| | WS1.1: Connection Lifecycle | 8h | Backend |
| | WS1.2: Resource Protection | 10h | Backend |
| | WS1.3: Performance & Observability | 3h | Backend |
| **Phase 2: P1** | | **16h** | |
| | WS2.1: Security & Correctness | 9h | Backend |
| | WS2.2: Reliability & Operations | 7h | Backend |
| **Phase 3: Tests** | | **12h** | |
| | WS3.1: Test Framework Fixes | 8h | Test |
| | WS3.2: Test Coverage Expansion | 4h | Test |
| **Phase 4: P2** | | **16h** | |
| | WS4.1: Performance Optimizations | 7h | Backend |
| | WS4.2: Developer Experience | 5h | Client |
| | WS4.3: Advanced Features | 4h | Backend |
| **Coordination** | | **16h** | All |
| **TOTAL** | | **77h** | |

---

## Acceptance Criteria Summary

### Phase 1 Complete
- [ ] All 6 P0 items implemented
- [ ] No known resource exhaustion vectors
- [ ] Duplicate connections handled correctly
- [ ] TTL cleanup operational
- [ ] Message size validated before parse
- [ ] Broadcast limit enforced
- [ ] Debug logging removed from hot paths

### Phase 2 Complete
- [ ] All 5 P1 items implemented
- [ ] Rate limiting active (10 msg/sec per session)
- [ ] Unsubscribe topic-specific
- [ ] Target address unambiguous
- [ ] Heartbeat 20s interval
- [ ] Structured JSON logging

### Phase 3 Complete
- [ ] 0 failing tests (was 13)
- [ ] Test suite stable (no flakes)
- [ ] Handler code coverage >80%
- [ ] Reconnection scenarios tested

### Phase 4 Complete (Optional)
- [ ] Subscription cleanup optimized
- [ ] Broadcast stringify optimized
- [ ] Error messages actionable
- [ ] Client convenience API available
- [ ] Token refresh implemented

---

**Document Owner:** Project Manager
**Next Review:** After Phase 1 completion
**Last Updated:** 2026-02-17
