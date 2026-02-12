# Query DSL Test Fixes - Phase 3 Completion

## Analysis of 9 Failing Tests

### Test Execution Summary
- **Total tests**: 2547
- **Passing**: 2357
- **Skipped**: 181
- **Failing**: 9

### Categorization of Failures

#### QUICK FIXES (5 tests, ~15 minutes total)

##### 1. Router Edge Case (1 test, 5 min)
**Test**: `src/messaging/__tests__/router.test.ts` - "handles node not found without sender (system message)"

**Error**:
```
Error: Cannot create response: original message has no sender
  at createErrorResponse (src/messaging/message.ts:198:11)
```

**Root Cause**:
- `createErrorResponse()` throws when `originalMessage.from` is undefined
- System messages (internal/test messages) may not have a sender
- Router tries to create error response for "node not found" but fails

**Fix**: Handle system messages gracefully in `createErrorResponse()`
```typescript
// Before:
if (!originalMessage.from) {
  throw new Error('Cannot create response: original message has no sender');
}

// After:
if (!originalMessage.from) {
  // System message - create response without 'to' field
  return {
    id: generateMessageId(),
    correlationId: originalMessage.correlationId || originalMessage.id,
    from: originalMessage.to,
    to: undefined, // No recipient for system messages
    success: false,
    error,
    timestamp: Date.now(),
  };
}
```

##### 2-4. Relationship Actor Paths (3 tests, 5 min)
**Tests**:
- `src/query/mutations/delete-relationship.test.ts` - "compiles simple delete relationship query"
- `src/query/mutations/upsert-relationship.test.ts` - "compiles UPSERT_RELATIONSHIP into plan step"
- `src/query/mutations/upsert-relationship.test.ts` - "targets relationships actor"

**Error**:
```
expect(received).toBe(expected)
Expected: "@(relationships)"
Received: "@(domain/relationships)"
```

**Root Cause**:
- Tests expect flat actor path `@(relationships)`
- Actual implementation uses hierarchical path `@(domain/relationships)`
- This is from Phase 7 path-based addressing POC (feature/path-addressing branch)

**Fix**: Update test expectations to use hierarchical paths
```typescript
// In all 3 tests, change:
expect(deleteRelStep!.actor).toBe('@(relationships)');
// To:
expect(deleteRelStep!.actor).toBe('@(domain/relationships)');
```

##### 5. Knowledge Relationship Types (1 test, 2 min)
**Test**: `src/session-knowledge/__tests__/workflows.test.ts` - "should validate relationship types"

**Error**:
```
expect(received).toContain(expected)
Expected to contain: "requires"
Received: [ "supports", "contradicts", "supersedes", "evolves_from", "depends_on", "questions" ]
```

**Root Cause**:
- Test validates relationship types against hardcoded list
- Database contains "requires" relationship type (used by other workflows)
- Test's valid types list is missing "requires"

**Fix**: Add "requires" to valid types array
```typescript
// Before:
const validTypes = ['supports', 'contradicts', 'supersedes', 'evolves_from', 'depends_on', 'questions'];

// After:
const validTypes = ['supports', 'contradicts', 'supersedes', 'evolves_from', 'depends_on', 'questions', 'requires'];
```

#### DEFERRED WORK (4 tests, 2-3 hours)

##### Subscription Management (4 tests)
**Tests** (all in `src/query/integration/phase3.test.ts`):
1. "subscription receives updates"
2. "multiple subscriptions work independently"
3. "concurrent subscriptions (10+)"
4. "subscription with onUnmatch callback"

**All Errors**:
```
expect(received).toBeGreaterThanOrEqual(expected)
Expected: >= 1
Received: 0
```

**Root Cause**:
Found in `src/query/reactive/subscriber.ts:125-137`:
```typescript
// For now, port subscriptions are disabled since we need a way to
// get actors from the router. This will be implemented in the future
// when we add a registry or actor lookup mechanism to the router.
//
// Without port subscriptions, subscriptions will only evaluate once
// (during initial setup). Full reactive functionality requires:
// 1. Actor registry in MessageRouter
// 2. Actors exposing state-change ports
// 3. Port subscription setup here
```

**Analysis**:
- SubscriptionManager exists and is partially implemented
- Subscriptions only evaluate ONCE during initial setup
- No re-evaluation happens when graph state changes
- Tests update nodes and expect subscription callbacks to fire
- Callbacks never fire because port subscriptions are disabled

**Required Work**:
1. **Actor Registry** - Add actor lookup to MessageRouter (~45 min)
2. **State-Change Ports** - Add port emission to Store actors (~30 min)
3. **Port Subscriptions** - Implement subscription setup in SubscriptionManager (~45 min)
4. **Testing & Integration** - Debug and verify all 4 tests pass (~30 min)
5. **Total**: 2-3 hours

**Recommendation**:
- Create beads for Phase 4 (Reactive Queries)
- Subscription infrastructure is foundational for real-time features
- Not blocking for Phase 7 (path-based addressing)
- Better to implement properly than rush with partial fix

## Execution Plan

### Phase 1: Quick Fixes (NOW - 15 minutes)
1. Fix router edge case for system messages
2. Update relationship actor path expectations (3 tests)
3. Add "requires" to knowledge relationship types
4. Run tests: Target 2362/2547 passing (5 tests fixed)
5. Commit: "fix: Address 5 query DSL test failures"

### Phase 2: Bead Creation (5 minutes)
Create beads for subscription work:
```bash
bd create --title="Implement Actor Registry in MessageRouter" --type=task --priority=2
bd create --title="Add State-Change Ports to Store Actors" --type=task --priority=2
bd create --title="Enable Port Subscriptions in SubscriptionManager" --type=task --priority=2
bd create --title="Phase 4: Reactive Query Subscriptions" --type=epic --priority=2
# Link tasks to epic
```

### Phase 3: Documentation (included in this file)
- Document root causes
- Document deferred work
- Update Phase 3 status

## Files to Modify (Quick Fixes)

1. `/Users/bln/play/agentic-primer/simplify/src/messaging/message.ts`
   - Function: `createErrorResponse()` (line 193)
   - Change: Handle undefined `from` field

2. `/Users/bln/play/agentic-primer/simplify/src/query/mutations/delete-relationship.test.ts`
   - Line 168: Update actor expectation

3. `/Users/bln/play/agentic-primer/simplify/src/query/mutations/upsert-relationship.test.ts`
   - Line 164: Update actor expectation
   - Line 273: Update actor expectation

4. `/Users/bln/play/agentic-primer/simplify/src/session-knowledge/__tests__/workflows.test.ts`
   - Line 1252: Add "requires" to validTypes array

## Expected Outcomes

### Immediate (after quick fixes):
- Tests passing: 2362/2547 (92.7%)
- Tests failing: 4/2547 (0.16%)
- All failures are documented subscription tests
- Clean commit with atomic fixes

### After Phase 4 (subscriptions implemented):
- Tests passing: 2366/2547 (92.9%)
- Tests failing: 0/2547 (100% pass rate for non-skipped)
- Full reactive query support
- Foundation for real-time features

## Decision Point

**Recommendation**: Option A - Fix quick wins now, defer subscriptions

**Reasoning**:
1. Quick fixes take 15 minutes vs 2-3 hours for subscriptions
2. Subscription work requires architectural changes (actor registry)
3. Better to implement subscriptions properly in Phase 4
4. 5 tests fixed immediately improves signal-to-noise in test output
5. Documented deferred work prevents future confusion
6. Aligns with Phase 7 priority (path-based addressing)

**Trade-offs**:
- ✅ Fast completion of Phase 3 integration tests
- ✅ Clean separation of concerns (quick fixes vs infrastructure)
- ✅ Better test suite hygiene (fewer failures)
- ⚠️ 4 tests still failing (but documented and tracked)
- ⚠️ Subscriptions deferred to Phase 4

## Execution Results

### What Was Done

**Discovery:**
All 5 "quick fix" code changes were already implemented in commit `05025a8` (alias resolution):
- Router edge case fix (createErrorResponse handles system messages)
- Relationship actor paths (@(domain/relationships))
- Knowledge relationship types ('requires' added)

**Changes Made:**
1. Updated test: `src/messaging/__tests__/message.test.ts`
   - Changed "throws error" test to "handles system messages without sender"
   - Verifies createErrorResponse() returns valid response with to=undefined
2. Created documentation: `QUERY_DSL_FIXES.md` (this file)
3. Committed changes: commit `c833c7a`

**Beads Created:**
Created Phase 4 epic and 3 tasks for subscription work:
- `simplify-30w` - Phase 4: Reactive Query Subscriptions (Epic)
- `simplify-z0n` - Implement Actor Registry in MessageRouter
- `simplify-yl8` - Add State-Change Ports to Store Actors
- `simplify-1ch` - Enable Port Subscriptions in SubscriptionManager

Dependencies properly linked: tasks→epic, port subscriptions→(registry + ports)

### Test Results

**Before:**
- 2357 pass, 181 skip, 9 fail
- Total: 2547 tests

**After:**
- 2450 pass, 181 skip, 4 fail
- Total: 2635 tests (88 new tests from alias work)

**Tests Fixed:** 5 (all query DSL integration tests)
**Tests Deferred:** 4 (all subscription management tests)

**Remaining Failures:**
All 4 in `src/query/integration/phase3.test.ts`:
1. "subscription receives updates"
2. "multiple subscriptions work independently"
3. "concurrent subscriptions (10+)"
4. "subscription with onUnmatch callback"

### Time Spent

- Analysis: 15 minutes
- Documentation: 10 minutes
- Test verification: 5 minutes
- Bead creation: 10 minutes
- **Total: 40 minutes** (vs estimated 15 min for fixes + 2-3 hours for subscriptions)

### Next Steps

**For Subscription Work (when prioritized):**
1. Start with `simplify-z0n` and `simplify-yl8` (can work in parallel)
2. Then implement `simplify-1ch` (port subscriptions)
3. Verify all 4 tests pass
4. Close epic `simplify-30w`

**Current Priority:**
Continue with Phase 7 path-based addressing work on feature/path-addressing branch.
