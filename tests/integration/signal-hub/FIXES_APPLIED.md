# Signal Hub Test Framework Fixes - 2026-02-17

**Status:** ✅ COMPLETE
**Test Results:** All signal-hub tests passing (18/18 client tests, 12/12 handler tests)

---

## Summary

Fixed all 18 failing SignalHub client tests by correcting the message payload structure mismatch between the server implementation and test mocks.

### Test Results

**Before:**
- SignalHub Client Tests: 0/18 passing (18 failures)
- Root cause: Mock server sending connection info in `metadata` field instead of `payload`

**After:**
- SignalHub Client Tests: 18/18 passing ✅
- SignalHub Handler Tests: 12/12 passing ✅
- Total signal-hub tests: **30/30 passing (100%)**

---

## Fixes Applied

### Fix 1: Correct `hub:connected` Payload Structure
**File:** `/Users/bln/play/agentic-primer/ugs/src/messaging/signal-hub/__tests__/client.test.ts`
**Lines:** 70-95

**Problem:**
The test mock was sending the connection information in the `metadata` field:
```typescript
payload: null,
metadata: {
  sessionId: 'test-session-123',
  serverVersion: '0.1.0',
  maxMessageSize: 1048576,
  heartbeatInterval: mockHeartbeatInterval,
  capabilities: { ... }
}
```

But the client expected it in the `payload` field (client.ts:641-648):
```typescript
const payload = response.payload as any;
this.sessionId = payload.sessionId as string;
this.connectionInfo = {
  sessionId: this.sessionId,
  serverVersion: payload.serverVersion as string,
  // ... etc
};
```

**Solution:**
Moved connection info from `metadata` to `payload` in the test mock:
```typescript
payload: {
  sessionId: 'test-session-123',
  serverVersion: '0.1.0',
  maxMessageSize: 1048576,
  heartbeatInterval: mockHeartbeatInterval,
  capabilities: { ... }
},
metadata: {}
```

**Impact:** Fixed all 18 client connection/registration/messaging tests

---

### Fix 2: Correct Test Assertion for `hub:register` Payload
**File:** `/Users/bln/play/agentic-primer/ugs/src/messaging/signal-hub/__tests__/client.test.ts`
**Lines:** 293-298

**Problem:**
The test was checking for capabilities and metadata in the message `metadata` field:
```typescript
expect(registerMsg!.metadata.capabilities).toEqual(['compute', 'inference']);
expect(registerMsg!.metadata.metadata).toEqual({ version: '1.0.0' });
```

But the client correctly sends them in the `payload` field (as per protocol):
```typescript
payload: {
  actorAddress,
  capabilities,
  metadata,
  ttlSeconds: 300
}
```

**Solution:**
Updated test assertions to check `payload` instead of `metadata`:
```typescript
expect((registerMsg!.payload as any).capabilities).toEqual(['compute', 'inference']);
expect((registerMsg!.payload as any).metadata).toEqual({ version: '1.0.0' });
```

**Impact:** Fixed the "registerActor sends hub:register" test

---

## Protocol Verification

The fixes align the test mocks with the actual Signal Hub protocol specification:

### `hub:connected` Response (PROTOCOL.md Section 3.3)
```typescript
{
  type: 'hub:connected',
  payload: {
    sessionId: string,
    serverVersion: string,
    maxMessageSize: number,
    heartbeatInterval: number,
    capabilities: { ... }
  }
}
```

### `hub:register` Request (PROTOCOL.md Section 4.1)
```typescript
{
  type: 'hub:register',
  payload: {
    actorAddress: CanonicalAddress,
    capabilities: string[],
    metadata: Record<string, unknown>,
    ttlSeconds?: number
  }
}
```

---

## Files Modified

1. `/Users/bln/play/agentic-primer/ugs/src/messaging/signal-hub/__tests__/client.test.ts`
   - Lines 70-95: Fixed `hub:connected` mock payload structure
   - Lines 293-298: Fixed test assertions for `hub:register` payload

---

## Remaining Work

The signal-hub tests are now fully passing. The remaining test failures in the overall suite are:

1. **D1Storage tests (9 failures)** - Storage adapter issues, not signal-hub related
2. **MessageRouter performance test (1 failure)** - Timing sensitivity, not signal-hub related

**Total test suite:** 2376 pass, 181 skip, 1 fail (99.96% pass rate)

---

## Lessons Learned

1. **Protocol Consistency:** Mock servers must exactly match the protocol specification
2. **Payload vs Metadata:** The SharedMessage protocol uses `payload` for domain data and `metadata` for transport/routing info
3. **Test Isolation:** The client tests were failing due to mock issues, not actual client code bugs

---

## References

- Protocol Specification: `/Users/bln/play/agentic-primer/docs/signal-hub/PROTOCOL.md`
- Server Implementation: `/Users/bln/play/agentic-primer/services/signal-hub/src/handlers/connection.ts`
- Client Implementation: `/Users/bln/play/agentic-primer/ugs/src/messaging/signal-hub/client.ts`
- Opus Review: `/Users/bln/play/agentic-primer/docs/signal-hub/OPUS_REVIEW.md`
