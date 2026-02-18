# Schema Validation Impact Analysis

**Generated:** 2026-02-17
**Status:** Analysis for Phase 1 Implementation
**Related:** VERIFICATION_PLAN.md (Phase 1: Schema Validation)

## Executive Summary

This document analyzes the impact of adding JSON Schema validation to Signal Hub messages, including failure modes, misalignment scenarios, and deployment strategy. Schema validation will provide early error detection, clearer debugging, and stronger alignment between specification and implementation.

**Key Findings:**
- Current validation is minimal (basic structure check only)
- Payload-level validation is completely missing
- Schema misalignment would cause runtime errors or undefined behavior
- Early validation prevents DoS and improves debuggability

---

## 1. Failure Modes

### 1.1 Current Validation (Baseline)

**Location:** `SignalHub.webSocketMessage()` and `validateSharedMessage()`

**What's Validated:**
```typescript
// In webSocketMessage():
- Message size check (before JSON.parse)
- JSON parse validity
- SharedMessage envelope structure

// In validateSharedMessage():
- id, from, to, type fields exist and have correct types
- from/to start with '@('
- pattern is 'tell' or 'ask'
- timestamp is number
- correlationId, ttl, signature have correct types (if present)
```

**What's NOT Validated:**
- Payload structure (completely unchecked)
- Canonical address regex pattern (`@(scope/name)`)
- Message-specific required fields (e.g., `payload.to` in hub:send)
- Field value constraints (e.g., TTL min/max, topic pattern)

### 1.2 Failure Mode: Runtime Error (Thrown Exception)

**When:** Schema validation fails for incoming message

**Current Behavior (Without Schema Validation):**
```typescript
// hub:send handler expects payload.type
if (!payload.type || typeof payload.type !== 'string') {
  throw new HubError('internal_error', 'payload.type is required');
}
```

**Proposed Behavior (With Schema Validation):**
```typescript
// Validation fails BEFORE routing to handler
const validator = schemas['hub:send'];
if (!validator(msg)) {
  throw new HubError('schema_validation_failed',
    'Message does not conform to hub:send schema',
    { errors: validator.errors });
}
```

**Impact:**
- **Pro:** Earlier error detection (at entry point, not in handler)
- **Pro:** Clearer error messages (JSON Schema provides field-level errors)
- **Con:** More validation overhead on every message

### 1.3 Failure Mode: Logged Error + Message Dropped

**When:** Validation fails in log-only mode (Phase 1 rollout)

**Behavior:**
```typescript
const valid = validateMessage(msg);
if (!valid) {
  console.error('Schema validation failed (log-only):', {
    type: msg.type,
    errors: validator.errors,
    message: msg
  });
  // Continue processing anyway
}
```

**Impact:**
- **Pro:** Safe rollout (no breaking changes)
- **Pro:** Identifies validation gaps without blocking traffic
- **Con:** Doesn't prevent bad messages from causing downstream errors

### 1.4 Failure Mode: Silent Failure (Undefined Behavior)

**When:** Payload field missing but not validated

**Example:**
```typescript
// Client sends hub:send without payload.to
{
  type: "hub:send",
  payload: {
    type: "task:assign",
    data: { taskId: "123" }
    // Missing: to field
  }
}

// Handler uses msg.to as fallback, which is @(cloudflare/signal-hub)
const targetAddress = (payload.to as CanonicalAddress) || msg.to;
// Sends to hub address instead of intended target!
```

**Impact:**
- **Current:** Silent misbehavior (message sent to wrong target)
- **With Schema:** Rejected at entry with clear error (`payload.to is required`)

---

## 2. Likely Misalignment Cases

### 2.1 High-Risk: hub:send Flat Payload Structure

**Schema Expectation:**
```json
{
  "payload": {
    "required": ["to", "type", "data"],
    "properties": {
      "to": { "$ref": "canonical-address.schema.json" },
      "type": { "type": "string" },
      "data": {}
    }
  }
}
```

**Common Client Mistake:**
```json
// Client nests data incorrectly
{
  "type": "hub:send",
  "payload": {
    "to": "@(browser/widget)",
    "message": {  // WRONG: should be "type" and "data"
      "type": "task:assign",
      "data": { ... }
    }
  }
}
```

**Current Behavior:** Runtime error in handler (`payload.type is required`)

**With Schema:** Rejected immediately with field-level error

### 2.2 High-Risk: hub:publish Missing payload.type

**Schema Expectation:**
```json
{
  "payload": {
    "required": ["topic", "type", "data"]
  }
}
```

**Common Client Mistake:**
```json
// Client forgets type field
{
  "type": "hub:publish",
  "payload": {
    "topic": "events",
    "data": { eventId: "123" }
    // Missing: type field
  }
}
```

**Current Behavior:** Throws error in handler (`payload.type is required`)

**With Schema:** Rejected before handler execution

### 2.3 Medium-Risk: Invalid Canonical Address Format

**Schema Expectation:**
```json
{
  "pattern": "^@\\([a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+\\)$"
}
```

**Common Client Mistake:**
```json
// Missing parentheses or slash
{
  "from": "@local/test",     // WRONG: missing parentheses
  "to": "@(cloudflare)"      // WRONG: missing scope/name separator
}
```

**Current Behavior:** Passes `validateSharedMessage()` (only checks `startsWith('@(')`)

**With Schema:** Rejected with regex pattern mismatch

### 2.4 Medium-Risk: hub:register TTL Out of Range

**Schema Expectation:**
```json
{
  "ttl": {
    "type": "integer",
    "minimum": 1000,
    "maximum": 3600000
  }
}
```

**Common Client Mistake:**
```json
// TTL in seconds instead of milliseconds
{
  "type": "hub:register",
  "payload": {
    "ttl": 300  // WRONG: 300ms instead of 300000ms
  }
}
```

**Current Behavior:** Accepted, registration expires in 0.3 seconds

**With Schema:** Rejected (below minimum 1000)

### 2.5 Low-Risk: Missing Optional Fields

**Schema Expectation:**
```json
{
  "properties": {
    "metadata": { "type": "object" }
  }
}
```

**Current Behavior:** Optional fields handled gracefully (undefined checks in code)

**With Schema:** No change (optional fields validated only if present)

---

## 3. Impact by Message Type

### 3.1 High-Impact Messages (Complex Payload)

| Message Type | Risk Level | Reason |
|--------------|-----------|---------|
| `hub:send` | **HIGH** | Complex payload (to, type, data), flat structure易confusion |
| `hub:publish` | **HIGH** | Complex payload (topic, type, data), similar to hub:send |
| `hub:register` | **HIGH** | Many optional fields, TTL range validation, capabilities array |
| `hub:discover` | **MEDIUM** | Optional filters (pattern, capabilities, metadata), limit/offset validation |

**Validation Priority:** These messages should be validated first in Phase 1.

### 3.2 Medium-Impact Messages (Simple Payload)

| Message Type | Risk Level | Reason |
|--------------|-----------|---------|
| `hub:subscribe` | **MEDIUM** | Topic regex validation important but payload simple |
| `hub:unsubscribe` | **MEDIUM** | Same as subscribe |
| `hub:renew` | **MEDIUM** | TTL range, renewal token format |
| `hub:broadcast` | **MEDIUM** | Similar to hub:send but no target address |

### 3.3 Low-Impact Messages (Minimal Payload)

| Message Type | Risk Level | Reason |
|--------------|-----------|---------|
| `hub:connect` | **LOW** | Simple payload (version, jwt), already validated |
| `hub:heartbeat` | **LOW** | Empty or minimal payload |
| `hub:disconnect` | **LOW** | Simple payload (reason) |
| `hub:unregister` | **LOW** | Single field (actorAddress) |

---

## 4. Concrete Examples

### Example 1: Missing Required Field

**Scenario:** Client sends `hub:register` without `actorAddress`

**Without Validation:**
```typescript
// Handler receives message
const payload = msg.payload as { actorAddress?: string, ... };

// Runtime error when trying to use actorAddress
this.registry.set(undefined, registration); // TypeError: Invalid key
```

**With Validation:**
```typescript
// Validation fails at entry
ajv.validate(schemas['hub:register'], msg);
// Error: data.payload must have required property 'actorAddress'

// Hub sends clear error response
{
  type: "hub:error",
  payload: {
    code: "schema_validation_failed",
    message: "Message does not conform to hub:register schema",
    details: {
      errors: [
        {
          instancePath: "/payload",
          keyword: "required",
          message: "must have required property 'actorAddress'",
          params: { missingProperty: "actorAddress" }
        }
      ]
    }
  }
}
```

**Impact:** Clear error at entry vs. undefined behavior or TypeError.

---

### Example 2: Wrong Field Type

**Scenario:** Client sends `hub:send` with `payload.data` as string instead of object

**Without Validation:**
```typescript
// Handler creates forwarded message
const forwardedMessage = createMessage(
  payload.type,
  payload.data,  // "some string" instead of { ... }
  msg.from,
  targetAddress
);

// Client receives:
{
  type: "task:assign",
  payload: "some string"  // WRONG: expected object
}
```

**With Validation:**
```typescript
// Schema expects payload.data to be any type (no constraint)
// This case PASSES validation (data can be any JSON value)

// NOTE: If we want to enforce object type, we update schema:
{
  "payload": {
    "properties": {
      "data": { "type": "object" }  // Add constraint
    }
  }
}
```

**Impact:** Schema validation depends on spec strictness. Current spec allows any JSON value for `data`.

---

### Example 3: Invalid Canonical Address Regex

**Scenario:** Client sends message with malformed `from` address

**Without Validation:**
```typescript
// validateSharedMessage only checks startsWith('@(')
{
  from: "@(local-test-actor)"  // WRONG: hyphen instead of slash
}
// Passes basic validation

// Later causes issues in discovery/routing
// Actor address doesn't match expected pattern
```

**With Validation:**
```typescript
// Canonical address schema validation
{
  "pattern": "^@\\([a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+\\)$"
}

// Validation fails
ajv.validate(canonicalAddressSchema, "@(local-test-actor)");
// Error: string must match pattern "^@\([a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+\)$"

// Hub sends error
{
  type: "hub:error",
  payload: {
    code: "schema_validation_failed",
    message: "Invalid canonical address format",
    details: {
      field: "from",
      value: "@(local-test-actor)",
      expected: "@(scope/name)"
    }
  }
}
```

**Impact:** Prevents malformed addresses from entering system, catches at entry instead of causing discovery failures later.

---

### Example 4: Out-of-Range TTL Value

**Scenario:** Client sends `hub:register` with TTL = 5000000 (83 minutes, exceeds 1 hour max)

**Without Validation:**
```typescript
// Handler accepts any TTL value
const expiresAt = Date.now() + payload.ttl;
// Registration expires in 83 minutes (violates spec)
```

**With Validation:**
```typescript
// Schema enforces TTL range
{
  "ttl": {
    "type": "integer",
    "minimum": 1000,
    "maximum": 3600000
  }
}

// Validation fails
ajv.validate(schema, msg);
// Error: data.payload.ttl must be <= 3600000

// Hub sends error
{
  type: "hub:error",
  payload: {
    code: "schema_validation_failed",
    message: "TTL exceeds maximum allowed value",
    details: {
      field: "payload.ttl",
      value: 5000000,
      maximum: 3600000
    }
  }
}
```

**Impact:** Enforces spec constraints, prevents registrations from staying alive too long.

---

### Example 5: Nested Data Structure (Wrong Payload Format)

**Scenario:** Client sends `hub:send` with nested `payload.data.data` structure

**Without Validation:**
```typescript
// Client sends (WRONG):
{
  type: "hub:send",
  payload: {
    to: "@(browser/widget)",
    type: "task:assign",
    data: {
      data: { taskId: "123" }  // WRONG: double-nested
    }
  }
}

// Handler forwards:
{
  type: "task:assign",
  payload: {
    data: { taskId: "123" }  // Recipient gets nested structure
  }
}

// Recipient expects:
{
  type: "task:assign",
  payload: { taskId: "123" }  // Flat structure
}
```

**With Validation:**
```typescript
// Schema doesn't constrain data structure (any JSON value allowed)
// This PASSES validation (data can be nested)

// Solution: Document flat structure as convention, not schema constraint
// OR: Add schema constraint if we want to enforce flat objects only
```

**Impact:** Schema validation alone may not catch this. Need clear spec documentation + client examples.

---

## 5. Test Implications

### 5.1 Tests Must Use Valid Messages

**Current State:**
Tests may create messages with missing fields, relying on partial validation.

**After Schema Validation:**
All test messages must conform to schemas or they'll be rejected at entry.

**Example Fix:**
```typescript
// Before (may work):
const msg = {
  type: 'hub:send',
  payload: { type: 'test' }  // Missing: to, data
};

// After (must be valid):
const msg = {
  type: 'hub:send',
  from: '@(seag/test)',
  to: '@(cloudflare/signal-hub)',
  payload: {
    to: '@(browser/widget)',
    type: 'test',
    data: { test: true }  // All required fields
  },
  // ... other SharedMessage fields
};
```

**Impact:** Schema validation catches test bugs early, forces tests to match spec.

### 5.2 Schema Changes Require Test Updates

**Example:**
If we add required field to schema, all tests creating that message type must be updated.

**Mitigation:**
- Use test helper factories that create valid messages
- Generate test messages from schemas (schema-to-example)
- Use JSON Schema faker for test data generation

### 5.3 Validation Failures in Tests Indicate Spec Drift

**Detection:**
```bash
# Run tests with schema validation enabled
npm test

# If tests fail on validation:
# - Either test is wrong (using invalid message)
# - OR schema is wrong (too strict)
# - OR spec changed but schema/tests not updated
```

**Action:**
1. Check if test message should be valid per spec
2. Fix test OR relax schema if test is correct
3. Update spec if both are correct but spec is unclear

---

## 6. Deployment Strategy

### Phase 1: Log-Only Mode (Week 1)

**Goal:** Identify validation gaps without breaking existing traffic.

**Implementation:**
```typescript
function validateWithLogging(msg: SharedMessage): boolean {
  const validator = schemas[msg.type];
  if (!validator) {
    console.warn('[Schema] No schema for message type:', msg.type);
    return true;  // Allow unvalidated types
  }

  const valid = validator(msg);
  if (!valid) {
    console.error('[Schema] Validation failed (log-only):', {
      type: msg.type,
      errors: validator.errors,
      message: msg
    });
  }
  return true;  // Always return true (log-only, don't block)
}
```

**Monitoring:**
- Track validation failure rate per message type
- Identify most common schema violations
- Update schemas if violations are valid per spec

**Success Criteria:**
- < 1% validation failure rate
- All failures are genuine client errors (not schema bugs)

---

### Phase 2: Validate Incoming, Log Outgoing (Week 2-3)

**Goal:** Block invalid incoming messages, log violations in outgoing messages.

**Implementation:**
```typescript
// Incoming (client-to-server): BLOCK
function validateIncoming(msg: SharedMessage): void {
  const valid = schemas[msg.type](msg);
  if (!valid) {
    throw new HubError('schema_validation_failed',
      'Message does not conform to schema',
      { errors: schemas[msg.type].errors });
  }
}

// Outgoing (server-to-client): LOG ONLY
function validateOutgoing(msg: SharedMessage): void {
  const valid = schemas[msg.type](msg);
  if (!valid) {
    console.error('[Schema] Outgoing validation failed:', {
      type: msg.type,
      errors: schemas[msg.type].errors
    });
    // Don't block, just log (server should never send invalid messages)
  }
}
```

**Monitoring:**
- Track client error rate (should increase as bad messages are rejected)
- Verify outgoing validation never fails (if it does, fix server code)

**Success Criteria:**
- Client errors stabilize (all invalid messages rejected)
- Zero outgoing validation failures (server always valid)

---

### Phase 3: Validate Both Directions (Week 4+)

**Goal:** Enforce schema validation in both directions, fail fast on violations.

**Implementation:**
```typescript
// Both directions: BLOCK
function validate(msg: SharedMessage): void {
  const validator = schemas[msg.type];
  if (!validator) {
    throw new HubError('internal_error',
      `No schema defined for message type: ${msg.type}`);
  }

  const valid = validator(msg);
  if (!valid) {
    throw new HubError('schema_validation_failed',
      'Message does not conform to schema',
      { errors: validator.errors });
  }
}
```

**Monitoring:**
- Ensure no regressions (client/server behavior unchanged)
- Verify error messages are clear and actionable

**Success Criteria:**
- All messages validated in both directions
- No production incidents from validation enforcement
- Improved debuggability (clear schema errors)

---

### Rollback Plan

**Triggers:**
- Validation failure rate > 5%
- Production incidents caused by schema enforcement
- Schema bugs blocking legitimate traffic

**Action:**
1. Revert to previous phase (log-only or validate incoming only)
2. Analyze failures, identify schema bugs or client issues
3. Fix schemas or communicate breaking changes to clients
4. Re-deploy with fixes

**Rollback Procedure:**
```bash
# Disable schema validation via environment variable
export SCHEMA_VALIDATION_ENABLED=false

# OR: Set validation mode
export SCHEMA_VALIDATION_MODE=log-only  # vs. enforce

# Deploy without schema validation
wrangler deploy
```

---

## 7. Summary and Recommendations

### Key Insights

1. **Current validation is minimal** - Only checks envelope structure, not payload
2. **High-risk messages:** hub:send, hub:publish, hub:register (complex payloads)
3. **Common mistakes:** Missing required fields, wrong types, out-of-range values
4. **Schema validation prevents:** Runtime errors, undefined behavior, spec drift

### Recommendations

**Immediate (Phase 1):**
- ✅ Add Ajv dependency
- ✅ Load JSON Schemas from `spec/` directory
- ✅ Validate incoming messages (log-only mode)
- ✅ Monitor validation failures for 1 week

**Short-term (Phase 2-3):**
- ✅ Enforce validation for incoming messages
- ✅ Validate outgoing messages (log-only)
- ✅ Add `schema_validation_failed` error code to protocol
- ✅ Document schema errors in client integration guides

**Long-term:**
- ✅ Generate TypeScript types from schemas (single source of truth)
- ✅ Add schema version negotiation (for protocol evolution)
- ✅ Build schema validator CLI tool for client testing
- ✅ Integrate schema validation into CI/CD pipeline

### Success Metrics

**Phase 1 Success:**
- [ ] Validation failures identified and categorized
- [ ] Schemas updated to match spec
- [ ] < 1% validation failure rate

**Phase 2-3 Success:**
- [ ] All invalid incoming messages rejected
- [ ] Zero outgoing validation failures
- [ ] No production incidents from validation

**Overall Success:**
- [ ] 100% schema conformance
- [ ] Improved error messages (field-level errors)
- [ ] Reduced debugging time (catch errors at entry)
- [ ] Stronger spec-implementation alignment

---

## Appendix: Validation Point Reference

### A. Validation Points in Code

| Location | What's Validated | When |
|----------|-----------------|------|
| `SignalHub.webSocketMessage()` | Message size, JSON parse | Before routing |
| `validateSharedMessage()` | Envelope structure | After parse, before routing |
| **[NEW] Schema Validation** | Payload structure | After envelope validation |
| Message handlers | Business logic | After schema validation |

### B. Error Response Format

**Current (Internal Errors):**
```json
{
  "type": "hub:error",
  "payload": {
    "code": "internal_error",
    "message": "Human-readable error"
  }
}
```

**Proposed (Schema Validation Errors):**
```json
{
  "type": "hub:error",
  "payload": {
    "code": "schema_validation_failed",
    "message": "Message does not conform to hub:send schema",
    "details": {
      "messageType": "hub:send",
      "errors": [
        {
          "instancePath": "/payload/to",
          "keyword": "required",
          "message": "must have required property 'to'"
        }
      ]
    }
  }
}
```

### C. Ajv Configuration

**Recommended Setup:**
```typescript
import Ajv from 'ajv';
import addFormats from 'ajv-formats';

const ajv = new Ajv({
  allErrors: true,        // Collect all errors, not just first
  verbose: true,          // Include data in errors
  strictSchema: false,    // Allow custom keywords ($id, _notes, etc.)
  removeAdditional: false // Don't remove extra fields (preserve metadata)
});

addFormats(ajv);  // Add string format validators (uuid, uri, etc.)
```

---

**End of Analysis**
