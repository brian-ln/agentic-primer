# Signal Hub Verification Plan

**Goal:** Ensure schemas, specs, code, and tests are aligned and consistent.

## Four-Layer Alignment

```
┌─────────────────────────────────────────────────────────┐
│ 1. INTENT (Success Criteria)                           │
│    - What the system should do                          │
│    - Documented in *.spec.md files                      │
└─────────────────────────────────────────────────────────┘
                        ↓ verifies
┌─────────────────────────────────────────────────────────┐
│ 2. SCHEMAS (Machine-Readable Contracts)                │
│    - JSON Schema definitions                            │
│    - AsyncAPI protocol definitions                      │
│    - State machine FSM definitions                      │
└─────────────────────────────────────────────────────────┘
                        ↓ validates
┌─────────────────────────────────────────────────────────┐
│ 3. IMPLEMENTATION (Runtime Behavior)                    │
│    - SignalHub.ts + handlers                            │
│    - Message routing and processing                     │
└─────────────────────────────────────────────────────────┘
                        ↓ exercises
┌─────────────────────────────────────────────────────────┐
│ 4. TESTS (Verification)                                 │
│    - Integration tests (71)                             │
│    - Unit tests (12)                                    │
└─────────────────────────────────────────────────────────┘
```

## Verification Strategies

### A. Schema Validation (Runtime)

**Goal:** Every message sent/received must conform to JSON Schema.

**Implementation:**
1. Add Ajv (JSON Schema validator) to dependencies
2. Load schemas from `spec/schemas/*.schema.json`
3. Validate incoming messages before routing
4. Validate outgoing messages before sending
5. Log schema violations with message type and failure reason

**Example:**
```typescript
import Ajv from 'ajv';

const ajv = new Ajv({ allErrors: true });
const schemas = {
  'hub:connect': loadSchema('connection/protocol.json#/hub:connect'),
  'hub:send': loadSchema('messaging/protocol.json#/hub:send'),
  // ... all 26 message types
};

function validateMessage(msg: SharedMessage): boolean {
  const validator = schemas[msg.type];
  if (!validator) {
    console.error(`No schema for message type: ${msg.type}`);
    return false;
  }

  const valid = validator(msg);
  if (!valid) {
    console.error('Schema validation failed:', {
      type: msg.type,
      errors: validator.errors
    });
  }
  return valid;
}
```

**Where to add:**
- `SignalHub.webSocketMessage()` - validate incoming before routing
- All handler return paths - validate outgoing before send

### B. Spec-to-Test Mapping

**Goal:** Every requirement in specs has corresponding test coverage.

**Method:**
1. Extract testable requirements from *.spec.md files
2. Tag tests with spec references
3. Generate coverage report: requirement → test mapping

**Example:**
```typescript
// tests/integration/connection-lifecycle.test.ts

test('hub:connect → hub:connected (success path)', async () => {
  // @spec: CONNECTION.spec.md#L45-L67 (Authentication flow)
  // @requirement: Valid JWT must result in hub:connected response
  // @requirement: actorIdentity must be set from JWT claims

  const response = await sendConnect(validJWT);
  expect(response.type).toBe('hub:connected');
  expect(session.actorIdentity).toBe('@(local/test-seag)');
});
```

**Tooling:**
- Grep test files for `@spec:` and `@requirement:` tags
- Generate markdown table: Spec Section → Test File → Line Number
- Identify untested spec sections

### C. State Machine Conformance

**Goal:** Runtime state transitions match `spec/connection/state-machine.json`.

**Method:**
1. Load FSM definition from JSON
2. Track actual state transitions at runtime
3. Validate transitions against allowed FSM edges
4. Log illegal transitions

**Example:**
```typescript
import stateMachine from '../spec/connection/state-machine.json';

function transitionState(
  session: Session,
  from: ConnectionState,
  to: ConnectionState,
  event: string
): boolean {
  const transition = stateMachine.transitions.find(
    t => t.from === from && t.event === event && t.to === to
  );

  if (!transition) {
    console.error('Illegal state transition:', {
      sessionId: session.sessionId,
      from,
      to,
      event,
      allowed: stateMachine.transitions.filter(t => t.from === from)
    });
    return false;
  }

  session.connectionState = to;
  return true;
}
```

### D. Contract Testing (Producer-Consumer)

**Goal:** Verify client expectations match hub behavior.

**Method:**
1. Define client contracts (what clients expect hub to do)
2. Run contract tests against hub implementation
3. Use Pact or similar consumer-driven contract testing

**Example Contract:**
```json
{
  "consumer": "SEAG Client",
  "provider": "Signal Hub",
  "interactions": [
    {
      "description": "Register actor with valid address",
      "request": {
        "type": "hub:register",
        "payload": {
          "actorAddress": "@(local/test-actor)",
          "capabilities": ["send", "receive"]
        }
      },
      "response": {
        "type": "hub:registered",
        "status": 200
      }
    }
  ]
}
```

### E. Coverage Analysis

**Goal:** Measure test coverage against spec requirements, not just code lines.

**Dimensions:**
1. **Message type coverage** - All 26 message types exercised?
2. **State transition coverage** - All FSM edges tested?
3. **Error path coverage** - All error conditions verified?
4. **Edge case coverage** - Spec edge cases (duplicate connect, heartbeat timeout, etc.) tested?

**Report Format:**
```markdown
## Spec Coverage Report

### Message Types (26 total)
- ✅ hub:connect (3 tests)
- ✅ hub:register (7 tests)
- ⚠️  hub:send (0 unit tests, only integration)
- ❌ hub:refresh_token (not implemented, deferred to Phase 2)

### State Transitions (7 edges)
- ✅ connecting → connected (tested)
- ✅ connected → disconnecting (tested)
- ❌ connected → disconnected (abnormal close - not explicitly tested)

### Error Conditions (12 defined)
- ✅ Invalid JWT (tested)
- ✅ Version mismatch (tested)
- ⚠️  Rate limited (implemented, not tested)
```

## Implementation Phases

### Phase 1: Schema Validation (Immediate)
- Add Ajv dependency
- Load JSON Schemas
- Validate all incoming/outgoing messages
- Log violations (non-blocking)

**Deliverable:** Schema validation layer with logging

### Phase 2: Test Annotation (1-2 hours)
- Add `@spec:` and `@requirement:` tags to existing tests
- Generate initial spec-to-test mapping
- Identify coverage gaps

**Deliverable:** SPEC_COVERAGE.md report

### Phase 3: FSM Conformance (2-3 hours)
- Load state-machine.json
- Add transition validation in transitionState()
- Log illegal transitions
- Add tests for all FSM edges

**Deliverable:** Runtime FSM validation + complete state transition tests

### Phase 4: Contract Testing (Future)
- Define client contracts (SEAG, browser, etc.)
- Implement Pact or similar
- Integrate into CI/CD

**Deliverable:** Consumer-driven contract test suite

## Success Criteria

**We achieve alignment when:**

1. ✅ **100% schema validation** - All messages validate against JSON Schemas
2. ✅ **100% spec coverage** - Every spec requirement has ≥1 test
3. ✅ **100% FSM coverage** - All state transitions tested
4. ✅ **Zero illegal transitions** - Runtime FSM validation passes
5. ✅ **Contract tests pass** - All client expectations met

## Continuous Verification

**On every commit:**
1. Run tests → ensure they pass
2. Run schema validator → ensure messages conform
3. Run FSM validator → ensure transitions legal
4. Generate coverage report → track spec-to-test mapping
5. Fail build if coverage drops

**Weekly:**
- Review spec coverage report
- Add tests for uncovered requirements
- Update schemas to match spec changes

## Tools and Dependencies

**Add to package.json:**
```json
{
  "devDependencies": {
    "ajv": "^8.12.0",              // JSON Schema validation
    "ajv-formats": "^2.1.1",       // Format validators (uri, email, etc.)
    "@pact-foundation/pact": "^12.0.0"  // Contract testing (Phase 4)
  }
}
```

**CLI Commands:**
```bash
# Validate all schemas
pnpm run validate:schemas

# Generate spec coverage report
pnpm run coverage:specs

# Run contract tests
pnpm run test:contracts
```

## Next Steps

**Immediate (this session):**
1. Decide on Phase 1 approach (schema validation)
2. Create bead/task for implementation
3. Add Ajv and implement validation layer

**Short term (next session):**
1. Annotate existing tests with spec references
2. Generate initial SPEC_COVERAGE.md
3. Identify and fill coverage gaps

**Long term:**
1. FSM runtime validation
2. Contract testing with real clients
3. CI/CD integration

---

**Status:** Drafted - Awaiting approval to proceed with Phase 1 (schema validation).
