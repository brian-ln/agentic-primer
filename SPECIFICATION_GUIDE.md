# Specification Guide: How to Read and Understand Event System Specifications

This guide teaches you how to read and understand the three types of specifications in this codebase. Each specification type serves a different purpose and complements the others.

---

## Quick Navigation

- [Overview: The Three Specification Types](#overview-the-three-specification-types)
- [Part 1: BDD Feature Specifications (Gherkin)](#part-1-bdd-feature-specifications-gherkin)
- [Part 2: State Machine Specifications](#part-2-state-machine-specifications)
- [Part 3: FIT Decision Tables](#part-3-fit-decision-tables)
- [Part 4: How Specifications Connect](#part-4-how-specifications-connect)
- [Part 5: Verifying Code Against Specifications](#part-5-verifying-code-against-specifications)

---

## Overview: The Three Specification Types

This project uses three complementary specification formats:

| Format | Purpose | Best For | Location |
|--------|---------|----------|----------|
| **Gherkin (BDD)** | Describe behavior as scenarios | Understanding "what" the system does | `specs/features/*.feature` |
| **State Machines** | Define valid states and transitions | Understanding lifecycle and state management | `specs/state-machines/*-state-machine.md` |
| **FIT Tables** | Decision tables for testing | Verifying behavior with specific inputs/outputs | `specs/fit-fixtures/*.fit.md` |

**Why three formats?**

- **Gherkin** tells the story of how users interact with the system
- **State Machines** prevent impossible states and ensure safe transitions
- **FIT Tables** provide concrete test cases with specific data

Think of them like a building's blueprints:
- Gherkin = The architectural drawings showing rooms and flow
- State Machines = The structural diagrams showing load-bearing walls
- FIT Tables = The inspection checklist with measurements

---

## Part 1: BDD Feature Specifications (Gherkin)

### What It Looks Like

**File**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/features/event-log-actor.feature`

```gherkin
Feature: EventLogActor - Append-only event storage with replay capability
  As an event system component
  I want to persist events to JSONL storage
  So that events can be queried and replayed

  Scenario: Append a single event successfully
    Given an EventLogActor that is running
    When I append an event with type "user.created" and data {"userId": "123"}
    Then the append should succeed
    And the response should include an event ID
    And the event count should be 1
    And the event should be written to the log file
```

### How to Read It

**Structure breakdown:**

1. **Feature**: High-level description of the component
   - "As a... I want... So that..." tells you the user story

2. **Scenario**: A specific example of behavior
   - **Given**: The starting state (preconditions)
   - **When**: The action being tested
   - **Then**: The expected outcomes
   - **And**: Additional expectations

**Reading the example above:**

| Line | What It Means |
|------|---------------|
| `Given an EventLogActor that is running` | Precondition: The actor has been started successfully |
| `When I append an event...` | Action: Call the `appendEvent()` method with specific data |
| `Then the append should succeed` | Expectation: Returns `{success: true}` |
| `And the response should include an event ID` | Expectation: Response has an `eventId` field |
| `And the event count should be 1` | Expectation: Internal counter incremented |
| `And the event should be written to the log file` | Expectation: JSONL file contains the event |

### Concrete Example from Code

**The scenario maps to this code path:**

```javascript
// File: src/actors/event-log.js

// Given: an EventLogActor that is running
const actor = new EventLogActor({ eventLog: { file: 'events.jsonl' } });
await actor.start(); // Sets isInitialized = true

// When: I append an event
const result = await actor.appendEvent({
  type: 'user.created',
  data: { userId: '123' }
});

// Then: the append should succeed
console.log(result.success); // true

// And: the response should include an event ID
console.log(result.eventId); // 'evt_01HQZXYZ...'

// And: the event count should be 1
console.log(result.eventCount); // 1

// And: the event should be written to the log file
// Check: events.jsonl now contains one line with the event
```

### What to Look For in the Code

When verifying a Gherkin scenario:

1. **Given**: Find the setup code (constructor, `start()` method)
2. **When**: Find the action method (`appendEvent()`)
3. **Then**: Verify the return value structure
4. **And**: Check side effects (file writes, counter increments)

**Example verification:**

```javascript
// spec/features/event-log-actor.feature line 39-45
// Scenario: Append a single event successfully

// Verify in code:
// 1. Check src/actors/event-log.js line 139-200 (appendEvent method)
// 2. Confirm it returns {success, eventId, eventCount}
// 3. Confirm it writes to writeStream (line 172)
// 4. Confirm it increments eventCount (line 179)
```

### Why It Matters

Gherkin scenarios ensure that:
- Every user-facing behavior is documented
- Requirements are executable (can become tests)
- Stakeholders can understand what the system does
- Edge cases and errors are explicitly covered

**What it prevents:**
- Undocumented behavior
- Assumptions about how the system works
- Missing error handling paths

---

## Part 2: State Machine Specifications

### What It Looks Like

**File**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/state-machines/event-log-actor-state-machine.md`

```markdown
## State Definitions

### STOPPED (isInitialized = false)
**Invariants**:
- `isInitialized === false`
- `writeStream === null`
- `eventCount` retains last known value

**Valid Operations**:
- `start()` - Initialize and open write stream
- `getStatus()` - Report stopped status

### RUNNING (isInitialized = true)
**Invariants**:
- `isInitialized === true`
- `writeStream !== null` (active file stream)
- `eventCount >= 0`

**Valid Operations**:
- `append(event)` - Write event to log
- `query(options)` - Read events from log
- `stop()` - Close stream and shutdown
```

**State Transition Table:**

| Current State | Event | Next State | Actions |
|---------------|-------|------------|---------|
| STOPPED | `start()` called | RUNNING | 1. Create log directory<br>2. Count existing events<br>3. Open write stream<br>4. Set `isInitialized = true` |
| RUNNING | `stop()` called | STOPPED | 1. Flush write stream<br>2. Close write stream<br>3. Set `isInitialized = false` |

### How to Read It

**State Machines answer these questions:**

1. **What states exist?** (STOPPED, RUNNING)
2. **What is true in each state?** (Invariants)
3. **What operations are valid?** (Valid Operations)
4. **How do I move between states?** (Transition Table)
5. **What happens during a transition?** (Actions)

**Reading the example:**

**State: STOPPED**
- **Invariants**: Things that MUST be true
  - `isInitialized === false` → Cannot call `append()`
  - `writeStream === null` → No file is open

- **Valid Operations**: What you CAN do
  - `start()` → Move to RUNNING
  - `getStatus()` → Just read, no state change

**State: RUNNING**
- **Invariants**: Things that MUST be true
  - `isInitialized === true` → Can call `append()`
  - `writeStream !== null` → File is open for writing

- **Valid Operations**: What you CAN do
  - `append()`, `query()`, `stop()`, `getStatus()`

### Concrete Example from Code

**The state machine maps to these code sections:**

```javascript
// File: src/actors/event-log.js

class EventLogActor {
  constructor(config = {}) {
    // Initial state: STOPPED
    this.writeStream = null;      // STOPPED invariant
    this.eventCount = 0;
    this.isInitialized = false;   // STOPPED invariant
  }

  async start() {
    // State transition: STOPPED -> RUNNING
    // Actions during transition:

    // 1. Create log directory (line 66)
    await fs.mkdir(logDir, { recursive: true });

    // 2. Count existing events (line 73)
    this.eventCount = await this._countEvents();

    // 3. Open write stream (line 83)
    this.writeStream = createWriteStream(this.logPath, { flags: 'a' });

    // 4. Set state to RUNNING (line 89)
    this.isInitialized = true;

    return { success: true };
  }

  async appendEvent(eventData) {
    // Guard: Only valid in RUNNING state (line 140)
    if (!this.isInitialized) {
      await this.initialize(); // Error: should fail
    }

    // RUNNING state invariant check:
    // - writeStream must exist
    this.writeStream.write(line, (err) => { ... });
  }

  async stop() {
    // State transition: RUNNING -> STOPPED
    return new Promise((resolve) => {
      if (this.writeStream) {
        // Action: Close write stream (line 416)
        this.writeStream.end(() => {
          // Action: Set state to STOPPED (line 417)
          this.isInitialized = false;
          resolve({ success: true });
        });
      }
    });
  }

  getStatus() {
    // Valid in both states (line 429)
    return {
      isRunning: this.isInitialized,  // Reports current state
      eventCount: this.eventCount,
      logPath: this.logPath
    };
  }
}
```

### State Transition Table in Detail

**Example: Starting the actor**

| Column | Meaning | Example Value |
|--------|---------|---------------|
| Current State | Where you are | STOPPED |
| Event | What happens | `start()` called |
| Guards | Conditions required | None (always allowed) |
| Next State | Where you go | RUNNING |
| Actions | What happens during transition | See below |
| Error Handling | What if it fails | Remain in STOPPED |

**Actions during STOPPED → RUNNING:**

```javascript
// From spec: "1. Create log directory"
// In code (line 66):
await fs.mkdir(logDir, { recursive: true });

// From spec: "2. Count existing events"
// In code (line 73):
this.eventCount = await this._countEvents();

// From spec: "3. Open write stream"
// In code (line 83):
this.writeStream = createWriteStream(this.logPath, { flags: 'a' });

// From spec: "4. Set isInitialized = true"
// In code (line 89):
this.isInitialized = true;
```

### Why It Matters

State machines ensure that:
- **No impossible states**: Can't have `isInitialized=true` but `writeStream=null`
- **Safe transitions**: Can't skip steps (e.g., open stream without creating directory)
- **Idempotency**: Calling `start()` twice doesn't break anything
- **Error recovery**: Failed transitions leave state consistent

**What it prevents:**
- Calling `append()` on a closed stream
- Memory leaks from unclosed streams
- Race conditions in startup/shutdown
- Inconsistent state after errors

---

## Part 3: FIT Decision Tables

### What It Looks Like

**File**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/fit-fixtures/event-log-actor.fit.md`

**Table 1: Lifecycle State Transitions**

| Initial State | Action | Expected State | Expected Success | Expected Message/Error |
|--------------|--------|----------------|------------------|------------------------|
| stopped | start() | running | true | - |
| running | start() | running | false | already running |
| running | stop() | stopped | true | - |
| stopped | stop() | stopped | true | - |

**Table 2: Append Event - Happy Path**

| Event Type | Event Data | Has ID | Has Timestamp | Expected Success | EventCount Increments | ID Generated |
|-----------|-----------|--------|---------------|------------------|----------------------|--------------|
| user.created | {name: "Alice"} | no | no | true | yes | yes |
| user.created | {name: "Bob"} | yes | no | true | yes | no |
| order.placed | {} | no | no | true | yes | yes |

### How to Read It

**FIT Tables are decision tables:**
- **Left columns**: Inputs/conditions
- **Right columns**: Expected outputs
- **Each row**: A specific test case

**Reading Table 1 (row 2):**

| Input → | Input → | ← Output | ← Output | ← Output |
|---------|---------|----------|----------|----------|
| Initial State | Action | Expected State | Expected Success | Expected Message |
| **running** | **start()** | **running** | **false** | **already running** |

**Interpretation:**
- IF the actor is in "running" state
- AND you call `start()`
- THEN it stays in "running" state
- AND returns `success: false`
- AND the error says "already running"

**Reading Table 2 (row 1):**

| Input → | Input → | Input → | Input → | ← Output | ← Output | ← Output |
|---------|---------|---------|---------|----------|----------|----------|
| Event Type | Event Data | Has ID | Has Timestamp | Expected Success | EventCount Increments | ID Generated |
| **user.created** | **{name: "Alice"}** | **no** | **no** | **true** | **yes** | **yes** |

**Interpretation:**
- Given event type "user.created"
- And data `{name: "Alice"}`
- And NO pre-assigned ID
- And NO pre-assigned timestamp
- Then append succeeds
- And event count goes up by 1
- And system generates an ID (evt_...)

### Concrete Example from Code

**Table 1, Row 2: "running + start() = error"**

```javascript
// File: src/actors/event-log.js

async start() {
  // Guard check (not in current code, but SHOULD be):
  // if (this.isInitialized) {
  //   return { success: false, error: 'Already running' };
  // }

  // Current code on line 62-93 doesn't check this!
  // This is a BUG found by the specification
}

// What the spec says SHOULD happen:
const actor = new EventLogActor();
await actor.start(); // { success: true }
await actor.start(); // { success: false, error: 'already running' }
```

**Table 2, Row 1: "Generate ID if missing"**

```javascript
// File: src/actors/event-log.js line 146-158

async appendEvent(eventData) {
  const event = {
    // If no ID provided, generate one
    id: eventData.id || `evt_${generateULID()}`,  // Line 147

    // If no timestamp provided, generate one
    timestamp: eventData.timestamp || new Date().toISOString(),  // Line 148

    type: eventData.type,
    data: eventData.data || {},
    metadata: { ... }
  };

  // Validates the spec's expectations:
  // - ID is generated: YES (line 147)
  // - Success: YES (if type is provided)
  // - EventCount increments: YES (line 179)
}
```

### Why It Matters

FIT tables ensure that:
- Every combination of inputs is tested
- Edge cases are explicit (empty data, nulls, etc.)
- Behavior is predictable and documented
- Tables can be automated as tests

**What it prevents:**
- Untested input combinations
- Undocumented edge cases
- Surprising behavior with unusual inputs
- Regressions when changing code

---

## Part 4: How Specifications Connect

### Example: Appending an Event

Let's trace one feature across all three specifications.

#### Gherkin Scenario
```gherkin
Scenario: Append a single event successfully
  Given an EventLogActor that is running
  When I append an event with type "user.created"
  Then the append should succeed
  And the response should include an event ID
```

**Says WHAT**: "Appending an event succeeds when running"

#### State Machine
```markdown
| Current State | Event | Guards | Next State | Actions |
|---------------|-------|--------|------------|---------|
| RUNNING | `append(event)` called | Valid event object | RUNNING | 1. Validate event<br>2. Write JSONL line<br>3. Increment eventCount |
```

**Says HOW**: "Can only append in RUNNING state, write happens, count increments"

#### FIT Table
```markdown
| Event Type | Event Data | Has ID | Expected Success | EventCount Increments | ID Generated |
|-----------|-----------|--------|------------------|----------------------|--------------|
| user.created | {name: "Alice"} | no | true | yes | yes |
```

**Says WITH WHAT**: "Specific data produces specific results"

### The Complete Picture

```
┌─────────────────────────────────────────────────────────────┐
│ GHERKIN: User Story                                         │
│ "When I append an event, it should succeed"                 │
└────────────────────┬────────────────────────────────────────┘
                     │
        ┌────────────┴────────────┐
        │                         │
        ▼                         ▼
┌───────────────┐        ┌────────────────┐
│ STATE MACHINE │        │   FIT TABLES   │
│               │        │                │
│ Defines valid │        │ Tests specific │
│ transitions   │        │ inputs/outputs │
│               │        │                │
│ STOPPED ←→    │        │ Type: "user.  │
│    RUNNING    │        │  created"      │
│               │        │ Success: true  │
└───────┬───────┘        └────────┬───────┘
        │                         │
        └────────────┬────────────┘
                     │
                     ▼
        ┌────────────────────────┐
        │   ACTUAL CODE          │
        │   src/actors/          │
        │   event-log.js         │
        └────────────────────────┘
```

---

## Part 5: Verifying Code Against Specifications

### Step-by-Step Verification Process

Let's verify the `FunctionExecutorActor` using all three specs.

#### Step 1: Check Lifecycle (State Machine)

**Spec**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/state-machines/function-executor-actor-state-machine.md`

**What to verify:**

```markdown
Transition: STOPPED → RUNNING
Guard: isRunning === false
Actions:
1. Set isRunning = true
2. Return success
```

**In code** (`src/actors/function-executor.js`):

```javascript
// Line 57-77
async start() {
  // ✓ Guard check (line 58)
  if (this.isRunning) {
    return {
      success: false,
      error: 'FunctionExecutorActor is already running'
    };
  }

  try {
    // ✓ Action 1: Set isRunning = true (line 66)
    this.isRunning = true;

    // ✓ Action 2: Return success (line 67-70)
    return {
      success: true,
      message: 'FunctionExecutorActor started successfully'
    };
  }
}
```

**Verification Result:** ✓ PASS - All actions match spec

#### Step 2: Check Behavior (Gherkin)

**Spec**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/features/function-executor-actor.feature`

**Scenario to verify:**

```gherkin
Scenario: Execute a simple code function successfully
  Given a FunctionExecutorActor that is running
  And a code function at "/tmp/test-func.js" that returns "success"
  When I execute the function with event {"type": "test.event"}
  Then the execution should succeed
  And the result should be "success"
  And the response should include executionTime
  And a "function.executed" event should be emitted
```

**In code** (`src/actors/function-executor.js`):

```javascript
// Line 136-366 (execute method)

// Given: a FunctionExecutorActor that is running
// Check: isRunning is checked implicitly (no guard, potential bug!)

// When: I execute the function
async execute({ functionId, functionPath, event, config }) {
  const startTime = Date.now();  // For executionTime

  // Then: the execution should succeed
  // ✓ Returns success (line 324-330)
  return createMessage(PROTOCOLS.FUNCTION, ACTIONS.COMPLETE, {
    functionId,
    functionPath: absolutePath,
    result,                     // ✓ And the result should be "success"
    executionTime,             // ✓ And response includes executionTime
    success: true
  });

  // And: a "function.executed" event should be emitted
  // ✓ Emits event (line 307-322)
  if (this.emitCallback) {
    await this.emitCallback({
      type: 'function.executed',
      data: { functionId, result, executionTime, event },
      metadata: { ... }
    });
  }
}
```

**Verification Result:** ⚠️ PARTIAL PASS
- ✓ Returns result
- ✓ Includes executionTime
- ✓ Emits event
- ✗ Missing: Guard check for `isRunning` state

#### Step 3: Check Data Flows (FIT Tables)

**Spec**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/fit-fixtures/function-executor-actor.fit.md`

**Table 3: Execute Code Function - Input Validation**

| Function ID | Function Path | Event | Expected Success | Expected Error Contains |
|------------|--------------|-------|------------------|------------------------|
| null | /valid/func.js | {type: 'test'} | false | functionId is required |
| "valid" | null | {type: 'test'} | false | functionPath is required |
| "valid" | /valid/func.js | null | false | event is required |

**In code** (`src/actors/function-executor.js`):

```javascript
// Line 145-165
// ✓ Validates functionId (line 146-150)
if (!functionId) {
  return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
    error: 'functionId is required',
    functionId
  });
}

// ✓ Validates functionPath (line 152-157)
if (!functionPath) {
  return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
    error: 'functionPath is required',
    functionId
  });
}

// ✓ Validates event (line 159-164)
if (!event) {
  return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
    error: 'event is required',
    functionId
  });
}
```

**Verification Result:** ✓ PASS - All validations match spec

### Verification Checklist

Use this checklist when verifying code:

#### State Machine Verification
- [ ] All states defined in code match spec
- [ ] All invariants are enforced (asserts or guards)
- [ ] All transitions are implemented
- [ ] All transition actions are performed
- [ ] Error handling matches spec
- [ ] Idempotent operations are truly idempotent

#### Gherkin Verification
- [ ] Each Given/When/Then maps to code
- [ ] Happy paths are implemented
- [ ] Error paths are implemented
- [ ] Edge cases are handled
- [ ] Events are emitted as specified
- [ ] Return values match expectations

#### FIT Table Verification
- [ ] All input combinations are tested
- [ ] All outputs match expected values
- [ ] Boundary conditions are handled
- [ ] Null/undefined cases are covered
- [ ] Error messages match spec

---

## Common Patterns and Anti-Patterns

### Pattern: Reading a New Specification

When you encounter a new actor specification:

1. **Start with State Machine**
   - Understand valid states
   - Identify lifecycle (start/stop)
   - Note what operations are valid in each state

2. **Read Gherkin Scenarios**
   - Start with "Happy Path" scenarios
   - Then read error scenarios
   - Note edge cases and concurrency

3. **Use FIT Tables for Details**
   - See specific input/output combinations
   - Understand boundary conditions
   - Find test cases to implement

### Anti-Pattern: Specification Drift

**BAD**: Code changes without updating specs

```javascript
// Code added:
async forceStop() {
  this.isRunning = false;  // Force stop without cleanup
}

// But no spec exists for this!
// - Not in state machine
// - Not in Gherkin
// - Not in FIT tables
```

**GOOD**: Specs updated first, then code

```markdown
<!-- Add to state machine spec -->
### Force Stop (Dangerous)
Transition: ANY STATE → STOPPED
Actions: Set isRunning = false (no cleanup)
⚠️ WARNING: May leak resources
```

### Pattern: Finding Bugs with Specs

**Specs often reveal bugs:**

```javascript
// Spec says:
// Table: Lifecycle State Transitions
// | Initial State | Action  | Expected Success |
// | running       | start() | false            |

// But code does:
async start() {
  // Missing guard check!
  this.isRunning = true;
  return { success: true };  // BUG: Should return false
}

// FIX:
async start() {
  if (this.isRunning) {
    return { success: false, error: 'Already running' };
  }
  this.isRunning = true;
  return { success: true };
}
```

---

## Quick Reference Cards

### Gherkin Cheat Sheet

| Keyword | Purpose | Example |
|---------|---------|---------|
| Feature | Component description | `Feature: EventLogActor` |
| Scenario | Specific behavior | `Scenario: Append event` |
| Given | Preconditions | `Given an actor that is running` |
| When | Action | `When I call append()` |
| Then | Expected outcome | `Then it should succeed` |
| And | Additional expectations | `And the event count increases` |
| But | Negative expectations | `But no error is logged` |

### State Machine Cheat Sheet

| Element | Purpose | What to Verify |
|---------|---------|----------------|
| State Definition | What's true in a state | Check invariants in code |
| Invariants | Must always be true | Add assertions/guards |
| Valid Operations | What you can do | Check method implementations |
| Transition Table | How to move between states | Verify guard conditions |
| Actions | What happens during transition | Trace code execution |
| Error Handling | What if transition fails | Check error paths |

### FIT Table Cheat Sheet

| Table Type | Purpose | How to Use |
|------------|---------|------------|
| Lifecycle | Test state transitions | Verify start/stop/getStatus |
| Input Validation | Test parameter checking | Verify required fields |
| Happy Path | Test successful operations | Verify normal flows |
| Error Cases | Test failure modes | Verify error returns |
| Edge Cases | Test boundaries | Verify null/empty/large |
| Decision Logic | Test conditional behavior | Verify if/else branches |

---

## Examples: Reading Complex Scenarios

### Complex Example 1: Function Execution with Depth Tracking

**Gherkin** (features/function-executor-actor.feature:145-153):

```gherkin
Scenario: Track event depth for emitted events
  Given a FunctionExecutorActor with emit callback configured
  And a code function that emits an event
  And the triggering event has depth 2
  When I execute the function
  Then the execution should succeed
  And the emitted event should have depth 3
```

**State Machine** (function-executor-actor-state-machine.md):

```markdown
| Current State | Event | Actions |
|---------------|-------|---------|
| RUNNING | Function emits event | 1. Validate event object<br>2. Call emitCallback(event)<br>3. Continue execution |
```

**FIT Table** (function-executor-actor.fit.md:198-209):

```markdown
| Incoming Event Depth | Function emits() Event | Expected Emitted Event Depth |
|---------------------|----------------------|----------------------------|
| 0 | yes | 1 |
| 2 | yes | 3 |
| undefined | yes | 1 (default 0 + 1) |
```

**In Code** (src/actors/function-executor.js:240-256):

```javascript
// Create execution context
const context = {
  emit: async (newEvent) => {
    if (this.emitCallback) {
      // Add depth tracking
      const eventWithMetadata = {
        ...newEvent,
        metadata: {
          ...newEvent.metadata,
          source: functionId,
          triggeredBy: event.id,
          // ✓ Depth incremented (line 250)
          depth: (event.metadata?.depth || 0) + 1
        }
      };
      await this.emitCallback(eventWithMetadata);
    }
  },
  logger,
  config: { ... }
};
```

**Verification:**
1. ✓ Gherkin: Emitted event depth is original + 1
2. ✓ State Machine: emitCallback is called
3. ✓ FIT Table: All depth combinations handled correctly (0→1, 2→3, undefined→1)
4. ✓ Code: Line 250 implements `(depth || 0) + 1`

### Complex Example 2: Query Events with Filters

**Gherkin** (features/event-log-actor.feature:76-83):

```gherkin
Scenario: Query events with type filter
  Given an EventLogActor with mixed event types
  And 3 events of type "user.created"
  And 2 events of type "order.placed"
  When I query events with filter for type "user.created"
  Then the query should succeed
  And I should receive 3 events
  And all events should have type "user.created"
```

**FIT Table** (event-log-actor.fit.md:62-71):

```markdown
| Event Types Stored | Filter Type | Expected Matches | Expected Types |
|-------------------|-------------|------------------|----------------|
| user.created (x3), order.placed (x2) | user.created | 3 | user.created |
| user.created (x3), order.placed (x2) | order.placed | 2 | order.placed |
```

**In Code** (src/actors/event-log.js:211-277):

```javascript
async queryEvents(options = {}) {
  const {
    filter = () => true,  // Default: no filtering
    limit = Infinity,
    offset = 0,
    reverse = false
  } = options;

  const events = [];
  let count = 0;

  await this._readEvents((event) => {
    // ✓ Apply filter (line 233)
    if (filter(event)) {
      if (skipped < offset) {
        skipped++;
      } else if (count < limit) {
        events.push(event);
        count++;
      }
    }
    return true;
  });

  // ✓ Returns filtered results (line 264-269)
  return {
    success: true,
    events,
    count: events.length,
    total: this.eventCount
  };
}
```

**To use in practice:**

```javascript
// Query with filter function
const result = await actor.queryEvents({
  filter: (event) => event.type === 'user.created'
});

// ✓ Returns only user.created events
console.log(result.events.length); // 3
console.log(result.events.every(e => e.type === 'user.created')); // true
```

---

## Glossary

| Term | Meaning | Example |
|------|---------|---------|
| **Actor** | Autonomous component with state | EventLogActor, FunctionExecutorActor |
| **Invariant** | Condition that must always be true | `isInitialized === true` implies `writeStream !== null` |
| **Guard** | Condition that must be true for a transition | Can't call `append()` unless `isInitialized` |
| **Action** | Something that happens during a transition | Open file stream, increment counter |
| **State** | A distinct mode of operation | STOPPED, RUNNING |
| **Transition** | Moving from one state to another | STOPPED → RUNNING |
| **Idempotent** | Safe to call multiple times | `stop()` on STOPPED returns success |
| **UAP** | Universal Actor Protocol | Message format for actor communication |
| **Depth** | Event chain depth | Event triggered by event triggered by event... |

---

## Conclusion

You now have a complete guide to reading and understanding the specifications:

1. **Start with State Machines** to understand valid states and transitions
2. **Read Gherkin Scenarios** to understand behavior and user stories
3. **Check FIT Tables** for specific input/output combinations
4. **Verify in Code** by tracing each spec element to its implementation

**Remember:**
- Specs and code should always match
- If they don't, the spec is the source of truth
- Update specs first, then code
- Use all three formats together for complete understanding

**Files referenced in this guide:**
- Gherkin: `/Users/bln/play/agentic-primer/.wt/event-system/specs/features/*.feature`
- State Machines: `/Users/bln/play/agentic-primer/.wt/event-system/specs/state-machines/*-state-machine.md`
- FIT Tables: `/Users/bln/play/agentic-primer/.wt/event-system/specs/fit-fixtures/*.fit.md`
- Code: `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/*.js`
