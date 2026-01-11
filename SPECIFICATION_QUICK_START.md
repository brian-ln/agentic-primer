# Specification Quick Start

A 5-minute guide to understanding the Event System specifications.

---

## The Three Specification Types (At a Glance)

```
┌─────────────────────────────────────────────────────────────────┐
│                    YOUR QUESTION                                │
└────────┬────────────────────┬──────────────────┬────────────────┘
         │                    │                  │
         ▼                    ▼                  ▼
    ┌────────┐          ┌──────────┐      ┌──────────┐
    │ WHAT?  │          │   HOW?   │      │  WITH    │
    │        │          │          │      │  WHAT?   │
    └────┬───┘          └─────┬────┘      └─────┬────┘
         │                    │                  │
         ▼                    ▼                  ▼
    ┌─────────┐        ┌──────────────┐   ┌──────────────┐
    │ GHERKIN │        │    STATE     │   │  FIT TABLES  │
    │         │        │   MACHINE    │   │              │
    │ Stories │        │              │   │  Test Cases  │
    │ Behavior│        │  Lifecycle   │   │  w/ Data     │
    └─────────┘        └──────────────┘   └──────────────┘
```

---

## Quick Reference: Which Spec Do I Use?

| I Want To... | Use This | Location |
|--------------|----------|----------|
| Understand what the actor does | **Gherkin** | `specs/features/*.feature` |
| Know valid states and transitions | **State Machine** | `specs/state-machines/*-state-machine.md` |
| See specific test cases with data | **FIT Tables** | `specs/fit-fixtures/*.fit.md` |
| Verify my code is correct | **All Three** | Compare all specs to code |

---

## Example: "How do I append an event?"

### Step 1: Read the Gherkin (What happens?)

```gherkin
Scenario: Append a single event successfully
  Given an EventLogActor that is running
  When I append an event with type "user.created"
  Then the append should succeed
  And the response should include an event ID
```

**Answer**: When running, append succeeds and returns an event ID.

### Step 2: Check the State Machine (When can I do it?)

```markdown
| Current State | Event | Valid? |
|---------------|-------|--------|
| RUNNING | append() | ✓ YES |
| STOPPED | append() | ✗ NO - must start first |
```

**Answer**: Only works in RUNNING state.

### Step 3: Look at FIT Tables (What exactly happens?)

```markdown
| Event Type | Has ID | Expected Success | ID Generated |
|-----------|--------|------------------|--------------|
| user.created | no | true | yes (evt_...) |
| user.created | yes | true | no (uses provided) |
```

**Answer**: If no ID provided, one is auto-generated.

### Step 4: Find it in Code

```javascript
// src/actors/event-log.js line 139-200

async appendEvent(eventData) {
  if (!this.isInitialized) {  // State check (RUNNING)
    await this.initialize();
  }

  const event = {
    id: eventData.id || `evt_${generateULID()}`,  // Auto-generate ID
    type: eventData.type,
    data: eventData.data || {}
  };

  // Validate and write...
  return { success: true, eventId: event.id };  // Return ID
}
```

---

## Reading Patterns Cheat Sheet

### Pattern: Understanding a New Actor

1. **Skim the Gherkin Feature description** (first 5 lines)
   - Tells you the actor's purpose

2. **Read the State Machine states** (STOPPED/RUNNING/etc.)
   - Tells you the lifecycle

3. **Scan Gherkin scenario titles** (just the "Scenario:" lines)
   - Tells you what operations are supported

4. **Pick one operation and trace it**:
   - Gherkin → What behavior looks like
   - State Machine → What state is required
   - FIT Table → What inputs/outputs are expected
   - Code → How it's implemented

### Pattern: Debugging a Failed Test

1. **Check FIT Table** for the exact input/output combination
2. **Verify State Machine** that you're in the right state
3. **Read Gherkin** to understand the full scenario context
4. **Compare to Code** to find the mismatch

### Pattern: Adding a New Feature

1. **Write Gherkin scenario** describing the behavior
2. **Update State Machine** if new states/transitions needed
3. **Add FIT Table rows** for input/output cases
4. **Implement code** to match all three specs

---

## Visual State Machine Example

### EventLogActor States

```
     START                              STOP
       ↓                                 ↓
  ┌─────────┐        start()        ┌─────────┐
  │ STOPPED │ ───────────────────→  │ RUNNING │
  │         │                       │         │
  │ Can:    │                       │ Can:    │
  │ - start │                       │ - append│
  │ - status│                       │ - query │
  │         │                       │ - stop  │
  │ Cannot: │        stop()         │ - status│
  │ - append│ ←─────────────────── │         │
  └─────────┘                       └─────────┘
      ↑                                 ↓
      │                                 │
      └─────────────────────────────────┘
           (file closed)           (file open)
```

### FunctionExecutorActor States

```
     START                              STOP
       ↓                                 ↓
  ┌─────────┐        start()        ┌─────────┐
  │ STOPPED │ ───────────────────→  │ RUNNING │
  │         │                       │         │
  │ Can:    │                       │ Can:    │
  │ - start │                       │ - execute│
  │ - status│                       │ - stop  │
  │         │                       │ - status│
  │ Cannot: │        stop()         │         │
  │ - execute│ ←──────────────────  │         │
  └─────────┘                       └─────────┘
```

---

## Concrete Code Examples

### Example 1: EventLogActor Lifecycle

**Spec says** (State Machine):
```markdown
STOPPED → start() → RUNNING
Actions: Create directory, open stream, set isInitialized = true
```

**Code does** (event-log.js):
```javascript
async start() {
  await fs.mkdir(logDir, { recursive: true });     // Create directory
  this.writeStream = createWriteStream(this.logPath); // Open stream
  this.isInitialized = true;                       // Set flag
  return { success: true };
}
```

**Verification**: ✓ All actions match

### Example 2: FunctionExecutor Input Validation

**Spec says** (FIT Table):
```markdown
| functionId | functionPath | event | Expected Success |
|------------|-------------|-------|------------------|
| null       | /valid      | {...} | false            |
```

**Code does** (function-executor.js):
```javascript
if (!functionId) {
  return createMessage(PROTOCOLS.FUNCTION, ACTIONS.ERROR, {
    error: 'functionId is required',
    functionId
  });
}
```

**Verification**: ✓ Returns error when functionId is null

### Example 3: Event Depth Tracking

**Spec says** (Gherkin):
```gherkin
Given the triggering event has depth 2
When I execute the function
Then the emitted event should have depth 3
```

**Code does** (function-executor.js):
```javascript
const eventWithMetadata = {
  ...newEvent,
  metadata: {
    ...newEvent.metadata,
    depth: (event.metadata?.depth || 0) + 1  // 2 + 1 = 3
  }
};
```

**Verification**: ✓ Increments depth by 1

---

## Common Questions

### Q: Do I need to read all three specs every time?

**A:** No. Use the right tool for the job:

- Understanding behavior? → Gherkin
- Checking if operation is valid? → State Machine
- Testing specific inputs? → FIT Tables
- Comprehensive verification? → All three

### Q: What if specs and code don't match?

**A:** The spec is the source of truth. Either:

1. The code has a bug (fix the code)
2. The spec is outdated (update the spec)
3. The spec was wrong (discuss with team, update spec, then code)

### Q: Can I skip writing specs and just write code?

**A:** No. Specs prevent:

- Undocumented behavior
- Missing edge cases
- Impossible states
- Untested code paths
- Breaking changes without warning

### Q: Which spec do I write first?

**A:** Usually this order:

1. **State Machine** - Define states and lifecycle
2. **Gherkin** - Describe user-facing behavior
3. **FIT Tables** - Specify exact input/output cases
4. **Code** - Implement to match all three

### Q: How do I know if my code is correct?

**A:** Checklist:

- [ ] Every Gherkin scenario has a code path
- [ ] Every state transition is implemented
- [ ] Every invariant is enforced
- [ ] Every FIT table row passes as a test
- [ ] No code exists without a spec

---

## Speed Reading Tips

### Gherkin: Read the Titles

```gherkin
Feature: EventLogActor                              ← Read this
  Scenario: Start EventLogActor successfully        ← Read this
  Scenario: Append a single event successfully      ← Read this
  Scenario: Query events with type filter           ← Read this
```

You can scan scenario titles to quickly understand what the actor does.

### State Machine: Read the Table

```markdown
| Current State | Event | Next State |  ← Just scan this column
|---------------|-------|------------|
| STOPPED       | start | RUNNING    |  ← Key transition
| RUNNING       | stop  | STOPPED    |  ← Key transition
```

The transition table tells you 80% of what you need to know.

### FIT Tables: Read First and Last Rows

```markdown
| Input | Expected Output |
|-------|----------------|
| valid | success        |  ← Happy path (first)
| ...   | ...            |
| null  | error          |  ← Error case (last)
```

First row = happy path, last rows = edge cases.

---

## Test Yourself

### Quiz 1: EventLogActor

**Question**: Can I append an event when the actor is stopped?

<details>
<summary>Click to reveal answer</summary>

**Answer**: No

**Why?**
- State Machine says: append() only valid in RUNNING state
- Gherkin says: "Given an EventLogActor that is running"
- Code: Should check `isInitialized` flag

</details>

### Quiz 2: FunctionExecutor

**Question**: If I execute a function without providing a functionId, what happens?

<details>
<summary>Click to reveal answer</summary>

**Answer**: Returns error "functionId is required"

**Why?**
- FIT Table (Table 3) shows: functionId=null → Expected Error="functionId is required"
- Code (line 146-150): Validates and returns error
- Gherkin (line 163-167): "Reject execution without functionId"

</details>

### Quiz 3: Event Depth

**Question**: An event with depth=5 triggers a function that emits an event. What depth does the new event have?

<details>
<summary>Click to reveal answer</summary>

**Answer**: depth=6

**Why?**
- FIT Table (Table 18): depth is incremented by 1
- Code: `depth: (event.metadata?.depth || 0) + 1`
- Gherkin (line 149-152): "emitted event should have depth 3" (when input is 2)

</details>

---

## Next Steps

1. **Read the full guide**: [SPECIFICATION_GUIDE.md](./SPECIFICATION_GUIDE.md)
2. **Pick an actor**: Start with EventLogActor (simplest)
3. **Follow the example**: Trace one operation through all three specs
4. **Verify code**: Open the source file and find the matching code
5. **Write a test**: Use FIT tables as test cases

---

## File Locations

```
event-system/
├── specs/
│   ├── features/              ← Gherkin (.feature files)
│   │   ├── event-log-actor.feature
│   │   ├── function-executor-actor.feature
│   │   └── ...
│   ├── state-machines/        ← State machines (.md files)
│   │   ├── event-log-actor-state-machine.md
│   │   ├── function-executor-actor-state-machine.md
│   │   └── ...
│   └── fit-fixtures/          ← FIT tables (.fit.md files)
│       ├── event-log-actor.fit.md
│       ├── function-executor-actor.fit.md
│       └── ...
└── src/
    └── actors/                ← Implementation (.js files)
        ├── event-log.js
        ├── function-executor.js
        └── ...
```

---

## Summary

**Three specs, one truth:**

1. **Gherkin** = The story (what happens)
2. **State Machine** = The rules (when it's allowed)
3. **FIT Tables** = The details (specific cases)

**All three point to:**

4. **Code** = The implementation (how it works)

**When they all match: ✓ Correct system**

**When they don't match: ✗ Bug or outdated spec**
