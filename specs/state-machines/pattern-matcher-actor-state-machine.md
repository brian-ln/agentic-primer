# PatternMatcherActor State Machine Specification

## Overview

PatternMatcherActor manages event pattern registration and matching with in-memory pattern storage.

**Actors**: PatternMatcherActor
**States**: STOPPED, RUNNING
**Resources**: In-memory Map of patterns
**File**: `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/pattern-matcher.js`

---

## State Definitions

### STOPPED (isRunning = false)
**Description**: Actor is not operational, pattern operations are not available.

**Invariants**:
- `isRunning === false`
- `patterns` Map exists but operations are disabled
- Pattern count is preserved (data persists)

**Entry Actions**:
- Set `isRunning = false`

**Exit Actions**: None

**Valid Operations**:
- `start()` - Enable pattern operations
- `getStatus()` - Report stopped status

---

### RUNNING (isRunning = true)
**Description**: Actor is operational and can register/match patterns.

**Invariants**:
- `isRunning === true`
- `patterns` is a Map instance
- `patterns.size >= 0`
- All stored patterns are valid objects

**Entry Actions**:
1. Set `isRunning = true`
2. Initialize patterns Map (if not exists)
3. Return success

**Exit Actions**:
1. Set `isRunning = false`
2. Patterns remain in memory (data preserved)

**Valid Operations**:
- `registerPattern(pattern)` - Add pattern to registry
- `matchEvent(event)` - Find matching patterns for event
- `getPattern(patternId)` - Retrieve specific pattern
- `listPatterns()` - Get all patterns
- `removePattern(patternId)` - Delete pattern
- `stop()` - Disable operations
- `getStatus()` - Report running status

---

## State Transition Table

| Current State | Event | Guards | Next State | Actions | Error Handling |
|---------------|-------|--------|------------|---------|----------------|
| STOPPED | `start()` called | None | RUNNING | 1. Initialize patterns Map (if needed)<br>2. Set `isRunning = true`<br>3. Return `{ success: true, message: "PatternMatcherActor started" }` | On error:<br>- Remain in STOPPED<br>- Return `{ success: false, error: message }` |
| STOPPED | `start()` called | Initialization fails | STOPPED | 1. Return `{ success: false, error: "Init failed" }` | Remain in STOPPED |
| STOPPED | `stop()` called | None | STOPPED | 1. Return `{ success: true, message: "Was not running" }` | No error possible |
| STOPPED | `getStatus()` called | None | STOPPED | 1. Return `{ isRunning: false, patternCount: patterns.size }` | No error possible |
| STOPPED | `registerPattern()` called | None | STOPPED | 1. Return error or throw exception | Actor not running |
| STOPPED | `matchEvent()` called | None | STOPPED | 1. Return empty array or error | Actor not running |
| STOPPED | `listPatterns()` called | None | STOPPED | 1. Return empty array or error | Actor not running |
| RUNNING | `start()` called | Already running | RUNNING | 1. Return `{ success: false, error: "Already running" }` | No state change |
| RUNNING | `stop()` called | None | STOPPED | 1. Set `isRunning = false`<br>2. Preserve patterns Map in memory<br>3. Return `{ success: true, message: "PatternMatcherActor stopped" }` | On error:<br>- Force `isRunning = false`<br>- Return `{ success: false, error: message }` |
| RUNNING | `getStatus()` called | None | RUNNING | 1. Return `{ isRunning: true, patternCount: patterns.size }` | No error possible |
| RUNNING | `registerPattern(pattern)` called | Valid pattern | RUNNING | 1. Validate pattern object<br>2. Generate pattern ID (if needed)<br>3. Store in `patterns` Map<br>4. Return pattern with ID | On error:<br>- Remain RUNNING<br>- Return error object |
| RUNNING | `registerPattern(pattern)` called | Invalid pattern | RUNNING | 1. Validate fails<br>2. Return `{ success: false, error: "Invalid pattern" }` | No state change |
| RUNNING | `matchEvent(event)` called | Valid event | RUNNING | 1. Iterate through all patterns<br>2. Test each pattern against event<br>3. Collect matching patterns<br>4. Return array of matches | On error:<br>- Return empty array<br>- Remain RUNNING |
| RUNNING | `matchEvent(event)` called | Invalid event | RUNNING | 1. Return empty array or error | No state change |
| RUNNING | `getPattern(id)` called | Pattern exists | RUNNING | 1. Look up pattern by ID<br>2. Return pattern object | No error possible |
| RUNNING | `getPattern(id)` called | Pattern not found | RUNNING | 1. Return null or undefined | No error possible |
| RUNNING | `listPatterns()` called | None | RUNNING | 1. Convert patterns Map to array<br>2. Return array of pattern objects | On error:<br>- Return empty array |
| RUNNING | `removePattern(id)` called | Pattern exists | RUNNING | 1. Delete from patterns Map<br>2. Return `{ success: true }` | No error possible |
| RUNNING | `removePattern(id)` called | Pattern not found | RUNNING | 1. Return `{ success: false }` or succeed silently | No error possible |

---

## State Diagram

```
                 ┌────────────────────────────────┐
                 │                                │
                 │  start() [already running]     │
                 │  registerPattern() [disabled]  │
                 │  matchEvent() [disabled]       │
                 ▼                                │
            ┌─────────┐                           │
            │ STOPPED │◄──────────────────────────┼───────┐
            └────┬────┘                           │       │
                 │                                │       │
                 │ start()                        │       │
                 │ [success]                      │       │
                 ▼                                │       │
         ┌──────────────┐                         │       │
         │  Initialize  │                         │       │
         │  patterns    │                         │       │
         └───────┬──────┘                         │       │
                 │                                │       │
                 ▼                                │       │
            ┌─────────┐                           │       │
            │ RUNNING │───────────────────────────┘       │
            └────┬────┘                                   │
                 │  Pattern operations active             │
                 │                                        │
                 │ stop()                                 │
                 │ [success]                              │
                 │                                        │
                 └────────────────────────────────────────┘
```

---

## Transition Guards

### start() Guards
- **Not Already Running**: `isRunning === false`
- **Map Initializable**: Can create new Map() (always true in JS)

### stop() Guards
- **No Guards**: Always safe to call (idempotent)

### registerPattern() Guards
- **Is Running**: `isRunning === true`
- **Valid Pattern**: Pattern object has required fields (type, match criteria)
- **Pattern Type Valid**: Pattern type is supported

### matchEvent() Guards
- **Is Running**: `isRunning === true`
- **Valid Event**: Event object has required fields (type, data)

### getPattern() Guards
- **Is Running**: `isRunning === true`

### removePattern() Guards
- **Is Running**: `isRunning === true`

---

## Error States and Recovery

### Error Scenarios

1. **Start Fails (Very Rare)**
   - **State**: Remain STOPPED
   - **Recovery**: Retry start(), check memory

2. **Invalid Pattern Registration**
   - **State**: Remain RUNNING
   - **Recovery**: Fix pattern object, retry registration

3. **Match Error (Pattern Test Throws)**
   - **State**: Remain RUNNING
   - **Recovery**: Return empty array, log error, continue

4. **Memory Exhaustion (Too Many Patterns)**
   - **State**: Remain RUNNING but degraded
   - **Recovery**: Remove old patterns, implement LRU cache

5. **Operations Called When Stopped**
   - **State**: Remain STOPPED
   - **Recovery**: Start actor before operations

---

## State Invariants

### Global Invariants (all states)
- `patterns` is always a Map instance
- `patterns.size >= 0`
- All patterns have unique IDs
- Pattern data persists across stop/start cycles

### STOPPED Invariants
- `isRunning === false`
- `patterns` Map exists (data preserved)
- Cannot register or match patterns

### RUNNING Invariants
- `isRunning === true`
- `patterns` Map is accessible
- All pattern operations are available
- Patterns can be registered, matched, retrieved, removed

---

## Pattern Data Structure

### Pattern Object
```javascript
{
  id: string,              // Unique pattern ID
  type: string,            // Event type to match
  conditions: {            // Match conditions
    field: value,          // Field-value pairs
    ...
  },
  action: string,          // Action to trigger on match
  metadata: {              // Optional metadata
    created: timestamp,
    description: string
  }
}
```

### Pattern Matching Logic
1. **Type Match**: Event type must equal pattern type (or pattern type is wildcard)
2. **Condition Match**: All pattern conditions must match event data fields
3. **Return**: Array of matching pattern objects

---

## Lifecycle Examples

### Example 1: Normal Startup and Shutdown
```javascript
const matcher = createPatternMatcher();

// Initial state: STOPPED
console.log(matcher.getStatus());
// { isRunning: false, patternCount: 0 }

// Transition: STOPPED -> RUNNING
await matcher.start();
// { success: true, message: "PatternMatcherActor started" }

console.log(matcher.getStatus());
// { isRunning: true, patternCount: 0 }

// Register patterns in RUNNING state
matcher.registerPattern({
  type: 'user.login',
  conditions: { source: 'web' },
  action: 'notify-security'
});

console.log(matcher.getStatus());
// { isRunning: true, patternCount: 1 }

// Transition: RUNNING -> STOPPED
await matcher.stop();
// { success: true, message: "PatternMatcherActor stopped" }

console.log(matcher.getStatus());
// { isRunning: false, patternCount: 1 } (patterns preserved)
```

### Example 2: Pattern Matching
```javascript
await matcher.start();

// Register pattern
matcher.registerPattern({
  id: 'p1',
  type: 'user.login',
  conditions: { source: 'web' }
});

// Match event
const event = { type: 'user.login', data: { source: 'web', user: 'alice' } };
const matches = matcher.matchEvent(event);
// [{ id: 'p1', type: 'user.login', conditions: { source: 'web' } }]

// Non-matching event
const event2 = { type: 'user.logout', data: { user: 'alice' } };
const matches2 = matcher.matchEvent(event2);
// []
```

### Example 3: Idempotent Operations
```javascript
await matcher.start(); // { success: true }
await matcher.start(); // { success: false, error: "Already running" }

await matcher.stop(); // { success: true }
await matcher.stop(); // { success: true, message: "Was not running" }
```

### Example 4: Operations When Stopped
```javascript
// Actor is STOPPED
matcher.registerPattern({ type: 'test' });
// Error: Actor not running

await matcher.start();
matcher.registerPattern({ type: 'test' }); // OK
await matcher.stop();

// Pattern data preserved
console.log(matcher.getStatus());
// { isRunning: false, patternCount: 1 }
```

### Example 5: Pattern Persistence
```javascript
await matcher.start();
matcher.registerPattern({ id: 'p1', type: 'test' });
// patternCount: 1

await matcher.stop();
// patternCount: 1 (data preserved)

await matcher.start();
// patternCount: 1 (data still there)

const pattern = matcher.getPattern('p1');
// { id: 'p1', type: 'test' } (retrieved successfully)
```

---

## Testing Checklist

- [ ] Start from STOPPED transitions to RUNNING
- [ ] Start from RUNNING returns error
- [ ] Stop from RUNNING transitions to STOPPED
- [ ] Stop from STOPPED is idempotent
- [ ] getStatus returns correct state and count
- [ ] registerPattern works in RUNNING state
- [ ] registerPattern fails in STOPPED state
- [ ] matchEvent returns correct matches
- [ ] matchEvent returns empty array for non-matches
- [ ] matchEvent fails in STOPPED state
- [ ] getPattern retrieves correct pattern
- [ ] getPattern returns null for unknown ID
- [ ] listPatterns returns all patterns
- [ ] removePattern deletes pattern
- [ ] Pattern data persists across stop/start
- [ ] Invalid pattern registration returns error
- [ ] Pattern count is accurate

---

## Summary

PatternMatcherActor implements a simple two-state machine (STOPPED, RUNNING) with primary responsibility for managing in-memory pattern storage and matching logic. The state machine ensures:

1. **Data Persistence**: Patterns remain in memory across stop/start cycles
2. **Operational Safety**: Operations only allowed in RUNNING state
3. **Idempotency**: Start/stop operations are safe to retry
4. **Match Isolation**: Match errors don't corrupt state
5. **Operational Visibility**: Status reports pattern count
6. **Stateless Resources**: No file I/O or network resources to manage

The specification provides complete transition coverage for all events and pattern operations.
