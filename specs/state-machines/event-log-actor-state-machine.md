# EventLogActor State Machine Specification

## Overview

EventLogActor manages the append-only event log file (JSONL format) with lifecycle-managed file stream resources.

**Actors**: EventLogActor
**States**: STOPPED, RUNNING
**Resources**: File write stream, event counter
**File**: `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/event-log.js`

---

## State Definitions

### STOPPED (isInitialized = false)
**Description**: Actor is not operational, no file stream is open.

**Invariants**:
- `isInitialized === false`
- `writeStream === null`
- `eventCount` retains last known value (persisted state)

**Entry Actions**: None (initial state)

**Exit Actions**: None

**Valid Operations**:
- `start()` - Initialize and open write stream
- `getStatus()` - Report stopped status

---

### RUNNING (isInitialized = true)
**Description**: Actor is operational with an active write stream to events.jsonl.

**Invariants**:
- `isInitialized === true`
- `writeStream !== null` (active file stream)
- `eventCount >= 0` (tracks total events)
- `logPath` is valid file path

**Entry Actions**:
1. Ensure log directory exists
2. Count existing events from file
3. Create append write stream to logPath
4. Set `isInitialized = true`

**Exit Actions**:
1. Flush and close write stream
2. Set `isInitialized = false`
3. Set `writeStream = null`

**Valid Operations**:
- `append(event)` - Write event to log
- `query(options)` - Read events from log
- `stop()` - Close stream and shutdown
- `getStatus()` - Report running status with metrics

---

## State Transition Table

| Current State | Event | Guards | Next State | Actions | Error Handling |
|---------------|-------|--------|------------|---------|----------------|
| STOPPED | `start()` called | None | RUNNING | 1. Create log directory<br>2. Count existing events<br>3. Open write stream (append mode)<br>4. Set `isInitialized = true`<br>5. Return `{ success: true }` | On error:<br>- Remain in STOPPED<br>- Return `{ success: false, error: message }` |
| STOPPED | `start()` called | Log directory creation fails | STOPPED | 1. Return `{ success: false, error: "mkdir failed" }` | Remain in STOPPED |
| STOPPED | `start()` called | Write stream creation fails | STOPPED | 1. Return `{ success: false, error: "stream failed" }` | Remain in STOPPED |
| STOPPED | `stop()` called | None | STOPPED | 1. Return `{ success: true }` (idempotent) | No error possible |
| STOPPED | `getStatus()` called | None | STOPPED | 1. Return `{ isRunning: false, eventCount, logPath }` | No error possible |
| STOPPED | `append()` called | None | STOPPED | 1. Throw error or return failure | Actor not initialized |
| STOPPED | `query()` called | None | STOPPED | 1. Read directly from file (no stream needed)<br>2. Return events | On error:<br>- Return empty array or error |
| RUNNING | `start()` called | Already initialized | RUNNING | 1. Return `{ success: false, error: "Already running" }` | No state change |
| RUNNING | `stop()` called | None | STOPPED | 1. Flush write stream<br>2. Close write stream (wait for 'finish')<br>3. Set `isInitialized = false`<br>4. Set `writeStream = null`<br>5. Return `{ success: true }` | On error:<br>- Best effort to close<br>- Return `{ success: false, error: message }` |
| RUNNING | `stop()` called | Stream close fails | STOPPED | 1. Force `isInitialized = false`<br>2. Force `writeStream = null`<br>3. Return `{ success: false, error: "close failed" }` | Transition anyway to prevent stuck state |
| RUNNING | `getStatus()` called | None | RUNNING | 1. Return `{ isRunning: true, eventCount, logPath }` | No error possible |
| RUNNING | `append(event)` called | Valid event object | RUNNING | 1. Validate event has required fields<br>2. Write JSONL line to stream<br>3. Increment eventCount<br>4. Return event with ID | On error:<br>- Remain RUNNING<br>- Log error but don't crash |
| RUNNING | `append(event)` called | Invalid event | RUNNING | 1. Return `{ success: false, error: "Invalid event" }` | No state change |
| RUNNING | `query(options)` called | None | RUNNING | 1. Read from log file<br>2. Parse JSONL<br>3. Filter by options<br>4. Return event array | On error:<br>- Return empty array or error<br>- Remain RUNNING |
| RUNNING | Write stream error | Stream emits 'error' | RUNNING | 1. Log error<br>2. Emit error event<br>3. Attempt to recover or remain degraded | May need manual intervention |

---

## State Diagram

```
                 ┌──────────────────────────────┐
                 │                              │
                 │  start() [error]             │
                 │  append() [not initialized]  │
                 │  stop() [idempotent]         │
                 ▼                              │
            ┌─────────┐                         │
            │ STOPPED │◄────────────────────────┼───────┐
            └────┬────┘                         │       │
                 │                              │       │
                 │ start()                      │       │
                 │ [success]                    │       │
                 ▼                              │       │
         ┌───────────────┐                      │       │
         │   Create dir  │                      │       │
         │   Open stream │                      │       │
         └───────┬───────┘                      │       │
                 │                              │       │
                 ▼                              │       │
            ┌─────────┐                         │       │
            │ RUNNING │─────────────────────────┘       │
            └────┬────┘                                 │
                 │                                      │
                 │ stop()                               │
                 │ [success]                            │
                 ▼                                      │
         ┌──────────────┐                              │
         │ Close stream │                              │
         │ Cleanup      │──────────────────────────────┘
         └──────────────┘
```

---

## Transition Guards

### start() Guards
- **Not Already Running**: `isInitialized === false`
- **Valid Configuration**: `logPath` is defined and valid
- **Directory Accessible**: Can create parent directory
- **File Writable**: Can open file for appending

### stop() Guards
- **No Guards**: Always safe to call (idempotent)

### append() Guards
- **Is Running**: `isInitialized === true`
- **Valid Event**: Event object has required fields
- **Stream Available**: `writeStream !== null`

### query() Guards
- **File Exists**: Log file exists (can work in STOPPED state)

---

## Error States and Recovery

### Error Scenarios

1. **Log Directory Creation Failure**
   - **State**: Remain STOPPED
   - **Recovery**: Fix permissions, retry start()

2. **Write Stream Open Failure**
   - **State**: Remain STOPPED
   - **Recovery**: Fix file permissions, retry start()

3. **Write Stream Error During Operation**
   - **State**: Remain RUNNING but degraded
   - **Recovery**: Stop and restart actor, check disk space

4. **Close Stream Failure**
   - **State**: Force transition to STOPPED
   - **Recovery**: Check for leaked resources, restart if needed

5. **Append Failure**
   - **State**: Remain RUNNING
   - **Recovery**: Event is lost, caller should retry

6. **Query Failure**
   - **State**: No change (STOPPED or RUNNING)
   - **Recovery**: Return empty array, caller can retry

---

## State Invariants

### Global Invariants (all states)
- `eventCount >= 0`
- `logPath` is a non-empty string
- If `isInitialized === true`, then `writeStream !== null`
- If `isInitialized === false`, then `writeStream === null`

### STOPPED Invariants
- `isInitialized === false`
- `writeStream === null`
- Cannot call `append()` (will fail)

### RUNNING Invariants
- `isInitialized === true`
- `writeStream !== null`
- `writeStream.writable === true` (stream is active)
- Can call `append()` and `query()`

---

## Lifecycle Examples

### Example 1: Normal Startup and Shutdown
```javascript
const eventLog = new EventLogActor({ eventLog: { file: 'events.jsonl' } });

// Initial state: STOPPED
console.log(eventLog.getStatus()); // { isRunning: false, eventCount: 0, logPath: 'events.jsonl' }

// Transition: STOPPED -> RUNNING
await eventLog.start(); // { success: true }
console.log(eventLog.getStatus()); // { isRunning: true, eventCount: 0, logPath: 'events.jsonl' }

// Operations in RUNNING state
await eventLog.append({ type: 'test', data: {} }); // Event written

// Transition: RUNNING -> STOPPED
await eventLog.stop(); // { success: true }
console.log(eventLog.getStatus()); // { isRunning: false, eventCount: 1, logPath: 'events.jsonl' }
```

### Example 2: Idempotent Operations
```javascript
// Multiple start() calls
await eventLog.start(); // { success: true }
await eventLog.start(); // { success: false, error: "Already running" }

// Multiple stop() calls
await eventLog.stop(); // { success: true }
await eventLog.stop(); // { success: true } (idempotent)
```

### Example 3: Error Handling
```javascript
// Start with invalid path
const eventLog = new EventLogActor({ eventLog: { file: '/invalid/path/events.jsonl' } });
await eventLog.start(); // { success: false, error: "mkdir failed: ..." }
console.log(eventLog.getStatus()); // { isRunning: false, ... } (remained STOPPED)
```

---

## Testing Checklist

- [ ] Start from STOPPED transitions to RUNNING
- [ ] Start from RUNNING returns error
- [ ] Stop from RUNNING transitions to STOPPED
- [ ] Stop from STOPPED is idempotent
- [ ] Append in RUNNING works correctly
- [ ] Append in STOPPED fails appropriately
- [ ] Query works in both states
- [ ] getStatus returns correct state
- [ ] Write stream is properly closed on stop
- [ ] Directory is created if missing
- [ ] Event count is accurate after restart
- [ ] Error during start leaves actor in STOPPED
- [ ] Error during stop forces transition to STOPPED

---

## Summary

EventLogActor has a simple two-state machine (STOPPED, RUNNING) with primary responsibility for managing file I/O resources. The state machine ensures:

1. **Resource Safety**: File streams are properly opened and closed
2. **Idempotency**: Operations are safe to retry
3. **Error Isolation**: Failures don't corrupt state
4. **Graceful Degradation**: Errors are handled without crashing
5. **Operational Visibility**: Status clearly indicates current state

The specification provides complete transition coverage for all events and error conditions.
