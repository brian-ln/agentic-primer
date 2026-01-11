# DaemonActor State Machine Specification

## Overview

DaemonActor is the main orchestrator that manages the lifecycle of all other actors in the event system.

**Actors**: DaemonActor
**States**: STOPPED, STARTING, RUNNING, STOPPING, ERROR
**Resources**: Configuration file, spawned child actors, signal handlers
**File**: `/Users/bln/play/agentic-primer/.wt/event-system/src/daemon.js`

---

## State Definitions

### STOPPED (state = 'stopped')
**Description**: Daemon is not operational, no actors are running.

**Invariants**:
- `state === 'stopped'`
- `config === null`
- All child actors are null or stopped
- `startTime === null`
- `error === null`

**Entry Actions**:
- Set `state = 'stopped'`
- Clear `config`, `startTime`
- Set all actors to null

**Exit Actions**: None

**Valid Operations**:
- `start()` - Load config and start daemon
- `getStatus()` - Report stopped status

---

### STARTING (state = 'starting')
**Description**: Daemon is initializing, loading config, and spawning actors.

**Invariants**:
- `state === 'starting'`
- `config` may be partially loaded
- Actors are being spawned
- Not yet ready for operations

**Entry Actions**:
1. Set `state = 'starting'`
2. Set `startTime = new Date().toISOString()`
3. Begin configuration loading
4. Begin actor spawning sequence

**Exit Actions**:
- On success: Transition to RUNNING
- On failure: Transition to ERROR

**Valid Operations**:
- None (transient state)

**Typical Duration**: Milliseconds to seconds

---

### RUNNING (state = 'running')
**Description**: Daemon is fully operational, all actors are started and ready.

**Invariants**:
- `state === 'running'`
- `config !== null` (configuration loaded)
- `startTime !== null`
- All required actors are started
- Signal handlers are registered
- `error === null`

**Entry Actions**:
1. Set `state = 'running'`
2. Log startup success
3. Return success to caller

**Exit Actions**:
1. Prepare for shutdown
2. Transition to STOPPING state

**Valid Operations**:
- `emit(event)` - Emit events to system
- `query(options)` - Query event log
- `registerPattern(pattern)` - Register event patterns
- `registerFunction(name, fn)` - Register functions
- `executeFunction(name, args)` - Execute functions
- `stop()` - Initiate graceful shutdown
- `getStatus()` - Report running status
- Signal handling (SIGINT, SIGTERM)

---

### STOPPING (state = 'stopping')
**Description**: Daemon is shutting down gracefully, stopping all actors.

**Invariants**:
- `state === 'stopping'`
- Actors are being stopped in reverse order
- No new operations accepted
- Shutdown in progress

**Entry Actions**:
1. Set `state = 'stopping'`
2. Log shutdown initiation
3. Begin actor shutdown sequence

**Exit Actions**:
- On success: Transition to STOPPED
- On failure: Transition to ERROR (but force cleanup)

**Valid Operations**:
- None (transient state, waiting for cleanup)

**Typical Duration**: Milliseconds to seconds (waits for actor shutdown)

---

### ERROR (state = 'error')
**Description**: Daemon encountered an error during startup, operation, or shutdown.

**Invariants**:
- `state === 'error'`
- `error !== null` (error details available)
- `config` may be partial or null
- Actors may be in inconsistent state

**Entry Actions**:
1. Set `state = 'error'`
2. Set `error` with details
3. Log error details
4. Attempt partial cleanup

**Exit Actions**:
- Require manual recovery (restart)

**Valid Operations**:
- `stop()` - Force cleanup and return to STOPPED
- `start()` - Retry startup (after stop)
- `getStatus()` - Report error status

**Recovery**: Call `stop()` to cleanup, then `start()` to retry

---

## State Transition Table

| Current State | Event | Guards | Next State | Actions | Error Handling |
|---------------|-------|--------|------------|---------|----------------|
| STOPPED | `start()` called | None | STARTING | 1. Set `state = 'starting'`<br>2. Set `startTime`<br>3. Begin config load | On error: → ERROR |
| STOPPED | `stop()` called | None | STOPPED | 1. Return `{ success: true }` (idempotent) | No error possible |
| STOPPED | `getStatus()` called | None | STOPPED | 1. Return `{ state: 'stopped', uptime: 0 }` | No error possible |
| STARTING | Config loads successfully | Valid config | STARTING | 1. Store config<br>2. Begin actor spawning | On error: → ERROR |
| STARTING | Config load fails | Invalid/missing config | ERROR | 1. Set `error`<br>2. Return `{ success: false, error: message }` | Transition to ERROR |
| STARTING | EventLogActor spawned | Actor start succeeds | STARTING | 1. Store actor reference<br>2. Continue spawning | On error: → ERROR |
| STARTING | Actor spawn fails | Actor start fails | ERROR | 1. Set `error`<br>2. Return `{ success: false, error: message }` | Transition to ERROR |
| STARTING | All actors started | All required actors running | RUNNING | 1. Set `state = 'running'`<br>2. Setup signal handlers<br>3. Return success | - |
| RUNNING | `start()` called | Already running | RUNNING | 1. Return `{ success: false, error: "Cannot start in running state" }` | No state change |
| RUNNING | `stop()` called | None | STOPPING | 1. Set `state = 'stopping'`<br>2. Begin actor shutdown | On error: → ERROR |
| RUNNING | `getStatus()` called | None | RUNNING | 1. Calculate uptime<br>2. Gather actor statuses<br>3. Return complete status | No error possible |
| RUNNING | SIGINT/SIGTERM received | Signal handler active | STOPPING | 1. Log signal received<br>2. Set `state = 'stopping'`<br>3. Begin graceful shutdown | On error: → ERROR |
| RUNNING | `emit(event)` called | EventLog available | RUNNING | 1. Validate event<br>2. Forward to EventLogActor<br>3. Return result | On error:<br>- Return error<br>- Remain RUNNING |
| RUNNING | `query(options)` called | EventLog available | RUNNING | 1. Forward to EventLogActor<br>2. Return results | On error:<br>- Return error<br>- Remain RUNNING |
| RUNNING | Actor crashes | Child actor error | ERROR | 1. Set `state = 'error'`<br>2. Set `error` details<br>3. Log error | Transition to ERROR |
| STOPPING | All actors stopped | Shutdown complete | STOPPED | 1. Remove signal handlers<br>2. Set `state = 'stopped'`<br>3. Clear references<br>4. Return success | - |
| STOPPING | Actor stop fails | Timeout or error | ERROR | 1. Force cleanup<br>2. Set `error`<br>3. Log error | Transition to ERROR |
| STOPPING | Shutdown timeout | Takes too long | ERROR | 1. Force terminate actors<br>2. Set `error`<br>3. Transition to ERROR | Force cleanup |
| ERROR | `stop()` called | None | STOPPED | 1. Force cleanup all actors<br>2. Remove signal handlers<br>3. Set `state = 'stopped'`<br>4. Clear `error` | Best-effort cleanup |
| ERROR | `start()` called | None | STARTING | 1. Clear `error`<br>2. Reset state<br>3. Retry startup sequence | On error: → ERROR |
| ERROR | `getStatus()` called | None | ERROR | 1. Return `{ state: 'error', error: <details> }` | No error possible |

---

## State Diagram

```
                    ┌─────────────────────────────────────────┐
                    │                                         │
                    │                                         ▼
               ┌─────────┐    start()          ┌──────────────────────┐
            ┌──┤ STOPPED │────────────────────►│     STARTING         │
            │  └─────────┘                     │ (Load config,        │
            │       ▲                          │  Spawn actors)       │
            │       │                          └──┬────────────────┬──┘
            │       │                             │                │
            │       │                             │ success        │ error
            │       │                             ▼                ▼
            │       │                        ┌─────────┐      ┌───────┐
            │       │                        │ RUNNING │      │ ERROR │
            │       │                        └────┬────┘      └───┬───┘
            │       │                             │               │
            │       │        stop()               │               │
            │       │        SIGINT/SIGTERM       │               │
            │       │        ◄────────────────────┘               │
            │       │                                             │
            │  ┌──────────┐                                       │
            │  │ STOPPING │◄──────────────────────────────────────┘
            │  │ (Stop    │                       stop()
            │  │  actors) │
            │  └─────┬────┘
            │        │
            │        │ success
            └────────┘
```

---

## Actor Spawning Sequence

### Startup Order (STARTING state)
1. **Load Configuration** - Read config.json
2. **Spawn EventLogActor** - Start event logging first
3. **Spawn PatternMatcherActor** (future)
4. **Spawn FunctionRegistryActor** (future)
5. **Spawn FunctionExecutorActor** (future)
6. **Spawn HTTPServerActor** (future)
7. **Setup Signal Handlers** - SIGINT, SIGTERM
8. **Transition to RUNNING**

### Shutdown Order (STOPPING state)
1. **Remove Signal Handlers**
2. **Stop HTTPServerActor** (future) - Stop accepting requests
3. **Stop FunctionExecutorActor** (future) - Stop executing functions
4. **Stop PatternMatcherActor** (future)
5. **Stop FunctionRegistryActor** (future)
6. **Stop EventLogActor** - Close log file last
7. **Transition to STOPPED**

---

## Transition Guards

### start() Guards
- **Is Stopped**: `state === 'stopped'`
- **Config File Exists**: configPath points to valid file
- **Config Parseable**: JSON is valid

### stop() Guards
- **No Guards**: Always safe to call (idempotent, force cleanup if needed)

### Operations Guards (emit, query, etc.)
- **Is Running**: `state === 'running'`
- **Actor Available**: Required actor is spawned and running

---

## Error States and Recovery

### Error Scenarios

1. **Configuration Load Failure**
   - **State**: STARTING → ERROR
   - **Recovery**:
     - Call `stop()` to cleanup
     - Fix config.json
     - Call `start()` to retry

2. **Actor Spawn Failure**
   - **State**: STARTING → ERROR
   - **Recovery**:
     - Call `stop()` to cleanup partial state
     - Fix actor dependencies
     - Call `start()` to retry

3. **Actor Crash During Operation**
   - **State**: RUNNING → ERROR
   - **Recovery**:
     - Call `stop()` to cleanup
     - Investigate logs
     - Call `start()` to restart

4. **Graceful Shutdown Failure**
   - **State**: STOPPING → ERROR
   - **Recovery**:
     - Call `stop()` to force cleanup
     - Check for leaked resources
     - Call `start()` if needed

5. **Signal Handler Error**
   - **State**: RUNNING → ERROR
   - **Recovery**:
     - Manual intervention may be required
     - Check process state

---

## State Invariants

### Global Invariants (all states)
- `state` is one of: 'stopped', 'starting', 'running', 'stopping', 'error'
- If `state === 'running'`, then `config !== null`
- If `state === 'running'`, then all actors are started
- If `state === 'error'`, then `error !== null`

### STOPPED Invariants
- `state === 'stopped'`
- `config === null`
- `startTime === null`
- All actors are null
- No signal handlers registered

### STARTING Invariants
- Transient state (seconds)
- `state === 'starting'`
- `startTime !== null`
- Configuration loading in progress
- Actors being spawned

### RUNNING Invariants
- `state === 'running'`
- `config !== null`
- `startTime !== null`
- All required actors are running
- Signal handlers registered
- Can handle operations

### STOPPING Invariants
- Transient state (seconds)
- `state === 'stopping'`
- Actors being stopped in order
- No new operations accepted

### ERROR Invariants
- `state === 'error'`
- `error !== null` with error details
- Requires manual recovery

---

## Configuration Structure

### config.json
```json
{
  "eventLog": {
    "file": "events.jsonl"
  },
  "http": {
    "port": 3000,
    "host": "localhost"
  },
  "patterns": {},
  "functions": {}
}
```

---

## Lifecycle Examples

### Example 1: Normal Startup and Shutdown
```javascript
const daemon = new DaemonActor('./config.json');

// Initial state: STOPPED
console.log(daemon.getStatus());
// { state: 'stopped', uptime: 0 }

// Transition: STOPPED -> STARTING -> RUNNING
const result = await daemon.start();
// { success: true, state: 'running' }

console.log(daemon.getStatus());
// {
//   state: 'running',
//   config: { eventLog: {...}, http: {...} },
//   actors: { eventLog: { isRunning: true, ... } },
//   startTime: '2026-01-11T...',
//   uptime: 1234
// }

// Transition: RUNNING -> STOPPING -> STOPPED
await daemon.stop();
// { success: true }

console.log(daemon.getStatus());
// { state: 'stopped', uptime: 0 }
```

### Example 2: Configuration Error
```javascript
const daemon = new DaemonActor('./invalid.json');

// Transition: STOPPED -> STARTING -> ERROR
const result = await daemon.start();
// { success: false, error: "Failed to load config: ENOENT" }

console.log(daemon.getStatus());
// { state: 'error', error: "Failed to load config: ENOENT" }

// Recovery
await daemon.stop(); // Force cleanup
// Fix config file
await daemon.start(); // Retry
```

### Example 3: Actor Spawn Failure
```javascript
// EventLog directory is not writable
const daemon = new DaemonActor('./config.json');

const result = await daemon.start();
// { success: false, error: "Failed to spawn EventLogActor: EACCES" }

console.log(daemon.getStatus());
// { state: 'error', error: "..." }

// Recovery
await daemon.stop();
// Fix permissions
await daemon.start();
```

### Example 4: Signal Handling (SIGINT)
```javascript
const daemon = new DaemonActor();
await daemon.start();
// state: 'running'

// User presses Ctrl+C
// Signal handler catches SIGINT
// Transition: RUNNING -> STOPPING -> STOPPED

// Graceful shutdown:
// 1. Logs "Received SIGINT, shutting down..."
// 2. Stops all actors in order
// 3. Removes signal handlers
// 4. Exits cleanly
```

### Example 5: Idempotent Operations
```javascript
await daemon.start(); // { success: true, state: 'running' }
await daemon.start(); // { success: false, error: "Cannot start in running state" }

await daemon.stop(); // { success: true }
await daemon.stop(); // { success: true } (idempotent)
```

### Example 6: Operations in RUNNING State
```javascript
await daemon.start();
// state: 'running'

// Emit event
const event = await daemon.emit({ type: 'test', data: { foo: 'bar' } });
// { id: 'evt_...', type: 'test', ... }

// Query events
const events = await daemon.query({ type: 'test' });
// [{ id: 'evt_...', type: 'test', ... }]

// Get complete status
const status = daemon.getStatus();
// {
//   state: 'running',
//   config: {...},
//   actors: {
//     eventLog: { isRunning: true, eventCount: 1, ... }
//   },
//   startTime: '...',
//   uptime: 5000
// }
```

---

## Signal Handling

### Supported Signals

| Signal | Action | State Transition |
|--------|--------|------------------|
| SIGINT | Graceful shutdown | RUNNING → STOPPING → STOPPED |
| SIGTERM | Graceful shutdown | RUNNING → STOPPING → STOPPED |

### Signal Handler Implementation
```javascript
setupSignalHandlers() {
  process.on('SIGINT', async () => {
    console.log('[DaemonActor] Received SIGINT, shutting down...');
    await this.stop();
    process.exit(0);
  });

  process.on('SIGTERM', async () => {
    console.log('[DaemonActor] Received SIGTERM, shutting down...');
    await this.stop();
    process.exit(0);
  });
}
```

---

## Testing Checklist

- [ ] Start from STOPPED transitions through STARTING to RUNNING
- [ ] Start with missing config transitions to ERROR
- [ ] Start with invalid config transitions to ERROR
- [ ] Start from RUNNING returns error
- [ ] Stop from RUNNING transitions through STOPPING to STOPPED
- [ ] Stop from STOPPED is idempotent
- [ ] Stop from ERROR forces cleanup
- [ ] getStatus returns correct state in all states
- [ ] Configuration loads correctly
- [ ] EventLogActor spawns successfully
- [ ] Actor spawn failure transitions to ERROR
- [ ] Signal handlers are registered in RUNNING
- [ ] SIGINT triggers graceful shutdown
- [ ] SIGTERM triggers graceful shutdown
- [ ] Signal handlers are removed on stop
- [ ] Actors stop in correct order
- [ ] emit() works in RUNNING state
- [ ] query() works in RUNNING state
- [ ] Operations fail appropriately in non-RUNNING states
- [ ] Uptime calculation is accurate
- [ ] Error recovery works (stop then start)

---

## Summary

DaemonActor implements a five-state machine (STOPPED, STARTING, RUNNING, STOPPING, ERROR) as the main orchestrator for the event system. The state machine ensures:

1. **Orchestration**: Manages lifecycle of all child actors
2. **Configuration Management**: Loads and validates config.json
3. **Graceful Startup**: Actors start in correct order
4. **Graceful Shutdown**: Actors stop in reverse order
5. **Signal Handling**: Responds to SIGINT/SIGTERM
6. **Error Recovery**: Explicit error state with recovery path
7. **Resource Safety**: All resources are properly managed
8. **Operational Visibility**: Complete status including all actors

The specification provides complete transition coverage for all states, including transient states (STARTING, STOPPING), error handling, and signal processing.
