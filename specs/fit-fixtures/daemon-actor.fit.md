# DaemonActor - FIT Decision Tables

FIT-style decision tables for testing DaemonActor behavior declaratively.

## Table 1: Lifecycle State Transitions

Tests the core lifecycle methods: start(), stop(), getStatus()

| Initial State | Action | Expected State | Expected Success | Expected Error/Message |
|--------------|--------|----------------|------------------|------------------------|
| STOPPED | start() | RUNNING | true | - |
| STOPPED | stop() | STOPPED | true | - |
| STOPPED | getStatus() | STOPPED | - | state=stopped, pid present |
| RUNNING | start() | RUNNING | false | Cannot start daemon in running state |
| RUNNING | stop() | STOPPED | true | - |
| RUNNING | getStatus() | RUNNING | - | state=running, uptime > 0 |
| STARTING | start() | STARTING | false | Cannot start daemon in starting state |
| STOPPING | stop() | STOPPING | true | already stopping |
| ERROR | start() | ERROR | false | Cannot start daemon in error state |

## Table 2: Configuration Loading

Tests configuration file loading and parsing.

| Config File Exists | Config Valid JSON | Expected Success | Expected Behavior |
|-------------------|------------------|------------------|-------------------|
| yes | yes | true | Config loaded into daemon.config |
| yes | no | false | Parse error, state=ERROR |
| no | - | false | File not found, state=ERROR |
| yes (empty) | yes | true | Empty config object |

## Table 3: Actor Spawning - EventLogActor

Tests spawning of EventLogActor during daemon startup.

| EventLogActor Available | Expected Behavior | Expected actors.eventLog |
|------------------------|-------------------|-------------------------|
| yes (module exists) | Spawned and started | EventLogActor instance |
| no (module missing) | Fallback placeholder | {state: 'not_implemented'} |
| yes (start() fails) | Error propagated | - |

## Table 4: Startup Sequence

Tests the complete daemon startup sequence.

| Step | Expected Order | Expected Success |
|------|---------------|------------------|
| 1. Load config | first | varies |
| 2. Spawn EventLogActor | second | varies |
| 3. Setup signal handlers | third | true |
| 4. Set state to RUNNING | fourth | true |

## Table 5: Shutdown Sequence

Tests the graceful shutdown sequence.

| Initial State | Expected Shutdown Steps | Expected Final State |
|--------------|------------------------|---------------------|
| RUNNING | Stop EventLogActor → Run shutdown handlers → STOPPED | STOPPED |
| RUNNING (with handlers) | Stop actors → Run all handlers → STOPPED | STOPPED |
| STOPPING | Skip (already stopping) | STOPPED |
| STOPPED | Skip (already stopped) | STOPPED |

## Table 6: Signal Handling - SIGINT

Tests SIGINT (Ctrl+C) signal handling.

| Daemon State | Signal Received | Expected Behavior |
|-------------|----------------|-------------------|
| RUNNING | SIGINT | Graceful shutdown → process.exit(0) |
| STARTING | SIGINT | Graceful shutdown → process.exit(0) |
| STOPPED | SIGINT | No action needed |

## Table 7: Signal Handling - SIGTERM

Tests SIGTERM signal handling.

| Daemon State | Signal Received | Expected Behavior |
|-------------|----------------|-------------------|
| RUNNING | SIGTERM | Graceful shutdown → process.exit(0) |
| STARTING | SIGTERM | Graceful shutdown → process.exit(0) |
| STOPPED | SIGTERM | No action needed |

## Table 8: Signal Handling - Uncaught Exception

Tests handling of uncaught exceptions.

| Exception Type | Daemon State | Expected Behavior |
|---------------|-------------|-------------------|
| Error | RUNNING | State=ERROR → Shutdown → process.exit(1) |
| TypeError | RUNNING | State=ERROR → Shutdown → process.exit(1) |
| ReferenceError | RUNNING | State=ERROR → Shutdown → process.exit(1) |

## Table 9: Signal Handling - Unhandled Rejection

Tests handling of unhandled promise rejections.

| Rejection Reason | Daemon State | Expected Behavior |
|-----------------|-------------|-------------------|
| Error | RUNNING | State=ERROR → Shutdown → process.exit(1) |
| String | RUNNING | State=ERROR → Shutdown → process.exit(1) |
| null | RUNNING | State=ERROR → Shutdown → process.exit(1) |

## Table 10: Shutdown Handlers

Tests custom shutdown handler registration and execution.

| Handlers Registered | Expected Execution | Handler Errors |
|--------------------|-------------------|----------------|
| 0 | none | - |
| 1 | 1 handler called | handled gracefully |
| 3 | all 3 called in order | handled gracefully |
| 1 (throws error) | called, error logged | shutdown continues |

## Table 11: Status Information

Tests the getStatus() information returned.

| Daemon State | Expected Status Fields |
|-------------|----------------------|
| STOPPED | state, pid, uptime=0, startTime=null, error=null, config, actors |
| RUNNING | state, pid, uptime>0, startTime set, error=null, config, actors |
| ERROR | state, pid, uptime, startTime, error set, config, actors |

## Table 12: Actor Status in getStatus()

Tests that actor status is included in daemon status.

| EventLogActor State | Expected actors.eventLog Value |
|--------------------|------------------------------|
| spawned and running | 'running' or actor.state |
| not spawned | 'not_spawned' |
| not implemented | 'not_implemented' |

## Table 13: Error Handling - Config Load Failure

Tests error handling when config fails to load.

| Failure Reason | Expected State | Expected Error Field |
|---------------|----------------|---------------------|
| File not found | ERROR | error message set |
| Invalid JSON | ERROR | error message set |
| Permission denied | ERROR | error message set |

## Table 14: Error Handling - Actor Spawn Failure

Tests error handling when actor spawning fails.

| EventLogActor Status | Expected Behavior |
|---------------------|-------------------|
| Module loads, start() succeeds | RUNNING |
| Module loads, start() fails | ERROR (propagated) |
| Module fails to load | Fallback (not_implemented) |

## Table 15: Error Handling - Shutdown Failure

Tests error handling during shutdown.

| Failure During | Expected Behavior | Expected State |
|---------------|------------------|----------------|
| Actor stop() | Error logged, state=ERROR | ERROR |
| Shutdown handler | Error logged, continue | varies |
| Multiple failures | All logged, state=ERROR | ERROR |

## Table 16: Process Information

Tests that process information is correctly tracked.

| Field | Expected Value | When Available |
|-------|---------------|---------------|
| pid | process.pid | always |
| startTime | ISO timestamp | after start() |
| uptime | milliseconds | when running |
| state | STATE enum | always |

## Table 17: Config Path Options

Tests different configuration file path options.

| Config Path | Expected Behavior |
|------------|------------------|
| ./config.json | Loaded from CWD |
| /absolute/path/config.json | Loaded from absolute path |
| ../relative/config.json | Loaded from relative path |
| config.json (default) | Loaded from CWD |

## Table 18: Multiple Start/Stop Cycles

Tests that daemon can be restarted multiple times.

| Action Sequence | Expected Behavior |
|----------------|-------------------|
| start() → stop() | Both succeed |
| start() → stop() → start() | All succeed |
| start() → stop() → start() → stop() | All succeed |
| (start/stop) x 10 | All succeed |

## Table 19: Actor Lifecycle Integration

Tests that daemon correctly manages actor lifecycles.

| Daemon Action | Expected Actor Action |
|--------------|----------------------|
| daemon.start() | actors started |
| daemon.stop() | actors stopped |
| daemon.start() → daemon.stop() | actors started then stopped |

## Table 20: Environment Variables

Tests environment-based configuration.

| Env Variable | Value | Expected Behavior |
|-------------|-------|-------------------|
| DEBUG | true | Debug logging enabled |
| DEBUG | false | Debug logging disabled |
| (none) | - | Default behavior |

## Table 21: Concurrent Operations

Tests behavior under concurrent operations.

| Operation | Expected Behavior |
|-----------|------------------|
| start() while starting | Rejected (not STOPPED) |
| stop() while stopping | Accepted (idempotent) |
| getStatus() while running | Returns current status |
| Signal during startup | Graceful shutdown |

## Table 22: Edge Cases - Actor References

Tests actor reference handling.

| Scenario | Expected Behavior |
|----------|------------------|
| EventLogActor is null | Handled gracefully in stop() |
| EventLogActor has no stop() | Error handled |
| EventLogActor.stop() throws | Error logged, shutdown continues |

## Table 23: State Consistency

Tests that state remains consistent across operations.

| Operation | State Before | State After Success | State After Failure |
|-----------|-------------|---------------------|-------------------|
| start() | STOPPED | RUNNING | ERROR |
| stop() | RUNNING | STOPPED | ERROR |
| load config | any | unchanged | ERROR |

## Table 24: Uptime Calculation

Tests that uptime is correctly calculated.

| Start Time | Current Time | Expected Uptime |
|-----------|-------------|-----------------|
| T | T | 0 |
| T | T+1000ms | ~1000ms |
| T | T+60000ms | ~60000ms |
| null | any | 0 |

## Table 25: Error State Recovery

Tests behavior when daemon is in ERROR state.

| Action | Expected Behavior |
|--------|------------------|
| start() | Rejected (must be STOPPED) |
| stop() | Attempt shutdown |
| getStatus() | Return error information |

## Implementation Notes

### Table Format
Each table represents a specific test scenario or behavior category. Tables can be executed by:
1. Setting up initial daemon state
2. Performing the action
3. Verifying expected outcomes

### Execution Strategy
- Tables 1-4: Core lifecycle and startup
- Tables 5-10: Shutdown and signal handling
- Tables 11-12: Status reporting
- Tables 13-15: Error handling
- Tables 16-25: Integration, edge cases, state management

### Success Criteria
- All lifecycle transitions work correctly
- Configuration is loaded properly
- Actors are spawned and managed correctly
- Signals are handled gracefully
- Shutdown handlers execute properly
- Error conditions are handled correctly
- State remains consistent
- Multiple start/stop cycles work
- Edge cases are handled properly

### State Diagram

```
┌─────────┐
│ STOPPED │
└────┬────┘
     │
     │ start()
     ▼
┌──────────┐
│ STARTING │
└────┬─────┘
     │ (success)
     ▼
┌─────────┐      ┌───────┐
│ RUNNING │◄─────┤ ERROR │
└────┬────┘      └───────┘
     │               ▲
     │ stop()        │ (error)
     ▼               │
┌──────────┐        │
│ STOPPING ├────────┘
└────┬─────┘
     │
     ▼
┌─────────┐
│ STOPPED │
└─────────┘
```
