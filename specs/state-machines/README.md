# Event System State Machine Specifications

Complete state machine specifications for all actors in the Event System, including transition tables, guards, invariants, and error handling.

---

## Overview

This directory contains formal state machine specifications for all 6 actors in the Event System. Each specification includes:

- **State Definitions** - Complete description of each state with invariants
- **Transition Tables** - All possible transitions with guards and actions
- **State Diagrams** - Visual representation of state flow
- **Error Handling** - Error states and recovery procedures
- **Lifecycle Examples** - Code examples demonstrating transitions
- **Testing Checklists** - Verification requirements

---

## Actor Specifications

### 1. EventLogActor
**File**: [event-log-actor-state-machine.md](./event-log-actor-state-machine.md)

**States**: STOPPED, RUNNING
**Resources**: File write stream, event counter
**Complexity**: Simple (2 states)

Manages the append-only event log file (JSONL format) with lifecycle-managed file stream resources.

**Key Transitions**:
- STOPPED → RUNNING: Open write stream, count existing events
- RUNNING → STOPPED: Close write stream gracefully

---

### 2. HTTPServerActor
**File**: [http-server-actor-state-machine.md](./http-server-actor-state-machine.md)

**States**: STOPPED, STARTING, RUNNING, STOPPING, ERROR
**Resources**: Bun HTTP server, network port
**Complexity**: Complex (5 states)

Provides HTTP REST API endpoints for the event system with lifecycle-managed server resources.

**Key Transitions**:
- STOPPED → STARTING → RUNNING: Bind port, start server
- RUNNING → STOPPING → STOPPED: Stop server, release port
- Error handling for port conflicts

---

### 3. PatternMatcherActor
**File**: [pattern-matcher-actor-state-machine.md](./pattern-matcher-actor-state-machine.md)

**States**: STOPPED, RUNNING
**Resources**: In-memory Map of patterns
**Complexity**: Simple (2 states)

Manages event pattern registration and matching with in-memory pattern storage.

**Key Transitions**:
- STOPPED → RUNNING: Enable pattern operations
- RUNNING → STOPPED: Disable operations (data persists)

**Note**: Patterns persist in memory across stop/start cycles

---

### 4. FunctionRegistryActor
**File**: [function-registry-actor-state-machine.md](./function-registry-actor-state-machine.md)

**States**: STOPPED, RUNNING
**Resources**: In-memory Map of functions
**Complexity**: Simple (2 states)

Manages function registration and lookup with in-memory function storage.

**Key Transitions**:
- STOPPED → RUNNING: Enable function operations
- RUNNING → STOPPED: Disable operations (data persists)

**Note**: Functions persist in memory across stop/start cycles

---

### 5. FunctionExecutorActor
**File**: [function-executor-actor-state-machine.md](./function-executor-actor-state-machine.md)

**States**: STOPPED, RUNNING
**Resources**: Event emitter callback reference
**Complexity**: Simple (2 states)

Executes registered functions in response to events with lifecycle-managed execution state.

**Key Transitions**:
- STOPPED → RUNNING: Enable function execution
- RUNNING → STOPPED: Disable execution (emit callback persists)

**Note**: Execution errors are isolated and don't crash the actor

---

### 6. DaemonActor
**File**: [daemon-actor-state-machine.md](./daemon-actor-state-machine.md)

**States**: STOPPED, STARTING, RUNNING, STOPPING, ERROR
**Resources**: Configuration, spawned actors, signal handlers
**Complexity**: Very Complex (5 states)

Main orchestrator that manages the lifecycle of all other actors in the event system.

**Key Transitions**:
- STOPPED → STARTING → RUNNING: Load config, spawn all actors
- RUNNING → STOPPING → STOPPED: Stop all actors in reverse order
- Signal handling: SIGINT/SIGTERM trigger graceful shutdown

**Note**: Most complex actor, coordinates all other actors

---

## State Machine Complexity Matrix

| Actor | States | Transient States | Resources | Complexity |
|-------|--------|------------------|-----------|------------|
| EventLogActor | 2 | 0 | File I/O | Simple |
| PatternMatcherActor | 2 | 0 | Memory | Simple |
| FunctionRegistryActor | 2 | 0 | Memory | Simple |
| FunctionExecutorActor | 2 | 0 | Callback | Simple |
| HTTPServerActor | 5 | 2 | Network | Complex |
| DaemonActor | 5 | 2 | Multi-Actor | Very Complex |

---

## Common Patterns

### Pattern 1: Simple Two-State Actor (STOPPED, RUNNING)

Used by: EventLogActor, PatternMatcherActor, FunctionRegistryActor, FunctionExecutorActor

**Characteristics**:
- Minimal state complexity
- No transient states
- Idempotent start/stop operations
- Data persistence across cycles (for in-memory actors)

**Transition Template**:
```
STOPPED → start() → RUNNING
RUNNING → stop() → STOPPED
```

---

### Pattern 2: Complex Multi-State Actor with Transients

Used by: HTTPServerActor, DaemonActor

**Characteristics**:
- Five states including ERROR
- Transient states for startup/shutdown (STARTING, STOPPING)
- Complex resource management (network, child actors)
- Explicit error state with recovery

**Transition Template**:
```
STOPPED → start() → STARTING → success → RUNNING
RUNNING → stop() → STOPPING → success → STOPPED
Any → error → ERROR → stop() → STOPPED
```

---

## State Transition Table Format

All specifications use a standard table format:

| Current State | Event | Guards | Next State | Actions | Error Handling |
|---------------|-------|--------|------------|---------|----------------|

**Columns**:
- **Current State**: Starting state before transition
- **Event**: Trigger (method call, signal, internal event)
- **Guards**: Conditions that must be true for transition
- **Next State**: Resulting state after transition
- **Actions**: Operations performed during transition
- **Error Handling**: What happens if transition fails

---

## State Invariants

Each state has documented invariants that must always hold true:

### Global Invariants
- Conditions true in all states for an actor
- Example: `eventCount >= 0`, `functions is a Map`

### State-Specific Invariants
- Conditions true only in specific states
- Example: `If RUNNING, then server !== null`

---

## Error Handling Patterns

### Pattern A: Remain in Current State
Used for: Validation errors, operation errors

```javascript
// Example: Invalid function registration
State: RUNNING
Error: Invalid function
Result: Remain RUNNING, return error
```

### Pattern B: Transition to ERROR State
Used for: Resource allocation failures, critical errors

```javascript
// Example: Port bind failure
State: STARTING
Error: Port in use
Result: Transition to ERROR
```

### Pattern C: Force State Change
Used for: Shutdown failures, cleanup errors

```javascript
// Example: Stop fails but need to cleanup
State: STOPPING
Error: Timeout
Result: Force transition to ERROR, cleanup anyway
```

---

## Testing Coverage

Each specification includes a testing checklist covering:

- [ ] All state transitions work correctly
- [ ] Guards prevent invalid transitions
- [ ] Idempotent operations behave correctly
- [ ] Error handling works as specified
- [ ] Resources are properly allocated/deallocated
- [ ] State invariants hold at all times
- [ ] Status reporting is accurate

---

## Usage Guidelines

### For Implementers
1. Read the state machine specification before implementing
2. Implement all states and transitions as specified
3. Ensure all guards are checked before transitions
4. Implement error handling as documented
5. Add tests for all transitions in the checklist

### For Testers
1. Use the transition table to generate test cases
2. Verify all state invariants hold
3. Test error scenarios and recovery paths
4. Verify idempotent operations
5. Check resource cleanup on shutdown

### For Maintainers
1. Update specification when adding new states
2. Document new transitions with guards and actions
3. Add error scenarios to error handling section
4. Update testing checklist
5. Keep examples up to date with implementation

---

## Relationship to Other Specifications

### Lifecycle Specification
**File**: `/Users/bln/play/agentic-primer/.wt/event-system/ACTOR_LIFECYCLE_SPEC.md`

Defines the standard lifecycle interface (start, stop, getStatus) that all actors implement. State machine specs detail the internal state transitions that occur during these lifecycle methods.

### BDD Scenarios
**Directory**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/bdd/` (planned)

Behavior-Driven Development scenarios will reference these state machines for given/when/then clauses based on state transitions.

### FIT Decision Tables
**Directory**: `/Users/bln/play/agentic-primer/.wt/event-system/specs/fit/` (planned)

Framework for Integrated Test decision tables will use transition tables as input/output specifications for testing.

---

## References

### State Machine Theory
- **Finite State Machine (FSM)**: Mathematical model of computation with finite states
- **Deterministic FSM**: Each state + input has exactly one next state
- **Transient States**: States that actor passes through quickly (STARTING, STOPPING)
- **Error States**: Special states for error conditions requiring recovery

### Event System Documentation
- [Actor Lifecycle Specification](../../ACTOR_LIFECYCLE_SPEC.md)
- [Architecture Documentation](../../ARCHITECTURE_COMPLETE.md)
- [Project Structure](../../PROJECT_STRUCTURE_SPEC.md)

---

## Maintenance

**Last Updated**: 2026-01-11
**Version**: 1.0
**Status**: Complete - All 6 actors specified

### Change Log
- 2026-01-11: Initial creation of all 6 state machine specifications

### Future Enhancements
- [ ] Add sequence diagrams for complex transitions
- [ ] Add timing diagrams for concurrent actor interactions
- [ ] Generate test cases automatically from transition tables
- [ ] Create visualization tools for state machines
- [ ] Add formal verification proofs for invariants

---

## Summary

These state machine specifications provide a complete, formal description of all actor behavior in the Event System. They serve as:

1. **Implementation Guide**: Clear specification for developers
2. **Testing Blueprint**: Complete test coverage requirements
3. **Documentation**: Understanding actor behavior and interactions
4. **Verification Tool**: Ensuring correctness of implementations
5. **Maintenance Reference**: Onboarding and debugging support

All specifications follow consistent formatting and include complete transition coverage, making them suitable for both manual implementation and potential automated code generation.
