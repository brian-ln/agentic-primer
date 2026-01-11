# FunctionExecutorActor State Machine Specification

## Overview

FunctionExecutorActor executes registered functions in response to events with lifecycle-managed execution state.

**Actors**: FunctionExecutorActor
**States**: STOPPED, RUNNING
**Resources**: Event emitter callback reference
**File**: `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/function-executor.js`

---

## State Definitions

### STOPPED (isRunning = false)
**Description**: Actor is not operational, function execution is not available.

**Invariants**:
- `isRunning === false`
- `emitCallback` may be null or preserved
- Cannot execute functions

**Entry Actions**:
- Set `isRunning = false`

**Exit Actions**: None

**Valid Operations**:
- `start()` - Enable function execution
- `getStatus()` - Report stopped status
- `setEmitCallback(callback)` - Configure event emitter (optional in STOPPED)

---

### RUNNING (isRunning = true)
**Description**: Actor is operational and can execute functions.

**Invariants**:
- `isRunning === true`
- `emitCallback` is available (may be null if not set)
- Can execute functions safely

**Entry Actions**:
1. Set `isRunning = true`
2. Return success

**Exit Actions**:
1. Set `isRunning = false`
2. Preserve `emitCallback` reference (data persists)

**Valid Operations**:
- `execute(functionName, args, context)` - Execute a function
- `setEmitCallback(callback)` - Set event emitter callback
- `stop()` - Disable execution
- `getStatus()` - Report running status

---

## State Transition Table

| Current State | Event | Guards | Next State | Actions | Error Handling |
|---------------|-------|--------|------------|---------|----------------|
| STOPPED | `start()` called | None | RUNNING | 1. Set `isRunning = true`<br>2. Return `{ success: true, message: "FunctionExecutorActor started" }` | On error:<br>- Remain in STOPPED<br>- Return `{ success: false, error: message }` |
| STOPPED | `stop()` called | None | STOPPED | 1. Return `{ success: true, message: "Was not running" }` | No error possible |
| STOPPED | `getStatus()` called | None | STOPPED | 1. Return `{ isRunning: false, hasEmitCallback: emitCallback !== null }` | No error possible |
| STOPPED | `execute()` called | None | STOPPED | 1. Return error or throw exception | Actor not running |
| STOPPED | `setEmitCallback()` called | Valid callback | STOPPED | 1. Store callback reference<br>2. Return success (optional) | No error possible |
| RUNNING | `start()` called | Already running | RUNNING | 1. Return `{ success: false, error: "Already running" }` | No state change |
| RUNNING | `stop()` called | None | STOPPED | 1. Set `isRunning = false`<br>2. Preserve `emitCallback`<br>3. Return `{ success: true, message: "FunctionExecutorActor stopped" }` | On error:<br>- Force `isRunning = false`<br>- Return `{ success: false, error: message }` |
| RUNNING | `getStatus()` called | None | RUNNING | 1. Return `{ isRunning: true, hasEmitCallback: emitCallback !== null }` | No error possible |
| RUNNING | `setEmitCallback(callback)` called | Valid callback | RUNNING | 1. Store callback reference<br>2. Return success (optional) | No error possible |
| RUNNING | `setEmitCallback(callback)` called | Invalid callback | RUNNING | 1. Return error or ignore | No state change |
| RUNNING | `execute(name, args, ctx)` called | Valid parameters | RUNNING | 1. Validate function exists in registry<br>2. Prepare execution context<br>3. Call function with args<br>4. Handle return value/promise<br>5. Return result | On error:<br>- Catch exception<br>- Return error result<br>- Remain RUNNING |
| RUNNING | `execute(name, args, ctx)` called | Function not found | RUNNING | 1. Return `{ success: false, error: "Function not found" }` | No state change |
| RUNNING | `execute(name, args, ctx)` called | Function throws error | RUNNING | 1. Catch exception<br>2. Log error<br>3. Return `{ success: false, error: message }` | Remain RUNNING |
| RUNNING | `execute(name, args, ctx)` called | Function returns promise | RUNNING | 1. Await promise<br>2. Handle resolution or rejection<br>3. Return result | On error:<br>- Return error result<br>- Remain RUNNING |
| RUNNING | Function emits event | `emitCallback` available | RUNNING | 1. Validate event object<br>2. Call `emitCallback(event)`<br>3. Continue execution | On error:<br>- Log emit error<br>- Continue execution |
| RUNNING | Function emits event | `emitCallback` is null | RUNNING | 1. Log warning (no emitter)<br>2. Continue execution | No error possible |

---

## State Diagram

```
                 ┌──────────────────────────┐
                 │                          │
                 │  start() [already run]   │
                 │  execute() [disabled]    │
                 ▼                          │
            ┌─────────┐                     │
            │ STOPPED │◄────────────────────┼──────┐
            └────┬────┘                     │      │
                 │                          │      │
                 │ start()                  │      │
                 │ [success]                │      │
                 ▼                          │      │
         ┌──────────────┐                   │      │
         │  Initialize  │                   │      │
         │  executor    │                   │      │
         └───────┬──────┘                   │      │
                 │                          │      │
                 ▼                          │      │
            ┌─────────┐                     │      │
            │ RUNNING │─────────────────────┘      │
            └────┬────┘                            │
                 │  Function execution active      │
                 │                                 │
                 │ stop()                          │
                 │ [success]                       │
                 │                                 │
                 └─────────────────────────────────┘
```

---

## Transition Guards

### start() Guards
- **Not Already Running**: `isRunning === false`

### stop() Guards
- **No Guards**: Always safe to call (idempotent)

### execute() Guards
- **Is Running**: `isRunning === true`
- **Valid Function Name**: `name` is a non-empty string
- **Valid Arguments**: `args` is an array or object
- **Valid Context**: `context` has required fields (event, etc.)

### setEmitCallback() Guards
- **Valid Callback**: `callback` is a function or null
- **No State Requirement**: Can be called in STOPPED or RUNNING

---

## Error States and Recovery

### Error Scenarios

1. **Start Fails (Very Rare)**
   - **State**: Remain STOPPED
   - **Recovery**: Retry start()

2. **Function Not Found**
   - **State**: Remain RUNNING
   - **Recovery**: Return error to caller, register function

3. **Function Execution Error**
   - **State**: Remain RUNNING
   - **Recovery**: Catch exception, return error, log for debugging

4. **Function Timeout (if implemented)**
   - **State**: Remain RUNNING
   - **Recovery**: Kill execution, return timeout error

5. **Event Emit Error**
   - **State**: Remain RUNNING
   - **Recovery**: Log error, continue function execution

6. **No Emit Callback Set**
   - **State**: Remain RUNNING
   - **Recovery**: Log warning, function can't emit events

7. **Operations Called When Stopped**
   - **State**: Remain STOPPED
   - **Recovery**: Start actor before execution

---

## State Invariants

### Global Invariants (all states)
- `emitCallback` is null or a function
- Execution context is isolated per function call
- Function errors don't crash the actor

### STOPPED Invariants
- `isRunning === false`
- `emitCallback` may be preserved (data persists)
- Cannot execute functions

### RUNNING Invariants
- `isRunning === true`
- Can execute functions safely
- Function execution is isolated
- Errors are caught and returned

---

## Function Execution Context

### Context Object
```javascript
{
  event: {               // Original triggering event
    id: string,
    type: string,
    data: object,
    timestamp: string
  },
  emit: Function,        // Event emitter function
  logger: object,        // Logger instance (optional)
  config: object         // Configuration (optional)
}
```

### Execution Flow
1. **Validate**: Check actor is running, function exists
2. **Prepare Context**: Build context object with event and emit callback
3. **Execute**: Call function with args and context
4. **Handle Result**:
   - Sync return: Return immediately
   - Async return: Await promise, return result
   - Error: Catch exception, return error object
5. **Emit Events**: If function calls `context.emit()`, emit events

---

## Lifecycle Examples

### Example 1: Normal Startup and Shutdown
```javascript
const executor = new FunctionExecutorActor();

// Initial state: STOPPED
console.log(executor.getStatus());
// { isRunning: false, hasEmitCallback: false }

// Transition: STOPPED -> RUNNING
await executor.start();
// { success: true, message: "FunctionExecutorActor started" }

console.log(executor.getStatus());
// { isRunning: true, hasEmitCallback: false }

// Set emit callback
executor.setEmitCallback((event) => {
  console.log('Event emitted:', event);
});

console.log(executor.getStatus());
// { isRunning: true, hasEmitCallback: true }

// Transition: RUNNING -> STOPPED
await executor.stop();
// { success: true, message: "FunctionExecutorActor stopped" }
```

### Example 2: Function Execution
```javascript
await executor.start();

// Set emit callback
const events = [];
executor.setEmitCallback((event) => events.push(event));

// Execute synchronous function
const result1 = await executor.execute('add', [2, 3], {
  event: { type: 'test', data: {} }
});
// { success: true, result: 5 }

// Execute async function
const result2 = await executor.execute('fetchData', ['user123'], {
  event: { type: 'user.fetch', data: { id: 'user123' } }
});
// { success: true, result: { name: 'Alice', ... } }

// Execute function that emits events
await executor.execute('processOrder', [order], {
  event: { type: 'order.created', data: order }
});
// Function emits 'order.validated' and 'inventory.reserved' events
console.log(events.length); // 2
```

### Example 3: Error Handling
```javascript
await executor.start();

// Function not found
const result1 = await executor.execute('unknownFunction', [], {});
// { success: false, error: "Function not found: unknownFunction" }

// Function throws error
const result2 = await executor.execute('divideByZero', [10, 0], {});
// { success: false, error: "Division by zero" }

// Actor still running after errors
console.log(executor.getStatus());
// { isRunning: true, hasEmitCallback: true }
```

### Example 4: Idempotent Operations
```javascript
await executor.start(); // { success: true }
await executor.start(); // { success: false, error: "Already running" }

await executor.stop(); // { success: true }
await executor.stop(); // { success: true, message: "Was not running" }
```

### Example 5: Operations When Stopped
```javascript
// Actor is STOPPED
await executor.execute('test', [], {}); // Error: Actor not running

await executor.start();
await executor.execute('test', [], {}); // OK (if function exists)
```

### Example 6: Emit Callback Persistence
```javascript
// Set callback in STOPPED state
executor.setEmitCallback((event) => console.log(event));

await executor.start();
// hasEmitCallback: true (callback preserved)

await executor.stop();
// hasEmitCallback: true (callback still there)

await executor.start();
// Can execute functions with emit support immediately
```

---

## Testing Checklist

- [ ] Start from STOPPED transitions to RUNNING
- [ ] Start from RUNNING returns error
- [ ] Stop from RUNNING transitions to STOPPED
- [ ] Stop from STOPPED is idempotent
- [ ] getStatus returns correct state
- [ ] setEmitCallback stores callback correctly
- [ ] setEmitCallback works in both STOPPED and RUNNING
- [ ] execute works in RUNNING state
- [ ] execute fails in STOPPED state
- [ ] execute returns correct result for sync functions
- [ ] execute awaits and returns result for async functions
- [ ] execute catches and returns errors
- [ ] execute handles function not found
- [ ] Function can emit events via context.emit
- [ ] Emit callback is called when function emits
- [ ] Execution continues if emit callback is null
- [ ] Multiple executions can run (if async)
- [ ] Emit callback persists across stop/start

---

## Summary

FunctionExecutorActor implements a simple two-state machine (STOPPED, RUNNING) with primary responsibility for executing user-defined functions with proper error isolation. The state machine ensures:

1. **Execution Safety**: Functions run in isolated context
2. **Error Isolation**: Function errors don't crash actor
3. **Operational Safety**: Execution only allowed in RUNNING state
4. **Event Integration**: Functions can emit events via callback
5. **Callback Persistence**: Emit callback persists across cycles
6. **Async Support**: Properly handles both sync and async functions
7. **Operational Visibility**: Status reports emit callback availability

The specification provides complete transition coverage for all events and execution scenarios.
