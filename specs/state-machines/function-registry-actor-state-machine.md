# FunctionRegistryActor State Machine Specification

## Overview

FunctionRegistryActor manages function registration and lookup with in-memory function storage.

**Actors**: FunctionRegistryActor
**States**: STOPPED, RUNNING
**Resources**: In-memory Map of functions
**File**: `/Users/bln/play/agentic-primer/.wt/event-system/src/actors/function-registry.js`

---

## State Definitions

### STOPPED (isRunning = false)
**Description**: Actor is not operational, function operations are not available.

**Invariants**:
- `isRunning === false`
- `functions` Map exists but operations are disabled
- Function count is preserved (data persists)

**Entry Actions**:
- Set `isRunning = false`

**Exit Actions**: None

**Valid Operations**:
- `start()` - Enable function operations
- `getStatus()` - Report stopped status

---

### RUNNING (isRunning = true)
**Description**: Actor is operational and can register/lookup functions.

**Invariants**:
- `isRunning === true`
- `functions` is a Map instance
- `functions.size >= 0`
- All stored functions are valid function objects

**Entry Actions**:
1. Set `isRunning = true`
2. Initialize functions Map (if not exists)
3. Return success

**Exit Actions**:
1. Set `isRunning = false`
2. Functions remain in memory (data preserved)

**Valid Operations**:
- `registerFunction(name, fn)` - Register a function
- `getFunction(name)` - Retrieve function by name
- `hasFunction(name)` - Check if function exists
- `listFunctions()` - Get all function names
- `unregisterFunction(name)` - Remove function
- `stop()` - Disable operations
- `getStatus()` - Report running status

---

## State Transition Table

| Current State | Event | Guards | Next State | Actions | Error Handling |
|---------------|-------|--------|------------|---------|----------------|
| STOPPED | `start()` called | None | RUNNING | 1. Initialize functions Map (if needed)<br>2. Set `isRunning = true`<br>3. Return `{ success: true, message: "FunctionRegistryActor started" }` | On error:<br>- Remain in STOPPED<br>- Return `{ success: false, error: message }` |
| STOPPED | `start()` called | Initialization fails | STOPPED | 1. Return `{ success: false, error: "Init failed" }` | Remain in STOPPED |
| STOPPED | `stop()` called | None | STOPPED | 1. Return `{ success: true, message: "Was not running" }` | No error possible |
| STOPPED | `getStatus()` called | None | STOPPED | 1. Return `{ isRunning: false, functionCount: functions.size, functions: [] }` | No error possible |
| STOPPED | `registerFunction()` called | None | STOPPED | 1. Return error or throw exception | Actor not running |
| STOPPED | `getFunction()` called | None | STOPPED | 1. Return null or throw exception | Actor not running |
| STOPPED | `hasFunction()` called | None | STOPPED | 1. Return false or throw exception | Actor not running |
| STOPPED | `listFunctions()` called | None | STOPPED | 1. Return empty array or error | Actor not running |
| RUNNING | `start()` called | Already running | RUNNING | 1. Return `{ success: false, error: "Already running" }` | No state change |
| RUNNING | `stop()` called | None | STOPPED | 1. Set `isRunning = false`<br>2. Preserve functions Map in memory<br>3. Return `{ success: true, message: "FunctionRegistryActor stopped" }` | On error:<br>- Force `isRunning = false`<br>- Return `{ success: false, error: message }` |
| RUNNING | `getStatus()` called | None | RUNNING | 1. Get function names array<br>2. Return `{ isRunning: true, functionCount: functions.size, functions: [...] }` | No error possible |
| RUNNING | `registerFunction(name, fn)` called | Valid name and function | RUNNING | 1. Validate name is string<br>2. Validate fn is function<br>3. Store in `functions` Map<br>4. Return `{ success: true }` | On error:<br>- Remain RUNNING<br>- Return error object |
| RUNNING | `registerFunction(name, fn)` called | Invalid name | RUNNING | 1. Validate fails (name not string)<br>2. Return `{ success: false, error: "Invalid name" }` | No state change |
| RUNNING | `registerFunction(name, fn)` called | Invalid function | RUNNING | 1. Validate fails (fn not function)<br>2. Return `{ success: false, error: "Invalid function" }` | No state change |
| RUNNING | `registerFunction(name, fn)` called | Name already exists | RUNNING | 1. Overwrite existing function (OR return error)<br>2. Return `{ success: true }` or error | Configurable behavior |
| RUNNING | `getFunction(name)` called | Function exists | RUNNING | 1. Look up function by name<br>2. Return function object | No error possible |
| RUNNING | `getFunction(name)` called | Function not found | RUNNING | 1. Return null or undefined | No error possible |
| RUNNING | `hasFunction(name)` called | None | RUNNING | 1. Check if name exists in Map<br>2. Return boolean | No error possible |
| RUNNING | `listFunctions()` called | None | RUNNING | 1. Get keys from functions Map<br>2. Convert to array<br>3. Return array of names | On error:<br>- Return empty array |
| RUNNING | `unregisterFunction(name)` called | Function exists | RUNNING | 1. Delete from functions Map<br>2. Return `{ success: true }` | No error possible |
| RUNNING | `unregisterFunction(name)` called | Function not found | RUNNING | 1. Return `{ success: false }` or succeed silently | No error possible |

---

## State Diagram

```
                 ┌───────────────────────────────┐
                 │                               │
                 │  start() [already running]    │
                 │  registerFunction() [disabled]│
                 │  getFunction() [disabled]     │
                 ▼                               │
            ┌─────────┐                          │
            │ STOPPED │◄─────────────────────────┼───────┐
            └────┬────┘                          │       │
                 │                               │       │
                 │ start()                       │       │
                 │ [success]                     │       │
                 ▼                               │       │
         ┌──────────────┐                        │       │
         │  Initialize  │                        │       │
         │  functions   │                        │       │
         └───────┬──────┘                        │       │
                 │                               │       │
                 ▼                               │       │
            ┌─────────┐                          │       │
            │ RUNNING │──────────────────────────┘       │
            └────┬────┘                                  │
                 │  Function operations active           │
                 │                                       │
                 │ stop()                                │
                 │ [success]                             │
                 │                                       │
                 └───────────────────────────────────────┘
```

---

## Transition Guards

### start() Guards
- **Not Already Running**: `isRunning === false`
- **Map Initializable**: Can create new Map() (always true in JS)

### stop() Guards
- **No Guards**: Always safe to call (idempotent)

### registerFunction() Guards
- **Is Running**: `isRunning === true`
- **Valid Name**: `name` is a non-empty string
- **Valid Function**: `fn` is a function (typeof fn === 'function')
- **Not Duplicate** (optional): `!functions.has(name)` (configurable)

### getFunction() Guards
- **Is Running**: `isRunning === true`

### hasFunction() Guards
- **Is Running**: `isRunning === true`

### listFunctions() Guards
- **Is Running**: `isRunning === true`

### unregisterFunction() Guards
- **Is Running**: `isRunning === true`

---

## Error States and Recovery

### Error Scenarios

1. **Start Fails (Very Rare)**
   - **State**: Remain STOPPED
   - **Recovery**: Retry start(), check memory

2. **Invalid Function Registration**
   - **State**: Remain RUNNING
   - **Recovery**: Fix function object, retry registration

3. **Duplicate Name Registration**
   - **State**: Remain RUNNING
   - **Recovery**: Choose to overwrite or return error (configurable)

4. **Function Not Found**
   - **State**: Remain RUNNING
   - **Recovery**: Return null, caller handles missing function

5. **Memory Exhaustion (Too Many Functions)**
   - **State**: Remain RUNNING but degraded
   - **Recovery**: Remove old functions, implement cleanup

6. **Operations Called When Stopped**
   - **State**: Remain STOPPED
   - **Recovery**: Start actor before operations

---

## State Invariants

### Global Invariants (all states)
- `functions` is always a Map instance
- `functions.size >= 0`
- All function names are non-empty strings
- All function values are actual functions
- Function data persists across stop/start cycles

### STOPPED Invariants
- `isRunning === false`
- `functions` Map exists (data preserved)
- Cannot register or retrieve functions

### RUNNING Invariants
- `isRunning === true`
- `functions` Map is accessible
- All function operations are available
- Functions can be registered, retrieved, checked, listed, removed

---

## Function Data Structure

### Function Registry Entry
```javascript
// Map structure
functions.set(name, functionReference)

// Example entries
functions.set('sendEmail', async (data) => { /* ... */ })
functions.set('validateUser', (user) => { /* ... */ })
functions.set('processPayment', async (payment) => { /* ... */ })
```

### Function Metadata (Optional Extension)
```javascript
{
  name: string,           // Function name
  fn: Function,           // Function reference
  metadata: {
    registered: timestamp,
    description: string,
    parameters: [...],
    returnType: string
  }
}
```

---

## Lifecycle Examples

### Example 1: Normal Startup and Shutdown
```javascript
const registry = new FunctionRegistryActor();

// Initial state: STOPPED
console.log(registry.getStatus());
// { isRunning: false, functionCount: 0, functions: [] }

// Transition: STOPPED -> RUNNING
await registry.start();
// { success: true, message: "FunctionRegistryActor started" }

console.log(registry.getStatus());
// { isRunning: true, functionCount: 0, functions: [] }

// Register functions in RUNNING state
registry.registerFunction('add', (a, b) => a + b);
registry.registerFunction('multiply', (a, b) => a * b);

console.log(registry.getStatus());
// { isRunning: true, functionCount: 2, functions: ['add', 'multiply'] }

// Transition: RUNNING -> STOPPED
await registry.stop();
// { success: true, message: "FunctionRegistryActor stopped" }

console.log(registry.getStatus());
// { isRunning: false, functionCount: 2, functions: ['add', 'multiply'] } (functions preserved)
```

### Example 2: Function Registration and Retrieval
```javascript
await registry.start();

// Register function
registry.registerFunction('greet', (name) => `Hello, ${name}!`);

// Retrieve and call function
const greet = registry.getFunction('greet');
console.log(greet('Alice')); // "Hello, Alice!"

// Check if function exists
console.log(registry.hasFunction('greet')); // true
console.log(registry.hasFunction('unknown')); // false

// List all functions
console.log(registry.listFunctions()); // ['greet']
```

### Example 3: Idempotent Operations
```javascript
await registry.start(); // { success: true }
await registry.start(); // { success: false, error: "Already running" }

await registry.stop(); // { success: true }
await registry.stop(); // { success: true, message: "Was not running" }
```

### Example 4: Operations When Stopped
```javascript
// Actor is STOPPED
registry.registerFunction('test', () => {}); // Error: Actor not running
registry.getFunction('test'); // Error or null

await registry.start();
registry.registerFunction('test', () => {}); // OK
await registry.stop();

// Function data preserved
console.log(registry.getStatus());
// { isRunning: false, functionCount: 1, functions: ['test'] }
```

### Example 5: Function Persistence
```javascript
await registry.start();
registry.registerFunction('persist', () => 'data');
// functionCount: 1

await registry.stop();
// functionCount: 1 (data preserved)

await registry.start();
// functionCount: 1 (data still there)

const fn = registry.getFunction('persist');
console.log(fn()); // "data" (function still works)
```

### Example 6: Error Handling
```javascript
await registry.start();

// Invalid name
registry.registerFunction('', () => {}); // Error: Invalid name
registry.registerFunction(null, () => {}); // Error: Invalid name

// Invalid function
registry.registerFunction('test', 'not a function'); // Error: Invalid function
registry.registerFunction('test', null); // Error: Invalid function

// Valid registration
registry.registerFunction('valid', () => {}); // { success: true }
```

---

## Testing Checklist

- [ ] Start from STOPPED transitions to RUNNING
- [ ] Start from RUNNING returns error
- [ ] Stop from RUNNING transitions to STOPPED
- [ ] Stop from STOPPED is idempotent
- [ ] getStatus returns correct state and count
- [ ] registerFunction works in RUNNING state
- [ ] registerFunction fails with invalid name
- [ ] registerFunction fails with invalid function
- [ ] registerFunction fails in STOPPED state
- [ ] getFunction retrieves correct function
- [ ] getFunction returns null for unknown name
- [ ] hasFunction returns correct boolean
- [ ] listFunctions returns all function names
- [ ] unregisterFunction removes function
- [ ] Function data persists across stop/start
- [ ] Duplicate registration is handled correctly
- [ ] Function count is accurate

---

## Summary

FunctionRegistryActor implements a simple two-state machine (STOPPED, RUNNING) with primary responsibility for managing in-memory function storage and lookup. The state machine ensures:

1. **Data Persistence**: Functions remain in memory across stop/start cycles
2. **Operational Safety**: Operations only allowed in RUNNING state
3. **Idempotency**: Start/stop operations are safe to retry
4. **Type Safety**: Functions are validated before registration
5. **Operational Visibility**: Status reports function count and names
6. **Stateless Resources**: No file I/O or network resources to manage

The specification provides complete transition coverage for all events and function operations.
