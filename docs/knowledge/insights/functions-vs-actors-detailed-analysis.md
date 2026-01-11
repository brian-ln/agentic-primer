# Functions vs Actors - Architectural Analysis

## Current System Architecture

### What We Have: "Functions" (NOT Actors)

**Current implementation**:
```javascript
// functions/send-email.js
export default async function(event, context) {
  const { emit, logger, config } = context;
  // Process event
  await emit({ type: 'email.sent', data: {...} });
  return { success: true };
}
```

**Properties**:
- ❌ **Not isolated** - No private state
- ❌ **Not addressable** - Can't send messages to it
- ❌ **Not concurrent** - Executed synchronously by FunctionExecutorActor
- ✅ **Stateless** - Pure function behavior
- ✅ **Simple** - Easy to write and understand

**Reality**: These are just **callbacks/handlers**, not actors.

---

## Hewitt Actor Model - What's Required

### True Actor Properties

1. **Isolated State** - Private mutable state, no sharing
2. **Message-Based** - Only communicate via async messages
3. **Addressable** - Has unique address/identity
4. **Concurrent** - Processes messages independently
5. **Behavioral** - Can change behavior for next message
6. **Spawning** - Can create child actors

### Our Current Actors (6 of them)

✅ **DaemonActor, EventLogActor, HTTPServerActor, etc.**

These ARE closer to Hewitt actors because they:
- Have isolated state (this.config, this.writeStream, etc.)
- Communicate via UAP messages
- Are independently addressable
- Can spawn child actors (DaemonActor spawns others)
- Have lifecycle and behavior

**But**: They're not truly concurrent (single process, shared event loop)

---

## The Question: Should Functions Be Actors?

### Option 1: Functions as Actors (Full Actor Model)

**Design**:
```javascript
// Each function type becomes an actor
class SendEmailActor {
  constructor(config) {
    this.state = { emailsSent: 0 };  // Private state
    this.config = config;
  }
  
  async handleMessage(message) {
    if (message.action === 'send') {
      // Process
      this.state.emailsSent++;
      await this.emit({ type: 'email.sent', ... });
      return { success: true, count: this.state.emailsSent };
    }
  }
  
  async start() { /* lifecycle */ }
  async stop() { /* cleanup */ }
}
```

**Pros**:
- ✅ Consistent actor model throughout
- ✅ Functions can maintain state across invocations
- ✅ Functions are addressable (can send messages to specific instances)
- ✅ Natural supervision hierarchies
- ✅ Can spawn child actors

**Cons**:
- ❌ More complexity for simple cases
- ❌ Overhead: start/stop lifecycle for every function
- ❌ Memory: Each function type needs persistent actor instance
- ❌ Harder to write: Users must understand actor lifecycle

---

### Option 2: Keep Functions Stateless (Current)

**Keep**:
```javascript
// Simple, stateless, easy to write
export default async function(event, context) {
  // Do work
  return result;
}
```

**Pros**:
- ✅ Simple for users to write
- ✅ Pure functions (easier to reason about)
- ✅ No lifecycle complexity
- ✅ Lower memory overhead
- ✅ Easier testing

**Cons**:
- ❌ Can't maintain state between invocations
- ❌ Can't be addressed directly
- ❌ Not consistent with actor model
- ❌ Can't spawn child actors

---

### Option 3: Hybrid (Both Functions AND Function Actors)

**Support both patterns**:

```javascript
// Stateless function (simple case)
export default async function simpleHandler(event, context) {
  return { success: true };
}

// Stateful actor (complex case)
export class EmailActor {
  async start() { /* init state */ }
  async handleMessage(message) { /* process */ }
  async stop() { /* cleanup */ }
}
```

**Pros**:
- ✅ Flexibility: Users choose based on needs
- ✅ Simple things stay simple (stateless functions)
- ✅ Complex things possible (stateful actors)
- ✅ Gradual migration path

**Cons**:
- ❌ Two mental models to learn
- ❌ More complex implementation
- ❌ Confusion about when to use which

---

## Analysis: Are Pure Functions Just Stateless Actors?

### Theoretical Perspective: YES

**Pure functions ARE actors** if you consider:
- No mutable state = actor with empty state
- Function invocation = single-message actor
- Return value = reply message
- Termination after return = actor stops

**Erlang perspective**: Every function call IS an actor (process)

### Practical Perspective: NO

**Functions lack key actor properties**:
- Not addressable (can't send messages to them)
- Not concurrent (executed synchronously)
- No supervision (FunctionExecutorActor manages them, not supervision tree)
- No behavioral changes (same function every time)

**Reality**: Functions are **passive data processors**, not active entities.

---

## Recommendation

### Current System: **Hybrid is Best**

**Rationale**:
1. **Most functions should be stateless** (80% of use cases)
   - Event transformers
   - Notification senders
   - Simple validators

2. **Some functions SHOULD be actors** (20% of use cases)
   - Rate limiters (need state: request counts)
   - Connection pools (need state: active connections)
   - Aggregators (need state: accumulated data)
   - Circuit breakers (need state: failure counts)

3. **Implementation path**:
   - Keep current stateless functions
   - Add FunctionActor interface
   - FunctionExecutorActor detects type and spawns accordingly

---

## Proposed Enhancement

### Add "Function Actors" alongside "Functions"

```javascript
// Directory structure
functions/
├── simple-handler.js           // Stateless function
├── rate-limiter.actor.js       // Stateful actor
└── circuit-breaker.actor.js    // Stateful actor
```

**FunctionExecutorActor enhancement**:
```javascript
async execute({ functionPath, event }) {
  if (functionPath.endsWith('.actor.js')) {
    // Spawn or reuse actor instance
    const actor = await this.getOrCreateActor(functionPath);
    return await actor.handleMessage(event);
  } else {
    // Execute stateless function (current behavior)
    const fn = await import(functionPath);
    return await fn.default(event, context);
  }
}
```

**Benefits**:
- Backward compatible (existing functions work)
- Opt-in complexity (only use actors when needed)
- Clear naming (.actor.js extension)
- Supervision hierarchy (FunctionExecutorActor supervises function actors)

---

## Hewitt's Actor Model: How Close Are We?

### Current System vs Hewitt

| Requirement | Current Actors | Current Functions | Function Actors (Proposed) |
|-------------|---------------|-------------------|---------------------------|
| Isolated State | ✅ Yes | ❌ Stateless | ✅ Yes |
| Message-Based | ✅ UAP | ⚠️ Via executor | ✅ Yes |
| Addressable | ✅ Yes | ❌ No | ✅ Yes |
| Concurrent | ⚠️ Same process | ❌ Synchronous | ⚠️ Same process |
| Spawning | ✅ Yes | ❌ No | ✅ Yes |
| Supervision | ⚠️ Partial | ❌ No | ✅ Yes |

**Verdict**: 
- Current 6 actors: **70% Hewitt-compliant** (missing true concurrency, robust supervision)
- Current functions: **20% Hewitt-compliant** (just passive callbacks)
- Proposed function actors: **70% Hewitt-compliant** (same as main actors)

---

## Recommendation Summary

### Keep BOTH with clear distinction

**Stateless Functions** (current):
- Use for: Simple event handlers, transformers, one-shot operations
- File pattern: `*.js`
- Example: send-email.js, validate-user.js

**Function Actors** (add):
- Use for: Stateful operations, resource management, supervision needs
- File pattern: `*.actor.js`
- Example: rate-limiter.actor.js, connection-pool.actor.js

**Main System Actors** (current 6):
- Core infrastructure
- Always running
- System-wide responsibilities

---

## Answer to Your Questions

### 1. "Are functions real actors?"
**No** - Current functions are passive callbacks, not Hewitt actors.

### 2. "Should we have both?"
**Yes** - Stateless functions for simplicity + Function actors for stateful needs.

### 3. "Are pure functions just stateless actors?"
**Philosophically yes, practically no**:
- Pure functions ≈ single-message actors that terminate
- But they lack: addressability, concurrency, supervision
- Better viewed as: **data processors** rather than **concurrent entities**

---

## Next Steps (If Implementing Function Actors)

1. Define FunctionActor interface (start/stop/handleMessage/getStatus)
2. Update FunctionExecutorActor to detect and manage actor instances
3. Add actor lifecycle management (spawning, supervision, termination)
4. Create examples: rate-limiter.actor.js, circuit-breaker.actor.js
5. Document when to use functions vs function actors
6. Add to specifications (BDD scenarios for function actors)

---

## Current State

**What we have**: 
- 6 system actors (close to Hewitt model)
- Stateless functions (not actors, just callbacks)

**What we could add**:
- Function actors (user-defined actors for stateful operations)

**Recommendation**: 
- Keep current design for simplicity
- Add function actors as **opt-in enhancement** when needed
- Document the distinction clearly

