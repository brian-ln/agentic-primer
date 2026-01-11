# ADR-001: Functions vs Actors Architecture

**Status:** Proposed (Decision Pending)
**Date:** 2026-01-11
**Deciders:** System Architects
**Related Bead:** agentic-primer-jv9

## Context

The event-driven system currently has 6 main actors (DaemonActor, EventLogActor, HTTPServerActor, FunctionExecutorActor, LambdaInvokerActor, DeadLetterActor) that follow the Hewitt Actor Model principles. However, user-defined "functions" are implemented as stateless callbacks, not as true actors.

### Current Implementation

**Functions (NOT Actors):**
```javascript
// functions/send-email.js
export default async function(event, context) {
  const { emit, logger, config } = context;
  // Process event
  await emit({ type: 'email.sent', data: {...} });
  return { success: true };
}
```

**Properties:**
- No isolated state (stateless)
- Not independently addressable
- Executed synchronously by FunctionExecutorActor
- Simple to write and understand
- Pure function behavior

**Reality:** These are passive callbacks/handlers, not active concurrent entities.

### Hewitt Actor Model Requirements

True actors must have:
1. **Isolated State** - Private mutable state, no sharing
2. **Message-Based** - Only communicate via async messages
3. **Addressable** - Unique address/identity
4. **Concurrent** - Process messages independently
5. **Behavioral** - Can change behavior for next message
6. **Spawning** - Can create child actors

### Current System Actors

Our 6 system actors (DaemonActor, EventLogActor, HTTPServerActor, etc.) ARE closer to Hewitt actors:
- Have isolated state (this.config, this.writeStream, etc.)
- Communicate via UAP messages
- Independently addressable
- Can spawn child actors (DaemonActor spawns others)
- Have lifecycle and behavior

**Limitation:** Not truly concurrent (single process, shared event loop)

## Decision Question

**Should user-defined functions be real actors, or remain stateless callbacks?**

## Options Considered

### Option 1: Pure Actor Model (Everything is an Actor)

**Design:**
```javascript
class SendEmailActor {
  constructor(config) {
    this.state = { emailsSent: 0 };  // Private state
    this.config = config;
  }

  async handleMessage(message) {
    if (message.action === 'send') {
      this.state.emailsSent++;
      await this.emit({ type: 'email.sent', ... });
      return { success: true, count: this.state.emailsSent };
    }
  }

  async start() { /* lifecycle */ }
  async stop() { /* cleanup */ }
}
```

**Pros:**
- Consistent actor model throughout
- Functions can maintain state across invocations
- Functions are addressable (can send messages to specific instances)
- Natural supervision hierarchies
- Can spawn child actors

**Cons:**
- More complexity for simple cases
- Overhead: start/stop lifecycle for every function
- Memory: Each function type needs persistent actor instance
- Harder to write: Users must understand actor lifecycle

### Option 2: Keep Functions Stateless (Current)

**Design:**
```javascript
// Simple, stateless, easy to write
export default async function(event, context) {
  // Do work
  return result;
}
```

**Pros:**
- Simple for users to write
- Pure functions (easier to reason about)
- No lifecycle complexity
- Lower memory overhead
- Easier testing

**Cons:**
- Can't maintain state between invocations
- Can't be addressed directly
- Not consistent with actor model
- Can't spawn child actors

### Option 3: Hybrid (Both Functions AND Function Actors)

**Design:**
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

**Directory structure:**
```
functions/
├── simple-handler.js           // Stateless function
├── rate-limiter.actor.js       // Stateful actor
└── circuit-breaker.actor.js    // Stateful actor
```

**FunctionExecutorActor enhancement:**
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

**Pros:**
- Flexibility: Users choose based on needs
- Simple things stay simple (stateless functions)
- Complex things possible (stateful actors)
- Gradual migration path
- Backward compatible

**Cons:**
- Two mental models to learn
- More complex implementation
- Confusion about when to use which

## Analysis

### Are Pure Functions Just Stateless Actors?

**Theoretical Perspective: YES**
- No mutable state = actor with empty state
- Function invocation = single-message actor
- Return value = reply message
- Termination after return = actor stops
- **Erlang perspective:** Every function call IS an actor (process)

**Practical Perspective: NO**
- Not addressable (can't send messages to them)
- Not concurrent (executed synchronously)
- No supervision (FunctionExecutorActor manages them, not supervision tree)
- No behavioral changes (same function every time)

**Reality:** Functions are passive data processors, not active concurrent entities.

### Hewitt Compliance Matrix

| Requirement | Current Actors | Current Functions | Function Actors (Proposed) |
|-------------|---------------|-------------------|---------------------------|
| Isolated State | ✓ Yes | ✗ Stateless | ✓ Yes |
| Message-Based | ✓ UAP | ⚠ Via executor | ✓ Yes |
| Addressable | ✓ Yes | ✗ No | ✓ Yes |
| Concurrent | ⚠ Same process | ✗ Synchronous | ⚠ Same process |
| Spawning | ✓ Yes | ✗ No | ✓ Yes |
| Supervision | ⚠ Partial | ✗ No | ✓ Yes |

**Verdict:**
- Current 6 actors: **70% Hewitt-compliant** (missing true concurrency, robust supervision)
- Current functions: **20% Hewitt-compliant** (just passive callbacks)
- Proposed function actors: **70% Hewitt-compliant** (same as main actors)

## Decision

**RECOMMENDATION: Option 3 - Hybrid Approach**

### Rationale

**Most functions should be stateless (80% of use cases):**
- Event transformers
- Notification senders
- Simple validators
- Data formatters

**Some functions SHOULD be actors (20% of use cases):**
- Rate limiters (need state: request counts)
- Connection pools (need state: active connections)
- Aggregators (need state: accumulated data)
- Circuit breakers (need state: failure counts)
- Caching layers (need state: cached values)

### Implementation Path

1. Keep current stateless functions (backward compatible)
2. Add FunctionActor interface
3. FunctionExecutorActor detects type (.actor.js extension) and spawns accordingly
4. Document clear guidelines for when to use each

## Consequences

### Positive

- **Flexibility:** Users choose the right tool for their needs
- **Simplicity preserved:** 80% of functions remain simple
- **Power available:** 20% of cases can use stateful actors
- **Backward compatible:** Existing functions continue to work
- **Clear naming:** .actor.js extension makes it obvious
- **Supervision hierarchy:** FunctionExecutorActor supervises function actors

### Negative

- **Dual mental models:** Users must understand when to use which
- **Implementation complexity:** FunctionExecutorActor becomes more complex
- **Documentation burden:** Must explain both patterns clearly
- **Potential confusion:** Users may misuse actors for simple cases

### Neutral

- **Learning curve:** Users need to understand actor lifecycle (but only when needed)
- **Testing:** Both patterns require different testing approaches

## Implementation Requirements (If Approved)

### Phase 1: Core Infrastructure
1. Define FunctionActor interface (start/stop/handleMessage/getStatus)
2. Update FunctionExecutorActor to detect and manage actor instances
3. Add actor lifecycle management (spawning, supervision, termination)
4. Implement instance pooling/reuse for function actors

### Phase 2: Examples and Documentation
1. Create example function actors:
   - rate-limiter.actor.js
   - circuit-breaker.actor.js
   - connection-pool.actor.js
2. Document when to use functions vs function actors
3. Add to specifications (BDD scenarios for function actors)
4. Update README and architecture docs

### Phase 3: Testing and Validation
1. Add FIT/SLIM decision tables for function actors
2. Create state machine specs for actor lifecycle
3. Add integration tests
4. Performance benchmarking (overhead of actor vs function)

## Alternatives Considered

### Keep Only Stateless Functions
- **Why rejected:** Some use cases genuinely need state (rate limiting, circuit breaking)
- Would force users to use external state stores (Redis, DB) for simple counters

### Make Everything an Actor
- **Why rejected:** Adds unnecessary complexity for simple transformations
- Developer experience suffers for the 80% common case

### Use External State Management
- **Why rejected:** Adds external dependencies (Redis, DB)
- Network overhead for simple in-memory state
- Doesn't align with actor model philosophy

## Open Questions

1. **How should function actor instances be managed?**
   - One instance per function type? (singleton)
   - Pool of instances? (for concurrency)
   - Spawn on demand? (for isolation)

2. **Should function actors support clustering/distribution?**
   - Current actors run in single process
   - True Hewitt model would support distributed actors
   - Defer to future phase?

3. **How to handle function actor failures?**
   - Restart policy (supervised restart)
   - Circuit breaking for failing actors
   - Dead letter handling

4. **What's the migration path for existing stateful patterns?**
   - Some users may have implemented stateful functions via external stores
   - Provide migration guide

## References

- Hewitt Actor Model: [Wikipedia](https://en.wikipedia.org/wiki/Actor_model)
- Erlang/OTP Processes: [Erlang Documentation](https://www.erlang.org/doc/reference_manual/processes.html)
- Akka Actors: [Akka Documentation](https://doc.akka.io/docs/akka/current/typed/actors.html)
- Original Analysis: `/tmp/functions-vs-actors-analysis.md`
- Related Bead: `agentic-primer-jv9`

## Decision Status

**Currently: PROPOSED**

This ADR captures the architectural analysis. The decision will be finalized after:
1. Team review
2. Prototype validation (small-scale implementation)
3. Performance benchmarking
4. Developer experience feedback

**Next Steps:**
- Review this ADR with team
- Build proof-of-concept for hybrid approach
- Measure performance overhead
- Document clear guidelines for when to use each pattern

---

**Change Log:**
- 2026-01-11: Initial proposal (ADR created)
