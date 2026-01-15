# Error Model Exploration for tk-agents

## Executive Summary

This document explores error handling patterns for actor-based systems and proposes a practical, lightweight approach for tk-agents. The goal is to move beyond simple `throw new Error()` to a model that distinguishes recoverable from fatal errors while maintaining simplicity and debuggability during the exploration phase.

**Recommended Approach**: Start with Result types and error classification, add supervision for critical actors, defer circuit breakers and retry loops until needed.

---

## 1. Error Taxonomy

### 1.1 Four Categories of Errors

| Category | Description | Examples | Recovery Strategy |
|----------|-------------|----------|-------------------|
| **Transient** | Temporary failures that may resolve on their own | Network timeouts, rate limits, momentary DB unavailability | Retry with exponential backoff |
| **Recoverable** | Errors that can be handled with alternative logic | Validation errors, missing optional data, non-critical tool failures | Continue with fallback or degraded mode |
| **Fatal** | Unrecoverable errors requiring actor restart | Invalid state, corrupted data, programming errors | Restart actor (supervision) or escalate |
| **Permanent** | Structural errors that won't resolve with retries | Authentication failures, invalid configuration, missing required resources | Fail fast, escalate to supervisor |

### 1.2 Error Decision Tree

```
Error Occurs
    │
    ├─ Is it expected? (validation, business logic)
    │  └─ YES → Return error in Result type (recoverable)
    │
    ├─ Is it temporary? (network, rate limit)
    │  └─ YES → Retry with backoff (transient)
    │
    ├─ Can we continue? (optional feature, degraded mode)
    │  └─ YES → Log warning, continue (recoverable)
    │
    └─ Cannot recover
       ├─ Can restart help? (state corruption)
       │  └─ YES → Restart actor (fatal)
       └─ NO → Escalate to supervisor (permanent)
```

---

## 2. Patterns from Actor Systems

### 2.1 Erlang: "Let It Crash" Philosophy

**Core Principle**: Don't write defensive code for errors you don't know how to handle. Let processes crash and supervisors restart them.

**Key Mechanisms**:
- **Process Isolation**: One actor crash doesn't affect others (shared-nothing)
- **Supervision Trees**: Parent actors monitor children and decide restart strategies
- **Automatic Recovery**: Supervisors restart failed actors automatically

**Erlang Supervision Strategies**:
```erlang
% Four restart strategies:
one_for_one   % Restart only the failed child
one_for_all   % Restart all children if one fails
rest_for_one  % Restart failed child and all started after it
simple_one_for_one % Dynamic children with same restart strategy
```

**Tradeoffs**:
- ✅ Simple actor code (no defensive error handling)
- ✅ Clear separation of concerns (supervision vs business logic)
- ✅ Automatic fault isolation
- ❌ Requires BEAM VM semantics (lightweight processes)
- ❌ Lost in-memory state on restart (unless persisted)
- ❌ May hide underlying issues if restart always "works"

**Sources**:
- [The "let it crash" error handling strategy of Erlang](https://dev.to/adolfont/the-let-it-crash-error-handling-strategy-of-erlang-by-joe-armstrong-25hf)
- [Erlang "Let it Crash" Approach to Building Reliable Services](https://medium.com/@vamsimokari/erlang-let-it-crash-philosophy-53486d2a6da)

### 2.2 Akka: Typed Supervision Strategies

**Core Principle**: Distinguish between validation errors (part of protocol) and unexpected failures (handled by supervisor).

**Four Supervision Directives**:
1. **Resume**: Keep actor's internal state, continue processing
2. **Restart**: Clear internal state, create fresh instance
3. **Stop**: Terminate actor permanently
4. **Escalate**: Pass error to parent supervisor

**Error Classification**:
```typescript
// Part of actor protocol (don't crash on these)
type ValidationError = {
  type: "validation_error";
  field: string;
  message: string;
};

// Unexpected failures (supervision territory)
type UnexpectedFailure = {
  type: "failure";
  cause: "network" | "database" | "timeout" | "unknown";
  message: string;
};
```

**Akka Strategy Example**:
```scala
// Resume for certain exceptions, restart for others
override val supervisorStrategy = OneForOneStrategy() {
  case _: ArithmeticException => Resume
  case _: NullPointerException => Restart
  case _: IllegalArgumentException => Stop
  case _: Exception => Escalate
}
```

**Fatal Error Handling**:
- `ActorInitializationException`: Always restart
- `ActorKilledException`: Stop actor
- `NoClassDefFoundError`: Escalate (not recoverable)
- Generic `Exception`: Apply strategy

**Tradeoffs**:
- ✅ Clear separation: validation vs failure
- ✅ Flexible recovery strategies per error type
- ✅ Prevents cascading failures
- ❌ More complex than Erlang's "just crash"
- ❌ Requires careful error classification
- ❌ Can lead to over-engineering for simple cases

**Sources**:
- [Fault tolerance | Akka.NET Documentation](https://getakka.net/articles/actors/fault-tolerance.html)
- [Supervision and Monitoring | Baeldung on Scala](https://www.baeldung.com/scala/akka-supervision)
- [Fault Tolerance • Akka core](https://doc.akka.io/docs/akka/current/typed/fault-tolerance.html)

### 2.3 Orleans: Virtual Actors with Automatic Recovery

**Core Principle**: An actor never fails - if a server crashes, the next message automatically re-instantiates the actor on another server.

**Key Features**:
- **Virtual Existence**: Actors always exist virtually, transcending in-memory instances
- **Automatic Instantiation**: No explicit creation/destruction
- **State Persistence**: Actor state automatically persisted and restored
- **Cluster Fault Tolerance**: Silos coordinate to detect failures and recover

**Error Handling Approach**:
```csharp
// Orleans uses standard C# error handling
public async Task<int> DoWork() {
    try {
        var result = await externalService.Call();
        return result;
    } catch (TransientException ex) {
        // Log and retry or return default
        logger.LogWarning(ex, "Transient error");
        return -1;
    }
    // Let permanent errors bubble up to Orleans runtime
}
```

**Orleans Abstraction Benefits**:
- Developers write normal try/catch code
- Runtime handles distributed concerns (placement, activation, recovery)
- State management abstracts persistence complexity

**Tradeoffs**:
- ✅ Familiar error handling patterns (try/catch)
- ✅ Automatic actor placement and recovery
- ✅ Implicit state persistence
- ❌ Requires Orleans runtime and cluster
- ❌ .NET ecosystem (not JS/TS)
- ❌ Less explicit control over restart behavior

**Sources**:
- [Orleans overview - .NET | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/orleans/overview)
- [Intro to Virtual Actors by Microsoft Orleans](https://bogdan-dina03.medium.com/intro-to-virtual-actors-by-microsoft-orleans-6ae3264f138d)
- [Microsoft Orleans Overview: Actors, Grains, and Cloud-Native Architecture](https://bool.dev/blog/detail/microsoft-orleans-overview)

---

## 3. Lightweight Approaches for TypeScript

### 3.1 Result Types (Functional Error Handling)

**Pattern**: Return explicit success/failure objects instead of throwing.

```typescript
// Current tk-agents Response type (already using this!)
export interface Response {
  success: boolean;
  data?: unknown;
  error?: string;
  metadata?: {
    durationMs?: number;
    costUsd?: number;
    sessionId?: string;
  };
}

// Enhanced with error categories
export interface EnhancedResponse<T = unknown> {
  success: boolean;
  data?: T;
  error?: {
    type: "validation" | "transient" | "fatal" | "permanent";
    code: string;
    message: string;
    retryable: boolean;
    context?: Record<string, unknown>;
  };
  metadata?: Record<string, unknown>;
}

// Usage in actor
async send(message: Message): Promise<Response> {
  if (!this.validateMessage(message)) {
    return {
      success: false,
      error: {
        type: "validation",
        code: "INVALID_MESSAGE",
        message: "Message payload is invalid",
        retryable: false,
      },
    };
  }

  try {
    const result = await this.execute(message);
    return { success: true, data: result };
  } catch (error) {
    if (error instanceof NetworkError) {
      return {
        success: false,
        error: {
          type: "transient",
          code: "NETWORK_TIMEOUT",
          message: error.message,
          retryable: true,
        },
      };
    }

    // Unexpected error - let supervisor handle
    throw error;
  }
}
```

**Tradeoffs**:
- ✅ Explicit error handling (caller must check `success`)
- ✅ Type-safe error information
- ✅ No hidden control flow (vs exceptions)
- ❌ More verbose than exceptions
- ❌ Caller might ignore errors
- ❌ Still need exceptions for truly unexpected errors

### 3.2 Retry with Exponential Backoff

**Pattern**: Automatically retry transient failures with increasing delays.

```typescript
interface RetryConfig {
  maxAttempts: number;
  initialDelayMs: number;
  maxDelayMs: number;
  backoffMultiplier: number;
  jitter: boolean; // Prevent thundering herd
}

async function withRetry<T>(
  fn: () => Promise<Response<T>>,
  config: RetryConfig,
  isRetryable: (error: Response["error"]) => boolean
): Promise<Response<T>> {
  let attempt = 0;
  let delay = config.initialDelayMs;

  while (attempt < config.maxAttempts) {
    attempt++;
    const response = await fn();

    if (response.success) {
      return response;
    }

    if (!isRetryable(response.error)) {
      return response; // Don't retry permanent errors
    }

    if (attempt >= config.maxAttempts) {
      return {
        ...response,
        error: {
          ...response.error!,
          message: `Failed after ${attempt} attempts: ${response.error!.message}`,
        },
      };
    }

    // Exponential backoff with optional jitter
    const actualDelay = config.jitter
      ? delay * (0.5 + Math.random())
      : delay;

    await sleep(Math.min(actualDelay, config.maxDelayMs));
    delay *= config.backoffMultiplier;
  }

  throw new Error("Unreachable");
}

// Usage in ClaudeActor
async send(message: Message): Promise<Response> {
  return withRetry(
    () => this.executeClaude(message),
    {
      maxAttempts: 3,
      initialDelayMs: 1000,
      maxDelayMs: 10000,
      backoffMultiplier: 2,
      jitter: true,
    },
    (error) => error?.type === "transient"
  );
}
```

**Best Practices**:
- Start with 3-5 retries for most operations
- Use exponential backoff: 1s → 2s → 4s → 8s
- Add jitter to prevent synchronized retries (thundering herd)
- Only retry transient errors (network, rate limits, timeouts)
- Never retry validation or authentication errors
- Set maximum retry delay (cap exponential growth)

**Tradeoffs**:
- ✅ Handles transient failures automatically
- ✅ Configurable per operation
- ✅ Prevents cascading failures
- ❌ Increases latency on failures
- ❌ Can mask underlying issues
- ❌ Needs idempotent operations

**Sources**:
- [Best practices for retry pattern](https://harish-bhattbhatt.medium.com/best-practices-for-retry-pattern-f29d47cd5117)
- [Transient fault handling - Azure Architecture Center](https://learn.microsoft.com/en-us/azure/architecture/best-practices/transient-faults)
- [Retry logic in Workflows: Best practices for failure handling | Temporal](https://temporal.io/blog/failure-handling-in-practice)

### 3.3 Circuit Breaker Pattern

**Pattern**: Stop calling a failing service to allow it to recover.

```typescript
type CircuitState = "closed" | "open" | "half-open";

interface CircuitBreakerConfig {
  failureThreshold: number; // Number of failures before opening
  successThreshold: number; // Successes in half-open before closing
  timeout: number; // How long to stay open before trying again
}

class CircuitBreaker {
  private state: CircuitState = "closed";
  private failureCount = 0;
  private successCount = 0;
  private nextAttempt = 0;

  constructor(private config: CircuitBreakerConfig) {}

  async execute<T>(fn: () => Promise<T>): Promise<T> {
    if (this.state === "open") {
      if (Date.now() < this.nextAttempt) {
        throw new Error("Circuit breaker is OPEN");
      }
      this.state = "half-open";
      this.successCount = 0;
    }

    try {
      const result = await fn();
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      throw error;
    }
  }

  private onSuccess(): void {
    this.failureCount = 0;

    if (this.state === "half-open") {
      this.successCount++;
      if (this.successCount >= this.config.successThreshold) {
        this.state = "closed";
      }
    }
  }

  private onFailure(): void {
    this.failureCount++;

    if (this.failureCount >= this.config.failureThreshold) {
      this.state = "open";
      this.nextAttempt = Date.now() + this.config.timeout;
    }
  }

  getState(): CircuitState {
    return this.state;
  }
}

// Usage in actor
class ClaudeActor {
  private circuitBreaker = new CircuitBreaker({
    failureThreshold: 5,
    successThreshold: 2,
    timeout: 60000, // 1 minute
  });

  async send(message: Message): Promise<Response> {
    try {
      return await this.circuitBreaker.execute(() =>
        this.executeClaude(message)
      );
    } catch (error) {
      if (error.message.includes("Circuit breaker is OPEN")) {
        return {
          success: false,
          error: {
            type: "transient",
            code: "CIRCUIT_OPEN",
            message: "Service temporarily unavailable",
            retryable: false, // Don't retry while circuit is open
          },
        };
      }
      throw error;
    }
  }
}
```

**When to Use**:
- External API calls (Claude CLI, databases, HTTP services)
- Operations that can cascade failures
- Services with rate limits
- Expensive operations that might fail repeatedly

**Tradeoffs**:
- ✅ Prevents cascading failures
- ✅ Gives failing services time to recover
- ✅ Fast-fail when service is down
- ❌ Adds complexity
- ❌ May reject valid requests during recovery
- ❌ Requires tuning thresholds

**Sources**:
- [Resilience Patterns in TypeScript: Circuit Breaker](https://nobuti.com/thoughts/resilience-patterns-circuit-breaker)
- [Circuit Breaker Pattern in Node.js and TypeScript](https://dev.to/wallacefreitas/circuit-breaker-pattern-in-nodejs-and-typescript-enhancing-resilience-and-stability-bfi)
- [Tutorial on Circuit Breaker Pattern in TypeScript](https://www.squash.io/tutorial-on-circuit-breaker-pattern-in-typescript/)

### 3.4 Timeout Strategies

**Pattern**: Prevent hanging indefinitely on slow operations.

```typescript
// Simple timeout wrapper
async function withTimeout<T>(
  promise: Promise<T>,
  timeoutMs: number,
  operation: string
): Promise<T> {
  let timeoutId: Timer;

  const timeoutPromise = new Promise<never>((_, reject) => {
    timeoutId = setTimeout(() => {
      reject(new Error(`Operation "${operation}" timed out after ${timeoutMs}ms`));
    }, timeoutMs);
  });

  try {
    const result = await Promise.race([promise, timeoutPromise]);
    clearTimeout(timeoutId);
    return result;
  } catch (error) {
    clearTimeout(timeoutId);
    throw error;
  }
}

// Usage in ClaudeActor
async send(message: Message): Promise<Response> {
  try {
    const result = await withTimeout(
      this.executeClaude(message),
      120000, // 2 minutes
      "Claude API call"
    );
    return { success: true, data: result };
  } catch (error) {
    if (error.message.includes("timed out")) {
      return {
        success: false,
        error: {
          type: "transient",
          code: "TIMEOUT",
          message: error.message,
          retryable: true,
        },
      };
    }
    throw error;
  }
}

// Timeout with cancellation (better for long-running operations)
class CancellableOperation<T> {
  private controller = new AbortController();

  async execute(
    fn: (signal: AbortSignal) => Promise<T>,
    timeoutMs: number
  ): Promise<T> {
    const timeoutId = setTimeout(() => {
      this.controller.abort();
    }, timeoutMs);

    try {
      const result = await fn(this.controller.signal);
      clearTimeout(timeoutId);
      return result;
    } catch (error) {
      clearTimeout(timeoutId);
      if (error.name === "AbortError") {
        throw new Error(`Operation cancelled after ${timeoutMs}ms`);
      }
      throw error;
    }
  }

  cancel(): void {
    this.controller.abort();
  }
}
```

**Timeout Configuration Guidelines**:
- ClaudeActor: 2-5 minutes (LLM calls can be slow)
- BashActor: 30 seconds default, configurable per command
- Network calls: 10-30 seconds
- Database queries: 5-10 seconds
- Internal operations: 1-5 seconds

**Tradeoffs**:
- ✅ Prevents infinite hangs
- ✅ Frees resources on slow operations
- ✅ Improves system responsiveness
- ❌ May interrupt legitimate long operations
- ❌ Needs careful tuning per operation
- ❌ Cancellation not always clean (resource cleanup)

### 3.5 Supervisor Pattern (Simplified for TypeScript)

**Pattern**: Parent actors monitor and restart child actors on failure.

```typescript
interface SupervisorConfig {
  maxRestarts: number; // Max restarts in time window
  windowMs: number; // Time window for restart counting
  strategy: "one-for-one" | "one-for-all";
}

class Supervisor {
  private children = new Map<string, Actor>();
  private restartCounts = new Map<string, number[]>();

  constructor(private config: SupervisorConfig) {}

  async supervise(actor: Actor): Promise<void> {
    this.children.set(actor.id, actor);
    this.restartCounts.set(actor.id, []);
  }

  async send(actorId: string, message: Message): Promise<Response> {
    const actor = this.children.get(actorId);
    if (!actor) {
      return {
        success: false,
        error: {
          type: "permanent",
          code: "ACTOR_NOT_FOUND",
          message: `Actor ${actorId} not found`,
          retryable: false,
        },
      };
    }

    try {
      return await actor.send(message);
    } catch (error) {
      // Fatal error - attempt restart
      return await this.handleFailure(actorId, error as Error);
    }
  }

  private async handleFailure(
    actorId: string,
    error: Error
  ): Promise<Response> {
    const now = Date.now();
    const restarts = this.restartCounts.get(actorId) || [];

    // Clean up old restart timestamps outside window
    const recentRestarts = restarts.filter(
      (timestamp) => now - timestamp < this.config.windowMs
    );

    if (recentRestarts.length >= this.config.maxRestarts) {
      // Too many restarts - give up
      this.children.delete(actorId);
      return {
        success: false,
        error: {
          type: "fatal",
          code: "MAX_RESTARTS_EXCEEDED",
          message: `Actor ${actorId} failed ${this.config.maxRestarts} times in ${this.config.windowMs}ms`,
          retryable: false,
          context: { originalError: error.message },
        },
      };
    }

    // Record restart and recreate actor
    recentRestarts.push(now);
    this.restartCounts.set(actorId, recentRestarts);

    console.warn(
      `Restarting actor ${actorId} (${recentRestarts.length}/${this.config.maxRestarts})`
    );

    // In a real implementation, you'd recreate the actor here
    // For now, just return error
    return {
      success: false,
      error: {
        type: "fatal",
        code: "ACTOR_CRASHED",
        message: `Actor ${actorId} crashed: ${error.message}`,
        retryable: false,
        context: { restartCount: recentRestarts.length },
      },
    };
  }
}

// Usage
const supervisor = new Supervisor({
  maxRestarts: 3,
  windowMs: 60000, // 1 minute
  strategy: "one-for-one",
});

const claudeActor = new ClaudeActor({ id: "claude-1", model: "haiku" });
await supervisor.supervise(claudeActor);

// Supervisor handles crashes automatically
const response = await supervisor.send("claude-1", message);
```

**Tradeoffs**:
- ✅ Automatic recovery from crashes
- ✅ Prevents infinite restart loops
- ✅ Isolates failures
- ❌ Complexity overhead
- ❌ Lost actor state on restart
- ❌ Requires actor factory functions

---

## 4. Current tk-agents Error Handling Audit

### 4.1 What's Already Good

✅ **Response Type**: Already using `Response` with `success: boolean`
```typescript
export interface Response {
  success: boolean;
  data?: unknown;
  error?: string;
  metadata?: { durationMs?: number; costUsd?: number; sessionId?: string };
}
```

✅ **Timeout Support**: BashActor has configurable timeouts
```typescript
const timeoutId = this.config.timeout
  ? setTimeout(() => {
      timedOut = true;
      proc.kill();
    }, this.config.timeout)
  : null;
```

✅ **Exit Code Handling**: BashActor properly captures non-zero exit codes
```typescript
if (result.exitCode !== 0) {
  return {
    success: false,
    error: result.stderr || `Exit code ${result.exitCode}`,
    data: { stdout: result.stdout, stderr: result.stderr, exitCode: result.exitCode },
    metadata: { durationMs },
  };
}
```

### 4.2 Current Issues

❌ **Uncategorized Errors**: All errors are strings, no type classification
```typescript
error: "Node not found: task_123"  // Is this permanent? transient?
```

❌ **Throws on Unknown Messages**: Crashes actor instead of returning error
```typescript
default:
  throw new Error(`Unknown message type: ${message.type}`);
  // Should return: { success: false, error: { type: "validation", ... } }
```

❌ **No Retry Logic**: Single-shot calls to Claude CLI (what if timeout?)
```typescript
const proc = spawn(args, { stdout: "pipe", stderr: "pipe" });
// No retry if this fails transiently
```

❌ **No Circuit Breaker**: Could hammer failing Claude CLI repeatedly
```typescript
// If Claude CLI is down, we'll just keep trying indefinitely
```

❌ **Session Hanging**: No way to cancel or timeout a hung ClaudeActor session
```typescript
async *stream(message: Message): AsyncGenerator<StreamEvent, Response> {
  // What if this hangs forever?
  for await (const chunk of proc.stdout) {
    // No timeout, no cancellation
  }
}
```

### 4.3 Specific Concerns

**ClaudeActor Hanging Indefinitely**:
- Streaming can hang if CLI doesn't respond
- No timeout on `proc.stdout` iteration
- No way to cancel an in-progress session

**Network Failures**:
- Claude CLI might fail due to network issues
- No retry logic for transient failures
- No backoff if API rate limits

**Invalid State**:
- Unknown message types throw instead of returning error
- No validation of message payloads before processing

---

## 5. Recommended Approach for Exploration Phase

### Phase 1: Error Classification (Immediate)

**Goal**: Make errors explicit and categorized.

```typescript
// src/actors/errors.ts
export type ErrorType = "validation" | "transient" | "fatal" | "permanent";

export interface ActorError {
  type: ErrorType;
  code: string;
  message: string;
  retryable: boolean;
  context?: Record<string, unknown>;
}

// Update Response type
export interface Response<T = unknown> {
  success: boolean;
  data?: T;
  error?: ActorError;
  metadata?: Record<string, unknown>;
}

// Error factory helpers
export const Errors = {
  validation: (code: string, message: string, context?: Record<string, unknown>): ActorError => ({
    type: "validation",
    code,
    message,
    retryable: false,
    context,
  }),

  transient: (code: string, message: string, context?: Record<string, unknown>): ActorError => ({
    type: "transient",
    code,
    message,
    retryable: true,
    context,
  }),

  fatal: (code: string, message: string, context?: Record<string, unknown>): ActorError => ({
    type: "fatal",
    code,
    message,
    retryable: false,
    context,
  }),

  permanent: (code: string, message: string, context?: Record<string, unknown>): ActorError => ({
    type: "permanent",
    code,
    message,
    retryable: false,
    context,
  }),
};
```

**Changes to Actors**:
```typescript
// Before
throw new Error(`Unknown message type: ${message.type}`);

// After
return {
  success: false,
  error: Errors.validation(
    "UNKNOWN_MESSAGE_TYPE",
    `Unknown message type: ${message.type}`,
    { messageType: message.type }
  ),
};
```

**Tradeoffs**:
- ✅ Minimal code changes
- ✅ Explicit error handling
- ✅ No new dependencies
- ✅ Foundation for future patterns
- ❌ More verbose
- ❌ Caller still needs to check `success`

### Phase 2: Timeouts and Retry (As Needed)

**Goal**: Handle hanging operations and transient failures.

**Add to ClaudeActor**:
```typescript
import { withTimeout, withRetry } from "./resilience";

async send(message: Message): Promise<Response> {
  return withRetry(
    () => withTimeout(
      this.executeClaude(message),
      120000, // 2 minute timeout
      "Claude CLI call"
    ),
    {
      maxAttempts: 2, // Only retry once
      initialDelayMs: 5000,
      maxDelayMs: 10000,
      backoffMultiplier: 2,
      jitter: true,
    },
    (error) => error?.type === "transient"
  );
}
```

**When to Add**:
- After observing actual timeout issues
- When transient failures become frequent
- When debugging indicates need

**Tradeoffs**:
- ✅ Solves real problems
- ✅ Simple to add
- ✅ Configurable per actor
- ❌ Increases latency on failures
- ❌ Requires tuning

### Phase 3: Supervision (Later)

**Goal**: Automatic recovery from crashes in critical actors.

**When to Add**:
- Multi-session deployments
- Long-running actor systems
- Production use

**Not Needed Yet**:
- Exploration phase doesn't need supervision
- State is not critical (can recreate actors)
- Simpler to debug without restart logic

### Phase 4: Circuit Breakers (Defer)

**Goal**: Prevent cascading failures in distributed scenarios.

**When to Add**:
- Multiple instances calling shared services
- External API rate limiting issues
- Cascade failures observed

**Not Needed Yet**:
- Single-user exploration
- No shared external dependencies yet
- Adds complexity without current benefit

---

## 6. Tradeoff Analysis: Simplicity vs Robustness

### Simplicity Spectrum

```
Simplest                                                    Most Robust
   │                                                              │
   ▼                                                              ▼
throw Error ──> Result types ──> +Retry ──> +Timeouts ──> +Circuit Breaker ──> +Supervision
   │               │                │           │                │                  │
   Easy         Still easy       Getting     Complex          Expert            Production
   to debug     to reason       complex     tradeoffs         level             hardened
```

### Recommendation Matrix

| Pattern | Phase | Complexity | Value for tk-agents | Recommended? |
|---------|-------|------------|---------------------|--------------|
| Result Types | 1 | Low | High (foundation) | ✅ Yes, now |
| Error Classification | 1 | Low | High (clarity) | ✅ Yes, now |
| Timeouts | 2 | Low | High (prevents hangs) | ✅ Yes, as needed |
| Retry Logic | 2 | Medium | Medium (transient failures) | ⚠️ Only for Claude CLI |
| Circuit Breaker | 4 | High | Low (single user) | ❌ Defer |
| Supervision | 3 | High | Low (exploration) | ❌ Defer |

### Decision Criteria

**Add a pattern when**:
- ✅ You observe the failure mode in practice
- ✅ The pattern solves a real pain point
- ✅ The complexity is justified by the benefit
- ✅ You can test it effectively

**Don't add a pattern when**:
- ❌ "Might need it someday"
- ❌ Complicates debugging
- ❌ No evidence of the problem
- ❌ Simpler alternative exists

---

## 7. Implementation Examples

### Example 1: Fix Unknown Message Handler

**Before**:
```typescript
// src/task.ts
default:
  throw new Error(`Unknown message type: ${message.type}`);
```

**After**:
```typescript
// src/task.ts
default:
  return {
    success: false,
    error: Errors.validation(
      "UNKNOWN_MESSAGE_TYPE",
      `Task does not support message type: ${message.type}`,
      {
        supportedTypes: ["get", "observe", "update", "start", "spawn", "eval", "complete", "block"],
        receivedType: message.type,
      }
    ),
  };
```

### Example 2: Add Timeout to ClaudeActor.stream()

**Before**:
```typescript
async *stream(message: Message): AsyncGenerator<StreamEvent, Response> {
  const proc = spawn(args, { stdout: "pipe", stderr: "pipe" });

  for await (const chunk of proc.stdout) {
    // Could hang forever
    buffer += decoder.decode(chunk);
    // ...
  }
}
```

**After**:
```typescript
async *stream(message: Message): AsyncGenerator<StreamEvent, Response> {
  const proc = spawn(args, { stdout: "pipe", stderr: "pipe" });
  const startTime = Date.now();
  const timeoutMs = this.config.timeoutMs || 300000; // 5 minutes

  for await (const chunk of proc.stdout) {
    if (Date.now() - startTime > timeoutMs) {
      proc.kill();
      yield {
        type: "error",
        data: {
          error: Errors.transient(
            "STREAM_TIMEOUT",
            `Stream exceeded timeout of ${timeoutMs}ms`,
            { durationMs: Date.now() - startTime }
          ),
        },
        timestamp: new Date(),
      };
      break;
    }

    buffer += decoder.decode(chunk);
    // ... rest of logic
  }
}
```

### Example 3: Add Retry to ClaudeActor.send()

**Before**:
```typescript
async send(message: Message): Promise<Response> {
  const args = this.buildArgs(prompt, false);
  const proc = spawn(args, { stdout: "pipe", stderr: "pipe" });
  // Single attempt only
  const exitCode = await proc.exited;

  if (exitCode !== 0) {
    return { success: false, error: result.stderr };
  }
  // ...
}
```

**After**:
```typescript
async send(message: Message): Promise<Response> {
  return withRetry(
    async () => {
      const args = this.buildArgs(prompt, false);
      const proc = spawn(args, { stdout: "pipe", stderr: "pipe" });
      const exitCode = await proc.exited;

      if (exitCode !== 0) {
        // Check if retryable
        const isNetworkError = result.stderr.includes("network") ||
                               result.stderr.includes("timeout") ||
                               result.stderr.includes("ECONNREFUSED");

        return {
          success: false,
          error: isNetworkError
            ? Errors.transient("CLAUDE_CLI_NETWORK", result.stderr)
            : Errors.permanent("CLAUDE_CLI_ERROR", result.stderr),
        };
      }

      return { success: true, data: result.stdout };
    },
    {
      maxAttempts: 2, // One retry
      initialDelayMs: 2000,
      maxDelayMs: 5000,
      backoffMultiplier: 2,
      jitter: true,
    },
    (error) => error?.retryable === true
  );
}
```

---

## 8. Testing Strategy

### Test Categories

**Unit Tests**: Error classification and factories
```typescript
test("Errors.validation creates correct error", () => {
  const err = Errors.validation("TEST", "test message", { field: "foo" });
  expect(err.type).toBe("validation");
  expect(err.retryable).toBe(false);
  expect(err.context?.field).toBe("foo");
});
```

**Integration Tests**: Retry and timeout behavior
```typescript
test("ClaudeActor retries on network error", async () => {
  let attempts = 0;
  const mock = createMockClaude(() => {
    attempts++;
    if (attempts < 2) {
      return { success: false, error: Errors.transient("NETWORK", "timeout") };
    }
    return { success: true, data: "ok" };
  });

  const response = await mock.send(createMessage("test", "hello"));
  expect(attempts).toBe(2);
  expect(response.success).toBe(true);
});
```

**Failure Mode Tests**: What happens when things go wrong
```typescript
test("BashActor timeout kills process", async () => {
  const actor = createBashActor({ id: "bash", timeout: 100 });
  const response = await actor.send(createMessage("exec", "sleep 10"));

  expect(response.success).toBe(false);
  expect(response.error?.type).toBe("transient");
  expect(response.error?.code).toBe("TIMEOUT");
});
```

---

## 9. Migration Path

### Week 1: Foundation
1. Create `src/actors/errors.ts` with error types
2. Update `Response` interface in `base.ts`
3. Replace `throw new Error()` in message handlers with error returns
4. Add tests for error classification

### Week 2: Timeouts
1. Add `withTimeout()` utility
2. Add timeout config to `ClaudeActorConfig`
3. Wrap Claude CLI calls with timeout
4. Add timeout tests

### Week 3: Retry (Optional)
1. Add `withRetry()` utility
2. Identify retryable vs permanent errors in Claude CLI
3. Wrap ClaudeActor calls with retry
4. Add retry tests with mock failures

### Week 4+: As Needed
- Add circuit breaker if hammering issues observed
- Add supervision if actor crashes become frequent
- Add metrics/logging for error rates

---

## 10. Conclusion

### Start Here (Phase 1)
1. **Error Classification**: Add `ActorError` type with categories
2. **Result Types**: Update `Response` to use `ActorError`
3. **Remove Throws**: Convert `throw new Error()` to error returns

### Add When Needed (Phase 2)
4. **Timeouts**: Add to ClaudeActor to prevent hanging
5. **Retry**: Add for transient Claude CLI failures (if observed)

### Defer for Now (Phases 3-4)
6. **Circuit Breaker**: Not needed for single-user exploration
7. **Supervision**: Not needed until multi-session or production

### Key Principles
- ✅ **Explicit over implicit**: Return errors, don't throw (except truly unexpected)
- ✅ **Categorize errors**: Validation, transient, fatal, permanent
- ✅ **YAGNI**: Don't add patterns until you need them
- ✅ **Debuggable**: Prefer simple, traceable error paths
- ✅ **Testable**: Write tests for failure modes

---

## Sources

### Erlang Actor Model
- [The "let it crash" error handling strategy of Erlang](https://dev.to/adolfont/the-let-it-crash-error-handling-strategy-of-erlang-by-joe-armstrong-25hf)
- [Erlang "Let it Crash" Approach to Building Reliable Services](https://medium.com/@vamsimokari/erlang-let-it-crash-philosophy-53486d2a6da)
- [Erlang-style Supervisors in C# with Akka.NET and the Actor Model](https://buildplease.com/pages/supervisors-csharp/)

### Akka Supervision
- [Fault tolerance | Akka.NET Documentation](https://getakka.net/articles/actors/fault-tolerance.html)
- [Supervision and Monitoring | Baeldung on Scala](https://www.baeldung.com/scala/akka-supervision)
- [Fault Tolerance • Akka core](https://doc.akka.io/docs/akka/current/typed/fault-tolerance.html)
- [Supervision | Akka.NET Documentation](https://getakka.net/articles/concepts/supervision.html)

### Orleans Virtual Actors
- [Orleans overview - .NET | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/orleans/overview)
- [Intro to Virtual Actors by Microsoft Orleans](https://bogdan-dina03.medium.com/intro-to-virtual-actors-by-microsoft-orleans-6ae3264f138d)
- [Microsoft Orleans Overview: Actors, Grains, and Cloud-Native Architecture](https://bool.dev/blog/detail/microsoft-orleans-overview)

### TypeScript Patterns
- [Resilience Patterns in TypeScript: Circuit Breaker](https://nobuti.com/thoughts/resilience-patterns-circuit-breaker)
- [Circuit Breaker Pattern in Node.js and TypeScript](https://dev.to/wallacefreitas/circuit-breaker-pattern-in-nodejs-and-typescript-enhancing-resilience-and-stability-bfi)
- [Tutorial on Circuit Breaker Pattern in TypeScript](https://www.squash.io/tutorial-on-circuit-breaker-pattern-in-typescript/)
- [Node.js Advanced Patterns: Implementing Robust Retry Logic](https://v-checha.medium.com/advanced-node-js-patterns-implementing-robust-retry-logic-656cf70f8ee9)

### Retry and Error Classification
- [Best practices for retry pattern](https://harish-bhattbhatt.medium.com/best-practices-for-retry-pattern-f29d47cd5117)
- [Transient fault handling - Azure Architecture Center](https://learn.microsoft.com/en-us/azure/architecture/best-practices/transient-faults)
- [Retry logic in Workflows: Best practices for failure handling | Temporal](https://temporal.io/blog/failure-handling-in-practice)
- [Retry strategy | Cloud Storage | Google Cloud](https://cloud.google.com/storage/docs/retry-strategy)
