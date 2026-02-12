# setTimeout Migration Guide: SchedulerActor Best Practices

## Executive Summary

This guide provides practical patterns for migrating from `setTimeout`/`setInterval` to the SchedulerActor system. The migration achieves **99.9% test speedup** by using virtual time control while maintaining production compatibility with real time.

**Quick Stats:**
- **Speedup Achieved:** 99.9% (2,340x faster) on migrated tests
- **Total setTimeout/setInterval occurrences:** 291 across 53 files
- **Migrated so far:** 3 test suites (streaming timeouts, scheduler demo)
- **Test suite baseline:** 36.4s (2545 passing tests)

---

## When to Migrate

Use this decision tree to determine whether to migrate a setTimeout/setInterval call:

```
Is this setTimeout/setInterval in actor business logic?
├─ YES: Should migrate to SchedulerActor
│   ├─ Is it a test delay?                    → MIGRATE (easy win, instant speedup)
│   ├─ Is it actor retry/backoff logic?       → MIGRATE (better testability)
│   ├─ Is it a recurring health check/poll?   → MIGRATE (clean cancellation)
│   └─ Is it batch processing throttling?     → MIGRATE (message-based control)
│
└─ NO: Consider keeping setTimeout
    ├─ Infrastructure timeouts (router.ask())  → KEEP (core messaging, not actor logic)
    ├─ Process management timeouts             → KEEP (subprocess cleanup, OS-level)
    ├─ Performance measurement delays          → KEEP (measuring real timing behavior)
    └─ Real-world timing tests (backpressure)  → KEEP (validating actual performance)
```

### ✅ SHOULD Migrate

**Test delays** - Instant wins:
```typescript
// Before: Wait 5 seconds in real time
await new Promise(resolve => setTimeout(resolve, 5000));

// After: Instant time advancement
await virtualClock.advance(5000);
```

**Actor retry logic** - Better patterns:
```typescript
// Before: Callback hell
setTimeout(() => this.retry(), backoffDelay);

// After: Message-based
await this.schedule(backoffDelay, 'retry');
```

**Recurring timers** - Clean lifecycle:
```typescript
// Before: Manual cleanup
this.interval = setInterval(() => this.poll(), 60000);
clearInterval(this.interval);

// After: Cancellable schedules
this.scheduleId = await this.scheduleRecurring(60000, 'poll');
await this.cancelSchedule(this.scheduleId);
```

### ❌ SHOULD NOT Migrate

**Infrastructure timeouts** - Core messaging:
```typescript
// router.ts - Message timeout handling
// KEEP: This is framework infrastructure, not actor business logic
const timeout = setTimeout(() => reject(new Error('Timeout')), 30000);
```

**Process management** - OS-level operations:
```typescript
// program-executor.ts - Subprocess cleanup
// KEEP: Process timeout is OS-level concern
setTimeout(() => process.kill(), 5000);
```

**Real timing tests** - Measuring actual performance:
```typescript
// streaming.test.ts - Backpressure validation
// KEEP: Tests real-world timing behavior
const start = Date.now();
await slowConsumer();
const duration = Date.now() - start;
expect(duration).toBeGreaterThan(100); // Needs real time
```

---

## Migration Patterns

### Pattern 1: Test Delays (99.9% speedup)

**Before:**
```typescript
test('should timeout after 5 seconds', async () => {
  actor.startTask();

  // Wait 5 seconds in real time
  await new Promise(resolve => setTimeout(resolve, 5000));

  expect(actor.timedOut).toBe(true);
}, 10000); // 10 second test timeout
```

**After:**
```typescript
test('should timeout after 5 seconds', async () => {
  // Setup virtual time
  const virtualClock = new VirtualClock();
  const scheduler = new SchedulerActor(router, { clock: virtualClock });
  router.registerActor('/system/scheduler', scheduler);

  const actor = new TaskActor('task', router);
  router.registerActor('/task', actor);

  // Start task
  await router.tell(address('/task'), 'start', {});

  // Advance time instantly (no waiting!)
  await virtualClock.advance(5000);

  expect(actor.timedOut).toBe(true);
}); // Completes in ~1ms instead of 5000ms
```

**Impact:** 5000ms → 1ms = 99.9% speedup

### Pattern 2: Actor Retry Logic

**Before:**
```typescript
class RetryActor extends Actor {
  private retryCount = 0;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'attempt') {
      try {
        await this.doWork();
        return createResponse(message, { success: true });
      } catch (error) {
        this.retryCount++;
        const delay = Math.pow(2, this.retryCount) * 1000;

        // OLD: Direct setTimeout
        setTimeout(async () => {
          await this.receive(createMessage(this.address, 'attempt', {}));
        }, delay);

        return createResponse(message, { retrying: true });
      }
    }
    return createResponse(message, { success: false });
  }
}
```

**After:**
```typescript
class RetryActor extends Actor {
  private retryCount = 0;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'attempt') {
      try {
        await this.doWork();
        this.retryCount = 0; // Reset on success
        return createResponse(message, { success: true });
      } catch (error) {
        this.retryCount++;
        const delay = Math.pow(2, this.retryCount) * 1000;

        // NEW: Schedule message to self
        await this.schedule(delay, 'retry');

        return createResponse(message, { retrying: true, attempt: this.retryCount });
      }
    }

    if (message.type === 'retry') {
      // Scheduled retry - send attempt message
      return await this.receive(createMessage(this.address, 'attempt', {}));
    }

    return createResponse(message, { success: false });
  }
}
```

**Benefits:**
- ✅ Testable with virtual time (instant retries in tests)
- ✅ Observable via message logs
- ✅ Cancellable (can cancel pending retries)
- ✅ No callback nesting

### Pattern 3: Recurring Timers (Health Checks, Polling)

**Before:**
```typescript
class HealthCheckActor extends Actor {
  private intervalId: any;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      // OLD: setInterval with manual cleanup
      this.intervalId = setInterval(() => {
        this.checkHealth();
      }, 60000); // Every minute

      return createResponse(message, { monitoring: true });
    }

    if (message.type === 'stop') {
      if (this.intervalId) {
        clearInterval(this.intervalId);
        this.intervalId = null;
      }
      return createResponse(message, { monitoring: false });
    }

    return createResponse(message, { success: false });
  }

  private async checkHealth(): Promise<void> {
    // Health check logic
  }
}
```

**After:**
```typescript
class HealthCheckActor extends Actor {
  private scheduleId: string | null = null;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      // NEW: Recurring schedule via message
      this.scheduleId = await this.scheduleRecurring(60000, 'health-check');

      return createResponse(message, { monitoring: true });
    }

    if (message.type === 'stop') {
      if (this.scheduleId) {
        await this.cancelSchedule(this.scheduleId);
        this.scheduleId = null;
      }
      return createResponse(message, { monitoring: false });
    }

    if (message.type === 'health-check') {
      // Recurring health check triggered by scheduler
      await this.performHealthCheck();
      return createResponse(message, { healthy: true });
    }

    return createResponse(message, { success: false });
  }

  private async performHealthCheck(): Promise<void> {
    // Health check logic
  }
}
```

**Benefits:**
- ✅ Clean cancellation (no dangling intervals)
- ✅ Testable with instant time advancement
- ✅ Can list active schedules for debugging

### Pattern 4: Timeout with Cancellation

**Before:**
```typescript
class TaskActor extends Actor {
  private timeoutId: any;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      // OLD: setTimeout with manual cleanup
      this.timeoutId = setTimeout(() => {
        this.handleTimeout();
      }, 5000);

      return createResponse(message, { started: true });
    }

    if (message.type === 'complete') {
      // Clear timeout on completion
      if (this.timeoutId) {
        clearTimeout(this.timeoutId);
        this.timeoutId = null;
      }
      return createResponse(message, { completed: true });
    }

    return createResponse(message, { success: false });
  }

  private handleTimeout(): void {
    // Timeout logic
  }
}
```

**After:**
```typescript
class TaskActor extends Actor {
  private timeoutScheduleId: string | null = null;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      // NEW: Schedule timeout message
      this.timeoutScheduleId = await this.schedule(5000, 'timeout');

      return createResponse(message, { started: true });
    }

    if (message.type === 'complete') {
      // Cancel timeout on completion
      if (this.timeoutScheduleId) {
        await this.cancelSchedule(this.timeoutScheduleId);
        this.timeoutScheduleId = null;
      }
      return createResponse(message, { completed: true });
    }

    if (message.type === 'timeout') {
      // Timeout triggered
      this.timeoutScheduleId = null;
      await this.handleTimeout();
      return createResponse(message, { timed_out: true });
    }

    return createResponse(message, { success: false });
  }

  private async handleTimeout(): Promise<void> {
    // Timeout logic
  }
}
```

**Benefits:**
- ✅ Deterministic cancellation (no race conditions)
- ✅ Testable timeout behavior with virtual time
- ✅ Observable via message logs

---

## Testing with VirtualClock

### Basic Test Setup

Every test file that uses timing should follow this pattern:

```typescript
import { test, expect, describe, beforeEach } from 'bun:test';
import { MessageRouter } from '../router.ts';
import { SchedulerActor, VirtualClock } from '../../system-actors/scheduler.ts';

describe('MyActor Tests', () => {
  let router: MessageRouter;
  let scheduler: SchedulerActor;
  let virtualClock: VirtualClock;

  beforeEach(() => {
    const store = createMockGraphStore();
    const programManager = createMockProgramManager();
    router = new MessageRouter(store, programManager);

    // CRITICAL: Use VirtualClock in tests
    virtualClock = new VirtualClock();
    scheduler = new SchedulerActor(router, { clock: virtualClock });
    router.registerActor('/system/scheduler', scheduler);
  });

  test('actor with timeout', async () => {
    const actor = new MyActor('test-actor', router);
    router.registerActor('/test-actor', actor);

    // Start task
    await router.tell(address('/test-actor'), 'start', {});

    // Advance time instantly
    await virtualClock.advance(5000);

    // Verify timeout occurred
    expect(actor.timedOut).toBe(true);
  });
});
```

### Advanced Time Control

```typescript
test('complex timing scenario', async () => {
  const actor = new StreamingActor('streamer', router, items, 20, true);
  router.registerActor('/streamer', actor);

  const result = streamAsync(actor, 'stream.start', {}, { timeout: 500 });

  // Advance time step-by-step
  await virtualClock.advance(20); // First item
  await virtualClock.advance(20); // Second item
  await virtualClock.advance(20); // Third item

  // Jump to timeout
  await virtualClock.advance(500);

  // Verify behavior at exact moment
  await expect(result).rejects.toThrow(/timed out/);
});
```

### Production vs Test Configuration

```typescript
// src/index.ts (Production)
const scheduler = new SchedulerActor(router, { clock: 'real' });
router.registerActor('/system/scheduler', scheduler);

// test setup (Tests)
const virtualClock = new VirtualClock();
const scheduler = new SchedulerActor(router, { clock: virtualClock });
router.registerActor('/system/scheduler', scheduler);
```

---

## Common Pitfalls

### Pitfall 1: Forgetting to Advance Time in Tests

**Problem:**
```typescript
test('timeout test', async () => {
  await router.tell(address('/actor'), 'start', {});

  // WRONG: Expecting timeout without advancing time
  expect(actor.timedOut).toBe(true); // FAILS - time hasn't advanced!
});
```

**Solution:**
```typescript
test('timeout test', async () => {
  await router.tell(address('/actor'), 'start', {});

  // CORRECT: Advance virtual time
  await virtualClock.advance(5000);

  expect(actor.timedOut).toBe(true); // PASSES
});
```

### Pitfall 2: Using Real setTimeout in Actor Code

**Problem:**
```typescript
class MyActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // WRONG: Direct setTimeout bypasses SchedulerActor
    setTimeout(() => this.doWork(), 1000);
    return createResponse(message, { success: true });
  }
}
```

**Solution:**
```typescript
class MyActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // CORRECT: Use schedule method
    await this.schedule(1000, 'do-work');
    return createResponse(message, { success: true });
  }
}
```

### Pitfall 3: Not Handling Scheduled Messages

**Problem:**
```typescript
class MyActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      await this.schedule(1000, 'timeout');
      return createResponse(message, { started: true });
    }

    // WRONG: No handler for 'timeout' message
    return createResponse(message, { success: false });
  }
}
```

**Solution:**
```typescript
class MyActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      await this.schedule(1000, 'timeout');
      return createResponse(message, { started: true });
    }

    // CORRECT: Handle scheduled message
    if (message.type === 'timeout') {
      this.logWarn('Task timed out');
      return createResponse(message, { timed_out: true });
    }

    return createResponse(message, { success: false });
  }
}
```

### Pitfall 4: Race Conditions with Time Advancement

**Problem:**
```typescript
test('flaky test', async () => {
  router.tell(address('/actor'), 'start', {}); // No await!

  // WRONG: Race condition - might advance before message processes
  await virtualClock.advance(1000);

  expect(actor.state).toBe('ready'); // FLAKY
});
```

**Solution:**
```typescript
test('deterministic test', async () => {
  // CORRECT: Wait for message to process
  await router.tell(address('/actor'), 'start', {});

  // Now safe to advance time
  await virtualClock.advance(1000);

  expect(actor.state).toBe('ready'); // RELIABLE
});
```

---

## API Reference

### Actor Base Class Methods

All actors inherit these methods:

```typescript
/**
 * Schedule a delayed message to self
 * @param delay - Delay in milliseconds
 * @param type - Message type
 * @param payload - Optional message payload
 * @returns Schedule ID for cancellation
 */
protected async schedule(delay: number, type: string, payload?: any): Promise<string>

/**
 * Schedule a recurring message to self
 * @param interval - Interval in milliseconds
 * @param type - Message type
 * @param payload - Optional message payload
 * @returns Schedule ID for cancellation
 */
protected async scheduleRecurring(interval: number, type: string, payload?: any): Promise<string>

/**
 * Cancel a scheduled or recurring message
 * @param scheduleId - Schedule ID from schedule() or scheduleRecurring()
 */
protected async cancelSchedule(scheduleId: string): Promise<void>
```

### VirtualClock Methods

For test control:

```typescript
class VirtualClock {
  /**
   * Get current virtual time (milliseconds since start)
   */
  now(): number

  /**
   * Advance virtual time and execute all timers up to new time
   * @param ms - Milliseconds to advance
   */
  async advance(ms: number): Promise<void>

  /**
   * Execute next pending timer (if any)
   * @returns true if a timer was executed
   */
  async runNext(): Promise<boolean>

  /**
   * Execute all pending timers
   */
  async runAll(): Promise<void>

  /**
   * Reset virtual time to 0
   */
  reset(): void
}
```

---

## Migration Checklist

Use this checklist when migrating a setTimeout/setInterval:

- [ ] **Identify the purpose** - What does this timer do?
- [ ] **Check the decision tree** - Should this be migrated?
- [ ] **Create message type** - What message should the scheduler send?
- [ ] **Replace timer call** - `setTimeout(fn, delay)` → `await this.schedule(delay, 'type')`
- [ ] **Add message handler** - Handle the scheduled message in `receive()`
- [ ] **Update tests** - Add VirtualClock setup
- [ ] **Verify speedup** - Measure test duration before/after
- [ ] **Document if novel** - Add pattern to this guide if new

---

## Performance Metrics

### Migrated Tests (Phase 1-2)

| Test Suite | Tests Migrated | Before | After | Speedup |
|------------|----------------|--------|-------|---------|
| `scheduler-migration-demo.test.ts` | 1 retry test | 351ms | 0.15ms | 99.9% |
| `streaming.test.ts` | 3 timeout tests | ~5060ms | instant | 99.9% |
| **Total** | **4 tests** | **~5411ms** | **~1ms** | **99.9%** |

### Test Suite Baseline

- **Total tests:** 2545 passing, 181 skipped
- **Total duration:** 36.4s
- **Total setTimeout/setInterval occurrences:** 291 across 53 files
- **Migrated:** ~4 tests (using VirtualClock)
- **Remaining:** ~287 occurrences

### Projected Impact (Full Migration)

If all test delays were migrated:

- **Current test suite:** 36.4s
- **Estimated time savings:** 5-10s (test delays)
- **Projected duration:** 26-31s
- **Target speedup:** 15-27%

**Note:** Original 80% target assumed more test delays. Many setTimeout calls are in production code (not tests), so realistic test speedup is 15-30%.

---

## Next Steps

### Immediate Priorities

1. **Migrate high-impact test files:**
   - `performance.test.ts` (2 occurrences)
   - `ask.test.ts` (1 occurrence)
   - `channels.test.ts` (13 occurrences)

2. **Document production migrations:**
   - Identify retry logic in production actors
   - Plan health check conversions
   - Update connection management

3. **Establish team conventions:**
   - Add migration patterns to code review checklist
   - Create ESLint rule to prevent new setTimeout in actors
   - Update actor development guide

### Long-term Vision

- **Complete migration:** Target 100% of actor business logic
- **Monitoring:** Add metrics for scheduled message counts
- **Debugging tools:** Create dev tools to inspect pending schedules
- **Performance:** Optimize VirtualClock for large test suites

---

## References

**Implementation:**
- SchedulerActor: `/Users/bln/play/agentic-primer/simplify/src/system-actors/scheduler.ts`
- VirtualClock: `/Users/bln/play/agentic-primer/simplify/src/system-actors/scheduler.ts`
- Actor base class: `/Users/bln/play/agentic-primer/simplify/src/messaging/actor.ts`

**Documentation:**
- Phase 1 Report: `docs/phases/PHASE_1_SCHEDULER_COMPLETE.md`
- Phase 2 Report: `docs/phases/PHASE_2_MIGRATION_COMPLETE.md`
- Phase 3 Report: `docs/phases/PHASE_3_COMPLETION_REPORT.md`
- Original Analysis: `SCHEDULER_MIGRATION_REPORT.md`
- System Actors Guide: `src/system-actors/MIGRATION_GUIDE.md`

**Examples:**
- Demo tests: `src/messaging/__tests__/scheduler-migration-demo.test.ts`
- Streaming tests: `src/messaging/__tests__/streaming.test.ts`

---

**Last Updated:** 2026-02-06
**Status:** Phase 1-2 Complete, Phase 3 Documentation Complete
**Next Phase:** Migrate remaining test files
