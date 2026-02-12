# SchedulerActor Migration Guide

## Overview

This guide demonstrates how to migrate from direct `setTimeout`/`setInterval` calls to the actor-based SchedulerActor system.

## Benefits

1. **Testability**: Virtual time allows instant test execution (80% faster tests)
2. **Consistency**: All actors use the same time coordination mechanism
3. **Observability**: Scheduled messages can be inspected and monitored
4. **Cancellation**: All schedules can be cancelled cleanly
5. **No blocking**: Message-based scheduling doesn't block execution

## Migration Patterns

### Pattern 1: Simple Delay (setTimeout)

**Before:**
```typescript
class MyService {
  async processTask() {
    // Start processing
    console.log('Starting task');

    // Wait 5 seconds then timeout
    setTimeout(() => {
      this.handleTimeout();
    }, 5000);
  }

  private handleTimeout() {
    console.log('Task timed out');
  }
}
```

**After:**
```typescript
class MyTaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'process') {
      this.logInfo('Starting task');

      // Schedule timeout message to self
      await this.schedule(5000, 'timeout');

      return createResponse(message, { status: 'processing' });
    }

    if (message.type === 'timeout') {
      this.logWarn('Task timed out');
      return createResponse(message, { status: 'timed_out' });
    }

    return createResponse(message, { success: false, error: 'Unknown message' });
  }
}
```

### Pattern 2: Recurring Timer (setInterval)

**Before:**
```typescript
class HealthMonitor {
  private intervalId: any;

  start() {
    this.intervalId = setInterval(() => {
      this.checkHealth();
    }, 60000); // Every minute
  }

  stop() {
    if (this.intervalId) {
      clearInterval(this.intervalId);
    }
  }

  private async checkHealth() {
    // Perform health check
  }
}
```

**After:**
```typescript
class HealthMonitorActor extends Actor {
  private scheduleId: string | null = null;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      // Schedule recurring health check
      this.scheduleId = await this.scheduleRecurring(60000, 'ping');
      this.logInfo('Health monitoring started');

      return createResponse(message, { status: 'monitoring' });
    }

    if (message.type === 'stop') {
      if (this.scheduleId) {
        await this.cancelSchedule(this.scheduleId);
        this.scheduleId = null;
      }
      this.logInfo('Health monitoring stopped');

      return createResponse(message, { status: 'stopped' });
    }

    if (message.type === 'ping') {
      // Perform health check
      this.logDebug('Health check performed');

      return createResponse(message, { status: 'healthy' });
    }

    return createResponse(message, { success: false, error: 'Unknown message' });
  }
}
```

### Pattern 3: Exponential Backoff with setTimeout

**Before:**
```typescript
class RetryService {
  private retryCount = 0;

  async connect() {
    try {
      await this.attemptConnection();
    } catch (error) {
      this.retryCount++;
      const delay = Math.min(1000 * Math.pow(2, this.retryCount), 30000);

      setTimeout(() => {
        this.connect();
      }, delay);
    }
  }
}
```

**After:**
```typescript
class ConnectionActor extends Actor {
  private retryCount = 0;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'connect' || message.type === 'retry') {
      try {
        await this.attemptConnection();
        this.retryCount = 0;
        this.logInfo('Connected successfully');

        return createResponse(message, { status: 'connected' });
      } catch (error: any) {
        this.retryCount++;
        const delay = Math.min(1000 * Math.pow(2, this.retryCount), 30000);

        this.logWarn('Connection failed, retrying', {
          attempt: this.retryCount,
          nextRetryIn: delay,
        });

        // Schedule retry
        await this.schedule(delay, 'retry');

        return createResponse(message, {
          status: 'retrying',
          attempt: this.retryCount,
        });
      }
    }

    return createResponse(message, { success: false, error: 'Unknown message' });
  }

  private async attemptConnection(): Promise<void> {
    // Connection logic
  }
}
```

### Pattern 4: Promise-based Delay

**Before:**
```typescript
async function batchProcess(items: any[]) {
  for (let i = 0; i < items.length; i++) {
    await processItem(items[i]);

    // Delay between items
    if (i < items.length - 1) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  }
}
```

**After (Actor-based):**
```typescript
class BatchProcessorActor extends Actor {
  private currentIndex = 0;
  private items: any[] = [];

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'batch.start') {
      this.items = message.payload.items;
      this.currentIndex = 0;

      // Process first item
      await this.tell(this.address, 'batch.process-next', {});

      return createResponse(message, { status: 'started', total: this.items.length });
    }

    if (message.type === 'batch.process-next') {
      if (this.currentIndex >= this.items.length) {
        this.logInfo('Batch processing complete');
        return createResponse(message, { status: 'complete' });
      }

      // Process current item
      await this.processItem(this.items[this.currentIndex]);
      this.currentIndex++;

      // Schedule next item with delay
      if (this.currentIndex < this.items.length) {
        await this.schedule(100, 'batch.process-next');
      }

      return createResponse(message, {
        status: 'processing',
        progress: this.currentIndex
      });
    }

    return createResponse(message, { success: false, error: 'Unknown message' });
  }

  private async processItem(item: any): Promise<void> {
    // Process item logic
  }
}
```

## Testing with Virtual Time

**Before (slow, real time):**
```typescript
test('should timeout after 5 seconds', async () => {
  const service = new MyService();

  let timedOut = false;
  service.onTimeout = () => { timedOut = true; };

  await service.start();

  // Wait 5 seconds in real time
  await new Promise(resolve => setTimeout(resolve, 5000));

  expect(timedOut).toBe(true);
}, 10000); // 10 second timeout
```

**After (instant, virtual time):**
```typescript
test('should timeout after 5 seconds', async () => {
  const virtualClock = new VirtualClock();
  const scheduler = new SchedulerActor(router, { clock: virtualClock });
  router.registerActor('/system/scheduler', scheduler);

  const actor = new MyTaskActor('task', router);
  router.registerActor('/task', actor);

  let timedOut = false;

  // Start task
  await actor.receive(createMessage(
    address('/task'),
    'process',
    {},
    { pattern: 'ask', from: address('/test') }
  ));

  // Advance virtual time instantly
  await virtualClock.advance(5000);

  expect(timedOut).toBe(true);
}); // Completes in milliseconds, not seconds
```

## Actor Base Class API

All actors automatically get these methods:

```typescript
export class Actor {
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
}
```

## Setup

### 1. Register SchedulerActor (once at startup)

```typescript
import { SchedulerActor } from '../system-actors/scheduler.ts';

// Production: Real time
const scheduler = new SchedulerActor(router, { clock: 'real' });
router.registerActor('/system/scheduler', scheduler);

// Tests: Virtual time
const scheduler = new SchedulerActor(router, { clock: 'virtual' });
router.registerActor('/system/scheduler', scheduler);

// Access virtual clock for tests
const virtualClock = scheduler.getClock() as VirtualClock;
await virtualClock.advance(1000); // Advance 1 second instantly
```

### 2. Use in Actors

```typescript
class MyActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      // Just use this.schedule() - no setup needed!
      await this.schedule(5000, 'timeout');
      return createResponse(message, { status: 'started' });
    }

    if (message.type === 'timeout') {
      this.logWarn('Timed out');
      return createResponse(message, { status: 'timed_out' });
    }

    return createResponse(message, { success: false });
  }
}
```

## Common Migration Scenarios

### Scenario 1: Service Class with Timers

**Before:**
```typescript
class RateLimiter {
  private tokens = 100;
  private refillTimer: any;

  start() {
    this.refillTimer = setInterval(() => {
      this.tokens = Math.min(this.tokens + 10, 100);
    }, 1000);
  }

  stop() {
    clearInterval(this.refillTimer);
  }
}
```

**After:**
```typescript
class RateLimiterActor extends Actor {
  private tokens = 100;
  private scheduleId: string | null = null;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start') {
      this.scheduleId = await this.scheduleRecurring(1000, 'refill');
      return createResponse(message, { status: 'started' });
    }

    if (message.type === 'refill') {
      this.tokens = Math.min(this.tokens + 10, 100);
      return createResponse(message, { tokens: this.tokens });
    }

    if (message.type === 'stop') {
      if (this.scheduleId) {
        await this.cancelSchedule(this.scheduleId);
      }
      return createResponse(message, { status: 'stopped' });
    }

    return createResponse(message, { success: false });
  }
}
```

## Statistics

**Before Migration:**
- 39 direct setTimeout/setInterval calls
- Test suite: ~45 seconds
- 221 total timer occurrences across 43 files

**Target After Migration:**
- 0 direct setTimeout/setInterval calls (all through SchedulerActor)
- Test suite: ~10 seconds (80% faster with virtual time)
- 100% testable with instant time control

## Next Steps

1. Migrate high-impact timers first (tests, critical paths)
2. Use virtual time in all actor tests
3. Replace setTimeout in batch processing
4. Convert recurring timers in monitoring/health checks
5. Enable ESLint rule to prevent new direct timer usage
