# Supervision Architecture for Actor System

## Overview

This document defines the hierarchical supervision architecture for the Simplify actor system, inspired by Erlang/OTP supervision trees but adapted for TypeScript and the UGS graph model.

**Core Principle:** "Let it crash" - Actors fail independently, supervisors detect failures and apply restart strategies to restore service without cascading failures.

## Architecture

### Supervision Tree

Supervisors form a hierarchical tree where each supervisor monitors child actors and applies restart policies on failure.

```
                    ┌─────────────────┐
                    │  Root Supervisor │
                    │  (ActorSystem)   │
                    └────────┬─────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
      ┌───────▼────┐  ┌─────▼─────┐  ┌────▼─────┐
      │  Channel   │  │  Session   │  │  Tool    │
      │ Supervisor │  │ Supervisor │  │Supervisor│
      └──────┬─────┘  └─────┬──────┘  └────┬─────┘
             │              │               │
      ┌──────┼──────┐      │        ┌──────┼──────┐
      │      │      │      │        │      │      │
   ┌──▼─┐ ┌─▼──┐ ┌─▼──┐  │     ┌──▼─┐  ┌─▼──┐  │
   │WA  │ │TG  │ │Disc│  │     │FS  │  │Code│  │
   │Chan│ │Chan│ │Chan│  │     │Tool│  │Tool│  │
   └────┘ └────┘ └────┘  │     └────┘  └────┘  │
                    ┌─────▼────┐                │
                    │ Session  │                │
                    │  Actor   │                │
                    └──────────┘                │
```

**Legend:**
- WA = WhatsApp, TG = Telegram, Disc = Discord
- FS = FileSystem, Code = CodeExecution

### Fault Isolation Boundaries

Supervision boundaries provide fault isolation:

1. **Root Supervisor (ActorSystem)**
   - Monitors top-level supervisors
   - System-wide failure recovery
   - Strategy: `one-for-one` (restart only failed supervisor)

2. **Channel Supervisor**
   - Monitors all messaging channel actors
   - Strategy: `one-for-one` (independent channel failures)
   - Boundary: Channel failures don't affect sessions or tools

3. **Session Supervisor**
   - Monitors user session actors
   - Strategy: `one-for-one` (independent session failures)
   - Boundary: Session failures don't affect channels or other sessions

4. **Tool Supervisor**
   - Monitors tool actors (filesystem, code execution, etc.)
   - Strategy: `rest-for-one` (filesystem failure may require code execution restart)
   - Boundary: Tool failures don't affect channels or sessions

## Restart Strategies

### One-for-One

Restart only the failed child actor. Other children continue running.

**Use Case:** Independent actors where failure doesn't affect siblings.

```typescript
// Example: Channel actors
// WhatsApp failure doesn't affect Telegram
strategy: {
  type: 'one-for-one',
  maxRestarts: 3,
  withinSeconds: 60
}
```

**Behavior:**
1. Child actor fails
2. Supervisor detects failure
3. Supervisor calls child's `preRestart()` hook
4. Supervisor creates new child instance
5. Supervisor calls new child's `postRestart()` hook
6. Other children continue unchanged

### One-for-All

Restart all children when any child fails.

**Use Case:** Dependent actors where all must restart together for consistency.

```typescript
// Example: Database connection pool with transaction manager
// If connection pool fails, transaction manager needs reset
strategy: {
  type: 'one-for-all',
  maxRestarts: 3,
  withinSeconds: 60
}
```

**Behavior:**
1. Any child actor fails
2. Supervisor detects failure
3. Supervisor calls `preRestart()` on all children
4. Supervisor terminates all children
5. Supervisor creates new instances for all children
6. Supervisor calls `postRestart()` on all new children

### Rest-for-One

Restart the failed child and all children started after it (in dependency order).

**Use Case:** Actors with start-order dependencies.

```typescript
// Example: Tool chain where later tools depend on earlier ones
// Order: FileSystem → CodeExecution → Knowledge
// If FileSystem fails, restart FileSystem and CodeExecution (not Knowledge)
strategy: {
  type: 'rest-for-one',
  maxRestarts: 3,
  withinSeconds: 60,
  childOrder: ['filesystem', 'code-execution', 'knowledge']
}
```

**Behavior:**
1. Child actor at position N fails
2. Supervisor detects failure
3. Supervisor calls `preRestart()` on children at positions N, N+1, N+2...
4. Supervisor terminates those children
5. Supervisor creates new instances (in order)
6. Supervisor calls `postRestart()` on new instances

### Permanent Failure (Escalation)

If restart limits are exceeded, supervisor escalates to its parent:

```typescript
// Example: Channel supervisor restarts exceeded
maxRestarts: 3,
withinSeconds: 60

// If 4th failure within 60s:
// 1. Channel supervisor gives up
// 2. Channel supervisor escalates to root supervisor
// 3. Root supervisor decides: restart channel supervisor or shutdown system
```

## Lifecycle Hooks

### preRestart(error: Error, message?: Message)

Called on failing actor before termination.

**Purpose:** Cleanup resources, save state, notify dependents.

```typescript
async preRestart(error: Error, message?: Message): Promise<void> {
  // Log failure context
  this.logger.error('Actor restarting due to error', { error, message });

  // Save critical state to persistent storage
  await this.saveCheckpoint({
    queuedMessages: this.messageQueue,
    connections: this.activeConnections,
    timestamp: Date.now()
  });

  // Close resources
  this.closePorts();
  await this.disconnect();

  // Notify subscribers
  await this.notifySubscribers({ type: 'actor-restart', reason: error.message });
}
```

**Contract:**
- Must complete within timeout (default: 5s)
- Should not throw errors (failures are logged but ignored)
- Cannot prevent restart (cleanup only)

### postRestart(checkpoint?: any)

Called on new actor instance after restart.

**Purpose:** Restore state, reconnect, resume operations.

```typescript
async postRestart(checkpoint?: any): Promise<void> {
  // Restore state from checkpoint
  if (checkpoint) {
    this.messageQueue = checkpoint.queuedMessages || [];
    this.activeConnections = checkpoint.connections || [];
    this.logger.info('Restored checkpoint from previous instance');
  }

  // Reconnect to external services
  await this.connect();

  // Resume message processing
  await this.resumeQueue();

  // Notify subscribers actor is ready
  await this.notifySubscribers({ type: 'actor-ready' });
}
```

**Contract:**
- Must complete within timeout (default: 10s)
- If throws error, supervisor retries or escalates
- Should be idempotent (safe to call multiple times)

## Error Monitoring

### Health Checks

Supervisors continuously monitor child health:

```typescript
interface HealthCheck {
  interval: number;        // Check every N milliseconds
  timeout: number;         // Health check timeout
  threshold: number;       // Consecutive failures before restart
}

// Example: Channel health check
healthCheck: {
  interval: 30000,         // Every 30 seconds
  timeout: 5000,           // 5s timeout per check
  threshold: 3             // 3 consecutive failures = restart
}
```

**Health Check Methods:**
1. **Ping/Pong:** Send ping message, expect pong response
2. **State Query:** Check actor's internal state validity
3. **External Validation:** Verify connection to external service

### Error Classification

Supervisors classify errors to determine restart behavior:

```typescript
type ErrorSeverity = 'transient' | 'permanent' | 'fatal';

interface ErrorClassification {
  severity: ErrorSeverity;
  shouldRestart: boolean;
  escalate: boolean;
}

// Example classifications
const classifyError = (error: Error): ErrorClassification => {
  // Network errors: transient, should restart
  if (error instanceof NetworkError) {
    return { severity: 'transient', shouldRestart: true, escalate: false };
  }

  // Configuration errors: permanent, escalate
  if (error instanceof ConfigError) {
    return { severity: 'permanent', shouldRestart: false, escalate: true };
  }

  // Memory errors: fatal, escalate immediately
  if (error instanceof OutOfMemoryError) {
    return { severity: 'fatal', shouldRestart: false, escalate: true };
  }

  return { severity: 'transient', shouldRestart: true, escalate: false };
};
```

## Escalation Policy

### Escalation Flow

```
Child Actor Failure
       │
       ▼
 Supervisor Detects ──────── Within restart limits? ───┐
       │                                                │
       │ Yes                                       No   │
       ▼                                                ▼
 Apply Restart Strategy                    Escalate to Parent
       │                                                │
       ▼                                                │
 Child Restarted ◄───────────────────────────────────┐ │
       │                                              │ │
       │                                              │ │
       ▼                                              │ │
 Monitor Child ──────── Restart successful? ─────────┘ │
                                │                       │
                                │ No                    │
                                ▼                       │
                         Increment failure count ──────┘
                                │
                                ▼
                        Exceeded max restarts?
                                │
                                ▼ Yes
                        Escalate to Parent
```

### Escalation Strategies

**1. Supervisor Restart**

Parent restarts the failed supervisor, which recreates all children:

```typescript
// Root supervisor escalation handler
async handleEscalation(childSupervisor: SupervisorActor, error: Error) {
  this.logger.warn('Supervisor failed, restarting supervisor tree', {
    supervisor: childSupervisor.address,
    error
  });

  // Restart supervisor (one-for-one)
  await this.restartChild(childSupervisor);
}
```

**2. System Shutdown**

If root supervisor fails, graceful system shutdown:

```typescript
// Root supervisor failure handler
async handleRootFailure(error: Error) {
  this.logger.fatal('Root supervisor failed, shutting down system', { error });

  // Graceful shutdown
  await this.shutdownAllSupervisors();
  await this.saveSystemCheckpoint();

  // Exit or alert operators
  process.exit(1);
}
```

**3. Manual Intervention**

For permanent errors, alert operators and await manual fix:

```typescript
async handlePermanentError(actor: Actor, error: Error) {
  this.logger.error('Permanent error requires manual intervention', {
    actor: actor.address,
    error
  });

  // Alert operators (webhook, email, etc.)
  await this.alertOperators({
    severity: 'critical',
    actor: actor.address,
    error: error.message,
    suggestion: 'Check configuration and restart actor'
  });

  // Put actor in 'suspended' state
  this.suspendActor(actor);
}
```

## Supervision Protocol

### SupervisorActor Interface

```typescript
interface SupervisorActor extends Actor {
  /**
   * Supervise a child actor with specified strategy
   */
  supervise(
    actor: Actor,
    strategy: RestartStrategy,
    options?: SupervisionOptions
  ): void;

  /**
   * Remove actor from supervision
   */
  unsupervise(actorId: string): void;

  /**
   * Get supervision status for child
   */
  getChildStatus(actorId: string): SupervisionStatus;

  /**
   * Manual restart trigger
   */
  restartChild(actorId: string): Promise<void>;

  /**
   * Handle escalation from child supervisor
   */
  handleEscalation(childId: string, error: Error): Promise<void>;
}
```

### Message Protocol

Supervisors respond to standard actor messages plus supervision messages:

```typescript
// Supervision-specific message types
type SupervisionMessage =
  | { type: 'supervision:register'; actor: Actor; strategy: RestartStrategy }
  | { type: 'supervision:unregister'; actorId: string }
  | { type: 'supervision:status'; actorId?: string }
  | { type: 'supervision:restart'; actorId: string }
  | { type: 'supervision:escalate'; actorId: string; error: Error }
  | { type: 'supervision:health-check'; actorId: string };
```

## Implementation Strategy

### Phase 1: Core Types and Interfaces

1. Define `RestartStrategy` types
2. Define `SupervisorActor` interface
3. Define lifecycle hook types (`SupervisedActor` mixin)
4. Define error classification types

**Location:** `src/messaging/supervision/types.ts`

### Phase 2: Base Supervisor Implementation

1. Implement `BaseSupervisorActor` class
2. Implement restart strategy logic
3. Implement health check system
4. Implement error classification

**Location:** `src/messaging/supervision/supervisor.ts`

### Phase 3: Specialized Supervisors

1. Implement `ChannelSupervisor`
2. Implement `SessionSupervisor`
3. Implement `ToolSupervisor`
4. Integrate with `ActorSystem` as root supervisor

**Location:** `src/messaging/supervision/supervisors/`

### Phase 4: Lifecycle Hooks

1. Create `SupervisedActor` mixin
2. Add `preRestart()` and `postRestart()` hooks
3. Add checkpoint save/restore utilities

**Location:** `src/messaging/supervision/lifecycle.ts`

### Phase 5: Integration

1. Integrate supervisors into ActorSystem
2. Update existing actors to implement lifecycle hooks
3. Add supervision monitoring and metrics
4. Add operator alerts and dashboards

## Examples

### Channel Actor with Supervision

```typescript
import { Actor } from './actor';
import { SupervisedActor } from './supervision/lifecycle';

class WhatsAppChannelActor extends SupervisedActor(Actor) {
  private connection?: WhatsAppConnection;
  private messageQueue: Message[] = [];

  async receive(message: Message): Promise<MessageResponse> {
    switch (message.type) {
      case 'send':
        return await this.sendMessage(message.payload);
      case 'connect':
        return await this.connect();
      case 'disconnect':
        return await this.disconnect();
      default:
        return createErrorResponse(message, new Error('Unknown message type'));
    }
  }

  // Lifecycle: Save state before restart
  async preRestart(error: Error, message?: Message): Promise<void> {
    this.logger.warn('WhatsApp channel restarting', { error });

    // Save pending messages
    await this.saveCheckpoint({
      queuedMessages: this.messageQueue,
      connectionState: this.connection?.getState()
    });

    // Clean disconnect
    await this.disconnect();
    this.closePorts();
  }

  // Lifecycle: Restore state after restart
  async postRestart(checkpoint?: any): Promise<void> {
    if (checkpoint) {
      this.messageQueue = checkpoint.queuedMessages || [];
      this.logger.info('Restored message queue', {
        count: this.messageQueue.length
      });
    }

    // Reconnect
    await this.connect();

    // Resume message processing
    await this.processQueue();
  }

  private async sendMessage(payload: any): Promise<MessageResponse> {
    if (!this.connection) {
      throw new Error('Not connected');
    }
    // ... send message logic
  }

  private async connect(): Promise<MessageResponse> {
    this.connection = await WhatsApp.connect(this.config);
    return createResponse(this.address, { status: 'connected' });
  }

  private async disconnect(): Promise<MessageResponse> {
    await this.connection?.disconnect();
    this.connection = undefined;
    return createResponse(this.address, { status: 'disconnected' });
  }
}
```

### Supervisor Setup

```typescript
import { ActorSystem } from './actor';
import { ChannelSupervisor } from './supervision/supervisors/channel';
import { WhatsAppChannelActor } from './actors/channels/whatsapp';
import { TelegramChannelActor } from './actors/channels/telegram';

// Create actor system with root supervision
const system = new ActorSystem(store, programManager);

// Create channel supervisor
const channelSupervisor = new ChannelSupervisor(
  'channel-supervisor',
  system.router,
  {
    strategy: {
      type: 'one-for-one',
      maxRestarts: 3,
      withinSeconds: 60
    },
    healthCheck: {
      interval: 30000,
      timeout: 5000,
      threshold: 3
    }
  }
);

// Register channel supervisor with root
system.supervise(channelSupervisor, {
  type: 'one-for-one',
  maxRestarts: 5,
  withinSeconds: 300
});

// Create and supervise channel actors
const whatsapp = new WhatsAppChannelActor('whatsapp', system.router, config);
const telegram = new TelegramChannelActor('telegram', system.router, config);

channelSupervisor.supervise(whatsapp, {
  type: 'one-for-one',
  maxRestarts: 3,
  withinSeconds: 60
});

channelSupervisor.supervise(telegram, {
  type: 'one-for-one',
  maxRestarts: 3,
  withinSeconds: 60
});

// Start system
await system.start();
```

## Testing Strategy

### Unit Tests

1. Test each restart strategy in isolation
2. Test lifecycle hooks (preRestart/postRestart)
3. Test error classification logic
4. Test escalation behavior

### Integration Tests

1. Test supervisor tree with multiple levels
2. Test cascading failures and escalation
3. Test checkpoint save/restore
4. Test health check system

### Failure Scenarios

1. Transient failures (should auto-recover)
2. Permanent failures (should escalate)
3. Cascading failures (should isolate)
4. Resource exhaustion (should throttle)
5. Supervisor failures (should restart supervisor tree)

## Production Considerations

### Monitoring

Track supervision metrics:

```typescript
interface SupervisionMetrics {
  totalRestarts: number;
  restartsByActor: Map<string, number>;
  restartsByStrategy: Map<RestartStrategy['type'], number>;
  escalationCount: number;
  averageRestartTime: number;
  healthCheckFailures: number;
}
```

### Alerting

Alert operators on:
- Restart limit exceeded (escalation triggered)
- Repeated failures (flapping actor)
- Supervisor failure (tree restart)
- Permanent errors (manual intervention needed)

### Debugging

Provide supervision debugging tools:
```typescript
// Get supervision tree visualization
system.getSupervisionTree(): SupervisionTreeNode;

// Get restart history for actor
system.getRestartHistory(actorId: string): RestartEvent[];

// Force restart for debugging
system.forceRestart(actorId: string): Promise<void>;
```

## References

- Erlang/OTP Supervision: https://www.erlang.org/doc/design_principles/sup_princ.html
- Akka Supervision: https://doc.akka.io/docs/akka/current/typed/fault-tolerance.html
- Actor Model: https://en.wikipedia.org/wiki/Actor_model

## Changelog

- 2026-02-05: Initial design document
