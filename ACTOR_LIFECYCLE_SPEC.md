# Actor Lifecycle Specification

Complete specification for actor lifecycle management in the Event System.

## Table of Contents

1. [Overview](#overview)
2. [Standard Actor Interface](#standard-actor-interface)
3. [Lifecycle States](#lifecycle-states)
4. [Lifecycle Methods](#lifecycle-methods)
5. [Implementation Patterns](#implementation-patterns)
6. [Actor Examples](#actor-examples)
7. [Best Practices](#best-practices)
8. [Testing Lifecycle](#testing-lifecycle)

---

## Overview

All actors in the Event System follow a standardized lifecycle contract that ensures consistent behavior across the system. This specification defines the required methods, expected return values, and state transitions for all actors.

### Core Principles

1. **Consistent Interface**: All actors implement `start()`, `stop()`, and `getStatus()`
2. **Idempotency**: Lifecycle methods are safe to call multiple times
3. **Graceful Shutdown**: Actors clean up resources properly on stop
4. **Status Transparency**: Actors expose their current state via `getStatus()`
5. **Error Handling**: Lifecycle methods return success/error objects

---

## Standard Actor Interface

Every actor MUST implement the following three methods:

```javascript
class Actor {
  /**
   * Start the actor - initialize resources and begin operation
   * @returns {Promise<Object>} Result object with success status
   */
  async start() {
    // Implementation
  }

  /**
   * Stop the actor - cleanup resources and halt operation
   * @returns {Promise<Object>} Result object with success status
   */
  async stop() {
    // Implementation
  }

  /**
   * Get current actor status
   * @returns {Object} Status object with current state
   */
  getStatus() {
    // Implementation
  }
}
```

### Return Value Format

#### start() and stop() Return Value

```javascript
{
  success: boolean,        // true if operation succeeded
  message?: string,        // Optional success message
  error?: string          // Error message if success is false
}
```

#### getStatus() Return Value

```javascript
{
  isRunning: boolean,     // Current running state
  // Additional actor-specific status fields
}
```

---

## Lifecycle States

Actors have two primary states:

### 1. STOPPED (isRunning = false)

- Actor is not operational
- No resources are allocated
- Safe to start the actor

### 2. RUNNING (isRunning = true)

- Actor is operational
- Resources are allocated
- Processing requests/messages
- Safe to stop the actor

### State Diagram

```
┌─────────┐
│ STOPPED │
└────┬────┘
     │
     │ start()
     │ success
     ▼
┌─────────┐
│ RUNNING │
└────┬────┘
     │
     │ stop()
     │ success
     ▼
┌─────────┐
│ STOPPED │
└─────────┘
```

### Error Handling

If `start()` or `stop()` fails, the actor SHOULD remain in its current state and return an error result.

---

## Lifecycle Methods

### start()

**Purpose**: Initialize the actor and prepare it for operation

**Responsibilities**:
- Allocate resources (file handles, connections, etc.)
- Initialize internal state
- Validate configuration
- Set `isRunning = true` on success

**Return Values**:
```javascript
// Success
{
  success: true,
  message: "ActorName started successfully"
}

// Error
{
  success: false,
  error: "Detailed error message"
}
```

**Idempotency**:
- If actor is already running, return error:
  ```javascript
  {
    success: false,
    error: "ActorName is already running"
  }
  ```

**Example**:
```javascript
async start() {
  if (this.isRunning) {
    return {
      success: false,
      error: 'Actor is already running'
    };
  }

  try {
    // Initialize resources
    await this.initializeResources();

    this.isRunning = true;
    return {
      success: true,
      message: 'Actor started successfully'
    };
  } catch (error) {
    return {
      success: false,
      error: `Failed to start: ${error.message}`
    };
  }
}
```

---

### stop()

**Purpose**: Gracefully shutdown the actor and release resources

**Responsibilities**:
- Release resources (close files, connections, etc.)
- Complete pending operations
- Cleanup internal state
- Set `isRunning = false` on success

**Return Values**:
```javascript
// Success
{
  success: true,
  message: "ActorName stopped successfully"
}

// Error
{
  success: false,
  error: "Detailed error message"
}
```

**Idempotency**:
- If actor is already stopped, return success:
  ```javascript
  {
    success: true,
    message: "ActorName was not running"
  }
  ```

**Example**:
```javascript
async stop() {
  if (!this.isRunning) {
    return {
      success: true,
      message: 'Actor was not running'
    };
  }

  try {
    // Cleanup resources
    await this.cleanupResources();

    this.isRunning = false;
    return {
      success: true,
      message: 'Actor stopped successfully'
    };
  } catch (error) {
    return {
      success: false,
      error: `Failed to stop: ${error.message}`
    };
  }
}
```

---

### getStatus()

**Purpose**: Return current actor state and statistics

**Responsibilities**:
- Return `isRunning` boolean
- Include actor-specific metrics
- Provide operational visibility

**Return Value**:
```javascript
{
  isRunning: boolean,          // Required
  // Actor-specific fields
}
```

**Example**:
```javascript
getStatus() {
  return {
    isRunning: this.isRunning,
    patternCount: this.patterns.size,
    lastUpdate: this.lastUpdate
  };
}
```

---

## Implementation Patterns

### Pattern 1: Class-Based Actor (Recommended for Stateful Actors)

```javascript
export class MyActor {
  constructor(config = {}) {
    this.config = config;
    this.isRunning = false;
    // Other state
  }

  async start() {
    if (this.isRunning) {
      return { success: false, error: 'Already running' };
    }

    try {
      // Initialize
      this.isRunning = true;
      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  }

  async stop() {
    if (!this.isRunning) {
      return { success: true, message: 'Was not running' };
    }

    try {
      // Cleanup
      this.isRunning = false;
      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  }

  getStatus() {
    return {
      isRunning: this.isRunning,
      // Additional fields
    };
  }
}
```

---

### Pattern 2: Factory Function Actor (Recommended for Simple Actors)

```javascript
export function createMyActor(config = {}) {
  let isRunning = false;
  const state = {};

  return {
    async start() {
      if (isRunning) {
        return { success: false, error: 'Already running' };
      }

      try {
        // Initialize
        isRunning = true;
        return { success: true };
      } catch (error) {
        return { success: false, error: error.message };
      }
    },

    async stop() {
      if (!isRunning) {
        return { success: true, message: 'Was not running' };
      }

      try {
        // Cleanup
        isRunning = false;
        return { success: true };
      } catch (error) {
        return { success: false, error: error.message };
      }
    },

    getStatus() {
      return {
        isRunning,
        // Additional fields
      };
    }

    // Other methods...
  };
}
```

---

## Actor Examples

### EventLogActor (File I/O Resource Management)

```javascript
export class EventLogActor {
  constructor(config = {}) {
    this.logPath = config.eventLog?.file || 'events.jsonl';
    this.writeStream = null;
    this.isInitialized = false;
    this.eventCount = 0;
  }

  async start() {
    try {
      // Ensure directory exists
      const logDir = dirname(this.logPath);
      await fs.mkdir(logDir, { recursive: true });

      // Count existing events
      this.eventCount = await this._countEvents();

      // Create append stream
      this.writeStream = createWriteStream(this.logPath, { flags: 'a' });

      this.isInitialized = true;
      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  }

  async stop() {
    return new Promise((resolve) => {
      if (this.writeStream) {
        this.writeStream.end(() => {
          this.isInitialized = false;
          resolve({ success: true });
        });
      } else {
        resolve({ success: true });
      }
    });
  }

  getStatus() {
    return {
      isRunning: this.isInitialized,
      eventCount: this.eventCount,
      logPath: this.logPath
    };
  }
}
```

---

### HTTPServerActor (Network Resource Management)

```javascript
export class HTTPServerActor {
  constructor(config = {}) {
    this.port = config.http?.port || 3000;
    this.server = null;
    this.isRunning = false;
    this.startTime = null;
  }

  async start() {
    if (this.isRunning) {
      return { success: false, error: 'Server is already running' };
    }

    try {
      this.server = Bun.serve({
        port: this.port,
        fetch: async (request) => this.handleRequest(request)
      });

      this.isRunning = true;
      this.startTime = new Date().toISOString();

      return {
        success: true,
        url: `http://localhost:${this.server.port}`,
        port: this.server.port
      };
    } catch (error) {
      return { success: false, error: error.message };
    }
  }

  async stop() {
    if (!this.isRunning || !this.server) {
      return { success: true };
    }

    try {
      this.server.stop();
      this.isRunning = false;
      return { success: true };
    } catch (error) {
      return { success: false, error: error.message };
    }
  }

  getStatus() {
    return {
      isRunning: this.isRunning,
      port: this.port,
      url: this.isRunning ? `http://localhost:${this.port}` : null,
      startTime: this.startTime,
      uptime: this.startTime ? Date.now() - new Date(this.startTime).getTime() : 0
    };
  }
}
```

---

### PatternMatcherActor (Stateless Actor)

```javascript
export function createPatternMatcher() {
  const patterns = new Map();
  let isRunning = false;

  return {
    async start() {
      if (isRunning) {
        return { success: false, error: 'Already running' };
      }

      isRunning = true;
      return { success: true, message: 'PatternMatcherActor started' };
    },

    async stop() {
      if (!isRunning) {
        return { success: true, message: 'Was not running' };
      }

      // No cleanup needed for in-memory patterns
      isRunning = false;
      return { success: true, message: 'PatternMatcherActor stopped' };
    },

    getStatus() {
      return {
        isRunning,
        patternCount: patterns.size
      };
    },

    // Other pattern methods...
  };
}
```

---

### FunctionRegistryActor (Stateless Actor with Registry)

```javascript
export class FunctionRegistryActor {
  constructor() {
    this.functions = new Map();
    this.isRunning = false;
  }

  async start() {
    if (this.isRunning) {
      return { success: false, error: 'Already running' };
    }

    this.isRunning = true;
    return { success: true, message: 'FunctionRegistryActor started' };
  }

  async stop() {
    if (!this.isRunning) {
      return { success: true, message: 'Was not running' };
    }

    // No cleanup needed for in-memory registry
    this.isRunning = false;
    return { success: true, message: 'FunctionRegistryActor stopped' };
  }

  getStatus() {
    return {
      isRunning: this.isRunning,
      functionCount: this.functions.size,
      functions: Array.from(this.functions.keys())
    };
  }
}
```

---

### FunctionExecutorActor (Execution Actor)

```javascript
export class FunctionExecutorActor {
  constructor(config = {}) {
    this.config = config;
    this.emitCallback = null;
    this.isRunning = false;
  }

  async start() {
    if (this.isRunning) {
      return { success: false, error: 'Already running' };
    }

    this.isRunning = true;
    return { success: true, message: 'FunctionExecutorActor started' };
  }

  async stop() {
    if (!this.isRunning) {
      return { success: true, message: 'Was not running' };
    }

    // No cleanup needed for stateless executor
    this.isRunning = false;
    return { success: true, message: 'FunctionExecutorActor stopped' };
  }

  getStatus() {
    return {
      isRunning: this.isRunning,
      hasEmitCallback: this.emitCallback !== null
    };
  }
}
```

---

## Best Practices

### 1. Idempotency

Always check if the actor is already in the desired state:

```javascript
async start() {
  if (this.isRunning) {
    return { success: false, error: 'Already running' };
  }
  // ...
}

async stop() {
  if (!this.isRunning) {
    return { success: true, message: 'Was not running' };
  }
  // ...
}
```

### 2. Resource Cleanup

Always cleanup resources in `stop()`:

```javascript
async stop() {
  if (!this.isRunning) {
    return { success: true };
  }

  try {
    // Close file handles
    if (this.writeStream) {
      await new Promise(resolve => this.writeStream.end(resolve));
    }

    // Close network connections
    if (this.server) {
      this.server.stop();
    }

    // Clear timers
    if (this.timer) {
      clearInterval(this.timer);
    }

    this.isRunning = false;
    return { success: true };
  } catch (error) {
    return { success: false, error: error.message };
  }
}
```

### 3. Error Handling

Wrap lifecycle operations in try-catch:

```javascript
async start() {
  try {
    // Initialization logic
    this.isRunning = true;
    return { success: true };
  } catch (error) {
    // Log error
    console.error(`Failed to start ${this.constructor.name}:`, error);

    // Return error result
    return {
      success: false,
      error: `Failed to start: ${error.message}`
    };
  }
}
```

### 4. Status Visibility

Include useful metrics in `getStatus()`:

```javascript
getStatus() {
  return {
    isRunning: this.isRunning,
    // Counts
    itemCount: this.items.size,
    processedCount: this.processed,
    errorCount: this.errors,
    // Timestamps
    startTime: this.startTime,
    lastActivity: this.lastActivity,
    // Uptime
    uptime: this.startTime ? Date.now() - this.startTime : 0
  };
}
```

### 5. Async Safety

Use async/await properly:

```javascript
async start() {
  // Wait for async operations
  await this.loadConfig();
  await this.connectDatabase();
  await this.initializeCache();

  this.isRunning = true;
  return { success: true };
}
```

### 6. Graceful Degradation

Handle partial failures gracefully:

```javascript
async stop() {
  const errors = [];

  // Try to cleanup each resource independently
  try {
    await this.closeDatabase();
  } catch (error) {
    errors.push(`Database: ${error.message}`);
  }

  try {
    await this.closeCache();
  } catch (error) {
    errors.push(`Cache: ${error.message}`);
  }

  this.isRunning = false;

  if (errors.length > 0) {
    return {
      success: false,
      error: `Partial failure: ${errors.join(', ')}`
    };
  }

  return { success: true };
}
```

---

## Testing Lifecycle

### Test Template

```javascript
import { describe, it, expect, beforeEach } from 'bun:test';
import { MyActor } from './my-actor.js';

describe('MyActor Lifecycle', () => {
  let actor;

  beforeEach(() => {
    actor = new MyActor();
  });

  it('should start successfully', async () => {
    const result = await actor.start();
    expect(result.success).toBe(true);

    const status = actor.getStatus();
    expect(status.isRunning).toBe(true);
  });

  it('should not start if already running', async () => {
    await actor.start();
    const result = await actor.start();

    expect(result.success).toBe(false);
    expect(result.error).toContain('already running');
  });

  it('should stop successfully', async () => {
    await actor.start();
    const result = await actor.stop();

    expect(result.success).toBe(true);

    const status = actor.getStatus();
    expect(status.isRunning).toBe(false);
  });

  it('should handle stop when not running', async () => {
    const result = await actor.stop();
    expect(result.success).toBe(true);
  });

  it('should return correct status', async () => {
    let status = actor.getStatus();
    expect(status.isRunning).toBe(false);

    await actor.start();
    status = actor.getStatus();
    expect(status.isRunning).toBe(true);

    await actor.stop();
    status = actor.getStatus();
    expect(status.isRunning).toBe(false);
  });

  it('should cleanup resources on stop', async () => {
    await actor.start();
    // Verify resources are allocated

    await actor.stop();
    // Verify resources are released
  });
});
```

---

## Summary

The Actor Lifecycle Specification ensures:

1. **Consistent Interface**: All actors implement `start()`, `stop()`, and `getStatus()`
2. **Predictable Behavior**: Idempotent operations with clear return values
3. **Resource Management**: Proper cleanup on shutdown
4. **Operational Visibility**: Status reporting for monitoring
5. **Error Handling**: Graceful error reporting and recovery

All actors in the Event System MUST follow this specification to ensure system reliability and maintainability.
