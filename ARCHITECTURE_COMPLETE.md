# Event System Architecture

Complete architecture documentation for the Actor-based Event System with Universal Actor Protocol (UAP).

## Table of Contents

1. [System Overview](#system-overview)
2. [Actor Architecture](#actor-architecture)
3. [Actor Lifecycle Interface](#actor-lifecycle-interface)
4. [Universal Actor Protocol (UAP)](#universal-actor-protocol)
5. [Message Flow Diagrams](#message-flow-diagrams)
6. [Protocol Specifications](#protocol-specifications)
7. [Loop Prevention System](#loop-prevention-system)
8. [Example Sequences](#example-sequences)
9. [API Reference](#api-reference)

---

## System Overview

The Event System is a reactive, actor-based architecture that processes events through a chain of specialized actors. Each actor has a single responsibility and communicates using the Universal Actor Protocol (UAP).

### Core Principles

1. **Actor Model**: Each component is an isolated actor with a single responsibility
2. **Message Passing**: All communication happens via structured UAP messages
3. **Event Sourcing**: All events are persisted to an append-only log (JSONL)
4. **Reactive Processing**: Events trigger pattern matching and function execution
5. **Loop Prevention**: 4-layer system prevents infinite event loops

### System Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                         Event System                             │
│                                                                   │
│  ┌──────────────┐     ┌──────────────┐     ┌──────────────┐    │
│  │  CLI Client  │     │ HTTP Client  │     │ Code/Agents  │    │
│  └──────┬───────┘     └──────┬───────┘     └──────┬───────┘    │
│         │                    │                     │             │
│         └────────────────────┼─────────────────────┘             │
│                              ▼                                   │
│                    ┌──────────────────┐                          │
│                    │  DaemonActor     │                          │
│                    │  (Orchestrator)  │                          │
│                    └────────┬─────────┘                          │
│                             │                                    │
│         ┌───────────────────┼───────────────────┐               │
│         ▼                   ▼                   ▼               │
│  ┌──────────────┐    ┌──────────────┐   ┌──────────────┐       │
│  │ HTTPServer   │    │  EventLog    │   │FunctionReg.  │       │
│  │   Actor      │    │   Actor      │   │   Actor      │       │
│  └──────┬───────┘    └──────┬───────┘   └──────────────┘       │
│         │                   │                                    │
│         │         ┌─────────┴─────────┐                         │
│         │         ▼                   ▼                         │
│         │  ┌──────────────┐    ┌──────────────┐                │
│         │  │PatternMatcher│    │FunctionExec. │                │
│         │  │   Actor      │    │   Actor      │                │
│         │  └──────────────┘    └──────────────┘                │
│         │                                                        │
│         └────────────────────────────────────────────────────►  │
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐    │
│  │           Loop Prevention Coordinator                   │    │
│  │  ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐                  │    │
│  │  │Depth │ │Finger│ │Ancestry│Circuit│                  │    │
│  │  │Count.│ │print │ │ Chain  │Breaker│                  │    │
│  │  └──────┘ └──────┘ └──────┘ └──────┘                  │    │
│  └────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Storage: events.jsonl (append-only event log)                  │
└──────────────────────────────────────────────────────────────────┘
```

---

## Actor Architecture

### 6 Core Actors

#### 1. DaemonActor (Orchestrator)

**Responsibility**: Lifecycle management and actor coordination

**Capabilities**:
- Start/stop daemon process
- Load configuration from config.json
- Spawn and manage child actors
- Handle graceful shutdown (SIGINT/SIGTERM)
- Process lifecycle states: STOPPED, STARTING, RUNNING, STOPPING, ERROR

**Configuration**:
```json
{
  "daemon": {
    "port": 0,
    "host": "127.0.0.1",
    "logFile": "daemon.log"
  }
}
```

**States**:
- `STOPPED`: Not running
- `STARTING`: Initializing actors
- `RUNNING`: Active and processing
- `STOPPING`: Graceful shutdown in progress
- `ERROR`: Fatal error occurred

---

#### 2. EventLogActor

**Responsibility**: Append-only event storage with replay capability

**Capabilities**:
- Persist events to JSONL file (one event per line)
- Query events with filters (type, limit, offset, reverse)
- Checkpoint for replay positions
- Replay events from checkpoint
- ULID generation for event IDs

**Protocols**: `event.v1`

**Actions**:
- `append`: Add new event to log
- `query`: Search events with filters
- `checkpoint`: Mark current position
- `replay`: Replay from checkpoint

**Event Structure**:
```json
{
  "id": "evt_01KEM4PMQGZC8B4W1EWF8NMDBN",
  "timestamp": "2026-01-10T14:24:27.376Z",
  "type": "user.login",
  "data": {
    "userId": "alice"
  },
  "metadata": {
    "source": "cli",
    "triggeredBy": "evt_01KEM4PM...",
    "depth": 0,
    "fingerprint": "hash123"
  }
}
```

**Storage Format** (JSONL):
```
{"id":"evt_001","timestamp":"...","type":"user.login",...}
{"id":"evt_002","timestamp":"...","type":"user.logout",...}
```

---

#### 3. HTTPServerActor

**Responsibility**: REST API server for event system operations

**Capabilities**:
- Serve HTTP endpoints using Bun.serve
- Parse JSON request bodies
- Route requests to appropriate actors
- Return proper HTTP status codes
- Handle CORS for browser access

**Lifecycle**:
- `start()`: Start Bun HTTP server, bind to port
- `stop()`: Stop HTTP server gracefully
- `getStatus()`: Returns `{ isRunning, port, url, startTime, uptime }`

**Protocols**: `http.v1`

**Endpoints**:

| Method | Path | Description |
|--------|------|-------------|
| GET | /health | Health check with uptime |
| POST | /events | Emit new event |
| GET | /events | Query events (?type=, ?limit=, ?offset=, ?reverse=) |
| GET | /functions | List registered functions (?type=) |
| GET | /patterns | List registered patterns (?sortByPriority=) |

**Example Requests**:
```bash
# Health check
curl http://localhost:3000/health

# Emit event
curl -X POST http://localhost:3000/events \
  -H "Content-Type: application/json" \
  -d '{"type":"user.login","data":{"userId":"alice"}}'

# Query events
curl "http://localhost:3000/events?type=user.login&limit=10"
```

---

#### 4. FunctionRegistryActor

**Responsibility**: Track available functions and their metadata

**Capabilities**:
- Register functions (code or agent types)
- Unregister functions by ID
- List functions with filters
- Auto-discover functions from directory
- Get function metadata by ID

**Lifecycle**:
- `start()`: Initialize registry (set running flag)
- `stop()`: Stop registry gracefully (set stopped flag)
- `getStatus()`: Returns `{ isRunning, functionCount, functions }`

**Protocols**: `registry.v1`

**Actions**:
- `register`: Add function to registry
- `unregister`: Remove function from registry
- `list`: List all functions
- `get`: Get specific function metadata
- `scan`: Auto-discover functions in directory

**Function Types**:
1. **Code Functions** (`.js` files): Direct JavaScript module execution
2. **Agent Functions** (`.agent.js` files): Claude CLI subprocess execution

**Function Metadata**:
```json
{
  "functionId": "send-email",
  "type": "code",
  "path": "/path/to/send-email.js",
  "agentCommand": null,
  "maxStackDepth": null,
  "metadata": {
    "name": "Send Email",
    "description": "Sends email notifications",
    "author": "system"
  },
  "registeredAt": "2026-01-10T14:00:00Z"
}
```

---

#### 5. PatternMatcherActor

**Responsibility**: Evaluate events against JavaScript predicates

**Capabilities**:
- Register patterns with predicates
- Match events against all patterns
- Priority-based evaluation (higher priority first)
- Safe predicate execution with error handling
- Return all matching patterns

**Lifecycle**:
- `start()`: Initialize pattern matcher (set running flag)
- `stop()`: Stop pattern matcher gracefully (set stopped flag)
- `getStatus()`: Returns `{ isRunning, patternCount }`

**Pattern Structure**:
```json
{
  "id": "user-login",
  "predicate": "event.type === 'user.login' && event.data.userId",
  "priority": 10,
  "metadata": {
    "description": "Matches valid user login events",
    "actions": ["log-activity", "send-notification"]
  }
}
```

**Predicate Evaluation**:
- Predicates are JavaScript expressions
- Have access to `event` object
- Return truthy/falsy value
- Executed in order of priority (descending)

**Match Result**:
```json
{
  "success": true,
  "matches": [
    {
      "patternId": "user-login",
      "priority": 10,
      "metadata": {...}
    }
  ],
  "errors": [],
  "eventMatched": {...}
}
```

---

#### 6. FunctionExecutorActor

**Responsibility**: Execute individual function invocations

**Capabilities**:
- Dynamic ES module loading (Bun)
- Context injection (emit, logger, config)
- Return value capture
- Graceful error handling
- Event emission for success/error
- Agent execution via subprocess

**Lifecycle**:
- `start()`: Initialize executor (set running flag)
- `stop()`: Stop executor gracefully (set stopped flag)
- `getStatus()`: Returns `{ isRunning, hasEmitCallback }`

**Protocols**: `function.v1`

**Actions**:
- `execute`: Run function with event
- `complete`: Function finished successfully
- `error`: Function execution failed

**Execution Context** (injected):
```javascript
{
  emit: async (event) => {...},  // Emit new events
  logger: {                       // Structured logging
    info(...args),
    warn(...args),
    error(...args),
    debug(...args)
  },
  config: {...}                   // Configuration object
}
```

**Code Function Example**:
```javascript
// functions/echo.js
export default async function echo(event, context) {
  context.logger.info('Echo received:', event.type);

  // Emit response event
  await context.emit({
    type: 'echo.response',
    data: {
      original: event.data,
      timestamp: new Date().toISOString()
    }
  });

  return { status: 'echoed' };
}
```

**Agent Function Example**:
```javascript
// functions/analyze-error.agent.js
// Metadata for agent functions
export const metadata = {
  name: 'Analyze Error',
  description: 'Uses Claude to analyze error events',
  agentCommand: 'claude'
};
```

---

## Actor Lifecycle Interface

All actors in the Event System implement a standardized lifecycle interface that ensures consistent behavior across the system. This interface provides uniform methods for starting, stopping, and monitoring actors.

### Standard Lifecycle Methods

Every actor MUST implement these three methods:

```javascript
class Actor {
  /**
   * Start the actor - initialize resources and begin operation
   * @returns {Promise<Object>} Result: { success: boolean, message?: string, error?: string }
   */
  async start() { }

  /**
   * Stop the actor - cleanup resources and halt operation
   * @returns {Promise<Object>} Result: { success: boolean, message?: string, error?: string }
   */
  async stop() { }

  /**
   * Get current actor status
   * @returns {Object} Status: { isRunning: boolean, ...actorSpecificFields }
   */
  getStatus() { }
}
```

### Lifecycle States

Actors have two primary states:

- **STOPPED** (`isRunning = false`): Actor is not operational, no resources allocated
- **RUNNING** (`isRunning = true`): Actor is operational, processing requests

### State Transitions

```
┌─────────┐
│ STOPPED │
└────┬────┘
     │ start()
     ▼
┌─────────┐
│ RUNNING │
└────┬────┘
     │ stop()
     ▼
┌─────────┐
│ STOPPED │
└─────────┘
```

### start() Method

**Purpose**: Initialize the actor and prepare it for operation

**Return Value**:
```javascript
// Success
{ success: true, message: "Actor started successfully" }

// Error
{ success: false, error: "Detailed error message" }
```

**Idempotency**: If already running, returns error
```javascript
{ success: false, error: "Actor is already running" }
```

**Example**:
```javascript
async start() {
  if (this.isRunning) {
    return { success: false, error: 'Already running' };
  }

  try {
    // Initialize resources
    await this.initializeResources();
    this.isRunning = true;
    return { success: true, message: 'Actor started' };
  } catch (error) {
    return { success: false, error: error.message };
  }
}
```

### stop() Method

**Purpose**: Gracefully shutdown the actor and release resources

**Return Value**:
```javascript
// Success
{ success: true, message: "Actor stopped successfully" }

// Error
{ success: false, error: "Detailed error message" }
```

**Idempotency**: If already stopped, returns success
```javascript
{ success: true, message: "Actor was not running" }
```

**Example**:
```javascript
async stop() {
  if (!this.isRunning) {
    return { success: true, message: 'Was not running' };
  }

  try {
    // Cleanup resources
    await this.cleanupResources();
    this.isRunning = false;
    return { success: true, message: 'Actor stopped' };
  } catch (error) {
    return { success: false, error: error.message };
  }
}
```

### getStatus() Method

**Purpose**: Return current actor state and statistics

**Return Value**:
```javascript
{
  isRunning: boolean,      // Required
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

### Actor Lifecycle Implementation Summary

| Actor | start() Behavior | stop() Behavior | Status Fields |
|-------|-----------------|-----------------|---------------|
| **EventLogActor** | Create write stream, count events | Close write stream | `isRunning`, `eventCount`, `logPath` |
| **HTTPServerActor** | Start Bun.serve, bind port | Stop server | `isRunning`, `port`, `url`, `uptime` |
| **PatternMatcherActor** | Set running flag | Set stopped flag | `isRunning`, `patternCount` |
| **FunctionRegistryActor** | Set running flag | Set stopped flag | `isRunning`, `functionCount`, `functions` |
| **FunctionExecutorActor** | Set running flag | Set stopped flag | `isRunning`, `hasEmitCallback` |

### Best Practices

1. **Idempotency**: Always check current state before transitioning
2. **Resource Cleanup**: Release all resources in `stop()`
3. **Error Handling**: Wrap operations in try-catch
4. **Status Visibility**: Include useful metrics in `getStatus()`
5. **Async Safety**: Use async/await properly

For complete lifecycle specification, see [ACTOR_LIFECYCLE_SPEC.md](./ACTOR_LIFECYCLE_SPEC.md).

---

## Universal Actor Protocol (UAP)

All messages between actors follow UAP structure:

### Message Structure

```json
{
  "protocol": "event.v1",
  "action": "append",
  "data": {...},
  "timestamp": "2026-01-10T14:00:00Z"
}
```

### Protocol Namespaces

| Protocol | Purpose |
|----------|---------|
| `event.v1` | Event log operations |
| `function.v1` | Function execution |
| `registry.v1` | Function registry management |
| `http.v1` | HTTP request/response |

### Standard Actions

| Action | Description |
|--------|-------------|
| `append` | Add new data |
| `query` | Search/retrieve data |
| `register` | Register new entity |
| `unregister` | Remove entity |
| `list` | List entities |
| `execute` | Execute operation |
| `complete` | Operation succeeded |
| `error` | Operation failed |
| `checkpoint` | Mark position |

### Message Creation

```javascript
import { createMessage, PROTOCOLS, ACTIONS } from './protocol.js';

const message = createMessage(
  PROTOCOLS.EVENT,
  ACTIONS.APPEND,
  { type: 'user.login', data: { userId: 'alice' } }
);
```

### Message Validation

```javascript
import { validateMessage } from './protocol.js';

const validation = validateMessage(message);
if (!validation.valid) {
  console.error('Invalid message:', validation.error);
}
```

---

## Message Flow Diagrams

### 1. Event Emission Flow (CLI)

```
┌─────┐                                                    ┌──────────┐
│ CLI │                                                    │EventLog  │
│     │                                                    │ Actor    │
└──┬──┘                                                    └────┬─────┘
   │                                                            │
   │ 1. event-system emit '{"type":"test"}'                    │
   │────────────────────────────────────────────►              │
   │                                                            │
   │ 2. createMessage(EVENT, APPEND, eventData)                │
   │─────────────────────────────────────────────────────────► │
   │                                                            │
   │                                  3. Append to events.jsonl│
   │                                                    ◄───────┤
   │                                                            │
   │ 4. Response: {success: true, eventId: "evt_..."}          │
   │ ◄──────────────────────────────────────────────────────── │
   │                                                            │
   │ 5. Display: "✅ Event emitted: evt_..."                   │
   │◄───                                                        │
   │                                                            │
```

### 2. Event Emission Flow (HTTP)

```
┌──────┐     ┌──────────┐     ┌──────────┐     ┌──────────┐
│Client│     │HTTPServer│     │EventLog  │     │ Loop     │
│      │     │ Actor    │     │ Actor    │     │Prevention│
└───┬──┘     └────┬─────┘     └────┬─────┘     └────┬─────┘
    │             │                │                 │
    │ POST /events                 │                 │
    │  {"type":"user.login"}       │                 │
    │─────────────►                │                 │
    │             │                │                 │
    │             │ Parse JSON     │                 │
    │             │────────        │                 │
    │             │       │        │                 │
    │             │◄──────         │                 │
    │             │                │                 │
    │             │ Check loop prevention            │
    │             │─────────────────────────────────►│
    │             │                │                 │
    │             │                │  Check depth,   │
    │             │                │  fingerprint,   │
    │             │                │  ancestry,      │
    │             │                │  circuit breaker│
    │             │                │         ◄───────┤
    │             │                │                 │
    │             │ {allowed: true}                  │
    │             │◄─────────────────────────────────│
    │             │                │                 │
    │             │ createMessage(EVENT, APPEND)     │
    │             │────────────────►                 │
    │             │                │                 │
    │             │           Append to JSONL        │
    │             │                │─────            │
    │             │                │    │            │
    │             │                │◄───             │
    │             │                │                 │
    │             │ {success: true, eventId}         │
    │             │◄────────────────                 │
    │             │                │                 │
    │ 201 Created │                │                 │
    │ {success:true, eventId}      │                 │
    │◄─────────────                │                 │
    │             │                │                 │
```

### 3. Pattern Matching and Function Execution Flow

```
┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐
│EventLog  │  │Pattern   │  │Function  │  │Function  │  │EventLog  │
│ Actor    │  │Matcher   │  │Registry  │  │Executor  │  │ Actor    │
└────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘
     │             │              │              │              │
     │ New event   │              │              │              │
     │ appended    │              │              │              │
     │─────────────►              │              │              │
     │             │              │              │              │
     │             │ Match event  │              │              │
     │             │────────      │              │              │
     │             │       │      │              │              │
     │             │◄──────       │              │              │
     │             │              │              │              │
     │ Matches: ["send-email"]   │              │              │
     │◄─────────────              │              │              │
     │             │              │              │              │
     │             │         Get function metadata              │
     │             │──────────────────────────────►             │
     │             │              │              │              │
     │             │         {type: "code", path: "/..."}       │
     │             │◄──────────────────────────────             │
     │             │              │              │              │
     │             │              │ Execute function            │
     │             │              │──────────────────────────►  │
     │             │              │              │              │
     │             │              │              │ Load module  │
     │             │              │              │─────         │
     │             │              │              │    │         │
     │             │              │              │◄───          │
     │             │              │              │              │
     │             │              │              │ Invoke fn()  │
     │             │              │              │─────         │
     │             │              │              │    │         │
     │             │              │              │◄───          │
     │             │              │              │              │
     │             │              │              │ Emit new event
     │             │              │              │──────────────────►
     │             │              │              │              │
     │             │              │              │         Append│
     │             │              │              │              │─────
     │             │              │              │              │    │
     │             │              │              │              │◄───
     │             │              │              │              │
     │             │              │  {success: true, result}    │
     │             │              │◄──────────────────────────  │
     │             │              │              │              │
```

### 4. Loop Prevention Flow

```
┌──────────┐                    ┌─────────────────────────┐
│ Event    │                    │ Loop Prevention         │
│ Incoming │                    │ Coordinator             │
└────┬─────┘                    └────┬────────────────────┘
     │                               │
     │ Check if event allowed        │
     │──────────────────────────────►│
     │                               │
     │                          ┌────┴────┐
     │                          │ 1. Depth│
     │                          │ Counter │
     │                          └────┬────┘
     │                               │
     │                          Check depth < max (50)
     │                               │
     │                          ✓ Pass
     │                               │
     │                          ┌────┴────────┐
     │                          │ 2. Finger-  │
     │                          │    printing │
     │                          └────┬────────┘
     │                               │
     │                          Generate fingerprint
     │                          Check if seen before
     │                               │
     │                          ✓ Pass (new event)
     │                               │
     │                          ┌────┴────────┐
     │                          │ 3. Ancestry │
     │                          │    Chain    │
     │                          └────┬────────┘
     │                               │
     │                          Check parent chain
     │                          for cycles
     │                               │
     │                          ✓ Pass (no cycle)
     │                               │
     │                          ┌────┴────────┐
     │                          │ 4. Circuit  │
     │                          │    Breaker  │
     │                          └────┬────────┘
     │                               │
     │                          Check function rate
     │                          < threshold
     │                               │
     │                          ✓ Pass
     │                               │
     │ {allowed: true}               │
     │◄──────────────────────────────│
     │                               │
```

---

## Protocol Specifications

### event.v1 Protocol

**Purpose**: Event log operations

**Actions**:

#### append
```javascript
// Request
{
  protocol: "event.v1",
  action: "append",
  data: {
    type: "user.login",
    data: { userId: "alice" },
    metadata: {
      source: "cli",
      triggeredBy: "evt_parent_id",
      depth: 1
    }
  }
}

// Response
{
  success: true,
  eventId: "evt_01KEM4PMQGZC8B4W1EWF8NMDBN",
  eventCount: 42
}
```

#### query
```javascript
// Request
{
  protocol: "event.v1",
  action: "query",
  data: {
    filter: (event) => event.type === "user.login",
    limit: 100,
    offset: 0,
    reverse: false
  }
}

// Response
{
  success: true,
  events: [...],
  count: 10,
  total: 42
}
```

#### checkpoint
```javascript
// Request
{
  protocol: "event.v1",
  action: "checkpoint",
  data: {}
}

// Response
{
  success: true,
  checkpoint: 42,
  eventCount: 42
}
```

---

### function.v1 Protocol

**Purpose**: Function execution

**Actions**:

#### execute
```javascript
// Request
{
  protocol: "function.v1",
  action: "execute",
  data: {
    functionId: "send-email",
    functionPath: "/path/to/send-email.js",
    functionType: "code",
    event: {...},
    config: {}
  }
}

// Response (success)
{
  protocol: "function.v1",
  action: "complete",
  data: {
    functionId: "send-email",
    result: {...},
    executionTime: 123,
    success: true
  }
}

// Response (error)
{
  protocol: "function.v1",
  action: "error",
  data: {
    functionId: "send-email",
    error: "Module not found",
    phase: "import"
  }
}
```

---

### registry.v1 Protocol

**Purpose**: Function registry management

**Actions**:

#### register
```javascript
// Request
{
  protocol: "registry.v1",
  action: "register",
  data: {
    functionId: "send-email",
    metadata: {
      type: "code",
      path: "/path/to/send-email.js",
      metadata: {
        name: "Send Email",
        description: "Sends notifications"
      }
    }
  }
}

// Response
{
  protocol: "registry.v1",
  action: "register",
  data: {
    functionId: "send-email",
    success: true,
    message: "Function 'send-email' registered successfully"
  }
}
```

#### list
```javascript
// Request
{
  protocol: "registry.v1",
  action: "list",
  data: {
    filters: { type: "code" }
  }
}

// Response
{
  protocol: "registry.v1",
  action: "list",
  data: {
    functions: [...],
    count: 5,
    filters: { type: "code" }
  }
}
```

---

## Loop Prevention System

4-layer protection against infinite event loops:

### Layer 1: Depth Counter

**Purpose**: Limit event chain depth

**Mechanism**:
- Each event has `metadata.depth` field
- Child events have `depth = parent.depth + 1`
- Reject events with `depth > maxDepth` (default: 50)

**Configuration**:
```json
{
  "loopPrevention": {
    "maxDepth": 50
  }
}
```

**Check Logic**:
```javascript
const depthCheck = depthCounter.check(event);
// Returns: { allowed: boolean, depth: number, reason?: string }
```

---

### Layer 2: Fingerprinting

**Purpose**: Detect duplicate events

**Mechanism**:
- Generate fingerprint from `type + JSON.stringify(data)`
- Keep cache of recent fingerprints (LRU, size: 1000)
- Reject events with duplicate fingerprint

**Configuration**:
```json
{
  "loopPrevention": {
    "fingerprintCacheSize": 1000
  }
}
```

**Fingerprint Generation**:
```javascript
import { generateFingerprint } from './loop-prevention/fingerprinting.js';

const fingerprint = generateFingerprint(event);
// Returns SHA-256 hash of type + data
```

---

### Layer 3: Ancestry Chain

**Purpose**: Detect cycles in causation

**Mechanism**:
- Track parent chain via `metadata.triggeredBy`
- Build ancestry: `[evt_1, evt_2, evt_3, ...]`
- Reject if current event ID appears in ancestry

**Check Logic**:
```javascript
const ancestryCheck = ancestryTracker.check(event);
// Returns: { allowed: boolean, ancestry: string[], cycle?: string[] }
```

**Example Cycle Detection**:
```
Event A triggers Event B
Event B triggers Event C
Event C triggers Event A ❌ CYCLE DETECTED
```

---

### Layer 4: Circuit Breaker

**Purpose**: Per-function rate limiting

**Mechanism**:
- Count function executions per time window
- Reject if count > threshold within window
- Auto-reset after window expires

**Configuration**:
```json
{
  "loopPrevention": {
    "circuitBreakerThreshold": 100,
    "circuitBreakerWindow": 60000
  }
}
```

**Check Logic**:
```javascript
const circuitCheck = circuitBreaker.check(functionId);
// Returns: { allowed: boolean, count: number, threshold: number }
```

---

### Coordinator Usage

```javascript
import { LoopPreventionCoordinator } from './loop-prevention/index.js';

const coordinator = new LoopPreventionCoordinator({
  maxDepth: 50,
  fingerprintCacheSize: 1000,
  circuitBreakerThreshold: 100,
  circuitBreakerWindow: 60000
});

// Check event
const result = coordinator.checkEvent(event);
if (!result.allowed) {
  console.error('Loop detected:', result.reason, result.mechanism);
  return;
}

// Event allowed, proceed
processEvent(event);
```

**Statistics**:
```javascript
const stats = coordinator.getStats();
// {
//   totalPrevented: 42,
//   preventedByMechanism: {
//     'depth-counter': 10,
//     'fingerprinting': 20,
//     'ancestry-chain': 5,
//     'circuit-breaker': 7
//   },
//   ...
// }
```

---

## Example Sequences

### Example 1: Simple Event Emission

```javascript
// 1. Client emits event via HTTP
POST /events
{
  "type": "user.login",
  "data": { "userId": "alice" }
}

// 2. HTTPServerActor receives request
// 3. HTTPServerActor sends to EventLogActor
createMessage(PROTOCOLS.EVENT, ACTIONS.APPEND, {
  type: "user.login",
  data: { userId: "alice" }
})

// 4. EventLogActor enriches event
{
  id: "evt_01KEM4PMQG...",
  timestamp: "2026-01-10T14:24:27.376Z",
  type: "user.login",
  data: { userId: "alice" },
  metadata: {
    source: "http",
    triggeredBy: null,
    depth: 0,
    fingerprint: "sha256:abc123..."
  }
}

// 5. EventLogActor appends to events.jsonl
// 6. EventLogActor returns success
{
  success: true,
  eventId: "evt_01KEM4PMQG...",
  eventCount: 1
}

// 7. HTTPServerActor returns 201 Created
```

---

### Example 2: Pattern Matching with Function Execution

```javascript
// 1. Event logged: user.login

// 2. PatternMatcher evaluates patterns
const matches = patternMatcher.matchEvent({
  type: "user.login",
  data: { userId: "alice" }
});
// matches = [{ patternId: "user-login", ... }]

// 3. For each match, look up associated functions
const pattern = patternMatcher.getPattern("user-login");
// pattern.metadata.actions = ["send-welcome-email", "log-activity"]

// 4. For each function, get metadata from registry
const funcMeta = functionRegistry.getFunction("send-welcome-email");
// { type: "code", path: "/functions/send-welcome-email.js" }

// 5. Execute function
const result = await functionExecutor.execute({
  functionId: "send-welcome-email",
  functionPath: funcMeta.data.function.path,
  functionType: funcMeta.data.function.type,
  event: {...}
});

// 6. Function code runs
export default async function sendWelcomeEmail(event, context) {
  const { userId } = event.data;

  // Send email (pseudo-code)
  await sendEmail({
    to: userId,
    subject: "Welcome!",
    body: "Thanks for logging in"
  });

  // Emit success event
  await context.emit({
    type: "email.sent",
    data: {
      recipient: userId,
      template: "welcome"
    }
  });

  return { status: "sent" };
}

// 7. FunctionExecutor emits function.executed event
{
  type: "function.executed",
  data: {
    functionId: "send-welcome-email",
    result: { status: "sent" },
    executionTime: 123
  },
  metadata: {
    source: "FunctionExecutorActor",
    triggeredBy: "evt_01KEM4PMQG...",
    depth: 1
  }
}

// 8. Function also emitted email.sent event
{
  type: "email.sent",
  data: {
    recipient: "alice",
    template: "welcome"
  },
  metadata: {
    source: "send-welcome-email",
    triggeredBy: "evt_01KEM4PMQG...",
    depth: 1
  }
}

// 9. Both events are appended to event log
```

---

### Example 3: Loop Prevention in Action

```javascript
// Scenario: A function accidentally emits an event that triggers itself

// 1. Initial event
{
  id: "evt_001",
  type: "process.data",
  data: { value: 100 },
  metadata: { depth: 0, triggeredBy: null }
}

// 2. Pattern matches, executes function
// 3. Function emits same event type
await context.emit({
  type: "process.data",
  data: { value: 100 }  // Same data!
});

// 4. Loop Prevention Coordinator checks:

// Layer 1: Depth Counter
depth = 1 (< 50) ✓ PASS

// Layer 2: Fingerprinting
fingerprint = sha256("process.data" + '{"value":100}')
if (seenBefore(fingerprint)) {
  return { allowed: false, reason: "Duplicate event detected" };
}
❌ FAIL - Event already processed

// Event is rejected, loop prevented!
```

---

## API Reference

### CLI Commands

```bash
# Start daemon
event-system daemon start

# Check status
event-system daemon status

# Emit event
event-system emit '{"type":"test","data":{"message":"hello"}}'

# Stop daemon
event-system daemon stop

# Help
event-system help
```

---

### HTTP API

#### GET /health
```bash
curl http://localhost:3000/health

# Response
{
  "status": "ok",
  "uptime": 12345,
  "startTime": "2026-01-10T14:00:00Z",
  "actors": {
    "eventLog": "available",
    "functionRegistry": "available",
    "patternMatcher": "available"
  }
}
```

#### POST /events
```bash
curl -X POST http://localhost:3000/events \
  -H "Content-Type: application/json" \
  -d '{"type":"user.login","data":{"userId":"alice"}}'

# Response (201 Created)
{
  "success": true,
  "eventId": "evt_01KEM4PMQG...",
  "eventCount": 1
}
```

#### GET /events
```bash
# All events
curl http://localhost:3000/events

# Filter by type
curl "http://localhost:3000/events?type=user.login"

# Pagination
curl "http://localhost:3000/events?limit=10&offset=20"

# Reverse order
curl "http://localhost:3000/events?reverse=true&limit=5"

# Response
{
  "success": true,
  "events": [
    {
      "id": "evt_01KEM4PMQG...",
      "timestamp": "2026-01-10T14:24:27.376Z",
      "type": "user.login",
      "data": {...},
      "metadata": {...}
    }
  ],
  "count": 1,
  "total": 42
}
```

#### GET /functions
```bash
# All functions
curl http://localhost:3000/functions

# Filter by type
curl "http://localhost:3000/functions?type=code"

# Response
{
  "success": true,
  "functions": [
    {
      "functionId": "send-email",
      "type": "code",
      "path": "/functions/send-email.js",
      "metadata": {
        "name": "Send Email",
        "description": "Sends email notifications"
      },
      "registeredAt": "2026-01-10T14:00:00Z"
    }
  ],
  "count": 1
}
```

#### GET /patterns
```bash
# All patterns
curl http://localhost:3000/patterns

# Sorted by priority
curl "http://localhost:3000/patterns?sortByPriority=true"

# Response
{
  "success": true,
  "patterns": [
    {
      "id": "user-login",
      "predicate": "event.type === 'user.login'",
      "priority": 10,
      "metadata": {
        "description": "Matches user login events"
      }
    }
  ],
  "count": 1
}
```

---

### JavaScript API

#### EventLogActor

```javascript
import { EventLogActor } from './src/actors/event-log.js';

const eventLog = new EventLogActor({
  eventLog: { file: 'events.jsonl' }
});

await eventLog.initialize();

// Append event
const result = await eventLog.appendEvent({
  type: 'user.login',
  data: { userId: 'alice' }
});

// Query events
const query = await eventLog.queryEvents({
  filter: (event) => event.type === 'user.login',
  limit: 10
});

// Close
await eventLog.close();
```

#### FunctionRegistryActor

```javascript
import FunctionRegistryActor from './src/actors/function-registry.js';

const registry = new FunctionRegistryActor();

// Register function
registry.registerFunction('send-email', {
  type: 'code',
  path: '/functions/send-email.js'
});

// List functions
const list = registry.listFunctions({ type: 'code' });

// Get function
const func = registry.getFunction('send-email');

// Scan directory
const scan = await registry.scanDirectory('./functions', {
  recursive: false,
  overwrite: false
});
```

#### PatternMatcherActor

```javascript
import { createPatternMatcher } from './src/actors/pattern-matcher.js';

const matcher = createPatternMatcher();

// Register pattern
matcher.registerPattern({
  id: 'user-login',
  predicate: 'event.type === "user.login"',
  priority: 10
});

// Match event
const result = matcher.matchEvent({
  type: 'user.login',
  data: { userId: 'alice' }
});
// result.matches = [{ patternId: 'user-login', ... }]
```

#### FunctionExecutorActor

```javascript
import { FunctionExecutorActor } from './src/actors/function-executor.js';

const executor = new FunctionExecutorActor();

// Set emit callback
executor.setEmitCallback(async (event) => {
  await eventLog.appendEvent(event);
});

// Execute function
const result = await executor.execute({
  functionId: 'send-email',
  functionPath: '/functions/send-email.js',
  functionType: 'code',
  event: { type: 'user.login', data: { userId: 'alice' } }
});
```

---

## Configuration

### config.json

```json
{
  "daemon": {
    "port": 0,
    "host": "127.0.0.1",
    "logFile": "daemon.log"
  },
  "eventLog": {
    "file": "events.jsonl",
    "checkpointInterval": 1000
  },
  "loopPrevention": {
    "maxDepth": 50,
    "fingerprintCacheSize": 1000,
    "circuitBreakerThreshold": 100,
    "circuitBreakerWindow": 60000
  },
  "functions": {
    "directory": "functions",
    "autoDiscover": true
  },
  "http": {
    "port": 3000,
    "host": "localhost"
  }
}
```

---

## File Structure

```
event-system/
├── config.json              # System configuration
├── events.jsonl             # Event log (append-only)
├── daemon.log               # Daemon output log
├── src/
│   ├── daemon.js            # DaemonActor (orchestrator)
│   ├── cli.js               # CLI interface
│   ├── protocol.js          # Universal Actor Protocol
│   ├── loop-prevention/
│   │   ├── index.js         # Loop Prevention Coordinator
│   │   ├── depth-counter.js
│   │   ├── fingerprinting.js
│   │   ├── ancestry-chain.js
│   │   └── circuit-breaker.js
│   └── actors/
│       ├── event-log.js     # EventLogActor
│       ├── http-server.js   # HTTPServerActor
│       ├── function-registry.js   # FunctionRegistryActor
│       ├── pattern-matcher.js     # PatternMatcherActor
│       └── function-executor.js   # FunctionExecutorActor
├── functions/               # Function implementations
│   ├── echo.js
│   ├── transform.js
│   └── analyze-error.agent.js
├── tests/                   # Test files
│   ├── event-log.test.js
│   ├── function-registry.test.js
│   └── function-executor.test.js
└── examples/
    └── demo-http-server.js  # HTTP server demo
```

---

## Summary

The Event System is a robust, actor-based architecture for reactive event processing with:

1. **6 specialized actors** with single responsibilities
2. **Universal Actor Protocol** for consistent message passing
3. **4-layer loop prevention** for safety
4. **Append-only event log** for auditability
5. **Pattern-based routing** for flexibility
6. **Both code and agent functions** for extensibility
7. **HTTP API** for external integration
8. **CLI interface** for manual operations

The system is production-ready with comprehensive error handling, structured logging, and graceful shutdown.
