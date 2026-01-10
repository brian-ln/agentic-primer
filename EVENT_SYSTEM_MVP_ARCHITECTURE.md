# Event Capture System MVP Architecture

## System Overview

### What We're Building

A "dead simple" event capture and async thread protocol system that:
- Captures events via CLI and HTTP
- Matches events against predicates (filter on any field)
- Executes registered functions when patterns match
- Prevents infinite loops using multiple safety mechanisms
- Stores everything in append-only JSONL logs (event sourcing)
- Runs as a daemon with configurable stack depth

### Why We're Building It

This system provides the foundation for:
- **Event Capture**: Record everything that happens in the system
- **Async Thread Protocol**: Functions trigger other functions without blocking
- **Pattern Matching**: Respond to events based on flexible predicates
- **Agent Integration**: Special "agent" functions that call Claude CLI
- **Event Sourcing**: Complete audit trail, replay capability, debugging

### Phase 1 MVP Scope

**What we ARE doing:**
- Event capture via CLI and HTTP
- JSONL event log (append-only)
- Predicate-based pattern matching
- Code-based function execution (.js files)
- Special "agent" function type (calls claude cli)
- Configurable stack depth (default: 50)
- Loop prevention (depth counter, fingerprinting, ancestry, circuit breaker)
- Basic daemon with HTTP server

**What we are NOT doing:**
- No database (pure JSONL files)
- No authentication/authorization
- No web UI
- No distributed system features
- No persistence layer beyond files
- No advanced monitoring/metrics
- No production hardening

## Core Components

The system is built using the Actor Model with 6 core actor types:

### 1. DaemonActor (Orchestrator)

**Responsibility**: Main event loop, coordinates all other actors

**State**:
```javascript
{
  running: boolean,
  actors: Map<actorId, actorReference>,
  config: {
    maxStackDepth: 50,
    eventLogPath: "./events.jsonl",
    functionsDir: "./functions",
    httpPort: 3000
  }
}
```

**Handles**:
- `START` - Initialize system, spawn child actors
- `STOP` - Graceful shutdown
- `RELOAD_CONFIG` - Reload configuration
- `HEALTH_CHECK` - Return system status

**Spawns**:
- EventLogActor (event persistence)
- PatternMatcherActor (predicate evaluation)
- FunctionRegistryActor (function catalog)
- HTTPServerActor (HTTP interface)

### 2. EventLogActor (Persistence)

**Responsibility**: Append-only event storage and replay

**State**:
```javascript
{
  logPath: string,
  writeStream: WriteStream,
  eventCount: number,
  lastCheckpoint: number
}
```

**Handles**:
- `APPEND_EVENT` - Write event to JSONL log
- `QUERY_EVENTS` - Read events with filters
- `CHECKPOINT` - Mark current position for replay
- `REPLAY_FROM` - Replay events from checkpoint

**Event Format (JSONL)**:
```javascript
{
  "id": "evt_01JBCD...",          // ulid
  "timestamp": "2026-01-10T...",  // ISO8601
  "type": "user.action",          // event type
  "data": { ... },                // arbitrary payload
  "metadata": {
    "source": "cli|http|function",
    "triggeredBy": "evt_...",     // parent event (for chains)
    "depth": 0,                   // stack depth
    "fingerprint": "abc123..."    // content hash for dedup
  }
}
```

### 3. PatternMatcherActor (Predicate Engine)

**Responsibility**: Match events against registered predicates

**State**:
```javascript
{
  patterns: Map<patternId, {
    predicate: Function,
    functionId: string,
    priority: number
  }>
}
```

**Handles**:
- `MATCH_EVENT` - Evaluate event against all patterns
- `REGISTER_PATTERN` - Add new pattern
- `UNREGISTER_PATTERN` - Remove pattern
- `LIST_PATTERNS` - Query registered patterns

**Predicate Format**:
```javascript
// Predicates are JavaScript functions that return boolean
(event) => {
  // Full access to event object
  return event.type === "user.login" &&
         event.data.userId === "123";
}

// Can filter on ANY field
(event) => event.metadata.depth < 10
(event) => event.data.priority === "high"
(event) => event.timestamp > Date.now() - 3600000
```

**Matching Algorithm**:
1. Receive `MATCH_EVENT` message with event
2. Evaluate event against ALL predicates in parallel
3. Sort matches by priority (higher first)
4. Return list of `{functionId, patternId}` matches
5. DaemonActor dispatches to FunctionActor instances

### 4. FunctionRegistryActor (Function Catalog)

**Responsibility**: Track available functions and their metadata

**State**:
```javascript
{
  functions: Map<functionId, {
    type: "code" | "agent",
    path: string,              // file path for code functions
    agentCommand: string,      // for agent functions
    maxStackDepth: number,     // override default
    metadata: {
      name: string,
      description: string,
      author: string
    }
  }>
}
```

**Handles**:
- `REGISTER_FUNCTION` - Add function to registry
- `UNREGISTER_FUNCTION` - Remove function
- `GET_FUNCTION` - Retrieve function metadata
- `LIST_FUNCTIONS` - Query all functions
- `SCAN_DIRECTORY` - Auto-discover functions from filesystem

**Function Types**:

1. **Code Functions** (.js files):
```javascript
// functions/notify-slack.js
export default async function(event, context) {
  // event: the triggering event
  // context: { emit, logger, config }

  const result = await fetch("https://slack.com/api/...", {
    method: "POST",
    body: JSON.stringify({ text: event.data.message })
  });

  // Emit new events (creates chains)
  context.emit({
    type: "notification.sent",
    data: { channel: "alerts", messageId: result.id }
  });

  return { success: true };
}
```

2. **Agent Functions** (special type):
```javascript
// functions/analyze-error.agent.js
export const config = {
  type: "agent",
  command: "claude --model sonnet-4.5"
};

export default async function(event, context) {
  // This function calls Claude CLI with the event
  const prompt = `Analyze this error event and suggest fixes:\n${JSON.stringify(event.data, null, 2)}`;

  const result = await context.claudeCLI(prompt);

  context.emit({
    type: "analysis.complete",
    data: { analysis: result.response }
  });
}
```

### 5. HTTPServerActor (HTTP Interface)

**Responsibility**: HTTP API for event ingestion and queries

**State**:
```javascript
{
  port: 3000,
  server: HTTPServer,
  requestCount: number
}
```

**Handles**:
- `START_SERVER` - Bind to port
- `STOP_SERVER` - Close connections
- `GET_STATS` - Return request metrics

**HTTP Endpoints**:

```
POST /events
  Body: { "type": "...", "data": {...} }
  Response: { "eventId": "evt_...", "status": "accepted" }

GET /events
  Query: ?type=user.login&limit=100
  Response: [{ event }, { event }, ...]

GET /functions
  Response: [{ functionId, name, type }, ...]

GET /patterns
  Response: [{ patternId, functionId, predicate }, ...]

GET /health
  Response: { "status": "healthy", "uptime": 1234, "eventCount": 5678 }
```

### 6. FunctionActor (Execution Worker)

**Responsibility**: Execute individual function invocations

**State**:
```javascript
{
  functionId: string,
  event: Event,
  depth: number,
  ancestryChain: string[]  // [evt_1, evt_2, evt_3, ...]
}
```

**Handles**:
- `EXECUTE` - Run function with event
- `TIMEOUT` - Kill execution after max time
- `EMIT_EVENT` - Child function emitting new event

**Lifecycle**:
1. DaemonActor spawns FunctionActor for each match
2. FunctionActor loads function from registry
3. Executes function with event + context
4. Captures emitted events
5. Applies loop prevention checks
6. Terminates and reports result

## Universal Actor Protocol (UAP)

All messages between actors follow this structure:

```javascript
{
  protocol: "UAP/1.0",
  action: "ACTION_NAME",      // e.g., "APPEND_EVENT", "MATCH_EVENT"
  data: { ... },              // action-specific payload
  metadata: {
    from: "actorId",          // sender
    to: "actorId",            // recipient
    messageId: "msg_...",     // ulid
    timestamp: "2026-01-10T...",
    replyTo: "msg_..."        // for request/response
  }
}
```

**Why UAP?**
- Uniform message structure across all actors
- Easy to log, replay, and debug
- Clear provenance (who sent what to whom)
- Request/response pairing via `replyTo`

## Loop Prevention Mechanisms

Critical for async function chains. We use 4 complementary strategies:

### 1. Depth Counter

**How it works**:
- Each event has `metadata.depth` field
- Initial events (from CLI/HTTP) start at depth 0
- When function emits new event, depth = parent.depth + 1
- DaemonActor rejects events exceeding `maxStackDepth`

**Configuration**:
```javascript
// Global default
config.maxStackDepth = 50;

// Per-function override
functions['analyze-error'].maxStackDepth = 100;
```

**Prevents**: Simple infinite recursion (A → A → A → ...)

### 2. Fingerprinting (Content Hash)

**How it works**:
- Each event gets `metadata.fingerprint` = hash(type + data)
- DaemonActor maintains sliding window of recent fingerprints
- Reject events with duplicate fingerprints within window

**Configuration**:
```javascript
config.fingerprintWindow = 1000;  // last 1000 events
```

**Prevents**: Exact duplicate events in tight loops

### 3. Ancestry Chain

**How it works**:
- Each event tracks `metadata.triggeredBy` (parent event ID)
- FunctionActor builds full ancestry: [evt_1, evt_2, evt_3, ...]
- Reject if current event type already in ancestry

**Example**:
```
Event A (type: error.detected)
  → Event B (type: analyze.error)  // ancestry: [A]
    → Event C (type: error.detected)  // REJECTED! "error.detected" already in ancestry
```

**Prevents**: Cyclical event chains (A → B → A → B → ...)

### 4. Circuit Breaker

**How it works**:
- Track event emission rate per function
- If function emits > threshold events/second, enter "open" state
- In open state, function calls fail fast
- After timeout, enter "half-open" (allow 1 call to test)
- If successful, close circuit; if fails, re-open

**Configuration**:
```javascript
config.circuitBreaker = {
  threshold: 10,        // events per second
  timeout: 60000,       // 60s before retry
  rollingWindow: 10000  // 10s window
};
```

**Prevents**: High-frequency event storms

## Event Sourcing Patterns

### Append-Only Log

**File**: `events.jsonl` (one JSON object per line)

**Properties**:
- Immutable: never modify or delete events
- Ordered: events written sequentially
- Durable: fsync after each write (configurable)
- Compact: JSONL format (not a database)

### Checkpointing

**Purpose**: Mark position for replay without re-processing everything

**Format** (special event type):
```javascript
{
  "type": "system.checkpoint",
  "data": {
    "checkpointId": "ckpt_...",
    "eventCount": 5678,
    "timestamp": "2026-01-10T..."
  }
}
```

**Usage**:
```javascript
// Replay from checkpoint
await eventLog.replayFrom("ckpt_abc123", (event) => {
  // process event
});
```

### Replay Strategy

**Use cases**:
- Rebuild state after crash
- Test new functions on historical events
- Debugging and forensics

**Implementation**:
```javascript
// Full replay
for (const event of readEventsFromLog()) {
  await patternMatcher.match(event);
}

// Partial replay (from checkpoint)
const checkpoint = findCheckpoint("ckpt_abc");
for (const event of readEventsFromLog(checkpoint.offset)) {
  await patternMatcher.match(event);
}
```

## API Surface

### CLI Commands

```bash
# Emit event
event-system emit --type "user.login" --data '{"userId": "123"}'

# Query events
event-system query --type "user.login" --limit 100

# List functions
event-system functions list

# Register function
event-system functions register ./functions/my-function.js

# List patterns
event-system patterns list

# Register pattern
event-system patterns register --function "notify-slack" --predicate "event.type === 'error.critical'"

# Daemon control
event-system daemon start
event-system daemon stop
event-system daemon status

# Replay events
event-system replay --from "ckpt_abc123"
```

### HTTP Endpoints

See "HTTPServerActor" section above for endpoint details.

### JavaScript API (for functions)

```javascript
// Context object passed to functions
const context = {
  // Emit new event (creates chain)
  emit: async (eventData) => { ... },

  // Logger
  logger: {
    info: (msg) => { ... },
    error: (msg) => { ... }
  },

  // Configuration
  config: { ... },

  // Agent functions only
  claudeCLI: async (prompt, options) => { ... }
};
```

## Function System

### Code-Based Functions

**File structure**:
```
functions/
├── notify-slack.js
├── analyze-error.js
├── send-email.js
└── utils/
    └── formatting.js
```

**Function signature**:
```javascript
export default async function(event, context) {
  // Your code here
  return { success: true, data: {...} };
}

// Optional metadata
export const metadata = {
  name: "Notify Slack",
  description: "Send event to Slack channel",
  author: "me@example.com"
};

// Optional config
export const config = {
  maxStackDepth: 100  // override default
};
```

### Agent Functions (Special Case)

**Purpose**: Call Claude CLI as a function

**File structure**:
```javascript
// functions/analyze-error.agent.js
export const config = {
  type: "agent",
  model: "sonnet-4.5",
  maxTokens: 4000
};

export default async function(event, context) {
  const prompt = buildPrompt(event);
  const result = await context.claudeCLI(prompt);

  // Emit result as new event
  context.emit({
    type: "analysis.complete",
    data: { analysis: result.response }
  });

  return { success: true };
}

function buildPrompt(event) {
  return `Analyze this error and suggest fixes:\n\n${JSON.stringify(event.data, null, 2)}`;
}
```

**How it works**:
1. Function is registered with `type: "agent"`
2. FunctionActor detects agent type
3. Spawns `claude` CLI as subprocess
4. Streams prompt to stdin
5. Captures stdout as result
6. Wraps in standard function response

## Phase 1 Constraints

**What we're NOT doing (explicitly out of scope):**

1. **No Database**: Only JSONL files. No SQLite, Postgres, etc.
2. **No Authentication**: HTTP endpoints are public (assume localhost)
3. **No Authorization**: All functions can access all events
4. **No UI**: CLI and HTTP only, no web interface
5. **No Monitoring**: Basic health check only, no metrics/dashboards
6. **No Distributed System**: Single process, single machine
7. **No Persistence Layer**: No ORM, no migrations, just files
8. **No Advanced Querying**: Linear scan of JSONL, no indexing
9. **No Rate Limiting**: Circuit breaker only, no per-client limits
10. **No Versioning**: No API versions, no backward compatibility
11. **No Testing Framework**: Manual testing only for Phase 1
12. **No Documentation Site**: Markdown docs only

**Why these constraints?**
- Keep Phase 1 "dead simple"
- Prove core concept works
- Iterate quickly
- Add complexity in later phases

## Success Criteria

Phase 1 MVP is complete when:

1. **Daemon runs**: Can start/stop via CLI
2. **Event capture works**: CLI and HTTP can emit events
3. **Events persist**: JSONL log contains all events
4. **Pattern matching works**: Predicates trigger functions
5. **Code functions execute**: .js files run successfully
6. **Agent functions work**: Can call Claude CLI
7. **Loop prevention active**: All 4 mechanisms prevent infinite loops
8. **Replay works**: Can replay events from checkpoint
9. **Health check works**: HTTP endpoint returns status

**Acceptance Tests** (manual for Phase 1):
```bash
# Test 1: Event capture
event-system emit --type "test.event" --data '{"hello": "world"}'
grep "test.event" events.jsonl  # Should find event

# Test 2: Pattern matching
event-system patterns register --function "echo" --predicate "event.type === 'test.event'"
event-system emit --type "test.event" --data '{}'
# Should see function execution in logs

# Test 3: Loop prevention
event-system emit --type "loop.test" --data '{}'
# Function emits same event type → should be rejected

# Test 4: Agent function
event-system emit --type "analyze.request" --data '{"error": "null pointer"}'
# Should call Claude CLI and emit analysis.complete event

# Test 5: Replay
event-system replay --from-beginning
# Should re-process all events
```

## Implementation Notes

### Technology Stack

- **Runtime**: Bun.js (fast, built-in TypeScript, compatible with Node)
- **Language**: JavaScript (ES modules, async/await)
- **File Format**: JSONL (newline-delimited JSON)
- **HTTP**: Bun's built-in HTTP server
- **CLI**: Simple argument parsing (process.argv)
- **IDs**: ULID (sortable, timestamp-encoded)

### File Organization

```
event-system/
├── README.md
├── EVENT_SYSTEM_MVP_ARCHITECTURE.md  (this file)
├── EVENT_SYSTEM_WBS.md               (work breakdown)
├── src/
│   ├── daemon.js                     (DaemonActor)
│   ├── actors/
│   │   ├── event-log.js              (EventLogActor)
│   │   ├── pattern-matcher.js        (PatternMatcherActor)
│   │   ├── function-registry.js      (FunctionRegistryActor)
│   │   ├── http-server.js            (HTTPServerActor)
│   │   └── function-executor.js      (FunctionActor)
│   ├── protocol/
│   │   └── uap.js                    (Universal Actor Protocol)
│   ├── loop-prevention/
│   │   ├── depth-counter.js
│   │   ├── fingerprinting.js
│   │   ├── ancestry-chain.js
│   │   └── circuit-breaker.js
│   └── cli.js                        (CLI entry point)
├── functions/
│   ├── echo.js                       (example function)
│   └── analyze-error.agent.js        (example agent function)
├── tests/
│   ├── manual-test-plan.md
│   └── example-events.json
├── events.jsonl                      (generated at runtime)
└── config.json                       (configuration)
```

### Configuration File

```json
{
  "daemon": {
    "maxStackDepth": 50,
    "eventLogPath": "./events.jsonl",
    "functionsDir": "./functions",
    "httpPort": 3000
  },
  "eventLog": {
    "fsync": true,
    "checkpointInterval": 1000
  },
  "patternMatcher": {
    "maxConcurrentMatches": 100
  },
  "loopPrevention": {
    "fingerprintWindow": 1000,
    "circuitBreaker": {
      "threshold": 10,
      "timeout": 60000,
      "rollingWindow": 10000
    }
  },
  "agent": {
    "command": "claude",
    "defaultModel": "sonnet-4.5",
    "maxTokens": 4000
  }
}
```

## Open Questions & Decisions

**Q1: What should HTTP response format be?**
- **Decision**: JSON for all responses, plaintext errors
- **Rationale**: Standard, easy to parse

**Q2: Should CLI support piping input?**
- **Decision**: Phase 2 - keep Phase 1 simple
- **Rationale**: Reduces complexity, can add later

**Q3: What happens when event log grows to 1GB?**
- **Decision**: Phase 2 - log rotation
- **Rationale**: Not critical for MVP, can add later

**Q4: Should functions run in separate processes?**
- **Decision**: No - same process for Phase 1
- **Rationale**: Simplicity over isolation

**Q5: How to handle function crashes?**
- **Decision**: Log error, emit error event, continue
- **Rationale**: System stays up, crash becomes observable event

**Q6: Should we support TypeScript for functions?**
- **Decision**: Phase 2 - Bun supports TS natively
- **Rationale**: Can add easily, not critical for proof-of-concept

## Next Steps

1. Read `EVENT_SYSTEM_WBS.md` for task breakdown
2. Review beads issues for implementation tracking
3. Start with Phase 1a: Foundation (daemon skeleton, event log, CLI)
4. Validate with manual acceptance tests after each phase
5. Iterate based on real usage

## References

- Universal Actor Protocol: Inspired by Erlang/OTP, Akka patterns
- Event Sourcing: Martin Fowler's event sourcing patterns
- Loop Prevention: Inspired by network routing protocols (TTL, path vector)
- Circuit Breaker: Michael Nygard's "Release It!" pattern
- JSONL Format: http://jsonlines.org/
- ULID: https://github.com/ulid/spec

---

**Document Status**: v1.0 (Initial)
**Last Updated**: 2026-01-10
**Author**: Claude (Sonnet 4.5)
**Review**: Pending user feedback
