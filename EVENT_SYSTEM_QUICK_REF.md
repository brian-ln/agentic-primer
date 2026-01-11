# Event System - Quick Reference Guide

Fast reference for common operations and key concepts.

---

## System At-a-Glance

### 6 Actors

| Actor | Responsibility | Protocol |
|-------|---------------|----------|
| DaemonActor | Orchestration & lifecycle | - |
| EventLogActor | Event storage (JSONL) | event.v1 |
| HTTPServerActor | REST API server | http.v1 |
| FunctionRegistryActor | Function catalog | registry.v1 |
| PatternMatcherActor | Event matching | - |
| FunctionExecutorActor | Function execution | function.v1 |

### 4 Loop Prevention Layers

1. **Depth Counter**: Max depth = 50
2. **Fingerprinting**: SHA-256 hash deduplication
3. **Ancestry Chain**: Cycle detection
4. **Circuit Breaker**: 100 calls/60s per function

---

## Common Commands

### Start System

```bash
# HTTP Demo (standalone)
bun run demo-http-server.js

# Daemon (background)
event-system daemon start
```

### Emit Events

```bash
# CLI
event-system emit '{"type":"test","data":{"x":1}}'

# HTTP
curl -X POST http://localhost:3000/events \
  -H "Content-Type: application/json" \
  -d '{"type":"test","data":{"x":1}}'
```

### Query Events

```bash
# All events
curl http://localhost:3000/events

# Filter by type
curl "http://localhost:3000/events?type=user.login"

# Pagination
curl "http://localhost:3000/events?limit=10&offset=20"

# Reverse order
curl "http://localhost:3000/events?reverse=true"
```

### List Resources

```bash
# Health check
curl http://localhost:3000/health

# Patterns
curl http://localhost:3000/patterns

# Functions
curl http://localhost:3000/functions
```

---

## API Endpoints

| Endpoint | Method | Purpose | Query Params |
|----------|--------|---------|--------------|
| /health | GET | System health | - |
| /events | POST | Emit event | - |
| /events | GET | Query events | type, limit, offset, reverse |
| /functions | GET | List functions | type |
| /patterns | GET | List patterns | sortByPriority |

---

## Event Structure

```javascript
{
  id: "evt_01KEM4PMQG...",           // ULID (sortable)
  timestamp: "2026-01-10T14:24:27Z", // ISO 8601
  type: "user.login",                 // Event type
  data: {                             // Event payload
    userId: "alice"
  },
  metadata: {
    source: "http",                   // Origin
    triggeredBy: "evt_parent_id",     // Parent event
    depth: 1,                          // Chain depth
    fingerprint: "sha256:abc123..."   // Dedup hash
  }
}
```

---

## UAP Message Format

```javascript
{
  protocol: "event.v1",     // Protocol namespace
  action: "append",          // Action to perform
  data: {...},              // Action payload
  timestamp: "2026-01-10T..." // Message timestamp
}
```

---

## Protocols & Actions

### event.v1

- `append` - Add event to log
- `query` - Search events
- `checkpoint` - Mark position
- `replay` - Replay from position

### function.v1

- `execute` - Run function
- `complete` - Function succeeded
- `error` - Function failed

### registry.v1

- `register` - Add function
- `unregister` - Remove function
- `list` - List functions
- `get` - Get function metadata
- `scan` - Auto-discover functions

---

## Configuration (config.json)

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

## Function Types

### Code Function (functions/echo.js)

```javascript
export default async function echo(event, context) {
  context.logger.info('Echo:', event.type);

  await context.emit({
    type: 'echo.response',
    data: event.data
  });

  return { status: 'echoed' };
}
```

### Agent Function (functions/analyze.agent.js)

```javascript
// Metadata only - execution via Claude CLI subprocess
export const metadata = {
  name: 'Analyze Event',
  description: 'Uses Claude to analyze events',
  agentCommand: 'claude'
};
```

---

## Execution Context

Functions receive context object:

```javascript
{
  emit: async (event) => {...},  // Emit new events
  logger: {
    info(...args),
    warn(...args),
    error(...args),
    debug(...args)
  },
  config: {...}  // Configuration
}
```

---

## Pattern Predicates

JavaScript expressions with `event` in scope:

```javascript
// Simple type check
"event.type === 'user.login'"

// Multiple conditions
"event.type === 'user.login' && event.data.userId"

// Complex logic
"event.type.startsWith('error.') && event.data.severity === 'critical'"

// Property access
"event.data.amount > 1000"
```

---

## Further Reading

- **ARCHITECTURE_COMPLETE.md**: Full architecture documentation (100+ pages)
- **DEMONSTRATION.md**: Working system demonstrations (50+ pages)
- **MESSAGE_FLOWS.md**: Detailed message sequences (40+ pages)
- **DELIVERABLES_SUMMARY.md**: Project summary
