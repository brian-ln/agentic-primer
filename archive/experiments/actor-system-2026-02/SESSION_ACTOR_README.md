# SessionActor Implementation

## Overview

The SessionActor is a Durable Object that handles WebSocket connections and routes messages between client-side actors and server-side actors (CoordinatorActor and WorkerActor). It implements the "two faces" transport pattern described in ACTOR_ARCHITECTURE.md.

## Architecture

```
Client Browser                    SessionActor                    Server Actors
     │                                 │                               │
     │   WebSocket Connection          │                               │
     ├────────────────────────────────>│                               │
     │                                 │                               │
     │   Wire Protocol Message         │                               │
     ├────────────────────────────────>│                               │
     │                                 │  Route to Actor               │
     │                                 ├──────────────────────────────>│
     │                                 │                               │
     │                                 │  Actor Response               │
     │                                 │<──────────────────────────────┤
     │   Wire Protocol Response        │                               │
     │<────────────────────────────────┤                               │
```

## Wire Protocol

All messages follow a standardized wire protocol format:

### Client to Server
```json
{
  "to": "coordinator:main",           // Target actor (format: "type:id")
  "from": "client:session-abc",       // Source identifier
  "type": "RegisterWorker",           // Message type
  "payload": { "workerId": "w1" },    // Message data
  "id": "msg-uuid",                   // Unique message ID
  "timestamp": 1234567890             // Timestamp
}
```

### Server to Client
```json
{
  "to": "client:ui",                  // Target (always client:ui)
  "from": "coordinator:main",         // Source actor
  "type": "RegisterWorkerResponse",   // Response type (original + "Response")
  "payload": { ... },                 // Response data
  "id": "msg-uuid",                   // Unique message ID
  "timestamp": 1234567890,            // Timestamp
  "replyTo": "original-msg-uuid"      // Optional: ID of original message
}
```

## Implementation Details

### Files

1. **src/session-actor.ts** - Standalone SessionActor implementation
2. **src/actor-system-session.ts** - Complete system with SessionActor + CoordinatorActor + WorkerActor
3. **src/session-actor.test.ts** - Comprehensive test suite (11 tests)
4. **wrangler-session.jsonc** - Wrangler configuration with all three actor types

### Key Features

#### 1. WebSocket Lifecycle Management
- Accepts WebSocket upgrade requests
- Maintains connection state
- Handles close and error events
- Rejects non-WebSocket requests

#### 2. Message Routing
Routes messages based on `to` field format `"actor-type:actor-id"`:
- `coordinator:main` → CoordinatorActor
- `worker:worker-id` → WorkerActor

#### 3. Protocol Translation
- Parses incoming JSON wire messages
- Routes to appropriate Durable Object
- Formats responses back to wire protocol
- Adds message IDs and timestamps

#### 4. Error Handling
- Invalid JSON → Error response
- Invalid `to` field → Error response
- Unknown actor type → Error response
- Routing errors → Error response with details

### Special Cases

#### Register Worker Flow
For `register_worker` messages, the SessionActor preserves the original `from` field because CoordinatorActor uses it as the worker ID:

```typescript
const fromField = wireMsg.type === "register_worker"
  ? wireMsg.from  // Preserve original for worker registration
  : `session:${this.sessionId}`;  // Use session ID for other messages
```

## Testing

### Running Tests
```bash
npm run test:session
```

### Test Coverage
11 comprehensive tests covering:

1. **WebSocket Connection** (2 tests)
   - Upgrade and session establishment
   - Rejection of non-WebSocket requests

2. **Coordinator Routing** (3 tests)
   - RegisterWorker message routing
   - AssignTask message routing
   - GetStatus message routing

3. **Worker Routing** (1 test)
   - GetState message routing

4. **Wire Protocol Validation** (4 tests)
   - Complete wire format validation
   - Invalid `to` field handling
   - Unknown actor type handling
   - Invalid JSON handling

5. **Complete Flow** (1 test)
   - Full workflow: register → assign → status

### Test Results
```
✓ src/session-actor.test.ts (11 tests) 3579ms
  ✓ SessionActor WebSocket Connection (2 tests)
  ✓ SessionActor Message Routing to CoordinatorActor (3 tests)
  ✓ SessionActor Message Routing to WorkerActor (1 test)
  ✓ SessionActor Wire Protocol Validation (4 tests)
  ✓ SessionActor Complete Flow (1 test)

Test Files  1 passed (1)
     Tests  11 passed (11)
```

## Usage

### Wrangler Configuration
```jsonc
{
  "durable_objects": {
    "bindings": [
      { "name": "COORDINATOR_ACTOR", "class_name": "CoordinatorActor" },
      { "name": "WORKER_ACTOR", "class_name": "WorkerActor" },
      { "name": "SESSION_ACTOR", "class_name": "SessionActor" }
    ]
  },
  "migrations": [
    {
      "tag": "v1",
      "new_sqlite_classes": ["CoordinatorActor", "WorkerActor"]
    },
    {
      "tag": "v2",
      "new_sqlite_classes": ["SessionActor"]
    }
  ]
}
```

### Worker Routing
```typescript
export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);

    // WebSocket endpoint - route to SessionActor
    if (url.pathname === "/ws") {
      const sessionId = url.searchParams.get("session") ?? crypto.randomUUID();
      const stub = env.SESSION_ACTOR.get(
        env.SESSION_ACTOR.idFromName(sessionId)
      );
      return stub.fetch(request);
    }

    // ... other routes
  }
}
```

### Client Connection
```javascript
const ws = new WebSocket('ws://localhost:8787/ws');

ws.onopen = () => {
  // Send wire protocol message
  ws.send(JSON.stringify({
    to: 'coordinator:main',
    from: 'client:my-session',
    type: 'get_status',
    payload: {},
    id: crypto.randomUUID(),
    timestamp: Date.now()
  }));
};

ws.onmessage = (event) => {
  const wireMsg = JSON.parse(event.data);
  console.log('Received:', wireMsg.type, wireMsg.payload);
};
```

## Next Steps

This SessionActor implementation enables the following future work:

1. **play-3u6**: Remove legacy REST endpoints, use only WebSocket
2. **play-42e**: Integration tests for full client-server flow
3. **Client-side actors**: Implement TransportActor, UIActor, ProxyActors

## Success Criteria

✅ WebSocket connection accepts and upgrades
✅ Messages route to CoordinatorActor correctly
✅ Messages route to WorkerActor correctly
✅ Responses sent back to client
✅ Wire protocol matches spec exactly
✅ All 11 tests pass

## Dependencies

- `cloudflare:workers` - DurableObject base class
- Existing CoordinatorActor and WorkerActor implementations
- WebSocketPair for connection handling

## Notes

- Session IDs are Durable Object IDs (64-char hex strings)
- WebSocket connections are per-session (isolated)
- No persistence needed in SessionActor (stateless router)
- All message routing is synchronous (await actor responses)
- Error responses include original message ID in `replyTo` field
