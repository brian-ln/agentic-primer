# Actor System on Cloudflare

Pure actor architecture using Durable Objects with WebSocket-only communication between client and server actors.

## Architecture

**Pattern:** Actors everywhere - client and server actors communicate via WebSocket

```
Client Actors → TransportActor → WebSocket → SessionActor → Server Actors
```

### System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    CLIENT (Browser)                         │
│                                                             │
│  UIActor ──┐                                                │
│  CoordinatorProxyActor ──┤                                  │
│  WorkerProxyActor ──┴───► TransportActor                    │
│  EventLogActor           (WebSocket Bridge)                 │
│  ConnectionMonitorActor                                     │
└──────────────────────────────────┬──────────────────────────┘
                                   │
                            WebSocket (JSON Messages)
                                   │
┌──────────────────────────────────▼──────────────────────────┐
│                    SERVER (Cloudflare Edge)                 │
│                                                             │
│  SessionActor (per connection)                              │
│       │                                                     │
│       ├──► CoordinatorActor (orchestration)                 │
│       ├──► WorkerActor (task processing)                    │
│       └──► EventLogActor (activity logging)                 │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### 1. Install Dependencies

```bash
npm install
```

### 2. Run Tests

```bash
# All unit tests (client-side actors)
npx vitest run --config vitest-client.config.ts

# All tests including integration
npm test

# Watch mode
npm run test:watch
```

**Test Coverage:**
- Runtime: 12 tests ✅
- TransportActor: 20 tests ✅
- UI Actors: 47 tests ✅
- SessionActor: 11 tests ✅
- Integration: 35 tests (28 passing)
- **Total: 125 tests**

### 3. Start Dev Server

```bash
# Start SessionActor-based server
wrangler dev --config wrangler-session.jsonc
```

The server provides:
- WebSocket endpoint at `ws://localhost:8787`
- Dashboard HTML at `http://localhost:8787/`
- No REST APIs - all communication via WebSocket

### 4. Open Dashboard

```bash
open http://localhost:8787/dashboard-actors.html
```

The dashboard connects via WebSocket and uses client-side actors for all interactions:
- Register workers
- Assign tasks
- View coordinator status
- Monitor worker state
- See real-time activity log

## How It Works

### Client-Side Actors

The browser runs a lightweight actor runtime with these actors:

- **UIActor** - Manages DOM, handles button clicks, updates display
- **CoordinatorProxyActor** - Tracks coordinator state (workers, tasks)
- **WorkerProxyActor** - Tracks worker state (current task, processed count)
- **EventLogActor** - Maintains activity log
- **ConnectionMonitorActor** - Tracks WebSocket connection status
- **TransportActor** - Two-faced actor that bridges WebSocket to local actors

### Server-Side Actors (Durable Objects)

The Cloudflare edge runs these actors:

- **SessionActor** - One per WebSocket connection, routes messages
- **CoordinatorActor** - Task orchestration and worker management
- **WorkerActor** - Task processing
- **EventLogActor** - Activity logging

### Message Flow Example: Register Worker

```
1. User clicks "Register Worker" button
   → UIActor receives click event

2. UIActor sends message to TransportActor
   { type: 'SendToServer', payload: { to: 'coordinator:main', type: 'RegisterWorker', ... } }

3. TransportActor translates to wire protocol and sends over WebSocket
   { to: 'coordinator:main', from: 'client:session-abc', type: 'RegisterWorker', ... }

4. SessionActor receives WebSocket message
   → Routes to CoordinatorActor

5. CoordinatorActor processes registration
   → Updates state
   → Persists to SQLite
   → Sends StatusUpdated response

6. SessionActor sends response over WebSocket

7. TransportActor receives and routes to CoordinatorProxyActor

8. CoordinatorProxyActor updates local state
   → Sends CoordinatorStatusUpdated to UIActor
   → Sends LogEvent to EventLogActor

9. UIActor updates DOM
   EventLogActor updates activity log
```

**Key Point:** Every step is message passing. No REST APIs, no polling, no callbacks.

## Files

### Client-Side

- **`src/runtime/`** - Actor runtime for browser
  - `actor.js` - Actor base class and runtime
  - `actor.test.js` - 12 tests

- **`src/transport/`** - WebSocket bridge actor
  - `transport-actor.js` - Two-faced TransportActor
  - `transport-actor.test.js` - 20 tests

- **`src/ui/`** - UI actors for dashboard
  - `ui-actor.js` - DOM management
  - `coordinator-proxy-actor.js` - Coordinator state tracking
  - `worker-proxy-actor.js` - Worker state tracking
  - `event-log-actor.js` - Activity log
  - `connection-monitor-actor.js` - Connection status
  - `*.test.js` - 47 tests total

- **`public/dashboard-actors.html`** - Actor-based dashboard

### Server-Side

- **`src/session-actor.ts`** - WebSocket handler and message router
- **`src/actor-system-session.ts`** - Integrated actor system with SessionActor
- **`actor-system-queue.ts`** - Alternative with event queue

### Configuration

- **`wrangler-session.jsonc`** - SessionActor-based deployment
- **`vitest-client.config.ts`** - Client-side test config
- **`vitest.config.ts`** - Server-side test config

### Tests

- **`src/**/*.test.js`** - Unit tests (79 passing)
- **`src/session-actor.test.ts`** - SessionActor tests (11 tests)
- **`test/integration/`** - E2E integration tests (35 tests, 28 passing)

### Documentation

- **`ACTOR_ARCHITECTURE.md`** - Detailed architecture design
- **`WORKFLOWS_VS_ACTORS.md`** - When to use DOs vs Workflows
- **`src/*/README.md`** - Component-level documentation

## Key Concepts

### 1. Pure Actor Model

Every component is an actor. Communication is exclusively via asynchronous message passing.

**No:**
- REST APIs
- HTTP polling
- Direct function calls
- Shared state

**Yes:**
- Messages between actors
- WebSocket for network
- State owned by each actor
- Push-based updates

### 2. TransportActor: Two Faces

The TransportActor is the secret to making remote actors feel local.

**Local Face** - To other client actors, it looks like a normal actor:
```javascript
// Send message to server
uiActor.send('transport', {
  type: 'SendToServer',
  payload: { to: 'coordinator:main', type: 'RegisterWorker', ... }
});
```

**Network Face** - Manages WebSocket and wire protocol:
```javascript
// TransportActor translates to wire format
{
  "to": "coordinator:main",
  "from": "client:session-abc",
  "type": "RegisterWorker",
  "payload": { "workerId": "w1" },
  "id": "msg-uuid",
  "timestamp": 1234567890
}
```

This separation:
- Hides network complexity from other actors
- Makes remote communication feel just like local messages
- Handles reconnection automatically
- Enables offline testing with mock TransportActor

### 3. Wire Protocol

All WebSocket messages use a standard JSON format:

```json
{
  "to": "coordinator:main",
  "from": "client:session-abc",
  "type": "RegisterWorker",
  "payload": { "workerId": "w1" },
  "id": "msg-uuid",
  "timestamp": 1234567890
}
```

Fields:
- `to` - Target actor (e.g., "coordinator:main", "client:ui")
- `from` - Source actor (e.g., "client:session-abc", "coordinator:main")
- `type` - Message type (e.g., "RegisterWorker", "StatusUpdated")
- `payload` - Message data
- `id` - Unique message ID
- `timestamp` - Timestamp (milliseconds since epoch)

### 4. Actor Lifecycle

**Client Actors:**
```javascript
const runtime = new ActorRuntime();
const transport = runtime.spawn(TransportActor, 'transport', wsUrl);
const ui = runtime.spawn(UIActor, 'ui');
```

**Server Actors:**
```typescript
// SessionActor (per WebSocket connection)
const sessionId = env.SESSION.idFromName(connectionId);
const session = env.SESSION.get(sessionId);

// CoordinatorActor (singleton)
const coordId = env.COORDINATOR.idFromName('main');
const coordinator = env.COORDINATOR.get(coordId);
```

### 5. State Synchronization

State flows from server to client via messages:

1. Server actor updates state
2. Sends update message via SessionActor
3. TransportActor routes to proxy actor
4. Proxy actor updates local state
5. Proxy sends update to UIActor
6. UIActor updates DOM

No polling needed - state is pushed when it changes.

## Testing

The actor system has comprehensive test coverage across all layers.

### Client-Side Tests (79 tests, all passing)

Run with:
```bash
npx vitest run --config vitest-client.config.ts
```

**Runtime Tests (12 tests):**
- Actor spawning and message routing
- Asynchronous message delivery
- Missing actor handling
- Multi-actor coordination

**TransportActor Tests (20 tests):**
- WebSocket connection management
- Session establishment
- Wire protocol translation
- Reconnection with exponential backoff
- Error handling

**UI Actor Tests (47 tests):**
- UIActor: DOM updates, button handling
- CoordinatorProxyActor: State tracking
- WorkerProxyActor: State tracking
- EventLogActor: Log management
- ConnectionMonitorActor: Connection status

### Server-Side Tests (11 tests)

Run with:
```bash
npm test src/session-actor.test.ts
```

**SessionActor Tests:**
- WebSocket upgrade and connection
- Message routing to CoordinatorActor
- Message routing to WorkerActor
- Wire protocol validation
- Error handling

### Integration Tests (35 tests, 28 passing)

Run with:
```bash
npm run test:integration
```

**E2E Actor Flow Tests:**
- Complete register worker flow
- Complete assign task flow
- State synchronization across actors
- Multi-actor coordination
- Error handling

**WebSocket Lifecycle Tests:**
- Connection establishment
- Message routing
- State synchronization
- Reconnection handling

**State Sync Tests:**
- Initial state retrieval
- Incremental updates
- Multi-client synchronization

### Test Architecture

Tests use:
- **Vitest** for fast, modern testing
- **Mock WebSocket** for client-side tests
- **Miniflare** for Durable Object simulation
- **JSDOM** for DOM testing

### Running Tests

```bash
# All client-side tests
npx vitest run --config vitest-client.config.ts

# All tests (client + server + integration)
npm test

# Watch mode
npm run test:watch

# Specific test file
npx vitest run src/runtime/actor.test.js

# Integration tests only
npm run test:integration
```

## Design Benefits

### Compared to Traditional REST + Polling

**Before (REST + Polling):**
```javascript
// Imperative, callback-heavy code
async function registerWorker() {
  const workerId = `worker-${Date.now()}`;
  const response = await fetch(`${API_BASE}/coordinator`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      id: `reg-${Date.now()}`,
      type: 'register_worker',
      from: workerId
    })
  });
  const data = await response.json();
  log(`Registered worker: ${workerId}`);
  await refreshAll(); // Manual state sync!
}

// Wasteful polling
setInterval(refreshAll, 2000); // High latency, inefficient
```

**After (Pure Actors):**
```javascript
// Clean message passing
class UIActor {
  handleClick(button) {
    if (button === 'registerWorker') {
      this.send('transport', {
        type: 'SendToServer',
        payload: {
          to: 'coordinator:main',
          type: 'RegisterWorker',
          payload: { workerId: this.generateWorkerId() }
        }
      });
    }
  }

  receive(message) {
    switch (message.type) {
      case 'CoordinatorStatusUpdated':
        this.updateDOM(message.payload);
        break;
    }
  }
}

// No polling - state pushed via messages
```

### Key Advantages

1. **Uniform Model** - Same actor pattern on client and server
2. **No Polling** - Server pushes state changes via WebSocket
3. **Low Latency** - Real-time updates, not 2-second delays
4. **Clean Abstraction** - TransportActor hides all network complexity
5. **Testable** - Mock TransportActor for offline testing
6. **Type-Safe** - Message types can be typed end-to-end (TypeScript)
7. **Resilient** - Automatic reconnection with exponential backoff
8. **Scalable** - Each SessionActor is a Durable Object at the edge

## Production Considerations

### Deployment

Deploy using Wrangler:
```bash
wrangler deploy --config wrangler-session.jsonc
```

This deploys:
- Worker for HTTP/WebSocket handling
- SessionActor (Durable Object)
- CoordinatorActor (Durable Object)
- WorkerActor (Durable Object)
- EventLogActor (Durable Object)

### Monitoring

Add observability in `wrangler-session.jsonc`:
```jsonc
{
  "observability": {
    "enabled": true
  }
}
```

### Scaling

The architecture scales automatically:
- **SessionActor**: One per WebSocket connection (100k+ connections)
- **CoordinatorActor**: Singleton with SQLite persistence
- **WorkerActor**: One per worker (sharded across Cloudflare's network)
- **WebSocket**: Cloudflare handles load balancing

### Adding Features

**New Actor Type:**
1. Create actor class extending `DurableObject`
2. Add binding to `wrangler-session.jsonc`
3. Add migration for SQLite classes
4. Add routing in SessionActor

**New Message Type:**
1. Add message handler in target actor
2. Add message type to wire protocol
3. Add sender in client actor
4. Add tests

## Related Documentation

- **`ACTOR_ARCHITECTURE.md`** - Detailed architecture and design rationale
- **`WORKFLOWS_VS_ACTORS.md`** - When to use Durable Objects vs Workflows
- **`src/runtime/README.md`** - Client-side actor runtime
- **`src/transport/README.md`** - TransportActor details and wire protocol
- **`src/ui/README.md`** - UI actors architecture

## Resources

- [Durable Objects Documentation](https://developers.cloudflare.com/durable-objects/)
- [WebSocket Documentation](https://developers.cloudflare.com/durable-objects/examples/websocket-server/)
- [Vitest Documentation](https://vitest.dev/)
- [Actor Model on Wikipedia](https://en.wikipedia.org/wiki/Actor_model)
