# Entangled Actors Pattern

**Pattern Category:** Distributed Actor Communication
**Complexity:** Intermediate
**Use Case:** Cross-runtime transparent actor communication

## Intent

Enable actors in different runtime environments (browser, server, worker) to communicate transparently through paired transport actors that handle network serialization, connection management, and error handling while preserving the illusion of local message passing.

## Problem

Traditional distributed systems require actors to:
- Know about network boundaries
- Handle serialization/deserialization explicitly
- Manage connection lifecycle manually
- Implement retry logic separately
- Deal with different error types (local vs network)

This creates coupling between business logic and transport concerns.

## Solution

**Entangled transport actors** are paired implementations (server + client) that:
1. Share an identical message protocol
2. Handle all transport concerns internally
3. Present a uniform actor interface to the system
4. Enable transparent cross-runtime communication

Actors send messages using the same API regardless of whether the target is local or remote.

## Structure

```
┌─────────────────────────────────────────────────────────────┐
│                    Actor Message Protocol                    │
│         (Same interface for local and remote actors)         │
└─────────────────────────────────────────────────────────────┘
                            ▲     ▲
                            │     │
                    ┌───────┘     └───────┐
                    │                     │
        ┌───────────▼──────────┐ ┌───────▼──────────┐
        │  Client Transport    │ │ Server Transport │
        │      Actor           │ │      Actor       │
        │                      │ │                  │
        │ - connect()          │ │ - startServer()  │
        │ - send()             │ │ - broadcast()    │
        │ - request()          │ │ - sendToClient() │
        │ - onMessage()        │ │ - setRouter()    │
        │ - autoReconnect()    │ │ - getClients()   │
        └──────────┬───────────┘ └───────┬──────────┘
                   │                     │
                   │  WebSocket/SSE/HTTP │
                   └──────────┬──────────┘
                              │
                  ┌───────────▼───────────┐
                  │  Shared Message Format │
                  │  (JSON over wire)      │
                  └────────────────────────┘
```

## Participants

### 1. TransportMessage (Shared Protocol)

The contract that binds server and client transports:

```typescript
interface TransportMessage {
  type: 'actor-message' | 'connection-status' | 'error';
  target?: string;        // Actor name to route to
  messageType?: string;   // Actor message type
  data?: any;             // Message payload
  requestId?: string;     // For request/response pairing
  sender?: string;        // Message origin
  timestamp?: string;     // ISO 8601 timestamp
}
```

### 2. Server Transport Actor

Accepts connections and routes messages to local actors.

**Core Responsibilities:**
- Listen for incoming connections
- Maintain client registry
- Route messages to target actors
- Send responses back to clients
- Broadcast to multiple clients

**Message Types:**
- `start-server` → Start listening on port
- `stop-server` → Shutdown gracefully
- `send-to-client` → Send to specific client
- `broadcast` → Send to all clients
- `get-status` → Connection statistics
- `set-message-router` → Register routing function

### 3. Client Transport Actor

Connects to server and enables remote actor calls.

**Core Responsibilities:**
- Connect to server endpoint
- Send messages to remote actors
- Handle incoming push messages
- Manage pending requests
- Auto-reconnect on disconnect

**Message Types:**
- `connect` → Establish connection
- `disconnect` → Close connection
- `send` → Fire-and-forget message
- `request` → Request with response
- `get-status` → Connection state
- `on-message` → Register message handler
- `set-auto-reconnect` → Configure reconnection

## Collaborations

### Sequence: Request/Response Pattern

```
Browser Actor          Client Transport       Server Transport       Backend Actor
     │                        │                       │                    │
     │ ask(target, msg) ──────┼──────────────────────►│                    │
     │                        │  {requestId: 'r1'}    │                    │
     │                        │                       │ route(target) ────►│
     │                        │                       │                    │
     │                        │                       │◄────response────── │
     │                        │ ◄─────────────────────┼                    │
     │                        │  {requestId: 'r1'}    │                    │
     │ ◄──────result─────────┼                       │                    │
     │                        │                       │                    │
```

### Sequence: Server Push Pattern

```
Backend Actor          Server Transport       Client Transport       Browser Actor
     │                        │                       │                    │
     │ broadcast(msg) ───────►│                       │                    │
     │                        │ ───────push──────────►│                    │
     │                        │                       │ handle(msg) ──────►│
     │                        │                       │                    │
```

## Implementation

### Minimal Server Transport

```typescript
export class WebSocketServerTransportActor extends Actor {
  private clients = new Map<string, Client>();
  private messageRouter: ((clientId: string, message: TransportMessage) => Promise<any>) | null = null;

  async accept(message: ActorMessage): Promise<any> {
    switch (message.type) {
      case 'start-server':
        return this.startServer(message.data?.port || 8080);
      case 'send-to-client':
        return this.sendToClient(message.data?.clientId, message.data?.message);
      case 'broadcast':
        return this.broadcast(message.data);
      case 'set-message-router':
        this.messageRouter = message.data?.router;
        return { success: true };
      default:
        throw new Error(`Unknown message type: ${message.type}`);
    }
  }

  private async startServer(port: number): Promise<any> {
    // Runtime-specific WebSocket server setup
    // Bun: Bun.serve()
    // Deno: Deno.serve()
    // Node: http.createServer() + manual upgrade
  }

  private async handleClientMessage(clientId: string, rawData: any): Promise<void> {
    const message: TransportMessage = JSON.parse(rawData);

    if (this.messageRouter) {
      const response = await this.messageRouter(clientId, message);

      if (message.requestId) {
        this.sendToClient(clientId, {
          type: 'actor-message',
          data: response,
          requestId: message.requestId,
          sender: 'websocket-server',
          timestamp: new Date().toISOString()
        });
      }
    }
  }
}
```

### Minimal Client Transport

```typescript
export class WebSocketClientTransportActor extends Actor {
  private websocket: WebSocket | null = null;
  private pendingRequests = new Map<string, { resolve: Function, reject: Function, timeout: any }>();

  async accept(message: ActorMessage): Promise<any> {
    switch (message.type) {
      case 'connect':
        return this.connect(message.data?.url);
      case 'request':
        return this.request(message.data);
      case 'send':
        return this.send(message.data);
      default:
        throw new Error(`Unknown message type: ${message.type}`);
    }
  }

  private async request(messageData: any): Promise<any> {
    const requestId = this.generateRequestId();
    const transportMessage: TransportMessage = {
      type: 'actor-message',
      target: messageData.target,
      messageType: messageData.messageType,
      data: messageData.data,
      requestId,
      sender: 'browser-client',
      timestamp: new Date().toISOString()
    };

    this.websocket!.send(JSON.stringify(transportMessage));

    // Wait for response with timeout
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        this.pendingRequests.delete(requestId);
        reject(new Error(`Request timeout`));
      }, 10000);

      this.pendingRequests.set(requestId, { resolve, reject, timeout });
    });
  }

  private handleIncomingMessage(rawData: string): void {
    const message: TransportMessage = JSON.parse(rawData);

    if (message.requestId && this.pendingRequests.has(message.requestId)) {
      const { resolve, timeout } = this.pendingRequests.get(message.requestId)!;
      clearTimeout(timeout);
      this.pendingRequests.delete(message.requestId);
      resolve({ success: true, data: message.data });
    }
  }
}
```

## Sample Code

### Browser → Backend Communication

**Browser Side:**

```javascript
// Setup browser runtime with WebSocket client transport
import { createRuntime } from './runtimes/browser';
import { createTransport } from './runtimes/browser';

const runtime = createRuntime({ debug: true });

// Create and register WebSocket client transport
const transport = createTransport('websocket');
runtime.register(transport);

// Connect to backend
await runtime.tell('websocket-client-transport', {
  type: 'connect',
  data: { url: 'ws://localhost:8080' }
});

// Create form handler actor
class FormHandlerActor {
  constructor() {
    this.name = 'form-handler';
    this.intent = 'Handle form submissions';
  }

  async accept(message) {
    if (message.type === 'submit-form') {
      // Call backend actor via transport
      const result = await this.runtime.ask('backend/form-validator', {
        type: 'validate-contact-form',
        data: message.data
      });

      return result;
    }
  }
}

runtime.register(new FormHandlerActor());

// Use it
const result = await runtime.ask('form-handler', {
  type: 'submit-form',
  data: { name: 'John', email: 'john@test.com', message: 'Hello!' }
});

console.log('Form submitted:', result);
// → {valid: true, saved: {id: 'contact-abc123'}}
```

**Backend Side:**

```javascript
// Setup backend runtime with WebSocket server transport
import { createRuntime, createServerTransport } from './runtimes/server';

const runtime = createRuntime({ debug: true });

// Create form validator actor
class FormValidatorActor {
  constructor() {
    this.name = 'form-validator';
    this.intent = 'Validate contact forms';
  }

  async accept(message) {
    if (message.type === 'validate-contact-form') {
      const { name, email, message: text } = message.data;
      const errors = [];

      if (!name || name.length < 2) {
        errors.push({ field: 'name', error: 'Name too short' });
      }

      if (!email || !email.includes('@')) {
        errors.push({ field: 'email', error: 'Invalid email' });
      }

      if (errors.length > 0) {
        return { valid: false, errors };
      }

      // Save to database
      const contactId = `contact-${Date.now()}`;
      await this.db.save({ id: contactId, name, email, message: text });

      return { valid: true, saved: { id: contactId } };
    }
  }
}

runtime.register(new FormValidatorActor());

// Create and register WebSocket server transport
const transport = createServerTransport('websocket');
runtime.register(transport);

// Set up message routing
await runtime.tell('websocket-server-transport', {
  type: 'set-message-router',
  data: {
    router: async (clientId, message) => {
      // Route to target actor
      return await runtime.ask(message.target, {
        type: message.messageType,
        data: message.data
      });
    }
  }
});

// Start server
await runtime.tell('websocket-server-transport', {
  type: 'start-server',
  data: { port: 8080 }
});

console.log('Backend listening on ws://localhost:8080');
```

## Consequences

### Benefits

1. **Transparent Distribution**
   - Actors use the same API for local and remote calls
   - No special network-aware code in business logic
   - Easy to move actors between runtimes

2. **Automatic Error Handling**
   - Network errors appear as normal actor errors
   - Client handles reconnection automatically
   - Timeouts managed by transport layer

3. **Zero External Dependencies**
   - Uses only built-in runtime APIs
   - No npm packages required
   - Works across Node.js, Bun, Deno, browsers

4. **Built-in Request/Response**
   - Promise-based async/await API
   - Request ID tracking
   - Timeout handling

5. **Bidirectional Communication**
   - Server can push to clients
   - Client can request from server
   - Same transport for both directions

### Drawbacks

1. **Message Size Limits**
   - WebSocket has frame size limits
   - Large messages may need chunking
   - Not suitable for streaming large files

2. **Connection Overhead**
   - WebSocket connection per client
   - Memory overhead for connection state
   - Not suitable for thousands of concurrent connections

3. **Single Point of Failure**
   - Server restart disconnects all clients
   - Requires reconnection logic
   - In-flight requests may be lost

4. **Serialization Constraints**
   - Messages must be JSON-serializable
   - No function passing
   - No circular references

## Known Uses

### 1. Actor System Meta-Model (Original)

Cross-runtime actor communication for browser-server applications.

**Scale:** Hundreds of messages/second
**Runtimes:** Browser ↔ Node.js/Bun/Deno
**Status:** Production-ready, experimentally validated

### 2. AI Assistant Chatbot Example

Real-time chat application with form validation.

**Architecture:**
- Browser: Chat UI, form handling
- Backend: Message processing, validation, persistence

**Message Flow:**
```
Browser: chat-handler → backend/message-processor → validate → save → respond
Backend: notification-actor → broadcast → all connected browsers
```

## Related Patterns

### Actor Mesh

Extends entangled actors to server-to-server communication:

```javascript
const runtime = createDistributedRuntime({
  serverPort: 8080,           // Accept connections
  connectTo: [                // Connect to peers
    'ws://server2:8081',
    'ws://server3:8082'
  ]
});
```

Each server is both client (to peers) and server (to browsers).

### Switchable Transport

Automatically selects best transport based on capabilities:

```javascript
const transport = createBestTransport({
  preferred: ['websocket', 'sse', 'http-stream'],
  fallback: true
});
```

Priority: WebSocket > SSE > HTTP Streaming

### Transport Agnostic Actor

Actor that works locally or remotely without changes:

```javascript
class DataValidatorActor {
  // Works the same whether called:
  // - Locally in same runtime
  // - Remotely via WebSocket
  // - Remotely via SSE
  // - Remotely via HTTP
  async accept(message) {
    // Business logic only
  }
}
```

## Implementation Notes

### Runtime Detection

```javascript
function detectRuntime() {
  if (typeof Bun !== 'undefined') return 'bun';
  if (typeof Deno !== 'undefined') return 'deno';
  if (typeof window !== 'undefined') return 'browser';
  return 'node';
}
```

### Exponential Backoff Reconnection

```javascript
private scheduleReconnection(): void {
  this.reconnectAttempts++;

  setTimeout(async () => {
    try {
      await this.connect(this.url);
    } catch (error) {
      // Exponential backoff: 1s, 2s, 4s, 8s, 16s, 30s (cap)
      this.reconnectDelay = Math.min(this.reconnectDelay * 2, 30000);

      if (this.reconnectAttempts < this.maxReconnectAttempts) {
        this.scheduleReconnection();
      }
    }
  }, this.reconnectDelay);
}
```

### Request Timeout Handling

```javascript
private async request(messageData: any): Promise<any> {
  const requestId = this.generateRequestId();

  // Send message
  this.websocket.send(JSON.stringify({
    ...transportMessage,
    requestId
  }));

  // Wait for response with timeout
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      this.pendingRequests.delete(requestId);
      reject(new Error(`Request timeout after ${this.requestTimeout}ms`));
    }, this.requestTimeout);

    this.pendingRequests.set(requestId, { resolve, reject, timeout });
  });
}
```

## Testing Strategy

### Unit Tests

Test transport actors in isolation:

```javascript
test('client transport connects successfully', async () => {
  const transport = new WebSocketClientTransportActor();
  const result = await transport.accept({
    type: 'connect',
    data: { url: 'ws://localhost:8080' }
  });

  expect(result.success).toBe(true);
  expect(result.url).toBe('ws://localhost:8080');
});
```

### Integration Tests

Test server ↔ client interaction:

```javascript
test('request/response round-trip', async () => {
  // Start server
  const serverTransport = new WebSocketServerTransportActor();
  await serverTransport.accept({
    type: 'start-server',
    data: { port: 8080 }
  });

  // Connect client
  const clientTransport = new WebSocketClientTransportActor();
  await clientTransport.accept({
    type: 'connect',
    data: { url: 'ws://localhost:8080' }
  });

  // Send request
  const result = await clientTransport.accept({
    type: 'request',
    data: {
      target: 'echo-actor',
      messageType: 'echo',
      data: { message: 'Hello' }
    }
  });

  expect(result.data.message).toBe('Hello');
});
```

### Performance Tests

Measure latency and throughput:

```javascript
test('cross-runtime latency < 200ms', async () => {
  const start = performance.now();

  const result = await runtime.ask('backend/echo', {
    type: 'echo',
    data: { message: 'Hello' }
  });

  const latency = performance.now() - start;
  expect(latency).toBeLessThan(200);
});
```

## Applicability to tk-agents Workbench

### Use Case: Browser Workbench ↔ Daemon Communication

The entangled actors pattern is ideal for tk-agents workbench:

**Browser Side (Workbench UI):**
- Task list view
- Task detail editor
- Dependency graph visualization
- Status updates

**Daemon Side (Background Service):**
- Task storage (CozoDB)
- Task operations (CRUD)
- Graph queries
- File watching
- Agent coordination

**Benefits for tk-agents:**
1. **Zero Dependencies** - Aligns with project philosophy
2. **Transparent Calls** - UI doesn't know tasks are in daemon
3. **Auto-Reconnect** - Handles daemon restarts gracefully
4. **Push Updates** - Daemon can notify UI of changes
5. **Request/Response** - Natural fit for query operations

**Example Usage:**

```javascript
// Browser workbench
const tasks = await runtime.ask('daemon/task-manager', {
  type: 'list-tasks',
  data: { label: 'agent' }
});

// Daemon can push updates
daemon.broadcast({
  type: 'task-updated',
  data: { taskId: 'task_5', status: 'complete' }
});
```

---

**Pattern Status:** Proven, Production-Ready
**Complexity:** Intermediate (500 lines per transport)
**Dependencies:** None (built-in APIs only)
**Applicability:** High for distributed actor systems
