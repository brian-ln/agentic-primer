# WebSocketActor Design (Pure Actor Model)

**Date:** 2026-02-07
**Status:** Design Phase
**Branch:** feature/path-addressing
**Bead:** simplify-net.1

---

## Overview

WebSocketActor is a **system actor** that provides WebSocket client functionality with internal host whitelisting, connection lifecycle management, message buffering, and port-based event streaming. Following the pure actor model, WebSocketActor IS the capability - access control happens through routing, not helper objects.

**Core Principle:** Capabilities are actors. Access control = routing decisions.

---

## Pure Actor Model Architecture

### What This Means

**WebSocketActor is NOT a wrapper:**
```typescript
// ❌ OLD: Helper class approach
class WebSocketCapability {
  async connect(url: string) {
    this.validateHost(url); // Validation in helper
    return await this.router.ask(wsActor, 'ws.connect', { url });
  }
}

// ✅ NEW: Pure actor approach
class WebSocketActor extends Actor {
  async receive(message: Message) {
    if (message.type === 'ws.connect') {
      const host = this.extractHost(message.payload.url);
      if (!this.allowedHosts.has(host)) {
        return createErrorResponse(message, 'Host not allowed');
      }
      // Establish WebSocket connection
    }
  }
}

// Access control = routing
router.registerActor('/workflows/system/websocket', workflowsWs);
// NOT registered for /domain → access denied by absence
```

---

## Design Principles

### 1. System Actors ARE Capabilities

WebSocketActor is the WebSocket capability. Configuration happens at construction.

```typescript
const workflowsWs = new WebSocketActor('workflows-ws', router, {
  allowedHosts: ['ws.example.com', 'realtime.api.com'],
  maxConnections: 5,
  reconnect: {
    enabled: true,
    maxAttempts: 3,
    backoff: 'exponential' // 1s, 2s, 4s
  }
});
```

### 2. Routing Determines Access

If an actor path is registered, access is granted. If not registered, access is implicitly denied.

```typescript
// Workflows can use WebSocket
router.registerActor('/workflows/system/websocket', workflowsWs);

// Session knowledge can use WebSocket (different config)
router.registerActor('/session-knowledge/system/websocket', knowledgeWs);

// No WebSocket for /domain → messages fail with "Actor not found"
```

### 3. Internal Validation

WebSocketActor validates every message against its configuration before execution.

```typescript
async receive(message: Message): Promise<MessageResponse> {
  if (message.type === 'ws.connect') {
    // Check host allowed
    const host = this.extractHost(message.payload.url);
    if (!this.allowedHosts.has(host)) {
      return createErrorResponse(message,
        `Host '${host}' not in allowedHosts: [${Array.from(this.allowedHosts).join(', ')}]`
      );
    }

    // Check connection limit
    if (this.connections.size >= this.maxConnections) {
      return createErrorResponse(message, 'Max connections exceeded');
    }

    // Establish connection
    const connection = await this.createConnection(message.payload.url);
    return createSuccessResponse(message, { connectionId: connection.id });
  }
}
```

### 4. Message-Based Protocol + Port Streaming

All WebSocket operations are messages. Events are streamed through ports.

```typescript
// Connect to WebSocket
const response = await actor.ask(address('/workflows/system/websocket'), 'ws.connect', {
  url: 'wss://realtime.api.com/stream',
  protocols: ['json'],
  headers: { 'Authorization': 'Bearer xyz' }
});

const connectionId = response.payload.connectionId;

// Subscribe to messages via port
const port = await actor.ask(
  address('/workflows/system/websocket'),
  'ws.subscribe',
  { connectionId }
);

for await (const event of port.payload.stream) {
  if (event.type === 'message') {
    console.log('Received:', event.data);
  }
  if (event.type === 'close') {
    console.log('Connection closed');
    break;
  }
}

// Send message
await actor.tell(address('/workflows/system/websocket'), 'ws.send', {
  connectionId,
  data: { action: 'subscribe', channel: 'updates' }
});

// Close connection
await actor.tell(address('/workflows/system/websocket'), 'ws.close', {
  connectionId
});
```

---

## WebSocketActor Interface

### Constructor

```typescript
export class WebSocketActor extends Actor {
  private allowedHosts: Set<string>;
  private maxConnections: number;
  private reconnectConfig: ReconnectConfig;
  private connections: Map<string, WSConnection>;
  private eventPorts: Map<string, Port<WSEvent>>;

  constructor(
    id: string,
    router: MessageRouter,
    config: WebSocketActorConfig
  ) {
    super(id, router);
    this.allowedHosts = new Set(config.allowedHosts);
    this.maxConnections = config.maxConnections;
    this.reconnectConfig = config.reconnect;
    this.connections = new Map();
    this.eventPorts = new Map();
  }
}
```

### Configuration

```typescript
export interface WebSocketActorConfig {
  /** Allowed hosts (whitelist) */
  allowedHosts: string[];

  /** Maximum concurrent connections */
  maxConnections: number;

  /** Reconnection configuration */
  reconnect: {
    enabled: boolean;
    maxAttempts: number;
    backoff: 'linear' | 'exponential'; // 1s, 2s, 3s vs 1s, 2s, 4s
  };
}

export interface ReconnectConfig {
  enabled: boolean;
  maxAttempts: number;
  backoff: 'linear' | 'exponential';
}
```

### Message Protocol

**ws.connect** (Establish WebSocket connection)
```typescript
{
  type: 'ws.connect',
  payload: {
    url: 'wss://realtime.api.com/stream',
    protocols?: string[],
    headers?: Record<string, string>
  }
}

// Response
{
  success: true,
  payload: {
    connectionId: 'ws-abc123',
    readyState: 1 // OPEN
  }
}
```

**ws.send** (Send message to WebSocket)
```typescript
{
  type: 'ws.send',
  payload: {
    connectionId: 'ws-abc123',
    data: { action: 'subscribe', channel: 'updates' }
  }
}

// Response (ack)
{
  success: true,
  payload: { sent: true }
}
```

**ws.close** (Close WebSocket connection)
```typescript
{
  type: 'ws.close',
  payload: {
    connectionId: 'ws-abc123',
    code?: number,
    reason?: string
  }
}

// Response
{
  success: true,
  payload: { closed: true }
}
```

**ws.subscribe** (Subscribe to WebSocket events via port)
```typescript
{
  type: 'ws.subscribe',
  payload: {
    connectionId: 'ws-abc123'
  }
}

// Response (port stream)
{
  success: true,
  payload: {
    stream: AsyncIterator<WSEvent>
  }
}

// Events:
// { type: 'open', connectionId: 'ws-abc123' }
// { type: 'message', connectionId: 'ws-abc123', data: {...} }
// { type: 'error', connectionId: 'ws-abc123', error: 'Connection failed' }
// { type: 'close', connectionId: 'ws-abc123', code: 1000, reason: 'Normal' }
```

---

## Implementation

### Core Logic

```typescript
export class WebSocketActor extends Actor {
  private allowedHosts: Set<string>;
  private maxConnections: number;
  private reconnectConfig: ReconnectConfig;
  private connections: Map<string, WSConnection>;
  private eventPorts: Map<string, Port<WSEvent>>;

  constructor(id: string, router: MessageRouter, config: WebSocketActorConfig) {
    super(id, router);
    this.allowedHosts = new Set(config.allowedHosts);
    this.maxConnections = config.maxConnections;
    this.reconnectConfig = config.reconnect;
    this.connections = new Map();
    this.eventPorts = new Map();
  }

  async receive(message: Message): Promise<MessageResponse> {
    const { type, payload } = message;

    try {
      if (type === 'ws.connect') {
        return await this.handleConnect(message, payload);
      }

      if (type === 'ws.send') {
        return await this.handleSend(message, payload);
      }

      if (type === 'ws.close') {
        return await this.handleClose(message, payload);
      }

      if (type === 'ws.subscribe') {
        return await this.handleSubscribe(message, payload);
      }

      return createErrorResponse(message, `Unknown message type: ${type}`);
    } catch (error: any) {
      this.logError('WebSocket operation failed', {
        type,
        error: error.message
      });
      return createErrorResponse(message, error.message);
    }
  }

  private async handleConnect(
    message: Message,
    payload: {
      url: string;
      protocols?: string[];
      headers?: Record<string, string>;
    }
  ): Promise<MessageResponse> {
    // 1. Validate host
    const host = this.extractHost(payload.url);
    if (!this.allowedHosts.has(host)) {
      return createErrorResponse(message,
        `Host '${host}' not in allowedHosts: [${Array.from(this.allowedHosts).join(', ')}]`
      );
    }

    // 2. Check connection limit
    if (this.connections.size >= this.maxConnections) {
      return createErrorResponse(message,
        `Max connections exceeded: ${this.maxConnections}`
      );
    }

    // 3. Create WebSocket connection
    const connectionId = `ws-${Math.random().toString(36).substring(7)}`;
    const ws = new WebSocket(payload.url, payload.protocols);

    // 4. Create event port for this connection
    const port = this.createPort<WSEvent>(`ws-events-${connectionId}`);
    this.eventPorts.set(connectionId, port);

    // 5. Setup event handlers
    ws.onopen = () => {
      port.send({ type: 'open', connectionId });
    };

    ws.onmessage = (event) => {
      let data: any;
      try {
        data = JSON.parse(event.data);
      } catch {
        data = event.data;
      }
      port.send({ type: 'message', connectionId, data });
    };

    ws.onerror = (event) => {
      port.send({ type: 'error', connectionId, error: 'WebSocket error' });
    };

    ws.onclose = (event) => {
      port.send({ type: 'close', connectionId, code: event.code, reason: event.reason });
      this.cleanup(connectionId);

      // Attempt reconnection if enabled
      if (this.reconnectConfig.enabled) {
        this.scheduleReconnect(connectionId, payload.url, 0);
      }
    };

    // 6. Store connection
    this.connections.set(connectionId, {
      id: connectionId,
      ws,
      url: payload.url,
      reconnectAttempts: 0
    });

    return createSuccessResponse(message, {
      connectionId,
      readyState: ws.readyState
    });
  }

  private async handleSend(
    message: Message,
    payload: {
      connectionId: string;
      data: any;
    }
  ): Promise<MessageResponse> {
    const connection = this.connections.get(payload.connectionId);

    if (!connection) {
      return createErrorResponse(message,
        `Connection not found: ${payload.connectionId}`
      );
    }

    if (connection.ws.readyState !== WebSocket.OPEN) {
      return createErrorResponse(message,
        `Connection not open: readyState=${connection.ws.readyState}`
      );
    }

    // Send message
    const data = typeof payload.data === 'string'
      ? payload.data
      : JSON.stringify(payload.data);

    connection.ws.send(data);

    return createSuccessResponse(message, { sent: true });
  }

  private async handleClose(
    message: Message,
    payload: {
      connectionId: string;
      code?: number;
      reason?: string;
    }
  ): Promise<MessageResponse> {
    const connection = this.connections.get(payload.connectionId);

    if (!connection) {
      return createErrorResponse(message,
        `Connection not found: ${payload.connectionId}`
      );
    }

    // Close WebSocket
    connection.ws.close(payload.code ?? 1000, payload.reason ?? 'Normal closure');

    // Cleanup
    this.cleanup(payload.connectionId);

    return createSuccessResponse(message, { closed: true });
  }

  private async handleSubscribe(
    message: Message,
    payload: {
      connectionId: string;
    }
  ): Promise<MessageResponse> {
    const port = this.eventPorts.get(payload.connectionId);

    if (!port) {
      return createErrorResponse(message,
        `No event port for connection: ${payload.connectionId}`
      );
    }

    // Return async iterator for port events
    return createSuccessResponse(message, {
      stream: port[Symbol.asyncIterator]()
    });
  }

  private scheduleReconnect(connectionId: string, url: string, attempt: number): void {
    if (attempt >= this.reconnectConfig.maxAttempts) {
      this.logError('Max reconnect attempts reached', { connectionId, attempt });
      return;
    }

    // Calculate backoff delay
    const delay = this.reconnectConfig.backoff === 'exponential'
      ? Math.pow(2, attempt) * 1000 // 1s, 2s, 4s, 8s
      : (attempt + 1) * 1000;        // 1s, 2s, 3s, 4s

    setTimeout(async () => {
      this.logInfo('Attempting reconnect', { connectionId, attempt, delay });

      // Recreate connection
      const message = createMessage(
        address(this.id),
        address(this.id),
        'ws.connect',
        { url }
      );

      const response = await this.handleConnect(message, { url });

      if (!response.success) {
        // Retry
        this.scheduleReconnect(connectionId, url, attempt + 1);
      }
    }, delay);
  }

  private cleanup(connectionId: string): void {
    this.connections.delete(connectionId);

    const port = this.eventPorts.get(connectionId);
    if (port) {
      port.close();
      this.eventPorts.delete(connectionId);
    }
  }

  private extractHost(url: string): string {
    try {
      const parsed = new URL(url);
      return parsed.hostname;
    } catch {
      throw new Error(`Invalid WebSocket URL: ${url}`);
    }
  }
}

interface WSConnection {
  id: string;
  ws: WebSocket;
  url: string;
  reconnectAttempts: number;
}

export type WSEvent =
  | { type: 'open'; connectionId: string }
  | { type: 'message'; connectionId: string; data: any }
  | { type: 'error'; connectionId: string; error: string }
  | { type: 'close'; connectionId: string; code: number; reason: string };
```

---

## Usage Examples

### Setup: Register WebSocketActors

```typescript
const router = new MessageRouter(store, programManager);

// Workflows namespace WebSocket
const workflowsWs = new WebSocketActor('workflows-ws', router, {
  allowedHosts: ['ws.example.com', 'realtime.api.com'],
  maxConnections: 10,
  reconnect: {
    enabled: true,
    maxAttempts: 5,
    backoff: 'exponential'
  }
});
router.registerActor('/workflows/system/websocket', workflowsWs);

// Session knowledge WebSocket (no reconnect)
const knowledgeWs = new WebSocketActor('knowledge-ws', router, {
  allowedHosts: ['ws.cloudflare.com'],
  maxConnections: 2,
  reconnect: {
    enabled: false,
    maxAttempts: 0,
    backoff: 'linear'
  }
});
router.registerActor('/session-knowledge/system/websocket', knowledgeWs);

// Domain namespace: NO WebSocket ACCESS (not registered)
```

### Actors Use WebSocketActor via Messages

```typescript
class RealtimeActor extends Actor {
  private connectionId?: string;

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'start-realtime') {
      // 1. Connect to WebSocket
      const connectResponse = await this.ask(
        address('/workflows/system/websocket'),
        'ws.connect',
        {
          url: 'wss://realtime.api.com/stream',
          protocols: ['json'],
          headers: { 'Authorization': 'Bearer xyz' }
        }
      );

      if (!connectResponse.success) {
        return createErrorResponse(message, connectResponse.error);
      }

      this.connectionId = connectResponse.payload.connectionId;

      // 2. Subscribe to events
      const subscribeResponse = await this.ask(
        address('/workflows/system/websocket'),
        'ws.subscribe',
        { connectionId: this.connectionId }
      );

      // 3. Listen to events in background
      this.listenToEvents(subscribeResponse.payload.stream);

      // 4. Send initial message
      await this.tell(
        address('/workflows/system/websocket'),
        'ws.send',
        {
          connectionId: this.connectionId,
          data: { action: 'subscribe', channel: 'updates' }
        }
      );

      return createSuccessResponse(message, { connected: true });
    }

    if (message.type === 'stop-realtime') {
      if (this.connectionId) {
        await this.tell(
          address('/workflows/system/websocket'),
          'ws.close',
          { connectionId: this.connectionId }
        );
      }

      return createSuccessResponse(message, { stopped: true });
    }
  }

  private async listenToEvents(stream: AsyncIterator<WSEvent>): Promise<void> {
    for await (const event of stream) {
      if (event.type === 'message') {
        this.logInfo('Received message', { data: event.data });
        // Process message
      }

      if (event.type === 'close') {
        this.logInfo('Connection closed', { code: event.code });
        break;
      }

      if (event.type === 'error') {
        this.logError('WebSocket error', { error: event.error });
      }
    }
  }
}
```

### Access Control Through Routing

```typescript
// ✅ Workflows actor CAN use WebSocket
const realtimeActor = new RealtimeActor('realtime', router);
router.registerActor('/workflows/realtime', realtimeActor);

await realtimeActor.ask(
  address('/workflows/system/websocket'),
  'ws.connect',
  { url: 'wss://realtime.api.com/stream' }
);

// ❌ Domain actor CANNOT use WebSocket (not registered)
const domainActor = new Actor('domain', router);
router.registerActor('/domain/logic', domainActor);

await domainActor.ask(
  address('/domain/system/websocket'), // Not registered
  'ws.connect',
  { url: 'wss://realtime.api.com/stream' }
);
// Error: Actor not found at /domain/system/websocket
```

---

## Error Handling

### Access Denied Errors

```typescript
// Host not allowed
{
  success: false,
  error: "Host 'evil.com' not in allowedHosts: [ws.example.com, realtime.api.com]"
}

// Max connections exceeded
{
  success: false,
  error: "Max connections exceeded: 10"
}

// Connection not found
{
  success: false,
  error: "Connection not found: ws-abc123"
}

// Connection not open
{
  success: false,
  error: "Connection not open: readyState=3"
}

// Invalid URL
{
  success: false,
  error: "Invalid WebSocket URL: not-a-url"
}
```

### Clear Error Messages

All errors include:
- **What was denied:** Host, connection limit, or state
- **Why it was denied:** Not in allowedHosts, max connections, not open
- **What is allowed:** List of allowedHosts

---

## Reconnection Strategy

### Exponential Backoff

```typescript
// Config: { maxAttempts: 5, backoff: 'exponential' }
// Attempt 0: 1s delay
// Attempt 1: 2s delay
// Attempt 2: 4s delay
// Attempt 3: 8s delay
// Attempt 4: 16s delay
// Max attempts reached → give up
```

### Linear Backoff

```typescript
// Config: { maxAttempts: 5, backoff: 'linear' }
// Attempt 0: 1s delay
// Attempt 1: 2s delay
// Attempt 2: 3s delay
// Attempt 3: 4s delay
// Attempt 4: 5s delay
// Max attempts reached → give up
```

---

## Port-Based Event Streaming

### Why Ports?

Ports enable 1:N broadcasting of WebSocket events. Multiple actors can subscribe to the same connection's events.

```typescript
// Create port for connection events
const port = this.createPort<WSEvent>(`ws-events-${connectionId}`);

// WebSocket event handlers publish to port
ws.onmessage = (event) => {
  port.send({ type: 'message', connectionId, data: event.data });
};

// Multiple subscribers can consume events
for await (const event of port) {
  // Handle event
}
```

---

## Testing Strategy

### Test Coverage (>15 tests)

1. **Host Validation:**
   - Allow whitelisted hosts ✅
   - Deny non-whitelisted hosts ❌
   - WebSocket URL parsing

2. **Connection Lifecycle:**
   - Connect to WebSocket ✅
   - Send messages ✅
   - Receive messages ✅
   - Close connection ✅

3. **Connection Limits:**
   - Connections within limit ✅
   - Connections exceeding limit ❌

4. **Port Subscription:**
   - Subscribe to events ✅
   - Multiple subscribers ✅
   - Event delivery
   - Port cleanup on close

5. **Reconnection:**
   - Reconnect on disconnect (if enabled) ✅
   - Exponential backoff delays
   - Max attempts reached
   - Reconnect disabled

6. **Error Scenarios:**
   - Invalid WebSocket URL ❌
   - Connection not found ❌
   - Connection not open ❌
   - WebSocket errors

---

## Benefits

### 1. Real-Time Communication
- Bidirectional messaging
- Event-driven architecture
- Low latency updates

### 2. Security by Default
- Host whitelisting prevents unauthorized connections
- Connection limits prevent resource exhaustion

### 3. Resilience
- Automatic reconnection (configurable)
- Graceful error handling
- Connection state management

### 4. Composability
- Port-based event streaming
- Multiple subscribers per connection
- Integration with actor system

---

## Next Steps

1. ✅ Design approved (this document)
2. Implement WebSocketActor (simplify-net.4)
3. Create comprehensive tests (simplify-net.5)
4. Document usage patterns
5. Integration examples

---

**Document End**
