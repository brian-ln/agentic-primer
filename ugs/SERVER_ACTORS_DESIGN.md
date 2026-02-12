# Server Actors Design (Pure Actor Model)

**Date:** 2026-02-07
**Status:** Design Phase
**Context:** Complement to HTTPClientActor and WebSocketActor (clients)
**Goal:** Enable actors to serve HTTP and WebSocket connections from external clients

---

## Overview

Server actors enable the actor system to accept incoming HTTP requests and WebSocket connections from external clients. While HTTPClientActor and WebSocketActor provide **outbound** capabilities (making requests), server actors provide **inbound** capabilities (serving requests).

**Core Principle:** Server actors ARE the capability. Access control happens through routing, not middleware classes.

---

## Architecture: Three Server Actors

### 1. HTTPServerActor

**Purpose:** Accept HTTP requests and route them to handler actors

**Key Features:**
- Listen on configured port
- Route incoming requests to actors based on path patterns
- Support middleware (auth, CORS, logging, rate limiting)
- RESTful API conventions
- Request/response lifecycle management

### 2. WebSocketServerActor

**Purpose:** Accept WebSocket connections and manage client sessions

**Key Features:**
- Listen on configured port
- Accept client connections
- Track connected clients
- Broadcast messages to clients (1:N)
- Send messages to specific clients (1:1)
- Connection lifecycle (connect, message, error, close)

### 3. SSEServerActor (Server-Sent Events)

**Purpose:** Stream updates to browsers (one-way, server to client)

**Key Features:**
- Listen on configured port
- Accept SSE connections
- Send events to connected clients
- Automatic keep-alive (prevent timeout)
- Reconnection support (client-side)
- Simpler than WebSocket for one-way updates

---

## Design Decisions

### Decision 1: Routing Pattern

**Question:** How do incoming HTTP requests map to actor messages?

**Options:**
1. **Path-based routing to actors** - `/tasks/123` routes to `TaskActor(123)`
2. **Dedicated API actors** - `/api/tasks/*` routes to `TaskAPIActor`
3. **Hybrid** - Both patterns supported

**Decision: Hybrid approach**

Allow both patterns. HTTPServerActor supports configurable routing rules that map paths to actor addresses.

**Example:**
```typescript
// Pattern 1: Path-based (RESTful)
router.addRoute('GET', '/tasks/:id', {
  actor: (params) => address(`/tasks/${params.id}`),
  message: 'http.get'
});

// Pattern 2: API actor
router.addRoute('POST', '/api/tasks', {
  actor: address('/api/tasks'),
  message: 'tasks.create'
});
```

**Rationale:**
- Pattern 1 is natural for CRUD operations on entities
- Pattern 2 is better for complex operations or when actors don't map 1:1 to URLs
- Supporting both gives maximum flexibility

---

### Decision 2: Port Assignment Strategy

**Question:** How do namespaces share or isolate server ports?

**Options:**
1. **One server per namespace** - Each namespace listens on different port
2. **Shared server with path prefixes** - All namespaces share one server, routed by path prefix
3. **Hybrid** - Global server for public APIs, isolated servers for internal services

**Decision: Path-based namespace isolation (Option 2)**

One HTTPServerActor instance can serve multiple namespaces, with path prefixes determining which namespace handles the request.

**Example:**
```typescript
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/workflows': { namespace: '/workflows', prefix: '/api/workflows' },
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' },
    '/api/domain': { namespace: '/domain', prefix: '/api/domain' }
  }
});
```

**Incoming Request Flow:**
```
GET /api/workflows/orchestrator/123
  ↓
HTTPServerActor checks prefix: /api/workflows
  ↓
Namespace: /workflows
  ↓
Route within namespace: /orchestrator/123
  ↓
Actor: address('/workflows/orchestrator')
  ↓
Message: { type: 'http.get', payload: { params: { id: '123' } } }
```

**Rationale:**
- Simpler deployment (one port, one process)
- Clear namespace boundaries (path prefixes)
- Easier to add CORS, auth middleware globally
- Can still spin up isolated servers if needed

---

### Decision 3: Middleware Architecture

**Question:** How do we handle cross-cutting concerns (auth, logging, CORS)?

**Options:**
1. **Middleware chain (Express-style)** - Request flows through middleware stack
2. **Actor-based middleware** - Each middleware is an actor
3. **Internal validation** - HTTPServerActor validates internally (like HTTPClientActor)

**Decision: Internal validation with actor delegation (Option 3)**

HTTPServerActor performs validation internally (CORS, rate limiting) but delegates to system actors for complex operations (auth, logging).

**Example:**
```typescript
class HTTPServerActor extends Actor {
  async handleRequest(req: Request): Promise<Response> {
    // 1. CORS (internal validation)
    if (!this.validateCORS(req)) {
      return new Response('CORS not allowed', { status: 403 });
    }

    // 2. Rate limiting (internal validation)
    if (!this.rateLimiter.tryAcquire(req.ip)) {
      return new Response('Rate limit exceeded', { status: 429 });
    }

    // 3. Authentication (delegate to AuthActor)
    const authResponse = await this.ask(
      address('/system/auth'),
      'auth.validate',
      { token: req.headers.get('authorization') }
    );

    if (!authResponse.success) {
      return new Response('Unauthorized', { status: 401 });
    }

    // 4. Logging (delegate to LoggerActor)
    await this.tell(
      address('/system/logger'),
      'log.request',
      { method: req.method, url: req.url, user: authResponse.payload.user }
    );

    // 5. Route to handler actor
    const response = await this.routeRequest(req);

    return response;
  }
}
```

**Rationale:**
- Consistent with HTTPClientActor design (internal validation)
- Simple operations (CORS, rate limiting) don't need actors
- Complex operations (auth, logging) delegate to system actors
- Clearer than middleware chain (explicit ask/tell)

---

### Decision 4: WebSocket Connection Management

**Question:** How do actors broadcast to WebSocket clients?

**Pattern: Port-based broadcasting**

WebSocketServerActor exposes ports that actors can send messages to. Clients subscribe to channels.

**Example:**
```typescript
// Client connects to WebSocket
// Server creates a channel for 'tasks' updates

// TaskActor broadcasts update
await this.tell(
  address('/system/ws-server'),
  'ws.broadcast',
  { channel: 'tasks', data: { id: '123', status: 'completed' } }
);

// All clients subscribed to 'tasks' channel receive the update
```

**Rationale:**
- Decouples actors from WebSocket implementation details
- Actors don't need to track individual connections
- Channel-based broadcasting is simpler than connection-based
- Natural fit with pub/sub patterns

---

## HTTPServerActor Design

### Configuration

```typescript
export interface HTTPServerConfig {
  /** Port to listen on */
  port: number;

  /** Namespace routing configuration */
  namespaces: Record<string, {
    namespace: string; // Actor namespace (e.g., '/workflows')
    prefix: string;    // URL prefix (e.g., '/api/workflows')
  }>;

  /** CORS configuration */
  cors?: {
    allowedOrigins: string[];
    allowedMethods: string[];
    allowedHeaders: string[];
    credentials: boolean;
  };

  /** Rate limiting */
  rateLimit?: {
    requests: number;
    window: number; // milliseconds
    keyBy: 'ip' | 'user' | 'token';
  };

  /** Request timeout */
  timeout?: number;
}
```

### Message Protocol

**http-server.listen** (Start HTTP server)
```typescript
{
  type: 'http-server.listen',
  payload: {
    port?: number // Override config port
  }
}

// Response
{
  success: true,
  payload: {
    port: 3000,
    listening: true
  }
}
```

**http-server.route** (Register route handler)
```typescript
{
  type: 'http-server.route',
  payload: {
    method: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH',
    path: '/tasks/:id',
    actor: '/workflows/tasks', // Actor address to route to
    message: 'http.get'        // Message type to send
  }
}

// Response
{
  success: true,
  payload: {
    route: 'GET /tasks/:id'
  }
}
```

**http-server.stop** (Stop HTTP server)
```typescript
{
  type: 'http-server.stop',
  payload: {}
}

// Response
{
  success: true,
  payload: {
    stopped: true
  }
}
```

### Request Routing Flow

```
1. Client sends: GET /api/workflows/tasks/123

2. HTTPServerActor receives request
   ↓
3. Match prefix: /api/workflows → namespace: /workflows
   ↓
4. Find route: GET /tasks/:id
   ↓
5. Extract params: { id: '123' }
   ↓
6. Send message to actor:
   {
     to: address('/workflows/tasks'),
     type: 'http.get',
     payload: {
       params: { id: '123' },
       query: {},
       headers: { ... },
       body: null
     }
   }
   ↓
7. Actor processes request
   ↓
8. Actor returns response:
   {
     success: true,
     payload: {
       id: '123',
       title: 'Task title',
       status: 'open'
     }
   }
   ↓
9. HTTPServerActor converts to HTTP response:
   HTTP 200 OK
   Content-Type: application/json
   { "id": "123", "title": "Task title", "status": "open" }
```

### Route Registration

**Dynamic route registration:**
```typescript
// Actor can register its own routes
class TaskActor extends Actor {
  async onInit() {
    // Register GET /tasks/:id
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'GET',
        path: '/tasks/:id',
        actor: this.address,
        message: 'http.get'
      }
    );

    // Register POST /tasks
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'POST',
        path: '/tasks',
        actor: this.address,
        message: 'http.create'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.get') {
      // Handle GET /tasks/:id
      const { id } = message.payload.params;
      return createResponse(message, { id, title: 'Task', status: 'open' });
    }

    if (message.type === 'http.create') {
      // Handle POST /tasks
      const { title } = message.payload.body;
      return createResponse(message, { id: '123', title, status: 'open' });
    }
  }
}
```

**Static route configuration:**
```typescript
// Configure routes at server startup
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  routes: [
    {
      method: 'GET',
      path: '/tasks/:id',
      actor: '/workflows/tasks',
      message: 'http.get'
    },
    {
      method: 'POST',
      path: '/tasks',
      actor: '/workflows/tasks',
      message: 'http.create'
    }
  ]
});
```

### Implementation Sketch

```typescript
export class HTTPServerActor extends Actor {
  private server?: Server;
  private config: HTTPServerConfig;
  private routes: Map<string, RouteHandler>;
  private rateLimiter: RateLimiter;

  constructor(id: string, router: MessageRouter, config: HTTPServerConfig) {
    super(id, router);
    this.config = config;
    this.routes = new Map();
    this.rateLimiter = new RateLimiter(config.rateLimit);
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http-server.listen') {
      return await this.handleListen(message);
    }

    if (message.type === 'http-server.route') {
      return await this.handleRoute(message);
    }

    if (message.type === 'http-server.stop') {
      return await this.handleStop(message);
    }

    return createErrorResponse(message, `Unknown message type: ${message.type}`);
  }

  private async handleListen(message: Message): Promise<MessageResponse> {
    const port = message.payload.port ?? this.config.port;

    this.server = Bun.serve({
      port,
      fetch: (req) => this.handleRequest(req),
    });

    this.logInfo('HTTP server listening', { port });

    return createResponse(message, {
      port,
      listening: true
    });
  }

  private async handleRequest(req: Request): Promise<Response> {
    try {
      // 1. CORS
      if (!this.validateCORS(req)) {
        return new Response('CORS not allowed', { status: 403 });
      }

      // 2. Rate limiting
      const clientKey = this.getClientKey(req);
      if (!this.rateLimiter.tryAcquire(clientKey)) {
        return new Response('Rate limit exceeded', { status: 429 });
      }

      // 3. Match route
      const route = this.matchRoute(req.method, req.url);
      if (!route) {
        return new Response('Not found', { status: 404 });
      }

      // 4. Parse request
      const parsed = await this.parseRequest(req, route);

      // 5. Send to actor
      const response = await this.ask(
        address(route.actor),
        route.message,
        parsed
      );

      // 6. Convert to HTTP response
      return this.toHTTPResponse(response);
    } catch (error: any) {
      this.logError('Request handling failed', { error: error.message });
      return new Response('Internal Server Error', { status: 500 });
    }
  }

  private matchRoute(method: string, url: string): RouteHandler | null {
    const pathname = new URL(url).pathname;

    for (const [pattern, handler] of this.routes) {
      if (handler.method !== method) continue;

      const match = this.matchPattern(pattern, pathname);
      if (match) {
        return { ...handler, params: match };
      }
    }

    return null;
  }

  private async parseRequest(req: Request, route: RouteHandler): Promise<any> {
    const url = new URL(req.url);

    // Parse body (if present)
    let body = null;
    const contentType = req.headers.get('content-type');
    if (contentType?.includes('application/json')) {
      body = await req.json();
    } else if (req.method !== 'GET' && req.method !== 'HEAD') {
      body = await req.text();
    }

    return {
      params: route.params,
      query: Object.fromEntries(url.searchParams),
      headers: Object.fromEntries(req.headers),
      body
    };
  }

  private toHTTPResponse(response: MessageResponse): Response {
    if (!response.success) {
      return new Response(
        JSON.stringify({ error: response.error }),
        {
          status: 400,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    }

    return new Response(
      JSON.stringify(response.payload),
      {
        status: 200,
        headers: { 'Content-Type': 'application/json' }
      }
    );
  }

  private validateCORS(req: Request): boolean {
    if (!this.config.cors) return true;

    const origin = req.headers.get('origin');
    if (!origin) return true;

    return this.config.cors.allowedOrigins.includes(origin) ||
           this.config.cors.allowedOrigins.includes('*');
  }

  private getClientKey(req: Request): string {
    if (!this.config.rateLimit) return 'default';

    if (this.config.rateLimit.keyBy === 'ip') {
      return req.headers.get('x-forwarded-for') || 'unknown';
    }

    // TODO: Extract from auth token
    return 'default';
  }
}

interface RouteHandler {
  method: string;
  pattern: string;
  actor: string;
  message: string;
  params?: Record<string, string>;
}
```

---

## WebSocketServerActor Design

### Configuration

```typescript
export interface WebSocketServerConfig {
  /** Port to listen on */
  port: number;

  /** Maximum concurrent connections */
  maxConnections: number;

  /** Channels configuration */
  channels: {
    [channel: string]: {
      /** Actors that can publish to this channel */
      publishers: string[];
      /** Authentication required? */
      requireAuth?: boolean;
    };
  };

  /** Heartbeat interval (keep-alive) */
  heartbeatInterval?: number;
}
```

### Message Protocol

**ws-server.listen** (Start WebSocket server)
```typescript
{
  type: 'ws-server.listen',
  payload: {
    port?: number
  }
}

// Response
{
  success: true,
  payload: {
    port: 3001,
    listening: true
  }
}
```

**ws-server.broadcast** (Broadcast to channel)
```typescript
{
  type: 'ws-server.broadcast',
  payload: {
    channel: 'tasks',
    data: { id: '123', status: 'completed' }
  }
}

// Response
{
  success: true,
  payload: {
    sent: 15 // Number of clients
  }
}
```

**ws-server.send** (Send to specific client)
```typescript
{
  type: 'ws-server.send',
  payload: {
    clientId: 'client-abc123',
    data: { message: 'Hello' }
  }
}

// Response
{
  success: true,
  payload: {
    sent: true
  }
}
```

**ws-server.clients** (List connected clients)
```typescript
{
  type: 'ws-server.clients',
  payload: {
    channel?: 'tasks' // Optional: filter by channel
  }
}

// Response
{
  success: true,
  payload: {
    clients: [
      { id: 'client-1', channels: ['tasks'], connectedAt: 1234567890 },
      { id: 'client-2', channels: ['tasks', 'workflows'], connectedAt: 1234567900 }
    ]
  }
}
```

### Connection Flow

```
1. Client connects: ws://localhost:3001

2. WebSocketServerActor accepts connection
   ↓
3. Assign clientId: 'client-abc123'
   ↓
4. Client sends subscription message:
   { type: 'subscribe', channel: 'tasks' }
   ↓
5. WebSocketServerActor adds client to 'tasks' channel
   ↓
6. Actor broadcasts update:
   await this.tell(
     address('/system/ws-server'),
     'ws-server.broadcast',
     { channel: 'tasks', data: {...} }
   )
   ↓
7. WebSocketServerActor sends to all clients in 'tasks' channel
   ↓
8. Client receives message
```

### Implementation Sketch

```typescript
export class WebSocketServerActor extends Actor {
  private server?: Server;
  private config: WebSocketServerConfig;
  private clients: Map<string, WSClient>;
  private channels: Map<string, Set<string>>; // channel -> clientIds

  constructor(id: string, router: MessageRouter, config: WebSocketServerConfig) {
    super(id, router);
    this.config = config;
    this.clients = new Map();
    this.channels = new Map();
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'ws-server.listen') {
      return await this.handleListen(message);
    }

    if (message.type === 'ws-server.broadcast') {
      return await this.handleBroadcast(message);
    }

    if (message.type === 'ws-server.send') {
      return await this.handleSend(message);
    }

    if (message.type === 'ws-server.clients') {
      return await this.handleClients(message);
    }

    return createErrorResponse(message, `Unknown message type: ${message.type}`);
  }

  private async handleListen(message: Message): Promise<MessageResponse> {
    const port = message.payload.port ?? this.config.port;

    this.server = Bun.serve({
      port,
      fetch: (req, server) => {
        // Upgrade to WebSocket
        const success = server.upgrade(req);
        if (success) {
          return undefined;
        }
        return new Response('WebSocket upgrade failed', { status: 400 });
      },
      websocket: {
        open: (ws) => this.handleOpen(ws),
        message: (ws, message) => this.handleMessage(ws, message),
        close: (ws) => this.handleClose(ws),
        error: (ws, error) => this.handleError(ws, error)
      }
    });

    this.logInfo('WebSocket server listening', { port });

    return createResponse(message, {
      port,
      listening: true
    });
  }

  private handleOpen(ws: ServerWebSocket): void {
    const clientId = `client-${Math.random().toString(36).substring(7)}`;

    this.clients.set(clientId, {
      id: clientId,
      ws,
      channels: new Set(),
      connectedAt: Date.now()
    });

    ws.data = { clientId };

    this.logInfo('Client connected', { clientId });
  }

  private handleMessage(ws: ServerWebSocket, message: string | Buffer): void {
    const clientId = ws.data?.clientId;
    if (!clientId) return;

    const client = this.clients.get(clientId);
    if (!client) return;

    try {
      const data = typeof message === 'string' ? JSON.parse(message) : message;

      // Handle subscription
      if (data.type === 'subscribe') {
        this.subscribeClient(clientId, data.channel);
        ws.send(JSON.stringify({ type: 'subscribed', channel: data.channel }));
      }

      // Handle unsubscription
      if (data.type === 'unsubscribe') {
        this.unsubscribeClient(clientId, data.channel);
        ws.send(JSON.stringify({ type: 'unsubscribed', channel: data.channel }));
      }
    } catch (error: any) {
      this.logError('Invalid message from client', { clientId, error: error.message });
    }
  }

  private handleClose(ws: ServerWebSocket): void {
    const clientId = ws.data?.clientId;
    if (!clientId) return;

    const client = this.clients.get(clientId);
    if (client) {
      // Remove from all channels
      for (const channel of client.channels) {
        this.unsubscribeClient(clientId, channel);
      }
    }

    this.clients.delete(clientId);
    this.logInfo('Client disconnected', { clientId });
  }

  private handleError(ws: ServerWebSocket, error: Error): void {
    const clientId = ws.data?.clientId;
    this.logError('WebSocket error', { clientId, error: error.message });
  }

  private async handleBroadcast(message: Message): Promise<MessageResponse> {
    const { channel, data } = message.payload;

    const clientIds = this.channels.get(channel);
    if (!clientIds || clientIds.size === 0) {
      return createResponse(message, { sent: 0 });
    }

    const payload = JSON.stringify({ channel, data });
    let sent = 0;

    for (const clientId of clientIds) {
      const client = this.clients.get(clientId);
      if (client && client.ws.readyState === WebSocket.OPEN) {
        client.ws.send(payload);
        sent++;
      }
    }

    return createResponse(message, { sent });
  }

  private async handleSend(message: Message): Promise<MessageResponse> {
    const { clientId, data } = message.payload;

    const client = this.clients.get(clientId);
    if (!client) {
      return createErrorResponse(message, `Client not found: ${clientId}`);
    }

    if (client.ws.readyState !== WebSocket.OPEN) {
      return createErrorResponse(message, `Client not connected: ${clientId}`);
    }

    client.ws.send(JSON.stringify(data));

    return createResponse(message, { sent: true });
  }

  private async handleClients(message: Message): Promise<MessageResponse> {
    const { channel } = message.payload;

    const clients = Array.from(this.clients.values())
      .filter(client => !channel || client.channels.has(channel))
      .map(client => ({
        id: client.id,
        channels: Array.from(client.channels),
        connectedAt: client.connectedAt
      }));

    return createResponse(message, { clients });
  }

  private subscribeClient(clientId: string, channel: string): void {
    const client = this.clients.get(clientId);
    if (!client) return;

    client.channels.add(channel);

    if (!this.channels.has(channel)) {
      this.channels.set(channel, new Set());
    }

    this.channels.get(channel)!.add(clientId);

    this.logInfo('Client subscribed', { clientId, channel });
  }

  private unsubscribeClient(clientId: string, channel: string): void {
    const client = this.clients.get(clientId);
    if (client) {
      client.channels.delete(channel);
    }

    const channelClients = this.channels.get(channel);
    if (channelClients) {
      channelClients.delete(clientId);
      if (channelClients.size === 0) {
        this.channels.delete(channel);
      }
    }

    this.logInfo('Client unsubscribed', { clientId, channel });
  }
}

interface WSClient {
  id: string;
  ws: ServerWebSocket;
  channels: Set<string>;
  connectedAt: number;
}
```

---

## SSEServerActor Design

### Configuration

```typescript
export interface SSEServerConfig {
  /** Port to listen on */
  port: number;

  /** Maximum concurrent connections */
  maxConnections: number;

  /** Keep-alive interval (prevent timeout) */
  keepAliveInterval: number; // Default: 30000 (30 seconds)

  /** Channels configuration */
  channels: {
    [channel: string]: {
      publishers: string[];
    };
  };
}
```

### Message Protocol

**sse-server.listen** (Start SSE server)
```typescript
{
  type: 'sse-server.listen',
  payload: {
    port?: number
  }
}

// Response
{
  success: true,
  payload: {
    port: 3002,
    listening: true
  }
}
```

**sse-server.send** (Send event to channel)
```typescript
{
  type: 'sse-server.send',
  payload: {
    channel: 'notifications',
    event: 'task-update',
    data: { id: '123', status: 'completed' }
  }
}

// Response
{
  success: true,
  payload: {
    sent: 8 // Number of clients
  }
}
```

### SSE Response Format

```
HTTP/1.1 200 OK
Content-Type: text/event-stream
Cache-Control: no-cache
Connection: keep-alive

event: task-update
data: {"id":"123","status":"completed"}

event: heartbeat
data: {}
```

### Implementation Sketch

```typescript
export class SSEServerActor extends Actor {
  private server?: Server;
  private config: SSEServerConfig;
  private clients: Map<string, SSEClient>;
  private channels: Map<string, Set<string>>;
  private heartbeatTimer?: Timer;

  constructor(id: string, router: MessageRouter, config: SSEServerConfig) {
    super(id, router);
    this.config = config;
    this.clients = new Map();
    this.channels = new Map();
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'sse-server.listen') {
      return await this.handleListen(message);
    }

    if (message.type === 'sse-server.send') {
      return await this.handleSend(message);
    }

    return createErrorResponse(message, `Unknown message type: ${message.type}`);
  }

  private async handleListen(message: Message): Promise<MessageResponse> {
    const port = message.payload.port ?? this.config.port;

    this.server = Bun.serve({
      port,
      fetch: (req) => this.handleRequest(req),
    });

    // Start heartbeat timer
    this.heartbeatTimer = setInterval(
      () => this.sendHeartbeat(),
      this.config.keepAliveInterval
    );

    this.logInfo('SSE server listening', { port });

    return createResponse(message, {
      port,
      listening: true
    });
  }

  private handleRequest(req: Request): Response {
    const url = new URL(req.url);
    const channel = url.pathname.replace('/', '');

    if (!this.config.channels[channel]) {
      return new Response('Channel not found', { status: 404 });
    }

    const clientId = `sse-${Math.random().toString(36).substring(7)}`;

    // Create SSE stream
    const stream = new ReadableStream({
      start: (controller) => {
        this.clients.set(clientId, {
          id: clientId,
          channel,
          controller,
          connectedAt: Date.now()
        });

        if (!this.channels.has(channel)) {
          this.channels.set(channel, new Set());
        }
        this.channels.get(channel)!.add(clientId);

        this.logInfo('SSE client connected', { clientId, channel });
      },
      cancel: () => {
        this.removeClient(clientId);
      }
    });

    return new Response(stream, {
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive'
      }
    });
  }

  private async handleSend(message: Message): Promise<MessageResponse> {
    const { channel, event, data } = message.payload;

    const clientIds = this.channels.get(channel);
    if (!clientIds || clientIds.size === 0) {
      return createResponse(message, { sent: 0 });
    }

    const eventData = `event: ${event}\ndata: ${JSON.stringify(data)}\n\n`;
    let sent = 0;

    for (const clientId of clientIds) {
      const client = this.clients.get(clientId);
      if (client) {
        try {
          client.controller.enqueue(new TextEncoder().encode(eventData));
          sent++;
        } catch (error) {
          this.logError('Failed to send to client', { clientId, error });
          this.removeClient(clientId);
        }
      }
    }

    return createResponse(message, { sent });
  }

  private sendHeartbeat(): void {
    const heartbeat = `event: heartbeat\ndata: {}\n\n`;

    for (const client of this.clients.values()) {
      try {
        client.controller.enqueue(new TextEncoder().encode(heartbeat));
      } catch (error) {
        this.removeClient(client.id);
      }
    }
  }

  private removeClient(clientId: string): void {
    const client = this.clients.get(clientId);
    if (client) {
      const channelClients = this.channels.get(client.channel);
      if (channelClients) {
        channelClients.delete(clientId);
        if (channelClients.size === 0) {
          this.channels.delete(client.channel);
        }
      }
    }

    this.clients.delete(clientId);
    this.logInfo('SSE client disconnected', { clientId });
  }
}

interface SSEClient {
  id: string;
  channel: string;
  controller: ReadableStreamDefaultController;
  connectedAt: number;
}
```

---

## Integration Patterns

### Pattern 1: RESTful Task API

**Setup:**
```typescript
// HTTPServerActor listens on port 3000
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' }
  }
});

router.registerActor('/system/http-server', httpServer);

// TaskActor registers routes
class TaskActor extends Actor {
  async onInit() {
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'GET',
        path: '/api/tasks/:id',
        actor: this.address,
        message: 'http.get'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.get') {
      const { id } = message.payload.params;
      const task = await this.getTask(id);
      return createResponse(message, task);
    }
  }
}
```

**Usage:**
```bash
curl http://localhost:3000/api/tasks/123
# → { "id": "123", "title": "Task title", "status": "open" }
```

---

### Pattern 2: Real-Time Dashboard with WebSocket

**Setup:**
```typescript
// WebSocketServerActor listens on port 3001
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  maxConnections: 100,
  channels: {
    'tasks': { publishers: ['/tasks'] }
  }
});

router.registerActor('/system/ws-server', wsServer);

// TaskActor broadcasts updates
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'task.update') {
      // Update task
      const task = await this.updateTask(message.payload);

      // Broadcast to WebSocket clients
      await this.tell(
        address('/system/ws-server'),
        'ws-server.broadcast',
        { channel: 'tasks', data: task }
      );

      return createResponse(message, task);
    }
  }
}
```

**Client:**
```javascript
const ws = new WebSocket('ws://localhost:3001');

ws.onopen = () => {
  ws.send(JSON.stringify({ type: 'subscribe', channel: 'tasks' }));
};

ws.onmessage = (event) => {
  const { channel, data } = JSON.parse(event.data);
  console.log('Task update:', data);
  // Update UI
};
```

---

### Pattern 3: Server-Sent Events for Notifications

**Setup:**
```typescript
// SSEServerActor listens on port 3002
const sseServer = new SSEServerActor('sse-server', router, {
  port: 3002,
  maxConnections: 1000,
  keepAliveInterval: 30000,
  channels: {
    'notifications': { publishers: ['/notifications'] }
  }
});

router.registerActor('/system/sse-server', sseServer);

// NotificationActor sends events
class NotificationActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'notification.send') {
      await this.tell(
        address('/system/sse-server'),
        'sse-server.send',
        {
          channel: 'notifications',
          event: 'notification',
          data: message.payload
        }
      );

      return createResponse(message, { sent: true });
    }
  }
}
```

**Client:**
```javascript
const eventSource = new EventSource('http://localhost:3002/notifications');

eventSource.addEventListener('notification', (event) => {
  const data = JSON.parse(event.data);
  console.log('Notification:', data);
  // Show notification in UI
});
```

---

### Pattern 4: Service Mesh (Actors Calling Each Other via HTTP)

**Internal HTTP communication:**
```typescript
// HTTPServerActor serves internal APIs
const internalHttp = new HTTPServerActor('internal-http', router, {
  port: 4000,
  namespaces: {
    '/internal/tasks': { namespace: '/tasks', prefix: '/internal/tasks' }
  }
});

// WorkflowActor calls TaskActor via HTTP
class WorkflowActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'workflow.run') {
      // Call internal Task API
      const taskResponse = await this.ask(
        address('/workflows/system/http'),
        'http.post',
        {
          url: 'http://localhost:4000/internal/tasks',
          body: { title: 'Task from workflow' }
        }
      );

      return createResponse(message, { task: taskResponse.payload });
    }
  }
}
```

**Rationale:**
- Enables distributed deployments (actors in different processes/machines)
- Standard HTTP interface for inter-actor communication
- Can use load balancing, service discovery

---

## Security Model

### Namespace Isolation

**Rule:** Only actors in a namespace can register routes for that namespace.

**Example:**
```typescript
// TaskActor in /tasks namespace
class TaskActor extends Actor {
  async onInit() {
    // ✅ Allowed - registering route in own namespace
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      { method: 'GET', path: '/api/tasks/:id', actor: this.address }
    );

    // ❌ Denied - registering route in different namespace
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      { method: 'GET', path: '/api/workflows/:id', actor: this.address }
    );
  }
}
```

### CORS Configuration

**Per-namespace CORS:**
```typescript
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/workflows': {
      namespace: '/workflows',
      prefix: '/api/workflows',
      cors: {
        allowedOrigins: ['https://app.example.com'],
        allowedMethods: ['GET', 'POST'],
        credentials: true
      }
    },
    '/api/public': {
      namespace: '/public',
      prefix: '/api/public',
      cors: {
        allowedOrigins: ['*'],
        allowedMethods: ['GET'],
        credentials: false
      }
    }
  }
});
```

### Rate Limiting

**Per-client rate limiting:**
```typescript
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  rateLimit: {
    requests: 100,
    window: 60000, // 100 requests per minute
    keyBy: 'ip'    // Rate limit by IP address
  }
});
```

### Authentication

**Delegate to AuthActor:**
```typescript
class HTTPServerActor extends Actor {
  private async handleRequest(req: Request): Promise<Response> {
    // Check if route requires auth
    if (route.requireAuth) {
      const authResponse = await this.ask(
        address('/system/auth'),
        'auth.validate',
        { token: req.headers.get('authorization') }
      );

      if (!authResponse.success) {
        return new Response('Unauthorized', { status: 401 });
      }
    }

    // Continue with request
  }
}
```

---

## Benefits

### 1. Complete Actor-Based System

**Before (client only):**
- HTTPClientActor: Make outbound requests
- WebSocketActor: Connect to external WebSocket servers
- No way to accept incoming connections

**After (client + server):**
- HTTPClientActor: Outbound HTTP
- HTTPServerActor: Inbound HTTP
- WebSocketActor: Outbound WebSocket
- WebSocketServerActor: Inbound WebSocket
- SSEServerActor: Server-sent events

**Result:** Actors can both consume and provide HTTP/WebSocket services.

---

### 2. RESTful APIs as Actors

**Pattern:**
```typescript
class TaskAPIActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // GET /tasks
    if (message.type === 'http.list') {
      return createResponse(message, await this.listTasks());
    }

    // GET /tasks/:id
    if (message.type === 'http.get') {
      return createResponse(message, await this.getTask(message.payload.params.id));
    }

    // POST /tasks
    if (message.type === 'http.create') {
      return createResponse(message, await this.createTask(message.payload.body));
    }

    // PUT /tasks/:id
    if (message.type === 'http.update') {
      return createResponse(message, await this.updateTask(
        message.payload.params.id,
        message.payload.body
      ));
    }

    // DELETE /tasks/:id
    if (message.type === 'http.delete') {
      return createResponse(message, await this.deleteTask(message.payload.params.id));
    }
  }
}
```

**Benefits:**
- RESTful endpoints map naturally to actor messages
- HTTP method + path → actor message type
- Clean separation of HTTP concerns from business logic

---

### 3. Real-Time Updates

**WebSocket broadcasting:**
```typescript
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'task.update') {
      const task = await this.updateTask(message.payload);

      // Broadcast to all subscribed clients
      await this.tell(
        address('/system/ws-server'),
        'ws-server.broadcast',
        { channel: 'tasks', data: task }
      );

      return createResponse(message, task);
    }
  }
}
```

**Benefits:**
- Real-time dashboards
- Live notifications
- Collaborative editing
- Actors don't need to manage WebSocket connections

---

### 4. Widget Actors Can Serve Endpoints

**Pattern:**
```typescript
class DashboardWidgetActor extends Actor {
  async onInit() {
    // Widget registers its own HTTP endpoint
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'GET',
        path: '/api/widgets/dashboard/data',
        actor: this.address,
        message: 'http.get-data'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.get-data') {
      const data = await this.getDashboardData();
      return createResponse(message, data);
    }
  }
}
```

**Benefits:**
- Widgets can expose their own APIs
- Self-contained components (UI + API)
- Dynamic route registration

---

## Comparison: Client vs Server Actors

| Feature | Client Actor | Server Actor |
|---------|-------------|--------------|
| **Direction** | Outbound (actor → external) | Inbound (external → actor) |
| **HTTPClientActor** | Make HTTP requests | - |
| **HTTPServerActor** | - | Accept HTTP requests |
| **WebSocketActor** | Connect to WS servers | - |
| **WebSocketServerActor** | - | Accept WS connections |
| **SSEServerActor** | - | Stream events to browsers |
| **Use Case** | Call external APIs | Serve APIs to browsers/clients |
| **Example** | Fetch GitHub issues | Serve task API to frontend |

---

## Examples

### Example 1: Task Management API

**Setup:**
```typescript
// 1. Start HTTP server
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' }
  }
});
router.registerActor('/system/http-server', httpServer);

await httpServer.ask(address('/system/http-server'), 'http-server.listen', {});

// 2. TaskActor registers routes
class TaskActor extends Actor {
  async onInit() {
    await this.registerRoutes();
  }

  async registerRoutes() {
    const routes = [
      { method: 'GET', path: '/api/tasks', message: 'http.list' },
      { method: 'GET', path: '/api/tasks/:id', message: 'http.get' },
      { method: 'POST', path: '/api/tasks', message: 'http.create' },
      { method: 'PUT', path: '/api/tasks/:id', message: 'http.update' },
      { method: 'DELETE', path: '/api/tasks/:id', message: 'http.delete' }
    ];

    for (const route of routes) {
      await this.ask(
        address('/system/http-server'),
        'http-server.route',
        { ...route, actor: this.address }
      );
    }
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.list') {
      return createResponse(message, await this.listTasks());
    }

    if (message.type === 'http.get') {
      return createResponse(message, await this.getTask(message.payload.params.id));
    }

    if (message.type === 'http.create') {
      return createResponse(message, await this.createTask(message.payload.body));
    }

    // ... etc
  }
}
```

**Usage:**
```bash
# List tasks
curl http://localhost:3000/api/tasks

# Get task
curl http://localhost:3000/api/tasks/123

# Create task
curl -X POST http://localhost:3000/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"New task"}'

# Update task
curl -X PUT http://localhost:3000/api/tasks/123 \
  -H "Content-Type: application/json" \
  -d '{"status":"completed"}'

# Delete task
curl -X DELETE http://localhost:3000/api/tasks/123
```

---

### Example 2: Real-Time Dashboard

**Setup:**
```typescript
// 1. Start WebSocket server
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  maxConnections: 100,
  channels: {
    'dashboard': { publishers: ['/dashboard'] }
  }
});
router.registerActor('/system/ws-server', wsServer);

await wsServer.ask(address('/system/ws-server'), 'ws-server.listen', {});

// 2. DashboardActor broadcasts updates
class DashboardActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'metrics.update') {
      // Broadcast to all dashboard clients
      await this.tell(
        address('/system/ws-server'),
        'ws-server.broadcast',
        {
          channel: 'dashboard',
          data: {
            cpuUsage: 45,
            memoryUsage: 60,
            activeTasks: 12
          }
        }
      );

      return createResponse(message, { broadcasted: true });
    }
  }
}
```

**Client:**
```html
<!DOCTYPE html>
<html>
<head>
  <title>Dashboard</title>
</head>
<body>
  <div id="dashboard">
    <p>CPU: <span id="cpu">-</span>%</p>
    <p>Memory: <span id="memory">-</span>%</p>
    <p>Tasks: <span id="tasks">-</span></p>
  </div>

  <script>
    const ws = new WebSocket('ws://localhost:3001');

    ws.onopen = () => {
      ws.send(JSON.stringify({ type: 'subscribe', channel: 'dashboard' }));
    };

    ws.onmessage = (event) => {
      const { data } = JSON.parse(event.data);
      document.getElementById('cpu').textContent = data.cpuUsage;
      document.getElementById('memory').textContent = data.memoryUsage;
      document.getElementById('tasks').textContent = data.activeTasks;
    };
  </script>
</body>
</html>
```

---

### Example 3: Live Notifications with SSE

**Setup:**
```typescript
// 1. Start SSE server
const sseServer = new SSEServerActor('sse-server', router, {
  port: 3002,
  maxConnections: 1000,
  keepAliveInterval: 30000,
  channels: {
    'notifications': { publishers: ['/notifications'] }
  }
});
router.registerActor('/system/sse-server', sseServer);

await sseServer.ask(address('/system/sse-server'), 'sse-server.listen', {});

// 2. NotificationActor sends events
class NotificationActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'notification.send') {
      await this.tell(
        address('/system/sse-server'),
        'sse-server.send',
        {
          channel: 'notifications',
          event: 'notification',
          data: {
            id: message.payload.id,
            title: message.payload.title,
            message: message.payload.message,
            timestamp: Date.now()
          }
        }
      );

      return createResponse(message, { sent: true });
    }
  }
}
```

**Client:**
```html
<!DOCTYPE html>
<html>
<head>
  <title>Notifications</title>
</head>
<body>
  <div id="notifications"></div>

  <script>
    const eventSource = new EventSource('http://localhost:3002/notifications');

    eventSource.addEventListener('notification', (event) => {
      const notification = JSON.parse(event.data);

      const div = document.createElement('div');
      div.innerHTML = `
        <strong>${notification.title}</strong>
        <p>${notification.message}</p>
        <small>${new Date(notification.timestamp).toLocaleString()}</small>
      `;

      document.getElementById('notifications').prepend(div);
    });

    eventSource.onerror = (error) => {
      console.error('SSE error:', error);
    };
  </script>
</body>
</html>
```

---

## Testing Strategy

### HTTPServerActor Tests

1. **Route Registration:**
   - Register static routes
   - Register dynamic routes
   - Prevent duplicate routes
   - Namespace isolation (can't register in other namespace)

2. **Request Routing:**
   - Match exact paths
   - Match parameterized paths (/tasks/:id)
   - Extract path params
   - Parse query strings
   - Parse JSON body

3. **CORS:**
   - Allow configured origins
   - Deny non-configured origins
   - Preflight requests (OPTIONS)

4. **Rate Limiting:**
   - Allow requests within limit
   - Deny requests exceeding limit
   - Rate limit by IP
   - Rate limit reset

5. **Error Handling:**
   - 404 for unknown routes
   - 400 for invalid requests
   - 500 for actor errors

---

### WebSocketServerActor Tests

1. **Connection Management:**
   - Accept connections
   - Track connected clients
   - Handle disconnections
   - Max connections limit

2. **Channel Subscription:**
   - Subscribe to channels
   - Unsubscribe from channels
   - List clients in channel

3. **Broadcasting:**
   - Broadcast to channel
   - Send to specific client
   - Handle disconnected clients

4. **Error Handling:**
   - Invalid subscription
   - Client not found
   - Connection errors

---

### SSEServerActor Tests

1. **Connection Management:**
   - Accept SSE connections
   - Track connected clients
   - Handle disconnections

2. **Event Sending:**
   - Send events to channel
   - Format SSE messages correctly
   - Handle client errors

3. **Keep-Alive:**
   - Send heartbeat events
   - Prevent connection timeout

---

## Implementation Plan

### Phase 1: HTTPServerActor (3-4 days)
- [ ] Implement HTTPServerActor
- [ ] Route registration (static + dynamic)
- [ ] Request parsing (params, query, body)
- [ ] CORS support
- [ ] Rate limiting
- [ ] Tests (>15 tests)

### Phase 2: WebSocketServerActor (2-3 days)
- [ ] Implement WebSocketServerActor
- [ ] Connection management
- [ ] Channel subscription
- [ ] Broadcasting
- [ ] Tests (>10 tests)

### Phase 3: SSEServerActor (1-2 days)
- [ ] Implement SSEServerActor
- [ ] Event streaming
- [ ] Keep-alive heartbeat
- [ ] Tests (>8 tests)

### Phase 4: Integration Examples (1 day)
- [ ] Task API example
- [ ] Real-time dashboard example
- [ ] Notifications example
- [ ] Documentation

**Total Estimated Time:** 7-10 days

---

## Future Enhancements

### 1. GraphQL Support

**GraphQLServerActor:**
```typescript
const graphqlServer = new GraphQLServerActor('graphql', router, {
  port: 4000,
  schema: `
    type Query {
      task(id: ID!): Task
      tasks: [Task]
    }

    type Mutation {
      createTask(title: String!): Task
    }

    type Subscription {
      taskUpdated: Task
    }
  `
});
```

### 2. HTTP/2 and gRPC

**gRPCServerActor:**
```typescript
const grpcServer = new gRPCServerActor('grpc', router, {
  port: 5000,
  services: {
    TaskService: {
      GetTask: '/tasks',
      ListTasks: '/tasks',
      CreateTask: '/tasks'
    }
  }
});
```

### 3. Static File Serving

**StaticFileServerActor:**
```typescript
const staticServer = new StaticFileServerActor('static', router, {
  port: 8080,
  root: '/public',
  cache: true
});
```

### 4. API Gateway Pattern

**APIGatewayActor:**
```typescript
// Route to multiple backend actors
const gateway = new APIGatewayActor('gateway', router, {
  port: 80,
  routes: {
    '/tasks': address('/tasks-service/http-server'),
    '/workflows': address('/workflows-service/http-server'),
    '/users': address('/users-service/http-server')
  }
});
```

---

## Summary

**Server actors enable:**

1. **Complete Actor System:**
   - Client actors (outbound): HTTPClientActor, WebSocketActor
   - Server actors (inbound): HTTPServerActor, WebSocketServerActor, SSEServerActor

2. **RESTful APIs as Actors:**
   - HTTP method + path → actor message
   - Actors serve HTTP endpoints
   - Dynamic route registration

3. **Real-Time Updates:**
   - WebSocket broadcasting (1:N)
   - SSE streaming (server → client)
   - Channel-based pub/sub

4. **Widget Actors Can Serve Endpoints:**
   - Widgets register their own APIs
   - Self-contained components
   - Dynamic capabilities

5. **Service Mesh:**
   - Actors calling each other via HTTP
   - Distributed deployments
   - Standard HTTP interface

**Key Design Decisions:**
- Hybrid routing (path-based + API actors)
- Path-based namespace isolation
- Internal validation with actor delegation
- Port-based broadcasting for WebSocket
- Channel-based pub/sub for SSE

**Next Steps:**
1. Review and approve design
2. Implement HTTPServerActor (Phase 1)
3. Implement WebSocketServerActor (Phase 2)
4. Implement SSEServerActor (Phase 3)
5. Create integration examples (Phase 4)

---

**Document End**
