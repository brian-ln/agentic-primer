# Server Actors: Key Design Decisions

**Date:** 2026-02-07
**Purpose:** Document critical architectural decisions and their rationale

---

## Decision 1: Routing Pattern

### Question
How do incoming HTTP requests map to actor messages?

### Options Considered

**Option A: Path-based routing**
```typescript
// Incoming: GET /tasks/123
// Routes to: TaskActor(123).receive({ type: 'http.get', ... })

router.addRoute('GET', '/tasks/:id', {
  actor: (params) => address(`/tasks/${params.id}`),
  message: 'http.get'
});
```

**Pros:**
- Natural RESTful mapping (URL → actor)
- Each entity has its own actor instance
- Scales well (actors created on-demand)

**Cons:**
- Requires dynamic actor creation
- May create too many actors

**Option B: Dedicated API actors**
```typescript
// Incoming: GET /api/tasks/123
// Routes to: TaskAPIActor.receive({ type: 'get-task', id: 123 })

router.addRoute('GET', '/api/tasks/:id', {
  actor: address('/api/tasks'),
  message: 'get-task'
});
```

**Pros:**
- Single actor handles all requests for a resource type
- Easier to manage (fewer actors)
- Better for caching, connection pooling

**Cons:**
- Actor becomes bottleneck if not careful
- Less natural than path-based

**Option C: Hybrid (both patterns)**
```typescript
// Pattern A for entities
router.addRoute('GET', '/tasks/:id', {
  actor: (params) => address(`/tasks/${params.id}`),
  message: 'http.get'
});

// Pattern B for collections
router.addRoute('GET', '/api/tasks', {
  actor: address('/api/tasks'),
  message: 'http.list'
});
```

### Decision: Hybrid (Option C)

**Rationale:**
- Pattern A is perfect for CRUD on individual entities (GET /tasks/123)
- Pattern B is better for collection operations (GET /api/tasks, POST /api/tasks)
- Supporting both gives maximum flexibility
- Each team can choose the pattern that fits their use case

**Implementation:**
```typescript
// HTTPServerActor supports both patterns
class HTTPServerActor extends Actor {
  private matchRoute(method: string, url: string): RouteHandler | null {
    for (const [pattern, handler] of this.routes) {
      if (handler.method !== method) continue;

      const match = this.matchPattern(pattern, url);
      if (match) {
        // If actor is a function, call it with params
        const actorAddress = typeof handler.actor === 'function'
          ? handler.actor(match)
          : handler.actor;

        return { ...handler, actor: actorAddress, params: match };
      }
    }
    return null;
  }
}
```

---

## Decision 2: Port Assignment Strategy

### Question
How do namespaces share or isolate server ports?

### Options Considered

**Option A: One server per namespace**
```typescript
// Each namespace gets its own port
const tasksHttp = new HTTPServerActor('tasks-http', router, { port: 3001 });
const workflowsHttp = new HTTPServerActor('workflows-http', router, { port: 3002 });
const domainHttp = new HTTPServerActor('domain-http', router, { port: 3003 });
```

**Pros:**
- Complete isolation
- Easy to firewall (block ports)
- Independent scaling

**Cons:**
- Multiple ports to manage
- Harder for clients (which port?)
- More complex deployment

**Option B: Shared server with path prefixes**
```typescript
// All namespaces share one server
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' },
    '/api/workflows': { namespace: '/workflows', prefix: '/api/workflows' },
    '/api/domain': { namespace: '/domain', prefix: '/api/domain' }
  }
});
```

**Pros:**
- Single port (simpler for clients)
- Path-based routing (standard)
- Easier CORS, auth middleware

**Cons:**
- All namespaces exposed on same port
- Less isolation

**Option C: Hybrid (public on shared, internal on isolated)**
```typescript
// Public API: shared server
const publicHttp = new HTTPServerActor('public-http', router, { port: 80 });

// Internal APIs: isolated servers
const tasksHttp = new HTTPServerActor('tasks-http', router, { port: 4001 });
const workflowsHttp = new HTTPServerActor('workflows-http', router, { port: 4002 });
```

### Decision: Path-based namespace isolation (Option B)

**Rationale:**
- Simpler for clients (one port, path-based routing)
- Standard RESTful pattern
- Can still spin up isolated servers if needed
- Easier to add global middleware (CORS, auth)
- Matches industry standards (Express, Hono, etc.)

**Request Flow:**
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

**Security:**
- Path prefix determines namespace
- Actors can only register routes in their own namespace
- Prevents route hijacking

---

## Decision 3: Middleware Architecture

### Question
How do we handle cross-cutting concerns (auth, CORS, rate limiting)?

### Options Considered

**Option A: Middleware chain (Express-style)**
```typescript
// Request flows through middleware stack
const httpServer = new HTTPServerActor('http-server', router, {
  middleware: [
    corsMiddleware,
    authMiddleware,
    rateLimitMiddleware,
    loggingMiddleware
  ]
});

// Each middleware is a function
function corsMiddleware(req: Request, next: Function) {
  if (!validateCORS(req)) {
    return new Response('CORS not allowed', { status: 403 });
  }
  return next();
}
```

**Pros:**
- Familiar pattern (Express, Hono)
- Composable middleware
- Easy to add/remove middleware

**Cons:**
- Breaks pure actor model (middleware are functions, not actors)
- Hard to test middleware in isolation
- Implicit ordering (fragile)

**Option B: Actor-based middleware**
```typescript
// Each middleware is an actor
const corsActor = new CORSActor('cors', router, { allowedOrigins: ['*'] });
const authActor = new AuthActor('auth', router, { validateTokens: true });

const httpServer = new HTTPServerActor('http-server', router, {
  middleware: [
    address('/system/cors'),
    address('/system/auth'),
    address('/system/rate-limit'),
    address('/system/logger')
  ]
});

// HTTPServerActor sends request through middleware chain
for (const middlewareAddr of this.middleware) {
  const response = await this.ask(middlewareAddr, 'middleware.handle', { req });
  if (!response.success) {
    return createErrorResponse(message, response.error);
  }
}
```

**Pros:**
- Pure actor model (everything is actors)
- Easy to test (mock actors)
- Explicit message passing

**Cons:**
- More complex than functions
- Overhead (multiple ask calls)
- May be overkill for simple validation

**Option C: Internal validation + actor delegation**
```typescript
// Simple operations (CORS, rate limiting) are internal
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

    // 4. Route to handler actor
    return await this.routeRequest(req);
  }
}
```

### Decision: Internal validation + actor delegation (Option C)

**Rationale:**
- Consistent with HTTPClientActor design (internal validation)
- Simple operations (CORS, rate limiting) don't need actors
- Complex operations (auth, logging) delegate to system actors
- Clearer than middleware chain (explicit ask/tell)
- Avoids overhead of multiple actor calls for simple checks

**Implementation:**
```typescript
class HTTPServerActor extends Actor {
  private validateCORS(req: Request): boolean {
    if (!this.config.cors) return true;

    const origin = req.headers.get('origin');
    if (!origin) return true;

    return this.config.cors.allowedOrigins.includes(origin) ||
           this.config.cors.allowedOrigins.includes('*');
  }

  private async authenticateRequest(req: Request): Promise<MessageResponse> {
    // Delegate to AuthActor
    return await this.ask(
      address('/system/auth'),
      'auth.validate',
      { token: req.headers.get('authorization') }
    );
  }
}
```

---

## Decision 4: WebSocket Connection Management

### Question
How do actors broadcast to WebSocket clients?

### Options Considered

**Option A: Actors track individual connections**
```typescript
class TaskActor extends Actor {
  private wsConnections: Set<WebSocket> = new Set();

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'ws.connect') {
      this.wsConnections.add(message.payload.ws);
    }

    if (message.type === 'task.update') {
      // Broadcast to all connections
      for (const ws of this.wsConnections) {
        ws.send(JSON.stringify({ task: message.payload }));
      }
    }
  }
}
```

**Pros:**
- Direct control over connections
- No intermediary

**Cons:**
- Actors coupled to WebSocket implementation
- Hard to test (need real WebSocket)
- Doesn't scale (one actor per connection?)

**Option B: WebSocketServerActor manages connections**
```typescript
// WebSocketServerActor tracks connections
class WebSocketServerActor extends Actor {
  private connections: Map<string, WebSocket> = new Map();

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'ws-server.broadcast') {
      const { channel, data } = message.payload;

      // Send to all connections in channel
      for (const [clientId, ws] of this.connections) {
        if (this.isSubscribed(clientId, channel)) {
          ws.send(JSON.stringify({ channel, data }));
        }
      }
    }
  }
}

// TaskActor broadcasts via WebSocketServerActor
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'task.update') {
      // Broadcast to WebSocket clients
      await this.tell(
        address('/system/ws-server'),
        'ws-server.broadcast',
        { channel: 'tasks', data: message.payload }
      );
    }
  }
}
```

### Decision: Port-based broadcasting (Option B)

**Rationale:**
- Decouples actors from WebSocket implementation details
- Actors don't need to track individual connections
- Channel-based broadcasting is simpler than connection-based
- Natural fit with pub/sub patterns
- Actors send to channels, not individual clients
- WebSocketServerActor handles connection lifecycle

**Implementation:**
```typescript
// TaskActor doesn't know about WebSocket connections
await this.tell(
  address('/system/ws-server'),
  'ws-server.broadcast',
  { channel: 'tasks', data: { id: '123', status: 'completed' } }
);

// WebSocketServerActor handles the complexity
class WebSocketServerActor extends Actor {
  private clients: Map<string, WSClient> = new Map();
  private channels: Map<string, Set<string>> = new Map(); // channel -> clientIds

  private async handleBroadcast(message: Message): Promise<MessageResponse> {
    const { channel, data } = message.payload;
    const clientIds = this.channels.get(channel);

    if (!clientIds) {
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
}
```

---

## Decision 5: SSE vs WebSocket

### Question
When to use Server-Sent Events vs WebSocket?

### Comparison

| Feature | SSE | WebSocket |
|---------|-----|-----------|
| **Direction** | One-way (server → client) | Two-way (bidirectional) |
| **Protocol** | HTTP | WebSocket protocol |
| **Reconnection** | Automatic (browser) | Manual (JavaScript) |
| **Complexity** | Simple | More complex |
| **Use Case** | Notifications, live updates | Chat, real-time dashboards |

### Decision: Provide both SSEServerActor and WebSocketServerActor

**Rationale:**
- **SSE for notifications:** Simpler, automatic reconnection
- **WebSocket for real-time:** Bidirectional, lower latency
- Different tools for different jobs
- Actors can choose the right tool

**When to use SSE:**
- Live notifications
- Server → client streaming (stock prices, logs)
- Automatic reconnection needed
- Simpler is better

**When to use WebSocket:**
- Chat applications
- Real-time dashboards (bidirectional)
- Game state synchronization
- Low latency required

**Example:**
```typescript
// SSE for notifications
const sseServer = new SSEServerActor('sse-server', router, {
  port: 3002,
  channels: { 'notifications': { publishers: ['/notifications'] } }
});

// WebSocket for dashboard
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  channels: { 'dashboard': { publishers: ['/dashboard'] } }
});
```

---

## Decision 6: Dynamic vs Static Route Registration

### Question
Should routes be registered statically at startup or dynamically by actors?

### Options Considered

**Option A: Static (configuration)**
```typescript
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  routes: [
    { method: 'GET', path: '/tasks/:id', actor: '/tasks', message: 'http.get' },
    { method: 'POST', path: '/tasks', actor: '/tasks', message: 'http.create' }
  ]
});
```

**Pros:**
- All routes visible at startup
- Easy to audit
- No runtime changes

**Cons:**
- Inflexible (can't add routes dynamically)
- Hard to build modular systems

**Option B: Dynamic (actors register their own routes)**
```typescript
class TaskActor extends Actor {
  async onInit() {
    // Actor registers its own routes
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      { method: 'GET', path: '/tasks/:id', actor: this.address, message: 'http.get' }
    );
  }
}
```

**Pros:**
- Modular (actors own their routes)
- Flexible (add/remove routes dynamically)
- Self-contained

**Cons:**
- Routes registered at runtime
- Harder to audit (routes scattered)

### Decision: Support both (hybrid)

**Rationale:**
- Static for simple setups (all routes in one place)
- Dynamic for modular systems (actors register their own routes)
- Maximum flexibility
- Each team can choose their preferred style

**Implementation:**
```typescript
// Static routes
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  routes: [
    { method: 'GET', path: '/health', actor: '/system/health', message: 'http.health' }
  ]
});

// Dynamic route registration
class TaskActor extends Actor {
  async onInit() {
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      { method: 'GET', path: '/tasks/:id', actor: this.address, message: 'http.get' }
    );
  }
}
```

---

## Decision 7: Error Handling Strategy

### Question
How do server actors report errors?

### Options Considered

**Option A: HTTP status codes only**
```typescript
// Return HTTP error responses
if (!authenticated) {
  return new Response('Unauthorized', { status: 401 });
}
```

**Pros:**
- Standard HTTP semantics
- Simple

**Cons:**
- Loses actor response structure
- Hard to distinguish error types

**Option B: Actor error responses**
```typescript
// Return actor error response
if (!authenticated) {
  return createErrorResponse(message, 'Unauthorized');
}

// HTTPServerActor converts to HTTP response
private toHTTPResponse(response: MessageResponse): Response {
  if (!response.success) {
    return new Response(
      JSON.stringify({ error: response.error }),
      { status: 400, headers: { 'Content-Type': 'application/json' } }
    );
  }
  // ...
}
```

### Decision: Actor error responses + HTTP conversion (Option B)

**Rationale:**
- Consistent with actor model (all responses are MessageResponse)
- Easy to test (no HTTP objects in business logic)
- HTTPServerActor handles HTTP conversion
- Clear separation of concerns

**Implementation:**
```typescript
// Business actor returns actor response
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.get') {
      const task = await this.getTask(message.payload.params.id);

      if (!task) {
        return createErrorResponse(message, 'Task not found');
      }

      return createResponse(message, task);
    }
  }
}

// HTTPServerActor converts to HTTP response
class HTTPServerActor extends Actor {
  private toHTTPResponse(response: MessageResponse): Response {
    if (!response.success) {
      const status = this.mapErrorToStatus(response.error);
      return new Response(
        JSON.stringify({ error: response.error }),
        { status, headers: { 'Content-Type': 'application/json' } }
      );
    }

    return new Response(
      JSON.stringify(response.payload),
      { status: 200, headers: { 'Content-Type': 'application/json' } }
    );
  }

  private mapErrorToStatus(error: string): number {
    if (error.includes('not found')) return 404;
    if (error.includes('unauthorized')) return 401;
    if (error.includes('forbidden')) return 403;
    return 400;
  }
}
```

---

## Summary: Decision Matrix

| Decision | Option Chosen | Rationale |
|----------|---------------|-----------|
| **Routing Pattern** | Hybrid (path-based + API actors) | Maximum flexibility |
| **Port Assignment** | Path-based namespace isolation | Simpler for clients, standard RESTful |
| **Middleware** | Internal validation + actor delegation | Consistent with client actors, clear |
| **WebSocket Broadcasting** | Port-based (channel broadcasting) | Decouples actors from connections |
| **SSE vs WebSocket** | Both (different use cases) | Right tool for the job |
| **Route Registration** | Hybrid (static + dynamic) | Flexibility for different teams |
| **Error Handling** | Actor responses + HTTP conversion | Consistent with actor model |

---

## Key Principles

1. **Pure Actor Model:**
   - Server actors ARE the capability
   - Access control through routing
   - No helper classes

2. **Consistency with Client Actors:**
   - HTTPServerActor mirrors HTTPClientActor design
   - Internal validation pattern
   - Message-based protocols

3. **Flexibility:**
   - Multiple patterns supported (hybrid approach)
   - Each team chooses what fits their use case
   - Not one-size-fits-all

4. **Separation of Concerns:**
   - HTTPServerActor handles HTTP concerns
   - Business actors handle business logic
   - Clear boundaries

5. **Testability:**
   - Mock router for unit tests
   - No real HTTP objects in business logic
   - Easy to test in isolation

---

**Full Design:** SERVER_ACTORS_DESIGN.md
**Quick Reference:** SERVER_ACTORS_SUMMARY.md
**Examples:** SERVER_ACTORS_EXAMPLES.md
**Integration:** SERVER_CLIENT_INTEGRATION.md
**This Document:** Key design decisions and rationale
