# Server Actors Design - Quick Reference

**Date:** 2026-02-07
**Full Design:** SERVER_ACTORS_DESIGN.md

---

## Three Server Actors

| Actor | Purpose | Port | Use Case |
|-------|---------|------|----------|
| **HTTPServerActor** | Accept HTTP requests | 3000 | RESTful APIs |
| **WebSocketServerActor** | Accept WebSocket connections | 3001 | Real-time updates |
| **SSEServerActor** | Server-Sent Events | 3002 | Live notifications |

---

## Quick Start Examples

### HTTPServerActor (RESTful API)

```typescript
// 1. Start server
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' }
  }
});
router.registerActor('/system/http-server', httpServer);

// 2. Actor registers routes
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
      return createResponse(message, { id, title: 'Task', status: 'open' });
    }
  }
}
```

**Usage:**
```bash
curl http://localhost:3000/api/tasks/123
# → { "id": "123", "title": "Task", "status": "open" }
```

---

### WebSocketServerActor (Real-Time)

```typescript
// 1. Start server
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  maxConnections: 100,
  channels: {
    'tasks': { publishers: ['/tasks'] }
  }
});
router.registerActor('/system/ws-server', wsServer);

// 2. Actor broadcasts updates
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'task.update') {
      const task = await this.updateTask(message.payload);

      // Broadcast to all WebSocket clients
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
  const { data } = JSON.parse(event.data);
  console.log('Task update:', data);
};
```

---

### SSEServerActor (Live Notifications)

```typescript
// 1. Start server
const sseServer = new SSEServerActor('sse-server', router, {
  port: 3002,
  maxConnections: 1000,
  keepAliveInterval: 30000,
  channels: {
    'notifications': { publishers: ['/notifications'] }
  }
});
router.registerActor('/system/sse-server', sseServer);

// 2. Actor sends events
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
});
```

---

## Message Protocols

### HTTPServerActor

| Message | Purpose | Payload |
|---------|---------|---------|
| `http-server.listen` | Start server | `{ port?: number }` |
| `http-server.route` | Register route | `{ method, path, actor, message }` |
| `http-server.stop` | Stop server | `{}` |

### WebSocketServerActor

| Message | Purpose | Payload |
|---------|---------|---------|
| `ws-server.listen` | Start server | `{ port?: number }` |
| `ws-server.broadcast` | Broadcast to channel | `{ channel, data }` |
| `ws-server.send` | Send to client | `{ clientId, data }` |
| `ws-server.clients` | List clients | `{ channel?: string }` |

### SSEServerActor

| Message | Purpose | Payload |
|---------|---------|---------|
| `sse-server.listen` | Start server | `{ port?: number }` |
| `sse-server.send` | Send event | `{ channel, event, data }` |

---

## Key Design Decisions

### 1. Routing Pattern: Hybrid

**Both patterns supported:**

```typescript
// Pattern A: Path-based routing (RESTful)
router.addRoute('GET', '/tasks/:id', {
  actor: (params) => address(`/tasks/${params.id}`),
  message: 'http.get'
});

// Pattern B: API actor
router.addRoute('POST', '/api/tasks', {
  actor: address('/api/tasks'),
  message: 'tasks.create'
});
```

### 2. Port Assignment: Path-Based Namespace Isolation

**One server, multiple namespaces:**

```typescript
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/workflows': { namespace: '/workflows', prefix: '/api/workflows' },
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' }
  }
});
```

**Request Flow:**
```
GET /api/workflows/orchestrator/123
  ↓ prefix: /api/workflows
  ↓ namespace: /workflows
  ↓ actor: /workflows/orchestrator
  ↓ message: http.get
```

### 3. Middleware: Internal Validation + Actor Delegation

**Simple operations (internal):**
- CORS validation
- Rate limiting

**Complex operations (delegate to actors):**
- Authentication → `/system/auth`
- Logging → `/system/logger`

```typescript
// CORS (internal)
if (!this.validateCORS(req)) {
  return new Response('CORS not allowed', { status: 403 });
}

// Auth (delegate)
const authResponse = await this.ask(
  address('/system/auth'),
  'auth.validate',
  { token: req.headers.get('authorization') }
);
```

### 4. WebSocket Broadcasting: Port-Based

**Actors broadcast to channels, not individual clients:**

```typescript
// TaskActor doesn't track WebSocket connections
await this.tell(
  address('/system/ws-server'),
  'ws-server.broadcast',
  { channel: 'tasks', data: task }
);
```

**Benefits:**
- Decouples actors from WebSocket implementation
- Simple pub/sub pattern
- Scales naturally

---

## Integration Patterns

### Pattern 1: RESTful Task API
```
HTTPServerActor (port 3000)
  ↓ routes to
TaskActor (handles CRUD)
```

### Pattern 2: Real-Time Dashboard
```
WebSocketServerActor (port 3001)
  ↓ broadcasts from
DashboardActor (sends metrics)
  ↓ clients subscribe
Browser (receives updates)
```

### Pattern 3: Live Notifications
```
SSEServerActor (port 3002)
  ↓ streams from
NotificationActor (sends events)
  ↓ clients listen
Browser (receives notifications)
```

### Pattern 4: Service Mesh
```
WorkflowActor
  ↓ HTTP request via HTTPClientActor
HTTPServerActor (internal)
  ↓ routes to
TaskActor
```

---

## Client vs Server Actors

| Feature | Client | Server |
|---------|--------|--------|
| **Direction** | Outbound (actor → external) | Inbound (external → actor) |
| **HTTP** | HTTPClientActor (make requests) | HTTPServerActor (accept requests) |
| **WebSocket** | WebSocketActor (connect to servers) | WebSocketServerActor (accept connections) |
| **SSE** | - | SSEServerActor (stream events) |
| **Use Case** | Call external APIs | Serve APIs to browsers/clients |

---

## Security Model

### Namespace Isolation
- Actors can only register routes in their own namespace
- Path prefix determines namespace

### CORS Configuration
- Per-namespace CORS rules
- Allow specific origins or wildcard

### Rate Limiting
- Per-client (by IP, user, or token)
- Sliding window algorithm

### Authentication
- Delegate to AuthActor
- Token validation before routing

---

## Implementation Phases

### Phase 1: HTTPServerActor (3-4 days)
- Route registration (static + dynamic)
- Request parsing (params, query, body)
- CORS, rate limiting
- Tests (>15)

### Phase 2: WebSocketServerActor (2-3 days)
- Connection management
- Channel subscription
- Broadcasting
- Tests (>10)

### Phase 3: SSEServerActor (1-2 days)
- Event streaming
- Keep-alive heartbeat
- Tests (>8)

### Phase 4: Integration Examples (1 day)
- Task API
- Real-time dashboard
- Notifications
- Documentation

**Total:** 7-10 days

---

## Complete Example: Task Management System

**Architecture:**
```
Browser
  ↓ HTTP (REST API)
HTTPServerActor (port 3000)
  ↓ routes to
TaskActor
  ↓ broadcasts to
WebSocketServerActor (port 3001)
  ↓ streams to
Browser (real-time updates)
```

**Code:**
```typescript
// 1. HTTP Server (REST API)
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: { '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' } }
});
router.registerActor('/system/http-server', httpServer);

// 2. WebSocket Server (Real-time)
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  maxConnections: 100,
  channels: { 'tasks': { publishers: ['/tasks'] } }
});
router.registerActor('/system/ws-server', wsServer);

// 3. Task Actor (Business Logic)
class TaskActor extends Actor {
  async onInit() {
    // Register HTTP routes
    await this.ask(address('/system/http-server'), 'http-server.route', {
      method: 'GET', path: '/api/tasks/:id', actor: this.address, message: 'http.get'
    });
    await this.ask(address('/system/http-server'), 'http-server.route', {
      method: 'PUT', path: '/api/tasks/:id', actor: this.address, message: 'http.update'
    });
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.get') {
      const task = await this.getTask(message.payload.params.id);
      return createResponse(message, task);
    }

    if (message.type === 'http.update') {
      const task = await this.updateTask(
        message.payload.params.id,
        message.payload.body
      );

      // Broadcast to WebSocket clients
      await this.tell(address('/system/ws-server'), 'ws-server.broadcast', {
        channel: 'tasks',
        data: task
      });

      return createResponse(message, task);
    }
  }
}
```

**Client:**
```javascript
// HTTP (REST)
const response = await fetch('http://localhost:3000/api/tasks/123');
const task = await response.json();

// WebSocket (Real-time)
const ws = new WebSocket('ws://localhost:3001');
ws.onopen = () => {
  ws.send(JSON.stringify({ type: 'subscribe', channel: 'tasks' }));
};
ws.onmessage = (event) => {
  const { data } = JSON.parse(event.data);
  console.log('Task updated:', data);
  // Update UI
};

// Update task (triggers broadcast)
await fetch('http://localhost:3000/api/tasks/123', {
  method: 'PUT',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ status: 'completed' })
});
// → All WebSocket clients receive update
```

---

## Benefits Summary

1. **Complete Actor System:** Actors can both consume and provide HTTP/WebSocket services
2. **RESTful APIs as Actors:** HTTP endpoints map naturally to actor messages
3. **Real-Time Updates:** WebSocket broadcasting and SSE streaming
4. **Widget Actors:** Widgets can serve their own HTTP endpoints
5. **Service Mesh:** Actors calling each other via HTTP (distributed deployments)
6. **Security:** Namespace isolation, CORS, rate limiting, authentication
7. **Testability:** Mock router for unit tests, no external dependencies
8. **Pure Actor Model:** Capabilities ARE actors, access control through routing

---

**Full Design:** SERVER_ACTORS_DESIGN.md (comprehensive documentation)
**This Document:** Quick reference and examples
