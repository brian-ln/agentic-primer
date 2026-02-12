# Server-Client Actor Integration Guide

**Date:** 2026-02-07
**Purpose:** Show how HTTPServerActor/WebSocketServerActor integrate with HTTPClientActor/WebSocketActor

---

## Overview: Complete Network Actor Suite

| Actor | Direction | Purpose | Example |
|-------|-----------|---------|---------|
| **HTTPClientActor** | Outbound | Make HTTP requests | Call GitHub API |
| **HTTPServerActor** | Inbound | Accept HTTP requests | Serve REST API |
| **WebSocketActor** | Outbound | Connect to WebSocket servers | Stream from external service |
| **WebSocketServerActor** | Inbound | Accept WebSocket connections | Real-time dashboard |
| **SSEServerActor** | Inbound | Stream events to browsers | Live notifications |

---

## Integration Pattern 1: Proxy Pattern

**Use Case:** Actor receives HTTP request, calls external API, returns result.

```typescript
// Setup
const httpServer = new HTTPServerActor('http-server', router, { port: 3000 });
const httpClient = new HTTPClientActor('http-client', router, {
  methods: ['GET', 'POST'],
  allowedHosts: ['api.github.com'],
  rateLimit: { requests: 60, window: 60000 },
  timeout: 30000
});

router.registerActor('/system/http-server', httpServer);
router.registerActor('/workflows/system/http', httpClient);

// ProxyActor: HTTP request → call external API → return response
class GitHubProxyActor extends Actor {
  async onInit() {
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'GET',
        path: '/api/github/repos/:owner/:repo',
        actor: this.address,
        message: 'http.proxy'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.proxy') {
      const { owner, repo } = message.payload.params;

      // Call GitHub API via HTTPClientActor
      const response = await this.ask(
        address('/workflows/system/http'),
        'http.get',
        {
          url: `https://api.github.com/repos/${owner}/${repo}`,
          headers: {
            'Authorization': `token ${process.env.GITHUB_TOKEN}`,
            'User-Agent': 'ActorSystem/1.0'
          }
        }
      );

      if (!response.success) {
        return createErrorResponse(message, response.error);
      }

      // Return GitHub API response
      return createResponse(message, response.payload.body);
    }
  }
}
```

**Flow:**
```
Browser
  ↓ GET /api/github/repos/owner/repo
HTTPServerActor
  ↓ routes to
GitHubProxyActor
  ↓ http.get via
HTTPClientActor
  ↓ calls
GitHub API
  ↓ response
GitHubProxyActor
  ↓ returns
HTTPServerActor
  ↓ HTTP response
Browser
```

**Usage:**
```bash
curl http://localhost:3000/api/github/repos/owner/repo
# → Proxied GitHub API response
```

---

## Integration Pattern 2: Aggregation Pattern

**Use Case:** Actor receives HTTP request, calls multiple external APIs, aggregates results.

```typescript
class DashboardActor extends Actor {
  async onInit() {
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'GET',
        path: '/api/dashboard/summary',
        actor: this.address,
        message: 'http.summary'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.summary') {
      // Call multiple external APIs in parallel
      const [githubResponse, anthropicResponse, tasksResponse] = await Promise.all([
        // GitHub API
        this.ask(
          address('/workflows/system/http'),
          'http.get',
          {
            url: 'https://api.github.com/user',
            headers: { 'Authorization': `token ${process.env.GITHUB_TOKEN}` }
          }
        ),

        // Anthropic API (hypothetical)
        this.ask(
          address('/workflows/system/http'),
          'http.get',
          {
            url: 'https://api.anthropic.com/v1/account',
            headers: { 'x-api-key': process.env.ANTHROPIC_API_KEY }
          }
        ),

        // Internal Task API
        this.ask(
          address('/tasks'),
          'task.list',
          { status: 'open' }
        )
      ]);

      // Aggregate results
      const summary = {
        github: {
          user: githubResponse.success ? githubResponse.payload.body.login : null,
          repos: githubResponse.success ? githubResponse.payload.body.public_repos : 0
        },
        anthropic: {
          tier: anthropicResponse.success ? anthropicResponse.payload.body.tier : null
        },
        tasks: {
          open: tasksResponse.success ? tasksResponse.payload.length : 0
        }
      };

      return createResponse(message, summary);
    }
  }
}
```

**Flow:**
```
Browser
  ↓ GET /api/dashboard/summary
HTTPServerActor
  ↓ routes to
DashboardActor
  ↓ parallel calls
  ├─ HTTPClientActor → GitHub API
  ├─ HTTPClientActor → Anthropic API
  └─ TaskActor → Internal DB
  ↓ aggregates
DashboardActor
  ↓ returns
Browser (receives aggregated summary)
```

---

## Integration Pattern 3: WebSocket Relay

**Use Case:** Connect to external WebSocket, relay messages to internal WebSocket clients.

```typescript
// Setup
const wsClient = new WebSocketActor('ws-client', router, {
  allowedHosts: ['stream.external.com'],
  maxConnections: 5,
  reconnect: { enabled: true, maxAttempts: 5, backoff: 'exponential' }
});

const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  maxConnections: 100,
  channels: { 'external-stream': { publishers: ['/relay'] } }
});

router.registerActor('/workflows/system/websocket', wsClient);
router.registerActor('/system/ws-server', wsServer);

// RelayActor: External WebSocket → Internal WebSocket clients
class RelayActor extends Actor {
  private externalConnectionId?: string;

  async onInit() {
    // Connect to external WebSocket
    const connectResponse = await this.ask(
      address('/workflows/system/websocket'),
      'ws.connect',
      { url: 'wss://stream.external.com/data' }
    );

    if (connectResponse.success) {
      this.externalConnectionId = connectResponse.payload.connectionId;

      // Subscribe to external WebSocket events
      const subscribeResponse = await this.ask(
        address('/workflows/system/websocket'),
        'ws.subscribe',
        { connectionId: this.externalConnectionId }
      );

      // Listen to events in background
      this.listenToExternalStream(subscribeResponse.payload.stream);
    }
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Handle other messages
    return createResponse(message, {});
  }

  private async listenToExternalStream(stream: AsyncIterator<WSEvent>): Promise<void> {
    for await (const event of stream) {
      if (event.type === 'message') {
        // Relay message to internal WebSocket clients
        await this.tell(
          address('/system/ws-server'),
          'ws-server.broadcast',
          {
            channel: 'external-stream',
            data: event.data
          }
        );

        this.logInfo('Relayed message', { data: event.data });
      }

      if (event.type === 'close') {
        this.logWarn('External WebSocket closed');
        break;
      }
    }
  }
}
```

**Flow:**
```
External WebSocket Server
  ↓ streams data
WebSocketActor (client)
  ↓ receives events
RelayActor
  ↓ broadcasts
WebSocketServerActor (server)
  ↓ sends to clients
Internal Browser Clients
```

**Usage:**
```javascript
// Browser connects to internal WebSocket
const ws = new WebSocket('ws://localhost:3001');
ws.onopen = () => {
  ws.send(JSON.stringify({ type: 'subscribe', channel: 'external-stream' }));
};

ws.onmessage = (event) => {
  const { data } = JSON.parse(event.data);
  console.log('Relayed from external:', data);
};
```

---

## Integration Pattern 4: Webhook Handler

**Use Case:** Receive webhooks from external services, process, notify clients.

```typescript
// Setup
const httpServer = new HTTPServerActor('http-server', router, { port: 3000 });
const httpClient = new HTTPClientActor('http-client', router, {
  methods: ['POST'],
  allowedHosts: ['api.github.com'],
  rateLimit: { requests: 60, window: 60000 },
  timeout: 30000
});
const sseServer = new SSEServerActor('sse-server', router, {
  port: 3002,
  maxConnections: 1000,
  keepAliveInterval: 30000,
  channels: { 'webhooks': { publishers: ['/webhooks'] } }
});

router.registerActor('/system/http-server', httpServer);
router.registerActor('/workflows/system/http', httpClient);
router.registerActor('/system/sse-server', sseServer);

// WebhookActor: Receive webhook → process → notify clients → respond to external
class GitHubWebhookActor extends Actor {
  async onInit() {
    // Register webhook endpoint
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'POST',
        path: '/webhooks/github',
        actor: this.address,
        message: 'webhook.github'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'webhook.github') {
      const { body, headers } = message.payload;

      // Verify webhook signature (GitHub specific)
      const isValid = this.verifySignature(body, headers['x-hub-signature-256']);
      if (!isValid) {
        return createErrorResponse(message, 'Invalid signature');
      }

      // Process webhook event
      const event = body;

      if (event.action === 'opened' && event.pull_request) {
        // PR opened event
        const pr = event.pull_request;

        // Store PR in database
        await this.ask(
          address('/workflows/system/storage'),
          'storage.execute',
          {
            sql: 'INSERT INTO pull_requests (id, number, title, url) VALUES (?, ?, ?, ?)',
            params: [pr.id, pr.number, pr.title, pr.html_url]
          }
        );

        // Notify SSE clients
        await this.tell(
          address('/system/sse-server'),
          'sse-server.send',
          {
            channel: 'webhooks',
            event: 'pull-request-opened',
            data: {
              number: pr.number,
              title: pr.title,
              url: pr.html_url,
              author: pr.user.login
            }
          }
        );

        // Optionally: Comment on PR via GitHub API
        await this.ask(
          address('/workflows/system/http'),
          'http.post',
          {
            url: `https://api.github.com/repos/${event.repository.full_name}/issues/${pr.number}/comments`,
            headers: {
              'Authorization': `token ${process.env.GITHUB_TOKEN}`,
              'Content-Type': 'application/json'
            },
            body: {
              body: 'Thanks for the PR! We\'ll review it soon.'
            }
          }
        );
      }

      // Respond to GitHub (webhook acknowledgment)
      return createResponse(message, { received: true });
    }
  }

  private verifySignature(payload: any, signature: string): boolean {
    // Verify HMAC signature
    const crypto = require('crypto');
    const hmac = crypto.createHmac('sha256', process.env.GITHUB_WEBHOOK_SECRET);
    hmac.update(JSON.stringify(payload));
    const digest = 'sha256=' + hmac.digest('hex');
    return crypto.timingSafeEqual(Buffer.from(digest), Buffer.from(signature));
  }
}
```

**Flow:**
```
GitHub
  ↓ POST /webhooks/github (webhook)
HTTPServerActor
  ↓ routes to
GitHubWebhookActor
  ↓ verifies signature
  ↓ stores in DB (StorageActor)
  ↓ notifies clients (SSEServerActor)
  ↓ comments on PR (HTTPClientActor → GitHub API)
  ↓ returns 200 OK
GitHub (receives acknowledgment)

Browser (SSE client)
  ↓ receives event
  ↓ shows notification
```

**Usage:**
```javascript
// Browser listens for webhook events
const eventSource = new EventSource('http://localhost:3002/webhooks');

eventSource.addEventListener('pull-request-opened', (event) => {
  const pr = JSON.parse(event.data);
  console.log('New PR:', pr.title, pr.url);
  showNotification('New PR', `${pr.author} opened: ${pr.title}`);
});
```

---

## Integration Pattern 5: Service Mesh with Load Balancing

**Use Case:** Distribute requests across multiple backend services.

```typescript
// Service 1: Task Service (machine 1)
const taskHttpServer = new HTTPServerActor('task-http-server', router, {
  port: 4001,
  namespaces: { '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' } }
});

// Service 2: Workflow Service (machine 2)
const workflowHttpServer = new HTTPServerActor('workflow-http-server', router, {
  port: 4002,
  namespaces: { '/api/workflows': { namespace: '/workflows', prefix: '/api/workflows' } }
});

// API Gateway (machine 3)
class APIGatewayActor extends Actor {
  private taskServiceUrl = 'http://task-service:4001';
  private workflowServiceUrl = 'http://workflow-service:4002';

  async onInit() {
    // Register gateway routes
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'GET',
        path: '/api/tasks/:id',
        actor: this.address,
        message: 'gateway.tasks'
      }
    );

    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'GET',
        path: '/api/workflows/:id',
        actor: this.address,
        message: 'gateway.workflows'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'gateway.tasks') {
      const { id } = message.payload.params;

      // Forward to Task Service
      const response = await this.ask(
        address('/gateway/system/http'),
        'http.get',
        { url: `${this.taskServiceUrl}/api/tasks/${id}` }
      );

      if (!response.success) {
        return createErrorResponse(message, response.error);
      }

      return createResponse(message, response.payload.body);
    }

    if (message.type === 'gateway.workflows') {
      const { id } = message.payload.params;

      // Forward to Workflow Service
      const response = await this.ask(
        address('/gateway/system/http'),
        'http.get',
        { url: `${this.workflowServiceUrl}/api/workflows/${id}` }
      );

      if (!response.success) {
        return createErrorResponse(message, response.error);
      }

      return createResponse(message, response.payload.body);
    }
  }
}
```

**Flow:**
```
Browser
  ↓ GET /api/tasks/123
API Gateway (port 80)
  ↓ routes via HTTPClientActor
Task Service (port 4001)
  ↓ HTTPServerActor routes to
TaskActor
  ↓ returns task
  ↓ response chain
Browser (receives task)
```

**Benefits:**
- Service discovery
- Load balancing (round-robin, least connections)
- Circuit breaking
- Request routing
- Centralized auth/logging

---

## Integration Pattern 6: Real-Time Sync

**Use Case:** External data changes trigger internal WebSocket updates.

```typescript
// Setup
const httpServer = new HTTPServerActor('http-server', router, { port: 3000 });
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  channels: { 'tasks': { publishers: ['/tasks'] } }
});
const httpClient = new HTTPClientActor('http-client', router, {
  methods: ['GET', 'POST', 'PUT'],
  allowedHosts: ['api.external.com'],
  rateLimit: { requests: 100, window: 60000 },
  timeout: 30000
});

router.registerActor('/system/http-server', httpServer);
router.registerActor('/system/ws-server', wsServer);
router.registerActor('/tasks/system/http', httpClient);

// TaskSyncActor: Poll external API, push updates to WebSocket clients
class TaskSyncActor extends Actor {
  private syncInterval?: string;
  private lastSync?: number;

  async onInit() {
    // Poll external API every 10 seconds
    this.syncInterval = await this.scheduleRecurring(10000, 'sync.poll');
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'sync.poll') {
      // Fetch tasks from external API
      const response = await this.ask(
        address('/tasks/system/http'),
        'http.get',
        {
          url: 'https://api.external.com/tasks',
          headers: { 'Authorization': `Bearer ${process.env.EXTERNAL_API_KEY}` }
        }
      );

      if (!response.success) {
        this.logError('Failed to fetch external tasks', { error: response.error });
        return createErrorResponse(message, response.error);
      }

      const externalTasks = response.payload.body;

      // Compare with local tasks
      const localTasks = await this.getLocalTasks();
      const changes = this.detectChanges(localTasks, externalTasks);

      if (changes.length > 0) {
        // Update local database
        for (const change of changes) {
          await this.ask(
            address('/tasks/system/storage'),
            'storage.execute',
            {
              sql: 'UPDATE tasks SET status = ?, updated_at = ? WHERE id = ?',
              params: [change.status, Date.now(), change.id]
            }
          );
        }

        // Broadcast changes to WebSocket clients
        await this.tell(
          address('/system/ws-server'),
          'ws-server.broadcast',
          {
            channel: 'tasks',
            data: {
              event: 'tasks.synced',
              changes
            }
          }
        );

        this.logInfo('Synced tasks', { changes: changes.length });
      }

      this.lastSync = Date.now();
      return createResponse(message, { synced: changes.length });
    }
  }

  private async getLocalTasks() {
    const response = await this.ask(
      address('/tasks/system/storage'),
      'storage.query',
      { sql: 'SELECT * FROM tasks' }
    );
    return response.payload.rows;
  }

  private detectChanges(local: any[], external: any[]): any[] {
    const changes = [];

    for (const externalTask of external) {
      const localTask = local.find(t => t.id === externalTask.id);

      if (!localTask) {
        // New task (insert)
        changes.push({ ...externalTask, changeType: 'insert' });
      } else if (localTask.status !== externalTask.status) {
        // Status changed (update)
        changes.push({ ...externalTask, changeType: 'update' });
      }
    }

    return changes;
  }
}
```

**Flow:**
```
Every 10 seconds:
  TaskSyncActor
    ↓ polls external API (HTTPClientActor)
  External API
    ↓ returns tasks
  TaskSyncActor
    ↓ compares with local tasks
    ↓ detects changes
    ↓ updates database (StorageActor)
    ↓ broadcasts changes (WebSocketServerActor)
  Browser (WebSocket clients)
    ↓ receive updates
    ↓ refresh UI
```

**Client:**
```javascript
const ws = new WebSocket('ws://localhost:3001');
ws.onopen = () => {
  ws.send(JSON.stringify({ type: 'subscribe', channel: 'tasks' }));
};

ws.onmessage = (event) => {
  const { data } = JSON.parse(event.data);

  if (data.event === 'tasks.synced') {
    console.log('Tasks synced:', data.changes);
    refreshTaskList();
  }
};
```

---

## Complete Architecture Diagram

```
┌──────────────────────────────────────────────────────────────┐
│                         Browser/Client                        │
│                                                                │
│  HTTP Requests  │  WebSocket Client  │  SSE EventSource       │
└────────┬─────────────────┬───────────────────┬────────────────┘
         │                 │                   │
         ▼                 ▼                   ▼
┌────────────────┐  ┌──────────────┐  ┌─────────────────┐
│ HTTPServerActor│  │ WSServerActor│  │ SSEServerActor  │
│  (port 3000)   │  │ (port 3001)  │  │  (port 3002)    │
└────────┬───────┘  └──────┬───────┘  └────────┬────────┘
         │                 │                   │
         │                 │                   │
         ▼                 ▼                   ▼
┌────────────────────────────────────────────────────────────────┐
│                     Message Router                              │
│                   (Pure Actor Model)                            │
└────────┬───────────────┬───────────────┬─────────────┬─────────┘
         │               │               │             │
         ▼               ▼               ▼             ▼
┌─────────────┐  ┌──────────────┐  ┌─────────┐  ┌──────────────┐
│ TaskActor   │  │ WorkflowActor│  │RelayActor│ │WebhookActor  │
└─────┬───────┘  └──────┬───────┘  └────┬────┘  └──────┬───────┘
      │                 │               │              │
      │                 │               │              │
      ▼                 ▼               ▼              ▼
┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐
│StorageActor  │  │HTTPClientActor│ │WebSocketActor (client)   │
│(Database)    │  │(Outbound HTTP)│ │(Outbound WebSocket)      │
└──────────────┘  └──────────────┘  └──────────────────────────┘
                         │                     │
                         ▼                     ▼
                  ┌──────────────────────────────────┐
                  │    External Services             │
                  │  • GitHub API                    │
                  │  • Anthropic API                 │
                  │  • External WebSocket Servers    │
                  └──────────────────────────────────┘
```

---

## Summary: Integration Patterns

| Pattern | Inbound | Outbound | Use Case |
|---------|---------|----------|----------|
| **Proxy** | HTTPServerActor | HTTPClientActor | API gateway, request forwarding |
| **Aggregation** | HTTPServerActor | HTTPClientActor (multiple) | Dashboard, data aggregation |
| **WebSocket Relay** | WebSocketServerActor | WebSocketActor | Stream relay, real-time bridge |
| **Webhook Handler** | HTTPServerActor | HTTPClientActor + SSEServerActor | GitHub webhooks, event processing |
| **Service Mesh** | HTTPServerActor | HTTPClientActor | Distributed services, load balancing |
| **Real-Time Sync** | WebSocketServerActor | HTTPClientActor | Poll external API, push updates |

---

## Benefits of Integration

1. **Complete Network Stack:**
   - Actors can both consume and provide HTTP/WebSocket services
   - Pure actor model end-to-end

2. **Flexible Routing:**
   - Request → Actor → External API → Actor → Response
   - External Event → Actor → WebSocket Clients

3. **Service Composition:**
   - Combine multiple actors for complex workflows
   - Gateway, aggregation, relay patterns

4. **Real-Time Updates:**
   - External changes trigger internal broadcasts
   - WebSocket relay for streaming data

5. **Security:**
   - All network access through system actors
   - Consistent access control (routing-based)

---

**Full Design:** SERVER_ACTORS_DESIGN.md
**Quick Reference:** SERVER_ACTORS_SUMMARY.md
**Examples:** SERVER_ACTORS_EXAMPLES.md
**This Document:** Integration patterns with existing client actors
