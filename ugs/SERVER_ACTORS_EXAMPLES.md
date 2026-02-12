# Server Actors: Before/After Examples

**Date:** 2026-02-07
**Purpose:** Show concrete use cases for HTTPServerActor, WebSocketServerActor, and SSEServerActor

---

## Example 1: Task Management API

### Before: No Server Actors

**Problem:** Actors can make HTTP requests (HTTPClientActor) but can't serve them.

```typescript
// Actors can call external APIs
class WorkflowActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // ✅ Can make outbound HTTP request
    const response = await this.ask(
      address('/workflows/system/http'),
      'http.post',
      {
        url: 'https://api.github.com/repos/owner/repo/issues',
        body: { title: 'New issue' }
      }
    );
  }
}

// ❌ But actors can't serve HTTP requests
// → Need separate Express/Hono server outside the actor system
// → Tight coupling, hard to test, breaks actor model
```

### After: HTTPServerActor

**Solution:** Actors serve HTTP endpoints through HTTPServerActor.

```typescript
// 1. Setup HTTPServerActor
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' }
  }
});
router.registerActor('/system/http-server', httpServer);

// 2. TaskActor registers routes and handles requests
class TaskActor extends Actor {
  async onInit() {
    // Register routes
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
    // List all tasks
    if (message.type === 'http.list') {
      const tasks = await this.listTasks();
      return createResponse(message, tasks);
    }

    // Get single task
    if (message.type === 'http.get') {
      const { id } = message.payload.params;
      const task = await this.getTask(id);
      return createResponse(message, task);
    }

    // Create task
    if (message.type === 'http.create') {
      const { title, description } = message.payload.body;
      const task = await this.createTask(title, description);
      return createResponse(message, task);
    }

    // Update task
    if (message.type === 'http.update') {
      const { id } = message.payload.params;
      const { status } = message.payload.body;
      const task = await this.updateTask(id, status);
      return createResponse(message, task);
    }

    // Delete task
    if (message.type === 'http.delete') {
      const { id } = message.payload.params;
      await this.deleteTask(id);
      return createResponse(message, { deleted: true });
    }
  }

  private async listTasks() {
    const response = await this.ask(
      address('/tasks/system/storage'),
      'storage.query',
      { sql: 'SELECT * FROM tasks' }
    );
    return response.payload.rows;
  }

  private async getTask(id: string) {
    const response = await this.ask(
      address('/tasks/system/storage'),
      'storage.query',
      { sql: 'SELECT * FROM tasks WHERE id = ?', params: [id] }
    );
    return response.payload.rows[0];
  }

  private async createTask(title: string, description: string) {
    const response = await this.ask(
      address('/tasks/system/storage'),
      'storage.execute',
      {
        sql: 'INSERT INTO tasks (id, title, description, status) VALUES (?, ?, ?, ?)',
        params: [crypto.randomUUID(), title, description, 'open']
      }
    );
    return { id: response.payload.id, title, description, status: 'open' };
  }

  private async updateTask(id: string, status: string) {
    await this.ask(
      address('/tasks/system/storage'),
      'storage.execute',
      {
        sql: 'UPDATE tasks SET status = ? WHERE id = ?',
        params: [status, id]
      }
    );
    return { id, status };
  }

  private async deleteTask(id: string) {
    await this.ask(
      address('/tasks/system/storage'),
      'storage.execute',
      {
        sql: 'DELETE FROM tasks WHERE id = ?',
        params: [id]
      }
    );
  }
}
```

**Usage:**
```bash
# List tasks
curl http://localhost:3000/api/tasks
# → [{"id":"1","title":"Task 1","status":"open"}, ...]

# Get task
curl http://localhost:3000/api/tasks/1
# → {"id":"1","title":"Task 1","status":"open"}

# Create task
curl -X POST http://localhost:3000/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"New task","description":"Description"}'
# → {"id":"2","title":"New task","status":"open"}

# Update task
curl -X PUT http://localhost:3000/api/tasks/1 \
  -H "Content-Type: application/json" \
  -d '{"status":"completed"}'
# → {"id":"1","status":"completed"}

# Delete task
curl -X DELETE http://localhost:3000/api/tasks/1
# → {"deleted":true}
```

**Benefits:**
- ✅ RESTful API fully within the actor system
- ✅ Actors handle HTTP requests via messages
- ✅ Easy to test (mock router)
- ✅ No external web framework needed
- ✅ Full observability (all operations are messages)

---

## Example 2: Real-Time Dashboard

### Before: No Server Actors

**Problem:** No way to push updates to browsers in real-time.

```typescript
// ❌ Polling (inefficient)
// Client polls every 5 seconds
setInterval(async () => {
  const response = await fetch('http://localhost:3000/api/tasks');
  const tasks = await response.json();
  updateUI(tasks);
}, 5000);

// ❌ Need separate WebSocket server outside actor system
// → Can't use actor messaging for WebSocket
// → Tight coupling, hard to test
```

### After: WebSocketServerActor

**Solution:** Actors broadcast updates to WebSocket clients.

```typescript
// 1. Setup WebSocketServerActor
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  maxConnections: 100,
  channels: {
    'dashboard': { publishers: ['/dashboard'] }
  }
});
router.registerActor('/system/ws-server', wsServer);

// 2. DashboardActor broadcasts metrics
class DashboardActor extends Actor {
  private metricsInterval?: string;

  async onInit() {
    // Broadcast metrics every 5 seconds
    this.metricsInterval = await this.scheduleRecurring(5000, 'metrics.update');
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'metrics.update') {
      // Collect metrics
      const metrics = {
        cpuUsage: this.getCPUUsage(),
        memoryUsage: this.getMemoryUsage(),
        activeTasks: await this.getActiveTasks(),
        timestamp: Date.now()
      };

      // Broadcast to all WebSocket clients
      await this.tell(
        address('/system/ws-server'),
        'ws-server.broadcast',
        {
          channel: 'dashboard',
          data: metrics
        }
      );

      return createResponse(message, { broadcasted: true });
    }
  }

  private getCPUUsage(): number {
    // Calculate CPU usage
    return Math.random() * 100;
  }

  private getMemoryUsage(): number {
    // Calculate memory usage
    return Math.random() * 100;
  }

  private async getActiveTasks(): Promise<number> {
    const response = await this.ask(
      address('/tasks/system/storage'),
      'storage.query',
      { sql: 'SELECT COUNT(*) as count FROM tasks WHERE status = ?', params: ['running'] }
    );
    return response.payload.rows[0].count;
  }
}
```

**Client:**
```html
<!DOCTYPE html>
<html>
<head>
  <title>Dashboard</title>
  <style>
    body { font-family: sans-serif; padding: 20px; }
    .metric { margin: 10px 0; }
    .value { font-size: 24px; font-weight: bold; color: #0066cc; }
  </style>
</head>
<body>
  <h1>Real-Time Dashboard</h1>

  <div class="metric">
    <div>CPU Usage</div>
    <div class="value" id="cpu">-</div>
  </div>

  <div class="metric">
    <div>Memory Usage</div>
    <div class="value" id="memory">-</div>
  </div>

  <div class="metric">
    <div>Active Tasks</div>
    <div class="value" id="tasks">-</div>
  </div>

  <div class="metric">
    <div>Last Update</div>
    <div id="timestamp">-</div>
  </div>

  <script>
    const ws = new WebSocket('ws://localhost:3001');

    ws.onopen = () => {
      console.log('Connected to dashboard');
      ws.send(JSON.stringify({ type: 'subscribe', channel: 'dashboard' }));
    };

    ws.onmessage = (event) => {
      const { data } = JSON.parse(event.data);

      document.getElementById('cpu').textContent = data.cpuUsage.toFixed(1) + '%';
      document.getElementById('memory').textContent = data.memoryUsage.toFixed(1) + '%';
      document.getElementById('tasks').textContent = data.activeTasks;
      document.getElementById('timestamp').textContent = new Date(data.timestamp).toLocaleTimeString();
    };

    ws.onerror = (error) => {
      console.error('WebSocket error:', error);
    };

    ws.onclose = () => {
      console.log('Disconnected from dashboard');
    };
  </script>
</body>
</html>
```

**Benefits:**
- ✅ Real-time updates (no polling)
- ✅ Actors broadcast to multiple clients
- ✅ Low latency (WebSocket)
- ✅ Actors don't manage WebSocket connections
- ✅ Simple pub/sub pattern

---

## Example 3: Live Notifications

### Before: No Server Actors

**Problem:** No way to stream events to browsers.

```typescript
// ❌ Need to build custom notification system
// → Polling, long-polling, or external service (Firebase, Pusher)
// → Adds complexity and dependencies
```

### After: SSEServerActor

**Solution:** Stream notifications via Server-Sent Events.

```typescript
// 1. Setup SSEServerActor
const sseServer = new SSEServerActor('sse-server', router, {
  port: 3002,
  maxConnections: 1000,
  keepAliveInterval: 30000,
  channels: {
    'notifications': { publishers: ['/notifications'] }
  }
});
router.registerActor('/system/sse-server', sseServer);

// 2. NotificationActor sends events
class NotificationActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'notification.send') {
      const { userId, title, message: notifMessage } = message.payload;

      // Send to SSE clients
      await this.tell(
        address('/system/sse-server'),
        'sse-server.send',
        {
          channel: 'notifications',
          event: 'notification',
          data: {
            id: crypto.randomUUID(),
            userId,
            title,
            message: notifMessage,
            timestamp: Date.now()
          }
        }
      );

      return createResponse(message, { sent: true });
    }
  }
}

// 3. TaskActor sends notification when task completes
class TaskActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'task.complete') {
      const { id, title } = message.payload;

      // Update task status
      await this.ask(
        address('/tasks/system/storage'),
        'storage.execute',
        {
          sql: 'UPDATE tasks SET status = ? WHERE id = ?',
          params: ['completed', id]
        }
      );

      // Send notification
      await this.tell(
        address('/notifications'),
        'notification.send',
        {
          userId: message.payload.userId,
          title: 'Task Completed',
          message: `Task "${title}" has been completed`
        }
      );

      return createResponse(message, { completed: true });
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
  <style>
    body { font-family: sans-serif; padding: 20px; }
    #notifications { max-width: 600px; }
    .notification {
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 15px;
      margin: 10px 0;
      background: #f9f9f9;
    }
    .notification.new {
      background: #e3f2fd;
      animation: fadeIn 0.3s;
    }
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(-10px); }
      to { opacity: 1; transform: translateY(0); }
    }
    .notification-title {
      font-weight: bold;
      margin-bottom: 5px;
    }
    .notification-time {
      font-size: 12px;
      color: #666;
      margin-top: 5px;
    }
  </style>
</head>
<body>
  <h1>Live Notifications</h1>
  <div id="notifications"></div>

  <script>
    const eventSource = new EventSource('http://localhost:3002/notifications');

    eventSource.addEventListener('notification', (event) => {
      const notification = JSON.parse(event.data);

      const div = document.createElement('div');
      div.className = 'notification new';
      div.innerHTML = `
        <div class="notification-title">${notification.title}</div>
        <div>${notification.message}</div>
        <div class="notification-time">${new Date(notification.timestamp).toLocaleString()}</div>
      `;

      document.getElementById('notifications').prepend(div);

      // Remove 'new' class after animation
      setTimeout(() => div.classList.remove('new'), 300);

      // Show browser notification (if permitted)
      if (Notification.permission === 'granted') {
        new Notification(notification.title, {
          body: notification.message
        });
      }
    });

    eventSource.addEventListener('heartbeat', (event) => {
      console.log('Keep-alive heartbeat received');
    });

    eventSource.onerror = (error) => {
      console.error('SSE error:', error);
    };

    // Request notification permission
    if (Notification.permission === 'default') {
      Notification.requestPermission();
    }
  </script>
</body>
</html>
```

**Benefits:**
- ✅ One-way streaming (server → client)
- ✅ Simpler than WebSocket for notifications
- ✅ Automatic reconnection (browser handles it)
- ✅ Keep-alive heartbeat prevents timeout
- ✅ No need for external notification service

---

## Example 4: Widget Actors Serving Endpoints

### Before: No Server Actors

**Problem:** Widgets can't expose their own APIs.

```typescript
// ❌ Widget logic + API endpoints in separate files
// → Tight coupling, hard to test, breaks encapsulation

// widget.ts (business logic)
class DashboardWidget {
  getData() { return { ... }; }
}

// api.ts (HTTP endpoints - separate from widget)
app.get('/api/widgets/dashboard/data', async (req, res) => {
  const widget = new DashboardWidget();
  const data = widget.getData();
  res.json(data);
});
```

### After: HTTPServerActor

**Solution:** Widgets register their own HTTP endpoints.

```typescript
class DashboardWidgetActor extends Actor {
  async onInit() {
    // Widget registers its own API endpoint
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

    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'POST',
        path: '/api/widgets/dashboard/config',
        actor: this.address,
        message: 'http.set-config'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    // Serve widget data
    if (message.type === 'http.get-data') {
      const data = await this.getDashboardData();
      return createResponse(message, data);
    }

    // Update widget config
    if (message.type === 'http.set-config') {
      const { config } = message.payload.body;
      await this.updateConfig(config);
      return createResponse(message, { updated: true });
    }
  }

  private async getDashboardData() {
    // Fetch data from storage, external APIs, etc.
    const tasks = await this.ask(
      address('/tasks/system/storage'),
      'storage.query',
      { sql: 'SELECT COUNT(*) as count FROM tasks WHERE status = ?', params: ['open'] }
    );

    const workflows = await this.ask(
      address('/workflows/system/storage'),
      'storage.query',
      { sql: 'SELECT COUNT(*) as count FROM workflows WHERE status = ?', params: ['running'] }
    );

    return {
      openTasks: tasks.payload.rows[0].count,
      runningWorkflows: workflows.payload.rows[0].count,
      timestamp: Date.now()
    };
  }

  private async updateConfig(config: any) {
    // Store widget configuration
    await this.ask(
      address('/workflows/system/storage'),
      'storage.execute',
      {
        sql: 'UPDATE widget_configs SET config = ? WHERE widget_id = ?',
        params: [JSON.stringify(config), this.id]
      }
    );
  }
}
```

**Usage:**
```bash
# Get widget data
curl http://localhost:3000/api/widgets/dashboard/data
# → {"openTasks":5,"runningWorkflows":2,"timestamp":1234567890}

# Update widget config
curl -X POST http://localhost:3000/api/widgets/dashboard/config \
  -H "Content-Type: application/json" \
  -d '{"refreshInterval":5000,"showGraph":true}'
# → {"updated":true}
```

**Benefits:**
- ✅ Self-contained widgets (logic + API)
- ✅ Dynamic route registration
- ✅ Widgets expose their own endpoints
- ✅ Easy to add/remove widgets
- ✅ Clear encapsulation

---

## Example 5: Service Mesh (Actors Calling Each Other via HTTP)

### Before: No Server Actors

**Problem:** Actors can only communicate via in-process messages.

```typescript
// ❌ All actors must be in the same process
// → Can't distribute across machines
// → Single point of failure
// → Hard to scale
```

### After: HTTPServerActor + HTTPClientActor

**Solution:** Actors call each other via HTTP (distributed deployment).

```typescript
// === Machine 1: Task Service ===

// TaskActor serves HTTP API
const taskHttp = new HTTPServerActor('task-http', router, {
  port: 4001,
  namespaces: {
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' }
  }
});

class TaskActor extends Actor {
  async onInit() {
    await this.ask(
      address('/system/http-server'),
      'http-server.route',
      {
        method: 'POST',
        path: '/api/tasks',
        actor: this.address,
        message: 'http.create'
      }
    );
  }

  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'http.create') {
      const { title } = message.payload.body;
      const task = await this.createTask(title);
      return createResponse(message, task);
    }
  }

  private async createTask(title: string) {
    const id = crypto.randomUUID();
    await this.ask(
      address('/tasks/system/storage'),
      'storage.execute',
      {
        sql: 'INSERT INTO tasks (id, title, status) VALUES (?, ?, ?)',
        params: [id, title, 'open']
      }
    );
    return { id, title, status: 'open' };
  }
}

// === Machine 2: Workflow Service ===

// WorkflowActor calls TaskActor via HTTP
class WorkflowActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    if (message.type === 'workflow.run') {
      // Call Task Service on Machine 1
      const taskResponse = await this.ask(
        address('/workflows/system/http'),
        'http.post',
        {
          url: 'http://task-service:4001/api/tasks',
          headers: { 'Content-Type': 'application/json' },
          body: { title: 'Task from workflow' }
        }
      );

      if (!taskResponse.success) {
        return createErrorResponse(message, taskResponse.error);
      }

      this.logInfo('Created task', { task: taskResponse.payload.body });

      return createResponse(message, {
        workflow: 'running',
        task: taskResponse.payload.body
      });
    }
  }
}
```

**Deployment:**
```yaml
# docker-compose.yml

services:
  task-service:
    image: task-service
    ports:
      - "4001:4001"
    environment:
      - SERVICE_PORT=4001
      - DB_PATH=/data/tasks.db

  workflow-service:
    image: workflow-service
    ports:
      - "4002:4002"
    environment:
      - SERVICE_PORT=4002
      - TASK_SERVICE_URL=http://task-service:4001
```

**Benefits:**
- ✅ Distributed deployment (actors across machines)
- ✅ Service mesh pattern
- ✅ Standard HTTP interface
- ✅ Load balancing, service discovery
- ✅ Independent scaling

---

## Summary: Complete System

**Putting it all together:**

```typescript
// === Server Setup ===

// 1. HTTP Server (REST API)
const httpServer = new HTTPServerActor('http-server', router, {
  port: 3000,
  namespaces: {
    '/api/tasks': { namespace: '/tasks', prefix: '/api/tasks' }
  },
  cors: {
    allowedOrigins: ['http://localhost:8080'],
    allowedMethods: ['GET', 'POST', 'PUT', 'DELETE'],
    credentials: true
  },
  rateLimit: {
    requests: 100,
    window: 60000,
    keyBy: 'ip'
  }
});
router.registerActor('/system/http-server', httpServer);

// 2. WebSocket Server (Real-time)
const wsServer = new WebSocketServerActor('ws-server', router, {
  port: 3001,
  maxConnections: 100,
  channels: {
    'tasks': { publishers: ['/tasks'] }
  }
});
router.registerActor('/system/ws-server', wsServer);

// 3. SSE Server (Notifications)
const sseServer = new SSEServerActor('sse-server', router, {
  port: 3002,
  maxConnections: 1000,
  keepAliveInterval: 30000,
  channels: {
    'notifications': { publishers: ['/notifications'] }
  }
});
router.registerActor('/system/sse-server', sseServer);

// === Task Actor ===

class TaskActor extends Actor {
  async onInit() {
    // Register HTTP routes
    const routes = [
      { method: 'GET', path: '/api/tasks', message: 'http.list' },
      { method: 'POST', path: '/api/tasks', message: 'http.create' },
      { method: 'PUT', path: '/api/tasks/:id', message: 'http.update' }
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
    if (message.type === 'http.create') {
      const task = await this.createTask(message.payload.body);

      // Broadcast to WebSocket clients
      await this.tell(
        address('/system/ws-server'),
        'ws-server.broadcast',
        { channel: 'tasks', data: { event: 'task.created', task } }
      );

      // Send notification
      await this.tell(
        address('/notifications'),
        'notification.send',
        {
          title: 'Task Created',
          message: `Task "${task.title}" was created`
        }
      );

      return createResponse(message, task);
    }

    if (message.type === 'http.update') {
      const task = await this.updateTask(
        message.payload.params.id,
        message.payload.body
      );

      // Broadcast to WebSocket clients
      await this.tell(
        address('/system/ws-server'),
        'ws-server.broadcast',
        { channel: 'tasks', data: { event: 'task.updated', task } }
      );

      return createResponse(message, task);
    }
  }
}
```

**Client (Frontend):**
```javascript
// HTTP (REST API)
async function createTask(title) {
  const response = await fetch('http://localhost:3000/api/tasks', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ title })
  });
  return await response.json();
}

// WebSocket (Real-time updates)
const ws = new WebSocket('ws://localhost:3001');
ws.onopen = () => {
  ws.send(JSON.stringify({ type: 'subscribe', channel: 'tasks' }));
};
ws.onmessage = (event) => {
  const { data } = JSON.parse(event.data);
  if (data.event === 'task.created') {
    console.log('New task:', data.task);
    addTaskToUI(data.task);
  }
  if (data.event === 'task.updated') {
    console.log('Task updated:', data.task);
    updateTaskInUI(data.task);
  }
};

// SSE (Notifications)
const eventSource = new EventSource('http://localhost:3002/notifications');
eventSource.addEventListener('notification', (event) => {
  const notification = JSON.parse(event.data);
  showNotification(notification.title, notification.message);
});

// Usage
await createTask('New task');
// → HTTP POST creates task
// → WebSocket broadcasts update
// → SSE sends notification
// → UI updates in real-time
```

**Complete Flow:**
```
Browser
  ↓ POST /api/tasks (HTTP)
HTTPServerActor
  ↓ routes to
TaskActor
  ↓ creates task
  ↓ broadcasts via ws-server.broadcast
WebSocketServerActor
  ↓ sends to all subscribers
  ↓ sends notification via sse-server.send
SSEServerActor
  ↓ streams to all clients
Browser
  ↓ receives update (WebSocket)
  ↓ receives notification (SSE)
  ↓ updates UI
```

**Result:** Complete real-time system with REST API, WebSocket updates, and live notifications - all within the actor model!

---

**Full Design:** SERVER_ACTORS_DESIGN.md
**Quick Reference:** SERVER_ACTORS_SUMMARY.md
**This Document:** Concrete before/after examples
