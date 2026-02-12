# Universal Graph System - Examples

This directory contains working examples demonstrating key features of the Universal Graph System.

## Prerequisites

```bash
bun install
```

## Examples

### 01-system-actors-basics.ts

**Demonstrates:** Core system actors (Scheduler, Storage, HTTPClient)

```bash
bun run examples/01-system-actors-basics.ts
```

**Key Concepts:**
- Using SchedulerActor for delayed operations
- Storing data with StorageActor
- Making HTTP requests with HTTPClientActor
- Actor message patterns (ask/tell)

**Output:**
- Shows task processing with scheduled completion
- Demonstrates integration of multiple system actors
- Displays final task state retrieval

---

### 02-websocket-realtime.ts

**Demonstrates:** WebSocketActor for real-time communication

```bash
bun run examples/02-websocket-realtime.ts
```

**Key Concepts:**
- Connecting to WebSocket servers
- Port-based event streaming
- Bidirectional messaging
- Connection lifecycle management

**Output:**
- Starts local WebSocket server for demo
- Shows connection establishment
- Receives real-time updates
- Demonstrates message echo

---

### 03-path-based-routing.ts

**Demonstrates:** Hierarchical actor organization with path-based addressing

```bash
bun run examples/03-path-based-routing.ts
```

**Key Concepts:**
- Path-based addressing (`/workflows/tasks/task-123`)
- Namespace isolation (workflows vs domain)
- Per-namespace system actors with different configs
- Access control through routing

**Output:**
- Shows workflow and domain actors executing independently
- Demonstrates namespace isolation
- Displays different storage configurations per namespace

---

## Key Patterns

### System Actor Registration

```typescript
import { MessageRouter } from '../src/messaging/router.ts';
import { GraphStore } from '../src/graph.ts';
import { SchedulerActor, StorageActor, HTTPClientActor } from '../src/system-actors/index.ts';

const store = new GraphStore();
const router = new MessageRouter(store);

// Register system actors
const scheduler = new SchedulerActor(router, { clock: 'real' });
router.registerActor('/system/scheduler', scheduler);

const storage = new StorageActor('storage', router, {
  allowedKeys: ['*'],
  maxSize: 10 * 1024 * 1024
});
router.registerActor('/system/storage', storage);

const http = new HTTPClientActor('http', router, {
  methods: ['GET', 'POST'],
  allowedHosts: ['api.example.com'],
  rateLimit: { requests: 100, window: 60000 },
  timeout: 5000
});
router.registerActor('/system/http', http);
```

### Using System Actors

```typescript
import { Actor } from '../src/messaging/actor.ts';
import { address } from '../src/messaging/message.ts';

class MyActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // Schedule delayed operation
    await this.schedule(5000, 'delayed-task');

    // Store data
    await this.ask(
      address('/system/storage'),
      'storage.set',
      { key: 'my-key', value: { data: 'example' } }
    );

    // Make HTTP request
    const response = await this.ask(
      address('/system/http'),
      'http.get',
      { url: 'https://api.example.com/data' }
    );

    return createResponse(message, { success: true });
  }
}
```

### Path-Based Addressing

```typescript
// Register actors at hierarchical paths
router.registerActor('/workflows/tasks/task-123', taskActor);
router.registerActor('/workflows/system/storage', workflowStorage);
router.registerActor('/domain/system/storage', domainStorage);

// Send messages using path addresses
await actor.ask(
  address('/workflows/tasks/task-123'),
  'execute',
  {}
);

// Different namespaces have isolated resources
// /workflows/system/storage !== /domain/system/storage
```

## Architecture

All examples follow the **pure actor model**:

1. **Capabilities ARE actors** - No helper classes or wrappers
2. **Access control via routing** - Registration determines permissions
3. **Message-based communication** - All interactions through messages
4. **Namespace isolation** - Path-based organization enables separation

## Next Steps

- Read `ARCHITECTURE.md` for system overview
- Read `PATH_ADDRESSING.md` for routing details
- Read `docs/PHASE_3_GUIDE.md` for advanced features
- Explore `src/system-actors/` for implementation details

## Common Issues

### "Actor not found" errors

**Cause:** Actor not registered at expected path

**Solution:** Check registration path matches address used in messages

```typescript
// Registration
router.registerActor('/workflows/system/http', httpActor);

// Usage (must match)
await actor.ask(address('/workflows/system/http'), 'http.get', { ... });
```

### Rate limit errors

**Cause:** HTTPClientActor rate limit exceeded

**Solution:** Adjust rate limit config or space out requests

```typescript
const http = new HTTPClientActor('http', router, {
  // ... other config
  rateLimit: {
    requests: 100,  // Increase if needed
    window: 60000   // Per 60 seconds
  }
});
```

### Host whitelist errors

**Cause:** Attempting to connect to non-whitelisted host

**Solution:** Add host to allowedHosts config

```typescript
const http = new HTTPClientActor('http', router, {
  // ... other config
  allowedHosts: [
    'api.example.com',
    'other-api.com'  // Add hosts as needed
  ]
});
```

## More Examples

For additional examples, see:
- `src/query/examples-phase3.ts` - Query system examples
- `src/query/reactive/example-subscribe.ts` - Reactive subscriptions
- `examples/workflows/README.md` - Workflow orchestration

---

**Questions?** See `docs/` directory for comprehensive documentation.
