# Signal Hub - Quick Start Guide

Get started with Signal Hub in 5 minutes.

## Prerequisites

- Node.js 20+
- pnpm 10+
- Cloudflare account (for deployment only)

## Local Development

### 1. Install Dependencies

```bash
cd services/signal-hub
pnpm install
```

### 2. Start Development Server

```bash
pnpm dev
```

Signal Hub will start on `http://localhost:8787`

**Endpoints:**
- WebSocket: `ws://localhost:8787/ws`
- Health check: `http://localhost:8787/health`

### 3. Test the Connection

Open a new terminal and test the health endpoint:

```bash
curl http://localhost:8787/health
```

Expected response:
```json
{
  "status": "ok",
  "service": "signal-hub",
  "version": "0.1.0",
  "timestamp": 1739732123456
}
```

### 4. Run Example Client

```bash
# Install ws package for example client
pnpm add -D ws @types/node tsx

# Run example
tsx examples/client.ts
```

You should see:
```
Signal Hub Client Demo

1. Connecting to Signal Hub...
WebSocket connected
Connected! Session ID: session-abc123...

2. Registering actor...
[hub:registered] { ... }

3. Discovering actors...
[hub:discovered] { ... }

...
```

## Testing

```bash
# Run all tests
pnpm test

# Watch mode
pnpm test:watch

# Type check
pnpm typecheck
```

## Basic Usage

### Connect to Signal Hub

```typescript
import WebSocket from 'ws';

const ws = new WebSocket('ws://localhost:8787/ws');

ws.on('open', () => {
  // Send hub:connect
  ws.send(JSON.stringify({
    id: crypto.randomUUID(),
    from: '@(browser/my-actor)',
    to: '@(cloudflare/signal-hub)',
    type: 'hub:connect',
    pattern: 'ask',
    correlationId: null,
    timestamp: Date.now(),
    payload: null,
    metadata: {
      protocolVersion: '0.1.0',
      capabilities: ['send', 'broadcast']
    },
    ttl: 5000,
    signature: null
  }));
});

ws.on('message', (data) => {
  const msg = JSON.parse(data.toString());
  console.log('Received:', msg.type);
});
```

### Register an Actor

```typescript
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(browser/my-actor)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:register',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    actorAddress: '@(browser/my-actor)',
    capabilities: ['render', 'compute'],
    metadata: { type: 'widget' },
    ttlSeconds: 300
  },
  metadata: { renewOnHeartbeat: true },
  ttl: 5000,
  signature: null
}));
```

### Send a Message

```typescript
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(browser/sender)',
  to: '@(browser/receiver)',    // Final destination
  type: 'hub:send',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    type: 'task:assign',        // Application message type
    data: { taskId: '123' }     // Application data
  },
  metadata: {
    via: '@(cloudflare/signal-hub)',
    requireAck: true
  },
  ttl: 30000,
  signature: null
}));
```

### Broadcast

```typescript
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(local/coordinator)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:broadcast',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    type: 'system:announcement',
    data: { message: 'Server maintenance in 5 minutes' },
    excludeSelf: true
  },
  metadata: {},
  ttl: null,
  signature: null
}));
```

### Subscribe to Topic

```typescript
// Subscribe
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(browser/subscriber)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:subscribe',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: { topic: 'events', durable: false },
  metadata: {},
  ttl: 5000,
  signature: null
}));

// Publish
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(local/publisher)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:publish',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    topic: 'events',
    type: 'event:created',
    data: { eventId: '123', name: 'New Event' }
  },
  metadata: {},
  ttl: null,
  signature: null
}));
```

## Configuration

Edit `wrangler.toml` to customize:

```toml
[vars]
PROTOCOL_VERSION = "0.1.0"
MAX_MESSAGE_SIZE = "1048576"  # 1MB
HEARTBEAT_INTERVAL = "30000"  # 30s
ACTOR_REGISTRY_LIMIT = "50000"
DEFAULT_ACTOR_TTL = "300000"  # 5 min
```

## Deployment to Cloudflare

### 1. Authenticate

```bash
wrangler login
```

### 2. Set JWT Secret

```bash
# Generate a secure secret
openssl rand -base64 32

# Set in Cloudflare
wrangler secret put JWT_SECRET --env production
```

### 3. Deploy

```bash
pnpm deploy
```

Your Signal Hub will be available at:
`wss://signal-hub.<your-account>.workers.dev/ws`

### 4. Test Production

```bash
curl https://signal-hub.<your-account>.workers.dev/health
```

## Troubleshooting

**WebSocket connection fails:**
- Ensure Signal Hub is running: `pnpm dev`
- Check port 8787 is not in use: `lsof -i :8787`
- Verify WebSocket URL: `ws://localhost:8787/ws` (not `wss://` locally)

**Authentication errors:**
- Set `AUTH_ENABLED="false"` in wrangler.toml for development
- Generate valid JWT for production (see README.md)

**Message not delivered:**
- Check target actor is registered: Send `hub:list_actors`
- Verify actor hasn't expired (default TTL: 5 minutes)
- Check WebSocket is still connected

**Tests failing:**
- Clear node_modules: `rm -rf node_modules && pnpm install`
- Rebuild protocols package: `cd ../../packages/protocols && pnpm build`

## Next Steps

- Read [README.md](./README.md) for full documentation
- Review [Protocol Specification](/docs/signal-hub/PROTOCOL.md)
- Explore [Message Types](/docs/signal-hub/MESSAGE_TYPES.md)
- Check [Security Model](/docs/signal-hub/SECURITY.md)

## Support

- Issues: https://github.com/BrianLN-AI/agentic-primer/issues
- Protocol questions: See `/docs/signal-hub/PROTOCOL.md`
