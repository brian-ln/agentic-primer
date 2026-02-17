# Signal Hub

**WebSocket-based message router on Cloudflare Durable Objects**

Signal Hub is a real-time communication server that enables actors across different runtimes (SEAG local, browser, Beam) to discover each other and exchange messages via WebSocket connections.

## Features

- **Connection Management** - JWT authentication, heartbeat keep-alive, graceful shutdown
- **Actor Discovery** - Register actors with capabilities, discover by glob patterns
- **Point-to-Point Messaging** - Send messages to specific actors with delivery acknowledgment
- **Broadcast** - Fan-out messages to all registered actors
- **Pub/Sub** - Topic-based subscriptions and message distribution
- **Backpressure** - Flow control and queue monitoring
- **Security** - JWT-based authentication using `jose` library

## Architecture

```
┌─────────────┐           ┌──────────────┐           ┌─────────────┐
│ SEAG Actor  │──────────>│  Signal Hub  │<──────────│Browser Actor│
│  (Local)    │<──────────│ (Cloudflare) │──────────>│  (Chrome)   │
└─────────────┘           └──────────────┘           └─────────────┘
    local://                remote://do/                local://
```

### Components

- **Worker Entry Point** (`src/index.ts`) - Routes requests to Durable Object
- **SignalHub DO** (`src/durable-objects/SignalHub.ts`) - WebSocket server with hibernation
- **Handlers** - Message routing and business logic
  - `connection.ts` - hub:connect, hub:heartbeat, hub:disconnect
  - `registration.ts` - hub:register, hub:discover, hub:list_actors, hub:renew
  - `messaging.ts` - hub:send, hub:broadcast
  - `pubsub.ts` - hub:subscribe, hub:publish, hub:unsubscribe
  - `flowcontrol.ts` - hub:queue_stats, backpressure
- **Auth** (`src/auth/jwt.ts`) - JWT validation using `jose`
- **Utils** (`src/utils.ts`) - Helper functions

## Protocol

Signal Hub implements the complete protocol specification from `/docs/signal-hub/PROTOCOL.md`:

### Message Types (26 total)

| Category | Types |
|----------|-------|
| Connection | hub:connect, hub:connected, hub:heartbeat, hub:heartbeat_ack, hub:disconnect |
| Authentication | hub:refresh_token, hub:token_refreshed |
| Discovery | hub:register, hub:registered, hub:unregister, hub:discover, hub:discovered, hub:list_actors, hub:actor_list, hub:renew, hub:renewed |
| Delivery | hub:send, hub:delivery_ack, hub:broadcast, hub:broadcast_ack, hub:subscribe, hub:subscribed, hub:publish, hub:published, hub:unsubscribe |
| Flow Control | hub:pause, hub:resume, hub:queue_stats, hub:queue_stats_response |
| Errors | hub:error, hub:unknown_actor, hub:unauthorized, hub:rate_limited, hub:version_mismatch, hub:message_too_large |

### Wire Format

All messages use `SharedMessage` from `@agentic-primer/protocols`:

```typescript
interface SharedMessage {
  id: string;
  from: CanonicalAddress;
  to: CanonicalAddress;
  type: string;
  payload: unknown;
  pattern: 'tell' | 'ask';
  correlationId: string | null;
  timestamp: number;
  metadata: Record<string, unknown>;
  ttl: number | null;
  signature: string | null;
}
```

### Flat Payload Structure (CRITICAL)

Per protocol spec (commits 63ed8e8, 12f3c41), `hub:send` uses flat structure:

```typescript
// Client sends
{
  to: '@(browser/widget-123)',
  type: 'hub:send',
  payload: {
    type: 'task:assign',        // Application type
    data: { taskId: '456' }     // Application data (NOT nested!)
  }
}

// Hub forwards as
{
  to: '@(browser/widget-123)',
  type: 'task:assign',          // From payload.type
  payload: { taskId: '456' }    // From payload.data
}
```

## Setup

### Prerequisites

- Node.js 20+
- pnpm 10+
- Wrangler CLI (`pnpm add -g wrangler`)
- Cloudflare account (for deployment)

### Installation

```bash
# Install dependencies
pnpm install

# Type check
pnpm typecheck
```

### Development

```bash
# Start local dev server
pnpm dev

# Access at http://localhost:8787
```

### Testing

```bash
# Run tests
pnpm test

# Watch mode
pnpm test:watch
```

### Deployment

```bash
# Deploy to Cloudflare
pnpm deploy

# Set JWT secret (production)
wrangler secret put JWT_SECRET --env production
```

## Configuration

Environment variables are set in `wrangler.toml`:

| Variable | Default | Description |
|----------|---------|-------------|
| `PROTOCOL_VERSION` | `"0.1.0"` | Signal Hub protocol version |
| `MAX_MESSAGE_SIZE` | `"1048576"` | Max message size (1MB) |
| `HEARTBEAT_INTERVAL` | `"30000"` | Heartbeat interval (30s) |
| `ACTOR_REGISTRY_LIMIT` | `"50000"` | Max registered actors |
| `DEFAULT_ACTOR_TTL` | `"300000"` | Default registration TTL (5 min) |
| `MAX_ACTOR_TTL` | `"3600000"` | Max registration TTL (1 hour) |
| `BROADCAST_SYNC_THRESHOLD` | `"100"` | Sync broadcast limit |
| `AUTH_ENABLED` | `"false"` (dev) / `"true"` (prod) | Enable JWT auth |
| `JWT_SECRET` | Set via `wrangler secret` | JWT signing secret |

## Usage Examples

### 1. Connection with Authentication

```typescript
import { createJWT } from '@agentic-primer/signal-hub/auth/jwt';

// Generate JWT (testing)
const token = await createJWT(
  'browser/client-ui',
  'user-123',
  ['send', 'broadcast'],
  'dev-secret-change-in-production'
);

// Connect
const ws = new WebSocket('ws://localhost:8787/ws');

ws.onopen = () => {
  ws.send(JSON.stringify({
    id: crypto.randomUUID(),
    from: '@(browser/client-ui)',
    to: '@(cloudflare/signal-hub)',
    type: 'hub:connect',
    pattern: 'ask',
    correlationId: null,
    timestamp: Date.now(),
    payload: null,
    metadata: {
      protocolVersion: '0.1.0',
      authToken: `bearer ${token}`,
      capabilities: ['send', 'broadcast', 'subscribe']
    },
    ttl: 5000,
    signature: null
  }));
};
```

### 2. Actor Registration

```typescript
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(browser/widget-123)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:register',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    actorAddress: '@(browser/widget-123)',
    capabilities: ['render', 'handle-click'],
    metadata: { widgetType: 'chart' },
    ttlSeconds: 300
  },
  metadata: { renewOnHeartbeat: true },
  ttl: 5000,
  signature: null
}));
```

### 3. Point-to-Point Messaging

```typescript
// Send message to specific actor
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(local/coordinator)',
  to: '@(browser/widget-123)',  // Final destination
  type: 'hub:send',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    type: 'task:assign',          // Application type
    data: { taskId: 'task-456' }  // Application data
  },
  metadata: {
    via: '@(cloudflare/signal-hub)',
    requireAck: true
  },
  ttl: 30000,
  signature: null
}));
```

### 4. Broadcast

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
    type: 'system:shutdown',
    data: { reason: 'maintenance', shutdownAt: Date.now() + 60000 },
    excludeSelf: true
  },
  metadata: {
    targetCapability: 'compute'  // Only actors with 'compute' capability
  },
  ttl: null,
  signature: null
}));
```

### 5. Pub/Sub

```typescript
// Subscribe to topic
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(browser/widget-123)',
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

// Publish to topic
ws.send(JSON.stringify({
  id: crypto.randomUUID(),
  from: '@(local/event-source)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:publish',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    topic: 'events',
    type: 'event:created',
    data: { eventId: '123', name: 'Test Event' }
  },
  metadata: {},
  ttl: null,
  signature: null
}));
```

## Implementation Status

### Phase 1: Project Structure ✅
- [x] Package setup with dependencies
- [x] TypeScript configuration
- [x] Wrangler configuration
- [x] Type definitions

### Phase 2: Durable Object Core ✅
- [x] SignalHub DO class
- [x] WebSocket connection management
- [x] Message routing
- [x] In-memory actor registry

### Phase 3: Message Handlers ✅
- [x] Connection lifecycle (connect, heartbeat, disconnect)
- [x] Registration (register, unregister, discover, list, renew)
- [x] Messaging (send with flat payload, broadcast)
- [x] Pub/sub (subscribe, publish, unsubscribe)
- [x] Flow control (queue_stats)

### Phase 4: Security ✅
- [x] JWT validation (jose library)
- [x] Identity verification
- [x] Rate limiting (token bucket)

### Phase 5: Testing (Next)
- [ ] Unit tests for handlers
- [ ] Integration tests with Miniflare
- [ ] WebSocket connection tests
- [ ] Load testing

## Constraints

**Cloudflare Limits:**
- Max message size: 1MB (WebSocket frame limit)
- CPU limit: 30s per request
- Hibernation: 30s idle timeout (prevented by heartbeat)
- Registry limit: 50K actors per instance (configurable)

**Performance:**
- Broadcast threshold: 100 actors (sync) / >100 (async queue - Phase 2)
- Recommended heartbeat: 25s (< 30s hibernation threshold)

## Future Enhancements

**Phase 2:**
- [ ] Persistent actor registry (D1 database)
- [ ] Cross-shard routing (multi-DO)
- [ ] Async broadcast queue (Cloudflare Queues)
- [ ] Message persistence for offline actors
- [ ] Enhanced subscription management (durable subscriptions)

**Phase 3:**
- [ ] HMAC message signatures
- [ ] Advanced authorization (actor-to-actor permissions)
- [ ] Metrics and observability
- [ ] Rate limiting per actor

## License

MIT

## Related Documentation

- [Protocol Specification](/docs/signal-hub/PROTOCOL.md)
- [Message Types](/docs/signal-hub/MESSAGE_TYPES.md)
- [Security Model](/docs/signal-hub/SECURITY.md)
- [JSON Schema](/packages/protocols/schema/hub-messages.schema.json)
