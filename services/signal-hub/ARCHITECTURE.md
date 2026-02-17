# Signal Hub Architecture

**Version:** 0.1.0
**Last Updated:** 2026-02-17
**Status:** Production

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Component Architecture](#component-architecture)
3. [Data Flow](#data-flow)
4. [State Management](#state-management)
5. [Protocol Boundaries](#protocol-boundaries)
6. [Client Architecture](#client-architecture)
7. [Deployment Architecture](#deployment-architecture)
8. [Error Handling](#error-handling)
9. [Scalability & Constraints](#scalability--constraints)
10. [Testing Strategy](#testing-strategy)
11. [Security Model](#security-model)

---

## System Overview

### What is Signal Hub?

Signal Hub is a **WebSocket-based message router** running on Cloudflare Durable Objects that enables real-time communication between actors across different runtime environments (SEAG local, browser, Beam).

**Problem it solves:**
- Actors in different runtimes (Node.js SEAG, browser) cannot directly communicate
- Need for real-time bidirectional messaging with discovery and pub/sub
- Requirement for globally accessible, scalable infrastructure

**Solution:**
- Cloudflare Durable Objects provide globally distributed WebSocket server
- Centralized actor registry for discovery
- Message routing with point-to-point and broadcast capabilities
- Topic-based pub/sub for event distribution

### Key Capabilities

- **Actor Discovery** - Find actors by glob patterns or capabilities
- **Point-to-Point Messaging** - Send messages to specific actors with delivery tracking
- **Broadcast** - Fan-out messages to all or filtered actors
- **Pub/Sub** - Topic-based subscriptions and message distribution
- **Backpressure** - Flow control and queue monitoring
- **Security** - JWT-based authentication and authorization

### Design Principles

1. **Use existing types** - SharedMessage from `@agentic-primer/protocols`
2. **Server-enforced security** - Clients cannot spoof addresses
3. **Graceful degradation** - Handle network failures transparently
4. **Bounded resources** - Prevent memory/CPU exhaustion
5. **Observable behavior** - Clear error messages and metrics

---

## Component Architecture

### System Context Diagram

```
┌────────────────────────────────────────────────────────────────────┐
│                      Cloudflare Edge Network                       │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │                   Cloudflare Workers                         │ │
│  │  ┌────────────────────────────────────────────────────────┐  │ │
│  │  │            Signal Hub Durable Object                   │  │ │
│  │  │                                                        │  │ │
│  │  │  ┌──────────────┐      ┌──────────────┐             │  │ │
│  │  │  │Actor Registry│      │ WebSocket    │             │  │ │
│  │  │  │ (In-Memory)  │      │ Connections  │             │  │ │
│  │  │  └──────────────┘      └──────────────┘             │  │ │
│  │  │                                                        │  │ │
│  │  │  ┌──────────────┐      ┌──────────────┐             │  │ │
│  │  │  │Subscriptions │      │ Queue Stats  │             │  │ │
│  │  │  │   (Topics)   │      │ (Monitoring) │             │  │ │
│  │  │  └──────────────┘      └──────────────┘             │  │ │
│  │  └────────────────────────────────────────────────────────┘  │ │
│  └──────────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────────┘
           ↑                                            ↑
           │ WSS                                        │ WSS
           │                                            │
┌──────────┴──────────┐                    ┌───────────┴──────────┐
│   SEAG Runtime      │                    │  Browser Runtime     │
│   (Node.js)         │                    │  (Chrome + MCP)      │
│                     │                    │                      │
│  ┌──────────────┐   │                    │  ┌──────────────┐    │
│  │SignalHub     │   │                    │  │SignalHub     │    │
│  │Client (SEAG) │   │                    │  │Client (Web)  │    │
│  └──────┬───────┘   │                    │  └──────┬───────┘    │
│         │           │                    │         │            │
│  ┌──────┴───────┐   │                    │  ┌──────┴───────┐    │
│  │LocalActor123 │   │                    │  │WidgetActor456│    │
│  │@(seag/123)   │   │                    │  │@(browser/456)│    │
│  └──────────────┘   │                    │  └──────────────┘    │
└─────────────────────┘                    └──────────────────────┘
```

### Core Components

#### 1. Signal Hub Service (Durable Object)

**Location:** `services/signal-hub/src/durable-objects/SignalHub.ts`

**Purpose:**
Central WebSocket server that routes messages between actors across different runtimes.

**Responsibilities:**
- Accept and manage WebSocket connections
- Authenticate connections via JWT
- Maintain actor registry (actorAddress → WebSocket)
- Route messages between actors
- Manage pub/sub subscriptions
- Enforce rate limiting and backpressure
- Track queue statistics

**State Owned:**
- `sessions: Map<WebSocket, Session>` - Active WebSocket sessions
- `connections: Map<string, WebSocket>` - Session ID → WebSocket lookup
- `registry: Map<string, ActorRegistration>` - Actor address → registration info
- `subscriptions: Map<string, Set<CanonicalAddress>>` - Topic → subscribers
- `queueStats: QueueStats` - Message queue monitoring

**Deployment:**
- Runs as Cloudflare Durable Object (one instance per DO)
- Globally distributed (auto-routed to nearest edge location)
- Hibernation-aware (heartbeat prevents 30s idle timeout)

**Interface:**
- HTTP endpoint: `GET /ws` → WebSocket upgrade
- WebSocket protocol: SharedMessage (JSON)

#### 2. Worker Entry Point

**Location:** `services/signal-hub/src/index.ts`

**Purpose:**
Routes HTTP requests to appropriate Durable Object instance.

**Responsibilities:**
- Extract DO instance ID from request
- Route to SignalHub Durable Object
- Handle CORS if needed

**Note:** In MVP, single DO instance (no sharding)

#### 3. Message Handlers

**Location:** `services/signal-hub/src/handlers/`

**Purpose:**
Isolated business logic for each message type category.

**Modules:**

- **connection.ts** - Connection lifecycle
  - `hub:connect` → authenticate, create session
  - `hub:heartbeat` → prevent hibernation, renew actors
  - `hub:disconnect` → cleanup session and registrations

- **registration.ts** - Actor registry management
  - `hub:register` → add actor to registry
  - `hub:unregister` → remove actor from registry
  - `hub:discover` → find actors by glob pattern
  - `hub:list_actors` → list all registered actors
  - `hub:renew` → extend actor TTL

- **messaging.ts** - Message routing
  - `hub:send` → route message to specific actor (flat payload unwrap)
  - `hub:broadcast` → fan-out to all or filtered actors

- **pubsub.ts** - Topic subscriptions
  - `hub:subscribe` → subscribe to topic
  - `hub:publish` → publish message to topic subscribers
  - `hub:unsubscribe` → unsubscribe from topic

- **flowcontrol.ts** - Backpressure management
  - `hub:queue_stats` → return queue statistics
  - `hub:pause` / `hub:resume` → flow control (Phase 2)

**Design:**
- Pure functions (no direct DO state mutation)
- Return messages to send or errors to handle
- Testable in isolation

#### 4. Browser Client

**Location:** `packages/signal-hub-client/src/SignalHubClient.ts`

**Purpose:**
WebSocket client for browser environments (Chrome extension, web apps).

**Responsibilities:**
- Establish WebSocket connection to Signal Hub
- Send hub:connect with JWT authentication
- Register actors with hub:register
- Send messages via hub:send (handles flat payload wrapping)
- Receive messages and dispatch to handlers
- Maintain heartbeat to prevent hibernation
- Reconnect with exponential backoff on disconnect

**API:**
```typescript
const client = new SignalHubClient({
  url: 'wss://signal-hub.example.com',
  jwt: 'eyJhbGci...',
  protocolVersion: '0.1.0',
});

await client.connect();
await client.registerActor('@(browser/widget-123)', ['render']);
await client.send({
  to: '@(seag/coordinator)',
  type: 'app:message',
  payload: { data: 'hello' },
});
client.on('message', (msg) => console.log(msg));
```

**State Owned:**
- Connection state (disconnected, connecting, connected)
- Registered actors (Set<CanonicalAddress>)
- Message queue (pending messages during reconnect)
- Pending asks (awaiting ack)

#### 5. SEAG Client

**Location:** `ugs/src/messaging/signal-hub/client.ts`

**Purpose:**
WebSocket client for Node.js environments (SEAG runtime).

**Responsibilities:**
- Same as browser client, but Node.js runtime
- Uses `ws` library instead of browser WebSocket
- Auto-registers local actors on connect

**API:**
Identical to browser client (runtime-agnostic interface)

**Integration:**
- SEAG actors use client to send/receive messages to/from browser
- Client handles protocol translation (SharedMessage ↔ hub:send flat payload)

---

## Data Flow

### Actor Registration Flow

```
Client                          Signal Hub                  Registry
  │                                 │                          │
  ├─hub:connect──────────────────>│                          │
  │  (JWT)                         ├─validate JWT             │
  │                                ├─create session           │
  │<──hub:connected────────────────┤                          │
  │                                │                          │
  ├─hub:register──────────────────>│                          │
  │  actorAddress: @(browser/123)  ├─check rate limit         │
  │  capabilities: ['render']      ├─add to registry─────────>│
  │  ttlSeconds: 300               │                          │
  │                                ├─<registry updated>───────┤
  │<──hub:registered───────────────┤                          │
  │  expiresAt, renewalToken       │                          │
```

**Key Points:**
- Registration requires authenticated connection (hub:connect first)
- Actor address must match JWT actorId claim (server-enforced)
- TTL enforced (default 5 min, max 1 hour)
- Duplicate registrations: last-write-wins with version tracking

### Message Routing Flow

```
Sender                    Signal Hub                     Receiver
  │                           │                             │
  ├─hub:send─────────────────>│                             │
  │  to: @(browser/widget-123)├─lookup registry             │
  │  type: 'hub:send'         ├─found: @(browser/widget-123)│
  │  payload:                 ├─unwrap payload.type         │
  │    type: 'app:task'       ├─unwrap payload.data         │
  │    data: {...}            ├─forward──────────────────────>│
  │                           │  to: @(browser/widget-123)  │
  │                           │  type: 'app:task'           │
  │                           │  payload: {...}             │
  │                           │                             ├─process
  │<──hub:delivery_ack────────┤                             │
  │  messageId, deliveredAt   │                             │
```

**Critical: Flat Payload Structure**

Per protocol spec (commits 63ed8e8, 12f3c41), `hub:send` uses flat structure:

```typescript
// Client sends
{
  type: 'hub:send',
  payload: {
    type: 'app:task',       // Application message type
    data: { taskId: '456' } // Application payload (NOT nested!)
  }
}

// Hub forwards as
{
  type: 'app:task',          // From payload.type
  payload: { taskId: '456' } // From payload.data (unwrapped!)
}
```

**Why?** Hub acts as transparent router. Receiver sees application message, not hub:send wrapper.

### Pub/Sub Flow

```
Publisher              Signal Hub               Subscriber A    Subscriber B
  │                        │                         │              │
  │                        │<─hub:subscribe──────────┤              │
  │                        │  topic: 'events'        │              │
  │                        ├─add to subscriptions    │              │
  │                        │──hub:subscribed────────>│              │
  │                        │                         │              │
  │                        │<─hub:subscribe──────────┼──────────────┤
  │                        │  topic: 'events'        │              │
  │                        ├─add to subscriptions    │              │
  │                        │──hub:subscribed────────>┼─────────────>│
  │                        │                         │              │
  ├─hub:publish───────────>│                         │              │
  │  topic: 'events'       ├─lookup subscribers      │              │
  │  type: 'event:created' ├─forward────────────────>│              │
  │  data: {...}           ├─forward────────────────>┼─────────────>│
  │                        │                         │              │
```

**Key Points:**
- Subscriptions stored in `Map<topic, Set<actorAddress>>`
- Publisher does not know subscribers (decoupled)
- Hub handles fan-out (sync for <100, async queue for >100 in Phase 2)

---

## State Management

### Session State

**Owner:** Signal Hub Durable Object

**Lifecycle:**
- Created: On WebSocket connection (before auth)
- Updated: On hub:connect (authenticated), hub:heartbeat (timestamp)
- Destroyed: On WebSocket close or hub:disconnect

**Structure:**
```typescript
interface Session {
  sessionId: string;                 // Unique session ID
  actorIdentity: CanonicalAddress | null; // Verified from JWT
  capabilities: string[];            // From JWT claims
  connectedAt: number;               // Connection timestamp
  lastHeartbeat: number;             // Last heartbeat timestamp
  authenticated: boolean;            // JWT validated
  paused: boolean;                   // Backpressure state
}
```

**Storage:**
- In-memory: `Map<WebSocket, Session>`
- Lookup: `Map<string, WebSocket>` (sessionId → WebSocket)

**Not Persistent:** Sessions lost on DO restart (clients reconnect)

### Actor Registry

**Owner:** Signal Hub Durable Object

**Purpose:**
Map actor addresses to active WebSocket connections for routing.

**Lifecycle:**
- Created: On hub:register
- Updated: On hub:renew (TTL extension)
- Destroyed: On hub:unregister, TTL expiration, or session disconnect

**Structure:**
```typescript
interface ActorRegistration {
  actorAddress: CanonicalAddress;    // Unique actor address
  capabilities: string[];            // Actor capabilities
  metadata: Record<string, unknown>; // Custom metadata
  connectionId: string;              // Session ID
  registeredAt: number;              // Registration timestamp
  expiresAt: number;                 // TTL expiration
  version: number;                   // For conflict resolution
  renewalToken: string;              // For hub:renew
}
```

**Storage:**
- In-memory: `Map<string, ActorRegistration>` (actorAddress → registration)
- Not persistent: Phase 1 MVP (Phase 2: Durable Object storage or D1)

**Cleanup:**
- On disconnect: Remove all registrations for session
- On TTL expiry: Background task removes expired entries
- On duplicate: Last-write-wins (version increment)

### Subscriptions

**Owner:** Signal Hub Durable Object

**Purpose:**
Map topics to subscriber actor addresses for pub/sub routing.

**Lifecycle:**
- Created: On hub:subscribe
- Destroyed: On hub:unsubscribe or session disconnect

**Structure:**
```typescript
// Internal storage
Map<string, Set<CanonicalAddress>>
// Example:
{
  'events': Set(['@(browser/widget-123)', '@(seag/logger)']),
  'metrics': Set(['@(browser/dashboard)'])
}
```

**Cleanup:**
- On disconnect: Remove actor from all topic subscriber sets
- On hub:unsubscribe: Remove actor from specific topic

**Not Persistent:** Subscriptions lost on DO restart (clients re-subscribe)

### Queue Statistics

**Owner:** Signal Hub Durable Object

**Purpose:**
Monitor message processing for backpressure and observability.

**Structure:**
```typescript
interface QueueStats {
  pending: number;   // Messages awaiting delivery
  processed: number; // Total messages processed
  failed: number;    // Delivery failures
  paused: boolean;   // Backpressure active
}
```

**Updated:**
- Increment `processed` on successful delivery
- Increment `failed` on delivery error
- Update `pending` based on queue depth (Phase 2)

---

## Protocol Boundaries

### Wire Protocol (SharedMessage)

**Definition:** `@agentic-primer/protocols/shared-message`

**Responsibility:**
Message format only. Does NOT define routing logic, state management, or error handling.

**Structure:**
```typescript
interface SharedMessage {
  id: string;                      // UUID
  from: CanonicalAddress;          // @(path) sender
  to: CanonicalAddress;            // @(path) recipient
  type: string;                    // Message type (hub:* or app:*)
  payload: unknown;                // Arbitrary payload
  pattern: 'tell' | 'ask';         // Fire-and-forget vs request-response
  correlationId: string | null;    // For ask/tell correlation
  timestamp: number;               // Epoch ms
  metadata: Record<string, unknown>; // Extensible metadata
  ttl: number | null;              // Time-to-live (ms)
  signature: string | null;        // HMAC signature (Phase 2)
}
```

**Used By:**
- All hub:* messages (connection, registration, messaging, pub/sub)
- Application messages forwarded by hub

### Hub Protocol (hub:* messages)

**Examples:**
- hub:connect, hub:register, hub:send, hub:broadcast, hub:subscribe

**Purpose:**
Hub-specific control messages for connection management, discovery, and routing.

**Separate From:**
- Application messages (app:*, custom types)
- Hub only routes application messages; does not interpret them

**Critical Distinction:**
- `hub:send` is a routing command (Signal Hub understands)
- `app:task` is an application message (Signal Hub does NOT interpret, only forwards)

### Error Handling Protocol

**Error Types:**
```typescript
type HubErrorCode =
  | 'version_mismatch'    // Protocol version incompatible
  | 'unauthorized'        // JWT invalid or missing capabilities
  | 'rate_limited'        // Too many requests
  | 'unknown_actor'       // Target actor not registered
  | 'message_too_large'   // Exceeds 1MB limit
  | 'message_expired'     // TTL exceeded
  | 'timeout'             // Request timeout
  | 'internal_error';     // Server error
```

**Error Message:**
```typescript
{
  type: 'hub:error',
  pattern: 'tell',
  correlationId: 'failed-message-id',
  payload: {
    code: 'unknown_actor',
    message: 'Actor @(browser/widget-999) not registered',
    details: { actorAddress: '@(browser/widget-999)' }
  }
}
```

---

## Client Architecture

### Browser Client Architecture

**File:** `packages/signal-hub-client/src/SignalHubClient.ts`

**State Machine:**
```
┌─────────────┐   connect()   ┌────────────┐   hub:connected   ┌───────────┐
│disconnected │──────────────>│ connecting │─────────────────>│ connected │
└─────────────┘               └────────────┘                   └───────────┘
      ↑                              │                                │
      │                              │ error/timeout                  │
      │                              ↓                                │
      │                       ┌────────────┐                          │
      └───────────────────────┤reconnecting│<─────────────────────────┘
                              └────────────┘   close (auto-reconnect)
```

**Reconnection Strategy:**
- Exponential backoff: 1s → 2s → 4s → 8s → 16s → 30s (max)
- Max attempts: 10 (configurable)
- Message queue: Buffer messages during reconnect, flush on connect
- Actor re-registration: Automatic on reconnect

**Heartbeat:**
- Interval: 25s (< 30s Cloudflare hibernation threshold)
- Client sends hub:heartbeat
- Server responds with hub:heartbeat_ack
- Client tracks last ack timestamp (detect dead connections)

### SEAG Client Architecture

**File:** `ugs/src/messaging/signal-hub/client.ts`

**Identical to browser client:**
- Same state machine
- Same reconnection strategy
- Same heartbeat mechanism

**Differences:**
- Uses `ws` library (Node.js) instead of browser WebSocket
- Auto-registers local actors from SEAG actor registry

**Integration with SEAG:**
- SEAG ActorSystem creates SignalHubClient
- Actors send messages via `actorSystem.send()`
- ActorSystem routes to SignalHubClient for remote:// addresses
- SignalHubClient wraps in hub:send, sends to Signal Hub
- Hub forwards to target actor in browser/Beam

---

## Deployment Architecture

### Cloudflare Infrastructure

**Workers:**
- Entry point: `services/signal-hub/src/index.ts`
- Handles HTTP requests
- Routes to Durable Object

**Durable Objects:**
- Class: `SignalHub`
- Instances: Currently 1 (MVP - no sharding)
- Location: Automatically routed to nearest Cloudflare edge
- Persistence: Durable Object storage (Phase 2 - not used in Phase 1)

**Configuration:**
- `wrangler.toml` defines environment variables
- JWT_SECRET stored via `wrangler secret put`

### Environment Variables

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
| `JWT_SECRET` | (secret) | JWT signing secret |

### Deployment Workflow

```bash
# Development
cd services/signal-hub
pnpm dev  # Local dev server (Miniflare)

# Testing
pnpm test  # Vitest integration tests

# Production deployment
pnpm deploy  # Deploy to Cloudflare

# Set secrets
wrangler secret put JWT_SECRET --env production
```

---

## Error Handling

### Client Errors

**Connection Failures:**
- **Cause:** Network error, server down, invalid URL
- **Handling:** Client retries with exponential backoff
- **User Impact:** Queued messages delivered after reconnect

**Message Validation:**
- **Cause:** Invalid SharedMessage structure
- **Handling:** Client throws error, does not send
- **User Impact:** Application must handle send() rejection

**Authentication Failures:**
- **Cause:** Invalid JWT, expired token
- **Handling:** hub:error with code `unauthorized`
- **User Impact:** Client disconnects, requires new JWT

### Hub Errors

**Unknown Actor:**
- **Trigger:** hub:send to unregistered actor
- **Response:** hub:error with code `unknown_actor`
- **Payload:**
  ```typescript
  {
    code: 'unknown_actor',
    message: 'Actor @(browser/widget-999) not registered',
    details: { actorAddress: '@(browser/widget-999)' }
  }
  ```

**Unauthorized:**
- **Trigger:** JWT missing, invalid, or insufficient capabilities
- **Response:** hub:error with code `unauthorized`
- **Action:** Client must reconnect with valid JWT

**Rate Limited:**
- **Trigger:** Too many requests in time window (token bucket)
- **Response:** hub:error with code `rate_limited`
- **Payload:**
  ```typescript
  {
    code: 'rate_limited',
    message: 'Rate limit exceeded',
    details: { retryAfter: 5000 } // ms
  }
  ```

**Message Too Large:**
- **Trigger:** Message exceeds 1MB (Cloudflare WebSocket limit)
- **Response:** hub:error with code `message_too_large`

### Error Recovery Patterns

**Transient Errors:**
- Network failures → Reconnect with backoff
- Rate limits → Exponential backoff with jitter

**Permanent Errors:**
- Unauthorized → Require new JWT, manual intervention
- Unknown actor → Wait for actor registration, or fail fast

**Partial Failures:**
- Broadcast to 100 actors, 5 fail → hub:broadcast_ack includes failure count
- Pub/sub delivery → Best-effort (no acks in Phase 1)

---

## Scalability & Constraints

### Cloudflare Workers Limits

| Limit | Value | Impact |
|-------|-------|--------|
| CPU time | 10ms (free) / 30s (paid) | Use async queues for large broadcasts |
| WebSocket connections | ~1000 per DO | Shard actors across multiple DOs |
| Message size | 1MB | Reject messages >1MB |
| Hibernation timeout | 30s idle | Heartbeat every 25s prevents |
| Storage | Unlimited (billed) | Phase 2: Persist registry |

### Design Decisions for Scale

**Single Durable Object (Phase 1):**
- All actors connect to same DO
- Limits: ~1000 concurrent connections
- Routing: O(1) lookup in registry Map
- Broadcast: O(N) where N = connected actors

**Sharding Strategy (Phase 2):**
- Hash actor address → DO instance ID
- Cross-shard routing via DO-to-DO calls
- Discovery: Scatter-gather across shards

**Broadcast Optimization:**
- Sync delivery: <100 actors (within 30s CPU limit)
- Async queue: ≥100 actors (Cloudflare Queues)

**Registry Size:**
- Limit: 50K actors per instance (configurable)
- Memory: ~50KB per actor × 50K = ~2.5GB (Phase 2: move to storage)

### Performance Characteristics

**Latency:**
- WebSocket RTT: <50ms (Cloudflare edge proximity)
- Message routing: <5ms (in-memory registry lookup)
- Broadcast (100 actors): ~100ms (synchronous)

**Throughput:**
- Messages/sec: ~10K (single DO, limited by CPU)
- Connections/sec: ~100 (WebSocket upgrade overhead)

**Bottlenecks:**
- CPU: 30s limit for synchronous processing
- Memory: Actor registry size (bounded by limit)
- Network: Cloudflare egress bandwidth (unlimited for WebSocket)

---

## Testing Strategy

### Unit Tests

**Location:** `services/signal-hub/src/handlers/__tests__/`

**Coverage:**
- Handler logic in isolation (no DO state)
- Message validation
- Error cases (missing fields, invalid types)
- Edge cases (expired TTL, duplicate registration)

**Example:**
```typescript
// services/signal-hub/src/handlers/__tests__/registration.test.ts
test('handleRegister adds actor to registry', () => {
  const session = createMockSession();
  const message = createRegisterMessage();
  const result = handleRegister(message, session, registry, env);
  expect(result.type).toBe('hub:registered');
  expect(registry.has(message.payload.actorAddress)).toBe(true);
});
```

### Integration Tests

**Location:** `tests/integration/signal-hub/`

**Coverage:**
- Cross-runtime messaging (SEAG ↔ Browser)
- Actor discovery (glob patterns)
- Pub/sub delivery (topic subscriptions)
- Connection lifecycle (connect, heartbeat, disconnect)
- Error handling (unknown actor, unauthorized)

**Setup:**
- Vitest test runner
- Miniflare for local Durable Object testing
- SignalHubClient (browser) + SignalHubClient (SEAG)

**Example:**
```typescript
// tests/integration/signal-hub/messaging.test.ts
test('SEAG actor sends message to browser actor', async () => {
  const seagClient = new SignalHubClient({ ... });
  const browserClient = new SignalHubClient({ ... });

  await seagClient.connect();
  await browserClient.connect();

  await seagClient.registerActor('@(seag/sender)', []);
  await browserClient.registerActor('@(browser/receiver)', []);

  const received = new Promise((resolve) => {
    browserClient.on('message', resolve);
  });

  await seagClient.send({
    to: '@(browser/receiver)',
    type: 'test:message',
    payload: { data: 'hello' },
  });

  const msg = await received;
  expect(msg.type).toBe('test:message');
  expect(msg.payload.data).toBe('hello');
});
```

### E2E Tests

**Coverage:**
- Full application scenarios (multi-actor coordination)
- Real Cloudflare Workers deployment (not Miniflare)
- Browser extension + SEAG integration
- Performance/load testing (Phase 2)

**Tools:**
- Playwright for browser automation
- Cloudflare Workers deployed to test environment

---

## Security Model

### Authentication

**Mechanism:** JWT (JSON Web Tokens)

**Flow:**
1. Client obtains JWT from auth service (outside Signal Hub)
2. Client includes JWT in hub:connect metadata
3. Hub validates JWT signature and claims
4. Hub extracts actorId and capabilities from JWT

**JWT Structure:**
```typescript
{
  sub: 'user-123',              // User ID
  actorId: 'browser/client-ui', // Actor address path
  capabilities: ['send', 'broadcast', 'subscribe'],
  iss: 'signal-hub',
  exp: 1739731234567            // Unix timestamp
}
```

**Validation:**
- Signature: HMAC-SHA256 with JWT_SECRET
- Expiration: Reject if `exp < Date.now()`
- Claims: Verify actorId format, capabilities array

**Library:** `jose` (standards-compliant JWT library)

### Authorization

**Actor Address Verification:**
- Client claims actorId in JWT
- Client registers actor with actorAddress
- Hub verifies: `actorAddress` path matches JWT `actorId`
- **Prevents:** Actor spoofing (client cannot impersonate other actors)

**Capability-Based:**
- JWT includes capabilities: `['send', 'broadcast', 'subscribe']`
- Hub checks capability before allowing operation
- **Example:** `hub:broadcast` requires `broadcast` capability

**Rate Limiting:**
- Token bucket algorithm per session
- Default: 100 requests/min (configurable)
- Prevents: DoS attacks, resource exhaustion

### Message Integrity (Phase 2)

**HMAC Signatures:**
- SharedMessage includes `signature` field
- Sender signs message with shared secret
- Hub verifies signature before routing
- **Prevents:** Message tampering, replay attacks

**Not Implemented in Phase 1:** All clients trusted (JWT auth sufficient)

---

## Appendix: Key Files Reference

| Component | File Path | Description |
|-----------|-----------|-------------|
| Durable Object | `services/signal-hub/src/durable-objects/SignalHub.ts` | Main WebSocket server |
| Worker Entry | `services/signal-hub/src/index.ts` | HTTP request router |
| Connection Handler | `services/signal-hub/src/handlers/connection.ts` | hub:connect, heartbeat, disconnect |
| Registration Handler | `services/signal-hub/src/handlers/registration.ts` | hub:register, discover, list, renew |
| Messaging Handler | `services/signal-hub/src/handlers/messaging.ts` | hub:send, broadcast |
| Pub/Sub Handler | `services/signal-hub/src/handlers/pubsub.ts` | hub:subscribe, publish, unsubscribe |
| Flow Control Handler | `services/signal-hub/src/handlers/flowcontrol.ts` | hub:queue_stats |
| JWT Auth | `services/signal-hub/src/auth/jwt.ts` | JWT validation |
| Types | `services/signal-hub/src/types.ts` | TypeScript interfaces |
| Browser Client | `packages/signal-hub-client/src/SignalHubClient.ts` | WebSocket client (browser) |
| SEAG Client | `ugs/src/messaging/signal-hub/client.ts` | WebSocket client (Node.js) |
| Protocol Spec | `docs/signal-hub/PROTOCOL.md` | Complete protocol documentation |
| Integration Tests | `tests/integration/signal-hub/*.test.ts` | Cross-runtime tests |

---

**End of Architecture Document**
