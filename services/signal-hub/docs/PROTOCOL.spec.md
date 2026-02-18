# Specification: Signal Hub Protocol

## 1. Core Principles

### 1.1 The Signal Hub is a Message Router
Signal Hub is a **Durable Object** that routes messages between actors across runtime boundaries (Browser ↔ Cloudflare ↔ SEAG).

- **Behavior:** WebSocket server accepting connections, maintaining actor registry, routing messages
- **Persistence:** Volatile registry (in-memory), ephemeral connections (no durable storage of messages)
- **Addressing:** Uses canonical addresses from `@agentic-primer/protocols/shared-message`

### 1.2 Hibernatable WebSocket Model
Cloudflare's hibernatable WebSocket API has specific semantics:

- **Automatic Wake:** WebSockets automatically wake on incoming messages (no prevention mechanism)
- **Heartbeat Purpose:** Detect dead connections, NOT prevent hibernation
- **State Preservation:** In-memory state persists across hibernation cycles
- **Message Ordering:** FIFO per connection, no global ordering guarantees

### 1.3 Capability-Based Security
Actors authenticate via JWT tokens containing:

- **actorId:** Canonical address path (e.g., `browser/client-ui`)
- **capabilities:** Array of permitted actions (e.g., `["send", "receive", "discover"]`)
- **exp:** Token expiration timestamp

When `AUTH_ENABLED=true`, all messages after `hub:connect` require valid authentication.

### 1.4 At-Most-Once Delivery Guarantee
For MVP, Signal Hub provides **at-most-once delivery**:

- Messages may be lost on connection failure
- No automatic retries or message persistence
- Clients responsible for implementing retry logic
- Future: At-least-once with ACKs and message IDs

## 2. Protocol Version

**Current Version:** `1.0`

Specified in:
- Server: `Env.PROTOCOL_VERSION`
- Clients: Validated during `hub:connect` handshake

Version mismatch results in `version_mismatch` error.

## 3. Message Types

All messages use the `SharedMessage` format from `@agentic-primer/protocols`:

```typescript
interface SharedMessage {
  type: string;              // Message type (e.g., "hub:connect")
  from: CanonicalAddress;    // Sender canonical address
  to: CanonicalAddress;      // Recipient canonical address
  payload: unknown;          // Message-specific payload
  pattern?: string;          // Routing pattern (e.g., "request", "notify")
  timestamp?: number;        // Epoch milliseconds
  correlationId?: string;    // For request-response correlation
  ttl?: number;              // Time-to-live in milliseconds
}
```

### 3.1 Connection Lifecycle

#### `hub:connect`
Establish WebSocket connection and authenticate.

**Request:**
```typescript
{
  type: "hub:connect",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    version: "1.0",
    jwt: "eyJhbGc..." // Optional if AUTH_ENABLED=false
  }
}
```

**Response (Success):**
```typescript
{
  type: "hub:connected",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    sessionId: "sess_abc123",
    actorIdentity: "browser/client-ui",
    capabilities: ["send", "receive", "discover"],
    serverTime: 1708272000000
  }
}
```

**Response (Error):**
```typescript
{
  type: "hub:error",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    code: "version_mismatch" | "unauthorized",
    message: "Protocol version 1.0 required, got 0.9",
    details: { expected: "1.0", received: "0.9" }
  }
}
```

#### `hub:heartbeat`
Keep-alive message to detect dead connections.

**Request:**
```typescript
{
  type: "hub:heartbeat",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: { timestamp: 1708272000000 }
}
```

**Response:**
```typescript
{
  type: "hub:heartbeat_ack",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: { serverTime: 1708272000123 }
}
```

**Heartbeat Configuration:**
- Client sends every 30 seconds (configurable)
- Server expects within 60 seconds
- Missing heartbeats → session cleanup after TTL expires

#### `hub:disconnect`
Graceful disconnection with cleanup.

**Request:**
```typescript
{
  type: "hub:disconnect",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: { reason: "User logout" }
}
```

**Response:**
```typescript
{
  type: "hub:disconnect_ack",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    sessionId: "sess_abc123",
    cleanedUp: true
  }
}
```

**Critical Implementation Detail:**
- Server MUST send `hub:disconnect_ack` BEFORE closing WebSocket
- Otherwise response is lost (sent on closed socket)

### 3.2 Actor Registration

#### `hub:register`
Register actor in hub registry for discovery.

**Request:**
```typescript
{
  type: "hub:register",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    actorAddress: "browser/client-ui",
    capabilities: ["ui", "interaction"],
    metadata: {
      version: "1.0.0",
      userAgent: "Mozilla/5.0..."
    },
    ttl: 300000 // 5 minutes in milliseconds
  }
}
```

**Response (Success):**
```typescript
{
  type: "hub:registered",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    actorAddress: "browser/client-ui",
    renewalToken: "tok_xyz789",
    expiresAt: 1708272300000,
    version: 1 // Registration version for conflict resolution
  }
}
```

**Response (Conflict):**
```typescript
{
  type: "hub:error",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    code: "unknown_actor", // Reused for conflict
    message: "Actor browser/client-ui already registered",
    details: {
      existingVersion: 3,
      existingExpiresAt: 1708272300000,
      hint: "Wait for expiration or use different address"
    }
  }
}
```

#### `hub:unregister`
Remove actor from registry.

**Request:**
```typescript
{
  type: "hub:unregister",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    actorAddress: "browser/client-ui"
  }
}
```

**Response:**
```typescript
{
  type: "hub:unregistered",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    actorAddress: "browser/client-ui",
    unregisteredAt: 1708272000000
  }
}
```

#### `hub:renew`
Extend registration TTL before expiration.

**Request:**
```typescript
{
  type: "hub:renew",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    actorAddress: "browser/client-ui",
    renewalToken: "tok_xyz789",
    ttl: 300000 // New TTL
  }
}
```

**Response:**
```typescript
{
  type: "hub:renewed",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    actorAddress: "browser/client-ui",
    expiresAt: 1708272600000,
    newRenewalToken: "tok_abc123"
  }
}
```

### 3.3 Actor Discovery

#### `hub:discover`
Query registry for actors matching criteria.

**Request:**
```typescript
{
  type: "hub:discover",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    pattern?: string,              // Regex on actor address
    capabilities?: string[],       // Must have ALL these
    metadata?: Record<string, any>, // Filter on metadata
    limit?: number,                // Max results (default: 100, max: 1000)
    offset?: number                // Pagination offset
  }
}
```

**Response:**
```typescript
{
  type: "hub:discovery_result",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    actors: [
      {
        actorAddress: "seag/agent-1",
        capabilities: ["inference", "analysis"],
        metadata: { model: "claude-3.5-sonnet" },
        registeredAt: 1708272000000
      }
    ],
    count: 1,
    hasMore: false,
    totalMatches: 1
  }
}
```

#### `hub:list_actors`
List all registered actors (for debugging).

**Request:**
```typescript
{
  type: "hub:list_actors",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    limit?: number,
    offset?: number
  }
}
```

**Response:** Same as `hub:discovery_result`

### 3.4 Point-to-Point Messaging

#### `hub:send`
Send message to specific actor.

**Request:**
```typescript
{
  type: "hub:send",
  from: "browser/client-ui",
  to: "seag/agent-1",
  payload: {
    type: "inference_request",
    prompt: "Analyze this data...",
    sessionId: "sess_user_123"
  },
  correlationId: "req_abc123" // For response correlation
}
```

**Server Behavior:**
1. Lookup recipient in registry
2. Find WebSocket for recipient's session
3. Forward message to recipient's connection
4. No response to sender (fire-and-forget)

**Delivery Failure:**
If recipient not found or connection closed:
```typescript
{
  type: "hub:error",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    code: "unknown_actor",
    message: "Actor seag/agent-1 not registered or disconnected",
    details: { targetActor: "seag/agent-1" }
  },
  correlationId: "req_abc123"
}
```

### 3.5 Broadcast Messaging

#### `hub:broadcast`
Send message to all registered actors.

**Request:**
```typescript
{
  type: "hub:broadcast",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    type: "system_announcement",
    message: "Maintenance in 5 minutes"
  }
}
```

**Server Behavior:**
1. Iterate all registered actors
2. Send message to each actor's WebSocket
3. Use batching (100 actors at a time) to avoid CPU timeout
4. Continue on individual failures (best-effort delivery)

**Response:**
```typescript
{
  type: "hub:broadcast_ack",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    recipientCount: 42,
    successCount: 40,
    failureCount: 2
  }
}
```

### 3.6 Pub/Sub Topics

#### `hub:subscribe`
Subscribe to topic for filtered broadcasts.

**Request:**
```typescript
{
  type: "hub:subscribe",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    topic: "system/events",
    actorAddress: "browser/client-ui"
  }
}
```

**Response:**
```typescript
{
  type: "hub:subscribed",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    topic: "system/events",
    subscribedAt: 1708272000000
  }
}
```

#### `hub:publish`
Publish message to topic subscribers.

**Request:**
```typescript
{
  type: "hub:publish",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    topic: "system/events",
    message: {
      type: "user_action",
      action: "clicked_button",
      metadata: { buttonId: "submit" }
    }
  }
}
```

**Server Behavior:**
1. Lookup subscribers for topic
2. Send message to each subscriber's WebSocket
3. Same batching and best-effort as broadcast

**Response:**
```typescript
{
  type: "hub:delivery_ack",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    topic: "system/events",
    subscriberCount: 5,
    deliveredCount: 5
  }
}
```

#### `hub:unsubscribe`
Unsubscribe from topic.

**Request:**
```typescript
{
  type: "hub:unsubscribe",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {
    topic: "system/events",
    actorAddress: "browser/client-ui"
  }
}
```

**Response:**
```typescript
{
  type: "hub:unsubscribed",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    topic: "system/events",
    unsubscribedAt: 1708272000000
  }
}
```

### 3.7 Flow Control

#### `hub:queue_stats`
Query message queue statistics (for backpressure monitoring).

**Request:**
```typescript
{
  type: "hub:queue_stats",
  from: "browser/client-ui",
  to: "cloudflare/signal-hub",
  payload: {}
}
```

**Response:**
```typescript
{
  type: "hub:queue_stats_result",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    pending: 0,
    processed: 1234,
    failed: 2,
    paused: false
  }
}
```

## 4. Error Codes

All errors use `hub:error` message type:

```typescript
{
  type: "hub:error",
  from: "cloudflare/signal-hub",
  to: "<original-sender>",
  payload: {
    code: HubErrorCode,
    message: string,
    details?: Record<string, unknown>
  },
  correlationId?: string // If responding to specific request
}
```

### Error Code Reference

| Code | Description | Recovery |
|------|-------------|----------|
| `version_mismatch` | Protocol version incompatible | Upgrade client |
| `unauthorized` | JWT invalid or missing | Re-authenticate |
| `rate_limited` | Exceeded 100 msg/min | Backoff (see `retryAfter` in details) |
| `unknown_actor` | Target actor not registered | Check discovery, retry later |
| `message_too_large` | Message exceeds 512KB | Break into smaller messages |
| `message_expired` | TTL exceeded | Resend with new TTL |
| `timeout` | Operation timed out | Retry with exponential backoff |
| `internal_error` | Server-side error | Retry, report if persistent |

## 5. Logical Addressing

Signal Hub uses canonical addresses from `@agentic-primer/protocols`:

```typescript
type CanonicalAddress = `${string}/${string}`; // "runtime/actor-id"
```

**Examples:**
- Hub itself: `cloudflare/signal-hub`
- Browser actor: `browser/client-ui`
- SEAG actor: `seag/agent-1`

**Validation:**
- Must match pattern: `^[a-z0-9-]+/[a-z0-9-]+$`
- Runtime part identifies execution environment
- Actor ID part identifies specific actor instance

## 6. Scalability Constraints

### Per-Instance Limits
- **Max Actors:** 50,000 registered actors
- **Max Connections:** Limited by Cloudflare DO (typically 10,000+)
- **Broadcast Throughput:** ~1,000 actors/second (batched)
- **Message Rate:** 100 messages/min per connection (rate limited)

### Message Size Limits
- **Raw Message:** 1MB (WebSocket frame limit)
- **Recommended Max:** 512KB (payload + envelope)
- **Enforcement:** Server validates size before parsing

### Storage Constraints
- **Registry:** In-memory only (volatile)
- **No message persistence:** Messages are ephemeral
- **Session state:** Minimal (< 1KB per connection)

## 7. Cross-Reference

See also:
- **STATE_MACHINE.spec.md** - Connection lifecycle states
- **SCHEMAS.spec.md** - Type definitions
- **EDGE_CASES.spec.md** - Failure modes and recovery
- **CLIENT.spec.md** - Client implementation patterns
- **SERVER.spec.md** - Server implementation details
