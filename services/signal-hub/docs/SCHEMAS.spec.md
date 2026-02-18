# Specification: Type Schemas

## 1. Core Principle: Shared Protocol Types

All types are derived from `@agentic-primer/protocols/shared-message` to ensure cross-runtime compatibility between Browser ↔ Cloudflare ↔ SEAG.

## 2. Protocol Foundation

### 2.1 SharedMessage

The universal message envelope for all Signal Hub communication:

```typescript
interface SharedMessage {
  type: string;              // Message type (e.g., "hub:connect", "hub:send")
  from: CanonicalAddress;    // Sender's canonical address
  to: CanonicalAddress;      // Recipient's canonical address
  payload: unknown;          // Message-specific payload
  pattern?: string;          // Routing pattern ("request", "notify", "response")
  timestamp?: number;        // Epoch milliseconds
  correlationId?: string;    // For request-response correlation
  ttl?: number;              // Time-to-live in milliseconds
}
```

**Source:** `@agentic-primer/protocols/src/shared-message.ts`

**Validation:**
```typescript
function validateSharedMessage(msg: unknown): boolean {
  return (
    typeof msg === 'object' &&
    msg !== null &&
    'type' in msg &&
    typeof msg.type === 'string' &&
    'from' in msg &&
    typeof msg.from === 'string' &&
    'to' in msg &&
    typeof msg.to === 'string' &&
    'payload' in msg
  );
}
```

### 2.2 CanonicalAddress

Actor addressing format across all runtimes:

```typescript
type CanonicalAddress = `${string}/${string}`;
```

**Format:** `<runtime>/<actor-id>`

**Examples:**
- `browser/client-ui` - Browser-based UI actor
- `seag/agent-1` - SEAG inference agent
- `cloudflare/signal-hub` - Signal Hub itself

**Validation:**
```typescript
const CANONICAL_ADDRESS_PATTERN = /^[a-z0-9-]+\/[a-z0-9-]+$/;

function isValidCanonicalAddress(addr: string): addr is CanonicalAddress {
  return CANONICAL_ADDRESS_PATTERN.test(addr);
}
```

## 3. Session Management

### 3.1 Session

Represents a WebSocket connection's state and identity:

```typescript
interface Session {
  sessionId: string;                     // Unique session identifier (e.g., "sess_abc123")
  actorIdentity: CanonicalAddress | null; // Verified from JWT (null before hub:connect)
  capabilities: string[];                // Permitted actions (from JWT)
  connectedAt: number;                   // Connection establishment timestamp (epoch ms)
  lastHeartbeat: number;                 // Last message received timestamp (epoch ms)
  authenticated: boolean;                // JWT validation passed
  paused: boolean;                       // Backpressure state (future use)
  connectionState: ConnectionState;      // Connection lifecycle state
  disconnectedAt?: number;               // Disconnection timestamp (epoch ms)
  rateLimitBucket: TokenBucket;          // Rate limiting state (100 msg/min)
}
```

**Lifecycle:**
1. Created on WebSocket accept (connectionState: `connecting`)
2. Updated on `hub:connect` (actorIdentity, authenticated, capabilities set)
3. Updated on every message (lastHeartbeat timestamp)
4. Cleaned up on disconnect (removed from memory)

**Storage:** In-memory only (`Map<WebSocket, Session>`), not persisted

### 3.2 ConnectionState

```typescript
type ConnectionState = 'connecting' | 'connected' | 'disconnecting' | 'disconnected';
```

See **STATE_MACHINE.spec.md** for transition rules.

## 4. Actor Registry

### 4.1 ActorRegistration

Represents a registered actor available for discovery:

```typescript
interface ActorRegistration {
  actorAddress: CanonicalAddress;        // Actor's canonical address
  capabilities: string[];                // Actor capabilities (e.g., ["send", "receive"])
  metadata: Record<string, unknown>;     // Custom metadata (version, user agent, etc.)
  connectionId: string;                  // WebSocket connection ID (session ID)
  registeredAt: number;                  // Registration timestamp (epoch ms)
  expiresAt: number;                     // Expiration timestamp (epoch ms)
  version: number;                       // Registration version (for conflict resolution)
  renewalToken: string;                  // Token for hub:renew (opaque string)
}
```

**Version Tracking:**
- Increments on each registration (1, 2, 3, ...)
- Used to detect duplicate registrations from different connections
- Client compares versions to determine which registration is active

**TTL Calculation:**
```typescript
function calculateExpiration(ttl: number, maxTtl: number): number {
  const clampedTtl = Math.min(ttl, maxTtl); // Enforce MAX_ACTOR_TTL
  return Date.now() + clampedTtl;
}
```

**Cleanup:**
- Automatic on expiration (garbage collection)
- Manual on `hub:unregister`
- Automatic on connection close

**Storage:** In-memory only (`Map<CanonicalAddress, ActorRegistration>`), not persisted

## 5. Topic Subscriptions

### 5.1 TopicSubscription

Represents a pub/sub topic subscription:

```typescript
interface TopicSubscription {
  topic: string;                        // Topic name (e.g., "system/events")
  actorAddress: CanonicalAddress;       // Subscriber's canonical address
  subscribedAt: number;                 // Subscription timestamp (epoch ms)
}
```

**Storage Structure:**
```typescript
// Map<topic, Set<actorAddress>>
Map<string, Set<CanonicalAddress>>
```

**Example:**
```typescript
{
  "system/events": Set(["browser/client-ui", "seag/agent-1"]),
  "user/notifications": Set(["browser/client-ui"])
}
```

**Cleanup:**
- Automatic on `hub:unsubscribe`
- Automatic on actor unregister
- Automatic on connection close

## 6. Authentication

### 6.1 ActorIdentity

Extracted from validated JWT:

```typescript
interface ActorIdentity {
  actorId: string;                      // Canonical address path (e.g., "browser/client-ui")
  userId: string;                       // User ID from JWT sub claim
  capabilities: string[];               // Permitted actions
  expiresAt: number;                    // JWT expiration (epoch ms)
}
```

**Note:** This is an internal type. The `Session.actorIdentity` field stores only the `CanonicalAddress` string, not the full `ActorIdentity` object.

### 6.2 JWTPayload

JWT token structure for authentication:

```typescript
interface JWTPayload {
  sub: string;                          // User ID (subject)
  actorId: string;                      // Actor canonical address path
  capabilities: string[];               // Permitted actions
  iss: string;                          // Issuer: "signal-hub"
  exp: number;                          // Expiration (Unix timestamp)
}
```

**Validation:**
```typescript
async function validateJWT(token: string, secret: string): Promise<JWTPayload> {
  // 1. Decode and verify signature
  const decoded = await verifyJWT(token, secret);

  // 2. Check expiration
  if (decoded.exp < Date.now() / 1000) {
    throw new HubError('unauthorized', 'JWT expired');
  }

  // 3. Validate required fields
  if (!decoded.sub || !decoded.actorId || !decoded.capabilities) {
    throw new HubError('unauthorized', 'Invalid JWT structure');
  }

  return decoded as JWTPayload;
}
```

**Example JWT:**
```json
{
  "sub": "user_123",
  "actorId": "browser/client-ui",
  "capabilities": ["send", "receive", "discover"],
  "iss": "signal-hub",
  "exp": 1708272000
}
```

## 7. Error Handling

### 7.1 HubErrorCode

Enumeration of error codes:

```typescript
type HubErrorCode =
  | 'version_mismatch'     // Protocol version incompatible
  | 'unauthorized'         // JWT invalid or missing
  | 'rate_limited'         // Exceeded 100 msg/min
  | 'unknown_actor'        // Target actor not registered
  | 'message_too_large'    // Message exceeds 512KB
  | 'message_expired'      // TTL exceeded
  | 'timeout'              // Operation timed out
  | 'internal_error';      // Server-side error
```

### 7.2 HubError

Custom error class with structured details:

```typescript
class HubError extends Error {
  constructor(
    public code: HubErrorCode,
    message: string,
    public details?: Record<string, unknown>
  ) {
    super(message);
    this.name = 'HubError';
  }
}
```

**Usage:**
```typescript
throw new HubError(
  'rate_limited',
  'Rate limit exceeded. Max 100 messages per minute.',
  { retryAfter: 30, limit: '100 messages/min' }
);
```

**Wire Format:**
```typescript
{
  type: "hub:error",
  from: "cloudflare/signal-hub",
  to: "browser/client-ui",
  payload: {
    code: "rate_limited",
    message: "Rate limit exceeded. Max 100 messages per minute.",
    details: {
      retryAfter: 30,
      limit: "100 messages/min"
    }
  }
}
```

## 8. Rate Limiting

### 8.1 TokenBucket

Token bucket algorithm for rate limiting:

```typescript
interface TokenBucket {
  tokens: number;                       // Current token count (0 to capacity)
  capacity: number;                     // Maximum tokens (e.g., 100)
  refillRate: number;                   // Tokens per second (e.g., 1.67 = 100/min)
  lastRefill: number;                   // Last refill timestamp (epoch ms)
}
```

**Creation:**
```typescript
function createTokenBucket(capacity: number, refillRate: number): TokenBucket {
  return {
    tokens: capacity,
    capacity,
    refillRate,
    lastRefill: Date.now()
  };
}
```

**Token Consumption:**
```typescript
function consumeToken(bucket: TokenBucket): boolean {
  const now = Date.now();
  const elapsed = (now - bucket.lastRefill) / 1000; // seconds

  // Refill tokens based on elapsed time
  bucket.tokens = Math.min(
    bucket.capacity,
    bucket.tokens + elapsed * bucket.refillRate
  );
  bucket.lastRefill = now;

  // Try to consume one token
  if (bucket.tokens >= 1) {
    bucket.tokens -= 1;
    return true; // Success
  }

  return false; // Rate limited
}
```

**Configuration:**
- **Capacity:** 100 messages
- **Refill Rate:** 100 messages / 60 seconds = 1.67 messages/second
- **Effective Limit:** 100 messages per minute (burst allowed)

## 9. Queue Statistics

### 9.1 QueueStats

Server-side message queue statistics:

```typescript
interface QueueStats {
  pending: number;                      // Messages waiting to be processed
  processed: number;                    // Successfully processed messages
  failed: number;                       // Failed message attempts
  paused: boolean;                      // Queue paused (backpressure active)
}
```

**Usage:**
```typescript
// Increment on successful message processing
this.queueStats.processed++;

// Increment on error
this.queueStats.failed++;

// Query via hub:queue_stats
{
  type: "hub:queue_stats_result",
  payload: {
    pending: 0,
    processed: 1234,
    failed: 2,
    paused: false
  }
}
```

## 10. Configuration

### 10.1 Environment Bindings

Server-side environment configuration:

```typescript
interface Env {
  // Durable Object binding
  SIGNAL_HUB: DurableObjectNamespace;

  // Protocol configuration
  PROTOCOL_VERSION: string;              // e.g., "1.0"
  MAX_MESSAGE_SIZE: string;              // e.g., "524288" (512KB)
  HEARTBEAT_INTERVAL: string;            // e.g., "30000" (30s)

  // Registry limits
  ACTOR_REGISTRY_LIMIT: string;          // e.g., "50000"
  DEFAULT_ACTOR_TTL: string;             // e.g., "300000" (5 minutes)
  MAX_ACTOR_TTL: string;                 // e.g., "3600000" (1 hour)

  // Broadcast configuration
  BROADCAST_SYNC_THRESHOLD: string;      // e.g., "100" (sync if <= 100 recipients)

  // Authentication
  JWT_SECRET?: string;                   // JWT signing secret (optional)
  AUTH_ENABLED: string;                  // "true" or "false"
}
```

**Type Coercion:**
All environment variables are strings. Coerce to numbers/booleans as needed:

```typescript
const maxSize = parseInt(env.MAX_MESSAGE_SIZE, 10);
const authEnabled = env.AUTH_ENABLED === 'true';
```

## 11. Type Relationships

### Dependency Graph

```
SharedMessage (protocol foundation)
    ↓
CanonicalAddress
    ↓
    ├─→ ActorRegistration (registry)
    │       ↓
    │   TopicSubscription (pub/sub)
    │
    └─→ Session (connection state)
            ↓
        ConnectionState
        TokenBucket
```

### Data Flow

1. **WebSocket Accept:**
   - Create `Session` (connectionState: `connecting`)
   - Store in `Map<WebSocket, Session>`

2. **hub:connect:**
   - Validate JWT → extract `ActorIdentity`
   - Update `Session` (actorIdentity, authenticated, capabilities)
   - Transition to `connectionState: connected`

3. **hub:register:**
   - Create `ActorRegistration`
   - Store in `Map<CanonicalAddress, ActorRegistration>`
   - Link to `Session` via `connectionId`

4. **hub:subscribe:**
   - Create `TopicSubscription`
   - Store in `Map<string, Set<CanonicalAddress>>`

5. **hub:send:**
   - Lookup recipient in registry
   - Find WebSocket via session
   - Forward `SharedMessage`

6. **hub:disconnect:**
   - Cleanup `ActorRegistration` (remove from registry)
   - Cleanup `TopicSubscription` (remove from all topics)
   - Cleanup `Session` (remove from sessions map)

## 12. Version History

### Version 1.0 (Current)
- Initial schema definition
- Core message types
- Actor registration with TTL
- Pub/sub topics
- Rate limiting with token bucket
- Connection state tracking

### Future Considerations
- **Message IDs:** For at-least-once delivery (requires deduplication)
- **Sequence Numbers:** For ordering guarantees
- **Message Acknowledgments:** For reliable delivery
- **Persistent Storage:** For durable actor registry
- **Sharding Keys:** For multi-instance distribution

## 13. Cross-Reference

See also:
- **PROTOCOL.spec.md** - Message types and wire format
- **STATE_MACHINE.spec.md** - Connection lifecycle
- **EDGE_CASES.spec.md** - Schema validation edge cases
- **CLIENT.spec.md** - Client-side type usage
- **SERVER.spec.md** - Server-side type usage
