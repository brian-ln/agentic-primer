# Signal Hub + SharedMessage Integration

**Status:** Design Phase
**Last Updated:** 2026-02-16
**Related:** `packages/protocols/src/shared-message.ts`

---

## Executive Summary

**Signal Hub MUST use `SharedMessage` as its wire format.** Do NOT create a parallel `WireMessage` type.

The existing `SharedMessage` type (from `@agentic-primer/protocols`) already provides:
- ✅ Cross-runtime addressing (`CanonicalAddress` with `@(path)` format)
- ✅ Message correlation (`correlationId` + `pattern: 'ask'`)
- ✅ Time-to-live (`ttl`)
- ✅ Authentication hook (`signature`)
- ✅ Extensibility (`metadata` bag)
- ✅ Runtime conversion (`toCanonical`/`fromCanonical`)

**Why this matters:** Creating a parallel wire format leads to two serialization paths, schema duplication, and confusion about which format to use when.

---

## SharedMessage Structure

From `/packages/protocols/src/shared-message.ts`:

```typescript
interface SharedMessage {
  readonly id: string;                      // UUID
  readonly from: CanonicalAddress;          // @(path) sender
  readonly to: CanonicalAddress;            // @(path) recipient
  readonly type: string;                    // Message type discriminator
  readonly payload: unknown;                // Message-specific data
  readonly pattern: 'tell' | 'ask';         // Fire-and-forget vs request-response
  readonly correlationId: string | null;    // For ask/reply correlation
  readonly timestamp: number;               // Epoch milliseconds
  readonly metadata: Record<string, unknown>; // Extensible context
  readonly ttl: number | null;              // Time-to-live in ms
  readonly signature: string | null;        // Base64 HMAC
}

type CanonicalAddress = `@(${string})`;     // Runtime-agnostic address
type Runtime = 'local' | 'cloudflare' | 'browser' | 'beam';
```

**Schema source:** `/packages/protocols/schema/domain.schema.json` (lines 882-908)

---

## Signal Hub Message Types

Signal Hub defines message types as `type` discriminators on `SharedMessage`:

### Connection Lifecycle

| Type | Pattern | Purpose | Metadata |
|------|---------|---------|----------|
| `hub:connect` | ask | Establish session | `{ protocolVersion, authToken, capabilities }` |
| `hub:connected` | tell | Connection established | `{ sessionId, serverVersion }` |
| `hub:heartbeat` | tell | Keep-alive ping | `{ timestamp }` |
| `hub:heartbeat_ack` | tell | Heartbeat response | `{ timestamp }` |
| `hub:disconnect` | tell | Graceful shutdown | `{ reason }` |

**Example: Connect Request**

```typescript
const connectMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(browser/client-ui)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:connect',
  pattern: 'ask',  // Expects response
  correlationId: null,
  timestamp: Date.now(),
  payload: null,
  metadata: {
    protocolVersion: '0.1.0',
    authToken: 'bearer xyz...',
    capabilities: ['send', 'broadcast', 'subscribe']
  },
  ttl: 5000,  // 5s timeout
  signature: null
};
```

**Response: Connected**

```typescript
const connectedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:connected',
  pattern: 'tell',
  correlationId: connectMsg.id,  // Links to connect request
  timestamp: Date.now(),
  payload: { sessionId: 'sess-abc123' },
  metadata: { serverVersion: '0.1.0' },
  ttl: null,
  signature: null
};
```

---

### Actor Discovery

| Type | Pattern | Purpose | Payload |
|------|---------|---------|---------|
| `hub:register` | ask | Register actor | `{ actorAddress, capabilities, metadata }` |
| `hub:registered` | tell | Registration success | `{ actorAddress, expiresAt }` |
| `hub:unregister` | tell | Remove registration | `{ actorAddress }` |
| `hub:discover` | ask | Query actors by pattern | `{ pattern, limit }` |
| `hub:discovered` | tell | Discovery results | `{ actors: [...] }` |
| `hub:list_actors` | ask | Get all registered actors | `{ offset, limit }` |
| `hub:actor_list` | tell | Actor list response | `{ actors: [...], total, hasMore }` |

**Example: Register Actor**

```typescript
const registerMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(browser/widget-actor-123)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:register',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    actorAddress: '@(browser/widget-actor-123)',
    capabilities: ['render', 'handle-click'],
    metadata: { widgetType: 'chart' }
  },
  metadata: {
    connectionId: 'conn-abc',
    ttlSeconds: 300  // Auto-expire after 5 min
  },
  ttl: 5000,
  signature: null
};
```

---

### Message Delivery

| Type | Pattern | Purpose | Payload |
|------|---------|---------|---------|
| `hub:send` | tell/ask | Point-to-point message | Forwarded message |
| `hub:delivery_ack` | tell | Delivery confirmation | `{ messageId, deliveredAt }` |
| `hub:broadcast` | tell | Broadcast to all actors | `{ message }` |
| `hub:broadcast_ack` | tell | Broadcast confirmation | `{ deliveredTo: number }` |
| `hub:subscribe` | ask | Subscribe to topic | `{ topic, durable }` |
| `hub:subscribed` | tell | Subscription success | `{ topic, subscriptionId }` |
| `hub:publish` | tell | Publish to topic | `{ topic, message }` |
| `hub:published` | tell | Publish confirmation | `{ topic, subscriberCount }` |
| `hub:unsubscribe` | tell | Unsubscribe from topic | `{ subscriptionId }` |

**Example: Send Point-to-Point**

```typescript
const sendMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(browser/widget-actor-123)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:send',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    // The actual message to forward
    targetAddress: '@(local/coordinator-main)',
    message: {
      type: 'task:assign',
      payload: { taskId: 'task-456', priority: 1 }
    }
  },
  metadata: {
    traceId: 'trace-xyz',
    requireAck: true
  },
  ttl: 30000,  // 30s delivery timeout
  signature: null
};
```

---

### Error Handling

| Type | Pattern | Purpose | Payload |
|------|---------|---------|---------|
| `hub:error` | tell | Generic error | `{ code, message, details }` |
| `hub:unknown_actor` | tell | Actor not found | `{ actorAddress }` |
| `hub:unauthorized` | tell | Permission denied | `{ action, reason }` |
| `hub:rate_limited` | tell | Too many requests | `{ retryAfter }` |

**Example: Error Response**

```typescript
const errorMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/widget-actor-123)',
  type: 'hub:unknown_actor',
  pattern: 'tell',
  correlationId: sendMsg.id,  // Links to failed send
  timestamp: Date.now(),
  payload: {
    actorAddress: '@(local/coordinator-main)',
    message: 'Actor not registered with Signal Hub'
  },
  metadata: {
    errorCode: 'ACTOR_NOT_FOUND',
    retryable: false
  },
  ttl: null,
  signature: null
};
```

---

## Metadata Conventions

Use `metadata` for extensible context and routing hints:

### Protocol Version Negotiation

```typescript
metadata: {
  protocolVersion: '0.1.0',  // Client version
  serverVersion: '0.1.0'     // Server version (in response)
}
```

### Authentication

```typescript
metadata: {
  authToken: 'bearer xyz...',  // JWT or bearer token
  actorIdentity: '@(verified/actor-123)'  // Server-verified identity
}
```

### Actor Capabilities

```typescript
metadata: {
  capabilities: ['send', 'broadcast', 'subscribe'],
  maxMessageSize: 1048576,  // 1MB limit
  supportedContentTypes: ['json', 'msgpack']
}
```

### Routing & Tracing

```typescript
metadata: {
  traceId: 'trace-xyz',           // Distributed tracing
  spanId: 'span-abc',
  connectionId: 'conn-123',        // WebSocket connection ID
  sessionId: 'sess-456',           // Durable Object session ID
  requireAck: true,                // Require delivery acknowledgment
  priority: 1                      // Message priority (0=high, 2=low)
}
```

### TTL & Expiration

```typescript
metadata: {
  ttlSeconds: 300,                 // Actor registration TTL
  expiresAt: 1739731553000,        // Absolute expiration (epoch ms)
  autoRenew: true                  // Auto-renew before expiration
}
```

---

## Runtime Conversion

Signal Hub actors exist in different runtimes and use runtime-specific addresses:

| Runtime | Address Format | Example |
|---------|----------------|---------|
| Browser | `local://path` | `local://widget-actor-123` |
| Cloudflare | `remote://do/path` | `remote://do/signal-hub` |
| Local (SEAG) | `@(path)` or plain | `coordinator:main` |
| Beam | plain path | `actor-123` |

**Use converters from `shared-message.ts`:**

```typescript
import { toCanonical, fromCanonical } from '@agentic-primer/protocols';

// Browser → Canonical
const canonical = toCanonical('local://widget-123', 'browser');
// → '@(widget-123)'

// Canonical → Cloudflare
const cloudflareAddr = fromCanonical('@(widget-123)', 'cloudflare');
// → 'remote://do/widget-123'
```

**Signal Hub routing:**

1. Client sends message with browser address: `local://widget-123`
2. Signal Hub receives, converts to canonical: `@(widget-123)`
3. Routes to target runtime (e.g., SEAG local address: `widget:123`)
4. Target converts canonical to its native format

---

## Pattern Usage

### Fire-and-Forget (pattern: 'tell')

Use `tell` for one-way messages where no response is expected:
- Heartbeats
- Disconnect notifications
- Broadcast messages
- Delivery acknowledgments

```typescript
pattern: 'tell',
correlationId: null  // Not used for tell
```

### Request-Response (pattern: 'ask')

Use `ask` for messages expecting a response:
- Connect requests
- Actor registration
- Discovery queries
- Point-to-point with ack

```typescript
// Request
pattern: 'ask',
correlationId: null  // Will be filled by response

// Response
pattern: 'tell',
correlationId: requestMsg.id  // Links to original request
```

**Correlation:**
- Client sends `ask` message with `correlationId: null`
- Server responds with `tell` message setting `correlationId: requestMsg.id`
- Client matches response to request via `correlationId`

---

## Security Integration

`SharedMessage` includes a `signature` field for HMAC authentication:

### During Connect

```typescript
const connectMsg: SharedMessage = {
  // ... other fields
  metadata: {
    authToken: 'bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...'
  },
  signature: null  // For future HMAC implementation
};
```

**Server validates:**
1. Extract `authToken` from metadata
2. Verify JWT signature
3. Associate connection with identity
4. Server-enforce `from` field on subsequent messages

**Future HMAC (Phase 2):**
- Sign entire message with shared secret
- Store signature in `signature` field
- Server validates before processing

---

## Comparison: Planned WireMessage vs SharedMessage

| Field | Planned WireMessage | SharedMessage | Notes |
|-------|---------------------|---------------|-------|
| `id` | string | UUID ✅ | Same |
| `from` | string | CanonicalAddress ✅ | Better - runtime-agnostic |
| `to` | string | CanonicalAddress ✅ | Better - runtime-agnostic |
| `type` | string | string ✅ | Same |
| `payload` | unknown | unknown ✅ | Same |
| `timestamp` | number | number ✅ | Same |
| `replyTo` | string? | correlationId ✅ | Better - ask/tell pattern |
| `ttl` | number? | number \| null ✅ | Same |
| — | — | pattern ✅ | **New** - tell/ask distinction |
| — | — | metadata ✅ | **New** - extensible context |
| — | — | signature ✅ | **New** - authentication hook |

**Verdict:** SharedMessage is strictly better. Use it.

---

## Implementation Checklist

Phase 1 (Protocol Design) must address:

- [x] ~~Create new wire format~~ → **Use SharedMessage**
- [ ] Define Signal Hub message types as `type` discriminators
- [ ] Document metadata conventions (version, auth, capabilities, tracing)
- [ ] Specify runtime conversion strategy (browser ↔ cloudflare ↔ local)
- [ ] Define connection lifecycle using ask/tell patterns
- [ ] Error handling via `hub:error` types with correlationId
- [ ] Security: authToken in metadata, server-enforced `from` field
- [ ] Version negotiation in `hub:connect` metadata

Phase 2 (Workers Implementation) builds on this:

- [ ] WebSocket → SharedMessage parser
- [ ] Type-based message router (switch on msg.type)
- [ ] Runtime address converter (browser ↔ canonical)
- [ ] Connection state machine (uses existing ConnectionState enum)
- [ ] Actor registry (stores canonical addresses)
- [ ] Broadcast queue (async fan-out for >100 actors)
- [ ] Auth validator (JWT in metadata)

---

## References

**Source Files:**
- `/packages/protocols/src/shared-message.ts` - SharedMessage type and converters
- `/packages/protocols/schema/domain.schema.json` - JSON Schema (lines 882-908)
- `/packages/protocols/src/domain.types.ts` - Generated TypeScript types
- `/docs/actor-design/patterns/session-gateway-pattern.md` - SessionActor experiment

**Related Specs:**
- `~/knowledge/specs/formal-model/layer-5-projections.md` Sec 5.1 - SharedMessage spec

**Reviews:**
- Opus architectural review: Use SharedMessage (P0)
- Haiku practical review: `/SIGNAL_HUB_PROTOCOL_REVIEW.md`

---

**Next:** Update Phase 1-7 plan to use SharedMessage instead of creating WireMessage.
