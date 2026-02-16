# Signal Hub Protocol Specification

**Version:** 0.1.0
**Status:** Design Complete
**Last Updated:** 2026-02-16

---

## Quick Reference

**Essential Links:**
- [Wire Protocol](#3-wire-protocol) - SharedMessage integration
- [Message Types](#4-message-types) - Complete catalog (24 types)
- [Connection Lifecycle](#5-connection-lifecycle) - State machine and timeouts
- [Security](#6-security) - Authentication and authorization
- [Examples](#10-examples) - Runnable code samples
- [Implementation Checklist](#implementation-checklist) - Step-by-step guide

**Key Constraints:**
- Max message size: 1MB (Cloudflare WebSocket limit)
- CPU limit: 30s per request
- Hibernation: 30s idle timeout
- Registry limit: 50K actors per instance
- Broadcast threshold: 100 actors (sync) / >100 (async queue)

**Common Operations:**
- [Connect with Auth](#101-connection-with-authentication)
- [Register Actor](#102-actor-registration)
- [Send Message](#103-point-to-point-messaging)
- [Broadcast](#104-broadcast-to-all-actors)
- [Subscribe to Topic](#105-topic-subscription)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Architecture](#2-architecture)
3. [Wire Protocol](#3-wire-protocol)
4. [Message Types](#4-message-types)
5. [Connection Lifecycle](#5-connection-lifecycle)
6. [Security](#6-security)
7. [Delivery Guarantees](#7-delivery-guarantees)
8. [Scalability](#8-scalability)
9. [Error Handling](#9-error-handling)
10. [Examples](#10-examples)
11. [Cloudflare Constraints](#11-cloudflare-constraints)
12. [Protocol Versioning](#12-protocol-versioning)
13. [Implementation Checklist](#implementation-checklist)

---

## 1. Overview

### 1.1 What is Signal Hub?

Signal Hub is a **WebSocket-based message router** running on Cloudflare Durable Objects that enables real-time communication between actors across different runtimes (SEAG local, browser, Beam).

**Key capabilities:**
- **Actor discovery** - Find actors by pattern or capability
- **Point-to-point messaging** - Send messages to specific actors
- **Broadcast** - Fan-out messages to all registered actors
- **Pub/sub** - Topic-based message distribution
- **Guaranteed delivery** - At-least-once with acknowledgment (Phase 2)

### 1.2 Use Cases

**Primary use case:** SEAG (local) ↔ Browser communication via Signal Hub

```
┌─────────────┐           ┌──────────────┐           ┌─────────────┐
│ SEAG Actor  │──────────>│  Signal Hub  │<──────────│Browser Actor│
│  (Local)    │<──────────│ (Cloudflare) │──────────>│  (Chrome)   │
└─────────────┘           └──────────────┘           └─────────────┘
    local://                remote://do/                local://
```

**Other use cases:**
- **Multi-actor coordination** - Orchestrate work across distributed actors
- **Event notification** - Broadcast state changes to interested parties
- **Service mesh** - Route messages between microservices
- **Real-time dashboards** - Push updates to browser clients

### 1.3 Design Principles

1. **Use existing types** - SharedMessage from `@agentic-primer/protocols`
2. **Server-enforced security** - Clients cannot spoof addresses
3. **Graceful degradation** - Handle network failures transparently
4. **Bounded resources** - Prevent memory/CPU exhaustion
5. **Observable behavior** - Clear error messages and metrics

### 1.4 Non-Goals

- ❌ **Not a message queue** - No persistent storage for offline actors (MVP)
- ❌ **Not a database** - No queryable state beyond actor registry
- ❌ **Not a CDN** - No large file transfers (1MB message limit)
- ❌ **Not ordered delivery** - No global message ordering guarantees

---

## 2. Architecture

### 2.1 System Context

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
│  │  │  │ Durable      │      │ Broadcast    │             │  │ │
│  │  │  │ Storage      │      │ Queue        │             │  │ │
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
│  │LocalActor123 │   │                    │  │WidgetActor456│    │
│  │@(seag/123)   │   │                    │  │@(browser/456)│    │
│  └──────────────┘   │                    │  └──────────────┘    │
└─────────────────────┘                    └──────────────────────┘
```

### 2.2 Durable Objects Design

**Durable Object = Single instance per actor group/shard**

- **Persistent storage** - Actor registry backed by DO storage
- **WebSocket state** - All connections managed in single DO
- **CPU limit** - 30s per request (requires async queuing for large broadcasts)
- **Hibernation** - WebSocket pauses after 30s idle (heartbeat prevents)

**Why Durable Objects?**
- Strongly consistent storage (no race conditions)
- Automatic geographic proximity (low latency)
- Scales to millions of instances (sharding strategy)

### 2.3 Runtime Diagram

**Message flow: SEAG → Browser via Signal Hub**

```
T=0  SEAG Actor                Signal Hub               Browser Actor
     │                         │                        │
     ├─hub:connect──────────────>│                        │
     │                         ├─validate JWT           │
     │                         ├─create session         │
     │<───hub:connected────────┤                        │
     │                         │                        │
     ├─hub:register────────────>│                        │
     │                         ├─add to registry        │
     │<───hub:registered───────┤                        │
     │                         │                        │
     │                         │<─hub:connect───────────┤
     │                         ├─validate JWT           │
     │                         │<─hub:connected─────────┤
     │                         │                        │
     │                         │<─hub:register──────────┤
     │                         ├─add to registry        │
     │                         │─hub:registered────────>│
     │                         │                        │
T=1  ├─hub:send───────────────>│                        │
     │  to: @(browser/456)     ├─lookup registry        │
     │                         ├─forward────────────────>│
     │                         │                        ├─process message
     │                         │                        │
     │<───hub:delivery_ack─────┤                        │
     │                         │                        │
```

---

## 3. Wire Protocol

### 3.1 SharedMessage Structure

Signal Hub uses `SharedMessage` from `@agentic-primer/protocols` as its wire format.

**From** `/packages/protocols/src/shared-message.ts`:

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
  readonly signature: string | null;        // Base64 HMAC (future)
}

type CanonicalAddress = `@(${string})`;     // Runtime-agnostic address
```

**Schema source:** `/packages/protocols/schema/domain.schema.json` (lines 882-908)

### 3.2 Why SharedMessage (Not WireMessage)

**Decision:** Use SharedMessage directly, do NOT create a parallel `WireMessage` type.

**Rationale:**
- ✅ Already provides cross-runtime addressing (`CanonicalAddress`)
- ✅ Already has correlation (`correlationId` + `pattern: 'ask'`)
- ✅ Already has TTL and signature hooks
- ✅ Avoids two serialization paths and schema duplication
- ✅ Converters exist for all runtimes (`toCanonical`/`fromCanonical`)

### 3.3 Type Discriminators

Signal Hub message types use the `hub:*` namespace as type discriminators:

```typescript
type HubMessageType =
  // Connection lifecycle
  | 'hub:connect' | 'hub:connected' | 'hub:heartbeat' | 'hub:heartbeat_ack'
  | 'hub:disconnect'
  // Actor discovery
  | 'hub:register' | 'hub:registered' | 'hub:unregister'
  | 'hub:discover' | 'hub:discovered'
  | 'hub:list_actors' | 'hub:actor_list'
  | 'hub:renew' | 'hub:renewed'
  // Message delivery
  | 'hub:send' | 'hub:delivery_ack'
  | 'hub:broadcast' | 'hub:broadcast_ack'
  | 'hub:subscribe' | 'hub:subscribed'
  | 'hub:publish' | 'hub:published'
  | 'hub:unsubscribe'
  // Flow control
  | 'hub:pause' | 'hub:resume'
  | 'hub:queue_stats' | 'hub:queue_stats_response'
  // Errors
  | 'hub:error' | 'hub:unknown_actor' | 'hub:unauthorized'
  | 'hub:rate_limited' | 'hub:version_mismatch' | 'hub:message_too_large';
```

### 3.4 Metadata Conventions

Use `metadata` field for extensible context:

| Key | Type | Purpose | Example |
|-----|------|---------|---------|
| `protocolVersion` | string | Version negotiation | `"0.1.0"` |
| `authToken` | string | JWT bearer token | `"bearer eyJ..."` |
| `actorIdentity` | string | Server-verified address | `"@(verified/actor-123)"` |
| `capabilities` | string[] | Actor capabilities | `["send", "broadcast"]` |
| `traceId` | string | Distributed tracing | `"trace-xyz-789"` |
| `requireAck` | boolean | Delivery acknowledgment | `true` |
| `ttlSeconds` | number | Registration TTL | `300` |

### 3.5 Pattern Usage

**Fire-and-forget (pattern: 'tell')**

Use for one-way messages with no response expected:

```typescript
{
  type: 'hub:heartbeat',
  pattern: 'tell',
  correlationId: null  // Not used
}
```

**Request-response (pattern: 'ask')**

Use for messages expecting a response:

```typescript
// Request
{
  type: 'hub:connect',
  pattern: 'ask',
  correlationId: null  // Will be set by response
}

// Response
{
  type: 'hub:connected',
  pattern: 'tell',
  correlationId: 'original-request-id'  // Links to request
}
```

### 3.6 Runtime Conversion

**Address format by runtime:**

| Runtime | Format | Example |
|---------|--------|---------|
| Browser | `local://path` | `local://widget-123` |
| Cloudflare | `remote://do/path` | `remote://do/signal-hub` |
| SEAG Local | `@(path)` or plain | `coordinator:main` |
| Beam | plain | `actor-123` |

**Convert using SharedMessage utilities:**

```typescript
import { toCanonical, fromCanonical } from '@agentic-primer/protocols';

// Browser → Canonical
const canonical = toCanonical('local://widget-123', 'browser');
// Result: '@(widget-123)'

// Canonical → Cloudflare
const cloudflareAddr = fromCanonical('@(widget-123)', 'cloudflare');
// Result: 'remote://do/widget-123'
```

---

## 4. Message Types

### 4.1 Message Type Catalog

**Total message types: 24**

| Category | Count | Types |
|----------|-------|-------|
| **Connection** | 5 | connect, connected, heartbeat, heartbeat_ack, disconnect |
| **Discovery** | 9 | register, registered, unregister, discover, discovered, list_actors, actor_list, renew, renewed |
| **Delivery** | 9 | send, delivery_ack, broadcast, broadcast_ack, subscribe, subscribed, publish, published, unsubscribe |
| **Flow Control** | 4 | pause, resume, queue_stats, queue_stats_response |
| **Errors** | 6 | error, unknown_actor, unauthorized, rate_limited, version_mismatch, message_too_large |

### 4.2 Connection Lifecycle Messages

#### hub:connect

**Direction:** Client → Server
**Pattern:** `ask` (expects `hub:connected` or `hub:error`)

```typescript
{
  id: crypto.randomUUID(),
  from: '@(browser/client-ui)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:connect',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: null,
  metadata: {
    protocolVersion: '0.1.0',        // Required
    authToken: 'bearer eyJ...',      // Required in production
    capabilities: ['send', 'broadcast', 'subscribe'],
    clientMetadata: {                // Optional
      userAgent: 'SEAG/1.0',
      platform: 'darwin'
    }
  },
  ttl: 5000,  // 5s timeout
  signature: null
}
```

**Validation:**
- `protocolVersion` MUST be semver string
- `authToken` MUST be valid JWT in production
- `capabilities` MUST be non-empty array

#### hub:connected

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:connect`)

```typescript
{
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:connected',
  pattern: 'tell',
  correlationId: 'connect-request-id',
  timestamp: Date.now(),
  payload: {
    sessionId: 'sess-abc123',
    serverVersion: '0.1.0',
    maxMessageSize: 1048576,         // 1MB
    heartbeatInterval: 25000,        // 25s
    capabilities: {
      maxActorsPerInstance: 50000,
      supportsBackpressure: true,
      supportedContentTypes: ['json', 'msgpack']
    }
  },
  metadata: {
    actorIdentity: '@(verified/user-123-browser)',  // Server-verified
    tokenExpiresAt: Date.now() + 86400000  // 24h
  },
  ttl: null,
  signature: null
}
```

#### hub:heartbeat

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)

**Purpose:** Prevent Cloudflare hibernation (30s idle timeout)

```typescript
{
  id: crypto.randomUUID(),
  from: '@(browser/client-ui)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:heartbeat',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: { timestamp: Date.now() },
  metadata: {},
  ttl: 10000,
  signature: null
}
```

**Timing:** Client sends every 25s (< 30s hibernation threshold)

#### hub:heartbeat_ack

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:heartbeat`)

```typescript
{
  type: 'hub:heartbeat_ack',
  pattern: 'tell',
  payload: {
    timestamp: 1739731234567,  // Client timestamp (echoed)
    serverTime: 1739731234568  // Server timestamp
  }
}
```

**Clock skew detection:** Compare `serverTime - timestamp` to detect drift

#### hub:disconnect

**Direction:** Client → Server OR Server → Client
**Pattern:** `tell` (fire-and-forget)

```typescript
{
  type: 'hub:disconnect',
  pattern: 'tell',
  payload: {
    reason: 'client_requested' | 'duplicate_connection' | 'unauthorized' | 'server_shutdown'
  }
}
```

### 4.3 Actor Discovery Messages

#### hub:register

**Direction:** Client → Server
**Pattern:** `ask` (expects `hub:registered`)

```typescript
{
  type: 'hub:register',
  pattern: 'ask',
  payload: {
    actorAddress: '@(browser/widget-123)',
    capabilities: ['render', 'handle-click'],
    metadata: { widgetType: 'chart', version: '2.1.0' },
    ttlSeconds: 300  // Default: 300, max: 3600
  },
  metadata: {
    renewOnHeartbeat: true  // Auto-renew on each heartbeat
  },
  ttl: 5000
}
```

**Duplicate handling:** Last-write-wins with version tracking

#### hub:registered

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:register`)

```typescript
{
  type: 'hub:registered',
  pattern: 'tell',
  correlationId: 'register-request-id',
  payload: {
    actorAddress: '@(browser/widget-123)',
    expiresAt: Date.now() + 300000,  // 5 min from now
    renewalToken: 'renewal-token-xyz',
    version: 1
  }
}
```

#### hub:discover

**Direction:** Client → Server
**Pattern:** `ask` (expects `hub:discovered`)

```typescript
{
  type: 'hub:discover',
  pattern: 'ask',
  payload: {
    pattern: '@(browser/widget-*)',  // Glob pattern
    limit: 50  // Max: 100
  },
  ttl: 5000
}
```

**Pattern matching:** Supports glob wildcards `*`

#### hub:discovered

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:discover`)

```typescript
{
  type: 'hub:discovered',
  pattern: 'tell',
  correlationId: 'discover-request-id',
  payload: {
    actors: [
      {
        actorAddress: '@(browser/widget-123)',
        capabilities: ['render', 'handle-click'],
        metadata: { widgetType: 'chart' },
        connectionId: 'conn-abc',
        registeredAt: 1739731000000,
        expiresAt: 1739731300000,
        version: 1
      }
    ],
    hasMore: false
  }
}
```

### 4.4 Message Delivery Messages

#### hub:send

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget) OR `ask` (with ack)

```typescript
{
  type: 'hub:send',
  pattern: 'ask',  // 'ask' requires hub:delivery_ack
  payload: {
    targetAddress: '@(browser/widget-123)',
    message: {
      type: 'task:assign',
      payload: { taskId: 'task-456', priority: 1 }
    }
  },
  metadata: {
    requireAck: true,
    traceId: 'trace-xyz',
    priority: 1  // 0=high, 2=low
  },
  ttl: 30000  // 30s delivery timeout
}
```

**Constraints:**
- Max message size: 1MB
- Target must be registered
- If offline (MVP): returns `hub:unknown_actor`

#### hub:delivery_ack

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:send` with `pattern: 'ask'`)

```typescript
{
  type: 'hub:delivery_ack',
  pattern: 'tell',
  correlationId: 'send-request-id',
  payload: {
    messageId: 'msg-123',
    deliveredAt: Date.now(),
    status: 'delivered'  // 'delivered' | 'queued' (Phase 2)
  }
}
```

#### hub:broadcast

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)

```typescript
{
  type: 'hub:broadcast',
  pattern: 'tell',
  payload: {
    message: {
      type: 'system:shutdown',
      payload: { reason: 'maintenance', shutdownAt: Date.now() + 60000 }
    },
    excludeSelf: true  // Don't send to sender
  },
  metadata: {
    targetCapability: 'compute'  // Only actors with this capability
  }
}
```

**Constraints:**
- ≤100 actors: Synchronous fan-out (< 5s)
- >100 actors: Async queue (Cloudflare Queues)
- Max throughput: ~1K actors/sec per instance

#### hub:broadcast_ack

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:broadcast`)

```typescript
{
  type: 'hub:broadcast_ack',
  pattern: 'tell',
  payload: {
    messageId: 'broadcast-123',
    deliveredCount: 99,
    queuedCount: 0,  // Non-zero if async
    failedCount: 1
  }
}
```

#### hub:subscribe

**Direction:** Client → Server
**Pattern:** `ask` (expects `hub:subscribed`)

```typescript
{
  type: 'hub:subscribe',
  pattern: 'ask',
  payload: {
    topic: 'events',
    durable: false  // Persist across reconnects?
  },
  ttl: 5000
}
```

#### hub:subscribed

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:subscribe`)

```typescript
{
  type: 'hub:subscribed',
  pattern: 'tell',
  correlationId: 'subscribe-request-id',
  payload: {
    topic: 'events',
    subscriptionId: 'sub-xyz'
  }
}
```

#### hub:publish

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)

```typescript
{
  type: 'hub:publish',
  pattern: 'tell',
  payload: {
    topic: 'events',
    message: {
      type: 'user:login',
      payload: { userId: 'user-123' }
    }
  }
}
```

#### hub:published

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:publish`)

```typescript
{
  type: 'hub:published',
  pattern: 'tell',
  payload: {
    topic: 'events',
    subscriberCount: 5
  }
}
```

### 4.5 Flow Control Messages

#### hub:pause

**Direction:** Server → Client
**Pattern:** `tell` (command)

**Purpose:** Instruct client to pause sending (queue full)

```typescript
{
  type: 'hub:pause',
  pattern: 'tell',
  payload: {
    reason: 'outbound_queue_full'
  }
}
```

**Client behavior:** MUST stop sending until `hub:resume`

#### hub:resume

**Direction:** Server → Client
**Pattern:** `tell` (command)

```typescript
{
  type: 'hub:resume',
  pattern: 'tell',
  payload: {}
}
```

#### hub:queue_stats

**Direction:** Client → Server
**Pattern:** `ask` (expects `hub:queue_stats_response`)

```typescript
{
  type: 'hub:queue_stats',
  pattern: 'ask',
  payload: {},
  ttl: 5000
}
```

#### hub:queue_stats_response

**Direction:** Server → Client
**Pattern:** `tell` (response to `hub:queue_stats`)

```typescript
{
  type: 'hub:queue_stats_response',
  pattern: 'tell',
  correlationId: 'queue-stats-request-id',
  payload: {
    queueDepth: 500,
    processingRate: 100,  // msgs/sec
    pauseThreshold: 1000,
    resumeThreshold: 500
  }
}
```

### 4.6 Error Messages

#### hub:error

**Direction:** Server → Client
**Pattern:** `tell` (error response)

```typescript
{
  type: 'hub:error',
  pattern: 'tell',
  correlationId: 'failed-request-id',
  payload: {
    code: 'timeout' | 'message_expired' | 'internal_error',
    message: 'Human-readable error',
    details: { /* additional context */ },
    retryable: true  // Can client retry?
  }
}
```

**Error codes:**
- `timeout` - Operation exceeded timeout (retryable)
- `message_expired` - TTL expired before delivery (not retryable)
- `internal_error` - Server error (retryable)

#### hub:unknown_actor

**Direction:** Server → Client
**Pattern:** `tell` (error response)

```typescript
{
  type: 'hub:unknown_actor',
  pattern: 'tell',
  correlationId: 'send-request-id',
  payload: {
    actorAddress: '@(browser/missing-actor)',
    message: 'Actor not registered with Signal Hub'
  }
}
```

#### hub:unauthorized

**Direction:** Server → Client
**Pattern:** `tell` (error response)

```typescript
{
  type: 'hub:unauthorized',
  pattern: 'tell',
  correlationId: 'connect-request-id',
  payload: {
    action: 'connect',
    reason: 'Invalid JWT signature'
  }
}
```

#### hub:rate_limited

**Direction:** Server → Client
**Pattern:** `tell` (error response)

```typescript
{
  type: 'hub:rate_limited',
  pattern: 'tell',
  correlationId: 'send-request-id',
  payload: {
    retryAfter: 1000  // Milliseconds
  }
}
```

#### hub:version_mismatch

**Direction:** Server → Client
**Pattern:** `tell` (error response)

```typescript
{
  type: 'hub:version_mismatch',
  pattern: 'tell',
  correlationId: 'connect-request-id',
  payload: {
    clientVersion: '0.1.0',
    serverVersion: '0.2.0',
    supportedVersions: ['0.1.0', '0.2.0'],
    message: 'Upgrade client to 0.2.0'
  }
}
```

#### hub:message_too_large

**Direction:** Server → Client
**Pattern:** `tell` (error response)

```typescript
{
  type: 'hub:message_too_large',
  pattern: 'tell',
  correlationId: 'send-request-id',
  payload: {
    messageSize: 2097152,  // 2MB
    maxSize: 1048576       // 1MB
  }
}
```

---

## 5. Connection Lifecycle

### 5.1 State Machine

Signal Hub uses the existing `ConnectionState` enum from domain schema:

```typescript
type ConnectionState = 'disconnected' | 'connecting' | 'connected' | 'disconnecting';
```

**State transitions:**

```
                      ┌─────────────────────┐
                      │                     │
                      │   disconnected      │
                      │                     │
                      └─────────┬───────────┘
                                │
                                │ connect() → send hub:connect
                                ▼
                      ┌─────────────────────┐
                      │                     │
         timeout(5s)  │    connecting       │
              ┌───────┤                     │
              │       └─────────┬───────────┘
              │                 │
              │                 │ recv hub:connected
              │                 ▼
              │       ┌─────────────────────┐
              │       │                     │◄──────┐
              │       │     connected       │       │ heartbeat (25s)
              │       │                     │───────┘
              │       └─────────┬───────────┘
              │                 │
              │                 │ disconnect() OR hub:disconnect OR error
              │                 ▼
              │       ┌─────────────────────┐
              │       │                     │
              │       │   disconnecting     │
              │       │                     │
              │       └─────────┬───────────┘
              │                 │
              │                 │ cleanup complete
              ▼                 ▼
        ┌─────────────────────┐
        │                     │
        │   disconnected      │
        │                     │
        └─────────────────────┘
```

### 5.2 State Properties

| State | WebSocket | Actor Registry | Message Queue | Heartbeat Timer |
|-------|-----------|----------------|---------------|-----------------|
| **disconnected** | Closed | None | Empty | Stopped |
| **connecting** | Open, handshake pending | Pending | Buffered locally | Not started |
| **connected** | Open, authenticated | Active | Active | Running (25s) |
| **disconnecting** | Closing | Being removed | Flushing | Stopped |

### 5.3 Timeouts

| Operation | Timeout | Action on Timeout | Retryable |
|-----------|---------|-------------------|-----------|
| Connect response | 5s | Close WebSocket, transition to `disconnected` | Yes (with backoff) |
| Heartbeat ack | 10s | Consider connection dead, transition to `disconnected` | Yes (reconnect) |
| Message delivery (ask) | 30s | Return `hub:error` with `code='timeout'` | Yes (client retry) |
| Actor registration | 5s | Return `hub:error` with `code='timeout'` | Yes |
| Disconnect (cleanup) | 2s | Force close WebSocket | No |

### 5.4 Hibernation Handling

**Cloudflare constraint:** Durable Object hibernates after 30s idle

**Strategy:**
1. Client sends `hub:heartbeat` every 25s (< 30s threshold)
2. Server responds with `hub:heartbeat_ack` within 1s
3. If no ack within 10s → client considers connection dead
4. On hibernation: WebSocket preserved, Durable Object wakes on next message

**Hibernation during send (edge case):**
- Message sent at T=29.5s may be lost if hibernation occurs at T=30s
- **Mitigation:** Use `pattern: 'ask'` for critical messages + retry

### 5.5 Duplicate Connection Handling

**Scenario:** Same actor connects twice (page refresh, network glitch)

**Resolution: Last connection wins**

```typescript
// T=0: Actor connects (WebSocket A)
// T=5: Actor connects again (WebSocket B)

// Server behavior:
1. Detect duplicate by actorIdentity from JWT
2. Send hub:disconnect to WebSocket A (reason: 'duplicate_connection')
3. Close WebSocket A
4. Accept WebSocket B
5. Update registry: connectionId → WebSocket B
6. Increment version for conflict resolution
```

**Client behavior:**
- On receiving `hub:disconnect` with `reason: 'duplicate_connection'`
- Do NOT reconnect (new connection already active)
- Transition to `disconnected` state

### 5.6 Exponential Backoff

**Reconnection strategy after failure:**

```typescript
interface ReconnectConfig {
  initialDelayMs: number;      // 100ms
  maxDelayMs: number;          // 30s
  multiplier: number;          // 2
  jitterFraction: number;      // 0.25
}

// Delays: 100ms → 200ms → 400ms → 800ms → 1600ms → ... → 30s (capped)
// With 25% jitter to prevent synchronized reconnects
```

**Reconnect triggers:**
1. Connect timeout (5s)
2. Heartbeat ack timeout (10s)
3. Unexpected WebSocket close
4. `hub:error` with `retryable: true`

**No reconnect on:**
1. Client calls `disconnect()` (graceful)
2. `hub:disconnect` with `reason: 'duplicate_connection'`
3. `hub:unauthorized` (auth failure)
4. `hub:version_mismatch` (client upgrade required)

---

## 6. Security

### 6.1 Authentication Flow (MVP)

**Step 1: Client obtains JWT from external auth service**

```typescript
// Outside Signal Hub (your auth server)
const jwt = issueToken({
  sub: 'user-123',
  actorId: 'browser/client-ui',
  capabilities: ['send', 'broadcast', 'subscribe'],
  iss: 'signal-hub',
  exp: Math.floor(Date.now() / 1000) + 86400  // 24h
});
```

**Step 2: Client connects with JWT**

```typescript
{
  type: 'hub:connect',
  metadata: {
    protocolVersion: '0.1.0',
    authToken: `bearer ${jwt}`
  }
}
```

**Step 3: Server validates JWT**

```typescript
import { verify } from 'jsonwebtoken';

async function validateJWT(authToken: string, jwtSecret: string): Promise<ActorIdentity> {
  const token = authToken.replace(/^bearer\s+/i, '');

  const decoded = verify(token, jwtSecret, {
    algorithms: ['HS256', 'RS256'],
    issuer: 'signal-hub',
    maxAge: '24h'
  }) as JWTPayload;

  return {
    actorId: decoded.actorId,
    userId: decoded.sub,
    capabilities: decoded.capabilities ?? [],
    expiresAt: decoded.exp * 1000
  };
}
```

**Step 4: Server responds with verified identity**

```typescript
{
  type: 'hub:connected',
  metadata: {
    actorIdentity: '@(browser/client-ui)',  // Server-verified
    tokenExpiresAt: 1739731553000
  }
}
```

### 6.2 Server-Enforced Identity

**Security principle:** Clients cannot spoof `from` addresses

```typescript
class SignalHubSession {
  private verifiedIdentity: CanonicalAddress;

  constructor(identity: ActorIdentity) {
    this.verifiedIdentity = `@(${identity.actorId})`;
  }

  async sendMessage(msg: SharedMessage): Promise<void> {
    // CRITICAL: Replace client-provided 'from' with verified identity
    const secureMsg: SharedMessage = {
      ...msg,
      from: this.verifiedIdentity  // Server-enforced
    };

    await this.deliverMessage(secureMsg);
  }
}
```

**Attack scenario prevented:**

```typescript
// ❌ Malicious client tries to spoof
const maliciousMsg: SharedMessage = {
  from: '@(admin/privileged-actor)',  // Fake
  type: 'hub:broadcast',
  payload: { message: { type: 'system:shutdown' } }
};

// ✅ Server replaces with verified identity
// Actual message sent:
// from: '@(browser/client-ui)'  (NOT spoofed address)
```

### 6.3 JWT Configuration

**Recommended payload structure:**

```typescript
interface JWTPayload {
  sub: string;              // User ID (required)
  actorId: string;          // Actor path without @() (required)
  capabilities: string[];   // Actor capabilities (required)
  iss: string;              // Issuer: "signal-hub" (required)
  exp: number;              // Expiration Unix timestamp (required)
  iat: number;              // Issued at (required)
  nbf?: number;             // Not before (optional)
  jti?: string;             // JWT ID for revocation (optional)
}
```

**Environment variables:**

```bash
JWT_SECRET="base64-encoded-secret"
JWT_ISSUER="signal-hub"
JWT_MAX_AGE="24h"
JWT_ALGORITHM="HS256"  # or RS256 for asymmetric
```

**Token expiry by environment:**

| Environment | Expiry | Rationale |
|-------------|--------|-----------|
| Development | 7 days | Developer convenience |
| Staging | 24 hours | Balance security/usability |
| Production | 1-4 hours | High security, frequent refresh |

### 6.4 Authorization Model (Phase 2 - Future)

**Actor-to-actor permissions:**

```typescript
interface ActorPermissions {
  actorId: CanonicalAddress;
  allowedTargets: CanonicalAddress[];  // Empty = allow all
  deniedTargets: CanonicalAddress[];
  capabilities: string[];
}

// Example: Actor can only send to specific targets
{
  actorId: '@(browser/widget-123)',
  allowedTargets: ['@(local/coordinator-main)', '@(browser/widget-*)'],
  deniedTargets: ['@(admin/*)'],
  capabilities: ['send', 'subscribe']  // Cannot broadcast
}
```

**Topic ACLs:**

```typescript
interface TopicACL {
  topic: string;
  publishers: CanonicalAddress[];
  subscribers: CanonicalAddress[];
  publicRead: boolean;
  publicWrite: boolean;
}

// Example: Private admin topic
{
  topic: 'admin:commands',
  publishers: ['@(admin/*)'],
  subscribers: ['@(admin/*)'],
  publicRead: false,
  publicWrite: false
}
```

### 6.5 Threat Model

| Threat | Mitigation | Status |
|--------|------------|--------|
| **Address spoofing** | Server-enforced `from` field | ✅ MVP |
| **Man-in-the-middle** | WSS (TLS 1.3) encryption | ✅ MVP |
| **Replay attacks** | Message ID deduplication + timestamp | ✅ MVP |
| **Denial of service** | Rate limiting + backpressure | ✅ MVP |
| **Unauthorized access** | JWT validation | ✅ MVP |
| **Message tampering** | HMAC signatures | ⏳ Phase 3 |

**See also:** `docs/signal-hub/SECURITY.md` for detailed security analysis

---

## 7. Delivery Guarantees

### 7.1 At-Most-Once (MVP)

**Default behavior:** Fire-and-forget with no acknowledgment

**Semantics:**
- Client sends message with `pattern: 'tell'`
- Server attempts delivery
- No acknowledgment required
- Message may be delivered 0 or 1 times

**When messages may be lost:**
- WebSocket disconnect during send
- Durable Object eviction mid-delivery
- Hibernation transition
- Target actor offline
- Message too large (>1MB)
- TTL expired

**Use cases:**
- Heartbeats (periodic, loss acceptable)
- Status updates (transient state)
- Metrics/telemetry (high volume)
- Best-effort notifications

**Example:**

```typescript
const msg: SharedMessage = {
  type: 'hub:send',
  pattern: 'tell',  // ← At-most-once
  payload: {
    targetAddress: '@(browser/widget-123)',
    message: { type: 'status:update', payload: { status: 'idle' } }
  },
  ttl: 5000
};

await signalHub.send(msg);
// ✓ Message sent, no guarantee of delivery
```

### 7.2 At-Least-Once (Phase 2 - Future)

**With acknowledgment:** Guaranteed delivery with retry

**Semantics:**
- Client sends message with `pattern: 'ask'`
- Server confirms delivery with `hub:delivery_ack`
- Client retries if no ack within timeout
- Message may be delivered 1 or more times (duplicates possible)

**Flow:**

```
Client                 Signal Hub              Target Actor
  |                       |                         |
  ├─hub:send (ask)──────>│                         │
  |                       ├─deliver───────────────>│
  |                       │                         │
  |<──hub:delivery_ack────┤                         │
  |                       |                         |
```

**Retry strategy:**

```typescript
async function sendWithRetry(
  message: SharedMessage,
  maxRetries = 3,
  timeoutMs = 30000
): Promise<void> {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      const ack = await signalHub.sendAndWait(message, timeoutMs);
      if (ack.type === 'hub:delivery_ack') {
        return;  // Success
      }
    } catch (err) {
      if (err.code === 'timeout' && attempt < maxRetries) {
        console.warn(`Attempt ${attempt} timed out, retrying...`);
        await sleep(1000 * attempt);  // Exponential backoff
        continue;
      }
      throw err;
    }
  }
  throw new Error('Failed after retries');
}
```

**Use cases:**
- Task assignments (critical work)
- State synchronization (must propagate)
- Commands (must execute)
- Durable workflows

### 7.3 Ordering Guarantees

**Per-connection FIFO: YES**

Messages from Actor A → Actor B maintain send order:

```typescript
// Actor A sends three messages
await hub.send({ to: actorB, seq: 1 });
await hub.send({ to: actorB, seq: 2 });
await hub.send({ to: actorB, seq: 3 });

// Actor B receives in order: seq=1, seq=2, seq=3
```

**Global ordering: NO**

Messages from different actors have no ordering guarantee:

```typescript
// Actor A and Actor C both send to Actor B
actorA.send({ to: actorB, from: 'A', seq: 1 });
actorC.send({ to: actorB, from: 'C', seq: 1 });

// Actor B may receive in ANY order: A:1, C:1 OR C:1, A:1
```

**Broadcast ordering: NO**

Broadcast messages may arrive in different order at different actors:

```typescript
await hub.broadcast({ type: 'event:1' });
await hub.broadcast({ type: 'event:2' });

// Actor X receives: event:1, event:2
// Actor Y receives: event:2, event:1  ← Different order!
```

**Mitigation:** Include sequence numbers in payload for application-level ordering

### 7.4 TTL Handling

**Expiration check:**

```typescript
function isExpired(msg: SharedMessage, now: number): boolean {
  if (msg.ttl === null) return false;  // No expiration
  return (msg.timestamp + msg.ttl) < now;
}
```

**Server behavior:**
1. Check TTL on receive from client
2. Check TTL before delivery to target
3. Check TTL before queue processing

**If expired:**
- Drop message (no delivery)
- Send `hub:error` with `code: 'message_expired'`
- If `pattern: 'ask'`, error returned as response

**Recommended TTL values:**

| Message Type | TTL | Rationale |
|--------------|-----|-----------|
| Heartbeat | 10s | Short-lived, frequent |
| Status update | 5s | Transient state |
| Task assignment | 30s | Critical but time-sensitive |
| Broadcast | 60s | Large fan-out may take time |
| Query (ask) | 5s | User waiting, fast timeout |

### 7.5 Deduplication

**Problem:** At-least-once delivery causes duplicates on retry

**Solution:** Message ID seen-set with TTL-bounded cache

```typescript
class SeenMessageCache {
  private cache: Map<string, { timestamp: number; expiresAt: number }>;

  has(messageId: string): boolean {
    const entry = this.cache.get(messageId);
    if (!entry) return false;

    // Check expiration
    if (Date.now() > entry.expiresAt) {
      this.cache.delete(messageId);
      return false;
    }

    return true;
  }

  add(messageId: string, ttlMs = 60000): void {
    this.cache.set(messageId, {
      timestamp: Date.now(),
      expiresAt: Date.now() + ttlMs
    });
  }
}
```

**Deduplication strategy:**
- Server maintains seen-set with 60s TTL
- On duplicate: Return same ack (idempotent)
- Covers retry window (30s timeout + backoff)

**Application-level deduplication:**

Actors should implement their own deduplication for:
- Messages bypassing Signal Hub
- External system messages
- Idempotent operation semantics

**See also:** `docs/signal-hub/DELIVERY_GUARANTEES.md` for detailed analysis

---

## 8. Scalability

### 8.1 The Broadcast Problem

**⚠️ Critical:** Naive synchronous fan-out causes CPU exhaustion

```typescript
// ❌ DANGEROUS: Synchronous broadcast
async function naiveBroadcast(message: SharedMessage) {
  const actors = await getRegisteredActors();  // 1000+ actors

  for (const actor of actors) {
    await sendToActor(actor, message);  // ~30ms per send
  }

  return { deliveredCount: actors.length };
}

// Why it fails:
// 1000 actors × 30ms = 30,000ms = 30s
// Exactly at Cloudflare CPU limit → timeout → cascade failure
```

### 8.2 Async Batching Strategy

**✅ Solution:** Queue-based async batching for large broadcasts

```typescript
async function broadcast(message: SharedMessage, actors: ActorRegistration[]) {
  if (actors.length <= 100) {
    // Small broadcast - synchronous OK (< 1s)
    return await broadcastSync(message, actors);
  }

  // Large broadcast - queue for async processing
  const BATCH_SIZE = 100;
  const batches = chunkArray(actors, BATCH_SIZE);

  // Send first batch immediately (no queue latency)
  const firstBatch = batches[0];
  const immediate = await broadcastSync(message, firstBatch);

  // Queue remaining batches
  for (let i = 1; i < batches.length; i++) {
    const batch = batches[i];
    await cloudflareQueue.send({
      messageId: message.id,
      targetActors: batch.map(a => a.address),
      message,
      batchIndex: i,
      totalBatches: batches.length
    });
  }

  return {
    deliveredCount: immediate.deliveredCount,
    queuedCount: actors.length - firstBatch.length
  };
}
```

**Performance characteristics:**

| Actor Count | Strategy | Latency | CPU Time | Risk |
|-------------|----------|---------|----------|------|
| 1-100 | Synchronous | <5s | <5s | Low |
| 100-1000 | Mixed (first batch sync, rest queued) | 5-15s | <5s per batch | Low |
| 1000-10K | Queued batching | 15-60s | <5s per batch | Low |
| 10K+ | Queued + sharding | 30-120s | <5s per batch | Medium |

### 8.3 Actor Registry Limits

**Maximum actors per Signal Hub instance: 50,000**

**Calculation:**

```
Per-actor memory:
- Registration record: ~400 bytes
- WebSocket metadata: ~200 bytes
- Heartbeat timer: ~100 bytes
Total: ~700 bytes/actor

Durable Object memory limit: ~128MB
Theoretical max: 128MB / 700 bytes ≈ 180K actors

Practical limit: 50K actors (with overhead for queues, buffers, state)
```

**Two-tier storage:**

```typescript
class SignalHubRegistry {
  // Tier 1: Volatile (fast, in-memory)
  private volatile: Map<string, ActorRegistration> = new Map();

  // Tier 2: Durable (persistent, slower)
  private storage: DurableObjectStorage;

  // On wake from hibernation
  async restoreFromDurable(): Promise<void> {
    const keys = await this.storage.list({ prefix: 'actor:' });
    for (const [key, reg] of keys) {
      const actorReg = reg as ActorRegistration;
      if (Date.now() < actorReg.expiresAt) {
        this.volatile.set(actorReg.actorAddress, actorReg);
      }
    }
  }

  // On register
  async register(actor: ActorRegistration): Promise<void> {
    // Update Tier 1 (immediate)
    this.volatile.set(actor.actorAddress, actor);

    // Update Tier 2 (persistent)
    await this.storage.put(`actor:${actor.actorAddress}`, actor, {
      expirationTtl: actor.ttlSeconds
    });
  }
}
```

**Registry size monitoring:**

```typescript
function getRegistryMetrics(): RegistryMetrics {
  const totalActors = this.volatile.size;
  const capacityUtilization = totalActors / 50000;

  if (capacityUtilization > 0.95) {
    // Reject new registrations, trigger shard creation
    throw new Error('Registry at capacity (50K actors)');
  }

  return { totalActors, capacityUtilization };
}
```

### 8.4 Sharding Strategy

**When to shard:** Single instance exceeds 50K actors

**Consistent hashing:**

```typescript
function getShardForActor(actorAddress: string, numShards: number): string {
  const hash = hashString(actorAddress);  // FNV-1a hash
  const shardIndex = hash % numShards;
  return `signal-hub-shard-${shardIndex}`;
}

// Example: 10 shards = 500K actor capacity
const config = {
  numShards: 10,
  shardIds: Array.from({ length: 10 }, (_, i) => `signal-hub-shard-${i}`)
};

// Route actor to correct shard
const shardId = getShardForActor(actorAddress, config.numShards);
const hubStub = env.SIGNAL_HUB.get(env.SIGNAL_HUB.idFromName(shardId));
```

**Cross-shard discovery:**

```typescript
async function discoverActorsAcrossShards(
  pattern: string,
  limit: number
): Promise<ActorRegistration[]> {
  // Query all shards in parallel
  const shardPromises = config.shardIds.map(async (shardId) => {
    const hubStub = env.SIGNAL_HUB.get(env.SIGNAL_HUB.idFromName(shardId));
    const response = await hubStub.discover({ pattern, limit });
    return response.actors;
  });

  // Aggregate and deduplicate
  const allActors = (await Promise.all(shardPromises)).flat();
  const uniqueActors = deduplicateByAddress(allActors);
  return uniqueActors.slice(0, limit);
}
```

### 8.5 Thundering Herd Protection

**Problem:** 10K actors reconnect simultaneously after deploy

**Mitigation 1: Client-side jitter**

```typescript
// Random 0-30s delay on first reconnect attempt
const jitter = Math.random() * 30000;
await sleep(jitter);
await connect();

// Result: 10K actors / 30s = ~333 connections/sec (manageable)
```

**Mitigation 2: Server-side rate limiting**

```typescript
class TokenBucketRateLimiter {
  private tokens: number;
  private readonly capacity: number = 100;
  private readonly refillRate: number = 100;  // tokens/sec

  async allow(): Promise<boolean> {
    this.refill();

    if (this.tokens >= 1) {
      this.tokens--;
      return true;
    }

    return false;  // Rate limited
  }
}

// Limit: 100 connections/sec sustained
```

**Performance targets:**

| Metric | Target | Strategy |
|--------|--------|----------|
| Broadcast throughput | 1K actors/sec | Queue batching |
| Registry lookup | <10ms | In-memory Map |
| Connection rate | 100/sec sustained | Token bucket |
| Max actors per instance | 50K | Two-tier storage |

**See also:** `docs/signal-hub/SCALABILITY.md` for detailed scalability analysis

---

## 9. Error Handling

### 9.1 Error Types and Codes

| Error Type | Code | Retryable | Client Action |
|------------|------|-----------|---------------|
| `hub:unknown_actor` | N/A | No | Check actor exists, re-register |
| `hub:message_too_large` | N/A | No | Split message or reduce payload |
| `hub:rate_limited` | N/A | Yes | Wait `retryAfter` ms, retry |
| `hub:error` | `timeout` | Yes | Retry with exponential backoff |
| `hub:error` | `message_expired` | No | Increase TTL or don't retry |
| `hub:error` | `internal_error` | Yes | Retry once, report to ops |
| `hub:unauthorized` | N/A | No | Fix auth token, reconnect |
| `hub:version_mismatch` | N/A | No | Upgrade client version |

### 9.2 Correlation

**Every error includes `correlationId` linking to failed request:**

```typescript
// Original request
const sendMsg: SharedMessage = {
  id: 'msg-abc-123',  // ← Request ID
  type: 'hub:send',
  pattern: 'ask'
};

// Error response
{
  type: 'hub:error',
  correlationId: 'msg-abc-123',  // ← Links to request
  payload: {
    code: 'timeout',
    message: 'Message delivery timed out after 30s',
    retryable: true
  }
}
```

### 9.3 Distributed Tracing

**Add `traceId` in metadata for cross-service correlation:**

```typescript
// Request with trace context
const sendMsg: SharedMessage = {
  id: 'msg-123',
  type: 'hub:send',
  metadata: {
    traceId: 'trace-xyz-789',
    spanId: 'span-001'
  }
};

// Error response preserves trace context
{
  type: 'hub:error',
  correlationId: 'msg-123',
  metadata: {
    traceId: 'trace-xyz-789',  // ← Preserved
    spanId: 'span-002'
  },
  payload: { code: 'timeout', message: '...' }
}
```

### 9.4 Error Handling Strategy

```typescript
async function handleDeliveryError(error: HubMessage): Promise<void> {
  switch (error.type) {
    case 'hub:unknown_actor':
      console.error('Actor not found:', error.payload.actorAddress);
      // Option 1: Discover actor (may have moved)
      // Option 2: Notify user actor unavailable
      break;

    case 'hub:message_too_large':
      console.error('Message too large:', error.payload.messageSize);
      // Option 1: Split message into chunks
      // Option 2: Use external storage + reference
      break;

    case 'hub:rate_limited':
      const { retryAfter } = error.payload;
      console.warn(`Rate limited, retrying in ${retryAfter}ms`);
      await sleep(retryAfter);
      // Retry original request
      break;

    case 'hub:error':
      const { code, retryable } = error.payload;

      if (code === 'timeout' && retryable) {
        console.warn('Delivery timeout, retrying...');
        await retryWithBackoff();
      } else if (code === 'message_expired') {
        console.error('Message expired, discarding');
      } else if (code === 'internal_error' && retryable) {
        console.error('Server error, retrying once...');
        await retryWithBackoff({ maxRetries: 1 });
      }
      break;

    default:
      console.error('Unknown error type:', error.type);
  }
}
```

**See also:** `docs/signal-hub/DELIVERY_GUARANTEES.md` Section 5 for detailed error handling

---

## 10. Examples

### 10.1 Connection with Authentication

```typescript
import { SignalHubClient } from '@agentic-primer/signal-hub-client';

async function connectToSignalHub() {
  // 1. Obtain JWT from auth service
  const authResponse = await fetch('https://auth.example.com/token', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      userId: 'user-123',
      actorId: 'browser/client-ui'
    })
  });
  const { token } = await authResponse.json();

  // 2. Connect to Signal Hub with JWT
  const hub = new SignalHubClient('wss://hub.example.com/connect');

  try {
    await hub.connect(token);
    console.log('Connected to Signal Hub');

    // 3. Start heartbeat (prevents hibernation)
    hub.startHeartbeat();

  } catch (err) {
    if (err.type === 'hub:unauthorized') {
      console.error('Authentication failed:', err.payload.reason);
      // Redirect to login
    } else if (err.type === 'hub:version_mismatch') {
      console.error('Client version incompatible:', err.payload.message);
      // Prompt user to refresh/upgrade
    }
  }
}
```

### 10.2 Actor Registration

```typescript
async function registerActor(hub: SignalHubClient) {
  try {
    const registration = await hub.register({
      actorAddress: '@(browser/widget-123)',
      capabilities: ['render', 'handle-click', 'handle-resize'],
      metadata: {
        widgetType: 'chart',
        version: '2.1.0'
      },
      ttlSeconds: 300  // 5 minutes
    });

    console.log('Registered:', registration.actorAddress);
    console.log('Expires at:', new Date(registration.expiresAt));
    console.log('Renewal token:', registration.renewalToken);

    // Auto-renew before expiration
    setTimeout(() => {
      hub.renewRegistration(registration.renewalToken);
    }, 240_000);  // Renew after 4 minutes

  } catch (err) {
    if (err.type === 'hub:error' && err.payload.code === 'registry_full') {
      console.error('Signal Hub at capacity, try different shard');
    }
  }
}
```

### 10.3 Point-to-Point Messaging

**Fire-and-forget (at-most-once):**

```typescript
async function sendFireAndForget(hub: SignalHubClient) {
  await hub.send({
    to: '@(browser/widget-456)',
    type: 'hub:send',
    pattern: 'tell',  // No acknowledgment
    payload: {
      targetAddress: '@(browser/widget-456)',
      message: {
        type: 'status:update',
        payload: { status: 'idle' }
      }
    },
    ttl: 5000
  });

  console.log('Message sent (no ack)');
}
```

**With acknowledgment (at-least-once):**

```typescript
async function sendWithAck(hub: SignalHubClient) {
  try {
    const ack = await hub.sendAndWait({
      to: '@(browser/widget-456)',
      type: 'hub:send',
      pattern: 'ask',  // Require acknowledgment
      payload: {
        targetAddress: '@(browser/widget-456)',
        message: {
          type: 'task:assign',
          payload: { taskId: 'task-789', priority: 1 }
        }
      },
      metadata: {
        requireAck: true,
        traceId: 'trace-xyz'
      },
      ttl: 30000  // 30s timeout
    }, 30000);

    if (ack.type === 'hub:delivery_ack') {
      console.log('Message delivered:', ack.payload.status);
    }

  } catch (err) {
    if (err.code === 'timeout') {
      console.error('Delivery timeout, retrying...');
      // Retry logic
    }
  }
}
```

### 10.4 Broadcast to All Actors

```typescript
async function broadcastShutdown(hub: SignalHubClient) {
  const result = await hub.broadcast({
    type: 'hub:broadcast',
    pattern: 'tell',
    payload: {
      message: {
        type: 'system:shutdown',
        payload: {
          reason: 'maintenance',
          shutdownAt: Date.now() + 60000  // 1 minute warning
        }
      },
      excludeSelf: true  // Don't send to sender
    },
    metadata: {
      targetCapability: 'compute'  // Only actors with 'compute' capability
    }
  });

  console.log(`Broadcast delivered to ${result.deliveredCount} actors`);
  if (result.queuedCount > 0) {
    console.log(`${result.queuedCount} actors queued for async delivery`);
  }
}
```

### 10.5 Topic Subscription

**Subscribe:**

```typescript
async function subscribeToEvents(hub: SignalHubClient) {
  const subscription = await hub.subscribe({
    topic: 'events',
    durable: false  // Don't persist across reconnects
  });

  console.log('Subscribed to topic:', subscription.topic);
  console.log('Subscription ID:', subscription.subscriptionId);

  // Handle published messages
  hub.on('message', (msg: SharedMessage) => {
    if (msg.type === 'hub:publish' && msg.payload.topic === 'events') {
      console.log('Event received:', msg.payload.message);
    }
  });
}
```

**Publish:**

```typescript
async function publishEvent(hub: SignalHubClient) {
  const result = await hub.publish({
    topic: 'events',
    message: {
      type: 'user:login',
      payload: { userId: 'user-123', timestamp: Date.now() }
    }
  });

  console.log(`Published to ${result.subscriberCount} subscribers`);
}
```

**Unsubscribe:**

```typescript
async function unsubscribeFromEvents(
  hub: SignalHubClient,
  subscriptionId: string
) {
  await hub.unsubscribe({ subscriptionId });
  console.log('Unsubscribed from events');
}
```

### 10.6 Actor Discovery

**Discover actors by pattern:**

```typescript
async function discoverWidgets(hub: SignalHubClient) {
  const result = await hub.discover({
    pattern: '@(browser/widget-*)',  // Glob pattern
    limit: 50
  });

  console.log(`Found ${result.actors.length} widgets:`);
  result.actors.forEach(actor => {
    console.log(`- ${actor.actorAddress}`);
    console.log(`  Capabilities: ${actor.capabilities.join(', ')}`);
    console.log(`  Metadata:`, actor.metadata);
  });

  if (result.hasMore) {
    console.log('More results available (increase limit or use pagination)');
  }
}
```

**List all actors:**

```typescript
async function listAllActors(hub: SignalHubClient) {
  let offset = 0;
  const limit = 100;
  const allActors: ActorRegistration[] = [];

  while (true) {
    const result = await hub.listActors({ offset, limit });
    allActors.push(...result.actors);

    console.log(`Fetched ${result.actors.length} actors (offset: ${offset})`);

    if (!result.hasMore) break;
    offset += limit;
  }

  console.log(`Total actors: ${allActors.length}`);
  return allActors;
}
```

### 10.7 Error Handling with Retry

```typescript
async function sendWithRetry(
  hub: SignalHubClient,
  message: SharedMessage,
  maxRetries = 3
): Promise<void> {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      const ack = await hub.sendAndWait(message, 30000);

      if (ack.type === 'hub:delivery_ack') {
        console.log(`Delivered on attempt ${attempt}`);
        return;
      }

    } catch (err) {
      if (err.type === 'hub:rate_limited') {
        const retryAfter = err.payload.retryAfter;
        console.warn(`Rate limited, waiting ${retryAfter}ms...`);
        await sleep(retryAfter);
        continue;
      }

      if (err.type === 'hub:unknown_actor') {
        console.error('Target actor not registered');
        throw err;  // Don't retry
      }

      if (err.code === 'timeout' && attempt < maxRetries) {
        console.warn(`Attempt ${attempt} timed out, retrying...`);
        await sleep(1000 * attempt);  // Exponential backoff
        continue;
      }

      throw err;
    }
  }

  throw new Error(`Failed after ${maxRetries} attempts`);
}
```

### 10.8 SEAG to Browser Communication

**Complete end-to-end example:**

```typescript
// SEAG Actor (local runtime)
import { LocalActor } from '@agentic-primer/seag';
import { SignalHubClient } from '@agentic-primer/signal-hub-client';

class SEAGProcessor extends LocalActor {
  private hub: SignalHubClient;

  async initialize() {
    // Connect to Signal Hub
    this.hub = new SignalHubClient('wss://hub.example.com/connect');
    await this.hub.connect(this.authToken);

    // Register with capabilities
    await this.hub.register({
      actorAddress: '@(seag/processor-main)',
      capabilities: ['processing', 'coordination'],
      metadata: { runtime: 'seag', version: '1.0.0' }
    });

    console.log('SEAG processor registered with Signal Hub');
  }

  async sendToBrowser(widgetAddress: string, renderData: unknown) {
    await this.hub.send({
      to: widgetAddress,
      type: 'hub:send',
      pattern: 'ask',
      payload: {
        targetAddress: widgetAddress,
        message: {
          type: 'render',
          payload: renderData
        }
      },
      metadata: { requireAck: true },
      ttl: 30000
    });
  }
}

// Browser Actor (browser runtime)
import { BrowserActor } from '@agentic-primer/browser-runtime';

class WidgetActor extends BrowserActor {
  private hub: SignalHubClient;

  async initialize() {
    // Connect to Signal Hub
    this.hub = new SignalHubClient('wss://hub.example.com/connect');
    await this.hub.connect(this.authToken);

    // Register
    await this.hub.register({
      actorAddress: '@(browser/widget-123)',
      capabilities: ['render', 'interact'],
      metadata: { widgetType: 'chart' }
    });

    // Listen for messages from SEAG
    this.hub.on('message', (msg: SharedMessage) => {
      if (msg.type === 'hub:send' && msg.payload.message.type === 'render') {
        this.render(msg.payload.message.payload);
      }
    });

    console.log('Browser widget registered with Signal Hub');
  }

  private render(data: unknown) {
    console.log('Rendering data:', data);
    // Update DOM with data
  }
}

// Usage
const seag = new SEAGProcessor();
await seag.initialize();

const widget = new WidgetActor();
await widget.initialize();

// SEAG sends render data to browser widget
await seag.sendToBrowser('@(browser/widget-123)', {
  component: 'Chart',
  data: [1, 2, 3, 4, 5]
});

// Browser widget receives and renders
```

---

## 11. Cloudflare Constraints

### 11.1 CPU Limits

| Constraint | Limit | Implication |
|------------|-------|-------------|
| **CPU time per request** | 30 seconds | Broadcast >1000 actors requires async queue |
| **Wall-clock time** | Unlimited | Can wait for external APIs, but CPU budget still applies |

**Mitigation:**
- Use async batching for broadcasts >100 actors
- Process in 100-actor batches via Cloudflare Queues
- Each batch: <5s CPU time (safe margin)

### 11.2 Hibernation

| Constraint | Limit | Implication |
|------------|-------|-------------|
| **Hibernation timeout** | 30 seconds idle | WebSocket pauses, no messages processed |
| **Wake latency** | 1-3 seconds | First message after hibernation delayed |

**Mitigation:**
- Client sends heartbeat every 25s (< 30s threshold)
- Server responds with ack within 1s
- Prevents hibernation for active connections

**Hibernation behavior:**
- WebSocket preserved by Cloudflare
- Durable Object not invoked during hibernation
- Incoming messages queued by platform
- First message triggers wake, processes queue

### 11.3 Message Size

| Constraint | Limit | Implication |
|------------|-------|-------------|
| **WebSocket frame size** | 1 MB | Messages >1MB rejected with `hub:message_too_large` |
| **Durable Object storage value** | 128 KB | Actor registration metadata limited |

**Mitigation:**
- Split large messages into chunks
- Use external storage (R2) for large payloads
- Pass reference in message payload

### 11.4 Storage Limits

| Constraint | Limit | Implication |
|------------|-------|-------------|
| **Durable Object storage** | 128 MB (soft limit) | Max ~50K actor registrations per instance |
| **Key size** | 2 KB | Actor addresses limited to 2KB |
| **Value size** | 128 KB | Registration metadata limited |

**Mitigation:**
- Two-tier storage (volatile + durable)
- TTL-based auto-eviction (default: 5 minutes)
- Shard beyond 50K actors using consistent hashing

### 11.5 WebSocket Connections

| Constraint | Limit | Implication |
|------------|-------|-------------|
| **Connections per DO** | ~50,000 | Hard limit by Cloudflare platform |
| **Connection rate** | No enforced limit | Server implements 100/sec rate limit |

**Mitigation:**
- Token bucket rate limiter: 100 connections/sec
- Client-side jitter on reconnect (0-30s)
- Server returns `hub:rate_limited` if exceeded

### 11.6 Queues

| Constraint | Limit | Implication |
|------------|-------|-------------|
| **Message size** | 128 KB | Broadcast payloads limited |
| **Batch size** | 100 messages | Process 100 actors per queue message |
| **Throughput** | ~1000 msg/sec | Broadcast throughput: ~1K actors/sec |

**Mitigation:**
- Batch actors in groups of 100
- First batch synchronous (no queue latency)
- Remaining batches queued for async processing

### 11.7 Constraints Summary Table

| Resource | Limit | Signal Hub Usage | Mitigation |
|----------|-------|------------------|------------|
| CPU time | 30s | Broadcast fan-out | Async queue for >100 actors |
| Hibernation | 30s idle | Connection pause | Heartbeat every 25s |
| Message size | 1MB | Payload size | Reject with error, suggest chunking |
| Storage | 128MB | Actor registry | Max 50K actors, shard beyond |
| Connections | 50K | WebSocket limit | Reject new connections at capacity |
| Queue throughput | 1K msg/sec | Broadcast batches | Acceptable for target scale |

---

## 12. Protocol Versioning

### 12.1 Semantic Versioning

**Version format:** `MAJOR.MINOR.PATCH`

- **MAJOR:** Breaking changes (incompatible with previous versions)
- **MINOR:** New features (backward compatible)
- **PATCH:** Bug fixes (backward compatible)

**Current version:** `0.1.0` (MVP)

### 12.2 Version Negotiation

**Client sends version in `hub:connect`:**

```typescript
{
  type: 'hub:connect',
  metadata: {
    protocolVersion: '0.1.0'  // Client version
  }
}
```

**Server responds with compatible version:**

```typescript
// Compatible version
{
  type: 'hub:connected',
  metadata: {
    serverVersion: '0.1.0'  // Same or compatible
  }
}

// Incompatible version
{
  type: 'hub:version_mismatch',
  payload: {
    clientVersion: '0.1.0',
    serverVersion: '0.2.0',
    supportedVersions: ['0.1.0', '0.2.0'],
    message: 'Upgrade client to 0.2.0'
  }
}
```

### 12.3 Compatibility Rules

**Backward compatibility:**
- MINOR version bump: Server supports both old and new versions
- Client with older MINOR version can connect to newer server
- Example: Client 0.1.0 can connect to Server 0.2.0

**Breaking changes:**
- MAJOR version bump: Server drops support for old MAJOR version
- Client must upgrade to new MAJOR version
- Example: Client 0.x cannot connect to Server 1.x

### 12.4 Evolution Strategy

**Phase 1 (MVP): v0.1.0**
- At-most-once delivery
- Basic authentication (JWT)
- Synchronous broadcast (≤100 actors)

**Phase 2 (Reliability): v0.2.0**
- At-least-once delivery with ack
- Deduplication
- Async broadcast queue (>100 actors)
- Queued delivery for offline actors

**Phase 3 (Security): v0.3.0**
- HMAC signatures
- Actor-to-actor permissions
- Topic ACLs

**Phase 4 (Scale): v1.0.0**
- Sharding support
- Cross-shard discovery
- Production-ready

### 12.5 Version Detection

**Server validates version on connect:**

```typescript
function isVersionCompatible(
  clientVersion: string,
  serverVersion: string
): boolean {
  const [clientMajor] = clientVersion.split('.').map(Number);
  const [serverMajor] = serverVersion.split('.').map(Number);

  // MAJOR version must match
  return clientMajor === serverMajor;
}

async function handleConnect(msg: HubConnectMessage): Promise<void> {
  const clientVersion = msg.metadata.protocolVersion;
  const serverVersion = '0.1.0';

  if (!isVersionCompatible(clientVersion, serverVersion)) {
    return this.sendError({
      type: 'hub:version_mismatch',
      payload: {
        clientVersion,
        serverVersion,
        supportedVersions: ['0.1.0'],
        message: `Client version ${clientVersion} incompatible with server ${serverVersion}`
      }
    });
  }

  // Proceed with connection...
}
```

---

## Implementation Checklist

### Phase 1: Basic Connectivity

**Client:**
- [ ] WebSocket connection management
- [ ] SharedMessage serialization/deserialization
- [ ] Connection state machine (4 states)
- [ ] Connect with JWT auth token
- [ ] Heartbeat timer (25s interval)
- [ ] Heartbeat ack timeout detection (10s)
- [ ] Exponential backoff reconnect

**Server:**
- [ ] Durable Object WebSocket handler
- [ ] JWT validation (HS256/RS256)
- [ ] Session management with verified identity
- [ ] Server-enforced `from` field
- [ ] Heartbeat response handler
- [ ] Connection state tracking
- [ ] Protocol version negotiation

### Phase 2: Actor Discovery

**Client:**
- [ ] `hub:register` implementation
- [ ] Registration renewal (TTL-based)
- [ ] `hub:discover` with glob patterns
- [ ] `hub:list_actors` with pagination

**Server:**
- [ ] Actor registry (in-memory + durable)
- [ ] Duplicate registration handling (last-write-wins)
- [ ] TTL-based auto-expiration
- [ ] Registry size limit enforcement (50K)
- [ ] Pattern matching for discovery

### Phase 3: Message Delivery

**Client:**
- [ ] `hub:send` with `pattern: 'tell'` (fire-and-forget)
- [ ] `hub:send` with `pattern: 'ask'` (with ack)
- [ ] `hub:broadcast` implementation
- [ ] Retry logic for at-least-once delivery
- [ ] TTL handling

**Server:**
- [ ] Point-to-point message routing
- [ ] Delivery acknowledgment (`hub:delivery_ack`)
- [ ] Broadcast with async batching (>100 actors)
- [ ] Cloudflare Queue integration for large broadcasts
- [ ] TTL expiration checking
- [ ] Message ID deduplication cache

### Phase 4: Pub/Sub

**Client:**
- [ ] `hub:subscribe` implementation
- [ ] `hub:publish` implementation
- [ ] `hub:unsubscribe` implementation
- [ ] Topic message handler

**Server:**
- [ ] Topic registry
- [ ] Subscription management
- [ ] Topic-based message fan-out
- [ ] Subscriber count tracking

### Phase 5: Flow Control

**Client:**
- [ ] `hub:pause` handler (stop sending)
- [ ] `hub:resume` handler (resume sending)
- [ ] `hub:queue_stats` query
- [ ] Pause/resume state tracking

**Server:**
- [ ] Queue depth monitoring
- [ ] Backpressure thresholds (pause: 1000, resume: 500)
- [ ] `hub:pause` / `hub:resume` emission
- [ ] Queue statistics calculation

### Phase 6: Error Handling

**Client:**
- [ ] Error type handlers for all error types
- [ ] Retry strategy based on `retryable` flag
- [ ] Correlation ID tracking
- [ ] Distributed tracing integration

**Server:**
- [ ] Error message generation with correlation
- [ ] Rate limit error with `retryAfter`
- [ ] Version mismatch detection
- [ ] Message size validation

### Phase 7: Testing

- [ ] Unit tests for connection lifecycle
- [ ] Reconnection scenario tests (Priority 1)
- [ ] Message delivery guarantee tests (Priority 2)
- [ ] Broadcast and backpressure tests (Priority 3)
- [ ] Registration consistency tests (Priority 4)
- [ ] End-to-end integration tests (Priority 5)
- [ ] Load tests (10K actors)

**See also:** `docs/signal-hub/TESTING.md` for complete testing strategy

### Phase 8: Production Readiness

**Monitoring:**
- [ ] Metrics collection (actors, messages, broadcasts)
- [ ] Prometheus metrics export
- [ ] Alerting thresholds (capacity, latency, errors)
- [ ] Dashboard with key metrics

**Operations:**
- [ ] Deployment automation
- [ ] Health check endpoint
- [ ] Graceful shutdown handling
- [ ] Shard management (beyond 50K actors)

**Documentation:**
- [ ] API documentation
- [ ] Deployment guide
- [ ] Troubleshooting guide
- [ ] Performance tuning guide

---

## Related Documentation

**Design Phases (Detailed):**
- `docs/signal-hub/SHARED_MESSAGE_INTEGRATION.md` - Wire protocol design
- `docs/signal-hub/MESSAGE_TYPES.md` - Complete message catalog
- `docs/signal-hub/SECURITY.md` - Authentication and authorization
- `docs/signal-hub/CONNECTION_LIFECYCLE.md` - State machine and timeouts
- `docs/signal-hub/DELIVERY_GUARANTEES.md` - At-most-once and at-least-once
- `docs/signal-hub/SCALABILITY.md` - Broadcast queuing and sharding
- `docs/signal-hub/TESTING.md` - Testing strategy with priorities

**Planning:**
- `docs/signal-hub/PROTOCOL_DESIGN_PLAN_V2.md` - 8-phase implementation plan

**Source Code:**
- `/packages/protocols/src/shared-message.ts` - SharedMessage implementation
- `/packages/protocols/schema/domain.schema.json` - Type definitions

---

**Protocol Version:** 0.1.0
**Document Status:** Complete
**Ready for Implementation:** Yes
