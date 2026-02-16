# Signal Hub Message Types (Formal Specification)

**Status:** Design Phase
**Last Updated:** 2026-02-16
**Protocol Version:** 0.1.0

---

## Overview

All Signal Hub messages use `SharedMessage` wire format from `@agentic-primer/protocols`.

Message types are identified by the `type` field using the `hub:*` namespace:
- Connection: `hub:connect`, `hub:connected`, `hub:heartbeat`, `hub:disconnect`
- Discovery: `hub:register`, `hub:discover`, `hub:list_actors`
- Delivery: `hub:send`, `hub:broadcast`, `hub:subscribe`, `hub:publish`
- Flow Control: `hub:pause`, `hub:resume`
- Errors: `hub:error`, `hub:unknown_actor`, `hub:unauthorized`, `hub:rate_limited`

---

## Type Definitions

### Base Types

```typescript
import type { SharedMessage, CanonicalAddress } from '@agentic-primer/protocols';

// All Signal Hub messages extend SharedMessage
type HubMessage = SharedMessage & {
  type: `hub:${string}`;
};

// Actor registration record
interface ActorRegistration {
  actorAddress: CanonicalAddress;
  capabilities: string[];
  metadata: Record<string, unknown>;
  connectionId: string;
  registeredAt: number;  // epoch ms
  expiresAt: number;     // epoch ms
  version: number;       // for conflict resolution
}

// Error codes
type HubErrorCode =
  | 'version_mismatch'
  | 'unauthorized'
  | 'rate_limited'
  | 'unknown_actor'
  | 'message_too_large'
  | 'message_expired'
  | 'timeout'
  | 'internal_error';
```

---

## 1. Connection Lifecycle

### 1.1 hub:connect

**Direction:** Client → Server
**Pattern:** `ask` (expects response)
**Purpose:** Establish Signal Hub session with authentication and version negotiation

```typescript
interface HubConnectMessage extends HubMessage {
  type: 'hub:connect';
  pattern: 'ask';
  payload: null;
  metadata: {
    protocolVersion: string;      // Semantic version (e.g., "0.1.0")
    authToken?: string;            // JWT bearer token (REQUIRED in production)
    capabilities: string[];        // Client capabilities (e.g., ["send", "broadcast"])
    clientMetadata?: {             // Optional client info
      userAgent?: string;
      platform?: string;
      origin?: string;
    };
  };
  ttl: 5000;  // 5s timeout for connect
}
```

**Response:** `hub:connected` or `hub:error`

**Validation:**
- `protocolVersion` MUST be semver string
- `authToken` MUST be valid JWT (when auth enabled)
- `capabilities` MUST be non-empty array

**Example:**

```typescript
const connectMsg: SharedMessage = {
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
    authToken: 'bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...',
    capabilities: ['send', 'broadcast', 'subscribe']
  },
  ttl: 5000,
  signature: null
};
```

---

### 1.2 hub:connected

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:connect)
**Purpose:** Confirm connection established with server capabilities

```typescript
interface HubConnectedMessage extends HubMessage {
  type: 'hub:connected';
  pattern: 'tell';
  correlationId: string;  // Links to hub:connect request
  payload: {
    sessionId: string;            // Unique session identifier
    serverVersion: string;        // Server protocol version
    maxMessageSize: number;       // Max payload size in bytes
    heartbeatInterval: number;    // Recommended heartbeat interval (ms)
    capabilities: {
      maxActorsPerInstance: number;
      supportsBackpressure: boolean;
      supportedContentTypes: string[];
    };
  };
  metadata: {
    actorIdentity?: CanonicalAddress;  // Server-verified identity (from JWT)
  };
}
```

**Example:**

```typescript
const connectedMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:connected',
  pattern: 'tell',
  correlationId: connectMsg.id,
  timestamp: Date.now(),
  payload: {
    sessionId: 'sess-abc123',
    serverVersion: '0.1.0',
    maxMessageSize: 1048576,  // 1MB
    heartbeatInterval: 25000, // 25s
    capabilities: {
      maxActorsPerInstance: 50000,
      supportsBackpressure: true,
      supportedContentTypes: ['json', 'msgpack']
    }
  },
  metadata: {
    actorIdentity: '@(verified/user-123-browser)'
  },
  ttl: null,
  signature: null
};
```

---

### 1.3 hub:heartbeat

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)
**Purpose:** Keep WebSocket alive, prevent Cloudflare hibernation

```typescript
interface HubHeartbeatMessage extends HubMessage {
  type: 'hub:heartbeat';
  pattern: 'tell';
  payload: {
    timestamp: number;  // Client timestamp (epoch ms)
  };
}
```

**Response:** `hub:heartbeat_ack`

**Timing:**
- Client MUST send every 25s (< 30s Cloudflare hibernation threshold)
- Server responds with `hub:heartbeat_ack` within 1s
- If no ack within 10s, client considers connection dead

**Example:**

```typescript
const heartbeatMsg: SharedMessage = {
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
};
```

---

### 1.4 hub:heartbeat_ack

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:heartbeat)
**Purpose:** Confirm server is alive

```typescript
interface HubHeartbeatAckMessage extends HubMessage {
  type: 'hub:heartbeat_ack';
  pattern: 'tell';
  payload: {
    timestamp: number;       // Client timestamp (echoed)
    serverTime: number;      // Server timestamp (for clock skew detection)
  };
}
```

---

### 1.5 hub:disconnect

**Direction:** Client → Server OR Server → Client
**Pattern:** `tell` (fire-and-forget)
**Purpose:** Graceful connection shutdown

```typescript
interface HubDisconnectMessage extends HubMessage {
  type: 'hub:disconnect';
  pattern: 'tell';
  payload: {
    reason?: string;  // Optional disconnect reason
  };
}
```

**Common reasons:**
- `client_requested` - User closed page/app
- `duplicate_connection` - Same actor connected twice
- `unauthorized` - Auth token revoked
- `server_shutdown` - Server maintenance

**Example:**

```typescript
// Server closes duplicate connection
const disconnectMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:disconnect',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: { reason: 'duplicate_connection' },
  metadata: {},
  ttl: null,
  signature: null
};
```

---

## 2. Actor Discovery

### 2.1 hub:register

**Direction:** Client → Server
**Pattern:** `ask` (expects hub:registered)
**Purpose:** Register actor with Signal Hub for discovery and message routing

```typescript
interface HubRegisterMessage extends HubMessage {
  type: 'hub:register';
  pattern: 'ask';
  payload: {
    actorAddress: CanonicalAddress;  // Actor's canonical address
    capabilities: string[];           // Actor capabilities (e.g., ["render", "compute"])
    metadata: Record<string, unknown>; // Custom actor metadata
    ttlSeconds?: number;              // Registration TTL (default: 300, max: 3600)
  };
  metadata: {
    renewOnHeartbeat?: boolean;  // Auto-renew registration on heartbeat
  };
  ttl: 5000;  // 5s timeout for registration
}
```

**Response:** `hub:registered` or `hub:error`

**Validation:**
- `actorAddress` MUST be valid CanonicalAddress
- `capabilities` MUST be non-empty array
- `ttlSeconds` MUST be between 60-3600 (1 min - 1 hour)

**Duplicate handling:**
- Last-write-wins with version tracking
- Previous registration replaced
- New `renewalToken` issued

**Example:**

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
    capabilities: ['render', 'handle-click', 'handle-resize'],
    metadata: {
      widgetType: 'chart',
      version: '2.1.0'
    },
    ttlSeconds: 300  // 5 minutes
  },
  metadata: {
    renewOnHeartbeat: true
  },
  ttl: 5000,
  signature: null
};
```

---

### 2.2 hub:registered

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:register)
**Purpose:** Confirm actor registration with renewal token

```typescript
interface HubRegisteredMessage extends HubMessage {
  type: 'hub:registered';
  pattern: 'tell';
  correlationId: string;  // Links to hub:register request
  payload: {
    actorAddress: CanonicalAddress;
    expiresAt: number;        // Epoch ms when registration expires
    renewalToken: string;     // Token for renewing registration
    version: number;          // Registration version (for conflict resolution)
  };
}
```

**Example:**

```typescript
const registeredMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/widget-actor-123)',
  type: 'hub:registered',
  pattern: 'tell',
  correlationId: registerMsg.id,
  timestamp: Date.now(),
  payload: {
    actorAddress: '@(browser/widget-actor-123)',
    expiresAt: Date.now() + 300000,  // 5 min from now
    renewalToken: 'renewal-token-xyz',
    version: 1
  },
  metadata: {},
  ttl: null,
  signature: null
};
```

---

### 2.3 hub:unregister

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)
**Purpose:** Remove actor registration

```typescript
interface HubUnregisterMessage extends HubMessage {
  type: 'hub:unregister';
  pattern: 'tell';
  payload: {
    actorAddress: CanonicalAddress;
  };
}
```

---

### 2.4 hub:discover

**Direction:** Client → Server
**Pattern:** `ask` (expects hub:discovered)
**Purpose:** Query registered actors by pattern

```typescript
interface HubDiscoverMessage extends HubMessage {
  type: 'hub:discover';
  pattern: 'ask';
  payload: {
    pattern: string;    // Glob pattern (e.g., "@(browser/widget-*)")
    limit?: number;     // Max results (default: 100, max: 100)
  };
  ttl: 5000;
}
```

**Response:** `hub:discovered`

**Pattern matching:**
- Supports glob wildcards: `*` (any characters)
- Example: `@(browser/widget-*)` matches `@(browser/widget-123)`, `@(browser/widget-abc)`
- Example: `@(*/coordinator)` matches any runtime's coordinator

**Example:**

```typescript
const discoverMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(local/coordinator-main)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:discover',
  pattern: 'ask',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    pattern: '@(browser/widget-*)',
    limit: 50
  },
  metadata: {},
  ttl: 5000,
  signature: null
};
```

---

### 2.5 hub:discovered

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:discover)
**Purpose:** Return matching actors

```typescript
interface HubDiscoveredMessage extends HubMessage {
  type: 'hub:discovered';
  pattern: 'tell';
  correlationId: string;
  payload: {
    actors: ActorRegistration[];  // Matching actors
    hasMore: boolean;             // More results available (pagination)
  };
}
```

**Example:**

```typescript
const discoveredMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(local/coordinator-main)',
  type: 'hub:discovered',
  pattern: 'tell',
  correlationId: discoverMsg.id,
  timestamp: Date.now(),
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
  },
  metadata: {},
  ttl: null,
  signature: null
};
```

---

### 2.6 hub:list_actors

**Direction:** Client → Server
**Pattern:** `ask` (expects hub:actor_list)
**Purpose:** Get all registered actors with pagination

```typescript
interface HubListActorsMessage extends HubMessage {
  type: 'hub:list_actors';
  pattern: 'ask';
  payload: {
    offset?: number;  // Pagination offset (default: 0)
    limit?: number;   // Results per page (default: 100, max: 100)
  };
  ttl: 5000;
}
```

**Response:** `hub:actor_list`

**Constraints:**
- Max limit: 100 (prevents CPU timeout on large registries)
- Use pagination for >100 actors

---

### 2.7 hub:actor_list

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:list_actors)
**Purpose:** Return paginated actor list

```typescript
interface HubActorListMessage extends HubMessage {
  type: 'hub:actor_list';
  pattern: 'tell';
  correlationId: string;
  payload: {
    actors: ActorRegistration[];
    total: number;      // Total registered actors
    offset: number;     // Current offset
    limit: number;      // Results per page
    hasMore: boolean;   // More results available
  };
}
```

---

### 2.8 hub:renew

**Direction:** Client → Server
**Pattern:** `ask` (expects hub:renewed)
**Purpose:** Renew actor registration before expiration

```typescript
interface HubRenewMessage extends HubMessage {
  type: 'hub:renew';
  pattern: 'ask';
  payload: {
    renewalToken: string;  // Token from hub:registered
  };
  ttl: 5000;
}
```

**Response:** `hub:renewed` or `hub:error`

---

### 2.9 hub:renewed

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:renew)
**Purpose:** Confirm registration renewed

```typescript
interface HubRenewedMessage extends HubMessage {
  type: 'hub:renewed';
  pattern: 'tell';
  correlationId: string;
  payload: {
    expiresAt: number;       // New expiration time
    renewalToken: string;    // New renewal token
  };
}
```

---

## 3. Message Delivery

### 3.1 hub:send

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget) OR `ask` (with ack)
**Purpose:** Send point-to-point message to specific actor

```typescript
interface HubSendMessage extends HubMessage {
  type: 'hub:send';
  pattern: 'tell' | 'ask';  // 'ask' requires hub:delivery_ack
  payload: {
    targetAddress: CanonicalAddress;  // Recipient actor
    message: {
      type: string;           // Application-level message type
      payload: unknown;       // Application-level payload
    };
  };
  metadata: {
    requireAck?: boolean;     // Require delivery ack (forces pattern='ask')
    traceId?: string;         // Distributed tracing
    priority?: number;        // Message priority (0=high, 2=low)
  };
  ttl: 30000;  // 30s delivery timeout
}
```

**Response:** `hub:delivery_ack` (if pattern='ask') or `hub:error`

**Constraints:**
- Max message size: 1MB (Cloudflare WebSocket frame limit)
- `targetAddress` MUST be registered actor
- If actor offline, queued for delivery (up to TTL)

**Example:**

```typescript
const sendMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(local/coordinator-main)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:send',
  pattern: 'ask',  // Require ack
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    targetAddress: '@(browser/widget-123)',
    message: {
      type: 'task:assign',
      payload: { taskId: 'task-456', priority: 1 }
    }
  },
  metadata: {
    requireAck: true,
    traceId: 'trace-xyz'
  },
  ttl: 30000,
  signature: null
};
```

---

### 3.2 hub:delivery_ack

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:send with pattern='ask')
**Purpose:** Confirm message delivered to target actor

```typescript
interface HubDeliveryAckMessage extends HubMessage {
  type: 'hub:delivery_ack';
  pattern: 'tell';
  correlationId: string;
  payload: {
    messageId: string;        // Original message ID
    deliveredAt: number;      // Epoch ms when delivered
    status: 'delivered' | 'queued';  // Delivery status
  };
}
```

---

### 3.3 hub:broadcast

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)
**Purpose:** Broadcast message to all registered actors

```typescript
interface HubBroadcastMessage extends HubMessage {
  type: 'hub:broadcast';
  pattern: 'tell';
  payload: {
    message: {
      type: string;
      payload: unknown;
    };
    excludeSelf?: boolean;  // Exclude sender from broadcast
  };
  metadata: {
    targetCapability?: string;  // Only actors with this capability
  };
}
```

**Response:** `hub:broadcast_ack`

**Constraints:**
- Max throughput: ~1K actors/sec per instance
- >100 actors → async queue (Cloudflare Queues)
- <100 actors → synchronous fan-out

**Example:**

```typescript
const broadcastMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(local/coordinator-main)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:broadcast',
  pattern: 'tell',
  correlationId: null,
  timestamp: Date.now(),
  payload: {
    message: {
      type: 'system:shutdown',
      payload: { reason: 'maintenance', shutdownAt: Date.now() + 60000 }
    },
    excludeSelf: true
  },
  metadata: {
    targetCapability: 'compute'  // Only actors with 'compute' capability
  },
  ttl: null,
  signature: null
};
```

---

### 3.4 hub:broadcast_ack

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:broadcast)
**Purpose:** Confirm broadcast initiated

```typescript
interface HubBroadcastAckMessage extends HubMessage {
  type: 'hub:broadcast_ack';
  pattern: 'tell';
  payload: {
    messageId: string;          // Original broadcast ID
    deliveredCount: number;     // Actors reached immediately
    queuedCount?: number;       // Actors queued for async delivery
    failedCount?: number;       // Failed deliveries
  };
}
```

---

### 3.5 hub:subscribe

**Direction:** Client → Server
**Pattern:** `ask` (expects hub:subscribed)
**Purpose:** Subscribe to topic for pub/sub messaging

```typescript
interface HubSubscribeMessage extends HubMessage {
  type: 'hub:subscribe';
  pattern: 'ask';
  payload: {
    topic: string;          // Topic name (e.g., "events", "logs")
    durable?: boolean;      // Persist subscription across reconnects
  };
  ttl: 5000;
}
```

**Response:** `hub:subscribed` or `hub:error`

---

### 3.6 hub:subscribed

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:subscribe)
**Purpose:** Confirm topic subscription

```typescript
interface HubSubscribedMessage extends HubMessage {
  type: 'hub:subscribed';
  pattern: 'tell';
  correlationId: string;
  payload: {
    topic: string;
    subscriptionId: string;  // Unique subscription ID
  };
}
```

---

### 3.7 hub:publish

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)
**Purpose:** Publish message to topic subscribers

```typescript
interface HubPublishMessage extends HubMessage {
  type: 'hub:publish';
  pattern: 'tell';
  payload: {
    topic: string;
    message: {
      type: string;
      payload: unknown;
    };
  };
}
```

**Response:** `hub:published`

---

### 3.8 hub:published

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:publish)
**Purpose:** Confirm message published

```typescript
interface HubPublishedMessage extends HubMessage {
  type: 'hub:published';
  pattern: 'tell';
  payload: {
    topic: string;
    subscriberCount: number;  // How many subscribers received message
  };
}
```

---

### 3.9 hub:unsubscribe

**Direction:** Client → Server
**Pattern:** `tell` (fire-and-forget)
**Purpose:** Unsubscribe from topic

```typescript
interface HubUnsubscribeMessage extends HubMessage {
  type: 'hub:unsubscribe';
  pattern: 'tell';
  payload: {
    subscriptionId: string;  // From hub:subscribed
  };
}
```

---

## 4. Flow Control (Backpressure)

### 4.1 hub:pause

**Direction:** Server → Client
**Pattern:** `tell` (command)
**Purpose:** Instruct client to pause sending messages (queue full)

```typescript
interface HubPauseMessage extends HubMessage {
  type: 'hub:pause';
  pattern: 'tell';
  payload: {
    reason: string;  // Why paused (e.g., "outbound_queue_full")
  };
}
```

**Client behavior:**
- MUST stop sending messages immediately
- Wait for `hub:resume` before sending again
- Can still receive messages

---

### 4.2 hub:resume

**Direction:** Server → Client
**Pattern:** `tell` (command)
**Purpose:** Instruct client to resume sending messages

```typescript
interface HubResumeMessage extends HubMessage {
  type: 'hub:resume';
  pattern: 'tell';
  payload: {};
}
```

---

### 4.3 hub:queue_stats

**Direction:** Client → Server
**Pattern:** `ask` (expects hub:queue_stats_response)
**Purpose:** Query server queue depth and processing rate

```typescript
interface HubQueueStatsMessage extends HubMessage {
  type: 'hub:queue_stats';
  pattern: 'ask';
  payload: {};
  ttl: 5000;
}
```

**Response:** `hub:queue_stats_response`

---

### 4.4 hub:queue_stats_response

**Direction:** Server → Client
**Pattern:** `tell` (response to hub:queue_stats)
**Purpose:** Return current queue metrics

```typescript
interface HubQueueStatsResponseMessage extends HubMessage {
  type: 'hub:queue_stats_response';
  pattern: 'tell';
  correlationId: string;
  payload: {
    queueDepth: number;        // Messages in queue
    processingRate: number;    // Messages/sec throughput
    pauseThreshold: number;    // Pause when queue > this
    resumeThreshold: number;   // Resume when queue < this
  };
}
```

---

## 5. Error Handling

### 5.1 hub:error

**Direction:** Server → Client
**Pattern:** `tell` (error response)
**Purpose:** Generic error notification

```typescript
interface HubErrorMessage extends HubMessage {
  type: 'hub:error';
  pattern: 'tell';
  correlationId: string | null;  // Links to failed request (if applicable)
  payload: {
    code: HubErrorCode;
    message: string;              // Human-readable error
    details?: unknown;            // Additional error context
    retryable: boolean;           // Can client retry?
  };
}
```

**Example:**

```typescript
const errorMsg: SharedMessage = {
  id: crypto.randomUUID(),
  from: '@(cloudflare/signal-hub)',
  to: '@(browser/client-ui)',
  type: 'hub:error',
  pattern: 'tell',
  correlationId: failedRequestId,
  timestamp: Date.now(),
  payload: {
    code: 'timeout',
    message: 'Message delivery timed out after 30s',
    details: { targetAddress: '@(offline/actor-456)' },
    retryable: true
  },
  metadata: {},
  ttl: null,
  signature: null
};
```

---

### 5.2 hub:unknown_actor

**Direction:** Server → Client
**Pattern:** `tell` (error response)
**Purpose:** Target actor not registered

```typescript
interface HubUnknownActorMessage extends HubMessage {
  type: 'hub:unknown_actor';
  pattern: 'tell';
  correlationId: string;
  payload: {
    actorAddress: CanonicalAddress;
    message: string;  // e.g., "Actor not registered with Signal Hub"
  };
}
```

---

### 5.3 hub:unauthorized

**Direction:** Server → Client
**Pattern:** `tell` (error response)
**Purpose:** Permission denied

```typescript
interface HubUnauthorizedMessage extends HubMessage {
  type: 'hub:unauthorized';
  pattern: 'tell';
  correlationId: string | null;
  payload: {
    action: string;   // e.g., "register", "send", "broadcast"
    reason: string;   // e.g., "Invalid JWT", "Expired token"
  };
}
```

---

### 5.4 hub:rate_limited

**Direction:** Server → Client
**Pattern:** `tell` (error response)
**Purpose:** Too many requests, retry after delay

```typescript
interface HubRateLimitedMessage extends HubMessage {
  type: 'hub:rate_limited';
  pattern: 'tell';
  correlationId: string;
  payload: {
    retryAfter: number;  // Milliseconds to wait before retry
  };
}
```

---

### 5.5 hub:version_mismatch

**Direction:** Server → Client
**Pattern:** `tell` (error response to hub:connect)
**Purpose:** Client and server protocol versions incompatible

```typescript
interface HubVersionMismatchMessage extends HubMessage {
  type: 'hub:version_mismatch';
  pattern: 'tell';
  correlationId: string;
  payload: {
    clientVersion: string;        // Client's protocol version
    serverVersion: string;        // Server's protocol version
    supportedVersions: string[];  // Server's supported versions
    message: string;              // Upgrade instructions
  };
}
```

---

### 5.6 hub:message_too_large

**Direction:** Server → Client
**Pattern:** `tell` (error response)
**Purpose:** Message exceeds size limit

```typescript
interface HubMessageTooLargeMessage extends HubMessage {
  type: 'hub:message_too_large';
  pattern: 'tell';
  correlationId: string;
  payload: {
    messageSize: number;  // Actual size in bytes
    maxSize: number;      // Max allowed size (1048576 = 1MB)
  };
}
```

---

## Summary

**Total message types:** 24

| Category | Count | Types |
|----------|-------|-------|
| Connection | 5 | connect, connected, heartbeat, heartbeat_ack, disconnect |
| Discovery | 9 | register, registered, unregister, discover, discovered, list_actors, actor_list, renew, renewed |
| Delivery | 8 | send, delivery_ack, broadcast, broadcast_ack, subscribe, subscribed, publish, published, unsubscribe |
| Flow Control | 4 | pause, resume, queue_stats, queue_stats_response |
| Errors | 6 | error, unknown_actor, unauthorized, rate_limited, version_mismatch, message_too_large |

**All types use `SharedMessage` wire format** - no parallel format created.

---

## Next Steps

1. ✅ Message types formally defined
2. ⏳ Security model (auth, authorization, signatures)
3. ⏳ Connection lifecycle state machine
4. ⏳ Delivery guarantees implementation
5. ⏳ Scalability (broadcast queue, sharding)

**Status:** Phase 2 complete, ready for Phase 3 (Security)
