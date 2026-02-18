# Domain: Pub/Sub

Topic-based publish-subscribe messaging for Signal Hub, enabling many-to-many communication patterns.

## Overview

The pub/sub domain handles:
- Topic subscription (hub:subscribe)
- Topic publishing (hub:publish)
- Topic unsubscription (hub:unsubscribe)
- Topic-based message routing
- Flat payload structure for published messages
- Automatic cleanup on disconnect

## Machine-Readable Specifications

- **Protocol:** [`protocol.json`](./protocol.json) - AsyncAPI-style message definitions
- **Schemas:**
  - [`schemas/topic.schema.json`](./schemas/topic.schema.json) - Topic naming and validation
  - [`schemas/subscription.schema.json`](./schemas/subscription.schema.json) - Subscription records
  - [`../schemas/shared-message.schema.json`](../schemas/shared-message.schema.json) - Message envelope
  - [`../schemas/canonical-address.schema.json`](../schemas/canonical-address.schema.json) - Actor address format
- **Scenarios:** [`scenarios/`](./scenarios/) - Example flows and error paths

## Message Types

See [`protocol.json`](./protocol.json) for detailed message schemas.

### Subscription Flow

```
Client                          Server
  |                               |
  |-- hub:subscribe ------------>|
  |   {topic, durable}            |
  |                               | [Get/create topic Set]
  |                               | [Add subscriber]
  |<-- hub:subscribed ------------|
  |   {topic, subscriptionId}     |
  |                               |
  [Subscribed to topic]       [Actor in subscribers Set]
```

**Message: hub:subscribe**
```typescript
{
  type: "hub:subscribe",
  payload: {
    topic: "system/events",       // Hierarchical topic path
    durable: false                // Optional (future use)
  }
}
```

**Response: hub:subscribed**
```typescript
{
  type: "hub:subscribed",
  from: "@(cloudflare/signal-hub)",
  payload: {
    topic: "system/events",
    subscriptionId: "sub-550e8400-e29b-41d4-a716-446655440000"
  }
}
```

### Publish Flow

```
Publisher                   Server                    Subscribers
  |                           |                           |
  |-- hub:publish ----------->|                           |
  |   {topic, type, data}     |                           |
  |                           | [Get topic subscribers]   |
  |                           | [Forward to each]         |
  |                           |-- Forwarded Message ----->|
  |                           |   {type, data, from: Pub} |
  |<-- hub:delivery_ack ------|                           |
  |   {topic, count: 3}       |                           |
```

**Message: hub:publish**
```typescript
{
  type: "hub:publish",
  payload: {
    topic: "system/events",       // Target topic
    type: "event:created",        // Application message type
    data: {                       // Application data
      eventId: "evt_123",
      action: "user_signup"
    }
  }
}
```

**Forwarded Message to Subscribers:**
```typescript
{
  type: "event:created",          // From payload.type
  from: "@(seag/publisher-1)",    // Original publisher
  to: "@(browser/subscriber-1)",  // Each subscriber
  payload: {                      // From payload.data (FLAT!)
    eventId: "evt_123",
    action: "user_signup"
  },
  metadata: {
    forwarded: true,
    via: "@(cloudflare/signal-hub)",
    topic: "system/events"        // Original topic
  }
}
```

**Response: hub:delivery_ack**
```typescript
{
  type: "hub:delivery_ack",
  from: "@(cloudflare/signal-hub)",
  payload: {
    topic: "system/events",
    subscriberCount: 3,           // Number of subscribers
    delivered: true,
    timestamp: 1708272000000
  }
}
```

### Unsubscribe Flow

```
Client                          Server
  |                               |
  |-- hub:unsubscribe ---------->|
  |   {topic}                     |
  |                               | [Remove from subscribers]
  |                               | [Delete topic if empty]
  |<-- hub:unsubscribed ----------|
  |   {topic, unsubscribedAt}     |
  |                               |
  [Unsubscribed]              [Removed from Set]
```

**Message: hub:unsubscribe**
```typescript
{
  type: "hub:unsubscribe",
  payload: {
    topic: "system/events"
  }
}
```

**Response: hub:unsubscribed**
```typescript
{
  type: "hub:unsubscribed",
  from: "@(cloudflare/signal-hub)",
  payload: {
    topic: "system/events",
    unsubscribedAt: 1708272100000
  }
}
```

## Topic Naming

See [`schemas/topic.schema.json`](./schemas/topic.schema.json) for validation rules.

**Format:** Hierarchical path notation with forward slashes

**Pattern:** `^[a-zA-Z0-9/_-]+$`

**Constraints:**
- Minimum length: 1 character
- Maximum length: 256 characters
- Allowed characters: alphanumeric, `/`, `_`, `-`

**Recommendations:**
- Use lowercase for consistency
- Use forward slashes (`/`) for hierarchy
- Use hyphens (`-`) for multi-word components
- Keep depth to 2-3 levels

**Examples (Good):**
```
system/events
user/notifications
analytics/pageviews
debug/error-logs
```

**Examples (Avoid):**
```
SystemEvents                      // Use lowercase
user_notifications                // Use hyphens, not underscores
analytics.pageviews              // Use slashes, not dots
debug/errors/critical/network/timeout  // Too deep (5 levels)
```

## Topic Matching

**Current (MVP):** Exact match only
- Subscription to `system/events` only receives messages published to `system/events`
- No wildcard support (`system/*` does not work)
- Case-sensitive matching

**Future:** Wildcard support
- Single-level: `system/+/events` (matches `system/user/events`, `system/admin/events`)
- Multi-level: `system/#` (matches all under `system/`)

## Flat Payload Structure

**Pattern (Same as Messaging):**
```typescript
// Client publishes:
hub:publish {
  payload: {
    topic: <topic>,
    type: <app-type>,    // Application message type
    data: <app-data>     // Application data
  }
}

// Hub forwards to subscribers:
<app-type> {
  from: <publisher>,
  to: <subscriber>,
  payload: <app-data>    // FLAT! Not { data: <app-data> }
}
```

**Why Flat?**
- Consistency with messaging domain
- Simpler client code (no double-nesting)
- Easier validation

## Subscription Storage

See [`schemas/subscription.schema.json`](./schemas/subscription.schema.json) for data structure.

**Server-Side Storage:**
```typescript
Map<string, Set<CanonicalAddress>>

// Example:
{
  "system/events": Set([
    "@(browser/client-ui)",
    "@(seag/agent-1)"
  ]),
  "user/notifications": Set([
    "@(browser/client-ui)"
  ])
}
```

**Properties:**
- Topics are Map keys (strings)
- Subscribers are Set values (no duplicates)
- Empty topics (zero subscribers) are removed from Map
- In-memory only (no persistence)

## Cleanup Protocol

### On hub:unsubscribe

1. Get topic subscribers: `subscriptions.get(topic)`
2. Remove actor: `subscribers.delete(actorAddress)`
3. If `subscribers.size === 0`, delete topic: `subscriptions.delete(topic)`
4. Send `hub:unsubscribed` acknowledgment

### On Actor Unregister

1. Server iterates all topics in `subscriptions`
2. For each topic, remove actor from subscribers Set
3. Delete empty topics
4. No message sent (part of unregister cleanup)

### On Connection Close

1. Server looks up actor by connectionId
2. Unregisters actor (triggers cleanup above)
3. All subscriptions removed automatically
4. No message sent (connection already closed)

**Important:** No explicit unsubscribe needed before disconnect. Cleanup happens automatically.

## Subscriber Limits

See [`schemas/topic.schema.json`](./schemas/topic.schema.json) metadata for limits.

**Configuration:**
- Max subscribers per topic: No enforced limit (in-memory constraint)
- Max topics per actor: No enforced limit (cleanup on disconnect)
- Recommended: < 1000 subscribers per topic for performance

**Scalability Notes:**
- Broadcast to N subscribers is O(N) operation
- Large subscriber counts may increase message delivery latency
- No fan-out queuing (synchronous iteration)

## Delivery Guarantees

**Publish (hub:publish):**
- At-most-once delivery to each subscriber
- No persistence or queuing
- If subscriber offline → skipped (no error)
- Delivery ack counts subscribers message was sent to

**Subscribe (hub:subscribe):**
- Idempotent (subscribing twice to same topic is safe)
- No duplicate subscriptions (Set prevents duplicates)
- No message history (only receives messages published AFTER subscription)

**Not Guaranteed:**
- Message ordering (no sequence numbers per topic)
- Message persistence (in-memory only)
- Delivery to all subscribers (some may fail)
- Replay of past messages (no history)

## Error Scenarios

### Invalid Topic Name

**Scenario:** Client subscribes to topic with invalid characters

```json
{
  "type": "hub:error",
  "payload": {
    "code": "internal_error",
    "message": "topic is required"
  }
}
```

**Validation:**
- Topic must be non-empty string
- Must match pattern: `^[a-zA-Z0-9/_-]+$`
- Length: 1-256 characters

### Missing payload.type on Publish

**Scenario:** Client publishes without `payload.type` field

```json
{
  "type": "hub:error",
  "payload": {
    "code": "internal_error",
    "message": "payload.type is required"
  }
}
```

**CRITICAL:** The `payload.type` field specifies the application message type that subscribers receive. Without it, the hub cannot construct the forwarded message.

## Topic Discovery

**Not Supported (MVP):**
- No API to list all topics
- No API to get subscriber count for topic
- No topic metadata (description, owner, etc.)

**Workaround:**
- Use well-known topic names (documented in app)
- Coordinate topic names via out-of-band communication

**Future:**
- `hub:list_topics` - List all active topics
- `hub:topic_info` - Get subscriber count and metadata

## Cross-References

- **Shared Schemas:** [`../schemas/`](../schemas/) - Common type definitions
- **Registration Domain:** [`../registration/`](../registration/) - Actor identity for subscription
- **Messaging Domain:** [`../messaging/`](../messaging/) - Similar flat payload pattern
- **Original Spec:** `../../docs/PROTOCOL.spec.md` (Section 3.4 - Pub/Sub)

## Implementation Notes

**Subscription Management:**
- Storage: `Map<string, Set<CanonicalAddress>>`
- Add subscriber: `subscribers.add(actorAddress)`
- Remove subscriber: `subscribers.delete(actorAddress)`
- Cleanup empty: `if (subscribers.size === 0) subscriptions.delete(topic)`

**Message Forwarding:**
- Lookup subscribers: `subscriptions.get(topic)`
- Iterate subscribers: `for (const subscriber of subscribers) { ... }`
- Forward to each: Same as messaging domain (flat structure)
- Count deliveries: Track successful `ws.send()` calls

**Identity Assertion:**
- Use registered `actorAddress`, NOT `sessionId`
- Log subscription with actor identity for verification
- Example: `{ event: 'subscription_added', actorAddress: '@(browser/ui)', topic: 'events' }`

**Concurrency:**
- Single-threaded Durable Object (no race conditions)
- Subscriptions persist across hibernation (in-memory state preserved)
- No locking needed for Set operations
