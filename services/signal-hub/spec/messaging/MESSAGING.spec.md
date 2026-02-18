# Domain: Messaging

Point-to-point and broadcast message delivery for Signal Hub, enabling direct communication between registered actors.

## Overview

The messaging domain handles:
- Point-to-point message delivery (hub:send)
- Broadcast to all connected actors (hub:broadcast)
- Delivery acknowledgments (hub:delivery_ack)
- Flat payload structure for application messages
- Message size limits and validation
- Unknown actor error handling

## Machine-Readable Specifications

- **Protocol:** [`protocol.json`](./protocol.json) - AsyncAPI-style message definitions
- **Shared Schemas:**
  - [`../schemas/shared-message.schema.json`](../schemas/shared-message.schema.json) - Message envelope
  - [`../schemas/canonical-address.schema.json`](../schemas/canonical-address.schema.json) - Actor address format
  - [`../schemas/error-response.schema.json`](../schemas/error-response.schema.json) - Error format
- **Scenarios:** [`scenarios/`](./scenarios/) - Example flows and error paths

## Message Types

See [`protocol.json`](./protocol.json) for detailed message schemas.

### Point-to-Point Delivery (hub:send)

```
Client A                    Server                    Client B
  |                           |                           |
  |-- hub:send -------------->|                           |
  |   {to, type, data}        |                           |
  |                           | [Lookup target actor]     |
  |                           | [Validate registration]   |
  |                           |-- Forwarded Message ----->|
  |                           |   {type, data, from: A}   |
  |<-- hub:delivery_ack ------|                           |
  |   {messageId, delivered}  |                           |
```

**CRITICAL: Flat Payload Structure**

Input from Client A:
```typescript
{
  type: "hub:send",
  to: "@(cloudflare/signal-hub)",  // Hub address (ignored, target in payload)
  payload: {
    to: "@(browser/widget-123)",   // Actual target address
    type: "task:assign",            // Application message type
    data: { taskId: "456" }         // Application data (FLAT, not nested!)
  }
}
```

Forwarded to Client B:
```typescript
{
  type: "task:assign",              // From payload.type
  from: "@(seag/agent-1)",          // Original sender
  to: "@(browser/widget-123)",      // Target
  payload: { taskId: "456" },       // From payload.data (FLAT!)
  metadata: {
    forwarded: true,
    via: "@(cloudflare/signal-hub)",
    originalMessageId: "msg_abc123"
  }
}
```

**Response: hub:delivery_ack**
```typescript
{
  type: "hub:delivery_ack",
  from: "@(cloudflare/signal-hub)",
  to: "@(seag/agent-1)",
  payload: {
    messageId: "msg_abc123",
    delivered: true,
    timestamp: 1708272000000
  }
}
```

### Broadcast Delivery

```
Client A                    Server                    All Clients
  |                           |                           |
  |-- hub:broadcast --------->|                           |
  |   {type, data}            |                           |
  |                           | [Get all connections]     |
  |                           |-- Forwarded Message ----->|
  |                           |   {type, data, from: A}   |
  |<-- hub:delivery_ack ------|                           |
  |   {delivered, count: 5}   |                           |
```

**Message: hub:broadcast**
```typescript
{
  type: "hub:broadcast",
  payload: {
    type: "system:notification",    // Application type
    data: {                          // Application data
      message: "Server restart in 5 minutes",
      severity: "warning"
    }
  }
}
```

**Forwarded Message:**
```typescript
{
  type: "system:notification",      // From payload.type
  from: "@(seag/admin)",            // Original sender
  to: "@(broadcast)",               // Special broadcast address
  payload: {                        // From payload.data
    message: "Server restart in 5 minutes",
    severity: "warning"
  },
  metadata: {
    forwarded: true,
    via: "@(cloudflare/signal-hub)",
    broadcast: true
  }
}
```

## Message Size Limits

**Configuration:**
- Maximum message size: 1 MB (1,048,576 bytes)
- Checked BEFORE JSON.parse (prevents DoS)
- Applied to entire SharedMessage envelope

**Validation:**
```typescript
// In SignalHub.webSocketMessage()
const messageSize = new TextEncoder().encode(message).length;
if (messageSize > MAX_MESSAGE_SIZE) {
  throw new HubError('message_too_large',
    `Message size ${messageSize} exceeds limit ${MAX_MESSAGE_SIZE}`);
}
```

**Error Response:**
```json
{
  "type": "hub:error",
  "payload": {
    "code": "message_too_large",
    "message": "Message size 2000000 exceeds limit 1048576",
    "details": {
      "size": 2000000,
      "limit": 1048576
    }
  }
}
```

## Flat Payload Structure

**Why Flat?**
- Simpler for clients (no double-nesting)
- Easier to validate (single payload object)
- Clearer separation: envelope vs. application data

**Pattern:**
```typescript
// Client sends:
hub:send {
  payload: {
    to: <target>,
    type: <app-type>,    // Application message type
    data: <app-data>     // Application payload
  }
}

// Hub forwards:
<app-type> {
  from: <sender>,
  to: <target>,
  payload: <app-data>    // FLAT! Not { data: <app-data> }
}
```

**Anti-Pattern (WRONG):**
```typescript
// DON'T DO THIS:
<app-type> {
  payload: {
    data: <app-data>     // ❌ Nested data field
  }
}
```

## Error Scenarios

### Unknown Actor

**Scenario:** Client sends to actor that is not registered

```json
{
  "type": "hub:unknown_actor",
  "payload": {
    "actorAddress": "@(browser/widget-999)",
    "message": "Actor not registered with Signal Hub"
  }
}
```

**Behavior:**
- **ask pattern:** Returns `hub:unknown_actor` error
- **tell pattern:** Silently drops message (no error sent)

### Expired Registration

**Scenario:** Target actor's registration TTL expired

```json
{
  "type": "hub:unknown_actor",
  "payload": {
    "actorAddress": "@(browser/widget-123)",
    "message": "Actor registration expired"
  }
}
```

**Server Action:**
1. Check if `targetActor.expiresAt < Date.now()`
2. Remove from registry (`registry.delete(actorAddress)`)
3. Return error (ask pattern) or drop (tell pattern)

### Connection Not Found

**Scenario:** Actor is registered but WebSocket connection lost

```json
{
  "type": "hub:error",
  "payload": {
    "code": "internal_error",
    "message": "Target actor connection not found",
    "retryable": false
  }
}
```

**Cause:** Actor registration exists but `connections.get(connectionId)` returns null

## Delivery Guarantees

**Point-to-Point (hub:send):**
- At-most-once delivery
- No persistence or queuing
- If target offline → error or silent drop (based on pattern)
- Delivery ack confirms message sent to WebSocket (not received by client)

**Broadcast (hub:broadcast):**
- Best-effort delivery to all connected actors
- No guarantee all actors receive (some may be disconnected)
- Delivery ack includes count of actors message was sent to

**Not Guaranteed:**
- Message ordering (no sequence numbers)
- Message persistence (in-memory only)
- Delivery after reconnection (no store-and-forward)

## Message Patterns

### Ask Pattern (Request-Response)

```typescript
{
  type: "hub:send",
  pattern: "ask",  // Request acknowledgment
  payload: { ... }
}
```

**Behavior:**
- Hub sends `hub:delivery_ack` or error response
- Client can correlate via `correlationId`

### Tell Pattern (Fire-and-Forget)

```typescript
{
  type: "hub:send",
  pattern: "tell",  // No acknowledgment
  payload: { ... }
}
```

**Behavior:**
- No acknowledgment sent
- Errors silently dropped
- Lower overhead

## Cross-References

- **Shared Schemas:** [`../schemas/`](../schemas/) - Common type definitions
- **Registration Domain:** [`../registration/`](../registration/) - Actor registry lookup
- **Connection Domain:** [`../connection/`](../connection/) - WebSocket connections map
- **Original Spec:** `../../docs/PROTOCOL.spec.md` (Section 3.3 - Messaging)

## Implementation Notes

**Actor Lookup:**
- Target address in `payload.to` (NOT `msg.to`)
- Registry lookup: `registry.get(targetAddress)`
- Check expiration: `isExpired(targetActor.expiresAt)`

**Message Forwarding:**
- Preserves original `from` address (sender identity)
- Sets `to` to target address
- Extracts `type` from `payload.type`
- Extracts `data` from `payload.data`
- Adds metadata: `forwarded: true`, `via: <hub-address>`

**WebSocket Delivery:**
- Lookup: `connections.get(targetActor.connectionId)`
- Send: `ws.send(JSON.stringify(forwardedMessage))`
- No retry on failure (at-most-once)

**Broadcast:**
- Iterates all `connections.values()`
- Sends to every connected WebSocket
- Counts successful sends for delivery ack
- Skips sender (optional, based on implementation)
