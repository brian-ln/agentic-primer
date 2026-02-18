# Domain: Actor Registration

Actor registration and discovery for Signal Hub, allowing actors to advertise their presence and capabilities for dynamic discovery.

## Overview

The registration domain handles:
- Actor registration with capabilities and metadata
- Registration renewal (TTL-based expiration)
- Actor unregistration (cleanup on disconnect)
- Actor discovery with capability filtering
- Registry listing and pagination
- Renewal token rotation for security

## Machine-Readable Specifications

- **Protocol:** [`protocol.json`](./protocol.json) - AsyncAPI-style message definitions
- **Schemas:**
  - [`schemas/actor-registration.schema.json`](./schemas/actor-registration.schema.json) - Actor registration data structure
  - [`../schemas/canonical-address.schema.json`](../schemas/canonical-address.schema.json) - Actor address format
  - [`../schemas/shared-message.schema.json`](../schemas/shared-message.schema.json) - Message envelope
- **Scenarios:** [`scenarios/`](./scenarios/) - Example flows and error paths

## Message Types

See [`protocol.json`](./protocol.json) for detailed message schemas.

### Registration Flow

```
Client                          Server
  |                               |
  |-- hub:register -------------->|
  |   {actorAddress, caps, meta}  |
  |                               | [Store in registry]
  |                               | [Generate renewal token]
  |<-- hub:registered ------------|
  |   {token, expiresAt, version} |
  |                               |
  [Registered in registry]    [TTL timer started]
```

**Message: hub:register**
```typescript
{
  type: "hub:register",
  payload: {
    actorAddress: "@(local/my-actor)",
    capabilities: ["send", "receive", "ai-agent"],
    metadata: {
      version: "1.0.0",
      userAgent: "SEAG/0.1",
      model: "claude-sonnet-4.5"
    },
    ttl: 300000  // 5 minutes
  }
}
```

**Response: hub:registered**
```typescript
{
  type: "hub:registered",
  payload: {
    actorAddress: "@(local/my-actor)",
    renewalToken: "550e8400-e29b-41d4-a716-446655440000",
    expiresAt: 1708272300000,
    version: 1
  }
}
```

### Renewal Flow

```
Client                          Server
  |                               |
  |-- hub:renew ----------------->|
  |   {address, token, ttl}       |
  |                               | [Validate token]
  |                               | [Extend TTL]
  |                               | [Rotate token]
  |<-- hub:renewed ---------------|
  |   {expiresAt, newToken}       |
  |                               |
  [TTL extended]              [New TTL timer]
```

**Message: hub:renew**
```typescript
{
  type: "hub:renew",
  payload: {
    actorAddress: "@(local/my-actor)",
    renewalToken: "550e8400-e29b-41d4-a716-446655440000",
    ttl: 300000  // Extend by 5 more minutes
  }
}
```

**Response: hub:renewed**
```typescript
{
  type: "hub:renewed",
  payload: {
    actorAddress: "@(local/my-actor)",
    expiresAt: 1708272600000,
    newRenewalToken: "660e8400-e29b-41d4-a716-446655440001"  // Rotated
  }
}
```

### Unregistration Flow

```
Client                          Server
  |                               |
  |-- hub:unregister ------------>|
  |   {actorAddress}              |
  |                               | [Remove from registry]
  |                               | [Cancel TTL timer]
  |<-- hub:unregistered ----------|
  |   {actorAddress, timestamp}   |
  |                               |
  [Unregistered]              [Removed from registry]
```

### Discovery Flow

```
Client                          Server
  |                               |
  |-- hub:discover -------------->|
  |   {capabilities, pattern}     |
  |                               | [Query registry]
  |                               | [Filter by capabilities]
  |<-- hub:discovery_result ------|
  |   {actors[], count, hasMore}  |
  |                               |
```

**Message: hub:discover**
```typescript
{
  type: "hub:discover",
  payload: {
    capabilities: ["ai-agent"],  // Must have ALL these
    pattern: "^@\\(local/.*\\)",  // Regex on actorAddress
    metadata: {
      model: "claude-sonnet-4.5"  // Filter on metadata
    },
    limit: 10,
    offset: 0
  }
}
```

**Response: hub:discovery_result**
```typescript
{
  type: "hub:discovery_result",
  payload: {
    actors: [
      {
        actorAddress: "@(local/agent-1)",
        capabilities: ["ai-agent", "send", "receive"],
        metadata: { model: "claude-sonnet-4.5" },
        registeredAt: 1708272000000
      }
    ],
    count: 1,
    hasMore: false,
    totalMatches: 1
  }
}
```

## TTL and Expiration

**Configuration:**
- Minimum TTL: 1 second (1000ms)
- Default TTL: 5 minutes (300000ms)
- Maximum TTL: 1 hour (3600000ms)

**Behavior:**
- TTL timer starts when hub:registered is sent
- Client MUST renew before expiration to stay registered
- Server removes expired registrations automatically
- No notification sent on TTL expiration (client should track)

**Renewal Strategy (Client-Side):**
```typescript
// Renew at 75% of TTL to avoid expiration
const renewAt = registeredAt + (ttl * 0.75);
setTimeout(() => sendRenew(), renewAt - Date.now());
```

## Renewal Token Rotation

**Security Model:**
- Each hub:registered response includes a unique renewalToken
- Client must include this token in hub:renew requests
- Server rotates token on successful renewal (returns newRenewalToken)
- Client must update stored token after each renewal
- Invalid token → hub:error with code "unauthorized"

**Purpose:**
- Prevents unauthorized TTL extension by third parties
- Ensures only the actor that registered can renew
- Token rotation limits replay attack window

## Registry Limits

See [`protocol.json`](./protocol.json) metadata section for limits:

- Maximum registered actors: 50,000
- Maximum discovery results: 1,000 (pagination required beyond this)
- Discovery default limit: 100 per page

## Error Scenarios

### Duplicate Registration

**Scenario:** Actor tries to register with address that already exists

```json
{
  "type": "hub:error",
  "payload": {
    "code": "unknown_actor",
    "message": "Actor already registered",
    "details": {
      "targetActor": "@(local/my-actor)",
      "existingVersion": 3
    }
  }
}
```

**Client Action:** Check if this is a duplicate connection, consider unregister → register

### Invalid Renewal Token

**Scenario:** Client provides wrong or expired renewal token

```json
{
  "type": "hub:error",
  "payload": {
    "code": "unauthorized",
    "message": "Invalid renewal token",
    "details": {
      "reason": "Invalid renewal token",
      "hint": "Re-register to obtain new token"
    }
  }
}
```

**Client Action:** Re-register from scratch (hub:register)

### Actor Not Found (Renewal)

**Scenario:** Client tries to renew but registration expired/removed

```json
{
  "type": "hub:error",
  "payload": {
    "code": "unknown_actor",
    "message": "Actor not registered",
    "details": {
      "targetActor": "@(local/my-actor)"
    }
  }
}
```

**Client Action:** Re-register (hub:register)

## Discovery Filtering

**Capability Matching:**
- Client specifies array of required capabilities
- Actor must have ALL specified capabilities (AND logic)
- Matching is exact string comparison (no wildcards)

**Pattern Matching:**
- Pattern is ECMAScript regex applied to actorAddress
- Anchors (^, $) recommended for precise matching
- Examples:
  - `^@\\(local/.*\\)` - All local actors
  - `^@\\(browser/.*\\)` - All browser actors
  - `.*-agent$` - Actors ending with "-agent"

**Metadata Filtering:**
- Client provides key-value pairs to match
- Actor must have ALL specified metadata fields with exact values
- Nested object matching not supported (flat comparison only)

## Cleanup Protocol

### On Graceful Disconnect

1. Client sends hub:disconnect
2. Server receives disconnect
3. Server **automatically unregisters** actor (no hub:unregister needed)
4. Server removes from registry
5. Server cancels TTL timer
6. Server sends hub:disconnect_ack

**Important:** Client does NOT need to send hub:unregister before disconnect. Registration cleanup happens automatically.

### On Abnormal Disconnect

1. WebSocket closes unexpectedly
2. Server detects close event
3. Server **automatically unregisters** actor
4. Server removes from registry
5. Server cancels TTL timer

**Important:** No message sent (connection already closed). Cleanup happens silently.

### On TTL Expiration

1. TTL timer fires
2. Server removes actor from registry
3. Actor connection remains open (no forced disconnect)
4. Client can still send/receive messages
5. Client will not appear in discovery results
6. Client must re-register to become discoverable again

**Important:** TTL expiration ONLY affects registry visibility, not connection state.

## Cross-References

- **Shared Schemas:** [`../schemas/`](../schemas/) - Common type definitions
- **Connection Domain:** [`../connection/`](../connection/) - WebSocket lifecycle
- **Original Spec:** `../../docs/PROTOCOL.spec.md` (Section 3.2 - Registration)

## Implementation Notes

**Actor Address Validation:**
- Must match canonical format: `@(scope/name)`
- See [`../schemas/canonical-address.schema.json`](../schemas/canonical-address.schema.json) for regex pattern
- Validation happens before registration is stored

**Registry Storage:**
- In-memory Map keyed by actorAddress
- Durable Object provides persistence across hibernation
- No external database required

**Concurrency:**
- Single-threaded Durable Object = no race conditions
- Registration version field enables conflict detection
- Last-write-wins on duplicate registrations (increment version)
