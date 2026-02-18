# Domain: Connection Lifecycle

Connection lifecycle management for Signal Hub WebSocket connections, from initial handshake through graceful disconnection.

## Overview

The connection domain handles:
- WebSocket establishment and HTTP upgrade
- Protocol version negotiation
- JWT authentication (when enabled)
- Heartbeat keep-alive mechanism
- Graceful disconnection with cleanup
- Connection state tracking

## Machine-Readable Specifications

- **State Machine:** [`state-machine.json`](./state-machine.json) - XState-compatible FSM
- **Protocol:** [`protocol.json`](./protocol.json) - AsyncAPI-style message definitions
- **Scenarios:** [`scenarios/`](./scenarios/) - Example flows and error paths

## Connection States

See [`state-machine.json`](./state-machine.json) for complete state machine definition.

### States Summary

| State | Description | Can Send | Rate Limiting |
|-------|-------------|----------|---------------|
| `connecting` | Awaiting hub:connect | `hub:connect` only | Disabled |
| `connected` | Fully operational | All messages | 100 msg/min |
| `disconnecting` | Cleanup in progress | No new messages | N/A |
| `disconnected` | Terminal state | Nothing (closed) | N/A |

## Message Types

See [`protocol.json`](./protocol.json) for detailed message schemas.

### Connection Handshake

```
Client                          Server
  |                               |
  |-- WebSocket Upgrade -------->|
  |<-- 101 Switching Protocols --|
  |                               |
  |-- hub:connect --------------->|
  |   {version, jwt}              |
  |                               | [Validate JWT]
  |<-- hub:connected -------------|
  |   {sessionId, capabilities}   |
  |                               |
  [State: connected]          [State: connected]
```

### Heartbeat Flow

```
Client                          Server
  |                               |
  |-- hub:heartbeat ------------>|
  |   {timestamp}                 |
  |                               | [Update lastHeartbeat]
  |<-- hub:heartbeat_ack ---------|
  |   {serverTime}                |
  |                               |
```

**Configuration:**
- Client interval: 30 seconds
- Server timeout: 60 seconds
- Purpose: Detect dead connections, NOT prevent hibernation

### Graceful Disconnection

```
Client                          Server
  |                               |
  |-- hub:disconnect ----------->|
  |   {reason}                    |
  |                               | [Unregister actor]
  |                               | [Cleanup subscriptions]
  |<-- hub:disconnect_ack --------|
  |   {sessionId, cleanedUp}      |
  |                               |
  |<-- WebSocket Close -----------|
  [State: disconnected]       [State: disconnected]
```

**Critical:** Server MUST send `hub:disconnect_ack` BEFORE closing WebSocket to ensure client receives acknowledgment.

## Hibernation Behavior

Cloudflare's hibernatable WebSocket API:

- **Automatic Wake:** WebSockets wake automatically on incoming messages
- **No Prevention:** Cannot prevent hibernation
- **State Preservation:** In-memory state persists across hibernation
- **Transparent:** Application code unaware of hibernation

**Heartbeat Purpose:**
- Detect dead connections that can't wake
- Provide round-trip latency measurement
- Keep `lastHeartbeat` timestamp fresh

**NOT for:**
- Preventing hibernation (impossible)
- Keeping connection "active" (automatic)

## Authentication

When `AUTH_ENABLED=true`:

1. Client includes JWT in `hub:connect` payload
2. Server validates JWT signature and expiration
3. Server extracts `actorId` and `capabilities` from JWT claims
4. Session marked as `authenticated=true`
5. All subsequent messages require authentication

### JWT Structure

```json
{
  "sub": "user_123",
  "actorId": "browser/client-ui",
  "capabilities": ["send", "receive", "discover"],
  "iss": "signal-hub",
  "exp": 1708272000
}
```

## Error Scenarios

### Version Mismatch

```json
{
  "type": "hub:error",
  "payload": {
    "code": "version_mismatch",
    "message": "Protocol version 1.0 required, got 0.9",
    "details": {
      "expected": "1.0",
      "received": "0.9"
    }
  }
}
```

**Client Action:** Upgrade to compatible version

### Invalid JWT

```json
{
  "type": "hub:error",
  "payload": {
    "code": "unauthorized",
    "message": "Invalid JWT: signature verification failed",
    "details": {}
  }
}
```

**Client Action:** Re-authenticate and obtain new JWT

### Heartbeat Timeout

Server closes connection after 60s without any messages:

```
WebSocket Close: code=1000, reason="Heartbeat timeout"
```

**Client Action:** Auto-reconnect with exponential backoff

## Cleanup Protocol

### On Graceful Disconnect (hub:disconnect)

1. Transition to `disconnecting` state
2. Unregister actor from registry
3. Remove all topic subscriptions
4. Send `hub:disconnect_ack` **BEFORE** closing WebSocket
5. Close WebSocket (code 1000)
6. Remove session from memory
7. Transition to `disconnected` state

### On Abnormal Disconnect (WebSocket close)

1. Detect close event in `webSocketClose` handler
2. Emergency cleanup (no ack sent)
3. Unregister actor
4. Remove subscriptions
5. Remove session
6. Transition to `disconnected` state

## Scenarios

Detailed scenario documentation in [`scenarios/`](./scenarios/):

- [Initial Connect](./scenarios/initial-connect.md) - Happy path connection flow
- [Reconnect](./scenarios/reconnect.md) - Reconnection after disconnect
- [Hibernation Wake](./scenarios/hibernation-wake.md) - Wake from hibernation
- [Auth Failure](./scenarios/auth-failure.md) - Invalid JWT handling

## Cross-References

- **Shared Schemas:** [`../schemas/`](../schemas/) - Common type definitions
- **Registration Domain:** [`../registration/`](../registration/) - Actor registration
- **Original Spec:** `../../docs/PROTOCOL.spec.md` (Section 3.1), `../../docs/STATE_MACHINE.spec.md`

## Configuration

From environment variables:

```typescript
PROTOCOL_VERSION = "1.0"
HEARTBEAT_INTERVAL = "30000"  // 30 seconds (client)
// Server timeout = 60 seconds (hardcoded in logic)
```
