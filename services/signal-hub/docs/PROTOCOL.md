# Signal Hub WebSocket Protocol

**Version:** 0.1.0
**Date:** 2026-02-18
**Status:** Authoritative Reference

---

## WebSocket Keepalive Architecture

### Cloudflare Native Ping/Pong (Infrastructure Layer)

Signal Hub runs as a Cloudflare Durable Object using the **hibernatable WebSocket API**
(`ctx.acceptWebSocket()`). This is a critical architectural fact:

> Cloudflare manages TCP-level connection health natively. The infrastructure sends
> WebSocket-level ping frames and expects pong frames from the client. This happens
> entirely at the infrastructure layer — application code is not involved and the
> DO does not wake from hibernation to handle ping/pong.

The constructor configures this explicitly:

```typescript
state.setWebSocketAutoResponse(
  new WebSocketRequestResponsePair(
    JSON.stringify({ type: 'ping' }),
    JSON.stringify({ type: 'pong' })
  )
);
```

This line registers an auto-response pair that Cloudflare handles without ever
waking the DO. Dead connections are detected and cleaned up by the infrastructure.

### Application-Level Heartbeat (Session State Verification)

The `hub:heartbeat` / `hub:heartbeat_ack` message exchange is **not** a keepalive
mechanism. Its purpose is:

1. **Session state verification** — confirms the application-level session is
   still healthy (not just the TCP connection).
2. **lastHeartbeat tracking** — any message (not just `hub:heartbeat`) updates
   `session.lastHeartbeat`, so the interval matters only when the connection is
   otherwise idle.
3. **Dead application detection** — identifies clients whose process has hung
   (TCP alive, application unresponsive).

### Why the Interval Is 5 Minutes (Not 30 Seconds)

Early versions used a 25–30 second interval based on a misconception that
heartbeats prevent DO hibernation. They do not — and cannot. The DO hibernates
after ~30s of idle time regardless of heartbeat frequency. State is preserved
across hibernation; the DO auto-wakes when the next message arrives.

Sending heartbeats every 30 seconds:
- Wakes the DO 2× per minute per connection unnecessarily
- Generates network traffic with no functional benefit
- Costs compute time for heartbeat processing in the DO

At 300s (5 min) the application heartbeat:
- Detects truly dead application sessions within a reasonable window
- Does not interfere with DO hibernation
- Reduces unnecessary compute wake-ups significantly

### Configuration

| Variable | Type | Default | Unit | Description |
|---|---|---|---|---|
| `HB_INTERVAL_SECONDS` | env var | `300` | seconds | App heartbeat interval (canonical) |
| `HEARTBEAT_INTERVAL` | env var | `300000` | milliseconds | Legacy alias, kept for backward compatibility |

`HB_INTERVAL_SECONDS` takes precedence when set. Both are surfaced to connecting
clients in the `hub:connected` response payload as `heartbeatInterval` (milliseconds).

To override at deploy time:

```toml
# wrangler.toml
[vars]
HB_INTERVAL_SECONDS = "300"   # 5 minutes (default)
```

Or via environment override for faster session detection in development:

```bash
wrangler dev --var HB_INTERVAL_SECONDS:60
```

### Design Decision Record

| Question | Decision | Rationale |
|---|---|---|
| Who handles TCP keepalive? | Cloudflare infrastructure | Native hibernatable WS API handles ping/pong without waking DO |
| Should app heartbeats prevent hibernation? | No — and they cannot | DO hibernates on idle regardless; waking it for heartbeats wastes compute |
| What is the correct app heartbeat interval? | 300s default, configurable | Long enough to avoid unnecessary wake-ups; short enough to detect dead sessions |
| What is the canonical env var? | `HB_INTERVAL_SECONDS` (seconds) | Human-readable; millisecond vars are error-prone at scale |

---

## Connection Lifecycle

### State Machine

```
connecting → connected → disconnecting → disconnected
     └──────────────────────────────────→ disconnected  (connect failure)
```

All state transitions are validated by `FSMValidator` against the spec in
`spec/connection/CONNECTION.spec.md`.

### Message Flow

```
Client                          Signal Hub (DO)
  |                                  |
  |--- hub:connect ----------------> |  (connecting → connected)
  |<-- hub:connected ---------------|
  |                                  |
  |--- hub:heartbeat (every 5 min) -> |  (optional; any message updates lastHeartbeat)
  |<-- hub:heartbeat_ack ------------|
  |                                  |
  |--- hub:disconnect -------------> |  (connected → disconnecting)
  |<-- hub:disconnect (ack) --------|  (disconnecting → disconnected)
  |=== [WebSocket closed] ==========|
```

### hub:heartbeat

**Direction:** client → server
**Pattern:** `tell` (fire-and-forget; ack returned as courtesy)

**Request payload:**
```json
{ "timestamp": 1708300000000 }
```

**Response (`hub:heartbeat_ack`) payload:**
```json
{ "timestamp": 1708300000000, "serverTime": 1708300000012 }
```

Note: Any message to the hub updates `session.lastHeartbeat`. Clients do not
need to send a dedicated `hub:heartbeat` if they are sending other messages
regularly.

---

## Structured Logging

All hot paths use the `log()` utility from `src/utils.ts`:

```typescript
log(env, 'event_name', { key: 'value' });
```

- Emits structured JSON to `console.log` only when `env.DEBUG === 'true'`
- No-op in production (DEBUG unset), keeping the DO idle between messages
- Key hot paths instrumented: `webSocketMessage`, `routeMessage`, `handleSend`,
  `handleBroadcast`, state transitions, connection lifecycle

Lifecycle events (startup, shutdown) use `console.log` directly so they always
appear in Cloudflare Workers logs regardless of DEBUG flag.

---

## References

- `spec/connection/CONNECTION.spec.md` — full FSM and message spec
- `spec/connection/protocol.json` — machine-readable protocol configuration
- `spec/connection/scenarios/hibernation-wake.md` — hibernation scenario
- `HEARTBEAT_ANALYSIS.md` — historical analysis of interval decision
- `src/durable-objects/SignalHub.ts` — DO implementation
- `src/handlers/connection.ts` — connect/heartbeat/disconnect handlers
