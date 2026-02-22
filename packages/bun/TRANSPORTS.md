# WebSocket Transport Compatibility

Covers the transport stack for actor-system communication across Bun, Cloudflare, and browser targets.

## Architecture

Both server-side bridges (Bun and Cloudflare) ARE adapters: they adapt platform-specific WebSocket APIs to the shared actor routing protocol. Shared logic lives in `@agentic-primer/actors/transport/bridge-utils`.

```text
Browser (RemoteTransport)
    ↕ WebSocket
BunWebSocketBridge  OR  WebSocketBridge (Cloudflare DO)
    ↓ system.send()
ActorSystem
```

## Heartbeat Protocol

All bridges MUST implement the HEARTBEAT_PING → HEARTBEAT_PONG contract exactly, or `RemoteTransport` will disconnect.

### Client → Server (PING)

```json
{
  "type": "HEARTBEAT_PING",
  "to": "__system__",
  "from": "__client__",
  "payload": { "timestamp": 1708350000000 },
  "id": "heartbeat-1708350000000"
}
```

### Server → Client (PONG)

```json
{
  "type": "HEARTBEAT_PONG",
  "to": "__client__",
  "from": "__system__",
  "payload": {},
  "id": "heartbeat-1708350000000"
}
```

The `id` **must be echoed verbatim** — `RemoteTransport` uses it for RTT tracking and timeout detection.

Types: `HeartbeatPing`, `HeartbeatPong`, `makeHeartbeatPong()` — exported from `@agentic-primer/actors`.

## Bridge Implementations

| | `BunWebSocketBridge` | `WebSocketBridge` (Cloudflare) |
|---|---|---|
| Package | `packages/bun` | `packages/cloudflare` |
| Heartbeat | ✅ HEARTBEAT_PING/PONG | ✅ HEARTBEAT_PING/PONG |
| Connection tracking | `Set<ServerWebSocket>` | `ctx.getWebSockets()` (hibernation) |
| Upgrade | `server.upgrade()` → `undefined` | `WebSocketPair` → `Response { status: 101 }` |
| Backpressure | Drain queue — queues dropped/congested sends, flushes on `drain` event | n/a |
| Tags/filtering | No | Yes (via URL `?tag=` param) |

## Shared Utilities (`@agentic-primer/actors`)

Extracted from bridge implementations to avoid duplication:

```typescript
import { deserializeWsMessage, makeHeartbeatPong, routeWsActorMessage } from '@agentic-primer/actors';

// In any bridge's handleMessage():
const data = deserializeWsMessage(message, this.serde);
if (data.type === 'HEARTBEAT_PING') {
  ws.send(JSON.stringify(makeHeartbeatPong(data as { id: string })));
  return;
}
routeWsActorMessage(data, this.system);
```

## Bun WebSocket API: Key Differences from Browser

| Feature | Browser | Bun Client (`new WebSocket`) | Bun Server (`ServerWebSocket`) |
|---|---|---|---|
| Constructor | `new WebSocket(url, protocols?)` | Same + `{ headers }` extension | N/A |
| Event model | `addEventListener` per socket | `addEventListener` per socket | Handler object, once per server |
| `binaryType` default | `"blob"` | `"blob"` | `"nodebuffer"` — binary arrives as `Buffer` |
| `binaryType` options | `"blob"`, `"arraybuffer"` | `"blob"`, `"arraybuffer"` | `"nodebuffer"`, `"arraybuffer"`, `"uint8array"` |
| `send()` return | `void` | `void` | `number` (-1=backpressure, 0=dropped) |
| Pub/Sub | None | None | Built-in (`subscribe/publish`) |
| `cork()` | None | None | Batch sends into one syscall |
| `terminate()` | None | Bun ext | Immediate ungraceful close |
| Ping/pong control | None | Bun ext | `.ping()`, `.pong()` |

### Important gotcha: `binaryType` default

Bun's `ServerWebSocket` delivers binary frames as Node.js `Buffer` (default `binaryType = "nodebuffer"`). `Buffer` extends `Uint8Array`, so `deserializeWsMessage(message, serde)` handles this correctly without explicit `binaryType` configuration.

If you need `instanceof ArrayBuffer` checks to work, set `ws.binaryType = "arraybuffer"` inside `open()`.

### Important gotcha: `send()` backpressure

`BunWebSocketBridge.broadcast()` implements a per-connection drain queue. If `send()` returns `0` (dropped) or `-1` (backpressured), subsequent broadcasts for that socket are queued locally and flushed when the `drain` event fires.
