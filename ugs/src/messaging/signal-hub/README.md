# Signal Hub Client for SEAG/UGS

WebSocket client implementation for the formal Signal Hub protocol (see `docs/signal-hub/PROTOCOL.md`).

## Overview

The Signal Hub Client enables SEAG's local actor system to communicate with remote actors through Cloudflare's Signal Hub Durable Object.

**Architecture:**

```text
┌─────────────────────────────────────────────────────────────────┐
│                    SEAG/UGS (Local Runtime)                     │
│                                                                 │
│  ┌────────────────┐         ┌──────────────────────────────┐   │
│  │ Local Actors   │────────>│ SignalHubClientActor         │   │
│  │ @(local/...)   │<────────│ @(bridges/signal-hub-client) │   │
│  └────────────────┘         └──────────────────────────────┘   │
│                                      │                          │
│                                      │ WebSocket (wss://)       │
│                                      v                          │
└──────────────────────────────────────┼──────────────────────────┘
                                       │
                                       v
┌─────────────────────────────────────────────────────────────────┐
│                  Signal Hub (Cloudflare DO)                     │
│                                                                 │
│  ┌──────────────┐         ┌──────────────┐                     │
│  │ Actor Registry│         │ Message Router│                    │
│  │ (In-Memory)   │         │ (hub:send)    │                    │
│  └──────────────┘         └──────────────┘                     │
│                                      │                          │
│                                      │                          │
│                                      v                          │
└──────────────────────────────────────┼──────────────────────────┘
                                       │
                                       v
┌─────────────────────────────────────────────────────────────────┐
│                 Browser Runtime (Chrome MCP)                    │
│                                                                 │
│  ┌────────────────┐                                             │
│  │ Widget Actors  │                                             │
│  │ @(browser/...) │                                             │
│  └────────────────┘                                             │
└─────────────────────────────────────────────────────────────────┘
```

## Features

✅ **Protocol Implementation**
- Full Signal Hub protocol support (v0.1.0)
- Connection lifecycle (hub:connect, hub:connected, hub:disconnect)
- Actor registration (hub:register, hub:renew, hub:unregister)
- Message routing (hub:send with flat payload)
- Heartbeat (hub:heartbeat every 25s)

✅ **Reliability**
- Automatic reconnection with exponential backoff
- Message queuing during disconnect
- Actor re-registration after reconnect
- Connection state tracking

✅ **Integration**
- SEAG actor system integration via `SignalHubClientActor`
- Automatic actor registration for configured prefixes
- Bidirectional message routing
- Event-driven architecture

## Usage

### Standalone Client

```typescript
import { SignalHubClient } from './messaging/signal-hub/client.ts';

const client = new SignalHubClient({
  url: 'wss://signal-hub.example.com',
  jwt: 'your-jwt-token',
  protocolVersion: '0.1.0',
});

// Connect to Signal Hub
await client.connect();

// Register an actor
await client.registerActor(
  '@(local/my-actor)' as CanonicalAddress,
  ['compute', 'inference'],
  { version: '1.0.0' }
);

// Send message
client.send({
  to: '@(browser/widget-123)' as CanonicalAddress,
  type: 'app:message',
  payload: { data: 'hello' },
  from: '@(local/my-actor)' as CanonicalAddress,
});

// Receive messages
client.on('message', (msg: SharedMessage) => {
  console.log('Received:', msg);
});

// Disconnect
await client.disconnect();
```

### SEAG Integration

```typescript
import { SignalHubClientActor } from './system-actors/signal-hub-client-actor.ts';
import { MessageRouter } from './messaging/router.ts';
import { GraphStore } from './graph.ts';

const store = new GraphStore();
const router = new MessageRouter(store);

// Create and register Signal Hub bridge
const signalHubBridge = new SignalHubClientActor(router, {
  url: process.env.SIGNAL_HUB_URL!,
  jwt: process.env.SIGNAL_HUB_JWT!,
  autoConnect: true,
  autoRegisterPrefixes: ['domain/', 'services/'],
  reconnect: {
    enabled: true,
    maxAttempts: 10,
    initialDelay: 1000,
    maxDelay: 30000,
    multiplier: 2,
  },
});

router.registerActor('bridges/signal-hub-client', signalHubBridge);
await signalHubBridge.start();

// Local actors can now send messages to remote actors
const response = await router.ask({
  id: crypto.randomUUID(),
  from: address('domain/inference'),
  to: address('browser/ui-widget'),
  type: 'inference:result',
  payload: { prediction: 0.95, confidence: 0.87 },
  pattern: 'tell',
  timestamp: Date.now(),
});
```

### Configuration

```typescript
interface SignalHubConfig {
  // WebSocket URL (wss://...)
  url: string;

  // JWT authentication token
  jwt: string;

  // Protocol version (default: '0.1.0')
  protocolVersion?: string;

  // Heartbeat interval in ms (default: 25000)
  heartbeatInterval?: number;

  // Reconnection config
  reconnect?: {
    enabled: boolean;          // default: true
    maxAttempts: number;       // default: 10
    initialDelay: number;      // default: 1000ms
    maxDelay: number;          // default: 30000ms
    multiplier: number;        // default: 2 (exponential)
  };

  // Message queue config (during disconnect)
  messageQueue?: {
    enabled: boolean;          // default: true
    maxSize: number;           // default: 1000
    defaultTtl: number;        // default: 60000ms
  };
}
```

## Protocol Messages

### Connection

```typescript
// hub:connect → hub:connected
await client.connect();

// hub:disconnect
await client.disconnect();

// hub:heartbeat (automatic, every 25s)
```

### Actor Registration

```typescript
// hub:register → hub:registered
await client.registerActor(address, capabilities, metadata);

// hub:renew (automatic with heartbeat)

// hub:unregister
await client.unregisterActor(address);
```

### Message Routing

```typescript
// hub:send (fire-and-forget)
client.send({
  to: '@(browser/widget)',
  type: 'app:message',
  payload: { data: 'value' },
});

// hub:send (with delivery ack)
const ack = await client.sendWithAck({
  to: '@(browser/widget)',
  type: 'app:request',
  payload: { query: 'status' },
});
```

### Flat Payload Structure

Messages sent via `hub:send` use a flat payload structure:

```typescript
{
  id: 'uuid',
  from: '@(local/actor)',
  to: '@(cloudflare/signal-hub)',
  type: 'hub:send',
  payload: {
    to: '@(browser/widget)',    // Destination actor
    type: 'app:message',         // Application message type
    data: { ... }                // Application payload
  },
  metadata: {
    via: '@(cloudflare/signal-hub)'
  }
}
```

## Events

```typescript
// Connection events
client.on('connected', (sessionId: string) => { ... });
client.on('disconnected', (reason: string) => { ... });
client.on('reconnecting', (attempt: number) => { ... });

// Message events
client.on('message', (msg: SharedMessage) => { ... });

// Actor registration events
client.on('actorRegistered', (address: CanonicalAddress) => { ... });
client.on('actorUnregistered', (address: CanonicalAddress) => { ... });

// Error events
client.on('error', (error: Error) => { ... });
```

## State Management

```typescript
// Get connection state
const state = client.getState();
// 'disconnected' | 'connecting' | 'connected' | 'reconnecting' | 'error'

// Get session info
const sessionId = client.getSessionId();
const connectionInfo = client.getConnectionInfo();

// Get registered actors
const actors = client.getRegisteredActors();
```

## Testing

Run the test suite:

```bash
bun test src/messaging/signal-hub/__tests__/client.test.ts
```

Tests cover:
- Connection lifecycle (connect, disconnect)
- Actor registration (register, unregister, renew)
- Message routing (send, sendWithAck, receive)
- Heartbeat mechanism
- Message queuing during disconnect
- Reconnection with exponential backoff
- Error handling

## Implementation Notes

### WebSocket Library

- **Node.js**: Uses native `WebSocket` class (requires Node 21+)
- **Bun**: Uses native `WebSocket` implementation
- **Deno**: Uses native `WebSocket` implementation

### Actor Address Format

```typescript
// Local actors (SEAG)
'@(local/actor-name)'

// Signal Hub
'@(cloudflare/signal-hub)'

// Browser actors (Chrome MCP)
'@(browser/widget-name)'
```

### Message Size Limit

- Max message size: 1MB (Cloudflare WebSocket limit)
- Messages exceeding limit will trigger `hub:message_too_large` error

### Heartbeat & Hibernation

- Default heartbeat interval: 25s
- Prevents Cloudflare DO hibernation (30s idle timeout)
- Also renews actor registrations to keep them alive

### Reconnection Strategy

- Exponential backoff: 1s, 2s, 4s, 8s, 16s, 30s (max)
- Re-registers all actors after successful reconnect
- Flushes queued messages after reconnection

## Troubleshooting

### Connection Fails

**Symptom:** `hub:connect` times out or returns `hub:error`

**Causes:**
- Invalid JWT token
- Protocol version mismatch
- Network connectivity issues

**Solutions:**
- Verify JWT token is valid and not expired
- Check protocol version matches server (0.1.0)
- Test WebSocket connectivity with `wscat` or similar tool

### Messages Not Delivered

**Symptom:** `client.send()` doesn't deliver to remote actor

**Causes:**
- Remote actor not registered with Signal Hub
- Message exceeds size limit (1MB)
- Connection dropped during send

**Solutions:**
- Verify remote actor is registered: check Signal Hub logs
- Check message size (use `JSON.stringify(msg).length`)
- Enable message queueing for reliability during disconnects

### Actors Not Re-Registered After Reconnect

**Symptom:** Actors missing after reconnection

**Causes:**
- Registration TTL expired before reconnect completed
- Reconnection disabled in config
- Max reconnect attempts exceeded

**Solutions:**
- Enable reconnection: `reconnect: { enabled: true }`
- Increase max attempts: `maxAttempts: 10`
- Monitor reconnection events to track attempts

### High Memory Usage

**Symptom:** Memory grows over time

**Causes:**
- Message queue growing unbounded
- Event listeners not cleaned up
- Pending asks not timing out

**Solutions:**
- Set message queue size limit: `maxSize: 1000`
- Set message TTL: `defaultTtl: 60000`
- Clean up event listeners when done: `client.off(event, handler)`

## See Also

- [Signal Hub Protocol](../../../docs/signal-hub/PROTOCOL.md) - Full protocol specification
- [Message Types](../../../docs/signal-hub/MESSAGE_TYPES.md) - Complete message catalog
- [SharedMessage](../../../packages/protocols/src/shared-message.ts) - Wire format
