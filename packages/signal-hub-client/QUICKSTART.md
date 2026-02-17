# Signal Hub Client - Quick Start Guide

Get up and running with Signal Hub browser client in 5 minutes.

## Installation

```bash
npm install @agentic-primer/signal-hub-client @agentic-primer/protocols
```

## Minimal Example

```typescript
import { SignalHubClient } from '@agentic-primer/signal-hub-client';

// 1. Create client
const client = new SignalHubClient({
  url: 'wss://signal-hub.example.com',
  jwt: 'your-jwt-token'
});

// 2. Connect
await client.connect();

// 3. Register actor
const myAddress = await client.registerActor({
  address: '@(browser/my-app)',
  capabilities: ['ui', 'data']
});

// 4. Listen for messages
client.on('message', (event) => {
  console.log('Received:', event.message.type, event.message.payload);
});

// 5. Send a message
await client.send(
  myAddress,                    // from
  '@(local/backend)',           // to
  'app:hello',                  // type
  { greeting: 'Hello World!' }  // data
);
```

## Common Patterns

### Auto-Reconnect

```typescript
const client = new SignalHubClient({
  url: 'wss://hub.example.com',
  jwt: 'token',
  autoReconnect: true,           // Default: true
  maxReconnectAttempts: 10,      // Default: Infinity
  reconnectDelay: 1000,          // Default: 1s
  maxReconnectDelay: 30000       // Default: 30s
});

client.on('stateChange', (state) => {
  console.log('State:', state);  // connecting → connected → reconnecting
});
```

### Request/Response (Ask Pattern)

```typescript
// Send and wait for acknowledgment
const messageId = await client.sendWithAck(
  '@(browser/frontend)',
  '@(local/backend)',
  'app:fetch-data',
  { query: 'users' }
);

console.log('Delivered:', messageId);
```

### Multiple Actors

```typescript
// Register multiple actors in the same client
const widget1 = await client.registerActor({
  capabilities: ['render']
});

const widget2 = await client.registerActor({
  capabilities: ['chart']
});

// Each actor can send/receive independently
await client.send(widget1, widget2, 'data:update', { value: 42 });
```

### Error Handling

```typescript
client.on('error', (event) => {
  console.error('Error:', event.message);
  if (event.code === 'unauthorized') {
    // Handle auth error
  }
});

try {
  await client.connect();
} catch (error) {
  console.error('Connection failed:', error);
}

try {
  await client.sendWithAck(from, to, type, data);
} catch (error) {
  console.error('Message delivery failed:', error);
}
```

### Lifecycle Management

```typescript
// On app load
const client = new SignalHubClient({ url, jwt });
await client.connect();

// On visibility change
document.addEventListener('visibilitychange', () => {
  if (document.hidden) {
    // Optional: disconnect when hidden
    client.disconnect();
  } else {
    // Reconnect when visible
    client.connect();
  }
});

// On page unload
window.addEventListener('beforeunload', () => {
  client.disconnect();
});
```

## Environment Setup

### Development (Local)

```typescript
const client = new SignalHubClient({
  url: 'ws://localhost:8787',  // Local Signal Hub
  jwt: 'dev-token'              // Development token
});
```

### Production

```typescript
const client = new SignalHubClient({
  url: 'wss://signal-hub.myapp.com',
  jwt: await getAuthToken()  // Get from your auth system
});
```

## Running Examples

### Browser Example

```bash
# Start Signal Hub
cd packages/cloudflare/signal-hub
npm run dev

# Open example (in browser)
open packages/signal-hub-client/examples/basic-usage.html
```

### Node.js Example

```bash
cd packages/signal-hub-client
npm install ws
node examples/node-usage.js
```

## Debugging

### Enable Logging

```typescript
const client = new SignalHubClient({ url, jwt });

// Log all state changes
client.on('stateChange', (state) => {
  console.log('[State]', state);
});

// Log all messages
client.on('message', (event) => {
  console.log('[Message]', event.message.type, event.message);
});

// Log all errors
client.on('error', (event) => {
  console.error('[Error]', event.message, event);
});

// Log connection events
client.on('connected', (event) => {
  console.log('[Connected]', event.sessionId);
});

client.on('disconnected', () => {
  console.log('[Disconnected]');
});
```

### Inspect Network Traffic

```javascript
// In browser DevTools
// 1. Open Network tab
// 2. Filter: WS (WebSocket)
// 3. Click on connection
// 4. View Messages tab
```

## Next Steps

- Read [README.md](./README.md) for full API reference
- See [IMPLEMENTATION.md](./IMPLEMENTATION.md) for architecture details
- Check [examples/](./examples/) for more complex scenarios
- Review [docs/signal-hub/PROTOCOL.md](../../docs/signal-hub/PROTOCOL.md) for protocol spec

## Common Issues

### Connection Timeout

```typescript
// Increase timeout if needed (default: 10s in connect())
try {
  await client.connect();
} catch (error) {
  if (error.message.includes('timeout')) {
    console.error('Server not responding. Check URL and network.');
  }
}
```

### Auth Failure

```typescript
client.on('error', (event) => {
  if (event.code === 'unauthorized') {
    console.error('Invalid JWT. Check token and expiration.');
  }
});
```

### No Messages Received

```typescript
// Ensure handler is registered BEFORE connecting
client.on('message', handler);
await client.connect();

// Check actor is registered
console.log('Actors:', client.actors);

// Check connection state
console.log('Connected:', client.connected);
```

## Support

- Issues: https://github.com/BrianLN-AI/agentic-primer/issues
- Documentation: `/packages/signal-hub-client/README.md`
- Protocol: `/docs/signal-hub/PROTOCOL.md`
