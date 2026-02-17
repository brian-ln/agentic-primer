# @agentic-primer/signal-hub-client

Lightweight browser WebSocket client for connecting to Cloudflare Signal Hub.

## Features

- Zero dependencies (vanilla JavaScript/TypeScript)
- Native WebSocket with automatic reconnection
- JWT authentication
- Actor registration and message routing
- Type-safe TypeScript API
- Event-driven architecture
- Automatic heartbeat to prevent hibernation
- Message queuing during disconnect

## Installation

```bash
npm install @agentic-primer/signal-hub-client @agentic-primer/protocols
```

## Quick Start

```typescript
import { SignalHubClient } from '@agentic-primer/signal-hub-client';

// Create client
const client = new SignalHubClient({
  url: 'wss://signal-hub.example.com',
  jwt: 'your-jwt-token',
  autoReconnect: true
});

// Connect to hub
await client.connect();

// Register actor
const actorAddress = await client.registerActor({
  address: '@(browser/my-widget)',
  capabilities: ['render', 'interact'],
  metadata: { version: '1.0.0' }
});

// Listen for messages
client.on('message', (event) => {
  console.log('Received message:', event.message);
});

// Send message to another actor
await client.send(
  '@(browser/my-widget)',
  '@(local/coordinator)',
  'widget:ready',
  { id: 'widget-1' }
);
```

## API Reference

### Constructor

```typescript
new SignalHubClient(options: SignalHubClientOptions)
```

**Options:**
- `url` (string): WebSocket URL (wss:// or ws://)
- `jwt` (string): JWT authentication token
- `autoReconnect` (boolean): Auto-reconnect on disconnect (default: true)
- `maxReconnectAttempts` (number): Max reconnection attempts (default: Infinity)
- `reconnectDelay` (number): Initial reconnect delay in ms (default: 1000)
- `maxReconnectDelay` (number): Max reconnect delay in ms (default: 30000)
- `heartbeatInterval` (number): Heartbeat interval in ms (default: 25000)
- `protocolVersion` (string): Protocol version (default: "0.1.0")

### Connection Management

#### `connect(): Promise<void>`

Connect to Signal Hub. Throws if connection fails.

```typescript
await client.connect();
```

#### `disconnect(): Promise<void>`

Gracefully disconnect from Signal Hub.

```typescript
await client.disconnect();
```

#### `connected: boolean`

Check if currently connected.

```typescript
if (client.connected) {
  console.log('Connected!');
}
```

#### `connectionState: ConnectionState`

Get current connection state: `'disconnected'` | `'connecting'` | `'connected'` | `'reconnecting'` | `'disconnecting'`

```typescript
console.log('State:', client.connectionState);
```

### Actor Management

#### `registerActor(registration): Promise<CanonicalAddress>`

Register an actor with Signal Hub.

```typescript
// With explicit address
const address = await client.registerActor({
  address: '@(browser/my-actor)',
  capabilities: ['task1', 'task2'],
  metadata: { version: '1.0' }
});

// Auto-generated address
const address = await client.registerActor({
  capabilities: ['task1'],
});
```

#### `unregisterActor(address): Promise<void>`

Unregister an actor.

```typescript
await client.unregisterActor('@(browser/my-actor)');
```

#### `actors: CanonicalAddress[]`

Get list of registered actors.

```typescript
console.log('Registered actors:', client.actors);
```

### Messaging

#### `send(from, to, type, data, options?): Promise<void>`

Send a message to another actor (fire-and-forget).

```typescript
await client.send(
  '@(browser/widget)',      // from
  '@(local/coordinator)',   // to
  'widget:update',          // type
  { state: 'active' },      // data
  { ttl: 5000 }             // options (optional)
);
```

**Options:**
- `ttl` (number): Message TTL in ms (default: 30000)
- `traceId` (string): Trace ID for distributed tracing
- `priority` (number): Message priority 0=high, 1=normal, 2=low (default: 1)

#### `sendWithAck(from, to, type, data, options?): Promise<string>`

Send a message and wait for delivery acknowledgment.

```typescript
const messageId = await client.sendWithAck(
  '@(browser/widget)',
  '@(local/coordinator)',
  'widget:command',
  { action: 'start' }
);
console.log('Message delivered:', messageId);
```

### Events

#### `on(event, handler): void`

Register an event handler.

**Events:**

- **`'message'`**: Received a message from another actor

```typescript
client.on('message', (event: HubMessageEvent) => {
  console.log('From:', event.message.from);
  console.log('Type:', event.message.type);
  console.log('Payload:', event.message.payload);
  console.log('Original sender:', event.originalFrom);
  console.log('Forwarded:', event.forwarded);
});
```

- **`'connected'`**: Connected to Signal Hub

```typescript
client.on('connected', (event: ConnectionEvent) => {
  console.log('Session ID:', event.sessionId);
  console.log('Server version:', event.serverVersion);
  console.log('Actor identity:', event.actorIdentity);
});
```

- **`'disconnected'`**: Disconnected from Signal Hub

```typescript
client.on('disconnected', () => {
  console.log('Disconnected from hub');
});
```

- **`'error'`**: Error occurred

```typescript
client.on('error', (event: ErrorEvent) => {
  console.error('Error:', event.message);
  console.error('Code:', event.code);
  console.error('Details:', event.error);
});
```

- **`'stateChange'`**: Connection state changed

```typescript
client.on('stateChange', (state: ConnectionState) => {
  console.log('New state:', state);
});
```

#### `off(event, handler): void`

Remove an event handler.

```typescript
const handler = (event) => console.log(event);
client.on('message', handler);
client.off('message', handler);
```

## Complete Example

```typescript
import { SignalHubClient, generateBrowserAddress } from '@agentic-primer/signal-hub-client';

// Initialize client
const client = new SignalHubClient({
  url: 'wss://signal-hub.myapp.com',
  jwt: 'eyJhbGc...',
  autoReconnect: true,
  heartbeatInterval: 25000
});

// Set up event handlers
client.on('connected', async (event) => {
  console.log('Connected with session:', event.sessionId);
  console.log('Actor identity:', event.actorIdentity);

  // Register actor after connection
  const address = await client.registerActor({
    address: generateBrowserAddress('my-widget'),
    capabilities: ['render', 'interact', 'update'],
    metadata: {
      widgetType: 'dashboard',
      version: '1.0.0'
    }
  });

  console.log('Registered as:', address);
});

client.on('message', (event) => {
  const { message } = event;

  switch (message.type) {
    case 'app:render':
      console.log('Render request:', message.payload);
      break;

    case 'app:update':
      console.log('Update request:', message.payload);
      break;

    default:
      console.log('Unknown message type:', message.type);
  }
});

client.on('error', (event) => {
  console.error('Error:', event.message, event.code);
});

client.on('disconnected', () => {
  console.log('Disconnected - will auto-reconnect');
});

client.on('stateChange', (state) => {
  console.log('Connection state:', state);
});

// Connect
await client.connect();

// Send a message
await client.send(
  '@(browser/my-widget)',
  '@(local/coordinator)',
  'widget:ready',
  {
    capabilities: client.actors,
    timestamp: Date.now()
  }
);

// Clean up on page unload
window.addEventListener('beforeunload', () => {
  client.disconnect();
});
```

## Browser Usage (UMD)

For direct browser usage without a bundler, use the UMD build:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Signal Hub Client Example</title>
</head>
<body>
  <script src="https://unpkg.com/@agentic-primer/signal-hub-client/dist/index.umd.js"></script>
  <script>
    const { SignalHubClient } = window.SignalHubClient;

    const client = new SignalHubClient({
      url: 'wss://signal-hub.example.com',
      jwt: 'your-jwt-token'
    });

    client.on('connected', () => {
      console.log('Connected!');
    });

    client.on('message', (event) => {
      console.log('Message:', event.message);
    });

    client.connect();
  </script>
</body>
</html>
```

## Error Handling

```typescript
try {
  await client.connect();
} catch (error) {
  console.error('Connection failed:', error);
}

try {
  await client.registerActor({
    address: '@(browser/my-actor)',
    capabilities: ['task1']
  });
} catch (error) {
  console.error('Registration failed:', error);
}

try {
  await client.sendWithAck(
    '@(browser/sender)',
    '@(local/receiver)',
    'test:message',
    { data: 'test' }
  );
} catch (error) {
  console.error('Message delivery failed:', error);
}
```

## Reconnection

The client automatically reconnects using exponential backoff:

- Initial delay: 1s
- Backoff multiplier: 2x
- Max delay: 30s
- Max attempts: Infinity (configurable)

```typescript
const client = new SignalHubClient({
  url: 'wss://signal-hub.example.com',
  jwt: 'token',
  autoReconnect: true,
  maxReconnectAttempts: 10,
  reconnectDelay: 1000,
  maxReconnectDelay: 30000
});
```

## Heartbeat

The client automatically sends heartbeats every 25s (configurable) to prevent Cloudflare WebSocket hibernation (30s idle timeout).

```typescript
const client = new SignalHubClient({
  url: 'wss://signal-hub.example.com',
  jwt: 'token',
  heartbeatInterval: 25000  // 25s (< 30s Cloudflare hibernation)
});
```

## Protocol Details

### Message Structure

All messages use `SharedMessage` format from `@agentic-primer/protocols`:

```typescript
interface SharedMessage {
  id: string;                    // UUID
  from: CanonicalAddress;        // @(path) sender
  to: CanonicalAddress;          // @(path) recipient
  type: string;                  // Message type
  pattern: 'tell' | 'ask';       // Messaging pattern
  correlationId: string | null;  // For request/response
  timestamp: number;             // Epoch ms
  payload: unknown;              // Message data
  metadata: Record<string, unknown>;
  ttl: number | null;            // Time-to-live (ms)
  signature: string | null;      // Optional signature
}
```

### Hub Messages

The client uses these hub protocol messages:

- `hub:connect` - Initiate connection with JWT
- `hub:connected` - Connection confirmed
- `hub:register` - Register an actor
- `hub:registered` - Registration confirmed
- `hub:unregister` - Unregister an actor
- `hub:send` - Send message to actor
- `hub:delivery_ack` - Message delivered
- `hub:heartbeat` - Keep-alive ping
- `hub:heartbeat_ack` - Keep-alive pong
- `hub:error` - Error notification

### Flat Payload Structure

When sending messages through the hub, use flat payload structure:

```typescript
// Application sends:
await client.send(
  '@(browser/widget)',
  '@(local/coordinator)',
  'app:render',
  { component: 'Button' }
);

// Client converts to hub:send with flat payload:
{
  to: '@(cloudflare/signal-hub)',
  type: 'hub:send',
  payload: {
    to: '@(local/coordinator)',
    type: 'app:render',
    data: { component: 'Button' }
  }
}
```

## License

MIT
