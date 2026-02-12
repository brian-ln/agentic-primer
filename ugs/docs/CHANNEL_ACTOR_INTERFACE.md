## ChannelActor Interface Specification

**Version:** 1.0.0
**Status:** Stable
**Created:** 2026-02-05

## Overview

The `ChannelActor` interface defines a standard protocol for connecting external messaging platforms (WhatsApp, Telegram, Discord, Slack, etc.) to the Simplify actor system. It provides a uniform abstraction over platform-specific messaging protocols, enabling consistent message routing, supervision, and lifecycle management.

**Design Goals:**
- **Uniform Interface:** All messaging platforms implement the same interface
- **Actor Model:** Channels are supervised actors with lifecycle hooks
- **Platform Agnostic:** Abstract away platform-specific details
- **Reliable:** Built-in reconnection and error handling
- **Observable:** Rich status information for monitoring

## Architecture

### Channel Flow

```
┌─────────────────────────────────────────────────────────────┐
│                   External Platform                          │
│              (WhatsApp, Telegram, Discord)                   │
└────────────────────────┬────────────────────────────────────┘
                         │ Platform Protocol
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                  Platform Client                             │
│           (Baileys, Grammy, Discord.js)                      │
└────────────────────────┬────────────────────────────────────┘
                         │ Client Events
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                  ChannelActor                                │
│        (Normalize messages, manage connection)               │
└────────────────────────┬────────────────────────────────────┘
                         │ Actor Messages
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              ChannelMessageRouter                            │
│         (Route to sessions/agents by user)                   │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                  SessionActor                                │
│              (Process with agent/LLM)                        │
└─────────────────────────────────────────────────────────────┘
```

### Supervision Integration

```
┌─────────────────────────┐
│   ChannelSupervisor     │
│  (one-for-one restart)  │
└────────┬────────────────┘
         │ supervises
    ┌────┼────┬────┬────┐
    │    │    │    │    │
┌───▼┐ ┌─▼─┐ ┌▼─┐ ┌▼──┐│
│WA  │ │TG │ │DC│ │Slk││
│Chan│ │Chr│ │Chr│ │Chr││
└────┘ └───┘ └──┘ └───┘│
```

Each channel is supervised independently with:
- Health checks (ping/pong)
- Automatic restart on failure
- Reconnection with exponential backoff
- Checkpoint save/restore

## Core Interface

### ChannelActor

```typescript
interface ChannelActor extends SupervisedActor {
  readonly config: ChannelConfig;

  // Actor protocol
  receive(message: Message): Promise<MessageResponse>;

  // Status
  getStatus(): ChannelStatus;

  // Messaging
  send(message: OutboundChannelMessage): Promise<string>;

  // Lifecycle
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  reconnect(): Promise<void>;
}
```

**Supported Message Types:**
- `connect`: Connect to platform
- `disconnect`: Disconnect from platform
- `reconnect`: Force reconnection
- `send`: Send message to platform
- `status`: Get channel status

## Configuration

### ChannelConfig

```typescript
interface ChannelConfig {
  id: string;                           // Unique channel ID
  name: string;                         // Display name
  platform: string;                     // Platform type
  autoConnect?: boolean;                // Auto-connect on startup
  autoReconnect?: boolean;              // Auto-reconnect on disconnect
  reconnect?: {
    initialDelay?: number;              // Initial delay (ms)
    maxDelay?: number;                  // Max delay (ms)
    multiplier?: number;                // Backoff multiplier
    maxAttempts?: number;               // Max reconnect attempts
  };
  platformConfig: Record<string, any>;  // Platform-specific config
}
```

**Example: WhatsApp Channel**
```typescript
const whatsappConfig: ChannelConfig = {
  id: 'whatsapp-primary',
  name: 'WhatsApp Business',
  platform: 'whatsapp',
  autoConnect: true,
  autoReconnect: true,
  reconnect: {
    initialDelay: 1000,
    maxDelay: 30000,
    multiplier: 2,
    maxAttempts: Infinity
  },
  platformConfig: {
    sessionPath: '~/.simplify/whatsapp-session',
    phoneNumber: '+1234567890',
    webhook: 'https://api.example.com/whatsapp'
  }
};
```

**Example: Telegram Bot**
```typescript
const telegramConfig: ChannelConfig = {
  id: 'telegram-bot-123',
  name: 'My Telegram Bot',
  platform: 'telegram',
  autoConnect: true,
  autoReconnect: true,
  platformConfig: {
    botToken: process.env.TELEGRAM_BOT_TOKEN,
    allowedUsers: ['user123', 'user456'],
    commandPrefix: '/'
  }
};
```

## Message Format

### Inbound Messages

Messages received from external platforms are normalized to a common format:

```typescript
interface InboundChannelMessage {
  id: string;                           // Platform message ID
  from: ChannelRecipient;               // Sender
  to?: ChannelRecipient;                // Recipient (groups)
  content: ChannelContent[];            // Message content
  timestamp: number;                    // Platform timestamp
  replyTo?: string;                     // Reply to message ID
  metadata?: Record<string, any>;       // Platform metadata
}
```

**Example: Text Message**
```typescript
const inboundMessage: InboundChannelMessage = {
  id: 'msg_abc123',
  from: {
    id: '1234567890',
    type: 'user',
    name: 'John Doe'
  },
  content: [
    { type: 'text', text: 'Hello, how can you help me?' }
  ],
  timestamp: Date.now()
};
```

**Example: Image with Caption**
```typescript
const imageMessage: InboundChannelMessage = {
  id: 'msg_def456',
  from: {
    id: '1234567890',
    type: 'user',
    name: 'Jane Smith'
  },
  content: [
    {
      type: 'image',
      url: 'https://cdn.example.com/image.jpg',
      caption: 'Check out this photo'
    }
  ],
  timestamp: Date.now()
};
```

### Outbound Messages

Messages sent to external platforms:

```typescript
interface OutboundChannelMessage {
  to: ChannelRecipient;                 // Recipient
  content: ChannelContent[];            // Message content
  replyTo?: string;                     // Reply to message ID
  options?: Record<string, any>;        // Platform options
}
```

**Example: Reply with Text**
```typescript
const outboundMessage: OutboundChannelMessage = {
  to: {
    id: '1234567890',
    type: 'user'
  },
  content: [
    { type: 'text', text: 'I can help you with that!' }
  ],
  replyTo: 'msg_abc123'
};
```

### Content Types

Supported content types for cross-platform messaging:

```typescript
type ChannelContent =
  | { type: 'text'; text: string }
  | { type: 'image'; url: string; caption?: string }
  | { type: 'file'; url: string; filename?: string; mimeType?: string }
  | { type: 'audio'; url: string; duration?: number }
  | { type: 'video'; url: string; duration?: number; thumbnail?: string }
  | { type: 'location'; latitude: number; longitude: number; name?: string }
  | { type: 'contact'; name: string; phone?: string; email?: string };
```

**Content Type Support Matrix:**

| Content Type | WhatsApp | Telegram | Discord | Slack | SMS |
|--------------|----------|----------|---------|-------|-----|
| text         | ✅       | ✅       | ✅      | ✅    | ✅  |
| image        | ✅       | ✅       | ✅      | ✅    | ❌  |
| file         | ✅       | ✅       | ✅      | ✅    | ❌  |
| audio        | ✅       | ✅       | ✅      | ✅    | ❌  |
| video        | ✅       | ✅       | ✅      | ✅    | ❌  |
| location     | ✅       | ✅       | ❌      | ❌    | ❌  |
| contact      | ✅       | ✅       | ❌      | ❌    | ❌  |

Channels should gracefully degrade unsupported content types (e.g., send file URL as text link).

## Status and Monitoring

### ChannelStatus

```typescript
interface ChannelStatus {
  status: ChannelConnectionStatus;      // Current status
  connected: boolean;                   // Is connected?
  authenticated: boolean;               // Is authenticated?
  qrCode?: string;                      // QR code for pairing
  error?: Error;                        // Last error
  lastConnected?: number;               // Last connection time
  lastSent?: number;                    // Last sent time
  lastReceived?: number;                // Last received time
  messagesSent: number;                 // Messages sent count
  messagesReceived: number;             // Messages received count
  reconnectAttempts: number;            // Reconnect attempts
  metadata?: Record<string, any>;       // Platform metadata
}
```

**Connection Status Values:**
- `disconnected`: Not connected, initial state
- `connecting`: Connection in progress
- `authenticating`: Connected, awaiting authentication
- `connected`: Connected and ready
- `reconnecting`: Attempting to reconnect
- `error`: Error state, requires intervention

**Example Status Response:**
```typescript
{
  status: 'connected',
  connected: true,
  authenticated: true,
  lastConnected: 1738703000000,
  lastSent: 1738703100000,
  lastReceived: 1738703050000,
  messagesSent: 42,
  messagesReceived: 38,
  reconnectAttempts: 0,
  metadata: {
    phoneNumber: '+1234567890',
    deviceName: 'My Device',
    batteryLevel: 85
  }
}
```

## Lifecycle Management

### Connection Lifecycle

```
disconnected
     │
     │ connect()
     ▼
 connecting
     │
     │ platform auth
     ▼
authenticating
     │
     │ success
     ▼
 connected ◄──────────┐
     │                │
     │ disconnect()   │ reconnect()
     ▼                │
disconnected          │
     │                │
     │ network error  │
     ▼                │
  error ──────────────┘
     │
     │ auto-reconnect
     ▼
reconnecting
```

### Supervision Lifecycle

Channels implement `SupervisedActor` interface for supervision:

```typescript
// Called before restart
async preRestart(error: Error, message?: Message): Promise<any> {
  // Disconnect gracefully
  await this.disconnect();

  // Return checkpoint data
  return {
    config: this.config,
    status: this._status,
    reconnectAttempts: this._status.reconnectAttempts
  };
}

// Called after restart
async postRestart(checkpoint?: any): Promise<void> {
  // Restore state from checkpoint
  if (checkpoint?.status?.connected) {
    // Reconnect if previously connected
    await this.connect();
  }
}

// Health check
async healthCheck(): Promise<boolean> {
  // Check connection status
  return this._status.connected;
}
```

### Reconnection Strategy

Built-in exponential backoff for automatic reconnection:

```typescript
// Reconnect configuration
reconnect: {
  initialDelay: 1000,   // Start with 1s
  maxDelay: 30000,      // Cap at 30s
  multiplier: 2,        // Double each attempt
  maxAttempts: Infinity // Never give up
}

// Backoff sequence: 1s, 2s, 4s, 8s, 16s, 30s, 30s, ...
```

## Implementation Guide

### Creating a Channel Adapter

Step 1: Extend `BaseChannelActor`

```typescript
import { BaseChannelActor, type ChannelConfig } from './ChannelActor';
import { Actor } from '../actor';
import { MessageRouter } from '../router';

class MyPlatformChannel extends BaseChannelActor {
  private client?: MyPlatformClient;

  constructor(config: ChannelConfig, router: MessageRouter) {
    super(config);
    // Initialize actor properties
    this.address = address(config.id);
    this.router = router;
  }

  // Implement required Actor interface
  get address() { return this._address; }
  async tell(to: any, type: string, payload: any) { ... }
  async ask(to: any, type: string, payload: any) { ... }

  // Implement platform connection
  async connect(): Promise<void> {
    this.updateStatus({ status: 'connecting' });

    try {
      // Connect to platform
      this.client = await MyPlatformClient.connect(
        this.config.platformConfig
      );

      // Register message handler
      this.client.on('message', async (msg) => {
        const normalized = this.normalizeMessage(msg);
        await this.handleInboundMessage(normalized);
      });

      this.updateStatus({
        status: 'connected',
        connected: true,
        authenticated: true,
        lastConnected: Date.now()
      });
    } catch (error: any) {
      this.updateStatus({
        status: 'error',
        error
      });
      throw error;
    }
  }

  async disconnect(): Promise<void> {
    await this.client?.disconnect();
    this.client = undefined;
    this.updateStatus({
      status: 'disconnected',
      connected: false
    });
  }

  async send(message: OutboundChannelMessage): Promise<string> {
    if (!this.client) {
      throw new Error('Not connected');
    }

    // Convert to platform format
    const platformMessage = this.denormalizeMessage(message);

    // Send via platform
    const messageId = await this.client.send(platformMessage);

    this._status.messagesSent++;
    this._status.lastSent = Date.now();

    return messageId;
  }

  // Platform-specific message normalization
  private normalizeMessage(platformMsg: any): InboundChannelMessage {
    return {
      id: platformMsg.id,
      from: {
        id: platformMsg.senderId,
        type: 'user',
        name: platformMsg.senderName
      },
      content: [
        { type: 'text', text: platformMsg.text }
      ],
      timestamp: platformMsg.timestamp
    };
  }

  // Platform-specific message denormalization
  private denormalizeMessage(msg: OutboundChannelMessage): any {
    return {
      recipientId: msg.to.id,
      text: msg.content.find(c => c.type === 'text')?.text || ''
    };
  }
}
```

Step 2: Register with Supervisor

```typescript
import { ChannelSupervisor } from '../supervision/supervisors/channel';

// Create supervisor
const channelSupervisor = new ChannelSupervisor(
  'channel-supervisor',
  router,
  {
    strategy: {
      type: 'one-for-one',
      maxRestarts: 3,
      withinSeconds: 60
    }
  }
);

// Create and supervise channel
const myChannel = new MyPlatformChannel(config, router);

channelSupervisor.supervise(myChannel, {
  strategy: {
    type: 'one-for-one',
    maxRestarts: 3,
    withinSeconds: 60
  },
  healthCheck: {
    interval: 30000,
    timeout: 5000,
    threshold: 3
  }
});

// Auto-connect if configured
if (config.autoConnect) {
  await myChannel.connect();
}
```

## Example: WhatsApp Adapter

Mapping OpenClaw's WhatsApp channel to ChannelActor:

```typescript
import { BaseChannelActor } from './ChannelActor';
import makeWASocket, { DisconnectReason } from '@whiskeysockets/baileys';

class WhatsAppChannel extends BaseChannelActor {
  private sock?: ReturnType<typeof makeWASocket>;

  async connect(): Promise<void> {
    this.updateStatus({ status: 'connecting' });

    const { state, saveCreds } = await useMultiFileAuthState(
      this.config.platformConfig.sessionPath
    );

    this.sock = makeWASocket({
      auth: state,
      printQRInTerminal: false
    });

    // Handle QR code for pairing
    this.sock.ev.on('connection.update', (update) => {
      const { connection, lastDisconnect, qr } = update;

      if (qr) {
        this.updateStatus({
          status: 'authenticating',
          qrCode: qr // Base64 QR code
        });
      }

      if (connection === 'open') {
        this.updateStatus({
          status: 'connected',
          connected: true,
          authenticated: true,
          lastConnected: Date.now()
        });
      }

      if (connection === 'close') {
        const shouldReconnect =
          (lastDisconnect?.error as any)?.output?.statusCode !==
          DisconnectReason.loggedOut;

        if (shouldReconnect && this.config.autoReconnect) {
          this.reconnect();
        }
      }
    });

    // Handle incoming messages
    this.sock.ev.on('messages.upsert', async ({ messages }) => {
      for (const msg of messages) {
        if (msg.message) {
          const normalized = this.normalizeWhatsAppMessage(msg);
          await this.handleInboundMessage(normalized);
        }
      }
    });

    this.sock.ev.on('creds.update', saveCreds);
  }

  async disconnect(): Promise<void> {
    this.sock?.end(undefined);
    this.sock = undefined;
    this.updateStatus({
      status: 'disconnected',
      connected: false
    });
  }

  async send(message: OutboundChannelMessage): Promise<string> {
    if (!this.sock) throw new Error('Not connected');

    const content = message.content[0];
    let result;

    switch (content.type) {
      case 'text':
        result = await this.sock.sendMessage(message.to.id, {
          text: content.text
        });
        break;

      case 'image':
        result = await this.sock.sendMessage(message.to.id, {
          image: { url: content.url },
          caption: content.caption
        });
        break;

      // ... other content types
    }

    this._status.messagesSent++;
    this._status.lastSent = Date.now();

    return result.key.id!;
  }

  private normalizeWhatsAppMessage(msg: any): InboundChannelMessage {
    const senderId = msg.key.remoteJid!;
    const messageContent = msg.message;

    let content: ChannelContent[];

    if (messageContent.conversation) {
      content = [{
        type: 'text',
        text: messageContent.conversation
      }];
    } else if (messageContent.imageMessage) {
      content = [{
        type: 'image',
        url: messageContent.imageMessage.url!,
        caption: messageContent.imageMessage.caption
      }];
    } else {
      content = [{
        type: 'text',
        text: '[Unsupported message type]'
      }];
    }

    return {
      id: msg.key.id!,
      from: {
        id: senderId,
        type: senderId.includes('@g.us') ? 'group' : 'user',
        name: msg.pushName
      },
      content,
      timestamp: msg.messageTimestamp * 1000,
      metadata: {
        deviceType: msg.deviceType,
        forwarded: messageContent.forwardingScore > 0
      }
    };
  }
}
```

## Testing

### Unit Tests

Test channel lifecycle and message handling:

```typescript
describe('MyPlatformChannel', () => {
  let channel: MyPlatformChannel;
  let mockClient: jest.Mocked<MyPlatformClient>;

  beforeEach(() => {
    mockClient = createMockClient();
    channel = new MyPlatformChannel(config, router);
  });

  test('connects successfully', async () => {
    await channel.connect();

    expect(channel.getStatus().connected).toBe(true);
    expect(channel.getStatus().status).toBe('connected');
  });

  test('handles connection errors', async () => {
    mockClient.connect.mockRejectedValue(new Error('Network error'));

    await expect(channel.connect()).rejects.toThrow('Network error');
    expect(channel.getStatus().status).toBe('error');
  });

  test('sends messages', async () => {
    await channel.connect();

    const messageId = await channel.send({
      to: { id: 'user123', type: 'user' },
      content: [{ type: 'text', text: 'Hello' }]
    });

    expect(messageId).toBeDefined();
    expect(channel.getStatus().messagesSent).toBe(1);
  });

  test('receives and normalizes messages', async () => {
    const handler = jest.fn();
    channel.onMessage(handler);

    await channel.connect();

    // Simulate platform message
    mockClient.emit('message', {
      id: 'msg123',
      senderId: 'user456',
      text: 'Hi there'
    });

    expect(handler).toHaveBeenCalledWith({
      id: 'msg123',
      from: { id: 'user456', type: 'user' },
      content: [{ type: 'text', text: 'Hi there' }],
      timestamp: expect.any(Number)
    });
  });

  test('reconnects after disconnect', async () => {
    await channel.connect();
    mockClient.emit('disconnect');

    await new Promise(resolve => setTimeout(resolve, 1500));

    expect(channel.getStatus().status).toBe('connected');
    expect(channel.getStatus().reconnectAttempts).toBe(1);
  });
});
```

### Integration Tests

Test with supervisor:

```typescript
describe('Channel Supervision', () => {
  test('restarts on failure', async () => {
    const supervisor = new ChannelSupervisor('test-supervisor', router);
    const channel = new MyPlatformChannel(config, router);

    supervisor.supervise(channel, {
      strategy: { type: 'one-for-one', maxRestarts: 3, withinSeconds: 60 }
    });

    await channel.connect();

    // Simulate crash
    throw new Error('Channel crashed');

    // Wait for restart
    await new Promise(resolve => setTimeout(resolve, 2000));

    // Channel should be restarted and connected
    expect(channel.getStatus().connected).toBe(true);
  });
});
```

## Best Practices

### 1. Graceful Degradation

Handle unsupported content types gracefully:

```typescript
async send(message: OutboundChannelMessage): Promise<string> {
  for (const content of message.content) {
    if (content.type === 'location' && !this.supportsLocations) {
      // Degrade to text with link
      const textContent: ChannelContent = {
        type: 'text',
        text: `Location: https://maps.google.com/?q=${content.latitude},${content.longitude}`
      };
      message.content = [textContent];
    }
  }
  // ... send message
}
```

### 2. Error Classification

Classify errors for supervision:

```typescript
async connect(): Promise<void> {
  try {
    await this.client.connect();
  } catch (error: any) {
    // Classify error for supervisor
    if (error.code === 'ECONNREFUSED') {
      // Transient - supervisor will restart
      throw new NetworkError('Connection refused');
    } else if (error.code === 'AUTH_FAILED') {
      // Permanent - requires manual intervention
      throw new ConfigError('Authentication failed');
    } else {
      throw error;
    }
  }
}
```

### 3. Rate Limiting

Respect platform rate limits:

```typescript
class RateLimitedChannel extends BaseChannelActor {
  private sendQueue: OutboundChannelMessage[] = [];
  private rateLimiter = new RateLimiter(10, 1000); // 10 msg/sec

  async send(message: OutboundChannelMessage): Promise<string> {
    await this.rateLimiter.acquire();
    return await super.send(message);
  }
}
```

### 4. Message Deduplication

Prevent duplicate messages:

```typescript
class DeduplicatedChannel extends BaseChannelActor {
  private seenMessages = new Set<string>();

  protected async handleInboundMessage(msg: InboundChannelMessage) {
    if (this.seenMessages.has(msg.id)) {
      return; // Duplicate, skip
    }

    this.seenMessages.add(msg.id);

    // Cleanup old entries (keep last 1000)
    if (this.seenMessages.size > 1000) {
      const firstKey = this.seenMessages.values().next().value;
      this.seenMessages.delete(firstKey);
    }

    await super.handleInboundMessage(msg);
  }
}
```

### 5. Monitoring and Metrics

Track channel health:

```typescript
class MonitoredChannel extends BaseChannelActor {
  private metrics = {
    messagesSent: 0,
    messagesReceived: 0,
    errors: 0,
    avgSendTime: 0
  };

  async send(message: OutboundChannelMessage): Promise<string> {
    const start = Date.now();
    try {
      const result = await super.send(message);
      this.metrics.messagesSent++;
      this.metrics.avgSendTime =
        (this.metrics.avgSendTime + (Date.now() - start)) / 2;
      return result;
    } catch (error) {
      this.metrics.errors++;
      throw error;
    }
  }

  getMetrics() {
    return { ...this.metrics };
  }
}
```

## Migration from OpenClaw

### Mapping OpenClaw Channels

OpenClaw channels can be mapped to ChannelActor with these changes:

| OpenClaw | ChannelActor | Notes |
|----------|--------------|-------|
| `Channel.start()` | `connect()` | Same concept |
| `Channel.stop()` | `disconnect()` | Same concept |
| `Channel.send()` | `send()` | Now returns Promise<string> with message ID |
| Event: `message` | `handleInboundMessage()` | Now protected method |
| Event: `status` | `getStatus()` | Now synchronous method |
| Direct method calls | Actor messages | Use `receive()` for actor protocol |

### Example Migration

**Before (OpenClaw):**
```typescript
class WhatsAppChannel {
  async start() { ... }
  async stop() { ... }
  async send(to: string, message: string) { ... }

  on(event: 'message', handler: (msg: any) => void) { ... }
  on(event: 'status', handler: (status: string) => void) { ... }
}

// Usage
const channel = new WhatsAppChannel(config);
await channel.start();

channel.on('message', (msg) => {
  console.log('Received:', msg);
});

await channel.send('user123', 'Hello!');
```

**After (ChannelActor):**
```typescript
class WhatsAppChannel extends BaseChannelActor {
  async connect() { ... }
  async disconnect() { ... }
  async send(message: OutboundChannelMessage) { ... }

  // Override to handle inbound messages
  protected async handleInboundMessage(msg: InboundChannelMessage) {
    await super.handleInboundMessage(msg);
    // Additional handling if needed
  }
}

// Usage
const channel = new WhatsAppChannel(config, router);

// Register message handler
channel.onMessage((msg) => {
  console.log('Received:', msg);
});

// Connect via actor message or direct method
await channel.connect();
// OR
await router.ask({
  to: channel.address,
  type: 'connect'
});

// Send via direct method
await channel.send({
  to: { id: 'user123', type: 'user' },
  content: [{ type: 'text', text: 'Hello!' }]
});
// OR via actor message
await router.ask({
  to: channel.address,
  type: 'send',
  payload: { ... }
});
```

## Future Enhancements

### Planned Features

1. **Multi-Device Support**
   - Multiple instances of same platform
   - Load balancing across instances
   - Failover between instances

2. **Message Queuing**
   - Persistent queue for offline delivery
   - Retry failed messages
   - Delivery receipts

3. **Rich Content**
   - Interactive buttons
   - Carousels and cards
   - Reactions and emojis

4. **Platform Extensions**
   - Custom content types per platform
   - Platform-specific features (polls, payments, etc.)
   - WebRTC for voice/video calls

5. **Analytics**
   - Message delivery rates
   - Response times
   - User engagement metrics

## References

- OpenClaw Architecture: `OPENCLAW_ARCHITECTURE_MODEL.md`
- Supervision Design: `SUPERVISION_ARCHITECTURE.md`
- Actor System: `src/messaging/actor.ts`
- Channel Types: `src/messaging/channels/ChannelActor.ts`

## Changelog

- 2026-02-05: Initial specification (v1.0.0)
