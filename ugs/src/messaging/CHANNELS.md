# Channel<T> Abstraction

Unified interface for async communication patterns in the actor system.

## Overview

The Channel<T> abstraction provides a common protocol for three communication patterns:

1. **StreamChannel** - Point-to-point (1:1) streaming
2. **PortChannel** - Pub/sub (1:N) broadcasting
3. **BridgeChannel** - External async sources (DOM, WebSocket, etc.)

**Key Insight:** AsyncIterator IS a channel primitive. This unifies streaming, ports, and external bridges under one abstraction.

## Core Interface

```typescript
interface Channel<T> extends AsyncIterable<T>, Disposable {
  [Symbol.asyncIterator](): AsyncIterableIterator<T>;
  send?(value: T): Promise<void>;  // Optional (read-only channels)
  close(): void;
  readonly closed: boolean;
  readonly buffered: number;
  [Symbol.dispose](): void;  // TC39 Explicit Resource Management
}
```

## StreamChannel - Point-to-Point (1:1)

Wraps existing `streamAsync()` implementation as a Channel.

```typescript
import { createStreamChannel } from './channels/index.ts';

// From router.streamAsync()
const stream = router.streamAsync(address('actor'), 'query', {});
const channel = createStreamChannel(stream);

// Standard for-await-of consumption
for await (const item of channel) {
  console.log(item);
}

// Or with TC39 using keyword for auto-cleanup
using ch = createStreamChannel(stream);
for await (const item of ch) {
  processItem(item);
}
```

**Features:**
- Automatic 'data'/'end'/'error' handling from AsyncStreamMessage
- Backpressure via for-await-of
- Read-only (no send capability)
- Single consumer

## PortChannel - Pub/Sub (1:N)

Multicast channel where one producer broadcasts to multiple subscribers.

```typescript
import { createPortChannel } from './channels/index.ts';

const statusPort = createPortChannel<StatusEvent>();

// Producer
await statusPort.send({ type: 'ready', timestamp: Date.now() });
await statusPort.send({ type: 'processing', item: 42 });

// Consumer 1
for await (const event of statusPort.subscribe()) {
  console.log('Consumer 1:', event);
}

// Consumer 2
for await (const event of statusPort.subscribe()) {
  console.log('Consumer 2:', event);
}

// Close port (ends all subscriptions)
statusPort.close();
```

**Features:**
- Multiple subscribers, each with independent buffer
- Per-subscriber backpressure handling
- Read/write capability (send + subscribe)
- Auto-cleanup on unsubscribe or AbortSignal

**Per-Subscriber Options:**
```typescript
const sub = statusPort.subscribe({
  bufferSize: 50,  // Override port default
  signal: abortSignal,  // Auto-unsubscribe on abort
  onBackpressure: (isPaused) => {
    console.log('Subscriber backpressure:', isPaused);
  },
});
```

## BridgeChannel - External Sources

Adapts external async sources (DOM events, WebSocket, callbacks) into Channel protocol.

```typescript
import { createBridgeChannel } from './channels/index.ts';

// DOM Event → Channel
const clicks = createBridgeChannel<MouseEvent>((push) => {
  const handler = (e: MouseEvent) => push(e);
  button.addEventListener('click', handler);
  return () => button.removeEventListener('click', handler);
});

for await (const click of clicks) {
  console.log('Clicked at:', click.clientX, click.clientY);
}

// WebSocket → Channel
const messages = createBridgeChannel<string>((push) => {
  const handler = (e: MessageEvent) => push(e.data);
  ws.addEventListener('message', handler);
  return () => ws.removeEventListener('message', handler);
});

for await (const msg of messages) {
  handleMessage(msg);
}
```

**Features:**
- Adapts any callback-based API to Channel
- Automatic cleanup via returned function
- Backpressure handling
- Read-only (values come from external source)

## Common Patterns

### Cancellation with AbortSignal

```typescript
const controller = new AbortController();

// Auto-cancel after timeout
setTimeout(() => controller.abort(), 5000);

for await (const item of channel.subscribe({ signal: controller.signal })) {
  // Will stop after 5 seconds
}
```

### Backpressure Monitoring

```typescript
const channel = createPortChannel({
  bufferSize: 100,
  onBackpressure: (isPaused) => {
    if (isPaused) {
      console.warn('Consumer is slow, buffer full');
    } else {
      console.log('Consumer caught up');
    }
  },
});
```

### Early Termination

```typescript
for await (const item of channel) {
  results.push(item);
  if (results.length >= 10) {
    break;  // Stop consuming
  }
}
```

### Resource Cleanup (TC39 using)

```typescript
{
  using channel = createPortChannel();

  for await (const item of channel) {
    processItem(item);
  }

  // channel.close() called automatically at scope exit
}
```

## Architecture

### Why Channels?

Channels unify three communication patterns under one abstraction:

1. **Request/Stream** - Actor asks, receives AsyncIterator (StreamChannel)
2. **Pub/Sub** - Actor broadcasts, multiple observers subscribe (PortChannel)
3. **External Bridge** - DOM events, WebSocket, etc. adapted to actor protocol (BridgeChannel)

### Integration Points

**Current:**
- `router.streamAsync()` → Wrap in StreamChannel
- Actor helpers (`createAsyncStream()`) → Already return AsyncIterableIterator

**Future:**
- `actor.port(name)` → Returns PortChannel for reactive subscriptions
- Signal bridges → `signalToChannel()`, `domEventChannel()`
- Widget Actors → Web Components with Channel-based ports

## Performance

- **StreamChannel:** <1µs overhead per item
- **PortChannel:** O(N) broadcast, per-subscriber buffers prevent blocking
- **BridgeChannel:** Minimal overhead, direct push to buffer

## Testing

See `src/messaging/__tests__/channels.test.ts` for comprehensive test coverage:
- 20 tests covering all three channel types
- Backpressure handling
- Cancellation support
- Memory safety
- Error handling

Run tests:
```bash
bun test src/messaging/__tests__/channels.test.ts
```

## Files

- `src/messaging/channel.ts` - Core types and BaseChannel
- `src/messaging/channels/stream.ts` - StreamChannel implementation
- `src/messaging/channels/port.ts` - PortChannel implementation
- `src/messaging/channels/bridge.ts` - BridgeChannel implementation
- `src/messaging/channels/index.ts` - Unified exports

## Next Steps

1. ✅ Channel abstraction complete
2. → Implement `actor.port()` using PortChannel (bead: simplify-27g)
3. → Build Signal↔Channel bridges (bead: simplify-3io)
4. → Widget Actor base class (bead: simplify-hqh)
