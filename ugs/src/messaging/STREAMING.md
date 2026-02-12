# AsyncIterator Streaming API

## Overview

The messaging system now supports AsyncIterator-based streaming with backpressure handling, cancellation, and memory safety. This provides a modern, ergonomic API for streaming data between actors.

## Basic Usage

```typescript
import { MessageRouter } from './router.ts';
import { address } from './message.ts';

// Create router (assume router is initialized)
const router: MessageRouter;

// Stream data from an actor
for await (const event of router.streamAsync(address('actor-id'), 'query', {})) {
  if (event.type === 'data') {
    console.log('Received:', event.payload);
  } else if (event.type === 'end') {
    console.log('Stream completed');
    break;
  } else if (event.type === 'error') {
    console.error('Stream error:', event.error);
    throw new Error(event.error);
  }
}
```

## Features

### 1. Backpressure Handling

The router automatically buffers items when the consumer is slow, preventing memory overflow:

```typescript
for await (const event of router.streamAsync(address('fast-producer'), 'query', {}, {
  bufferSize: 100, // Buffer up to 100 items (default: 100)
})) {
  if (event.type === 'data') {
    // Slow processing
    await processExpensiveOperation(event.payload);
  }
}
```

### 2. Cancellation Support

Use AbortController to cancel streams mid-flight:

```typescript
const controller = new AbortController();

// Cancel after 5 seconds
setTimeout(() => controller.abort('Timeout'), 5000);

try {
  for await (const event of router.streamAsync(address('actor'), 'query', {}, {
    signal: controller.signal,
  })) {
    if (event.type === 'data') {
      console.log(event.payload);
    }
  }
} catch (error) {
  console.log('Stream cancelled:', error.message);
}
```

### 3. Timeout Protection

Prevent streams from running indefinitely:

```typescript
for await (const event of router.streamAsync(address('slow-actor'), 'query', {}, {
  timeout: 30000, // Timeout after 30 seconds (default: 60000)
})) {
  // Process events
}
```

### 4. Early Termination

Simply break from the loop to stop consuming:

```typescript
for await (const event of router.streamAsync(address('large-dataset'), 'query', {})) {
  if (event.type === 'data') {
    results.push(event.payload);
    // Stop after 10 items
    if (results.length >= 10) break;
  }
}
```

## Implementing Streaming Actors

### Using Helper Methods

The `Actor` base class provides helper methods for creating streams:

```typescript
class SessionActor extends Actor {
  private messages: Message[];

  async *streamAsync(payload: any): AsyncIterableIterator<AsyncStreamMessage<Message>> {
    // Simple: stream from an array
    yield* this.createAsyncStream(this.messages);
  }
}
```

### With Backpressure Detection

```typescript
class SmartActor extends Actor {
  async *streamAsync(payload: any): AsyncIterableIterator<AsyncStreamMessage<string>> {
    yield* this.createAsyncStreamWithBackpressure(this.dataSource, {
      checkInterval: 10,
      onBackpressure: (isPaused) => {
        if (isPaused) {
          console.log('Consumer is slow, consider throttling');
        }
      },
    });
  }
}
```

### Manual Implementation

For full control, implement the async generator directly:

```typescript
class CustomActor extends Actor {
  async *streamAsync(payload: any): AsyncIterableIterator<AsyncStreamMessage<Token>> {
    const corrId = generateCorrelationId();

    try {
      // Generate data
      for await (const token of this.generateTokens(payload)) {
        yield {
          id: generateMessageId(),
          correlationId: corrId,
          from: this.address,
          type: 'data',
          payload: token,
          timestamp: Date.now(),
        };
      }

      // Signal completion
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'end',
        timestamp: Date.now(),
      };
    } catch (error: any) {
      // Signal error
      yield {
        id: generateMessageId(),
        correlationId: corrId,
        from: this.address,
        type: 'error',
        error: error.message,
        timestamp: Date.now(),
      };
    }
  }
}
```

## Message Types

### AsyncStreamMessage<T>

```typescript
interface AsyncStreamMessage<T = any> {
  id: string;              // Unique message ID
  correlationId: string;   // Links all messages in stream
  from: Address;           // Source actor address
  type: 'data' | 'end' | 'error'; // Event type
  payload?: T;             // Data (present for 'data' type)
  error?: string;          // Error message (present for 'error' type)
  timestamp: number;       // Creation timestamp
}
```

### StreamAsyncOptions

```typescript
interface StreamAsyncOptions {
  signal?: AbortSignal;    // For cancellation
  bufferSize?: number;     // Buffer size (default: 100)
  timeout?: number;        // Timeout in ms (default: 60000)
}
```

## Performance

- **Overhead**: <1µs per item for fast consumers
- **Throughput**: >400,000 items/second on typical hardware
- **Memory**: Bounded by bufferSize (default 100 items)

## Backward Compatibility

The new AsyncIterator API is fully backward compatible:

- Existing callback-based `streamAsk()` still works
- Existing `stream()` method on actors still supported
- No breaking changes to existing APIs

## Migration Guide

### From Callback-Based Streaming

**Before:**
```typescript
await router.streamAsk(address('actor'), 'query', {}, {
  onChunk: (chunk) => {
    console.log(chunk);
  },
});
```

**After:**
```typescript
for await (const event of router.streamAsync(address('actor'), 'query', {})) {
  if (event.type === 'data') {
    console.log(event.payload);
  }
}
```

### From Custom Stream Implementation

**Before:**
```typescript
async stream(payload: any, onChunk: StreamCallback<Token>) {
  for (const token of this.tokens) {
    await onChunk({ type: 'token', content: token });
  }
  await onChunk({ type: 'done' });
}
```

**After:**
```typescript
async *streamAsync(payload: any): AsyncIterableIterator<AsyncStreamMessage<Token>> {
  yield* this.createAsyncStream(this.tokens);
}
```

## Best Practices

1. **Always handle all event types** (`data`, `end`, `error`)
2. **Use AbortController for user-initiated cancellations**
3. **Set appropriate timeouts** to prevent hanging streams
4. **Choose buffer size based on memory constraints** (default 100 is usually good)
5. **Break early if you have all the data you need**
6. **Use helper methods** (`createAsyncStream`) for simple cases
7. **Implement manual generators** only when you need fine-grained control

## Examples

See `src/messaging/__tests__/streaming-example.test.ts` for comprehensive examples including:

- Session message streaming with filtering
- AI token streaming with cancellation
- Large dataset streaming with backpressure
- Early termination patterns
- Timeout handling

## Testing

Run streaming tests:
```bash
bun test src/messaging/__tests__/streaming.test.ts
```

Run examples:
```bash
bun test src/messaging/__tests__/streaming-example.test.ts
```
