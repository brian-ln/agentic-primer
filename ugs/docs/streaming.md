# Real-Time Streaming Pattern

## Overview

The simplify-message-layer project implements a **callback-based streaming pattern** for real-time token delivery from LLM inference. This allows actors to stream responses progressively as tokens arrive, rather than waiting for the complete response.

## Architecture

### Components

1. **StreamCallback<T>** - Generic callback type for streaming events
2. **StreamOptions<R>** - Configuration for streaming requests
3. **TokenStreamEvent** - Event structure for token streaming
4. **Router.streamAsk()** - Method to invoke streaming on actors
5. **Actor.stream()** - Optional method that actors implement for streaming support
6. **SessionActor.stream()** - Implementation that forwards model tokens

### Type Definitions

```typescript
// Callback type for streaming events
export type StreamCallback<T> = (event: T) => void | Promise<void>;

// Options for streaming requests
export interface StreamOptions<R> {
  onChunk: StreamCallback<R>;
  timeout?: number;
}

// Token stream event structure
export interface TokenStreamEvent {
  type: 'token' | 'done' | 'error';
  content?: string;
  error?: string;
  timestamp: number;
}
```

### Flow

```
Client
  └─> Router.streamAsk(target, type, payload, { onChunk })
      └─> Actor.stream(payload, onChunk)
          └─> ModelManager.invokeModel(id, { stream: true, onToken })
              └─> Token arrives from LLM
                  └─> onToken(token)
                      └─> onChunk({ type: 'token', content: token })
                          └─> Client receives token in real-time
```

## Usage Example

### Basic Streaming

```typescript
import { MessageRouter } from './src/messaging/router.ts';
import { SessionActor } from './src/messaging/actors/session.ts';
import { address } from './src/messaging/message.ts';

// Create router and session actor with ModelManager
const router = new MessageRouter(store, programManager);
const sessionActor = new SessionActor(
  'my-session',
  sessionManager,
  programManager,
  store,
  router,
  modelManager  // Required for streaming support
);

// Register actor
router.registerActor('my-session', sessionActor);

// Stream inference request
await router.streamAsk(
  address('my-session'),
  'inference',
  {
    message: 'Explain quantum computing',
    system: 'You are a helpful assistant.',
  },
  {
    onChunk: async (event) => {
      if (event.type === 'token' && event.content) {
        // Display token in real-time
        process.stdout.write(event.content);
      } else if (event.type === 'done') {
        console.log('\nStreaming complete!');
      } else if (event.type === 'error') {
        console.error(`Error: ${event.error}`);
      }
    },
  }
);
```

### Progressive Display

The key feature of this pattern is **progressive output** - tokens are displayed as they arrive, not batched:

```typescript
let fullResponse = '';
let tokenCount = 0;

await router.streamAsk(
  address('session-id'),
  'inference',
  { message: 'Tell me a story' },
  {
    onChunk: async (event) => {
      if (event.type === 'token' && event.content) {
        // Immediate display (no buffering)
        process.stdout.write(event.content);
        fullResponse += event.content;
        tokenCount++;
      }
    },
  }
);

console.log(`\n\nReceived ${tokenCount} tokens`);
```

## Implementation Details

### SessionActor.stream()

The SessionActor implementation:
1. Validates payload contains a message
2. Retrieves session to get model configuration
3. Invokes ModelManager with streaming enabled
4. Forwards tokens via callback
5. Handles completion and errors gracefully

```typescript
async stream(payload: any, onChunk: StreamCallback<TokenStreamEvent>): Promise<void> {
  try {
    const message = payload.message || payload.content || payload;
    const session = this.sessionManager.getSession(this.sessionId);
    const modelId = session.model.match(/@\(([^)]+)\)/)?.[1] || session.model;

    await this.modelManager.invokeModel(modelId, {
      message,
      system: payload.system,
      situation: payload.situation,
      stream: true,
      onToken: async (token: string) => {
        await onChunk({
          type: 'token',
          content: token,
          timestamp: Date.now(),
        });
      },
    });

    await onChunk({ type: 'done', timestamp: Date.now() });
  } catch (error: any) {
    await onChunk({ type: 'error', error: error.message, timestamp: Date.now() });
  }
}
```

### Router.streamAsk()

The router's streaming method:
1. Resolves target actor by address
2. Checks if actor supports streaming (has `stream()` method)
3. Invokes actor's stream method with callback
4. Throws error if actor doesn't support streaming

```typescript
async streamAsk<T, R>(
  to: Address,
  type: string,
  payload: T,
  options: StreamOptions<R>
): Promise<void> {
  const targetId = parseAddress(to);
  const actor = this.actorRegistry.get(targetId);

  if (!actor || typeof actor.stream !== 'function') {
    throw new Error(`Actor ${targetId} does not support streaming`);
  }

  await actor.stream(payload, options.onChunk);
}
```

## Event Types

### Token Event
Emitted for each token chunk received from the model:
```typescript
{
  type: 'token',
  content: 'Hello',  // The token text
  timestamp: 1738595000000
}
```

### Done Event
Emitted when streaming completes successfully:
```typescript
{
  type: 'done',
  timestamp: 1738595001000
}
```

### Error Event
Emitted when an error occurs during streaming:
```typescript
{
  type: 'error',
  error: 'Model not found: my-model',
  timestamp: 1738595000500
}
```

## Running the Demo

Execute the streaming demo to see real-time token output:

```bash
bun run demo-streaming.ts
```

The demo will:
1. Create a SessionActor with streaming support
2. Send an inference request via Router.streamAsk()
3. Display tokens progressively as they arrive
4. Show completion status and statistics

### Expected Output

```
🌊 Real-time Streaming Demo

📝 Setting up provider and model...
✓ Provider published: @(stream-provider)
✓ Model published: @(stream-model)

📝 Creating session...
✓ Session created: @(stream-session)

✓ SessionActor initialized with streaming support

💬 Streaming inference request...
Question: "Explain how actors work in distributed systems in 2 sentences."

Response (streaming):
---
Actors are independent computational entities that process messages asynchronously...
---

✓ Streaming complete! Received 47 token chunks

📊 Streaming Statistics:
  • Total tokens: 47
  • Full response length: 234 chars
  • Streaming pattern: callback-based
  • Real-time display: ✓ Progressive output
```

## Comparison with Batch Mode

| Feature | Batch Mode | Streaming Mode |
|---------|-----------|----------------|
| **Latency** | Wait for full response | Immediate token display |
| **User Experience** | Loading spinner | Progressive output |
| **Memory** | Buffer entire response | Process tokens on arrival |
| **Cancellation** | After completion | During streaming |
| **Implementation** | Simple (ask pattern) | Requires stream() method |

## Requirements

### For SessionActor Streaming

1. **ModelManager** must be passed to SessionActor constructor
2. **Session** must have a model configured
3. **Model** must be published and support inference
4. **Provider** must be configured with credentials

### For Custom Actors

To add streaming support to a custom actor:

```typescript
class MyActor extends Actor {
  async receive(message: Message): Promise<MessageResponse> {
    // Regular request-response handling
  }

  async stream(payload: any, onChunk: StreamCallback<TokenStreamEvent>): Promise<void> {
    try {
      // Process payload
      const data = await this.processStreaming(payload);

      // Emit chunks
      for (const chunk of data) {
        await onChunk({
          type: 'token',
          content: chunk,
          timestamp: Date.now(),
        });
      }

      // Signal completion
      await onChunk({ type: 'done', timestamp: Date.now() });
    } catch (error: any) {
      await onChunk({ type: 'error', error: error.message, timestamp: Date.now() });
    }
  }
}
```

## Future Enhancements (Phase 5)

The current implementation uses **callbacks** for simplicity and immediate functionality. Phase 5 will add:

1. **AsyncIterator Wrapper** - For-await-of syntax support
2. **Backpressure Handling** - Control flow for slow consumers
3. **Stream Cancellation** - Abort in-flight requests
4. **Multiple Consumers** - Fan-out streaming to multiple listeners

Example future API:
```typescript
// Phase 5: AsyncIterator support
for await (const event of router.stream(address('session'), 'inference', payload)) {
  if (event.type === 'token') {
    console.log(event.content);
  }
}
```

## Verification Status

**Callback Pattern**: [VERIFIED: callback mechanism tested]
- ✓ Router.streamAsk() method implementation [VERIFIED: code inspection + demo]
- ✓ SessionActor.stream() implementation [VERIFIED: code inspection + demo]
- ✓ Real-time token callback invocation [VERIFIED: mock streaming demo]
- ✓ Progressive output display (not batched) [VERIFIED: mock tokens displayed individually]
- ✓ Error handling and completion events [VERIFIED: error type tested]
- ✓ Integration with ModelManager.invokeModel({ stream: true }) [VERIFIED: code inspection]

**Real LLM Streaming**: [HYPOTHESIS - requires credentials]
- ⚠️ End-to-end LLM streaming **not tested** in demo due to missing CLOUDFLARE_API_TOKEN
- ⚠️ Mock token streaming demonstrates callback pattern but not actual LLM integration
- ✅ ModelManager.invokeModel() supports streaming (option exists in code)
- 🔄 Real LLM verification requires: provider credentials, published model, active session

**What Was Verified**:
The streaming *infrastructure* works correctly - callbacks fire, tokens are delivered progressively, errors propagate. What remains untested is the complete path from LLM API through to callback with real credentials and network I/O.

## Related Files

- `/src/messaging/message.ts` - Streaming type definitions
- `/src/messaging/router.ts` - Router.streamAsk() implementation
- `/src/messaging/actor.ts` - Actor.stream() interface
- `/src/messaging/actors/session.ts` - SessionActor.stream() implementation
- `/src/entities/model.ts` - ModelManager.invokeModel() with streaming support
- `/demo-streaming.ts` - Complete working example
