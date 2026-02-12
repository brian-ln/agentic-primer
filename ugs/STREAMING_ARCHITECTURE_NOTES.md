# Streaming Architecture - Channel Primitive Insight

**Date:** 2026-02-04
**Context:** Implementation of AsyncIterator streaming (simplify-y0g)
**Key Insight:** AsyncIterator is conceptually a unidirectional channel primitive

## Core Observation

AsyncIterator provides the same semantics as channel primitives in CSP/Go/Rust:

**AsyncIterator = Pull-based Channel**
```typescript
// Consumer-controlled pace (backpressure built-in)
for await (const item of stream) {
  // Process at consumer's speed
}
```

**Channel Properties Provided:**
1. **Backpressure** - Consumer pulls at their pace
2. **Cancellation** - AbortSignal = channel close
3. **One-way flow** - Producer → Consumer
4. **Clean shutdown** - Iterator protocol handles completion
5. **Buffering** - Producer can buffer when consumer is slow

## Implementation Considerations

When implementing AsyncIterator streaming support, consider:

### 1. Channel Semantics
- AsyncIterator is a **bounded buffered channel** with pull-based consumption
- Buffer size controls memory usage
- Pull-based = natural backpressure (no explicit flow control needed)

### 2. Cancellation = Close
```typescript
// AbortSignal is channel close signal
const controller = new AbortController();
const stream = actor.streamAsync(payload, { signal: controller.signal });

// Close channel
controller.abort();
```

### 3. Error Propagation
- Errors in channel = iterator throws
- Clean shutdown = iterator returns `{ done: true }`
- Partial completion handled gracefully

## Architectural Patterns

We have three complementary channel patterns emerging:

### 1. Point-to-Point Stream (AsyncIterator)
- **Use case:** Token streaming, large result sets, real-time updates
- **Pattern:** 1 producer → 1 consumer
- **Backpressure:** Pull-based (consumer controls)
- **Example:** SessionActor streaming tokens

### 2. Pub/Sub Broadcast (EventBus)
- **Use case:** Event broadcasting, notifications
- **Pattern:** 1 producer → N consumers
- **Backpressure:** Push-based (producer controls)
- **Example:** EventBusActor (simplify-c49)

### 3. External Bridge (ChannelActor)
- **Use case:** WhatsApp, Telegram, external messaging
- **Pattern:** Bidirectional bridge
- **Backpressure:** External system controls
- **Example:** ChannelActor (simplify-dix)

## Unification Opportunity

All three patterns could be unified under a `Channel<T>` abstraction:

```typescript
interface Channel<T> {
  // AsyncIterator for consumption
  [Symbol.asyncIterator](): AsyncIterableIterator<T>;

  // Send to channel (for bidirectional)
  send?(value: T): Promise<void>;

  // Close channel
  close(): void;

  // Channel state
  closed: boolean;
  buffered: number;
}

// Point-to-point
class StreamChannel<T> implements Channel<T> { ... }

// Pub/sub
class BroadcastChannel<T> implements Channel<T> { ... }

// External bridge
class ExternalChannel<T> implements Channel<T> { ... }
```

## Benefits of Channel Abstraction

1. **Unified interface** - All streaming uses same API
2. **Composability** - Channels can be chained/merged
3. **Type safety** - Generic `Channel<T>` ensures correct types
4. **Testing** - Mock channels for testing
5. **Patterns** - Fan-out, merge, pipe, filter

## Implementation Notes for AsyncIterator Agent

When implementing AsyncIterator support:

1. **Think of it as a channel** - not just an iterator
2. **Buffer management** - size limits, overflow handling
3. **Cancellation** - AbortSignal integration
4. **Error propagation** - channel errors = iterator throws
5. **Clean shutdown** - proper resource cleanup

## Future Extensions

Once AsyncIterator streaming is working, consider:

1. **Channel patterns** - fan-out, merge, buffer operators
2. **Backpressure strategies** - drop, block, buffer, sample
3. **Channel adapters** - Convert between pattern types
4. **Monitoring** - Channel metrics (throughput, buffer size)
5. **Debugging** - Channel visualization/tracing

## References

- **Bead:** simplify-y0g (AsyncIterator streaming)
- **Related:** simplify-c49 (EventBusActor), simplify-dix (ChannelActor)
- **Pattern:** CSP (Communicating Sequential Processes)
- **Inspiration:** Go channels, Rust channels, Node.js streams

---

**Action Items:**

- [ ] Implement AsyncIterator as channel primitive
- [ ] Document channel semantics in code
- [ ] Consider Channel<T> abstraction for future work
- [ ] Add backpressure tests (slow consumer, buffer overflow)
- [ ] Verify cancellation = clean channel close
