# Mailbox Integration

## Overview

The Registry now includes optional mailbox-based message queuing. Messages are enqueued to actor-specific mailboxes and processed asynchronously via a message processing loop.

## Architecture

- **MailboxManager**: Actor that manages mailboxes for other actors
- **Mailbox**: FIFO queue with configurable max size (default: 1000)
- **Processing Loop**: Per-actor interval that dequeues and delivers messages (10ms polling)
- **Promise-based Responses**: Callers receive promises that resolve when messages are processed

## Configuration

### Feature Flag

Mailboxes can be disabled by setting `useMailboxes: false` in Registry constructor:

```typescript
const registry = new Registry();
// Mailboxes enabled by default (useMailboxes = true)
```

When disabled, Registry falls back to direct synchronous message delivery (original behavior).

### Mailbox Configuration

- **Default mailbox size**: 1000 messages
- **Processing interval**: 10ms
- **Lazy creation**: Mailboxes created on first actor registration

## API

### Send Message (Async with Mailbox)

```typescript
const response = await registry.sendTo("actor-id", "message-type", payload);
// With mailboxes: message enqueued, promise resolves when processed
// Response contains actual actor's response
```

### Check Mailbox Status

```typescript
const status = await registry.getMailboxStatus("actor-id");
// Returns: { exists, size, maxSize, isFull, isEmpty, availableCapacity }
```

## Backward Compatibility

- **API unchanged**: `send()` and `sendTo()` signatures remain the same
- **All existing tests pass**: 36 original actor tests + 15 mailbox tests + 4 new integration tests = 55 total
- **Feature flag**: Can disable mailboxes to revert to original synchronous behavior

## Benefits

1. **Async Message Delivery**: Messages queued when actors are busy
2. **FIFO Ordering**: Messages delivered in order received
3. **Backpressure Handling**: Mailbox full errors handled gracefully
4. **Death Detection**: Actor crashes detected during message processing
5. **Monitoring**: Mailbox status queryable for observability

## Implementation Details

### Message Flow

1. Caller: `registry.send(actorId, message)` → Returns promise
2. Registry: Enqueues message to actor's mailbox via MailboxManager
3. Processing loop: Dequeues message every 10ms
4. Registry: Delivers message to actor
5. Registry: Resolves caller's promise with actor's response

### Error Handling

- **Mailbox Full**: Returns error immediately, does not wait
- **Actor Crash**: Emits `actor_died` event, resolves promise with error
- **Enqueue Failure**: Returns error response

## Testing

Run tests:
```bash
bun test src/actors
```

All 55 tests should pass:
- 40 actor tests (including 4 new mailbox integration tests)
- 15 mailbox unit tests

## Future Enhancements

- [ ] Configurable processing interval per actor
- [ ] Priority queues for urgent messages
- [ ] Batch processing for efficiency
- [ ] Metrics/telemetry for mailbox depth and processing time
- [ ] Configurable mailbox size per actor
- [ ] Dead letter queue for repeatedly failing messages
