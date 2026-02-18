# Signal Hub Protocol Error Handling

**Version:** 0.1.0
**Date:** 2026-02-17
**Status:** Reference Documentation

---

## Overview

This document describes error handling patterns in the Signal Hub protocol, covering both **tell** (fire-and-forget) and **ask** (request-response) messaging patterns.

**Key Concepts:**
- **Tell pattern**: No reply expected, errors logged but not returned
- **Ask pattern**: Reply required, errors returned as `hub:error` messages
- **Error codes**: Standardized codes for different error types
- **Recovery patterns**: How clients should handle each error type

---

## Message Patterns

### Tell Pattern (Fire-and-Forget)

**Characteristics:**
- `pattern: 'tell'` in message
- No reply expected by sender
- Hub processes message but doesn't send acknowledgment
- Errors logged, not returned to sender

**Example:**
```json
{
  "id": "msg-123",
  "type": "hub:heartbeat",
  "pattern": "tell",
  "from": "runtime:browser/actor-456",
  "to": "cloudflare/signal-hub",
  "payload": {},
  "timestamp": 1708198765000
}
```

**Error Handling:**
- Hub logs error
- No `hub:error` sent to client
- Client doesn't know if message failed

**Use Cases:**
- Heartbeats
- Fire-and-forget notifications
- Telemetry/logging

---

### Ask Pattern (Request-Response)

**Characteristics:**
- `pattern: 'ask'` in message
- Reply required (either success or error)
- Hub sends `correlationId` matching original message
- Errors returned as `hub:error` messages

**Example Request:**
```json
{
  "id": "msg-456",
  "type": "hub:subscribe",
  "pattern": "ask",
  "from": "runtime:seag/actor-123",
  "to": "cloudflare/signal-hub",
  "payload": {
    "topic": "events"
  },
  "timestamp": 1708198765000
}
```

**Success Response:**
```json
{
  "id": "msg-789",
  "type": "hub:subscribed",
  "pattern": "tell",
  "from": "cloudflare/signal-hub",
  "to": "runtime:seag/actor-123",
  "correlationId": "msg-456",
  "payload": {
    "topic": "events",
    "subscriptionId": "sub-abc123"
  },
  "timestamp": 1708198765500
}
```

**Error Response:**
```json
{
  "id": "msg-790",
  "type": "hub:error",
  "pattern": "tell",
  "from": "cloudflare/signal-hub",
  "to": "runtime:seag/actor-123",
  "correlationId": "msg-456",
  "payload": {
    "code": "internal_error",
    "message": "topic is required",
    "details": {}
  },
  "timestamp": 1708198765500
}
```

**Use Cases:**
- Registration
- Subscription
- Actor discovery
- Send with acknowledgment

---

## Error Types Catalog

### unknown_actor

**Code:** `unknown_actor`

**When:** Actor address not found in registry

**Scenario:** Client sends message to actor that:
- Has not registered
- Registration expired (TTL exceeded)
- Disconnected and was cleaned up

**Example:**
```json
{
  "type": "hub:error",
  "pattern": "tell",
  "correlationId": "msg-123",
  "payload": {
    "code": "unknown_actor",
    "message": "Actor not found: runtime:browser/actor-456",
    "details": {
      "address": "runtime:browser/actor-456"
    }
  }
}
```

**Recovery:**
1. **Re-register** if this is your own actor
2. **Retry discovery** to find current address
3. **Fail gracefully** if actor truly doesn't exist

**Prevention:**
- Keep registrations fresh (send heartbeats)
- Use discovery before sending to new actors
- Handle `hub:disconnect` notifications

---

### rate_limited

**Code:** `rate_limited`

**When:** Client exceeded rate limits

**Limits:**
- 100 messages/second per session
- 1,000 messages/second per hub
- 10 subscriptions per actor

**Example:**
```json
{
  "type": "hub:error",
  "pattern": "tell",
  "correlationId": "msg-789",
  "payload": {
    "code": "rate_limited",
    "message": "Rate limit exceeded: 100 messages/second",
    "details": {
      "limit": 100,
      "window": 1000,
      "retryAfter": 500
    }
  }
}
```

**Recovery:**
1. **Wait** `retryAfter` milliseconds
2. **Implement backoff** (exponential or linear)
3. **Reduce send rate** going forward

**Prevention:**
- Batch messages when possible
- Use pub/sub for fan-out (1 publish vs N sends)
- Implement client-side rate limiting

---

### internal_error

**Code:** `internal_error`

**When:** Hub encountered unexpected error

**Scenarios:**
- Invalid message format
- Missing required fields
- Type validation failures
- Internal hub bugs

**Example:**
```json
{
  "type": "hub:error",
  "pattern": "tell",
  "correlationId": "msg-456",
  "payload": {
    "code": "internal_error",
    "message": "topic is required",
    "details": {
      "field": "topic",
      "expected": "string",
      "received": "undefined"
    }
  }
}
```

**Recovery:**
1. **Fix client code** (validation error)
2. **Retry** (transient hub error)
3. **Report bug** (hub implementation error)

**Prevention:**
- Validate messages before sending
- Use TypeScript types for message construction
- Test against hub error scenarios

---

### unauthorized

**Code:** `unauthorized`

**When:** JWT invalid, expired, or missing

**Scenarios:**
- No JWT provided
- JWT signature invalid
- JWT expired
- JWT claims invalid

**Example:**
```json
{
  "type": "hub:error",
  "pattern": "tell",
  "correlationId": "msg-123",
  "payload": {
    "code": "unauthorized",
    "message": "JWT expired",
    "details": {
      "reason": "token_expired",
      "expiredAt": 1708198700000
    }
  }
}
```

**Recovery:**
1. **Obtain new JWT** from auth service
2. **Reconnect** with new token
3. **Re-register** actor

**Prevention:**
- Refresh JWT before expiration
- Handle `unauthorized` by triggering re-auth flow
- Store JWT securely (don't log)

---

### duplicate_actor

**Code:** `duplicate_actor`

**When:** Actor address already registered

**Scenario:** Two clients try to register same address simultaneously

**Example:**
```json
{
  "type": "hub:error",
  "pattern": "tell",
  "correlationId": "msg-789",
  "payload": {
    "code": "duplicate_actor",
    "message": "Actor already registered: runtime:seag/actor-123",
    "details": {
      "address": "runtime:seag/actor-123",
      "existingSessionId": "session-xyz"
    }
  }
}
```

**Recovery:**
1. **Wait for old session to expire** (if you're reconnecting)
2. **Use different address** (if this is intentional collision)
3. **Check for stale connections** (disconnect old before reconnecting)

**Prevention:**
- Disconnect old session before reconnecting
- Use unique actor addresses per instance
- Handle duplicate detection (last connection wins)

---

### protocol_version_mismatch

**Code:** `protocol_version_mismatch`

**When:** Client uses unsupported protocol version

**Example:**
```json
{
  "type": "hub:error",
  "pattern": "tell",
  "correlationId": "msg-456",
  "payload": {
    "code": "protocol_version_mismatch",
    "message": "Protocol version not supported: 0.2.0",
    "details": {
      "clientVersion": "0.2.0",
      "hubVersion": "0.1.0",
      "supportedVersions": ["0.1.0"]
    }
  }
}
```

**Recovery:**
1. **Downgrade client** to supported version
2. **Upgrade hub** if client is newer
3. **Negotiate version** during connection

**Prevention:**
- Check hub version before connecting
- Support multiple protocol versions in client
- Document version compatibility

---

## Error Handling Patterns

### Pattern 1: Synchronous Validation (Ask Messages)

**Scenario:** Client sends `hub:subscribe` and waits for reply

**Code:**
```typescript
async function subscribe(topic: string): Promise<string> {
  const msg = {
    id: crypto.randomUUID(),
    type: 'hub:subscribe',
    pattern: 'ask',
    from: this.actorAddress,
    to: 'cloudflare/signal-hub',
    payload: { topic },
    timestamp: Date.now(),
  };

  try {
    const reply = await this.sendAndWaitForReply(msg, 5000);

    if (reply.type === 'hub:error') {
      throw new Error(`Subscribe failed: ${reply.payload.code} - ${reply.payload.message}`);
    }

    return reply.payload.subscriptionId;
  } catch (error) {
    // Handle timeout, network error, or hub error
    console.error('Subscribe failed:', error);
    throw error;
  }
}
```

**Key Points:**
- Always check if reply is `hub:error`
- Extract `code` and `message` from payload
- Throw or handle based on error code
- Set timeout for replies

---

### Pattern 2: Asynchronous Error Handling (Tell Messages)

**Scenario:** Client sends `hub:send` and doesn't wait for reply

**Code:**
```typescript
function send(to: CanonicalAddress, type: string, payload: unknown): void {
  const msg = {
    id: crypto.randomUUID(),
    type: 'hub:send',
    pattern: 'tell',
    from: this.actorAddress,
    to: 'cloudflare/signal-hub',
    payload: { to, type, data: payload },
    timestamp: Date.now(),
  };

  this.ws.send(JSON.stringify(msg));

  // No wait for reply - hub may send hub:error asynchronously
}

// Listen for async errors
client.on('message', (msg: SharedMessage) => {
  if (msg.type === 'hub:error') {
    console.error('Hub error:', msg.payload.code, msg.payload.message);

    // Check correlationId to match with original message
    const originalMessageId = msg.correlationId;

    // Handle based on error code
    switch (msg.payload.code) {
      case 'unknown_actor':
        // Re-discover actor
        break;
      case 'rate_limited':
        // Implement backoff
        break;
      default:
        // Log and continue
        break;
    }
  }
});
```

**Key Points:**
- Listen for `hub:error` messages globally
- Use `correlationId` to match errors with requests
- Handle errors asynchronously
- Don't block on tell messages

---

### Pattern 3: Retry with Backoff

**Scenario:** Client receives `rate_limited` error

**Code:**
```typescript
async function sendWithRetry(
  to: CanonicalAddress,
  type: string,
  payload: unknown,
  maxRetries: number = 3
): Promise<void> {
  let retries = 0;
  let delay = 1000; // Start with 1s delay

  while (retries < maxRetries) {
    try {
      const reply = await this.sendWithAck(to, type, payload);

      if (reply.type === 'hub:error') {
        if (reply.payload.code === 'rate_limited') {
          // Use retryAfter from error if available
          const retryAfter = reply.payload.details?.retryAfter ?? delay;
          console.log(`Rate limited, retrying after ${retryAfter}ms`);
          await sleep(retryAfter);
          retries++;
          delay *= 2; // Exponential backoff
          continue;
        }

        // Non-retryable error
        throw new Error(`Send failed: ${reply.payload.code}`);
      }

      // Success
      return;
    } catch (error) {
      if (retries >= maxRetries - 1) {
        throw error;
      }
      retries++;
      await sleep(delay);
      delay *= 2;
    }
  }

  throw new Error(`Send failed after ${maxRetries} retries`);
}
```

**Key Points:**
- Use `retryAfter` from error details
- Implement exponential backoff
- Limit total retry attempts
- Distinguish retryable vs non-retryable errors

---

### Pattern 4: Circuit Breaker

**Scenario:** Prevent cascading failures when hub is down

**Code:**
```typescript
class CircuitBreaker {
  private failures = 0;
  private lastFailureTime = 0;
  private state: 'closed' | 'open' | 'half-open' = 'closed';
  private threshold = 5;
  private timeout = 60000; // 1 minute

  async execute<T>(fn: () => Promise<T>): Promise<T> {
    if (this.state === 'open') {
      if (Date.now() - this.lastFailureTime > this.timeout) {
        this.state = 'half-open';
      } else {
        throw new Error('Circuit breaker open');
      }
    }

    try {
      const result = await fn();
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      throw error;
    }
  }

  private onSuccess(): void {
    this.failures = 0;
    this.state = 'closed';
  }

  private onFailure(): void {
    this.failures++;
    this.lastFailureTime = Date.now();

    if (this.failures >= this.threshold) {
      this.state = 'open';
    }
  }
}

// Usage
const breaker = new CircuitBreaker();

try {
  await breaker.execute(() => client.subscribe(topic));
} catch (error) {
  console.error('Operation failed (circuit breaker may be open):', error);
}
```

**Key Points:**
- Track consecutive failures
- Open circuit after threshold
- Allow recovery attempts after timeout
- Prevent cascading failures

---

## Error Handling Best Practices

### 1. Always Handle hub:error

**Bad:**
```typescript
const reply = await client.subscribe(topic);
return reply.payload.subscriptionId; // Assumes success
```

**Good:**
```typescript
const reply = await client.subscribe(topic);

if (reply.type === 'hub:error') {
  const { code, message } = reply.payload;
  throw new HubError(code, message);
}

return reply.payload.subscriptionId;
```

---

### 2. Use Correlation IDs

**Bad:**
```typescript
client.on('message', (msg) => {
  if (msg.type === 'hub:error') {
    console.error('Some error happened');
  }
});
```

**Good:**
```typescript
const pendingRequests = new Map<string, { resolve, reject }>();

async function sendWithAck(msg: SharedMessage): Promise<SharedMessage> {
  return new Promise((resolve, reject) => {
    pendingRequests.set(msg.id, { resolve, reject });
    client.send(msg);
  });
}

client.on('message', (msg) => {
  if (msg.correlationId) {
    const pending = pendingRequests.get(msg.correlationId);
    if (pending) {
      pendingRequests.delete(msg.correlationId);

      if (msg.type === 'hub:error') {
        pending.reject(new HubError(msg.payload.code, msg.payload.message));
      } else {
        pending.resolve(msg);
      }
    }
  }
});
```

---

### 3. Implement Timeouts

**Bad:**
```typescript
const reply = await sendAndWaitForReply(msg); // Waits forever
```

**Good:**
```typescript
const reply = await Promise.race([
  sendAndWaitForReply(msg),
  sleep(5000).then(() => {
    throw new Error('Request timeout');
  }),
]);
```

---

### 4. Log Error Details

**Bad:**
```typescript
console.error('Error occurred');
```

**Good:**
```typescript
console.error(JSON.stringify({
  event: 'hub_error',
  code: error.payload.code,
  message: error.payload.message,
  details: error.payload.details,
  correlationId: error.correlationId,
  timestamp: Date.now(),
}));
```

---

### 5. Distinguish Error Types

**Bad:**
```typescript
if (msg.type === 'hub:error') {
  throw new Error('Something failed');
}
```

**Good:**
```typescript
if (msg.type === 'hub:error') {
  const { code } = msg.payload;

  switch (code) {
    case 'unknown_actor':
      // Re-register or re-discover
      await handleUnknownActor(msg);
      break;

    case 'rate_limited':
      // Implement backoff
      await handleRateLimit(msg);
      break;

    case 'unauthorized':
      // Re-authenticate
      await handleUnauthorized(msg);
      break;

    default:
      // Log and continue
      console.error('Unhandled error:', code);
      break;
  }
}
```

---

## Testing Error Scenarios

### Unit Tests

**Test error responses from handlers:**

```typescript
import { describe, it, expect } from 'vitest';
import { handleSubscribe } from '../handlers/pubsub';

describe('handleSubscribe errors', () => {
  it('should throw internal_error if topic missing', () => {
    const msg = createMessage('hub:subscribe', {});

    expect(() => {
      handleSubscribe(msg, subscriptions, actorAddress);
    }).toThrow('topic is required');
  });

  it('should throw internal_error if topic not string', () => {
    const msg = createMessage('hub:subscribe', { topic: 123 });

    expect(() => {
      handleSubscribe(msg, subscriptions, actorAddress);
    }).toThrow('topic is required');
  });
});
```

### Integration Tests

**Test error responses from hub:**

```typescript
import { describe, it, expect } from 'vitest';
import { createSeagActor } from './helpers/seag-actor';

describe('Hub error handling', () => {
  it('should return hub:error for invalid subscribe', async () => {
    const actor = await createSeagActor({ ... });

    const errorPromise = actor.waitForMessage(
      msg => msg.type === 'hub:error',
      5000
    );

    // Send malformed message (missing topic)
    actor.send('cloudflare/signal-hub', 'hub:subscribe', {});

    const error = await errorPromise;
    expect(error.type).toBe('hub:error');
    expect(error.payload.code).toBe('internal_error');
    expect(error.payload.message).toContain('topic');
  });

  it('should return hub:error for unknown actor', async () => {
    const actor = await createSeagActor({ ... });

    const errorPromise = actor.waitForMessage(
      msg => msg.type === 'hub:error',
      5000
    );

    // Send to non-existent actor
    actor.send('runtime:fake/actor-999', 'test:message', { data: 'test' });

    const error = await errorPromise;
    expect(error.type).toBe('hub:error');
    expect(error.payload.code).toBe('unknown_actor');
  });
});
```

---

## Future Enhancements

### Phase 2: Enhanced Error Handling

**Planned improvements:**
1. **Error metrics** - Track error rates by code
2. **Error recovery** - Automatic retry for transient errors
3. **Error aggregation** - Batch error notifications
4. **Error details** - Stack traces for internal errors
5. **Error codes** - Additional error types (quota_exceeded, etc.)

---

## Related Documentation

- **PROTOCOL.md** - Wire protocol specification
- **ARCHITECTURE.md** - System architecture and error handling
- **WEBSOCKET_PATTERNS.md** - WebSocket connection patterns
- **Testing Guide** - Error scenario testing

---

**End of Document**
