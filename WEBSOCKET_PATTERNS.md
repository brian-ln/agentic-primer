# WebSocket/Session/Connection Issue Patterns Research

**Generated:** 2026-02-17 (verified via `date` command)
**Project:** Signal Hub (Cloudflare Durable Objects + WebSocket)
**Purpose:** Document patterns found from other Cloudflare projects and implement prevention strategies

---

## Executive Summary

Researched WebSocket/session/connection management patterns from:
1. Signal Hub's own incident history (GAPS_ANALYSIS.md)
2. Cloudflare Durable Objects reference documentation
3. Recent Signal Hub test failures and fixes

**Key Findings:**
- Session identity mismatch is a common issue (temp IDs vs registered addresses)
- WebSocket reconnection races cause state clobbering
- Event wrapper patterns hide underlying message structure
- Protocol error handling patterns (tell vs ask) need explicit documentation

**Prevention Strategies Implemented:**
1. Session identity verification test
2. JSDoc for wrapper event signatures
3. Assertion logging for subscriptions
4. Protocol error handling documentation

---

## Pattern 1: Session Identity Mismatch

### Problem

**From Signal Hub GAPS_ANALYSIS.md:**
> Tests assumed client stored registry (wrong) → confusion about who cleans up expired actors

**Recent Signal Hub Bug:**
Pub/sub subscriptions used temporary session ID instead of registered actor address, causing subscriptions to fail after registration.

**Root Cause:**
- Session object has two identities: `session.sessionId` (temporary) and `session.actorIdentity` (registered)
- Handlers used wrong identity when looking up subscriptions
- No test verified that `session.actorIdentity` matches registered address

### Cloudflare Pattern

**From `~/.claude/skills/cloudflare/references/durable-objects.md`:**

```typescript
export class DeviceSession extends DurableObject {
  // OAuth device flow session management:
  // - One DO per device code
  // - Stores authentication state
  // - Handles polling from device
  // - Manages expiration
}
```

**Key insight:** "DO provides strong consistency for auth flow state, avoiding race conditions."

### Signal Hub Implementation

```typescript
// SignalHub.ts - Session structure
interface Session {
  sessionId: string;              // Temporary ID (from connection)
  actorIdentity: CanonicalAddress; // Registered address (from hub:register)
  connectionState: ConnectionState;
  connectionId: string;
  lastHeartbeat: number;
}

// CRITICAL: actorIdentity is null until hub:register completes
// Subscriptions MUST use session.actorIdentity, NOT session.sessionId
```

### Prevention Strategy

**Implemented:**
1. New integration test (`session-identity.test.ts`) verifying:
   - `session.actorIdentity` is null before registration
   - `session.actorIdentity` matches registered address after registration
   - Subscriptions use `session.actorIdentity` correctly

**Test Pattern:**
```typescript
it('should use registered address for subscriptions, not session ID', async () => {
  const actor = await createSeagActor({...});

  // Verify actorIdentity matches registered address
  const registry = await getActorRegistry(env.hub);
  expect(registry.has(env.seagAddress)).toBe(true);

  // Subscribe and verify subscription uses correct identity
  await actor.subscribe('test-topic');
  const subscriptions = await getSubscriptions(env.hub);
  expect(subscriptions.get('test-topic')?.has(env.seagAddress)).toBe(true);
});
```

---

## Pattern 2: WebSocket Reconnection Race Conditions

### Problem

**From Signal Hub Recent Fix:**
Old WebSocket close handler clobbering new connection state during reconnection.

**Scenario:**
1. Client connects (WS1)
2. Network hiccup triggers reconnect
3. Client connects again (WS2) - new session created
4. Old WS1 close event fires AFTER WS2 is established
5. Old close handler removes new WS2 from registry

**Impact:**
- New connection appears successful but is not registered
- Messages fail to route to "registered" actor
- Client thinks it's connected, hub thinks it's disconnected

### Cloudflare Pattern

**From `~/.claude/skills/cloudflare/references/durable-objects.md`:**

```typescript
export class ChatRoom extends DurableObject {
  private connections: Set<WebSocket> = new Set();

  async fetch(request: Request): Promise<Response> {
    if (request.headers.get("Upgrade") === "websocket") {
      const pair = new WebSocketPair();
      const [client, server] = Object.values(pair);

      this.ctx.acceptWebSocket(server);
      this.connections.add(server);

      return new Response(null, { status: 101, webSocket: client });
    }
    return new Response("Not found", { status: 404 });
  }

  async webSocketMessage(ws: WebSocket, message: string): Promise<void> {
    // Broadcast to all
    for (const conn of this.connections) {
      conn.send(message);
    }
  }
}
```

**Key characteristics:**
- `ctx.acceptWebSocket(server)` is CRITICAL for routing
- Connections tracked in Set
- No explicit close handling shown (relies on Durable Objects lifecycle)

### Signal Hub Implementation

```typescript
// SignalHub.ts - Duplicate connection handling
private handleDuplicateConnection(
  actorIdentity: CanonicalAddress,
  newSessionId: string,
  newWs: WebSocket
): void {
  // Find existing session with same actorIdentity
  for (const [ws, session] of this.sessions.entries()) {
    if (
      session.actorIdentity === actorIdentity &&
      session.sessionId !== newSessionId &&
      session.connectionState !== 'disconnected'
    ) {
      // Send disconnect notification to old session
      // Close old WebSocket
      // Clean up old session
      this.cleanupConnection(ws, session);
    }
  }
}

// CRITICAL: cleanupConnection checks session.connectionState
// before removing from registry to avoid clobbering new connections
async cleanupConnection(ws: WebSocket, session: Session): Promise<void> {
  // Only cleanup if still connected (avoid race with new connection)
  if (session.connectionState === 'disconnected') {
    return; // Already cleaned up
  }

  // Mark as disconnected FIRST
  session.connectionState = 'disconnected';

  // Then remove from registry
  this.sessions.delete(ws);
  this.registry.delete(session.actorIdentity);
  this.connections.delete(session.connectionId);

  // Clean up subscriptions
  cleanupSubscriptions(this.subscriptions, session.actorIdentity);
}
```

### Prevention Strategy

**Implemented:**
1. Connection state machine prevents race conditions:
   - Mark `connectionState = 'disconnected'` BEFORE cleanup
   - Check `connectionState` before removing from registry
   - Duplicate connection handler explicitly closes old session first

2. Logging for debugging:
   ```typescript
   console.log(JSON.stringify({
     event: 'duplicate_connection_detected',
     actorIdentity,
     oldSessionId: session.sessionId,
     newSessionId,
     timestamp: Date.now(),
   }));
   ```

**Test Pattern:**
```typescript
it('should handle reconnection without losing state', async () => {
  const actor = await createSeagActor({...});
  await actor.subscribe('test-topic');

  // Force reconnect
  await actor.reconnect();

  // Verify subscription persists
  const subscriptions = await getSubscriptions(env.hub);
  expect(subscriptions.get('test-topic')?.has(env.seagAddress)).toBe(true);
});
```

---

## Pattern 3: Event Wrapper API Confusion

### Problem

**From Signal Hub Recent Fix:**
Test wrapper used `.on('message', handler)` pattern but delegated to client library's `.onMessage(handler)` method, causing event signature mismatch.

**Root Cause:**
- Wrapper provides `.on(event, handler)` API for consistency
- Underlying client has `.onMessage(handler)` method with different signature
- Tests expected `(message)` but got `(event: MessageEvent)`
- No JSDoc explaining the translation

### Cloudflare Pattern

**No direct pattern found** - this is specific to test infrastructure, not Cloudflare platform.

However, Cloudflare's WebSocket API is straightforward:
```typescript
webSocketMessage(ws: WebSocket, message: string | ArrayBuffer): void
```

Signal Hub wraps this in client libraries (browser, SEAG) for convenience.

### Signal Hub Implementation

**Before (broken):**
```typescript
// seag-actor.ts
class SeagActorWrapper {
  on(event: 'message', handler: (msg: SharedMessage) => void): void {
    // WRONG: onMessage receives MessageEvent, not SharedMessage
    this.client.onMessage(handler);
  }
}
```

**After (fixed):**
```typescript
// seag-actor.ts
class SeagActorWrapper {
  /**
   * Register event listener
   * @param event - Event name ('message', 'connected', 'disconnected', 'error')
   * @param handler - Event handler receiving SharedMessage (not MessageEvent)
   */
  on(event: 'message', handler: (msg: SharedMessage) => void): void {
    this.client.onMessage((evt: MessageEvent) => {
      const msg = JSON.parse(evt.data as string) as SharedMessage;
      handler(msg);
    });
  }
}
```

### Prevention Strategy

**Implemented:**
1. JSDoc comments on all wrapper methods explaining:
   - Event signature transformation
   - Parameter types (SharedMessage vs MessageEvent)
   - When events fire

2. Example JSDoc:
   ```typescript
   /**
    * Wait for message matching predicate
    * @param predicate - Function receiving SharedMessage (already parsed from JSON)
    * @param timeout - Timeout in milliseconds
    * @returns Promise resolving to matched SharedMessage
    *
    * @example
    * const msg = await actor.waitForMessage(
    *   msg => msg.type === 'hub:registered',
    *   5000
    * );
    * expect(msg.payload.address).toBe('runtime:seag/actor-123');
    */
   ```

**Documentation Pattern:**
- Explain what the method does
- Specify parameter types explicitly
- Call out transformations (MessageEvent → SharedMessage)
- Provide usage examples

---

## Pattern 4: Protocol Error Handling (Tell vs Ask)

### Problem

**From Signal Hub GAPS_ANALYSIS.md:**
> "How are rate limit errors communicated?" → hub:error with code 'rate_limited', includes retryAfter in details

**Recent Signal Hub Bug:**
Tests sent `hub:subscribe` with `pattern: 'ask'` expecting synchronous reply, but handler threw error instead of returning error message.

**Root Cause:**
- Hub protocol defines `tell` (no reply expected) vs `ask` (reply required)
- Some handlers throw HubError (breaks ask pattern)
- No documentation explaining when errors are returned vs thrown

### Cloudflare Pattern

**From `~/.claude/skills/cloudflare/references/durable-objects.md`:**

```typescript
async webSocketMessage(ws: WebSocket, message: string): Promise<void> {
  // Broadcast to all
  for (const conn of this.connections) {
    conn.send(message);
  }
}
```

**No explicit error handling shown** - Durable Objects rely on try/catch in handlers.

However, Cloudflare's RPC pattern (also from DO docs) shows:
```typescript
// RPC call with response
const result = await stub.fetch(request);
```

This implies request/response pairs, similar to Signal Hub's `ask` pattern.

### Signal Hub Implementation

**Pattern: Tell vs Ask**

```typescript
// Tell pattern (no reply expected)
{
  pattern: 'tell',
  type: 'hub:heartbeat',
  // No reply sent
}

// Ask pattern (reply required)
{
  pattern: 'ask',
  type: 'hub:subscribe',
  // Handler MUST return reply or error
}
```

**Error Handling:**
```typescript
// In handler - throw for internal errors
if (!payload.topic) {
  throw new HubError('internal_error', 'topic is required');
}

// In webSocketMessage - catch and send hub:error
try {
  const reply = handleSubscribe(msg, ...);
  ws.send(JSON.stringify(reply));
} catch (error) {
  const errorReply = createErrorReply(error, msg);
  ws.send(JSON.stringify(errorReply));
}
```

### Prevention Strategy

**Implemented:**
1. Documentation of protocol error patterns in new PROTOCOL_ERRORS.md:
   - Tell pattern: Errors logged, no reply
   - Ask pattern: Errors returned as hub:error
   - Error types catalog (unknown_actor, rate_limited, etc.)
   - Recovery patterns for each error type

2. Example documentation:
   ```markdown
   ## Error Pattern: Ask Messages

   **Scenario:** Client sends `hub:subscribe` with `pattern: 'ask'`

   **Success Response:**
   ```json
   {
     "type": "hub:subscribed",
     "pattern": "tell",
     "correlationId": "original-message-id",
     "payload": {
       "topic": "events",
       "subscriptionId": "sub-123"
     }
   }
   ```

   **Error Response:**
   ```json
   {
     "type": "hub:error",
     "pattern": "tell",
     "correlationId": "original-message-id",
     "payload": {
       "code": "internal_error",
       "message": "topic is required",
       "details": {}
     }
   }
   ```
   ```

**Documentation Structure:**
- Error types catalog
- Tell vs ask pattern explanation
- Recovery patterns (retry, fail, backoff)
- Examples for each error type

---

## Pattern 5: Assertion Logging for Debugging

### Problem

**From Signal Hub GAPS_ANALYSIS.md:**
> "How do I debug message routing?" → No observability documentation

**Recent Need:**
Need to verify subscriptions are using correct actor identity without reading Durable Object internal state.

### Cloudflare Pattern

**From `~/.claude/skills/cloudflare/references/debugging.md`:**
(File was empty in search results, but Cloudflare recommends `console.log` for DO debugging)

```bash
wrangler tail  # Stream live logs
```

### Signal Hub Implementation

**Assertion Logging Added:**

```typescript
// pubsub.ts - handleSubscribe
export function handleSubscribe(
  msg: SharedMessage,
  subscriptions: Map<string, Set<CanonicalAddress>>,
  actorAddress: CanonicalAddress
): SharedMessage {
  // ... validation ...

  subscribers.add(actorAddress);

  // ASSERTION LOG: Verify correct identity used
  console.log(JSON.stringify({
    event: 'subscription_added',
    actorAddress,       // Should be registered address, not temp session ID
    topic,
    subscriberCount: subscribers.size,
    timestamp: Date.now(),
  }));

  return createReply('hub:subscribed', {...}, msg, SIGNAL_HUB_ADDRESS);
}
```

**Benefits:**
- Structured JSON logs (easy to parse)
- Verifiable in tests via `wrangler tail` or test output
- Shows actual values used (not inferred from code)
- Helps diagnose identity mismatch issues

### Prevention Strategy

**Implemented:**
1. Assertion logs added to critical operations:
   - Subscription creation (logs `actorAddress`)
   - Message delivery (logs `from`, `to`, `type`)
   - Registration (logs `actorIdentity`)
   - Cleanup (logs what was removed)

2. Log format:
   ```typescript
   console.log(JSON.stringify({
     event: 'operation_name',
     key: 'value',
     timestamp: Date.now(),
   }));
   ```

**Testing Pattern:**
```typescript
it('should log subscription with correct actor address', async () => {
  const actor = await createSeagActor({...});
  await actor.subscribe('test-topic');

  // Check logs contain actorAddress (not sessionId)
  // In real tests, use wrangler tail or test harness to capture logs
});
```

---

## Summary of Prevention Strategies

### 1. Session Identity Verification Test ✅

**File:** `tests/integration/signal-hub/session-identity.test.ts`

**Verifies:**
- `session.actorIdentity` is null before registration
- `session.actorIdentity` matches registered address after registration
- Subscriptions use `actorIdentity`, not `sessionId`

**Pattern:**
```typescript
const registry = await getActorRegistry(env.hub);
expect(registry.has(env.seagAddress)).toBe(true);

const subscriptions = await getSubscriptions(env.hub);
expect(subscriptions.get('test-topic')?.has(env.seagAddress)).toBe(true);
```

### 2. JSDoc for Wrapper Event Signatures ✅

**Files:**
- `tests/integration/signal-hub/helpers/seag-actor.ts`
- `tests/integration/signal-hub/helpers/browser-actor.ts`

**Documents:**
- Event signature transformations (MessageEvent → SharedMessage)
- Parameter types
- Usage examples

**Pattern:**
```typescript
/**
 * Register event listener
 * @param event - Event name ('message', 'connected', 'disconnected', 'error')
 * @param handler - Event handler receiving SharedMessage (not MessageEvent)
 *
 * @example
 * actor.on('message', (msg: SharedMessage) => {
 *   console.log('Received:', msg.type);
 * });
 */
on(event: 'message', handler: (msg: SharedMessage) => void): void {
  // Implementation...
}
```

### 3. Assertion Logging for Subscriptions ✅

**File:** `services/signal-hub/src/handlers/pubsub.ts`

**Logs:**
- Subscription creation with `actorAddress`
- Publication delivery with `from`, `to`, `type`
- Subscription cleanup

**Pattern:**
```typescript
console.log(JSON.stringify({
  event: 'subscription_added',
  actorAddress,
  topic,
  subscriberCount: subscribers.size,
  timestamp: Date.now(),
}));
```

### 4. Protocol Error Handling Documentation ✅

**File:** `services/signal-hub/docs/PROTOCOL_ERRORS.md` (new)

**Documents:**
- Tell vs ask pattern error handling
- Error types catalog
- Recovery patterns
- Examples for each error type

**Structure:**
```markdown
## Error Type: unknown_actor

**Code:** `unknown_actor`
**When:** Actor address not found in registry
**Response:** hub:error
**Recovery:** Re-register actor, retry message

**Example:**
{
  "type": "hub:error",
  "payload": {
    "code": "unknown_actor",
    "message": "Actor not found: runtime:browser/actor-456"
  }
}
```

---

## Lessons Learned

### For WebSocket/Session Management

1. **Always track two identities**: temporary (session ID) and permanent (registered address)
2. **Use state machines for connection lifecycle**: prevents race conditions
3. **Log structured JSON for debugging**: makes verification easier
4. **Document event signature transformations**: wrapper APIs hide complexity
5. **Explicit error handling patterns**: tell vs ask requires different approaches

### For Cloudflare Durable Objects

1. **`ctx.acceptWebSocket()` is mandatory**: for message routing in wrangler dev
2. **Strong consistency is a feature**: use it to avoid auth race conditions
3. **One DO per entity**: e.g., one per chat room, device session, or signal hub
4. **WebSocket hibernation**: manage connections at scale
5. **Alarms for cleanup**: scheduled tasks per DO instance

### For Testing

1. **Verify state ownership**: test that state lives where you think it does
2. **Test reconnection scenarios**: common source of race conditions
3. **Check logs in tests**: assertion logs prove correct behavior
4. **Document wrapper APIs**: JSDoc prevents signature confusion
5. **Test error handling**: both tell and ask patterns

---

## References

### Internal Signal Hub Documentation

- `/Users/bln/play/agentic-primer/services/signal-hub/GAPS_ANALYSIS.md` - Architecture gaps analysis
- `/Users/bln/play/agentic-primer/services/signal-hub/ARCHITECTURE.md` - System architecture
- `/Users/bln/play/agentic-primer/services/signal-hub/docs/PROTOCOL.md` - Wire protocol spec

### Cloudflare Documentation

- `~/.claude/skills/cloudflare/references/durable-objects.md` - DO patterns
- `~/.claude/skills/cloudflare/references/debugging.md` - Debugging strategies
- Official: https://developers.cloudflare.com/durable-objects/

### Related Patterns

- OAuth device flow sessions (DeviceSession DO)
- Chat room broadcasting (ChatRoom DO)
- Real-time coordination (Brain DO with WebSockets + alarms)

---

**End of Document**
