# Browser Client Implementation TODO

This document outlines the methods that need to be added to the browser client (`packages/signal-hub-client/src/SignalHubClient.ts`) to enable full integration test coverage.

## Missing Methods

### 1. Discovery: `discover()`

**Purpose:** Discover actors by capability filter

**SEAG Client Reference:** `/Users/bln/play/agentic-primer/ugs/src/messaging/signal-hub/client.ts` (search for "hub:discover")

**Signature:**
```typescript
async discover(
  from: CanonicalAddress,
  capability?: string
): Promise<ActorInfo[]>
```

**Implementation Approach:**
```typescript
async discover(from: CanonicalAddress, capability?: string): Promise<ActorInfo[]> {
  if (!this.connected) {
    throw new Error('Not connected to Signal Hub');
  }

  const discoverMsg: SharedMessage = {
    id: crypto.randomUUID(),
    from,
    to: '@(cloudflare/signal-hub)' as CanonicalAddress,
    type: 'hub:discover',
    pattern: 'ask',
    correlationId: null,
    timestamp: Date.now(),
    payload: capability ? { capability } : null,
    metadata: {},
    ttl: 5000,
    signature: null,
  };

  await this.sendMessage(discoverMsg);

  // Wait for hub:actor_list response
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      this.off('message', handler);
      reject(new Error('Discovery timeout'));
    }, 5000);

    const handler = (event: HubMessageEvent) => {
      if (
        event.message.type === 'hub:actor_list' &&
        event.message.correlationId === discoverMsg.id
      ) {
        clearTimeout(timeout);
        this.off('message', handler);
        resolve(event.message.payload.actors || []);
      }
    };

    this.on('message', handler);
  });
}
```

**Test Validation:** `discovery.test.ts`

---

### 2. Broadcast: `broadcast()`

**Purpose:** Send message to all connected actors

**SEAG Client Reference:** Same file (search for "hub:broadcast")

**Signature:**
```typescript
async broadcast(
  from: CanonicalAddress,
  type: string,
  data: unknown
): Promise<void>
```

**Implementation Approach:**
```typescript
async broadcast(from: CanonicalAddress, type: string, data: unknown): Promise<void> {
  if (!this.connected) {
    throw new Error('Not connected to Signal Hub');
  }

  const broadcastMsg: SharedMessage = {
    id: crypto.randomUUID(),
    from,
    to: '@(cloudflare/signal-hub)' as CanonicalAddress,
    type: 'hub:broadcast',
    pattern: 'tell',
    correlationId: null,
    timestamp: Date.now(),
    payload: {
      type,
      data,
    },
    metadata: {},
    ttl: null,
    signature: null,
  };

  await this.sendMessage(broadcastMsg);
}
```

**Test Validation:** `broadcast.test.ts`

---

### 3. Subscribe: `subscribe()`

**Purpose:** Subscribe to a topic

**SEAG Client Reference:** Same file (search for "hub:subscribe")

**Signature:**
```typescript
async subscribe(
  from: CanonicalAddress,
  topic: string
): Promise<void>
```

**Implementation Approach:**
```typescript
async subscribe(from: CanonicalAddress, topic: string): Promise<void> {
  if (!this.connected) {
    throw new Error('Not connected to Signal Hub');
  }

  const subscribeMsg: SharedMessage = {
    id: crypto.randomUUID(),
    from,
    to: '@(cloudflare/signal-hub)' as CanonicalAddress,
    type: 'hub:subscribe',
    pattern: 'ask',
    correlationId: null,
    timestamp: Date.now(),
    payload: {
      topic,
    },
    metadata: {},
    ttl: 5000,
    signature: null,
  };

  await this.sendMessage(subscribeMsg);

  // Wait for hub:subscribed response
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      this.off('message', handler);
      reject(new Error('Subscribe timeout'));
    }, 5000);

    const handler = (event: HubMessageEvent) => {
      if (
        event.message.type === 'hub:subscribed' &&
        event.message.correlationId === subscribeMsg.id
      ) {
        clearTimeout(timeout);
        this.off('message', handler);
        resolve();
      } else if (
        event.message.type === 'hub:error' &&
        event.message.correlationId === subscribeMsg.id
      ) {
        clearTimeout(timeout);
        this.off('message', handler);
        const error = event.message.payload as HubError;
        reject(new Error(`Subscribe failed: ${error.message}`));
      }
    };

    this.on('message', handler);
  });
}
```

**Test Validation:** `pubsub.test.ts`

---

### 4. Unsubscribe: `unsubscribe()`

**Purpose:** Unsubscribe from a topic

**Signature:**
```typescript
async unsubscribe(
  from: CanonicalAddress,
  topic: string
): Promise<void>
```

**Implementation Approach:**
```typescript
async unsubscribe(from: CanonicalAddress, topic: string): Promise<void> {
  if (!this.connected) {
    throw new Error('Not connected to Signal Hub');
  }

  const unsubscribeMsg: SharedMessage = {
    id: crypto.randomUUID(),
    from,
    to: '@(cloudflare/signal-hub)' as CanonicalAddress,
    type: 'hub:unsubscribe',
    pattern: 'tell',
    correlationId: null,
    timestamp: Date.now(),
    payload: {
      topic,
    },
    metadata: {},
    ttl: null,
    signature: null,
  };

  await this.sendMessage(unsubscribeMsg);
}
```

**Test Validation:** `pubsub.test.ts`

---

### 5. Publish: `publish()`

**Purpose:** Publish message to a topic

**Signature:**
```typescript
async publish(
  from: CanonicalAddress,
  topic: string,
  type: string,
  data: unknown
): Promise<void>
```

**Implementation Approach:**
```typescript
async publish(
  from: CanonicalAddress,
  topic: string,
  type: string,
  data: unknown
): Promise<void> {
  if (!this.connected) {
    throw new Error('Not connected to Signal Hub');
  }

  const publishMsg: SharedMessage = {
    id: crypto.randomUUID(),
    from,
    to: '@(cloudflare/signal-hub)' as CanonicalAddress,
    type: 'hub:publish',
    pattern: 'tell',
    correlationId: null,
    timestamp: Date.now(),
    payload: {
      topic,
      type,
      data,
    },
    metadata: {},
    ttl: null,
    signature: null,
  };

  await this.sendMessage(publishMsg);
}
```

**Test Validation:** `pubsub.test.ts`

---

## Implementation Checklist

- [ ] Add `discover()` method
- [ ] Add `broadcast()` method
- [ ] Add `subscribe()` method
- [ ] Add `unsubscribe()` method
- [ ] Add `publish()` method
- [ ] Update browser-actor wrapper to use real methods
- [ ] Remove stub implementations from wrapper
- [ ] Run `discovery.test.ts` - should pass
- [ ] Run `broadcast.test.ts` - should pass
- [ ] Run `pubsub.test.ts` - should pass
- [ ] Run full test suite - all should pass

## Type Definitions

You may need to add these type definitions to `packages/signal-hub-client/src/types.ts`:

```typescript
export interface ActorInfo {
  address: CanonicalAddress;
  capabilities: string[];
  metadata?: Record<string, unknown>;
  expiresAt?: number;
}

export interface HubError {
  code: string;
  message: string;
  details?: unknown;
}
```

## Testing Locally

After implementing methods:

```bash
cd tests/integration/signal-hub
pnpm install
pnpm test:discovery
pnpm test:broadcast
pnpm test:pubsub
```

## Reference Implementation

The SEAG client has full implementations of all these methods. Use it as a reference:

```
/Users/bln/play/agentic-primer/ugs/src/messaging/signal-hub/client.ts
```

Key sections to review:
- Lines 290-360: Actor registration
- Lines 400-500: Discovery, broadcast, pub/sub
- Lines 550-650: Message handling patterns

## Notes

- All methods follow the same pattern: create message, send, optionally wait for response
- Use `pattern: 'ask'` for methods that need acknowledgment
- Use `pattern: 'tell'` for fire-and-forget
- Always check `this.connected` before sending
- Set reasonable timeouts (5s for most operations)
- Handle both success and error responses in promise handlers
