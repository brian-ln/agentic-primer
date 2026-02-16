# Signal Hub Testing Strategy

**Status:** Phase 7 - Testing Design
**Last Updated:** 2026-02-16
**Priority:** Reconnection scenarios (most fragile area per Haiku review)

---

## Overview

This document defines the testing strategy for Signal Hub, prioritizing the most fragile and high-risk areas identified during architectural review. The strategy covers:

1. **Reconnection Scenarios** (Priority 1) - Most fragile, not easily caught in unit tests
2. **Message Delivery Guarantees** (Priority 2) - Silent failures are catastrophic
3. **Fan-Out & Backpressure** (Priority 3) - Highest risk for CPU exhaustion
4. **Registration Consistency** (Priority 4) - Duplicates cause silent downstream failures
5. **Integration Tests** (Priority 5) - End-to-end SEAG → Hub → Browser validation

Each priority area includes:
- Specific test cases with rationale
- Implementation examples using Miniflare
- Expected outcomes and success criteria
- Risk mitigation strategy

---

## Test Infrastructure

### Miniflare for Durable Objects

Signal Hub uses Cloudflare Durable Objects, which require specialized testing infrastructure:

```typescript
// test/setup/miniflare.ts
import { Miniflare } from 'miniflare';
import { DurableObjectNamespace } from '@cloudflare/workers-types';

export async function createTestEnvironment() {
  const mf = new Miniflare({
    modules: true,
    script: `
      export class SignalHubDO {
        constructor(state, env) {
          this.state = state;
          this.env = env;
        }
        // ... SignalHub implementation
      }

      export default {
        async fetch(request, env) {
          // Route to Durable Object
          const id = env.SIGNAL_HUB.idFromName('test-hub');
          const stub = env.SIGNAL_HUB.get(id);
          return stub.fetch(request);
        }
      }
    `,
    durableObjects: {
      SIGNAL_HUB: 'SignalHubDO'
    }
  });

  return {
    mf,
    env: await mf.getBindings(),
    cleanup: () => mf.dispose()
  };
}
```

### WebSocket Mocking

Simulate WebSocket behavior including disconnects and hibernation:

```typescript
// test/mocks/websocket-mock.ts
export class MockWebSocket {
  private messageHandlers: ((data: any) => void)[] = [];
  private closeHandlers: ((code: number, reason: string) => void)[] = [];
  public readyState: number = WebSocket.CONNECTING;

  constructor() {
    // Simulate connection establishment
    setTimeout(() => {
      this.readyState = WebSocket.OPEN;
    }, 10);
  }

  send(data: string | ArrayBuffer) {
    if (this.readyState !== WebSocket.OPEN) {
      throw new Error('WebSocket is not open');
    }
    // Simulate network delay
    setTimeout(() => {
      // Deliver to server-side handler
      this.onServerMessage?.(data);
    }, 5);
  }

  close(code: number = 1000, reason: string = '') {
    this.readyState = WebSocket.CLOSING;
    setTimeout(() => {
      this.readyState = WebSocket.CLOSED;
      this.closeHandlers.forEach(h => h(code, reason));
    }, 5);
  }

  addEventListener(event: string, handler: any) {
    if (event === 'message') {
      this.messageHandlers.push(handler);
    } else if (event === 'close') {
      this.closeHandlers.push(handler);
    }
  }

  // Simulate abnormal close (network failure)
  simulateNetworkFailure() {
    this.readyState = WebSocket.CLOSED;
    this.closeHandlers.forEach(h => h(1006, 'Connection lost'));
  }

  // Simulate Cloudflare hibernation pause
  simulateHibernation(durationMs: number) {
    const wasOpen = this.readyState === WebSocket.OPEN;
    this.readyState = WebSocket.CLOSED; // Simulate pause

    setTimeout(() => {
      if (wasOpen) {
        this.readyState = WebSocket.OPEN; // Resume
      }
    }, durationMs);
  }

  // Test helper: trigger message handler
  private onServerMessage?: (data: any) => void;

  setServerHandler(handler: (data: any) => void) {
    this.onServerMessage = handler;
  }
}
```

### Network Partition Simulation

Simulate various network conditions:

```typescript
// test/utils/network-simulator.ts
export class NetworkSimulator {
  private latencyMs: number = 0;
  private packetLossRate: number = 0;
  private partitioned: boolean = false;

  setLatency(ms: number) {
    this.latencyMs = ms;
  }

  setPacketLoss(rate: number) {
    this.packetLossRate = rate;
  }

  partition() {
    this.partitioned = true;
  }

  heal() {
    this.partitioned = false;
  }

  async simulateSend<T>(fn: () => Promise<T>): Promise<T> {
    // Simulate partition
    if (this.partitioned) {
      throw new Error('Network partition: no route to host');
    }

    // Simulate packet loss
    if (Math.random() < this.packetLossRate) {
      throw new Error('Network packet lost');
    }

    // Simulate latency
    if (this.latencyMs > 0) {
      await new Promise(r => setTimeout(r, this.latencyMs));
    }

    return fn();
  }
}

// Usage in tests
const network = new NetworkSimulator();
network.setLatency(100); // 100ms RTT
network.setPacketLoss(0.05); // 5% packet loss

const result = await network.simulateSend(() =>
  client.send({ type: 'test', payload: {} })
);
```

---

## Priority 1: Reconnection Scenarios

### Why Priority 1

**Rationale:** Reconnection handling is the most fragile area of WebSocket-based systems. Haiku review identified this as "most fragile; not easily caught in unit tests." Failures in reconnection logic cause:
- Message loss during transition
- Duplicate registrations
- Connection thrashing
- Memory leaks from orphaned resources

### Test Case 1.1: WebSocket Disconnect During Send

**Scenario:** Actor sends message to Signal Hub, WebSocket drops mid-frame, actor retries before reconnect completes.

**Risk:** Message lost silently with no indication to sender.

```typescript
describe('Reconnection - Disconnect During Send', () => {
  it('should detect message loss on disconnect during send', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local');

    // Establish connection and register
    await client.connect();
    await client.register({ actorAddress: 'actor-1' });

    // Send message and immediately disconnect
    const sendPromise = client.send({
      type: 'test-message',
      payload: { data: 'important' },
      requiresAck: true
    });

    // Simulate network failure after send() but before server receives
    setTimeout(() => client.ws.simulateNetworkFailure(), 5);

    // Should throw error or timeout
    await expect(sendPromise).rejects.toThrow(/connection lost|timeout/);

    // Verify client detects failure
    expect(client.connectionState).toBe('disconnected');

    await cleanup();
  });

  it('should retry message after reconnect', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local', {
      retryConfig: { maxRetries: 3, timeoutMs: 1000 }
    });

    await client.connect();
    await client.register({ actorAddress: 'actor-1' });

    let disconnectCount = 0;
    client.on('disconnect', () => disconnectCount++);

    // Send with automatic retry on failure
    const messageId = crypto.randomUUID();
    const sendPromise = client.sendWithRetry({
      id: messageId,
      type: 'test-message',
      payload: { data: 'important' },
      requiresAck: true
    });

    // Disconnect on first attempt
    setTimeout(() => {
      if (disconnectCount === 0) {
        client.ws.simulateNetworkFailure();
      }
    }, 5);

    // Should succeed after reconnect
    const result = await sendPromise;
    expect(result.ack).toBe(true);
    expect(result.messageId).toBe(messageId);
    expect(disconnectCount).toBeGreaterThan(0);

    await cleanup();
  });
});
```

**Expected Outcome:**
- First test: Error detected and propagated to caller
- Second test: Automatic retry succeeds after reconnect
- No silent message loss

### Test Case 1.2: Reconnect After Hibernation

**Scenario:** Signal Hub Durable Object hibernates after 30s idle, client sends message during hibernation, server wakes up.

**Risk:** Messages lost or delayed during hibernation transition.

```typescript
describe('Reconnection - Hibernation', () => {
  it('should preserve connection during hibernation', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local', {
      heartbeatIntervalMs: 25_000 // Send heartbeat before 30s hibernation
    });

    await client.connect();
    await client.register({ actorAddress: 'actor-1' });

    // Wait for hibernation (30s + buffer)
    await new Promise(r => setTimeout(r, 35_000));

    // Send message after hibernation
    const result = await client.send({
      type: 'post-hibernation',
      payload: { test: true }
    });

    // Should succeed - hibernation should be transparent
    expect(result.success).toBe(true);
    expect(client.connectionState).toBe('connected');

    await cleanup();
  });

  it('should reconnect if hibernation breaks connection', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local', {
      heartbeatIntervalMs: 40_000 // Intentionally miss hibernation deadline
    });

    await client.connect();
    await client.register({ actorAddress: 'actor-1' });

    let reconnectCount = 0;
    client.on('reconnect', () => reconnectCount++);

    // Simulate hibernation breaking connection
    setTimeout(() => {
      client.ws.simulateHibernation(30_000);
    }, 5_000);

    // Wait for reconnect
    await new Promise(r => setTimeout(r, 40_000));

    // Should have reconnected
    expect(reconnectCount).toBeGreaterThan(0);
    expect(client.connectionState).toBe('connected');

    await cleanup();
  });
});
```

**Expected Outcome:**
- First test: Heartbeat prevents hibernation disconnect
- Second test: Client detects disconnection and reconnects automatically
- Session state preserved across reconnection

### Test Case 1.3: Duplicate Connection Handling

**Scenario:** Same actor connects twice (e.g., page refresh without closing previous connection).

**Risk:** Broadcast sends duplicate messages, stale registrations cause hard-to-debug failures.

```typescript
describe('Reconnection - Duplicate Connection', () => {
  it('should close first connection when actor connects again', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // First connection
    const client1 = new SignalHubClient('ws://test-hub.local');
    await client1.connect();
    await client1.register({ actorAddress: 'actor-1' });

    expect(client1.connectionState).toBe('connected');

    // Second connection (same actor)
    const client2 = new SignalHubClient('ws://test-hub.local');
    await client2.connect();
    await client2.register({ actorAddress: 'actor-1' });

    // Wait for duplicate resolution
    await new Promise(r => setTimeout(r, 100));

    // First connection should be closed
    expect(client1.connectionState).toBe('disconnected');
    expect(client1.closeReason).toContain('duplicate_connection');

    // Second connection should be active
    expect(client2.connectionState).toBe('connected');

    // Verify only one registration exists
    const registrations = await env.SIGNAL_HUB
      .get(env.SIGNAL_HUB.idFromName('test-hub'))
      .listActors();

    const actor1Regs = registrations.filter(r => r.actorAddress === 'actor-1');
    expect(actor1Regs).toHaveLength(1);
    expect(actor1Regs[0].connectionId).toBe(client2.connectionId);

    await cleanup();
  });

  it('should track connection version for duplicate detection', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const client = new SignalHubClient('ws://test-hub.local');

    // Register, disconnect, register again
    await client.connect();
    await client.register({ actorAddress: 'actor-1' });
    const firstVersion = (await client.getRegistration()).version;

    await client.disconnect();
    await new Promise(r => setTimeout(r, 100));

    await client.connect();
    await client.register({ actorAddress: 'actor-1' });
    const secondVersion = (await client.getRegistration()).version;

    // Version should increment
    expect(secondVersion).toBe(firstVersion + 1);

    // formerConnectionId should be tracked
    const registration = await client.getRegistration();
    expect(registration.formerConnectionId).toBeDefined();

    await cleanup();
  });
});
```

**Expected Outcome:**
- First connection closed with `duplicate_connection` reason
- Second connection established successfully
- Only one active registration per actor address
- Version tracking prevents race conditions

### Test Case 1.4: Heartbeat Timeout Detection

**Scenario:** Actor sends heartbeat, immediately crashes. Hub records heartbeat, cancels timeout, but never hears from actor again until TTL expires.

**Risk:** Memory leak of timeouts, stale actors persisting indefinitely.

```typescript
describe('Reconnection - Heartbeat Timeout', () => {
  it('should expire registration after heartbeat timeout', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local', {
      heartbeatIntervalMs: 30_000,
      heartbeatTimeoutMs: 60_000
    });

    await client.connect();
    await client.register({ actorAddress: 'actor-1' });

    // Verify registered
    let registration = await client.getRegistration();
    expect(registration).toBeDefined();
    expect(registration.actorAddress).toBe('actor-1');

    // Stop sending heartbeats (simulate crash)
    client.stopHeartbeat();

    // Wait for timeout (60s + buffer)
    await new Promise(r => setTimeout(r, 65_000));

    // Registration should be gone
    registration = await client.getRegistration();
    expect(registration).toBeUndefined();

    await cleanup();
  });

  it('should reset timeout on each heartbeat', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local', {
      heartbeatIntervalMs: 25_000,
      heartbeatTimeoutMs: 60_000
    });

    await client.connect();
    await client.register({ actorAddress: 'actor-1' });

    // Send heartbeats for 3 minutes (should not timeout)
    const startTime = Date.now();
    while (Date.now() - startTime < 180_000) {
      await new Promise(r => setTimeout(r, 25_000));
      await client.sendHeartbeat();

      // Verify still registered
      const registration = await client.getRegistration();
      expect(registration).toBeDefined();
    }

    await cleanup();
  });

  it('should clean up heartbeat timers on disconnect', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local');

    await client.connect();
    await client.register({ actorAddress: 'actor-1' });

    // Get handle to Durable Object
    const hubId = env.SIGNAL_HUB.idFromName('test-hub');
    const hubStub = env.SIGNAL_HUB.get(hubId);

    // Verify heartbeat timer exists
    let timers = await hubStub.getHeartbeatTimers();
    expect(timers.has('actor-1')).toBe(true);

    // Disconnect
    await client.disconnect();
    await new Promise(r => setTimeout(r, 100));

    // Timer should be cleaned up
    timers = await hubStub.getHeartbeatTimers();
    expect(timers.has('actor-1')).toBe(false);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Registration expires after heartbeat timeout
- Timeout resets on each heartbeat
- Timers cleaned up on disconnect (no memory leak)

### Test Case 1.5: Exponential Backoff on Reconnect

**Scenario:** Connection repeatedly fails, client should back off exponentially to avoid thundering herd.

**Risk:** Cascade failure when many actors reconnect simultaneously.

```typescript
describe('Reconnection - Exponential Backoff', () => {
  it('should implement exponential backoff with jitter', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local', {
      reconnectConfig: {
        initialDelayMs: 100,
        maxDelayMs: 30_000,
        multiplier: 2,
        jitterFraction: 0.25
      }
    });

    const connectionAttempts: number[] = [];
    client.on('connect-attempt', () => {
      connectionAttempts.push(Date.now());
    });

    // Force failures for first 5 attempts
    let attemptCount = 0;
    client.beforeConnect = () => {
      attemptCount++;
      if (attemptCount <= 5) {
        throw new Error('Simulated connection failure');
      }
    };

    await client.connect();

    // Verify exponential backoff
    const delays = [];
    for (let i = 1; i < connectionAttempts.length; i++) {
      delays.push(connectionAttempts[i] - connectionAttempts[i - 1]);
    }

    // Each delay should be ~2x previous (with jitter tolerance)
    for (let i = 1; i < delays.length; i++) {
      const expectedMin = delays[i - 1] * 1.5; // Account for jitter
      const expectedMax = delays[i - 1] * 2.5;
      expect(delays[i]).toBeGreaterThanOrEqual(expectedMin);
      expect(delays[i]).toBeLessThanOrEqual(expectedMax);
    }

    // Should eventually succeed
    expect(client.connectionState).toBe('connected');

    await cleanup();
  });

  it('should respect max retry limit', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const client = new SignalHubClient('ws://test-hub.local', {
      reconnectConfig: {
        maxRetries: 3
      }
    });

    // Force all connections to fail
    client.beforeConnect = () => {
      throw new Error('Permanent connection failure');
    };

    await expect(client.connect()).rejects.toThrow(/max retries exceeded/i);
    expect(client.connectionState).toBe('disconnected');

    await cleanup();
  });
});
```

**Expected Outcome:**
- Delays increase exponentially: 100ms → 200ms → 400ms → 800ms
- Jitter prevents synchronized reconnects
- Max retry limit prevents infinite loops

---

## Priority 2: Message Delivery Guarantees

### Why Priority 2

**Rationale:** Message delivery is core to Signal Hub's purpose. Silent failures (message sent but never delivered) are catastrophic for actor communication. At-most-once guarantees need verification; at-least-once with ack requires deduplication testing.

### Test Case 2.1: At-Most-Once Delivery (MVP)

**Scenario:** Fire-and-forget messages (`pattern: 'tell'`) may be lost during network failures.

**Risk:** Silent message loss without client awareness.

```typescript
describe('Message Delivery - At-Most-Once', () => {
  it('should accept message loss on network failure', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const network = new NetworkSimulator();
    network.setPacketLoss(0.1); // 10% packet loss

    const sender = new SignalHubClient('ws://test-hub.local', { network });
    const receiver = new SignalHubClient('ws://test-hub.local', { network });

    await sender.connect();
    await sender.register({ actorAddress: 'sender' });

    await receiver.connect();
    await receiver.register({ actorAddress: 'receiver' });

    const sent: string[] = [];
    const received: string[] = [];

    receiver.on('message', (msg: any) => {
      received.push(msg.id);
    });

    // Send 100 messages with 10% packet loss
    for (let i = 0; i < 100; i++) {
      const msgId = `msg-${i}`;
      sent.push(msgId);

      try {
        await sender.send({
          to: 'receiver',
          id: msgId,
          type: 'test',
          pattern: 'tell', // Fire-and-forget
          payload: { index: i }
        });
      } catch (e) {
        // Network failure expected sometimes
      }
    }

    await new Promise(r => setTimeout(r, 1000)); // Wait for delivery

    // Verify: received <= sent (at-most-once)
    expect(received.length).toBeLessThanOrEqual(sent.length);

    // All received messages were actually sent
    for (const msgId of received) {
      expect(sent).toContain(msgId);
    }

    // Some messages lost (with 10% packet loss)
    const lossRate = (sent.length - received.length) / sent.length;
    expect(lossRate).toBeGreaterThan(0.05); // At least 5% loss expected
    expect(lossRate).toBeLessThan(0.15); // Not more than 15% (with variance)

    console.log(`Loss rate: ${(lossRate * 100).toFixed(1)}%`);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Some messages lost (5-15% with 10% packet loss)
- No duplicate delivery
- No silent corruption

### Test Case 2.2: At-Least-Once with Acknowledgment

**Scenario:** Use `pattern: 'ask'` + `requiresAck: true` to guarantee delivery.

**Risk:** Without deduplication, retries cause duplicate processing.

```typescript
describe('Message Delivery - At-Least-Once', () => {
  it('should deliver message at least once with ack', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const network = new NetworkSimulator();
    network.setPacketLoss(0.1); // 10% packet loss

    const sender = new SignalHubClient('ws://test-hub.local', {
      network,
      retryConfig: { maxRetries: 5, timeoutMs: 1000 }
    });
    const receiver = new SignalHubClient('ws://test-hub.local', { network });

    await sender.connect();
    await sender.register({ actorAddress: 'sender' });

    await receiver.connect();
    await receiver.register({ actorAddress: 'receiver' });

    const received: string[] = [];

    receiver.on('message', (msg: any) => {
      received.push(msg.id);
    });

    // Send 100 messages with retry
    const sent = [];
    for (let i = 0; i < 100; i++) {
      const msgId = `msg-${i}`;
      sent.push(msgId);

      // Retry until ack received
      await sender.sendWithRetry({
        to: 'receiver',
        id: msgId,
        type: 'test',
        pattern: 'ask', // Request-response
        requiresAck: true,
        payload: { index: i }
      });
    }

    await new Promise(r => setTimeout(r, 1000)); // Wait for delivery

    // All messages should be delivered
    expect(received.length).toBeGreaterThanOrEqual(sent.length);

    // Verify all sent messages were received
    for (const msgId of sent) {
      expect(received).toContain(msgId);
    }

    await cleanup();
  });
});
```

**Expected Outcome:**
- All 100 messages delivered despite packet loss
- Retries handled automatically
- Success rate: 100%

### Test Case 2.3: Deduplication on Retry

**Scenario:** Server receives duplicate message ID, should process only once.

**Risk:** Duplicate processing causes incorrect state.

```typescript
describe('Message Delivery - Deduplication', () => {
  it('should deduplicate messages by ID', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const sender = new SignalHubClient('ws://test-hub.local');
    const receiver = new SignalHubClient('ws://test-hub.local');

    await sender.connect();
    await sender.register({ actorAddress: 'sender' });

    await receiver.connect();
    await receiver.register({ actorAddress: 'receiver' });

    let processCount = 0;
    receiver.on('message', (msg: any) => {
      if (msg.id === 'duplicate-test') {
        processCount++;
      }
    });

    // Send same message ID three times
    const msgId = 'duplicate-test';
    await sender.send({
      to: 'receiver',
      id: msgId,
      type: 'test',
      payload: { data: 'first' }
    });

    await sender.send({
      to: 'receiver',
      id: msgId, // Same ID
      type: 'test',
      payload: { data: 'second' }
    });

    await sender.send({
      to: 'receiver',
      id: msgId, // Same ID
      type: 'test',
      payload: { data: 'third' }
    });

    await new Promise(r => setTimeout(r, 500));

    // Should only process once
    expect(processCount).toBe(1);

    await cleanup();
  });

  it('should expire deduplication cache after TTL', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const sender = new SignalHubClient('ws://test-hub.local');
    const receiver = new SignalHubClient('ws://test-hub.local');

    await sender.connect();
    await sender.register({ actorAddress: 'sender' });

    await receiver.connect();
    await receiver.register({ actorAddress: 'receiver' });

    let processCount = 0;
    receiver.on('message', (msg: any) => {
      processCount++;
    });

    const msgId = 'ttl-test';

    // Send first time
    await sender.send({
      to: 'receiver',
      id: msgId,
      type: 'test',
      payload: { attempt: 1 }
    });

    // Wait for deduplication cache to expire (default: 5 minutes)
    await new Promise(r => setTimeout(r, 310_000)); // 5m 10s

    // Send again with same ID
    await sender.send({
      to: 'receiver',
      id: msgId,
      type: 'test',
      payload: { attempt: 2 }
    });

    await new Promise(r => setTimeout(r, 500));

    // Should process twice (cache expired)
    expect(processCount).toBe(2);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Duplicate message IDs processed only once
- Deduplication cache expires after TTL
- Memory bounded by TTL expiration

### Test Case 2.4: TTL Expiration

**Scenario:** Message with TTL expires before delivery.

**Risk:** Stale messages delivered, causing incorrect behavior.

```typescript
describe('Message Delivery - TTL Expiration', () => {
  it('should drop message if TTL expires before delivery', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const network = new NetworkSimulator();
    network.setLatency(2000); // 2 second latency

    const sender = new SignalHubClient('ws://test-hub.local', { network });
    const receiver = new SignalHubClient('ws://test-hub.local', { network });

    await sender.connect();
    await sender.register({ actorAddress: 'sender' });

    await receiver.connect();
    await receiver.register({ actorAddress: 'receiver' });

    const received: any[] = [];
    receiver.on('message', (msg: any) => {
      received.push(msg);
    });

    // Send message with 1s TTL (will expire during 2s latency)
    await sender.send({
      to: 'receiver',
      id: 'expired-msg',
      type: 'test',
      ttl: 1000, // 1 second TTL
      payload: { data: 'should expire' }
    });

    // Wait for delivery attempt (2s latency + buffer)
    await new Promise(r => setTimeout(r, 3000));

    // Message should be dropped due to TTL expiration
    expect(received).toHaveLength(0);

    // Sender should receive error
    const errors = sender.getErrors();
    expect(errors.some(e => e.code === 'message_expired')).toBe(true);

    await cleanup();
  });

  it('should deliver message if TTL not expired', async () => {
    const { env, cleanup } = await createTestEnvironment();
    const network = new NetworkSimulator();
    network.setLatency(1000); // 1 second latency

    const sender = new SignalHubClient('ws://test-hub.local', { network });
    const receiver = new SignalHubClient('ws://test-hub.local', { network });

    await sender.connect();
    await sender.register({ actorAddress: 'sender' });

    await receiver.connect();
    await receiver.register({ actorAddress: 'receiver' });

    const received: any[] = [];
    receiver.on('message', (msg: any) => {
      received.push(msg);
    });

    // Send message with 5s TTL (will not expire during 1s latency)
    await sender.send({
      to: 'receiver',
      id: 'valid-msg',
      type: 'test',
      ttl: 5000, // 5 second TTL
      payload: { data: 'should arrive' }
    });

    await new Promise(r => setTimeout(r, 2000));

    // Message should be delivered
    expect(received).toHaveLength(1);
    expect(received[0].id).toBe('valid-msg');

    await cleanup();
  });
});
```

**Expected Outcome:**
- Expired messages dropped with error
- Valid messages delivered within TTL
- No stale message delivery

---

## Priority 3: Fan-Out & Backpressure

### Why Priority 3

**Rationale:** Broadcast is the highest-risk operation for CPU exhaustion. Naive synchronous fan-out to 1000+ actors will exceed Cloudflare's 30s CPU limit. Backpressure prevents fast producers from overwhelming slow consumers.

### Test Case 3.1: Small Broadcast (Synchronous)

**Scenario:** Broadcast to 100 actors should complete synchronously without queueing.

**Risk:** Performance regression if queueing used unnecessarily.

```typescript
describe('Fan-Out - Small Broadcast', () => {
  it('should broadcast to 100 actors synchronously', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create 100 actors
    const actors: SignalHubClient[] = [];
    for (let i = 0; i < 100; i++) {
      const client = new SignalHubClient('ws://test-hub.local');
      await client.connect();
      await client.register({ actorAddress: `actor-${i}` });
      actors.push(client);
    }

    // Track received broadcasts
    const received = new Map<string, any>();
    actors.forEach((actor, idx) => {
      actor.on('message', (msg: any) => {
        if (msg.type === 'broadcast') {
          received.set(`actor-${idx}`, msg);
        }
      });
    });

    // Broadcast from actor-0
    const startTime = Date.now();
    await actors[0].broadcast({
      type: 'broadcast',
      payload: { message: 'hello everyone' }
    });
    const duration = Date.now() - startTime;

    // Wait for delivery
    await new Promise(r => setTimeout(r, 500));

    // All actors should receive (except sender if excludeSelf)
    expect(received.size).toBeGreaterThanOrEqual(99);

    // Should complete quickly (< 1 second for 100 actors)
    expect(duration).toBeLessThan(1000);

    await cleanup();
  });
});
```

**Expected Outcome:**
- All 99 actors receive broadcast
- Completes in < 1 second
- No queueing overhead

### Test Case 3.2: Large Broadcast (Async Queue)

**Scenario:** Broadcast to 1000 actors should use async queue to avoid CPU timeout.

**Risk:** CPU timeout causes partial broadcast, connection drops.

```typescript
describe('Fan-Out - Large Broadcast', () => {
  it('should queue broadcast to 1000 actors to avoid CPU timeout', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create 1000 actors
    const actors: SignalHubClient[] = [];
    for (let i = 0; i < 1000; i++) {
      const client = new SignalHubClient('ws://test-hub.local');
      await client.connect();
      await client.register({ actorAddress: `actor-${i}` });
      actors.push(client);
    }

    // Track received broadcasts
    const received = new Set<string>();
    actors.forEach((actor, idx) => {
      actor.on('message', (msg: any) => {
        if (msg.type === 'broadcast') {
          received.add(`actor-${idx}`);
        }
      });
    });

    // Broadcast from actor-0
    const startTime = Date.now();
    const result = await actors[0].broadcast({
      type: 'broadcast',
      payload: { message: 'hello everyone' }
    });
    const requestDuration = Date.now() - startTime;

    // Request should return immediately (queued)
    expect(requestDuration).toBeLessThan(1000);
    expect(result.queued).toBe(true);
    expect(result.queuedCount).toBeGreaterThanOrEqual(1000);

    // Wait for async delivery (1000 actors at ~1000/sec = ~1s)
    await new Promise(r => setTimeout(r, 2000));

    // All actors should eventually receive
    expect(received.size).toBeGreaterThanOrEqual(999);

    // Total time should not exceed CPU limit
    const totalDuration = Date.now() - startTime;
    expect(totalDuration).toBeLessThan(30_000); // Under 30s CPU limit

    await cleanup();
  });

  it('should handle broadcast queue depth limits', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create 10K actors
    const actors: SignalHubClient[] = [];
    for (let i = 0; i < 10_000; i++) {
      const client = new SignalHubClient('ws://test-hub.local');
      await client.connect();
      await client.register({ actorAddress: `actor-${i}` });
      actors.push(client);
    }

    // Send 100 broadcasts rapidly
    const promises = [];
    for (let i = 0; i < 100; i++) {
      promises.push(
        actors[0].broadcast({
          type: 'broadcast',
          payload: { index: i }
        })
      );
    }

    const results = await Promise.allSettled(promises);

    // Some should be queued
    const queued = results.filter(r =>
      r.status === 'fulfilled' && (r.value as any).queued
    );
    expect(queued.length).toBeGreaterThan(0);

    // Some may be rejected with 429 if queue full
    const rateLimited = results.filter(r =>
      r.status === 'rejected' && (r.reason as any).code === 'rate_limited'
    );

    console.log(`Queued: ${queued.length}, Rate limited: ${rateLimited.length}`);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Request returns immediately (< 1s)
- Broadcast queued for async processing
- All actors receive message within 2s
- No CPU timeout

### Test Case 3.3: Fast Producer Backpressure

**Scenario:** Producer sends 1000 messages/sec, consumer can only process 100/sec. Signal Hub should send pause signal.

**Risk:** Queue overflow, memory exhaustion, message loss.

```typescript
describe('Backpressure - Fast Producer', () => {
  it('should send pause when consumer queue depth exceeds threshold', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const producer = new SignalHubClient('ws://test-hub.local');
    const consumer = new SignalHubClient('ws://test-hub.local', {
      // Slow message processing
      messageProcessingDelayMs: 100 // 10 msgs/sec
    });

    await producer.connect();
    await producer.register({ actorAddress: 'producer' });

    await consumer.connect();
    await consumer.register({ actorAddress: 'consumer' });

    let pauseReceived = false;
    let resumeReceived = false;

    producer.on('pause', () => {
      pauseReceived = true;
    });

    producer.on('resume', () => {
      resumeReceived = true;
    });

    // Send messages rapidly (1000 msgs/sec)
    const sendPromises = [];
    for (let i = 0; i < 1000; i++) {
      sendPromises.push(
        producer.send({
          to: 'consumer',
          type: 'work',
          payload: { index: i }
        })
      );
    }

    // Wait for queue to build up
    await new Promise(r => setTimeout(r, 500));

    // Should receive pause signal
    expect(pauseReceived).toBe(true);

    // Wait for queue to drain
    await Promise.all(sendPromises);
    await new Promise(r => setTimeout(r, 2000));

    // Should receive resume signal
    expect(resumeReceived).toBe(true);

    await cleanup();
  });

  it('should respect pause and stop sending', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const producer = new SignalHubClient('ws://test-hub.local');
    const consumer = new SignalHubClient('ws://test-hub.local', {
      messageProcessingDelayMs: 100
    });

    await producer.connect();
    await producer.register({ actorAddress: 'producer' });

    await consumer.connect();
    await consumer.register({ actorAddress: 'consumer' });

    let paused = false;
    producer.on('pause', () => {
      paused = true;
    });

    // Send until paused
    let sentCount = 0;
    while (!paused && sentCount < 2000) {
      try {
        await producer.send({
          to: 'consumer',
          type: 'work',
          payload: { index: sentCount }
        });
        sentCount++;
      } catch (e) {
        // May fail if paused
        break;
      }

      await new Promise(r => setTimeout(r, 1)); // 1ms between sends
    }

    // Should have paused before sending all 2000
    expect(paused).toBe(true);
    expect(sentCount).toBeLessThan(2000);

    // Attempting to send while paused should throw
    await expect(
      producer.send({
        to: 'consumer',
        type: 'work',
        payload: { index: 9999 }
      })
    ).rejects.toThrow(/paused/i);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Producer receives pause when queue depth > 1000
- Producer stops sending while paused
- Producer resumes when queue depth < 500
- No message loss or memory exhaustion

### Test Case 3.4: Queue Statistics

**Scenario:** Client queries queue depth and processing rate.

**Risk:** No visibility into backpressure state.

```typescript
describe('Backpressure - Queue Stats', () => {
  it('should report queue depth and processing rate', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const producer = new SignalHubClient('ws://test-hub.local');
    const consumer = new SignalHubClient('ws://test-hub.local', {
      messageProcessingDelayMs: 10 // 100 msgs/sec
    });

    await producer.connect();
    await producer.register({ actorAddress: 'producer' });

    await consumer.connect();
    await consumer.register({ actorAddress: 'consumer' });

    // Send 500 messages
    for (let i = 0; i < 500; i++) {
      producer.send({
        to: 'consumer',
        type: 'work',
        payload: { index: i }
      });
    }

    // Query queue stats
    const stats = await producer.queryQueueStats();

    expect(stats.queueDepth).toBeGreaterThan(0);
    expect(stats.processingRate).toBeGreaterThan(0);
    expect(stats.processingRate).toBeLessThanOrEqual(100); // Max 100/sec

    console.log(`Queue depth: ${stats.queueDepth}, Rate: ${stats.processingRate} msgs/sec`);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Queue stats accurately reflect current state
- Processing rate measured in msgs/sec
- Client can make informed decisions based on stats

---

## Priority 4: Registration Consistency

### Why Priority 4

**Rationale:** Duplicate registrations cause silent failures downstream. Actor discovery must return consistent results. Registration must handle concurrent updates atomically.

### Test Case 4.1: Duplicate Registration Handling

**Scenario:** Same actor registers twice from different connections.

**Risk:** Broadcast sends duplicates, stale registrations.

```typescript
describe('Registration - Duplicate Handling', () => {
  it('should reject duplicate registration from active connection', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // First connection
    const client1 = new SignalHubClient('ws://test-hub.local');
    await client1.connect();
    await client1.register({ actorAddress: 'actor-1' });

    // Second connection (same actor)
    const client2 = new SignalHubClient('ws://test-hub.local');
    await client2.connect();

    // Should reject if first connection still active
    await expect(
      client2.register({ actorAddress: 'actor-1' })
    ).rejects.toThrow(/duplicate registration/i);

    await cleanup();
  });

  it('should allow re-registration after old connection expires', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // First connection
    const client1 = new SignalHubClient('ws://test-hub.local', {
      heartbeatIntervalMs: 30_000,
      heartbeatTimeoutMs: 60_000
    });
    await client1.connect();
    await client1.register({ actorAddress: 'actor-1' });

    // Stop heartbeat (simulate crash)
    client1.stopHeartbeat();

    // Wait for timeout
    await new Promise(r => setTimeout(r, 65_000));

    // Second connection should succeed
    const client2 = new SignalHubClient('ws://test-hub.local');
    await client2.connect();
    await client2.register({ actorAddress: 'actor-1' });

    expect(client2.connectionState).toBe('connected');

    await cleanup();
  });
});
```

**Expected Outcome:**
- Duplicate rejected if first connection active
- Re-registration allowed after timeout
- Only one registration per actor

### Test Case 4.2: Concurrent Registration

**Scenario:** Two actors register simultaneously with overlapping addresses.

**Risk:** Race condition causes registry corruption.

```typescript
describe('Registration - Concurrent Updates', () => {
  it('should handle concurrent registrations atomically', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create 10 clients registering simultaneously
    const clients = Array.from({ length: 10 }, (_, i) =>
      new SignalHubClient('ws://test-hub.local', {
        actorAddress: `actor-${i}`
      })
    );

    // Connect all
    await Promise.all(clients.map(c => c.connect()));

    // Register all simultaneously
    await Promise.all(clients.map(c =>
      c.register({ actorAddress: c.actorAddress })
    ));

    // Verify all registered
    const hubId = env.SIGNAL_HUB.idFromName('test-hub');
    const hubStub = env.SIGNAL_HUB.get(hubId);
    const registrations = await hubStub.listActors();

    expect(registrations).toHaveLength(10);

    // Verify no duplicates
    const addresses = registrations.map(r => r.actorAddress);
    const uniqueAddresses = new Set(addresses);
    expect(uniqueAddresses.size).toBe(10);

    await cleanup();
  });
});
```

**Expected Outcome:**
- All 10 actors registered
- No duplicates in registry
- Atomic updates prevent race conditions

### Test Case 4.3: TTL Expiration

**Scenario:** Actor registration expires after TTL, should be auto-removed.

**Risk:** Stale registrations consume memory, cause false discoveries.

```typescript
describe('Registration - TTL Expiration', () => {
  it('should auto-remove registration after TTL expires', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const client = new SignalHubClient('ws://test-hub.local');
    await client.connect();
    await client.register({
      actorAddress: 'actor-1',
      ttlSeconds: 60 // 1 minute TTL
    });

    // Verify registered
    const hubId = env.SIGNAL_HUB.idFromName('test-hub');
    const hubStub = env.SIGNAL_HUB.get(hubId);
    let registrations = await hubStub.listActors();
    expect(registrations.some(r => r.actorAddress === 'actor-1')).toBe(true);

    // Wait for TTL expiration
    await new Promise(r => setTimeout(r, 65_000)); // 65 seconds

    // Should be removed
    registrations = await hubStub.listActors();
    expect(registrations.some(r => r.actorAddress === 'actor-1')).toBe(false);

    await cleanup();
  });

  it('should allow TTL renewal', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const client = new SignalHubClient('ws://test-hub.local');
    await client.connect();
    const registration = await client.register({
      actorAddress: 'actor-1',
      ttlSeconds: 300 // 5 minutes
    });

    const initialExpiration = registration.expiresAt;

    // Wait 2 minutes
    await new Promise(r => setTimeout(r, 120_000));

    // Renew
    const renewed = await client.renewRegistration(registration.renewalToken);

    // Expiration should be extended
    expect(renewed.expiresAt).toBeGreaterThan(initialExpiration);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Registration removed after TTL
- TTL renewal extends expiration
- Memory bounded by TTL

### Test Case 4.4: Registry Size Limits

**Scenario:** Attempt to register beyond 50K actor limit.

**Risk:** Memory exhaustion, performance degradation.

```typescript
describe('Registration - Size Limits', () => {
  it('should reject registration beyond 50K actor limit', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Register 50K actors (skip in CI, run manually)
    if (process.env.RUN_STRESS_TESTS !== 'true') {
      console.log('Skipping stress test (set RUN_STRESS_TESTS=true to run)');
      await cleanup();
      return;
    }

    const clients: SignalHubClient[] = [];
    for (let i = 0; i < 50_000; i++) {
      const client = new SignalHubClient('ws://test-hub.local');
      await client.connect();
      await client.register({ actorAddress: `actor-${i}` });
      clients.push(client);

      if (i % 1000 === 0) {
        console.log(`Registered ${i} actors...`);
      }
    }

    // Verify registry at capacity
    const hubId = env.SIGNAL_HUB.idFromName('test-hub');
    const hubStub = env.SIGNAL_HUB.get(hubId);
    const stats = await hubStub.getStats();
    expect(stats.actorCount).toBe(50_000);

    // Attempt to register 50,001st actor
    const overflow = new SignalHubClient('ws://test-hub.local');
    await overflow.connect();

    await expect(
      overflow.register({ actorAddress: 'actor-50000' })
    ).rejects.toThrow(/registry at capacity|limit exceeded/i);

    await cleanup();
  });
});
```

**Expected Outcome:**
- 50K actors registered successfully
- 50,001st registration rejected
- Graceful error message

---

## Priority 5: Integration Tests

### Why Priority 5

**Rationale:** End-to-end tests validate the complete stack. SEAG (local) → Signal Hub (Workers) → Browser is the primary use case. Integration tests catch issues that unit tests miss.

### Test Case 5.1: SEAG to Browser Message Flow

**Scenario:** SEAG actor sends message to browser actor via Signal Hub.

**Risk:** Runtime incompatibilities, addressing errors.

```typescript
describe('Integration - SEAG to Browser', () => {
  it('should deliver message from SEAG to browser via Signal Hub', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create SEAG actor (local runtime)
    const seag = await createLocalActor({
      name: 'seag-processor',
      signalHubUrl: 'ws://test-hub.local'
    });

    // Create browser actor (simulated browser runtime)
    const browser = await createBrowserActor({
      name: 'browser-widget-123',
      signalHubUrl: 'ws://test-hub.local'
    });

    // Register both with Signal Hub
    await seag.register();
    await browser.register();

    // Browser waits for message
    const messagePromise = new Promise((resolve) => {
      browser.on('message', (msg: any) => {
        resolve(msg);
      });
    });

    // SEAG sends message to browser
    await seag.send('@(browser/browser-widget-123)', {
      type: 'render',
      payload: {
        component: 'Chart',
        data: [1, 2, 3, 4, 5]
      }
    });

    // Browser should receive
    const received = await messagePromise;
    expect(received.type).toBe('render');
    expect(received.payload.component).toBe('Chart');
    expect(received.payload.data).toEqual([1, 2, 3, 4, 5]);

    await cleanup();
  });

  it('should handle bidirectional communication', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const seag = await createLocalActor({
      name: 'seag-processor',
      signalHubUrl: 'ws://test-hub.local'
    });

    const browser = await createBrowserActor({
      name: 'browser-widget-123',
      signalHubUrl: 'ws://test-hub.local'
    });

    await seag.register();
    await browser.register();

    // Browser sends request to SEAG
    const responsePromise = new Promise((resolve) => {
      seag.on('message', async (msg: any) => {
        // SEAG responds
        await seag.send(msg.from, {
          type: 'response',
          correlationId: msg.id,
          payload: { result: 'processed' }
        });
      });

      browser.on('message', (msg: any) => {
        if (msg.type === 'response') {
          resolve(msg);
        }
      });
    });

    await browser.send('@(seag/seag-processor)', {
      type: 'request',
      payload: { action: 'process' }
    });

    const response = await responsePromise;
    expect(response.type).toBe('response');
    expect(response.payload.result).toBe('processed');

    await cleanup();
  });
});
```

**Expected Outcome:**
- SEAG and browser register successfully
- Messages delivered in both directions
- CanonicalAddress routing works correctly

### Test Case 5.2: Multi-Runtime Discovery

**Scenario:** Browser actor discovers available SEAG actors.

**Risk:** Runtime-specific addressing breaks discovery.

```typescript
describe('Integration - Multi-Runtime Discovery', () => {
  it('should discover actors across runtimes', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create 3 SEAG actors
    const seagActors = await Promise.all([
      createLocalActor({ name: 'seag-worker-1', signalHubUrl: 'ws://test-hub.local' }),
      createLocalActor({ name: 'seag-worker-2', signalHubUrl: 'ws://test-hub.local' }),
      createLocalActor({ name: 'seag-worker-3', signalHubUrl: 'ws://test-hub.local' })
    ]);

    // Create 2 browser actors
    const browserActors = await Promise.all([
      createBrowserActor({ name: 'browser-widget-1', signalHubUrl: 'ws://test-hub.local' }),
      createBrowserActor({ name: 'browser-widget-2', signalHubUrl: 'ws://test-hub.local' })
    ]);

    // Register all
    await Promise.all([
      ...seagActors.map(a => a.register()),
      ...browserActors.map(a => a.register())
    ]);

    // Browser discovers SEAG actors
    const discovered = await browserActors[0].discover({
      pattern: '^@\\(seag/',
      capabilities: ['processing']
    });

    expect(discovered.actors).toHaveLength(3);
    expect(discovered.actors.every(a =>
      a.actorAddress.startsWith('@(seag/')
    )).toBe(true);

    await cleanup();
  });
});
```

**Expected Outcome:**
- All 5 actors registered
- Discovery returns only matching actors
- Cross-runtime addressing works

### Test Case 5.3: End-to-End Error Handling

**Scenario:** SEAG sends to non-existent browser actor.

**Risk:** Errors not propagated correctly across runtime boundary.

```typescript
describe('Integration - Error Handling', () => {
  it('should propagate unknown actor error to sender', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const seag = await createLocalActor({
      name: 'seag-processor',
      signalHubUrl: 'ws://test-hub.local'
    });

    await seag.register();

    // Send to non-existent actor
    await expect(
      seag.send('@(browser/nonexistent)', {
        type: 'test',
        payload: {}
      })
    ).rejects.toThrow(/unknown actor|not registered/i);

    await cleanup();
  });

  it('should handle message too large error', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const seag = await createLocalActor({
      name: 'seag-processor',
      signalHubUrl: 'ws://test-hub.local'
    });

    const browser = await createBrowserActor({
      name: 'browser-widget-123',
      signalHubUrl: 'ws://test-hub.local'
    });

    await seag.register();
    await browser.register();

    // Send 2MB payload (exceeds 1MB limit)
    const largePayload = Buffer.alloc(2 * 1024 * 1024).toString('base64');

    await expect(
      seag.send('@(browser/browser-widget-123)', {
        type: 'large',
        payload: { data: largePayload }
      })
    ).rejects.toThrow(/message too large|payload exceeds/i);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Errors propagated to sender
- Error messages actionable
- No silent failures

---

## Load Testing

### Load Test 1: 10K Actors

**Goal:** Verify Signal Hub can handle 10K concurrent actors.

```bash
# test/load/10k-actors.sh
npm run test:load -- \
  --actors 10000 \
  --duration 300 \
  --message-rate 100 \
  --broadcast-rate 10 \
  --reconnect-rate 0.01
```

**Script:**

```typescript
// test/load/10k-actors.test.ts
describe('Load Test - 10K Actors', () => {
  it('should handle 10K concurrent actors', async () => {
    const { env, cleanup } = await createTestEnvironment();

    const config = {
      actorCount: 10_000,
      durationSec: 300,
      messageRatePerSec: 100,
      broadcastRatePerSec: 10,
      reconnectRate: 0.01 // 1% of actors reconnect per second
    };

    // Create actors
    console.log(`Creating ${config.actorCount} actors...`);
    const actors: SignalHubClient[] = [];
    for (let i = 0; i < config.actorCount; i++) {
      const client = new SignalHubClient('ws://test-hub.local');
      await client.connect();
      await client.register({ actorAddress: `actor-${i}` });
      actors.push(client);

      if (i % 1000 === 0) {
        console.log(`Created ${i} actors...`);
      }
    }

    console.log(`Running load test for ${config.durationSec}s...`);

    const startTime = Date.now();
    const metrics = {
      messagesSent: 0,
      messagesReceived: 0,
      broadcastsSent: 0,
      reconnects: 0,
      errors: 0
    };

    // Message receiver
    actors.forEach(actor => {
      actor.on('message', () => {
        metrics.messagesReceived++;
      });

      actor.on('error', () => {
        metrics.errors++;
      });

      actor.on('reconnect', () => {
        metrics.reconnects++;
      });
    });

    // Load generator
    const interval = setInterval(async () => {
      // Random messages
      for (let i = 0; i < config.messageRatePerSec; i++) {
        const sender = actors[Math.floor(Math.random() * actors.length)];
        const receiver = actors[Math.floor(Math.random() * actors.length)];

        try {
          await sender.send({
            to: receiver.actorAddress,
            type: 'test',
            payload: { timestamp: Date.now() }
          });
          metrics.messagesSent++;
        } catch (e) {
          metrics.errors++;
        }
      }

      // Random broadcasts
      for (let i = 0; i < config.broadcastRatePerSec; i++) {
        const sender = actors[Math.floor(Math.random() * actors.length)];

        try {
          await sender.broadcast({
            type: 'broadcast',
            payload: { timestamp: Date.now() }
          });
          metrics.broadcastsSent++;
        } catch (e) {
          metrics.errors++;
        }
      }

      // Random reconnects
      const reconnectCount = Math.floor(config.actorCount * config.reconnectRate);
      for (let i = 0; i < reconnectCount; i++) {
        const actor = actors[Math.floor(Math.random() * actors.length)];
        actor.ws.close();
        // Will auto-reconnect
      }
    }, 1000);

    // Wait for test duration
    await new Promise(r => setTimeout(r, config.durationSec * 1000));
    clearInterval(interval);

    const duration = (Date.now() - startTime) / 1000;

    // Report metrics
    console.log('Load Test Results:');
    console.log(`Duration: ${duration}s`);
    console.log(`Actors: ${config.actorCount}`);
    console.log(`Messages sent: ${metrics.messagesSent}`);
    console.log(`Messages received: ${metrics.messagesReceived}`);
    console.log(`Broadcasts sent: ${metrics.broadcastsSent}`);
    console.log(`Reconnects: ${metrics.reconnects}`);
    console.log(`Errors: ${metrics.errors}`);

    // Success criteria
    expect(metrics.errors).toBeLessThan(metrics.messagesSent * 0.01); // < 1% error rate
    expect(metrics.messagesReceived).toBeGreaterThan(metrics.messagesSent * 0.99); // > 99% delivery

    await cleanup();
  });
});
```

**Expected Metrics:**
- Latency p99: < 100ms
- Message loss rate: < 0.1%
- Broadcast completion: < 5s per 10K actors
- CPU time: < 20s per 30s window
- Storage: < 100MB
- Error rate: < 1%

### Load Test 2: Broadcast Throughput

**Goal:** Measure maximum broadcast throughput.

```typescript
describe('Load Test - Broadcast Throughput', () => {
  it('should sustain 1K actors/sec broadcast throughput', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create 10K actors
    const actors: SignalHubClient[] = [];
    for (let i = 0; i < 10_000; i++) {
      const client = new SignalHubClient('ws://test-hub.local');
      await client.connect();
      await client.register({ actorAddress: `actor-${i}` });
      actors.push(client);
    }

    // Measure broadcast throughput
    const startTime = Date.now();
    let totalActorsReached = 0;

    // Send 100 broadcasts
    for (let i = 0; i < 100; i++) {
      const result = await actors[0].broadcast({
        type: 'throughput-test',
        payload: { index: i }
      });

      totalActorsReached += result.deliveredCount || actors.length;
    }

    const duration = (Date.now() - startTime) / 1000;
    const throughput = totalActorsReached / duration;

    console.log(`Broadcast throughput: ${throughput.toFixed(0)} actors/sec`);

    // Should achieve at least 1K actors/sec
    expect(throughput).toBeGreaterThan(1000);

    await cleanup();
  });
});
```

**Expected Outcome:**
- Throughput: > 1K actors/sec
- No CPU timeouts
- Consistent performance over time

### Load Test 3: CPU Timeout Prevention

**Goal:** Verify no CPU timeouts under load.

```typescript
describe('Load Test - CPU Timeout Prevention', () => {
  it('should never exceed 30s CPU limit', async () => {
    const { env, cleanup } = await createTestEnvironment();

    // Create 10K actors
    const actors: SignalHubClient[] = [];
    for (let i = 0; i < 10_000; i++) {
      const client = new SignalHubClient('ws://test-hub.local');
      await client.connect();
      await client.register({ actorAddress: `actor-${i}` });
      actors.push(client);
    }

    const hubId = env.SIGNAL_HUB.idFromName('test-hub');
    const hubStub = env.SIGNAL_HUB.get(hubId);

    // Monitor CPU time
    const cpuSamples: number[] = [];
    const interval = setInterval(async () => {
      const stats = await hubStub.getStats();
      cpuSamples.push(stats.cpuTimeMs);
    }, 1000);

    // Run heavy load for 5 minutes
    for (let i = 0; i < 300; i++) {
      // Broadcast to all actors
      await actors[0].broadcast({
        type: 'cpu-test',
        payload: { index: i }
      });

      await new Promise(r => setTimeout(r, 1000));
    }

    clearInterval(interval);

    // Verify no sample exceeds 30s
    const maxCpu = Math.max(...cpuSamples);
    expect(maxCpu).toBeLessThan(30_000);

    console.log(`Max CPU time: ${maxCpu}ms`);

    await cleanup();
  });
});
```

**Expected Outcome:**
- CPU time: < 30s in all samples
- No evictions due to CPU timeout
- Stable performance over 5 minutes

---

## Test Execution

### Running Tests

```bash
# Run all tests
npm test

# Run specific priority
npm test -- --testNamePattern="Priority 1"
npm test -- --testNamePattern="Reconnection"

# Run load tests (manual only)
RUN_STRESS_TESTS=true npm test -- --testNamePattern="Load Test"

# Run with coverage
npm test -- --coverage

# Run in watch mode
npm test -- --watch
```

### CI Integration

```yaml
# .github/workflows/test.yml
name: Signal Hub Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '18'

      - run: npm install

      # Priority 1-4 tests
      - run: npm test

      # Priority 5 integration tests
      - run: npm run test:integration

      # Load tests (nightly only)
      - if: github.event_name == 'schedule'
        run: RUN_STRESS_TESTS=true npm run test:load
```

---

## Success Criteria

### Overall Success Criteria

- ✅ All Priority 1-4 tests pass (100% success rate)
- ✅ Integration tests pass (SEAG ↔ Browser communication works)
- ✅ Load tests pass (10K actors, 1K broadcasts/sec)
- ✅ No CPU timeouts under load
- ✅ Error rate < 1% under network failures
- ✅ Message loss rate < 0.1% with ack+retry
- ✅ Reconnection success rate > 99%

### Per-Priority Success Criteria

**Priority 1 (Reconnection):**
- WebSocket disconnect detected and recovered
- Hibernation transparent to client
- Duplicate connections resolved correctly
- Heartbeat timeouts detected within 10s
- Exponential backoff prevents thundering herd

**Priority 2 (Message Delivery):**
- At-most-once: < 0.1% unexpected loss
- At-least-once: 100% delivery with retry
- Deduplication: 0% duplicate processing
- TTL: 100% expired messages dropped

**Priority 3 (Fan-Out):**
- Small broadcast (100): < 1s synchronous
- Large broadcast (1000): queued, < 5s async
- Backpressure: pause at 1000, resume at 500
- Queue stats: accurate within 10%

**Priority 4 (Registration):**
- Duplicate: rejected or replaced correctly
- Concurrent: no race conditions
- TTL: expired within 10s of expiration
- Size limit: 50K enforced

**Priority 5 (Integration):**
- SEAG ↔ Browser: 100% message delivery
- Discovery: returns all matching actors
- Errors: propagated correctly across runtime

**Load Testing:**
- 10K actors: sustained for 5 minutes
- Broadcast: > 1K actors/sec throughput
- CPU: < 30s in all samples

---

## Appendix: Test Utilities

### SignalHubClient Mock

```typescript
// test/mocks/signal-hub-client.ts
export class SignalHubClient {
  public ws: MockWebSocket;
  public connectionState: ConnectionState = 'disconnected';
  public actorAddress: string;
  private messageHandlers: Map<string, (msg: any) => void> = new Map();

  constructor(
    public url: string,
    public config?: {
      heartbeatIntervalMs?: number;
      heartbeatTimeoutMs?: number;
      retryConfig?: RetryConfig;
      network?: NetworkSimulator;
    }
  ) {
    this.ws = new MockWebSocket();
  }

  async connect(): Promise<void> {
    this.connectionState = 'connecting';

    // Simulate network
    if (this.config?.network) {
      await this.config.network.simulateSend(async () => {
        this.ws.readyState = WebSocket.OPEN;
      });
    } else {
      await new Promise(r => setTimeout(r, 10));
      this.ws.readyState = WebSocket.OPEN;
    }

    this.connectionState = 'connected';
  }

  async register(opts: RegisterOptions): Promise<RegistrationResult> {
    const msg = {
      type: 'hub:register',
      payload: opts
    };

    await this.send(msg);

    return {
      actorAddress: opts.actorAddress,
      expiresAt: Date.now() + 300_000,
      renewalToken: crypto.randomUUID(),
      version: 1
    };
  }

  async send(msg: any): Promise<void> {
    if (this.ws.readyState !== WebSocket.OPEN) {
      throw new Error('Connection not open');
    }

    if (this.config?.network) {
      await this.config.network.simulateSend(async () => {
        this.ws.send(JSON.stringify(msg));
      });
    } else {
      this.ws.send(JSON.stringify(msg));
    }
  }

  async sendWithRetry(msg: any): Promise<any> {
    const maxRetries = this.config?.retryConfig?.maxRetries || 3;
    const timeoutMs = this.config?.retryConfig?.timeoutMs || 1000;

    for (let attempt = 0; attempt < maxRetries; attempt++) {
      try {
        return await this.send(msg);
      } catch (e) {
        if (attempt === maxRetries - 1) throw e;
        await new Promise(r => setTimeout(r, 100 * Math.pow(2, attempt)));
      }
    }
  }

  on(event: string, handler: (msg: any) => void): void {
    this.messageHandlers.set(event, handler);
  }

  emit(event: string, data: any): void {
    const handler = this.messageHandlers.get(event);
    if (handler) {
      handler(data);
    }
  }
}
```

---

**END OF TESTING STRATEGY**
