# Future Work: Actor Platform Enhancements

This document captures planned enhancements deferred from MVP implementation.

## Phase 3: Cross-Transport Claim Checks

**Current Limitation:** Claim check pattern requires shared storage (LocalTransport, Cloudflare KV).

**Problem:** When actors communicate across transport boundaries (RemoteTransport, SystemBridge, cross-account), they don't share the same KV storage. A claim check reference stored in one system's KV cannot be retrieved by an actor in another system.

**Goal:** Enable claim checks across transport boundaries (RemoteTransport, SystemBridge, cross-account).

### Option A: Actor-Based Retrieval (Recommended)

Instead of direct storage access, receiving actors retrieve payloads by sending a message back to a retrieval actor.

```typescript
interface ClaimCheckReference {
  claimCheckId: string;
  retrieveFrom: Address;  // Actor address to retrieve from
  size: number;
}

// Receiving actor retrieves by sending message:
const payload = await this.ask(ref.retrieveFrom, 'retrieve-claim', {
  id: ref.claimCheckId
});
```

**Implementation:**
```typescript
class ClaimCheckRetrievalActor extends Actor {
  constructor(id: string, router: IMessageRouter, private store: ClaimCheckStore) {
    super(id, router);
  }

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    if (message.type === 'retrieve-claim') {
      const { id } = message.payload as { id: string };

      const ref: ClaimCheckReference = {
        claimCheckId: id,
        storageType: 'kv',
        contentType: 'json',
        size: 0, // size not needed for retrieval
      };

      try {
        const payload = await this.store.retrieve(ref);
        return createResponse(message, payload);
      } catch (error) {
        return createErrorResponse(message, `Retrieval failed: ${error}`);
      }
    }

    throw new Error(`Unknown message type: ${message.type}`);
  }
}
```

**Pros:**
- ✅ Works across any transport (actor addresses are universal)
- ✅ Retrieval actor can use any storage backend
- ✅ Natural fit for actor model
- ✅ Can chain/proxy across systems
- ✅ Supports access control (retrieval actor can validate permissions)
- ✅ Enables audit logging

**Cons:**
- ❌ Additional message roundtrip (2x latency)
- ❌ Requires claim check retrieval actors to be deployed
- ❌ Complex failure modes (retrieval actor down, network partition)
- ❌ Requires correlation ID tracking

**Implementation Estimate:** 4 hours

**Use Cases:**
- Cross-cloud communication (AWS → Cloudflare → GCP)
- Multi-tenant systems (tenant A → tenant B)
- Edge → origin communication

---

### Option B: Addressable Storage

Claim check references include HTTP-accessible storage endpoints with read-only credentials.

```typescript
interface ClaimCheckReference {
  claimCheckId: string;
  storageEndpoint: string;  // HTTP URL to retrieve from
  credentials?: string;     // Read-only token
  size: number;
}

// Direct HTTP retrieval:
const response = await fetch(ref.storageEndpoint, {
  headers: { Authorization: `Bearer ${ref.credentials}` }
});
const payload = await response.json();
```

**Implementation:**
```typescript
class HttpClaimCheckStore {
  constructor(
    private kv: IKeyValueStorage,
    private baseUrl: string,
    private generateReadToken: (id: string) => string
  ) {}

  async store(payload: unknown): Promise<ClaimCheckReference> {
    const id = `claim-check:${crypto.randomUUID()}`;
    const json = JSON.stringify(payload);

    await this.kv.put(id, json, {
      expirationTtl: 3600,
      metadata: { type: 'claim-check', created: Date.now() }
    });

    return {
      claimCheckId: id,
      storageEndpoint: `${this.baseUrl}/claim-check/${id}`,
      credentials: this.generateReadToken(id),
      size: json.length
    };
  }
}

// HTTP endpoint (Cloudflare Worker example)
export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);
    const match = url.pathname.match(/^\/claim-check\/(.+)$/);

    if (!match) return new Response('Not found', { status: 404 });

    const id = match[1];

    // Validate token
    const token = request.headers.get('Authorization')?.replace('Bearer ', '');
    if (!validateToken(token, id)) {
      return new Response('Unauthorized', { status: 401 });
    }

    // Retrieve from KV
    const payload = await env.KV.get(id);
    if (!payload) {
      return new Response('Not found', { status: 404 });
    }

    return new Response(payload, {
      headers: { 'Content-Type': 'application/json' }
    });
  }
};
```

**Pros:**
- ✅ Direct retrieval, no actor messaging overhead
- ✅ Can use CDN/edge caching for frequently accessed payloads
- ✅ Standard HTTP semantics (caching, compression, retries)
- ✅ Works with existing HTTP infrastructure

**Cons:**
- ❌ Security concerns (credentials in messages, token management)
- ❌ Requires HTTP-accessible storage
- ❌ Not pure actor model
- ❌ Complex token lifecycle (generation, rotation, expiration)
- ❌ Credential leakage risk

**Implementation Estimate:** 6 hours

**Use Cases:**
- Public data sharing
- Systems with existing HTTP APIs
- CDN-backed claim checks

---

### Option C: Streaming Protocols

Bypass claim check entirely for cross-transport by streaming large payloads directly.

```typescript
// Sender streams directly to receiver
for await (const chunk of largePayloadStream) {
  await this.tell(target, 'stream-chunk', { chunk, index });
}
await this.tell(target, 'stream-end', {});
```

**Implementation:**
```typescript
class StreamingActor extends Actor {
  private chunks = new Map<string, Array<{ index: number; chunk: any }>>();

  protected async handleMessage(message: Message): Promise<MessageResponse> {
    if (message.type === 'stream-chunk') {
      const { correlationId, index, chunk } = message.payload;

      if (!this.chunks.has(correlationId)) {
        this.chunks.set(correlationId, []);
      }

      this.chunks.get(correlationId)!.push({ index, chunk });

      return createResponse(message, { received: true });
    }

    if (message.type === 'stream-end') {
      const { correlationId } = message.payload;
      const chunks = this.chunks.get(correlationId);

      if (!chunks) {
        return createErrorResponse(message, 'No chunks received');
      }

      // Reassemble payload
      const sorted = chunks.sort((a, b) => a.index - b.index);
      const payload = sorted.map(c => c.chunk).join('');

      this.chunks.delete(correlationId);

      // Process complete payload
      return this.handleLargePayload(JSON.parse(payload), message);
    }

    return createErrorResponse(message, 'Unknown message type');
  }
}
```

**Pros:**
- ✅ No intermediate storage needed
- ✅ Works for very large payloads (> 25MB)
- ✅ Memory efficient (process chunks as they arrive)
- ✅ Natural backpressure mechanism

**Cons:**
- ❌ Requires streaming support in all transports
- ❌ Complex error handling (missing chunks, out-of-order)
- ❌ Ordering guarantees needed
- ❌ State management (partial streams on failure)
- ❌ Not suitable for request-response patterns

**Implementation Estimate:** 8 hours

**Use Cases:**
- Very large payloads (ML models, datasets)
- Real-time data pipelines
- Systems with limited storage

---

## Recommendation

**Start with Option A (Actor-Based Retrieval)** for the following reasons:

1. **Natural fit:** Aligns with actor model philosophy (message passing)
2. **Incremental:** Can be added without breaking existing code
3. **Flexible:** Retrieval actor can use any storage backend
4. **Secure:** Built-in access control and audit logging
5. **Testable:** Easy to mock and test

**Implementation Plan:**
1. Create `ClaimCheckRetrievalActor` base class
2. Add `retrieveFrom` field to `ClaimCheckReference`
3. Update `Actor.receive()` to detect cross-transport references
4. Auto-send retrieval message if `retrieveFrom` is present
5. Add tests for cross-transport scenarios

**Phase 3 Timeline:** 2-3 weeks

---

## Other Future Enhancements

### Storage Layer
- **R2 blob storage validation** - Validate blob operations (size, key format)
- **Vector store validation** - Validate vector dimensions, similarity thresholds
- **Query complexity analysis** - Detect expensive SQL queries (cartesian products, missing indexes)
- **Audit logging** - Log all storage operations for compliance

### Claim Check
- **Compression before storage** - gzip payloads before storing (reduce size by 70-90%)
- **Blob storage backend (> 25MB)** - Use R2/S3 for payloads exceeding KV limits
- **Multi-part messages (chunking)** - Split very large payloads into multiple claim checks
- **Encryption at rest** - Encrypt payloads in storage
- **Analytics** - Track hit rate, size distribution, retrieval latency

### Actor System
- **Rate limiting per actor/storage** - Prevent abuse, enforce quotas
- **Circuit breakers** - Auto-disable actors on repeated failures
- **Distributed tracing** - End-to-end observability across transports
- **Health monitoring** - Periodic health checks, auto-restart on failure

### Testing
- **Property-based testing** - Generate random payloads, verify roundtrip
- **Chaos testing** - Inject failures (storage down, network partition)
- **Load testing** - Benchmark throughput, latency under load
- **Integration tests** - Test cross-transport scenarios with real infrastructure

---

**Document Status:** Living document for future phases
**Last Updated:** 2026-02-11
**Next Review:** After Phase 2 completion
