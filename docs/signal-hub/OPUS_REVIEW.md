# Signal Hub Protocol Review - Opus Perspective

**Reviewed by:** Claude Opus 4.6
**Date:** 2026-02-16
**Generated:** 2026-02-16T21:58:13Z (verified via `date` command)
**Documents Reviewed:** PROTOCOL.md (2,577 lines) + SHARED_MESSAGE_INTEGRATION.md + SECURITY.md + SCALABILITY.md + DELIVERY_GUARANTEES.md + CONNECTION_LIFECYCLE.md + shared-message.ts (source)

---

## Executive Summary

Signal Hub is a well-architected protocol design for a Cloudflare Workers-based actor coordination system. The specification demonstrates deep understanding of Cloudflare's platform constraints and makes consistently sound engineering trade-offs. The decision to reuse `SharedMessage` as the wire format rather than creating a parallel `WireMessage` type is the single most important architectural decision in the design, and it was made correctly. The `hub:*` namespace convention for message type discriminators is clean, the connection lifecycle state machine is minimal and well-defined, and the phased security model (JWT authentication for MVP, CBAC and HMAC in later phases) is appropriately scoped.

The protocol scores well on completeness: 24 message types across 5 categories, explicit timeout specifications for every operation, defined behavior for edge cases (hibernation during send, duplicate connections, concurrent connects), and a detailed 8-phase implementation checklist. The scalability design -- particularly the async batching strategy for broadcasts exceeding 100 actors and the consistent hashing sharding approach -- shows production-level thinking.

Where the design falls short is in a few structural areas: the metadata bag is doing too much work without type safety, the `hub:send` message has a confusing nested payload structure, and there is no mechanism for graceful protocol evolution of the metadata schema. These are correctable, and none of them are blocking for an MVP implementation. Overall, this is a strong B+ protocol design that is ready for implementation with the caveats noted below.

**Overall Grade: B+**

---

## Architectural Assessment

### Strengths

1. **SharedMessage reuse is exactly right** (PROTOCOL.md Section 3.2, SHARED_MESSAGE_INTEGRATION.md). Avoiding a parallel `WireMessage` type eliminates dual serialization paths, schema duplication, and the inevitable confusion about which type to use at which layer boundary. The existing `SharedMessage` already provides `CanonicalAddress` for cross-runtime addressing, `correlationId` + `pattern` for request-response semantics, `ttl` for expiration, `signature` for future HMAC, and `metadata` for extensibility. The comparison table in SHARED_MESSAGE_INTEGRATION.md (lines 386-402) makes the case conclusively.

2. **Cloudflare constraint awareness is thorough and honest** (PROTOCOL.md Section 11, SCALABILITY.md). The design does not hand-wave over platform limits. The 30s CPU limit, 30s hibernation timeout, 1MB WebSocket frame limit, 128MB storage soft limit, and ~50K connection limit per DO are all explicitly addressed with concrete mitigations. The cascade failure analysis in SCALABILITY.md (the "CPU Exhaustion Cascade" diagram, lines 78-93) is particularly valuable -- it demonstrates understanding that broadcast is a denial-of-service vector if not handled properly.

3. **Server-enforced identity is a correct security primitive** (SECURITY.md Section 1.4, PROTOCOL.md Section 6.2). The design principle that the server replaces the client-provided `from` field with the JWT-verified identity eliminates an entire class of spoofing attacks. This is enforced at the session level, meaning every message through a connection automatically inherits the verified identity. This is the right approach for a centralized hub topology.

4. **The connection lifecycle state machine is minimal** (CONNECTION_LIFECYCLE.md Section 1). Using the existing 4-state `ConnectionState` enum (`disconnected | connecting | connected | disconnecting`) from the domain schema rather than inventing new states shows discipline. The explicitly enumerated invalid transitions (Section 2.3) prevent accidental state machine bloat. The edge case analysis (Sections 6.1-6.6) covering hibernation during send, connection lost during registration, concurrent connects, and disconnect during broadcast demonstrates thorough thinking.

5. **Phased delivery guarantees are pragmatic** (DELIVERY_GUARANTEES.md). Starting with at-most-once delivery for MVP and deferring at-least-once to Phase 2 is the correct trade-off. The document is honest about when messages can be lost (Table in Section 1.2) and clearly delineates which use cases require which guarantee level. The deduplication strategy (Section 6) with a TTL-bounded seen-set is well thought out.

6. **Explicit non-goals prevent scope creep** (PROTOCOL.md Section 1.4). "Not a message queue," "Not a database," "Not a CDN," and "Not ordered delivery" are all important boundaries. These prevent the system from accreting features that would compromise its core purpose as a real-time message router.

### Concerns

1. **The `metadata` bag is a typed hole doing too much structural work** -- Severity: **High**

   The `metadata: Record<string, unknown>` field carries authentication tokens (`authToken`), server-verified identity (`actorIdentity`), protocol version negotiation (`protocolVersion`), distributed tracing (`traceId`, `spanId`), delivery semantics (`requireAck`), session identification (`connectionId`, `sessionId`), actor capabilities (`capabilities`), TTL information (`ttlSeconds`, `expiresAt`), priority (`priority`), and content type negotiation (`supportedContentTypes`). This is at least 10 different concerns packed into an untyped bag.

   The metadata conventions table (PROTOCOL.md Section 3.4) documents the expected keys, but nothing enforces them at the type level. A client omitting `protocolVersion` from the connect metadata, or misspelling `authToken` as `auth_token`, will produce a runtime error with no compile-time feedback. Worse, as the protocol evolves, new metadata keys will be added without any schema migration strategy, making backward compatibility analysis impossible.

2. **The `hub:send` payload nesting creates a message-within-a-message** -- Severity: **Medium**

   Looking at the `hub:send` definition (PROTOCOL.md Section 4.4, line 570-588), the payload structure is:

   ```typescript
   payload: {
     targetAddress: '@(browser/widget-123)',
     message: {
       type: 'task:assign',
       payload: { taskId: 'task-456', priority: 1 }
     }
   }
   ```

   This creates an awkward nesting where a `SharedMessage` (the outer envelope) contains a `payload` that contains a `targetAddress` and a `message` sub-object that itself has a `type` and `payload`. The `to` field of the outer SharedMessage is `@(cloudflare/signal-hub)` (the hub itself), while the actual intended recipient is buried in `payload.targetAddress`. This dual-addressing creates ambiguity about which address the hub should route on. It also means the inner `message` is not a full `SharedMessage` -- it lacks `id`, `from`, `to`, `pattern`, `correlationId`, `timestamp`, etc. -- making it impossible to apply uniform message handling at the receiver.

3. **No `hub:refresh_token` in the MVP message type catalog** -- Severity: **Medium**

   The 24 message types enumerated in PROTOCOL.md Section 3.3 do not include a token refresh mechanism. SECURITY.md Section 1.5 describes a refresh strategy that requires disconnecting and reconnecting with a new token. For connections with long session durations (e.g., a browser tab open for hours), this means a forced disconnect/reconnect cycle every 1-4 hours in production. SECURITY.md Section 1.5 proposes `hub:refresh_token` / `hub:token_refreshed` as a Phase 2 addition, but this seems like a gap for any non-trivial deployment. The reconnection will cause temporary actor deregistration and potential message loss during the reconnection window.

4. **Replay protection seen-set is volatile memory only** -- Severity: **Medium**

   The replay protection mechanism (SECURITY.md Section 4.3, PROTOCOL.md Section 7.5) uses an in-memory `Map<string, number>` for the seen-set with periodic eviction. During a Durable Object eviction or hibernation-wake cycle, this seen-set is lost. An attacker who captures a valid message could replay it after a DO restart within the 5-minute timestamp freshness window. The two-tier storage architecture described for the actor registry (volatile + durable) is not applied to the replay protection seen-set.

5. **Cross-shard messaging is under-specified** -- Severity: **Medium**

   The sharding strategy (SCALABILITY.md Section 5, PROTOCOL.md Section 8.4) uses consistent hashing to assign actors to shards. Cross-shard discovery is well-specified (fan out query to all shards, aggregate results). However, cross-shard point-to-point messaging -- the case where Actor A on Shard 1 wants to send a message to Actor B on Shard 3 -- is not specified. The shard router (SCALABILITY.md lines 567-594) handles connection routing but not message forwarding between shards. This is a critical gap for the sharding story.

---

## Design Pattern Analysis

### Connection Lifecycle State Machine
**Grade:** A-
**Assessment:** The 4-state machine (`disconnected -> connecting -> connected -> disconnecting -> disconnected`) is the minimal correct state machine for a WebSocket connection with authentication handshake. The explicit prohibition of invalid transitions (e.g., `disconnected -> connected` without passing through `connecting`) prevents shortcut paths that would bypass authentication. The heartbeat-as-hibernation-prevention pattern (25s < 30s threshold) is well-calibrated with a 5-second safety margin. The duplicate connection resolution ("last connection wins") is simple and correct for the use case.

**Recommendations:**
- Consider adding a `reconnecting` sub-state (or at minimum a boolean flag) to distinguish a client-initiated disconnect from an error-triggered reconnect. The current model conflates these two in the `disconnected` state, requiring the reconnect manager to track this distinction externally.
- The heartbeat optimization in CONNECTION_LIFECYCLE.md Section 6.4 (skip heartbeat during high message activity) should be promoted to the main PROTOCOL.md, as it materially reduces overhead for active connections.

### Message Type System (hub:* namespace)
**Grade:** B+
**Assessment:** The `hub:*` namespace convention is clean and avoids collision with application-level message types (e.g., `task:assign`, `render`, `status:update`). The 24 message types across 5 categories (Connection: 5, Discovery: 9, Delivery: 9, Flow Control: 4, Errors: 6) provide good coverage. The request-response pairing convention (`hub:connect` -> `hub:connected`, `hub:register` -> `hub:registered`, etc.) is consistent and predictable.

However, the naming has some inconsistencies. Error types mix between specific types (`hub:unknown_actor`, `hub:unauthorized`, `hub:rate_limited`, `hub:version_mismatch`, `hub:message_too_large`) and the generic `hub:error` with a code field (`timeout`, `message_expired`, `internal_error`). This creates two parallel error reporting mechanisms -- one via distinct message types and one via error codes within `hub:error`. A client must handle both patterns.

**Recommendations:**
- Standardize on either specific error types OR `hub:error` with codes, not both. The specific-type approach is better for type-safe matching but creates namespace pressure. The code-based approach is more extensible. Pick one.
- The count of 24 types is manageable now but approaching the complexity threshold. Consider whether `hub:list_actors` / `hub:actor_list` could be folded into `hub:discover` / `hub:discovered` with a wildcard pattern to reduce the type count.
- Add a `hub:renew` / `hub:renewed` pair to the type catalog for registration renewal. These are listed in the type union (PROTOCOL.md Section 3.3, line 240) but have no corresponding detailed specification in Section 4.3.

### Security Model (JWT + Server-Enforced Identity)
**Grade:** A-
**Assessment:** The three-phase security approach (Phase 1: JWT + server-enforced identity; Phase 2: CBAC + Topic ACLs; Phase 3: HMAC signatures) is well-staged. The MVP security is solid: JWT validation on connect, server-enforced `from` field replacement, WSS transport encryption, and per-actor rate limiting. The threat model (SECURITY.md Section 5) covers the five most relevant attack vectors (address spoofing, MITM, replay, DoS, unauthorized access) with concrete mitigations.

The `jsonwebtoken` library import in the reference implementation (SECURITY.md Section 1.2) is a concern for Cloudflare Workers, which does not natively support the Node.js `crypto` module. Workers has the Web Crypto API and libraries like `jose` are Workers-compatible, but `jsonwebtoken` is not. This is an implementation detail, but it appears in the specification as reference code and could mislead implementers.

**Recommendations:**
- Replace `jsonwebtoken` references with `jose` (Workers-compatible) or the Web Crypto API directly. The specification should use libraries that work in the target runtime.
- The JWT issuer is hardcoded as `'signal-hub'` but the JWT is issued by an external auth service. The spec should clarify whether the auth service must set `iss: 'signal-hub'` or whether the issuer should match the auth service identity.
- Consider adding audience (`aud`) claim validation to scope tokens to specific Signal Hub instances or shards.

---

## Integration Analysis

### SharedMessage Reuse Decision
**Decision:** Use existing SharedMessage vs create WireMessage
**Grade:** A
**Rationale:** This was the right decision. The SharedMessage type from `@agentic-primer/protocols` already provides all the primitives needed: `CanonicalAddress` for cross-runtime addressing, `pattern` for tell/ask semantics, `correlationId` for request-response correlation, `ttl` for message expiration, `signature` for future HMAC, and `metadata` for extensibility. Creating a parallel `WireMessage` would have meant maintaining two serialization paths, two sets of validators, and an inevitable mapping layer between them. The comparison table in SHARED_MESSAGE_INTEGRATION.md conclusively demonstrates that SharedMessage is a strict superset of what WireMessage would have provided.

The one cost of this decision is that all hub-specific semantics (auth tokens, capabilities, protocol version) must be expressed through the untyped `metadata` bag rather than as first-class fields. This is the source of the "metadata bag doing too much" concern noted above. But the alternative -- forking the message type -- would be worse.

### CanonicalAddress Usage
**Grade:** B+
**Assessment:** The `@(path)` format is appropriate for hub actors. The address translation functions in `shared-message.ts` (`toCanonical` and `fromCanonical`) correctly handle all four runtimes (local, browser, cloudflare, beam). The template literal type `type CanonicalAddress = \`@(\${string})\`` provides compile-time enforcement that addresses conform to the format.

However, the path structure within `@()` is not formally defined. Examples use varied conventions: `@(browser/widget-123)`, `@(seag/processor-main)`, `@(cloudflare/signal-hub)`, `@(local/coordinator-main)`, `@(admin/privileged-actor)`, `@(verified/user-123-browser)`. There is no specification of what the path segments mean, whether they are hierarchical, or how they relate to the actor identity in the JWT. The `actorId` in the JWT payload is described as "Actor canonical address path (e.g., 'browser/client-ui')" -- meaning it is the path without the `@()` wrapper -- but the relationship between JWT `actorId` and the `from` address is only implicit.

**Recommendation:** Define a formal path grammar (e.g., `<runtime>/<actor-name>` or `<namespace>/<actor-id>`) and validate it in the `canonicalAddressSchema`. This would catch malformed addresses at parse time rather than during routing.

### Protocol Composition
The Signal Hub protocol composes well with the broader `@agentic-primer/protocols` ecosystem. It imports from the same source of truth (`domain.schema.json`), uses the same `ConnectionState` enum, and the `SharedMessage` type flows cleanly between the protocols package and the hub. The converter functions (`simplifyToShared`, `brianToShared`, `sharedToBrian`) in `shared-message.ts` provide integration points for the existing actor runtimes.

One integration concern: the `SimplifyMessage` interface in `shared-message.ts` (line 145-155) supports a `'stream'` pattern value that maps to `'tell'` in SharedMessage, but Signal Hub's `hub:*` messages never use `'stream'`. If streaming semantics are added later (e.g., for large data transfers), the protocol would need to accommodate this pattern.

---

## Scalability & Performance

### Cloudflare Constraints Handling

| Constraint | Strategy | Grade | Notes |
|------------|----------|-------|-------|
| 30s CPU limit | Async queue for >100 actors | **A** | Well-designed batch splitting with first batch synchronous and rest queued. The 100-actor threshold is empirically grounded (100 x 30ms = 3s). |
| 1MB WebSocket frames | Message size validation + `hub:message_too_large` error | **A-** | Clean error reporting. Missing: guidance on message chunking protocol for payloads that legitimately need >1MB. |
| 30s hibernation | 25s heartbeat interval | **A** | 5s safety margin is appropriate. The heartbeat optimization (skip during activity) in CONNECTION_LIFECYCLE.md is a good refinement. |
| 128MB DO storage | 50K actor limit with two-tier storage | **B+** | Conservative limit (theoretical max 180K). Two-tier volatile+durable is correct. Missing: explicit memory accounting for the broadcast queue and seen-set cache. |
| 50K WebSocket connections | Rate limiting + capacity monitoring | **B+** | Token bucket limiter at 100 conn/sec is reasonable. Missing: graceful connection draining during shard rebalancing. |
| 128KB queue message size | Batch actors in groups of 100 | **A-** | Good fit with queue constraints. The `BroadcastJob` payload must stay under 128KB -- this is tight if message payloads are large. |

### Bottleneck Analysis

1. **The `getWebSockets().find()` lookup is O(N)** (SCALABILITY.md, lines 118-119, 248-249). During broadcast, for each actor in a batch, the implementation calls `sockets.find(s => s.actorAddress === actor.actorAddress)`. With 100 actors per batch and potentially 50K total sockets, this is O(100 * 50K) = O(5M) comparisons per batch. This should use a `Map<string, WebSocket>` for O(1) lookup.

2. **Sequential batch queuing under load**. The `largeBroadcast` function (SCALABILITY.md lines 157-190) queues batches sequentially with `await this.env.BROADCAST_QUEUE.send(job)` in a for loop. For a 10K actor broadcast (100 batches), this is 99 sequential queue writes. Consider using `Promise.all` for parallel queue submission, or better yet, a single batch message with multiple entries if the Cloudflare Queue API supports it.

3. **Replay protection memory growth**. The seen-set for replay protection (SECURITY.md Section 4.3) with 60-second TTL and cleanup every 5 minutes means the set could grow to hold up to 5 minutes of message IDs. At 100 messages/second (the per-actor rate limit), that is 30,000 entries per actor times 50K actors = 1.5 billion entries in the worst case. The practical limit of 10,000 entries (DELIVERY_GUARANTEES.md Section 6.3) is good, but the memory pressure from per-actor rate limiting is not analyzed.

4. **Cross-shard discovery is a fan-out multiplier**. Every `hub:discover` query fans out to all shards (SCALABILITY.md lines 600-658). With 10 shards, each discovery is 10x the work. With 100 shards, it is 100x. The spec does not address pagination at the shard level or caching of discovery results.

---

## Missing Pieces & Implicit Assumptions

### Critical Gaps

1. **Cross-shard message routing is not specified.** The sharding strategy covers connection routing (which shard an actor connects to) and cross-shard discovery (fan-out query), but does not specify what happens when Actor A on Shard 1 sends `hub:send` targeting Actor B on Shard 3. Does Shard 1 forward the message to Shard 3? Does the client need to know which shard the target is on? This is the most important gap in the sharding design and will be painful to retrofit.

2. **No message schema validation for `hub:*` type payloads.** The `SharedMessage` has `payload: unknown`. Each `hub:*` message type expects a specific payload shape (e.g., `hub:register` expects `{ actorAddress, capabilities, metadata, ttlSeconds }`), but there is no Zod schema or JSON Schema for these payloads. The `sharedMessageSchema` in `shared-message.ts` validates the envelope but treats payload as `z.unknown()`. Without payload validation, malformed messages will pass parsing and fail at the handler level with unclear errors.

3. **No graceful shutdown protocol for the hub itself.** The design specifies `hub:disconnect` for actor disconnection and mentions `server_shutdown` as a disconnect reason, but does not define a protocol for the Signal Hub Durable Object itself shutting down (e.g., during deployment). How does the hub notify all connected actors? Is there a `hub:server_shutdown` broadcast? What is the expected client behavior? The thundering herd protection addresses reconnection after restart, but the shutdown notification path is missing.

4. **Token refresh requires disconnect/reconnect in MVP.** As noted in the Concerns section, the lack of `hub:refresh_token` in the MVP means a forced reconnection cycle every few hours. For a browser tab that stays open all day, this means 6-24 disconnection/reconnection events, each of which deregisters the actor and potentially loses in-flight messages.

### Implicit Assumptions That Should Be Explicit

1. **Single DO instance per logical hub.** The protocol assumes a single Durable Object handles all connections within a shard. This assumption breaks if Cloudflare's platform ever changes the DO consistency model or introduces multi-region replication. The design should explicitly state this assumption and document behavior if it changes.

2. **Clock synchronization.** The heartbeat ack includes `serverTime` for clock skew detection (PROTOCOL.md Section 4.2, line 455), but the protocol does not specify what to do if skew exceeds a threshold. TTL expiration (`timestamp + ttl < now`) is sensitive to clock differences between client and server. The 5-minute replay rejection window implicitly assumes <5 minutes of clock skew.

3. **Message ordering within the hub.** The per-connection FIFO guarantee assumes the hub processes messages from each WebSocket sequentially. If the Durable Object processes WebSocket messages concurrently (which Cloudflare allows via `blockConcurrencyWhile` or not), the FIFO guarantee would break. The design should explicitly state that messages from each WebSocket are processed sequentially.

4. **JSON serialization only.** The protocol implicitly assumes JSON serialization for all messages (via `JSON.stringify`/`JSON.parse`). The `hub:connected` payload mentions `supportedContentTypes: ['json', 'msgpack']`, suggesting msgpack support is planned, but no specification exists for binary serialization. The 1MB WebSocket frame limit is more restrictive for JSON (text) than for binary (msgpack) encoding.

### Nice-to-Haves

- **Presence/status notification**: When an actor comes online or goes offline, other actors interested in that actor's status are not notified. A `hub:actor_online` / `hub:actor_offline` event would enable reactive patterns.
- **Message acknowledgment from the target actor** (not just the hub): The current `hub:delivery_ack` confirms the hub delivered the message to the target's WebSocket, not that the target processed it. An end-to-end acknowledgment would require the target to explicitly ack.
- **Bulk operations**: Registering multiple actors or subscribing to multiple topics in a single message would reduce round trips for initialization-heavy clients.
- **Connection metadata queries**: Allow clients to query their own connection state (`hub:connection_info`) to verify session ID, verified identity, remaining TTL, etc.
- **Message priority queuing**: The metadata supports `priority: 0|1|2` but the queue processing does not prioritize. High-priority messages in a large broadcast should be delivered first.

---

## Recommendations

### P0 (Must Fix Before Implementation)

No critical blocking issues were found. The design is ready for implementation with the P1 items noted below as soon-after improvements. The architectural foundation is sound.

**Explicit declaration: No P0 issues found.**

The design demonstrates sufficient rigor for an MVP implementation to proceed. All concerns identified are addressable without fundamental redesign.

### P1 (Should Fix Soon)

1. **Define typed payload schemas for each `hub:*` message type.**

   Create Zod schemas (or at minimum TypeScript interfaces) for each message type's payload. Add a `hubPayloadSchemas` map keyed by message type. Validate payloads on both client (before send) and server (on receive). This will catch malformed messages at parse time, improve developer experience with autocomplete, and make the protocol self-documenting.

   ```typescript
   const hubRegisterPayloadSchema = z.object({
     actorAddress: canonicalAddressSchema,
     capabilities: z.array(z.string()).min(1),
     metadata: z.record(z.unknown()).optional(),
     ttlSeconds: z.number().int().min(1).max(3600).default(300),
   });
   ```

   Impact: Developer experience, error quality, protocol safety.

2. **Flatten the `hub:send` payload to avoid message-within-a-message.**

   Instead of nesting `targetAddress` and `message` inside the payload, use the SharedMessage `to` field for routing and put the forwarded content directly in `payload`:

   ```typescript
   // Current (nested):
   { to: '@(cloudflare/signal-hub)', payload: { targetAddress: '@(target)', message: {...} } }

   // Proposed (flat):
   { to: '@(target)', payload: { type: 'task:assign', data: {...} }, metadata: { via: 'signal-hub' } }
   ```

   This eliminates the dual-addressing ambiguity and makes the inner message a proper `SharedMessage` field. The hub can route based on the `to` field and the `hub:send` type discriminator.

   Impact: Simplicity, consistency, reduced error surface.

3. **Add `hub:refresh_token` to the MVP message catalog.**

   Token refresh without disconnect is essential for long-lived connections. The design already describes this in SECURITY.md Section 1.5 as a Phase 2 feature, but it should be promoted to MVP. The implementation is straightforward: client sends new JWT in `hub:refresh_token`, server validates, updates session, responds with `hub:token_refreshed`.

   Impact: Reliability for browser-based actors with long sessions.

4. **Specify cross-shard message forwarding.**

   When `hub:send` targets an actor on a different shard, define the forwarding protocol. Options: (a) The shard router intercepts and forwards, (b) The source shard makes a fetch to the target shard's DO, or (c) The client is redirected to the correct shard. Option (b) is simplest: the source shard's DO calls `targetShardStub.fetch()` with the message. Define the internal API endpoint and error handling for shard-to-shard communication.

   Impact: Correctness of the sharding story.

5. **Replace `jsonwebtoken` references with Workers-compatible library.**

   The reference implementation code in SECURITY.md uses `import { decode, verify } from 'jsonwebtoken'`, which is a Node.js library that does not run in Cloudflare Workers. Replace with `jose` or the Web Crypto API. This is a documentation fix but an important one since the spec serves as implementation guidance.

   Impact: Implementability on the target platform.

### P2 (Consider for V2)

1. **Introduce typed metadata keys with a registry pattern.** Instead of freeform `Record<string, unknown>`, define a `HubMetadataKey` enum and typed accessors:

   ```typescript
   type HubMetadata = {
     'hub:protocolVersion'?: string;
     'hub:authToken'?: string;
     'hub:actorIdentity'?: CanonicalAddress;
     'hub:traceId'?: string;
     [key: string]: unknown; // extensible
   };
   ```

   This preserves extensibility while providing type safety for known keys.

2. **Persist the replay protection seen-set to durable storage.** Use a lightweight approach: write the seen-set to durable storage on every N-th message or on hibernation (using `blockConcurrencyWhile`), and restore it on wake alongside the actor registry. This closes the replay-after-restart vulnerability.

3. **Add actor presence notifications.** Define `hub:actor_joined` and `hub:actor_left` events that are broadcast (or published to a system topic) when actors register/unregister. This enables reactive coordination patterns without polling `hub:discover`.

4. **Define a message chunking protocol for payloads approaching 1MB.** When a message payload is large, the client should be able to split it into chunks with a `hub:chunk_start` / `hub:chunk` / `hub:chunk_end` sequence, with the hub reassembling before delivery. Alternatively, define an R2 object reference pattern for out-of-band large payloads.

5. **Formalize the CanonicalAddress path grammar.** Define a BNF or regex for valid path structures within `@()`. Document the convention for runtime prefixes (`browser/`, `seag/`, `cloudflare/`, `admin/`) and validate at parse time.

6. **Add connection draining for shard rebalancing.** When adding or removing shards, the current design disconnects actors with `reason: 'shard_rebalancing'`. Instead, implement connection draining: stop accepting new connections on the old shard, allow existing connections to continue until they naturally disconnect or the TTL expires, then decommission the shard.

---

## Overall Grade: B+

**Summary:** Signal Hub's protocol design is thorough, well-structured, and demonstrates strong understanding of both the Cloudflare platform constraints and actor-model coordination patterns. The SharedMessage reuse decision is the cornerstone of the architecture and it was made correctly. The security model is appropriately phased, the state machine is minimal, the scalability strategy is production-aware, and the document set is comprehensive. The primary weaknesses are in the type safety of the metadata bag, the nested payload structure of `hub:send`, and some under-specification in the sharding story. None of these are architectural blockers -- they are refinements that can be addressed during or shortly after initial implementation. The protocol is ready to be built.

**Confidence:** High. All 7 documents were read in full. The source code of `shared-message.ts` was reviewed to verify alignment between the protocol specification and the actual type definitions. The analysis covers architecture, design patterns, security, scalability, integration, and missing pieces. The primary area of lower confidence is the Cloudflare platform specifics (e.g., exact WebSocket `send()` latency, DO eviction behavior) which are based on the spec's own measurements rather than independent verification.
