# FluxRelayActor Design

Date: 2026-02-21

---

## Why This Actor Exists

SttActor (nova-3) uses the Deepgram REST API: it accumulates audio into a buffer, flushes when that buffer crosses a threshold (~32 KB), and gets back a single transcript per HTTP POST. That batch approach introduces mandatory latency — you always wait for a full buffer before you see any words. Deepgram's `@cf/deepgram/flux` model works differently: it is a real-time streaming WebSocket API that emits interim transcripts continuously as audio arrives, with a final transcript when the speaker pauses. FluxRelayActor exists to bridge that streaming protocol into the actor mesh. Its entire job is bidirectional relay: binary audio frames flow upstream to the Flux WebSocket, and transcript events flow back downstream to whichever channel sent the audio. No ML, no buffering strategy, no REST calls — just a relay with correct per-channel routing.

---

## Name Rationale

"Flux" names the upstream model (`@cf/deepgram/flux`) and connotes continuous flow. "Relay" names the actor's role precisely: it is not a processor, it is a conduit. The combined name telegraphs both the upstream dependency and the architectural pattern to any reader of the codebase.

---

## Address

```
ai/flux/<namespace>
```

Example: `ai/flux/bln_ai`

**Rationale.** The address encodes capability (`flux`), not provider + model, because there is only one Flux model. Adding `/deepgram/flux` would be redundant segments that carry no routing information — no factory needs to differentiate by provider or model within this capability. The namespace segment isolates tenant credentials and DO instances. One address = one named DO instance = one shared upstream Flux WebSocket for all callers in that namespace (see Q6 below).

---

## Interface

```typescript
// packages/ai/src/flux-relay-actor.ts

import type { Message, MessageResponse, MessageHandler, Address } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type {
  SttStartPayload,
  SttTranscriptPayload,
  DiscoverResponsePayload,
  HealthPayload,
  HealthResponsePayload,
} from './types.ts';

export interface FluxRelayActorConfig {
  /** Cloudflare AI Gateway WebSocket URL for @cf/deepgram/flux. */
  fluxWsUrl: string;
  /**
   * CF AI Gateway token (CF_AIG_TOKEN).
   * Injected at construction from env binding — never from message payload.
   */
  aigToken: string;
  /** Namespace this actor serves (extracted from address). */
  namespace: string;
}

export interface FluxStartPayload {
  channelId: string;
  encoding?: 'linear16' | 'mulaw' | 'opus';
  sampleRate?: number;
  language?: string;
}

export interface FluxStopPayload {
  channelId: string;
}

export class FluxRelayActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly config: FluxRelayActorConfig;

  /** Active sender sessions keyed by channelId (FNV-1a string, e.g. "2847563921"). */
  private channels = new Map<string, FluxChannel>();

  /**
   * The single upstream Flux WebSocket shared by all channels in this namespace.
   * null = not yet connected.
   */
  private fluxWs: WebSocket | null = null;
  private fluxWsState: 'disconnected' | 'connecting' | 'connected' | 'failed' = 'disconnected';

  /** Optional system reference for dispatching transcripts — set via factory. */
  protected system: { send(to: Address, type: string, payload: unknown): void } | null = null;

  constructor(address: string, config: FluxRelayActorConfig) {
    this.actorAddress = address;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      switch (message.type) {
        case AI_MESSAGE_TYPES.STT_START:    return await this.handleStart(message);
        case AI_MESSAGE_TYPES.STT_STOP:     return await this.handleStop(message);
        case AI_MESSAGE_TYPES.AUDIO_FRAME:  return await this.handleAudioFrame(message);
        case AI_MESSAGE_TYPES.HEALTH:       return this.handleHealth(message);
        case AI_MESSAGE_TYPES.DISCOVER:     return this.handleDiscover(message);
        default:
          return createErrorResponse(
            message,
            `FluxRelayActor(${this.actorAddress}): unhandled message type '${message.type}'`,
          );
      }
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  // Handlers — see section bodies below
  private async handleStart(message: Message): Promise<MessageResponse> { /* ... */ return createResponse(message, {}); }
  private async handleStop(message: Message): Promise<MessageResponse>  { /* ... */ return createResponse(message, {}); }
  private async handleAudioFrame(message: Message): Promise<MessageResponse> { /* ... */ return createResponse(message, {}); }
  private handleHealth(message: Message): MessageResponse { /* ... */ return createResponse(message, {}); }
  private handleDiscover(message: Message): MessageResponse { /* ... */ return createResponse(message, {}); }

  /** Override in system-integrated variant to use system.send(). */
  protected dispatchTranscript(replyTo: Address, payload: SttTranscriptPayload): void {
    void replyTo; void payload;
  }

  /** Override in system-integrated variant to use system.send(). */
  protected notifyChannelError(replyTo: Address, error: string): void {
    void replyTo; void error;
  }
}
```

---

## Message Protocol

| Message Type | Direction | Payload | Notes |
|---|---|---|---|
| `audio.frame` | in | `Uint8Array` | Binary fast-path; channelId encoded as 4-byte LE prefix by transport layer; actor receives raw PCM |
| `ai.stt.start` | in | `FluxStartPayload` | Opens a channel entry; triggers lazy Flux WS connection; `channelId` is required |
| `ai.stt.stop` | in | `FluxStopPayload` | Closes the channel entry; does NOT close Flux WS (shared) |
| `ai.stt.transcript` | out | `SttTranscriptPayload` | Dispatched to the sender address stored per-channel; `isFinal` mirrors Flux's `is_final` field |
| `ai.stt.error` | out | `{ channelId: string, error: string }` | Sent to all active senders when Flux WS drops unrecoverably |
| `ai.health` | in | `HealthPayload` | Returns Flux WS state in `meta`; `status: 'degraded'` if `fluxWsState !== 'connected'` |
| `ai.discover` | in | `DiscoverPayload` | Returns capabilities, handled message types, and namespace |

**Note on `audio.frame` and `ai.stt.start` ordering:** `ai.stt.start` MUST arrive before the first `audio.frame` for a given channelId, because `start` establishes the `replyTo` address. An `audio.frame` arriving for an unknown channelId is silently dropped (matching the binary-audio-pattern fast-path rule for missing channelIds).

---

## State Shape

```typescript
/** Per-channel (per-sender) state within a FluxRelayActor instance. */
interface FluxChannel {
  /** The address to dispatch ai.stt.transcript messages back to. */
  replyTo: Address;
  /** Audio encoding, used for Flux WS query params. */
  encoding: 'linear16' | 'mulaw' | 'opus';
  sampleRate: number;
  language: string;
  /** Timestamp of last audio.frame, for idle-channel detection. */
  lastFrameAt: number;
}

/** Full actor-owned state (all ephemeral — no DO SQLite persistence). */
interface FluxRelayState {
  /**
   * Active channels keyed by channelId string.
   * channelId is the FNV-1a hash of the actor address, as a decimal string.
   * Populated by ai.stt.start, removed by ai.stt.stop or upstream failure.
   */
  channels: Map<string, FluxChannel>;

  /**
   * The single upstream Flux WebSocket.
   * Opened lazily on first ai.stt.start. Shared by all channels.
   */
  fluxWs: WebSocket | null;

  /**
   * Lifecycle state of the Flux WebSocket.
   * Used to prevent double-open and to report accurate health status.
   */
  fluxWsState: 'disconnected' | 'connecting' | 'connected' | 'failed';
}
```

**No DO SQLite persistence.** All state is ephemeral. The Flux WS is a live streaming connection — there is nothing durable to checkpoint. If the DO evicts, the WebSocket connection ends; clients must re-send `ai.stt.start` to reopen a channel. Transcript history is not stored; callers that need persistence must handle it upstream.

---

## Flux WebSocket Lifecycle

### Q1 Decision: Lazy open on first `ai.stt.start`

The Flux WS is opened when the first `ai.stt.start` message arrives, not at construction and not on `audio.frame`.

**Rationale:**
- Construction-time open is wrong: the actor is constructed by the virtual actor factory the moment any message arrives at the address prefix. That message could be `ai.discover` or `ai.health` — opening a WS unconditionally wastes the connection budget.
- `audio.frame`-time open is too late: audio arrives immediately after start in real usage, and the WS handshake takes one RTT. Frames arriving before the WS is ready would need buffering, introducing complexity with no benefit.
- `ai.stt.start` is the correct trigger: it is an explicit signal that a channel is active and audio will follow. It arrives before any audio frames (enforced by the client protocol).

### Open sequence

```
1. handleStart() called → fluxWsState === 'disconnected'
2. Set fluxWsState = 'connecting'
3. new WebSocket(fluxWsUrl, { headers: { Authorization: `Bearer ${aigToken}` } })
4. Register onopen, onmessage, onerror, onclose handlers
5. onopen → fluxWsState = 'connected'; return ok to start caller
6. onerror / onclose unexpectedly → see Failure section
```

If a second `ai.stt.start` arrives while `fluxWsState === 'connecting'`, the actor registers the new channel in `this.channels` and returns `ok` immediately — the in-flight WS will serve it when it opens. No second WS is created.

If a second `ai.stt.start` arrives while `fluxWsState === 'connected'`, the channel is registered and `ok` is returned immediately. The existing WS is reused.

### Transcript dispatch on `onmessage`

```
fluxWs.onmessage = (event) => {
  const msg = JSON.parse(event.data as string);
  // msg.channel: number (Flux multiplexes by channel index)
  // msg.is_final: boolean
  // msg.channel_index corresponds to channelId registration order
  // → see Multi-Channel Routing section
  broadcastToAllChannels(msg);
}
```

### Failure handling

When `onerror` or `onclose` fires unexpectedly (not as a result of the actor calling `fluxWs.close()`):

1. Set `fluxWsState = 'failed'`
2. For every entry in `this.channels`, call `notifyChannelError(channel.replyTo, 'Flux upstream disconnected')`
3. Clear `this.channels`
4. Set `fluxWs = null`, `fluxWsState = 'disconnected'`

No automatic reconnect. The Flux WS is stateful at the model level — it has seen audio it cannot replay. Reconnecting silently would corrupt the transcript stream. The correct recovery path is for the client to re-send `ai.stt.start` (after receiving the error notification), which triggers a fresh WS open. This is simple, auditable, and matches how CF Workers streaming connections work in practice.

### Shutdown (all channels closed)

When `ai.stt.stop` removes the last entry from `this.channels`:

1. Call `fluxWs.close(1000, 'no active channels')`
2. Set `fluxWs = null`, `fluxWsState = 'disconnected'`

This frees the upstream connection when idle. A new `ai.stt.start` will reopen it.

---

## Multi-Channel Routing

### Q2 Decision: Broadcast to all active senders

The Flux WebSocket returns a single merged transcript stream. The model has no awareness of which "channel" (in the actor's sense of channelId) sent which audio — all audio sent over the WebSocket is mixed at the model level.

**Decision: broadcast every Flux transcript event to all active senders.**

```typescript
private broadcastTranscript(fluxEvent: FluxTranscriptEvent): void {
  const payload: SttTranscriptPayload = {
    transcript: fluxEvent.transcript,
    isFinal: fluxEvent.is_final,
    confidence: fluxEvent.confidence,
  };
  for (const [, channel] of this.channels) {
    this.dispatchTranscript(channel.replyTo, payload);
  }
}
```

**Rationale:** The Flux model receives all audio frames from all channels as a single mixed stream. It is impossible for the actor to know which words came from which sender, because the model itself doesn't know — it sees one audio stream. Attempting per-channel correlation would require either:
- (a) separate WebSocket per channel — multiplies upstream connections, defeats the "shared upstream" goal, and would be costly in CF Workers; or
- (b) out-of-band speaker diarization — a separate ML step the actor explicitly does not do.

The intended deployment scenario is that FluxRelayActor serves a namespace (e.g. `bln_ai`) which in practice has one or two active channels at a time (a user's mic input). Broadcast to all active senders is the correct semantic: every participant in the namespace session receives the transcript of the mixed audio they collectively sent.

If true per-speaker isolation is required in the future, the design point is to launch a separate `ai/flux/<namespace>` DO instance per speaker (each with its own channelId) rather than to add per-channel correlation logic inside a single actor instance.

### Per-channel `replyTo` tracking

The `replyTo` address stored in `FluxChannel` is `message.from` captured at `ai.stt.start`. This follows the exact pattern in SttActor: the actor never stores WebSocket references, only the logical sender address. Dispatch uses `system.send(replyTo, ...)`.

---

## Credential Injection

### Q5 Decision: Config construction from `env` binding at factory time

`CF_AIG_TOKEN` (the Cloudflare AI Gateway bearer token needed to authenticate the Flux WebSocket upgrade) is injected at actor construction via `FluxRelayActorConfig.aigToken`.

```typescript
// In the DO constructor or factory:
const actor = new SystemFluxRelayActor(
  'ai/flux/bln_ai',
  {
    fluxWsUrl: `https://gateway.ai.cloudflare.com/v1/${env.CF_ACCOUNT_ID}/${env.CF_GATEWAY_NAME}/deepgram/flux`,
    aigToken: env.CF_AIG_TOKEN,
    namespace: 'bln_ai',
  },
  system,
);
```

**Rationale:** Three options were considered:

| Option | Problem |
|--------|---------|
| Via `CredentialsActor` message | Requires an async round-trip before the first WS open; introduces failure mode (CredentialsActor unavailable) on the hot path; adds message complexity |
| Via `ai.stt.start` payload | Callers would need to know the token — a security anti-pattern. Tokens belong in the server environment, not in client messages |
| Via `env` binding at construction | Simple, synchronous, follows existing GatewayActor and SttActor patterns; `env` is the CF Workers canonical credential store |

The factory receives `env` from the DO and bakes the token into `FluxRelayActorConfig`. The actor never reads from `env` directly.

---

## Named DO Deployment

### Q6 Decision: `idFromName(namespace)` — one DO per namespace

```
ai/flux/bln_ai  →  FLUX_RELAY.idFromName('bln_ai')  →  one DO instance
ai/flux/corp_ai →  FLUX_RELAY.idFromName('corp_ai') →  separate DO instance
```

**Rationale:** One Flux WS per actor instance, one actor instance per namespace. This matches the requirement "own one flux WebSocket per actor instance (not per channel)." The namespace is a tenant-level isolation boundary: different namespaces have different credentials and different billing contexts. Using `idFromName(namespace)` ensures that multiple transport connections (WebSocket, HTTP binary) serving the same namespace all route to the same DO instance, sharing the single upstream Flux WS.

Using `idFromName(sessionId)` instead would be appropriate if there were per-session Flux contexts — but the design says one actor = one shared upstream. One per namespace is the correct granularity.

Using `newUniqueId()` would mean every new connection gets its own DO and its own Flux WS — wasteful and wrong for shared sessions.

### wrangler.toml snippet

Add to the Worker that hosts this DO (e.g. a new `services/flux-relay/wrangler.toml` or alongside `signal-hub`):

```toml
name = "flux-relay"
main = "src/index.ts"
compatibility_date = "2024-12-01"
compatibility_flags = ["nodejs_compat"]

[durable_objects]
bindings = [
  { name = "FLUX_RELAY", class_name = "FluxRelayDO" }
]

[[migrations]]
tag = "v1"
new_classes = ["FluxRelayDO"]

[vars]
# CF AI Gateway config
CF_ACCOUNT_ID  = "your-account-id"
CF_GATEWAY_NAME = "your-gateway-name"

# Secrets (set via: wrangler secret put CF_AIG_TOKEN)
# CF_AIG_TOKEN = "<set via wrangler secret>"
```

The DO class:

```typescript
// services/flux-relay/src/index.ts
export class FluxRelayDO implements DurableObject {
  private bridge: WebSocketBridge;

  constructor(ctx: DurableObjectState, env: Env) {
    const namespace = /* extracted from URL path by Worker routing */ 'bln_ai';
    const fluxWsUrl = `https://gateway.ai.cloudflare.com/v1/${env.CF_ACCOUNT_ID}/${env.CF_GATEWAY_NAME}/deepgram/flux`;
    const system = new ActorSystem({ name: `flux-relay-${namespace}` });

    system.registerFactory({
      prefix: AI_PREFIXES.FLUX,   // 'ai/flux/'
      factory: createFluxRelayFactory({ fluxWsUrl, aigToken: env.CF_AIG_TOKEN }, system),
    });

    this.bridge = new WebSocketBridge(ctx, system);
  }

  async fetch(request: Request): Promise<Response> {
    return this.bridge.handleUpgrade(request);
  }

  async webSocketMessage(ws: WebSocket, msg: string | ArrayBuffer): Promise<void> {
    this.bridge.handleMessage(ws, msg);
  }

  async webSocketClose(ws: WebSocket): Promise<void> {
    this.bridge.handleClose(ws);
  }
}

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);
    const namespace = url.pathname.split('/')[2] ?? 'default';  // /flux/<namespace>
    const id = env.FLUX_RELAY.idFromName(namespace);
    const stub = env.FLUX_RELAY.get(id);
    return stub.fetch(request);
  },
};
```

---

## Key Design Decisions

1. **Lazy Flux WS open on `ai.stt.start`, not construction.** Construction is triggered by `ai.discover` and `ai.health` too. Opening a WebSocket on every actor instantiation would be wasteful and incorrect. `ai.stt.start` is the explicit signal that audio is coming. (See transport-actor-boundary.md: DO lifecycle and resource management.)

2. **One Flux WS shared by all channels within one actor instance.** The Flux model is designed as a streaming WebSocket — the connection is cheap to hold open but expensive to open repeatedly. One WS per actor, per namespace, is the right tradeoff. Per-channel WS would multiply upstream connections and defeat the "named DO / shared session" design. (See cross-transport-pattern.md: `idFromName` for shared sessions.)

3. **Broadcast transcripts to all active senders.** The Flux upstream does not carry sender metadata per audio frame — all frames sent over the WS are mixed. Broadcast is the only architecturally honest option. Callers that need speaker isolation must use separate actor instances. (See multi-channel routing section above.)

4. **No automatic reconnect after Flux WS failure.** The Flux connection is stateful at the model level; silent reconnect would corrupt the transcript stream. Callers receive `ai.stt.error`, clean up, and re-initiate with a new `ai.stt.start`. This keeps failure modes visible and auditable. (See ANTI_PATTERNS.md #8: never hide failures.)

5. **`ai.stt.stop` closes the channel entry but does not close the Flux WS unless it is the last channel.** Multiple channels share the upstream. Closing the WS when one channel stops would disrupt other active channels. Only when `this.channels` is empty does the actor close the WS.

6. **Credentials injected at factory/construction time from `env`, not via messages.** Tokens in client messages are a security anti-pattern. `env` bindings are the CF Workers canonical credential store. This matches the pattern established by GatewayActor and SttActor.

7. **`channelId` is a string key in `this.channels`, not an integer.** The FNV-1a hash is generated by the transport layer and arrives as a 4-byte prefix on binary frames. The actor receives it after transport decoding. Storing as a string avoids uint32 overflow ambiguity and is consistent with how the binary-audio-pattern doc describes `channelMap: Map<number, string>` at the transport level vs. string keys at the actor level.

8. **Address is `ai/flux/<namespace>` (two segments after `ai/`), not the full `ai/stt/<ns>/deepgram/flux`.** This actor is not a generic STT actor — it is a specific real-time relay capability. The shorter address reflects that there are no provider/model routing dimensions to encode; there is exactly one flux model. An `AI_PREFIXES.FLUX = 'ai/flux/'` constant must be added to `packages/ai/src/index.ts`. (See address-conventions.md: capability segment encodes routing dimensions.)

9. **`dispatchTranscript` and `notifyChannelError` are overridable hooks, not direct `system.send` calls.** This follows the SttActor pattern (base class with protected hook, `SystemSttActor` subclass with real dispatch). The base class is testable without a live ActorSystem. The `SystemFluxRelayActor` subclass wires real dispatch.

10. **State is entirely ephemeral — no DO SQLite.** The Flux WS is a live streaming connection with no meaningful checkpoint state. DO storage would add write latency on every audio frame path. On DO eviction, the WS naturally terminates; clients reconnect via `ai.stt.start`.

---

## What This Is NOT

FluxRelayActor is not a replacement for SttActor: SttActor handles batch REST transcription via Deepgram nova-3 (high-accuracy, post-hoc), while FluxRelayActor handles real-time streaming via the flux model (low-latency, continuous). FluxRelayActor is also not a VoiceSession or general session coordinator — it owns no conversation state, no LLM routing, and no turn-taking logic; it is a single-purpose audio relay with transcript dispatch.

---

## Implementation Notes

1. **CF Workers `new WebSocket()` in Durable Objects.** CF Workers support outbound WebSocket connections from within a DO via the standard `new WebSocket(url)` constructor. The `cf-connecting-ip` and other CF headers are added automatically. The `headers` option for auth must be passed at construction time, not added later.

2. **Flux WS URL for CF AI Gateway.** The URL format for routing through AI Gateway to the Deepgram Flux model is:
   ```
   wss://gateway.ai.cloudflare.com/v1/<account-id>/<gateway-name>/deepgram/flux
   ```
   The `Authorization: Bearer <CF_AIG_TOKEN>` header authenticates at the gateway. This is different from the Deepgram API key — it is the AI Gateway token. Both may be needed depending on gateway configuration.

3. **Flux transcript message shape.** The Deepgram Flux WebSocket emits JSON events with the shape:
   ```json
   { "type": "Results", "channel": { "alternatives": [{ "transcript": "...", "confidence": 0.99 }] }, "is_final": false }
   ```
   The actor maps this to `SttTranscriptPayload { transcript, isFinal, confidence }`.

4. **`audio.frame` ordering requirement.** The binary channel fast-path delivers frames in order (TCP-ordered WebSocket frames). The actor passes them directly to `fluxWs.send(audioBytes)` without buffering. Do not re-buffer — flux is designed for streaming, not batching.

5. **DO hibernation and WebSocket.** The Flux upstream WS is an outbound connection from the DO, not an inbound hibernatable WebSocket. It does not survive DO eviction/hibernation. Plan accordingly: treat Flux WS lifetime as coterminous with the DO's active wakeup period.

6. **Concurrent `ai.stt.start` race condition.** If two `ai.stt.start` messages arrive nearly simultaneously while `fluxWsState === 'connecting'`, both should register their channels and return `ok`. Only the first should trigger the `new WebSocket(...)` call. Guard with `if (this.fluxWsState === 'disconnected')` before opening — the CF Workers DO single-threaded event loop makes this race-free at the JS level.

7. **`AI_PREFIXES.FLUX` must be added to `packages/ai/src/index.ts`.** The factory registration uses this constant. Add:
   ```typescript
   FLUX: 'ai/flux/',
   ```
   and a corresponding address builder:
   ```typescript
   export function fluxRelayAddress(namespace: string): string {
     return `${AI_PREFIXES.FLUX}${namespace}`;
   }
   ```

8. **`SttTranscriptPayload` is reused as-is.** The existing type in `types.ts` already captures `transcript`, `isFinal`, `confidence`, and `words`. No new payload type is needed for the transcript output. The `FluxStartPayload` and `FluxStopPayload` types (which carry `channelId`) are new and should be defined in `flux-relay-actor.ts` or added to `types.ts`.

---

## Quality Audit Checklist

```
[x] Single responsibility — actor manages ONE domain concept (relay audio to Flux WS, return transcripts)
[x] State privately owned — no external mutation; channels and fluxWs are private
[x] Address follows runtime convention — ai/flux/<namespace> matches ai/... prefix pattern
[x] Message types are semantic — ai.stt.start, ai.stt.stop, ai.stt.transcript, ai.stt.error, audio.frame
[x] Commands and events are distinct — start/stop are commands; transcript/error are events (outbound)
[x] Binary fast-path for audio — audio.frame receives Uint8Array, forwarded directly to fluxWs.send()
[x] Transport concerns isolated — actor holds fluxWs (upstream, not client-facing); client dispatch via system.send
[x] receive() returns createErrorResponse, never throws — try/catch wraps all handlers
[x] Session cleanup handled — ai.stt.stop removes channel; last channel closes Flux WS; failure clears all channels
[x] Named DO (idFromName) for cross-transport sessions — idFromName(namespace) for shared Flux WS
[x] Virtual actor factory used — createFluxRelayFactory registers on AI_PREFIXES.FLUX prefix
[x] Tests should cover: buffer boundary (N/A — no buffering), session isolation (channel A stop doesn't affect B),
     transport decoupling (dispatchTranscript uses system.send not fluxWs.send), cleanup (stop closes WS on last channel),
     upstream failure (all channels notified, state cleared)
[x] discover/health messages handled
[x] dispatchTranscript uses system.send, not direct client WS reference
[x] Note: UIActor-specific checklist items not applicable (CF Workers actor, not browser)
```
