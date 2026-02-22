/**
 * FluxRelayActor — Real-time STT via @cf/deepgram/flux streaming WebSocket
 *
 * Address: ai/flux/<namespace>
 * Example: ai/flux/bln_ai
 *
 * Address segments:
 *   [0] = 'ai'
 *   [1] = 'flux'
 *   [2] = namespace
 *
 * Distinct from SttActor (nova-3 batch REST): FluxRelayActor maintains a
 * persistent WebSocket to the Deepgram Flux model and streams audio frames
 * directly — no buffering, no batch, low latency.
 *
 * Audio → transcript flow:
 *   1. Client opens binary channel: channel:open → address = ai/flux/bln_ai
 *   2. Client sends ai.stt.start to configure the channel
 *   3. FluxRelayActor lazy-opens a Flux WebSocket on the first ai.stt.start
 *   4. Binary audio frames arrive via binary channel → system.send(address, 'audio.frame', bytes)
 *   5. FluxRelayActor forwards raw PCM directly to the Flux WS — no buffering
 *   6. Flux WS emits transcript events; actor broadcasts ai.stt.transcript to all active senders
 *   7. On ai.stt.stop (last channel): actor closes the Flux WS
 *   8. On Flux WS failure: actor sends ai.stt.error to all active senders, resets state
 *
 * Credentials:
 *   aigToken (CF_AIG_TOKEN) is injected at factory construction from env binding.
 *   It is never read from client messages (security boundary).
 *
 * Named DO:
 *   Use FLUX_RELAY.idFromName(namespace) — one DO per namespace.
 *   Ensures browser WS and BLE relay share the same actor instance and Flux WS.
 */

import type { Message, MessageResponse, MessageHandler, Address } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type {
  SttStartPayload,
  SttTranscriptPayload,
  SttErrorPayload,
  DiscoverResponsePayload,
  HealthPayload,
  HealthResponsePayload,
} from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface FluxRelayActorConfig {
  /**
   * Cloudflare AI Gateway WebSocket URL for @cf/deepgram/flux.
   * Format: wss://gateway.ai.cloudflare.com/v1/<account-id>/<gateway-name>/deepgram/flux
   */
  fluxWsUrl: string;
  /**
   * CF AI Gateway token (CF_AIG_TOKEN).
   * Injected at construction from env binding — never from message payload.
   */
  aigToken: string;
  /** Namespace this actor serves (extracted from address). */
  namespace: string;
}

// ---------------------------------------------------------------------------
// Address parsing
// ---------------------------------------------------------------------------

export interface FluxRelayAddress {
  namespace: string;
}

export function parseFluxRelayAddress(address: string): FluxRelayAddress | null {
  const parts = address.split('/');
  if (parts.length < 3 || parts[0] !== 'ai' || parts[1] !== 'flux') return null;
  return { namespace: parts[2] };
}

// ---------------------------------------------------------------------------
// Per-channel state
// ---------------------------------------------------------------------------

interface FluxChannel {
  /** Sender address to dispatch ai.stt.transcript and ai.stt.error back to. */
  replyTo: Address;
  encoding: 'linear16' | 'mulaw' | 'opus';
  sampleRate: number;
  language: string;
  /** Timestamp of last audio.frame for idle-channel detection. */
  lastFrameAt: number;
}

// Internal Flux transcript shape (Deepgram streaming WS output)
interface FluxTranscriptEvent {
  type: string;
  channel?: {
    alternatives?: Array<{
      transcript?: string;
      confidence?: number;
    }>;
  };
  is_final?: boolean;
  words?: Array<{ word: string; start: number; end: number; confidence?: number }>;
}

// ---------------------------------------------------------------------------
// FluxRelayActor
// ---------------------------------------------------------------------------

export class FluxRelayActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly parsed: FluxRelayAddress;
  private readonly config: FluxRelayActorConfig;

  /**
   * Active sender channels keyed by sender address string (message.from).
   * Populated by ai.stt.start, removed by ai.stt.stop or upstream failure.
   */
  private channels = new Map<string, FluxChannel>();

  /**
   * The single upstream Flux WebSocket shared by all channels in this namespace.
   * Opened lazily on the first ai.stt.start. null = not yet connected.
   */
  private fluxWs: WebSocket | null = null;
  private fluxWsState: 'disconnected' | 'connecting' | 'connected' | 'failed' = 'disconnected';

  constructor(address: string, config: FluxRelayActorConfig) {
    this.actorAddress = address;
    const parsed = parseFluxRelayAddress(address);
    if (!parsed) throw new Error(`Invalid FluxRelayActor address: ${address}`);
    this.parsed = parsed;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      switch (message.type) {
        case AI_MESSAGE_TYPES.STT_START:
          return await this.handleStart(message);
        case AI_MESSAGE_TYPES.STT_STOP:
          return this.handleStop(message);
        case AI_MESSAGE_TYPES.AUDIO_FRAME:
          return this.handleAudioFrame(message);
        case AI_MESSAGE_TYPES.HEALTH:
          return this.handleHealth(message);
        case AI_MESSAGE_TYPES.DISCOVER:
          return this.handleDiscover(message);
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

  // --- Handlers ---

  private async handleStart(message: Message): Promise<MessageResponse> {
    const payload = (message.payload ?? {}) as SttStartPayload;
    const senderKey = message.from ? String(message.from) : 'unknown';

    const channel: FluxChannel = {
      replyTo: message.from as Address,
      encoding: (payload.encoding ?? 'linear16') as FluxChannel['encoding'],
      sampleRate: payload.sampleRate ?? 16000,
      language: payload.language ?? 'en-US',
      lastFrameAt: Date.now(),
    };
    this.channels.set(senderKey, channel);

    // Lazy-open Flux WS on first start
    if (this.fluxWsState === 'disconnected' || this.fluxWsState === 'failed') {
      await this.openFluxWs(channel);
    }

    return createResponse(message, {
      status: 'started',
      encoding: channel.encoding,
      sampleRate: channel.sampleRate,
      language: channel.language,
      namespace: this.parsed.namespace,
    });
  }

  private handleStop(message: Message): MessageResponse {
    const senderKey = message.from ? String(message.from) : 'unknown';

    if (!this.channels.has(senderKey)) {
      return createErrorResponse(message, 'No active Flux channel for this sender');
    }

    this.channels.delete(senderKey);

    // Close the upstream Flux WS if no channels remain
    if (this.channels.size === 0 && this.fluxWs !== null) {
      this.fluxWs.close(1000, 'no active channels');
      this.fluxWs = null;
      this.fluxWsState = 'disconnected';
    }

    return createResponse(message, { status: 'stopped' });
  }

  private handleAudioFrame(message: Message): MessageResponse {
    const audio = message.payload as Uint8Array;
    const senderKey = message.from ? String(message.from) : 'unknown';

    const channel = this.channels.get(senderKey);
    if (!channel) {
      // Audio for unknown channel — fast-path drop (matches binary-audio-pattern rule)
      return createResponse(message, { dropped: true });
    }

    channel.lastFrameAt = Date.now();

    // Forward directly to Flux WS — no buffering, flux is designed for streaming
    if (this.fluxWsState === 'connected' && this.fluxWs !== null) {
      this.fluxWs.send(audio);
    }
    // If not connected yet (e.g. WS still opening), frame is silently dropped.
    // Clients should wait for the ai.stt.start ack before sending audio.

    return createResponse(message, { forwarded: this.fluxWsState === 'connected' });
  }

  private handleDiscover(message: Message): MessageResponse {
    const result: DiscoverResponsePayload = {
      address: this.actorAddress,
      type: 'flux',
      handles: [
        AI_MESSAGE_TYPES.STT_START,
        AI_MESSAGE_TYPES.STT_STOP,
        AI_MESSAGE_TYPES.AUDIO_FRAME,
        AI_MESSAGE_TYPES.HEALTH,
        AI_MESSAGE_TYPES.DISCOVER,
      ],
      meta: {
        namespace: this.parsed.namespace,
        upstream: '@cf/deepgram/flux',
        wsState: this.fluxWsState,
      },
    };
    return createResponse(message, result);
  }

  private handleHealth(message: Message): MessageResponse {
    const payload = message.payload as HealthPayload;
    const status: HealthResponsePayload['status'] =
      this.fluxWsState === 'connected' ? 'ok' : 'degraded';
    const result: HealthResponsePayload = {
      status,
      address: this.actorAddress,
      token: payload?.token,
      ...(this.fluxWsState !== 'connected' && { error: `Flux WS state: ${this.fluxWsState}` }),
    };
    return createResponse(message, result);
  }

  // --- Flux WebSocket lifecycle ---

  /**
   * Open the upstream Flux WebSocket.
   * Called lazily on the first ai.stt.start.
   * Returns when the WS is open (connected) or immediately if already connecting.
   */
  private openFluxWs(firstChannel: FluxChannel): Promise<void> {
    if (this.fluxWsState === 'connecting' || this.fluxWsState === 'connected') {
      return Promise.resolve();
    }

    this.fluxWsState = 'connecting';

    return new Promise<void>((resolve) => {
      const url = new URL(this.config.fluxWsUrl);
      // Query params for the Flux WS (encoding, sample_rate, language from first channel)
      url.searchParams.set('encoding', firstChannel.encoding);
      url.searchParams.set('sample_rate', String(firstChannel.sampleRate));
      url.searchParams.set('language', firstChannel.language);

      const ws = new WebSocket(url.toString(), {
        headers: { Authorization: `Bearer ${this.config.aigToken}` },
      } as unknown as string[]);

      ws.onopen = () => {
        this.fluxWs = ws;
        this.fluxWsState = 'connected';
        resolve();
      };

      ws.onmessage = (event: MessageEvent) => {
        this.handleFluxMessage(event.data as string);
      };

      ws.onerror = () => {
        // onerror is always followed by onclose — let onclose do the cleanup
      };

      ws.onclose = (event: CloseEvent) => {
        const wasConnecting = this.fluxWsState === 'connecting';
        // Only treat as failure if we didn't close it intentionally (code 1000)
        if (event.code !== 1000) {
          this.handleFluxFailure();
        }
        if (wasConnecting) {
          // WS failed before onopen — resolve so handleStart can return an error response
          this.fluxWsState = 'failed';
          this.fluxWs = null;
          resolve();
        }
      };
    });
  }

  /**
   * Handle a transcript event from the upstream Flux WS.
   * Broadcasts to all active senders.
   */
  private handleFluxMessage(raw: string): void {
    let event: FluxTranscriptEvent;
    try {
      event = JSON.parse(raw) as FluxTranscriptEvent;
    } catch {
      return; // Ignore unparseable events
    }

    if (event.type !== 'Results') return;

    const transcript = event.channel?.alternatives?.[0]?.transcript ?? '';
    const confidence = event.channel?.alternatives?.[0]?.confidence;
    const isFinal = event.is_final ?? false;

    if (!transcript) return;

    const payload: SttTranscriptPayload = {
      transcript,
      isFinal,
      confidence,
    };

    // Broadcast to all active senders — Flux mixes all audio into one stream
    for (const [, channel] of this.channels) {
      this.dispatchTranscript(channel.replyTo, payload);
    }
  }

  /**
   * Handle unexpected Flux WS closure or error.
   * Notifies all active senders, clears state.
   * No automatic reconnect — callers must re-send ai.stt.start.
   */
  private handleFluxFailure(): void {
    this.fluxWsState = 'failed';

    for (const [, channel] of this.channels) {
      this.notifyChannelError(channel.replyTo, 'Flux upstream disconnected');
    }

    this.channels.clear();
    this.fluxWs = null;
    this.fluxWsState = 'disconnected';
  }

  // --- Overridable dispatch hooks ---

  /**
   * Dispatch a transcript to the originating sender.
   * Override in SystemFluxRelayActor to use system.send().
   * Default: no-op (transcript lost without system reference).
   */
  protected dispatchTranscript(replyTo: Address, payload: SttTranscriptPayload): void {
    void replyTo;
    void payload;
  }

  /**
   * Notify a sender of a Flux WS failure.
   * Override in SystemFluxRelayActor to use system.send().
   */
  protected notifyChannelError(replyTo: Address, error: string): void {
    void replyTo;
    void error;
  }
}

// ---------------------------------------------------------------------------
// System-integrated variant
// ---------------------------------------------------------------------------

/**
 * FluxRelayActor variant that holds a reference to the ActorSystem so it can
 * dispatch transcripts and errors back to senders via system.send().
 */
export class SystemFluxRelayActor extends FluxRelayActor {
  private readonly system: { send(to: Address, type: string, payload: unknown): void };

  constructor(
    actorAddress: string,
    config: FluxRelayActorConfig,
    system: { send(to: Address, type: string, payload: unknown): void },
  ) {
    super(actorAddress, config);
    this.system = system;
  }

  protected override dispatchTranscript(replyTo: Address, payload: SttTranscriptPayload): void {
    this.system.send(replyTo, AI_MESSAGE_TYPES.STT_TRANSCRIPT, payload);
  }

  protected override notifyChannelError(replyTo: Address, error: string): void {
    const payload: SttErrorPayload = { error };
    this.system.send(replyTo, AI_MESSAGE_TYPES.STT_ERROR, payload);
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/flux/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.FLUX,
 *   factory: createFluxRelayFactory(
 *     {
 *       fluxWsUrl: `wss://gateway.ai.cloudflare.com/v1/${env.CF_ACCOUNT_ID}/${env.CF_GATEWAY_NAME}/deepgram/flux`,
 *       aigToken: env.CF_AIG_TOKEN,
 *     },
 *     system,
 *   ),
 * });
 */
export function createFluxRelayFactory(
  config: Omit<FluxRelayActorConfig, 'namespace'>,
  system: { send(to: Address, type: string, payload: unknown): void },
): (address: string) => SystemFluxRelayActor | null {
  return (actorAddress: string) => {
    const parsed = parseFluxRelayAddress(actorAddress);
    if (!parsed) return null;
    return new SystemFluxRelayActor(
      actorAddress,
      { ...config, namespace: parsed.namespace },
      system,
    );
  };
}
