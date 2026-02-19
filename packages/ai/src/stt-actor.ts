/**
 * SttActor — Speech-to-text capability actor (Deepgram Nova)
 *
 * Address: ai/stt/<namespace>/<provider>/<model>
 * Example: ai/stt/bln_ai/deepgram/nova-3
 *
 * Address segments:
 *   [0] = 'ai'
 *   [1] = 'stt'
 *   [2] = namespace
 *   [3] = provider  (e.g. 'deepgram')
 *   [4] = model     (e.g. 'nova-3', 'nova-2')
 *
 * Handles:
 *   ai.stt.start      → configure transcription session (encoding, sample rate, language)
 *   ai.stt.stop       → end session, flush any buffered audio
 *   audio.frame       → Uint8Array audio chunk via binary channel fast-path
 *
 * Audio → transcript flow:
 *   1. Client opens binary channel: channel:open → address = ai/stt/bln_ai/deepgram/nova-3
 *   2. Client sends ai.stt.start to configure session
 *   3. Binary audio frames arrive via binary channel → system.send(address, 'audio.frame', bytes)
 *   4. SttActor accumulates frames and sends to Deepgram REST API in chunks
 *   5. Transcripts are sent back as ai.stt.transcript messages to the sender
 */

import type { Message, MessageResponse, MessageHandler, Address } from '@agentic-primer/actors';
import { createResponse, createErrorResponse, address } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type { AiSttStartPayload, AiSttTranscriptPayload, SttAudioEncoding } from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface SttActorConfig {
  /** AI Gateway URL or Deepgram API URL. */
  gatewayUrl: string;
  /** Deepgram API key. */
  apiKey?: string;
  /**
   * Minimum audio buffer before sending to transcription API (bytes).
   * @default 32768 (32 KB ≈ 1 second at 16-bit 16kHz mono)
   */
  minBufferBytes?: number;
}

// ---------------------------------------------------------------------------
// Address parsing
// ---------------------------------------------------------------------------

export interface SttAddress {
  namespace: string;
  provider: string;
  model: string;
}

export function parseSttAddress(address: string): SttAddress | null {
  const parts = address.split('/');
  if (parts.length < 5 || parts[0] !== 'ai' || parts[1] !== 'stt') return null;
  return {
    namespace: parts[2],
    provider: parts[3],
    model: parts.slice(4).join('/'),
  };
}

// ---------------------------------------------------------------------------
// Per-session state
// ---------------------------------------------------------------------------

interface SttSession {
  encoding: SttAudioEncoding;
  sampleRate: number;
  language: string;
  model: string;
  /** Sender address to reply transcripts to. */
  replyTo: Address | undefined;
  audioBuffer: Uint8Array[];
  totalBytes: number;
}

// ---------------------------------------------------------------------------
// SttActor
// ---------------------------------------------------------------------------

export class SttActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly parsed: SttAddress;
  private readonly config: SttActorConfig;
  private readonly minBuffer: number;
  /** Per-connection sessions keyed by sender address string. */
  private sessions = new Map<string, SttSession>();

  constructor(address: string, config: SttActorConfig) {
    this.actorAddress = address;
    const parsed = parseSttAddress(address);
    if (!parsed) throw new Error(`Invalid STT actor address: ${address}`);
    this.parsed = parsed;
    this.config = config;
    this.minBuffer = config.minBufferBytes ?? 32768;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      switch (message.type) {
        case AI_MESSAGE_TYPES.STT_START:
          return this.handleStart(message);
        case AI_MESSAGE_TYPES.STT_STOP:
          return await this.handleStop(message);
        case AI_MESSAGE_TYPES.AUDIO_FRAME:
          return await this.handleAudioFrame(message);
        default:
          return createErrorResponse(
            message,
            `SttActor(${this.actorAddress}): unhandled message type '${message.type}'`,
          );
      }
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  // --- Handlers ---

  private handleStart(message: Message): MessageResponse {
    const payload = (message.payload ?? {}) as AiSttStartPayload;
    const senderKey = message.from ? String(message.from) : 'unknown';

    const session: SttSession = {
      encoding: payload.encoding ?? 'linear16',
      sampleRate: payload.sampleRate ?? 16000,
      language: payload.language ?? 'en-US',
      model: payload.model ?? this.parsed.model,
      replyTo: message.from,
      audioBuffer: [],
      totalBytes: 0,
    };
    this.sessions.set(senderKey, session);

    return createResponse(message, {
      status: 'started',
      encoding: session.encoding,
      sampleRate: session.sampleRate,
      language: session.language,
      model: session.model,
    });
  }

  private async handleStop(message: Message): Promise<MessageResponse> {
    const senderKey = message.from ? String(message.from) : 'unknown';
    const session = this.sessions.get(senderKey);

    if (!session) {
      return createErrorResponse(message, 'No active STT session for this sender');
    }

    // Flush remaining buffered audio
    if (session.totalBytes > 0) {
      await this.flushAndTranscribe(session, true);
    }

    this.sessions.delete(senderKey);
    return createResponse(message, { status: 'stopped' });
  }

  private async handleAudioFrame(message: Message): Promise<MessageResponse> {
    const audio = message.payload as Uint8Array;
    const senderKey = message.from ? String(message.from) : 'unknown';

    // Get or create a default session for senders that skipped ai.stt.start
    let session = this.sessions.get(senderKey);
    if (!session) {
      session = {
        encoding: 'linear16',
        sampleRate: 16000,
        language: 'en-US',
        model: this.parsed.model,
        replyTo: message.from,
        audioBuffer: [],
        totalBytes: 0,
      };
      this.sessions.set(senderKey, session);
    }

    session.audioBuffer.push(audio);
    session.totalBytes += audio.byteLength;

    // Send to transcription when buffer is large enough
    if (session.totalBytes >= this.minBuffer) {
      await this.flushAndTranscribe(session, false);
    }

    return createResponse(message, { buffered: session.totalBytes });
  }

  // --- Transcription ---

  private async flushAndTranscribe(session: SttSession, isFinal: boolean): Promise<void> {
    if (session.audioBuffer.length === 0) return;

    // Concatenate buffered audio
    const merged = this.mergeBuffers(session.audioBuffer);
    session.audioBuffer = [];
    session.totalBytes = 0;

    try {
      const transcript = await this.transcribe(merged, session);
      if (transcript && session.replyTo) {
        // Note: in a real ActorSystem, this would use system.send()
        // For now we capture the transcript in the response
        // The caller pattern is: register an onMessage callback or use ask()
        // The full integration wires this via the SessionActor
        void this.dispatchTranscript(session.replyTo, transcript, isFinal);
      }
    } catch {
      // Transcription errors are non-fatal — log and continue
    }
  }

  private async transcribe(audio: Uint8Array, session: SttSession): Promise<string | null> {
    const { url, headers } = this.buildTranscribeRequest(session);

    const response = await fetch(url, {
      method: 'POST',
      headers: { ...headers, 'Content-Type': 'audio/raw' },
      body: audio,
    });

    if (!response.ok) return null;

    const result = await response.json() as {
      results?: { channels?: Array<{ alternatives?: Array<{ transcript?: string }> }> };
    };
    return result.results?.channels?.[0]?.alternatives?.[0]?.transcript ?? null;
  }

  private buildTranscribeRequest(session: SttSession): {
    url: string;
    headers: Record<string, string>;
  } {
    const headers: Record<string, string> = {};
    if (this.config.apiKey) headers['Authorization'] = `Token ${this.config.apiKey}`;

    const params = new URLSearchParams({
      model: session.model,
      language: session.language,
      encoding: session.encoding,
      sample_rate: String(session.sampleRate),
    });

    const base = this.config.gatewayUrl.endsWith('/')
      ? this.config.gatewayUrl
      : `${this.config.gatewayUrl}/`;

    return {
      url: `${base}deepgram/v1/listen?${params}`,
      headers,
    };
  }

  private mergeBuffers(buffers: Uint8Array[]): Uint8Array {
    const total = buffers.reduce((sum, b) => sum + b.byteLength, 0);
    const merged = new Uint8Array(total);
    let offset = 0;
    for (const buf of buffers) {
      merged.set(buf, offset);
      offset += buf.byteLength;
    }
    return merged;
  }

  /**
   * Dispatch a transcript result to the originating sender.
   * In a full ActorSystem integration, this would be replaced by system.send().
   * Declared here as an overridable hook for testing and integration.
   */
  protected async dispatchTranscript(
    replyTo: Address,
    transcript: string,
    isFinal: boolean,
  ): Promise<void> {
    // Subclasses or integration wrappers override this to use system.send().
    // Default: no-op (transcript is lost without a system reference).
    void replyTo;
    void transcript;
    void isFinal;
  }
}

// ---------------------------------------------------------------------------
// System-integrated variant
// ---------------------------------------------------------------------------

/**
 * SttActor variant that holds a reference to the ActorSystem so it can
 * dispatch transcripts back to the sender via system.send().
 */
export class SystemSttActor extends SttActor {
  private readonly system: { send(to: Address, type: string, payload: unknown): void };

  constructor(
    actorAddress: string,
    config: SttActorConfig,
    system: { send(to: Address, type: string, payload: unknown): void },
  ) {
    super(actorAddress, config);
    this.system = system;
  }

  protected override async dispatchTranscript(
    replyTo: Address,
    transcript: string,
    isFinal: boolean,
  ): Promise<void> {
    const payload: AiSttTranscriptPayload = { transcript, isFinal };
    this.system.send(replyTo, AI_MESSAGE_TYPES.STT_TRANSCRIPT, payload);
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/stt/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.STT,
 *   factory: createSttFactory({ gatewayUrl: env.AI_GATEWAY_URL, apiKey: env.DEEPGRAM_API_KEY }, system),
 * });
 */
export function createSttFactory(
  config: SttActorConfig,
  system: { send(to: Address, type: string, payload: unknown): void },
): (address: string) => SystemSttActor | null {
  return (actorAddress: string) => {
    const parsed = parseSttAddress(actorAddress);
    if (!parsed) return null;
    return new SystemSttActor(actorAddress, config, system);
  };
}
