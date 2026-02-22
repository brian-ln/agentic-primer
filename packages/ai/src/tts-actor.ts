/**
 * TtsActor — Text-to-speech capability actor (Deepgram Aura)
 *
 * Address: ai/tts/<namespace>/<provider>/<voice>
 * Example: ai/tts/bln_ai/deepgram/aura-2-en-us
 *
 * Address segments:
 *   [0] = 'ai'
 *   [1] = 'tts'
 *   [2] = namespace
 *   [3] = provider  (e.g. 'deepgram', 'openai', 'elevenlabs')
 *   [4] = voice     (e.g. 'aura-2-en-us', 'tts-1-hd')
 *
 * Handles:
 *   ai.tts.request → TtsRequestPayload
 *   Response payload contains synthesized audio as base64 or binary reference.
 *
 * Audio delivery:
 *   For real-time streaming, TTS audio is sent back via binary channel to the
 *   caller's WebSocket connection. For non-streaming, audio bytes are included
 *   in the response payload.
 */

import type { Message, MessageResponse, MessageHandler } from '@agentic-primer/actors';
import { createResponse, createErrorResponse } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';
import type {
  TtsRequestPayload,
  AudioEncoding,
  DiscoverResponsePayload,
  HealthPayload,
  HealthResponsePayload,
} from './types.ts';

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

export interface TtsActorConfig {
  /** AI Gateway URL or provider API URL. */
  gatewayUrl: string;
  /** Bearer token for the provider. */
  apiKey?: string;
  /** Default encoding. @default 'opus' */
  defaultEncoding?: AudioEncoding;
  /** Default sample rate in Hz. @default 24000 */
  defaultSampleRate?: number;
}

// ---------------------------------------------------------------------------
// Address parsing
// ---------------------------------------------------------------------------

export interface TtsAddress {
  namespace: string;
  provider: string;
  voice: string;
}

export function parseTtsAddress(address: string): TtsAddress | null {
  const parts = address.split('/');
  if (parts.length < 5 || parts[0] !== 'ai' || parts[1] !== 'tts') return null;
  return {
    namespace: parts[2],
    provider: parts[3],
    voice: parts.slice(4).join('/'),
  };
}

// ---------------------------------------------------------------------------
// TtsActor
// ---------------------------------------------------------------------------

export class TtsActor implements MessageHandler {
  private readonly actorAddress: string;
  private readonly parsed: TtsAddress;
  private readonly config: TtsActorConfig;

  constructor(address: string, config: TtsActorConfig) {
    this.actorAddress = address;
    const parsed = parseTtsAddress(address);
    if (!parsed) throw new Error(`Invalid TTS actor address: ${address}`);
    this.parsed = parsed;
    this.config = config;
  }

  async receive(message: Message): Promise<MessageResponse> {
    try {
      if (message.type === AI_MESSAGE_TYPES.TTS_REQUEST) {
        return await this.handleTtsRequest(message);
      }
      if (message.type === AI_MESSAGE_TYPES.DISCOVER) {
        return this.handleDiscover(message);
      }
      if (message.type === AI_MESSAGE_TYPES.HEALTH) {
        return this.handleHealth(message);
      }
      return createErrorResponse(
        message,
        `TtsActor(${this.actorAddress}): unhandled message type '${message.type}'`,
      );
    } catch (err) {
      return createErrorResponse(message, err instanceof Error ? err.message : String(err));
    }
  }

  private handleDiscover(message: Message): MessageResponse {
    const result: DiscoverResponsePayload = {
      address: this.actorAddress,
      type: 'tts',
      handles: [AI_MESSAGE_TYPES.TTS_REQUEST, AI_MESSAGE_TYPES.DISCOVER, AI_MESSAGE_TYPES.HEALTH],
      meta: { namespace: this.parsed.namespace, provider: this.parsed.provider, voice: this.parsed.voice },
    };
    return createResponse(message, result);
  }

  private handleHealth(message: Message): MessageResponse {
    const payload = message.payload as HealthPayload;
    const result: HealthResponsePayload = { status: 'ok', address: this.actorAddress, token: payload?.token };
    return createResponse(message, result);
  }

  private async handleTtsRequest(message: Message): Promise<MessageResponse> {
    const payload = message.payload as TtsRequestPayload;
    const voice = payload.voice ?? this.parsed.voice;
    const encoding = payload.encoding ?? this.config.defaultEncoding ?? 'opus';
    const sampleRate = payload.sampleRate ?? this.config.defaultSampleRate ?? 24000;

    const audioBytes = await this.synthesize(payload.text, voice, encoding, sampleRate);

    return createResponse(message, {
      audio: audioBytes,
      encoding,
      sampleRate,
      voice,
      byteLength: audioBytes.byteLength,
    });
  }

  private async synthesize(
    text: string,
    voice: string,
    encoding: AudioEncoding,
    sampleRate: number,
  ): Promise<ArrayBuffer> {
    const { url, body, headers } = this.buildProviderRequest(text, voice, encoding, sampleRate);

    const response = await fetch(url, {
      method: 'POST',
      headers,
      body: JSON.stringify(body),
    });

    if (!response.ok) {
      throw new Error(`TTS gateway error ${response.status}: ${await response.text()}`);
    }

    return response.arrayBuffer();
  }

  private buildProviderRequest(
    text: string,
    voice: string,
    encoding: AudioEncoding,
    sampleRate: number,
  ): { url: string; body: unknown; headers: Record<string, string> } {
    const headers: Record<string, string> = { 'Content-Type': 'application/json' };
    if (this.config.apiKey) headers['Authorization'] = `Token ${this.config.apiKey}`;

    switch (this.parsed.provider) {
      case 'deepgram': {
        // Deepgram Aura TTS REST API
        const encodingMap: Record<AudioEncoding, string> = {
          opus: 'opus',
          mp3: 'mp3',
          linear16: 'linear16',
        };
        return {
          url: `${this.config.gatewayUrl}/deepgram/v1/speak?model=${voice}&encoding=${encodingMap[encoding]}&sample_rate=${sampleRate}`,
          body: { text },
          headers,
        };
      }
      case 'openai': {
        return {
          url: `${this.config.gatewayUrl}/openai/v1/audio/speech`,
          body: { model: 'tts-1-hd', input: text, voice, response_format: encoding === 'linear16' ? 'pcm' : encoding },
          headers,
        };
      }
      default: {
        // Generic OpenAI-compatible endpoint via AI Gateway
        return {
          url: `${this.config.gatewayUrl}/openai/v1/audio/speech`,
          body: { model: voice, input: text, response_format: encoding },
          headers,
        };
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create an ActorFactory for the `ai/tts/` prefix.
 *
 * @example
 * system.registerFactory({
 *   prefix: AI_PREFIXES.TTS,
 *   factory: createTtsFactory({ gatewayUrl: env.AI_GATEWAY_URL, apiKey: env.DEEPGRAM_API_KEY }),
 * });
 */
export function createTtsFactory(
  config: TtsActorConfig,
): (address: string) => TtsActor | null {
  return (address: string) => {
    const parsed = parseTtsAddress(address);
    if (!parsed) return null;
    return new TtsActor(address, config);
  };
}
