/**
 * Integration tests for TtsActor
 *
 * Address: ai/tts/<namespace>/<provider>/<voice>
 * Example: ai/tts/bln_ai/deepgram/aura-2-en-us
 */

import { describe, it, expect, mock, beforeEach } from 'bun:test';
import { TtsActor, parseTtsAddress, createTtsFactory } from './tts-actor.ts';
import type { TtsActorConfig } from './tts-actor.ts';
import { address } from '@agentic-primer/actors';
import type { Message } from '@agentic-primer/actors';
import { AI_MESSAGE_TYPES } from './types.ts';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

let seq = 0;
function msg(type: string, payload: unknown): Message {
  return {
    id: `test-${++seq}`,
    pattern: 'ask',
    type,
    payload,
    to: address('ai/tts/bln_ai/deepgram/aura-2-en-us'),
    from: address('sender'),
    correlationId: `corr-${seq}`,
    timestamp: Date.now(),
  };
}

const BASE_CONFIG: TtsActorConfig = {
  gatewayUrl: 'https://gateway.ai.cloudflare.com/v1/test-account/test-gateway',
  apiKey: 'test-deepgram-key',
  defaultEncoding: 'mp3',
  defaultSampleRate: 24000,
};

const VALID_ADDRESS = 'ai/tts/bln_ai/deepgram/aura-2-en-us';

// ---------------------------------------------------------------------------
// parseTtsAddress
// ---------------------------------------------------------------------------

describe('parseTtsAddress', () => {
  it('parses a valid TTS address', () => {
    const result = parseTtsAddress('ai/tts/bln_ai/deepgram/aura-2-en-us');
    expect(result).not.toBeNull();
    expect(result!.namespace).toBe('bln_ai');
    expect(result!.provider).toBe('deepgram');
    expect(result!.voice).toBe('aura-2-en-us');
  });

  it('handles voice names with slashes', () => {
    const result = parseTtsAddress('ai/tts/bln_ai/openai/tts-1-hd/alloy');
    expect(result).not.toBeNull();
    expect(result!.voice).toBe('tts-1-hd/alloy');
  });

  it('returns null for wrong prefix (ai/stt/...)', () => {
    expect(parseTtsAddress('ai/stt/bln_ai/deepgram/nova-3')).toBeNull();
  });

  it('returns null for too-short address', () => {
    expect(parseTtsAddress('ai/tts/ns/provider')).toBeNull();
  });

  it('returns null for wrong top-level prefix', () => {
    expect(parseTtsAddress('wrong/tts/ns/prov/voice')).toBeNull();
  });

  it('returns null for empty string', () => {
    expect(parseTtsAddress('')).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

describe('TtsActor — constructor', () => {
  it('constructs successfully with a valid address', () => {
    expect(() => new TtsActor(VALID_ADDRESS, BASE_CONFIG)).not.toThrow();
  });

  it('throws for invalid address', () => {
    expect(() => new TtsActor('bad/address', BASE_CONFIG)).toThrow(
      /Invalid TTS actor address/,
    );
  });

  it('throws for wrong-prefix address', () => {
    expect(() => new TtsActor('ai/inference/ns/prov/model', BASE_CONFIG)).toThrow();
  });
});

// ---------------------------------------------------------------------------
// Unknown message type
// ---------------------------------------------------------------------------

describe('TtsActor — unknown message type', () => {
  it('returns error response for unknown type', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg('ai.unknown.type', {}));
    expect(response.success).toBe(false);
    expect(response.error).toContain('unhandled message type');
  });
});

// ---------------------------------------------------------------------------
// ai.discover
// ---------------------------------------------------------------------------

describe('TtsActor — ai.discover', () => {
  it('returns address in payload', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { address: string }).address).toBe(VALID_ADDRESS);
  });

  it('returns type = tts', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    expect((response.payload as { type: string }).type).toBe('tts');
  });

  it('includes voice and provider in meta', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.DISCOVER, {}));
    const meta = (response.payload as { meta: Record<string, string> }).meta;
    expect(meta.provider).toBe('deepgram');
    expect(meta.voice).toBe('aura-2-en-us');
  });
});

// ---------------------------------------------------------------------------
// ai.health
// ---------------------------------------------------------------------------

describe('TtsActor — ai.health', () => {
  it('returns { status: "ok" }', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, {}));
    expect(response.success).toBe(true);
    expect((response.payload as { status: string }).status).toBe('ok');
  });

  it('echoes token from request', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(msg(AI_MESSAGE_TYPES.HEALTH, { token: 'tts-health-ping' }));
    expect((response.payload as { token: string }).token).toBe('tts-health-ping');
  });
});

// ---------------------------------------------------------------------------
// ai.tts.request — happy path
// ---------------------------------------------------------------------------

describe('TtsActor — ai.tts.request happy path', () => {
  let mockFetch: ReturnType<typeof mock>;

  beforeEach(() => {
    // TTS returns binary audio (ArrayBuffer)
    const audioData = new Uint8Array([0x01, 0x02, 0x03, 0x04]).buffer;
    mockFetch = mock(() =>
      Promise.resolve({
        ok: true,
        arrayBuffer: () => Promise.resolve(audioData),
        text: () => Promise.resolve(''),
      }),
    );
    global.fetch = mockFetch as unknown as typeof fetch;
  });

  it('returns audio payload with byteLength', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.TTS_REQUEST, { text: 'Hello world' }),
    );
    expect(response.success).toBe(true);
    const payload = response.payload as { byteLength: number; encoding: string; voice: string };
    expect(payload.byteLength).toBe(4);
    expect(typeof payload.encoding).toBe('string');
    expect(payload.voice).toBe('aura-2-en-us');
  });

  it('uses voice override from payload', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.TTS_REQUEST, { text: 'Hi', voice: 'custom-voice' }),
    );
    const payload = response.payload as { voice: string };
    expect(payload.voice).toBe('custom-voice');
  });

  it('uses encoding override from payload', async () => {
    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.TTS_REQUEST, { text: 'Hi', encoding: 'opus' }),
    );
    const payload = response.payload as { encoding: string };
    expect(payload.encoding).toBe('opus');
  });
});

// ---------------------------------------------------------------------------
// ai.tts.request — gateway error
// ---------------------------------------------------------------------------

describe('TtsActor — gateway error', () => {
  it('returns error response when fetch returns ok=false', async () => {
    global.fetch = mock(() =>
      Promise.resolve({
        ok: false,
        status: 401,
        text: () => Promise.resolve('Unauthorized'),
        arrayBuffer: () => Promise.resolve(new ArrayBuffer(0)),
      }),
    ) as unknown as typeof fetch;

    const actor = new TtsActor(VALID_ADDRESS, BASE_CONFIG);
    const response = await actor.receive(
      msg(AI_MESSAGE_TYPES.TTS_REQUEST, { text: 'Hello' }),
    );
    expect(response.success).toBe(false);
    expect(response.error).toContain('401');
  });
});

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

describe('createTtsFactory', () => {
  it('returns null for wrong prefix', () => {
    const factory = createTtsFactory(BASE_CONFIG);
    expect(factory('ai/stt/ns/prov/model')).toBeNull();
  });

  it('returns null for too-short address', () => {
    const factory = createTtsFactory(BASE_CONFIG);
    expect(factory('ai/tts/ns/prov')).toBeNull();
  });

  it('returns TtsActor for correct prefix', () => {
    const factory = createTtsFactory(BASE_CONFIG);
    const actor = factory('ai/tts/bln_ai/deepgram/aura-2-en-us');
    expect(actor).not.toBeNull();
    expect(actor).toBeInstanceOf(TtsActor);
  });
});
